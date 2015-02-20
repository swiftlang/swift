//===--- GenCoverage.cpp - IR Generation for coverage ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for the initialization of
//  coverage related variables.
//
//===----------------------------------------------------------------------===//

#include "IRGenModule.h"

#include "swift/SIL/SILModule.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/ProfileData/CoverageMappingWriter.h"
#include "llvm/Support/FileSystem.h"

using namespace swift;
using namespace irgen;

using llvm::coverage::CounterMappingRegion;

void IRGenModule::emitCoverageMapping() {
  const auto &Mappings = SILMod->getCoverageMapList();
  // If there aren't any coverage maps, there's nothing to emit.
  if (Mappings.empty())
    return;

  std::vector<StringRef> Files;
  for (const auto &M : Mappings)
    if (std::find(Files.begin(), Files.end(), M.getFile()) == Files.end())
      Files.push_back(M.getFile());

  // Awkwardly munge absolute filenames into a vector of StringRefs.
  // TODO: This is heinous - the same thing is happening in clang, but the API
  // really needs to be cleaned up for both.
  llvm::SmallVector<std::string, 8> FilenameStrs;
  llvm::SmallVector<StringRef, 8> FilenameRefs;
  for (StringRef Name : Files) {
    llvm::SmallString<256> Path(Name);
    llvm::sys::fs::make_absolute(Path);
    FilenameStrs.push_back(std::string(Path.begin(), Path.end()));
    FilenameRefs.push_back(FilenameStrs.back());
  }

  // Encode the filenames first.
  std::string EncodedDataBuf;
  llvm::raw_string_ostream OS(EncodedDataBuf);
  llvm::coverage::CoverageFilenamesSectionWriter(FilenameRefs).write(OS);
  size_t FilenamesSize = OS.str().size();
  size_t CurrentSize, PrevSize = FilenamesSize;

  // Now we need to build up the list of function records.
  auto *Int32Ty = llvm::Type::getInt32Ty(LLVMContext);
  auto *Int64Ty = llvm::Type::getInt64Ty(LLVMContext);
  auto *Int8PtrTy = llvm::Type::getInt8PtrTy(LLVMContext);
  auto *FunctionRecordTy = llvm::StructType::get(
      LLVMContext, {Int8PtrTy, Int32Ty, Int32Ty, Int64Ty});

  std::vector<llvm::Constant *> FunctionRecords;
  std::vector<CounterMappingRegion> Regions;
  for (const auto &M : Mappings) {
    unsigned FileID =
        std::find(Files.begin(), Files.end(), M.getFile()) - Files.begin();
    Regions.clear();
    for (const auto &MR : M.getMappedRegions())
      Regions.emplace_back(CounterMappingRegion::makeRegion(
          MR.Counter, /*FileID=*/0, MR.StartLine, MR.StartCol, MR.EndLine,
          MR.EndCol));
    // Append each function's regions into the encoded buffer.
    llvm::coverage::CoverageMappingWriter W({FileID}, M.getExpressions(),
                                            Regions);
    W.write(OS);

    auto *NameVal =
        llvm::ConstantDataArray::getString(LLVMContext, M.getName(), true);
    auto *NameVar =
        new llvm::GlobalVariable(*getModule(), NameVal->getType(), true,
                                 llvm::GlobalValue::LinkOnceAnyLinkage, NameVal,
                                 "__llvm_profile_name_" + M.getName());

    CurrentSize = OS.str().size();
    // Create a record for this function.
    llvm::Constant *FunctionRecordVals[] = {
        llvm::ConstantExpr::getBitCast(NameVar, Int8PtrTy),
        // TODO: We're including the null to match the profile, but we should
        // really skip the null in the profile instead.
        llvm::ConstantInt::get(Int32Ty, M.getName().size() + 1),
        llvm::ConstantInt::get(Int32Ty, CurrentSize - PrevSize),
        llvm::ConstantInt::get(Int64Ty, M.getHash())};
    FunctionRecords.push_back(llvm::ConstantStruct::get(
        FunctionRecordTy, makeArrayRef(FunctionRecordVals)));
    PrevSize = CurrentSize;
  }
  size_t CoverageMappingSize = PrevSize - FilenamesSize;

  // Append extra zeroes if necessary to ensure that the size of the filenames
  // and coverage mappings is a multiple of 8.
  if (size_t Rem = OS.str().size() % 8) {
    CoverageMappingSize += 8 - Rem;
    for (size_t I = 0, S = 8 - Rem; I < S; ++I)
      OS << '\0';
  }
  auto *EncodedData =
      llvm::ConstantDataArray::getString(LLVMContext, OS.str(), false);

  auto *RecordsTy =
      llvm::ArrayType::get(FunctionRecordTy, FunctionRecords.size());
  auto *RecordsVal = llvm::ConstantArray::get(RecordsTy, FunctionRecords);

  // Now we embed everything into a constant with a well-known name.
  auto *CovDataTy =
      llvm::StructType::get(LLVMContext, {Int32Ty, Int32Ty, Int32Ty, Int32Ty,
                                          RecordsTy, EncodedData->getType()});
  llvm::Constant *TUDataVals[] = {
      llvm::ConstantInt::get(Int32Ty, FunctionRecords.size()),
      llvm::ConstantInt::get(Int32Ty, FilenamesSize),
      llvm::ConstantInt::get(Int32Ty, CoverageMappingSize),
      llvm::ConstantInt::get(Int32Ty, llvm::coverage::CoverageMappingVersion1),
      RecordsVal, EncodedData};
  auto CovDataVal =
      llvm::ConstantStruct::get(CovDataTy, makeArrayRef(TUDataVals));
  auto CovData = new llvm::GlobalVariable(
      *getModule(), CovDataTy, true, llvm::GlobalValue::InternalLinkage,
      CovDataVal, "__llvm_coverage_mapping");

  addUsedGlobal(CovData);
}
