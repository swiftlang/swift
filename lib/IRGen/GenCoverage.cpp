//===--- GenCoverage.cpp - IR Generation for coverage ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for the initialization of
//  coverage related variables.
//
//===----------------------------------------------------------------------===//

#include "IRGenModule.h"
#include "SwiftTargetInfo.h"

#include "swift/SIL/SILModule.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/ProfileData/InstrProf.h"
#include "llvm/ProfileData/Coverage/CoverageMappingWriter.h"
#include "llvm/Support/FileSystem.h"

using namespace swift;
using namespace irgen;

using llvm::coverage::CovMapVersion;
using llvm::coverage::CounterMappingRegion;

static std::string getCoverageSection(IRGenModule &IGM) {
  return llvm::getInstrProfSectionName(llvm::IPSK_covmap,
                                       IGM.Triple.getObjectFormat());
}

void IRGenModule::emitCoverageMapping() {
  std::vector<const SILCoverageMap *> Mappings;
  for (const auto &M : getSILModule().getCoverageMaps()) {
    // Check whether this coverage mapping can reference its name data within
    // the profile symbol table. If the name global is gone, this function has
    // been optimized out.
    StringRef PGOFuncName = M.second->getPGOFuncName();
    std::string PGOFuncNameVar = llvm::getPGOFuncNameVarName(
        PGOFuncName, llvm::GlobalValue::LinkOnceAnyLinkage);
    if (!Module.getNamedGlobal(PGOFuncNameVar))
      continue;
    Mappings.push_back(M.second);
  }

  // If there aren't any coverage maps, there's nothing to emit.
  if (Mappings.empty())
    return;

  std::vector<StringRef> Files;
  for (const auto &M : Mappings)
    if (std::find(Files.begin(), Files.end(), M->getFile()) == Files.end())
      Files.push_back(M->getFile());

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
  std::string FilenamesAndCoverageMappings;
  llvm::raw_string_ostream OS(FilenamesAndCoverageMappings);
  llvm::coverage::CoverageFilenamesSectionWriter(FilenameRefs).write(OS);
  size_t FilenamesSize = OS.str().size();
  size_t CurrentSize, PrevSize = FilenamesSize;

  // Now we need to build up the list of function records.
  llvm::LLVMContext &Ctx = LLVMContext;
  auto *Int32Ty = llvm::Type::getInt32Ty(Ctx);

  llvm::Type *FunctionRecordTypes[] = {
#define COVMAP_FUNC_RECORD(Type, LLVMType, Name, Init) LLVMType,
#include "llvm/ProfileData/InstrProfData.inc"
#undef COVMAP_FUNC_RECORD
  };

  auto FunctionRecordTy =
      llvm::StructType::get(Ctx, llvm::makeArrayRef(FunctionRecordTypes),
                            /*isPacked=*/true);

  std::vector<llvm::Constant *> FunctionRecords;
  std::vector<CounterMappingRegion> Regions;
  for (const auto &M : Mappings) {
    unsigned FileID =
        std::find(Files.begin(), Files.end(), M->getFile()) - Files.begin();
    Regions.clear();
    for (const auto &MR : M->getMappedRegions())
      Regions.emplace_back(CounterMappingRegion::makeRegion(
          MR.Counter, /*FileID=*/0, MR.StartLine, MR.StartCol, MR.EndLine,
          MR.EndCol));
    // Append each function's regions into the encoded buffer.
    ArrayRef<unsigned> VirtualFileMapping(FileID);
    llvm::coverage::CoverageMappingWriter W(VirtualFileMapping,
                                            M->getExpressions(), Regions);
    W.write(OS);

    CurrentSize = OS.str().size();
    unsigned MappingLen = CurrentSize - PrevSize;
    StringRef CoverageMapping(OS.str().c_str() + PrevSize, MappingLen);

    StringRef NameValue = M->getPGOFuncName();
    assert(!NameValue.empty() && "Expected a named record");
    uint64_t FuncHash = M->getHash();

    // Create a record for this function.
    llvm::Constant *FunctionRecordVals[] = {
#define COVMAP_FUNC_RECORD(Type, LLVMType, Name, Init) Init,
#include "llvm/ProfileData/InstrProfData.inc"
#undef COVMAP_FUNC_RECORD
    };

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
  auto *FilenamesAndMappingsVal =
      llvm::ConstantDataArray::getString(Ctx, OS.str(), false);

  auto *RecordsTy =
      llvm::ArrayType::get(FunctionRecordTy, FunctionRecords.size());
  auto *RecordsVal = llvm::ConstantArray::get(RecordsTy, FunctionRecords);

  // Create the coverage data header.
  llvm::Type *CovDataHeaderTypes[] = {
#define COVMAP_HEADER(Type, LLVMType, Name, Init) LLVMType,
#include "llvm/ProfileData/InstrProfData.inc"
#undef COVMAP_HEADER
  };
  auto *CovDataHeaderTy =
      llvm::StructType::get(Ctx, makeArrayRef(CovDataHeaderTypes));
  llvm::Constant *CovDataHeaderVals[] = {
#define COVMAP_HEADER(Type, LLVMType, Name, Init) Init,
#include "llvm/ProfileData/InstrProfData.inc"
#undef COVMAP_HEADER
  };
  auto *CovDataHeaderVal = llvm::ConstantStruct::get(
      CovDataHeaderTy, makeArrayRef(CovDataHeaderVals));

  // Combine the header, function records, and mappings together.
  llvm::Type *CovDataTypes[] = {CovDataHeaderTy, RecordsTy,
                                FilenamesAndMappingsVal->getType()};
  auto *CovDataTy = llvm::StructType::get(Ctx, makeArrayRef(CovDataTypes));
  llvm::Constant *TUDataVals[] = {CovDataHeaderVal, RecordsVal,
                                  FilenamesAndMappingsVal};
  auto *CovDataVal =
      llvm::ConstantStruct::get(CovDataTy, makeArrayRef(TUDataVals));

  auto CovData = new llvm::GlobalVariable(
      *getModule(), CovDataTy, true, llvm::GlobalValue::InternalLinkage,
      CovDataVal, llvm::getCoverageMappingVarName());
  std::string CovSection = getCoverageSection(*this);
  CovData->setSection(CovSection);
  CovData->setAlignment(8);
  addUsedGlobal(CovData);
}
