//===--- GenCoverage.cpp - IR Generation for coverage ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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

static bool isMachO(IRGenModule &IGM) {
  return SwiftTargetInfo::get(IGM).OutputObjectFormat == llvm::Triple::MachO;
}

static StringRef getCoverageSection(IRGenModule &IGM) {
  return llvm::getInstrProfCoverageSectionName(isMachO(IGM));
}

void IRGenModule::emitCoverageMapping() {
  const auto &Mappings = getSILModule().getCoverageMapList();
  // If there aren't any coverage maps, there's nothing to emit.
  if (Mappings.empty())
    return;

  std::vector<StringRef> Files;
  for (const auto &M : Mappings)
    if (std::find(Files.begin(), Files.end(), M.getFile()) == Files.end())
      Files.push_back(M.getFile());

  llvm::SmallVector<std::string, 8> FilenameStrs;
  for (StringRef Name : Files) {
    llvm::SmallString<256> Path(Name);
    llvm::sys::fs::make_absolute(Path);
    FilenameStrs.push_back(std::string(Path.begin(), Path.end()));
  }

  // Now we need to build up the list of function records.
  llvm::LLVMContext &Ctx = LLVMContext;
  auto *Int32Ty = llvm::Type::getInt32Ty(Ctx);
  auto *Int8PtrTy = llvm::Type::getInt8PtrTy(Ctx);

  llvm::Type *FunctionRecordTypes[] = {
#define COVMAP_FUNC_RECORD(Type, LLVMType, Name, Init) LLVMType,
#include "llvm/ProfileData/InstrProfData.inc"
#undef COVMAP_FUNC_RECORD
  };

  auto FunctionRecordTy =
      llvm::StructType::get(Ctx, llvm::makeArrayRef(FunctionRecordTypes),
                            /*isPacked=*/true);

  std::vector<llvm::Constant *> FunctionNames;
  std::vector<llvm::Constant *> FunctionRecords;
  std::vector<CounterMappingRegion> Regions;
  std::vector<std::string> CoverageMappings;
  for (const auto &M : Mappings) {
    unsigned FileID =
        std::find(Files.begin(), Files.end(), M.getFile()) - Files.begin();
    Regions.clear();
    for (const auto &MR : M.getMappedRegions())
      Regions.emplace_back(CounterMappingRegion::makeRegion(
          MR.Counter, /*FileID=*/0, MR.StartLine, MR.StartCol, MR.EndLine,
          MR.EndCol));

    std::string CoverageMapping;
    llvm::raw_string_ostream OS(CoverageMapping);
    llvm::coverage::CoverageMappingWriter W({FileID}, M.getExpressions(),
                                            Regions);
    W.write(OS);
    OS.flush();

    std::string NameValue = llvm::getPGOFuncName(
        M.getName(),
        M.isPossiblyUsedExternally() ? llvm::GlobalValue::ExternalLinkage
                                     : llvm::GlobalValue::PrivateLinkage,
        M.getFile());
    llvm::GlobalVariable *NamePtr = llvm::createPGOFuncNameVar(
        *getModule(), llvm::GlobalValue::LinkOnceAnyLinkage, NameValue);
    FunctionNames.push_back(llvm::ConstantExpr::getBitCast(NamePtr, Int8PtrTy));

    uint64_t FuncHash = M.getHash();

    // Create a record for this function.
    llvm::Constant *FunctionRecordVals[] = {
#define COVMAP_FUNC_RECORD(Type, LLVMType, Name, Init) Init,
#include "llvm/ProfileData/InstrProfData.inc"
    };

    FunctionRecords.push_back(llvm::ConstantStruct::get(
        FunctionRecordTy, makeArrayRef(FunctionRecordVals)));
    CoverageMappings.push_back(CoverageMapping);
  }

  size_t FilenamesSize;
  size_t CoverageMappingSize;
  llvm::Expected<std::string> CoverageDataOrErr =
      llvm::coverage::encodeFilenamesAndRawMappings(
          FilenameStrs, CoverageMappings, FilenamesSize, CoverageMappingSize);
  if (llvm::Error E = CoverageDataOrErr.takeError()) {
    llvm::handleAllErrors(std::move(E), [](llvm::ErrorInfoBase &EI) {
      llvm::report_fatal_error(EI.message());
    });
  }
  std::string CoverageData = std::move(CoverageDataOrErr.get());
  auto *FilenamesAndMappingsVal =
      llvm::ConstantDataArray::getString(Ctx, CoverageData, false);

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
  CovData->setSection(getCoverageSection(*this));
  CovData->setAlignment(8);
  addUsedGlobal(CovData);

  if (!FunctionNames.empty()) {
    auto *NamesArrTy = llvm::ArrayType::get(Int8PtrTy, FunctionNames.size());
    auto *NamesArrVal = llvm::ConstantArray::get(NamesArrTy, FunctionNames);
    new llvm::GlobalVariable(*getModule(), NamesArrTy, true,
                             llvm::GlobalValue::InternalLinkage, NamesArrVal,
                             llvm::getCoverageUnusedNamesVarName());
  }
}
