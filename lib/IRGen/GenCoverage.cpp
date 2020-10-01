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

#include "swift/AST/IRGenOptions.h"
#include "swift/SIL/SILModule.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/ProfileData/Coverage/CoverageMappingWriter.h"
#include "llvm/ProfileData/InstrProf.h"
#include "llvm/Support/FileSystem.h"

// This selects the coverage mapping format defined when `InstrProfData.inc`
// is textually included.
#define COVMAP_V3

using namespace swift;
using namespace irgen;

using llvm::coverage::CounterMappingRegion;
using llvm::coverage::CovMapVersion;

static std::string getInstrProfSection(IRGenModule &IGM,
                                       llvm::InstrProfSectKind SK) {
  return llvm::getInstrProfSectionName(SK, IGM.Triple.getObjectFormat());
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

  auto remapper = getOptions().CoveragePrefixMap;
  // Awkwardly munge absolute filenames into a vector of StringRefs.
  llvm::SmallVector<std::string, 8> FilenameStrs;
  llvm::SmallVector<StringRef, 8> FilenameRefs;
  for (StringRef Name : Files) {
    llvm::SmallString<256> Path(Name);
    llvm::sys::fs::make_absolute(Path);
    FilenameStrs.push_back(remapper.remapPath(Path));
    FilenameRefs.push_back(FilenameStrs.back());
  }

  // Encode the filenames.
  std::string Filenames;
  llvm::LLVMContext &Ctx = getLLVMContext();
  {
    llvm::raw_string_ostream OS(Filenames);
    llvm::coverage::CoverageFilenamesSectionWriter(FilenameRefs).write(OS);
  }
  auto *FilenamesVal =
      llvm::ConstantDataArray::getString(Ctx, Filenames, false);
  const int64_t FilenamesRef = llvm::IndexedInstrProf::ComputeHash(Filenames);
  const size_t FilenamesSize = Filenames.size();

  // Emit the function records.
  auto *Int32Ty = llvm::Type::getInt32Ty(Ctx);
  for (const auto &M : Mappings) {
    StringRef NameValue = M->getPGOFuncName();
    assert(!NameValue.empty() && "Expected a named record");
    uint64_t FuncHash = M->getHash();

    const uint64_t NameHash = llvm::IndexedInstrProf::ComputeHash(NameValue);
    std::string FuncRecordName = "__covrec_" + llvm::utohexstr(NameHash);

    unsigned FileID =
        std::find(Files.begin(), Files.end(), M->getFile()) - Files.begin();
    std::vector<CounterMappingRegion> Regions;
    for (const auto &MR : M->getMappedRegions())
      Regions.emplace_back(CounterMappingRegion::makeRegion(
          MR.Counter, /*FileID=*/0, MR.StartLine, MR.StartCol, MR.EndLine,
          MR.EndCol));
    // Append each function's regions into the encoded buffer.
    ArrayRef<unsigned> VirtualFileMapping(FileID);
    llvm::coverage::CoverageMappingWriter W(VirtualFileMapping,
                                            M->getExpressions(), Regions);
    std::string CoverageMapping;
    {
      llvm::raw_string_ostream OS(CoverageMapping);
      W.write(OS);
    }

#define COVMAP_FUNC_RECORD(Type, LLVMType, Name, Init) LLVMType,
    llvm::Type *FunctionRecordTypes[] = {
#include "llvm/ProfileData/InstrProfData.inc"
    };
    auto *FunctionRecordTy =
        llvm::StructType::get(Ctx, makeArrayRef(FunctionRecordTypes),
                              /*isPacked=*/true);

    // Create the function record constant.
#define COVMAP_FUNC_RECORD(Type, LLVMType, Name, Init) Init,
    llvm::Constant *FunctionRecordVals[] = {
#include "llvm/ProfileData/InstrProfData.inc"
    };
    auto *FuncRecordConstant = llvm::ConstantStruct::get(
        FunctionRecordTy, makeArrayRef(FunctionRecordVals));

    // Create the function record global.
    auto *FuncRecord = new llvm::GlobalVariable(
        *getModule(), FunctionRecordTy, /*isConstant=*/true,
        llvm::GlobalValue::LinkOnceODRLinkage, FuncRecordConstant,
        FuncRecordName);
    FuncRecord->setVisibility(llvm::GlobalValue::HiddenVisibility);
    FuncRecord->setSection(getInstrProfSection(*this, llvm::IPSK_covfun));
    FuncRecord->setAlignment(llvm::Align(8));
    if (Triple.supportsCOMDAT())
      FuncRecord->setComdat(getModule()->getOrInsertComdat(FuncRecordName));

    // Make sure the data doesn't get deleted.
    addUsedGlobal(FuncRecord);
  }

  // Create the coverage data header.
  const unsigned NRecords = 0;
  const unsigned CoverageMappingSize = 0;
  llvm::Type *CovDataHeaderTypes[] = {
#define COVMAP_HEADER(Type, LLVMType, Name, Init) LLVMType,
#include "llvm/ProfileData/InstrProfData.inc"
  };
  auto CovDataHeaderTy =
      llvm::StructType::get(Ctx, makeArrayRef(CovDataHeaderTypes));
  llvm::Constant *CovDataHeaderVals[] = {
#define COVMAP_HEADER(Type, LLVMType, Name, Init) Init,
#include "llvm/ProfileData/InstrProfData.inc"
  };
  auto CovDataHeaderVal = llvm::ConstantStruct::get(
      CovDataHeaderTy, makeArrayRef(CovDataHeaderVals));

  // Create the coverage data record
  llvm::Type *CovDataTypes[] = {CovDataHeaderTy, FilenamesVal->getType()};
  auto CovDataTy = llvm::StructType::get(Ctx, makeArrayRef(CovDataTypes));
  llvm::Constant *TUDataVals[] = {CovDataHeaderVal, FilenamesVal};
  auto CovDataVal =
      llvm::ConstantStruct::get(CovDataTy, makeArrayRef(TUDataVals));
  auto CovData = new llvm::GlobalVariable(
      *getModule(), CovDataTy, true, llvm::GlobalValue::PrivateLinkage,
      CovDataVal, llvm::getCoverageMappingVarName());

  CovData->setSection(getInstrProfSection(*this, llvm::IPSK_covmap));
  CovData->setAlignment(llvm::Align(8));
  addUsedGlobal(CovData);
}
