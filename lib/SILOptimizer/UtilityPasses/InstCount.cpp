//===--- InstCount.cpp - Collects the count of all instructions -----------===//
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
// This pass collects the count of all instructions and reports them
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-instcount"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                 Statistics
//===----------------------------------------------------------------------===//

// Local aggregate statistics
STATISTIC(TotalInsts, "Number of instructions (of all types) in non-external "
                      "functions");
STATISTIC(TotalBlocks, "Number of basic blocks in non-external functions");
STATISTIC(TotalFuncs , "Number of non-external functions");

// External aggregate statistics
STATISTIC(TotalExternalFuncInsts, "Number of instructions (of all types) in "
                                  "external functions");
STATISTIC(TotalExternalFuncBlocks, "Number of basic blocks in external "
                                   "functions");
STATISTIC(TotalExternalFuncDefs, "Number of external funcs definitions");
STATISTIC(TotalExternalFuncDecls, "Number of external funcs declarations");

// Linkage statistics
STATISTIC(TotalPublicFuncs, "Number of public funcs");
STATISTIC(TotalPublicNonABIFuncs, "Number of public non-ABI funcs");
STATISTIC(TotalPackageFuncs, "Number of package funcs");
STATISTIC(TotalPackageNonABIFuncs, "Number of package non-ABI funcs");
STATISTIC(TotalHiddenFuncs, "Number of hidden funcs");
STATISTIC(TotalPrivateFuncs, "Number of private funcs");
STATISTIC(TotalSharedFuncs, "Number of shared funcs");
STATISTIC(TotalPublicExternalFuncs, "Number of public external funcs");
STATISTIC(TotalPackageExternalFuncs, "Number of package external funcs");
STATISTIC(TotalHiddenExternalFuncs, "Number of hidden external funcs");

// Individual instruction statistics
#define INST(Id, Parent) \
  STATISTIC(Num##Id, "Number of " #Id);
#include "swift/SIL/SILNodes.def"

namespace {

struct InstCountVisitor : SILInstructionVisitor<InstCountVisitor> {
  // We store these locally so that we do not continually check if the function
  // is external or not. Instead, we just check once at the end and accumulate.
  unsigned InstCount = 0;
  unsigned BlockCount = 0;

  void visitSILBasicBlock(SILBasicBlock *BB) {
    ++BlockCount;
    SILInstructionVisitor<InstCountVisitor>::visitSILBasicBlock(BB);
  }

  void visitSILFunction(SILFunction *F) {
    SILInstructionVisitor<InstCountVisitor>::visitSILFunction(F);
  }

  void visitValueBase(ValueBase *V) { }

#define INST(Id, Parent)                                                       \
  void visit##Id(Id *I) {                                                      \
    ++Num##Id;                                                                 \
    ++InstCount;                                                               \
  }
#include "swift/SIL/SILNodes.def"

};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

namespace {
class InstCount : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() override {
    SILFunction *F = getFunction();
    InstCountVisitor V;
    V.visitSILFunction(F);
    if (F->isAvailableExternally()) {
      if (F->isDefinition()) {
        TotalExternalFuncInsts += V.InstCount;
        TotalExternalFuncBlocks += V.BlockCount;
        ++TotalExternalFuncDefs;
      } else {
        ++TotalExternalFuncDecls;
      }
    } else {
      TotalInsts += V.InstCount;
      TotalBlocks += V.BlockCount;
      ++TotalFuncs;
    }

    switch (F->getLinkage()) {
    case SILLinkage::Public:
      ++TotalPublicFuncs;
      break;
    case SILLinkage::PublicNonABI:
      ++TotalPublicNonABIFuncs;
      break;
    case SILLinkage::Package:
      ++TotalPackageFuncs;
      break;
    case SILLinkage::PackageNonABI:
      ++TotalPackageNonABIFuncs;
      break;
    case SILLinkage::Hidden:
      ++TotalHiddenFuncs;
      break;
    case SILLinkage::Shared:
      ++TotalSharedFuncs;
      break;
    case SILLinkage::Private:
      ++TotalPrivateFuncs;
      break;
    case SILLinkage::PublicExternal:
      ++TotalPublicExternalFuncs;
      break;
    case SILLinkage::PackageExternal:
      ++TotalPackageExternalFuncs;
      break;
    case SILLinkage::HiddenExternal:
      ++TotalHiddenExternalFuncs;
      break;
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createInstCount() {
  return new InstCount();
}

void swift::performSILInstCountIfNeeded(SILModule *M) {
  if (!M->getOptions().PrintInstCounts)
    return;
  executePassPipelinePlan(
      M, SILPassPipelinePlan::getInstCountPassPipeline(M->getOptions()));
}
