//===--- ProgramTerminationAnalysis.h -------------------------------------===//
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
///
/// \file
///
/// This is an analysis which determines if a block is a "program terminating
/// block". Define a program terminating block is defined as follows:
///
/// 1. A block at whose end point according to the SIL model, the program must
/// end. An example of such a block is one that includes a call to fatalError.
/// 2. Any block that is joint post-dominated by program terminating blocks.
///
/// For now we only identify instances of 1. But the analysis could be extended
/// appropriately via simple dataflow or through the use of post-dominator
/// trees.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILANALYSIS_PROGRAMTERMINATIONANALYSIS_H
#define SWIFT_SILANALYSIS_PROGRAMTERMINATIONANALYSIS_H

#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace swift {

class ProgramTerminationFunctionInfo {
  llvm::SmallPtrSet<SILBasicBlock *, 4> ProgramTerminatingBlocks;

public:
  ProgramTerminationFunctionInfo(SILFunction *F) {
    for (auto &BB : *F) {
      if (!isARCInertTrapBB(&BB))
        continue;
      ProgramTerminatingBlocks.insert(&BB);
    }
  }

  bool isProgramTerminatingBlock(SILBasicBlock *BB) {
    return ProgramTerminatingBlocks.count(BB);
  }
};

class ProgramTerminationAnalysis
    : public FunctionAnalysisBase<ProgramTerminationFunctionInfo> {
public:
  ProgramTerminationAnalysis(SILModule *M)
      : FunctionAnalysisBase<ProgramTerminationFunctionInfo>(
            AnalysisKind::ProgramTermination) {}

  ProgramTerminationAnalysis(const ProgramTerminationAnalysis &) = delete;
  ProgramTerminationAnalysis &
  operator=(const ProgramTerminationAnalysis &) = delete;

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::ProgramTermination;
  }

  virtual void initialize(SILPassManager *PM) override {}

  virtual ProgramTerminationFunctionInfo *
  newFunctionAnalysis(SILFunction *F) override {
    return new ProgramTerminationFunctionInfo(F);
  }

  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind K) override {
    return K & InvalidationKind::Branches;
  }
};

} // end swift namespace

#endif
