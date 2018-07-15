//===--- LoopAnalysis.h - SIL Loop Analysis ---------------------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_LOOPINFOANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_LOOPINFOANALYSIS_H

#include "swift/SIL/CFG.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
  class DominanceInfo;
  class SILLoop;
  class DominanceAnalysis;
}

namespace swift {

/// Computes natural loop information for SIL basic blocks.
class SILLoopAnalysis : public FunctionAnalysisBase<SILLoopInfo> {
  DominanceAnalysis *DA;
public:
  SILLoopAnalysis(SILModule *)
      : FunctionAnalysisBase(SILAnalysisKind::Loop), DA(nullptr) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::Loop;
  }

  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind K) override {
    return K & InvalidationKind::Branches;
  }

  // Computes loop information for the given function using dominance
  // information.
  virtual SILLoopInfo *newFunctionAnalysis(SILFunction *F) override;

  virtual void initialize(SILPassManager *PM) override;
};

} // end namespace swift

#endif
