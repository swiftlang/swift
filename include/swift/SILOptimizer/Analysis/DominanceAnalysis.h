//===--- DominanceAnalysis.h - SIL Dominance Analysis -----------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_DOMINANCEANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_DOMINANCEANALYSIS_H

#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/Dominance.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
class SILModule;
class SILInstruction;

class DominanceAnalysis : public FunctionAnalysisBase<DominanceInfo> {
protected:
  virtual void verify(DominanceInfo *DI) const override {
    if (DI->getRoots().empty())
      return;
    DI->verify();
  }

public:
  DominanceAnalysis()
      : FunctionAnalysisBase<DominanceInfo>(SILAnalysisKind::Dominance) {}

  DominanceAnalysis(const DominanceAnalysis &) = delete;
  DominanceAnalysis &operator=(const DominanceAnalysis &) = delete;

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::Dominance;
  }

  DominanceInfo *newFunctionAnalysis(SILFunction *F) override {
    return new DominanceInfo(F);
  }

  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind K) override {
    return K & InvalidationKind::Branches;
  }
};

class PostDominanceAnalysis : public FunctionAnalysisBase<PostDominanceInfo> {
protected:
  virtual void verify(PostDominanceInfo *PDI) const override {
    if (PDI->getRoots().empty())
      return;
    PDI->verify();
  }

public:
  PostDominanceAnalysis()
      : FunctionAnalysisBase<PostDominanceInfo>(
            SILAnalysisKind::PostDominance) {}

  PostDominanceAnalysis(const PostDominanceAnalysis &) = delete;
  PostDominanceAnalysis &operator=(const PostDominanceAnalysis &) = delete;

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::PostDominance;
  }

  PostDominanceInfo *newFunctionAnalysis(SILFunction *F) override {
    return new PostDominanceInfo(F);
  }

  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind K) override {
    return K & InvalidationKind::Branches;
  }
};

} // end namespace swift



#endif
