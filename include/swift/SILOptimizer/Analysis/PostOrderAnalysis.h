//===--- PostOrderAnalysis.h - SIL POT and RPOT Analysis --------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_POSTORDERANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_POSTORDERANALYSIS_H

#include "swift/Basic/Range.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/PostOrder.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/iterator_range.h"

namespace swift {

class SILBasicBlock;
class SILFunction;

/// This class is a simple wrapper around the POT iterator provided by LLVM. It
/// lazily re-evaluates the post order when it is invalidated so that we do not
/// reform the post order over and over again (it can be expensive).
class PostOrderAnalysis : public FunctionAnalysisBase<PostOrderFunctionInfo> {
  /// Incremented whenever a cached PostOrderFunctionInfo is freed, so that holders of one can
  /// detect that it is gone.
  uint64_t epoch = 0;

protected:
  virtual std::unique_ptr<PostOrderFunctionInfo>
  newFunctionAnalysis(SILFunction *F) override {
    return std::make_unique<PostOrderFunctionInfo>(F);
  }

  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind K) override {
    return K & InvalidationKind::Branches;
  }

public:
  PostOrderAnalysis()
      : FunctionAnalysisBase<PostOrderFunctionInfo>(
            SILAnalysisKind::PostOrder) {}

  // This is a cache and shouldn't be copied around.
  PostOrderAnalysis(const PostOrderAnalysis &) = delete;
  PostOrderAnalysis &operator=(const PostOrderAnalysis &) = delete;

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::PostOrder;
  }

  uint64_t getEpoch() const { return epoch; }

  virtual void invalidate() override {
    ++epoch;
    FunctionAnalysisBase::invalidate();
  }

  virtual void invalidate(SILFunction *F,
                          SILAnalysis::InvalidationKind K) override {
    if (shouldInvalidate(K) && hasFunctionInfo(F))
      ++epoch;
    FunctionAnalysisBase::invalidate(F, K);
  }

  virtual void notifyWillDeleteFunction(SILFunction *F) override {
    if (hasFunctionInfo(F))
      ++epoch;
    FunctionAnalysisBase::notifyWillDeleteFunction(F);
  }
};

} // end namespace swift

#endif
