//===--- DeadEndBlocksAnalysis.h ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_DEADENDBLOCKSANALYSIS_H
#define SWIFT_SILOPTIMIZER_DEADENDBLOCKSANALYSIS_H

#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"

namespace swift {

class DeadEndBlocksAnalysis final : public FunctionAnalysisBase<DeadEndBlocks> {
public:
  DeadEndBlocksAnalysis()
      : FunctionAnalysisBase<DeadEndBlocks>(SILAnalysisKind::DeadEndBlocks) {}

  DeadEndBlocksAnalysis(const DeadEndBlocksAnalysis &) = delete;
  DeadEndBlocksAnalysis &operator=(const DeadEndBlocksAnalysis &) = delete;

  static SILAnalysisKind getAnalysisKind() {
    return SILAnalysisKind::DeadEndBlocks;
  }

  static bool classof(const SILAnalysis *s) {
    return s->getKind() == SILAnalysisKind::DeadEndBlocks;
  }

  std::unique_ptr<DeadEndBlocks> newFunctionAnalysis(SILFunction *f) override {
    return std::make_unique<DeadEndBlocks>(f);
  }

  bool shouldInvalidate(SILAnalysis::InvalidationKind k) override {
    return k & InvalidationKind::Branches;
  }

protected:
  void verify(DeadEndBlocks *deBlocks) const override;
};

} // namespace swift

#endif
