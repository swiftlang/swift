//===--- ColdBlockInfo.h - Fast/slow path analysis for SIL CFG --*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_COLDBLOCKS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_COLDBLOCKS_H

#include "DominanceAnalysis.h"

#include "swift/Basic/FixedBitSet.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
class DominanceAnalysis;
class PostDominanceAnalysis;
class SILBasicBlock;
class CondBranchInst;
class SILPassManager;
class ColdBlockAnalysis;

/// Cache a set of basic blocks that have been determined to be cold or hot.
class ColdBlockInfo {
public:
  // Represents the temperatures of edges flowing into a block.
  //
  //         T = "top" -- both warm and cold edges
  //        /  \
  //     Warm  Cold
  //        \  /
  //         B = "bottom" -- neither warm or cold edges
  struct State {
    // Each state kind, as an integer, is its position in any bit vectors.
    enum Temperature {
      Warm = 0,
      Cold = 1
    };

    // Number of states, excluding Top or Bottom, in this flow problem.
    static constexpr unsigned NumStates = 2;
  };

  using Energy = FixedBitSet<State::NumStates, State::Temperature>;

  ColdBlockInfo(DominanceAnalysis *DA, PostDominanceAnalysis *PDA);

  /// Is this basic block considered to be cold / rarely executed?
  bool isCold(const SILBasicBlock *BB) const;

  friend ColdBlockAnalysis;

private:
  DominanceAnalysis *DA;
  PostDominanceAnalysis *PDA;

  /// Each block in this map has been determined to be cold and/or warm.
  /// Make sure to always use the set/resetToCold methods to update this!
  llvm::DenseMap<const SILBasicBlock*, Energy> EnergyMap;

  void analyze(SILFunction *F);

  // This is a cache and shouldn't be copied around.
  ColdBlockInfo(const ColdBlockInfo &) = delete;
  ColdBlockInfo &operator=(const ColdBlockInfo &) = delete;
  LLVM_DUMP_METHOD void dump() const;

  /// Tracks whether any information was changed in the energy map.
  bool changedMap = false;
  void resetToCold(const SILBasicBlock *BB);
  void set(const SILBasicBlock *BB, State::Temperature temp);

  using ExpectedValue = std::optional<bool>;

  void setExpectedCondition(CondBranchInst *CondBranch, ExpectedValue value);

  ExpectedValue searchForExpectedValue(SILValue Cond,
                                       unsigned recursionDepth = 0);
  void searchForExpectedValue(CondBranchInst *CondBranch);

  bool inferFromEdgeProfile(SILBasicBlock *BB);
};

/// A SILAnalysis that caches ColdBlockInfo per function, invalidated on
/// control-flow changes. Depends on DominanceAnalysis and PostDominanceAnalysis.
class ColdBlockAnalysis : public FunctionAnalysisBase<ColdBlockInfo> {
  DominanceAnalysis *DA = nullptr;
  PostDominanceAnalysis *PDA = nullptr;

public:
  ColdBlockAnalysis(SILModule *)
      : FunctionAnalysisBase<ColdBlockInfo>(SILAnalysisKind::ColdBlock) {}

  ColdBlockAnalysis(const ColdBlockAnalysis &) = delete;
  ColdBlockAnalysis &operator=(const ColdBlockAnalysis &) = delete;

  static SILAnalysisKind getAnalysisKind() {
    return SILAnalysisKind::ColdBlock;
  }

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::ColdBlock;
  }

  virtual void initialize(SILPassManager *PM) override;

  std::unique_ptr<ColdBlockInfo> newFunctionAnalysis(SILFunction *F) override;

  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind K) override {
    return (K & InvalidationKind::BranchesAndInstructions) ||
           DA->shouldInvalidate(K) || PDA->shouldInvalidate(K);
  }
};
} // end namespace swift

#endif
