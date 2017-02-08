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

#include "llvm/ADT/DenseMap.h"
#include "swift/SIL/SILValue.h"

namespace swift {
class DominanceAnalysis;
class SILBasicBlock;

/// Cache a set of basic blocks that have been determined to be cold or hot.
///
/// This does not inherit from SILAnalysis because it is not worth preserving
/// across passes.
class ColdBlockInfo {
  DominanceAnalysis *DA;

  /// Each block in this map has been determined to be either cold or hot.
  llvm::DenseMap<const SILBasicBlock*, bool> ColdBlockMap;

  // This is a cache and shouldn't be copied around.
  ColdBlockInfo(const ColdBlockInfo &) = delete;
  ColdBlockInfo &operator=(const ColdBlockInfo &) = delete;

  /// Tri-value return code for checking branch hints.
  enum BranchHint : unsigned {
    None,
    LikelyTrue,
    LikelyFalse
  };

  enum {
    RecursionDepthLimit = 3
  };

  BranchHint getBranchHint(SILValue Cond, int recursionDepth);

  bool isSlowPath(const SILBasicBlock *FromBB, const SILBasicBlock *ToBB,
                  int recursionDepth);

  bool isCold(const SILBasicBlock *BB,
              int recursionDepth);

public:
  ColdBlockInfo(DominanceAnalysis *DA): DA(DA) {}

  bool isSlowPath(const SILBasicBlock *FromBB, const SILBasicBlock *ToBB) {
    return isSlowPath(FromBB, ToBB, 0);
  }

  bool isCold(const SILBasicBlock *BB) { return isCold(BB, 0); }
};
} // end namespace swift

#endif
