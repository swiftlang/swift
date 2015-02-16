//===-- ColdBlocks.h - Fast/slow path analysis for the SIL CFG -*- C++ -*--===//
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

#ifndef SWIFT_SILANALYSIS_COLDBLOCKS_H
#define SWIFT_SILANALYSIS_COLDBLOCKS_H

#include "llvm/ADT/DenseMap.h"

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

public:
  ColdBlockInfo(DominanceAnalysis *DA): DA(DA) {}

  static bool isSlowPath(const SILBasicBlock *FromBB, const SILBasicBlock *ToBB);

  bool isCold(const SILBasicBlock *BB);
};
} // end namespace swift

#endif
