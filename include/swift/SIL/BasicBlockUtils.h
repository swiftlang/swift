//===--- BasicBlockUtils.h - Utilities for SILBasicBlock  -------*- C++ -*-===//
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

#ifndef SWIFT_SIL_DEADENDBLOCKS_H
#define SWIFT_SIL_DEADENDBLOCKS_H

#include "swift/SIL/SILValue.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class SILFunction;
class SILBasicBlock;
class TermInst;
class DominanceInfo;
class SILLoopInfo;

/// \brief Replace a branch target.
///
/// \param T The terminating instruction to modify.
/// \param edgeIdx The successor edges index that will be replaced.
/// \param newDest The new target block.
/// \param preserveArgs If set, preserve arguments on the replaced edge.
void changeBranchTarget(TermInst *T, unsigned edgeIdx, SILBasicBlock *newDest,
                        bool preserveArgs);

/// Returns the arguments values on the specified CFG edge. If necessary, may
/// add create new SILPHIArguments, using `NewEdgeBB` as the placeholder.
void getEdgeArgs(TermInst *T, unsigned edgeIdx, SILBasicBlock *newEdgeBB,
                 llvm::SmallVectorImpl<SILValue> &args);

/// \brief Splits the edge from terminator.
///
/// Also updates dominance and loop information if not null.
///
/// Returns the newly created basic block.
SILBasicBlock *splitEdge(TermInst *T, unsigned edgeIdx,
                         DominanceInfo *DT = nullptr,
                         SILLoopInfo *LI = nullptr);

/// \brief Merge a basic block ending in a branch with its successor
/// if possible.
void mergeBasicBlockWithSingleSuccessor(SILBasicBlock *BB,
                                        SILBasicBlock *succBB);

/// A utility for finding dead-end blocks.
///
/// Dead-end blocks are blocks from which there is no path to the function exit
/// (either return or throw). These are blocks which end with an unreachable
/// instruction and blocks from which all paths end in "unreachable" blocks.
/// This utility is needed to determine if the a value definition can have a
/// lack of users ignored along a specific path.
class DeadEndBlocks {
  llvm::SetVector<const SILBasicBlock *> ReachableBlocks;
  const SILFunction *F;
  bool isComputed = false;

  void compute();

public:
  DeadEndBlocks(const SILFunction *F) : F(F) {}

  /// Returns true if \p BB is a dead-end block.
  bool isDeadEnd(SILBasicBlock *BB) {
    if (!isComputed) {
      // Lazily compute the dataflow.
      compute();
      isComputed = true;
    }
    return ReachableBlocks.count(BB) == 0;
  }
};

} // namespace swift

#endif
