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

#ifndef SWIFT_SIL_BASICBLOCKUTILS_H
#define SWIFT_SIL_BASICBLOCKUTILS_H

#include "swift/SIL/SILValue.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class SILFunction;
class SILBasicBlock;
class TermInst;
class DominanceInfo;
class SILLoopInfo;

/// Replace a branch target.
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

/// Splits the edge from terminator.
///
/// Also updates dominance and loop information if not null.
///
/// Returns the newly created basic block.
SILBasicBlock *splitEdge(TermInst *T, unsigned edgeIdx,
                         DominanceInfo *DT = nullptr,
                         SILLoopInfo *LI = nullptr);

/// Merge a basic block ending in a branch with its successor
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
  bool isDeadEnd(const SILBasicBlock *block) {
    if (!isComputed) {
      // Lazily compute the dataflow.
      compute();
      isComputed = true;
    }
    return ReachableBlocks.count(block) == 0;
  }

  bool empty() {
    if (!isComputed) {
      // Lazily compute the dataflow.
      compute();
      isComputed = true;
    }
    return ReachableBlocks.empty();
  }

  const SILFunction *getFunction() const { return F; }
};

/// A struct that contains the intermediate state used in computing
/// joint-dominance sets. Enables a pass to easily reuse the same small data
/// structures with clearing (noting that clearing our internal state does not
/// cause us to shrink meaning that once we malloc, we keep the malloced
/// memory).
struct JointPostDominanceSetComputer {
  /// The worklist that drives the algorithm.
  SmallVector<SILBasicBlock *, 32> worklist;

  /// A set that guards our worklist. Any block before it is added to worklist
  /// should be checked against visitedBlocks.
  SmallPtrSet<SILBasicBlock *, 32> visitedBlocks;

  /// The set of blocks where we begin our walk.
  SmallPtrSet<SILBasicBlock *, 8> initialBlocks;

  /// A subset of our initial blocks that we found as a predecessor of another
  /// block along our walk.
  SmallVector<SILBasicBlock *, 8> reachableInputBlocks;

  /// As we process the worklist, any successors that we see that have not been
  /// visited yet are placed in here. At the end of our worklist, any blocks
  /// that remain here are "leaking blocks" that together with our initial set
  /// would provide a jointly-postdominating set of our dominating value.
  SmallSetVector<SILBasicBlock *, 8> blocksThatLeakIfNeverVisited;

  DeadEndBlocks &deadEndBlocks;

  JointPostDominanceSetComputer(DeadEndBlocks &deadEndBlocks)
      : deadEndBlocks(deadEndBlocks) {}

  void clear() {
    worklist.clear();
    visitedBlocks.clear();
    initialBlocks.clear();
    reachableInputBlocks.clear();
    blocksThatLeakIfNeverVisited.clear();
  }

  /// Compute joint-postdominating set for \p dominatingBlock and \p
  /// dominatedBlockSet found by walking up the CFG from the latter to the
  /// former.
  ///
  /// We pass back the following information via callbacks so our callers can
  /// use whatever container they need to:
  ///
  /// * inputBlocksFoundDuringWalk: Any blocks from the "dominated
  ///   block set" that was found as a predecessor block during our traversal is
  ///   passed to this callback. These can occur for two reasons:
  ///
  ///   1. We actually had a block in \p dominatedBlockSet that was reachable
  ///      from another block in said set. This is a valid usage of the API
  ///      since it could be that the user does not care about such uses and
  ///      leave this callback empty.
  ///
  ///   2. We had a block in \p dominatedBlockSet that is in a sub-loop in the
  ///      loop-nest relative to \p dominatingBlock causing us to go around a
  ///      backedge and hit the block during our traversal. In this case, we
  ///      have already during the traversal passed the exiting blocks of the
  ///      sub-loop as joint postdominace completion set blocks. This is useful
  ///      if one is using this API for lifetime extension purposes of lifetime
  ///      ending uses and one needs to insert compensating copy_value at these
  ///      locations due to the lack of strong control-equivalence in between
  ///      the block and \p dominatingBlock.
  ///
  ///
  /// * foundJointPostDomSetCompletionBlocks: The set of blocks not in \p
  ///   dominatedBlockSet that together with \p dominatedBlockSet
  ///   jointly-postdominate \p dominatedBlock. This is "completing" the joint
  ///   post-dominance set.
  ///
  /// * inputBlocksInJointPostDomSet: Any of our input blocks that were never
  ///   found as a predecessor is passed to this callback. This block is in the
  ///   final minimal joint-postdominance set and is passed to this
  ///   callback. This is optional and we will avoid doing work if it is not
  ///   set.
  void findJointPostDominatingSet(
      SILBasicBlock *dominatingBlock,
      ArrayRef<SILBasicBlock *> dominatedBlockSet,
      function_ref<void(SILBasicBlock *)> inputBlocksFoundDuringWalk,
      function_ref<void(SILBasicBlock *)> foundJointPostDomSetCompletionBlocks,
      function_ref<void(SILBasicBlock *)> inputBlocksInJointPostDomSet = {});
};

} // namespace swift

#endif
