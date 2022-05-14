//===--- VisitBarrierAccessScopes.h - Find access scopes with barriers ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Given a region of a function, backwards-reachable from some set of roots, on
// which gen/kill effects can be determined, determines which access scopes must
// also be treated as kills in view of the rule that a kill within an access
// scope makes the access scope itself a kill.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace swift {

class BeginAccessInst;
class EndAccessInst;
class SILInstruction;
class SILBasicBlock;
class SILInstruction;

/// Visits the begin_access instructions that must be regarded as barriers
/// because they contain barriers.
///
/// interface Effects {
///   /// The effect, if any, of the specified instruction.
///   Effects::Effect effectForPhi(SILBasicBlock *)
///
///   /// The effect, if any, of the phis of the specified block.
///   Effects::Effect effectForInstruction(SILInstruction *)
/// }
/// interface Visitor {
///   /// Whether the indicated basic block is within the region of the graph
///   /// that should be traversed.
///   bool isInRegion(SILBasicBlock *block)
///
///   /// The roots from which all blocks in the region are backwards-reachable.
///   ArrayRef<SILBasicBlock *> roots();
///
///   /// Visit each discovered begin_access instruction which defines a barrier
///   /// scope.
///   void visitBarrierAccessScope(BeginAccessInst *)
/// }
template <typename Effects, typename Visitor>
class VisitBarrierAccessScopes {
  using This = VisitBarrierAccessScopes<Effects, Visitor>;
  /// Describes the effect, if any, of each instruction and phi.
  Effects &effects;
  /// Describes the search region and visits the found barriers.
  Visitor &visitor;
  /// The function in which to find barrier access scopes.
  SILFunction *function;
  /// The blocks which have already been visited.  Used to determine whether a
  /// block being visited is a barrier block.
  BasicBlockSet visited;

  /// The access scopes that are live at the begin of each block after visiting
  /// all the block's predecessors and its instructions.
  llvm::DenseMap<SILBasicBlock *, llvm::SmallPtrSet<BeginAccessInst *, 2>>
      liveInAccessScopes;
  /// While visiting a block's instructions, the access scopes that are
  /// currently live.
  llvm::SmallPtrSet<BeginAccessInst *, 2> runningLiveAccessScopes;
  /// The blocks in the region, in topological order.
  SmallVector<SILBasicBlock *, 32> order;

public:
  VisitBarrierAccessScopes(SILFunction *function, Effects &effects,
                           Visitor &visitor)
      : effects(effects), visitor(visitor), function(function),
        visited(function){};

  /// Visit the begin_access instructions.
  ///
  /// Sort the backwards-reachable, in-region blocks topologically.  Then visit
  /// them in the reverse of that order, so that when any block is visited, all
  /// of its non-backedge* successors will already have been visited.
  ///
  /// While visiting, which access scopes are live within blocks is tracked,
  /// storing the access scopes that are live at the begin of a block, and
  /// noting that every access scope live at the begin of any successor is also
  /// live at the end of a block.
  ///
  /// * These are backedges in the reversed graph, that are ignored in a reverse
  /// depth-first search.
  void visit() {
    visitSortedTopologicallyBackwards(function, visitor.roots(), *this);
    for (auto *block : llvm::reverse(order)) {
      visitBlock(block);
    }
  }

  /// visitSortedTopologicallyBackwards::Visitor

  /// Whether the block is in the region of interest.  Just a passhthrough to
  /// our visitor.
  bool isInRegion(SILBasicBlock *block) { return visitor.isInRegion(block); }

  /// Called for each block in the region in topological order.  Record the
  /// order into \p order so that we can visit the blocks in the reverse of that
  /// order.
  void visit(SILBasicBlock *block) { order.push_back(block); }

private:
  /// Block visitation

  void visitBlock(SILBasicBlock *block) {
    visitBlockEnd(block);
    for (auto &instruction : llvm::reverse(*block)) {
      visitInstruction(&instruction);
    }
    if (block->hasPhi())
      visitPhi(block);
    visitBlockBegin(block);
  }

  void visitInstruction(SILInstruction *instruction) {
    if (auto *eai = dyn_cast<EndAccessInst>(instruction)) {
      runningLiveAccessScopes.insert(eai->getBeginAccess());
    } else if (auto *bai = dyn_cast<BeginAccessInst>(instruction)) {
      runningLiveAccessScopes.erase(bai);
    }
    handleEffect(effects.effectForInstruction(instruction));
  }

  void visitPhi(SILBasicBlock *block) {
    handleEffect(effects.effectForPhi(block));
  }

  void visitBlockBegin(SILBasicBlock *block) {
    if (!runningLiveAccessScopes.empty()) {
      liveInAccessScopes[block] = runningLiveAccessScopes;
    }
  }

  void visitBlockEnd(SILBasicBlock *block) {
    visited.insert(block);
    runningLiveAccessScopes.clear();
    for (auto *successor : block->getSuccessorBlocks()) {
      auto iterator = liveInAccessScopes.find(successor);
      if (iterator != liveInAccessScopes.end()) {
        for (auto *bai : iterator->getSecond()) {
          runningLiveAccessScopes.insert(bai);
        }
      }
    }
    // If any of this block's predecessors haven't already been visited, it
    // means that they aren't in the region and consequently this block is a
    // barrier block.
    if (llvm::any_of(block->getSuccessorBlocks(), [&](SILBasicBlock *block) {
          return !visited.contains(block);
        }))
      handleBarrier();
  }

  /// Effect procecessing

  void handleEffect(typename Effects::Effect effect) {
    switch (effect.value) {
    case Effects::Effect::Value::NoEffect:
      return;
    case Effects::Effect::Value::Gen:
      runningLiveAccessScopes.clear();
      return;
    case Effects::Effect::Value::Kill:
      handleBarrier();
      return;
    }
  }

  void handleBarrier() {
    for (auto *scope : runningLiveAccessScopes) {
      visitor.visitBarrierAccessScope(scope);
    }
  }
};

} // end namespace swift
