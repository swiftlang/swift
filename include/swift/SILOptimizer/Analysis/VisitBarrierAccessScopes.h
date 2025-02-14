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
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallPtrSet.h"
#include <optional>

namespace swift {

class BeginAccessInst;
class EndAccessInst;
class SILInstruction;
class SILBasicBlock;
class SILInstruction;

/// Visits the begin_access instructions corresponding to access scopes that
/// must be regarded as barriers (in particular, their end_access instructions
/// must be) because they contain other barriers.
///
/// interface Effects {
///   typename Effect
///
///   /// The root instructions from which to walk.
///   iterable gens()
///
///   /// The gens which are killed within the block where they occur.
///   iterable localGens()
///
///   /// Whether the indicated instruction is killed within the specified
///   /// block.
///   bool isLocalGen(SILInstruction *)
///
///   /// The effect, if any, of the specified instruction.
///   Effects::Effect effectForPhi(SILBasicBlock *)
///
///   /// The effect, if any, of the phis of the specified block.
///   Effects::Effect effectForInstruction(SILInstruction *)
/// }
/// interface Visitor {
///   /// Whether the indicated basic block is within the region of the graph
///   /// that should be traversed.
///   bool isInRegion(SILBasicBlock *)
///
///   /// Visit each discovered begin_access instruction which defines a barrier
///   /// scope.
///   void visitBarrierAccessScope(BeginAccessInst *)
/// }
///
/// Implements SILCFGBackwardDFS::Visitor
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
  ///
  /// Finally, look through the instructions between local gens and their kills
  /// for further barrier access scopes.
  void visit() {
    // First, collect the gens whose blocks should be used as the roots of the
    // region--those that are non-local.
    //
    // Keep track of which gens are at the beginning of the region (i.e. none of
    // whose successors are in the region) so that we can avoid iterating over
    // the instructions below those gens.
    SmallVector<SILBasicBlock *, 32> rootBlocks;
    llvm::DenseMap<SILBasicBlock *, SILInstruction *> genForRoot;
    for (auto *instruction : effects.gens()) {
      if (effects.isLocalGen(instruction))
        continue;
      auto *block = instruction->getParent();
      rootBlocks.push_back(block);
      // If none of this block's successors are in the region, then we don't
      // need to visit the whole block--just the portion starting from this gen.
      if (!llvm::any_of(block->getSuccessorBlocks(),
                        [&](SILBasicBlock *successor) {
                          return visitor.isInRegion(successor);
                        })) {
        genForRoot[block] = instruction;
      }
    }
    // Then do a backward DFS from those roots, and visit the blocks in reverse
    // post-order.
    SILCFGBackwardDFS<This> dfs(*this, rootBlocks);
    for (auto *block : dfs.reversePostOrder()) {
      // Check whether this block contains a non-local gen.
      auto iterator = genForRoot.find(block);
      if (iterator != genForRoot.end()) {
        visitBlockFromGen(iterator->getSecond());
      } else {
        visitBlock(block);
      }
    }
    // Finally, visit local gens which weren't visited already.
    for (auto *instruction : effects.localGens()) {
      auto *block = instruction->getParent();
      auto isInRegion = dfs.cachedVisited && dfs.cachedVisited->contains(block);
      auto visitedFullBlock = genForRoot.find(block) == genForRoot.end();
      if (isInRegion && visitedFullBlock)
        continue;
      // This local gen is either entirely outside the blocks that define the
      // region (!isInRegion) or it is in one of the bottom (i.e. one none of
      // whose successors are in the region) blocks in the region only the top
      // of which was already visited.  Either way, the instructions between the
      // local gen and its kill have not yet been visited.  Visit them now.
      auto foundLocalKill = visitBlockFromGenThroughKill(instruction);
      assert(foundLocalKill && "local gen without local kill?!");
      (void)foundLocalKill;
    }
  }

private:
  /// Entry points for visiting: they visit increasingly large portions of a
  /// block.
  /// - visitBlockFromGenThroughKill: Instructions and phi through the first
  ///                                 kill.
  /// - visitBlockFromGen: Instructions, phi, and begin.
  /// - visitBlock: End, instructions, phi, and begin.

  /// Visit instructions and phis starting from the specified gen until a kill
  /// is found.
  bool visitBlockFromGenThroughKill(SILInstruction *from) {
    assert(effects.effectForInstruction(from) == Effects::Effect::Gen());
    for (auto *instruction = from; instruction;
         instruction = instruction->getPreviousInstruction()) {
      if (visitInstruction(instruction))
        return true;
    }
    auto *block = from->getParent();
    if (block->hasPhi()) {
      if (visitPhi(block))
        return true;
    }
    return false;
  }

  /// Visit a block from a non-local gen which begins the region.
  ///
  /// Avoids visiting the portion of the block occurring after an initial gen.
  void visitBlockFromGen(SILInstruction *from) {
    auto *block = from->getParent();

    assert(effects.effectForInstruction(from) == Effects::Effect::Gen());
    assert(!llvm::any_of(
        block->getSuccessorBlocks(),
        [&](SILBasicBlock *successor) { return visited.contains(successor); }));

    visited.insert(block);
    for (auto *instruction = from; instruction;
         instruction = instruction->getPreviousInstruction()) {
      if (visitInstruction(instruction)) {
        // New kills are incrementally added as access scopes are determined to
        // be barriers.  For this reason, gens may newly be discovered to be
        // local.  This can only happen when the kill which makes the gen local
        // ends an access scope (i.e. is an end_access).
        assert(isa<EndAccessInst>(instruction) &&
               "found preexisting local kill for initially-non-local gen?!");
        // Even so, the remainder of the block must still be visited.
      }
    }
    if (block->hasPhi()) {
      visitPhi(block);
    }
    visitBlockBegin(block);
  }

  /// Visit a block fully; its end, its body, its phi, and its begin.
  void visitBlock(SILBasicBlock *block) {
    visitBlockEnd(block);
    for (auto &instruction : llvm::reverse(*block)) {
      visitInstruction(&instruction);
    }
    if (block->hasPhi())
      visitPhi(block);
    visitBlockBegin(block);
  }

  /// Visit block components:
  /// - block end
  /// - instruction
  /// - phi
  /// - block begin

  /// Visit an instruction.  Returns whether it is a barrier.
  bool visitInstruction(SILInstruction *instruction) {
    if (auto *eai = dyn_cast<EndAccessInst>(instruction)) {
      runningLiveAccessScopes.insert(eai->getBeginAccess());
    } else if (auto *bai = dyn_cast<BeginAccessInst>(instruction)) {
      runningLiveAccessScopes.erase(bai);
    }
    return handleEffect(effects.effectForInstruction(instruction));
  }

  /// Visit a phi.  Returns whether it is a barrier.
  bool visitPhi(SILBasicBlock *block) {
    return handleEffect(effects.effectForPhi(block));
  }

  /// Visit a block begin.  If any access scopes are live, record them for use
  /// (unioning) when the end of a predecessor is visited.
  void visitBlockBegin(SILBasicBlock *block) {
    if (!runningLiveAccessScopes.empty()) {
      liveInAccessScopes[block] = runningLiveAccessScopes;
    }
  }

  /// Visit a block end.  Set the running access scopes for the block to be the
  /// union of all access scopes live at the begin of successor blocks.  Returns
  /// whether the block is a barrier.
  bool visitBlockEnd(SILBasicBlock *block) {
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
    // means EITHER that those predecessors aren't in the region OR that a
    // barrier was encountered when visiting some (iterative) successor of that
    // predecessor.  Either way, this block is a barrier block as a result.
    if (llvm::any_of(block->getSuccessorBlocks(), [&](SILBasicBlock *block) {
          return !visited.contains(block);
        })) {
      handleBarrier();
      return true;
    }

    return false;
  }

  /// Effect procecessing

  bool handleEffect(typename Effects::Effect effect) {
    switch (effect.value) {
    case Effects::Effect::Value::NoEffect:
      return false;
    case Effects::Effect::Value::Gen:
      runningLiveAccessScopes.clear();
      return false;
    case Effects::Effect::Value::Kill:
      handleBarrier();
      return true;
    }
  }

  void handleBarrier() {
    for (auto *scope : runningLiveAccessScopes) {
      visitor.visitBarrierAccessScope(scope);
    }
  }

  /// SILCFGBackwardDFS::Visitor
  friend struct SILCFGBackwardDFS<This>;

  /// Whether the block is in the region of interest.  Just a passhthrough to
  /// our visitor.
  bool isInRegion(SILBasicBlock *block) { return visitor.isInRegion(block); }
};

} // end namespace swift
