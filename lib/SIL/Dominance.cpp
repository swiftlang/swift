//===--- Dominance.cpp - SIL basic block dominance analysis ---------------===//
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

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallVector.h"
#include "swift/SIL/Dominance.h"

using namespace swift;

// This algorithm is from the paper "A Simple, Fast Dominance
// Algorithm" by Cooper, Harvey, and Kennedy.


typedef llvm::DenseMap<SILBasicBlock*, SILBasicBlock*> ImmediateDominatorMap;

namespace {
  /// The index of a basic block in the post-order.  Higher indexes
  /// come later in the post-order and are therefore prone to dominate
  /// earlier indexes.
  enum PostOrderIndex : unsigned {
    Invalid = ~0U
  };

  class DominanceAnalysis {
    SmallVector<SILBasicBlock*, 16> PostOrder;
    llvm::DenseMap<SILBasicBlock*, PostOrderIndex> BlockToPostOrderIndex;

    /// The immediate-dominators map.  This essentially forms the
    /// current best-known dominance tree, although not every node
    /// will appear in it.
    SmallVector<PostOrderIndex, 16> ImmediateDominators;

  public:
    DominanceAnalysis(SILFunction *F);

    void fillMap(ImmediateDominatorMap &map);

  private:
    /// Return the post-order index of the given basic block.
    /// Requires buildPostOrder to have run first.
    PostOrderIndex getPostOrderIndex(SILBasicBlock *BB) {
      return BlockToPostOrderIndex.find(BB)->second;
    }

    /// Build PostOrder and BlockToPostOrderIndex.
    void buildPostOrder(SILBasicBlock *BB);

    /// Compute the immediate dominators, making ImmediateDominators valid.
    void computeImmediateDominators();

    /// Find the common dominator of the given two blocks according to
    /// the current dominance tree.
    PostOrderIndex getCommonDominator(PostOrderIndex l, PostOrderIndex r);
  };
}

DominanceAnalysis::DominanceAnalysis(SILFunction *F) {
  // First build the post-order traversal sequence.
  buildPostOrder(&*F->begin());

  // Then compute the immediate-dominators map.
  computeImmediateDominators();
}

void DominanceAnalysis::buildPostOrder(SILBasicBlock *BB) {
  // We use BlockToPostOrderIndex as a visited set.

  // Invariant: we've either already put BB in the visited set or
  // it's the entry block (which cannot have edges to it and
  // therefore does not need to be put in the visited set at start).
  assert(BlockToPostOrderIndex.count(BB) ||
         BB == &*BB->getParent()->begin());

  // Walk to each of the successors.
  for (SILBasicBlock *succ : BB->getSuccs()) {
    // Do nothing if we've already added the successor to the
    // visited set.  Note that DenseMap::insert silently ignores
    // the value if there's already an entry there.
    auto newEntry = std::make_pair(succ, PostOrderIndex::Invalid);
    if (!BlockToPostOrderIndex.insert(newEntry).second)
      continue;

    // Otherwise, recurse on it.
    buildPostOrder(succ);
  }

  // Add this block to the post-order traversal and record its
  // insertion index.
  BlockToPostOrderIndex[BB] = PostOrderIndex(PostOrder.size());
  PostOrder.push_back(BB);
}

void DominanceAnalysis::computeImmediateDominators() {
  // Initialize the immediate-dominator map to 'invalid' everywhere
  // except the entry node.
  ImmediateDominators.resize(PostOrder.size(), PostOrderIndex::Invalid);
  ImmediateDominators.back() = PostOrderIndex(PostOrder.size() - 1);

  // Repeat the following until convergence:
  bool changed;
  do {
    changed = false;

    // Walk the nodes in reverse post-order, not including the entry
    // node.  By walking in reverse post-order, we tend to process
    // dominating nodes first.
    for (unsigned i = PostOrder.size() - 1; i != 0; --i) {
      SILBasicBlock *BB = PostOrder[i-1];

      // Okay, find the best common dominator of all the predecessors
      // which have an immediate dominator.  This algorithm looks a
      // little weird using structured loops.

      // Find the first predecessor with a meaningful immediate dominator.
      for (auto pi = BB->pred_begin(), pe = BB->pred_end(); pi != pe; ++pi) {
        PostOrderIndex firstIndex = getPostOrderIndex(*pi);
        if (ImmediateDominators[firstIndex] == PostOrderIndex::Invalid)
          continue;

        // Alright, now look at all the others, intersecting each time
        // we find a meaningful immediate dominator.
        PostOrderIndex bestIndex = firstIndex;
        for (++pi; pi != pe; ++pi) {
          PostOrderIndex otherIndex = getPostOrderIndex(*pi);
          if (ImmediateDominators[otherIndex] == PostOrderIndex::Invalid)
            continue;

          bestIndex = getCommonDominator(bestIndex, otherIndex);
        }

        // Check whether this is different from the entry we've
        // already recorded.
        PostOrderIndex bbIndex = getPostOrderIndex(BB);
        if (ImmediateDominators[bbIndex] != bestIndex) {
          ImmediateDominators[bbIndex] = bestIndex;
          changed = true;
        }
        assert(pi == pe);
      }
    }
  } while (changed);
}

/// Given two nodes that both have a valid (if not necessarily wholly
/// accurate as yet) dominance ancestry, compute their current common
/// dominance ancestor.
PostOrderIndex
DominanceAnalysis::getCommonDominator(PostOrderIndex l, PostOrderIndex r) {
  assert(ImmediateDominators[l] != PostOrderIndex::Invalid);
  assert(ImmediateDominators[r] != PostOrderIndex::Invalid);
  while (l != r) {
    while (l < r) l = ImmediateDominators[l];
    while (r < l) r = ImmediateDominators[r];
  }
  assert(l == r);
  return l;
}

/// Fill in the given dominator map.
void DominanceAnalysis::fillMap(ImmediateDominatorMap &map) {
  // Note that we don't add an entry for the entry block.
  for (size_t i = 0, e = PostOrder.size() - 1; i != e; ++i) {
    map.insert(std::make_pair(PostOrder[i], PostOrder[ImmediateDominators[i]]));
  }
}

/// Compute the immmediate-dominators map.
DominanceInfo::DominanceInfo(SILFunction *F) {
  // There is nothing to do if there is exactly one basic block.
  if (llvm::next(F->begin()) == F->end()) return;

  DominanceAnalysis analysis(F);
  analysis.fillMap(ImmediateDominators);
}

bool DominanceInfo::dominates(SILInstruction *a, SILInstruction *b) const {
  auto aBlock = a->getParent(), bBlock = b->getParent();

  // If the blocks are different, it's as easy as whether A's block
  // dominates B's block.
  if (aBlock != bBlock)
    return dominates(a->getParent(), b->getParent());

  // Otherwise, they're in the same block, and we just need to check
  // whether B comes after A.  This is a non-strict computation.
  do {
    b = b->getPrevNode();
    if (a == b) return true;
  } while (b != nullptr);

  return false;
}

bool DominanceInfo::dominates(SILBasicBlock *a, SILBasicBlock *b) const {
  // Fast path.
  if (a == b) return true;

  auto it = DominanceCache.find(DominanceCacheKey(a,b));
  if (it != DominanceCache.end()) return it->second;

  // Just walk up the immediate-dominator chain from B.  If we run out
  // of blocks before we find A, A doesn't dominate B.
  auto cur = b;
  while ((cur = getImmediateDominator(cur)))
    if (cur == a)
      return true;

  return false;
}

