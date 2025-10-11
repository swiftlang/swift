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

#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockData.h"
#include "swift/SIL/BasicBlockDatastructures.h"
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
  llvm::SetVector<const SILBasicBlock *> reachableBlocks;
  const SILFunction *f;
  bool didComputeValue = false;

  /// When non-null, indicates whether dead-end blocks are present
  /// in the current function.
  std::optional<bool> hasAnyDeadEnds = std::nullopt;

  void compute();

public:
  DeadEndBlocks(const SILFunction *f) : f(f) {}

  ~DeadEndBlocks();

  /// Returns true if \p BB is a dead-end block.
  bool isDeadEnd(const SILBasicBlock *block) {
    if (!didComputeValue) {
      // Lazily compute the dataflow.
      compute();
      didComputeValue = true;
    }
    return reachableBlocks.count(block) == 0;
  }

  /// Returns true iff none of the function's blocks is a dead-end.
  /// Note: The underlying value is lazily computed & cached.
  bool isEmpty() {
    if (!hasAnyDeadEnds.has_value()) {
      hasAnyDeadEnds = llvm::any_of(
          *f, [this](const SILBasicBlock &BB) { return isDeadEnd(&BB); });
    }

    return !hasAnyDeadEnds.value();
  }

  /// Return true if this dead end blocks has computed its internal cache yet.
  ///
  /// Used to determine if we need to verify a DeadEndBlocks.
  bool isComputed() const { return didComputeValue; }

  /// Add any (new) blocks that are backward-reachable from \p reachableBB to
  /// the set of reachable blocks.
  void updateForReachableBlock(SILBasicBlock *reachableBB);

  /// Add new blocks to the set of reachable blocks.
  void updateForNewBlock(SILBasicBlock *newBB);

  const SILFunction *getFunction() const { return f; }

  /// Performs a simple check if \p block (or its single successor) ends in an
  /// "unreachable".
  ///
  /// This handles the common case of failure-handling blocks, which e.g.
  /// contain a call to fatalError().
  static bool triviallyEndsInUnreachable(SILBasicBlock *block);

protected:
  void propagateNewlyReachableBlocks(unsigned startIdx);
};

/// A utility for detecting edges that enter a dead-end region.
///
/// A dead-end region is a strongly-connected component of the CFG
/// consisting solely of dead-end blocks (i.e. from which it is not
/// possible to reach a function exit). The strongly-connected
/// components of a CFG form a DAG: once control flow from the entry
/// block has entered an SCC, it cannot return to an earlier SCC
/// (because then by definition they would have to be the same SCC).
///
/// Note that the interior edges of a dead-end region do not *enter*
/// the region. Only edges from an earlier SCC count as edges into
/// the region.
///
/// For example, in this CFG:
///
///      /-> bb1 -> bb2 -> return
///   bb0
///      \-> bb3 -> bb4 -> bb5 -> unreachable
///           ^      |
///           \------/
///
/// The edge from bb0 to bb3 enters a new dead-end region, as does
/// the edge from bb4 to bb5. The edge from bb4 to bb3 does not
/// enter a new region because it is an internal edge of its region.
///
/// Edges that enter dead-end regions are special in SIL because certain
/// joint post-dominance rules are relaxed for them. For example, the
/// stack does need not be consistent on different edges into a dead-end
/// region.
class DeadEndEdges {
  enum : unsigned {
    /// A region data value which represents that a block is unreachable
    /// from the entry block.
    UnreachableRegionData = 0,

    /// A region data value which represents that a block is reachable
    /// from the entry block but not in a dead-end region.
    NonDeadEndRegionData = 1,

    /// A value that must be added to a region index when storing it in
    /// a region data.
    ///
    /// This should be the smallest number such that
    ///   (IndexOffset << IndexShift)
    /// is always greater than all of the special region-data values
    /// above.
    IndexOffset = 1,

    /// A mask which can be applied to a region to say that it contains
    /// a cycle. This slightly optimizes the check in isDeadEndEdge for
    /// the common case where regions do not have cycles.
    HasCycleMask = 0x1,

    /// The amount to shift the region index by when storing it in a
    /// region data.
    ///
    /// This should be the smallest number such that an arbitrary value
    /// left-shifted by it will not have any of the mask bits set.
    IndexShift = 1,
  };

  /// An integer representing what we know about the SCC partition that
  /// a particular block is in. All blocks in the same region store the
  /// same value to make comparisons faster.
  ///
  /// Either:
  /// - UnreachableRegionData, representing a block that cannot be
  ///   reached from the entry block;
  /// - NonDeadEndRegionData, representing a block that can be reached
  ///   from the entry block but is not in a dead-end region; or
  /// - an encoded region index, representing a block that is in a
  ///   dead-end region.
  ///
  /// A region index is a unique value in 0..<numDeadEndRegions,
  /// selected for a specific dead-end SCC. It is encoded by adding
  /// IndexOffset, left-shifting by IndexShift, and then or'ing
  /// in any appropriate summary bits like HasCycleMask.
  ///
  /// If regionDataForBlock isn't initialized, the function contains
  /// no dead-end blocks.
  std::optional<BasicBlockData<unsigned>> regionDataForBlock;

  /// The total number of dead-end regions in the function.
  unsigned numDeadEndRegions;

  static constexpr bool isDeadEndRegion(unsigned regionData) {
    return regionData >= (IndexOffset << IndexShift);
  }

  static unsigned getIndexFromRegionData(unsigned regionData) {
    assert(isDeadEndRegion(regionData));
    return (regionData >> IndexShift) - IndexOffset;
  }

public:
  /// Perform the analysis on the given function. An existing
  /// DeadEndBlocks analysis can be passed in to avoid needing to
  /// compute it anew.
  explicit DeadEndEdges(SILFunction *F,
                        DeadEndBlocks *deadEndBlocks = nullptr);

  /// Return the number of dead-end regions in the function.
  unsigned getNumDeadEndRegions() const {
    return numDeadEndRegions;
  }

  /// Does the given CFG edge enter a new dead-end region?
  ///
  /// If so, return the index of the dead-end region it enters.
  std::optional<unsigned>
  entersDeadEndRegion(SILBasicBlock *srcBB, SILBasicBlock *dstBB) const {
    // If we didn't initialize regionDataForBlock, there are no dead-end
    // edges at all.
    if (!regionDataForBlock)
      return std::nullopt;

    auto dstRegionData = (*regionDataForBlock)[dstBB];

    // If the destination block is not in a dead-end region, this is
    // not a dead-end edge.
    if (!isDeadEndRegion(dstRegionData)) return std::nullopt;

    unsigned dstRegionIndex = getIndexFromRegionData(dstRegionData);

    // If the destination block is in a region with no cycles, every edge
    // to it is a dead-end edge; no need to look up the source block's
    // region.
    if (!(dstRegionData & HasCycleMask)) return dstRegionIndex;

    // Otherwise, it's a dead-end edge if the source block is in a
    // different region. (That region may or may not be itself be a
    // dead-end region.)
    auto srcRegionData = (*regionDataForBlock)[srcBB];
    if (srcRegionData != dstRegionData) {
      return dstRegionIndex;
    } else {
      return std::nullopt;
    }
  }

  /// A helper class for tracking visits to edges into dead-end regions.
  ///
  /// The client is assumed to be doing a walk of the function which will
  /// naturally visit each edge exactly once. This set allows the client
  /// to track when they've processed every edge to a particular dead-end
  /// region and can therefore safely enter it.
  ///
  /// The set does not count edges from unreachable blocks by default. This
  /// matches the normal expectation that the client is doing a CFG search
  /// and won't try to visit edges from unreachable blocks. If you are
  /// walking the function in some other, e.g. by iterating the blocks,
  /// you must pass `true` for `includeUnreachableEdges`.
  class VisitingSet {
    const DeadEndEdges &edges;

    /// Stores the remaining number of edges for each dead-end region
    /// in the function.
    SmallVector<unsigned> remainingEdgesForRegion;

    friend class DeadEndEdges;
    explicit VisitingSet(const DeadEndEdges &parent,
                         bool includeUnreachableEdges);

  public:
    /// Record that a dead-end edge to the given block was visited.
    ///
    /// Returns true if this was the last dead-end edge to the region
    /// containing the block.
    ///
    /// Do not call this multiple times for the same edge. Do not
    /// call this for an unreachable edge if you did not create the
    /// set including unreachable edges.
    bool visitEdgeTo(SILBasicBlock *destBB) {
      assert(edges.regionDataForBlock &&
             "visiting dead-end edge in function that has none");
      auto destRegionData = (*edges.regionDataForBlock)[destBB];
      assert(isDeadEndRegion(destRegionData) &&
             "destination block is not in a dead-end region");

      auto destRegionIndex = getIndexFromRegionData(destRegionData);
      assert(remainingEdgesForRegion[destRegionIndex] > 0 &&
             "no remaining dead-end edges for region; visited "
             "multiple times?");

      auto numRemaining = --remainingEdgesForRegion[destRegionIndex];
      return numRemaining == 0;
    }

    /// Return true if all of the edges have been visited.
    bool visitedAllEdges() const {
      for (auto count : remainingEdgesForRegion) {
        if (count) return false;
      }
      return true;
    }
  };

  /// Create a counter set which can be used to count edges in the
  /// dead-end regions.
  ///
  /// By default, the set does not include edges from unreachable blocks.
  VisitingSet createVisitingSet(bool includeUnreachableEdges = false) const {
    return VisitingSet(*this, includeUnreachableEdges);
  }
};


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
///      sub-loop as joint postdominance completion set blocks. This is useful
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

#ifndef NDEBUG
bool checkDominates(SILBasicBlock *sourceBlock, SILBasicBlock *destBlock);
#endif

/// Walk depth-first the region backwards reachable from the provided roots
/// constrained by \p region's \p isInRegion member function.
///
/// interface Region {
///   /// Whether the indicated basic block is within the region of the graph
///   /// that should be traversed.
///   bool isInRegion(SILBasicBlock *)
/// }
template <typename Region>
struct SILCFGBackwardDFS {
  Region &region;
  ArrayRef<SILBasicBlock *> roots;
  std::optional<SmallVector<SILBasicBlock *, 16>> cachedPostOrder;
  std::optional<BasicBlockSet> cachedVisited;

  SILCFGBackwardDFS(Region &region, ArrayRef<SILBasicBlock *> roots)
      : region(region), roots(roots) {}

  /// Visit the blocks of the region in post-order.
  ///
  /// interface Visitor {
  ///     /// Visit each block in topological order.
  ///     void visit(SILBasicBlock *)
  /// }
  template <typename Visitor>
  void visitPostOrder(Visitor &visitor) {
    if (roots.empty())
      return;
    auto *function = roots.front()->getParent();
    cachedVisited.emplace(function);
    for (auto *root : roots) {
      SmallVector<std::pair<SILBasicBlock *, SILBasicBlock::pred_iterator>, 32>
          stack;
      if (!region.isInRegion(root))
        continue;
      stack.push_back({root, root->pred_begin()});
      while (!stack.empty()) {
        while (stack.back().second != stack.back().first->pred_end()) {
          auto predecessor = *stack.back().second;
          stack.back().second++;
          if (!region.isInRegion(predecessor))
            continue;
          if (cachedVisited->insert(predecessor))
            stack.push_back({predecessor, predecessor->pred_begin()});
        }
        visitor.visit(stack.back().first);
        stack.pop_back();
      }
    }
  }

  /// Visit the region in post-order and cache the visited blocks.
  void cachePostOrder() {
    if (cachedPostOrder)
      return;
    struct Visitor {
      SILCFGBackwardDFS<Region> &dfs;
      void visit(SILBasicBlock *block) {
        dfs.cachedPostOrder->push_back(block);
      }
    };
    cachedPostOrder.emplace();
    Visitor visitor{*this};
    visitPostOrder(visitor);
  }

  /// The region in post-order.
  ArrayRef<SILBasicBlock *> postOrder() {
    cachePostOrder();
    return *cachedPostOrder;
  };

  /// The region in reverse post-order.
  auto reversePostOrder() { return llvm::reverse(postOrder()); }
};
} // namespace swift

#endif
