//===--- PrunedLiveness.hpp - Compute liveness from selected uses ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Incrementally compute and represent basic block liveness of a single live
/// range. The live range is defined by points in the CFG, independent of any
/// particular SSA value; however, it must be contiguous. Unlike traditional
/// variable liveness, a definition within the live range does not create a
/// "hole" in the live range. The client initializes liveness with a set of
/// definition blocks, typically a single block. The client then incrementally
/// updates liveness by providing a set of "interesting" uses one at a time.
///
/// This supports discovery of pruned liveness during control flow traversal. It
/// is not tied to a single SSA value and allows the client to select
/// interesting uses while ignoring other uses.
///
/// The PrunedLiveBlocks result maps each block to its current liveness state:
/// Dead, LiveWithin, LiveOut.
///
/// A LiveWithin block has a liveness boundary within the block. The client can
/// determine the boundary's instruction position by searching for the last use.
///
/// LiveOut indicates that liveness extends into a successor edges, therefore,
/// no uses within that block can be on the liveness boundary, unless that use
/// occurs before a def in the same block.
///
/// All blocks are initially assumed Dead. Initializing a definition block marks
/// that block LiveWithin. Each time an interesting use is discovered, blocks
/// liveness may undergo one of these transitions:
///
/// - Dead -> LiveWithin
/// - Dead -> LiveOut
/// - LiveWithin -> LiveOut
///
/// Example 1. Local liveness.
///
///  -----
/// |     | [Dead]
///  -----
///    |
///  -----
/// | Def | [LiveWithin]
/// | Use |
///  -----
///    |
///  -----
/// |     | [Dead]
///  -----
///
/// Example 2. Cross-block liveness.
///
/// Initial State after initializing a def block:
///
///  -----
/// | Def | [LiveWithin]
///  -----
///    |
///  -----
/// |     | [Dead]
///  -----
///    |
///  -----
/// | Use | [Dead]
///  -----
///
/// Later state after updateForUse is applied to the use:
///
///  -----
/// | Def | [LiveOut]
///  -----
///    |
///  -----
/// |     | [LiveOut]
///  -----
///    |
///  -----
/// | Use | [LiveWithin]
///  -----
///
///
/// An invariant is that for any liveness region, the post-dominating blocks of
/// the region are the LiveWithin regions.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_PRUNEDLIVENESS_H
#define SWIFT_SILOPTIMIZER_UTILS_PRUNEDLIVENESS_H

#include "swift/SIL/SILBasicBlock.h"
#include "llvm/ADT/MapVector.h"

namespace swift {

class DeadEndBlocks;

/// Discover "pruned" liveness for an arbitrary set of uses. The client builds
/// liveness by first initializing "def" blocks, then incrementally feeding uses
/// to updateForUse().
///
/// For SSA live ranges, a single "def" block will dominate all uses. If no def
/// block is provided, liveness is computed as if defined by a function
/// argument. If the client does not provide a single, dominating def block,
/// then the client must at least ensure that no uses precede the first
/// definition in a def block. Since this analysis does not remember the
/// positions of defs, it assumes that, within a block, uses follow
/// defs. Breaking this assumption will result in a "hole" in the live range in
/// which the def block's predecessors incorrectly remain dead. This situation
/// could be handled by adding an updateForUseBeforeFirstDef() API.
///
/// We allow for multiple bits of liveness information to be tracked by
/// internally using a SmallBitVector. We default to only tracking a single
/// bit. The multiple bit tracking is useful when tracking state for multiple
/// fields of the same root value.
///
/// TODO: This can be made space-efficient if all clients can maintain a block
/// numbering so liveness info can be represented as bitsets across the blocks.
class PrunedLiveBlocks {
public:
  /// Per-block liveness state computed during backward dataflow propagation.
  /// All unvisited blocks are considered Dead. As the are visited, blocks
  /// transition through these states in one direction:
  ///
  /// Dead -> LiveWithin -> LiveOut
  ///
  /// Dead blocks are either outside of the def's pruned liveness region, or
  /// they have not yet been discovered by the liveness computation.
  ///
  /// LiveWithin blocks have at least one use and/or def within the block, but
  /// are not (yet) LiveOut.
  ///
  /// LiveOut blocks are live on at least one successor path. LiveOut blocks may
  /// or may not contain defs or uses.
  enum IsLive { Dead, LiveWithin, LiveOut };

private:
  /// Map all blocks in which current def is live to a SmallBitVector indicating
  /// whether the value represented by said bit is also liveout of the block.
  llvm::SmallDenseMap<SILBasicBlock *, SmallBitVector, 4> liveBlocks;

  /// Number of bits of liveness to track. By default 1. Used to track multiple
  /// liveness bits.
  unsigned numBitsToTrack;

  /// Optional vector of live blocks for clients that deterministically iterate.
  SmallVectorImpl<SILBasicBlock *> *discoveredBlocks;

  /// Once the first use has been seen, no definitions can be added.
  SWIFT_ASSERT_ONLY_DECL(bool seenUse = false);

public:
  PrunedLiveBlocks(unsigned numBitsToTrack,
                   SmallVectorImpl<SILBasicBlock *> *discoveredBlocks = nullptr)
      : numBitsToTrack(numBitsToTrack), discoveredBlocks(discoveredBlocks) {
    assert(!discoveredBlocks || discoveredBlocks->empty());
  }

  bool empty() const { return liveBlocks.empty(); }

  void clear() {
    liveBlocks.clear();
    SWIFT_ASSERT_ONLY(seenUse = false);
  }

  unsigned numLiveBlocks() const { return liveBlocks.size(); }

  /// If the constructor was provided with a vector to populate, then this
  /// returns the list of all live blocks with no duplicates.
  ArrayRef<SILBasicBlock *> getDiscoveredBlocks() const {
    return *discoveredBlocks;
  }

  void initializeDefBlock(SILBasicBlock *defBB, unsigned bitNo) {
    markBlockLive(defBB, LiveWithin, bitNo);
  }

  /// Update this liveness result for a single use.
  IsLive updateForUse(SILInstruction *user, unsigned bitNo);

  IsLive getBlockLiveness(SILBasicBlock *bb, unsigned bitNo) const {
    auto liveBlockIter = liveBlocks.find(bb);
    if (liveBlockIter == liveBlocks.end())
      return Dead;
    assert(liveBlockIter->second.size() == 1);
    return liveBlockIter->second[bitNo] ? LiveOut : LiveWithin;
  }

protected:
  void markBlockLive(SILBasicBlock *bb, IsLive isLive, unsigned bitNo) {
    assert(isLive != Dead && "erasing live blocks isn't implemented.");
    bool isLiveOut = (isLive == LiveOut);
    auto iterAndInserted =
        liveBlocks.insert(std::make_pair(bb, SmallBitVector(numBitsToTrack)));
    if (iterAndInserted.second) {
      iterAndInserted.first->getSecond()[bitNo] = isLiveOut;
      if (discoveredBlocks)
        discoveredBlocks->push_back(bb);
    } else if (isLiveOut) {
      // Update the existing entry to be live-out.
      iterAndInserted.first->getSecond()[bitNo] = true;
    }
  }

  void computeUseBlockLiveness(SILBasicBlock *userBB, unsigned bitNo);
};

/// PrunedLiveness tracks PrunedLiveBlocks along with "interesting" use
/// points. The set of interesting uses is a superset of all uses on the
/// liveness boundary. Filtering out uses that are obviously not on the liveness
/// boundary improves efficiency over tracking all uses.
///
/// Additionally, all interesting uses that are potentially "lifetime-ending"
/// are flagged. These instruction are included as interesting use points, even
/// if they don't occur on the liveness boundary. Lifetime-ending uses that end
/// up on the final liveness boundary may be used to end the lifetime. It is up
/// to the client to determine which uses are potentially lifetime-ending. In
/// OSSA, the lifetime-ending property might be determined by
/// OwnershipConstraint::isLifetimeEnding(). In non-OSSA, it might be determined
/// by deallocation. If a lifetime-ending use ends up within the liveness
/// boundary, then it is up to the client to figure out how to "extend" the
/// lifetime beyond those uses.
///
/// Note: unlike OwnershipLiveRange, this represents a lifetime in terms of the
/// CFG boundary rather that the use set, and, because it is "pruned", it only
/// includes liveness generated by select uses. For example, it does not
/// necessarily include liveness up to destroy_value or end_borrow
/// instructions.
class PrunedLiveness {
  PrunedLiveBlocks liveBlocks;

  // Map all "interesting" user instructions in this def's live range to a flag
  // indicating whether they must end the lifetime.
  //
  // Lifetime-ending users are always on the boundary so are always interesting.
  //
  // Non-lifetime-ending uses within a LiveWithin block are interesting because
  // they may be the last use in the block.
  //
  // Non-lifetime-ending within a LiveOut block are uninteresting.
  llvm::SmallMapVector<SILInstruction *, bool, 8> users;

  /// A side array that stores any non lifetime ending uses we find in live out
  /// blocks. This is used to enable our callers to emit errors on non-lifetime
  /// ending uses that extend liveness into a loop body.
  SmallSetVector<SILInstruction *, 8> *nonLifetimeEndingUsesInLiveOut;

private:
  bool isWithinBoundaryHelper(SILInstruction *inst, SILValue def) const;

  bool areUsesWithinBoundaryHelper(ArrayRef<Operand *> uses, SILValue def,
                                   DeadEndBlocks *deadEndBlocks) const;

  bool areUsesOutsideBoundaryHelper(ArrayRef<Operand *> uses, SILValue def,
                                    DeadEndBlocks *deadEndBlocks) const;

public:
  PrunedLiveness(SmallVectorImpl<SILBasicBlock *> *discoveredBlocks = nullptr,
                 SmallSetVector<SILInstruction *, 8>
                     *nonLifetimeEndingUsesInLiveOut = nullptr)
      : liveBlocks(1 /*num bits*/, discoveredBlocks),
        nonLifetimeEndingUsesInLiveOut(nonLifetimeEndingUsesInLiveOut) {}

  bool empty() const {
    assert(!liveBlocks.empty() || users.empty());
    return liveBlocks.empty();
  }

  void clear() {
    liveBlocks.clear();
    users.clear();
    if (nonLifetimeEndingUsesInLiveOut)
      nonLifetimeEndingUsesInLiveOut->clear();
  }

  unsigned numLiveBlocks() const { return liveBlocks.numLiveBlocks(); }

  /// If the constructor was provided with a vector to populate, then this
  /// returns the list of all live blocks with no duplicates.
  ArrayRef<SILBasicBlock *> getDiscoveredBlocks() const {
    return liveBlocks.getDiscoveredBlocks();
  }

  using NonLifetimeEndingUsesInLiveOutRange =
      iterator_range<SILInstruction *const *>;

  NonLifetimeEndingUsesInLiveOutRange
  getNonLifetimeEndingUsesInLiveOut() const {
    assert(nonLifetimeEndingUsesInLiveOut &&
           "Called without passing in nonLifetimeEndingUsesInLiveOut to "
           "constructor?!");
    return llvm::make_range(nonLifetimeEndingUsesInLiveOut->begin(),
                            nonLifetimeEndingUsesInLiveOut->end());
  }

  using NonLifetimeEndingUsesInLiveOutBlocksRange =
      TransformRange<NonLifetimeEndingUsesInLiveOutRange,
                     function_ref<SILBasicBlock *(const SILInstruction *&)>>;
  NonLifetimeEndingUsesInLiveOutBlocksRange
  getNonLifetimeEndingUsesInLiveOutBlocks() const {
    function_ref<SILBasicBlock *(const SILInstruction *&)> op;
    op = [](const SILInstruction *&ptr) -> SILBasicBlock * {
      return ptr->getParent();
    };
    return NonLifetimeEndingUsesInLiveOutBlocksRange(
        getNonLifetimeEndingUsesInLiveOut(), op);
  }

  using UserRange = iterator_range<const std::pair<SILInstruction *, bool> *>;
  UserRange getAllUsers() const {
    return llvm::make_range(users.begin(), users.end());
  }

  using UserBlockRange = TransformRange<
      UserRange,
      function_ref<SILBasicBlock *(const std::pair<SILInstruction *, bool> &)>>;
  UserBlockRange getAllUserBlocks() const {
    function_ref<SILBasicBlock *(const std::pair<SILInstruction *, bool> &)> op;
    op = [](const std::pair<SILInstruction *, bool> &pair) -> SILBasicBlock * {
      return pair.first->getParent();
    };
    return UserBlockRange(getAllUsers(), op);
  }

  void initializeDefBlock(SILBasicBlock *defBB) {
    liveBlocks.initializeDefBlock(defBB, 0);
  }

  /// For flexibility, \p lifetimeEnding is provided by the
  /// caller. PrunedLiveness makes no assumptions about the def-use
  /// relationships that generate liveness. For example, use->isLifetimeEnding()
  /// cannot distinguish the end of the borrow scope that defines this extended
  /// live range vs. a nested borrow scope within the extended live range.
  void updateForUse(SILInstruction *user, bool lifetimeEnding);

  /// Updates the liveness for a whole borrow scope, beginning at \p op.
  /// Returns false if this cannot be done.
  bool updateForBorrowingOperand(Operand *op);

  /// Update this liveness to extend across the given liveness.
  void extendAcrossLiveness(PrunedLiveness &otherLiveness);

  PrunedLiveBlocks::IsLive getBlockLiveness(SILBasicBlock *bb) const {
    return liveBlocks.getBlockLiveness(bb, 0);
  }

  enum IsInterestingUser { NonUser, NonLifetimeEndingUse, LifetimeEndingUse };

  /// Return a result indicating whether the given user was identified as an
  /// interesting use of the current def and whether it ends the lifetime.
  IsInterestingUser isInterestingUser(SILInstruction *user) const {
    auto useIter = users.find(user);
    if (useIter == users.end())
      return NonUser;
    return useIter->second ? LifetimeEndingUse : NonLifetimeEndingUse;
  }

  /// Return true if \p inst occurs before the liveness boundary. Used when the
  /// client already knows that inst occurs after the start of liveness.
  bool isWithinBoundary(SILInstruction *inst) const;

  /// \p deadEndBlocks is optional.
  bool areUsesWithinBoundary(ArrayRef<Operand *> uses,
                             DeadEndBlocks *deadEndBlocks) const;

  /// \p deadEndBlocks is optional.
  bool areUsesOutsideBoundary(ArrayRef<Operand *> uses,
                              DeadEndBlocks *deadEndBlocks) const;

  /// PrunedLiveness utilities can be used with multiple defs. This api can be
  /// used to check if \p inst occurs in between the definition \p def and the
  /// liveness boundary.
  // This api varies from isWithinBoundary(SILInstruction *inst) which cannot
  // distinguish when \p inst is a use before definition in the same block as
  // the definition.
  bool isWithinBoundaryOfDef(SILInstruction *inst, SILValue def) const;

  /// Returns true when all \p uses are between \p def and the liveness boundary
  /// \p deadEndBlocks is optional.
  bool areUsesWithinBoundaryOfDef(ArrayRef<Operand *> uses, SILValue def,
                                  DeadEndBlocks *deadEndBlocks) const;

  /// Returns true if any of the \p uses are before the \p def or after the
  /// liveness boundary
  /// \p deadEndBlocks is optional.
  bool areUsesOutsideBoundaryOfDef(ArrayRef<Operand *> uses, SILValue def,
                                   DeadEndBlocks *deadEndBlocks) const;

  /// Compute liveness for a single SSA definition.
  void computeSSALiveness(SILValue def);
};

/// Record the last use points and CFG edges that form the boundary of
/// PrunedLiveness.
struct PrunedLivenessBoundary {
  SmallVector<SILInstruction *, 8> lastUsers;
  SmallVector<SILBasicBlock *, 8> boundaryEdges;

  void clear() {
    lastUsers.clear();
    boundaryEdges.clear();
  }

  /// Visit the point at which a lifetime-ending instruction must be inserted,
  /// excluding dead-end blocks. This is only useful when it is known that none
  /// of the lastUsers ends the lifetime, for example when creating a new borrow
  /// scope to enclose all uses.
  void visitInsertionPoints(
      llvm::function_ref<void(SILBasicBlock::iterator insertPt)> visitor,
      DeadEndBlocks *deBlocks = nullptr);

  /// Compute the boundary from the blocks discovered during liveness analysis.
  ///
  /// Precondition: \p liveness.getDiscoveredBlocks() is a valid list of all
  /// live blocks with no duplicates.
  ///
  /// The computed boundary will completely post-dominate, including dead end
  /// paths. The client should query DeadEndBlocks to ignore those dead end
  /// paths.
  void compute(const PrunedLiveness &liveness);

  /// Compute the boundary from a backward CFG traversal from a known set of
  /// jointly post-dominating blocks. Avoids the need to record an ordered list
  /// of live blocks during liveness analysis. It's ok if postDomBlocks has
  /// duplicates or extraneous blocks, as long as they jointly post-dominate all
  /// live blocks that aren't on dead-end paths.
  ///
  /// If the jointly post-dominating destroys do not include dead end paths,
  /// then any uses on those paths will not be included in the boundary. The
  /// resulting partial boundary will have holes along those paths. The dead end
  /// successors of blocks in this live set on are not necessarily identified
  /// by DeadEndBlocks.
  void compute(const PrunedLiveness &liveness,
               ArrayRef<SILBasicBlock *> postDomBlocks);
};

} // namespace swift

#endif
