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
/// particular SSA value. The client initializes liveness with a set of
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
/// determine the boundary's intruction position by searching for the last use.
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
/// Initial State:
///
///  -----
/// | Def | [LiveOut]
///  -----
///    |
///  -----
/// |     | [Dead]
///  -----
///    |
///  -----
/// |     | [Dead]
///  -----
///
/// State after updateForUse:
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
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_PRUNEDLIVENESS_H
#define SWIFT_SILOPTIMIZER_UTILS_PRUNEDLIVENESS_H

#include "swift/SIL/SILBasicBlock.h"

#ifdef NDEBUG
#define SWIFT_ASSERT_ONLY_MEMBER(X)
#define SWIFT_ASSERT_ONLY(X) do { } while (false)
#else
#define SWIFT_ASSERT_ONLY_MEMBER(X) X
#define SWIFT_ASSERT_ONLY(X) do { X; } while (false)
#endif

namespace swift {

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
  // Map all blocks in which current def is live to a flag indicating whether
  // the value is also liveout of the block.
  llvm::SmallDenseMap<SILBasicBlock *, bool, 4> liveBlocks;

  // Once the first use has been seen, no definitions can be added.
  SWIFT_ASSERT_ONLY_MEMBER(bool seenUse = false);

public:
  bool empty() const { return liveBlocks.empty(); }

  void clear() { liveBlocks.clear(); SWIFT_ASSERT_ONLY(seenUse = false); }

  void initializeDefBlock(SILBasicBlock *defBB) {
    assert(!seenUse && "cannot initialize more defs with partial liveness");
    markBlockLive(defBB, LiveWithin);
  }

  /// Update this liveness result for a single use.
  IsLive updateForUse(Operand *use);

  IsLive getBlockLiveness(SILBasicBlock *bb) const {
    auto liveBlockIter = liveBlocks.find(bb);
    if (liveBlockIter == liveBlocks.end())
      return Dead;
    return liveBlockIter->second ? LiveOut : LiveWithin;
  }

protected:
  void markBlockLive(SILBasicBlock *bb, IsLive isLive) {
    assert(isLive != Dead && "erasing live blocks isn't implemented.");
    liveBlocks[bb] = (isLive == LiveOut);
  }

  void computeUseBlockLiveness(SILBasicBlock *userBB);
};

/// PrunedLiveness tracks PrunedLiveBlocks along with "interesting" use
/// points. The set of interesting uses is a superset of all uses on the
/// liveness boundary. Filtering out uses that are obviously not on the liveness
/// boundary improves efficiency over tracking all uses. Additionally, all
/// interesting uses that are "lifetime-ending" are flagged. These uses must be
/// on the liveness boundary by their nature, regardless of any other uses. It
/// is up to the client to determine which uses are lifetime-ending. In OSSA,
/// the lifetime-ending property might be detemined by
/// OwnershipConstraint::isLifetimeEnding(). In non-OSSA, it might be determined
/// by deallocation.
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
  llvm::SmallDenseMap<SILInstruction *, bool, 8> users;

public:
  bool empty() const {
    assert(!liveBlocks.empty() || users.empty());
    return liveBlocks.empty();
  }

  void clear() {
    liveBlocks.clear();
    users.clear();
  }

  void initializeDefBlock(SILBasicBlock *defBB) {
    liveBlocks.initializeDefBlock(defBB);
  }

  void updateForUse(Operand *use, bool lifetimeEnding);

  PrunedLiveBlocks::IsLive getBlockLiveness(SILBasicBlock *bb) const {
    return liveBlocks.getBlockLiveness(bb);
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
};

} // namespace swift

#endif
