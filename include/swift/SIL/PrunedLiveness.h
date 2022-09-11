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

#include "swift/AST/TypeExpansionContext.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"

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
/// internally using a SmallBitVector. The multiple bit tracking is useful when
/// tracking state for multiple fields of the same root value. To do this, we
/// actually track 2 bits per actual needed bit so we can represent 3 Dead,
/// LiveOut, LiveWithin. This was previously unnecessary since we could just
/// represent dead by not having liveness state for a block. With multiple bits
/// possible this is no longer true.
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
  ///
  /// NOTE: The values below for Dead, LiveWithin, LiveOut were picked to ensure
  /// that given a 2 bit representation of the value, a value is Dead if the
  /// first bit is 0 and is LiveOut if the second bit is set.
  enum IsLive {
    Dead = 0,
    LiveWithin = 1,
    LiveOut = 3,
  };

  /// A bit vector that stores information about liveness. This is composed
  /// with SmallBitVector since it contains two bits per liveness so that it
  /// can represent 3 states, Dead, LiveWithin, LiveOut. We take advantage of
  /// their numeric values to make testing easier \see documentation on IsLive.
  class LivenessSmallBitVector {
    SmallBitVector bits;

  public:
    LivenessSmallBitVector() : bits() {}

    void init(unsigned numBits) {
      assert(bits.size() == 0);
      assert(numBits != 0);
      bits.resize(numBits * 2);
    }

    unsigned size() const { return bits.size() / 2; }

    IsLive getLiveness(unsigned bitNo) const {
      SmallVector<IsLive, 1> foundLiveness;
      getLiveness(bitNo, bitNo + 1, foundLiveness);
      return foundLiveness[0];
    }

    void getLiveness(unsigned startBitNo, unsigned endBitNo,
                     SmallVectorImpl<IsLive> &resultingFoundLiveness) const {
      unsigned actualStartBitNo = startBitNo * 2;
      unsigned actualEndBitNo = endBitNo * 2;

      // NOTE: We pad both before/after with Dead to ensure that we are
      // returning an array that acts as a bit mask and thus can be directly
      // compared against other such bitmasks. This invariant is used when
      // computing boundaries.
      for (unsigned i = 0; i != startBitNo; ++i) {
        resultingFoundLiveness.push_back(Dead);
      }
      for (unsigned i = actualStartBitNo, e = actualEndBitNo; i != e; i += 2) {
        if (!bits[i]) {
          resultingFoundLiveness.push_back(Dead);
          continue;
        }

        resultingFoundLiveness.push_back(bits[i + 1] ? LiveOut : LiveWithin);
      }
      for (unsigned i = endBitNo, e = size(); i != e; ++i) {
        resultingFoundLiveness.push_back(Dead);
      }
    }

    void setLiveness(unsigned startBitNo, unsigned endBitNo, IsLive isLive) {
      for (unsigned i = startBitNo * 2, e = endBitNo * 2; i != e; i += 2) {
        bits[i] = isLive & 1;
        bits[i + 1] = isLive & 2;
      }
    }

    void setLiveness(unsigned bitNo, IsLive isLive) {
      setLiveness(bitNo, bitNo + 1, isLive);
    }
  };

private:
  /// Map all blocks in which current def is live to a SmallBitVector indicating
  /// whether the value represented by said bit is also liveout of the block.
  llvm::SmallDenseMap<SILBasicBlock *, LivenessSmallBitVector, 4> liveBlocks;

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

  unsigned getNumBitsToTrack() const { return numBitsToTrack; }

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
    markBlockLive(defBB, bitNo, LiveWithin);
  }

  void initializeDefBlock(SILBasicBlock *defBB, unsigned startBitNo,
                          unsigned endBitNo) {
    markBlockLive(defBB, startBitNo, endBitNo, LiveWithin);
  }

  /// Update this liveness result for a single use.
  IsLive updateForUse(SILInstruction *user, unsigned bitNo) {
    SmallVector<IsLive, 1> resultingLiveness;
    updateForUse(user, bitNo, bitNo + 1, resultingLiveness);
    return resultingLiveness[0];
  }

  /// Update this range of liveness results for a single use.
  void updateForUse(SILInstruction *user, unsigned startBitNo,
                    unsigned endBitNo,
                    SmallVectorImpl<IsLive> &resultingLiveness);

  IsLive getBlockLiveness(SILBasicBlock *bb, unsigned bitNo) const {
    SmallVector<IsLive, 1> isLive;
    getBlockLiveness(bb, bitNo, bitNo + 1, isLive);
    return isLive[0];
  }

  void getBlockLiveness(SILBasicBlock *bb, unsigned startBitNo,
                        unsigned endBitNo,
                        SmallVectorImpl<IsLive> &foundLivenessInfo) const {
    auto liveBlockIter = liveBlocks.find(bb);
    if (liveBlockIter == liveBlocks.end()) {
      for (unsigned i : range(numBitsToTrack)) {
        (void)i;
        foundLivenessInfo.push_back(Dead);
      }
      return;
    }

    liveBlockIter->second.getLiveness(startBitNo, endBitNo, foundLivenessInfo);
  }

protected:
  void markBlockLive(SILBasicBlock *bb, unsigned bitNo, IsLive isLive) {
    markBlockLive(bb, bitNo, bitNo + 1, isLive);
  }

  void markBlockLive(SILBasicBlock *bb, unsigned startBitNo, unsigned endBitNo,
                     IsLive isLive) {
    assert(isLive != Dead && "erasing live blocks isn't implemented.");
    auto iterAndInserted =
        liveBlocks.insert(std::make_pair(bb, LivenessSmallBitVector()));
    if (iterAndInserted.second) {
      // We initialize the size of the small bit vector here rather than in
      // liveBlocks.insert above to prevent us from allocating upon failure if
      // we have more than SmallBitVector's small size number of bits.
      auto &insertedBV = iterAndInserted.first->getSecond();
      insertedBV.init(numBitsToTrack);
      insertedBV.setLiveness(startBitNo, endBitNo, isLive);
      if (discoveredBlocks)
        discoveredBlocks->push_back(bb);
    } else if (isLive == LiveOut) {
      // Update the existing entry to be live-out.
      iterAndInserted.first->getSecond().setLiveness(startBitNo, endBitNo,
                                                     LiveOut);
    }
  }

  void computeUseBlockLiveness(SILBasicBlock *userBB, unsigned startBitNo,
                               unsigned endBitNo);
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

/// Given a type T and a descendent field F in T's type tree, then the
/// sub-element number of F is the first leaf element of the type tree in its
/// linearized representation.
struct SubElementNumber {
  unsigned number;

  SubElementNumber(unsigned number) : number(number) {}

  /// Given an arbitrary projection \p projectionFromRoot from the \p
  /// rootAddress, compute the sub element number for that \p SILValue. The sub
  /// element number of a type T is always the index of its first leaf node
  /// descendent in the type tree.
  ///
  /// DISCUSSION: This works for non-leaf types in the type tree as well as
  /// normal leaf elements. It is the index of the first leaf element that is a
  /// sub element of the root SILType that this projection will effect. The rest
  /// of the elements effected can be found by computing the number of leaf sub
  /// elements of \p projectionFromRoot's type and adding this to the result of
  /// this function.
  ///
  /// \returns None if we didn't know how to compute sub-element for this
  /// projection.
  static Optional<SubElementNumber> compute(SILValue projectionFromRoot,
                                            SILValue root);

  operator unsigned() const { return number; }
};

/// Given a type T, this is the number of leaf field types in T's type tree. A
/// leaf field type is a descendent field of T that does not have any
/// descendent's itself.
struct TypeSubElementCount {
  unsigned number;

  TypeSubElementCount(unsigned number) : number(number) {}

  /// Given a type \p type, compute the total number of leaf sub-elements of \p
  /// type in the type tree.
  ///
  /// Some interesting properties of this computation:
  ///
  /// 1. When applied to the root type, this equals the total number of bits of
  /// liveness that we track.
  ///
  /// 2. When applied to a field type F of the type tree for a type T,
  /// computeNumLeafSubElements(F) when added to F's start sub element number
  /// will go to the next sibling node in the type tree, walking up the tree and
  /// attempting to find siblings if no further siblings exist.
  TypeSubElementCount(SILType type, SILModule &mod,
                      TypeExpansionContext context);

  TypeSubElementCount(SILValue value)
      : TypeSubElementCount(value->getType(), *value->getModule(),
                            TypeExpansionContext(*value->getFunction())) {}

  operator unsigned() const { return number; }
};

/// A span of leaf elements in the sub-element break down of the linearization
/// of the type tree of a type T.
struct TypeTreeLeafTypeRange {
  SubElementNumber startEltOffset;
  SubElementNumber endEltOffset;

  /// The leaf type range for the entire type tree.
  TypeTreeLeafTypeRange(SILValue rootAddress)
      : startEltOffset(0), endEltOffset(TypeSubElementCount(rootAddress)) {}

  /// The leaf type sub-range of the type tree of \p rootAddress, consisting of
  /// \p projectedAddress and all of \p projectedAddress's descendent fields in
  /// the type tree.
  TypeTreeLeafTypeRange(SILValue projectedAddress, SILValue rootAddress)
      : startEltOffset(
            *SubElementNumber::compute(projectedAddress, rootAddress)),
        endEltOffset(startEltOffset + TypeSubElementCount(projectedAddress)) {}

  /// Is the given leaf type specified by \p singleLeafElementNumber apart of
  /// our \p range of leaf type values in the our larger type.
  bool contains(SubElementNumber singleLeafElementNumber) const {
    return startEltOffset <= singleLeafElementNumber &&
           singleLeafElementNumber < endEltOffset;
  }

  /// Returns true if either of this overlaps at all with the given range.
  bool contains(TypeTreeLeafTypeRange range) const {
    if (startEltOffset <= range.startEltOffset &&
        range.startEltOffset < endEltOffset)
      return true;

    // If our start and end offsets, our extent is only 1 and we know that our
    // value
    unsigned rangeLastElt = range.endEltOffset - 1;
    if (range.startEltOffset == rangeLastElt)
      return false;

    // Othrwise, see if endEltOffset - 1 is within the range.
    return startEltOffset <= rangeLastElt && rangeLastElt < endEltOffset;
  }
};

/// This is exactly like pruned liveness except that instead of tracking a
/// single bit of liveness, it tracks multiple bits of liveness for leaf type
/// tree nodes of an allocation one is calculating pruned liveness for.
///
/// DISCUSSION: One can view a type T as a tree with recursively each field F of
/// the type T being a child of T in the tree. We say recursively since the tree
/// unfolds for F and its children as well.
class FieldSensitiveAddressPrunedLiveness {
  PrunedLiveBlocks liveBlocks;

  struct InterestingUser {
    TypeTreeLeafTypeRange subEltSpan;
    bool isConsuming;

    InterestingUser(TypeTreeLeafTypeRange subEltSpan, bool isConsuming)
        : subEltSpan(subEltSpan), isConsuming(isConsuming) {}

    InterestingUser &operator&=(bool otherValue) {
      isConsuming &= otherValue;
      return *this;
    }
  };

  /// Map all "interesting" user instructions in this def's live range to a pair
  /// consisting of the SILValue that it uses and a flag indicating whether they
  /// must end the lifetime.
  ///
  /// Lifetime-ending users are always on the boundary so are always
  /// interesting.
  ///
  /// Non-lifetime-ending uses within a LiveWithin block are interesting because
  /// they may be the last use in the block.
  ///
  /// Non-lifetime-ending within a LiveOut block are uninteresting.
  llvm::SmallMapVector<SILInstruction *, InterestingUser, 8> users;

  /// The root address of our type tree.
  SILValue rootAddress;

public:
  FieldSensitiveAddressPrunedLiveness(
      SILFunction *fn, SILValue rootValue,
      SmallVectorImpl<SILBasicBlock *> *discoveredBlocks = nullptr)
      : liveBlocks(TypeSubElementCount(rootValue), discoveredBlocks),
        rootAddress(rootValue) {}

  bool empty() const {
    assert(!liveBlocks.empty() || users.empty());
    return liveBlocks.empty();
  }

  void clear() {
    liveBlocks.clear();
    users.clear();
  }

  SILValue getRootAddress() const { return rootAddress; }

  unsigned numLiveBlocks() const { return liveBlocks.numLiveBlocks(); }

  /// If the constructor was provided with a vector to populate, then this
  /// returns the list of all live blocks with no duplicates.
  ArrayRef<SILBasicBlock *> getDiscoveredBlocks() const {
    return liveBlocks.getDiscoveredBlocks();
  }

  using UserRange =
      iterator_range<const std::pair<SILInstruction *, InterestingUser> *>;
  UserRange getAllUsers() const {
    return llvm::make_range(users.begin(), users.end());
  }

  using UserBlockRange = TransformRange<
      UserRange, function_ref<SILBasicBlock *(
                     const std::pair<SILInstruction *, InterestingUser> &)>>;
  UserBlockRange getAllUserBlocks() const {
    function_ref<SILBasicBlock *(
        const std::pair<SILInstruction *, InterestingUser> &)>
        op;
    op = [](const std::pair<SILInstruction *, InterestingUser> &pair)
        -> SILBasicBlock * { return pair.first->getParent(); };
    return UserBlockRange(getAllUsers(), op);
  }

  void initializeDefBlock(SILBasicBlock *defBB, TypeTreeLeafTypeRange span) {
    liveBlocks.initializeDefBlock(defBB, span.startEltOffset,
                                  span.endEltOffset);
  }

  /// For flexibility, \p lifetimeEnding is provided by the
  /// caller. PrunedLiveness makes no assumptions about the def-use
  /// relationships that generate liveness. For example, use->isLifetimeEnding()
  /// cannot distinguish the end of the borrow scope that defines this extended
  /// live range vs. a nested borrow scope within the extended live range.
  ///
  /// Also for flexibility, \p affectedAddress must be a derived projection from
  /// the base that \p user is affecting.
  void updateForUse(SILInstruction *user, TypeTreeLeafTypeRange span,
                    bool lifetimeEnding);

  void getBlockLiveness(
      SILBasicBlock *bb, TypeTreeLeafTypeRange span,
      SmallVectorImpl<PrunedLiveBlocks::IsLive> &resultingFoundLiveness) const {
    liveBlocks.getBlockLiveness(bb, span.startEltOffset, span.endEltOffset,
                                resultingFoundLiveness);
  }

  /// Return the liveness for this specific sub-element of our root value.
  PrunedLiveBlocks::IsLive getBlockLiveness(SILBasicBlock *bb,
                                            unsigned subElementNumber) const {
    SmallVector<PrunedLiveBlocks::IsLive, 1> isLive;
    liveBlocks.getBlockLiveness(bb, subElementNumber, subElementNumber + 1,
                                isLive);
    return isLive[0];
  }

  void getBlockLiveness(
      SILBasicBlock *bb,
      SmallVectorImpl<PrunedLiveBlocks::IsLive> &foundLiveness) const {
    liveBlocks.getBlockLiveness(bb, 0, liveBlocks.getNumBitsToTrack(),
                                foundLiveness);
  }

  enum IsInterestingUser { NonUser, NonLifetimeEndingUse, LifetimeEndingUse };

  /// Return a result indicating whether the given user was identified as an
  /// interesting use of the current def and whether it ends the lifetime.
  std::pair<IsInterestingUser, Optional<TypeTreeLeafTypeRange>>
  isInterestingUser(SILInstruction *user) const {
    auto useIter = users.find(user);
    if (useIter == users.end())
      return {NonUser, None};
    auto isInteresting =
        useIter->second.isConsuming ? LifetimeEndingUse : NonLifetimeEndingUse;
    return {isInteresting, useIter->second.subEltSpan};
  }

  unsigned getNumSubElements() const { return liveBlocks.getNumBitsToTrack(); }

  /// Return true if \p inst occurs before the liveness boundary. Used when the
  /// client already knows that inst occurs after the start of liveness.
  void isWithinBoundary(SILInstruction *inst, SmallBitVector &outVector) const;
};

/// Record the last use points and CFG edges that form the boundary of
/// FieldSensitiveAddressPrunedLiveness. It does this on a per type tree leaf
/// node basis.
struct FieldSensitiveAddressPrunedLivenessBoundary {
  /// The list of last users and an associated SILValue that is the address that
  /// is being used. The address can be used to determine the start sub element
  /// number of the user in the type tree and the end sub element number.
  ///
  /// TODO (MG): If we don't eventually need to store the SILValue here (I am
  /// not sure yet...), just store a tuple with the start/end sub element
  /// number.
  SmallVector<std::tuple<SILInstruction *, TypeTreeLeafTypeRange>, 8> lastUsers;

  /// Blocks where the value was live out but had a successor that was dead.
  SmallVector<SILBasicBlock *, 8> boundaryEdges;

  void clear() {
    lastUsers.clear();
    boundaryEdges.clear();
  }

  /// Compute the boundary from the blocks discovered during liveness analysis.
  ///
  /// Precondition: \p liveness.getDiscoveredBlocks() is a valid list of all
  /// live blocks with no duplicates.
  ///
  /// The computed boundary will completely post-dominate, including dead end
  /// paths. The client should query DeadEndBlocks to ignore those dead end
  /// paths.
  void compute(const FieldSensitiveAddressPrunedLiveness &liveness);

private:
  void
  findLastUserInBlock(SILBasicBlock *bb,
                      FieldSensitiveAddressPrunedLivenessBoundary &boundary,
                      const FieldSensitiveAddressPrunedLiveness &liveness,
                      unsigned subElementNumber);
};

} // namespace swift

#endif
