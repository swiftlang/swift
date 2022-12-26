//===--- PrunedLiveness.hpp - Compute liveness from selected uses ---------===//
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
/// ---------------------------------------------------------------------------
///
/// "Use points" are the instructions that "generate" liveness for a given
/// operand. A generalized use point visitor would look like this:
///
/// Given an \p operand, visit the use points relevant for liveness. For most
/// operands, this is simply the user instruction. For scoped operands, each
/// scope-ending instruction is a separate use point.
/// template<typename Operation>
/// inline bool visitUsePoints(Operand *use, Operation visitUsePoint) {
///   // Handle TrivialUse operands: begin_access & store_borrow address
///   // Handle InteriorPointer operands: store_borrow source
///   if (auto scopedAddress = ScopedAddressValue::forUse(use)) {
///     return scopedAddress.visitScopeEndingUses(visitUsePoint);
///   }
///   // Handle Borrow operands...
///   // Handles borrow scope introducers: begin_borrow & load_borrow.
///   // Handles guaranteed return values: begin_apply.
///   if (!BorrowingOperand(operand).visitScopeEndingUses([this](Operand *end) {
///     if (!visitUsePoint(end))
///       return false;
///     return true;
///   }
///   return visitUsePoint(use);
/// }
///
/// The visitors that switch on OperandOwnership use a specialized
/// implementation because each case above is specific to an ownership case.
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_PRUNEDLIVENESS_H
#define SWIFT_SILOPTIMIZER_UTILS_PRUNEDLIVENESS_H

#include "swift/AST/TypeExpansionContext.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/NodeDatastructures.h"
#include "swift/SIL/OwnershipUtils.h"
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
/// Incrementally building liveness is important for algorithms that create an
/// initial live region, perform some analysis on that, then expand the live
/// region by adding new uses before continuing the analysis.
///
/// Initializing "def blocks" restricts liveness on any path through those def
/// blocks to the blocks that occur on or after the def block. If any uses is
/// not dominated by a def block, then liveness will include the entry block,
/// as if defined by a function argument
///
/// We allow for multiple bits of liveness information to be tracked by
/// internally using a SmallBitVector. The multiple bit tracking is useful when
/// tracking state for multiple fields of the same root value. To do this, we
/// actually track 2 bits per actual needed bit so we can represent 3 Dead,
/// LiveOut, LiveWithin. This was previously unnecessary since we could just
/// represent dead by not having liveness state for a block. With multiple bits
/// possible this is no longer true.
///
/// TODO: For efficiency, use BasicBlockBitfield rather than SmallDenseMap.
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

    // FIXME: specialize this for scalar liveness, which is the critical path
    // for all OSSA utilities.
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

  // FIXME: This API should directly return the live bitset. The live bitset
  // type should have an api for querying and iterating over the live fields.
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

  llvm::StringRef getStringRef(IsLive isLive) const;
  void print(llvm::raw_ostream &OS) const;
  void dump() const;

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

/// If inner borrows are 'Contained', then liveness is fully described by the
/// scope-ending instructions of any inner borrow scopes, and those scope-ending
/// uses are dominated by the current def. This is known as a "simple" live
/// range.
///
/// If nested borrows are 'Reborrowed' then simple liveness computed here based
/// on dominated uses is not sufficient to guarantee the value's lifetime. To do
/// that, the client needs to consider the reborrow scopes. OSSALiveness handles
/// those details.
///
/// Reborrows are only relevant when they apply to the first level of borrow
/// scope. Reborrows within nested borrows scopes are already summarized by the
/// outer borrow scope.
enum class InnerBorrowKind {
  Contained, // any borrows are fully contained within this live range
  Reborrowed // at least one immediately nested borrow is reborrowed
};

inline InnerBorrowKind meet(InnerBorrowKind lhs, InnerBorrowKind rhs) {
  return (lhs > rhs) ? lhs : rhs;
}

/// Summarize reborrows and pointer escapes that affect a live range. Reborrows
/// and pointer escapes that are encapsulated in a nested borrow don't affect
/// the outer live range.
struct SimpleLiveRangeSummary {
  InnerBorrowKind innerBorrowKind;
  AddressUseKind addressUseKind;

  SimpleLiveRangeSummary(): innerBorrowKind(InnerBorrowKind::Contained),
                            addressUseKind(AddressUseKind::NonEscaping)
  {}

  void meet(const InnerBorrowKind lhs) {
    innerBorrowKind = swift::meet(innerBorrowKind, lhs);
  }
  void meet(const AddressUseKind lhs) {
    addressUseKind = swift::meet(addressUseKind, lhs);
  }
  void meet(const SimpleLiveRangeSummary lhs) {
    meet(lhs.innerBorrowKind);
    meet(lhs.addressUseKind);
  }
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
/// Note: a live-out block may contain a lifetime-ending use. This happens when
/// the client is computing "extended" livenes, for example by ignoring
/// copies. Lifetime ending uses are irrelevant for finding the liveness
/// boundary.
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

public:
  PrunedLiveness(SmallVectorImpl<SILBasicBlock *> *discoveredBlocks = nullptr)
      : liveBlocks(1 /*num bits*/, discoveredBlocks) {}

  bool empty() const {
    assert(!liveBlocks.empty() || users.empty());
    return liveBlocks.empty();
  }

  void clear() {
    liveBlocks.clear();
    users.clear();
  }

  unsigned numLiveBlocks() const { return liveBlocks.numLiveBlocks(); }

  /// If the constructor was provided with a vector to populate, then this
  /// returns the list of all live blocks with no duplicates.
  ArrayRef<SILBasicBlock *> getDiscoveredBlocks() const {
    return liveBlocks.getDiscoveredBlocks();
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
  /// Returns false if this cannot be done. This assumes that nested OSSA
  /// lifetimes are complete.
  InnerBorrowKind updateForBorrowingOperand(Operand *operand);

  /// Update liveness for an interior pointer use. These are normally handled
  /// like an instantaneous use. But if \p operand "borrows" a value for the
  /// duration of a scoped address (store_borrow), then update liveness for the
  /// entire scope. This assumes that nested OSSA lifetimes are complete.
  AddressUseKind checkAndUpdateInteriorPointer(Operand *operand);

  /// Update this liveness to extend across the given liveness.
  void extendAcrossLiveness(PrunedLiveness &otherLiveness);

  PrunedLiveBlocks::IsLive getBlockLiveness(SILBasicBlock *bb) const {
    return liveBlocks.getBlockLiveness(bb, 0);
  }

  enum IsInterestingUser {
    NonUser = 0,
    NonLifetimeEndingUse,
    LifetimeEndingUse
  };

  /// Return a result indicating whether the given user was identified as an
  /// interesting use of the current def and whether it ends the lifetime.
  IsInterestingUser isInterestingUser(SILInstruction *user) const {
    auto useIter = users.find(user);
    if (useIter == users.end())
      return NonUser;
    return useIter->second ? LifetimeEndingUse : NonLifetimeEndingUse;
  }

  void print(llvm::raw_ostream &OS) const;
  void dump() const;
};

/// Record the last use points and CFG edges that form the boundary of
/// PrunedLiveness.
///
/// Dead defs may occur even when the liveness result has uses for every
/// definition because those uses may occur in unreachable blocks. A dead def
/// must either be a SILInstruction or SILArgument. This supports memory
/// location liveness, so there isn't necessary a defining SILValue.
///
/// Each boundary edge is identified by its target block. The source of the edge
/// is the target block's single predecessor which must have at least one other
/// non-boundary successor.
struct PrunedLivenessBoundary {
  SmallVector<SILInstruction *, 8> lastUsers;
  SmallVector<SILBasicBlock *, 8> boundaryEdges;
  SmallVector<SILNode *, 1> deadDefs;

  void clear() {
    lastUsers.clear();
    boundaryEdges.clear();
    deadDefs.clear();
  }

  /// Visit the point at which a lifetime-ending instruction must be inserted,
  /// excluding dead-end blocks. This is only useful when it is known that none
  /// of the lastUsers ends the lifetime, for example when creating a new borrow
  /// scope to enclose all uses.
  void visitInsertionPoints(
      llvm::function_ref<void(SILBasicBlock::iterator insertPt)> visitor,
      DeadEndBlocks *deBlocks = nullptr);

  void print(llvm::raw_ostream &OS) const;
  void dump() const;
};

/// PrunedLiveness with information about defs for computing the live range
/// boundary.
///
/// LivenessWithDefs implements:
///
///   bool isInitialized() const
///
///   bool isDef(SILInstruction *inst) const
///
///   bool isDefBlock(SILBasicBlock *block) const
///
template <typename LivenessWithDefs>
class PrunedLiveRange : public PrunedLiveness {
protected:
  const LivenessWithDefs &asImpl() const {
    return static_cast<const LivenessWithDefs &>(*this);
  }

  PrunedLiveRange(SmallVectorImpl<SILBasicBlock *> *discoveredBlocks = nullptr)
      : PrunedLiveness(discoveredBlocks) {}

public:
  /// Update liveness for all direct uses of \p def.
  SimpleLiveRangeSummary updateForDef(SILValue def);

  /// Check if \p inst occurs in between the definition this def and the
  /// liveness boundary.
  bool isWithinBoundary(SILInstruction *inst) const;

  /// Returns true when all \p uses are between this def and the liveness
  /// boundary \p deadEndBlocks is optional.
  bool areUsesWithinBoundary(ArrayRef<Operand *> uses,
                             DeadEndBlocks *deadEndBlocks) const;

  /// Returns true if any of the \p uses are before this def or after the
  /// liveness boundary
  /// \p deadEndBlocks is optional.
  bool areUsesOutsideBoundary(ArrayRef<Operand *> uses,
                              DeadEndBlocks *deadEndBlocks) const;

  /// Compute the boundary from the blocks discovered during liveness analysis.
  ///
  /// Precondition: \p liveness.getDiscoveredBlocks() is a valid list of all
  /// live blocks with no duplicates.
  ///
  /// The computed boundary will completely post-dominate, including dead end
  /// paths. The client should query DeadEndBlocks to ignore those dead end
  /// paths.
  void computeBoundary(PrunedLivenessBoundary &boundary) const;

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
  void computeBoundary(PrunedLivenessBoundary &boundary,
                       ArrayRef<SILBasicBlock *> postDomBlocks) const;
};

// Singly-defined liveness.
//
// An SSA def results in pruned liveness with a contiguous liverange.
//
// An unreachable self-loop might result in a "gap" between the last use above
// the def in the same block.
//
// For SSA live ranges, a single "def" block dominates all uses. If no def
// block is provided, liveness is computed as if defined by a function
// argument. If the client does not provide a single, dominating def block,
// then the client must at least ensure that no uses precede the first
// definition in a def block. Since this analysis does not remember the
// positions of defs, it assumes that, within a block, uses follow
// defs. Breaking this assumption will result in a "hole" in the live range in
// which the def block's predecessors incorrectly remain dead. This situation
// could be handled by adding an updateForUseBeforeFirstDef() API.
class SSAPrunedLiveness : public PrunedLiveRange<SSAPrunedLiveness> {
  SILValue def;
  SILInstruction *defInst = nullptr; // nullptr for argument defs.

public:
  SSAPrunedLiveness(
      SmallVectorImpl<SILBasicBlock *> *discoveredBlocks = nullptr)
      : PrunedLiveRange(discoveredBlocks) {}

  SILValue getDef() const { return def; }

  void clear() {
    def = SILValue();
    defInst = nullptr;
    PrunedLiveRange::clear();
  }

  void initializeDef(SILValue def) {
    assert(!this->def && "reinitialization");

    this->def = def;
    defInst = def->getDefiningInstruction();
    initializeDefBlock(def->getParentBlock());
  }

  bool isInitialized() const { return bool(def); }

  bool isDef(SILInstruction *inst) const { return inst == defInst; }

  bool isDefBlock(SILBasicBlock *block) const {
    return def->getParentBlock() == block;
  }

  /// SSA implementation of computeBoundary.
  void findBoundariesInBlock(SILBasicBlock *block, bool isLiveOut,
                             PrunedLivenessBoundary &boundary) const;

  /// Compute liveness for a single SSA definition. The lifetime-ending uses are
  /// also recorded--destroy_value or end_borrow.
  ///
  /// This only handles simple liveness in which all uses are dominated by the
  /// definition. If the returned summary includes InnerBorrowKind::Reborrow,
  /// then the resulting liveness does not includes potentially non-dominated
  /// uses within the reborrow scope. If the summary returns something other
  /// than AddressUseKind::NonEscaping, then the resulting liveness does not
  /// necessarilly encapsulate value ownership.
  ///
  /// Warning: If OSSA lifetimes are incomplete, then destroy_values might not
  /// jointly-post dominate if dead-end blocks are present. Nested scopes may
  /// also lack scope-ending instructions, so the liveness of their nested uses
  /// may be ignored.
  SimpleLiveRangeSummary computeSimple() {
    assert(def && "SSA def uninitialized");
    return updateForDef(def);
  }
};

/// MultiDefPrunedLiveness is computed incrementally by calling updateForUse.
///
/// Defs should be initialized before calling updatingForUse on any def
/// that reaches the use.
class MultiDefPrunedLiveness : public PrunedLiveRange<MultiDefPrunedLiveness> {
  NodeSetVector defs;
  BasicBlockSet defBlocks;

public:
  MultiDefPrunedLiveness(
      SILFunction *function,
      SmallVectorImpl<SILBasicBlock *> *discoveredBlocks = nullptr)
      : PrunedLiveRange(discoveredBlocks), defs(function), defBlocks(function) {
  }

  void clear() {
    llvm_unreachable("multi-def liveness cannot be reused");
  }

  void initializeDef(SILNode *def) {
    assert(isa<SILInstruction>(def) || isa<SILArgument>(def));
    defs.insert(def);
    auto *block = def->getParentBlock();
    defBlocks.insert(block);
    initializeDefBlock(block);
  }

  bool isInitialized() const { return !defs.empty(); }

  bool isDef(SILInstruction *inst) const {
    return defs.contains(cast<SILNode>(inst));
  }

  bool isDefBlock(SILBasicBlock *block) const {
    return defBlocks.contains(block);
  }

  /// Multi-Def implementation of computeBoundary.
  void findBoundariesInBlock(SILBasicBlock *block, bool isLiveOut,
                             PrunedLivenessBoundary &boundary) const;

  /// Compute liveness for a all currently initialized definitions. The
  /// lifetime-ending uses are also recorded--destroy_value or
  /// end_borrow. However destroy_values might not jointly-post dominate if
  /// dead-end blocks are present.
  ///
  /// This only handles simple liveness in which all uses are dominated by the
  /// definition. If the returned summary includes InnerBorrowKind::Reborrow,
  /// then the resulting liveness does not includes potentially non-dominated
  /// uses within the reborrow scope. If the summary returns something other
  /// than AddressUseKind::NonEscaping, then the resulting liveness does not
  /// necessarilly encapsulate value ownership.
  ///
  /// Warning: If OSSA lifetimes are incomplete, then destroy_values might not
  /// jointly-post dominate if dead-end blocks are present. Nested scopes may
  /// also lack scope-ending instructions, so the liveness of their nested uses
  /// may be ignored.
  SimpleLiveRangeSummary computeSimple();
};

//===----------------------------------------------------------------------===//
//                          DiagnosticPrunedLiveness
//===----------------------------------------------------------------------===//

// FIXME: it isn't clear what this is for or what nonLifetimeEndingUseInLiveOut
// means precisely.
class DiagnosticPrunedLiveness : public SSAPrunedLiveness {
  /// A side array that stores any non lifetime ending uses we find in live out
  /// blocks. This is used to enable our callers to emit errors on non-lifetime
  /// ending uses that extend liveness into a loop body.
  SmallSetVector<SILInstruction *, 8> *nonLifetimeEndingUsesInLiveOut;

public:
  DiagnosticPrunedLiveness(
      SmallVectorImpl<SILBasicBlock *> *discoveredBlocks = nullptr,
      SmallSetVector<SILInstruction *, 8> *nonLifetimeEndingUsesInLiveOut =
          nullptr)
      : SSAPrunedLiveness(discoveredBlocks),
        nonLifetimeEndingUsesInLiveOut(nonLifetimeEndingUsesInLiveOut) {}

  void clear() {
    SSAPrunedLiveness::clear();
    if (nonLifetimeEndingUsesInLiveOut)
      nonLifetimeEndingUsesInLiveOut->clear();
  }

  void updateForUse(SILInstruction *user, bool lifetimeEnding);

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
};

} // namespace swift

#endif
