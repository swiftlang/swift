//===--- FieldSensitivePrunedLiveness.h -----------------------------------===//
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
/// \file This is a field sensitive implementation of PrunedLiveness. It is a
/// completely separate implementation for efficiency reasons but in spirit is
/// implementing the exact same algorithms with changes to account for dealing
/// with multiple elements.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_FIELDSENSITIVEPRUNTEDLIVENESS_H
#define SWIFT_SIL_FIELDSENSITIVEPRUNTEDLIVENESS_H

#include "swift/AST/TypeExpansionContext.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

/// Given a type T and a descendent field F in T's type tree, then the
/// sub-element number of F is the first leaf element of the type tree in its
/// linearized representation.
///
/// Linearized Representation of Structs/Tuples
/// -------------------------------------------
///
/// For structs/tuples, the linearized representation is just an array with one
/// element for each leaf element. Thus if we have a struct of the following
/// sort:
///
/// ```
/// struct Pair {
///   var lhs: Int
///   var rhs: Int
/// }
///
/// struct MyStruct {
///   var firstField: Int
///   var pairField: Pair
///   var tupleField: (Int, Int)
/// }
/// ```
///
/// the linearized representation of MyStruct's type tree leaves would be:
///
/// ```
/// [firstField, pairField.lhs, pairField.rhs, tupleField.0, tupleField.1]
/// ```
///
/// So if one had an uninitialized myStruct and initialized pairField, one would
/// get the following bit set of liveness:
///
/// ```
/// [0, 1, 1, 0, 0]
/// ```
///
/// NOTE: Our representation allows for partial initialization/reinitialization
/// since we do not include a bit for each level of struct/tuple. The effect of
/// this is that we cannot distinguish a single field type from its child
/// field. This makes this type not suited for projection operations. This is a
/// trade-off that was made to make it easy to support partial
/// initialization/reinitialization of liveness.
///
/// Linearized Representation of Enums
/// ----------------------------------
///
/// Since enums are sum types, an enum can require different numbers of bits in
/// its linearized representation depending on the payload of the case that the
/// enum is initialized to. To work around this problem in our representation,
/// we always store enough bits for the max sized payload of all cases of the
/// enum and add an additional last bit for the discriminator. Any extra bits
/// that may be needed (e.x.: we are processing a enum case with a smaller
/// payload) are always assumed to be set to the same value that the
/// discriminator bit is set to. This representation allows us to track liveness
/// trading off the ability to determine information about the actual case that
/// we are tracking. Since we just care about liveness, this is a trade off that
/// we are willing to make since our goal (computing liveness) is still solved.
///
/// With the above paragraph in mind, an example of the bit layout of an enum
/// looks as follows:
///
/// ```
/// [ PAYLOAD BITS | EXTRA_TOP_LEVEL_BITS | DISCRIMINATOR_BIT ]
/// ```
///
/// Notice how placing the discriminator bit last ensures that separately the
/// payload and the extra top level bits/discriminator bit are both contiguous
/// in the representation. This ensures that we can test both that the payload
/// is live and separately that the discriminator/extra top level bits are live
/// with a single contiguous range of bits. This is important since field
/// sensitive liveness can only compute liveness for contiguous ranges of bits.
///
/// Lets look at some examples, starting with E:
///
/// ```
/// enum E {
/// case firstCase
/// case secondCase(Int)
/// case thirdCase(Pair)
/// }
/// ```
///
/// The linearized representation of E would be three slots since the payload
/// with the largest linearized representation is Pair:
///
///            ----- |E| --------
///           /                  \
///          /                    \
///         v                      v
///      | Pair |          | Discriminator |
///       /    \
///      /      \
///     /        \
///    v          v
/// | LHS |    | RHS |
///
/// This in term would mean the following potential bit representations given
/// various cases/states of deinitialization of the payload.
///
/// ```
/// firstCase inited:            [1, 1, 1]
/// firstCase deinited:          [0, 0, 0]
///
/// secondCase inited:           [1, 1, 1]
/// secondCase payload deinited: [0, 1, 1]
/// secondCase deinited:         [0, 0, 0]
///
/// thirdCase inited:            [1, 1, 1]
/// thirdCase payload deinited:  [0, 0, 1]
/// thirdCase deinited:          [0, 0, 0]
/// ```
///
/// Now lets consider an enum without any payload cases. Given such an enum:
///
/// ```
/// enum E2 {
/// case firstCase
/// case secondCase
/// case thirdCase
/// }
/// ```
///
/// we would only use a single bit in our linearized representation, just for
/// the discriminator value.
///
/// Enums and Partial Initialization/Deinitialization
/// -------------------------------------------------
///
/// One property of our representation of structs and tuples is that a code
/// generator can init/reinit/deinit a struct/tuple completely just by
/// performing the relevant operation on each of its sub-types
/// individually. This is not possible for enums in our representation since if
/// one just took the leaf nodes for the payload, one would not update the bit
/// for the enum case itself and any additional spare bits. Luckily for us, this
/// is safe to assume since we only use this in misc address contexts and move
/// only object contexts in Raw SIL which have the following invariants:
///
/// 1. SIL addresses of enum type cannot dynamically change the payload of an
///    enum without destroying the original enum completely. So such an address
///    can never be reinitialized by storing into the payload of an enum.
///
/// 2. It is illegal in SIL to unchecked_enum_data a move only type in Raw
///    SIL. One must instead use a switch_enum which creates a new value for the
///    destructured enum. We when writing such verifiers consider the switch to
///    produce an entire new value rather than a derived forwarding value.
struct SubElementOffset {
  /// Our internal sub element representation number. We force 32 bits so that
  /// our type tree span is always pointer width. This is convenient for storing
  /// it in other data structures.
  uint32_t number;

  SubElementOffset(unsigned number) : number(number) {}

  /// Given an arbitrary projection \p projectionFromRoot from the \p
  /// rootValue, compute the sub element number for that \p SILValue. The sub
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
  static std::optional<SubElementOffset> compute(SILValue projectionFromRoot,
                                                 SILValue root) {
    assert(projectionFromRoot->getType().getCategory() ==
               root->getType().getCategory() &&
           "projectionFromRoot and root must both be objects or address.");
    if (root->getType().isObject())
      return computeForValue(projectionFromRoot, root);
    return computeForAddress(projectionFromRoot, root);
  }

  operator unsigned() const { return number; }

  SubElementOffset &operator+=(unsigned other) {
    number += other;
    return *this;
  }

private:
  static std::optional<SubElementOffset>
  computeForAddress(SILValue projectionFromRoot, SILValue rootAddress);
  static std::optional<SubElementOffset>
  computeForValue(SILValue projectionFromRoot, SILValue rootValue);
};

/// Counts the leaf fields aggregated together into a particular type.
///
/// Defined in such a way as to enable walking up the tree of aggregations
/// node-by-node, visiting each type along the way.
///
/// The definition is given recursively as follows:
/// a an atom  => count(a) := 1
/// t a tuple  => count(t) := sum(t.elements, { elt in count(type(elt)) })
/// s a struct => count(s) := sum(s.fields, { f in count(type(f)) })
///                             + s.hasDeinit
/// e an enum  => count(e) := sum(e.elements, { elt in count(type(elt)) })
///                             + 1 // discriminator
///                             + e.hasDeinit
///
/// The deinit bit is at the end to make drop_deinit produce a value whose
/// leaves are contiguous.
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

  /// Helper method that invokes the SILModule &mod entry point.
  TypeSubElementCount(SILType type, SILFunction *fn)
      : TypeSubElementCount(type, fn->getModule(), TypeExpansionContext(*fn)) {}

  TypeSubElementCount(SILValue value);

  operator unsigned() const { return number; }

  TypeSubElementCount operator-=(unsigned other) {
    *this = TypeSubElementCount(unsigned(*this) - other);
    return *this;
  }
};

class FieldSensitivePrunedLiveness;

enum NeedsDestroy_t {
  DoesNotNeedDestroy = false,
  NeedsDestroy = true,
};

/// A span of leaf elements in the sub-element break down of the linearization
/// of the type tree of a type T.
struct TypeTreeLeafTypeRange {
  friend FieldSensitivePrunedLiveness;

  SubElementOffset startEltOffset;
  SubElementOffset endEltOffset;

  TypeTreeLeafTypeRange() : startEltOffset(0), endEltOffset(0) {}

  TypeTreeLeafTypeRange(SubElementOffset start, SubElementOffset end)
      : startEltOffset(start), endEltOffset(end) {}

  /// The leaf type range for the entire type tree.
  TypeTreeLeafTypeRange(SILValue rootValue)
      : startEltOffset(0), endEltOffset(TypeSubElementCount(rootValue)) {}

  /// The leaf type range for the entire type tree.
  TypeTreeLeafTypeRange(SILType rootType, SILFunction *fn)
      : startEltOffset(0), endEltOffset(TypeSubElementCount(rootType, fn)) {}

  /// The leaf type sub-range of the type tree of \p rootAddress, consisting of
  /// \p projectedAddress and all of \p projectedAddress's descendent fields in
  /// the type tree.
  static void get(SILValue projectedValue, SILValue rootValue,
                  SmallVectorImpl<TypeTreeLeafTypeRange> &ranges) {
    auto startEltOffset = SubElementOffset::compute(projectedValue, rootValue);
    if (!startEltOffset)
      return;
    ranges.push_back({*startEltOffset,
                      *startEltOffset + TypeSubElementCount(projectedValue)});
  }

  /// Which bits of \p rootValue are involved in \p op.
  ///
  /// This is a subset of (usually equal to) the bits of op->getType() in \p
  /// rootValue.
  static void get(Operand *op, SILValue rootValue,
                  SmallVectorImpl<TypeTreeLeafTypeRange> &ranges);

  static void constructProjectionsForNeededElements(
      SILValue rootValue, SILInstruction *insertPt, DominanceInfo *domTree,
      SmallBitVector &neededElements,
      SmallVectorImpl<std::tuple<SILValue, TypeTreeLeafTypeRange,
                                 NeedsDestroy_t>> &resultingProjections);

  static void visitContiguousRanges(
      SmallBitVector const &bits,
      llvm::function_ref<void(TypeTreeLeafTypeRange)> callback);

  bool operator==(const TypeTreeLeafTypeRange &other) const {
    return startEltOffset == other.startEltOffset &&
           endEltOffset == other.endEltOffset;
  }

  bool operator!=(const TypeTreeLeafTypeRange &other) const {
    return !(*this == other);
  }

  /// Return the type tree leaf type range that is the intersection of \p this
  /// and \p other.
  std::optional<TypeTreeLeafTypeRange>
  setIntersection(const TypeTreeLeafTypeRange &other) const {
    unsigned start = startEltOffset;
    if (startEltOffset < other.startEltOffset)
      start = other.startEltOffset;
    unsigned end = endEltOffset;
    if (endEltOffset >= other.endEltOffset)
      end = other.endEltOffset;
    if (start >= end)
      return std::nullopt;
    return TypeTreeLeafTypeRange(start, end);
  }

  /// Whether \p bits contains any of the in-range bits.
  bool intersects(SmallBitVector const &bits) const {
    for (auto element : getRange()) {
      if (bits.test(element)) {
        return true;
      }
    }
    return false;
  }

  /// Is the given leaf type specified by \p singleLeafElementNumber apart of
  /// our \p range of leaf type values in the our larger type.
  bool contains(SubElementOffset singleLeafElementNumber) const {
    return startEltOffset <= singleLeafElementNumber &&
           singleLeafElementNumber < endEltOffset;
  }

  /// Returns true if \p range is completely within this range.
  bool contains(TypeTreeLeafTypeRange range) const {
    return startEltOffset <= range.startEltOffset &&
           endEltOffset >= range.endEltOffset;
  }

  /// Sets each bit in \p bits corresponding to an element of this range.
  void setBits(SmallBitVector &bits) const {
    bits.set(startEltOffset, endEltOffset);
  }

  /// Resets each bit in \p bits corresponding to an element of this range.
  void resetBits(SmallBitVector &bits) const {
    bits.reset(startEltOffset, endEltOffset);
  }

  IntRange<unsigned> getRange() const {
    return range(startEltOffset, endEltOffset);
  }

  bool empty() const { return startEltOffset == endEltOffset; }

  unsigned size() const { return endEltOffset - startEltOffset; }

  /// Construct per field projections if the projection range has any bits in
  /// common with filterBitVector.
  void constructFilteredProjections(
      SILValue value, SILInstruction *insertPt, SmallBitVector &filterBitVector,
      DominanceInfo *domTree,
      llvm::function_ref<bool(SILValue, TypeTreeLeafTypeRange, NeedsDestroy_t)>
          callback);

  void print(llvm::raw_ostream &os) const {
    os << "TypeTreeLeafTypeRange: (start: " << startEltOffset
       << ", end: " << endEltOffset << ")";
  }
  void dump() const { print(llvm::dbgs()); }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const TypeTreeLeafTypeRange &value) {
  value.print(os);
  return os;
}

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
class FieldSensitivePrunedLiveBlocks {
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
  /// DeadToLiveEdge blocks are not live within the block itself, but the value
  /// becomes live on one or more of the edges out.
  ///
  /// LiveOut blocks are live on at least one successor path. LiveOut blocks may
  /// or may not contain defs or uses.
  enum IsLive {
    Dead = 0,
    LiveWithin = 1,
    DeadToLiveEdge = 2,
    LiveOut = 3,
  };
  
  static bool isDead(IsLive liveness) {
    return liveness == Dead || liveness == DeadToLiveEdge;
  }

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
      return IsLive((bits[bitNo * 2 + 1] << 1) | bits[bitNo * 2]);
    }

    /// Returns the liveness in \p resultingFoundLiveness. We only return the
    /// bits for endBitNo - startBitNo.
    void getLiveness(SmallBitVector const &bitsOfInterest,
                     SmallVectorImpl<IsLive> &resultingFoundLiveness) const {
      for (auto bit : bitsOfInterest.set_bits()) {
        resultingFoundLiveness.push_back(getLiveness(bit));
      }
    }

    void setLiveness(unsigned bitNo, IsLive isLive) {
      bits[bitNo * 2] = isLive & 1;
      bits[bitNo * 2 + 1] = bool(isLive & 2);
    }

    void setLiveness(unsigned startBitNo, unsigned endBitNo, IsLive isLive) {
      for (unsigned i = startBitNo, e = endBitNo; i != e; ++i) {
        setLiveness(i, isLive);
      }
    }
  };

private:
  /// Map all blocks in which current def is live to a SmallBitVector indicating
  /// whether the value represented by said bit is also liveout of the block.
  llvm::SmallDenseMap<SILBasicBlock *, LivenessSmallBitVector, 4> liveBlocks;

  /// Number of bits of liveness to track. By default 1. Used to track multiple
  /// liveness bits.
  ///
  /// NOTE: After clearing, this is set to None to ensure that the user
  /// reinitializes it as appropriate.
  std::optional<unsigned> numBitsToTrack;

  /// Optional vector of live blocks for clients that deterministically iterate.
  SmallVectorImpl<SILBasicBlock *> *discoveredBlocks;

  /// Once the first use has been seen, no definitions can be added.
  SWIFT_ASSERT_ONLY_DECL(bool seenUse = false);

public:
  FieldSensitivePrunedLiveBlocks(
      SmallVectorImpl<SILBasicBlock *> *discoveredBlocks = nullptr)
      : numBitsToTrack(), discoveredBlocks(discoveredBlocks) {
    assert(!discoveredBlocks || discoveredBlocks->empty());
  }

  bool isInitialized() const { return numBitsToTrack.has_value(); }

  unsigned getNumBitsToTrack() const { return *numBitsToTrack; }

  bool empty() const { return liveBlocks.empty(); }

  void clear() {
    liveBlocks.clear();
    if (discoveredBlocks)
      discoveredBlocks->clear();
    numBitsToTrack = std::nullopt;
    SWIFT_ASSERT_ONLY(seenUse = false);
  }

  void init(unsigned inputNumBitsToTrack) {
    clear();
    numBitsToTrack = inputNumBitsToTrack;
  }

  unsigned numLiveBlocks() const {
    assert(isInitialized());
    return liveBlocks.size();
  }

  /// If the constructor was provided with a vector to populate, then this
  /// returns the list of all live blocks with no duplicates.
  ArrayRef<SILBasicBlock *> getDiscoveredBlocks() const {
    assert(isInitialized());
    return *discoveredBlocks;
  }

  void initializeDefBlock(SILBasicBlock *defBB, unsigned bitNo) {
    assert(isInitialized());
    markBlockLive(defBB, bitNo, LiveWithin);
  }

  void initializeDefBlock(SILBasicBlock *defBB, unsigned startBitNo,
                          unsigned endBitNo,
                          IsLive isLive = LiveWithin) {
    assert(isInitialized());
    markBlockLive(defBB, startBitNo, endBitNo, isLive);
  }

  /// Update this liveness result for a single use.
  IsLive updateForUse(SILInstruction *user, unsigned bitNo,
                      bool isUserBeforeDef) {
    assert(isInitialized());
    auto *block = user->getParent();
    if (!isUserBeforeDef) {
      auto liveness = getBlockLiveness(block, bitNo);
      if (!isDead(liveness))
        return liveness;
    }
    computeScalarUseBlockLiveness(block, bitNo);
    return getBlockLiveness(block, bitNo);
  }

  /// Update this range of liveness results for a single use.
  void updateForUse(SILInstruction *user, unsigned startBitNo,
                    unsigned endBitNo, SmallBitVector const &useBeforeDefBits,
                    SmallVectorImpl<IsLive> &resultingLiveness);

  IsLive getBlockLiveness(SILBasicBlock *bb, unsigned bitNo) const {
    assert(isInitialized());
    auto liveBlockIter = liveBlocks.find(bb);
    if (liveBlockIter == liveBlocks.end()) {
      return Dead;
    }

    return liveBlockIter->second.getLiveness(bitNo);
  }

  /// FIXME: This API should directly return the live bitset. The live bitset
  /// type should have an api for querying and iterating over the live fields.
  void getBlockLiveness(SILBasicBlock *bb, unsigned startBitNo,
                        unsigned endBitNo,
                        SmallVectorImpl<IsLive> &foundLivenessInfo) const {
    SmallBitVector bits(*numBitsToTrack);
    for (auto index = startBitNo; index < endBitNo; ++index) {
      bits.set(index);
    }
    getBlockLiveness(bb, bits, foundLivenessInfo);
  }

  void getBlockLiveness(SILBasicBlock *bb, SmallBitVector const &bits,
                        SmallVectorImpl<IsLive> &foundLivenessInfo) const {
    assert(isInitialized());
    auto liveBlockIter = liveBlocks.find(bb);
    if (liveBlockIter == liveBlocks.end()) {
      for (auto bit : bits.set_bits()) {
        (void)bit;
        foundLivenessInfo.push_back(Dead);
      }
      return;
    }

    liveBlockIter->second.getLiveness(bits, foundLivenessInfo);
  }

  llvm::StringRef getStringRef(IsLive isLive) const;
  void print(llvm::raw_ostream &OS) const;
  void dump() const;

protected:
  void markBlockLive(SILBasicBlock *bb, unsigned bitNo, IsLive isLive) {
    assert(isInitialized());

    assert(isLive != Dead && "erasing live blocks isn't implemented.");
    auto iterAndInserted =
        liveBlocks.insert(std::make_pair(bb, LivenessSmallBitVector()));
    if (iterAndInserted.second) {
      // We initialize the size of the small bit vector here rather than in
      // liveBlocks.insert above to prevent us from allocating upon failure if
      // we have more than SmallBitVector's small size number of bits.
      auto &insertedBV = iterAndInserted.first->getSecond();
      insertedBV.init(*numBitsToTrack);
      insertedBV.setLiveness(bitNo, bitNo + 1, isLive);
      if (discoveredBlocks)
        discoveredBlocks->push_back(bb);
    } else {
      // If we are dead, always update to the new liveness.
      switch (iterAndInserted.first->getSecond().getLiveness(bitNo)) {
      case Dead:
      case DeadToLiveEdge:
        iterAndInserted.first->getSecond().setLiveness(bitNo, bitNo + 1,
                                                       isLive);
        break;
      case LiveWithin:
        if (isLive == LiveOut) {
          // Update the existing entry to be live-out.
          iterAndInserted.first->getSecond().setLiveness(bitNo, bitNo + 1,
                                                         LiveOut);
        }
        break;
      case LiveOut:
        break;
      }
    }
  }

  void markBlockLive(SILBasicBlock *bb, unsigned startBitNo, unsigned endBitNo,
                     IsLive isLive) {
    assert(isInitialized());
    for (unsigned index : range(startBitNo, endBitNo)) {
      markBlockLive(bb, index, isLive);
    }
  }

private:
  /// A helper routine that as a fast path handles the scalar case. We do not
  /// handle the mult-bit case today since the way the code is written today
  /// assumes we process a bit at a time.
  ///
  /// TODO: Make a multi-bit query for efficiency reasons.
  void computeScalarUseBlockLiveness(SILBasicBlock *userBB,
                                     unsigned startBitNo);
};

/// This is exactly like pruned liveness except that instead of tracking a
/// single bit of liveness, it tracks multiple bits of liveness for leaf type
/// tree nodes of an allocation one is calculating pruned liveness for.
///
/// DISCUSSION: One can view a type T as a tree with recursively each field F of
/// the type T being a child of T in the tree. We say recursively since the tree
/// unfolds for F and its children as well.
class FieldSensitivePrunedLiveness {
  FieldSensitivePrunedLiveBlocks liveBlocks;

public:
  enum IsInterestingUser { NonUser, NonLifetimeEndingUse, LifetimeEndingUse };

  struct InterestingUser {
    // Together these bit vectors encode four states per field:
    //   +---------------+----------------+---------------+
    // - | liveBits[bit] | consumingBits] |     state     |
    //   +---------------+----------------+---------------+
    //   |      0        |        0       | dead          |
    //   |      0        |        1       | non-use       |
    //   |      1        |        0       | non-consuming |
    //   |      1        |        1       | consuming     |
    //   +---------------+----------------+---------------+
    SmallBitVector liveBits;
    SmallBitVector consumingBits;

    InterestingUser(unsigned bitCount)
        : liveBits(bitCount), consumingBits(bitCount) {}

    InterestingUser(unsigned bitCount, TypeTreeLeafTypeRange range,
                    bool lifetimeEnding)
        : liveBits(bitCount), consumingBits(bitCount) {
      addUses(range, lifetimeEnding);
    }

    /// Record that the instruction uses the bits of the value in \p range.
    void addUses(TypeTreeLeafTypeRange range, bool lifetimeEnding) {
      SmallBitVector bits(liveBits.size());
      range.setBits(bits);
      addUses(bits, lifetimeEnding);
    }

    /// Record that the instruction uses the bits in \p bits.
    void addUses(SmallBitVector const &bits, bool lifetimeEnding) {
      if (lifetimeEnding) {
        consumingBits |= bits & ~liveBits;
      } else {
        consumingBits &= ~bits;
      }
      liveBits |= bits;
    }

    /// Extend liveness at the bits in the specified  \p range without
    /// overriding whether the lifetimes of those bits end.
    void extendToNonUse(TypeTreeLeafTypeRange range) {
      SmallBitVector bits(liveBits.size());
      range.setBits(bits);
      extendToNonUse(bits);
    }

    /// Extend liveness at the specified \p bits without overriding whether the
    /// lifetimes of those bits end.
    void extendToNonUse(SmallBitVector const &bits) {
      consumingBits |= bits & ~liveBits;
    }

    /// Populates the provided vector with contiguous ranges of bits which are
    /// users of the same sort.
    ///
    /// All bits not selected by \p selectedBits are assumed to be
    /// IsInterestingUser::NonUser.
    void getContiguousRanges(
        SmallVectorImpl<std::pair<TypeTreeLeafTypeRange, IsInterestingUser>>
            &ranges,
        const SmallBitVector &selectedBits) const {
      if (liveBits.size() == 0)
        return;

      assert(ranges.empty());
      std::optional<std::pair<unsigned, IsInterestingUser>> current =
          std::nullopt;
      for (unsigned bit = 0, size = liveBits.size(); bit < size; ++bit) {
        auto interesting = selectedBits.test(bit) ? isInterestingUser(bit)
                                                  : IsInterestingUser::NonUser;
        if (!current) {
          current = {bit, interesting};
          continue;
        }
        if (current->second != interesting) {
          ranges.push_back(
              {TypeTreeLeafTypeRange(current->first, bit), current->second});
          current = {bit, interesting};
        }
      }
      ranges.push_back({TypeTreeLeafTypeRange(current->first, liveBits.size()),
                        current->second});
    }

    IsInterestingUser isInterestingUser(unsigned element) const {
      auto isLive = liveBits.test(element);
      auto isConsuming = consumingBits.test(element);
      if (!isLive && !isConsuming) {
        return NonUser;
      } else if (!isLive && isConsuming) {
        return NonLifetimeEndingUse;
      } else if (isLive && isConsuming) {
        return LifetimeEndingUse;
      } else if (isLive && !isConsuming) {
        return NonLifetimeEndingUse;
      }
      llvm_unreachable("covered conditions");
    }
  };

private:
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
  SILValue rootValue;

public:
  FieldSensitivePrunedLiveness(
      SILFunction *fn,
      SmallVectorImpl<SILBasicBlock *> *discoveredBlocks = nullptr)
      : liveBlocks(discoveredBlocks) {}

  bool empty() const {
    assert(!liveBlocks.empty() || users.empty());
    return liveBlocks.empty();
  }

  void clear() {
    liveBlocks.clear();
    users.clear();
    rootValue = SILValue();
  }

  void init(SILValue newRootValue) {
    clear();
    rootValue = newRootValue;
    liveBlocks.init(TypeSubElementCount(newRootValue));
  }

  bool isInitialized() const {
    return liveBlocks.isInitialized() && bool(rootValue);
  }

  SILValue getRootValue() const {
    assert(isInitialized());
    return rootValue;
  }

  SILType getRootType() const {
    assert(isInitialized());
    return rootValue->getType();
  }

  unsigned numLiveBlocks() const {
    assert(isInitialized());
    return liveBlocks.numLiveBlocks();
  }

  TypeTreeLeafTypeRange getTopLevelSpan() const {
    assert(isInitialized());
    return TypeTreeLeafTypeRange(0, getNumSubElements());
  }

  /// If the constructor was provided with a vector to populate, then this
  /// returns the list of all live blocks with no duplicates.
  ArrayRef<SILBasicBlock *> getDiscoveredBlocks() const {
    assert(isInitialized());
    return liveBlocks.getDiscoveredBlocks();
  }

  using UserRange =
      iterator_range<const std::pair<SILInstruction *, InterestingUser> *>;
  UserRange getAllUsers() const {
    assert(isInitialized());
    return llvm::make_range(users.begin(), users.end());
  }

  using UserBlockRange = TransformRange<
      UserRange, function_ref<SILBasicBlock *(
                     const std::pair<SILInstruction *, InterestingUser> &)>>;
  UserBlockRange getAllUserBlocks() const {
    assert(isInitialized());
    function_ref<SILBasicBlock *(
        const std::pair<SILInstruction *, InterestingUser> &)>
        op;
    op = [](const std::pair<SILInstruction *, InterestingUser> &pair)
        -> SILBasicBlock * { return pair.first->getParent(); };
    return UserBlockRange(getAllUsers(), op);
  }

  void initializeDefBlock(SILBasicBlock *defBB, TypeTreeLeafTypeRange span,
                          FieldSensitivePrunedLiveBlocks::IsLive isLive
                            = FieldSensitivePrunedLiveBlocks::LiveWithin) {
    assert(isInitialized());
    liveBlocks.initializeDefBlock(defBB, span.startEltOffset,
                                  span.endEltOffset, isLive);
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
                    bool lifetimeEnding,
                    SmallBitVector const &useBeforeDefBits);

  void updateForUse(SILInstruction *user, SmallBitVector const &bits,
                    bool lifetimeEnding,
                    SmallBitVector const &useBeforeDefBits);

  /// Adds \p user which doesn't use the def to liveness.
  ///
  /// Different from calling updateForUse because it never overrides the value
  /// \p lifetimeEnding stored for \p inst.
  void extendToNonUse(SILInstruction *user, TypeTreeLeafTypeRange span,
                      SmallBitVector const &useBeforeDefBits);

  void extendToNonUse(SILInstruction *user, SmallBitVector const &bits,
                      SmallBitVector const &useBeforeDefBits);

  void getBlockLiveness(SILBasicBlock *bb, TypeTreeLeafTypeRange span,
                        SmallVectorImpl<FieldSensitivePrunedLiveBlocks::IsLive>
                            &resultingFoundLiveness) const {
    liveBlocks.getBlockLiveness(bb, span.startEltOffset, span.endEltOffset,
                                resultingFoundLiveness);
  }

  void getBlockLiveness(SILBasicBlock *bb, SmallBitVector const &bits,
                        SmallVectorImpl<FieldSensitivePrunedLiveBlocks::IsLive>
                            &foundLivenessInfo) const {
    liveBlocks.getBlockLiveness(bb, bits, foundLivenessInfo);
  }

  /// Return the liveness for this specific sub-element of our root value.
  FieldSensitivePrunedLiveBlocks::IsLive
  getBlockLiveness(SILBasicBlock *bb, unsigned subElementNumber) const {
    return liveBlocks.getBlockLiveness(bb, subElementNumber);
  }

  void getBlockLiveness(SILBasicBlock *bb,
                        SmallVectorImpl<FieldSensitivePrunedLiveBlocks::IsLive>
                            &foundLiveness) const {
    liveBlocks.getBlockLiveness(bb, 0, liveBlocks.getNumBitsToTrack(),
                                foundLiveness);
  }

  void getBlockLiveness(SILBasicBlock *bb, SmallBitVector &liveWithinBits,
                        SmallBitVector &liveOutBits,
                        SmallBitVector &deadBits) const;

  InterestingUser &getOrCreateInterestingUser(SILInstruction *user) {
    auto iter = users.find(user);
    if (iter == users.end()) {
      iter = users.insert({user, InterestingUser(getNumSubElements())}).first;
    }
    return *&iter->second;
  }

  /// If \p user has had uses recorded, return a pointer to the InterestingUser
  /// where they've been recorded.
  InterestingUser const *getInterestingUser(SILInstruction *user) const {
    auto iter = users.find(user);
    if (iter == users.end())
      return nullptr;
    return &iter->second;
  }

  /// How \p user uses the field at \p element.
  IsInterestingUser isInterestingUser(SILInstruction *user,
                                      unsigned element) const {
    assert(isInitialized());
    auto *record = getInterestingUser(user);
    if (!record)
      return NonUser;
    return record->isInterestingUser(element);
  }

  /// Whether \p user uses the fields in \p bits as indicated by \p kind.
  bool isInterestingUserOfKind(SILInstruction *user, IsInterestingUser kind,
                               SmallBitVector const &bits) const {
    auto *record = getInterestingUser(user);
    if (!record) {
      return kind == IsInterestingUser::NonUser;
    }

    for (auto bit : bits.set_bits()) {
      if (record->isInterestingUser(bit) != kind)
        return false;
    }
    return true;
  }

  unsigned getNumSubElements() const { return liveBlocks.getNumBitsToTrack(); }

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

protected:
  /// Helper function used in FieldSensitivePrunedLiveness::updateForUse and
  /// FieldSensitivePrunedLiveRange::updateForUse. Please do not use directly!
  /// This is an implementation detail.
  ///
  /// Note that a user may use the current value from multiple operands. If any
  /// of the uses are non-lifetime-ending, then we must consider the user
  /// itself non-lifetime-ending; it cannot be a final destroy point because
  /// the value of the non-lifetime-ending operand must be kept alive until the
  /// end of the user. Consider a call that takes the same value using
  /// different conventions:
  ///
  ///   apply %f(%val, %val) : $(@guaranteed, @owned) -> ()
  ///
  /// This call is not considered the end of %val's lifetime. The @owned
  /// argument must be copied.
  void addInterestingUser(SILInstruction *user, TypeTreeLeafTypeRange range,
                          bool lifetimeEnding) {
    getOrCreateInterestingUser(user).addUses(range, lifetimeEnding);
  }

  void addInterestingUser(SILInstruction *user, SmallBitVector const &bits,
                          bool lifetimeEnding) {
    getOrCreateInterestingUser(user).addUses(bits, lifetimeEnding);
  }

  void extendToNonUse(SILInstruction *user, TypeTreeLeafTypeRange range) {
    getOrCreateInterestingUser(user).extendToNonUse(range);
  }

  void extendToNonUse(SILInstruction *user, SmallBitVector const &bits) {
    getOrCreateInterestingUser(user).extendToNonUse(bits);
  }
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
class FieldSensitivePrunedLivenessBoundary {
  llvm::SmallMapVector<SILInstruction *, SmallBitVector, 8> lastUsers;
  llvm::SmallMapVector<SILBasicBlock *, SmallBitVector, 8> boundaryEdges;
  llvm::SmallMapVector<SILNode *, SmallBitVector, 1> deadDefs;
  unsigned numBits;

public:
  FieldSensitivePrunedLivenessBoundary(unsigned numBits) : numBits(numBits) {}

  /// Soundness check meant for NDEBUG mode.
  unsigned getNumLastUsersAndDeadDefs(unsigned bitNo) const {
#ifdef NDEBUG
    llvm_unreachable("Only call in asserts build!\n");
#else
    unsigned count = 0;
    for (auto &pair : lastUsers) {
      count += unsigned(pair.second[bitNo]);
    }
    for (auto &pair : deadDefs) {
      count += unsigned(pair.second[bitNo]);
    }
    return count;
#endif
  }

  using LastUserRange = iterator_range<decltype(lastUsers)::iterator>;
  LastUserRange getLastUsers() {
    return make_range(lastUsers.begin(), lastUsers.end());
  }

  using BoundaryEdgeRange = iterator_range<decltype(boundaryEdges)::iterator>;
  BoundaryEdgeRange getBoundaryEdges() {
    return make_range(boundaryEdges.begin(), boundaryEdges.end());
  }

  using DeadDefRange = iterator_range<decltype(deadDefs)::iterator>;
  DeadDefRange getDeadDefs() {
    return make_range(deadDefs.begin(), deadDefs.end());
  }

  /// Helper entry point to get the last user that creates the correct size
  /// small bit vector if we haven't seen this last user yet.
  SmallBitVector &getLastUserBits(SILInstruction *inst) {
    auto iter = lastUsers.insert({inst, SmallBitVector()});
    if (iter.second) {
      iter.first->second.resize(numBits);
    }
    return iter.first->second;
  }

  SmallBitVector &getBoundaryEdgeBits(SILBasicBlock *block) {
    auto iter = boundaryEdges.insert({block, SmallBitVector()});
    if (iter.second) {
      iter.first->second.resize(numBits);
    }
    return iter.first->second;
  }

  SmallBitVector &getDeadDefsBits(SILNode *def) {
    assert(def->getParentBlock() && "Always expect to have a parent block!\n");
    auto iter = deadDefs.insert({def, SmallBitVector()});
    if (iter.second) {
      iter.first->second.resize(numBits);
    }
    return iter.first->second;
  }

  void clear() {
    lastUsers.clear();
    boundaryEdges.clear();
    deadDefs.clear();
  }

  void print(llvm::raw_ostream &os) const;
  void dump() const;
};

template <typename LivenessWithDefs>
class FieldSensitivePrunedLiveRange : public FieldSensitivePrunedLiveness {
  const LivenessWithDefs &asImpl() const {
    return reinterpret_cast<const LivenessWithDefs &>(*this);
  }

public:
  FieldSensitivePrunedLiveRange(
      SILFunction *fn,
      SmallVectorImpl<SILBasicBlock *> *discoveredBlocks = nullptr)
      : FieldSensitivePrunedLiveness(fn, discoveredBlocks) {}

  /// Check if \p inst occurs in between the definition of a def and the
  /// liveness boundary for \p bits.
  ///
  /// NOTE: It is assumed that \p inst is correctly described by \p bits.
  bool isWithinBoundary(SILInstruction *inst, SmallBitVector const &bits) const;

  /// Customize updateForUse for FieldSensitivePrunedLiveness such that we check
  /// that we consider defs as stopping liveness from being propagated up.
  void updateForUse(SILInstruction *user, TypeTreeLeafTypeRange span,
                    bool lifetimeEnding);

  /// Customize updateForUse for FieldSensitivePrunedLiveness such that we check
  /// that we consider defs as stopping liveness from being propagated up.
  void updateForUse(SILInstruction *user, SmallBitVector const &bits,
                    bool lifetimeEnding);

  /// Customize extendToNonUse for FieldSensitivePrunedLiveness to consider
  /// defs as kills.
  void extendToNonUse(SILInstruction *user, TypeTreeLeafTypeRange span);

  /// Customize extendToNonUse for FieldSensitivePrunedLiveness to consider
  /// defs as kills.
  void extendToNonUse(SILInstruction *user, SmallBitVector const &bits);

  /// Compute the boundary from the blocks discovered during liveness analysis.
  ///
  /// Precondition: \p liveness.getDiscoveredBlocks() is a valid list of all
  /// live blocks with no duplicates.
  ///
  /// The computed boundary will completely post-dominate, including dead end
  /// paths. The client should query DeadEndBlocks to ignore those dead end
  /// paths.
  void computeBoundary(FieldSensitivePrunedLivenessBoundary &boundary) const;
};

/// Single defined liveness.
///
// An SSA def results in pruned liveness with a contiguous liverange.
///
/// An unreachable self-loop might result in a "gap" between the last use above
/// the def in the same block.
///
/// For SSA live ranges, a single "def" block dominates all uses. If no def
/// block is provided, liveness is computed as if defined by a function
/// argument. If the client does not provide a single, dominating def block,
/// then the client must at least ensure that no uses precede the first
/// definition in a def block. Since this analysis does not remember the
/// positions of defs, it assumes that, within a block, uses follow
/// defs. Breaking this assumption will result in a "hole" in the live range in
/// which the def block's predecessors incorrectly remain dead. This situation
/// could be handled by adding an updateForUseBeforeFirstDef() API.
class FieldSensitiveSSAPrunedLiveRange
    : public FieldSensitivePrunedLiveRange<FieldSensitiveSSAPrunedLiveRange> {
  using Super = FieldSensitivePrunedLiveRange<FieldSensitiveSSAPrunedLiveRange>;

  std::pair<SILValue, std::optional<TypeTreeLeafTypeRange>> def = {{}, {}};

  /// None for arguments.
  std::pair<SILInstruction *, std::optional<TypeTreeLeafTypeRange>> defInst = {
      nullptr, std::nullopt};

public:
  FieldSensitiveSSAPrunedLiveRange(
      SILFunction *fn,
      SmallVectorImpl<SILBasicBlock *> *discoveredBlocks = nullptr)
      : FieldSensitivePrunedLiveRange(fn, discoveredBlocks) {}

  std::pair<SILValue, std::optional<TypeTreeLeafTypeRange>> getDef() const {
    assert(isInitialized());
    return def;
  }

  void clear() {
    def = {{}, {}};
    defInst = {{}, {}};
    FieldSensitivePrunedLiveRange::clear();
  }

  void initializeDef(SILValue def, TypeTreeLeafTypeRange span) {
    assert(Super::isInitialized());
    assert(!this->def.first && !this->def.second && "reinitialization");

    this->def = {def, span};
    defInst = {def->getDefiningInstruction(), span};
    initializeDefBlock(def->getParentBlock(), span);
  }

  bool isInitialized() const {
    return Super::isInitialized() && bool(def.first) && bool(def.second);
  }

  bool isDef(SILInstruction *inst, unsigned bit) const {
    return inst == defInst.first && defInst.second->contains(bit);
  }

  void isDef(SILInstruction *inst, SmallBitVector const &bits,
             SmallBitVector &bitsOut) const {
    assert(bitsOut.none());
    if (inst != defInst.first)
      return;
    defInst.second->setBits(bitsOut);
    bitsOut &= bits;
  }

  void isDef(SILInstruction *inst, TypeTreeLeafTypeRange span,
             SmallBitVector &bitsOut) const {
    assert(bitsOut.none());
    if (inst != defInst.first)
      return;
    auto intersection = defInst.second->setIntersection(span);
    if (!intersection.has_value())
      return;
    intersection.value().setBits(bitsOut);
  }

  bool isDefBlock(SILBasicBlock *block, unsigned bit) const {
    return def.first->getParentBlock() == block && def.second->contains(bit);
  }

  template <typename Iterable>
  void isUserBeforeDef(SILInstruction *user, Iterable const &iterable,
                       SmallBitVector &useBeforeDefBits) const {
    assert(useBeforeDefBits.none());
  }

  void
  findBoundariesInBlock(SILBasicBlock *block, unsigned bitNo, bool isLiveOut,
                        FieldSensitivePrunedLivenessBoundary &boundary) const;
};

static inline SILBasicBlock *getDefinedInBlock(SILNode *node) {
  // try_apply defines the value only on the success edge.
  if (auto ta = dyn_cast<TryApplyInst>(node)) {
    return ta->getNormalBB();
  }
  return node->getParentBlock();
}

/// MultiDefPrunedLiveness is computed incrementally by calling updateForUse.
///
/// Defs should be initialized before calling updatingForUse on any def
/// that reaches the use.
class FieldSensitiveMultiDefPrunedLiveRange
    : public FieldSensitivePrunedLiveRange<
          FieldSensitiveMultiDefPrunedLiveRange> {
  using Super =
      FieldSensitivePrunedLiveRange<FieldSensitiveMultiDefPrunedLiveRange>;

  // TODO: See if we can make this more efficient.
  SmallFrozenMultiMap<SILNode *, TypeTreeLeafTypeRange, 8> defs;
  SmallFrozenMultiMap<SILBasicBlock *, TypeTreeLeafTypeRange, 8> defBlocks;

public:
  FieldSensitiveMultiDefPrunedLiveRange(
      SILFunction *fn, SILValue rootValue,
      SmallVectorImpl<SILBasicBlock *> *discoveredBlocks = nullptr)
      : FieldSensitivePrunedLiveRange(fn, discoveredBlocks) {
    // We init here since we do not allow for reinitialization to occur.
    Super::init(rootValue);
  }

  void init(SILValue rootValue) {
    llvm_unreachable("multi-def liveness cannot be reused");
  }

  void clear() { llvm_unreachable("multi-def liveness cannot be reused"); }

  /// Call this when we have finished initializing defs and can begin to add
  /// liveness use information.
  ///
  /// Internally this freezes our def/defblocks arrays so we can use them as
  /// maps.
  void finishedInitializationOfDefs() {
    assert(isInitialized());
    defs.setFrozen();
    defBlocks.setFrozen();
  }

  void initializeDef(SILInstruction *def, SmallBitVector const &bits) {
    TypeTreeLeafTypeRange::visitContiguousRanges(
        bits, [&](auto range) { initializeDef(def, range); });
  }

  void initializeDef(SILNode *node, TypeTreeLeafTypeRange span) {
    assert(Super::isInitialized());
    defs.insert(node, span);
    auto defBlock = getDefinedInBlock(node);
    defBlocks.insert(defBlock, span);
    initializeDefBlock(defBlock, span);
    
    if (defBlock != node->getParentBlock()) {
      // If the block the value becomes defined in is different from the
      // defining instruction, then the def notionally occurs "on the edge"
      // between the instruction (which must be a terminator) and the defined-in
      // successor block. Mark the original block as a dead-to-live edge.
      auto ti = cast<TermInst>(node);
      
      assert(std::find(ti->getSuccessorBlocks().begin(),
                       ti->getSuccessorBlocks().end(),
                       defBlock) != ti->getSuccessorBlocks().end()
             && "defined-in block should be either the same block as the "
                "defining instruction or a successor of the "
                "defining terminator");

      initializeDefBlock(ti->getParent(), span,
                         FieldSensitivePrunedLiveBlocks::DeadToLiveEdge);
    }
  }

  void initializeDef(SILInstruction *def, TypeTreeLeafTypeRange span) {
    initializeDef(cast<SILNode>(def), span);
  }

  bool isInitialized() const { return Super::isInitialized() && !defs.empty(); }

  /// Return true if this block is a def block for this specific bit.
  bool isDefBlock(SILBasicBlock *block, unsigned bit) const {
    assert(isInitialized());
    auto iter = defBlocks.find(block);
    if (!iter)
      return false;
    return llvm::any_of(
        *iter, [&](TypeTreeLeafTypeRange span) { return span.contains(bit); });
  }

  void isDefBlock(SILBasicBlock *block, TypeTreeLeafTypeRange span,
                  SmallBitVector &bitsOut) const {
    assert(isInitialized());
    assert(bitsOut.none());
    auto iter = defBlocks.find(block);
    if (!iter)
      return;
    for (auto defSpan : *iter) {
      auto intersection = span.setIntersection(defSpan);
      if (!intersection.has_value())
        continue;
      intersection.value().setBits(bitsOut);
    }
  }

  void isDefBlock(SILBasicBlock *block, SmallBitVector const &bits,
                  SmallBitVector &bitsOut) const {
    assert(isInitialized());
    assert(bitsOut.none());
    auto iter = defBlocks.find(block);
    if (!iter)
      return;
    for (auto defSpan : *iter) {
      defSpan.setBits(bitsOut);
    }
    bitsOut &= bits;
  }

  /// Return true if \p user occurs before the first def in the same basic
  /// block. In classical liveness dataflow terms, gen/kill conditions over all
  /// users in 'bb' are:
  ///
  ///   Gen(bb)  |= !isDefBlock(bb) || isUserBeforeDef(bb)
  ///   Kill(bb) &= isDefBlock(bb) && !isUserBeforeDef(bb)
  ///
  /// If 'bb' has no users, it is neither a Gen nor Kill. Otherwise, Gen and
  /// Kill are complements.
  bool isUserBeforeDef(SILInstruction *user, unsigned element) const;
  template <typename Iterable>
  void isUserBeforeDef(SILInstruction *user, Iterable const &iterable,
                       SmallBitVector &useBeforeDefBits) const {
    for (auto bit : iterable) {
      if (isUserBeforeDef(user, bit)) {
        useBeforeDefBits.set(bit);
      }
    }
  }

  bool isDef(SILNode *node, unsigned bit) const {
    assert(isInitialized());
    auto iter = defs.find(node);
    if (!iter)
      return false;
    return llvm::any_of(
        *iter, [&](TypeTreeLeafTypeRange span) { return span.contains(bit); });
  }

  bool isDef(SILInstruction *inst, unsigned bit) const {
    return isDef(cast<SILNode>(inst), bit);
  }

  bool isDef(SILValue value, unsigned bit) const {
    return isDef(cast<SILNode>(value), bit);
  }

  void isDef(SILNode *node, SmallBitVector const &bits,
             SmallBitVector &bitsOut) const {
    assert(isInitialized());
    assert(bitsOut.none());
    auto iter = defs.find(node);
    if (!iter)
      return;
    for (auto range : *iter) {
      range.setBits(bitsOut);
    }
    bitsOut &= bits;
  }

  void isDef(SILValue value, SmallBitVector const &bits,
             SmallBitVector &bitsOut) const {
    isDef(cast<SILNode>(value), bits, bitsOut);
  }

  void isDef(SILInstruction *inst, SmallBitVector const &bits,
             SmallBitVector &bitsOut) const {
    isDef(cast<SILNode>(inst), bits, bitsOut);
  }

  void isDef(SILNode *node, TypeTreeLeafTypeRange span,
             SmallBitVector &bitsOut) const {
    assert(isInitialized());
    assert(bitsOut.none());
    auto iter = defs.find(node);
    if (!iter)
      return;
    for (auto defSpan : *iter) {
      auto intersection = span.setIntersection(defSpan);
      if (!intersection.has_value())
        continue;
      span.setBits(bitsOut);
    }
  }

  void isDef(SILInstruction *inst, TypeTreeLeafTypeRange span,
             SmallBitVector &bitsOut) const {
    return isDef(cast<SILNode>(inst), span, bitsOut);
  }

  void isDef(SILValue value, TypeTreeLeafTypeRange span,
             SmallBitVector &bitsOut) const {
    return isDef(cast<SILNode>(value), span, bitsOut);
  }

  void
  findBoundariesInBlock(SILBasicBlock *block, unsigned bitNo, bool isLiveOut,
                        FieldSensitivePrunedLivenessBoundary &boundary) const;

  /// Walk from \p inst until we find a def for \p index. If we see a consuming
  /// use, call \p callback. If \p callback returns true, then this is not the
  /// consuming use we are looking for and we should keep on
  /// searching. Otherwise, if it returns false, we bail early and return
  /// false. If we find a def, we return true. If we stopped due to a consuming
  /// use, we return false.
  bool findEarlierConsumingUse(
      SILInstruction *inst, unsigned index,
      llvm::function_ref<bool(SILInstruction *)> callback) const;
};

} // namespace swift

#endif
