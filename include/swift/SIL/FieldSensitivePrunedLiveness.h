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

#ifndef SWIFT_SIL_FIELDSENSITIVEPRUNTEDLIVENESS_H
#define SWIFT_SIL_FIELDSENSITIVEPRUNTEDLIVENESS_H

#include "swift/SIL/PrunedLiveness.h"

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
/// Enums and Partial Initialization
/// --------------------------------
///
/// One property of our representation of structs and tuples is that a code
/// generator can reinitialize a struct/tuple completely just by re-initializing
/// each of its sub-types individually. This is not possible for enums in our
/// representation since if one just took the leaf nodes for the payload, one
/// would not update the bit for the enum case itself and any additional spare
/// bits. Luckily for us, this is actually impossible to do in SIL since it is
/// impossible to dynamically change the payload of an enum without destroying
/// the original enum and its payload since that would be a verifier caught
/// leak.
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

private:
  TypeTreeLeafTypeRange(SubElementNumber start, SubElementNumber end)
      : startEltOffset(start), endEltOffset(end) {}

public:
  /// The leaf type range for the entire type tree.
  TypeTreeLeafTypeRange(SILValue rootAddress)
      : startEltOffset(0), endEltOffset(TypeSubElementCount(rootAddress)) {}

  /// The leaf type sub-range of the type tree of \p rootAddress, consisting of
  /// \p projectedAddress and all of \p projectedAddress's descendent fields in
  /// the type tree.
  ///
  /// \returns None if we are unable to understand the path in between \p
  /// projectedAddress and \p rootAddress.
  static Optional<TypeTreeLeafTypeRange> get(SILValue projectedAddress,
                                             SILValue rootAddress) {
    auto startEltOffset =
        SubElementNumber::compute(projectedAddress, rootAddress);
    if (!startEltOffset)
      return None;
    return {{*startEltOffset,
             *startEltOffset + TypeSubElementCount(projectedAddress)}};
  }

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
