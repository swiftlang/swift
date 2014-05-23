//===- ReferenceCountState.h - Represents a Reference Count -----*- C++ -*-===//
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

#ifndef SWIFT_SILANALYSIS_REFERENCECOUNTSTATE_H
#define SWIFT_SILANALYSIS_REFERENCECOUNTSTATE_H

#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include <algorithm>

namespace swift {
  class AliasAnalysis;
} // end namespace swift

namespace swift {
namespace arc {

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

/// Is I an instruction that we recognize as a "reference count increment"
/// instruction?
static inline bool isRefCountIncrement(SILInstruction &I) {
  return isa<StrongRetainInst>(I) || isa<RetainValueInst>(I);
}

/// Is I an instruction that we recognize as a "reference count decrement"
/// instruction?
static inline bool isRefCountDecrement(SILInstruction &I) {
  return isa<StrongReleaseInst>(I) || isa<ReleaseValueInst>(I);
}

/// Returns true if Inc and Dec are compatible reference count instructions.
///
/// In more specific terms this means that means a (strong_retain,
/// strong_release) pair or a (retain_value, release_value) pair.
static inline bool matchingRefCountPairType(SILInstruction *I1,
                                            SILInstruction *I2) {
  // Always put the strong retain on the left.
  if (isa<StrongRetainInst>(I2) || isa<RetainValueInst>(I2))
    std::swap(I1, I2);

  // Now we know that I1 should be an increment and I2 a decrement. Assert to
  // check for programmer error.
  assert(isRefCountIncrement(*I1) && isRefCountDecrement(*I2) &&
         "Submitting two increments or decrements to this function is "
         "invalid.");
  return (isa<StrongRetainInst>(I1) && isa<StrongReleaseInst>(I2)) ||
    (isa<RetainValueInst>(I1) && isa<ReleaseValueInst>(I2));
}

//===----------------------------------------------------------------------===//
//                              Ref Count State
//===----------------------------------------------------------------------===//

/// A struct that abstracts over reference counts manipulated by strong_retain,
/// retain_value, strong_release,
template <typename ImplStruct>
struct RefCountState {
  /// Was the pointer we are tracking known incremented when we visited the
  /// current increment we are tracking? In that case we know that it is safe
  /// to move the inner retain over instructions that may decrement ref counts
  /// since the outer retain will keep the reference counted value alive.
  bool KnownSafe = false;

  /// The latest point we can move Instruction without moving it over an
  /// instruction that might be able to decrement the value with reference
  /// semantics.
  SILInstruction *InsertPt = nullptr;

  /// Return this as the CRTP substruct.
  ImplStruct *asImpl() { return static_cast<ImplStruct *>(this); }
  ImplStruct *asImpl() const {
    return const_cast<RefCountState<ImplStruct> *>(this)->asImpl();
  }

  RefCountState() {}
  ~RefCountState() {}

  /// Initializes/reinitialized the state for I. If we reinitialize we return
  /// true.
  bool initWithInst(SILInstruction *I) {
    // Are we already tracking a ref count modification?
    bool Nested = isTrackingRefCount();

    // This retain is known safe if the operand we are tracking was already
    // known incremented previously. This occurs when you have nested
    // increments.
    KnownSafe = isRefCountStateModified();

    // Reset our insertion point to null.
    InsertPt = nullptr;

    return Nested;
  }

  /// Uninitialize the current state.
  void clear() {
    asImpl()->clear();
    KnownSafe = false;
    InsertPt = nullptr;
  }

  /// Can we gaurantee that the given reference counted value has been modified?
  bool isRefCountStateModified() const {
    return asImpl()->isRefCountStateModified();
  }

  /// Is this ref count initialized and tracking a ref count ptr.
  bool isTrackingRefCount() const {
    return asImpl()->isTrackingRefCount();
  }

  /// Are we tracking an instruction currently? This returns false when given an
  /// uninitialized ReferenceCountState.
  bool isTrackingRefCountInst() const {
    return asImpl()->isTrackingRefCountInst();
  }

  /// Are we tracking a source of ref counts? This currently means that we are
  /// tracking an argument that is @owned. In the future this will include
  /// return values of functions that are @owned.
  bool isTrackingRefCountSource() const {
    return asImpl()->isTrackingRefCountSource();
  }

  /// Return the increment we are tracking.
  SILInstruction *getInstruction() const {
   return asImpl()->getInstruction();
  }

  /// Return the value with reference semantics that is the operand of our
  /// increment.
  SILValue getValue() const {
    return getInstruction()->getOperand(0).stripCasts();
  }

  /// The latest point we can move the increment without bypassing instructions
  /// that may have reference semantics.
  SILInstruction *getInsertPoint() const { return InsertPt; }

  /// This retain is known safe if the operand we are tracking was already
  /// known incremented previously. This occurs when you have nested
  /// increments.
  bool isKnownSafe() const { return KnownSafe; }

  /// Check if PotentialDecrement can decrement the reference count associated
  /// with the value we are tracking. If so advance the state's sequence
  /// appropriately and return true. Otherwise return false.
  bool handlePotentialDecrement(SILInstruction *PotentialDecrement,
                                AliasAnalysis *AA) {
    // If we are not tracking a ref count, just return false.
    if (!isTrackingRefCountInst())
      return false;

    // If we can prove that Other can not use the pointer we are tracking,
    // return...
    if (!canDecrementRefCount(PotentialDecrement, getValue(), AA))
      return false;

    // Otherwise, allow the CRTP substruct to update itself given we have a
    // potential decrement.
    return asImpl()->handleDecrement(PotentialDecrement);
  }

  /// Check if PotentialUser could be a use of the reference counted value that
  /// requires user to be alive. If so advance the state's sequence
  /// appropriately and return true. Otherwise return false.
  bool handlePotentialUser(SILInstruction *PotentialUser, AliasAnalysis *AA) {
    // If we are not tracking a ref count, just return false.
    if (!isTrackingRefCount())
      return false;

    if (!canUseValue(PotentialUser, getValue(), AA))
      return false;

    return asImpl()->handleUser(PotentialUser);
  }

  /// Returns true if the passed in ref count inst matches the ref count inst
  /// we are tracking. This handles generically retains/release.
  bool isRefCountInstMatchedToTrackedInstruction(SILInstruction *RefCountInst) {
    // If our state is not initialized, return false since we are not tracking
    // anything. If the ValueKind of our increments, decrement do not match, we
    // can not eliminate the pair so return false.
    if (!isTrackingRefCountInst() ||
        !matchingRefCountPairType(getInstruction(), RefCountInst))
      return false;
    return asImpl()->handleRefCountInstMatch(RefCountInst);
  }
};

//===----------------------------------------------------------------------===//
//                         Bottom Up Ref Count State
//===----------------------------------------------------------------------===//

struct BottomUpRefCountState : public RefCountState<BottomUpRefCountState> {
  using Super = RefCountState<BottomUpRefCountState>;

  /// Sequence of states that a value with reference semantics can go through
  /// when visiting decrements bottom up. The reason why I have this separate
  /// from TopDownSubstruct is I think it gives more clarity to the algorithm
  /// by giving it typed form.
  enum struct LatticeState {
    None,               ///< The pointer has no information associated with it.
    Decremented,        ///< The pointer will be decremented.
    MightBeUsed,        ///< The pointer will be used and then at this point
                        ///  be decremented
    MightBeDecremented, ///< The pointer might be decremented again implying
                        ///  that we can not, without being known safe remove
                        ///  this decrement.
  };

  /// The decrement that we are tracking.
  NullablePtr<SILInstruction> Decrement;

  /// Current place in the sequence of the value.
  LatticeState LatState = LatticeState::None;

  /// Initializes/reinitialized the state for I. If we reinitialize we return
  /// true.
  bool initWithInst(SILInstruction *I) {
    assert((isa<StrongReleaseInst>(I) || isa<ReleaseValueInst>(I)) &&
           "strong_release and release_value are only supported.");

    bool NestingDetected = Super::initWithInst(I);

    // Set increment to I.
    Decrement = I;

    // Set our lattice state to be incremented.
    LatState = LatticeState::Decremented;

    return NestingDetected;
  }

  /// Uninitialize the current state.
  void clear() {
    Decrement = nullptr;
    LatState = LatticeState::None;
  }

  /// Can we gaurantee that the given reference counted value has been modified?
  bool isRefCountStateModified() const {
    return LatState == LatticeState::Decremented;
  }

  /// Is this ref count initialized and tracking a ref count ptr.
  bool isTrackingRefCount() const {
    return !Decrement.isNull();
  }

  /// Are we tracking an instruction currently? This returns false when given an
  /// uninitialized ReferenceCountState.
  bool isTrackingRefCountInst() const {
    return !Decrement.isNull();
  }

  /// Are we tracking a source of ref counts? This currently means that we are
  /// tracking an argument that is @owned. In the future this will include
  /// return values of functions that are @owned.
  bool isTrackingRefCountSource() const {
    return false;
  }

  /// Return the increment we are tracking.
  SILInstruction *getInstruction() const {
    return const_cast<BottomUpRefCountState *>(this)->Decrement.get();
  }

  /// If advance the state's sequence appropriately for a decrement. If we do
  /// advance return true. Otherwise return false.
  bool handleDecrement(SILInstruction *PotentialDecrement) {
    switch (LatState) {
      case LatticeState::MightBeUsed:
        LatState = LatticeState::MightBeDecremented;
        return true;
      case LatticeState::None:
      case LatticeState::MightBeDecremented:
      case LatticeState::Decremented:
        return false;
    }
  }

  /// Given the current lattice state, if we have seen a use, advance the
  /// lattice state. Return true if we do so and false otherwise.
  bool handleUser(SILInstruction *PotentialUser) {
    // Advance the sequence...
    switch (LatState) {
      case LatticeState::Decremented:
        LatState = LatticeState::MightBeUsed;
        assert(!InsertPt && "Insert Point should be empty at this point.");
        InsertPt = std::next(SILBasicBlock::iterator(PotentialUser));
        return true;
      case LatticeState::MightBeUsed:
      case LatticeState::MightBeDecremented:
      case LatticeState::None:
        return false;
    }
  }

  /// We have a matching ref count inst. Return true if we advance the sequence
  /// and false otherwise.
  bool handleRefCountInstMatch(SILInstruction *RefCountInst) {
    // Otherwise modify the state appropriately in preparation for removing the
    // increment, decrement pair.
    switch (LatState) {
      case LatticeState::None:
        return false;
      case LatticeState::Decremented:
      case LatticeState::MightBeUsed:
        // Unset InsertPt so we remove retain release pairs instead of
        // performing code motion.
        InsertPt = nullptr;
        SWIFT_FALLTHROUGH;
      case LatticeState::MightBeDecremented:
        return true;
    }
  }
};

//===----------------------------------------------------------------------===//
//                          Top Down Ref Count State
//===----------------------------------------------------------------------===//

struct TopDownRefCountState : public RefCountState<TopDownRefCountState> {
  using Super = RefCountState<TopDownRefCountState>;

  /// Sequence of states that a value with reference semantics can go through
  /// when visiting decrements bottom up. The reason why I have this separate
  /// from BottomUpRefCountState is I think it gives more clarity to the
  /// algorithm by giving it typed form.
  enum struct LatticeState {
    None,               ///< The pointer has no information associated with it.
    Incremented,        ///< The pointer has been incremented.
    MightBeDecremented, ///< The pointer has been incremented and might be
                        ///  decremented. be decremented again implying
    MightBeUsed,        ///< The pointer has been incremented,
  };

  /// The increment that we are tracking.
  llvm::PointerUnion<SILInstruction *, SILArgument *> Increment;

  /// Current place in the sequence of the value.
  LatticeState LatState = LatticeState::None;

  /// Initializes/reinitialized the state for I. If we reinitialize we return
  /// true.
  bool initWithInst(SILInstruction *I) {
    assert((isa<StrongRetainInst>(I) || isa<RetainValueInst>(I)) &&
           "strong_retain and retain_value are only supported.");

    bool NestingDetected = Super::initWithInst(I);

    // Set increment to I.
    Increment = I;

    // Set our lattice state to be incremented.
    LatState = LatticeState::Incremented;

    return NestingDetected;
  }

  void initWithArg(SILArgument *Arg) {
    LatState = LatticeState::Incremented;
    Increment = Arg;
    InsertPt = nullptr;
    KnownSafe = false;
  }

  /// Uninitialize the current state.
  void clear() {
    Increment = llvm::PointerUnion<SILInstruction *, SILArgument *>();
    LatState = LatticeState::None;
  }

  /// Can we gaurantee that the given reference counted value has been modified?
  bool isRefCountStateModified() const {
    return LatState == LatticeState::Incremented;
  }

  /// Is this ref count initialized and tracking a ref count ptr.
  bool isTrackingRefCount() const {
    return !Increment.isNull();
  }

  /// Are we tracking an instruction currently? This returns false when given an
  /// uninitialized ReferenceCountState.
  bool isTrackingRefCountInst() const {
    return Increment && Increment.is<SILInstruction *>();
  }

  /// Are we tracking a source of ref counts? This currently means that we are
  /// tracking an argument that is @owned. In the future this will include
  /// return values of functions that are @owned.
  bool isTrackingRefCountSource() const {
    return Increment && Increment.is<SILArgument *>();
  }

  /// Return the increment we are tracking.
  SILInstruction *getInstruction() const {
    return Increment.get<SILInstruction *>();
  }

  /// If advance the state's sequence appropriately for a decrement. If we do
  /// advance return true. Otherwise return false.
  bool handleDecrement(SILInstruction *PotentialDecrement) {
    switch (LatState) {
      case LatticeState::Incremented:
        LatState = LatticeState::MightBeDecremented;
        InsertPt = PotentialDecrement;
        return true;
      case LatticeState::None:
      case LatticeState::MightBeDecremented:
      case LatticeState::MightBeUsed:
        return false;
    }
  }

  /// Given the current lattice state, if we have seen a use, advance the
  /// lattice state. Return true if we do so and false otherwise.
  bool handleUser(SILInstruction *PotentialUser) {
    // Otherwise advance the sequence...
    switch (LatState) {
      case LatticeState::MightBeDecremented:
        LatState = LatticeState::MightBeUsed;
        return true;
      case LatticeState::Incremented:
      case LatticeState::None:
      case LatticeState::MightBeUsed:
        return false;
    }
  }

  /// We have a matching ref count inst. Return true if we advance the sequence
  /// and false otherwise.
  bool handleRefCountInstMatch(SILInstruction *RefCountInst) {
    // Otherwise modify the state appropriately in preparation for removing the
    // increment, decrement pair.
    switch (LatState) {
      case LatticeState::None:
        return false;
      case LatticeState::Incremented:
      case LatticeState::MightBeDecremented:
        // Unset InsertPt so we remove retain release pairs instead of performing
        // code motion.
        InsertPt = nullptr;
        SWIFT_FALLTHROUGH;
      case LatticeState::MightBeUsed:
        return true;
    }
  }
};

} // end arc namespace
} // end swift namespace

#endif
