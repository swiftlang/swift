//===--- RefCountState.h - Represents a Reference Count ---------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_PASSMANAGER_ARC_REFCOUNTSTATE_H
#define SWIFT_SILOPTIMIZER_PASSMANAGER_ARC_REFCOUNTSTATE_H

#include "RCStateTransition.h"
#include "swift/Basic/type_traits.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/ImmutablePointerSet.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/EpilogueARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include <algorithm>

namespace swift {
class AliasAnalysis;
} // end namespace swift

//===----------------------------------------------------------------------===//
//                              Ref Count State
//===----------------------------------------------------------------------===//

namespace swift {

/// A struct that abstracts over reference counts manipulated by strong_retain,
/// retain_value, strong_release,
class RefCountState {
protected:
  /// Return the SILValue that represents the RCRoot that we are
  /// tracking.
  SILValue RCRoot;

  /// The last state transition that this RefCountState went through. None if we
  /// have not see any transition on this ref count yet.
  RCStateTransition Transition;

  /// Was the pointer we are tracking known incremented when we visited the
  /// current increment we are tracking? In that case we know that it is safe
  /// to move the inner retain over instructions that may decrement ref counts
  /// since the outer retain will keep the reference counted value alive.
  bool KnownSafe = false;

public:
  RefCountState() = default;
  ~RefCountState() = default;
  RefCountState(const RefCountState &) = default;
  RefCountState &operator=(const RefCountState &) = default;
  RefCountState(RefCountState &&) = default;
  RefCountState &operator=(RefCountState &&) = default;

  /// Initializes/reinitialized the state for I. If we reinitialize we return
  /// true.
  bool initWithMutatorInst(ImmutablePointerSet<SILInstruction> *I,
                           RCIdentityFunctionInfo *RCFI) {
    assert(I->size() == 1);

    // Are we already tracking a ref count modification?
    bool Nested = isTrackingRefCount();

    Transition = RCStateTransition(I);
    assert(Transition.isMutator() && "Expected I to be a mutator!\n");

    // Initialize KnownSafe to a conservative false value.
    KnownSafe = false;

    // Initialize value.
    RCRoot = RCFI->getRCIdentityRoot((*I->begin())->getOperand(0));

    return Nested;
  }

  /// Uninitialize the current state.
  void clear() {
    KnownSafe = false;
  }

  /// Is this ref count initialized and tracking a ref count ptr.
  bool isTrackingRefCount() const { return Transition.isValid(); }

  /// Are we tracking an instruction currently? This returns false when given an
  /// uninitialized ReferenceCountState.
  bool isTrackingRefCountInst() const {
    return Transition.isValid() && Transition.isMutator();
  }

  /// Are we tracking a source of ref counts? This currently means that we are
  /// tracking an argument that is @owned. In the future this will include
  /// return values of functions that are @owned.
  bool isTrackingRefCountSource() const {
    return Transition.isValid() && Transition.isEndPoint();
  }

  /// Return the increment we are tracking.
  RCStateTransition::mutator_range getInstructions() const {
    return Transition.getMutators();
  }

  /// Returns true if I is in the instructions we are tracking.
  bool containsInstruction(SILInstruction *I) const {
    return Transition.isValid() && Transition.containsMutator(I);
  }

  /// Return the value with reference semantics that is the operand of our
  /// increment.
  SILValue getRCRoot() const {
    assert(RCRoot && "Value should never be null here");
    return RCRoot;
  }

  /// Returns true if we have a valid value that we are tracking.
  bool hasRCRoot() const {
    return (bool)RCRoot;
  }

  /// This retain is known safe if the operand we are tracking was already known
  /// incremented previously. This occurs when you have nested increments.
  bool isKnownSafe() const { return KnownSafe; }

  /// Set KnownSafe to true if \p NewValue is true. If \p NewValue is false,
  /// this is a no-op.
  void updateKnownSafe(bool NewValue) {
    KnownSafe |= NewValue;
  }
};

//===----------------------------------------------------------------------===//
//                         Bottom Up Ref Count State
//===----------------------------------------------------------------------===//

class BottomUpRefCountState : public RefCountState {
public:
  /// Sequence of states that a value with reference semantics can go through
  /// when visiting decrements bottom up. The reason why I have this separate
  /// from TopDownSubstruct is I think it gives more clarity to the algorithm by
  /// giving it typed form.
  enum class LatticeState {
    None,               ///< The pointer has no information associated with it.
    Decremented,        ///< The pointer will be decremented.
    MightBeUsed,        ///< The pointer will be used and then at this point
                        ///  be decremented
    MightBeDecremented, ///< The pointer might be decremented again implying
                        ///  that we cannot, without being known safe remove
                        ///  this decrement.
  };

private:
  using SuperTy = RefCountState;

  /// Current place in the sequence of the value.
  LatticeState LatState = LatticeState::None;

  /// True if we have seen a NonARCUser of this instruction. This means bottom
  /// up assuming we have this property as a meet over all paths property, we
  /// know that all releases we see are known safe.
  bool FoundNonARCUser = false;

public:
  BottomUpRefCountState() = default;
  ~BottomUpRefCountState() = default;
  BottomUpRefCountState(const BottomUpRefCountState &) = default;
  BottomUpRefCountState &operator=(const BottomUpRefCountState &) = default;
  BottomUpRefCountState(BottomUpRefCountState &&) = default;
  BottomUpRefCountState &operator=(BottomUpRefCountState &&) = default;

  /// Return true if the release can be moved to the retain.
  bool isCodeMotionSafe() const {
    return LatState != LatticeState::MightBeDecremented;
  }

  /// Initializes/reinitialized the state for I. If we reinitialize we return
  /// true.
  bool initWithMutatorInst(ImmutablePointerSet<SILInstruction> *I,
                           RCIdentityFunctionInfo *RCFI);

  /// Update this reference count's state given the instruction \p I. \p
  /// InsertPt is the point furthest up the CFG where we can move the currently
  /// tracked reference count.
  void
  updateForSameLoopInst(SILInstruction *I,
                        ImmutablePointerSetFactory<SILInstruction> &SetFactory,
                        AliasAnalysis *AA);

  /// Update this reference count's state given the instruction \p I. \p
  /// InsertPts are the points furthest up the CFG where we can move the
  /// currently tracked reference count.
  //
  /// The main difference in between this routine and update for same loop inst
  /// is that if we see any decrements on a value, we treat it as being
  /// guaranteed used. We treat any uses as regular uses.
  void updateForDifferentLoopInst(
      SILInstruction *I,
      ImmutablePointerSetFactory<SILInstruction> &SetFactory,
      AliasAnalysis *AA);

  // Determine the conservative effect of the given list of predecessor
  // terminators upon this reference count.
  void updateForPredTerminators(
      ArrayRef<SILInstruction *> PredTerms,
      ImmutablePointerSetFactory<SILInstruction> &SetFactory,
      AliasAnalysis *AA);

  /// Attempt to merge \p Other into this ref count state. Return true if we
  /// succeed and false otherwise.
  bool merge(const BottomUpRefCountState &Other);

  /// Returns true if the passed in ref count inst matches the ref count inst
  /// we are tracking. This handles generically retains/release.
  bool isRefCountInstMatchedToTrackedInstruction(SILInstruction *RefCountInst);

  /// Uninitialize the current state.
  void clear();

private:
  /// Return true if we *might* remove this instruction.
  ///
  /// This is a conservative query given the information we know, so as we
  /// perform the dataflow it may change value.
  bool mightRemoveMutators();

  /// Can we guarantee that the given reference counted value has been modified?
  bool isRefCountStateModified() const;

  /// Returns true if given the current lattice state, do we care if the value
  /// we are tracking is decremented.
  bool valueCanBeDecrementedGivenLatticeState() const;

  /// If advance the state's sequence appropriately for a decrement. If we do
  /// advance return true. Otherwise return false.
  bool handleDecrement();

  /// Check if PotentialDecrement can decrement the reference count associated
  /// with the value we are tracking. If so advance the state's sequence
  /// appropriately and return true. Otherwise return false.
  bool handlePotentialDecrement(SILInstruction *Decrement, AliasAnalysis *AA);

  /// Returns true if given the current lattice state, do we care if the value
  /// we are tracking is used.
  bool valueCanBeUsedGivenLatticeState() const;

  /// Given the current lattice state, if we have seen a use, advance the
  /// lattice state. Return true if we do so and false otherwise. \p InsertPt is
  /// the location where if \p PotentialUser is a user of this ref count, we
  /// would insert a release.
  bool handleUser(SILValue RCIdentity,
                  ImmutablePointerSetFactory<SILInstruction> &SetFactory,
                  AliasAnalysis *AA);

  /// Check if PotentialUser could be a use of the reference counted value that
  /// requires user to be alive. If so advance the state's sequence
  /// appropriately and return true. Otherwise return false.
  bool
  handlePotentialUser(SILInstruction *PotentialUser,
                      ImmutablePointerSetFactory<SILInstruction> &SetFactory,
                      AliasAnalysis *AA);

  /// Returns true if given the current lattice state, do we care if the value
  /// we are tracking is used.
  bool valueCanBeGuaranteedUsedGivenLatticeState() const;

  /// Given the current lattice state, if we have seen a use, advance the
  /// lattice state. Return true if we do so and false otherwise. \p InsertPt is
  /// the location where if \p PotentialUser is a user of this ref count, we
  /// would insert a release.
  bool
  handleGuaranteedUser(SILValue RCIdentity,
                       ImmutablePointerSetFactory<SILInstruction> &SetFactory,
                       AliasAnalysis *AA);

  /// Check if PotentialGuaranteedUser can use the reference count associated
  /// with the value we are tracking. If so advance the state's sequence
  /// appropriately and return true. Otherwise return false.
  bool handlePotentialGuaranteedUser(
      SILInstruction *User,
      ImmutablePointerSetFactory<SILInstruction> &SetFactory,
      AliasAnalysis *AA);

  /// We have a matching ref count inst. Return true if we advance the sequence
  /// and false otherwise.
  bool handleRefCountInstMatch(SILInstruction *RefCountInst);
};

//===----------------------------------------------------------------------===//
//                          Top Down Ref Count State
//===----------------------------------------------------------------------===//

class TopDownRefCountState : public RefCountState {
public:
  /// Sequence of states that a value with reference semantics can go through
  /// when visiting decrements bottom up. The reason why I have this separate
  /// from BottomUpRefCountState is I think it gives more clarity to the
  /// algorithm by giving it typed form.
  enum class LatticeState {
    None,               ///< The pointer has no information associated with it.
    Incremented,        ///< The pointer has been incremented.
    MightBeDecremented, ///< The pointer has been incremented and might be
                        ///  decremented. be decremented again implying
    MightBeUsed,        ///< The pointer has been incremented,
  };

private:
  using SuperTy = RefCountState;

  /// Current place in the sequence of the value.
  LatticeState LatState = LatticeState::None;

public:
  TopDownRefCountState() = default;
  ~TopDownRefCountState() = default;
  TopDownRefCountState(const TopDownRefCountState &) = default;
  TopDownRefCountState &operator=(const TopDownRefCountState &) = default;
  TopDownRefCountState(TopDownRefCountState &&) = default;
  TopDownRefCountState &operator=(TopDownRefCountState &&) = default;

  /// Return true if the retain can be moved to the release.
  bool isCodeMotionSafe() const {
    return LatState != LatticeState::MightBeUsed;
  }

  /// Initializes/reinitialized the state for I. If we reinitialize we return
  /// true.
  bool initWithMutatorInst(ImmutablePointerSet<SILInstruction> *I,
                           RCIdentityFunctionInfo *RCFI);

  /// Initialize the state given the consumed argument Arg.
  void initWithArg(SILFunctionArgument *Arg);

  /// Initialize this RefCountState with an instruction which introduces a new
  /// ref count at +1.
  void initWithEntranceInst(ImmutablePointerSet<SILInstruction> *I,
                            SILValue RCIdentity);

  /// Uninitialize the current state.
  void clear();

  /// Update this reference count's state given the instruction \p I. \p
  /// InsertPt is the point furthest up the CFG where we can move the currently
  /// tracked reference count.
  void
  updateForSameLoopInst(SILInstruction *I,
                        ImmutablePointerSetFactory<SILInstruction> &SetFactory,
                        AliasAnalysis *AA);

  /// Update this reference count's state given the instruction \p I. \p
  /// InsertPts are the points furthest up the CFG where we can move the
  /// currently tracked reference count.
  ///
  /// The main difference in between this routine and update for same loop inst
  /// is that if we see any decrements on a value, we treat it as being
  /// guaranteed used. We treat any uses as regular uses.
  void updateForDifferentLoopInst(
      SILInstruction *I,
      ImmutablePointerSetFactory<SILInstruction> &SetFactory,
      AliasAnalysis *AA);

  /// Returns true if the passed in ref count inst matches the ref count inst
  /// we are tracking. This handles generically retains/release.
  bool isRefCountInstMatchedToTrackedInstruction(SILInstruction *RefCountInst);

  /// Attempt to merge \p Other into this ref count state. Return true if we
  /// succeed and false otherwise.
  bool merge(const TopDownRefCountState &Other);

private:
  /// Can we guarantee that the given reference counted value has been modified?
  bool isRefCountStateModified() const;

  /// Returns true if given the current lattice state, do we care if the value
  /// we are tracking is decremented.
  bool valueCanBeDecrementedGivenLatticeState() const;

  /// If advance the state's sequence appropriately for a decrement. If we do
  /// advance return true. Otherwise return false.
  bool handleDecrement(SILInstruction *PotentialDecrement,
                       ImmutablePointerSetFactory<SILInstruction> &SetFactory);

  /// Check if PotentialDecrement can decrement the reference count associated
  /// with the value we are tracking. If so advance the state's sequence
  /// appropriately and return true. Otherwise return false.
  bool handlePotentialDecrement(
      SILInstruction *PotentialDecrement,
      ImmutablePointerSetFactory<SILInstruction> &SetFactory,
      AliasAnalysis *AA);

  /// Returns true if given the current lattice state, do we care if the value
  /// we are tracking is used.
  bool valueCanBeUsedGivenLatticeState() const;

  /// Given the current lattice state, if we have seen a use, advance the
  /// lattice state. Return true if we do so and false otherwise.
  bool handleUser(SILInstruction *PotentialUser,
                  SILValue RCIdentity, AliasAnalysis *AA);

  /// Check if PotentialUser could be a use of the reference counted value that
  /// requires user to be alive. If so advance the state's sequence
  /// appropriately and return true. Otherwise return false.
  bool handlePotentialUser(SILInstruction *PotentialUser, AliasAnalysis *AA);

  /// Returns true if given the current lattice state, do we care if the value
  /// we are tracking is used.
  bool valueCanBeGuaranteedUsedGivenLatticeState() const;

  /// Given the current lattice state, if we have seen a use, advance the
  /// lattice state. Return true if we do so and false otherwise.
  bool
  handleGuaranteedUser(SILInstruction *PotentialGuaranteedUser,
                       SILValue RCIdentity,
                       ImmutablePointerSetFactory<SILInstruction> &SetFactory,
                       AliasAnalysis *AA);

  /// Check if PotentialGuaranteedUser can use the reference count associated
  /// with the value we are tracking. If so advance the state's sequence
  /// appropriately and return true. Otherwise return false.
  bool handlePotentialGuaranteedUser(
      SILInstruction *PotentialGuaranteedUser,
      ImmutablePointerSetFactory<SILInstruction> &SetFactory,
      AliasAnalysis *AA);

  /// We have a matching ref count inst. Return true if we advance the sequence
  /// and false otherwise.
  bool handleRefCountInstMatch(SILInstruction *RefCountInst);
};

// These static asserts are here for performance reasons.
static_assert(IsTriviallyCopyable<BottomUpRefCountState>::value,
              "All ref count states must be trivially copyable");
static_assert(IsTriviallyCopyable<TopDownRefCountState>::value,
              "All ref count states must be trivially copyable");

} // end swift namespace

namespace llvm {

raw_ostream &operator<<(raw_ostream &OS,
                        swift::BottomUpRefCountState::LatticeState S);
raw_ostream &operator<<(raw_ostream &OS,
                        swift::TopDownRefCountState::LatticeState S);

} // end namespace llvm

#endif
