//===--- RefCountState.cpp ------------------------------------------------===//
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

#define DEBUG_TYPE "sil-global-arc-opts"
#include "RefCountState.h"
#include "RCStateTransition.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                           Lattice State Merging
//===----------------------------------------------------------------------===//

static inline BottomUpRefCountState::LatticeState
MergeBottomUpLatticeStates(BottomUpRefCountState::LatticeState L1,
                           BottomUpRefCountState::LatticeState L2) {
  using LatticeState = BottomUpRefCountState::LatticeState;
  // If both states equal, return the first.
  if (L1 == L2)
    return L1;

  // If either are none, return None.
  if (L1 == LatticeState::None || L2 == LatticeState::None)
    return LatticeState::None;

  // Canonicalize.
  if (unsigned(L1) > unsigned(L2))
    std::swap(L1, L2);

  // Choose the side further along in the sequence.
  if ((L1 == LatticeState::Decremented || L1 == LatticeState::MightBeUsed) ||
      (L2 == LatticeState::MightBeUsed ||
       L2 == LatticeState::MightBeDecremented))
    return L2;

  // Otherwise, we don't know what happened, be conservative and return none.
  return LatticeState::None;
}

static inline TopDownRefCountState::LatticeState
MergeTopDownLatticeStates(TopDownRefCountState::LatticeState L1,
                          TopDownRefCountState::LatticeState L2) {
  using LatticeState = TopDownRefCountState::LatticeState;
  // If both states equal, return the first.
  if (L1 == L2)
    return L1;

  // If either are none, return None.
  if (L1 == LatticeState::None || L2 == LatticeState::None)
    return LatticeState::None;

  // Canonicalize.
  if (unsigned(L1) > unsigned(L2))
    std::swap(L1, L2);

  // Choose the side further along in the sequence.
  if ((L1 == LatticeState::Incremented ||
       L1 == LatticeState::MightBeDecremented) ||
      (L2 == LatticeState::MightBeDecremented ||
       L2 == LatticeState::MightBeUsed))
    return L2;

  // Otherwise, we don't know what happened, return none.
  return LatticeState::None;
}

//===----------------------------------------------------------------------===//
//                         Bottom Up Ref Count State
//===----------------------------------------------------------------------===//

/// Initializes/reinitialized the state for I. If we reinitialize we return
/// true.
bool BottomUpRefCountState::initWithMutatorInst(SILInstruction *I) {
  assert((isa<StrongReleaseInst>(I) || isa<ReleaseValueInst>(I)) &&
         "strong_release and release_value are only supported.");

  bool NestingDetected = SuperTy::initWithMutatorInst(I);

  // If we know that there is another decrement on the same pointer that has
  // not been matched up to an increment, then the pointer must have a
  // reference count of at least 2 before this decrement. This implies it is
  // known safe.
  KnownSafe = NestingDetected;

  // If we saw a non arc user that will keep this value alive, set known safe
  // since we will not move non-arc instructions.
  KnownSafe |= FoundNonARCUser;

  // Set our lattice state to be incremented.
  LatState = LatticeState::Decremented;

  return NestingDetected;
}

/// Return true if we *might* remove this instruction.
///
/// This is a conservative query given the information we know, so as we
/// perform the dataflow it may change value.
bool BottomUpRefCountState::mightRemoveMutators() {
  if (LatState == LatticeState::None)
    return false;

  // We will not remove mutators if we have a might be decremented value that
  // is not known safe.
  return LatState != LatticeState::MightBeDecremented || isKnownSafe();
}

/// Uninitialize the current state.
void BottomUpRefCountState::clear() {
  // If we can not conservatively prove that the given RefCountState will not
  // be removed, be conservative and clear the transition state, so we do not
  // propagate KnownSafety forward.
  if (mightRemoveMutators())
    Transition = None;
  LatState = LatticeState::None;
  SuperTy::clear();
}

/// If advance the state's sequence appropriately for a decrement. If we do
/// advance return true. Otherwise return false.
bool BottomUpRefCountState::isRefCountStateModified() const {
  switch (LatState) {
  case LatticeState::Decremented:
    return true;
  case LatticeState::None:
  case LatticeState::MightBeDecremented:
  case LatticeState::MightBeUsed:
    return false;
  }
}

/// Returns true if given the current lattice state, do we care if the value
/// we are tracking is decremented.
bool BottomUpRefCountState::valueCanBeDecrementedGivenLatticeState() const {
  switch (LatState) {
  case LatticeState::MightBeUsed:
    return true;
  case LatticeState::None:
  case LatticeState::MightBeDecremented:
  case LatticeState::Decremented:
    return false;
  }
}

/// If advance the state's sequence appropriately for a decrement. If we do
/// advance return true. Otherwise return false.
bool BottomUpRefCountState::
handleDecrement(SILInstruction *PotentialDecrement) {
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

/// Returns true if given the current lattice state, do we care if the value we
/// are tracking is used.
bool BottomUpRefCountState::valueCanBeUsedGivenLatticeState() const {
  switch (LatState) {
  case LatticeState::Decremented:
    return true;
  case LatticeState::None:
  case LatticeState::MightBeDecremented:
  case LatticeState::MightBeUsed:
    return false;
  }
}

/// Given the current lattice state, if we have seen a use, advance the
/// lattice state. Return true if we do so and false otherwise.
bool
BottomUpRefCountState::handleUser(SILInstruction *PotentialUser,
                                  SILValue RCIdentity,
                                  AliasAnalysis *AA) {
  assert(valueCanBeUsedGivenLatticeState() &&
         "Must be able to be used at this point of the lattice.");

  // Instructions that we do not recognize (and thus will not move) and that
  // *must* use RCIdentity, implies we are always known safe as long as meet
  // over all path constraints are satisfied.
  if (isRCStateTransitionUnknown(PotentialUser))
    if (mustUseValue(PotentialUser, RCIdentity, AA))
      FoundNonARCUser = true;

  // Advance the sequence...
  switch (LatState) {
  case LatticeState::Decremented:
    LatState = LatticeState::MightBeUsed;
    assert(InsertPts.empty() && "If we are decremented, we should have no "
                                "insertion points.");
    InsertPts.insert(std::next(SILBasicBlock::iterator(PotentialUser)));
    return true;
  case LatticeState::MightBeUsed:
  case LatticeState::MightBeDecremented:
  case LatticeState::None:
    return false;
  }
}

/// Returns true if given the current lattice state, do we care if the value
/// we are tracking is used.
bool BottomUpRefCountState::
valueCanBeGuaranteedUsedGivenLatticeState() const {
  switch (LatState) {
  case LatticeState::None:
  case LatticeState::MightBeDecremented:
    return false;
  case LatticeState::Decremented:
  case LatticeState::MightBeUsed:
    return true;
  }
}

/// Given the current lattice state, if we have seen a use, advance the
/// lattice state. Return true if we do so and false otherwise.
bool
BottomUpRefCountState::
handleGuaranteedUser(SILInstruction *PotentialGuaranteedUser,
                     SILValue RCIdentity,
                     AliasAnalysis *AA) {
  assert(valueCanBeGuaranteedUsedGivenLatticeState() &&
         "Must be able to be used at this point of the lattice.");

  // Instructions that we do not recognize (and thus will not move) and that
  // *must* use RCIdentity, implies we are always known safe as long as meet
  // over all path constraints are satisfied.
  if (isRCStateTransitionUnknown(PotentialGuaranteedUser))
    if (mustUseValue(PotentialGuaranteedUser, RCIdentity, AA))
      FoundNonARCUser = true;

  // Advance the sequence...
  switch (LatState) {
  // If were decremented, insert the insertion point.
  case LatticeState::Decremented: {
    assert(InsertPts.empty() && "If we are decremented, we should have no "
                                "insertion points.");
    auto Iter = SILBasicBlock::iterator(PotentialGuaranteedUser);
    InsertPts.insert(std::next(Iter));
    LatState = LatticeState::MightBeDecremented;
    return true;
  }
  case LatticeState::MightBeUsed:
    // If we have a might be used, we already created an insertion point
    // earlier. Just move to MightBeDecremented.
    LatState = LatticeState::MightBeDecremented;
    return true;
  case LatticeState::MightBeDecremented:
  case LatticeState::None:
    return false;
  }
}

/// We have a matching ref count inst. Return true if we advance the sequence
/// and false otherwise.
bool
BottomUpRefCountState::
handleRefCountInstMatch(SILInstruction *RefCountInst) {
  // Otherwise modify the state appropriately in preparation for removing the
  // increment, decrement pair.
  switch (LatState) {
  case LatticeState::None:
    return false;
  case LatticeState::Decremented:
  case LatticeState::MightBeUsed:
    // Unset InsertPt so we remove retain release pairs instead of
    // performing code motion.
    InsertPts.clear();
    SWIFT_FALLTHROUGH;
  case LatticeState::MightBeDecremented:
    return true;
  }
}

bool BottomUpRefCountState::merge(const BottomUpRefCountState &Other) {

  auto NewState = MergeBottomUpLatticeStates(LatState, Other.LatState);
  DEBUG(llvm::dbgs() << "            Performing BottomUp Merge.\n");
  DEBUG(llvm::dbgs() << "                Left: " << LatState << "; Right: "
                     << Other.LatState << "; Result: " << NewState << "\n");
  DEBUG(llvm::dbgs() << "                V: ";
        if (hasRCRoot())
          getRCRoot()->dump();
        else
          llvm::dbgs() << "\n";
        llvm::dbgs() << "                OtherV: ";
        if (Other.hasRCRoot())
          Other.getRCRoot()->dump();
        else
          llvm::dbgs() << "\n");

  LatState = NewState;
  KnownSafe &= Other.KnownSafe;
  FoundNonARCUser &= Other.FoundNonARCUser;

  // If we're doing a merge on a path that's previously seen a partial merge,
  // conservatively drop the sequence, to avoid doing partial RR elimination. If
  // the branch predicates for the two merge differ, mixing them is unsafe since
  // they are not control dependent.
  //
  // TODO: Add support for working around control dependence issues.
  if (LatState == BottomUpRefCountState::LatticeState::None) {
    DEBUG(llvm::dbgs() << "            Found LatticeState::None. "
                          "Clearing State!\n");
    clear();
    return false;
  }

  if (!Transition.hasValue() || !Other.Transition.hasValue() ||
      !Transition->merge(Other.Transition.getValue())) {
    DEBUG(llvm::dbgs() << "            Failed merge!\n");
    clear();
    return false;
  }

  Partial |= Other.Partial;
  Partial |= InsertPts.size() != Other.InsertPts.size();
  for (auto *SI : Other.InsertPts)
    Partial |= InsertPts.insert(SI).second;

  DEBUG(llvm::dbgs() << "            Partial: " << (Partial ? "yes" : "no")
                     << "\n");
  return true;
}


//===----------------------------------------------------------------------===//
//                          Top Down Ref Count State
//===----------------------------------------------------------------------===//

/// Initializes/reinitialized the state for I. If we reinitialize we return
/// true.
bool TopDownRefCountState::initWithMutatorInst(SILInstruction *I) {
  assert((isa<StrongRetainInst>(I) || isa<RetainValueInst>(I)) &&
         "strong_retain and retain_value are only supported.");

  bool NestingDetected = SuperTy::initWithMutatorInst(I);

  // Set our lattice state to be incremented.
  LatState = LatticeState::Incremented;

  return NestingDetected;
}

/// Initialize this ref count state with the @owned Arg at +1.
void TopDownRefCountState::initWithArg(SILArgument *Arg) {
  LatState = LatticeState::Incremented;
  Transition = RCStateTransition(Arg);
  assert((*Transition).getKind() == RCStateTransitionKind::StrongEntrance &&
         "Expected a strong entrance here");
  RCRoot = Arg;
  KnownSafe = false;
  InsertPts.clear();
}

/// Initiailize this RefCountState with an instruction which introduces a new
/// ref count at +1.
void
TopDownRefCountState::initWithEntranceInst(SILInstruction *I,
                                           SILValue RCIdentity) {
  LatState = LatticeState::Incremented;
  Transition = RCStateTransition(I);
  assert((*Transition).getKind() == RCStateTransitionKind::StrongEntrance &&
         "Expected a strong entrance here");
  RCRoot = RCIdentity;
  KnownSafe = false;
  InsertPts.clear();
}

/// Uninitialize the current state.
void TopDownRefCountState::clear() {
  Transition = None;
  LatState = LatticeState::None;
  SuperTy::clear();
}

/// Can we gaurantee that the given reference counted value has been modified?
bool TopDownRefCountState::isRefCountStateModified() const {
  switch (LatState) {
  case LatticeState::Incremented:
    return true;
  case LatticeState::None:
  case LatticeState::MightBeDecremented:
  case LatticeState::MightBeUsed:
    return false;
  }
}

/// Returns true if given the current lattice state, do we care if the value
/// we are tracking is decremented.
bool TopDownRefCountState::valueCanBeDecrementedGivenLatticeState() const {
  switch (LatState) {
  case LatticeState::Incremented:
    return true;
  case LatticeState::None:
  case LatticeState::MightBeDecremented:
  case LatticeState::MightBeUsed:
    return false;
  }
}

/// If advance the state's sequence appropriately for a decrement. If we do
/// advance return true. Otherwise return false.
bool TopDownRefCountState::handleDecrement(SILInstruction *PotentialDecrement) {
  switch (LatState) {
  case LatticeState::Incremented:
    LatState = LatticeState::MightBeDecremented;
    InsertPts.insert(PotentialDecrement);
    return true;
  case LatticeState::None:
  case LatticeState::MightBeDecremented:
  case LatticeState::MightBeUsed:
    return false;
  }
}

/// Returns true if given the current lattice state, do we care if the value
/// we are tracking is used.
bool TopDownRefCountState::valueCanBeUsedGivenLatticeState() const {
  switch (LatState) {
  case LatticeState::MightBeDecremented:
    return true;
  case LatticeState::None:
  case LatticeState::Incremented:
  case LatticeState::MightBeUsed:
    return false;
  }
}

/// Given the current lattice state, if we have seen a use, advance the
/// lattice state. Return true if we do so and false otherwise.
bool TopDownRefCountState::handleUser(SILInstruction *PotentialUser,
                                      SILValue RCIdentity,
                                      AliasAnalysis *AA) {
  assert(valueCanBeUsedGivenLatticeState() &&
         "Must be able to be used at this point of the lattice.");

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

/// Returns true if given the current lattice state, do we care if the value
/// we are tracking is used.
bool
TopDownRefCountState::
valueCanBeGuaranteedUsedGivenLatticeState() const {
  switch (LatState) {
  case LatticeState::None:
  case LatticeState::MightBeUsed:
    return false;
  case LatticeState::Incremented:
  case LatticeState::MightBeDecremented:
    return true;
  }
}

/// Given the current lattice state, if we have seen a use, advance the
/// lattice state. Return true if we do so and false otherwise.
bool
TopDownRefCountState::
handleGuaranteedUser(SILInstruction *PotentialGuaranteedUser,
                     SILValue RCIdentity, AliasAnalysis *AA) {
  assert(valueCanBeGuaranteedUsedGivenLatticeState() &&
         "Must be able to be used at this point of the lattice.");
  // Advance the sequence...
  switch (LatState) {
  // If were decremented, insert the insertion point.
  case LatticeState::Incremented: {
    assert(InsertPts.empty() && "If we are decremented, we should have no "
                                "insertion points.");
    LatState = LatticeState::MightBeUsed;
    InsertPts.insert(PotentialGuaranteedUser);
    return true;
  }
  case LatticeState::MightBeDecremented:
    // If we have a might be used, we already created an insertion point
    // earlier. Just move to MightBeDecremented.
    LatState = LatticeState::MightBeUsed;
    return true;
  case LatticeState::MightBeUsed:
  case LatticeState::None:
    return false;
  }
}

/// We have a matching ref count inst. Return true if we advance the sequence
/// and false otherwise.
bool TopDownRefCountState::
handleRefCountInstMatch(SILInstruction *RefCountInst) {
  // Otherwise modify the state appropriately in preparation for removing the
  // increment, decrement pair.
  switch (LatState) {
  case LatticeState::None:
    return false;
  case LatticeState::Incremented:
  case LatticeState::MightBeDecremented:
    // Unset InsertPt so we remove retain release pairs instead of performing
    // code motion.
    InsertPts.clear();
    SWIFT_FALLTHROUGH;
  case LatticeState::MightBeUsed:
    return true;
  }
}

bool TopDownRefCountState::merge(const TopDownRefCountState &Other) {
  auto NewState = MergeTopDownLatticeStates(LatState, Other.LatState);
  DEBUG(llvm::dbgs() << "        Performing TopDown Merge.\n");
  DEBUG(llvm::dbgs() << "            Left: " << LatState << "; Right: "
                     << Other.LatState << "; Result: " << NewState << "\n");
  DEBUG(llvm::dbgs() << "            V: ";
        if (hasRCRoot())
          getRCRoot()->dump();
        else
          llvm::dbgs() << "\n";
        llvm::dbgs() << "            OtherV: ";
        if (Other.hasRCRoot())
          Other.getRCRoot()->dump();
        else
          llvm::dbgs() << "\n");

  LatState = NewState;
  KnownSafe &= Other.KnownSafe;

  // If we're doing a merge on a path that's previously seen a partial merge,
  // conservatively drop the sequence, to avoid doing partial RR elimination. If
  // the branch predicates for the two merge differ, mixing them is unsafe since
  // they are not control dependent.
  //
  // TODO: Add support for determining control dependence.
  if (LatState == TopDownRefCountState::LatticeState::None) {
    clear();
    DEBUG(llvm::dbgs() << "            Found LatticeState::None. "
                          "Clearing State!\n");
    return false;
  }

  if (!Transition.hasValue() || !Other.Transition.hasValue() ||
      !Transition->merge(Other.Transition.getValue())) {
    DEBUG(llvm::dbgs() << "            Failed merge!\n");
    clear();
    return false;
  }

  Partial |= Other.Partial;
  Partial |= InsertPts.size() != Other.InsertPts.size();
  for (auto *SI : Other.InsertPts)
    Partial |= InsertPts.insert(SI).second;

  DEBUG(llvm::dbgs() << "            Partial: " << (Partial ? "yes" : "no")
                     << "\n");

  return true;
}

//===----------------------------------------------------------------------===//
//                             Printing Utilities
//===----------------------------------------------------------------------===//

namespace llvm {

raw_ostream &operator<<(raw_ostream &OS,
                        BottomUpRefCountState::LatticeState S) {
  using LatticeState = BottomUpRefCountState::LatticeState;
  switch (S) {
  case LatticeState::None:
    return OS << "None";
  case LatticeState::Decremented:
    return OS << "Decremented";
  case LatticeState::MightBeUsed:
    return OS << "MightBeUsed";
  case LatticeState::MightBeDecremented:
    return OS << "MightBeDecremented";
  }
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                              TopDownRefCountState::LatticeState S) {
  using LatticeState = TopDownRefCountState::LatticeState;
  switch (S) {
  case LatticeState::None:
    return OS << "None";
  case LatticeState::Incremented:
    return OS << "Incremented";
  case LatticeState::MightBeUsed:
    return OS << "MightBeUsed";
  case LatticeState::MightBeDecremented:
    return OS << "MightBeDecremented";
  }
}

} // end namespace llvm
