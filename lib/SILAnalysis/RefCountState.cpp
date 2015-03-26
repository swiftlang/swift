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
//                    Reference Count State Implementation
//===----------------------------------------------------------------------===//

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

  DEBUG(llvm::dbgs() << "                Partial: " << (Partial ? "yes" : "no")
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

