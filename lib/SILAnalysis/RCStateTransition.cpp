//===--- RCStateTransition.cpp --------------------------------------------===//
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
#include "RCStateTransition.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                           RCStateTransitionKind
//===----------------------------------------------------------------------===//

bool swift::isRCStateTransitionEndPoint(RCStateTransitionKind Kind) {
  switch (Kind) {
  case RCStateTransitionKind::StrongEntrance:
    return true;
  case RCStateTransitionKind::Unknown:
  case RCStateTransitionKind::StrongIncrement:
  case RCStateTransitionKind::StrongDecrement:
    return false;
  }

  llvm_unreachable("Covered switch isn't covered?!");
}

/// Returns true if Kind mutates an RCIdentity that already has been introduced.
bool swift::isRCStateTransitionMutator(RCStateTransitionKind Kind) {
  switch (Kind) {
  case RCStateTransitionKind::Unknown:
  case RCStateTransitionKind::StrongEntrance:
    return false;
  case RCStateTransitionKind::StrongIncrement:
  case RCStateTransitionKind::StrongDecrement:
    return true;
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

RCStateTransitionKind swift::getRCStateTransitionKind(ValueBase *V) {
  switch (V->getKind()) {
  case ValueKind::StrongRetainInst:
  case ValueKind::RetainValueInst:
    return RCStateTransitionKind::StrongIncrement;

  case ValueKind::StrongReleaseInst:
  case ValueKind::ReleaseValueInst:
    return RCStateTransitionKind::StrongDecrement;

  case ValueKind::SILArgument: {
    auto *Arg = cast<SILArgument>(V);
    if (Arg->isFunctionArg() &&
        Arg->hasConvention(ParameterConvention::Direct_Owned))
      return RCStateTransitionKind::StrongEntrance;
    return RCStateTransitionKind::Unknown;
  }

  default:
    return RCStateTransitionKind::Unknown;
  }
}

//===----------------------------------------------------------------------===//
//                             RCStateTransition
//===----------------------------------------------------------------------===//

bool RCStateTransition::isEndPoint() const {
  return isRCStateTransitionEndPoint(getKind());
}

bool RCStateTransition::isMutator() const {
  return isRCStateTransitionMutator(getKind());
}

RCStateTransition::RCStateTransition(const RCStateTransition &R) {
  Kind = R.Kind;
  if (R.isEndPoint()) {
    EndPoint = R.EndPoint;
    return;
  }

  if (!R.isMutator())
    return;
  Mutators = R.Mutators;
}

bool RCStateTransition::matchingInst(SILInstruction *Inst) const {
  // We only pair mutators for now.
  if (!isMutator())
    return false;

  if (Kind == RCStateTransitionKind::StrongIncrement) {
    auto InstTransKind = getRCStateTransitionKind(Inst);
    return InstTransKind == RCStateTransitionKind::StrongDecrement;
  }

  if (Kind == RCStateTransitionKind::StrongDecrement) {
    auto InstTransKind = getRCStateTransitionKind(Inst);
    return InstTransKind == RCStateTransitionKind::StrongIncrement;
  }

  return false;
}

bool RCStateTransition::merge(const RCStateTransition &Other) {
  // If our kinds do not match, bail. We don't cross the streams.
  if (Kind != Other.Kind)
    return false;

  // If we are not a mutator, there is nothing further to do here.
  if (!isMutator())
    return true;

  Mutators.insert(Other.Mutators.begin(), Other.Mutators.end());

  return true;
}
