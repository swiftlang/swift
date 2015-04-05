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
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

static bool isAutoreleasePoolCall(SILInstruction *I) {
  auto *AI = dyn_cast<ApplyInst>(I);
  if (!AI)
    return false;

  auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (!FRI)
    return false;

  return llvm::StringSwitch<bool>(FRI->getReferencedFunction()->getName())
      .Case("objc_autoreleasePoolPush", true)
      .Case("objc_autoreleasePoolPop", true)
      .Default(false);
}

//===----------------------------------------------------------------------===//
//                           RCStateTransitionKind
//===----------------------------------------------------------------------===//

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

  case ValueKind::ApplyInst: {
    auto *AI = cast<ApplyInst>(V);
    if (isAutoreleasePoolCall(AI))
      return RCStateTransitionKind::AutoreleasePoolCall;

    // If we have an @owned return value. This AI is a strong entrance for its
    // return value.
    //
    // TODO: When we support pairing retains with @owned parameters, we will
    // need to be able to handle the potential of multiple state transition
    // kinds.
    if (AI->hasResultConvention(ResultConvention::Owned))
      return RCStateTransitionKind::StrongEntrance;

    return RCStateTransitionKind::Unknown;
  }
  default:
    return RCStateTransitionKind::Unknown;
  }
}

/// Define test functions for all of our abstract value kinds.
#define ABSTRACT_VALUE(Name, StartKind, EndKind)                           \
  bool swift::isRCStateTransition ## Name(RCStateTransitionKind Kind) {    \
    return unsigned(RCStateTransitionKind::StartKind) <= unsigned(Kind) && \
      unsigned(RCStateTransitionKind::EndKind) >= unsigned(Kind);          \
  }
#include "RCStateTransition.def"

raw_ostream &llvm::operator<<(raw_ostream &os, RCStateTransitionKind Kind) {
  switch (Kind) {
#define KIND(K)                                 \
  case RCStateTransitionKind::K:                \
    return os << #K;
#include "RCStateTransition.def"
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

//===----------------------------------------------------------------------===//
//                             RCStateTransition
//===----------------------------------------------------------------------===//

#define ABSTRACT_VALUE(Name, Start, End)            \
  bool RCStateTransition::is ## Name() const {      \
    return isRCStateTransition ## Name(getKind());  \
  }
#include "RCStateTransition.def"

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

