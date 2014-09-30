//===--- OverloadChoice.cpp - A Choice from an Overload Set  ----*- C++ -*-===//
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
//
// This file provides the \c OverloadChoice class and its related types,
// which is used by the constraint-based type checker to describe the
// selection of a particular overload from a set.
//
//===----------------------------------------------------------------------===//

#include "OverloadChoice.h"
#include "ConstraintSystem.h"
#include "swift/AST/Availability.h"

using namespace swift;
using namespace constraints;

OverloadChoice::OverloadChoice(
    Type base, ValueDecl *value, bool isSpecialized, ConstraintSystem &CS,
    const Optional<UnavailabilityReason> &reasonUnavailable)
    : BaseAndBits(base, isSpecialized ? IsSpecializedBit : 0) {
  assert((reinterpret_cast<uintptr_t>(value) & (uintptr_t)0x03) == 0 &&
         "Badly aligned decl");

  DeclOrKind = reinterpret_cast<uintptr_t>(value);
  UnavailableReasonData = compactUnavailableReasonData(CS, reasonUnavailable);
}

uint16_t OverloadChoice::compactUnavailableReasonData(
    ConstraintSystem &CS, const Optional<UnavailabilityReason> &reason) {
  // A value of 0 indicates no reason (that is, that the overload choice is
  // always available).
  if (!reason.hasValue()) {
    return 0;
  }

  // If there is a reason, we add it to UnavailabilityReasons and encode the
  // index in our compact representation. We could get more fancy here by
  // hashing the reason but, for the moment, we take a similar approach to that
  // that taken by Fix.
  unsigned index = CS.UnavailabilityReasons.size();
  CS.UnavailabilityReasons.push_back(reason.getValue());

  // Encode the reason as the index into UnavailabilityReasons + 1.
  return index + 1;
}

const UnavailabilityReason &
OverloadChoice::getReasonUnavailable(ConstraintSystem &CS) const {
  assert(UnavailableReasonData != 0);
  unsigned index = UnavailableReasonData - 1;
  return CS.UnavailabilityReasons[index];
}
