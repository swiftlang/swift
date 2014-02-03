//===-------------- AliasAnalysis.cpp - SIL Alias Analysis ----------------===//
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

#include "swift/SILPasses/Utils/AliasAnalysis.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILInstruction.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                             Utility Functions
//===----------------------------------------------------------------------===//

/// Strip off casts/address projections from V until there is nothing
/// left to strip.
static SILValue getUnderlyingObject(SILValue V) {
  while (true) {
    SILValue NewV = V.stripCasts();
    NewV = NewV.stripAddressProjections();
    if (NewV == V)
      return NewV;
    V = NewV;
  }
}

static bool isAllocationInst(ValueKind Kind) {
  switch (Kind) {
  case ValueKind::AllocStackInst:
  case ValueKind::AllocBoxInst:
  case ValueKind::AllocRefInst:
  case ValueKind::AllocArrayInst:
    return true;
  default:
    return false;
  }
}

/// Return true if V is an object that at compile time can be uniquely
/// identified.
static bool isIdentifiableObject(SILValue V) {
  ValueKind Kind = V->getKind();

  if (isAllocationInst(Kind) || Kind == ValueKind::SILArgument ||
      isa<LiteralInst>(*V))
    return true;

  return false;
}

static bool aliasUnequalObjects(SILValue O1, SILValue O2) {
  assert(O1 != O2 && "This function should only be called on unequal values.");

  // If O1 and O2 do not equal and they are both values that can be statically
  // and uniquely identified, they can not alias.
  if (isIdentifiableObject(O1) && isIdentifiableObject(O2))
    return true;

  // We failed to prove that the two objects are different.
  return false;
}

//===----------------------------------------------------------------------===//
//                                Entry Points
//===----------------------------------------------------------------------===//

AliasAnalysis::Result AliasAnalysis::alias(SILValue V1, SILValue V2) {
  // Get the underlying objects for V1, V2.
  SILValue O1 = getUnderlyingObject(V1);
  SILValue O2 = getUnderlyingObject(V2);

  // If O1 and O2 do not equal, see if we can prove that they can not be the
  // same object. If we can, return No Alias.
  if (O1 != O2 && aliasUnequalObjects(O1, O2))
    return Result::NoAlias;

  // We could not prove anything. Be conservative and return that V1, V2 may
  // alias.
  return Result::MayAlias;
}

AliasAnalysis::Result AliasAnalysis::alias(SILInstruction *Inst, SILValue V2) {
  /// FIXME: Fill this out.
  return Result::MayAlias;
}
