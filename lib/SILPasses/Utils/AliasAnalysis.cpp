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

AliasAnalysis::Result AliasAnalysis::alias(SILValue V1, SILValue V2) {
  /// FIXME: Fill this out.
  return Result::MayAlias;
}

AliasAnalysis::Result AliasAnalysis::alias(SILInstruction *Inst, SILValue V2) {
  /// FIXME: Fill this out.
  return Result::MayAlias;
}
