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
#include "swift/SIL/SILArgument.h"

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

/// Returns true if Kind is one of AllocStackInst, AllocBoxInst, AllocRefInst,
/// or AllocArrayInst.
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

/// A no alias argument is an argument that is an address type.
static bool isNoAliasArgument(SILValue V) {
  auto *Arg = dyn_cast<SILArgument>(V.getDef());
  if (!Arg)
    return false;
  return Arg->getType().isAddress();
}

/// Return true if V is an object that at compile time can be uniquely
/// identified.
static bool isIdentifiableObject(SILValue V) {
  ValueKind Kind = V->getKind();

  if (isAllocationInst(Kind) || isNoAliasArgument(V) || isa<LiteralInst>(*V))
    return true;

  return false;
}

/// Returns true if we can prove that the two input SILValues which do not equal
/// can not alias.
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

AliasAnalysis::AliasResult AliasAnalysis::alias(SILValue V1, SILValue V2) {
  // Strip off any casts on V1, V2.
  V1 = V1.stripCasts();
  V2 = V2.stripCasts();

  // Create a key to lookup if we have already computed an alias result for V1,
  // V2. Canonicalize our cache keys so that the pointer with the lower address
  // is always the first element of the pair. This ensures we do not pollute our
  // cache with two entries with the same key, albeit with the key's fields
  // swapped.
  auto Key = V1 < V2? std::make_pair(V1, V2) : std::make_pair(V2, V1);

  // If we find our key in the cache, just return the alias result.
  auto Pair = AliasCache.find(Key);
  if (Pair != AliasCache.end())
    return Pair->second;

  // Ok, we need to actually compute an Alias Analysis result for V1, V2. Begin
  // by finding the "base" of V1, V2 by stripping off all casts and GEPs.
  SILValue O1 = getUnderlyingObject(V1);
  SILValue O2 = getUnderlyingObject(V2);

  // If O1 and O2 do not equal, see if we can prove that they can not be the
  // same object. If we can, return No Alias.
  if (O1 != O2 && aliasUnequalObjects(O1, O2))
    return AliasCache[Key] = AliasResult::NoAlias;

  // We could not prove anything. Be conservative and return that V1, V2 may
  // alias.
  return AliasCache[Key] = AliasResult::MayAlias;
}

SILInstruction::MemoryBehavior
AliasAnalysis::getMemoryBehavior(SILInstruction *Inst, SILValue V) {
  // FIXME: Fill this in.
  return SILInstruction::MemoryBehavior::MayHaveSideEffects;
}
