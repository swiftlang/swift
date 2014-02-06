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

#define DEBUG_TYPE "sil-aa"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILArgument.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                             Utility Functions
//===----------------------------------------------------------------------===//

/// Strip off casts/indexing insts/address projections from V until there is
/// nothing left to strip.
static SILValue getUnderlyingObject(SILValue V) {
  while (true) {
    SILValue V2 = V.stripCasts().stripAddressProjections().stripIndexingInsts();
    if (V2 == V)
      return V2;
    V = V2;
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
  DEBUG(llvm::dbgs() << "ALIAS ANALYSIS:\n    V1: " << *V1.getDef()
        << "    V2: " << *V2.getDef());
  // Strip off any casts on V1, V2.
  V1 = V1.stripCasts();
  V2 = V2.stripCasts();
  DEBUG(llvm::dbgs() << "        After Cast Stripping V1:" << *V1.getDef());
  DEBUG(llvm::dbgs() << "        After Cast Stripping V2:" << *V2.getDef());

  // Create a key to lookup if we have already computed an alias result for V1,
  // V2. Canonicalize our cache keys so that the pointer with the lower address
  // is always the first element of the pair. This ensures we do not pollute our
  // cache with two entries with the same key, albeit with the key's fields
  // swapped.
  auto Key = V1 < V2? std::make_pair(V1, V2) : std::make_pair(V2, V1);

  // If we find our key in the cache, just return the alias result.
  auto Pair = AliasCache.find(Key);
  if (Pair != AliasCache.end()) {
    DEBUG(llvm::dbgs() << "      Found value in the cache: "
          << unsigned(Pair->second));

    return Pair->second;
  }

  // Ok, we need to actually compute an Alias Analysis result for V1, V2. Begin
  // by finding the "base" of V1, V2 by stripping off all casts and GEPs.
  SILValue O1 = getUnderlyingObject(V1);
  SILValue O2 = getUnderlyingObject(V2);
  DEBUG(llvm::dbgs() << "        Underlying V1:" << *O1.getDef());
  DEBUG(llvm::dbgs() << "        Underlying V2:" << *O2.getDef());


  // If O1 and O2 do not equal, see if we can prove that they can not be the
  // same object. If we can, return No Alias.
  if (O1 != O2 && aliasUnequalObjects(O1, O2))
    return AliasCache[Key] = AliasResult::NoAlias;

  // We could not prove anything. Be conservative and return that V1, V2 may
  // alias.
  return AliasResult::MayAlias;
}

SILInstruction::MemoryBehavior
AliasAnalysis::getMemoryBehavior(SILInstruction *Inst, SILValue V) {
  DEBUG(llvm::dbgs() << "GET MEMORY BEHAVIOR FOR:\n    " << *Inst << "    "
        << *V.getDef());

  // If we already know that we do not read or write memory, just return None.
  if (!Inst->mayReadOrWriteMemory()) {
    DEBUG(llvm::dbgs() << "  Inst does not write memory. Returning None.\n");
    return MemoryBehavior::None;
  }

  switch (Inst->getKind()) {
  case ValueKind::LoadInst:
    // If the load address doesn't alias the given address, it doesn't read or
    // write the specified memory.
    if (alias(Inst->getOperand(0), V) == AliasResult::NoAlias) {
      DEBUG(llvm::dbgs() << "  Load does not alias inst. Returning None.\n");
      return MemoryBehavior::None;
    }

    // Otherwise be conservative and just return reads since loads can only
    // read.
    DEBUG(llvm::dbgs() << "  Could not prove load does not alias inst. "
          "Returning MayRead.\n");
    return MemoryBehavior::MayRead;
  case ValueKind::StoreInst:
    // If the store dest cannot alias the pointer in question, then the
    // specified value can not be modified by the store.
    if (alias(cast<StoreInst>(Inst)->getDest(), V) == AliasResult::NoAlias) {
      DEBUG(llvm::dbgs() << "  Store Dst does not alias inst. Returning "
            "None.\n");
      return MemoryBehavior::None;
    }

    // Otherwise, a store just writes.
    DEBUG(llvm::dbgs() << "  Could not prove store does not alias inst. "
          "Returning MayWrite.\n");
    return MemoryBehavior::MayWrite;
  case ValueKind::ApplyInst: {
    // If the ApplyInst is from a no-read builtin it can not read or write and
    // if it comes from a no-side effect builtin, it can only read.
    auto *AI = cast<ApplyInst>(Inst);
    auto *BFR = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee().getDef());

    // If our callee is not a builtin, be conservative and return may have side
    // effects.
    if (!BFR) {
      DEBUG(llvm::dbgs() << "  Found apply we don't understand returning "
            "MHSF.\n");
      return MemoryBehavior::MayHaveSideEffects;
    }

    // If the builtin is read none, it does not read or write memory.
    if (isReadNone(BFR)) {
      DEBUG(llvm::dbgs() << "  Found apply of read none builtin. Returning"
            " None.\n");
      return MemoryBehavior::None;
    }
    // If the builtin is side effect free, then it can only read memory.
    if (isSideEffectFree(BFR)) {
      DEBUG(llvm::dbgs() << "  Found apply of side effect free builtin. "
            "Returning MayRead.\n");
      return MemoryBehavior::MayRead;
    }

    // Otherwise be conservative and return that we may have side effects.
    DEBUG(llvm::dbgs() << "  Found apply of side effect builtin. "
          "Returning MayHaveSideEffects.\n");
    return MemoryBehavior::MayHaveSideEffects;
  }
  default:
    // If we do not have a special case, just return the generic memory
    // behavior of Inst.
    return Inst->getMemoryBehavior();
  }
}

SILAnalysis *swift::createAliasAnalysis(SILModule *M) {
  return new AliasAnalysis(M);
}

