//===--- SILValue.cpp - Implementation for SILValue -----------------------===//
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

#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"

using namespace swift;

void SILValue::replaceAllUsesWith(SILValue V) {
  assert(*this != V && "Cannot RAUW a value with itself");
  assert(getType() == V.getType() && "Invalid type");
  while (!use_empty())
    (**use_begin()).set(V);
}

static bool isRCIdentityPreservingCast(ValueKind Kind) {
  switch (Kind) {
  case ValueKind::UpcastInst:
  case ValueKind::AddressToPointerInst:
  case ValueKind::PointerToAddressInst:
  case ValueKind::UncheckedRefCastInst:
  case ValueKind::UncheckedAddrCastInst:
  case ValueKind::RefToRawPointerInst:
  case ValueKind::RawPointerToRefInst:
  case ValueKind::UnconditionalCheckedCastInst:
  case ValueKind::UncheckedRefBitCastInst:
    return true;
  default:
    return false;
  }
}

SILValue SILValue::stripCasts() {
  SILValue V = *this;

  while (true) {
    auto K = V->getKind();
    if (isRCIdentityPreservingCast(K) ||
        K == ValueKind::UncheckedTrivialBitCastInst) {
      V = cast<SILInstruction>(V.getDef())->getOperand(0);
      continue;
    }

    return V;
  }
}

SILValue SILValue::stripAddressProjections() {
  SILValue V = *this;

  while (true) {
    switch (V->getKind()) {
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::RefElementAddrInst:
      V = cast<SILInstruction>(V.getDef())->getOperand(0);
      continue;
    default:
      return V;
    }
  }
}

SILValue SILValue::stripAggregateProjections() {
  SILValue V = *this;

  while (true) {
    switch (V->getKind()) {
    case ValueKind::StructExtractInst:
    case ValueKind::TupleExtractInst:
      V = cast<SILInstruction>(V.getDef())->getOperand(0);
      continue;
    default:
      return V;
    }
  }
}

SILValue SILValue::stripIndexingInsts() {
  SILValue V = *this;
  while (true) {
    if (!isa<IndexingInst>(V.getDef()))
      return V;
    V = cast<IndexingInst>(V)->getBase();
  }
}

/// Returns true if S is a decl of a struct with only one stored field.
static bool isSingleFieldStruct(StructDecl *S) {
  auto R = S->getStoredProperties();
  auto B = R.begin();
  if (B == R.end())
    return false;
  ++B;
  if (B != R.end())
    return false;
  return true;
}

/// Returns true if \p Elt is the EnumElementDecl of the first payloaded case of
/// \p E.
static bool isFirstPayloadedCaseOfEnum(EnumDecl *E, EnumElementDecl *Elt) {
  for (EnumElementDecl *Iter : E->getAllElements())
    if (Iter->hasArgumentType())
      return Iter == Elt;
  return false;
}

SILValue SILValue::stripRCIdentityPreservingOps() {
  SILValue V = *this;
  while (true) {
    V = V.stripNonPHIRCIdentityPreservingArgs();

    // Strip off RC identity preserving casts.
    if (isRCIdentityPreservingCast(V->getKind())) {
      V = cast<SILInstruction>(V.getDef())->getOperand(0);
      continue;
    }

    // Then if we have a struct_extract from a struct with one member is is ref
    // count equivalent with the member. Strip off extract.
    if (auto *SEI = dyn_cast<StructExtractInst>(V)) {
      if (isSingleFieldStruct(SEI->getStructDecl())) {
        V = SEI->getOperand();
        continue;
      }
    }

    // If we have an unchecked_enum_data from the first payloaded argument of an
    // enum, strip off the unchecked_enum_data.
    if (auto *UEDI = dyn_cast<UncheckedEnumDataInst>(V)) {
      EnumDecl *E = UEDI->getOperand().getType().getEnumOrBoundGenericEnum();
      if (isFirstPayloadedCaseOfEnum(E, UEDI->getElement())) {
        V = UEDI->getOperand();
        continue;
      }
    }

    return V;
  }
}

/// Return the underlying SILValue after stripping off non-phi SILArguments.
SILValue SILValue::stripNonPHIArgs() {
  SILValue V = *this;

  while (true) {
    auto *A = dyn_cast<SILArgument>(V);
    if (!A)
      return V;

    SILBasicBlock *BB = A->getParent();

    // First try and grab the single predecessor of our parent BB. If we don't
    // have one, bail.
    SILBasicBlock *Pred = BB->getSinglePredecessor();
    if (!Pred)
      return V;

    // Then grab the terminator of Pred...
    TermInst *PredTI = Pred->getTerminator();

    // And attempt to find our matching argument.
    if (auto *BI = dyn_cast<BranchInst>(PredTI)) {
      V = BI->getArg(A->getIndex());
      continue;
    }

    if (auto *CBI = dyn_cast<CondBranchInst>(PredTI)) {
      V = CBI->getArgForDestBB(BB, A->getIndex());
      continue;
    }

    return V;
  }
}

/// Return the underlying SILValue after stripping off non-phi SILArguments
/// that can not affect RC Identity.
SILValue SILValue::stripNonPHIRCIdentityPreservingArgs() {
  SILValue V = *this;

  while (true) {
    // First strip off non PHI args that
    V = V.stripNonPHIArgs();

    auto *A = dyn_cast<SILArgument>(V);
    if (!A)
      return V;

    SILBasicBlock *BB = A->getParent();

    // First try and grab the single predecessor of our parent BB. If we don't
    // have one, bail.
    SILBasicBlock *Pred = BB->getSinglePredecessor();
    if (!Pred)
      return V;

    // Then grab the terminator of Pred...
    TermInst *PredTI = Pred->getTerminator();

    // Maybe we need a cast stripping equivalent?
    if (auto *CCBI = dyn_cast<CheckedCastBranchInst>(PredTI)) {
      V = CCBI->getOperand();
      continue;
    }

    if (auto *SWEI = dyn_cast<SwitchEnumInst>(PredTI)) {
      V = SWEI->getOperand();
      continue;
    }

    return V;
  }
}
