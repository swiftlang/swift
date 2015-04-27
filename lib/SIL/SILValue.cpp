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

#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                       Check SILValue Type Properties
//===----------------------------------------------------------------------===//

/// These are just for performance and verification. If one needs to make
/// changes that cause the asserts the fire, please update them. The purpose is
/// to prevent these predicates from changing values by mistake.
static_assert(std::is_standard_layout<SILValue>::value,
              "Expected SILValue to be standard layout");
static_assert(sizeof(SILValue) == sizeof(uintptr_t),
              "SILValue should be pointer sized");

//===----------------------------------------------------------------------===//
//                              Utility Methods
//===----------------------------------------------------------------------===//

void SILValue::replaceAllUsesWith(SILValue V) {
  assert(*this != V && "Cannot RAUW a value with itself");
  assert(getType() == V.getType() && "Invalid type");
  while (!use_empty())
    (**use_begin()).set(V);
}

static bool isRCIdentityPreservingCast(ValueKind Kind) {
  switch (Kind) {
  case ValueKind::UpcastInst:
  case ValueKind::UncheckedRefCastInst:
  case ValueKind::UncheckedAddrCastInst:
  case ValueKind::UnconditionalCheckedCastInst:
  case ValueKind::UncheckedRefBitCastInst:
  case ValueKind::RefToBridgeObjectInst:
  case ValueKind::BridgeObjectToRefInst:
    return true;
  default:
    return false;
  }
}

/// Return the underlying SILValue after stripping off identity SILArguments if
/// we belong to a BB with one predecessor.
static SILValue stripSinglePredecessorArgs(SILValue V) {
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
      if (SILValue Arg = CBI->getArgForDestBB(BB, A)) {
        V = Arg;
        continue;
      }
    }

    return V;
  }
}

SILValue SILValue::stripCasts() {
  SILValue V = *this;

  while (true) {
    V = stripSinglePredecessorArgs(V);

    auto K = V->getKind();
    if (isRCIdentityPreservingCast(K) ||
        K == ValueKind::UncheckedTrivialBitCastInst) {
      V = cast<SILInstruction>(V.getDef())->getOperand(0);
      continue;
    }

    return V;
  }
}

SILValue SILValue::stripUpCasts() {
  assert(getType().isClassOrClassMetatype() &&
         "Expected class or class metatype!");

  SILValue V = stripSinglePredecessorArgs(*this);

  while (isa<UpcastInst>(V))
    V = stripSinglePredecessorArgs(cast<UpcastInst>(V)->getOperand());

  return V;
}

SILValue SILValue::stripClassCasts() {
  SILValue V = *this;
  while (true) {
    if (auto *UI = dyn_cast<UpcastInst>(V)) {
      V = UI->getOperand();
      continue;
    }

    if (auto *UCCI = dyn_cast<UnconditionalCheckedCastInst>(V)) {
      V = UCCI->getOperand();
      continue;
    }

    return V;
  }
}


SILValue SILValue::stripAddressProjections() {
  SILValue V = *this;

  while (true) {
    V = stripSinglePredecessorArgs(V);

    switch (V->getKind()) {
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::RefElementAddrInst:
    case ValueKind::UncheckedTakeEnumDataAddrInst:
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
    V = stripSinglePredecessorArgs(V);

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

SILValue SILValue::stripExpectIntrinsic() {
  SILValue V = *this;
  auto *BI = dyn_cast<BuiltinInst>(V);
  if (!BI)
    return V;
  if (BI->getIntrinsicInfo().ID != llvm::Intrinsic::expect)
    return V;
  return BI->getArguments()[0];
}

SILBasicBlock *ValueBase::getParentBB() {
  if (auto Inst = dyn_cast<SILInstruction>(this))
    return Inst->getParent();
  if (auto Arg = dyn_cast<SILArgument>(this))
    return Arg->getParent();
  return nullptr;
}

void Operand::hoistAddressProjections(SILInstruction *InsertBefore,
                                      DominanceInfo *DomTree) {
  SILValue V = get();
  SILInstruction *Prev = nullptr;
  while (true) {
    SILValue Incoming = stripSinglePredecessorArgs(V);

    // Forward the incoming arg from a single predeccessor.
    if (V != Incoming) {
      if (V == get()) {
        // If we are the operand itself set the operand to the incoming
        // arugment.
        set(Incoming);
      } else {
        // Otherwise, set the previous projections operand to the incoming
        // argument.
        assert(Prev && "Must have seen a projection");
        Prev->setOperand(0, Incoming);
      }
    }

    switch (V->getKind()) {
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::RefElementAddrInst:
    case ValueKind::UncheckedTakeEnumDataAddrInst: {
      auto *Inst = cast<SILInstruction>(V);
      // We are done once the current projection dominates the insert point.
      if (DomTree->dominates(Inst->getParent(), InsertBefore->getParent()))
        return;

      // Move the current projection and memorize it for the next iteration.
      Prev = Inst;
      Inst->moveBefore(InsertBefore);
      V = Inst->getOperand(0);
      continue;
    }
    default:
      assert(DomTree->dominates(V->getParentBB(), InsertBefore->getParent()) &&
             "The projected value must dominate the insertion point");
      return;
    }
  }
}

