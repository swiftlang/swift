//===--- InstructionUtils.cpp - Utilities for SIL instructions ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-inst-utils"
#include "swift/SIL/InstructionUtils.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILVisitor.h"

using namespace swift;

/// Strip off casts/indexing insts/address projections from V until there is
/// nothing left to strip.
/// FIXME: Why don't we strip projections after stripping indexes?
SILValue swift::getUnderlyingObject(SILValue V) {
  while (true) {
    SILValue V2 = stripIndexingInsts(stripAddressProjections(stripCasts(V)));
    if (V2 == V)
      return V2;
    V = V2;
  }
}

/// Strip off casts and address projections into the interior of a value. Unlike
/// getUnderlyingObject, this does not find the root of a heap object--a class
/// property is itself an address root.
SILValue swift::getUnderlyingAddressRoot(SILValue V) {
  while (true) {
    SILValue V2 = stripIndexingInsts(stripCasts(V));
    switch (V2->getKind()) {
      case ValueKind::StructElementAddrInst:
      case ValueKind::TupleElementAddrInst:
      case ValueKind::UncheckedTakeEnumDataAddrInst:
        V2 = cast<SingleValueInstruction>(V2)->getOperand(0);
        break;
      default:
        break;
    }
    if (V2 == V)
      return V2;
    V = V2;
  }
}


SILValue swift::getUnderlyingObjectStopAtMarkDependence(SILValue V) {
  while (true) {
    SILValue V2 = stripIndexingInsts(stripAddressProjections(stripCastsWithoutMarkDependence(V)));
    if (V2 == V)
      return V2;
    V = V2;
  }
}

static bool isRCIdentityPreservingCast(ValueKind Kind) {
  switch (Kind) {
  case ValueKind::UpcastInst:
  case ValueKind::UncheckedRefCastInst:
  case ValueKind::UnconditionalCheckedCastInst:
  case ValueKind::UnconditionalCheckedCastValueInst:
  case ValueKind::RefToBridgeObjectInst:
  case ValueKind::BridgeObjectToRefInst:
    return true;
  default:
    return false;
  }
}

/// Return the underlying SILValue after stripping off identity SILArguments if
/// we belong to a BB with one predecessor.
SILValue swift::stripSinglePredecessorArgs(SILValue V) {
  while (true) {
    auto *A = dyn_cast<SILArgument>(V);
    if (!A)
      return V;
    
    SILBasicBlock *BB = A->getParent();
    
    // First try and grab the single predecessor of our parent BB. If we don't
    // have one, bail.
    SILBasicBlock *Pred = BB->getSinglePredecessorBlock();
    if (!Pred)
      return V;
    
    // Then grab the terminator of Pred...
    TermInst *PredTI = Pred->getTerminator();
    
    // And attempt to find our matching argument.
    //
    // *NOTE* We can only strip things here if we know that there is no semantic
    // change in terms of upcasts/downcasts/enum extraction since this is used
    // by other routines here. This means that we can only look through
    // cond_br/br.
    //
    // For instance, routines that use stripUpcasts() do not want to strip off a
    // downcast that results from checked_cast_br.
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

SILValue swift::stripCastsWithoutMarkDependence(SILValue V) {
  while (true) {
    V = stripSinglePredecessorArgs(V);

    auto K = V->getKind();
    if (isRCIdentityPreservingCast(K) ||
        K == ValueKind::UncheckedTrivialBitCastInst) {
      V = cast<SingleValueInstruction>(V)->getOperand(0);
      continue;
    }

    return V;
  }
}

SILValue swift::stripCasts(SILValue V) {
  while (true) {
    V = stripSinglePredecessorArgs(V);
    
    auto K = V->getKind();
    if (isRCIdentityPreservingCast(K)
        || K == ValueKind::UncheckedTrivialBitCastInst
        || K == ValueKind::MarkDependenceInst) {
      V = cast<SingleValueInstruction>(V)->getOperand(0);
      continue;
    }
    
    return V;
  }
}

SILValue swift::stripUpCasts(SILValue V) {
  assert(V->getType().isClassOrClassMetatype() &&
         "Expected class or class metatype!");
  
  V = stripSinglePredecessorArgs(V);
  
  while (auto upcast = dyn_cast<UpcastInst>(V))
    V = stripSinglePredecessorArgs(upcast->getOperand());
  
  return V;
}

SILValue swift::stripClassCasts(SILValue V) {
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

SILValue swift::stripAddressAccess(SILValue V) {
  while (true) {
    switch (V->getKind()) {
    default:
      return V;
    case ValueKind::BeginBorrowInst:
    case ValueKind::BeginAccessInst:
      V = cast<SingleValueInstruction>(V)->getOperand(0);
    }
  }
}

SILValue swift::stripAddressProjections(SILValue V) {
  while (true) {
    V = stripSinglePredecessorArgs(V);
    if (!Projection::isAddressProjection(V))
      return V;
    V = cast<SingleValueInstruction>(V)->getOperand(0);
  }
}

SILValue swift::stripUnaryAddressProjections(SILValue V) {
  while (true) {
    V = stripSinglePredecessorArgs(V);
    if (!Projection::isAddressProjection(V))
      return V;
    auto *Inst = cast<SingleValueInstruction>(V);
    if (Inst->getNumOperands() > 1)
      return V;
    V = Inst->getOperand(0);
  }
}

SILValue swift::stripValueProjections(SILValue V) {
  while (true) {
    V = stripSinglePredecessorArgs(V);
    if (!Projection::isObjectProjection(V))
      return V;
    V = cast<SingleValueInstruction>(V)->getOperand(0);
  }
}

SILValue swift::stripIndexingInsts(SILValue V) {
  while (true) {
    if (!isa<IndexingInst>(V))
      return V;
    V = cast<IndexingInst>(V)->getBase();
  }
}

SILValue swift::stripExpectIntrinsic(SILValue V) {
  auto *BI = dyn_cast<BuiltinInst>(V);
  if (!BI)
    return V;
  if (BI->getIntrinsicInfo().ID != llvm::Intrinsic::expect)
    return V;
  return BI->getArguments()[0];
}

SILValue swift::stripBorrow(SILValue V) {
  if (auto *BBI = dyn_cast<BeginBorrowInst>(V))
    return BBI->getOperand();
  return V;
}

SingleValueInstruction *swift::getSingleValueCopyOrCast(SILInstruction *I) {
  if (auto *convert = dyn_cast<ConversionInst>(I))
    return convert;

  switch (I->getKind()) {
  default:
    return nullptr;
  case SILInstructionKind::CopyValueInst:
  case SILInstructionKind::CopyBlockInst:
  case SILInstructionKind::BeginBorrowInst:
  case SILInstructionKind::BeginAccessInst:
    return cast<SingleValueInstruction>(I);
  }
}

// Does this instruction terminate a SIL-level scope?
bool swift::isEndOfScopeMarker(SILInstruction *user) {
  switch (user->getKind()) {
  default:
    return false;
  case SILInstructionKind::EndAccessInst:
  case SILInstructionKind::EndBorrowInst:
  case SILInstructionKind::EndLifetimeInst:
    return true;
  }
}

bool swift::isIncidentalUse(SILInstruction *user) {
  return isEndOfScopeMarker(user) || user->isDebugInstruction() ||
         isa<FixLifetimeInst>(user);
}

bool swift::onlyAffectsRefCount(SILInstruction *user) {
  switch (user->getKind()) {
  default:
    return false;
  case SILInstructionKind::AutoreleaseValueInst:
  case SILInstructionKind::DestroyValueInst:
  case SILInstructionKind::ReleaseValueInst:
  case SILInstructionKind::RetainValueInst:
  case SILInstructionKind::StrongReleaseInst:
  case SILInstructionKind::StrongRetainInst:
  case SILInstructionKind::UnmanagedAutoreleaseValueInst:
  case SILInstructionKind::UnmanagedReleaseValueInst:
  case SILInstructionKind::UnmanagedRetainValueInst:
  case SILInstructionKind::UnownedReleaseInst:
  case SILInstructionKind::UnownedRetainInst:
    return true;
  }
}

SILValue swift::stripConvertFunctions(SILValue V) {
  while (true) {
    if (auto CFI = dyn_cast<ConvertFunctionInst>(V)) {
      V = CFI->getOperand();
      continue;
    }
    else if (auto *Cvt = dyn_cast<ConvertEscapeToNoEscapeInst>(V)) {
      V = Cvt->getOperand();
      continue;
    }
    break;
  }
  return V;
}


SILValue swift::isPartialApplyOfReabstractionThunk(PartialApplyInst *PAI) {
  if (PAI->getNumArguments() != 1)
    return SILValue();

  auto *Fun = PAI->getReferencedFunction();
  if (!Fun)
    return SILValue();

  // Make sure we have a reabstraction thunk.
  if (Fun->isThunk() != IsReabstractionThunk)
    return SILValue();

  // The argument should be a closure.
  auto Arg = PAI->getArgument(0);
  if (!Arg->getType().is<SILFunctionType>() ||
      (!Arg->getType().isReferenceCounted(PAI->getFunction()->getModule()) &&
       Arg->getType().getAs<SILFunctionType>()->getRepresentation() !=
           SILFunctionType::Representation::Thick))
    return SILValue();

  return Arg;
}

/// Given a block used as a noescape function argument, attempt to find
/// the Swift closure that invoking the block will call.
static SILValue findClosureStoredIntoBlock(SILValue V) {
  auto FnType = V->getType().castTo<SILFunctionType>();
  assert(FnType->getRepresentation() == SILFunctionTypeRepresentation::Block);

  // Given a no escape block argument to a function,
  // pattern match to find the noescape closure that invoking the block
  // will call:
  //     %noescape_closure = ...
  //     %storage = alloc_stack
  //     %storage_address = project_block_storage %storage
  //     store %noescape_closure to [init] %storage_address
  //     %block = init_block_storage_header %storage invoke %thunk
  //     %arg = copy_block %block

  InitBlockStorageHeaderInst *IBSHI = nullptr;

  // Look through block copies to find the initialization of block storage.
  while (true) {
    if (auto *CBI = dyn_cast<CopyBlockInst>(V)) {
      V = CBI->getOperand();
      continue;
    }

    IBSHI = dyn_cast<InitBlockStorageHeaderInst>(V);
    break;
  }

  if (!IBSHI)
    return nullptr;

  SILValue BlockStorage = IBSHI->getBlockStorage();
  auto *PBSI = BlockStorage->getSingleUserOfType<ProjectBlockStorageInst>();
  assert(PBSI && "Couldn't find block storage projection");

  auto *SI = PBSI->getSingleUserOfType<StoreInst>();
  assert(SI && "Couldn't find single store of function into block storage");

  return SI->getSrc();
}

/// Look through a value passed as a function argument to determine whether
/// it is a closure.
///
/// Return the partial_apply and a flag set to true if the closure is
/// indirectly captured by a reabstraction thunk.
FindClosureResult swift::findClosureForAppliedArg(SILValue V) {
  // Look through borrows.
  if (auto *bbi = dyn_cast<BeginBorrowInst>(V))
    V = bbi->getOperand();

  if (V->getType().getOptionalObjectType()) {
    auto *EI = dyn_cast<EnumInst>(V);
    if (!EI || !EI->hasOperand())
      return FindClosureResult(nullptr, false);

    V = EI->getOperand();
  }

  auto fnType = V->getType().getAs<SILFunctionType>();
  if (fnType->getRepresentation() == SILFunctionTypeRepresentation::Block) {
    V = findClosureStoredIntoBlock(V);
    if (!V)
      return FindClosureResult(nullptr, false);
  }
  auto *PAI = dyn_cast<PartialApplyInst>(stripConvertFunctions(V));
  if (!PAI)
    return FindClosureResult(nullptr, false);

  SILValue thunkArg = isPartialApplyOfReabstractionThunk(PAI);
  if (thunkArg) {
    // Handle reabstraction thunks recursively. This may reabstract over
    // @convention(block).
    auto result = findClosureForAppliedArg(thunkArg);
    return FindClosureResult(result.PAI, true);
  }
  return FindClosureResult(PAI, false);
}

namespace {

enum class OwnershipQualifiedKind {
  NotApplicable,
  Qualified,
  Unqualified,
};

struct OwnershipQualifiedKindVisitor : SILInstructionVisitor<OwnershipQualifiedKindVisitor, OwnershipQualifiedKind> {

  OwnershipQualifiedKind visitSILInstruction(SILInstruction *I) {
    return OwnershipQualifiedKind::NotApplicable;
  }

#define QUALIFIED_INST(CLASS) \
  OwnershipQualifiedKind visit ## CLASS(CLASS *I) { \
    return OwnershipQualifiedKind::Qualified;             \
  }
  QUALIFIED_INST(EndBorrowInst)
  QUALIFIED_INST(LoadBorrowInst)
  QUALIFIED_INST(CopyValueInst)
  QUALIFIED_INST(CopyUnownedValueInst)
  QUALIFIED_INST(DestroyValueInst)
#undef QUALIFIED_INST

  OwnershipQualifiedKind visitLoadInst(LoadInst *LI) {
    if (LI->getOwnershipQualifier() == LoadOwnershipQualifier::Unqualified)
      return OwnershipQualifiedKind::Unqualified;
    return OwnershipQualifiedKind::Qualified;
  }

  OwnershipQualifiedKind visitStoreInst(StoreInst *SI) {
    if (SI->getOwnershipQualifier() == StoreOwnershipQualifier::Unqualified)
      return OwnershipQualifiedKind::Unqualified;
    return OwnershipQualifiedKind::Qualified;
  }
};

} // end anonymous namespace

bool FunctionOwnershipEvaluator::evaluate(SILInstruction *I) {
  assert(I->getFunction() == F.get() && "Can not evaluate function ownership "
         "implications of an instruction that "
         "does not belong to the instruction "
         "that we are evaluating");

  switch (OwnershipQualifiedKindVisitor().visit(I)) {
  case OwnershipQualifiedKind::Unqualified: {
    // If we already know that the function has unqualified ownership, just
    // return early.
    if (!F.get()->hasQualifiedOwnership())
      return true;

    // Ok, so we know at this point that we have qualified ownership. If we have
    // seen any instructions with qualified ownership, we have an error since
    // the function mixes qualified and unqualified instructions.
    if (HasOwnershipQualifiedInstruction)
      return false;

    // Otherwise, set the function to have unqualified ownership. This will
    // ensure that no more Qualified instructions can be added to the given
    // function.
    F.get()->setUnqualifiedOwnership();
    return true;
  }
  case OwnershipQualifiedKind::Qualified: {
    // First check if our function has unqualified ownership. If we already do
    // have unqualified ownership, then we know that we have already seen an
    // unqualified ownership instruction. This means the function has both
    // qualified and unqualified instructions. =><=.
    if (!F.get()->hasQualifiedOwnership())
      return false;

    // Ok, at this point we know that we are still qualified. Since functions
    // start as qualified, we need to set the HasOwnershipQualifiedInstructions
    // so we do not need to look back through the function if we see an
    // unqualified instruction later on.
    HasOwnershipQualifiedInstruction = true;
    return true;
  }
  case OwnershipQualifiedKind::NotApplicable: {
    // Not Applicable instr
    return true;
  }
  }

  llvm_unreachable("Unhandled OwnershipQualifiedKind in switch.");
}
