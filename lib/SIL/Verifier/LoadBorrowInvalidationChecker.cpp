//===--- LoadBorrowInvalidationChecker.cpp --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This file defines a verifier that exhaustively validates that there aren't
/// any load_borrows in a SIL module that are invalidated by a write to their
/// underlying storage.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-load-borrow-invalidation-checker"
#include "VerifierPrivate.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/MultiMapCache.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/LinearLifetimeChecker.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

using namespace swift;
using namespace swift::silverifier;

//===----------------------------------------------------------------------===//
//                               Write Gatherer
//===----------------------------------------------------------------------===//

static bool constructValuesForBuiltinKey(
    Operand *op, BuiltinInst *bi,
    SmallVectorImpl<Operand *> &wellBehavedWriteAccumulator) {
  // If we definitely do not write to memory, just return true early.
  if (!bi->mayWriteToMemory()) {
    return true;
  }

  // TODO: Should we make this an exhaustive list so that when new builtins are
  // added, they need to actually update this code?
  wellBehavedWriteAccumulator.push_back(op);
  return true;
}

/// Returns true if we were able to ascertain that either the initialValue has
/// no write uses or all of the write uses were writes that we could understand.
static bool
constructValuesForKey(SILValue initialValue,
                      SmallVectorImpl<Operand *> &wellBehavedWriteAccumulator) {
  SmallVector<Operand *, 8> worklist(initialValue->getUses());

  while (!worklist.empty()) {
    auto *op = worklist.pop_back_val();

    // Skip type dependent operands.
    if (op->isTypeDependent()) {
      continue;
    }

    SILInstruction *user = op->getUser();

    if (Projection::isAddressProjection(user) ||
        isa<ProjectBlockStorageInst>(user)) {
      for (SILValue r : user->getResults()) {
        llvm::copy(r->getUses(), std::back_inserter(worklist));
      }
      continue;
    }

    if (auto *oeai = dyn_cast<OpenExistentialAddrInst>(user)) {
      // Mutable access!
      if (oeai->getAccessKind() != OpenedExistentialAccess::Immutable) {
        wellBehavedWriteAccumulator.push_back(op);
      }

      //  Otherwise, look through it and continue.
      llvm::copy(oeai->getUses(), std::back_inserter(worklist));
      continue;
    }

    // Add any destroy_addrs to the resultAccumulator.
    if (isa<DestroyAddrInst>(user)) {
      wellBehavedWriteAccumulator.push_back(op);
      continue;
    }

    // load_borrow, load_weak, load_unowned and incidental uses are fine as
    // well.
    if (isa<LoadBorrowInst>(user) || isIncidentalUse(user)) {
      continue;
    }

    if (auto *mdi = dyn_cast<MarkDependenceInst>(user)) {
      if (mdi->getValue() == op->get()) {
        wellBehavedWriteAccumulator.push_back(op);
      }
      continue;
    }

    if (isa<InjectEnumAddrInst>(user)) {
      wellBehavedWriteAccumulator.push_back(op);
      continue;
    }

    // switch_enum_addr never writes to memory.
    if (isa<SwitchEnumAddrInst>(user)) {
      continue;
    }

    if (auto *ccbi = dyn_cast<CheckedCastAddrBranchInst>(user)) {
      if (ccbi->getConsumptionKind() == CastConsumptionKind::TakeAlways ||
          ccbi->getConsumptionKind() == CastConsumptionKind::TakeOnSuccess) {
        wellBehavedWriteAccumulator.push_back(op);
        continue;
      }
    }

    if (isa<PointerToAddressInst>(user)) {
      continue;
    }

    // Skip store_borrow.
    if (isa<StoreBorrowInst>(user)) {
      continue;
    }

    // Look through immutable begin_access.
    if (auto *bai = dyn_cast<BeginAccessInst>(user)) {
      // If we do not have a read, mark this as a write.
      if (bai->getAccessKind() != SILAccessKind::Read) {
        wellBehavedWriteAccumulator.push_back(op);
      }

      // Otherwise, add the users to the worklist and continue.
      llvm::copy(bai->getUses(), std::back_inserter(worklist));
      continue;
    }

    // If we have a load, we just need to mark the load [take] as a write.
    if (auto *li = dyn_cast<LoadInst>(user)) {
      if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Take) {
        wellBehavedWriteAccumulator.push_back(op);
      }
      continue;
    }

#define NEVER_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, name, NAME)      \
  if (auto *li = dyn_cast<Load##Name##Inst>(user)) {                           \
    if (li->isTake() == IsTake) {                                              \
      wellBehavedWriteAccumulator.push_back(op);                               \
    }                                                                          \
    continue;                                                                  \
  }                                                                            \
  if (isa<Store##Name##Inst>(user)) {                                          \
    wellBehavedWriteAccumulator.push_back(op);                                 \
    continue;                                                                  \
  }
#include "swift/AST/ReferenceStorage.def"

    // If we have a FullApplySite, see if we use the value as an
    // indirect_guaranteed parameter. If we use it as inout, we need
    // interprocedural analysis that we do not perform here.
    if (auto fas = FullApplySite::isa(user)) {
      if (fas.isIndirectResultOperand(*op)) {
        wellBehavedWriteAccumulator.push_back(op);
        continue;
      }

      auto argConv = fas.getArgumentConvention(*op);

      // We should have an indirect convention here.
      if (!argConv.isIndirectConvention()) {
        llvm::errs() << "Full apply site taking non-indirect operand: "
                     << *user;
        return false;
      }

      if (argConv == SILArgumentConvention::Indirect_In_Guaranteed) {
        continue;
      }

      if (argConv.isInoutConvention()) {
        wellBehavedWriteAccumulator.push_back(op);
        continue;
      }

      if (argConv.isOwnedConvention()) {
        wellBehavedWriteAccumulator.push_back(op);
        continue;
      }

      // Otherwise, be conservative and return that we had a write that we did
      // not understand.
      llvm::errs() << "Full apply site not understood: " << *user;
      return false;
    }

    if (auto as = ApplySite::isa(user)) {
      wellBehavedWriteAccumulator.push_back(op);
      continue;
    }

    // Copy addr that read are just loads.
    if (auto *cai = dyn_cast<CopyAddrInst>(user)) {
      // If our value is the destination, this is a write.
      if (cai->getDest() == op->get()) {
        wellBehavedWriteAccumulator.push_back(op);
        continue;
      }

      // Ok, so we are Src by process of elimination. Make sure we are not being
      // taken.
      if (cai->isTakeOfSrc()) {
        wellBehavedWriteAccumulator.push_back(op);
        continue;
      }

      // Otherwise, we are safe and can continue.
      continue;
    }

    if (isa<StoreInst>(user) || isa<AssignInst>(user)) {
      wellBehavedWriteAccumulator.push_back(op);
      continue;
    }

    if (isa<DeallocStackInst>(user)) {
      continue;
    }

    if (isa<WitnessMethodInst>(user)) {
      continue;
    }

    if (isa<SelectEnumAddrInst>(user)) {
      continue;
    }

    // We consider address_to_pointer to be an escape from our system. The
    // frontend must protect the uses of the load_borrow as appropriate in other
    // ways (for instance by using a mark_dependence).
    if (isa<AddressToPointerInst>(user)) {
      continue;
    }

    if (auto *yi = dyn_cast<YieldInst>(user)) {
      auto info = yi->getYieldInfoForOperand(*op);
      if (info.isIndirectInGuaranteed()) {
        continue;
      }

      if (info.isIndirectMutating() || info.isConsumed()) {
        wellBehavedWriteAccumulator.push_back(op);
        continue;
      }
    }

    // Existential metatype doesnt write to memory.
    if (isa<ExistentialMetatypeInst>(user)) {
      continue;
    }

    // unconditional_checked_cast_addr does a take on its input memory.
    if (isa<UnconditionalCheckedCastAddrInst>(user)) {
      wellBehavedWriteAccumulator.push_back(op);
      continue;
    }

    if (auto *ccabi = dyn_cast<CheckedCastAddrBranchInst>(user)) {
      if (ccabi->getConsumptionKind() != CastConsumptionKind::CopyOnSuccess) {
        wellBehavedWriteAccumulator.push_back(op);
      }
      continue;
    }

    if (auto *bi = dyn_cast<BuiltinInst>(user)) {
      if (constructValuesForBuiltinKey(op, bi, wellBehavedWriteAccumulator)) {
        continue;
      }
    }

    // If we did not recognize the user, just return conservatively that it was
    // written to in a way we did not understand.
    llvm::errs() << "Function: " << user->getFunction()->getName() << "\n";
    llvm::errs() << "Value: " << op->get();
    llvm::errs() << "Unknown instruction!: " << *user;
    // llvm::report_fatal_error("Unable to handle instruction?!");
    return false;
  }

  // Ok, we finished our worklist and this address is not being written to.
  return true;
}

//===----------------------------------------------------------------------===//
//                   Load Borrow Never Invalidated Analysis
//===----------------------------------------------------------------------===//

LoadBorrowNeverInvalidatedAnalysis::LoadBorrowNeverInvalidatedAnalysis(
    DeadEndBlocks &deadEndBlocks)
    : cache(constructValuesForKey), deadEndBlocks(deadEndBlocks) {}

bool LoadBorrowNeverInvalidatedAnalysis::
    doesAddressHaveWriteThatInvalidatesLoadBorrow(
        LoadBorrowInst *lbi, ArrayRef<Operand *> endBorrowUses,
        SILValue address) {
  SmallPtrSet<SILBasicBlock *, 8> visitedBlocks;
  LinearLifetimeChecker checker(visitedBlocks, deadEndBlocks);
  auto writes = cache.get(address);

  // Treat None as a write.
  if (!writes) {
    llvm::errs() << "Failed to find cached writes for: " << *address;
    return false;
  }

  auto lbiProjPath =
      ProjectionPath::getProjectionPath(address, lbi->getOperand());

  // Then for each write...
  for (auto *op : *writes) {
    visitedBlocks.clear();

    // First see if the write is a dead end block. In such a case, just skip it.
    if (deadEndBlocks.isDeadEnd(op->getUser()->getParent())) {
      continue;
    }

    // See if the write is within the load borrow's lifetime. If it isn't, we
    // don't have to worry about it.
    if (!checker.validateLifetime(lbi, endBorrowUses, op)) {
      continue;
    }

    // Ok, we found a write that overlaps with our load_borrow. We now need to
    // prove that the write is to an address that can not trivially alias our
    // load_borrow.
    //
    // First we check if we were actually able to compute a projection path to
    // our address from lbiProjPath. If not, we have to a
    if (!lbiProjPath) {
      llvm::errs() << "Couldn't find path root for load_borrow: " << *lbi;
      return false;
    }

    auto writePath = ProjectionPath::getProjectionPath(address, op->get());
    if (!writePath) {
      llvm::errs() << "Couldn't find path root for write: " << *op->getUser();
      return false;
    }

    // The symmetric difference of two projection paths consists of the parts of
    // two projection paths that are unique to each of the two. Naturally, if
    // such a thing exists we must have that the two values can not alias.
    if (writePath->hasNonEmptySymmetricDifference(*lbiProjPath)) {
      continue;
    }

    llvm::errs() << "Write: " << *op->getUser();
    return false;
  }

  // Ok, we are good.
  return true;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

bool LoadBorrowNeverInvalidatedAnalysis::
    doesBoxHaveWritesThatInvalidateLoadBorrow(LoadBorrowInst *lbi,
                                              ArrayRef<Operand *> endBorrowUses,
                                              SILValue originalBox) {
  SILValue box = originalBox;
  SmallVector<ProjectBoxInst *, 2> otherProjBoxInsts;
  SmallVector<SILInstruction *, 16> worklist;

  // First walk up use->def from our project_box's operand to the actual box. As
  // we do the walk, we gather up any project_box that we see (for later write
  // checking) and then if we are either a copy_value or a begin_borrow strip
  // the value and continue.
  //
  // So by the end of this loop, the worklist will contain not the copy_value,
  // begin_borrow that we stripped, but rather any uses of those copy_value,
  // begin_borrow that we stripped. This is to make sure we find project_box
  // from webs of copy_value, begin_borrow.
  do {
    for (auto *use : box->getUses()) {
      auto *user = use->getUser();
      if (auto *pbi = dyn_cast<ProjectBoxInst>(user)) {
        otherProjBoxInsts.push_back(pbi);
        continue;
      }

      if (isa<CopyValueInst>(user) || isa<BeginBorrowInst>(user)) {
        worklist.push_back(user);
      }
    }

    if (isa<CopyValueInst>(box) || isa<BeginBorrowInst>(box)) {
      box = cast<SingleValueInstruction>(box)->getOperand(0);
      continue;
    }
  } while (false);

  // Now that we finished our walk and gathered up copy_value, begin_borrow,
  // visit each of those instructions recursively def->use, looking through
  // further copy_value, begin_borrow and stashing any project_box we see for
  // later write checking.
  while (!worklist.empty()) {
    auto *inst = worklist.pop_back_val();
    for (SILValue result : inst->getResults()) {
      for (auto *use : result->getUses()) {
        auto *user = use->getUser();
        if (auto *pbi = dyn_cast<ProjectBoxInst>(user)) {
          otherProjBoxInsts.push_back(pbi);
          continue;
        }

        if (isa<CopyValueInst>(user) || isa<BeginBorrowInst>(user)) {
          worklist.push_back(user);
        }
      }
    }
  }

  // Ok! We now know that we have all project_box from the "local phi" web of
  // the alloc_box our project_box is from. Now check that none of those have
  // simple aliasing writes when our load_borrow is live.
  while (!otherProjBoxInsts.empty()) {
    auto *otherProjBox = otherProjBoxInsts.pop_back_val();

    if (doesAddressHaveWriteThatInvalidatesLoadBorrow(lbi, endBorrowUses,
                                                      otherProjBox)) {
      return true;
    }
  }

  return false;
}

bool LoadBorrowNeverInvalidatedAnalysis::isNeverInvalidated(
    LoadBorrowInst *lbi) {
  SILValue address = getAddressAccess(lbi->getOperand());
  if (!address)
    return false;

  // If we have a let address, then we are already done.
  if (isLetAddress(stripAccessMarkers(address)))
    return true;

  // At this point, we know that we /may/ have writes. Now we go through various
  // cases to try and exhaustively identify if those writes overlap with our
  // load_borrow.
  SmallVector<Operand *, 8> endBorrowUses;
  transform(lbi->getUsersOfType<EndBorrowInst>(),
            std::back_inserter(endBorrowUses),
            [](EndBorrowInst *ebi) { return &ebi->getAllOperands()[0]; });

  // If we have a begin_access and...
  if (auto *bai = dyn_cast<BeginAccessInst>(address)) {
    // We do not have a modify, assume we are correct.
    if (bai->getAccessKind() != SILAccessKind::Modify) {
      return true;
    }

    // Otherwise, validate that any writes to our begin_access is not when the
    // load_borrow's result is live.
    return doesAddressHaveWriteThatInvalidatesLoadBorrow(lbi, endBorrowUses,
                                                         bai);
  }

  // Check if our unidentified access storage is a project_box. In such a case,
  // validate that all uses of the project_box are not writes that overlap with
  // our load_borrow's result. These are things that may not be a formal access
  // base.
  //
  // FIXME: we don't seem to verify anywhere that a pointer_to_address cannot
  // itself be derived from another address that is accessible in the same
  // function, either because it was returned from a call or directly
  // address_to_pointer'd.
  if (isa<PointerToAddressInst>(address)) {
    return doesAddressHaveWriteThatInvalidatesLoadBorrow(lbi, endBorrowUses,
                                                         address);
  }

  // If we have a project_box, we need to see if our base, modulo begin_borrow,
  // copy_value have any other project_box that we need to analyze.
  if (auto *pbi = dyn_cast<ProjectBoxInst>(address)) {
    return doesBoxHaveWritesThatInvalidateLoadBorrow(lbi, endBorrowUses,
                                                     pbi->getOperand());
  }

  auto storage = findAccessedStorage(address);

  // If we couldn't find an access storage, return that we are assumed to write.
  if (!storage) {
    llvm::errs() << "Couldn't compute access storage?!\n";
    return false;
  }

  switch (storage.getKind()) {
  case AccessedStorage::Stack: {
    // For now assume that stack is safe.
    return doesAddressHaveWriteThatInvalidatesLoadBorrow(lbi, endBorrowUses,
                                                         address);
  }
  case AccessedStorage::Argument: {
    auto *arg = cast<SILFunctionArgument>(storage.getArgument());
    // We return false if visit a non-address here. Object args are things like
    // pointer_to_address that we handle earlier.
    if (!arg->getType().isAddress())
      return false;
    if (arg->hasConvention(SILArgumentConvention::Indirect_In_Guaranteed))
      return true;
    return doesAddressHaveWriteThatInvalidatesLoadBorrow(lbi, endBorrowUses,
                                                         arg);
  }
  case AccessedStorage::Yield: {
    // For now, do this. Otherwise, check for in_guaranteed, etc.
    //
    // FIXME: The yielded address could overlap with another address in this
    // function.
    return true;
  }
  case AccessedStorage::Box: {
    return doesAddressHaveWriteThatInvalidatesLoadBorrow(lbi, endBorrowUses,
                                                         address);
  }
  case AccessedStorage::Class: {
    // Check that the write to the class's memory doesn't overlap with our
    // load_borrow.
    //
    // FIXME: how do we know that other projections of the same object don't
    // occur within the same function?
    return doesAddressHaveWriteThatInvalidatesLoadBorrow(lbi, endBorrowUses,
                                                         address);
  }
  case AccessedStorage::Tail: {
    // This should be as strong as the Class address case, but to handle it we
    // need to find all aliases of the object and all tail projections within
    // that object.
    return false;
  }
  case AccessedStorage::Global: {
    // Check that the write to the class's memory doesn't overlap with our
    // load_borrow.
    return doesAddressHaveWriteThatInvalidatesLoadBorrow(lbi, endBorrowUses,
                                                         address);
  }
  case AccessedStorage::Unidentified: {
    // Otherwise, we didn't understand this, so bail.
    llvm::errs() << "Unidentified access storage: " << storage;
    return false;
  }
  case AccessedStorage::Nested: {
    llvm_unreachable("Should have been handled above");
  }
  }
  llvm_unreachable("Covered switch isn't covered?!");
}
