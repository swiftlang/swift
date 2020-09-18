//===--- LoadCopyToLoadBorrowOpt.cpp --------------------------------------===//
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
/// Defines the main optimization that converts load [copy] -> load_borrow if we
/// can prove that the +1 is not actually needed and the memory loaded from is
/// never written to while the load [copy]'s object value is being used.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-semantic-arc-opts"

#include "OwnershipLiveRange.h"
#include "SemanticARCOptVisitor.h"
#include "swift/SIL/LinearLifetimeChecker.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace swift::semanticarc;

//===----------------------------------------------------------------------===//
//                        Well Behaved Write Analysis
//===----------------------------------------------------------------------===//

/// Returns true if we were able to ascertain that either the initialValue has
/// no write uses or all of the write uses were writes that we could understand.
bool swift::semanticarc::constructCacheValue(
    SILValue initialValue,
    SmallVectorImpl<Operand *> &wellBehavedWriteAccumulator) {
  SmallVector<Operand *, 8> worklist(initialValue->getUses());

  while (!worklist.empty()) {
    auto *op = worklist.pop_back_val();
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

    if (auto *si = dyn_cast<StoreInst>(user)) {
      // We must be the dest since addresses can not be stored.
      assert(si->getDest() == op->get());
      wellBehavedWriteAccumulator.push_back(op);
      continue;
    }

    // Add any destroy_addrs to the resultAccumulator.
    if (isa<DestroyAddrInst>(user)) {
      wellBehavedWriteAccumulator.push_back(op);
      continue;
    }

    // load_borrow and incidental uses are fine as well.
    if (isa<LoadBorrowInst>(user) || isIncidentalUse(user)) {
      continue;
    }

    // Look through begin_access and mark them/their end_borrow as users.
    if (auto *bai = dyn_cast<BeginAccessInst>(user)) {
      // If we do not have a read, mark this as a write. Also, insert our
      // end_access as well.
      if (bai->getAccessKind() != SILAccessKind::Read) {
        wellBehavedWriteAccumulator.push_back(op);
        transform(bai->getUsersOfType<EndAccessInst>(),
                  std::back_inserter(wellBehavedWriteAccumulator),
                  [](EndAccessInst *eai) { return &eai->getAllOperands()[0]; });
      }

      // And then add the users to the worklist and continue.
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

    // If we have a FullApplySite, we need to do per convention/inst logic.
    if (auto fas = FullApplySite::isa(user)) {
      // Begin by seeing if we have an in_guaranteed use. If we do, we are done.
      if (fas.getArgumentConvention(*op) ==
          SILArgumentConvention::Indirect_In_Guaranteed) {
        continue;
      }

      // Then see if we have an apply site that is not a coroutine apply
      // site. In such a case, without further analysis, we can treat it like an
      // instantaneous write and validate that it doesn't overlap with our load
      // [copy].
      if (!fas.beginsCoroutineEvaluation() &&
          fas.getArgumentConvention(*op).isInoutConvention()) {
        wellBehavedWriteAccumulator.push_back(op);
        continue;
      }

      // Otherwise, be conservative and return that we had a write that we did
      // not understand.
      LLVM_DEBUG(llvm::dbgs()
                 << "Function: " << user->getFunction()->getName() << "\n");
      LLVM_DEBUG(llvm::dbgs() << "Value: " << op->get());
      LLVM_DEBUG(llvm::dbgs() << "Unhandled apply site!: " << *user);

      return false;
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

    // If we did not recognize the user, just return conservatively that it was
    // written to in a way we did not understand.
    LLVM_DEBUG(llvm::dbgs()
               << "Function: " << user->getFunction()->getName() << "\n");
    LLVM_DEBUG(llvm::dbgs() << "Value: " << op->get());
    LLVM_DEBUG(llvm::dbgs() << "Unknown instruction!: " << *user);
    return false;
  }

  // Ok, we finished our worklist and this address is not being written to.
  return true;
}

//===----------------------------------------------------------------------===//
//                              Memory Analysis
//===----------------------------------------------------------------------===//

namespace {

/// A class that computes in a flow insensitive way if we can prove that our
/// storage is either never written to, or is initialized exactly once and never
/// written to again. In both cases, we can convert load [copy] -> load_borrow
/// safely.
class StorageGuaranteesLoadVisitor
    : public AccessUseDefChainVisitor<StorageGuaranteesLoadVisitor> {
  // The outer SemanticARCOptVisitor.
  SemanticARCOptVisitor &ARCOpt;

  // The live range of the original load.
  const OwnershipLiveRange &liveRange;

  // The current address being visited.
  SILValue currentAddress;

  Optional<bool> isWritten;

public:
  StorageGuaranteesLoadVisitor(SemanticARCOptVisitor &arcOpt, LoadInst *load,
                               const OwnershipLiveRange &liveRange)
      : ARCOpt(arcOpt), liveRange(liveRange),
        currentAddress(load->getOperand()) {}

  void answer(bool written) {
    currentAddress = nullptr;
    isWritten = written;
  }

  void next(SILValue address) { currentAddress = address; }

  void visitNestedAccess(BeginAccessInst *access) {
    // First see if we have read/modify. If we do not, just look through the
    // nested access.
    switch (access->getAccessKind()) {
    case SILAccessKind::Init:
    case SILAccessKind::Deinit:
      return next(access->getOperand());
    case SILAccessKind::Read:
    case SILAccessKind::Modify:
      break;
    }

    // Next check if our live range is completely in the begin/end access
    // scope. If so, we may be able to use a load_borrow here!
    SmallVector<Operand *, 8> endScopeUses;
    transform(access->getEndAccesses(), std::back_inserter(endScopeUses),
              [](EndAccessInst *eai) { return &eai->getAllOperands()[0]; });
    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
    LinearLifetimeChecker checker(visitedBlocks, ARCOpt.getDeadEndBlocks());
    if (!checker.validateLifetime(access, endScopeUses,
                                  liveRange.getAllConsumingUses())) {
      // If we fail the linear lifetime check, then just recur:
      return next(access->getOperand());
    }

    // Otherwise, if we have read, then we are done!
    if (access->getAccessKind() == SILAccessKind::Read) {
      return answer(false);
    }

    // If we have a modify, check if our value is /ever/ written to. If it is
    // never actually written to, then we convert to a load_borrow.
    auto result = ARCOpt.addressToExhaustiveWriteListCache.get(access);
    if (!result.hasValue()) {
      return answer(true);
    }

    if (result.getValue().empty()) {
      return answer(false);
    }

    return answer(true);
  }

  void visitArgumentAccess(SILFunctionArgument *arg) {
    // If this load_copy is from an indirect in_guaranteed argument, then we
    // know for sure that it will never be written to.
    if (arg->hasConvention(SILArgumentConvention::Indirect_In_Guaranteed)) {
      return answer(false);
    }

    // If we have an inout parameter that isn't ever actually written to, return
    // false.
    if (arg->getKnownParameterInfo().isIndirectMutating()) {
      auto wellBehavedWrites =
          ARCOpt.addressToExhaustiveWriteListCache.get(arg);
      if (!wellBehavedWrites.hasValue()) {
        return answer(true);
      }

      // No writes.
      if (wellBehavedWrites->empty()) {
        return answer(false);
      }

      // Ok, we have some writes. See if any of them are within our live
      // range. If any are, we definitely can not promote to load_borrow.
      SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
      SmallVector<BeginAccessInst *, 16> foundBeginAccess;
      LinearLifetimeChecker checker(visitedBlocks, ARCOpt.getDeadEndBlocks());
      SILValue introducerValue = liveRange.getIntroducer().value;
      if (!checker.usesNotContainedWithinLifetime(introducerValue,
                                                  liveRange.getDestroyingUses(),
                                                  *wellBehavedWrites)) {
        return answer(true);
      }

      // Finally, check if our live range is strictly contained within any of
      // our scoped writes.
      SmallVector<Operand *, 16> endAccessList;
      for (Operand *use : *wellBehavedWrites) {
        auto *bai = dyn_cast<BeginAccessInst>(use->getUser());
        if (!bai) {
          continue;
        }

        endAccessList.clear();
        llvm::transform(
            bai->getUsersOfType<EndAccessInst>(),
            std::back_inserter(endAccessList),
            [](EndAccessInst *eai) { return &eai->getAllOperands()[0]; });
        visitedBlocks.clear();

        // We know that our live range is based on a load [copy], so we know
        // that our value must have a defining inst.
        auto *definingInst =
            cast<LoadInst>(introducerValue->getDefiningInstruction());

        // Then if our defining inst is not in our bai, endAccessList region, we
        // know that the two ranges must be disjoint, so continue.
        if (!checker.validateLifetime(bai, endAccessList,
                                      &definingInst->getAllOperands()[0])) {
          continue;
        }

        // Otherwise, we do have an overlap, return true.
        return answer(true);
      }

      // Otherwise, there isn't an overlap, so we don't write to it.
      return answer(false);
    }

    // TODO: This should be extended:
    //
    // 1. We should be able to analyze in arguments and see if they are only
    //    ever destroyed at the end of the function. In such a case, we may be
    //    able to also to promote load [copy] from such args to load_borrow.
    return answer(true);
  }

  void visitGlobalAccess(SILValue global) {
    return answer(!AccessedStorage(global, AccessedStorage::Global)
                       .isLetAccess(&ARCOpt.F));
  }

  void visitClassAccess(RefElementAddrInst *field) {
    currentAddress = nullptr;

    // We know a let property won't be written to if the base object is
    // guaranteed for the duration of the access.
    // For non-let properties conservatively assume they may be written to.
    if (!field->getField()->isLet()) {
      return answer(true);
    }

    // The lifetime of the `let` is guaranteed if it's dominated by the
    // guarantee on the base. See if we can find a single borrow introducer for
    // this object. If we could not find a single such borrow introducer, assume
    // that our property is conservatively written to.
    SILValue baseObject = field->getOperand();
    auto value = getSingleBorrowIntroducingValue(baseObject);
    if (!value) {
      return answer(true);
    }

    // Ok, we have a single borrow introducing value. First do a quick check if
    // we have a non-local scope that is a function argument. In such a case, we
    // know statically that our let can not be written to in the current
    // function. To be conservative, assume that all other non-local scopes
    // write to memory.
    if (!value->isLocalScope()) {
      if (value->kind == BorrowedValueKind::SILFunctionArgument) {
        return answer(false);
      }

      // TODO: Once we model Coroutine results as non-local scopes, we should be
      // able to return false here for them as well.
      return answer(true);
    }

    // TODO: This is disabled temporarily for guaranteed phi args just for
    // staging purposes. Thus be conservative and assume true in these cases.
    if (value->kind == BorrowedValueKind::Phi) {
      return answer(true);
    }

    // Ok, we now know that we have a local scope whose lifetime we need to
    // analyze. With that in mind, gather up the lifetime ending uses of our
    // borrow scope introducing value and then use the linear lifetime checker
    // to check whether the copied value is dominated by the lifetime of the
    // borrow it's based on.
    SmallVector<Operand *, 4> endScopeInsts;
    value->visitLocalScopeEndingUses(
        [&](Operand *use) { endScopeInsts.push_back(use); });

    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
    LinearLifetimeChecker checker(visitedBlocks, ARCOpt.getDeadEndBlocks());

    // Returns true on success. So we invert.
    bool foundError = !checker.validateLifetime(
        baseObject, endScopeInsts, liveRange.getAllConsumingUses());
    return answer(foundError);
  }

  // TODO: Handle other access kinds?
  void visitBase(SILValue base, AccessedStorage::Kind kind) {
    return answer(true);
  }

  void visitNonAccess(SILValue addr) { return answer(true); }

  void visitCast(SingleValueInstruction *cast, Operand *parentAddr) {
    return next(parentAddr->get());
  }

  void visitPathComponent(SingleValueInstruction *projectedAddr,
                          Operand *parentAddr) {
    return next(parentAddr->get());
  }

  void visitPhi(SILPhiArgument *phi) {
    // We shouldn't have address phis in OSSA SIL, so we don't need to recur
    // through the predecessors here.
    return answer(true);
  }

  /// See if we have an alloc_stack that is only written to once by an
  /// initializing instruction.
  void visitStackAccess(AllocStackInst *stack) {
    SmallVector<Operand *, 8> destroyAddrOperands;
    bool initialAnswer = isSingleInitAllocStack(stack, destroyAddrOperands);
    if (!initialAnswer)
      return answer(true);

    // Then make sure that all of our load [copy] uses are within the
    // destroy_addr.
    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
    LinearLifetimeChecker checker(visitedBlocks, ARCOpt.getDeadEndBlocks());
    // Returns true on success. So we invert.
    bool foundError = !checker.validateLifetime(
        stack, destroyAddrOperands /*consuming users*/,
        liveRange.getAllConsumingUses() /*non consuming users*/);
    return answer(foundError);
  }

  bool doIt() {
    while (currentAddress) {
      visit(currentAddress);
    }
    return *isWritten;
  }
};

} // namespace

bool SemanticARCOptVisitor::isWrittenTo(LoadInst *load,
                                        const OwnershipLiveRange &lr) {
  StorageGuaranteesLoadVisitor visitor(*this, load, lr);
  return visitor.doIt();
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

// Convert a load [copy] from unique storage [read] that has all uses that can
// accept a guaranteed parameter to a load_borrow.
bool SemanticARCOptVisitor::visitLoadInst(LoadInst *li) {
  // This optimization can use more complex analysis. We should do some
  // experiments before enabling this by default as a guaranteed optimization.
  if (onlyGuaranteedOpts)
    return false;

  if (li->getOwnershipQualifier() != LoadOwnershipQualifier::Copy)
    return false;

  // Ok, we have our load [copy]. Make sure its value is truly a dead live range
  // implying it is only ever consumed by destroy_value instructions. If it is
  // consumed, we need to pass off a +1 value, so bail.
  //
  // FIXME: We should consider if it is worth promoting a load [copy]
  // -> load_borrow if we can put a copy_value on a cold path and thus
  // eliminate RR traffic on a hot path.
  OwnershipLiveRange lr(li);
  if (bool(lr.hasUnknownConsumingUse()))
    return false;

  // Then check if our address is ever written to. If it is, then we cannot use
  // the load_borrow because the stored value may be released during the loaded
  // value's live range.
  if (isWrittenTo(li, lr))
    return false;

  // Ok, we can perform our optimization. Convert the load [copy] into a
  // load_borrow.
  auto *lbi =
      SILBuilderWithScope(li).createLoadBorrow(li->getLoc(), li->getOperand());

  lr.insertEndBorrowsAtDestroys(lbi, getDeadEndBlocks(), lifetimeFrontier);
  std::move(lr).convertToGuaranteedAndRAUW(lbi, getCallbacks());
  return true;
}
