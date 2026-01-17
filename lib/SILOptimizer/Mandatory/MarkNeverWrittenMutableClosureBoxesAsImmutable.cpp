//===--- MarkNeverWrittenMutableClosureBoxesAsImmutable.cpp ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-mark-never-written-mutable-closure-boxes-as-immutable"

#include "swift/SIL/ApplySite.h"
#include "swift/SIL/OperandDatastructures.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/SILIsolationInfo.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                            MARK: Implementation
//===----------------------------------------------------------------------===//

static bool isImmutable(SILValue start, StoreWeakInst *allowableWeakStore,
                        llvm::DenseSet<SILFunctionArgument *> &visitedArgs) {
  LLVM_DEBUG(llvm::dbgs() << "Checking in function "
                          << start->getFunction()->getName() << ": " << *start);
  // We store the partial apply that we are going to visit serially after we
  // finish processing the partial_apply so that we do not create too many
  // OperandWorklist. We can only create a finite amount of them.
  SmallVector<SILFunctionArgument *, 8> funcArgsToVisit;

  {
    // Walk the uses to search for the partial_apply. If we have a debug_value,
    // a move_value [lexical], or a begin_borrow [lexical].
    OperandWorklist worklist(start->getFunction());
    worklist.pushResultOperandsIfNotVisited(start);

    while (auto *use = worklist.pop()) {
      auto *user = use->getUser();
      LLVM_DEBUG(llvm::dbgs() << "    Visiting User: " << *user);

      // Uses to skip.
      if (isa<EndBorrowInst>(user) || isa<DestroyValueInst>(user) ||
          isa<LoadWeakInst>(user) || isa<EndAccessInst>(user) ||
          isa<DebugValueInst>(user) || isa<MarkFunctionEscapeInst>(user)) {
        LLVM_DEBUG(llvm::dbgs() << "        Ignoring!\n");
        continue;
      }

      // Uses to look through.
      if (isa<CopyValueInst>(user) || isa<ProjectBoxInst>(user) ||
          isa<MoveValueInst>(user) || isa<BeginBorrowInst>(user) ||
          isa<BeginAccessInst>(user)) {
        LLVM_DEBUG(llvm::dbgs() << "        Looking through!\n");
        worklist.pushResultOperandsIfNotVisited(user);
        continue;
      }

      // If we have a store_weak, continue if it is the store_weak that we are
      // ok with.
      if (auto *swi = dyn_cast<StoreWeakInst>(user);
          swi && swi == allowableWeakStore) {
        LLVM_DEBUG(llvm::dbgs() << "        Ignoring allowable store_weak!\n");
        continue;
      }

      // Visit partial_apply uses and see if:
      //
      // 1. We can look up the function.
      //
      // 2. If we already know that the function argument is inferred
      // immutable. In that case, we can just continue.
      //
      // 3. Then we check if we already visited the function argument. That
      // means that we know that it is not immutable if it has not been marked
      // yet... so just return false.
      //
      // 4. Otherwise, we add it to a worklist to process after we finish
      // walking uses in this function. We do this to ensure we do not create
      // too many OperandWorklists at the same time since we can only create a
      // finite amount of them at the same time.
      if (auto *pai = dyn_cast<PartialApplyInst>(user)) {
        if (auto *calleeFunc = pai->getReferencedFunctionOrNull()) {
          auto calleeArgIndex = ApplySite(pai).getCalleeArgIndex(*use);
          auto *fArg = cast<SILFunctionArgument>(
              calleeFunc->getArgument(calleeArgIndex));
          if (fArg->isInferredImmutable()) {
            LLVM_DEBUG(llvm::dbgs()
                       << "        Found partial_apply with inferred immutable "
                          "function arg. Can ignore it!\n");
            continue;
          }
          if (visitedArgs.count(fArg)) {
            LLVM_DEBUG(llvm::dbgs()
                       << "        Found mutable function arg user!\n");

            return false;
          }
          LLVM_DEBUG(llvm::dbgs()
                     << "        Found partial apply to check later!\n");
          funcArgsToVisit.push_back(fArg);
          continue;
        }
      }

      // Unrecognized user. Bail.
      LLVM_DEBUG(llvm::dbgs()
                 << "    Not transforming due to unhandled user!\n");
      return false;
    }
  }

  // Now check recursively if our function argument users are immutable. We do
  // this after we walk to avoid creating too many OperandWorklist.
  bool allFArgUsersImmutable = true;
  for (auto *fArg : funcArgsToVisit) {
    assert(!fArg->isInferredImmutable() && "Should have been checked earlier");
    visitedArgs.insert(fArg);
    if (isImmutable(fArg, nullptr, visitedArgs)) {
      fArg->setInferredImmutable(true);
      continue;
    }
    allFArgUsersImmutable = false;
  }

  return allFArgUsersImmutable;
}

/// Make sure that the given box fits out pattern matching conditions and return
/// its single initializing begin_borrow scope and store_weak so we can do a
/// later more intensive recursive check.
///
/// The conditions are:
///
/// 1. The box must be mutable.
///
/// 2. The box must contain a weak reference to a Sendable type.
///
/// 3. The box must have a single begin_borrow user that all uses are
/// initialized from.
///
/// 4. There must be a single store_weak that initializes the box from a
/// project_box from the single begin_borrow.
///
/// 5. The box should not have a debug_value use.
///
/// This is safe since later we are going to recursively look at uses of the
/// begin_borrow and if we find any memory uses that are a load_weak or a
/// different store_weak besides the one we found, we fail the box.
static StoreWeakInst *isPatternMatchableBox(AllocBoxInst *abi) {
  LLVM_DEBUG(llvm::dbgs() << "Checking if box can be matched: " << *abi);

  CanSILBoxType boxType = abi->getType().castTo<SILBoxType>();
  if (boxType->getNumFields() != 1 ||
      !SILIsolationInfo::boxContainsOnlySendableFields(abi)) {
    LLVM_DEBUG(llvm::dbgs() << "    Cannot match since either has multiple "
                               "fields or a non-Sendable field\n");
    return nullptr;
  }

  // For now to be conservative, only do this if we have a weak
  // parameter.
  if (auto ownership = boxType->getFieldType(*abi->getFunction(), 0)
                           .getReferenceStorageOwnership();
      !ownership || *ownership != ReferenceOwnership::Weak) {
    LLVM_DEBUG(llvm::dbgs()
               << "    Cannot match since field is not a weak reference\n");
    return nullptr;
  }

  BeginBorrowInst *singleBBI = nullptr;
  for (auto *use : abi->getUses()) {
    if (isa<DestroyValueInst>(use->getUser()) ||
        isa<DeallocBoxInst>(use->getUser()) ||
        isa<CopyValueInst>(use->getUser()))
      continue;
    auto *bbi = dyn_cast<BeginBorrowInst>(use->getUser());
    if (!bbi) {
      LLVM_DEBUG(llvm::dbgs()
                 << "    Cannot match since has a non-begin_borrow, "
                    "destroy_value, dealloc_box immediate user: "
                 << *use->getUser());
      return nullptr;
    }

    if (bbi->isFromVarDecl()) {
      LLVM_DEBUG(llvm::dbgs()
                 << "    Cannot match since begin_borrow from var_decl\n");
      return nullptr;
    }

    if (singleBBI) {
      LLVM_DEBUG(llvm::dbgs() << "    Cannot match since found multiple "
                                 "begin_borrow initializations\n");
      return nullptr;
    }
    singleBBI = bbi;
  }

  if (!singleBBI) {
    LLVM_DEBUG(llvm::dbgs() << "    Cannot match since did not find "
                               "begin_borrow for initialization\n");
    return nullptr;
  }

  // Now look for a single store_weak from a project_box from our singleBBI.
  //
  // DISCUSSION: We could be lazier here and leave the checking of multiple
  // store_weak to the later recursive check... but why not just check now and
  // end earlier.
  StoreWeakInst *singleStoreWeak = nullptr;
  for (auto *use : singleBBI->getUsersOfType<ProjectBoxInst>()) {
    if (auto *swi = use->getSingleUserOfType<StoreWeakInst>()) {
      if (singleStoreWeak) {
        LLVM_DEBUG(llvm::dbgs()
                   << "    Cannot match since found multiple store_weak\n");
        return nullptr;
      }
      singleStoreWeak = swi;
    }
  }
  if (!singleStoreWeak) {
    LLVM_DEBUG(llvm::dbgs() << "    Cannot match since did not find a single "
                               "store_weak initialization\n");
    return {};
  }

  return singleStoreWeak;
}

namespace {

class MarkNeverWrittenMutableClosureBoxesAsImmutable
    : public SILModuleTransform {
  void run() override {
    bool madeChange = false;
    llvm::DenseSet<SILFunctionArgument *> visitedArgs;
    for (auto &fn : *getModule()) {
      for (auto &block : fn) {
        for (auto &inst : block) {
          auto *abi = dyn_cast<AllocBoxInst>(&inst);
          if (!abi)
            continue;
          auto *singleInitialization = isPatternMatchableBox(abi);
          if (!singleInitialization ||
              !isImmutable(abi, singleInitialization, visitedArgs))
            continue;

          abi->setInferredImmutable(true);
          LLVM_DEBUG(llvm::dbgs() << "Marking Box as Inferred Immutable!\n");
          madeChange = true;
        }
      }
    }

    if (madeChange)
      invalidateAll();
  };
};

} // namespace

//===----------------------------------------------------------------------===//
//                         MARK: Top Level Entrypoint
//===----------------------------------------------------------------------===//

SILTransform *swift::createMarkNeverWrittenMutableClosureBoxesAsImmutable() {
  return new MarkNeverWrittenMutableClosureBoxesAsImmutable();
}
