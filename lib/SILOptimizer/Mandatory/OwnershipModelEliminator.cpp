//===--- OwnershipModelEliminator.cpp - Eliminate SILOwnership Instr. -----===//
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
///
///  \file
///
///  This file contains a small pass that lowers SIL ownership instructions to
///  their constituent operations. This will enable us to separate
///  implementation
///  of Semantic ARC in SIL and SILGen from ensuring that all of the optimizer
///  passes respect Semantic ARC. This is done by running this pass right after
///  SILGen and as the pass pipeline is updated, moving this pass further and
///  further back in the pipeline.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-ownership-model-eliminator"

#include "swift/Basic/BlotSetVector.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/SimplifyInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

// Utility command line argument to dump the module before we eliminate
// ownership from it.
static llvm::cl::opt<std::string>
DumpBefore("sil-dump-before-ome-to-path", llvm::cl::Hidden);

//===----------------------------------------------------------------------===//
//                               Implementation
//===----------------------------------------------------------------------===//

namespace {

/// A high level SILInstruction visitor that lowers Ownership SSA from SIL.
///
/// NOTE: Erasing instructions must always be done by the method
/// eraseInstruction /and/ any instructions that are created in one visit must
/// not be deleted in the same visit since after each visit, we empty the
/// tracking list into the instructionsToSimplify array. We do this in order to
/// ensure that when we use inst-simplify on these instructions, we have
/// consistent non-ossa vs ossa code rather than an intermediate state.
struct OwnershipModelEliminatorVisitor
    : SILInstructionVisitor<OwnershipModelEliminatorVisitor, bool> {
  SILBuilder &builder;
  SmallVector<SILInstruction *, 8> trackingList;
  SmallBlotSetVector<SILInstruction *, 8> instructionsToSimplify;
  SILOpenedArchetypesTracker openedArchetypesTracker;

  OwnershipModelEliminatorVisitor(SILBuilder &newBuilder)
      : builder(newBuilder),
        openedArchetypesTracker(&newBuilder.getFunction()) {
    newBuilder.setTrackingList(&trackingList);
    newBuilder.setOpenedArchetypesTracker(&openedArchetypesTracker);
  }

  void drainTrackingList() {
    // Called before we visit a new instruction and before we ever erase an
    // instruction. This ensures that we can post-process instructions that need
    // simplification in a purely non-ossa world instead of an indeterminate
    // state mid elimination.
    while (!trackingList.empty()) {
      instructionsToSimplify.insert(trackingList.pop_back_val());
    }
  }

  void beforeVisit(SILInstruction *instToVisit) {
    // Add any elements to the tracking list that we currently have in the
    // tracking list that we haven't added yet.
    drainTrackingList();
    builder.setInsertionPoint(instToVisit);
    builder.setCurrentDebugScope(instToVisit->getDebugScope());
  }

  void eraseInstruction(SILInstruction *i) {
    // Before we erase anything, drain the tracking list.
    drainTrackingList();

    // Make sure to blot our instruction.
    instructionsToSimplify.erase(i);
    i->eraseFromParent();
  }

  void eraseInstructionAndRAUW(SingleValueInstruction *i, SILValue newValue) {
    // Make sure to blot our instruction.
    i->replaceAllUsesWith(newValue);
    eraseInstruction(i);
  }

  bool visitSILInstruction(SILInstruction *) { return false; }
  bool visitLoadInst(LoadInst *li);
  bool visitStoreInst(StoreInst *si);
  bool visitStoreBorrowInst(StoreBorrowInst *si);
  bool visitCopyValueInst(CopyValueInst *cvi);
  bool visitDestroyValueInst(DestroyValueInst *dvi);
  bool visitLoadBorrowInst(LoadBorrowInst *lbi);
  bool visitBeginBorrowInst(BeginBorrowInst *bbi) {
    eraseInstructionAndRAUW(bbi, bbi->getOperand());
    return true;
  }
  bool visitEndBorrowInst(EndBorrowInst *ebi) {
    eraseInstruction(ebi);
    return true;
  }
  bool visitEndLifetimeInst(EndLifetimeInst *eli) {
    eraseInstruction(eli);
    return true;
  }
  bool visitUncheckedOwnershipConversionInst(
      UncheckedOwnershipConversionInst *uoci) {
    eraseInstructionAndRAUW(uoci, uoci->getOperand());
    return true;
  }
  bool visitUnmanagedRetainValueInst(UnmanagedRetainValueInst *urvi);
  bool visitUnmanagedReleaseValueInst(UnmanagedReleaseValueInst *urvi);
  bool visitUnmanagedAutoreleaseValueInst(UnmanagedAutoreleaseValueInst *uavi);
  bool visitCheckedCastBranchInst(CheckedCastBranchInst *cbi);
  bool visitSwitchEnumInst(SwitchEnumInst *swi);
  bool visitDestructureStructInst(DestructureStructInst *dsi);
  bool visitDestructureTupleInst(DestructureTupleInst *dti);

  // We lower this to unchecked_bitwise_cast losing our assumption of layout
  // compatibility.
  bool visitUncheckedValueCastInst(UncheckedValueCastInst *uvci) {
    auto *newVal = builder.createUncheckedBitwiseCast(
        uvci->getLoc(), uvci->getOperand(), uvci->getType());
    eraseInstructionAndRAUW(uvci, newVal);
    return true;
  }

  void splitDestructure(SILInstruction *destructure,
                        SILValue destructureOperand);
};

} // end anonymous namespace

bool OwnershipModelEliminatorVisitor::visitLoadInst(LoadInst *li) {
  auto qualifier = li->getOwnershipQualifier();

  // If the qualifier is unqualified, there is nothing further to do
  // here. Just return.
  if (qualifier == LoadOwnershipQualifier::Unqualified)
    return false;

  SILValue result = builder.emitLoadValueOperation(
      li->getLoc(), li->getOperand(), li->getOwnershipQualifier());

  // Then remove the qualified load and use the unqualified load as the def of
  // all of LI's uses.
  eraseInstructionAndRAUW(li, result);
  return true;
}

bool OwnershipModelEliminatorVisitor::visitStoreInst(StoreInst *si) {
  auto qualifier = si->getOwnershipQualifier();

  // If the qualifier is unqualified, there is nothing further to do
  // here. Just return.
  if (qualifier == StoreOwnershipQualifier::Unqualified)
    return false;

  builder.emitStoreValueOperation(si->getLoc(), si->getSrc(), si->getDest(),
                                  si->getOwnershipQualifier());

  // Then remove the qualified store.
  eraseInstruction(si);
  return true;
}

bool OwnershipModelEliminatorVisitor::visitStoreBorrowInst(
    StoreBorrowInst *si) {
  builder.emitStoreValueOperation(si->getLoc(), si->getSrc(), si->getDest(),
                                  StoreOwnershipQualifier::Init);

  // Then remove the qualified store.
  eraseInstruction(si);
  return true;
}

bool OwnershipModelEliminatorVisitor::visitLoadBorrowInst(LoadBorrowInst *lbi) {
  // Break down the load borrow into an unqualified load.
  auto *unqualifiedLoad = builder.createLoad(
      lbi->getLoc(), lbi->getOperand(), LoadOwnershipQualifier::Unqualified);

  // Then remove the qualified load and use the unqualified load as the def of
  // all of LI's uses.
  eraseInstructionAndRAUW(lbi, unqualifiedLoad);
  return true;
}

bool OwnershipModelEliminatorVisitor::visitCopyValueInst(CopyValueInst *cvi) {
  // A copy_value of an address-only type cannot be replaced.
  if (cvi->getType().isAddressOnly(builder.getFunction()))
    return false;

  // Now that we have set the unqualified ownership flag, destroy value
  // operation will delegate to the appropriate strong_release, etc.
  builder.emitCopyValueOperation(cvi->getLoc(), cvi->getOperand());
  eraseInstructionAndRAUW(cvi, cvi->getOperand());
  return true;
}

bool OwnershipModelEliminatorVisitor::visitUnmanagedRetainValueInst(
    UnmanagedRetainValueInst *urvi) {
  // Now that we have set the unqualified ownership flag, destroy value
  // operation will delegate to the appropriate strong_release, etc.
  builder.emitCopyValueOperation(urvi->getLoc(), urvi->getOperand());
  eraseInstruction(urvi);
  return true;
}

bool OwnershipModelEliminatorVisitor::visitUnmanagedReleaseValueInst(
    UnmanagedReleaseValueInst *urvi) {
  // Now that we have set the unqualified ownership flag, destroy value
  // operation will delegate to the appropriate strong_release, etc.
  builder.emitDestroyValueOperation(urvi->getLoc(), urvi->getOperand());
  eraseInstruction(urvi);
  return true;
}

bool OwnershipModelEliminatorVisitor::visitUnmanagedAutoreleaseValueInst(
    UnmanagedAutoreleaseValueInst *UAVI) {
  // Now that we have set the unqualified ownership flag, destroy value
  // operation will delegate to the appropriate strong_release, etc.
  builder.createAutoreleaseValue(UAVI->getLoc(), UAVI->getOperand(),
                                 UAVI->getAtomicity());
  eraseInstruction(UAVI);
  return true;
}

bool OwnershipModelEliminatorVisitor::visitDestroyValueInst(
    DestroyValueInst *dvi) {
  // A destroy_value of an address-only type cannot be replaced.
  if (dvi->getOperand()->getType().isAddressOnly(builder.getFunction()))
    return false;

  // Now that we have set the unqualified ownership flag, destroy value
  // operation will delegate to the appropriate strong_release, etc.
  builder.emitDestroyValueOperation(dvi->getLoc(), dvi->getOperand());
  eraseInstruction(dvi);
  return true;
}

bool OwnershipModelEliminatorVisitor::visitCheckedCastBranchInst(
    CheckedCastBranchInst *cbi) {
  // In ownership qualified SIL, checked_cast_br must pass its argument to the
  // fail case so we can clean it up. In non-ownership qualified SIL, we expect
  // no argument from the checked_cast_br in the default case. The way that we
  // handle this transformation is that:
  //
  // 1. We replace all uses of the argument to the false block with a use of the
  // checked cast branch's operand.
  // 2. We delete the argument from the false block.
  SILBasicBlock *failureBlock = cbi->getFailureBB();
  if (failureBlock->getNumArguments() == 0)
    return false;
  failureBlock->getArgument(0)->replaceAllUsesWith(cbi->getOperand());
  failureBlock->eraseArgument(0);
  return true;
}

bool OwnershipModelEliminatorVisitor::visitSwitchEnumInst(
    SwitchEnumInst *swei) {
  // In ownership qualified SIL, switch_enum must pass its argument to the fail
  // case so we can clean it up. In non-ownership qualified SIL, we expect no
  // argument from the switch_enum in the default case. The way that we handle
  // this transformation is that:
  //
  // 1. We replace all uses of the argument to the false block with a use of the
  // checked cast branch's operand.
  // 2. We delete the argument from the false block.
  if (!swei->hasDefault())
    return false;

  SILBasicBlock *defaultBlock = swei->getDefaultBB();
  if (defaultBlock->getNumArguments() == 0)
    return false;
  defaultBlock->getArgument(0)->replaceAllUsesWith(swei->getOperand());
  defaultBlock->eraseArgument(0);
  return true;
}

void OwnershipModelEliminatorVisitor::splitDestructure(
    SILInstruction *destructureInst, SILValue destructureOperand) {
  assert((isa<DestructureStructInst>(destructureInst) ||
          isa<DestructureTupleInst>(destructureInst)) &&
         "Only destructure operations can be passed to splitDestructure");

  // First before we destructure anything, see if we can simplify any of our
  // instruction operands.

  SILModule &M = destructureInst->getModule();
  SILLocation loc = destructureInst->getLoc();
  SILType opType = destructureOperand->getType();

  llvm::SmallVector<Projection, 8> projections;
  Projection::getFirstLevelProjections(
      opType, M, builder.getTypeExpansionContext(), projections);
  assert(projections.size() == destructureInst->getNumResults());

  auto destructureResults = destructureInst->getResults();
  for (unsigned index : indices(destructureResults)) {
    SILValue result = destructureResults[index];

    // If our result doesnt have any uses, do not emit instructions, just skip
    // it.
    if (result->use_empty())
      continue;

    // Otherwise, create a projection.
    const auto &proj = projections[index];
    SingleValueInstruction *projInst =
        proj.createObjectProjection(builder, loc, destructureOperand).get();

    // First RAUW Result with ProjInst. This ensures that we have a complete IR
    // before we perform any simplifications.
    result->replaceAllUsesWith(projInst);
  }

  // Now that all of its uses have been eliminated, erase the destructure.
  eraseInstruction(destructureInst);
}

bool OwnershipModelEliminatorVisitor::visitDestructureStructInst(
    DestructureStructInst *dsi) {
  splitDestructure(dsi, dsi->getOperand());
  return true;
}

bool OwnershipModelEliminatorVisitor::visitDestructureTupleInst(
    DestructureTupleInst *dti) {
  splitDestructure(dti, dti->getOperand());
  return true;
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

static bool stripOwnership(SILFunction &func) {
  // If F is an external declaration, do not process it.
  if (func.isExternalDeclaration())
    return false;

  // Set F to have unqualified ownership.
  func.setOwnershipEliminated();

  bool madeChange = false;
  SmallVector<SILInstruction *, 32> createdInsts;
  SILBuilder builder(func);
  OwnershipModelEliminatorVisitor visitor(builder);

  for (auto &block : func) {
    // Change all arguments to have OwnershipKind::None.
    for (auto *arg : block.getArguments()) {
      arg->setOwnershipKind(OwnershipKind::None);
    }

    for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
      // Since we are going to be potentially removing instructions, we need
      // to make sure to increment our iterator before we perform any
      // visits.
      SILInstruction *inst = &*ii;
      ++ii;

      madeChange |= visitor.visit(inst);
    }
  }

  // Once we have finished processing all instructions, we should not be
  // consistently in non-ossa form. Now go through any instructions and simplify
  // using inst simplify.
  while (!visitor.instructionsToSimplify.empty()) {
    auto value = visitor.instructionsToSimplify.pop_back_val();
    if (!value.hasValue())
      continue;
    if (SILValue newValue = simplifyInstruction(*value)) {
      replaceAllSimplifiedUsesAndErase(*value, newValue,
                                       [&](SILInstruction *instToErase) {
                                         visitor.eraseInstruction(instToErase);
                                       });
      madeChange = true;
    }
  }

  return madeChange;
}

static void prepareNonTransparentSILFunctionForOptimization(ModuleDecl *,
                                                            SILFunction *f) {
  if (!f->hasOwnership() || f->isTransparent())
    return;

  LLVM_DEBUG(llvm::dbgs() << "After deserialization, stripping ownership in:"
                          << f->getName() << "\n");

  stripOwnership(*f);
}

static void prepareSILFunctionForOptimization(ModuleDecl *, SILFunction *f) {
  if (!f->hasOwnership())
    return;

  LLVM_DEBUG(llvm::dbgs() << "After deserialization, stripping ownership in:"
                          << f->getName() << "\n");

  stripOwnership(*f);
}

namespace {

struct OwnershipModelEliminator : SILFunctionTransform {
  bool skipTransparent;
  bool skipStdlibModule;

  OwnershipModelEliminator(bool skipTransparent, bool skipStdlibModule)
      : skipTransparent(skipTransparent), skipStdlibModule(skipStdlibModule) {}

  void run() override {
    if (DumpBefore.size()) {
      getFunction()->dump(DumpBefore.c_str());
    }

    auto *f = getFunction();
    auto &mod = getFunction()->getModule();

    // If we are supposed to skip the stdlib module and we are in the stdlib
    // module bail.
    if (skipStdlibModule && mod.isStdlibModule()) {
      return;
    }

    if (!f->hasOwnership())
      return;

    // If we were asked to not strip ownership from transparent functions in
    // /our/ module, return.
    if (skipTransparent && f->isTransparent())
      return;

    // Verify here to make sure ownership is correct before we strip.
    {
      // Add a pretty stack trace entry to tell users who see a verification
      // failure triggered by this verification check that they need to re-run
      // with -sil-verify-all to actually find the pass that introduced the
      // verification error.
      //
      // DISCUSSION: This occurs due to the crash from the verification
      // failure happening in the pass itself. This causes us to dump the
      // SILFunction and emit a msg that this pass (OME) is the culprit. This
      // is generally correct for most passes, but not for OME since we are
      // verifying before we have even modified the function to ensure that
      // all ownership invariants have been respected before we lower
      // ownership from the function.
      llvm::PrettyStackTraceString silVerifyAllMsgOnFailure(
          "Found verification error when verifying before lowering "
          "ownership. Please re-run with -sil-verify-all to identify the "
          "actual pass that introduced the verification error.");
      f->verify();
    }

    if (stripOwnership(*f)) {
      auto InvalidKind = SILAnalysis::InvalidationKind::BranchesAndInstructions;
      invalidateAnalysis(InvalidKind);
    }

    // If we were asked to strip transparent, we are at the beginning of the
    // performance pipeline. In such a case, we register a handler so that all
    // future things we deserialize have ownership stripped.
    using NotificationHandlerTy =
        FunctionBodyDeserializationNotificationHandler;
    std::unique_ptr<DeserializationNotificationHandler> ptr;
    if (skipTransparent) {
      if (!mod.hasRegisteredDeserializationNotificationHandlerForNonTransparentFuncOME()) {
        ptr.reset(new NotificationHandlerTy(
            prepareNonTransparentSILFunctionForOptimization));
        mod.registerDeserializationNotificationHandler(std::move(ptr));
        mod.setRegisteredDeserializationNotificationHandlerForNonTransparentFuncOME();
      }
    } else {
      if (!mod.hasRegisteredDeserializationNotificationHandlerForAllFuncOME()) {
        ptr.reset(new NotificationHandlerTy(prepareSILFunctionForOptimization));
        mod.registerDeserializationNotificationHandler(std::move(ptr));
        mod.setRegisteredDeserializationNotificationHandlerForAllFuncOME();
      }
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createOwnershipModelEliminator() {
  return new OwnershipModelEliminator(false /*skip transparent*/,
                                      false /*ignore stdlib*/);
}

SILTransform *swift::createNonTransparentFunctionOwnershipModelEliminator() {
  return new OwnershipModelEliminator(true /*skip transparent*/,
                                      false /*ignore stdlib*/);
}

SILTransform *
swift::createNonStdlibNonTransparentFunctionOwnershipModelEliminator() {
  return new OwnershipModelEliminator(true /*skip transparent*/,
                                      true /*ignore stdlib*/);
}
