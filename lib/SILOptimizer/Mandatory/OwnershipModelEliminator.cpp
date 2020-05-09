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

struct OwnershipModelEliminatorVisitor
    : SILInstructionVisitor<OwnershipModelEliminatorVisitor, bool> {
  SILBuilder &B;
  SILOpenedArchetypesTracker OpenedArchetypesTracker;

  OwnershipModelEliminatorVisitor(SILBuilder &B)
      : B(B), OpenedArchetypesTracker(&B.getFunction()) {
    B.setOpenedArchetypesTracker(&OpenedArchetypesTracker);
  }

  void beforeVisit(SILInstruction *I) {
    B.setInsertionPoint(I);
    B.setCurrentDebugScope(I->getDebugScope());
  }

  bool visitSILInstruction(SILInstruction *I) { return false; }
  bool visitLoadInst(LoadInst *LI);
  bool visitStoreInst(StoreInst *SI);
  bool visitStoreBorrowInst(StoreBorrowInst *SI);
  bool visitCopyValueInst(CopyValueInst *CVI);
  bool visitDestroyValueInst(DestroyValueInst *DVI);
  bool visitLoadBorrowInst(LoadBorrowInst *LBI);
  bool visitBeginBorrowInst(BeginBorrowInst *BBI) {
    BBI->replaceAllUsesWith(BBI->getOperand());
    BBI->eraseFromParent();
    return true;
  }
  bool visitEndBorrowInst(EndBorrowInst *EBI) {
    EBI->eraseFromParent();
    return true;
  }
  bool visitEndLifetimeInst(EndLifetimeInst *ELI) {
    ELI->eraseFromParent();
    return true;
  }
  bool visitUncheckedOwnershipConversionInst(
      UncheckedOwnershipConversionInst *UOCI) {
    UOCI->replaceAllUsesWith(UOCI->getOperand());
    UOCI->eraseFromParent();
    return true;
  }
  bool visitUnmanagedRetainValueInst(UnmanagedRetainValueInst *URVI);
  bool visitUnmanagedReleaseValueInst(UnmanagedReleaseValueInst *URVI);
  bool visitUnmanagedAutoreleaseValueInst(UnmanagedAutoreleaseValueInst *UAVI);
  bool visitCheckedCastBranchInst(CheckedCastBranchInst *CBI);
  bool visitSwitchEnumInst(SwitchEnumInst *SWI);
  bool visitDestructureStructInst(DestructureStructInst *DSI);
  bool visitDestructureTupleInst(DestructureTupleInst *DTI);
};

} // end anonymous namespace

bool OwnershipModelEliminatorVisitor::visitLoadInst(LoadInst *LI) {
  auto Qualifier = LI->getOwnershipQualifier();

  // If the qualifier is unqualified, there is nothing further to do
  // here. Just return.
  if (Qualifier == LoadOwnershipQualifier::Unqualified)
    return false;

  SILValue Result = B.emitLoadValueOperation(LI->getLoc(), LI->getOperand(),
                                             LI->getOwnershipQualifier());

  // Then remove the qualified load and use the unqualified load as the def of
  // all of LI's uses.
  LI->replaceAllUsesWith(Result);
  LI->eraseFromParent();
  return true;
}

bool OwnershipModelEliminatorVisitor::visitStoreInst(StoreInst *SI) {
  auto Qualifier = SI->getOwnershipQualifier();

  // If the qualifier is unqualified, there is nothing further to do
  // here. Just return.
  if (Qualifier == StoreOwnershipQualifier::Unqualified)
    return false;

  B.emitStoreValueOperation(SI->getLoc(), SI->getSrc(), SI->getDest(),
                            SI->getOwnershipQualifier());

  // Then remove the qualified store.
  SI->eraseFromParent();
  return true;
}

bool OwnershipModelEliminatorVisitor::visitStoreBorrowInst(
    StoreBorrowInst *SI) {
  B.emitStoreValueOperation(SI->getLoc(), SI->getSrc(), SI->getDest(),
                            StoreOwnershipQualifier::Init);

  // Then remove the qualified store.
  SI->eraseFromParent();
  return true;
}

bool
OwnershipModelEliminatorVisitor::visitLoadBorrowInst(LoadBorrowInst *LBI) {
  // Break down the load borrow into an unqualified load.
  auto *UnqualifiedLoad = B.createLoad(LBI->getLoc(), LBI->getOperand(),
                                       LoadOwnershipQualifier::Unqualified);

  // Then remove the qualified load and use the unqualified load as the def of
  // all of LI's uses.
  LBI->replaceAllUsesWith(UnqualifiedLoad);
  LBI->eraseFromParent();
  return true;
}

bool OwnershipModelEliminatorVisitor::visitCopyValueInst(CopyValueInst *CVI) {
  // A copy_value of an address-only type cannot be replaced.
  if (CVI->getType().isAddressOnly(B.getFunction()))
    return false;

  // Now that we have set the unqualified ownership flag, destroy value
  // operation will delegate to the appropriate strong_release, etc.
  B.emitCopyValueOperation(CVI->getLoc(), CVI->getOperand());
  CVI->replaceAllUsesWith(CVI->getOperand());
  CVI->eraseFromParent();
  return true;
}

bool OwnershipModelEliminatorVisitor::visitUnmanagedRetainValueInst(
    UnmanagedRetainValueInst *URVI) {
  // Now that we have set the unqualified ownership flag, destroy value
  // operation will delegate to the appropriate strong_release, etc.
  B.emitCopyValueOperation(URVI->getLoc(), URVI->getOperand());
  URVI->eraseFromParent();
  return true;
}

bool OwnershipModelEliminatorVisitor::visitUnmanagedReleaseValueInst(
    UnmanagedReleaseValueInst *URVI) {
  // Now that we have set the unqualified ownership flag, destroy value
  // operation will delegate to the appropriate strong_release, etc.
  B.emitDestroyValueOperation(URVI->getLoc(), URVI->getOperand());
  URVI->eraseFromParent();
  return true;
}

bool OwnershipModelEliminatorVisitor::visitUnmanagedAutoreleaseValueInst(
    UnmanagedAutoreleaseValueInst *UAVI) {
  // Now that we have set the unqualified ownership flag, destroy value
  // operation will delegate to the appropriate strong_release, etc.
  B.createAutoreleaseValue(UAVI->getLoc(), UAVI->getOperand(),
                           UAVI->getAtomicity());
  UAVI->eraseFromParent();
  return true;
}

bool OwnershipModelEliminatorVisitor::visitDestroyValueInst(DestroyValueInst *DVI) {
  // A destroy_value of an address-only type cannot be replaced.
  if (DVI->getOperand()->getType().isAddressOnly(B.getFunction()))
    return false;

  // Now that we have set the unqualified ownership flag, destroy value
  // operation will delegate to the appropriate strong_release, etc.
  B.emitDestroyValueOperation(DVI->getLoc(), DVI->getOperand());
  DVI->eraseFromParent();
  return true;
}

bool OwnershipModelEliminatorVisitor::visitCheckedCastBranchInst(
    CheckedCastBranchInst *CBI) {
  // In ownership qualified SIL, checked_cast_br must pass its argument to the
  // fail case so we can clean it up. In non-ownership qualified SIL, we expect
  // no argument from the checked_cast_br in the default case. The way that we
  // handle this transformation is that:
  //
  // 1. We replace all uses of the argument to the false block with a use of the
  // checked cast branch's operand.
  // 2. We delete the argument from the false block.
  SILBasicBlock *FailureBlock = CBI->getFailureBB();
  if (FailureBlock->getNumArguments() == 0)
    return false;
  FailureBlock->getArgument(0)->replaceAllUsesWith(CBI->getOperand());
  FailureBlock->eraseArgument(0);
  return true;
}

bool OwnershipModelEliminatorVisitor::visitSwitchEnumInst(
    SwitchEnumInst *SWEI) {
  // In ownership qualified SIL, switch_enum must pass its argument to the fail
  // case so we can clean it up. In non-ownership qualified SIL, we expect no
  // argument from the switch_enum in the default case. The way that we handle
  // this transformation is that:
  //
  // 1. We replace all uses of the argument to the false block with a use of the
  // checked cast branch's operand.
  // 2. We delete the argument from the false block.
  if (!SWEI->hasDefault())
    return false;

  SILBasicBlock *DefaultBlock = SWEI->getDefaultBB();
  if (DefaultBlock->getNumArguments() == 0)
    return false;
  DefaultBlock->getArgument(0)->replaceAllUsesWith(SWEI->getOperand());
  DefaultBlock->eraseArgument(0);
  return true;
}

static void splitDestructure(SILBuilder &B, SILInstruction *I, SILValue Op) {
  assert((isa<DestructureStructInst>(I) || isa<DestructureTupleInst>(I)) &&
         "Only destructure operations can be passed to splitDestructure");

  // First before we destructure anything, see if we can simplify any of our
  // instruction operands.

  SILModule &M = I->getModule();
  SILLocation Loc = I->getLoc();
  SILType OpType = Op->getType();

  llvm::SmallVector<Projection, 8> Projections;
  Projection::getFirstLevelProjections(OpType, M, B.getTypeExpansionContext(),
                                       Projections);
  assert(Projections.size() == I->getNumResults());

  auto Results = I->getResults();
  for (unsigned Index : indices(Results)) {
    SILValue Result = Results[Index];

    // If our result doesnt have any uses, do not emit instructions, just skip
    // it.
    if (Result->use_empty())
      continue;

    // Otherwise, create a projection.
    const auto &Proj = Projections[Index];
    SingleValueInstruction *ProjInst =
        Proj.createObjectProjection(B, Loc, Op).get();

    // If we can simplify, do so.
    if (SILValue NewV = simplifyInstruction(ProjInst)) {
      Result->replaceAllUsesWith(NewV);
      ProjInst->eraseFromParent();
      continue;
    }

    Result->replaceAllUsesWith(ProjInst);
  }

  // Now that all of its uses have been eliminated, erase the destructure.
  I->eraseFromParent();
}

bool OwnershipModelEliminatorVisitor::visitDestructureStructInst(
    DestructureStructInst *DSI) {
  splitDestructure(B, DSI, DSI->getOperand());
  return true;
}

bool OwnershipModelEliminatorVisitor::visitDestructureTupleInst(
    DestructureTupleInst *DTI) {
  splitDestructure(B, DTI, DTI->getOperand());
  return true;
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

static bool stripOwnership(SILFunction &F) {
  // If F is an external declaration, do not process it.
  if (F.isExternalDeclaration())
    return false;

  // Set F to have unqualified ownership.
  F.setOwnershipEliminated();

  bool MadeChange = false;
  SILBuilder B(F);
  OwnershipModelEliminatorVisitor Visitor(B);

  for (auto &BB : F) {
    // Change all arguments to have ValueOwnershipKind::None.
    for (auto *Arg : BB.getArguments()) {
      Arg->setOwnershipKind(ValueOwnershipKind::None);
    }

    for (auto II = BB.begin(), IE = BB.end(); II != IE;) {
      // Since we are going to be potentially removing instructions, we need
      // to make sure to increment our iterator before we perform any
      // visits.
      SILInstruction *I = &*II;
      ++II;

      MadeChange |= Visitor.visit(I);
    }
  }
  return MadeChange;
}

static void prepareNonTransparentSILFunctionForOptimization(ModuleDecl *,
                                                            SILFunction *F) {
  if (!F->hasOwnership() || F->isTransparent())
    return;

  LLVM_DEBUG(llvm::dbgs() << "After deserialization, stripping ownership in:"
                          << F->getName() << "\n");

  stripOwnership(*F);
}

static void prepareSILFunctionForOptimization(ModuleDecl *, SILFunction *F) {
  if (!F->hasOwnership())
    return;

  LLVM_DEBUG(llvm::dbgs() << "After deserialization, stripping ownership in:"
                          << F->getName() << "\n");

  stripOwnership(*F);
}

namespace {

struct OwnershipModelEliminator : SILModuleTransform {
  bool SkipTransparent;

  OwnershipModelEliminator(bool SkipTransparent)
      : SkipTransparent(SkipTransparent) {}

  void run() override {
    if (DumpBefore.size()) {
      getModule()->dump(DumpBefore.c_str());
    }

    auto &Mod = *getModule();
    for (auto &F : Mod) {
      // If F does not have ownership, skip it. We have no further work to do.
      if (!F.hasOwnership())
        continue;

      // If we were asked to not strip ownership from transparent functions in
      // /our/ module, continue.
      if (SkipTransparent && F.isTransparent())
        continue;

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
        F.verify();
      }

      if (stripOwnership(F)) {
        auto InvalidKind =
            SILAnalysis::InvalidationKind::BranchesAndInstructions;
        invalidateAnalysis(&F, InvalidKind);
      }
    }

    // If we were asked to strip transparent, we are at the beginning of the
    // performance pipeline. In such a case, we register a handler so that all
    // future things we deserialize have ownership stripped.
    using NotificationHandlerTy =
        FunctionBodyDeserializationNotificationHandler;
    std::unique_ptr<DeserializationNotificationHandler> ptr;
    if (SkipTransparent) {
      ptr.reset(new NotificationHandlerTy(
          prepareNonTransparentSILFunctionForOptimization));
    } else {
      ptr.reset(new NotificationHandlerTy(prepareSILFunctionForOptimization));
    }
    Mod.registerDeserializationNotificationHandler(std::move(ptr));
  }
};

} // end anonymous namespace

SILTransform *swift::createOwnershipModelEliminator() {
  return new OwnershipModelEliminator(false /*skip transparent*/);
}

SILTransform *swift::createNonTransparentFunctionOwnershipModelEliminator() {
  return new OwnershipModelEliminator(true /*skip transparent*/);
}
