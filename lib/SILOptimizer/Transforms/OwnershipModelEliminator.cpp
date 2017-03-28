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
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILVisitor.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                               Implementation
//===----------------------------------------------------------------------===//

namespace {

struct OwnershipModelEliminatorVisitor
    : SILInstructionVisitor<OwnershipModelEliminatorVisitor, bool> {
  SILBuilder &B;

  OwnershipModelEliminatorVisitor(SILBuilder &B) : B(B) {}
  void beforeVisit(ValueBase *V) {
    auto *I = cast<SILInstruction>(V);
    B.setInsertionPoint(I);
    B.setCurrentDebugScope(I->getDebugScope());
  }

  bool visitValueBase(ValueBase *V) { return false; }
  bool visitLoadInst(LoadInst *LI);
  bool visitStoreInst(StoreInst *SI);
  bool visitStoreBorrowInst(StoreBorrowInst *SI);
  bool visitCopyValueInst(CopyValueInst *CVI);
  bool visitCopyUnownedValueInst(CopyUnownedValueInst *CVI);
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
  if (CVI->getType().isAddressOnly(B.getModule()))
    return false;

  // Now that we have set the unqualified ownership flag, destroy value
  // operation will delegate to the appropriate strong_release, etc.
  B.emitCopyValueOperation(CVI->getLoc(), CVI->getOperand());
  CVI->replaceAllUsesWith(CVI->getOperand());
  CVI->eraseFromParent();
  return true;
}

bool OwnershipModelEliminatorVisitor::visitCopyUnownedValueInst(
    CopyUnownedValueInst *CVI) {
  B.createStrongRetainUnowned(CVI->getLoc(), CVI->getOperand(),
                              B.getDefaultAtomicity());
  // Users of copy_value_unowned expect an owned value. So we need to convert
  // our unowned value to a ref.
  auto *UTRI =
      B.createUnownedToRef(CVI->getLoc(), CVI->getOperand(), CVI->getType());
  CVI->replaceAllUsesWith(UTRI);
  CVI->eraseFromParent();
  return true;
}

bool OwnershipModelEliminatorVisitor::visitUnmanagedRetainValueInst(
    UnmanagedRetainValueInst *URVI) {
  // Now that we have set the unqualified ownership flag, destroy value
  // operation will delegate to the appropriate strong_release, etc.
  B.emitCopyValueOperation(URVI->getLoc(), URVI->getOperand());
  URVI->replaceAllUsesWith(URVI->getOperand());
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
  if (DVI->getOperand()->getType().isAddressOnly(B.getModule()))
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

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

namespace {

struct OwnershipModelEliminator : SILFunctionTransform {
  void run() override {
    SILFunction *F = getFunction();

    // Set F to have unqualified ownership.
    F->setUnqualifiedOwnership();

    bool MadeChange = false;
    SILBuilder B(*F);
    OwnershipModelEliminatorVisitor Visitor(B);

    for (auto &BB : *F) {
      for (auto II = BB.begin(), IE = BB.end(); II != IE;) {
        // Since we are going to be potentially removing instructions, we need
        // to make sure to grab out instruction and increment first.
        SILInstruction *I = &*II;
        ++II;

        MadeChange |= Visitor.visit(I);
      }
    }

    if (MadeChange) {
      invalidateAnalysis(
          SILAnalysis::InvalidationKind::BranchesAndInstructions);
    }
  }

  StringRef getName() override { return "Ownership Model Eliminator"; }
};

} // end anonymous namespace

SILTransform *swift::createOwnershipModelEliminator() {
  return new OwnershipModelEliminator();
}
