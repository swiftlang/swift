//===--- OwnershipModelEliminator.cpp - Eliminate SILOwnership Instr. -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

  bool visitValueBase(ValueBase *V) { return false; }
  bool visitLoadInst(LoadInst *LI);
  bool visitStoreInst(StoreInst *SI);
  bool visitCopyValueInst(CopyValueInst *CVI);
  bool visitDestroyValueInst(DestroyValueInst *DVI);
  bool visitLoadBorrowInst(LoadBorrowInst *LBI);
  bool visitEndBorrowInst(EndBorrowInst *EBI) {
    EBI->eraseFromParent();
    return true;
  }
};

} // end anonymous namespace

bool OwnershipModelEliminatorVisitor::visitLoadInst(LoadInst *LI) {
  auto Qualifier = LI->getOwnershipQualifier();

  // If the qualifier is unqualified, there is nothing further to do
  // here. Just return.
  if (Qualifier == LoadOwnershipQualifier::Unqualified)
    return false;

  // Otherwise, we need to break down the load inst into its unqualified
  // components.
  B.setInsertionPoint(LI);
  B.setCurrentDebugScope(LI->getDebugScope());
  auto *UnqualifiedLoad = B.createLoad(LI->getLoc(), LI->getOperand());

  // If we have a copy, insert a retain_value. All other copies do not require
  // more work.
  if (Qualifier == LoadOwnershipQualifier::Copy) {
    B.createRetainValue(UnqualifiedLoad->getLoc(), UnqualifiedLoad,
                        Atomicity::Atomic);
  }

  // Then remove the qualified load and use the unqualified load as the def of
  // all of LI's uses.
  LI->replaceAllUsesWith(UnqualifiedLoad);
  LI->eraseFromParent();
  return true;
}

bool OwnershipModelEliminatorVisitor::visitStoreInst(StoreInst *SI) {
  auto Qualifier = SI->getOwnershipQualifier();

  // If the qualifier is unqualified, there is nothing further to do
  // here. Just return.
  if (Qualifier == StoreOwnershipQualifier::Unqualified)
    return false;

  // Otherwise, we need to break down the store.
  B.setInsertionPoint(SI);
  B.setCurrentDebugScope(SI->getDebugScope());

  if (Qualifier != StoreOwnershipQualifier::Assign) {
    // If the ownership qualifier is not an assign, we can just emit an
    // unqualified store.
    B.createStore(SI->getLoc(), SI->getSrc(), SI->getDest());
  } else {
    // If the ownership qualifier is [assign], then we need to eliminate the
    // old value.
    //
    // 1. Load old value.
    // 2. Store new value.
    // 3. Release old value.
    auto *Old = B.createLoad(SI->getLoc(), SI->getDest());
    B.createStore(SI->getLoc(), SI->getSrc(), SI->getDest());
    B.createReleaseValue(SI->getLoc(), Old, Atomicity::Atomic);
  }

  // Then remove the qualified store.
  SI->eraseFromParent();
  return true;
}

bool
OwnershipModelEliminatorVisitor::visitLoadBorrowInst(LoadBorrowInst *LBI) {
  // Break down the load borrow into an unqualified load.
  B.setInsertionPoint(LBI);
  B.setCurrentDebugScope(LBI->getDebugScope());
  auto *UnqualifiedLoad = B.createLoad(LBI->getLoc(), LBI->getOperand());

  // Then remove the qualified load and use the unqualified load as the def of
  // all of LI's uses.
  LBI->replaceAllUsesWith(UnqualifiedLoad);
  LBI->eraseFromParent();
  return true;
}

bool OwnershipModelEliminatorVisitor::visitCopyValueInst(CopyValueInst *CVI) {
  B.setInsertionPoint(CVI);
  B.setCurrentDebugScope(CVI->getDebugScope());
  B.createRetainValue(CVI->getLoc(), CVI->getOperand(), Atomicity::Atomic);
  CVI->replaceAllUsesWith(CVI->getOperand());
  CVI->eraseFromParent();
  return true;
}

bool OwnershipModelEliminatorVisitor::visitDestroyValueInst(DestroyValueInst *DVI) {
  B.setInsertionPoint(DVI);
  B.setCurrentDebugScope(DVI->getDebugScope());
  B.createReleaseValue(DVI->getLoc(), DVI->getOperand(), Atomicity::Atomic);
  DVI->eraseFromParent();
  return true;
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

namespace {

struct OwnershipModelEliminator : SILFunctionTransform {
  void run() override {
    SILFunction *F = getFunction();
    // We should only run this when SILOwnership is enabled.
    assert(F->getModule().getOptions().EnableSILOwnership &&
           "Can not run ownership model eliminator when SIL ownership is not "
           "enabled");
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
      // If we made any changes, we just changed instructions, so invalidate
      // that analysis.
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }

    // Now that we have lowered to unqualified ownership, set the unqualified
    // ownership flag on the function.
    F->setUnqualifiedOwnership();
  }

  StringRef getName() override { return "Ownership Model Eliminator"; }
};

} // end anonymous namespace

SILTransform *swift::createOwnershipModelEliminator() {
  return new OwnershipModelEliminator();
}
