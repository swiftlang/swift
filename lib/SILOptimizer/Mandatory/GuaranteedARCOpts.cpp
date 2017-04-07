//===--- GuaranteedARCOpts.cpp --------------------------------------------===//
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

#define DEBUG_TYPE "sil-guaranteed-arc-opts"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SIL/SILVisitor.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

STATISTIC(NumInstsEliminated, "Number of instructions eliminated");

namespace {

struct GuaranteedARCOptsVisitor
    : SILInstructionVisitor<GuaranteedARCOptsVisitor, bool> {
  bool visitValueBase(ValueBase *V) { return false; }
  bool visitDestroyAddrInst(DestroyAddrInst *DAI);
  bool visitStrongReleaseInst(StrongReleaseInst *SRI);
  bool visitDestroyValueInst(DestroyValueInst *DVI);
  bool visitReleaseValueInst(ReleaseValueInst *RVI);
};

} // end anonymous namespace

static SILBasicBlock::reverse_iterator
getPrevReverseIterator(SILInstruction *I) {
  return std::next(I->getIterator().getReverse());
}

bool GuaranteedARCOptsVisitor::visitDestroyAddrInst(DestroyAddrInst *DAI) {
  SILValue Operand = DAI->getOperand();

  for (auto II = getPrevReverseIterator(DAI), IE = DAI->getParent()->rend();
       II != IE;) {
    auto *Inst = &*II;
    ++II;

    if (auto *CA = dyn_cast<CopyAddrInst>(Inst)) {
      if (CA->getSrc() == Operand && !CA->isTakeOfSrc()) {
        CA->setIsTakeOfSrc(IsTake);
        DAI->eraseFromParent();
        NumInstsEliminated += 2;
        return true;
      }
    }

    // destroy_addrs commonly exist in a block of dealloc_stack's, which don't
    // affect take-ability.
    if (isa<DeallocStackInst>(Inst))
      continue;

    // This code doesn't try to prove tricky validity constraints about whether
    // it is safe to push the destroy_addr past interesting instructions.
    if (Inst->mayHaveSideEffects())
      break;
  }

  // If we didn't find a copy_addr to fold this into, emit the destroy_addr.
  return false;
}

static bool couldReduceStrongRefcount(SILInstruction *Inst) {
  // Simple memory accesses cannot reduce refcounts.
  if (isa<LoadInst>(Inst) || isa<StoreInst>(Inst) ||
      isa<RetainValueInst>(Inst) || isa<UnownedRetainInst>(Inst) ||
      isa<UnownedReleaseInst>(Inst) || isa<StrongRetainUnownedInst>(Inst) ||
      isa<StoreWeakInst>(Inst) || isa<StrongRetainInst>(Inst) ||
      isa<AllocStackInst>(Inst) || isa<DeallocStackInst>(Inst))
    return false;

  // Assign and copyaddr of trivial types cannot drop refcounts, and 'inits'
  // never can either.  Nontrivial ones can though, because the overwritten
  // value drops a retain.  We would have to do more alias analysis to be able
  // to safely ignore one of those.
  if (auto *AI = dyn_cast<AssignInst>(Inst)) {
    SILType StoredType = AI->getOperand(0)->getType();
    if (StoredType.isTrivial(Inst->getModule()) ||
        StoredType.is<ReferenceStorageType>())
      return false;
  }

  if (auto *CAI = dyn_cast<CopyAddrInst>(Inst)) {
    // Initializations can only increase refcounts.
    if (CAI->isInitializationOfDest())
      return false;

    SILType StoredType = CAI->getOperand(0)->getType().getObjectType();
    if (StoredType.isTrivial(Inst->getModule()) ||
        StoredType.is<ReferenceStorageType>())
      return false;
  }

  // This code doesn't try to prove tricky validity constraints about whether
  // it is safe to push the release past interesting instructions.
  return Inst->mayHaveSideEffects();
}

bool GuaranteedARCOptsVisitor::visitStrongReleaseInst(StrongReleaseInst *SRI) {
  SILValue Operand = SRI->getOperand();
  // Release on a functionref is a noop.
  if (isa<FunctionRefInst>(Operand)) {
    SRI->eraseFromParent();
    ++NumInstsEliminated;
    return true;
  }

  // Check to see if the instruction immediately before the insertion point is a
  // strong_retain of the specified operand.  If so, we can zap the pair.
  for (auto II = getPrevReverseIterator(SRI), IE = SRI->getParent()->rend();
       II != IE;) {
    auto *Inst = &*II;
    ++II;

    if (auto *SRA = dyn_cast<StrongRetainInst>(Inst)) {
      if (SRA->getOperand() == Operand) {
        SRA->eraseFromParent();
        SRI->eraseFromParent();
        NumInstsEliminated += 2;
        return true;
      }
      // Skip past unrelated retains.
      continue;
    }

    // Scan past simple instructions that cannot reduce strong refcounts.
    if (couldReduceStrongRefcount(Inst))
      break;
  }

  // If we didn't find a retain to fold this into, return false.
  return false;
}

bool GuaranteedARCOptsVisitor::visitDestroyValueInst(DestroyValueInst *DVI) {
  SILValue Operand = DVI->getOperand();
  for (auto II = getPrevReverseIterator(DVI), IE = DVI->getParent()->rend();
       II != IE;) {
    auto *Inst = &*II;
    ++II;

    if (auto *CVI = dyn_cast<CopyValueInst>(Inst)) {
      if (SILValue(CVI) == Operand || CVI->getOperand() == Operand) {
        CVI->replaceAllUsesWith(CVI->getOperand());
        CVI->eraseFromParent();
        DVI->eraseFromParent();
        NumInstsEliminated += 2;
        return true;
      }
      // Skip past unrelated retains.
      continue;
    }

    // Scan past simple instructions that cannot reduce refcounts.
    if (couldReduceStrongRefcount(Inst))
      break;
  }

  return false;
}

bool GuaranteedARCOptsVisitor::visitReleaseValueInst(ReleaseValueInst *RVI) {
  SILValue Operand = RVI->getOperand();

  for (auto II = getPrevReverseIterator(RVI), IE = RVI->getParent()->rend();
       II != IE;) {
    auto *Inst = &*II;
    ++II;

    if (auto *SRA = dyn_cast<RetainValueInst>(Inst)) {
      if (SRA->getOperand() == Operand) {
        SRA->eraseFromParent();
        RVI->eraseFromParent();
        NumInstsEliminated += 2;
        return true;
      }
      // Skip past unrelated retains.
      continue;
    }

    // Scan past simple instructions that cannot reduce refcounts.
    if (couldReduceStrongRefcount(Inst))
      break;
  }

  // If we didn't find a retain to fold this into, emit the release.
  return false;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

struct GuaranteedARCOpts : SILFunctionTransform {
  void run() override {
    GuaranteedARCOptsVisitor Visitor;

    bool MadeChange = false;
    SILFunction *F = getFunction();
    for (auto &BB : *F) {
      for (auto II = BB.begin(), IE = BB.end(); II != IE;) {
        SILInstruction *I = &*II;
        ++II;
        MadeChange |= Visitor.visit(I);
      }
    }

    if (MadeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }

  StringRef getName() override { return "Guaranteed ARC Opts"; }
};

} // end anonymous namespace

SILTransform *swift::createGuaranteedARCOpts() {
  return new GuaranteedARCOpts();
}
