//===--------- LICM.cpp - Loop invariant code motion ------*- C++ -*-------===//
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

#define DEBUG_TYPE "sil-licm"

#include "swift/SIL/Dominance.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/LoopAnalysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/CFG.h"
#include "swift/SILPasses/Utils/SILSSAUpdater.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"

#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/Debug.h"

#include "llvm/Support/CommandLine.h"

using namespace swift;

using ReadSet = llvm::SmallPtrSet<LoadInst *, 8>;
using WriteSet = SmallVector<SILInstruction *, 8>;

static bool mayWriteTo(AliasAnalysis *AA, WriteSet &MayWrites, LoadInst *LI) {
  for (auto *W : MayWrites)
    if (AA->mayWriteToMemory(W, LI->getOperand().getDef())) {
      DEBUG(llvm::dbgs() << " mayWriteTo\n" << *W << " to " << *LI << "\n");
      return true;
    }
  return false;
}

static void removeWrittenTo(AliasAnalysis *AA, ReadSet &Reads,
                            SILInstruction *ByInst) {

  // We can ignore retains, cond_fails, and dealloc_stacks.
  if (isa<StrongRetainInst>(ByInst) || isa<RetainValueInst>(ByInst) ||
      isa<CondFailInst>(ByInst) || isa<DeallocStackInst>(ByInst))
    return;

  SmallVector<LoadInst *, 8> RS(Reads.begin(), Reads.end());
  for (auto R : RS)
    if (AA->mayWriteToMemory(ByInst, R->getOperand())) {
      DEBUG(llvm::dbgs() << " mayWriteTo\n" << *ByInst << " to " << *R << "\n");
      Reads.erase(R);
    }
}

static bool hasLoopInvariantOperands(SILInstruction *I, SILLoop *L) {
  auto Opds = I->getAllOperands();

  return std::all_of(Opds.begin(), Opds.end(), [=](Operand &Op) {

    auto *Def = Op.get().getDef();

    // Operand is defined outside the loop.
    if (auto *Inst = dyn_cast<SILInstruction>(Def))
      return !L->contains(Inst->getParent());
    if (auto *Arg = dyn_cast<SILArgument>(Def))
      return !L->contains(Arg->getParent());

    return false;
  });
}

static bool mayHaveSideEffects(SILInstruction *I) {
  if (auto *AI = dyn_cast<ApplyInst>(I))
    if (auto *BFRI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee()))
      return !isSideEffectFree(BFRI);

  return I->mayHaveSideEffects();
}

static bool mayRead(SILInstruction *I) {
  if (auto *AI = dyn_cast<ApplyInst>(I))
    if (auto *BFRI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee()))
      return !isReadNone(BFRI);

  return I->mayReadFromMemory();
}

/// Check if an address does not depend on other values in a basic block.
static SILInstruction *addressIndependent(SILValue Addr) {
  Addr = Addr.stripCasts();
  if (SILGlobalAddrInst *SGAI = dyn_cast<SILGlobalAddrInst>(Addr))
    return SGAI;
  if (StructElementAddrInst *SEAI = dyn_cast<StructElementAddrInst>(Addr))
    return addressIndependent(SEAI->getOperand());
  return nullptr;
}

/// Check if two addresses can potentially access the same memory.
/// For now, return true when both can be traced to the same global variable.
static bool addressCanPairUp(SILValue Addr1, SILValue Addr2) {
  SILInstruction *Origin1 = addressIndependent(Addr1);
  return Origin1 && Origin1 == addressIndependent(Addr2);
}

/// Move cond_fail down if it can potentially help register promotion later.
static bool sinkCondFail(SILLoop *Loop) {
  // Only handle innermost loops for now.
  if (!Loop->getSubLoops().empty())
    return false;

  bool Changed = false;
  for (auto *BB : Loop->getBlocks()) {
    // A list of CondFails that can be moved down.
    SmallVector<CondFailInst*, 4> CFs;
    // A pair of load and store that are independent of the CondFails and
    // can potentially access the same memory.
    LoadInst *LIOfPair = nullptr;
    bool foundPair = false;

    for (auto &Inst : *BB) {
      if (foundPair) {
        // Move CFs to right before Inst.
        for (unsigned I = 0, E = CFs.size(); I < E; I++) {
          DEBUG(llvm::dbgs() << "sinking cond_fail down ");
          DEBUG(CFs[I]->dump());
          DEBUG(llvm::dbgs() << "  before ");
          DEBUG(Inst.dump());
          CFs[I]->moveBefore(&Inst);
        }
        Changed = true;

        foundPair = false;
        LIOfPair = nullptr;
      }

      if (auto CF = dyn_cast<CondFailInst>(&Inst)) {
        CFs.push_back(CF);
      } else if (auto LI = dyn_cast<LoadInst>(&Inst)) {
        if (addressIndependent(LI->getOperand())) {
          LIOfPair = LI;
        } else {
          CFs.clear();
          LIOfPair = nullptr;
        }
      } else if (auto SI = dyn_cast<StoreInst>(&Inst)) {
        if (addressIndependent(SI->getDest())) {
          if (LIOfPair &&
              addressCanPairUp(SI->getDest(), LIOfPair->getOperand()))
            foundPair = true;
        } else {
          CFs.clear();
          LIOfPair = nullptr;
        }
      } else if (mayHaveSideEffects(&Inst)) {
        CFs.clear();
        LIOfPair = nullptr;
      }
    }
  }
  return Changed;
}

static bool hoistInstructions(SILLoop *Loop, DominanceInfo *DT, SILLoopInfo *LI,
                              AliasAnalysis *AA, bool ShouldVerify) {
  auto HeaderBB = Loop->getHeader();
  if (!HeaderBB)
    return false;

  auto Preheader = Loop->getLoopPreheader();
  if (!Preheader) {
    // TODO: split off preheader
    return false;
  }

  // Only handle innermost loops for now.
  if (!Loop->getSubLoops().empty())
    return false;

  DEBUG(llvm::dbgs() << "hoisting in " << *Loop);
  DEBUG(HeaderBB->getParent()->dump());

  // Collect loads that are not clobbered.
  ReadSet Reads;
  WriteSet Writes;
  for (auto *BB : Loop->getBlocks()) {
    for (auto &Inst : *BB) {

      // Collect loads.
      auto LI = dyn_cast<LoadInst>(&Inst);
      if (LI) {
        if (!mayWriteTo(AA, Writes, LI))
          Reads.insert(LI);
        continue;
      }

      // Remove clobbered loads we have seen before.
      removeWrittenTo(AA, Reads, &Inst);

      if (Inst.mayHaveSideEffects())
        Writes.push_back(&Inst);
    }
  }

  // We found no read give up.
  if (Reads.empty()) {
    DEBUG(llvm::dbgs() << " not hoisting - no reads found\n");
    return false;
  }

  // Traverse the dominator tree starting at the loop header. Hoisting
  // instructions as we go.
  auto DTRoot = DT->getNode(HeaderBB);
  SmallVector<SILBasicBlock *, 8> ExitingBBs;
  Loop->getExitingBlocks(ExitingBBs);
  for (llvm::df_iterator<DominanceInfoNode *> It = llvm::df_begin(DTRoot),
                                              E = llvm::df_end(DTRoot);
       It != E;) {
    auto *CurBB = It->getBlock();

    // Don't decent into control-dependent code. Only traverse into basic blocks
    // that dominate all exits.
    if (!std::all_of(ExitingBBs.begin(), ExitingBBs.end(),
                     [=](SILBasicBlock *ExitBB) {
          if (DT->dominates(CurBB, ExitBB))
            return true;
          return false;
        })) {
      DEBUG(llvm::dbgs() << " skipping conditional block " << *CurBB << "\n");
      It.skipChildren();
      continue;
    }

    // We now that the block is guaranteed to be executed. Hoist if we can.
    for (auto InstIt = CurBB->begin(), E = CurBB->end(); InstIt != E; ) {
      SILInstruction *Inst = &*InstIt;
      ++InstIt;
      DEBUG(llvm::dbgs() << " looking at " << *Inst);

      // Can't hoist terminators.
      if (isa<TermInst>(Inst))
        continue;
      // Can't hoist allocation and dealloc stacks.
      if (isa<AllocationInst>(Inst) || isa<DeallocStackInst>(Inst))
        continue;

      // Can't hoist instructions which may have side effects.
      // We can (and must) hoist cond_fail instructions if the operand is
      // invariant. We must hoist them so that we preserve memory safety. A
      // cond_fail that would have protected (executed before) a memory access
      // must - after hoisting - also be executed before said access.
      if (mayHaveSideEffects(Inst) && !isa<CondFailInst>(Inst)) {
        DEBUG(llvm::dbgs() << " not side effect free\n");
        continue;
      }

      // Can't hoist if the instruction could read from memory and is not marked
      // as safe.
      LoadInst *LI = nullptr;
      if (mayRead(Inst) && !isa<CondFailInst>(Inst) &&
          (!(LI = dyn_cast<LoadInst>(Inst)) || !Reads.count(LI))) {
        DEBUG(llvm::dbgs() << " may read aliased writes\n");
        continue;
      }

      // The operands need to be loop invariant.
      if (!hasLoopInvariantOperands(Inst, Loop)) {
        DEBUG(llvm::dbgs() << " loop variant operands\n");
        continue;
      }

      Inst->moveBefore(Preheader->getTerminator());
    }

    // Next block in dominator tree.
    ++It;
  }
  return false;
}

namespace {
/// Hoist loop invariant code out of innermost loops.
class LICM : public SILFunctionTransform {

public:
  LICM() {}

  StringRef getName() override { return "SIL Loop Invariant Code Motion"; }

  void run() override {
    SILLoopAnalysis *LA = PM->getAnalysis<SILLoopAnalysis>();
    assert(LA);
    DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
    assert(DA);
    AliasAnalysis *AA = PM->getAnalysis<AliasAnalysis>();
    assert(AA);

    SILFunction *F = getFunction();
    assert(F);
    SILLoopInfo *LI = LA->getLoopInfo(F);
    assert(LI);
    DominanceInfo *DT = DA->getDomInfo(F);
    assert(DT);

    if (LI->empty()) {
      DEBUG(llvm::dbgs() << "No loops in " << F->getName() << "\n");
      return;
    }

    bool ShouldVerify = getOptions().VerifyAll;
    bool Changed = false;

    for (auto *LoopIt : *LI) {
      // Process loops recursively bottom-up in the loop tree.
      SmallVector<SILLoop *, 8> Worklist;
      Worklist.push_back(LoopIt);
      for (unsigned i = 0; i < Worklist.size(); ++i) {
        auto *L = Worklist[i];
        for (auto *SubLoop : *L)
          Worklist.push_back(SubLoop);
      }

      while (!Worklist.empty()) {
        SILLoop *work = Worklist.pop_back_val();
        Changed |= sinkCondFail(work);
        Changed |= hoistInstructions(work, DT, LI, AA,
                                     ShouldVerify);
      }
    }

    if (Changed) {
      // TODO: Verify: We have updated the DominanceInfo and SILLoopInfo. It
      // should be safe not to invalidate the CFG.
      PM->invalidateAnalysis(F, SILAnalysis::InvalidationKind::Instructions);
    }
  }
};
}

SILTransform *swift::createLICMPass() {
  return new LICM();
}
