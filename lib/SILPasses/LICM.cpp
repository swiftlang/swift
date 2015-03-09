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
      DEBUG(llvm::dbgs() << "  mayWriteTo\n" << *W << " to " << *LI << "\n");
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
      DEBUG(llvm::dbgs() << "  mayWriteTo\n" << *ByInst << " to " << *R << "\n");
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

/// Check if an address does not depend on other values in a basic block.
static SILInstruction *addressIndependent(SILValue Addr) {
  Addr = Addr.stripCasts();
  if (GlobalAddrInst *SGAI = dyn_cast<GlobalAddrInst>(Addr))
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
      } else if (Inst.mayHaveSideEffects()) {
        CFs.clear();
        LIOfPair = nullptr;
      }
    }
  }
  return Changed;
}

static bool hoistInstructions(SILLoop *Loop, DominanceInfo *DT,
                              ReadSet &SafeReads) {
  auto Preheader = Loop->getLoopPreheader();
  if (!Preheader)
    return false;

  DEBUG(llvm::dbgs() << " Hoisting instructions.\n");

  auto HeaderBB = Loop->getHeader();
  bool Changed = false;

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
      DEBUG(llvm::dbgs() << "  skipping conditional block " << *CurBB << "\n");
      It.skipChildren();
      continue;
    }

    // We now that the block is guaranteed to be executed. Hoist if we can.
    for (auto InstIt = CurBB->begin(), E = CurBB->end(); InstIt != E; ) {
      SILInstruction *Inst = &*InstIt;
      ++InstIt;
      DEBUG(llvm::dbgs() << "  looking at " << *Inst);

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
      if (Inst->mayHaveSideEffects() && !isa<CondFailInst>(Inst)) {
        DEBUG(llvm::dbgs() << "   not side effect free.\n");
        continue;
      }

      // Can't hoist if the instruction could read from memory and is not marked
      // as safe.
      LoadInst *LI = nullptr;
      if (Inst->mayReadFromMemory() && !isa<CondFailInst>(Inst) &&
          (!(LI = dyn_cast<LoadInst>(Inst)) || !SafeReads.count(LI))) {
        DEBUG(llvm::dbgs() << "   may read aliased writes\n");
        continue;
      }

      // The operands need to be loop invariant.
      if (!hasLoopInvariantOperands(Inst, Loop)) {
        DEBUG(llvm::dbgs() << "   loop variant operands\n");
        continue;
      }

      DEBUG(llvm::dbgs() << "   hoisting to preheader.\n");
      Changed = true;
      Inst->moveBefore(Preheader->getTerminator());
    }

    // Next block in dominator tree.
    ++It;
  }
  return Changed;
}

static bool sinkFixLiftime(SILLoop *Loop, DominanceInfo *DomTree,
                           SILLoopInfo *LI) {
  DEBUG(llvm::errs() << " Sink fix_lifetime attempt\n");
  auto Preheader = Loop->getLoopPreheader();
  if (!Preheader)
    return false;

  // Only handle innermost loops for now.
  if (!Loop->getSubLoops().empty())
    return false;

  // Only handle single exit blocks for now.
  auto *ExitBB = Loop->getExitBlock();
  if (!ExitBB)
    return false;
  auto *ExitingBB = Loop->getExitingBlock();
  if (!ExitingBB)
    return false;

  // We can sink fix_lifetime instructions if there are no reference counting
  // instructions in the loop.
  SmallVector<FixLifetimeInst *, 16> FixLifetimeInsts;
  for (auto *BB : Loop->getBlocks()) {
    for (auto &Inst : *BB) {
      if (auto FLI = dyn_cast<FixLifetimeInst>(&Inst)) {
        FixLifetimeInsts.push_back(FLI);
      } else if (Inst.mayHaveSideEffects() && !isa<LoadInst>(&Inst) &&
                 !isa<StoreInst>(&Inst)) {
        DEBUG(llvm::errs() << "  mayhavesideeffects because of" << Inst);
        DEBUG(Inst.getParent()->dump());
        return false;
      }
    }
  }

  // Sink the fix_lifetime instruction.
  bool Changed = false;
  for (auto *FLI : FixLifetimeInsts)
    if (DomTree->dominates(FLI->getOperand().getDef()->getParentBB(),
                           Preheader)) {
      auto Succs = ExitingBB->getSuccs();
      for (unsigned EdgeIdx = 0; EdgeIdx <  Succs.size(); ++EdgeIdx) {
        SILBasicBlock *BB = Succs[EdgeIdx];
        if (BB == ExitBB) {
          auto *SplitBB = splitCriticalEdge(ExitingBB->getTerminator(), EdgeIdx,
                                            DomTree, LI);
          auto *OutsideBB = SplitBB ? SplitBB : ExitBB;
          // Update the ExitBB.
          ExitBB = OutsideBB;
          DEBUG(llvm::errs() << "  moving fix_lifetime to exit BB " << *FLI);
          FLI->moveBefore(&*OutsideBB->begin());
          Changed = true;
        }
      }
    } else {
      DEBUG(llvm::errs() << "  does not dominate " << *FLI);
    }

  return Changed;
}

namespace {
/// \brief Summmary of may writes occuring in the loop tree rooted at \p
/// Loop. This includes all writes of the sub loops and the loop itself.
struct LoopNestSummary {
  SILLoop *Loop;
  WriteSet MayWrites;

  LoopNestSummary(SILLoop *Curr) : Loop(Curr) {}


  void copySummary(LoopNestSummary &Other) {
    MayWrites.append(Other.MayWrites.begin(), Other.MayWrites.end());
  }

  LoopNestSummary(const LoopNestSummary &) = delete;
  LoopNestSummary &operator=(const LoopNestSummary &) = delete;
  LoopNestSummary(LoopNestSummary &&) = delete;
};

/// \brief Optimize the loop tree bottom up propagating loop's summaries up the
/// loop tree.
class LoopTreeOptimization {
  llvm::DenseMap<SILLoop *, std::unique_ptr<LoopNestSummary>>
      LoopNestSummaryMap;
  SmallVector<SILLoop *, 8> BotUpWorkList;
  SILLoopInfo *LoopInfo;
  AliasAnalysis *AA;
  DominanceInfo *DomTree;
  bool Changed;

public:
  LoopTreeOptimization(SILLoop *TopLevelLoop, SILLoopInfo *LI,
                       AliasAnalysis *AA, DominanceInfo *DT)
      : LoopInfo(LI), AA(AA), DomTree(DT), Changed(false) {
    // Collect loops for a recursive bottom-up traversal in the loop tree.
    BotUpWorkList.push_back(TopLevelLoop);
    for (unsigned i = 0; i < BotUpWorkList.size(); ++i) {
      auto *L = BotUpWorkList[i];
      for (auto *SubLoop : *L)
        BotUpWorkList.push_back(SubLoop);
    }
  }

  /// \brief Optimize this loop tree.
  bool optimize();

protected:
  /// \brief Propagate the sub-loops' summaries up to the current loop.
  void propagateSummaries(std::unique_ptr<LoopNestSummary> &CurrSummary);

  /// \brief Collect a set of reads that can be hoisted to the loop's preheader.
  void analyzeCurrentLoop(std::unique_ptr<LoopNestSummary> &CurrSummary,
                          ReadSet &SafeReads);

  /// \brief Optimize the current loop nest.
  void optimizeLoop(SILLoop *CurrentLoop, ReadSet &SafeReads);
};
}

bool LoopTreeOptimization::optimize() {
  // Process loops bottom up in the loop tree.
  while (!BotUpWorkList.empty()) {
    SILLoop *CurrentLoop = BotUpWorkList.pop_back_val();
    DEBUG(llvm::dbgs() << "Processing loop " << *CurrentLoop);

    // Collect all summary of all sub loops of the current loop. Since we
    // process the loop tree bottom up they are guaranteed to be available in
    // the map.
    auto CurrLoopSummary = llvm::make_unique<LoopNestSummary>(CurrentLoop);
    propagateSummaries(CurrLoopSummary);

    // Analyse the current loop for reads that can be hoisted.
    ReadSet SafeReads;
    analyzeCurrentLoop(CurrLoopSummary, SafeReads);

    optimizeLoop(CurrentLoop, SafeReads);

    // Store the summary for parent loops to use.
    LoopNestSummaryMap[CurrentLoop] = std::move(CurrLoopSummary);
  }
  return Changed;
}

void LoopTreeOptimization::propagateSummaries(
    std::unique_ptr<LoopNestSummary> &CurrSummary) {
  for (auto *SubLoop : *CurrSummary->Loop) {
    assert(LoopNestSummaryMap.count(SubLoop) && "Must have data for sub loops");
    CurrSummary->copySummary(*LoopNestSummaryMap[SubLoop]);
    LoopNestSummaryMap.erase(SubLoop);
  }
}

void LoopTreeOptimization::analyzeCurrentLoop(
    std::unique_ptr<LoopNestSummary> &CurrSummary, ReadSet &SafeReads) {
  WriteSet &MayWrites = CurrSummary->MayWrites;
  SILLoop *Loop = CurrSummary->Loop;
  DEBUG(llvm::dbgs() << " Analysing accesses.\n");

  for (auto *BB : Loop->getBlocks()) {
    for (auto &Inst : *BB) {
      // Ignore fix_lifetime instructions.
      if (isa<FixLifetimeInst>(&Inst))
        continue;

      // Collect loads.
      auto LI = dyn_cast<LoadInst>(&Inst);
      if (LI) {
        if (!mayWriteTo(AA, MayWrites, LI))
          SafeReads.insert(LI);
        continue;
      }

      // Remove clobbered loads we have seen before.
      removeWrittenTo(AA, SafeReads, &Inst);

      if (Inst.mayHaveSideEffects())
        MayWrites.push_back(&Inst);
    }
  }
}

void LoopTreeOptimization::optimizeLoop(SILLoop *CurrentLoop,
                                        ReadSet &SafeReads) {
  Changed |= sinkCondFail(CurrentLoop);
  Changed |= hoistInstructions(CurrentLoop, DomTree, SafeReads);
  Changed |= sinkFixLiftime(CurrentLoop, DomTree, LoopInfo);
}

namespace {
/// Hoist loop invariant code out of innermost loops.
class LICM : public SILFunctionTransform {

public:
  LICM() {}

  StringRef getName() override { return "SIL Loop Invariant Code Motion"; }

  void run() override {
    SILFunction *F = getFunction();
    SILLoopAnalysis *LA = PM->getAnalysis<SILLoopAnalysis>();
    SILLoopInfo *LoopInfo = LA->getLoopInfo(F);

    if (LoopInfo->empty()) {
      DEBUG(llvm::dbgs() << "No loops in " << F->getName() << "\n");
      return;
    }

    DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
    AliasAnalysis *AA = PM->getAnalysis<AliasAnalysis>();
    DominanceInfo *DomTree = nullptr;

    DEBUG(llvm::dbgs() << "Processing loops in " << F->getName() << "\n");
    bool Changed = false;

    for (auto *TopLevelLoop : *LoopInfo) {
      if (!DomTree) DomTree = DA->getDomInfo(F);
      LoopTreeOptimization Opt(TopLevelLoop, LoopInfo, AA, DomTree);
      Changed |= Opt.optimize();
    }

    if (Changed) {
      // TODO: Verify: We have updated the DominanceInfo and SILLoopInfo. It
      // should be safe not to invalidate the CFG.
      PM->invalidateAnalysis(F, SILAnalysis::InvalidationKind::Instructions);
    }
  }
};
}

SILTransform *swift::createLICM() {
  return new LICM();
}
