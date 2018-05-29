//===--- LICM.cpp - Loop invariant code motion ----------------------------===//
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

#define DEBUG_TYPE "sil-licm"

#include "swift/SIL/Dominance.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SILOptimizer/Analysis/SideEffectAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/InstructionUtils.h"

#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/Debug.h"

#include "llvm/Support/CommandLine.h"

using namespace swift;

/// Instructions which read from memory, e.g. loads, or function calls without
/// side effects.
using ReadSet = llvm::SmallPtrSet<SILInstruction *, 8>;

/// Instructions which (potentially) write memory.
using WriteSet = SmallVector<SILInstruction *, 8>;

/// Returns true if the \p MayWrites set contains any memory writes which may
/// alias with the memory addressed by \a LI.
static bool mayWriteTo(AliasAnalysis *AA, WriteSet &MayWrites, LoadInst *LI) {
  for (auto *W : MayWrites)
    if (AA->mayWriteToMemory(W, LI->getOperand())) {
      DEBUG(llvm::dbgs() << "  mayWriteTo\n" << *W << " to " << *LI << "\n");
      return true;
    }
  return false;
}

/// Returns true if the \p MayWrites set contains any memory writes which may
/// alias with any memory which is read by \p AI.
static bool mayWriteTo(AliasAnalysis *AA, SideEffectAnalysis *SEA,
                       WriteSet &MayWrites, ApplyInst *AI) {
  FunctionSideEffects E;
  SEA->getCalleeEffects(E, AI);
  assert(E.getMemBehavior(RetainObserveKind::IgnoreRetains) <=
         SILInstruction::MemoryBehavior::MayRead &&
         "apply should only read from memory");
  if (E.getGlobalEffects().mayRead() && !MayWrites.empty()) {
    // We don't know which memory is read in the callee. Therefore we bail if
    // there are any writes in the loop.
    return true;
  }

  for (unsigned Idx = 0, End = AI->getNumArguments(); Idx < End; ++Idx) {
    auto &ArgEffect = E.getParameterEffects()[Idx];
    assert(!ArgEffect.mayRelease() && "apply should only read from memory");
    if (!ArgEffect.mayRead())
      continue;

    SILValue Arg = AI->getArgument(Idx);

    // Check if the memory addressed by the argument may alias any writes.
    for (auto *W : MayWrites) {
      if (AA->mayWriteToMemory(W, Arg)) {
        DEBUG(llvm::dbgs() << "  mayWriteTo\n" << *W << " to " << *AI << "\n");
        return true;
      }
    }
  }
  return false;
}

static void removeWrittenTo(AliasAnalysis *AA, ReadSet &Reads,
                            SILInstruction *ByInst) {

  // We can ignore retains, cond_fails, and dealloc_stacks.
  if (isa<StrongRetainInst>(ByInst) || isa<RetainValueInst>(ByInst) ||
      isa<CondFailInst>(ByInst) || isa<DeallocStackInst>(ByInst))
    return;

  SmallVector<SILInstruction *, 8> RS(Reads.begin(), Reads.end());
  for (auto R : RS) {
    auto *LI = dyn_cast<LoadInst>(R);
    if (LI && !AA->mayWriteToMemory(ByInst, LI->getOperand()))
      continue;

    DEBUG(llvm::dbgs() << "  mayWriteTo\n" << *ByInst << " to " << *R << "\n");
    Reads.erase(R);
  }
}

static bool hasLoopInvariantOperands(SILInstruction *I, SILLoop *L) {
  auto Opds = I->getAllOperands();

  return std::all_of(Opds.begin(), Opds.end(), [=](Operand &Op) {

    ValueBase *Def = Op.get();

    // Operand is defined outside the loop.
    if (auto *Inst = Def->getDefiningInstruction())
      return !L->contains(Inst->getParent());
    if (auto *Arg = dyn_cast<SILArgument>(Def))
      return !L->contains(Arg->getParent());

    return false;
  });
}

/// Check if an address does not depend on other values in a basic block.
static SILInstruction *addressIndependent(SILValue Addr) {
  Addr = stripCasts(Addr);
  if (auto *SGAI = dyn_cast<GlobalAddrInst>(Addr))
    return SGAI;
  if (auto *SEAI = dyn_cast<StructElementAddrInst>(Addr))
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

/// Checks if \p Inst has no side effects which prevent hoisting.
/// The \a SafeReads set contain instructions which we already proved to have
/// no such side effects.
static bool hasNoSideEffect(SILInstruction *Inst, ReadSet &SafeReads) {
  // We can (and must) hoist cond_fail instructions if the operand is
  // invariant. We must hoist them so that we preserve memory safety. A
  // cond_fail that would have protected (executed before) a memory access
  // must - after hoisting - also be executed before said access.
  if (isa<CondFailInst>(Inst))
    return true;
  
  // Can't hoist if the instruction could read from memory and is not marked
  // as safe.
  if (SafeReads.count(Inst))
    return true;

  if (Inst->getMemoryBehavior() == SILInstruction::MemoryBehavior::None)
    return true;
  
  return false;
}

static bool canHoistInstruction(SILInstruction *Inst, SILLoop *Loop,
                                ReadSet &SafeReads) {
  // Can't hoist terminators.
  if (isa<TermInst>(Inst))
    return false;
  
  // Can't hoist allocation and dealloc stacks.
  if (isa<AllocationInst>(Inst) || isa<DeallocStackInst>(Inst))
    return false;

  // Can't hoist instructions which may have side effects.
  if (!hasNoSideEffect(Inst, SafeReads))
    return false;

  // The operands need to be loop invariant.
  if (!hasLoopInvariantOperands(Inst, Loop)) {
    DEBUG(llvm::dbgs() << "   loop variant operands\n");
    return false;
  }
  
  return true;
}

static bool hoistInstructions(SILLoop *Loop, DominanceInfo *DT,
                              ReadSet &SafeReads, bool RunsOnHighLevelSil) {
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
          return DT->dominates(CurBB, ExitBB);
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
      if (canHoistInstruction(Inst, Loop, SafeReads)) {
        DEBUG(llvm::dbgs() << "   hoisting to preheader.\n");
        Changed = true;
        Inst->moveBefore(Preheader->getTerminator());
      } else if (RunsOnHighLevelSil) {
        ArraySemanticsCall semCall(Inst);
        switch (semCall.getKind()) {
        case ArrayCallKind::kGetCount:
        case ArrayCallKind::kGetCapacity:
          if (hasLoopInvariantOperands(Inst, Loop) &&
              semCall.canHoist(Preheader->getTerminator(), DT)) {
            Changed = true;
            semCall.hoist(Preheader->getTerminator(), DT);
          }
          break;
        default:
          break;
        }
      }
    }

    // Next block in dominator tree.
    ++It;
  }
  return Changed;
}

static bool sinkFixLifetime(SILLoop *Loop, DominanceInfo *DomTree,
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
    if (DomTree->dominates(FLI->getOperand()->getParentBlock(),
                           Preheader)) {
      auto Succs = ExitingBB->getSuccessors();
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
/// \brief Summary of may writes occurring in the loop tree rooted at \p
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
  SideEffectAnalysis *SEA;
  DominanceInfo *DomTree;
  bool Changed;

  /// True if LICM is done on high-level SIL, i.e. semantic calls are not
  /// inlined yet. In this case some semantic calls can be hoisted.
  bool RunsOnHighLevelSil;

public:
  LoopTreeOptimization(SILLoop *TopLevelLoop, SILLoopInfo *LI,
                       AliasAnalysis *AA, SideEffectAnalysis *SEA,
                       DominanceInfo *DT,
                       bool RunsOnHighLevelSil)
      : LoopInfo(LI), AA(AA), SEA(SEA), DomTree(DT), Changed(false),
        RunsOnHighLevelSil(RunsOnHighLevelSil) {
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
} // end anonymous namespace

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

    // Analyze the current loop for reads that can be hoisted.
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
  DEBUG(llvm::dbgs() << " Analyzing accesses.\n");

  // Contains function calls in the loop, which only read from memory.
  SmallVector<ApplyInst *, 8> ReadOnlyApplies;

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
      if (auto *AI = dyn_cast<ApplyInst>(&Inst)) {
        // In contrast to load instructions, we first collect all read-only
        // function calls and add them later to SafeReads.
        FunctionSideEffects E;
        SEA->getCalleeEffects(E, AI);

        auto MB = E.getMemBehavior(RetainObserveKind::ObserveRetains);
        if (MB <= SILInstruction::MemoryBehavior::MayRead)
          ReadOnlyApplies.push_back(AI);
      }
      if (Inst.mayHaveSideEffects()) {
        MayWrites.push_back(&Inst);
        // Remove clobbered loads we have seen before.
        removeWrittenTo(AA, SafeReads, &Inst);
      }
    }
  }
  for (auto *AI : ReadOnlyApplies) {
    if (!mayWriteTo(AA, SEA, MayWrites, AI))
      SafeReads.insert(AI);
  }
}

void LoopTreeOptimization::optimizeLoop(SILLoop *CurrentLoop,
                                        ReadSet &SafeReads) {
  Changed |= sinkCondFail(CurrentLoop);
  Changed |= hoistInstructions(CurrentLoop, DomTree, SafeReads,
                               RunsOnHighLevelSil);
  Changed |= sinkFixLifetime(CurrentLoop, DomTree, LoopInfo);
}

namespace {
/// Hoist loop invariant code out of innermost loops.
///
/// Transforms are identified by type, not instance. Split this
/// Into two types: "High-level Loop Invariant Code Motion"
/// and "Loop Invariant Code Motion".
class LICM : public SILFunctionTransform {

public:
  LICM(bool RunsOnHighLevelSil) : RunsOnHighLevelSil(RunsOnHighLevelSil) {}

  /// True if LICM is done on high-level SIL, i.e. semantic calls are not
  /// inlined yet. In this case some semantic calls can be hoisted.
  /// We only hoist semantic calls on high-level SIL because we can be sure that
  /// e.g. an Array as SILValue is really immutable (including its content).
  bool RunsOnHighLevelSil;

  void run() override {
    SILFunction *F = getFunction();
    SILLoopAnalysis *LA = PM->getAnalysis<SILLoopAnalysis>();
    SILLoopInfo *LoopInfo = LA->get(F);

    if (LoopInfo->empty()) {
      DEBUG(llvm::dbgs() << "No loops in " << F->getName() << "\n");
      return;
    }

    DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
    AliasAnalysis *AA = PM->getAnalysis<AliasAnalysis>();
    SideEffectAnalysis *SEA = PM->getAnalysis<SideEffectAnalysis>();
    DominanceInfo *DomTree = nullptr;

    DEBUG(llvm::dbgs() << "Processing loops in " << F->getName() << "\n");
    bool Changed = false;

    for (auto *TopLevelLoop : *LoopInfo) {
      if (!DomTree) DomTree = DA->get(F);
      LoopTreeOptimization Opt(TopLevelLoop, LoopInfo, AA, SEA, DomTree,
                               RunsOnHighLevelSil);
      Changed |= Opt.optimize();
    }

    if (Changed) {
      LA->lockInvalidation();
      DA->lockInvalidation();
      PM->invalidateAnalysis(F, SILAnalysis::InvalidationKind::FunctionBody);
      LA->unlockInvalidation();
      DA->unlockInvalidation();
    }
  }
};
} // end anonymous namespace

SILTransform *swift::createLICM() {
  return new LICM(false);
}

SILTransform *swift::createHighLevelLICM() {
  return new LICM(true);
}
