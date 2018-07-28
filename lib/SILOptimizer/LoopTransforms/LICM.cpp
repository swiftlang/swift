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
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/Analysis/SideEffectAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"

#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/Debug.h"

#include "llvm/Support/CommandLine.h"

using namespace swift;

/// Instructions which can be hoisted:
/// loads, function calls without side effects and (some) exclusivity checks
using InstSet = llvm::SmallPtrSet<SILInstruction *, 8>;

/// A subset of instruction which may have side effects.
/// Doesn't contain ones that have special handling (e.g. fix_lifetime)
using WriteSet = SmallPtrSet<SILInstruction *, 8>;

/// Returns true if the \p MayWrites set contains any memory writes which may
/// alias with the memory addressed by \a LI.
template <SILInstructionKind K, typename T>
static bool mayWriteTo(AliasAnalysis *AA, WriteSet &MayWrites,
                       UnaryInstructionBase<K, T> *Inst) {
  for (auto *W : MayWrites)
    if (AA->mayWriteToMemory(W, Inst->getOperand())) {
      LLVM_DEBUG(llvm::dbgs() << "  mayWriteTo\n" << *W << " to "
                              << *Inst << "\n");
      return true;
    }
  return false;
}

/// Returns true if the \p MayWrites set contains any memory writes which may
/// alias with any memory which is read by \p AI.
/// Note: This function should only be called on a read-only apply!
static bool mayWriteTo(AliasAnalysis *AA, SideEffectAnalysis *SEA,
                       WriteSet &MayWrites, ApplyInst *AI) {
  FunctionSideEffects E;
  SEA->getCalleeEffects(E, AI);
  assert(E.getMemBehavior(RetainObserveKind::IgnoreRetains) <=
         SILInstruction::MemoryBehavior::MayRead &&
         "apply should only read from memory");
  assert(!E.getGlobalEffects().mayRead() &&
         "apply should not have global effects");

  for (unsigned Idx = 0, End = AI->getNumArguments(); Idx < End; ++Idx) {
    auto &ArgEffect = E.getParameterEffects()[Idx];
    assert(!ArgEffect.mayRelease() && "apply should only read from memory");
    if (!ArgEffect.mayRead())
      continue;

    SILValue Arg = AI->getArgument(Idx);

    // Check if the memory addressed by the argument may alias any writes.
    for (auto *W : MayWrites) {
      if (AA->mayWriteToMemory(W, Arg)) {
        LLVM_DEBUG(llvm::dbgs() << "  mayWriteTo\n" << *W << " to "
                                << *AI << "\n");
        return true;
      }
    }
  }
  return false;
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

// When Hoisting / Sinking,
// Don't descend into control-dependent code.
// Only traverse into basic blocks that dominate all exits.
static void getDominatingBlocks(SmallVectorImpl<SILBasicBlock *> &domBlocks,
                                SILLoop *Loop, DominanceInfo *DT) {
  auto HeaderBB = Loop->getHeader();
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
      LLVM_DEBUG(llvm::dbgs() << "  skipping conditional block "
                              << *CurBB << "\n");
      It.skipChildren();
      continue;
    }
    domBlocks.push_back(CurBB);
    // Next block in dominator tree.
    ++It;
  }
}

static bool hoistInstruction(DominanceInfo *DT, SILInstruction *Inst,
                             SILLoop *Loop, SILBasicBlock *&Preheader) {
  if (!hasLoopInvariantOperands(Inst, Loop)) {
    LLVM_DEBUG(llvm::dbgs() << "   loop variant operands\n");
    return false;
  }

  auto mvBefore = Preheader->getTerminator();
  ArraySemanticsCall semCall(Inst);
  if (semCall.canHoist(mvBefore, DT)) {
    semCall.hoist(mvBefore, DT);
  } else {
    Inst->moveBefore(mvBefore);
  }
  return true;
}

static bool hoistInstructions(SILLoop *Loop, DominanceInfo *DT,
                              InstSet &HoistUpSet) {
  LLVM_DEBUG(llvm::dbgs() << " Hoisting instructions.\n");
  auto Preheader = Loop->getLoopPreheader();
  assert(Preheader && "Expected a preheader");
  bool Changed = false;
  SmallVector<SILBasicBlock *, 8> domBlocks;
  getDominatingBlocks(domBlocks, Loop, DT);

  for (auto *CurBB : domBlocks) {
    // We know that the block is guaranteed to be executed. Hoist if we can.
    for (auto InstIt = CurBB->begin(), E = CurBB->end(); InstIt != E;) {
      SILInstruction *Inst = &*InstIt;
      ++InstIt;
      LLVM_DEBUG(llvm::dbgs() << "  looking at " << *Inst);
      if (!HoistUpSet.count(Inst)) {
        continue;
      }
      if (!hoistInstruction(DT, Inst, Loop, Preheader)) {
        continue;
      }
      LLVM_DEBUG(llvm::dbgs() << "Hoisted " << *Inst);
      Changed = true;
    }
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
    MayWrites.insert(Other.MayWrites.begin(), Other.MayWrites.end());
  }

  LoopNestSummary(const LoopNestSummary &) = delete;
  LoopNestSummary &operator=(const LoopNestSummary &) = delete;
  LoopNestSummary(LoopNestSummary &&) = delete;
};

static unsigned getEdgeIndex(SILBasicBlock *BB, SILBasicBlock *ExitingBB) {
  auto Succs = ExitingBB->getSuccessors();
  for (unsigned EdgeIdx = 0; EdgeIdx < Succs.size(); ++EdgeIdx) {
    SILBasicBlock *CurrBB = Succs[EdgeIdx];
    if (CurrBB == BB) {
      return EdgeIdx;
    }
  }
  llvm_unreachable("BB is not a Successor");
}

static bool sinkInstruction(DominanceInfo *DT,
                            std::unique_ptr<LoopNestSummary> &LoopSummary,
                            SILInstruction *Inst, SILLoopInfo *LI) {
  auto *Loop = LoopSummary->Loop;
  SmallVector<SILBasicBlock *, 8> ExitBBs;
  Loop->getExitBlocks(ExitBBs);
  SmallVector<SILBasicBlock *, 8> NewExitBBs;
  SmallVector<SILBasicBlock *, 8> ExitingBBs;
  Loop->getExitingBlocks(ExitingBBs);
  auto *ExitBB = Loop->getExitBlock();

  bool Changed = false;
  for (auto *ExitingBB : ExitingBBs) {
    SmallVector<SILBasicBlock *, 8> BBSuccessors;
    auto Succs = ExitingBB->getSuccessors();
    for (unsigned EdgeIdx = 0; EdgeIdx < Succs.size(); ++EdgeIdx) {
      SILBasicBlock *BB = Succs[EdgeIdx];
      BBSuccessors.push_back(BB);
    }
    while (!BBSuccessors.empty()) {
      SILBasicBlock *BB = BBSuccessors.pop_back_val();
      if (std::find(NewExitBBs.begin(), NewExitBBs.end(), BB) !=
          NewExitBBs.end()) {
        // Already got a copy there
        continue;
      }
      auto EdgeIdx = getEdgeIndex(BB, ExitingBB);
      SILBasicBlock *OutsideBB = nullptr;
      if (std::find(ExitBBs.begin(), ExitBBs.end(), BB) != ExitBBs.end()) {
        auto *SplitBB =
            splitCriticalEdge(ExitingBB->getTerminator(), EdgeIdx, DT, LI);
        OutsideBB = SplitBB ? SplitBB : BB;
        NewExitBBs.push_back(OutsideBB);
      }
      if (!OutsideBB) {
        continue;
      }
      // If OutsideBB already contains Inst -> skip
      // This might happen if we have a conditional control flow
      // And a pair
      // We hoisted the first part, we can safely ignore sinking
      auto matchPred = [&](SILInstruction &CurrIns) {
        return Inst->isIdenticalTo(&CurrIns);
      };
      if (std::find_if(OutsideBB->begin(), OutsideBB->end(), matchPred) !=
          OutsideBB->end()) {
        LLVM_DEBUG(llvm::errs() << "  instruction already at exit BB "
                                << *Inst);
        ExitBB = nullptr;
      } else if (ExitBB) {
        // easy case
        LLVM_DEBUG(llvm::errs() << "  moving instruction to exit BB " << *Inst);
        Inst->moveBefore(&*OutsideBB->begin());
      } else {
        LLVM_DEBUG(llvm::errs() << "  cloning instruction to exit BB "
                                << *Inst);
        Inst->clone(&*OutsideBB->begin());
      }
      Changed = true;
    }
  }
  if (Changed && !ExitBB) {
    // Created clones of instruction
    // Remove it from the may write set - dangling pointer
    LoopSummary->MayWrites.erase(Inst);
    Inst->getParent()->erase(Inst);
  }
  return Changed;
}

static bool sinkInstructions(std::unique_ptr<LoopNestSummary> &LoopSummary,
                             DominanceInfo *DT, SILLoopInfo *LI,
                             InstSet &SinkDownSet) {
  auto *Loop = LoopSummary->Loop;
  LLVM_DEBUG(llvm::errs() << " Sink instructions attempt\n");
  SmallVector<SILBasicBlock *, 8> domBlocks;
  getDominatingBlocks(domBlocks, Loop, DT);

  bool Changed = false;
  for (auto *Inst : SinkDownSet) {
    // only sink if the block is guaranteed to be executed.
    if (std::find(domBlocks.begin(), domBlocks.end(), Inst->getParent()) ==
        domBlocks.end()) {
      continue;
    }
    Changed |= sinkInstruction(DT, LoopSummary, Inst, LI);
  }

  return Changed;
}

static void getEndAccesses(BeginAccessInst *BI,
                           SmallVectorImpl<EndAccessInst *> &EndAccesses) {
  for (auto Use : BI->getUses()) {
    auto *User = Use->getUser();
    auto *EI = dyn_cast<EndAccessInst>(User);
    if (!EI) {
      continue;
    }
    EndAccesses.push_back(EI);
  }
}

static bool
hoistSpecialInstruction(std::unique_ptr<LoopNestSummary> &LoopSummary,
                        DominanceInfo *DT, SILLoopInfo *LI, InstSet &Special) {
  auto *Loop = LoopSummary->Loop;
  LLVM_DEBUG(llvm::errs() << " Hoist and Sink pairs attempt\n");
  auto Preheader = Loop->getLoopPreheader();
  assert(Preheader && "Expected a preheader");

  bool Changed = false;

  for (auto *Inst : Special) {
    auto *BI = dyn_cast<BeginAccessInst>(Inst);
    assert(BI && "Only BeginAccessInst are supported");
    SmallVector<EndAccessInst *, 2> Ends;
    getEndAccesses(BI, Ends);
    if (!hoistInstruction(DT, BI, Loop, Preheader)) {
      continue;
    }
    LLVM_DEBUG(llvm::dbgs() << "Hoisted " << *BI);
    for (auto *instSink : Ends) {
      if (!sinkInstruction(DT, LoopSummary, instSink, LI)) {
        llvm_unreachable("LICM: Could not perform must-sink instruction");
      }
    }
    LLVM_DEBUG(llvm::errs() << " Successfully hosited and sank pair\n");
    Changed = true;
  }

  return Changed;
}

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
  bool RunsOnHighLevelSIL;

  /// Instructions that we may be able to hoist up
  InstSet HoistUp;

  /// Instructions that we may be able to sink down
  InstSet SinkDown;

  /// Hoistable Instructions that need special treatment
  /// e.g. begin_access
  InstSet SpecialHoist;

public:
  LoopTreeOptimization(SILLoop *TopLevelLoop, SILLoopInfo *LI,
                       AliasAnalysis *AA, SideEffectAnalysis *SEA,
                       DominanceInfo *DT, bool RunsOnHighLevelSil)
      : LoopInfo(LI), AA(AA), SEA(SEA), DomTree(DT), Changed(false),
        RunsOnHighLevelSIL(RunsOnHighLevelSil) {
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

  /// \brief Collect a set of instructions that can be hoisted
  void analyzeCurrentLoop(std::unique_ptr<LoopNestSummary> &CurrSummary);

  /// \brief Optimize the current loop nest.
  bool optimizeLoop(std::unique_ptr<LoopNestSummary> &CurrSummary);
};
} // end anonymous namespace

bool LoopTreeOptimization::optimize() {
  // Process loops bottom up in the loop tree.
  while (!BotUpWorkList.empty()) {
    SILLoop *CurrentLoop = BotUpWorkList.pop_back_val();
    LLVM_DEBUG(llvm::dbgs() << "Processing loop " << *CurrentLoop);

    // Collect all summary of all sub loops of the current loop. Since we
    // process the loop tree bottom up they are guaranteed to be available in
    // the map.
    auto CurrLoopSummary = llvm::make_unique<LoopNestSummary>(CurrentLoop);
    propagateSummaries(CurrLoopSummary);

    // If the current loop changed, then we might reveal more instr to hoist
    // For example, a fix_lifetime's operand, if hoisted outside,
    // Might allow us to sink the instruction out of the loop
    bool currChanged = false;
    do {
      currChanged = false;

      // Analyze the current loop for instructions that can be hoisted.
      analyzeCurrentLoop(CurrLoopSummary);

      currChanged = optimizeLoop(CurrLoopSummary);

      // Reset the data structures for next loop in the list
      HoistUp.clear();
      SinkDown.clear();
      SpecialHoist.clear();
    } while (currChanged);

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

static bool isSafeReadOnlyApply(SideEffectAnalysis *SEA, ApplyInst *AI) {
  FunctionSideEffects E;
  SEA->getCalleeEffects(E, AI);

  if (E.getGlobalEffects().mayRead()) {
    // If we have Global effects,
    // we don't know which memory is read in the callee.
    // Therefore we bail for safety
    return false;
  }

  auto MB = E.getMemBehavior(RetainObserveKind::ObserveRetains);
  return (MB <= SILInstruction::MemoryBehavior::MayRead);
}

static void checkSideEffects(swift::SILInstruction &Inst, WriteSet &MayWrites) {
  if (Inst.mayHaveSideEffects()) {
    MayWrites.insert(&Inst);
  }
}

/// Returns true if the \p Inst follows the default hoisting heuristic
static bool canHoistUpDefault(SILInstruction *inst, SILLoop *Loop,
                              DominanceInfo *DT, bool RunsOnHighLevelSil) {
  auto Preheader = Loop->getLoopPreheader();
  if (!Preheader) {
    return false;
  }

  if (isa<TermInst>(inst) || isa<AllocationInst>(inst) ||
      isa<DeallocationInst>(inst)) {
    return false;
  }

  if (inst->getMemoryBehavior() == SILInstruction::MemoryBehavior::None) {
    return true;
  }

  if (!RunsOnHighLevelSil) {
    return false;
  }

  // We can’t hoist everything that is hoist-able
  // The canHoist method does not do all the required analysis
  // Some of the work is done at COW Array Opt
  // TODO: Refactor COW Array Opt + canHoist - radar 41601468
  ArraySemanticsCall semCall(inst);
  switch (semCall.getKind()) {
  case ArrayCallKind::kGetCount:
  case ArrayCallKind::kGetCapacity:
    return semCall.canHoist(Preheader->getTerminator(), DT);
  default:
    return false;
  }
}

// Check If all the end accesses of the given begin do not prevent hoisting
// There are only two legal placements for the end access instructions:
// 1) Inside the same loop (sink to loop exists)
// Potential TODO: At loop exit block
static bool handledEndAccesses(BeginAccessInst *BI, SILLoop *Loop) {
  SmallVector<EndAccessInst *, 2> AllEnds;
  getEndAccesses(BI, AllEnds);
  if (AllEnds.empty()) {
    return false;
  }
  for (auto *User : AllEnds) {
    auto *BB = User->getParent();
    if (Loop->getBlocksSet().count(BB) != 0) {
      continue;
    }
    return false;
  }
  return true;
}

static bool
analyzeBeginAccess(BeginAccessInst *BI,
                   SmallVector<BeginAccessInst *, 8> &BeginAccesses) {
  if (BI->getEnforcement() != SILAccessEnforcement::Dynamic) {
    return false;
  }

  const AccessedStorage &storage =
      findAccessedStorageNonNested(BI->getSource());
  if (!storage) {
    return false;
  }

  auto BIAccessedStorageNonNested = findAccessedStorageNonNested(BI);
  auto safeBeginPred = [&](BeginAccessInst *OtherBI) {
    if (BI == OtherBI) {
      return true;
    }
    return BIAccessedStorageNonNested.isDistinctFrom(
        findAccessedStorageNonNested(OtherBI));
  };

  return (
      std::all_of(BeginAccesses.begin(), BeginAccesses.end(), safeBeginPred));
}

// Analyzes current loop for hosting/sinking potential:
// Computes set of instructions we may be able to move out of the loop
// Important Note:
// We can't bail out of this method! we have to run it on all loops.
// We *need* to discover all MayWrites -
// even if the loop is otherwise skipped!
// This is because outer loops will depend on the inner loop's writes.
void LoopTreeOptimization::analyzeCurrentLoop(
    std::unique_ptr<LoopNestSummary> &CurrSummary) {
  WriteSet &MayWrites = CurrSummary->MayWrites;
  SILLoop *Loop = CurrSummary->Loop;
  LLVM_DEBUG(llvm::dbgs() << " Analyzing accesses.\n");

  // Contains function calls in the loop, which only read from memory.
  SmallVector<ApplyInst *, 8> ReadOnlyApplies;
  // Contains Loads inside the loop.
  SmallVector<LoadInst *, 8> Loads;
  // Contains fix_lifetime, we might be able to sink them.
  SmallVector<FixLifetimeInst *, 8> FixLifetimes;
  // Contains begin_access, we might be able to hoist them.
  SmallVector<BeginAccessInst *, 8> BeginAccesses;

  for (auto *BB : Loop->getBlocks()) {
    for (auto &Inst : *BB) {
      switch (Inst.getKind()) {
      case SILInstructionKind::FixLifetimeInst: {
        auto *FL = dyn_cast<FixLifetimeInst>(&Inst);
        assert(FL && "Expected a FixLifetime instruction");
        FixLifetimes.push_back(FL);
        // We can ignore the side effects of FixLifetimes
        break;
      }
      case SILInstructionKind::LoadInst: {
        auto *LI = dyn_cast<LoadInst>(&Inst);
        assert(LI && "Expected a Load instruction");
        Loads.push_back(LI);
        break;
      }
      case SILInstructionKind::BeginAccessInst: {
        auto *BI = dyn_cast<BeginAccessInst>(&Inst);
        assert(BI && "Expected a Begin Access");
        BeginAccesses.push_back(BI);
        checkSideEffects(Inst, MayWrites);
        break;
      }
      case swift::SILInstructionKind::CondFailInst: {
        // We can (and must) hoist cond_fail instructions if the operand is
        // invariant. We must hoist them so that we preserve memory safety. A
        // cond_fail that would have protected (executed before) a memory access
        // must - after hoisting - also be executed before said access.
        HoistUp.insert(&Inst);
        checkSideEffects(Inst, MayWrites);
        break;
      }
      case SILInstructionKind::ApplyInst: {
        auto *AI = dyn_cast<ApplyInst>(&Inst);
        assert(AI && "Expected an Apply Instruction");
        if (isSafeReadOnlyApply(SEA, AI)) {
          ReadOnlyApplies.push_back(AI);
        }
        // check for array semantics and side effects - same as default
        LLVM_FALLTHROUGH;
      }
      default: {
        checkSideEffects(Inst, MayWrites);
        if (canHoistUpDefault(&Inst, Loop, DomTree, RunsOnHighLevelSIL)) {
          HoistUp.insert(&Inst);
        }
        break;
      }
      }
    }
  }

  auto *Preheader = Loop->getLoopPreheader();
  if (!Preheader) {
    // Can't hoist/sink instructions
    return;
  }
  for (auto *AI : ReadOnlyApplies) {
    if (!mayWriteTo(AA, SEA, MayWrites, AI)) {
      HoistUp.insert(AI);
    }
  }
  for (auto *LI : Loads) {
    if (!mayWriteTo(AA, MayWrites, LI)) {
      HoistUp.insert(LI);
    }
  }
  bool mayWritesMayRelease =
      std::any_of(MayWrites.begin(), MayWrites.end(),
                  [&](SILInstruction *W) { return W->mayRelease(); });
  for (auto *FL : FixLifetimes) {
    if (!DomTree->dominates(FL->getOperand()->getParentBlock(), Preheader)) {
      continue;
    }
    if (!mayWriteTo(AA, MayWrites, FL) || !mayWritesMayRelease) {
      SinkDown.insert(FL);
    }
  }
  for (auto *BI : BeginAccesses) {
    if (!handledEndAccesses(BI, Loop)) {
      LLVM_DEBUG(llvm::dbgs() << "Skipping: " << *BI);
      LLVM_DEBUG(llvm::dbgs() << "Some end accesses can't be handled\n");
      continue;
    }
    if (analyzeBeginAccess(BI, BeginAccesses)) {
      SpecialHoist.insert(BI);
    }
  }
}

bool LoopTreeOptimization::optimizeLoop(
    std::unique_ptr<LoopNestSummary> &CurrSummary) {
  auto *CurrentLoop = CurrSummary->Loop;
  // We only support Loops with a preheader
  if (!CurrentLoop->getLoopPreheader())
    return false;
  bool currChanged = false;
  currChanged |= hoistInstructions(CurrentLoop, DomTree, HoistUp);
  currChanged |= sinkInstructions(CurrSummary, DomTree, LoopInfo, SinkDown);
  currChanged |=
      hoistSpecialInstruction(CurrSummary, DomTree, LoopInfo, SpecialHoist);
  Changed |= currChanged;
  return currChanged;
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
      LLVM_DEBUG(llvm::dbgs() << "No loops in " << F->getName() << "\n");
      return;
    }

    DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
    AliasAnalysis *AA = PM->getAnalysis<AliasAnalysis>();
    SideEffectAnalysis *SEA = PM->getAnalysis<SideEffectAnalysis>();
    DominanceInfo *DomTree = nullptr;

    LLVM_DEBUG(llvm::dbgs() << "Processing loops in " << F->getName() << "\n");
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
