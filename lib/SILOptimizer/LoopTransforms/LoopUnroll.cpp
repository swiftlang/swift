//===--- LoopUnroll.cpp - Loop unrolling ----------------------------------===//
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

#define DEBUG_TYPE "sil-loopunroll"


#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/IsSelfRecursiveAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/BasicBlockOptUtils.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "swift/SILOptimizer/Utils/OwnershipOptUtils.h"
#include "swift/SILOptimizer/Utils/PerformanceInlinerUtils.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"

using namespace swift;
using namespace swift::PatternMatch;

using llvm::MapVector;

/// Whether \p BB is cloned for each unrolled iteration: it is in the loop or it
/// is an exit prefix.
static bool isInUnrolledRegion(SILLoop *Loop,
                               ArrayRef<SILBasicBlock *> ExitPrefixes,
                               SILBasicBlock *BB) {
  return Loop->contains(BB) || llvm::is_contained(ExitPrefixes, BB);
}

namespace {

/// Clone the basic blocks in a loop, along with the exit block prefixes which
/// use the loop's stack allocations (see getExitPrefixEnd).
///
/// Currently invalidates the DomTree.
class LoopCloner : public SILCloner<LoopCloner> {
  SILLoop *Loop;
  ArrayRef<SILBasicBlock *> ExitPrefixes;

  friend class SILInstructionVisitor<LoopCloner>;
  friend class SILCloner<LoopCloner>;

public:
  LoopCloner(SILLoop *Loop, ArrayRef<SILBasicBlock *> ExitPrefixes)
      : SILCloner<LoopCloner>(*Loop->getHeader()->getParent()), Loop(Loop),
        ExitPrefixes(ExitPrefixes), mustCloneScopes(false),
        scopeCloner(*Loop->getHeader()->getParent()) {

    // If any debug info-carrying instructions use a @pack_element type that was
    // opened inside the loop, we must clone the debug scopes. Otherwise, two
    // instances of the same variable (with the same scope, location and name)
    // from different iterations of the loop could have different types, after
    // the @pack_element type is replaced with the appropriate concrete type for
    // each iteration. This could cause a verification error.
    for (SILBasicBlock *BB : Loop->getBlocks()) {
      for (auto &inst : *BB) {
        if (auto opei = dyn_cast<OpenPackElementInst>(&inst)) {
          for (SILInstruction *user : opei->getUsers()) {
            DebugVarCarryingInst debugVarCarryingInst(user);
            if (debugVarCarryingInst.getKind() !=
                DebugVarCarryingInst::Kind::Invalid) {
              mustCloneScopes = true;
            }
          }
        }
      }
    }
  }

  /// Clone the basic blocks in the loop.
  void cloneLoop();

  void sinkAddressProjections();

  // Update SSA helper.
  void collectLoopLiveOutValues(
      MapVector<SILValue, SmallVector<SILValue, 8>> &LoopLiveOutValues);

protected:
  // SILCloner CRTP override.
  SILValue getMappedValue(SILValue V) {
    if (auto *BB = V->getParentBlock()) {
      if (!isInRegion(BB))
        return V;
    }
    return SILCloner<LoopCloner>::getMappedValue(V);
  }
  // SILCloner CRTP override.
  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    SILCloner<LoopCloner>::postProcess(Orig, Cloned);
  }

private:
  bool mustCloneScopes;
  ScopeCloner scopeCloner;
  const SILDebugScope *remapScope(const SILDebugScope *DS) {
    if (mustCloneScopes)
      return scopeCloner.getOrCreateClonedScope(DS);
    return SILCloner<LoopCloner>::remapScope(DS);
  }

  bool isInRegion(SILBasicBlock *BB) const {
    return isInUnrolledRegion(Loop, ExitPrefixes, BB);
  }

  /// The blocks which are cloned.
  auto getRegionBlocks() const {
    return llvm::concat<SILBasicBlock *const>(Loop->getBlocks(), ExitPrefixes);
  }
};

} // end anonymous namespace

void LoopCloner::sinkAddressProjections() {
  SinkAddressProjections sinkProj;
  for (auto *bb : getRegionBlocks()) {
    for (auto &inst : *bb) {
      for (auto res : inst.getResults()) {
        if (!res->getType().isAddress()) {
          continue;
        }
        for (auto use : res->getUses()) {
          auto *user = use->getUser();
          if (isInRegion(user->getParent())) {
            continue;
          }
          bool canSink = sinkProj.analyzeAddressProjections(&inst);
          assert(canSink);
          sinkProj.cloneProjections();
        }
      }
    }
  }
}

void LoopCloner::cloneLoop() {
  SmallVector<SILBasicBlock *, 16> ExitBlocks;
  Loop->getExitBlocks(ExitBlocks);
  // Exit prefixes are cloned, so stop cloning at their successors instead.
  for (auto *&ExitBB : ExitBlocks) {
    if (llvm::is_contained(ExitPrefixes, ExitBB))
      ExitBB = ExitBB->getSingleSuccessorBlock();
  }

  sinkAddressProjections();
  // Clone the entire loop.
  cloneReachableBlocks(Loop->getHeader(), ExitBlocks,
                       /*insertAfter*/Loop->getLoopLatch());
}

namespace {

/// A loop exit which is taken after a known number of iterations.
struct CountedLoopExit {
  /// The block which conditionally branches out of the loop. It dominates the
  /// latch, so it executes on every iteration of the loop.
  SILBasicBlock *ExitingBlock;

  /// Whether ExitingBlock exits the loop through the true or false successor
  /// of its `cond_br`.
  bool ExitsOnTrue;

  /// The number of times ExitingBlock executes if no other exit is taken. The
  /// last time, it exits the loop. This is the number of copies of the loop
  /// body needed to fully unroll the loop.
  uint64_t TripCount;
};

} // end anonymous namespace

static std::optional<CountedLoopExit> getMaxLoopTripCountBoundedByExitingBlock(
    SILLoop *Loop, SILBasicBlock *Preheader, SILBasicBlock *Header,
    SILBasicBlock *Latch, SILBasicBlock *Exiting) {

  // Get the loop exit condition.
  auto *CondBr = dyn_cast<CondBranchInst>(Exiting->getTerminator());
  if (!CondBr)
    return std::nullopt;

  // Match an add 1 recurrence.

  auto *Cmp = dyn_cast<BuiltinInst>(CondBr->getCondition());
  if (!Cmp)
    return std::nullopt;

  unsigned Adjust = 0;
  SILBasicBlock *Exit = CondBr->getTrueBB();

  switch (Cmp->getBuiltinInfo().ID) {
    case BuiltinValueKind::ICMP_EQ:
    case BuiltinValueKind::ICMP_SGE:
      break;
    case BuiltinValueKind::ICMP_SGT:
      Adjust = 1;
      break;
    case BuiltinValueKind::ICMP_SLE:
      Exit = CondBr->getFalseBB();
      Adjust = 1;
      break;
    case BuiltinValueKind::ICMP_NE:
    case BuiltinValueKind::ICMP_SLT:
      Exit = CondBr->getFalseBB();
      break;
    default:
      return std::nullopt;
  }

  if (Loop->contains(Exit))
    return std::nullopt;

  auto *End = dyn_cast<IntegerLiteralInst>(Cmp->getArguments()[1]);
  if (!End)
    return std::nullopt;

  SILValue RecNext = Cmp->getArguments()[0];
  SILPhiArgument *RecArg;

  auto *RecNextArg = dyn_cast<SILPhiArgument>(RecNext);
  if (RecNextArg) {
    // The exit condition may compare the header argument itself, i.e. the
    // value before the increment, instead of the incremented value. The "add 1"
    // pattern may occur later in the loop, and be passed as a Phi value from
    // the latch to the header.
    if (RecNextArg->getParent() != Header)
      return std::nullopt;

    auto IncomingFromLatch = RecNextArg->getIncomingPhiValue(Latch);
    if (!IncomingFromLatch)
      return std::nullopt;
    RecNext = IncomingFromLatch;
  }

  // Match signed add with overflow, unsigned add with overflow and
  // add without overflow.
  if (!match(RecNext, m_TupleExtractOperation(
                          m_ApplyInst(BuiltinValueKind::SAddOver,
                                      m_SILPhiArgument(RecArg), m_One()),
                          0)) &&
      !match(RecNext, m_TupleExtractOperation(
                          m_ApplyInst(BuiltinValueKind::UAddOver,
                                      m_SILPhiArgument(RecArg), m_One()),
                          0)) &&
      !match(RecNext, m_ApplyInst(BuiltinValueKind::Add,
                                      m_SILPhiArgument(RecArg), m_One()))) {
    return std::nullopt;
  }

  if (RecArg->getParent() != Header)
    return std::nullopt;

  auto *Start = dyn_cast_or_null<IntegerLiteralInst>(
      RecArg->getIncomingPhiValue(Preheader));
  if (!Start)
    return std::nullopt;

  if (RecNext != RecArg->getIncomingPhiValue(Latch))
    return std::nullopt;

  auto StartVal = Start->getValue();
  auto EndVal = End->getValue();
  if (StartVal.sgt(EndVal))
    return std::nullopt;

  auto Dist = EndVal - StartVal;
  if (Dist.getBitWidth() > 64)
    return std::nullopt;

  if (Dist == 0)
    return std::nullopt;

  uint64_t TripCount = Dist.getZExtValue() + Adjust;
  // Comparing the value before the increment takes one more iteration to reach
  // the exit value.
  if (RecNextArg)
    ++TripCount;

  return CountedLoopExit{Exiting, Exit == CondBr->getTrueBB(), TripCount};
}

/// Determine the number of iterations the loop is at most executed. The loop
/// might contain early exits so this is the maximum if no early exits are
/// taken.
static std::optional<CountedLoopExit>
getMaxLoopTripCount(SILLoop *Loop, SILBasicBlock *Preheader,
                    SILBasicBlock *Header, SILBasicBlock *Latch,
                    DominanceInfo *DT) {
  SmallVector<swift::SILBasicBlock *, 2> ExitingBlocks;
  Loop->getExitingBlocks(ExitingBlocks);

  for (SILBasicBlock *Exiting : ExitingBlocks) {
    // An exit only bounds the trip count if its condition is checked on every
    // iteration.
    if (!DT->dominates(Exiting, Latch))
      continue;

    auto CountedExit = getMaxLoopTripCountBoundedByExitingBlock(
        Loop, Preheader, Header, Latch, Exiting);
    if (CountedExit.has_value()) {
      return CountedExit;
    }
  }

  return std::nullopt;
}

/// A loop that iterates over the elements of a variadic generic pack uses its
/// induction variable (a header block argument) as the index operand of a
/// `dynamic_pack_index`.
static bool isPackIterationLoop(SILLoop *Loop) {
  for (auto *arg : Loop->getHeader()->getArguments()) {
    for (auto *use : arg->getUses()) {
      if (isa<DynamicPackIndexInst>(use->getUser()))
        return true;
    }
  }
  return false;
}

/// An exit block may use stack allocations from the loop, for example to
/// deallocate them when the loop exits early. Each unrolled iteration needs its
/// own copy of these uses, so the exit block is split after the last of them,
/// and this prefix of the exit block is cloned along with the loop.
///
/// Returns the last instruction of the exit prefix of \p ExitBB, or nullptr if
/// \p ExitBB does not use the loop's stack allocations or cannot be split.
static SILInstruction *getExitPrefixEnd(SILLoop *Loop, SILBasicBlock *ExitBB) {
  // The prefix is cloned for each copy of the single exiting block.
  if (!ExitBB->getSinglePredecessorBlock())
    return nullptr;

  SILInstruction *PrefixEnd = nullptr;
  for (auto &Inst : *ExitBB) {
    for (SILValue Op : Inst.getOperandValues()) {
      auto *Def = Op->getDefiningInstruction();
      if (Def && Def->isAllocatingStack() && Loop->contains(Def))
        PrefixEnd = &Inst;
    }
  }
  if (isa_and_nonnull<TermInst>(PrefixEnd))
    return nullptr;
  return PrefixEnd;
}

/// Check whether we can duplicate the instructions in the loop and use a
/// heuristic that looks at the trip count and the cost of the instructions in
/// the loop to determine whether we should unroll this loop.
///
/// The exit prefixes ending at \p ExitPrefixEnds are duplicated along with the
/// loop.
static bool canAndShouldUnrollLoop(SILLoop *Loop, uint64_t TripCount,
                                   ArrayRef<SILInstruction *> ExitPrefixEnds,
                                   IsSelfRecursiveAnalysis *SRA,
                                   DeadEndBlocks *deb) {
  assert(Loop->getSubLoops().empty() && "Expect innermost loops");
  if (TripCount > 32)
    return false;

  // The instructions which are duplicated for each iteration.
  SmallVector<SILInstruction *, 64> RegionInsts;
  for (auto *BB : Loop->getBlocks()) {
    for (auto &Inst : *BB)
      RegionInsts.push_back(&Inst);
  }
  SmallVector<SILInstruction *, 8> ExitPrefixInsts;
  for (auto *PrefixEnd : ExitPrefixEnds) {
    for (auto &Inst : *PrefixEnd->getParent()) {
      ExitPrefixInsts.push_back(&Inst);
      if (&Inst == PrefixEnd)
        break;
    }
  }
  RegionInsts.append(ExitPrefixInsts.begin(), ExitPrefixInsts.end());
  auto isInRegion = [&](SILInstruction *Inst) {
    return Loop->contains(Inst) || llvm::is_contained(ExitPrefixInsts, Inst);
  };

  // We can unroll a loop if we can duplicate the instructions it holds.
  uint64_t Cost = 0;
  // Average number of instructions per basic block.
  // It is used to estimate the cost of the callee
  // inside a loop.
  const uint64_t InsnsPerBB = 4;
  // Use command-line threshold for unrolling.
  const uint64_t SILLoopUnrollThreshold = Loop->getBlocks().empty() ? 0 : 
    (Loop->getBlocks())[0]->getParent()->getModule().getOptions().UnrollThreshold;

  // Pack loops must be unrolled to specialize the body. This is critical for
  // performance, they should always be unrolled if possible.
  const bool isPackLoop = isPackIterationLoop(Loop);
  for (auto *Inst : RegionInsts) {
    if (!canDuplicateRegionInstruction(Inst, deb, isInRegion))
      return false;
    if (!isPackLoop && instructionInlineCost(*Inst) != InlineCost::Free)
      ++Cost;
    if (auto AI = FullApplySite::isa(Inst)) {
      auto Callee = AI.getCalleeFunction();
      // If the callee is unknown, it can be
      // devirtualized/specialized/always inlined later on which can lead to
      // code bloat, bailout. Pack-iteration loops are the exception: they
      // must be unrolled to devirtualize the witness methods
      // called on their pack elements, so don't bail out on their unknown
      // callees.
      if (!Callee && !isPackLoop) {
        return false;
      }
      if (!isPackLoop && Callee &&
          getEligibleFunction(AI, InlineSelection::Everything, SRA)) {
        // If callee is rather big and potentially inlinable, it may be better
        // not to unroll, so that the body of the callee can be inlined later.
        Cost += Callee->size() * InsnsPerBB;
      }
    }
    if (Cost * TripCount > SILLoopUnrollThreshold)
      return false;
  }
  return true;
}

/// Redirect the backedge of the current loop iteration's latch to the next
/// iteration's header.
static void redirectLatchToNextHeader(SILBasicBlock *Latch,
                                      SILBasicBlock *CurrentHeader,
                                      SILBasicBlock *NextIterationsHeader) {

  auto *CurrentTerminator = Latch->getTerminator();

  // We can either have a split backedge as our latch terminator.
  //   BackedgeBlock:
  //     br HeaderBlock
  //
  // Or a conditional branch back to the header.
  //   LatchBlock:
  //     ...
  //     cond_br %cond, ExitBlock, HeaderBlock
  //
  // Redirect the HeaderBlock target to the unrolled successor.

  // Handle the split backedge case.
  if (auto *Br = dyn_cast<BranchInst>(CurrentTerminator)) {
    SILBuilderWithScope(Br).createBranch(Br->getLoc(), NextIterationsHeader,
                                         Br->getArgs());
    Br->eraseFromParent();
    return;
  }

  // Otherwise, we have a conditional branch to the header.
  auto *CondBr = cast<CondBranchInst>(CurrentTerminator);
  if (CondBr->getTrueBB() == CurrentHeader) {
    SILBuilderWithScope(CondBr).createCondBranch(
        CondBr->getLoc(), CondBr->getCondition(), NextIterationsHeader,
        CondBr->getFalseBB());
  } else {
    assert(CondBr->getFalseBB() == CurrentHeader);
    SILBuilderWithScope(CondBr).createCondBranch(
        CondBr->getLoc(), CondBr->getCondition(), CondBr->getTrueBB(),
        NextIterationsHeader);
  }
  CondBr->eraseFromParent();
}

/// On the last iteration, the exit which bounds the trip count is always taken.
/// Replace its conditional branch with an unconditional branch out of the loop.
/// Because the exiting block dominates the latch, this makes the last
/// iteration's backedge unreachable.
static void foldLastIterationExit(SILBasicBlock *Exiting, bool ExitsOnTrue) {
  auto *CondBr = cast<CondBranchInst>(Exiting->getTerminator());
  // Cloning splits the edges from exiting blocks to exit blocks, so this is not
  // necessarily the original exit block.
  SILBasicBlock *Exit =
      ExitsOnTrue ? CondBr->getTrueBB() : CondBr->getFalseBB();
  SILBuilderWithScope(CondBr).createBranch(CondBr->getLoc(), Exit);
  CondBr->eraseFromParent();
}

/// Collect all the loop live out values in the map that maps original live out
/// value to live out value in the cloned loop.
void LoopCloner::collectLoopLiveOutValues(
    MapVector<SILValue, SmallVector<SILValue, 8>> &LoopLiveOutValues) {
  for (auto *Block : getRegionBlocks()) {
    // Look at block arguments.
    for (auto *Arg : Block->getArguments()) {
      for (auto *Op : Arg->getUses()) {
        // Is this use outside the cloned region?
        if (!isInRegion(Op->getParentBlock())) {
          auto ArgumentValue = SILValue(Arg);
          if (!LoopLiveOutValues.count(ArgumentValue))
            LoopLiveOutValues[ArgumentValue].push_back(
                getMappedValue(ArgumentValue));
        }
      }
    }
    // And the instructions.
    for (auto &Inst : *Block) {
      for (SILValue result : Inst.getResults()) {
        for (auto *Op : result->getUses()) {
          // Ignore uses inside the cloned region.
          if (isInRegion(Op->getParentBlock()))
            continue;

          auto UsedValue = Op->get();
          assert(UsedValue == result && "Instructions must match");

          if (!LoopLiveOutValues.count(UsedValue))
            LoopLiveOutValues[UsedValue].push_back(getMappedValue(result));
        }
      }
    }
  }
}

static void
updateSSA(SILFunction *Fn, SILLoop *Loop,
          ArrayRef<SILBasicBlock *> ExitPrefixes,
          MapVector<SILValue, SmallVector<SILValue, 8>> &LoopLiveOutValues) {
  SILSSAUpdater SSAUp;
  for (auto &MapEntry : LoopLiveOutValues) {
    // Collect the uses of this value outside the cloned region.
    auto OrigValue = MapEntry.first;
    SmallVector<UseWrapper, 16> UseList;
    for (auto Use : OrigValue->getUses())
      if (!isInUnrolledRegion(Loop, ExitPrefixes, Use->getParentBlock()))
        UseList.push_back(UseWrapper(Use));
    // Update SSA of use with the available values.
    SSAUp.initialize(Fn, OrigValue->getType(), OrigValue->getOwnershipKind());
    SSAUp.addAvailableValue(OrigValue->getParentBlock(), OrigValue);
    for (auto NewValue : MapEntry.second)
      SSAUp.addAvailableValue(NewValue->getParentBlock(), NewValue);
    for (auto U : UseList) {
      Operand *Use = U;
      SSAUp.rewriteUse(*Use);
    }
  }
}

/// Try to fully unroll the loop if we can determine the trip count and the trip
/// count is below a threshold.
static bool tryToUnrollLoop(SILLoop *Loop, IsSelfRecursiveAnalysis *SRA,
                            DeadEndBlocks *deb, DominanceInfo *DT) {
  assert(Loop->getSubLoops().empty() && "Expecting innermost loops");

  LLVM_DEBUG(llvm::dbgs() << "Trying to unroll loop : \n" << *Loop);
  auto *Preheader = Loop->getLoopPreheader();
  if (!Preheader)
    return false;

  auto *Latch = Loop->getLoopLatch();
  if (!Latch)
    return false;

  auto *Header = Loop->getHeader();

  std::optional<CountedLoopExit> CountedExit =
      getMaxLoopTripCount(Loop, Preheader, Header, Latch, DT);
  if (!CountedExit) {
    LLVM_DEBUG(llvm::dbgs() << "Not unrolling, did not find trip count\n");
    return false;
  }
  uint64_t MaxTripCount = CountedExit->TripCount;

  SmallVector<SILInstruction *, 4> ExitPrefixEnds;
  SmallVector<SILBasicBlock *, 8> ExitBlocks;
  Loop->getExitBlocks(ExitBlocks);
  for (auto *ExitBB : ExitBlocks) {
    if (auto *PrefixEnd = getExitPrefixEnd(Loop, ExitBB))
      ExitPrefixEnds.push_back(PrefixEnd);
  }

  if (!canAndShouldUnrollLoop(Loop, MaxTripCount, ExitPrefixEnds, SRA, deb)) {
    LLVM_DEBUG(llvm::dbgs() << "Not unrolling, exceeds cost threshold\n");
    return false;
  }

  // TODO: We need to split edges from non-condbr exits for the SSA updater. For
  // now just don't handle loops containing such exits.
  SmallVector<SILBasicBlock *, 16> ExitingBlocks;
  Loop->getExitingBlocks(ExitingBlocks);
  for (auto &Exit : ExitingBlocks)
    if (!isa<CondBranchInst>(Exit->getTerminator()))
      return false;

  LLVM_DEBUG(llvm::dbgs() << "Unrolling loop in "
                          << Header->getParent()->getName()
                          << " " << *Loop << "\n");

  // Split each exit prefix into its own block, which is cloned for each
  // iteration.
  SmallVector<SILBasicBlock *, 4> ExitPrefixes;
  for (auto *PrefixEnd : ExitPrefixEnds) {
    auto *SplitBefore = &*std::next(PrefixEnd->getIterator());
    SILBuilderWithScope Builder(SplitBefore);
    splitBasicBlockAndBranch(Builder, SplitBefore, /*domInfo*/ nullptr,
                             /*loopInfo*/ nullptr);
    ExitPrefixes.push_back(PrefixEnd->getParent());
  }

  SmallVector<SILBasicBlock *, 16> Headers;
  Headers.push_back(Header);

  SmallVector<SILBasicBlock *, 16> Latches;
  Latches.push_back(Latch);

  MapVector<SILValue, SmallVector<SILValue, 8>> LoopLiveOutValues;

  // The counted exiting block of the last iteration.
  SILBasicBlock *LastExiting = CountedExit->ExitingBlock;

  // Copy the body MaxTripCount-1 times.
  for (uint64_t Cnt = 1; Cnt < MaxTripCount; ++Cnt) {
    // Clone the blocks in the loop.
    LoopCloner cloner(Loop, ExitPrefixes);
    cloner.cloneLoop();
    Headers.push_back(cloner.getOpBasicBlock(Header));
    Latches.push_back(cloner.getOpBasicBlock(Latch));
    LastExiting = cloner.getOpBasicBlock(CountedExit->ExitingBlock);

    // Collect values defined in the loop but used outside. On the first
    // iteration we populate the map from original loop to cloned loop. On
    // subsequent iterations we only need to update this map with the values
    // from the new iteration's clone.
    if (Cnt == 1)
      cloner.collectLoopLiveOutValues(LoopLiveOutValues);
    else {
      for (auto &MapEntry : LoopLiveOutValues) {
        // Look it up in the value map.
        SILValue MappedValue = cloner.getOpValue(MapEntry.first);
        MapEntry.second.push_back(MappedValue);
        assert(MapEntry.second.size() == Cnt);
      }
    }
  }

  // Thread the loop clones by redirecting the loop latches to the successor
  // iteration's header.
  for (unsigned Iteration = 0, LastIteration = Latches.size() - 1;
       Iteration != LastIteration; ++Iteration) {
    redirectLatchToNextHeader(Latches[Iteration], Headers[Iteration],
                              Headers[Iteration + 1]);
  }

  // The last iteration always takes the counted exit.
  foldLastIterationExit(LastExiting, CountedExit->ExitsOnTrue);

  // Fixup SSA form for loop values used outside the loop.
  updateSSA(Loop->getFunction(), Loop, ExitPrefixes, LoopLiveOutValues);
  return true;
}

// =============================================================================
//                                 Driver
// =============================================================================

namespace {

class LoopUnrolling : public SILFunctionTransform {

  void run() override {
    bool Changed = false;
    auto *Fun = getFunction();
    SILLoopInfo *LoopInfo = PM->getAnalysis<SILLoopAnalysis>()->get(Fun);
    IsSelfRecursiveAnalysis *SRA = PM->getAnalysis<IsSelfRecursiveAnalysis>();
    DeadEndBlocks *deb = PM->getAnalysis<DeadEndBlocksAnalysis>()->get(Fun);
    // Unrolling one innermost loop does not change the dominance relation
    // between blocks of the other innermost loops, so this remains valid for
    // them.
    DominanceInfo *DT = PM->getAnalysis<DominanceAnalysis>()->get(Fun);

    LLVM_DEBUG(llvm::dbgs() << "Loop Unroll running on function : "
                            << Fun->getName() << "\n");

    // Collect innermost loops.
    SmallVector<SILLoop *, 16> InnermostLoops;

    for (auto *Loop : *LoopInfo) {
      SmallVector<SILLoop *, 8> Worklist;
      Worklist.push_back(Loop);

      for (unsigned i = 0; i < Worklist.size(); ++i) {
        auto *L = Worklist[i];
        for (auto *SubLoop : *L)
          Worklist.push_back(SubLoop);
        if (L->getSubLoops().empty())
          InnermostLoops.push_back(L);
      }
    }

    if (InnermostLoops.empty()) {
      LLVM_DEBUG(llvm::dbgs() << "No innermost loops\n");
      return;
    }

    // Try to unroll innermost loops.
    for (auto *Loop : InnermostLoops)
      Changed |= tryToUnrollLoop(Loop, SRA, deb, DT);

    if (Changed) {
      updateAllGuaranteedPhis(PM, Fun);
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
      removeUnreachableBlocks(*Fun);
      if (Fun->needBreakInfiniteLoops())
        breakInfiniteLoops(getPassManager(), Fun);
      if (Fun->needCompleteLifetimes())
        completeAllLifetimes(getPassManager(), Fun);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createLoopUnroll() {
  return new LoopUnrolling();
}
