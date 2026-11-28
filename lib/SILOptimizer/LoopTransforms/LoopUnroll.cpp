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

#include "swift/SIL/AddressWalker.h"
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

namespace {

/// A loop, and its "cleanup prefixes": the prefixes of its exit blocks that use
/// its stack allocations.
///
/// Before unrolling, each exit block that has one is split at the point after
/// its cleanup prefix, forming a (cleanup) prefix and a suffix block. When
/// unrolling, the cleanup prefix blocks are cloned with the loop body, so they
/// can refer to the appropriate copies of the loop's stack allocations. This
/// allows us to unroll a wider range of loops, including ones with early exits.
///
/// Only uses of allocations with an address are considered when computing the
/// cleanup prefix (e.g. alloc_stack, but not alloc_ref [stack]).
///
/// A loop can only be unrolled if each use of a stack allocation created inside
/// the loop is in either the loop or one of its cleanup prefixes.
class UnrollRegion {
  SILLoop *Loop;

  /// Maps each exit block that has a cleanup prefix to the last instruction in
  /// the prefix.
  llvm::MapVector<SILBasicBlock *, SILInstruction *> CleanupPrefixEnds;

  /// Whether each cleanup prefix has been split into its own block.
  bool PrefixesSplit = false;

  /// Records the transitive users of a stack allocation's address which are in
  /// an exit block of the loop, keeping the last one of each block.
  ///
  /// It would not be sufficient to visit a the address's stack-deallocating
  /// users, because the address may not be deallocated in a dead end.
  class PrefixEndCollector
      : public TransitiveAddressWalker<PrefixEndCollector> {
    const UnrollRegion &Region;
    const llvm::SmallPtrSetImpl<SILBasicBlock *> &ExitBlocks;
    llvm::MapVector<SILBasicBlock *, SILInstruction *> &PrefixEnds;

  public:
    PrefixEndCollector(const UnrollRegion &Region,
                       const llvm::SmallPtrSetImpl<SILBasicBlock *> &ExitBlocks,
                       llvm::MapVector<SILBasicBlock *, SILInstruction *> &Ends)
        : Region(Region), ExitBlocks(ExitBlocks), PrefixEnds(Ends) {}

    bool visitUse(Operand *use) {
      auto *user = use->getUser();
      auto *BB = user->getParent();
      // The prefix is cloned for each copy of the single exiting block. It
      // can't end in a terminator, which can't be split off from the block. A
      // use in a terminator, or in an exit block that doesn't have a prefix, is
      // outside the region, so the loop can't be unrolled.
      if (Region.Loop->contains(BB) || !ExitBlocks.contains(BB) ||
          isa<TermInst>(user)) {
        return true;
      }
      // Track the latest transitive user of one of the loop's stack allocations
      // found so far.
      auto [entry, inserted] = PrefixEnds.try_emplace(BB, user);
      if (!inserted && entry->second->strictlyDominatesInBlock(user))
        entry->second = user;
      return true;
    }

    /// Also visit users of projections, which are otherwise only followed.
    TransitiveUseVisitation visitTransitiveUseAsEndPointUse(Operand *use) {
      return TransitiveUseVisitation::BothUserAndUses;
    }
  };

  /// The block at \p Idx among the loop's blocks followed by the prefixes'.
  SILBasicBlock *getBlockAt(size_t Idx) const {
    auto LoopBlocks = Loop->getBlocks();
    if (Idx < LoopBlocks.size())
      return LoopBlocks[Idx];
    return (CleanupPrefixEnds.begin() + (Idx - LoopBlocks.size()))->first;
  }

  size_t getNumBlocks() const {
    return Loop->getBlocks().size() + CleanupPrefixEnds.size();
  }

  /// The end of the part of the block at \p Idx in the region.
  SILBasicBlock::iterator getEndOfBlockAt(size_t Idx) const {
    auto LoopBlocks = Loop->getBlocks();
    if (Idx < LoopBlocks.size())
      return LoopBlocks[Idx]->end();
    auto *PrefixEnd =
        (CleanupPrefixEnds.begin() + (Idx - LoopBlocks.size()))->second;
    return std::next(PrefixEnd->getIterator());
  }

public:
  /// Iterates over the instructions in the loop, then in its cleanup prefixes.
  class InstructionIterator {
    const UnrollRegion *Region;
    size_t NumBlocks;
    size_t BlockIdx;
    /// The current instruction, and the end of its part of the current block.
    SILBasicBlock::iterator Inst, BlockEnd;

    /// Start iterating over the block at BlockIdx, if there is one.
    void enterBlock() {
      if (BlockIdx == NumBlocks)
        return;
      Inst = Region->getBlockAt(BlockIdx)->begin();
      BlockEnd = Region->getEndOfBlockAt(BlockIdx);
    }

  public:
    InstructionIterator(const UnrollRegion *Region, size_t BlockIdx)
        : Region(Region), NumBlocks(Region->getNumBlocks()),
          BlockIdx(BlockIdx) {
      enterBlock();
    }

    SILInstruction *operator*() const { return &*Inst; }

    InstructionIterator &operator++() {
      if (++Inst == BlockEnd) {
        ++BlockIdx;
        enterBlock();
      }
      return *this;
    }

    bool operator==(const InstructionIterator &Other) const {
      return BlockIdx == Other.BlockIdx &&
             (BlockIdx == NumBlocks || Inst == Other.Inst);
    }
    bool operator!=(const InstructionIterator &Other) const {
      return !(*this == Other);
    }
  };

  explicit UnrollRegion(SILLoop *Loop) : Loop(Loop) {
    SmallVector<SILBasicBlock *, 8> ExitBlockList;
    Loop->getExitBlocks(ExitBlockList);
    llvm::SmallPtrSet<SILBasicBlock *, 8> ExitBlocks(ExitBlockList.begin(),
                                                     ExitBlockList.end());
    for (auto *BB : Loop->getBlocks()) {
      for (auto &Inst : *BB) {
        if (!Inst.isAllocatingStack())
          continue;
        for (auto result : Inst.getResults()) {
          if (result->getType().isAddress()) {
            PrefixEndCollector(*this, ExitBlocks, CleanupPrefixEnds)
                .walk(result);
          }
        }
      }
    }
  }

  SILLoop *getLoop() const { return Loop; }

  /// The instructions in the loop and the cleanup prefixes.
  llvm::iterator_range<InstructionIterator> instructions() const {
    return {InstructionIterator(this, 0),
            InstructionIterator(this, getNumBlocks())};
  }

  /// Whether \p Inst is in the loop or one of its cleanup prefixes. This works
  /// before the prefixes are split into their own blocks.
  bool contains(SILInstruction *Inst) const {
    ASSERT(
        !PrefixesSplit &&
        "Prefixes should only be found by instruction before they are split");
    if (Loop->contains(Inst))
      return true;
    auto Prefix = CleanupPrefixEnds.find(Inst->getParent());
    return Prefix != CleanupPrefixEnds.end() &&
           (Inst == Prefix->second ||
            Inst->strictlyDominatesInBlock(Prefix->second));
  }

  /// Whether \p BB is in the loop or is a cleanup prefix. The prefixes must
  /// have been split into their own blocks.
  bool contains(SILBasicBlock *BB) const {
    ASSERT(PrefixesSplit && "Prefixes must be split");
    return Loop->contains(BB) || CleanupPrefixEnds.contains(BB);
  }

  /// Check whether we can duplicate the instruction.
  bool canDuplicate(SILInstruction *Inst, DeadEndBlocks *deb) const {
    return canDuplicateRegionInstruction(
        Inst, deb, [&](SILInstruction *I) { return contains(I); });
  }

  /// The blocks in the loop and the cleanup prefixes. The prefixes must have
  /// been split into their own blocks.
  auto getBlocks() const {
    ASSERT(PrefixesSplit && "Prefixes must be split");
    return llvm::concat<SILBasicBlock *const>(Loop->getBlocks(),
                                              CleanupPrefixEnds.keys());
  }

  /// The blocks outside the region which it exits to. The prefixes must have
  /// been split into their own blocks.
  void getExitBlocks(SmallVectorImpl<SILBasicBlock *> &ExitBlocks) const {
    ASSERT(PrefixesSplit && "Prefixes must be split");
    Loop->getExitBlocks(ExitBlocks);
    // Cleanup prefixes are cloned, so stop cloning at their successors instead.
    for (auto *&ExitBB : ExitBlocks) {
      if (CleanupPrefixEnds.contains(ExitBB))
        ExitBB = ExitBB->getSingleSuccessorBlock();
    }
  }

  /// Split each cleanup prefix into its own block, so that the code to clone
  /// consists of whole blocks.
  ///
  /// The analyses are updated for the new blocks, which are outside the loop
  /// and so remain valid when other loops are unrolled afterwards.
  void splitCleanupPrefixes(DeadEndBlocks *deb, DominanceInfo *DT,
                            SILLoopInfo *LoopInfo) {
    ASSERT(!PrefixesSplit && "Prefixes can only be split once");
    for (auto [PrefixBB, PrefixEnd] : CleanupPrefixEnds) {
      auto *SplitBefore = PrefixEnd->getNextInstruction();
      SILBuilderWithScope Builder(SplitBefore);
      // The new block has the original terminator, so it is a dead end if and
      // only if the prefix block was. Query this before splitting, because the
      // first query computes the analysis.
      bool IsDeadEnd = deb->isDeadEnd(PrefixBB);
      auto *NewBB =
          splitBasicBlockAndBranch(Builder, SplitBefore, DT, LoopInfo);
      if (!IsDeadEnd)
        deb->updateForNewBlock(NewBB);
    }
    PrefixesSplit = true;
  }
};

} // end anonymous namespace

namespace {

/// Clone the basic blocks in an UnrollRegion.
///
/// Currently invalidates the DomTree.
class LoopCloner : public SILCloner<LoopCloner> {
  const UnrollRegion &Region;

  friend class SILInstructionVisitor<LoopCloner>;
  friend class SILCloner<LoopCloner>;

public:
  LoopCloner(const UnrollRegion &Region)
      : SILCloner<LoopCloner>(*Region.getLoop()->getHeader()->getParent()),
        Region(Region), mustCloneScopes(false),
        scopeCloner(*Region.getLoop()->getHeader()->getParent()) {

    // If any debug info-carrying instructions use a @pack_element type that was
    // opened inside the region, we must clone the debug scopes. Otherwise, two
    // instances of the same variable (with the same scope, location and name)
    // from different iterations of the loop could have different types, after
    // the @pack_element type is replaced with the appropriate concrete type for
    // each iteration. This could cause a verification error.
    for (SILBasicBlock *BB : Region.getBlocks()) {
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
    // Values defined outside the cloned region are used directly. The
    // exception is the arguments of the blocks the cloner creates when it
    // splits exit edges, which are mapped to the cloned block's arguments.
    if (auto *BB = V->getParentBlock()) {
      if (!Region.contains(BB) && !ValueMap.count(V))
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
};

} // end anonymous namespace

void LoopCloner::sinkAddressProjections() {
  SinkAddressProjections sinkProj;
  for (auto *bb : Region.getBlocks()) {
    for (auto &inst : *bb) {
      for (auto res : inst.getResults()) {
        if (!res->getType().isAddress()) {
          continue;
        }
        for (auto use : res->getUses()) {
          auto *user = use->getUser();
          if (Region.contains(user->getParent())) {
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
  Region.getExitBlocks(ExitBlocks);

  sinkAddressProjections();
  // Clone the entire loop.
  cloneReachableBlocks(Region.getLoop()->getHeader(), ExitBlocks,
                       /*insertAfter*/ Region.getLoop()->getLoopLatch());
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

  // Match an add 1 or sub 1 recurrence.

  auto *Cmp = dyn_cast<BuiltinInst>(CondBr->getCondition());
  if (!Cmp)
    return std::nullopt;

  // Only accept integer comparison builtins.
  auto CmpBuiltinKind = Cmp->getBuiltinInfo().ID;
  switch (CmpBuiltinKind) {
  case BuiltinValueKind::ICMP_EQ:
  case BuiltinValueKind::ICMP_NE:
  case BuiltinValueKind::ICMP_SGE:
  case BuiltinValueKind::ICMP_SGT:
  case BuiltinValueKind::ICMP_SLE:
  case BuiltinValueKind::ICMP_SLT:
    break;
  default:
    return std::nullopt;
  }

  SILValue RecNext = Cmp->getArguments()[0];
  SILPhiArgument *RecArg;

  // Extra iterations implied by the structure of the loop.
  // Adjust : [0, 2]
  uint64_t Adjust = 0;
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

    // Comparing the value before the increment takes one more iteration to
    // reach the exit value.
    Adjust += 1;
  }

  // Whether the loop counter is incremented on each iteration. If not, it is
  // decremented on each iteration.
  bool stepWithAdd;
  // Match signed add/sub with overflow, unsigned add/sub with overflow and
  // add/sub without overflow.
  if (match(RecNext, m_TupleExtractOperation(
                         m_ApplyInst(BuiltinValueKind::SAddOver,
                                     m_SILPhiArgument(RecArg), m_One()),
                         0)) ||
      match(RecNext, m_TupleExtractOperation(
                         m_ApplyInst(BuiltinValueKind::UAddOver,
                                     m_SILPhiArgument(RecArg), m_One()),
                         0)) ||
      match(RecNext, m_ApplyInst(BuiltinValueKind::Add,
                                 m_SILPhiArgument(RecArg), m_One()))) {
    stepWithAdd = true;
  } else if (match(RecNext, m_TupleExtractOperation(
                                m_ApplyInst(BuiltinValueKind::SSubOver,
                                            m_SILPhiArgument(RecArg), m_One()),
                                0)) ||
             match(RecNext, m_TupleExtractOperation(
                                m_ApplyInst(BuiltinValueKind::USubOver,
                                            m_SILPhiArgument(RecArg), m_One()),
                                0)) ||
             match(RecNext, m_ApplyInst(BuiltinValueKind::Sub,
                                        m_SILPhiArgument(RecArg), m_One()))) {
    stepWithAdd = false;
  } else {
    return std::nullopt;
  }

  if (RecArg->getParent() != Header)
    return std::nullopt;

  // Identify the exit block and account for extra iterations due to the branch
  // condition's boundary cases. When stepping with sub, the handling of
  // "greater" and "less" are reversed.
  SILBasicBlock *Exit = CondBr->getTrueBB();
  if (stepWithAdd) {
    switch (CmpBuiltinKind) {
    case BuiltinValueKind::ICMP_EQ:
    case BuiltinValueKind::ICMP_SGE:
      break;
    case BuiltinValueKind::ICMP_SGT:
      Adjust += 1;
      break;
    case BuiltinValueKind::ICMP_SLE:
      Exit = CondBr->getFalseBB();
      Adjust += 1;
      break;
    case BuiltinValueKind::ICMP_NE:
    case BuiltinValueKind::ICMP_SLT:
      Exit = CondBr->getFalseBB();
      break;
    default:
      return std::nullopt;
    }
  } else {
    switch (CmpBuiltinKind) {
    case BuiltinValueKind::ICMP_EQ:
    case BuiltinValueKind::ICMP_SLE:
      break;
    case BuiltinValueKind::ICMP_SLT:
      Adjust += 1;
      break;
    case BuiltinValueKind::ICMP_SGE:
      Exit = CondBr->getFalseBB();
      Adjust += 1;
      break;
    case BuiltinValueKind::ICMP_NE:
    case BuiltinValueKind::ICMP_SGT:
      Exit = CondBr->getFalseBB();
      break;
    default:
      return std::nullopt;
    }
  }

  if (Loop->contains(Exit))
    return std::nullopt;

  auto *End = dyn_cast<IntegerLiteralInst>(Cmp->getArguments()[1]);
  if (!End)
    return std::nullopt;

  auto *Start = dyn_cast_or_null<IntegerLiteralInst>(
      RecArg->getIncomingPhiValue(Preheader));
  if (!Start)
    return std::nullopt;

  if (RecNext != RecArg->getIncomingPhiValue(Latch))
    return std::nullopt;

  APInt MinVal, MaxVal;
  if (stepWithAdd) {
    MinVal = Start->getValue();
    MaxVal = End->getValue();
  } else {
    MaxVal = Start->getValue();
    MinVal = End->getValue();
  }
  if (MinVal.sgt(MaxVal))
    return std::nullopt;

  auto Dist = MaxVal - MinVal;
  if (Dist.isZero())
    return std::nullopt;

  // Ensure Dist has enough bits for Adjust to fit.
  if (!llvm::isUIntN(Dist.getBitWidth(), Adjust))
    return std::nullopt;

  bool overflow;
  Dist = Dist.uadd_ov(APInt(Dist.getBitWidth(), Adjust), overflow);

  if (overflow || Dist.getBitWidth() > 64)
    return std::nullopt;

  uint64_t TripCount = Dist.getZExtValue();

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

struct UnrollablePackLoopInfo {
  SILBasicBlock *header;
  unsigned depth;
};

/// Collect loop headers with an argument that is used as a dynamic pack index
/// by an `open_pack_element` instruction whose opened Pack types contain no
/// expansions. These are the headers of candidate loops for Pack Loop
/// Unrolling.
///
/// TODO: Avoid unrolling loops over homogeneous packs when possible. We must
/// unroll even loops over homogeneous packs in some cases; for example,
/// unrolling is necessary to eliminate `tuple_pack_element_addr` instructions
/// that use the dynamic pack index.
///
/// Returns an array containing each loop header and its depth.
static auto collectPackLoopHeaders(SILLoopInfo *LoopInfo, SILFunction *Fun) {
  llvm::SmallVector<UnrollablePackLoopInfo> packLoopHeaders;
  // Multiple `open_pack_element` instructions could use the same header
  // argument, so keep track of which ones we have already collected.
  BasicBlockSet collectedBlocks(Fun);

  for (auto *OutermostLoop : *LoopInfo) {
    for (auto *BB : OutermostLoop->getBlocks()) {
      for (auto &Inst : *BB) {
        // Match the pattern:
        // headerbb(%arg):
        //   dynamic_pack_index %arg
        //   (pack_pack_index)*
        //   open_pack_element ... of $Pack{Concrete, Types}
        auto *ope = dyn_cast<OpenPackElementInst>(&Inst);
        if (!ope)
          continue;
        auto *opeIdx = ope->getIndexOperand();
        while (auto *ppi = dyn_cast<PackPackIndexInst>(opeIdx)) {
          opeIdx = ppi->getSliceIndexOperand();
        }
        auto *dpi = dyn_cast<DynamicPackIndexInst>(opeIdx);
        if (!dpi)
          continue;
        auto *indexArg = dyn_cast<SILArgument>(dpi->getOperand());
        if (!indexArg || !LoopInfo->isLoopHeader(indexArg->getParent()))
          continue;

        // Check if every opened Pack type contains a list of non-pack expansion
        // (scalar) types.
        bool allScalar = true;
        ope->getOpenedGenericEnvironment()->forEachPackElementBinding(
            [&](ElementArchetypeType *elementArchetype, PackType *pack) {
              if (!allScalar)
                return;

              if (pack->getNumElements() == 0 || pack->containsPackExpansionType())
                allScalar = false;
            });

        if (allScalar &&
            !collectedBlocks.contains(indexArg->getParent())) {
          collectedBlocks.insert(indexArg->getParent());
          packLoopHeaders.push_back(
              {indexArg->getParent(),
               LoopInfo->getLoopDepth(indexArg->getParent())});
        }
      }
    }
  }

  return packLoopHeaders;
}

/// Check whether we can duplicate the instructions in the loop and its cleanup
/// prefixes. Return the loop's UnrollRegion if we can.
static std::optional<UnrollRegion> canUnrollLoop(SILLoop *Loop,
                                                 DeadEndBlocks *deb) {
  UnrollRegion Region(Loop);
  for (auto *Inst : Region.instructions()) {
    if (!Region.canDuplicate(Inst, deb))
      return std::nullopt;
  }
  return Region;
}

/// Check whether we can duplicate the instructions in the loop and its cleanup
/// prefixes, and use a heuristic that looks at the trip count and the cost of
/// the instructions in the region to determine whether we should unroll it.
/// Return the loop's UnrollRegion if we can.
static std::optional<UnrollRegion>
canAndShouldUnrollLoop(SILLoop *Loop, uint64_t TripCount,
                       IsSelfRecursiveAnalysis *SRA, DeadEndBlocks *deb) {
  assert(Loop->getSubLoops().empty() && "Expect innermost loops");
  if (TripCount > 32)
    return std::nullopt;

  UnrollRegion Region(Loop);
  uint64_t Cost = 0;
  // Average number of instructions per basic block.
  // It is used to estimate the cost of the callee
  // inside a loop.
  const uint64_t InsnsPerBB = 4;
  // Use command-line threshold for unrolling.
  const uint64_t SILLoopUnrollThreshold = Loop->getBlocks().empty() ? 0 : 
    (Loop->getBlocks())[0]->getParent()->getModule().getOptions().UnrollThreshold;

  const bool isPackLoop = isPackIterationLoop(Loop);
  for (auto *Inst : Region.instructions()) {
    // We can unroll a loop if we can duplicate the instructions it holds.
    if (!Region.canDuplicate(Inst, deb))
      return std::nullopt;
    if (instructionInlineCost(*Inst) != InlineCost::Free)
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
        return std::nullopt;
      }
      if (Callee && getEligibleFunction(AI, InlineSelection::Everything, SRA)) {
        // If callee is rather big and potentially inlinable, it may be better
        // not to unroll, so that the body of the callee can be inlined later.
        Cost += Callee->size() * InsnsPerBB;
      }
    }
    if (Cost * TripCount > SILLoopUnrollThreshold)
      return std::nullopt;
  }
  return Region;
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
  for (auto *Block : Region.getBlocks()) {
    // Look at block arguments.
    for (auto *Arg : Block->getArguments()) {
      for (auto *Op : Arg->getUses()) {
        // Is this use outside the cloned region?
        if (!Region.contains(Op->getParentBlock())) {
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
          if (Region.contains(Op->getParentBlock()))
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
updateSSA(SILFunction *Fn, const UnrollRegion &Region,
          MapVector<SILValue, SmallVector<SILValue, 8>> &LoopLiveOutValues) {
  SILSSAUpdater SSAUp;
  for (auto &MapEntry : LoopLiveOutValues) {
    // Collect the uses of this value outside the cloned region.
    auto OrigValue = MapEntry.first;
    SmallVector<UseWrapper, 16> UseList;
    for (auto Use : OrigValue->getUses())
      if (!Region.contains(Use->getParentBlock()))
        UseList.push_back(UseWrapper(Use));
    // Update SSA of use with the available values.
    SSAUp.initialize(Fn, OrigValue->getType(), OrigValue->getOwnershipKind());
    SSAUp.addAvailableValue(OrigValue->getParentBlock(), OrigValue);
    for (auto NewValue : MapEntry.second)
      SSAUp.addAvailableValue(NewValue->getParentBlock(), NewValue);
    SSAUp.rewriteUses(UseList);
  }
}

/// Fully unroll a loop that has a Preheader, Header, Latch & known max trip
/// count.
static bool unrollLoop(UnrollRegion &Region, SILBasicBlock *Preheader,
                       SILBasicBlock *Header, SILBasicBlock *Latch,
                       const CountedLoopExit &CountedExit, DeadEndBlocks *deb,
                       DominanceInfo *DT, SILLoopInfo *LoopInfo) {
  SILLoop *Loop = Region.getLoop();
  uint64_t MaxTripCount = CountedExit.TripCount;

  LLVM_DEBUG(llvm::dbgs() << "Unrolling loop in "
                          << Header->getParent()->getName()
                          << " " << *Loop << "\n");

  // The cleanup prefixes only need to be split off if they are cloned.
  if (MaxTripCount > 1)
    Region.splitCleanupPrefixes(deb, DT, LoopInfo);

  SmallVector<SILBasicBlock *, 16> Headers;
  Headers.push_back(Header);

  SmallVector<SILBasicBlock *, 16> Latches;
  Latches.push_back(Latch);

  MapVector<SILValue, SmallVector<SILValue, 8>> LoopLiveOutValues;

  // The counted exiting block of the last iteration.
  SILBasicBlock *LastExiting = CountedExit.ExitingBlock;

  // Copy the body MaxTripCount-1 times.
  for (uint64_t Cnt = 1; Cnt < MaxTripCount; ++Cnt) {
    // Clone the blocks in the loop.
    LoopCloner cloner(Region);
    cloner.cloneLoop();
    Headers.push_back(cloner.getOpBasicBlock(Header));
    Latches.push_back(cloner.getOpBasicBlock(Latch));
    LastExiting = cloner.getOpBasicBlock(CountedExit.ExitingBlock);

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
  foldLastIterationExit(LastExiting, CountedExit.ExitsOnTrue);

  // Fixup SSA form for loop values used outside the loop.
  updateSSA(Loop->getFunction(), Region, LoopLiveOutValues);
  return true;
}

/// Try to fully unroll the loop if we can determine the trip count and the trip
/// count is below a threshold.
static bool tryToUnrollLoop(SILLoop *Loop, IsSelfRecursiveAnalysis *SRA,
                            DeadEndBlocks *deb, DominanceInfo *DT,
                            SILLoopInfo *LoopInfo) {
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

  auto Region = canAndShouldUnrollLoop(Loop, CountedExit->TripCount, SRA, deb);
  if (!Region) {
    LLVM_DEBUG(llvm::dbgs() << "Not unrolling, exceeds cost threshold\n");
    return false;
  }

  return unrollLoop(*Region, Preheader, Header, Latch, *CountedExit, deb, DT,
                    LoopInfo);
}

/// Try to fully unroll the loop if we can determine the trip count.
static bool tryToUnrollPackLoop(SILLoop *Loop, DeadEndBlocks *deb,
                                DominanceInfo *DT, SILLoopInfo *LoopInfo) {
  assert(isPackIterationLoop(Loop) && "Expecting dynamic pack loop");

  LLVM_DEBUG(llvm::dbgs() << "Trying to unroll pack loop : \n" << *Loop);
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

  auto Region = canUnrollLoop(Loop, deb);
  if (!Region) {
    LLVM_DEBUG(llvm::dbgs() << "Not unrolling, contains instruction(s) that "
                               "cannot be duplicated\n");
    return false;
  }

  return unrollLoop(*Region, Preheader, Header, Latch, *CountedExit, deb, DT,
                    LoopInfo);
}

// =============================================================================
//                                 Driver
// =============================================================================

static void cleanupAfterUnrolling(SILFunction *Fun, SILPassManager *PM) {
  updateAllGuaranteedPhis(PM, Fun);
  PM->invalidateAnalysis(Fun, SILAnalysis::InvalidationKind::FunctionBody);
  removeUnreachableBlocks(*Fun);
  if (Fun->needBreakInfiniteLoops())
    breakInfiniteLoops(PM, Fun);
  if (Fun->needCompleteLifetimes())
    completeAllLifetimes(PM, Fun);
}

namespace swift {
bool unrollPackLoopsInFunction(SILFunction *Fun, SILPassManager *PM) {
  SILLoopInfo *LoopInfo = PM->getAnalysis<SILLoopAnalysis>()->get(Fun);

  auto unrollablePackLoopHeaders = collectPackLoopHeaders(LoopInfo, Fun);

  if (unrollablePackLoopHeaders.empty())
    return false;

  DeadEndBlocks *deb = PM->getAnalysis<DeadEndBlocksAnalysis>()->get(Fun);
  DominanceInfo *dom = PM->getAnalysis<DominanceAnalysis>()->get(Fun);
  bool anyChanged = false;

  // Attempt to unroll every collected pack loop, deepest first. Unrolling an
  // inner loop does not affect the blocks outside it, so the set of
  // unrollable Pack loop headers is unaffected. This lets us avoid repeating
  // the analysis to precisely detect pack loops, which is non-trivial.
  llvm::sort(unrollablePackLoopHeaders,
             [&](const auto &a, const auto &b) { return a.depth > b.depth; });
  auto lastAnalysisDepth = unrollablePackLoopHeaders[0].depth;
  bool changed = false;
  for (const auto &packLoopInfo : unrollablePackLoopHeaders) {
    const auto depth = packLoopInfo.depth;
    if (changed && depth != lastAnalysisDepth) {
      // An inner loop was unrolled, invalidating the loop and dead-end blocks
      // analyses for all loops at shallower depths. Clean up and re-analyse.
      cleanupAfterUnrolling(Fun, PM);
      LoopInfo = PM->getAnalysis<SILLoopAnalysis>()->get(Fun);
      deb = PM->getAnalysis<DeadEndBlocksAnalysis>()->get(Fun);
      dom = PM->getAnalysis<DominanceAnalysis>()->get(Fun);
      changed = false;
      lastAnalysisDepth = depth;
    }

    auto *Loop = LoopInfo->getLoopFor(packLoopInfo.header);
    ASSERT(Loop && "All collected blocks are loop headers.");
    // First canonicalize and rotate the loop so more pack loops can unroll.
    changed |= canonicalizeLoop(Loop, dom, LoopInfo);
    changed |=
        rotateLoopAtMostUpToLatch(Loop, dom, LoopInfo, PM, /*ignoreCost=*/true);
    changed |= tryToUnrollPackLoop(Loop, deb, dom, LoopInfo);
    anyChanged |= changed;
  }

  if (changed)
    cleanupAfterUnrolling(Fun, PM);

  return anyChanged;
}
} // namespace swift

namespace {

class LoopUnrolling : public SILFunctionTransform {

  void run() override {
    bool Changed = false;
    auto *Fun = getFunction();
    SILLoopInfo *LoopInfo = PM->getAnalysis<SILLoopAnalysis>()->get(Fun);
    IsSelfRecursiveAnalysis *SRA = PM->getAnalysis<IsSelfRecursiveAnalysis>();
    DeadEndBlocks *deb = PM->getAnalysis<DeadEndBlocksAnalysis>()->get(Fun);
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
      Changed |= tryToUnrollLoop(Loop, SRA, deb, DT, LoopInfo);

    if (Changed) {
      cleanupAfterUnrolling(Fun, PM);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createLoopUnroll() {
  return new LoopUnrolling();
}
