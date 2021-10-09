//===--- SILMem2Reg.cpp - Promotes AllocStacks to registers ---------------===//
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
//
// This pass promotes AllocStack instructions into virtual register
// references. It only handles load, store and deallocation
// instructions. The algorithm is based on:
//
//  Sreedhar and Gao. A linear time algorithm for placing phi-nodes. POPL '95.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-mem2reg"

#include "swift/AST/DiagnosticsSIL.h"
#include "swift/Basic/DAGNodeWorklist.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/OwnershipOptUtils.h"
#include "swift/SILOptimizer/Utils/ScopeOptUtils.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"
#include <algorithm>
#include <queue>

using namespace swift;
using namespace swift::siloptimizer;

STATISTIC(NumAllocStackFound,    "Number of AllocStack found");
STATISTIC(NumAllocStackCaptured, "Number of AllocStack captured");
STATISTIC(NumInstRemoved,        "Number of Instructions removed");

static bool shouldAddLexicalLifetime(AllocStackInst *asi);

namespace {

using DomTreeNode = llvm::DomTreeNodeBase<SILBasicBlock>;
using DomTreeLevelMap = llvm::DenseMap<DomTreeNode *, unsigned>;

/// The values pertaining to an alloc_stack that might be live, whether within
/// the block, as the block is entered (live-in) or exited
/// (live-out) relating to an alloc_stack.
struct LiveValues {
  SILValue stored = SILValue();
  SILValue borrow = SILValue();
  SILValue copy = SILValue();

  /// Create an instance of the minimum values required to replace a usage of an
  /// AllocStackInst.  It consists of only one value.
  ///
  /// Whether the one value occupies the stored or the copy field depends on
  /// whether the alloc_stack is lexical.  If it is lexical, then usages of the
  /// asi will be replaced with usages of the copy field; otherwise, those
  /// usages will be replaced with usages of the stored field.  The
  /// implementation constructs an instance to match those requirements.
  static LiveValues toReplace(AllocStackInst *asi, SILValue replacement) {
    if (shouldAddLexicalLifetime(asi))
      return {SILValue(), SILValue(), replacement};
    return {replacement, SILValue(), SILValue()};
  };
  /// The value with which usages of the provided AllocStackInst should be
  /// replaced.
  ///
  /// For lexical AllocStackInsts, that is the copy made of the borrowed value.
  /// For those that are non-lexical, that is the value that was stored into the
  /// storage.
  SILValue replacement(AllocStackInst *asi) {
    return shouldAddLexicalLifetime(asi) ? copy : stored;
  };
};

/// The instructions that are involved in or introduced in response to a store
/// into an alloc_stack.
///
/// There is always a store.  The begin_borrow and copy_value instructions are
/// introduced for lexical alloc_stacks.
struct ValueInstructions {
  // The value of interest is the operand of the store instruction.
  StoreInst *store = nullptr;
  // The value of interest is the result of the begin_borrow instruction.
  BeginBorrowInst *borrow = nullptr;
  // The value of interest is the result of the copy_value instruction.
  CopyValueInst *copy = nullptr;
};

} // anonymous namespace

//===----------------------------------------------------------------------===//
//                                 Utilities
//===----------------------------------------------------------------------===//

static void replaceDestroy(DestroyAddrInst *dai, SILValue newValue,
                           SILBuilderContext &ctx,
                           InstructionDeleter &deleter) {
  SILFunction *f = dai->getFunction();
  auto ty = dai->getOperand()->getType();

  assert(ty.isLoadable(*f) && "Unexpected promotion of address-only type!");

  assert(newValue ||
         (ty.is<TupleType>() && ty.getAs<TupleType>()->getNumElements() == 0));

  SILBuilderWithScope builder(dai, ctx);

  auto &typeLowering = f->getTypeLowering(ty);

  bool expand = shouldExpand(dai->getModule(),
                             dai->getOperand()->getType().getObjectType());
  using TypeExpansionKind = Lowering::TypeLowering::TypeExpansionKind;
  auto expansionKind = expand ? TypeExpansionKind::MostDerivedDescendents
                              : TypeExpansionKind::None;
  typeLowering.emitLoweredDestroyValue(builder, dai->getLoc(), newValue,
                                       expansionKind);
  deleter.forceDelete(dai);
}

/// Promote a DebugValue w/ address value to a DebugValue of non-address value.
static void promoteDebugValueAddr(DebugValueInst *dvai, SILValue value,
                                  SILBuilderContext &ctx,
                                  InstructionDeleter &deleter) {
  assert(dvai->getOperand()->getType().isLoadable(*dvai->getFunction()) &&
         "Unexpected promotion of address-only type!");
  assert(value && "Expected valid value");
  // Avoid inserting the same debug_value twice.
  for (auto *use : value->getUses()) {
    if (auto *dvi = dyn_cast<DebugValueInst>(use->getUser())) {
      // Since we're not comparing di-expression in
      // SILDebugVariable::operator==(), it's necessary to distinguish
      // debug_value w/ normal values from that with address-type values.
      if (!dvi->hasAddrVal() &&
          *dvi->getVarInfo() == *dvai->getVarInfo()) {
        deleter.forceDelete(dvai);
        return;
      }
    }
  }

  auto VarInfo = *dvai->getVarInfo();
  // Drop op_deref if dvai is actually a debug_value instruction
  if (isa<DebugValueInst>(dvai)) {
    auto &DIExpr = VarInfo.DIExpr;
    if (DIExpr)
      DIExpr.eraseElement(DIExpr.element_begin());
  }

  SILBuilderWithScope b(dvai, ctx);
  b.createDebugValue(dvai->getLoc(), value, std::move(VarInfo));
  deleter.forceDelete(dvai);
}

/// Returns true if \p I is a load which loads from \p ASI.
static bool isLoadFromStack(SILInstruction *i, AllocStackInst *asi) {
  if (!isa<LoadInst>(i))
    return false;

  // Skip struct and tuple address projections.
  ValueBase *op = i->getOperand(0);
  while (op != asi) {
    if (!isa<UncheckedAddrCastInst>(op) && !isa<StructElementAddrInst>(op) &&
        !isa<TupleElementAddrInst>(op))
      return false;
    op = cast<SingleValueInstruction>(op)->getOperand(0);
  }
  return true;
}

/// Collects all load instructions which (transitively) use \p I as address.
static void collectLoads(SILInstruction *i,
                         SmallVectorImpl<LoadInst *> &foundLoads) {
  if (auto *load = dyn_cast<LoadInst>(i)) {
    foundLoads.push_back(load);
    return;
  }
  if (!isa<UncheckedAddrCastInst>(i) && !isa<StructElementAddrInst>(i) &&
      !isa<TupleElementAddrInst>(i))
    return;

  // Recursively search for other loads in the instruction's uses.
  for (auto *use : cast<SingleValueInstruction>(i)->getUses()) {
    collectLoads(use->getUser(), foundLoads);
  }
}

static void replaceLoad(LoadInst *li, SILValue newValue, AllocStackInst *asi,
                        SILBuilderContext &ctx, InstructionDeleter &deleter) {
  ProjectionPath projections(newValue->getType());
  SILValue op = li->getOperand();
  SILBuilderWithScope builder(li, ctx);
  SILOptScope scope;

  while (op != asi) {
    assert(isa<UncheckedAddrCastInst>(op) || isa<StructElementAddrInst>(op) ||
           isa<TupleElementAddrInst>(op) &&
               "found instruction that should have been skipped in "
               "isLoadFromStack");
    auto *inst = cast<SingleValueInstruction>(op);
    projections.push_back(Projection(inst));
    op = inst->getOperand(0);
  }

  for (const auto &proj : llvm::reverse(projections)) {
    assert(proj.getKind() == ProjectionKind::BitwiseCast ||
           proj.getKind() == ProjectionKind::Struct ||
           proj.getKind() == ProjectionKind::Tuple);

    // struct_extract and tuple_extract expect guaranteed operand ownership
    // non-trivial RunningVal is owned. Insert borrow operation to convert them
    // to guaranteed!
    if (proj.getKind() == ProjectionKind::Struct ||
        proj.getKind() == ProjectionKind::Tuple) {
      if (auto opVal = scope.borrowValue(li, newValue)) {
        assert(*opVal != newValue &&
               "Valid value should be different from input value");
        newValue = *opVal;
      }
    }
    newValue =
        proj.createObjectProjection(builder, li->getLoc(), newValue).get();
  }

  op = li->getOperand();

  // Replace users of the loaded value with `val`
  // If we have a load [copy], replace the users with copy_value of `val`
  if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Copy) {
    li->replaceAllUsesWith(builder.createCopyValue(li->getLoc(), newValue));
  } else {
    assert(!asi->getFunction()->hasOwnership() ||
           newValue.getOwnershipKind() != OwnershipKind::Guaranteed);
    li->replaceAllUsesWith(newValue);
  }

  // Pop the scope so that we emit cleanups.
  std::move(scope).popAtEndOfScope(&*builder.getInsertionPoint());

  // Delete the load
  deleter.forceDelete(li);

  while (op != asi && op->use_empty()) {
    assert(isa<UncheckedAddrCastInst>(op) || isa<StructElementAddrInst>(op) ||
           isa<TupleElementAddrInst>(op));
    auto *inst = cast<SingleValueInstruction>(op);
    SILValue next = inst->getOperand(0);
    deleter.forceDelete(inst);
    op = next;
  }
}

/// Create a tuple value for an empty tuple or a tuple of empty tuples.
static SILValue createValueForEmptyTuple(SILType ty,
                                         SILInstruction *insertionPoint,
                                         SILBuilderContext &ctx) {
  auto tupleTy = ty.castTo<TupleType>();
  SmallVector<SILValue, 4> elements;
  for (unsigned idx : range(tupleTy->getNumElements())) {
    SILType elementTy = ty.getTupleElementType(idx);
    elements.push_back(
        createValueForEmptyTuple(elementTy, insertionPoint, ctx));
  }
  SILBuilderWithScope builder(insertionPoint, ctx);
  return builder.createTuple(insertionPoint->getLoc(), ty, elements);
}

/// Whether lexical lifetimes should be added for the values stored into the
/// alloc_stack.
static bool shouldAddLexicalLifetime(AllocStackInst *asi) {
  return asi->getFunction()->hasOwnership() &&
         asi->getFunction()
             ->getModule()
             .getASTContext()
             .LangOpts.EnableExperimentalLexicalLifetimes &&
         asi->isLexical() &&
         !asi->getElementType().isTrivial(*asi->getFunction());
}

/// Given a store instruction after which it is known that a lexical borrow
/// scope should be added, add the beginning at the appropriate position.
///
/// The beginning of the scope looks like
///
///     %lifetime = begin_borrow %original
///     %copy = copy_value %lifetime
static std::pair<ValueInstructions, LiveValues>
beginLexicalLifetimeAtStore(AllocStackInst *asi, StoreInst *si,
                            SILBuilderContext &ctx) {
  assert(shouldAddLexicalLifetime(asi));
  auto *bbi =
      SILBuilderWithScope(si, ctx).createBeginBorrow(si->getLoc(), si->getSrc(),
                                                     /*isLexical*/ true);
  auto *cvi = SILBuilderWithScope(si, ctx).createCopyValue(si->getLoc(), bbi);
  ValueInstructions insts = {si, bbi, cvi};
  LiveValues vals = {si->getSrc(), bbi, cvi};
  return std::make_pair(insts, vals);
}

/// Given a block to which it is known that the end of a lexical borrow scope
/// should be added, add the end at the appropriate position.
///
/// The end of the scope looks like
///
///     end_borrow %lifetime
///     destroy_value %original
///
/// These two instructions correspond to the following instructions that begin
/// a lexical borrow scope:
///
///     %lifetime = begin_borrow %original
///     %copy = copy_value %lifetime
///
/// There are a couple options for where the instructions will be added:
/// (1) If there are uses of the borrow within the block, then they are added
///     after the last use.
/// (2) If there are no uses of the borrow within the block, and the copy_value
///     instruction appears within the block, they are added immediately after
///     it.
/// (3) If there are no uses of the borrow within the block and the copy_value
///     instruction does not appear within the block, then the instructions are
///     added at the beginning of the block.
/// They should be added after the value is done being used.  If the value is
/// ever used, that means after the last use.  Otherwise, it means immediately
/// after the definition.
void endLexicalLifetimeInBlock(AllocStackInst *asi, SILBasicBlock *bb,
                               SILBuilderContext &ctx, DominanceInfo *domInfo,
                               SILValue copy, SILValue borrow,
                               SILValue original) {
  assert(shouldAddLexicalLifetime(asi));
  SILInstruction *lastInst = nullptr;
  if (auto *cvi = copy->getDefiningInstruction()) {
    if (cvi->getParent() == bb) {
      lastInst = cvi;
    }
  }
  for (auto *use : copy->getUses()) {
    auto *thisInst = use->getUser();
    if (thisInst->getParent() != bb)
      continue;
    if (!lastInst || domInfo->dominates(lastInst, thisInst))
      lastInst = thisInst;
  }
  if (lastInst) {
    assert(!isa<TermInst>(lastInst) &&
           "the last use of the value cannot be terminal--Mem2Reg does not run "
           "on alloc_stacks which are passed to checked_cast_addr_br or "
           "switch_enum_addr");
    SILBuilderWithScope::insertAfter(lastInst, [&](SILBuilder &B) {
      B.createEndBorrow(lastInst->getLoc(), borrow);
      B.emitDestroyValueOperation(lastInst->getLoc(), original);
    });
  } else {
    auto *firstInst = &*bb->begin();
    auto builder = SILBuilderWithScope(firstInst, ctx);
    builder.createEndBorrow(firstInst->getLoc(), borrow);
    builder.emitDestroyValueOperation(firstInst->getLoc(), original);
  }
}

//===----------------------------------------------------------------------===//
//                     Single Stack Allocation Promotion
//===----------------------------------------------------------------------===//

namespace {

/// Promotes a single AllocStackInst into registers..
class StackAllocationPromoter {
  using BlockSetVector = BasicBlockSetVector;
  using BlockToInstsMap = llvm::DenseMap<SILBasicBlock *, ValueInstructions>;

  // Use a priority queue keyed on dominator tree level so that inserted nodes
  // are handled from the bottom of the dom tree upwards.
  using DomTreeNodePair = std::pair<DomTreeNode *, unsigned>;
  using NodePriorityQueue =
      std::priority_queue<DomTreeNodePair, SmallVector<DomTreeNodePair, 32>,
                          llvm::less_second>;

  /// The AllocStackInst that we are handling.
  AllocStackInst *asi;

  /// The deallocation Instruction. This value could be NULL if there are
  /// multiple deallocations.
  DeallocStackInst *dsi;

  /// All the dealloc_stack instructions.
  llvm::SmallVector<DeallocStackInst *> dsis;

  /// The lexical begin_borrow instructions that were created to track the
  /// lexical lifetimes introduced by the alloc_stack, if it is lexical.
  llvm::SmallVector<SILBasicBlock *> lexicalBBIBlocks;

  /// Dominator info.
  DominanceInfo *domInfo;

  /// Map from dominator tree node to tree level.
  DomTreeLevelMap &domTreeLevels;

  /// The SIL builder used when creating new instructions during register
  /// promotion.
  SILBuilderContext &ctx;

  InstructionDeleter &deleter;

  /// Records the last store instruction in each block for a specific
  /// AllocStackInst.
  BlockToInstsMap lastStoreInBlock;

public:
  /// C'tor.
  StackAllocationPromoter(AllocStackInst *inputASI, DominanceInfo *inputDomInfo,
                          DomTreeLevelMap &inputDomTreeLevels,
                          SILBuilderContext &inputCtx,
                          InstructionDeleter &deleter)
      : asi(inputASI), dsi(nullptr), domInfo(inputDomInfo),
        domTreeLevels(inputDomTreeLevels), ctx(inputCtx), deleter(deleter) {
    // Scan the users in search of a deallocation instruction.
    for (auto *use : asi->getUses())
      if (auto *foundDealloc = dyn_cast<DeallocStackInst>(use->getUser()))
        dsis.push_back(foundDealloc);
    if (dsis.size() == 1) {
      // Record the deallocation instruction, but don't record multiple dealloc
      // instructions.
      dsi = dsis.front();
    }
  }

  /// Promote the Allocation.
  void run();

private:
  /// Promote AllocStacks into SSA.
  void promoteAllocationToPhi();

  /// Replace the dummy nodes with new block arguments.
  void addBlockArguments(BlockSetVector &phiBlocks);

  /// Check if \p phi is a proactively added phi by SILMem2Reg
  bool isProactivePhi(SILPhiArgument *phi, const BlockSetVector &phiBlocks);

  /// Check if \p proactivePhi is live.
  bool isNecessaryProactivePhi(SILPhiArgument *proactivePhi,
                               const BlockSetVector &phiBlocks);

  /// Given a \p proactivePhi that is live, backward propagate liveness to
  /// other proactivePhis.
  void propagateLiveness(SILPhiArgument *proactivePhi,
                         const BlockSetVector &phiBlocks,
                         SmallPtrSetImpl<SILPhiArgument *> &livePhis);

  /// End the lexical borrow scope that is introduced for lexical alloc_stack
  /// instructions.
  void endLexicalLifetime(BlockSetVector &phiBlocks);

  /// Fix all of the branch instructions and the uses to use
  /// the AllocStack definitions (which include stores and Phis).
  void fixBranchesAndUses(BlockSetVector &blocks, BlockSetVector &liveBlocks);

  /// update the branch instructions with the new Phi argument.
  /// The blocks in \p PhiBlocks are blocks that define a value, \p Dest is
  /// the branch destination, and \p Pred is the predecessors who's branch we
  /// modify.
  void fixPhiPredBlock(BlockSetVector &phiBlocks, SILBasicBlock *dest,
                       SILBasicBlock *pred);

  /// Get the values for this AllocStack variable that are flowing out of
  /// StartBB.
  Optional<LiveValues> getLiveOutValues(BlockSetVector &phiBlocks,
                                        SILBasicBlock *startBlock);

  /// Get the values for this AllocStack variable that are flowing out of
  /// StartBB or undef if there are none.
  LiveValues getEffectiveLiveOutValues(BlockSetVector &phiBlocks,
                                       SILBasicBlock *startBlock);

  /// Get the values for this AllocStack variable that are flowing into block.
  Optional<LiveValues> getLiveInValues(BlockSetVector &phiBlocks,
                                       SILBasicBlock *block);

  /// Get the values for this AllocStack variable that are flowing into block or
  /// undef if there are none.
  LiveValues getEffectiveLiveInValues(BlockSetVector &phiBlocks,
                                      SILBasicBlock *block);

  /// Prune AllocStacks usage in the function. Scan the function
  /// and remove in-block usage of the AllocStack. Leave only the first
  /// load and the last store.
  void pruneAllocStackUsage();

  /// Promote all of the AllocStacks in a single basic block in one
  /// linear scan. This function deletes all of the loads and stores except
  /// for the first load and the last store.
  /// \returns the last ValueInstructions (the store, and, if lexical, the
  ///          begin_borrow and copy_value) found, if any, or else llvm::None.
  Optional<ValueInstructions> promoteAllocationInBlock(SILBasicBlock *block);
};

} // end of namespace

Optional<ValueInstructions> StackAllocationPromoter::promoteAllocationInBlock(
    SILBasicBlock *blockPromotingWithin) {
  LLVM_DEBUG(llvm::dbgs() << "*** Promoting ASI in block: " << *asi);

  // RunningVal is the current value in the stack location.
  // We don't know the value of the alloca until we find the first store.
  Optional<LiveValues> runningVals;
  // Keep track of the last StoreInst that we found and the BeginBorrowInst and
  // CopyValueInst that we created in response if the alloc_stack was lexical.
  Optional<ValueInstructions> lastValueInstructions = llvm::None;

  // For all instructions in the block.
  for (auto bbi = blockPromotingWithin->begin(),
            bbe = blockPromotingWithin->end();
       bbi != bbe;) {
    SILInstruction *inst = &*bbi;
    ++bbi;

    if (isLoadFromStack(inst, asi)) {
      auto *li = cast<LoadInst>(inst);
      if (runningVals) {
        // If we are loading from the AllocStackInst and we already know the
        // content of the Alloca then use it.
        LLVM_DEBUG(llvm::dbgs() << "*** Promoting load: " << *li);
        replaceLoad(li, runningVals->replacement(asi), asi, ctx, deleter);
        ++NumInstRemoved;
      } else if (li->getOperand() == asi &&
                 li->getOwnershipQualifier() != LoadOwnershipQualifier::Copy) {
        // If we don't know the content of the AllocStack then the loaded
        // value *is* the new value;
        // Don't use result of load [copy] as a RunningVal, it necessitates
        // additional logic for cleanup of consuming instructions of the result.
        // StackAllocationPromoter::fixBranchesAndUses will later handle it.
        LLVM_DEBUG(llvm::dbgs() << "*** First load: " << *li);
        runningVals = LiveValues::toReplace(asi, /*replacement=*/li);
      }
      continue;
    }

    // Remove stores and record the value that we are saving as the running
    // value.
    if (auto *si = dyn_cast<StoreInst>(inst)) {
      if (si->getDest() != asi)
        continue;

      // If we see a store [assign], always convert it to a store [init]. This
      // simplifies further processing.
      if (si->getOwnershipQualifier() == StoreOwnershipQualifier::Assign) {
        if (runningVals) {
          SILBuilderWithScope(si, ctx).createDestroyValue(
              si->getLoc(), runningVals->replacement(asi));
        } else {
          SILBuilderWithScope localBuilder(si, ctx);
          auto *newLoad = localBuilder.createLoad(si->getLoc(), asi,
                                                  LoadOwnershipQualifier::Take);
          localBuilder.createDestroyValue(si->getLoc(), newLoad);
        }
        si->setOwnershipQualifier(StoreOwnershipQualifier::Init);
      }

      // If we met a store before this one, delete it.
      if (lastValueInstructions) {
        assert(lastValueInstructions->store->getOwnershipQualifier() !=
                   StoreOwnershipQualifier::Assign &&
               "store [assign] to the stack location should have been "
               "transformed to a store [init]");
        LLVM_DEBUG(llvm::dbgs() << "*** Removing redundant store: "
                                << lastValueInstructions->store);
        ++NumInstRemoved;
        deleter.forceDelete(lastValueInstructions->store);
      }

      // The stored value is the new running value.
      auto oldRunningVals = runningVals;
      runningVals = LiveValues::toReplace(asi, /*replacement=*/si->getSrc());
      // The current store is now the LastStore
      lastValueInstructions = {si, nullptr, nullptr};
      if (shouldAddLexicalLifetime(asi)) {
        if (oldRunningVals) {
          endLexicalLifetimeInBlock(
              asi, si->getParent(), ctx, domInfo, oldRunningVals->copy,
              oldRunningVals->borrow, oldRunningVals->stored);
        }
        std::tie(lastValueInstructions, runningVals) =
            beginLexicalLifetimeAtStore(asi, si, ctx);
      }
      continue;
    }

    // Replace debug_value w/ address value with debug_value of
    // the promoted value.
    // if we have a valid value to use at this point. Otherwise we'll
    // promote this when we deal with hooking up phis.
    if (auto *dvi = DebugValueInst::hasAddrVal(inst)) {
      if (dvi->getOperand() == asi && runningVals)
        promoteDebugValueAddr(dvi, runningVals->replacement(asi), ctx, deleter);
      continue;
    }

    // Replace destroys with a release of the value.
    if (auto *dai = dyn_cast<DestroyAddrInst>(inst)) {
      if (dai->getOperand() == asi && runningVals) {
        replaceDestroy(dai, runningVals->replacement(asi), ctx, deleter);
      }
      continue;
    }

    if (auto *dvi = dyn_cast<DestroyValueInst>(inst)) {
      if (runningVals && dvi->getOperand() == runningVals->replacement(asi)) {
        if (runningVals->borrow) {
          // If we have started a lexical borrow scope in this block for
          // runningVals->stored (when runningVals->stored's definition comes
          // from a store and not from a load), end the borrrow scope here.
          endLexicalLifetimeInBlock(asi, dvi->getParent(), ctx, domInfo,
                                    runningVals->copy, runningVals->borrow,
                                    runningVals->stored);
        }
        // Reset LastStore.
        // So that we don't end up passing dead values as phi args in
        // StackAllocationPromoter::fixBranchesAndUses
        lastValueInstructions = llvm::None;
      }
    }

    // Stop on deallocation.
    if (auto *dsi = dyn_cast<DeallocStackInst>(inst)) {
      if (dsi->getOperand() == asi)
        break;
    }
  }

  if (lastValueInstructions) {
    assert(lastValueInstructions->store->getOwnershipQualifier() !=
               StoreOwnershipQualifier::Assign &&
           "store [assign] to the stack location should have been "
           "transformed to a store [init]");
    LLVM_DEBUG(llvm::dbgs() << "*** Finished promotion. Last store: "
                            << lastValueInstructions->store);
  } else {
    LLVM_DEBUG(llvm::dbgs() << "*** Finished promotion with no stores.\n");
  }

  return lastValueInstructions;
}

void StackAllocationPromoter::addBlockArguments(BlockSetVector &phiBlocks) {
  LLVM_DEBUG(llvm::dbgs() << "*** Adding new block arguments.\n");

  for (auto *block : phiBlocks) {
    // The stored value.
    block->createPhiArgument(asi->getElementType(), OwnershipKind::Owned);
    if (shouldAddLexicalLifetime(asi)) {
      // The borrow scope.
      block->createPhiArgument(asi->getElementType(),
                               OwnershipKind::Guaranteed);
      // The copy of the borrowed value.
      block->createPhiArgument(asi->getElementType(), OwnershipKind::Owned);
    }
  }
}

Optional<LiveValues>
StackAllocationPromoter::getLiveOutValues(BlockSetVector &phiBlocks,
                                          SILBasicBlock *startBlock) {
  LLVM_DEBUG(llvm::dbgs() << "*** Searching for a value definition.\n");
  // Walk the Dom tree in search of a defining value:
  for (DomTreeNode *domNode = domInfo->getNode(startBlock); domNode;
       domNode = domNode->getIDom()) {
    SILBasicBlock *domBlock = domNode->getBlock();

    // If there is a store (that must come after the phi), use its value.
    BlockToInstsMap::iterator it = lastStoreInBlock.find(domBlock);
    if (it != lastStoreInBlock.end()) {
      auto storedValues = it->second;
      auto *si = storedValues.store;
      LLVM_DEBUG(llvm::dbgs() << "*** Found Store def " << *si->getSrc());
      SILValue borrow = storedValues.borrow;
      SILValue copy = storedValues.copy;
      LiveValues values = {si->getSrc(), borrow, copy};
      return values;
    }

    // If there is a Phi definition in this block:
    if (phiBlocks.contains(domBlock)) {
      // Return the dummy instruction that represents the new value that we will
      // add to the basic block.
      SILValue original;
      SILValue borrow;
      SILValue copy;
      if (shouldAddLexicalLifetime(asi)) {
        original = domBlock->getArgument(domBlock->getNumArguments() - 3);
        borrow = domBlock->getArgument(domBlock->getNumArguments() - 2);
        copy = domBlock->getArgument(domBlock->getNumArguments() - 1);
      } else {
        original = domBlock->getArgument(domBlock->getNumArguments() - 1);
        borrow = SILValue();
        copy = SILValue();
      }
      LLVM_DEBUG(llvm::dbgs() << "*** Found a dummy Phi def " << *original);
      LiveValues values = {original, borrow, copy};
      return values;
    }

    // Move to the next dominating block.
    LLVM_DEBUG(llvm::dbgs() << "*** Walking up the iDOM.\n");
  }
  LLVM_DEBUG(llvm::dbgs() << "*** Could not find a Def. Using Undef.\n");
  return llvm::None;
}

LiveValues
StackAllocationPromoter::getEffectiveLiveOutValues(BlockSetVector &phiBlocks,
                                                   SILBasicBlock *startBlock) {
  if (auto values = getLiveOutValues(phiBlocks, startBlock)) {
    return *values;
  }
  auto *undef = SILUndef::get(asi->getElementType(), *asi->getFunction());
  return {undef, undef, undef};
}

Optional<LiveValues>
StackAllocationPromoter::getLiveInValues(BlockSetVector &phiBlocks,
                                         SILBasicBlock *block) {
  // First, check if there is a Phi value in the current block. We know that
  // our loads happen before stores, so we need to first check for Phi nodes
  // in the first block, but stores first in all other stores in the idom
  // chain.
  if (phiBlocks.contains(block)) {
    LLVM_DEBUG(llvm::dbgs() << "*** Found a local Phi definition.\n");
    SILValue original;
    SILValue borrow;
    SILValue copy;
    if (shouldAddLexicalLifetime(asi)) {
      original = block->getArgument(block->getNumArguments() - 3);
      borrow = block->getArgument(block->getNumArguments() - 2);
      copy = block->getArgument(block->getNumArguments() - 1);
    } else {
      original = block->getArgument(block->getNumArguments() - 1);
      borrow = SILValue();
      copy = SILValue();
    }
    LiveValues values = {original, borrow, copy};
    return values;
  }

  if (block->pred_empty() || !domInfo->getNode(block))
    return llvm::None;

  // No phi for this value in this block means that the value flowing
  // out of the immediate dominator reaches here.
  DomTreeNode *iDom = domInfo->getNode(block)->getIDom();
  assert(iDom &&
         "Attempt to get live-in value for alloc_stack in entry block!");

  return getLiveOutValues(phiBlocks, iDom->getBlock());
}

LiveValues
StackAllocationPromoter::getEffectiveLiveInValues(BlockSetVector &phiBlocks,
                                                  SILBasicBlock *block) {
  if (auto values = getLiveInValues(phiBlocks, block)) {
    return *values;
  }
  auto *undef = SILUndef::get(asi->getElementType(), *asi->getFunction());
  return {undef, undef, undef};
}

void StackAllocationPromoter::fixPhiPredBlock(BlockSetVector &phiBlocks,
                                              SILBasicBlock *destBlock,
                                              SILBasicBlock *predBlock) {
  TermInst *ti = predBlock->getTerminator();
  LLVM_DEBUG(llvm::dbgs() << "*** Fixing the terminator " << ti << ".\n");

  LiveValues def = getEffectiveLiveOutValues(phiBlocks, predBlock);

  LLVM_DEBUG(llvm::dbgs() << "*** Found the definition: " << *def.copy);

  llvm::SmallVector<SILValue> vals;
  vals.push_back(def.stored);
  if (shouldAddLexicalLifetime(asi)) {
    vals.push_back(def.borrow);
    vals.push_back(def.copy);
  }

  addArgumentsToBranch(vals, destBlock, ti);
  deleter.forceDelete(ti);
}

bool StackAllocationPromoter::isProactivePhi(SILPhiArgument *phi,
                                             const BlockSetVector &phiBlocks) {
  auto *phiBlock = phi->getParentBlock();
  return phiBlocks.contains(phiBlock) &&
         phi == phiBlock->getArgument(phiBlock->getNumArguments() - 1);
}

bool StackAllocationPromoter::isNecessaryProactivePhi(
    SILPhiArgument *proactivePhi, const BlockSetVector &phiBlocks) {
  assert(isProactivePhi(proactivePhi, phiBlocks));
  for (auto *use : proactivePhi->getUses()) {
    auto *branch = dyn_cast<BranchInst>(use->getUser());
    // A non-branch use is a necessary use
    if (!branch)
      return true;
    auto *destBB = branch->getDestBB();
    auto opNum = use->getOperandNumber();
    // A phi has a necessary use if it is used as a branch op for a
    // non-proactive phi
    if (!phiBlocks.contains(destBB) || (opNum != branch->getNumArgs() - 1))
      return true;
  }
  return false;
}

void StackAllocationPromoter::propagateLiveness(
    SILPhiArgument *proactivePhi, const BlockSetVector &phiBlocks,
    SmallPtrSetImpl<SILPhiArgument *> &livePhis) {
  assert(isProactivePhi(proactivePhi, phiBlocks));
  if (livePhis.contains(proactivePhi))
    return;
  // If liveness has not been propagated, go over the incoming operands and mark
  // any operand values that are proactivePhis as live
  livePhis.insert(proactivePhi);
  SmallVector<SILValue, 16> incomingPhiVals;
  proactivePhi->getIncomingPhiValues(incomingPhiVals);
  for (auto &inVal : incomingPhiVals) {
    auto *inPhi = dyn_cast<SILPhiArgument>(inVal);
    if (!inPhi)
      continue;
    if (!isProactivePhi(inPhi, phiBlocks))
      continue;
    propagateLiveness(inPhi, phiBlocks, livePhis);
  }
}

void StackAllocationPromoter::fixBranchesAndUses(BlockSetVector &phiBlocks,
                                                 BlockSetVector &phiBlocksOut) {
  // First update uses of the value.
  SmallVector<LoadInst *, 4> collectedLoads;

  for (auto ui = asi->use_begin(), ue = asi->use_end(); ui != ue;) {
    auto *user = ui->getUser();
    ++ui;
    bool removedUser = false;

    collectedLoads.clear();
    collectLoads(user, collectedLoads);
    for (auto *li : collectedLoads) {
      LiveValues def;
      // If this block has no predecessors then nothing dominates it and
      // the instruction is unreachable. If the instruction we're
      // examining is a value, replace it with undef. Either way, delete
      // the instruction and move on.
      SILBasicBlock *loadBlock = li->getParent();
      def = getEffectiveLiveInValues(phiBlocks, loadBlock);

      LLVM_DEBUG(llvm::dbgs()
                 << "*** Replacing " << *li << " with Def " << def.stored);

      // Replace the load with the definition that we found.
      replaceLoad(li, def.replacement(asi), asi, ctx, deleter);
      removedUser = true;
      ++NumInstRemoved;
    }

    if (removedUser)
      continue;

    // If this block has no predecessors then nothing dominates it and
    // the instruction is unreachable. Delete the instruction and move
    // on.
    SILBasicBlock *userBlock = user->getParent();

    if (auto *dvi = DebugValueInst::hasAddrVal(user)) {
      // Replace debug_value w/ address-type value with
      // a new debug_value w/ promoted value.
      auto def = getEffectiveLiveInValues(phiBlocks, userBlock);
      promoteDebugValueAddr(dvi, def.replacement(asi), ctx, deleter);
      ++NumInstRemoved;
      continue;
    }

    // Replace destroys with a release of the value.
    if (auto *dai = dyn_cast<DestroyAddrInst>(user)) {
      auto def = getEffectiveLiveInValues(phiBlocks, userBlock);
      replaceDestroy(dai, def.replacement(asi), ctx, deleter);
      continue;
    }
  }

  // Now that all of the uses are fixed we can fix the branches that point
  // to the blocks with the added arguments.
  // For each Block with a new Phi argument:
  for (auto *block : phiBlocks) {
    // Fix all predecessors.
    for (auto pbbi = block->getPredecessorBlocks().begin(),
              pbbe = block->getPredecessorBlocks().end();
         pbbi != pbbe;) {
      auto *predBlock = *pbbi;
      ++pbbi;
      assert(predBlock && "Invalid block!");
      fixPhiPredBlock(phiBlocks, block, predBlock);
    }
  }

  // Fix ownership of proactively created non-trivial phis
  if (asi->getFunction()->hasOwnership() &&
      !asi->getType().isTrivial(*asi->getFunction())) {
    SmallPtrSet<SILPhiArgument *, 4> livePhis;

    for (auto *block : phiBlocks) {
      auto *proactivePhi = cast<SILPhiArgument>(
          block->getArgument(block->getNumArguments() - 1));
      // First, check if the proactively added phi is necessary by looking at
      // it's immediate uses.
      if (isNecessaryProactivePhi(proactivePhi, phiBlocks)) {
        // Backward propagate liveness to other dependent proactively added phis
        propagateLiveness(proactivePhi, phiBlocks, livePhis);
      }
    }
    // Go over all proactively added phis, and delete those that were not marked
    // live above.
    for (auto *block : phiBlocks) {
      auto *proactivePhi = cast<SILPhiArgument>(
          block->getArgument(block->getNumArguments() - 1));
      if (!livePhis.contains(proactivePhi)) {
        proactivePhi->replaceAllUsesWithUndef();
        erasePhiArgument(block, block->getNumArguments() - 1);
        if (shouldAddLexicalLifetime(asi)) {
          erasePhiArgument(block, block->getNumArguments() - 1);
          erasePhiArgument(block, block->getNumArguments() - 1);
        }
      } else {
        phiBlocksOut.insert(block);
      }
    }
  } else {
    for (auto *block : phiBlocks)
      phiBlocksOut.insert(block);
  }
}

/// End the lexical lifetimes that were introduced for the alloc_stack if it was
/// lexical, if any.
///
/// Identify the blocks which need to end a lexical borrow scope, and then
/// insert the end into them.  Find those blocks by visiting the successors of
/// each new lexical begin_borrow's block.  End the borrow scope in the visited
/// blocks if any of the following conditions are met:
/// (1) The block is terminal--if we haven't managed to end the borrow scope
///     before the last block in the function, it must be ended there.
/// (2) Any of its successors don't have live-in values for the alloc_stack--in
///     order for a block to propagate the copied value so that it might
///     eventually be destroyed, the block must pass that value on to all its
///     successors; if any successor does not take that value, then neither it
///     nor any of its successors could possibly destroy the value.
/// (3) The block had a dealloc_stack--if the allocated space was deallocated
///     in this block, the lifetime of any value stored in it must end there.
void StackAllocationPromoter::endLexicalLifetime(BlockSetVector &phiBlocks) {
  if (!shouldAddLexicalLifetime(asi))
    return;

  SmallPtrSet<SILBasicBlock *, 4> dsiBlocks;
  for (auto *dsi : dsis)
    dsiBlocks.insert(dsi->getParent());

  DAGNodeWorklist<SILBasicBlock *, 32> worklist;
  worklist.initializeRange(lexicalBBIBlocks);
  while (auto *bb = worklist.pop()) {
    bool isFunctionExit = bb->getSuccessorBlocks().empty();
    auto successorsHaveLiveInValues = [&]() -> bool {
      return llvm::all_of(bb->getSuccessorBlocks(), [&](auto *successor) {
        return getLiveInValues(phiBlocks, successor);
      });
    };
    auto hadDeallocStack = [&]() -> bool { return dsiBlocks.contains(bb); };
    if (isFunctionExit || !successorsHaveLiveInValues() || hadDeallocStack()) {
      auto values = getLiveOutValues(phiBlocks, bb);
      if (!values)
        continue;
      endLexicalLifetimeInBlock(asi, bb, ctx, domInfo, values->copy,
                                values->borrow, values->stored);
    } else {
      for (auto *successor : bb->getSuccessorBlocks()) {
        worklist.insert(successor);
      }
    }
  }
}

void StackAllocationPromoter::pruneAllocStackUsage() {
  LLVM_DEBUG(llvm::dbgs() << "*** Pruning : " << *asi);
  BlockSetVector functionBlocks(asi->getFunction());

  // Insert all of the blocks that asi is live in.
  for (auto *use : asi->getUses())
    functionBlocks.insert(use->getUser()->getParent());

  // Clear AllocStack state.
  lastStoreInBlock.clear();

  for (auto block : functionBlocks)
    if (auto lastValueInstructions = promoteAllocationInBlock(block)) {
      lastStoreInBlock[block] = *lastValueInstructions;
      if (lastValueInstructions->borrow) {
        // If begin_borrow instructions were created, record the blocks to which
        // they were added.  In order to end these scopes, we need to have a
        // list of the blocks in which they are begun.
        lexicalBBIBlocks.push_back(lastValueInstructions->borrow->getParent());
      }
    }

  LLVM_DEBUG(llvm::dbgs() << "*** Finished pruning : " << *asi);
}

void StackAllocationPromoter::promoteAllocationToPhi() {
  LLVM_DEBUG(llvm::dbgs() << "*** Placing Phis for : " << *asi);

  // A list of blocks that will require new Phi values.
  BlockSetVector phiBlocks(asi->getFunction());

  // The "piggy-bank" data-structure that we use for processing the dom-tree
  // bottom-up.
  NodePriorityQueue priorityQueue;

  // Collect all of the stores into the AllocStack. We know that at this point
  // we have at most one store per block.
  for (auto *use : asi->getUses()) {
    SILInstruction *user = use->getUser();
    // We need to place Phis for this block.
    if (isa<StoreInst>(user)) {
      // If the block is in the dom tree (dominated by the entry block).
      if (auto *node = domInfo->getNode(user->getParent()))
        priorityQueue.push(std::make_pair(node, domTreeLevels[node]));
    }
  }

  LLVM_DEBUG(llvm::dbgs() << "*** Found: " << priorityQueue.size()
                          << " Defs\n");

  // A list of nodes for which we already calculated the dominator frontier.
  llvm::SmallPtrSet<DomTreeNode *, 32> visited;

  SmallVector<DomTreeNode *, 32> worklist;

  // Scan all of the definitions in the function bottom-up using the priority
  // queue.
  while (!priorityQueue.empty()) {
    DomTreeNodePair rootPair = priorityQueue.top();
    priorityQueue.pop();
    DomTreeNode *root = rootPair.first;
    unsigned rootLevel = rootPair.second;

    // Walk all dom tree children of Root, inspecting their successors. Only
    // J-edges, whose target level is at most Root's level are added to the
    // dominance frontier.
    worklist.clear();
    worklist.push_back(root);

    while (!worklist.empty()) {
      DomTreeNode *node = worklist.pop_back_val();
      SILBasicBlock *nodeBlock = node->getBlock();

      // For all successors of the node:
      for (auto &nodeBlockSuccs : nodeBlock->getSuccessors()) {
        auto *successorNode = domInfo->getNode(nodeBlockSuccs);

        // Skip D-edges (edges that are dom-tree edges).
        if (successorNode->getIDom() == node)
          continue;

        // Ignore J-edges that point to nodes that are not smaller or equal
        // to the root level.
        unsigned succLevel = domTreeLevels[successorNode];
        if (succLevel > rootLevel)
          continue;

        // Ignore visited nodes.
        if (!visited.insert(successorNode).second)
          continue;

        // If the new PHInode is not dominated by the allocation then it's dead.
        if (!domInfo->dominates(asi->getParent(), successorNode->getBlock()))
          continue;

        // If the new PHInode is properly dominated by the deallocation then it
        // is obviously a dead PHInode, so we don't need to insert it.
        if (dsi && domInfo->properlyDominates(dsi->getParent(),
                                              successorNode->getBlock()))
          continue;

        // The successor node is a new PHINode. If this is a new PHI node
        // then it may require additional definitions, so add it to the PQ.
        if (phiBlocks.insert(nodeBlockSuccs))
          priorityQueue.push(std::make_pair(successorNode, succLevel));
      }

      // Add the children in the dom-tree to the worklist.
      for (auto *child : node->children())
        if (!visited.count(child))
          worklist.push_back(child);
    }
  }

  // At this point we calculated the locations of all of the new Phi values.
  // Next, add the Phi values and promote all of the loads and stores into the
  // new locations.

  // Replace the dummy values with new block arguments.
  addBlockArguments(phiBlocks);

  // The blocks which still have new phis after fixBranchesAndUses runs.  These
  // are not necessarily the same as phiBlocks because fixBranchesAndUses
  // removes superfluous proactive phis.
  BlockSetVector livePhiBlocks(asi->getFunction());
  // Hook up the Phi nodes, loads, and debug_value_addr with incoming values.
  fixBranchesAndUses(phiBlocks, livePhiBlocks);

  endLexicalLifetime(livePhiBlocks);

  LLVM_DEBUG(llvm::dbgs() << "*** Finished placing Phis ***\n");
}

void StackAllocationPromoter::run() {
  // Reduce the number of load/stores in the function to minimum.
  // After this phase we are left with up to one load and store
  // per block and the last store is recorded.
  pruneAllocStackUsage();

  // Replace AllocStacks with Phi-nodes.
  promoteAllocationToPhi();
}

//===----------------------------------------------------------------------===//
//                      General Memory To Registers Impl
//===----------------------------------------------------------------------===//

namespace {

/// Promote memory to registers
class MemoryToRegisters {
  /// Lazily initialized map from DomTreeNode to DomTreeLevel.
  ///
  /// DomTreeLevelMap is a DenseMap implying that if we initialize it, we always
  /// will initialize a heap object with 64 objects. Thus by using an optional,
  /// computing this lazily, we only do this if we actually need to do so.
  Optional<DomTreeLevelMap> domTreeLevels;

  /// The function that we are optimizing.
  SILFunction &f;

  /// Dominators.
  DominanceInfo *domInfo;

  /// The builder context used when creating new instructions during register
  /// promotion.
  SILBuilderContext ctx;

  InstructionDeleter deleter;

  /// Returns the dom tree levels for the current function. Computes these
  /// lazily.
  DomTreeLevelMap &getDomTreeLevels() {
    // If we already computed our levels, just return it.
    if (auto &levels = domTreeLevels) {
      return *levels;
    }

    // Otherwise, emplace the map and compute it.
    domTreeLevels.emplace();
    auto &levels = *domTreeLevels;
    SmallVector<DomTreeNode *, 32> worklist;
    DomTreeNode *rootNode = domInfo->getRootNode();
    levels[rootNode] = 0;
    worklist.push_back(rootNode);
    while (!worklist.empty()) {
      DomTreeNode *domNode = worklist.pop_back_val();
      unsigned childLevel = levels[domNode] + 1;
      for (auto *childNode : domNode->children()) {
        levels[childNode] = childLevel;
        worklist.push_back(childNode);
      }
    }
    return *domTreeLevels;
  }

  /// Check if the AllocStackInst \p ASI is only written into.
  bool isWriteOnlyAllocation(AllocStackInst *asi);

  /// Promote all of the AllocStacks in a single basic block in one
  /// linear scan. Note: This function deletes all of the users of the
  /// AllocStackInst, including the DeallocStackInst but it does not remove the
  /// AllocStackInst itself!
  void removeSingleBlockAllocation(AllocStackInst *asi);

  /// Attempt to promote the specified stack allocation, returning true if so
  /// or false if not.  On success, all uses of the AllocStackInst have been
  /// removed, but the ASI itself is still in the program.
  bool promoteSingleAllocation(AllocStackInst *asi);

public:
  /// C'tor
  MemoryToRegisters(SILFunction &inputFunc, DominanceInfo *inputDomInfo)
      : f(inputFunc), domInfo(inputDomInfo), ctx(inputFunc.getModule()) {}

  /// Promote memory to registers. Return True on change.
  bool run();
};

} // end anonymous namespace

/// Returns true if \p I is an address of a LoadInst, skipping struct and
/// tuple address projections. Sets \p singleBlock to null if the load (or
/// it's address is not in \p singleBlock.
/// This function looks for these patterns:
/// 1. (load %ASI)
/// 2. (load (struct_element_addr/tuple_element_addr/unchecked_addr_cast %ASI))
static bool isAddressForLoad(SILInstruction *load, SILBasicBlock *&singleBlock,
                             bool &hasGuaranteedOwnership) {

  if (isa<LoadInst>(load)) {
    // SILMem2Reg is disabled when we find:
    // (load [take] (struct_element_addr/tuple_element_addr %ASI))
    // struct_element_addr and tuple_element_addr are lowered into
    // struct_extract and tuple_extract and these SIL instructions have a
    // guaranteed ownership. For replacing load's users, we need an owned value.
    // We will need a new copy and destroy of the running val placed after the
    // last use. This is not implemented currently.
    if (hasGuaranteedOwnership &&
        cast<LoadInst>(load)->getOwnershipQualifier() ==
            LoadOwnershipQualifier::Take) {
      return false;
    }
    return true;
  }

  if (!isa<UncheckedAddrCastInst>(load) && !isa<StructElementAddrInst>(load) &&
      !isa<TupleElementAddrInst>(load))
    return false;

  if (isa<StructElementAddrInst>(load) || isa<TupleElementAddrInst>(load)) {
    hasGuaranteedOwnership = true;
  }

  // Recursively search for other (non-)loads in the instruction's uses.
  for (auto *use : cast<SingleValueInstruction>(load)->getUses()) {
    SILInstruction *user = use->getUser();
    if (user->getParent() != singleBlock)
      singleBlock = nullptr;

    if (!isAddressForLoad(user, singleBlock, hasGuaranteedOwnership))
      return false;
  }
  return true;
}

/// Returns true if \p I is a dead struct_element_addr or tuple_element_addr.
static bool isDeadAddrProjection(SILInstruction *inst) {
  if (!isa<UncheckedAddrCastInst>(inst) && !isa<StructElementAddrInst>(inst) &&
      !isa<TupleElementAddrInst>(inst))
    return false;

  // Recursively search for uses which are dead themselves.
  for (auto UI : cast<SingleValueInstruction>(inst)->getUses()) {
    SILInstruction *II = UI->getUser();
    if (!isDeadAddrProjection(II))
      return false;
  }
  return true;
}

/// Returns true if this AllocStacks is captured.
/// Sets \p inSingleBlock to true if all uses of \p ASI are in a single block.
static bool isCaptured(AllocStackInst *asi, bool &inSingleBlock) {
  SILBasicBlock *singleBlock = asi->getParent();

  // For all users of the AllocStack instruction.
  for (auto *use : asi->getUses()) {
    SILInstruction *user = use->getUser();

    if (user->getParent() != singleBlock)
      singleBlock = nullptr;

    // Loads are okay.
    bool hasGuaranteedOwnership = false;
    if (isAddressForLoad(user, singleBlock, hasGuaranteedOwnership))
      continue;

    // We can store into an AllocStack (but not the pointer).
    if (auto *si = dyn_cast<StoreInst>(user))
      if (si->getDest() == asi)
        continue;

    // Deallocation is also okay, as are DebugValue w/ address value. We will
    // promote the latter into normal DebugValue.
    if (isa<DeallocStackInst>(user) || DebugValueInst::hasAddrVal(user))
      continue;

    // Destroys of loadable types can be rewritten as releases, so
    // they are fine.
    if (auto *dai = dyn_cast<DestroyAddrInst>(user))
      if (dai->getOperand()->getType().isLoadable(*dai->getFunction()))
        continue;

    // Other instructions are assumed to capture the AllocStack.
    LLVM_DEBUG(llvm::dbgs() << "*** AllocStack is captured by: " << *user);
    return true;
  }

  // None of the users capture the AllocStack.
  inSingleBlock = (singleBlock != nullptr);
  return false;
}

/// Returns true if the AllocStack is only stored into.
bool MemoryToRegisters::isWriteOnlyAllocation(AllocStackInst *asi) {
  // For all users of the AllocStack:
  for (auto *use : asi->getUses()) {
    SILInstruction *user = use->getUser();

    // It is okay to store into this AllocStack.
    if (auto *si = dyn_cast<StoreInst>(user))
      if (!isa<AllocStackInst>(si->getSrc()))
        continue;

    // Deallocation is also okay.
    if (isa<DeallocStackInst>(user))
      continue;

    // If we haven't already promoted the AllocStack, we may see
    // DebugValue uses.
    if (DebugValueInst::hasAddrVal(user))
      continue;

    if (isDeadAddrProjection(user))
      continue;

    // Can't do anything else with it.
    LLVM_DEBUG(llvm::dbgs() << "*** AllocStack has non-write use: " << *user);
    return false;
  }

  return true;
}

void MemoryToRegisters::removeSingleBlockAllocation(AllocStackInst *asi) {
  LLVM_DEBUG(llvm::dbgs() << "*** Promoting in-block: " << *asi);

  SILBasicBlock *parentBlock = asi->getParent();

  // The default value of the AllocStack is NULL because we don't have
  // uninitialized variables in Swift.
  Optional<LiveValues> runningVals;

  // For all instructions in the block.
  for (auto bbi = parentBlock->begin(), bbe = parentBlock->end(); bbi != bbe;) {
    SILInstruction *inst = &*bbi;
    ++bbi;

    // Remove instructions that we are loading from. Replace the loaded value
    // with our running value.
    if (isLoadFromStack(inst, asi)) {
      if (!runningVals) {
        // Loading without a previous store is only acceptable if the type is
        // Void (= empty tuple) or a tuple of Voids.
        runningVals =
            LiveValues::toReplace(asi, /*replacement=*/createValueForEmptyTuple(
                                      asi->getElementType(), inst, ctx));
      }
      replaceLoad(cast<LoadInst>(inst), runningVals->replacement(asi), asi, ctx,
                  deleter);
      ++NumInstRemoved;
      continue;
    }

    // Remove stores and record the value that we are saving as the running
    // value.
    if (auto *si = dyn_cast<StoreInst>(inst)) {
      if (si->getDest() == asi) {
        if (si->getOwnershipQualifier() == StoreOwnershipQualifier::Assign) {
          assert(runningVals);
          SILBuilderWithScope(si, ctx).createDestroyValue(
              si->getLoc(), runningVals->replacement(asi));
        }
        auto oldRunningVals = runningVals;
        runningVals = LiveValues::toReplace(asi, /*replacement=*/si->getSrc());
        if (shouldAddLexicalLifetime(asi)) {
          if (oldRunningVals) {
            endLexicalLifetimeInBlock(
                asi, si->getParent(), ctx, domInfo, oldRunningVals->copy,
                oldRunningVals->borrow, oldRunningVals->stored);
          }
          runningVals = beginLexicalLifetimeAtStore(asi, si, ctx).second;
        }
        deleter.forceDelete(inst);
        ++NumInstRemoved;
        continue;
      }
    }

    // Replace debug_value w/ address value with debug_value of
    // the promoted value.
    if (auto *dvi = DebugValueInst::hasAddrVal(inst)) {
      if (dvi->getOperand() == asi) {
        if (runningVals) {
          promoteDebugValueAddr(dvi, runningVals->replacement(asi), ctx,
                                deleter);
        } else {
          // Drop debug_value of uninitialized void values.
          assert(asi->getElementType().isVoid() &&
                 "Expected initialization of non-void type!");
          deleter.forceDelete(dvi);
        }
      }
      continue;
    }

    // Replace destroys with a release of the value.
    if (auto *dai = dyn_cast<DestroyAddrInst>(inst)) {
      if (dai->getOperand() == asi) {
        replaceDestroy(dai, runningVals->replacement(asi), ctx, deleter);
      }
      continue;
    }

    // Remove deallocation.
    if (auto *dsi = dyn_cast<DeallocStackInst>(inst)) {
      if (dsi->getOperand() == asi) {
        deleter.forceDelete(inst);
        NumInstRemoved++;
        // No need to continue scanning after deallocation.
        break;
      }
    }

    // Remove dead address instructions that may be uses of the allocation.
    auto *addrInst = dyn_cast<SingleValueInstruction>(inst);
    while (addrInst && addrInst->use_empty() &&
           (isa<StructElementAddrInst>(addrInst) ||
            isa<TupleElementAddrInst>(addrInst) ||
            isa<UncheckedAddrCastInst>(addrInst))) {
      SILValue op = addrInst->getOperand(0);
      deleter.forceDelete(addrInst);
      ++NumInstRemoved;
      addrInst = dyn_cast<SingleValueInstruction>(op);
    }
  }

  if (runningVals && shouldAddLexicalLifetime(asi)) {
    endLexicalLifetimeInBlock(asi, asi->getParent(), ctx, domInfo,
                              runningVals->copy, runningVals->borrow,
                              runningVals->stored);
  }
}

/// Attempt to promote the specified stack allocation, returning true if so
/// or false if not.  On success, this returns true and usually drops all of the
/// uses of the AllocStackInst, but never deletes the ASI itself.  Callers
/// should check to see if the ASI is dead after this and remove it if so.
bool MemoryToRegisters::promoteSingleAllocation(AllocStackInst *alloc) {
  LLVM_DEBUG(llvm::dbgs() << "*** Memory to register looking at: " << *alloc);
  ++NumAllocStackFound;

  // In OSSA, don't do Mem2Reg on non-trivial alloc_stack with dynamic_lifetime.
  if (alloc->hasDynamicLifetime() && f.hasOwnership() &&
      !alloc->getType().isTrivial(f)) {
    return false;
  }

  // Don't handle captured AllocStacks.
  bool inSingleBlock = false;
  if (isCaptured(alloc, inSingleBlock)) {
    ++NumAllocStackCaptured;
    return false;
  }

  // Remove write-only AllocStacks.
  if (isWriteOnlyAllocation(alloc)) {
    deleter.forceDeleteWithUsers(alloc);

    LLVM_DEBUG(llvm::dbgs() << "*** Deleting store-only AllocStack: "<< *alloc);
    return true;
  }

  // For AllocStacks that are only used within a single basic blocks, use
  // the linear sweep to remove the AllocStack.
  if (inSingleBlock) {
    removeSingleBlockAllocation(alloc);

    LLVM_DEBUG(llvm::dbgs() << "*** Deleting single block AllocStackInst: "
                            << *alloc);
    if (alloc->use_empty()) {
      deleter.forceDelete(alloc);
    } else {
      // Handle a corner case where the ASI still has uses:
      // This can come up if the source contains a withUnsafePointer where
      // the pointer escapes. It's illegal code but we should not crash.
      // Re-insert a dealloc_stack so that the verifier is happy.
      auto *next = alloc->getNextInstruction();
      SILBuilderWithScope b(next, ctx);
      b.createDeallocStack(next->getLoc(), alloc);
    }
    return true;
  }

  LLVM_DEBUG(llvm::dbgs() << "*** Need to insert BB arguments for " << *alloc);

  // Promote this allocation, lazily computing dom tree levels for this function
  // if we have not done so yet.
  auto &domTreeLevels = getDomTreeLevels();
  StackAllocationPromoter(alloc, domInfo, domTreeLevels, ctx, deleter).run();

  // Make sure that all of the allocations were promoted into registers.
  assert(isWriteOnlyAllocation(alloc) && "Non-write uses left behind");
  // ... and erase the allocation.
  deleter.forceDeleteWithUsers(alloc);
  return true;
}

bool MemoryToRegisters::run() {
  bool madeChange = false;

  if (f.getModule().getOptions().VerifyAll)
    f.verifyCriticalEdges();

  for (auto &block : f) {
    for (SILInstruction *inst : deleter.updatingReverseRange(&block)) {
      auto *asi = dyn_cast<AllocStackInst>(inst);
      if (!asi)
        continue;

      if (promoteSingleAllocation(asi)) {
        ++NumInstRemoved;
        madeChange = true;
      }
    }
  }
  return madeChange;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class SILMem2Reg : public SILFunctionTransform {
  void run() override {
    SILFunction *f = getFunction();

    LLVM_DEBUG(llvm::dbgs()
               << "** Mem2Reg on function: " << f->getName() << " **\n");

    auto *da = PM->getAnalysis<DominanceAnalysis>();

    bool madeChange = MemoryToRegisters(*f, da->get(f)).run();
    if (madeChange)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
};

} // end anonymous namespace

SILTransform *swift::createMem2Reg() {
  return new SILMem2Reg();
}
