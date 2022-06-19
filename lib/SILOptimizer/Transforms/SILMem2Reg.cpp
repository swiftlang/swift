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
#include "swift/Basic/GraphNodeWorklist.h"
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
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
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

/// A transient structure containing the values that are accessible in some
/// context: coming into a block, going out of the block, or within a block
/// (during promoteAllocationInBlock and removeSingleBlockAllocation).
///
/// At block boundaries, these are phi arguments or initializationPoints.  As we
/// iterate over a block, a way to keep track of the current (running) value
/// within a block.
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

/// A transient structure used only by promoteAllocationInBlock and
/// removeSingleBlockAllocation.
///
/// A pair of a CFG-position-relative value T and a boolean indicating whether
/// the alloc_stack's storage is valid at the position where that value exists.
template <typename T>
struct StorageStateTracking {
  /// The value which exists at some CFG position.
  T value;
  /// Whether the stack storage is initialized at that position.
  bool isStorageValid;
};

} // anonymous namespace

//===----------------------------------------------------------------------===//
//                                 Utilities
//===----------------------------------------------------------------------===//

/// Make the specified instruction cease to be a user of its operands and add it
/// to the list of instructions to delete.
///
/// This both (1) removes the specified instruction from the list of users of
/// its operands, avoiding disrupting logic that examines those users and (2)
/// keeps the specified instruction in place, allowing it to be used for
/// insertion until instructionsToDelete is culled.
static void
prepareForDeletion(SILInstruction *inst,
                   SmallVectorImpl<SILInstruction *> &instructionsToDelete) {
  for (auto &operand : inst->getAllOperands()) {
    operand.set(SILUndef::get(operand.get()->getType(), *inst->getFunction()));
  }
  instructionsToDelete.push_back(inst);
}

static void
replaceDestroy(DestroyAddrInst *dai, SILValue newValue, SILBuilderContext &ctx,
               InstructionDeleter &deleter,
               SmallVectorImpl<SILInstruction *> &instructionsToDelete) {
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

  prepareForDeletion(dai, instructionsToDelete);
}

/// Promote a DebugValue w/ address value to a DebugValue of non-address value.
static void promoteDebugValueAddr(DebugValueInst *dvai, SILValue value,
                                  SILBuilderContext &ctx,
                                  InstructionDeleter &deleter) {
  assert(dvai->getOperand()->getType().isLoadable(*dvai->getFunction()) &&
         "Unexpected promotion of address-only type!");
  assert(value && "Expected valid value");

  // Avoid inserting the same debug_value twice.
  //
  // We remove the di expression when comparing since:
  //
  // 1. dvai is on will always have the deref diexpr since it is on addresses.
  //
  // 2. We are only trying to delete debug_var that are on values... values will
  //    never have an op_deref meaning that the comparison will always fail and
  //    not serve out purpose here.
  auto dvaiWithoutDIExpr = dvai->getVarInfo()->withoutDIExpr();
  for (auto *use : value->getUses()) {
    if (auto *dvi = dyn_cast<DebugValueInst>(use->getUser())) {
      if (!dvi->hasAddrVal() && *dvi->getVarInfo() == dvaiWithoutDIExpr) {
        deleter.forceDelete(dvai);
        return;
      }
    }
  }

  // Drop op_deref if dvai is actually a debug_value instruction
  auto varInfo = *dvai->getVarInfo();
  if (isa<DebugValueInst>(dvai)) {
    auto &diExpr = varInfo.DIExpr;
    if (diExpr)
      diExpr.eraseElement(diExpr.element_begin());
  }

  SILBuilderWithScope b(dvai, ctx);
  b.createDebugValue(dvai->getLoc(), value, std::move(varInfo));
  deleter.forceDelete(dvai);
}

/// Returns true if \p I is a load which loads from \p ASI.
static bool isLoadFromStack(SILInstruction *i, AllocStackInst *asi) {
  if (!isa<LoadInst>(i) && !isa<LoadBorrowInst>(i))
    return false;

  if (auto *lbi = dyn_cast<LoadBorrowInst>(i)) {
    if (BorrowedValue(lbi).hasReborrow())
      return false;
  }

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
                         SmallVectorImpl<SILInstruction *> &foundLoads) {
  if (isa<LoadInst>(i) || isa<LoadBorrowInst>(i)) {
    foundLoads.push_back(i);
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

static void
replaceLoad(SILInstruction *inst, SILValue newValue, AllocStackInst *asi,
            SILBuilderContext &ctx, InstructionDeleter &deleter,
            SmallVectorImpl<SILInstruction *> &instructionsToDelete) {
  assert(isa<LoadInst>(inst) || isa<LoadBorrowInst>(inst));
  ProjectionPath projections(newValue->getType());
  SILValue op = inst->getOperand(0);
  SILBuilderWithScope builder(inst, ctx);
  SILOptScope scope;

  while (op != asi) {
    assert(isa<UncheckedAddrCastInst>(op) || isa<StructElementAddrInst>(op) ||
           isa<TupleElementAddrInst>(op) &&
               "found instruction that should have been skipped in "
               "isLoadFromStack");
    auto *projInst = cast<SingleValueInstruction>(op);
    projections.push_back(Projection(projInst));
    op = projInst->getOperand(0);
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
      if (auto opVal = scope.borrowValue(inst, newValue)) {
        assert(*opVal != newValue &&
               "Valid value should be different from input value");
        newValue = *opVal;
      }
    }
    newValue =
        proj.createObjectProjection(builder, inst->getLoc(), newValue).get();
  }

  op = inst->getOperand(0);

  if (auto *lbi = dyn_cast<LoadBorrowInst>(inst)) {
    if (shouldAddLexicalLifetime(asi)) {
      assert(isa<BeginBorrowInst>(newValue));
      SmallVector<SILInstruction *, 4> endBorrows;
      for (auto *ebi : lbi->getUsersOfType<EndBorrowInst>()) {
        endBorrows.push_back(ebi);
      }
      for (auto *ebi : endBorrows) {
        ebi->eraseFromParent();
      }
      lbi->replaceAllUsesWith(newValue);
    }
    else {
      auto *borrow = SILBuilderWithScope(lbi, ctx).createBeginBorrow(
          lbi->getLoc(), newValue, asi->isLexical());
      lbi->replaceAllUsesWith(borrow);
    }
  } else {
    auto *li = cast<LoadInst>(inst);
    // Replace users of the loaded value with `newValue`
    // If we have a load [copy], replace the users with copy_value of `newValue`
    if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Copy) {
      li->replaceAllUsesWith(builder.createCopyValue(li->getLoc(), newValue));
    } else {
      li->replaceAllUsesWith(newValue);
    }
  }

  // Pop the scope so that we emit cleanups.
  std::move(scope).popAtEndOfScope(&*builder.getInsertionPoint());

  // Delete the load
  prepareForDeletion(inst, instructionsToDelete);

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
                 .SILOpts.LexicalLifetimes == LexicalLifetimesOption::On &&
         asi->isLexical() &&
         !asi->getElementType().isTrivial(*asi->getFunction());
}

/// Begin a lexical borrow scope for the value stored into the provided
/// StoreInst after that instruction.
///
/// The beginning of the scope looks like
///
///     %lifetime = begin_borrow %original
///     %copy = copy_value %lifetime
static StorageStateTracking<LiveValues>
beginLexicalLifetimeAfterStore(AllocStackInst *asi, StoreInst *si) {
  assert(shouldAddLexicalLifetime(asi));
  BeginBorrowInst *bbi = nullptr;
  CopyValueInst *cvi = nullptr;
  SILBuilderWithScope::insertAfter(si, [&](SILBuilder &builder) {
    SILLocation loc = RegularLocation::getAutoGeneratedLocation(si->getLoc());
    bbi = builder.createBeginBorrow(loc, si->getSrc(),
                                    /*isLexical*/ true);
    cvi = builder.createCopyValue(loc, bbi);
  });
  StorageStateTracking<LiveValues> vals = {{si->getSrc(), bbi, cvi},
                                           /*isStorageValid=*/true};
  return vals;
}

/// End the lexical borrow scope described by the provided LiveValues struct
/// before the specified instruction.
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
static void endLexicalLifetimeBeforeInst(AllocStackInst *asi,
                                         SILInstruction *beforeInstruction,
                                         SILBuilderContext &ctx,
                                         LiveValues values) {
  assert(shouldAddLexicalLifetime(asi));
  assert(beforeInstruction);

  auto builder = SILBuilderWithScope(beforeInstruction, ctx);
  SILLocation loc =
      RegularLocation::getAutoGeneratedLocation(beforeInstruction->getLoc());
  builder.createEndBorrow(loc, values.borrow);
  builder.emitDestroyValueOperation(loc, values.stored);
}

//===----------------------------------------------------------------------===//
//                     Single Stack Allocation Promotion
//===----------------------------------------------------------------------===//

namespace {

/// Promotes a single AllocStackInst into registers..
class StackAllocationPromoter {
  using BlockSetVector = BasicBlockSetVector;
  template <typename Inst = SILInstruction>
  using BlockToInstMap = llvm::DenseMap<SILBasicBlock *, Inst *>;

  // Use a priority queue keyed on dominator tree level so that inserted nodes
  // are handled from the bottom of the dom tree upwards.
  using DomTreeNodePair = std::pair<DomTreeNode *, unsigned>;
  using NodePriorityQueue =
      std::priority_queue<DomTreeNodePair, SmallVector<DomTreeNodePair, 32>,
                          llvm::less_second>;

  /// The AllocStackInst that we are handling.
  AllocStackInst *asi;

  /// The unique deallocation instruction. This value could be NULL if there are
  /// multiple deallocations.
  DeallocStackInst *dsi;

  /// Dominator info.
  DominanceInfo *domInfo;

  /// Map from dominator tree node to tree level.
  DomTreeLevelMap &domTreeLevels;

  /// The SIL builder used when creating new instructions during register
  /// promotion.
  SILBuilderContext &ctx;

  InstructionDeleter &deleter;

  /// Instructions that could not be deleted immediately with forceDelete until
  /// StackAllocationPromoter finishes its run.
  ///
  /// There are two reasons why an instruction might not be deleted:
  /// (1) new instructions are inserted before or after it
  /// (2) it ensures that an instruction remains used, preventing it from being
  ///     deleted
  SmallVectorImpl<SILInstruction *> &instructionsToDelete;

  /// The last instruction in each block that initializes the storage that is
  /// not succeeded by an instruction that deinitializes it.
  ///
  /// The live-out values for every block can be derived from these.
  ///
  /// For non-lexical alloc_stacks, that is just a StoreInst.  For lexical
  /// alloc_stacks, that is the StoreInst, a BeginBorrowInst of the
  /// value stored into the StoreInst and a CopyValueInst of the result of the
  /// BeginBorrowInst.
  BlockToInstMap<StoreInst> initializationPoints;

  /// The first instruction in each block that deinitializes the storage that is
  /// not preceeded by an instruction that initializes it.
  ///
  /// That includes:
  ///     store
  ///     destroy_addr
  ///     load [take]
  ///
  /// Ending lexical lifetimes before these instructions is one way that the
  /// cross-block lexical lifetimes of initializationPoints can be ended in
  /// StackAllocationPromoter::endLexicalLifetime.
  BlockToInstMap<> deinitializationPoints;

public:
  /// C'tor.
  StackAllocationPromoter(
      AllocStackInst *inputASI, DominanceInfo *inputDomInfo,
      DomTreeLevelMap &inputDomTreeLevels, SILBuilderContext &inputCtx,
      InstructionDeleter &deleter,
      SmallVectorImpl<SILInstruction *> &instructionsToDelete)
      : asi(inputASI), dsi(nullptr), domInfo(inputDomInfo),
        domTreeLevels(inputDomTreeLevels), ctx(inputCtx), deleter(deleter),
        instructionsToDelete(instructionsToDelete) {
    // Scan the users in search of a deallocation instruction.
    for (auto *use : asi->getUses()) {
      if (auto *foundDealloc = dyn_cast<DeallocStackInst>(use->getUser())) {
        // Don't record multiple dealloc instructions.
        if (dsi) {
          dsi = nullptr;
          break;
        }
        // Record the deallocation instruction.
        dsi = foundDealloc;
      }
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
  /// \returns the last StoreInst found, whose storage was not subsequently
  ///          deinitialized
  StoreInst *promoteAllocationInBlock(SILBasicBlock *block);
};

} // end of namespace

StoreInst *StackAllocationPromoter::promoteAllocationInBlock(
    SILBasicBlock *blockPromotingWithin) {
  LLVM_DEBUG(llvm::dbgs() << "*** Promoting ASI in block: " << *asi);

  // RunningVal is the current value in the stack location.
  // We don't know the value of the alloca until we find the first store.
  Optional<StorageStateTracking<LiveValues>> runningVals;
  // Keep track of the last StoreInst that we found and the BeginBorrowInst and
  // CopyValueInst that we created in response if the alloc_stack was lexical.
  Optional<StorageStateTracking<StoreInst *>> lastStoreInst;

  /// Returns true if we have enough information to end the lifetime during
  /// promoteAllocationInBlock.
  ///
  /// The lifetime cannot be ended during this function's execution if we don't
  /// yet have enough information to end it.  That occurs when the running
  /// values are from a load which was not preceeded by a store.  In that case,
  /// the lifetime end will be added later, when we have enough information,
  /// namely the live in values, to end it.
  auto canEndLexicalLifetime =
      [](StorageStateTracking<LiveValues> values) -> bool {
    return values.value.borrow;
  };

  // For all instructions in the block.
  for (auto bbi = blockPromotingWithin->begin(),
            bbe = blockPromotingWithin->end();
       bbi != bbe;) {
    SILInstruction *inst = &*bbi;
    ++bbi;

    if (isLoadFromStack(inst, asi)) {
      assert(!runningVals || runningVals->isStorageValid);
      if (auto *lbi = dyn_cast<LoadBorrowInst>(inst)) {
        if (runningVals) {
          if (shouldAddLexicalLifetime(asi)) {
            replaceLoad(lbi, runningVals->value.borrow, asi, ctx,
                        deleter, instructionsToDelete);
          }
          else {
            replaceLoad(lbi, runningVals->value.replacement(asi), asi, ctx,
                        deleter, instructionsToDelete);
          }
          ++NumInstRemoved;
        }
        continue;
      }
      auto *li = cast<LoadInst>(inst);
      if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Take) {
        if (shouldAddLexicalLifetime(asi)) {
          // End the lexical lifetime at a load [take].  The storage is no
          // longer keeping the value alive.
          if (runningVals && canEndLexicalLifetime(*runningVals)) {
            // End it right now if we have enough information.
            endLexicalLifetimeBeforeInst(asi, /*beforeInstruction=*/li, ctx,
                                         runningVals->value);
          } else {
            // If we dont't have enough information, end it endLexicalLifetime.
            assert(!deinitializationPoints[blockPromotingWithin]);
            deinitializationPoints[blockPromotingWithin] = li;
          }
        }
        if (runningVals)
          runningVals->isStorageValid = false;
        if (lastStoreInst)
          lastStoreInst->isStorageValid = false;
      }

      if (runningVals) {
        // If we are loading from the AllocStackInst and we already know the
        // content of the Alloca then use it.
        LLVM_DEBUG(llvm::dbgs() << "*** Promoting load: " << *li);
        replaceLoad(inst, runningVals->value.replacement(asi), asi, ctx,
                    deleter, instructionsToDelete);
        ++NumInstRemoved;
      } else if (li->getOperand() == asi &&
                 li->getOwnershipQualifier() != LoadOwnershipQualifier::Copy) {
        // If we don't know the content of the AllocStack then the loaded
        // value *is* the new value;
        // Don't use result of load [copy] as a RunningVal, it necessitates
        // additional logic for cleanup of consuming instructions of the result.
        // StackAllocationPromoter::fixBranchesAndUses will later handle it.
        LLVM_DEBUG(llvm::dbgs() << "*** First load: " << *li);
        runningVals = {LiveValues::toReplace(asi, /*replacement=*/li),
                       /*isStorageValid=*/true};
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
          assert(runningVals->isStorageValid);
          SILBuilderWithScope(si, ctx).createDestroyValue(
              si->getLoc(), runningVals->value.replacement(asi));
        } else {
          SILBuilderWithScope localBuilder(si, ctx);
          auto *newLoad = localBuilder.createLoad(si->getLoc(), asi,
                                                  LoadOwnershipQualifier::Take);
          localBuilder.createDestroyValue(si->getLoc(), newLoad);
          if (shouldAddLexicalLifetime(asi)) {
            assert(!deinitializationPoints[blockPromotingWithin]);
            deinitializationPoints[blockPromotingWithin] = si;
          }
        }
        si->setOwnershipQualifier(StoreOwnershipQualifier::Init);
      }

      // If we met a store before this one, delete it.
      if (lastStoreInst) {
        assert(lastStoreInst->value->getOwnershipQualifier() !=
                   StoreOwnershipQualifier::Assign &&
               "store [assign] to the stack location should have been "
               "transformed to a store [init]");
        LLVM_DEBUG(llvm::dbgs()
                   << "*** Removing redundant store: " << *lastStoreInst->value);
        ++NumInstRemoved;
        prepareForDeletion(lastStoreInst->value, instructionsToDelete);
      }

      auto oldRunningVals = runningVals;
      // The stored value is the new running value.
      runningVals = {LiveValues::toReplace(asi, /*replacement=*/si->getSrc()),
                     /*isStorageValid=*/true};
      // The current store is now the lastStoreInst (until we see
      // another).
      lastStoreInst = {si, /*isStorageValid=*/true};
      if (shouldAddLexicalLifetime(asi)) {
        if (oldRunningVals && oldRunningVals->isStorageValid &&
            canEndLexicalLifetime(*oldRunningVals)) {
          endLexicalLifetimeBeforeInst(asi, /*beforeInstruction=*/si, ctx,
                                       oldRunningVals->value);
        }
        runningVals = beginLexicalLifetimeAfterStore(asi, si);
        // Create a use of the newly created copy in order to keep phi pruning
        // from deleting our lifetime beginning instructions.
        //
        // TODO: Remove this hack, it is only necessary because erasePhiArgument
        //       calls deleteEdgeValue which calls
        //       deleteTriviallyDeadOperandsOfDeadArgument and deletes the copy
        //       and borrow that we added and want not to have deleted.
        SILBuilderWithScope::insertAfter(
            runningVals->value.copy->getDefiningInstruction(),
            [&](auto builder) {
              SILLocation loc = RegularLocation::getAutoGeneratedLocation();
              auto *useOfCopy =
                  builder.createCopyValue(loc, runningVals->value.copy);
              // Note that we don't call prepareToDelete useOfCopy because we
              // specifically want this instruction to remain a use of copy.
              instructionsToDelete.push_back(useOfCopy);
            });
      }
      continue;
    }

    // Replace debug_value w/ address value with debug_value of
    // the promoted value.
    // if we have a valid value to use at this point. Otherwise we'll
    // promote this when we deal with hooking up phis.
    if (auto *dvi = DebugValueInst::hasAddrVal(inst)) {
      if (dvi->getOperand() == asi && runningVals)
        promoteDebugValueAddr(dvi, runningVals->value.replacement(asi), ctx,
                              deleter);
      continue;
    }

    // Replace destroys with a release of the value.
    if (auto *dai = dyn_cast<DestroyAddrInst>(inst)) {
      if (dai->getOperand() == asi) {
        if (runningVals) {
          replaceDestroy(dai, runningVals->value.replacement(asi), ctx, deleter,
                         instructionsToDelete);
          if (shouldAddLexicalLifetime(asi)) {
            endLexicalLifetimeBeforeInst(asi, /*beforeInstruction=*/dai, ctx,
                                         runningVals->value);
          }
          runningVals->isStorageValid = false;
          if (lastStoreInst)
            lastStoreInst->isStorageValid = false;
        } else {
          assert(!deinitializationPoints[blockPromotingWithin]);
          deinitializationPoints[blockPromotingWithin] = dai;
        }
      }
      continue;
    }

    if (auto *dvi = dyn_cast<DestroyValueInst>(inst)) {
      if (runningVals &&
          dvi->getOperand() == runningVals->value.replacement(asi)) {
        // Reset LastStore.
        // So that we don't end up passing dead values as phi args in
        // StackAllocationPromoter::fixBranchesAndUses
        lastStoreInst = llvm::None;
      }
    }

    // Stop on deallocation.
    if (auto *dsi = dyn_cast<DeallocStackInst>(inst)) {
      if (dsi->getOperand() == asi)
        break;
    }
  }

  if (lastStoreInst && lastStoreInst->isStorageValid) {
    assert(lastStoreInst->value->getOwnershipQualifier() !=
               StoreOwnershipQualifier::Assign &&
           "store [assign] to the stack location should have been "
           "transformed to a store [init]");
    LLVM_DEBUG(llvm::dbgs() << "*** Finished promotion. Last store: "
                            << *lastStoreInst->value);
    return lastStoreInst->value;
  }

  LLVM_DEBUG(llvm::dbgs() << "*** Finished promotion with no stores.\n");
  return nullptr;
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
    BlockToInstMap<StoreInst>::iterator it =
        initializationPoints.find(domBlock);
    if (it != initializationPoints.end()) {
      auto *si = it->second;
      LLVM_DEBUG(llvm::dbgs() << "*** Found Store def " << si->getSrc());
      SILValue stored = si->getSrc();
      SILValue borrow = SILValue();
      SILValue copy = SILValue();
      if (shouldAddLexicalLifetime(asi)) {
        borrow = cast<BeginBorrowInst>(&*std::next(si->getIterator()));
        copy = cast<CopyValueInst>(borrow->getNextInstruction());
      }
      LiveValues values = {stored, borrow, copy};
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
  LLVM_DEBUG(llvm::dbgs() << "*** Fixing the terminator " << *ti << ".\n");

  LiveValues def = getEffectiveLiveOutValues(phiBlocks, predBlock);

  LLVM_DEBUG(llvm::dbgs() << "*** Found the definition: " << def.replacement(asi));

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
  SmallVector<SILValue, 4> incomingPhiVals;
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
  SmallVector<SILInstruction *, 4> collectedLoads;

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

      LLVM_DEBUG(llvm::dbgs() << "*** Replacing " << *li << " with Def "
                              << def.replacement(asi));

      // Replace the load with the definition that we found.
      replaceLoad(li, def.replacement(asi), asi, ctx, deleter,
                  instructionsToDelete);
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
      replaceDestroy(dai, def.replacement(asi), ctx, deleter,
                     instructionsToDelete);
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
    auto eraseLastPhiFromBlock = [](SILBasicBlock *block) {
      auto *phi = cast<SILPhiArgument>(
          block->getArgument(block->getNumArguments() - 1));
      phi->replaceAllUsesWithUndef();
      erasePhiArgument(block, block->getNumArguments() - 1);
    };
    for (auto *block : phiBlocks) {
      auto *proactivePhi = cast<SILPhiArgument>(
          block->getArgument(block->getNumArguments() - 1));
      if (!livePhis.contains(proactivePhi)) {
        eraseLastPhiFromBlock(block);
        if (shouldAddLexicalLifetime(asi)) {
          eraseLastPhiFromBlock(block);
          eraseLastPhiFromBlock(block);
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

/// End the lexical lifetimes that were introduced for storage to the
/// alloc_stack and have not already been ended.
///
/// Walk forward from the out-edge of each of the blocks which began but did not
/// end a borrow scope.  The scope must be ended if any of the following three
/// conditions hold:
///
/// Normally, we are relying on the invariant that the storage's
/// deinitializations must jointly postdominate its initializations.  That fact
/// allows us to simply end scopes when memory is deinitialized.  There is only
/// one simple check to do:
///
/// (1) A block deinitializes the storage before initializing it.
///
///     These blocks and the relevant instruction within them are tracked by the
///     deinitializationPoints map.
///
/// If this were all we needed to do, we could just iterate over that map.
///
/// The above invariant does not help us with unreachable terminators, however.
/// Because it is valid to have the alloc_stack be initialized when exiting a
/// function via an unreachable, we can't rely on the memory having been
/// deinitialized.  But we still need to ensure that borrow scopes are ended and
/// values are destroyed before getting to an unreachable.
///
/// (2.a) A block has as its terminator an UnreachableInst.
///
/// (2.b) A block's single successor does not have live-in values.
///
///       This can only happen if the successor is a CFG merge and all paths
///       from here lead to unreachable.
void StackAllocationPromoter::endLexicalLifetime(BlockSetVector &phiBlocks) {
  if (!shouldAddLexicalLifetime(asi))
    return;

  // We need to separately consider and visit incoming unopened borrow scopes
  // and outgoing unclosed borrow scopes.  The reason is that a walk should stop
  // on any path where it encounters an incoming unopened borrow scope but that
  // should _NOT_ count as a visit of outgoing unclosed borrow scopes.
  //
  // Without this distinction, a case like the following wouldn't be visited
  // properly:
  //
  //     bb1:
  //       %addr = alloc_stack
  //       store %value to [init] %addr
  //       br bb2
  //     bb2:
  //       %value_2 = load [take] %addr
  //       store %value_2 to [init] %addr
  //       br bb3
  //     bb3:
  //       destroy_addr %addr
  //       dealloc_stack %addr
  //       %r = tuple ()
  //       return %r
  //
  // Both bb1 and bb2 have cross-block initialization points.  Suppose that we
  // visited bb1 first.  We would see that it didn't have an incoming unopened
  // borrow scope (already, we can tell something is amiss that we're
  // considering this) and then add bb2 to the worklist--except it's already
  // there.  Next we would visit bb2.  We would see that it had an incoming
  // unopened borrow scope so we would close it.  And then we'd be done.  In
  // particular, we'd leave the scope that opens in bb2 unclosed.
  //
  // The root cause here is that it's important to stop walking when we hit a
  // scope close.  Otherwise, we could keep walking down to blocks which don't
  // have live-in or live-out values.
  //
  // Visiting the incoming and outgoing edges works as follows in the above
  // example:  The worklist is initialiized with {(bb1, ::Out), (bb2, ::Out)}.
  // When visiting (bb1, ::Out), we see that bb1 is neither unreachable nor
  // has exactly one successor without live-in values.  So we add (bb2, ::In) to
  // the worklist.  Next, we visit (bb2, ::Out).  We see that it _also_ doesn't
  // have an unreachable terminator or a unique successor without live-in
  // values, so we add (bb3, ::In).  Next, we visit (bb2, ::In).  We see that
  // it _does_ have an incoming unopened borrow scope, so we close it and stop.
  // Finally, we visit (bb3, ::Out).  We see that it too has an incoming
  // unopened borrow scope so we close it and stop.
  enum class AvailableValuesKind : uint8_t { In, Out };

  using ScopeEndPosition =
      llvm::PointerIntPair<SILBasicBlock *, 1, AvailableValuesKind>;

  GraphNodeWorklist<ScopeEndPosition, 16> worklist;
  for (auto pair : initializationPoints) {
    worklist.insert({pair.getFirst(), AvailableValuesKind::Out});
  }
  while (!worklist.empty()) {
    auto position = worklist.pop();
    auto *bb = position.getPointer();
    switch (position.getInt()) {
    case AvailableValuesKind::In: {
      if (auto *inst = deinitializationPoints[bb]) {
        auto values = getLiveInValues(phiBlocks, bb);
        endLexicalLifetimeBeforeInst(asi, /*beforeInstruction=*/inst, ctx,
                                     *values);
        continue;
      }
      worklist.insert({bb, AvailableValuesKind::Out});
      break;
    }
    case AvailableValuesKind::Out: {
      bool terminatesInUnreachable = isa<UnreachableInst>(bb->getTerminator());
      auto uniqueSuccessorLacksLiveInValues = [&]() -> bool {
        return bb->getSingleSuccessorBlock() &&
               !getLiveInValues(phiBlocks, bb->getSingleSuccessorBlock());
      };
      if (terminatesInUnreachable || uniqueSuccessorLacksLiveInValues()) {
        auto values = getLiveOutValues(phiBlocks, bb);
        endLexicalLifetimeBeforeInst(
            asi, /*beforeInstruction=*/bb->getTerminator(), ctx, *values);
        continue;
      }
      for (auto *successor : bb->getSuccessorBlocks()) {
        worklist.insert({successor, AvailableValuesKind::In});
      }
      break;
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

  for (auto block : functionBlocks)
    if (auto si = promoteAllocationInBlock(block)) {
      // There was a final storee instruction which was not followed by an
      // instruction that deinitializes the memory.  Record it as a cross-block
      // initialization point.
      initializationPoints[block] = si;
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
  SmallVector<SILInstruction *, 32> instructionsToDelete;

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
                             bool &involvesUntakableProjection) {
  if (auto *li = dyn_cast<LoadInst>(load)) {
    // SILMem2Reg is disabled when we find a load [take] of an untakable
    // projection.  See below for further discussion.
    if (involvesUntakableProjection &&
        li->getOwnershipQualifier() == LoadOwnershipQualifier::Take) {
      return false;
    }
    return true;
  }

  if (isa<LoadBorrowInst>(load)) {
    if (involvesUntakableProjection) {
      return false;
    }
    return true;
  }

  if (!isa<UncheckedAddrCastInst>(load) && !isa<StructElementAddrInst>(load) &&
      !isa<TupleElementAddrInst>(load))
    return false;

  // None of the projections are lowered to owned values:
  //
  // struct_element_addr and tuple_element_addr instructions are lowered to
  // struct_extract and tuple_extract instructions respectively.  These both
  // have guaranteed ownership (since they forward ownership and can only be
  // used on a guaranteed value).
  //
  // unchecked_addr_cast instructions are lowered to unchecked_bitwise_cast
  // instructions.  These have unowned ownership.
  //
  // So in no case can a load [take] be lowered into the new projected value
  // (some sequence of struct_extract, tuple_extract, and
  // unchecked_bitwise_cast instructions) taking over ownership of the original
  // value.  Without additional changes.
  //
  // For example, for a sequence of element_addr projections could be
  // transformed into a sequence of destructure instructions, followed by a
  // sequence of structure instructions where all the original values are
  // kept in place but the taken value is "knocked out" and replaced with
  // undef.  The running value would then be set to the newly structed
  // "knockout" value.
  //
  // Alternatively, a new copy of the running value could be created and a new
  // set of destroys placed after its last uses.
  involvesUntakableProjection = true;

  // Recursively search for other (non-)loads in the instruction's uses.
  auto *svi = cast<SingleValueInstruction>(load);
  for (auto *use : svi->getUses()) {
    SILInstruction *user = use->getUser();
    if (user->getParent() != singleBlock)
      singleBlock = nullptr;

    if (!isAddressForLoad(user, singleBlock, involvesUntakableProjection))
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
/// Sets \p inSingleBlock to true if all uses of \p asi are in a single block.
static bool isCaptured(AllocStackInst *asi, bool *inSingleBlock) {
  SILBasicBlock *singleBlock = asi->getParent();

  // For all users of the AllocStack instruction.
  for (auto *use : asi->getUses()) {
    SILInstruction *user = use->getUser();

    if (user->getParent() != singleBlock)
      singleBlock = nullptr;

    // Loads are okay.
    bool involvesUntakableProjection = false;
    if (isAddressForLoad(user, singleBlock, involvesUntakableProjection))
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
  *inSingleBlock = (singleBlock != nullptr);
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

    if (auto *sbi = dyn_cast<StoreBorrowInst>(user))
      if (!isa<AllocStackInst>(sbi->getSrc()))
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
  Optional<StorageStateTracking<LiveValues>> runningVals;

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
        runningVals = {
            LiveValues::toReplace(asi,
                                  /*replacement=*/createValueForEmptyTuple(
                                      asi->getElementType(), inst, ctx)),
            /*isStorageValid=*/true};
      }
      assert(runningVals && runningVals->isStorageValid);
      auto *loadInst = dyn_cast<LoadInst>(inst);
      if (loadInst &&
          loadInst->getOwnershipQualifier() == LoadOwnershipQualifier::Take) {
        if (shouldAddLexicalLifetime(asi)) {
          // End the lexical lifetime at a load [take].  The storage is no
          // longer keeping the value alive.
          endLexicalLifetimeBeforeInst(asi, /*beforeInstruction=*/inst, ctx,
                                       runningVals->value);
        }
        runningVals->isStorageValid = false;
      }
      replaceLoad(inst, runningVals->value.replacement(asi), asi, ctx, deleter,
                  instructionsToDelete);
      ++NumInstRemoved;
      continue;
    }

    // Remove stores and record the value that we are saving as the running
    // value.
    if (auto *si = dyn_cast<StoreInst>(inst)) {
      if (si->getDest() == asi) {
        if (si->getOwnershipQualifier() == StoreOwnershipQualifier::Assign) {
          assert(runningVals && runningVals->isStorageValid);
          SILBuilderWithScope(si, ctx).createDestroyValue(
              si->getLoc(), runningVals->value.replacement(asi));
        }
        auto oldRunningVals = runningVals;
        runningVals = {LiveValues::toReplace(asi, /*replacement=*/si->getSrc()),
                       /*isStorageValid=*/true};
        if (shouldAddLexicalLifetime(asi)) {
          if (oldRunningVals && oldRunningVals->isStorageValid) {
            endLexicalLifetimeBeforeInst(asi, /*beforeInstruction=*/si, ctx,
                                         oldRunningVals->value);
          }
          runningVals = beginLexicalLifetimeAfterStore(asi, si);
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
          promoteDebugValueAddr(dvi, runningVals->value.replacement(asi), ctx,
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
        assert(runningVals && runningVals->isStorageValid);
        replaceDestroy(dai, runningVals->value.replacement(asi), ctx, deleter,
                       instructionsToDelete);
        if (shouldAddLexicalLifetime(asi)) {
          endLexicalLifetimeBeforeInst(asi, /*beforeInstruction=*/dai, ctx,
                                       runningVals->value);
        }
        runningVals->isStorageValid = false;
      }
      continue;
    }

    // Remove deallocation.
    if (auto *dsi = dyn_cast<DeallocStackInst>(inst)) {
      if (dsi->getOperand() == asi) {
        deleter.forceDelete(dsi);
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

  if (runningVals && runningVals->isStorageValid &&
      shouldAddLexicalLifetime(asi)) {
    // There is still valid storage after visiting all instructions in this
    // block which are the only instructions involving this alloc_stack.
    // This can only happen if all paths from this block end in unreachable.
    //
    // We need to end the lexical lifetime at the last possible location, either
    // just before an unreachable instruction or just before a branch to a block
    // that is not dominated by parentBlock.

    // Walk forward from parentBlock until finding blocks which either
    // (1) terminate in unreachable
    // (2) have successors which are not dominated by parentBlock
    GraphNodeWorklist<SILBasicBlock *, 2> worklist;
    worklist.initialize(parentBlock);
    while (auto *block = worklist.pop()) {
      assert(domInfo->dominates(parentBlock, block));
      auto *terminator = block->getTerminator();
      if (isa<UnreachableInst>(terminator)) {
        endLexicalLifetimeBeforeInst(asi, /*beforeInstruction=*/terminator, ctx,
                                     runningVals->value);
        continue;
      }
      SILBasicBlock *successor = nullptr;
      // If any successor is not dominated by the parentBlock, then we must end
      // the lifetime before that successor.
      //
      // Suppose that a successor is not dominated by parentBlock.  Recall that
      // block _is_ dominated by parentBlock.  Thus that successor must have
      // more than one predecessor: block, and at least one other.  (Otherwise
      // it would be dominated by parentBlock contrary to our assumption.)
      // Recall that SIL does not allow critical edges.  Therefore block has
      // only a single successor.
      //
      // Use the above fact to only look for lack of domination of a successor
      // if that successor is the single successor of block.
      if ((successor = block->getSingleSuccessorBlock()) &&
          (!domInfo->dominates(parentBlock, successor))) {
        endLexicalLifetimeBeforeInst(asi, /*beforeInstruction=*/terminator, ctx,
                                     runningVals->value);
        continue;
      }
      for (auto *successor : block->getSuccessorBlocks()) {
        worklist.insert(successor);
      }
    }
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
  if (isCaptured(alloc, &inSingleBlock)) {
    ++NumAllocStackCaptured;
    return false;
  }

  // Remove write-only AllocStacks.
  if (isWriteOnlyAllocation(alloc) && !shouldAddLexicalLifetime(alloc)) {
    LLVM_DEBUG(llvm::dbgs() << "*** Deleting store-only AllocStack: "<< *alloc);
    deleter.forceDeleteWithUsers(alloc);
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
  } else {
    // For enums we require that all uses are in the same block.
    // Otherwise there could be a switch_enum of an optional where the none-case
    // does not have a destroy of the enum value.
    // After transforming such an alloc_stack, the value would leak in the none-
    // case block.
    if (f.hasOwnership() && alloc->getType().isOrHasEnum())
      return false;
  }

  LLVM_DEBUG(llvm::dbgs() << "*** Need to insert BB arguments for " << *alloc);

  // Promote this allocation, lazily computing dom tree levels for this function
  // if we have not done so yet.
  auto &domTreeLevels = getDomTreeLevels();
  StackAllocationPromoter(alloc, domInfo, domTreeLevels, ctx, deleter,
                          instructionsToDelete)
      .run();

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
    // Don't waste time optimizing unreachable blocks.
    if (!domInfo->isReachableFromEntry(&block)) {
      continue;
    }
    for (SILInstruction *inst : deleter.updatingReverseRange(&block)) {
      auto *asi = dyn_cast<AllocStackInst>(inst);
      if (!asi)
        continue;

      if (promoteSingleAllocation(asi)) {
        for (auto *inst : instructionsToDelete) {
          deleter.forceDelete(inst);
        }
        instructionsToDelete.clear();
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
