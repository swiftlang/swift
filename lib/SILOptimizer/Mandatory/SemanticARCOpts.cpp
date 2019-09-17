//===--- SemanticARCOpts.cpp ----------------------------------------------===//
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

#define DEBUG_TYPE "sil-semantic-arc-opts"
#include "swift/Basic/BlotSetVector.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/BranchPropagatedUser.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

STATISTIC(NumEliminatedInsts, "number of removed instructions");
STATISTIC(NumLoadCopyConvertedToLoadBorrow,
          "number of load_copy converted to load_borrow");

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

/// Return true if v only has invalidating uses that are destroy_value. Such an
/// owned value is said to represent a dead "live range".
///
/// Semantically this implies that a value is never passed off as +1 to memory
/// or another function implying it can be used everywhere at +0.
static bool isDeadLiveRange(
    SILValue v, SmallVectorImpl<DestroyValueInst *> &destroys,
    NullablePtr<SmallVectorImpl<SILInstruction *>> forwardingInsts = nullptr) {
  assert(v.getOwnershipKind() == ValueOwnershipKind::Owned);
  SmallVector<Operand *, 32> worklist(v->use_begin(), v->use_end());
  while (!worklist.empty()) {
    auto *op = worklist.pop_back_val();

    // Skip type dependent operands.
    if (op->isTypeDependent())
      continue;

    auto *user = op->getUser();

    // We know that a copy_value produces an @owned value. Look through all of
    // our uses and classify them as either invalidating or not
    // invalidating. Make sure that all of the invalidating ones are
    // destroy_value since otherwise the live_range is not complete.
    auto map = op->getOwnershipKindMap();
    auto constraint = map.getLifetimeConstraint(ValueOwnershipKind::Owned);
    switch (constraint) {
    case UseLifetimeConstraint::MustBeInvalidated: {
      // See if we have a destroy value. If we don't we have an
      // unknown consumer. Return false, we need this live range.
      if (auto *dvi = dyn_cast<DestroyValueInst>(user)) {
        destroys.push_back(dvi);
        continue;
      }

      // Otherwise, see if we have a forwarding value that has a single
      // non-trivial operand that can accept a guaranteed value. If not, we can
      // not recursively process it, so be conservative and assume that we /may
      // consume/ the value, so the live range must not be eliminated.
      //
      // DISCUSSION: For now we do not support forwarding instructions with
      // multiple non-trivial arguments since we would need to optimize all of
      // the non-trivial arguments at the same time.
      //
      // NOTE: Today we do not support TermInsts for simplicity... we /could/
      // support it though if we need to.
      if (forwardingInsts.isNull() || isa<TermInst>(user) ||
          !isGuaranteedForwardingInst(user) ||
          1 != count_if(user->getOperandValues(
                            true /*ignore type dependent operands*/),
                        [&](SILValue v) {
                          return v.getOwnershipKind() ==
                                 ValueOwnershipKind::Owned;
                        })) {
        return false;
      }

      // Ok, this is a forwarding instruction whose ownership we can flip from
      // owned -> guaranteed. Visit its users recursively to see if the the
      // users force the live range to be alive.
      forwardingInsts.get()->push_back(user);
      for (SILValue v : user->getResults()) {
        if (v.getOwnershipKind() != ValueOwnershipKind::Owned)
          continue;
        llvm::copy(v->getUses(), std::back_inserter(worklist));
      }
      continue;
    }
    case UseLifetimeConstraint::MustBeLive:
      // Ok, this constraint can take something owned as live. Assert that it
      // can also accept something that is guaranteed. Any non-consuming use of
      // an owned value should be able to take a guaranteed parameter as well
      // (modulo bugs). We assert to catch these.
      assert(map.canAcceptKind(ValueOwnershipKind::Guaranteed) &&
             "Any non-consuming use of an owned value should be able to take a "
             "guaranteed value");
      continue;
    }
  }

  // We visited all of our users and were able to prove that all of them were
  // benign. Return true.
  return true;
}

//===----------------------------------------------------------------------===//
//                               Implementation
//===----------------------------------------------------------------------===//

namespace {

/// A two stage visitor that optimizes ownership instructions and eliminates any
/// trivially dead code that results after optimization. The two stages are used
/// to avoid iterator invalidation. Specifically:
///
/// 1. We first process the CFG instruction by instruction, only eliminating
/// instructions that are guaranteed to be dominated by the visited
/// instrution. While we do that, we add the operands of any instruction that we
/// successfully optimize to the worklist. NOTE: We do not process arguments
/// here to get SSA guarantees around dominance.
///
/// 2. Once we have processed the CFG and done some initial optimization, we
/// enter phase 2 where we process the worklist. Here we are allowed to process
/// arbitrary values and instructions, removing things that we are erasing from
/// the worklist before we delete them.
struct SemanticARCOptVisitor
    : SILInstructionVisitor<SemanticARCOptVisitor, bool> {
  /// Our main worklist. We use this after an initial run through.
  SmallBlotSetVector<SILValue, 32> worklist;

  /// A secondary work list that we use to store dead trivial instructions to
  /// delete after we are done processing the worklist.
  SmallBlotSetVector<SILInstruction *, 32> deadTrivialInsts;

  SILFunction &F;
  Optional<DeadEndBlocks> TheDeadEndBlocks;

  explicit SemanticARCOptVisitor(SILFunction &F) : F(F) {}

  DeadEndBlocks &getDeadEndBlocks() {
    if (!TheDeadEndBlocks)
      TheDeadEndBlocks.emplace(&F);
    return *TheDeadEndBlocks;
  }

  /// Given a single value instruction, RAUW it with newValue, add newValue to
  /// the worklist, and then call eraseInstruction on i.
  void eraseAndRAUWSingleValueInstruction(SingleValueInstruction *i, SILValue newValue) {
    worklist.insert(newValue);
    i->replaceAllUsesWith(newValue);
    eraseInstruction(i);
  }

  /// Add all operands of i to the worklist and then call eraseInstruction on
  /// i. Assumes that the instruction doesnt have users.
  void eraseInstructionAndAddOperandsToWorklist(SILInstruction *i) {
    // Then copy all operands into the worklist for future processing.
    for (SILValue v : i->getOperandValues()) {
      worklist.insert(v);
    }
    eraseInstruction(i);
  }

  /// Remove all results of the given instruction from the worklist and then
  /// erase the instruction. Assumes that the instruction does not have any
  /// users left.
  void eraseInstruction(SILInstruction *i) {
    // Remove all SILValues of the instruction from the worklist and then erase
    // the instruction.
    for (SILValue result : i->getResults()) {
      worklist.erase(result);
    }
    deadTrivialInsts.erase(i);
    i->eraseFromParent();
  }

  /// The default visitor.
  bool visitSILInstruction(SILInstruction *i) { return false; }
  bool visitCopyValueInst(CopyValueInst *cvi);
  bool visitBeginBorrowInst(BeginBorrowInst *bbi);
  bool visitLoadInst(LoadInst *li);

  bool isWrittenTo(LoadInst *li);

  bool processWorklist();

  bool performGuaranteedCopyValueOptimization(CopyValueInst *cvi);
  bool eliminateDeadLiveRangeCopyValue(CopyValueInst *cvi);
};

} // end anonymous namespace

bool SemanticARCOptVisitor::processWorklist() {
  // NOTE: The madeChange here is not strictly necessary since we only have
  // items added to the worklist today if we have already made /some/ sort of
  // change. That being said, I think there is a low cost to including this here
  // and makes the algorithm more correct, visually and in the face of potential
  // refactoring.
  bool madeChange = false;

  while (!worklist.empty()) {
    // Pop the last element off the list. If we were returned None, we blotted
    // this element, so skip it.
    SILValue next = worklist.pop_back_val().getValueOr(SILValue());
    if (!next)
      continue;

    // First check if this is an instruction that is trivially dead. This can
    // occur if we eliminate rr traffic resulting in dead projections and the
    // like.
    //
    // If we delete, we first add all of our deleted instructions operands to
    // the worklist and then remove all results (since we are going to delete
    // the instruction).
    if (auto *defInst = next->getDefiningInstruction()) {
      if (isInstructionTriviallyDead(defInst)) {
        deadTrivialInsts.insert(defInst);
        continue;
      }
    }

    // Otherwise, if we have a single value instruction (to be expanded later
    // perhaps), try to visit that value recursively.
    if (auto *svi = dyn_cast<SingleValueInstruction>(next)) {
      madeChange |= visit(svi);
      continue;
    }
  }

  // Then eliminate the rest of the dead trivial insts.
  //
  // NOTE: We do not need to touch the worklist here since it is guaranteed to
  // be empty due to the loop above. We enforce this programatically with the
  // assert.
  assert(worklist.empty() && "Expected drained worklist so we don't have to "
                             "remove dead insts form it");
  while (!deadTrivialInsts.empty()) {
    auto val = deadTrivialInsts.pop_back_val();
    if (!val)
      continue;
    recursivelyDeleteTriviallyDeadInstructions(
        *val, true /*force*/,
        [&](SILInstruction *i) { deadTrivialInsts.erase(i); });
    madeChange = true;
  }

  return madeChange;
}

bool SemanticARCOptVisitor::visitBeginBorrowInst(BeginBorrowInst *bbi) {
  auto kind = bbi->getOperand().getOwnershipKind();
  SmallVector<EndBorrowInst *, 16> endBorrows;
  for (auto *op : bbi->getUses()) {
    auto *user = op->getUser();
    switch (user->getKind()) {
    case SILInstructionKind::EndBorrowInst:
      endBorrows.push_back(cast<EndBorrowInst>(user));
      break;
    default:
      // Make sure that this operand can accept our arguments kind.
      auto map = op->getOwnershipKindMap();
      if (map.canAcceptKind(kind))
        continue;
      return false;
    }
  }

  // At this point, we know that the begin_borrow's operand can be
  // used as an argument to all non-end borrow uses. Eliminate the
  // begin borrow and end borrows.
  while (!endBorrows.empty()) {
    auto *ebi = endBorrows.pop_back_val();
    eraseInstruction(ebi);
    ++NumEliminatedInsts;
  }

  eraseAndRAUWSingleValueInstruction(bbi, bbi->getOperand());
  ++NumEliminatedInsts;
  return true;
}

// Eliminate a copy of a borrowed value, if:
//
// 1. All of the copies users do not consume the copy (and thus can accept a
//    borrowed value instead).
// 2. The copies's non-destroy_value users are strictly contained within the
//    scope of the borrowed value.
//
// Example:
//
//   %0 = @guaranteed (argument or instruction)
//   %1 = copy_value %0
//   apply %f(%1) : $@convention(thin) (@guaranteed ...) ...
//   other_non_consuming_use %1
//   destroy_value %1
//   end_borrow %0 (if an instruction)
//
// =>
//
//   %0 = @guaranteed (argument or instruction)
//   apply %f(%0) : $@convention(thin) (@guaranteed ...) ...
//   other_non_consuming_use %0
//   end_borrow %0 (if an instruction)
//
// NOTE: This means that the destroy_value technically can be after the
// end_borrow. In practice, this will not be the case but we use this to avoid
// having to reason about the ordering of the end_borrow and destroy_value.
//
// NOTE: Today we only perform this for guaranteed parameters since this enables
// us to avoid doing the linear lifetime check to make sure that all destroys
// are within the borrow scope.
//
// TODO: This needs a better name.
bool SemanticARCOptVisitor::performGuaranteedCopyValueOptimization(CopyValueInst *cvi) {
  SmallVector<BorrowScopeIntroducingValue, 4> borrowScopeIntroducers;

  // Find all borrow introducers for our copy operand. If we are unable to find
  // all of the reproducers (due to pattern matching failure), conservatively
  // return false. We can not optimize.
  if (!getUnderlyingBorrowIntroducingValues(cvi->getOperand(),
                                            borrowScopeIntroducers))
    return false;

  // Then go over all of our uses and see if the value returned by our copy
  // value forms a dead live range. If we do not have a dead live range, there
  // must be some consuming use that we either do not understand is /actually/
  // forwarding or a user that truly represents a necessary consume of the
  // value (e.x. storing into memory).
  SmallVector<DestroyValueInst *, 16> destroys;
  SmallVector<SILInstruction *, 16> guaranteedForwardingInsts;
  if (!isDeadLiveRange(cvi, destroys, &guaranteedForwardingInsts))
    return false;

  // Next check if we do not have any destroys of our copy_value and are
  // processing a local borrow scope. In such a case, due to the way we ignore
  // dead end blocks, we may eliminate the copy_value, creating a use of the
  // borrowed value after the end_borrow. To avoid this, in such cases we
  // bail. In contrast, a non-local borrow scope does not have any end scope
  // instructions, implying we can avoid this hazard and still optimize in such
  // a case.
  //
  // DISCUSSION: Consider the following SIL:
  //
  // ```
  //   %1 = begin_borrow %0 : $KlassPair                            (1)
  //   %2 = struct_extract %1 : $KlassPair, #KlassPair.firstKlass
  //   %3 = copy_value %2 : $Klass
  //   ...
  //   end_borrow %1 : $LintCommand                                 (2)
  //   cond_br ..., bb1, bb2
  //
  //   ...
  //
  //   bbN:
  //     // Never return type implies dead end block.
  //     apply %f(%3) : $@convention(thin) (@guaranteed Klass) -> Never (3)
  //     unreachable
  // ```
  //
  // For simplicity, note that if bbN post-dominates %3, given that when we
  // compute linear lifetime errors we ignore dead end blocks, we would not
  // register that the copy_values only use is outside of the begin_borrow
  // region defined by (1), (2) and thus would eliminate the copy. This would
  // result in %2 being used by %f, causing the linear lifetime checker to
  // error.
  //
  // Naively one may assume that the solution to this is to just check if %3 has
  // /any/ destroy_values at all and if it doesn't have any reachable
  // destroy_values, then we are in this case. But is this correct in
  // general. We prove this below:
  //
  // The only paths along which the copy_value can not be destroyed or consumed
  // is along paths to dead end blocks. Trivially, we know that such a dead end
  // block, can not be reachable from the end_borrow since by their nature dead
  // end blocks end in unreachables.
  //
  // So we know that we can only run into this bug if we have a dead end block
  // reachable from the end_borrow, meaning that the bug can not occur if we
  // branch before the end_borrow since in that case, the borrow scope would
  // last over the dead end block's no return meaning that we will not use the
  // borrowed value after its lifetime is ended by the end_borrow.
  //
  // With that in hand, we note again that if we have exactly one consumed,
  // destroy_value /after/ the end_borrow we will not optimize here. This means
  // that this bug can only occur if the copy_value is only post-dominated by
  // dead end blocks that use the value in a non-consuming way.
  //
  // TODO: There may be some way of sinking this into the loop below.
  if (destroys.empty() &&
      llvm::any_of(borrowScopeIntroducers,
                   [](BorrowScopeIntroducingValue borrowScope) {
                     return borrowScope.isLocalScope();
                   })) {
    return false;
  }

  // If we reached this point, then we know that all of our users can accept a
  // guaranteed value and our owned value is destroyed only by
  // destroy_value. Check if all of our destroys are joint post-dominated by the
  // our end borrow scope set. If they do not, then the copy_value is lifetime
  // extending the guaranteed value, we can not eliminate it.
  {
    SmallVector<BranchPropagatedUser, 8> destroysForLinearLifetimeCheck(
        destroys.begin(), destroys.end());
    SmallVector<BranchPropagatedUser, 8> scratchSpace;
    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
    if (llvm::any_of(borrowScopeIntroducers,
                     [&](BorrowScopeIntroducingValue borrowScope) {
                       return !borrowScope.areInstructionsWithinScope(
                           destroysForLinearLifetimeCheck, scratchSpace,
                           visitedBlocks, getDeadEndBlocks());
                     })) {
      return false;
    }
  }

  // Otherwise, we know that our copy_value/destroy_values are all completely
  // within the guaranteed value scope. First delete the destroys/copies.
  while (!destroys.empty()) {
    auto *dvi = destroys.pop_back_val();
    eraseInstruction(dvi);
    ++NumEliminatedInsts;
  }

  eraseAndRAUWSingleValueInstruction(cvi, cvi->getOperand());

  // Then change all of our guaranteed forwarding insts to have guaranteed
  // ownership kind instead of what ever they previously had (ignoring trivial
  // results);
  while (!guaranteedForwardingInsts.empty()) {
    auto *i = guaranteedForwardingInsts.pop_back_val();

    assert(i->hasResults());

    for (SILValue result : i->getResults()) {
      if (auto *svi = dyn_cast<OwnershipForwardingSingleValueInst>(result)) {
        if (svi->getOwnershipKind() == ValueOwnershipKind::Owned) {
          svi->setOwnershipKind(ValueOwnershipKind::Guaranteed);
        }
        continue;
      }

      if (auto *ofci = dyn_cast<OwnershipForwardingConversionInst>(result)) {
        if (ofci->getOwnershipKind() == ValueOwnershipKind::Owned) {
          ofci->setOwnershipKind(ValueOwnershipKind::Guaranteed);
        }
        continue;
      }

      if (auto *sei = dyn_cast<OwnershipForwardingSelectEnumInstBase>(result)) {
        if (sei->getOwnershipKind() == ValueOwnershipKind::Owned) {
          sei->setOwnershipKind(ValueOwnershipKind::Guaranteed);
        }
        continue;
      }

      if (auto *mvir = dyn_cast<MultipleValueInstructionResult>(result)) {
        if (mvir->getOwnershipKind() == ValueOwnershipKind::Owned) {
          mvir->setOwnershipKind(ValueOwnershipKind::Guaranteed);
        }
        continue;
      }

      llvm_unreachable("unhandled forwarding instruction?!");
    }
  }
  ++NumEliminatedInsts;
  return true;
}

/// If cvi only has destroy value users, then cvi is a dead live range. Lets
/// eliminate all such dead live ranges.
bool SemanticARCOptVisitor::eliminateDeadLiveRangeCopyValue(CopyValueInst *cvi) {
  // See if we are lucky and have a simple case.
  if (auto *op = cvi->getSingleUse()) {
    if (auto *dvi = dyn_cast<DestroyValueInst>(op->getUser())) {
      eraseInstruction(dvi);
      eraseInstructionAndAddOperandsToWorklist(cvi);
      NumEliminatedInsts += 2;
      return true;
    }
  }

  // If all of our copy_value users are destroy_value, zap all of the
  // instructions. We begin by performing that check and gathering up our
  // destroy_value.
  SmallVector<DestroyValueInst *, 16> destroys;
  if (!all_of(cvi->getUses(), [&](Operand *op) {
        auto *dvi = dyn_cast<DestroyValueInst>(op->getUser());
        if (!dvi)
          return false;

        // Stash dvi in destroys so we can easily eliminate it later.
        destroys.push_back(dvi);
        return true;
      })) {
    return false;
  }

  // Now that we have a truly dead live range copy value, eliminate it!
  while (!destroys.empty()) {
    eraseInstruction(destroys.pop_back_val());
    ++NumEliminatedInsts;
  }
  eraseInstructionAndAddOperandsToWorklist(cvi);
  ++NumEliminatedInsts;
  return true;
}

bool SemanticARCOptVisitor::visitCopyValueInst(CopyValueInst *cvi) {
  // If our copy value inst has only destroy_value users, it is a dead live
  // range. Try to eliminate them.
  if (eliminateDeadLiveRangeCopyValue(cvi))
    return true;

  // Then try to perform the guaranteed copy value optimization.
  if (performGuaranteedCopyValueOptimization(cvi))
    return true;

  return false;
}

//===----------------------------------------------------------------------===//
//                         load [copy] Optimizations
//===----------------------------------------------------------------------===//

// A flow insensitive analysis that tells the load [copy] analysis if the
// storage has 0, 1, >1 writes to it.
//
// In the case of 0 writes, we return CanOptimizeLoadCopyResult::Always.
//
// In the case of 1 write, we return OnlyIfStorageIsLocal. We are taking
// advantage of definite initialization implying that an alloc_stack must be
// written to once before any loads from the memory location. Thus if we are
// local and see 1 write, we can still change to load_borrow if all other uses
// check out.
//
// If there is 2+ writes, we can not optimize = (.

bool mayFunctionMutateArgument(const AccessedStorage &storage, SILFunction &f) {
  auto *arg = cast<SILFunctionArgument>(storage.getArgument());

  // Then check if we have an in_guaranteed argument. In this case, we can
  // always optimize load [copy] from this.
  if (arg->hasConvention(SILArgumentConvention::Indirect_In_Guaranteed))
    return false;

  // For now just return false.
  return true;
}

// Then find our accessed storage to determine whether it provides a guarantee
// for the loaded value.
namespace {
class StorageGuaranteesLoadVisitor
  : public AccessUseDefChainVisitor<StorageGuaranteesLoadVisitor>
{
  // The outer SemanticARCOptVisitor.
  SemanticARCOptVisitor &ARCOpt;
  
  // The original load instruction.
  LoadInst *Load;
  
  // The current address being visited.
  SILValue currentAddress;
  
  Optional<bool> isWritten;
  
public:
  StorageGuaranteesLoadVisitor(SemanticARCOptVisitor &arcOpt, LoadInst *load)
    : ARCOpt(arcOpt), Load(load), currentAddress(load->getOperand())
  {}
  
  void answer(bool written) {
    currentAddress = nullptr;
    isWritten = written;
  }
  
  void next(SILValue address) {
    currentAddress = address;
  }
  
  void visitNestedAccess(BeginAccessInst *access) {
    // Look through nested accesses.
    return next(access->getOperand());
  }
  
  void visitArgumentAccess(SILFunctionArgument *arg) {
    return answer(mayFunctionMutateArgument(
                             AccessedStorage(arg, AccessedStorage::Argument),
                             ARCOpt.F));
  }
  
  void visitGlobalAccess(SILValue global) {
    return answer(!AccessedStorage(global, AccessedStorage::Global)
                    .isLetAccess(&ARCOpt.F));
  }
  
  void visitClassAccess(RefElementAddrInst *field) {
    currentAddress = nullptr;
    
    // We know a let property won't be written to if the base object is
    // guaranteed for the duration of the access.
    // For non-let properties conservatively assume they may be written to.
    if (!field->getField()->isLet()) {
      return answer(true);
    }
    
    // The lifetime of the `let` is guaranteed if it's dominated by the
    // guarantee on the base. Check for a borrow.
    SILValue baseObject = field->getOperand();
    auto beginBorrow = dyn_cast<BeginBorrowInst>(baseObject);
    if (beginBorrow)
      baseObject = beginBorrow->getOperand();
    baseObject = stripCasts(baseObject);

    // A guaranteed argument trivially keeps the base alive for the duration of
    // the projection.
    if (auto *arg = dyn_cast<SILFunctionArgument>(baseObject)) {
      if (arg->getArgumentConvention().isGuaranteedConvention()) {
        return answer(false);
      }
    }
    
    // See if there's a borrow of the base object our load is based on.
    SILValue borrowInst;
    if (isa<LoadBorrowInst>(baseObject)) {
      borrowInst = baseObject;
    } else {
      borrowInst = beginBorrow;
    }
    // TODO: We could also look at a guaranteed phi argument and see whether
    // the loaded copy is dominated by it.
    if (!borrowInst)
      return answer(true);

    // Use the linear lifetime checker to check whether the copied
    // value is dominated by the lifetime of the borrow it's based on.
    SmallVector<BranchPropagatedUser, 4> baseEndBorrows;
    for (auto *use : borrowInst->getUses()) {
      if (isa<EndBorrowInst>(use->getUser())) {
        baseEndBorrows.push_back(BranchPropagatedUser(use->getUser()));
      }
    }
    
    SmallVector<BranchPropagatedUser, 4> valueDestroys;
    for (auto *use : Load->getUses()) {
      if (isa<DestroyValueInst>(use->getUser())) {
        valueDestroys.push_back(BranchPropagatedUser(use->getUser()));
      }
    }
    
    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;

    LinearLifetimeChecker checker(visitedBlocks, ARCOpt.getDeadEndBlocks());
    // Returns true on success. So we invert.
    bool foundError =
        !checker.validateLifetime(baseObject, baseEndBorrows, valueDestroys);
    return answer(foundError);
  }
  
  // TODO: Handle other access kinds?
  void visitBase(SILValue base, AccessedStorage::Kind kind) {
    return answer(true);
  }

  void visitNonAccess(SILValue addr) {
    return answer(true);
  }
  
  void visitIncomplete(SILValue projectedAddr, SILValue parentAddr) {
    return next(parentAddr);
  }
  
  void visitPhi(SILPhiArgument *phi) {
    // We shouldn't have address phis in OSSA SIL, so we don't need to recur
    // through the predecessors here.
    return answer(true);
  }

  bool doIt() {
    while (currentAddress) {
      visit(currentAddress);
    }
    return *isWritten;
  }
};
} // namespace

bool SemanticARCOptVisitor::isWrittenTo(LoadInst *load) {
  StorageGuaranteesLoadVisitor visitor(*this, load);
  return visitor.doIt();
}

// Convert a load [copy] from unique storage [read] that has all uses that can
// accept a guaranteed parameter to a load_borrow.
bool SemanticARCOptVisitor::visitLoadInst(LoadInst *li) {
  if (li->getOwnershipQualifier() != LoadOwnershipQualifier::Copy)
    return false;

  // Ok, we have our load [copy]. Make sure its value is truly a dead live range
  // implying it is only ever consumed by destroy_value instructions. If it is
  // consumed, we need to pass off a +1 value, so bail.
  //
  // FIXME: We should consider if it is worth promoting a load [copy]
  // -> load_borrow if we can put a copy_value on a cold path and thus
  // eliminate RR traffic on a hot path.
  SmallVector<DestroyValueInst *, 32> destroyValues;
  if (!isDeadLiveRange(li, destroyValues))
    return false;

  // Then check if our address is ever written to. If it is, then we cannot use
  // the load_borrow because the stored value may be released during the loaded
  // value's live range.
  if (isWrittenTo(li))
    return false;

  // Ok, we can perform our optimization. Convert the load [copy] into a
  // load_borrow.
  auto *lbi =
      SILBuilderWithScope(li).createLoadBorrow(li->getLoc(), li->getOperand());

  // Since we are looking through forwarding uses that can accept guaranteed
  // parameters, we can have multiple destroy_value along the same path. We need
  // to find the post-dominating block set of these destroy value to ensure that
  // we do not insert multiple end_borrow.
  while (!destroyValues.empty()) {
    auto *dvi = destroyValues.pop_back_val();
    SILBuilderWithScope(dvi).createEndBorrow(dvi->getLoc(), lbi);
    eraseInstruction(dvi);
    ++NumEliminatedInsts;
  }

  eraseAndRAUWSingleValueInstruction(li, lbi);
  ++NumEliminatedInsts;
  ++NumLoadCopyConvertedToLoadBorrow;
  return true;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

// Even though this is a mandatory pass, it is rerun after deserialization in
// case DiagnosticConstantPropagation exposed anything new in this assert
// configuration.
struct SemanticARCOpts : SILFunctionTransform {
  void run() override {
    SILFunction &f = *getFunction();

    // Make sure we are running with ownership verification enabled.
    assert(f.getModule().getOptions().VerifySILOwnership &&
           "Can not perform semantic arc optimization unless ownership "
           "verification is enabled");

    // Iterate over all of the arguments, performing small peephole ARC
    // optimizations. We assume that the visitor will add any instructions we
    // need to recursively to the visitor's worklist. Also, note that we assume
    // that we do not look through /any/ sil block arguments here since our
    // iteration here is only valid up to SSA.
    bool madeChange = false;
    
    SemanticARCOptVisitor visitor(f);
    
    for (auto &bb : f) {
      auto ii = bb.rend();
      auto start = bb.rbegin();

      // If the bb is empty, continue.
      if (start == ii)
        continue;

      // Go to the first instruction to process.
      --ii;

      // Then until we process the first instruction of the block...
      while (ii != start) {
        // Move the iterator before ii.
        auto tmp = std::next(ii);

        // Then try to optimize. If we succeeded, then we deleted
        // ii. Move ii from the next value back onto the instruction
        // after ii's old value in the block instruction list and then
        // process that.
        if (visitor.visit(&*ii)) {
          madeChange = true;
          ii = std::prev(tmp);
          continue;
        }

        // Otherwise, we didn't delete ii. Just visit the next instruction.
        --ii;
      }

      // Finally visit the first instruction of the block.
      madeChange |= visitor.visit(&*ii);
    }

    // Finally drain the worklist on the visitor and process until we reach the
    // fixpoint and thus do not have any further work to do.
    //
    // NOTE: At this point madeChange has already been set to true if we have
    // anything in the worklist, so technically we do not need to do this. But I
    // would rather represent this state to future proof the pass to be
    // "visually" correct.
    madeChange |= visitor.processWorklist();

    if (madeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createSemanticARCOpts() { return new SemanticARCOpts(); }
