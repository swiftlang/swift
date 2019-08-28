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
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/BranchPropagatedUser.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
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

/// Return true if v only has invalidating uses that are destroy_value.
///
/// Semantically this implies that a value is never passed off as +1 to memory
/// or another function implying it can be used everywhere at +0.
static bool isConsumed(
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
      // non-trivial operand that can accept a guaranteed value. If so, at its
      // users to the worklist and continue.
      //
      // DISCUSSION: For now we do not support forwarding instructions with
      // multiple non-trivial arguments since we would need to optimize all of
      // the non-trivial arguments at the same time.
      //
      // NOTE: Today we do not support TermInsts for simplicity... we /could/
      // support it though if we need to.
      if (forwardingInsts.isNonNull() && !isa<TermInst>(user) &&
          isGuaranteedForwardingInst(user) &&
          1 == count_if(user->getOperandValues(
                            true /*ignore type dependent operands*/),
                        [&](SILValue v) {
                          return v.getOwnershipKind() ==
                                 ValueOwnershipKind::Owned;
                        })) {
        forwardingInsts.get()->push_back(user);
        for (SILValue v : user->getResults()) {
          if (v.getOwnershipKind() != ValueOwnershipKind::Owned)
            continue;
          llvm::copy(v->getUses(), std::back_inserter(worklist));
        }
        continue;
      }

      // Otherwise be conservative and assume that we /may consume/ the value.
      return true;
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

  return false;
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
  SmallSetVector<SILValue, 32> worklist;
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
  void eraseInstructionAndAddOptsToWorklist(SILInstruction *i) {
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
      worklist.remove(result);
    }
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
    SILValue next = worklist.pop_back_val();

    // First check if this is an instruction that is trivially dead. This can
    // occur if we eliminate rr traffic resulting in dead projections and the
    // like.
    //
    // If we delete, we first add all of our deleted instructions operands to
    // the worklist and then remove all results (since we are going to delete
    // the instruction).
    if (auto *defInst = next->getDefiningInstruction()) {
      if (isInstructionTriviallyDead(defInst)) {
        madeChange = true;
        recursivelyDeleteTriviallyDeadInstructions(
            defInst, true/*force*/,
            [&](SILInstruction *i) {
              for (SILValue operand : i->getOperandValues()) {
                worklist.insert(operand);
              }
              for (SILValue result : i->getResults()) {
                worklist.remove(result);
              }
              ++NumEliminatedInsts;
            });
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

static bool canHandleOperand(SILValue operand, SmallVectorImpl<SILValue> &out) {
  if (!getUnderlyingBorrowIntroducers(operand, out))
    return false;

  /// TODO: Add support for begin_borrow, load_borrow.
  auto canHandleValue = [](SILValue v) -> bool {
    return isa<SILFunctionArgument>(v) || isa<LoadBorrowInst>(v) ||
           isa<BeginBorrowInst>(v);
  };
  return all_of(out, canHandleValue);
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
  SmallVector<SILValue, 16> borrowIntroducers;

  // Whitelist the operands that we know how to support and make sure
  // our operand is actually guaranteed.
  if (!canHandleOperand(cvi->getOperand(), borrowIntroducers))
    return false;

  // Then go over all of our uses. Find our destroying instructions (ignoring
  // forwarding instructions that can forward both owned and guaranteed) and
  // make sure all of them are destroy_value. For our non-destroying
  // instructions, make sure that they accept a guaranteed value. After that,
  // make sure that our destroys are within the lifetime of our borrowed values.
  //
  // TODO: Change isConsumed to return branch propagated users for destroys, so
  // we do not need to construct another array.
  SmallVector<DestroyValueInst *, 16> destroys;
  SmallVector<SILInstruction *, 16> guaranteedForwardingInsts;
  if (isConsumed(cvi, destroys, &guaranteedForwardingInsts))
    return false;

  // If we reached this point, then we know that all of our users can
  // accept a guaranteed value and our owned value is destroyed only
  // by destroy_value. Check if all of our destroys are joint
  // post-dominated by the end_borrow set. If they do not, then the
  // copy_value is lifetime extending the guaranteed value, we can not
  // eliminate it.
  {
    SmallVector<BranchPropagatedUser, 8> destroysForLinearLifetimeCheck(
        destroys.begin(), destroys.end());
    SmallVector<BranchPropagatedUser, 8> endBorrowInsts;
    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
    for (SILValue v : borrowIntroducers) {
      if (isa<SILFunctionArgument>(v)) {
        continue;
      }

      SWIFT_DEFER {
        endBorrowInsts.clear();
        visitedBlocks.clear();
      };

      if (auto *lbi = dyn_cast<LoadBorrowInst>(v)) {
        llvm::copy(lbi->getEndBorrows(), std::back_inserter(endBorrowInsts));
      } else if (auto *bbi = dyn_cast<BeginBorrowInst>(v)) {
        llvm::copy(bbi->getEndBorrows(), std::back_inserter(endBorrowInsts));
      } else {
        llvm_unreachable("Unhandled borrow introducer?!");
      }

      // Make sure that our destroys are properly nested within our end
      // borrows. Otherwise, we can not optimize.
      auto result = valueHasLinearLifetime(
          v, endBorrowInsts, destroysForLinearLifetimeCheck, visitedBlocks,
          getDeadEndBlocks(), ownership::ErrorBehaviorKind::ReturnFalse);
      if (result.getFoundError())
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
      eraseInstructionAndAddOptsToWorklist(cvi);
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
  eraseInstructionAndAddOptsToWorklist(cvi);
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

bool SemanticARCOptVisitor::isWrittenTo(LoadInst *load) {
  auto addr = load->getOperand();
  
  // Then find our accessed storage. If we can not find anything, be
  // conservative and assume that the value is written to.
  const auto &storage = findAccessedStorageNonNested(addr);
  if (!storage)
    return true;

  // Then see if we ever write to this address in a flow insensitive
  // way (ignoring stores that are obviously the only initializer to
  // memory). We have to do this since load_borrow assumes that the
  // underlying memory is never written to.
  switch (storage.getKind()) {
  case AccessedStorage::Class: {
    // We know a let property won't be written to if the base object is
    // guaranteed for the duration of the access.
    if (!storage.isLetAccess(&F))
      return true;
   
    auto baseObject = stripCasts(storage.getObject());
    // A guaranteed argument trivially keeps the base alive for the duration of
    // the projection.
    if (auto *arg = dyn_cast<SILFunctionArgument>(baseObject)) {
      if (arg->getArgumentConvention().isGuaranteedConvention()) {
        return false;
      }
    }
    
    // See if there's a borrow of the base object our load is based on.
    SILValue borrowInst;
    if (isa<BeginBorrowInst>(baseObject)
        || isa<LoadBorrowInst>(baseObject)) {
      borrowInst = baseObject;
    } else {
      // TODO: We should walk the projection path again to get to the
      // originating borrow, if any
      
      BeginBorrowInst *singleBorrow = nullptr;
      for (auto *use : baseObject->getUses()) {
        if (auto *borrow = dyn_cast<BeginBorrowInst>(use->getUser())) {
          if (!singleBorrow) {
            singleBorrow = borrow;
          } else {
            singleBorrow = nullptr;
            break;
          }
        }
      }
      
      borrowInst = singleBorrow;
    }
    
    // Use the linear lifetime checker to check whether the copied
    // value is dominated by the lifetime of the borrow it's based on.
    if (!borrowInst)
      return true;
    
    SmallVector<BranchPropagatedUser, 4> baseEndBorrows;
    for (auto *use : borrowInst->getUses()) {
      if (isa<EndBorrowInst>(use->getUser())) {
        baseEndBorrows.push_back(BranchPropagatedUser(use->getUser()));
      }
    }
    
    SmallVector<BranchPropagatedUser, 4> valueDestroys;
    for (auto *use : load->getUses()) {
      if (isa<DestroyValueInst>(use->getUser())) {
        valueDestroys.push_back(BranchPropagatedUser(use->getUser()));
      }
    }
    
    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
    
    return valueHasLinearLifetime(baseObject, baseEndBorrows, valueDestroys,
                                  visitedBlocks, getDeadEndBlocks(),
                                  ownership::ErrorBehaviorKind::ReturnFalse)
      .getFoundError();
  }
  case AccessedStorage::Global:
    // Any legal load of a global let should have happened after its
    // initialization, at which point it can't be written to again for the
    // lifetime of the program.
    return !storage.isLetAccess(&F);

  case AccessedStorage::Box:
  case AccessedStorage::Stack:
  case AccessedStorage::Yield:
  case AccessedStorage::Nested:
  case AccessedStorage::Unidentified:
    return true;
      
  case AccessedStorage::Argument:
    return mayFunctionMutateArgument(storage, F);
  }
  llvm_unreachable("covered switch");
}

// Convert a load [copy] from unique storage [read] that has all uses that can
// accept a guaranteed parameter to a load_borrow.
bool SemanticARCOptVisitor::visitLoadInst(LoadInst *li) {
  if (li->getOwnershipQualifier() != LoadOwnershipQualifier::Copy)
    return false;

  // Ok, we have our load [copy]. Make sure its value is never
  // consumed. If it is consumed, we need to pass off a +1 value, so
  // bail.
  //
  // FIXME: We should consider if it is worth promoting a load [copy]
  // -> load_borrow if we can put a copy_value on a cold path and thus
  // eliminate RR traffic on a hot path.
  SmallVector<DestroyValueInst *, 32> destroyValues;
  if (isConsumed(li, destroyValues))
    return false;

  // Then check if our address is ever written to. If it is, then we
  // can not use the load_borrow.
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
