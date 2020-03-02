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
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

STATISTIC(NumEliminatedInsts, "number of removed instructions");
STATISTIC(NumLoadCopyConvertedToLoadBorrow,
          "number of load_copy converted to load_borrow");

//===----------------------------------------------------------------------===//
//                            Live Range Modeling
//===----------------------------------------------------------------------===//

namespace {

class LiveRange {
  /// A list of destroy_values of the live range.
  SmallVector<SILInstruction *, 2> destroys;

  /// A list of forwarding instructions that forward our destroys ownership, but
  /// that are also able to forward guaranteed ownership.
  SmallVector<SILInstruction *, 2> generalForwardingInsts;

  /// Consuming users that we were not able to understand as a forwarding
  /// instruction or a destroy_value. These must be passed a strongly control
  /// equivalent +1 value.
  SmallVector<SILInstruction *, 2> unknownConsumingUsers;

public:
  LiveRange(SILValue value);
  LiveRange(const LiveRange &) = delete;
  LiveRange &operator=(const LiveRange &) = delete;

  /// Return true if v only has invalidating uses that are destroy_value. Such
  /// an owned value is said to represent a dead "live range".
  ///
  /// Semantically this implies that a value is never passed off as +1 to memory
  /// or another function implying it can be used everywhere at +0.
  bool hasConsumingUse() const { return unknownConsumingUsers.size(); }

  ArrayRef<SILInstruction *> getDestroys() const { return destroys; }
  ArrayRef<SILInstruction *> getNonConsumingForwardingInsts() const {
    return generalForwardingInsts;
  }
};

} // end anonymous namespace

LiveRange::LiveRange(SILValue value)
    : destroys(), generalForwardingInsts(), unknownConsumingUsers() {
  assert(value.getOwnershipKind() == ValueOwnershipKind::Owned);

  // We know that our silvalue produces an @owned value. Look through all of our
  // uses and classify them as either consuming or not.
  SmallVector<Operand *, 32> worklist(value->getUses());
  while (!worklist.empty()) {
    auto *op = worklist.pop_back_val();

    // Skip type dependent operands.
    if (op->isTypeDependent())
      continue;

    // Do a quick check that we did not add ValueOwnershipKind that are not
    // owned to the worklist.
    assert(op->get().getOwnershipKind() == ValueOwnershipKind::Owned &&
           "Added non-owned value to worklist?!");

    auto *user = op->getUser();

    // Ok, this constraint can take something owned as live. Assert that it
    // can also accept something that is guaranteed. Any non-consuming use of
    // an owned value should be able to take a guaranteed parameter as well
    // (modulo bugs). We assert to catch these.
    if (!op->isConsumingUse()) {
      continue;
    }

    // Ok, we know now that we have a consuming use. See if we have a destroy
    // value, quickly up front. If we do have one, stash it and continue.
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
    auto *ti = dyn_cast<TermInst>(user);
    if ((ti && !ti->isTransformationTerminator()) ||
        !isGuaranteedForwardingInst(user) ||
        1 != count_if(user->getOperandValues(
                          true /*ignore type dependent operands*/),
                      [&](SILValue v) {
                        return v.getOwnershipKind() ==
                               ValueOwnershipKind::Owned;
                      })) {
      unknownConsumingUsers.push_back(user);
      continue;
    }

    // Ok, this is a forwarding instruction whose ownership we can flip from
    // owned -> guaranteed.
    generalForwardingInsts.push_back(user);

    // If we have a non-terminator, just visit its users recursively to see if
    // the the users force the live range to be alive.
    if (!ti) {
      for (SILValue v : user->getResults()) {
        if (v.getOwnershipKind() != ValueOwnershipKind::Owned)
          continue;
        llvm::copy(v->getUses(), std::back_inserter(worklist));
      }
      continue;
    }

    // Otherwise, we know that we have no only a terminator, but a
    // transformation terminator, so we should add the users of its results to
    // the worklist.
    for (auto &succ : ti->getSuccessors()) {
      auto *succBlock = succ.getBB();

      // If we do not have any arguments, then continue.
      if (succBlock->args_empty())
        continue;

      for (auto *succArg : succBlock->getSILPhiArguments()) {
        // If we have an any value, just continue.
        if (succArg->getOwnershipKind() == ValueOwnershipKind::None)
          continue;

        // Otherwise add all users of this BBArg to the worklist to visit
        // recursively.
        llvm::copy(succArg->getUses(), std::back_inserter(worklist));
      }
    }
  }
}

//===----------------------------------------------------------------------===//
//                        Address Written To Analysis
//===----------------------------------------------------------------------===//

namespace {

/// A simple analysis that checks if a specific def (in our case an inout
/// argument) is ever written to. This is conservative, local and processes
/// recursively downwards from def->use.
struct IsAddressWrittenToDefUseAnalysis {
  llvm::SmallDenseMap<SILValue, bool, 8> isWrittenToCache;

  bool operator()(SILValue value) {
    auto iter = isWrittenToCache.try_emplace(value, true);

    // If we are already in the map, just return that.
    if (!iter.second)
      return iter.first->second;

    // Otherwise, compute our value, cache it and return.
    bool result = isWrittenToHelper(value);
    iter.first->second = result;
    return result;
  }

private:
  bool isWrittenToHelper(SILValue value);
};

} // end anonymous namespace

bool IsAddressWrittenToDefUseAnalysis::isWrittenToHelper(SILValue initialValue) {
  SmallVector<Operand *, 8> worklist(initialValue->getUses());
  while (!worklist.empty()) {
    auto *op = worklist.pop_back_val();
    SILInstruction *user = op->getUser();

    if (Projection::isAddressProjection(user) ||
        isa<ProjectBlockStorageInst>(user)) {
      for (SILValue r : user->getResults()) {
        llvm::copy(r->getUses(), std::back_inserter(worklist));
      }
      continue;
    }

    if (auto *oeai = dyn_cast<OpenExistentialAddrInst>(user)) {
      // Mutable access!
      if (oeai->getAccessKind() != OpenedExistentialAccess::Immutable) {
        return true;
      }

      //  Otherwise, look through it and continue.
      llvm::copy(oeai->getUses(), std::back_inserter(worklist));
      continue;
    }

    // load_borrow and incidental uses are fine as well.
    if (isa<LoadBorrowInst>(user) || isIncidentalUse(user)) {
      continue;
    }

    // Look through immutable begin_access.
    if (auto *bai = dyn_cast<BeginAccessInst>(user)) {
      // If we do not have a read, return true.
      if (bai->getAccessKind() != SILAccessKind::Read) {
        return true;
      }

      // Otherwise, add the users to the worklist and continue.
      llvm::copy(bai->getUses(), std::back_inserter(worklist));
      continue;
    }

    // As long as we do not have a load [take], we are fine.
    if (auto *li = dyn_cast<LoadInst>(user)) {
      if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Take) {
        return true;
      }
      continue;
    }

    // If we have a FullApplySite, see if we use the value as an
    // indirect_guaranteed parameter. If we use it as inout, we need
    // interprocedural analysis that we do not perform here.
    if (auto fas = FullApplySite::isa(user)) {
      if (fas.getArgumentConvention(*op) ==
          SILArgumentConvention::Indirect_In_Guaranteed)
        continue;

      // Otherwise, be conservative and return true.
      return true;
    }

    // Copy addr that read are just loads.
    if (auto *cai = dyn_cast<CopyAddrInst>(user)) {
      // If our value is the destination, this is a write.
      if (cai->getDest() == op->get()) {
        return true;
      }

      // Ok, so we are Src by process of elimination. Make sure we are not being
      // taken.
      if (cai->isTakeOfSrc()) {
        return true;
      }

      // Otherwise, we are safe and can continue.
      continue;
    }

    // If we did not recognize the user, just return conservatively that it was
    // written to.
    LLVM_DEBUG(llvm::dbgs()
               << "Function: " << user->getFunction()->getName() << "\n");
    LLVM_DEBUG(llvm::dbgs() << "Value: " << op->get());
    LLVM_DEBUG(llvm::dbgs() << "Unknown instruction!: " << *user);
    return true;
  }

  // Ok, we finished our worklist and this address is not being written to.
  return false;
}

//===----------------------------------------------------------------------===//
//                               Implementation
//===----------------------------------------------------------------------===//

namespace {

/// A visitor that optimizes ownership instructions and eliminates any trivially
/// dead code that results after optimization. It uses an internal worklist that
/// is initialized on construction with targets to avoid iterator invalidation
/// issues. Rather than revisit the entire CFG like SILCombine and other
/// visitors do, we maintain a visitedSinceLastMutation list to ensure that we
/// revisit all interesting instructions in between mutations.
struct SemanticARCOptVisitor
    : SILInstructionVisitor<SemanticARCOptVisitor, bool> {
  /// Our main worklist. We use this after an initial run through.
  SmallBlotSetVector<SILValue, 32> worklist;

  /// A set of values that we have visited since the last mutation. We use this
  /// to ensure that we do not visit values twice without mutating.
  ///
  /// This is specifically to ensure that we do not go into an infinite loop
  /// when visiting phi nodes.
  SmallBlotSetVector<SILValue, 16> visitedSinceLastMutation;

  SILFunction &F;
  Optional<DeadEndBlocks> TheDeadEndBlocks;
  ValueLifetimeAnalysis::Frontier lifetimeFrontier;
  IsAddressWrittenToDefUseAnalysis isAddressWrittenToDefUseAnalysis;

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
    for (auto *use : i->getUses()) {
      for (SILValue result : use->getUser()->getResults()) {
        worklist.insert(result);
      }
    }
    i->replaceAllUsesWith(newValue);
    eraseInstructionAndAddOperandsToWorklist(i);
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
      visitedSinceLastMutation.erase(result);
    }
    i->eraseFromParent();

    // Add everything else from visitedSinceLastMutation to the worklist.
    for (auto opt : visitedSinceLastMutation) {
      if (!opt.hasValue()) {
        continue;
      }
      worklist.insert(*opt);
    }
    visitedSinceLastMutation.clear();
  }

  /// The default visitor.
  bool visitSILInstruction(SILInstruction *i) {
    assert(!isGuaranteedForwardingInst(i) &&
           "Should have forwarding visitor for all ownership forwarding "
           "instructions");
    return false;
  }

  bool visitCopyValueInst(CopyValueInst *cvi);
  bool visitBeginBorrowInst(BeginBorrowInst *bbi);
  bool visitLoadInst(LoadInst *li);
  static bool shouldVisitInst(SILInstruction *i) {
    switch (i->getKind()) {
    default:
      return false;
    case SILInstructionKind::CopyValueInst:
    case SILInstructionKind::BeginBorrowInst:
    case SILInstructionKind::LoadInst:
      return true;
    }
  }

#define FORWARDING_INST(NAME)                                                  \
  bool visit##NAME##Inst(NAME##Inst *cls) {                                    \
    for (SILValue v : cls->getResults()) {                                     \
      worklist.insert(v);                                                      \
    }                                                                          \
    return false;                                                              \
  }
  FORWARDING_INST(Tuple)
  FORWARDING_INST(Struct)
  FORWARDING_INST(Enum)
  FORWARDING_INST(OpenExistentialRef)
  FORWARDING_INST(Upcast)
  FORWARDING_INST(UncheckedRefCast)
  FORWARDING_INST(ConvertFunction)
  FORWARDING_INST(RefToBridgeObject)
  FORWARDING_INST(BridgeObjectToRef)
  FORWARDING_INST(UnconditionalCheckedCast)
  FORWARDING_INST(UncheckedEnumData)
  FORWARDING_INST(MarkUninitialized)
  FORWARDING_INST(SelectEnum)
  FORWARDING_INST(DestructureStruct)
  FORWARDING_INST(DestructureTuple)
  FORWARDING_INST(TupleExtract)
  FORWARDING_INST(StructExtract)
  FORWARDING_INST(OpenExistentialValue)
  FORWARDING_INST(OpenExistentialBoxValue)
  FORWARDING_INST(MarkDependence)
#undef FORWARDING_INST

#define FORWARDING_TERM(NAME)                                                  \
  bool visit##NAME##Inst(NAME##Inst *cls) {                                    \
    for (auto succValues : cls->getSuccessorBlockArgumentLists()) {            \
      for (SILValue v : succValues) {                                          \
        worklist.insert(v);                                                    \
      }                                                                        \
    }                                                                          \
    return false;                                                              \
  }

  FORWARDING_TERM(SwitchEnum)
  FORWARDING_TERM(CheckedCastBranch)
  FORWARDING_TERM(Branch)
  FORWARDING_TERM(CondBranch)
#undef FORWARDING_TERM

  bool isWrittenTo(LoadInst *li, const LiveRange &lr);

  bool processWorklist();

  bool performGuaranteedCopyValueOptimization(CopyValueInst *cvi);
  bool eliminateDeadLiveRangeCopyValue(CopyValueInst *cvi);
  bool tryJoiningCopyValueLiveRangeWithOperand(CopyValueInst *cvi);
};

} // end anonymous namespace

static llvm::cl::opt<bool>
VerifyAfterTransform("sil-semantic-arc-opts-verify-after-transform",
                     llvm::cl::init(false), llvm::cl::Hidden);

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

    // First check if this is a value that we have visited since the last time
    // we erased an instruction. If we have visited it, skip it. Every time we
    // modify something, we should be deleting an instruction, so we have not
    // found any further information.
    if (!visitedSinceLastMutation.insert(next).second) {
      continue;
    }

    // First check if this is an instruction that is trivially dead. This can
    // occur if we eliminate rr traffic resulting in dead projections and the
    // like.
    //
    // If we delete, we first add all of our deleted instructions operands to
    // the worklist and then remove all results (since we are going to delete
    // the instruction).
    if (auto *defInst = next->getDefiningInstruction()) {
      if (isInstructionTriviallyDead(defInst)) {
        deleteAllDebugUses(defInst);
        eraseInstruction(defInst);
        madeChange = true;
        if (VerifyAfterTransform) {
          F.verify();
        }
        continue;
      }
    }

    // Otherwise, if we have a single value instruction (to be expanded later
    // perhaps), try to visit that value recursively.
    if (auto *svi = dyn_cast<SingleValueInstruction>(next)) {
      bool madeSingleChange = visit(svi);
      madeChange |= madeSingleChange;
      if (VerifyAfterTransform && madeSingleChange) {
        F.verify();
      }
      continue;
    }
  }

  return madeChange;
}

bool SemanticARCOptVisitor::visitBeginBorrowInst(BeginBorrowInst *bbi) {
  auto kind = bbi->getOperand().getOwnershipKind();
  SmallVector<EndBorrowInst *, 16> endBorrows;
  for (auto *op : bbi->getUses()) {
    if (!op->isConsumingUse()) {
      // Make sure that this operand can accept our arguments kind.
      auto map = op->getOwnershipKindMap();
      if (map.canAcceptKind(kind))
        continue;
      return false;
    }

    // Otherwise, this borrow is being consumed. See if our consuming inst is an
    // end_borrow. If it isn't, then return false, this scope is
    // needed. Otherwise, add the end_borrow to our list of end borrows.
    auto *ebi = dyn_cast<EndBorrowInst>(op->getUser());
    if (!ebi) {
      return false;
    }
    endBorrows.push_back(ebi);
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

static void convertForwardingInstsFromOwnedToGuaranteed(
    ArrayRef<SILInstruction *> guaranteedForwardingInsts) {
  // Then change all of our guaranteed forwarding insts to have guaranteed
  // ownership kind instead of what ever they previously had (ignoring trivial
  // results);
  while (!guaranteedForwardingInsts.empty()) {
    auto *i = guaranteedForwardingInsts.back();
    guaranteedForwardingInsts = guaranteedForwardingInsts.drop_back();

    // If this is a term inst, just convert all of its incoming values that are
    // owned to be guaranteed.
    if (auto *ti = dyn_cast<TermInst>(i)) {
      for (auto &succ : ti->getSuccessors()) {
        auto *succBlock = succ.getBB();

        // If we do not have any arguments, then continue.
        if (succBlock->args_empty())
          continue;

        for (auto *succArg : succBlock->getSILPhiArguments()) {
          // If we have an any value, just continue.
          if (succArg->getOwnershipKind() == ValueOwnershipKind::Owned) {
            succArg->setOwnershipKind(ValueOwnershipKind::Guaranteed);
          }
        }
      }
      continue;
    }

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
  //
  // NOTE: We can get multiple introducers if our copy_value's operand
  // value runs through a phi or an aggregate forming instruction.
  if (!getAllBorrowIntroducingValues(cvi->getOperand(), borrowScopeIntroducers))
    return false;

  // Then go over all of our uses and see if the value returned by our copy
  // value forms a dead live range. If we do not have a dead live range, there
  // must be some consuming use that we either do not understand is /actually/
  // forwarding or a user that truly represents a necessary consume of the
  // value (e.x. storing into memory).
  LiveRange lr(cvi);
  if (lr.hasConsumingUse())
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
  bool haveAnyLocalScopes = llvm::any_of(
      borrowScopeIntroducers, [](BorrowScopeIntroducingValue borrowScope) {
        return borrowScope.isLocalScope();
      });

  auto destroys = lr.getDestroys();
  if (destroys.empty() && haveAnyLocalScopes) {
    return false;
  }

  // If we reached this point, then we know that all of our users can accept a
  // guaranteed value and our owned value is destroyed only by a set of
  // destroy_values. Check if:
  //
  // 1. All of our destroys are joint post-dominated by our end borrow scope
  //    set. If they do not, then the copy_value is lifetime extending the
  //    guaranteed value, we can not eliminate it.
  //
  // 2. If all of our destroy_values are dead end. In such a case, the linear
  //    lifetime checker will not perform any checks since it assumes that dead
  //    end destroys can be ignored. Since we are going to end the program
  //    anyways, we want to be conservative here and optimize only if we do not
  //    need to insert an end_borrow since all of our borrow introducers are
  //    non-local scopes.
  {
    bool foundNonDeadEnd = false;
    for (auto *dvi : destroys) {
      foundNonDeadEnd |= !getDeadEndBlocks().isDeadEnd(dvi->getParent());
    }
    if (!foundNonDeadEnd && haveAnyLocalScopes)
      return false;
    SmallVector<SILInstruction *, 8> scratchSpace;
    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
    if (llvm::any_of(borrowScopeIntroducers,
                     [&](BorrowScopeIntroducingValue borrowScope) {
                       return !borrowScope.areInstructionsWithinScope(
                           destroys, scratchSpace, visitedBlocks,
                           getDeadEndBlocks());
                     })) {
      return false;
    }
  }

  // Otherwise, we know that our copy_value/destroy_values are all completely
  // within the guaranteed value scope. First delete the destroys/copies.
  while (!destroys.empty()) {
    auto *dvi = destroys.back();
    destroys = destroys.drop_back();
    eraseInstruction(dvi);
    ++NumEliminatedInsts;
  }

  eraseAndRAUWSingleValueInstruction(cvi, cvi->getOperand());
  convertForwardingInstsFromOwnedToGuaranteed(
      lr.getNonConsumingForwardingInsts());

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

// Handle simple checking where we do not need to form live ranges and visit a
// bunch of instructions.
static bool canSafelyJoinSimpleRange(SILValue cviOperand,
                                     DestroyValueInst *cviOperandDestroy,
                                     CopyValueInst *cvi) {
  // We only handle cases where our copy_value has a single consuming use that
  // is not a forwarding use. We need to use the LiveRange functionality to
  // guarantee correctness in the presence of forwarding uses.
  //
  // NOTE: This use may be any type of consuming use and may not be a
  // destroy_value.
  auto *cviConsumer = cvi->getSingleConsumingUse();
  if (!cviConsumer || isOwnedForwardingInstruction(cviConsumer->getUser())) {
    return false;
  }

  // Ok, we may be able to eliminate this. The main thing we need to be careful
  // of here is that if the destroy_value is /after/ the consuming use of the
  // operand of copy_value, we may have normal uses of the copy_value's operand
  // that would become use-after-frees since we would be shrinking the lifetime
  // of the object potentially. Consider the following SIL:
  //
  //   %0 = ...
  //   %1 = copy_value %0
  //   apply %cviConsumer(%1)
  //   apply %guaranteedUser(%0)
  //   destroy_value %0
  //
  // Easily, if we were to eliminate the copy_value, destroy_value, the object's
  // lifetime could potentially be shrunk before guaranteedUser is executed,
  // causing guaranteedUser to be a use-after-free.
  //
  // As an extra wrinkle, until all interior pointer constructs (e.x.:
  // project_box) are guaranteed to be guaranted by a begin_borrow, we can not
  // in general safely shrink lifetimes. So even if we think we can prove that
  // all non-consuming uses of %0 are before apply %cviConsumer, we may miss
  // implicit uses that are not guarded yet by a begin_borrow, resulting in
  // use-after-frees.
  //
  // With that in mind, we only handle cases today where we can prove that
  // destroy_value is strictly before the consuming use of the operand. This
  // guarantees that we are not shrinking the lifetime of the underlying object.
  //
  // First we handle the simple case: where the cviConsumer is a return inst. In
  // such a case, we know for sure that cviConsumer post-dominates the
  // destroy_value.
  auto cviConsumerIter = cviConsumer->getUser()->getIterator();
  if (isa<ReturnInst>(cviConsumerIter)) {
    return true;
  }

  // Then see if our cviConsumer is in the same block as a return inst and the
  // destroy_value is not. In that case, we know that the cviConsumer must
  // post-dominate the destroy_value.
  auto *cviConsumingBlock = cviConsumerIter->getParent();
  if (isa<ReturnInst>(cviConsumingBlock->getTerminator()) &&
      cviConsumingBlock != cviOperandDestroy->getParent()) {
    return true;
  }

  // Otherwise, we only support joining live ranges where the cvi and the cvi's
  // operand's destroy are in the same block with the destroy_value of cvi
  // operand needing to be strictly after the copy_value. This analysis can be
  // made significantly stronger by using LiveRanges, but this is simple for
  // now.
  auto cviOperandDestroyIter = cviOperandDestroy->getIterator();
  if (cviConsumingBlock != cviOperandDestroyIter->getParent()) {
    return false;
  }

  // TODO: This should really be llvm::find, but for some reason, the templates
  // do not match up given the current state of the iterators. This impl works
  // in a pinch though.
  return llvm::any_of(
      llvm::make_range(cviOperandDestroyIter,
                       cviOperandDestroyIter->getParent()->end()),
      [&](const SILInstruction &val) { return &*cviConsumerIter == &val; });
}

// # The Problem We Are Solving
//
// The main idea here is that we are trying to eliminate the simplest, easiest
// form of live range joining. Consider the following SIL:
//
//   ```
//   %cviOperand = ...                // @owned value
//   %cvi = copy_value %cviOperand    // copy of @owned value
//   ...
//   destroy_value %cviOperandDestroy // destruction of @owned value
//   ...
//   apply %consumingUser(%cvi)       // destruction of copy of @owned value
//   ```
//
// We want to reduce reference count traffic by eliminating the middle
// copy/destroy yielding:
//
//   ```
//   %cviOperand = ...                // @owned value
//   // *eliminated copy_value*
//   ...
//   // *eliminated destroy_value*
//   ...
//   apply %consumingUser(%cviOperand)       // destruction of copy of @owned
//   value
//   ```
//
// # Safety
//
// In order to do this safely, we need to take the union of the two objects
// lifetimes since we are only joining lifetimes. This ensures that we can rely
// on SILGen's correctness on inserting safe lifetimes. To keep this simple
// today we only optimize if the destroy_value and consuming user are in the
// same block and the consuming user is later in the block than the
// destroy_value.
//
// DISCUSSION: The reason why we do not shrink lifetimes today is that all
// interior pointers (e.x. project_box) are properly guarded by
// begin_borrow. Because of that we can not shrink lifetimes and instead rely on
// SILGen's correctness.
bool SemanticARCOptVisitor::tryJoiningCopyValueLiveRangeWithOperand(
    CopyValueInst *cvi) {
  // First do a quick check if our operand is owned. If it is not owned, we can
  // not join live ranges.
  SILValue operand = cvi->getOperand();
  if (operand.getOwnershipKind() != ValueOwnershipKind::Owned) {
    return false;
  }

  // Then check if our operand has a single destroy_value. If it does and that
  // destroy_value is strictly before the consumer of our copy_value in the same
  // block as the consumer of said copy_value then we can always join the live
  // ranges.
  //
  // Example:
  //
  //   ```
  //   %1 = copy_value %0
  //   ...
  //   destroy_value %0
  //   apply %consumingUser(%1)
  //   ```
  // ->
  //
  //   ```
  //   apply %consumingUser(%0)
  //   ```
  //
  // DISCUSSION: We need to ensure that the consuming use of the copy_value is
  // strictly after the destroy_value to ensure that we do not shrink the live
  // range of the operand if the operand has any normal uses beyond our copy
  // value. Otherwise, we could have normal uses /after/ the consuming use of
  // our copy_value.
  if (auto *dvi = operand->getSingleConsumingUserOfType<DestroyValueInst>()) {
    if (canSafelyJoinSimpleRange(operand, dvi, cvi)) {
      eraseInstruction(dvi);
      eraseAndRAUWSingleValueInstruction(cvi, operand);
      NumEliminatedInsts += 2;
      return true;
    }
  }

  // Otherwise, we couldn't handle this case, so return false.
  return false;
}

bool SemanticARCOptVisitor::visitCopyValueInst(CopyValueInst *cvi) {
  // If our copy value inst has only destroy_value users, it is a dead live
  // range. Try to eliminate them.
  if (eliminateDeadLiveRangeCopyValue(cvi)) {
    return true;
  }

  // Then see if copy_value operand's lifetime ends after our copy_value via a
  // destroy_value. If so, we can join their lifetimes.
  if (tryJoiningCopyValueLiveRangeWithOperand(cvi)) {
    return true;
  }

  // Then try to perform the guaranteed copy value optimization.
  if (performGuaranteedCopyValueOptimization(cvi)) {
    return true;
  }

  return false;
}

//===----------------------------------------------------------------------===//
//                         load [copy] Optimizations
//===----------------------------------------------------------------------===//

namespace {

/// A class that computes in a flow insensitive way if we can prove that our
/// storage is either never written to, or is initialized exactly once and never
/// written to again. In both cases, we can convert load [copy] -> load_borrow
/// safely.
class StorageGuaranteesLoadVisitor
  : public AccessUseDefChainVisitor<StorageGuaranteesLoadVisitor>
{
  // The outer SemanticARCOptVisitor.
  SemanticARCOptVisitor &ARCOpt;

  // The live range of the original load.
  const LiveRange &liveRange;

  // The current address being visited.
  SILValue currentAddress;
  
  Optional<bool> isWritten;

public:
  StorageGuaranteesLoadVisitor(SemanticARCOptVisitor &arcOpt, LoadInst *load,
                               const LiveRange &liveRange)
      : ARCOpt(arcOpt), liveRange(liveRange),
        currentAddress(load->getOperand()) {}

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
    // If this load_copy is from an indirect in_guaranteed argument, then we
    // know for sure that it will never be written to.
    if (arg->hasConvention(SILArgumentConvention::Indirect_In_Guaranteed)) {
      return answer(false);
    }

    // If we have an inout parameter that isn't ever actually written to, return
    // false.
    if (arg->getKnownParameterInfo().isIndirectMutating()) {
      return answer(ARCOpt.isAddressWrittenToDefUseAnalysis(arg));
    }

    // TODO: This should be extended:
    //
    // 1. We should be able to analyze in arguments and see if they are only
    //    ever destroyed at the end of the function. In such a case, we may be
    //    able to also to promote load [copy] from such args to load_borrow.
    return answer(true);
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
    SmallVector<SILInstruction *, 4> baseEndBorrows;
    llvm::copy(borrowInst->getUsersOfType<EndBorrowInst>(),
               std::back_inserter(baseEndBorrows));

    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
    LinearLifetimeChecker checker(visitedBlocks, ARCOpt.getDeadEndBlocks());
    // Returns true on success. So we invert.
    bool foundError = !checker.validateLifetime(baseObject, baseEndBorrows,
                                                liveRange.getDestroys());
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

  /// See if we have an alloc_stack that is only written to once by an
  /// initializing instruction.
  void visitStackAccess(AllocStackInst *stack) {
    SmallVector<SILInstruction *, 8> destroyAddrs;
    bool initialAnswer = isSingleInitAllocStack(stack, destroyAddrs);
    if (!initialAnswer)
      return answer(true);

    // Then make sure that all of our load [copy] uses are within the
    // destroy_addr.
    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
    LinearLifetimeChecker checker(visitedBlocks, ARCOpt.getDeadEndBlocks());
    // Returns true on success. So we invert.
    bool foundError = !checker.validateLifetime(
        stack, destroyAddrs /*consuming users*/,
        liveRange.getDestroys() /*non consuming users*/);
    return answer(foundError);
  }

  bool doIt() {
    while (currentAddress) {
      visit(currentAddress);
    }
    return *isWritten;
  }
};

} // namespace

bool SemanticARCOptVisitor::isWrittenTo(LoadInst *load, const LiveRange &lr) {
  StorageGuaranteesLoadVisitor visitor(*this, load, lr);
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
  LiveRange lr(li);
  if (lr.hasConsumingUse())
    return false;

  // Then check if our address is ever written to. If it is, then we cannot use
  // the load_borrow because the stored value may be released during the loaded
  // value's live range.
  if (isWrittenTo(li, lr))
    return false;

  // Ok, we can perform our optimization. Convert the load [copy] into a
  // load_borrow.
  auto *lbi =
      SILBuilderWithScope(li).createLoadBorrow(li->getLoc(), li->getOperand());

  // Since we are looking through forwarding uses that can accept guaranteed
  // parameters, we can have multiple destroy_value along the same path. We need
  // to find the post-dominating block set of these destroy value to ensure that
  // we do not insert multiple end_borrow.
  assert(lifetimeFrontier.empty());
  auto destroyValues = lr.getDestroys();
  ValueLifetimeAnalysis analysis(li, destroyValues);
  bool foundCriticalEdges = !analysis.computeFrontier(
      lifetimeFrontier, ValueLifetimeAnalysis::DontModifyCFG,
      &getDeadEndBlocks());
  (void)foundCriticalEdges;
  assert(!foundCriticalEdges);
  auto loc = RegularLocation::getAutoGeneratedLocation();
  while (!lifetimeFrontier.empty()) {
    auto *insertPoint = lifetimeFrontier.pop_back_val();
    SILBuilderWithScope builder(insertPoint);
    builder.createEndBorrow(loc, lbi);
  }

  // Then delete all of our destroy_value.
  while (!destroyValues.empty()) {
    auto *dvi = destroyValues.back();
    destroyValues = destroyValues.drop_back();
    eraseInstruction(dvi);
    ++NumEliminatedInsts;
  }

  // RAUW our other uses from the load to the load_borrow.
  eraseAndRAUWSingleValueInstruction(li, lbi);

  // And then change the ownership all of our owned forwarding users to be
  // guaranteed.
  convertForwardingInstsFromOwnedToGuaranteed(
      lr.getNonConsumingForwardingInsts());

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

    // Return early if we are not performing OSSA optimizations.
    if (!f.getModule().getOptions().EnableOSSAOptimizations)
      return;

    // Make sure we are running with ownership verification enabled.
    assert(f.getModule().getOptions().VerifySILOwnership &&
           "Can not perform semantic arc optimization unless ownership "
           "verification is enabled");

    SemanticARCOptVisitor visitor(f);

    // Add all the results of all instructions that we want to visit to the
    // worklist.
    for (auto &block : f) {
      for (auto &inst : block) {
        if (SemanticARCOptVisitor::shouldVisitInst(&inst)) {
          for (SILValue v : inst.getResults()) {
            visitor.worklist.insert(v);
          }
        }
      }
    }

    // Then process the worklist. We only destroy instructions, so invalidate
    // that. Once we modify the ownership of block arguments, we will need to
    // perhaps invalidate branches as well.
    if (visitor.processWorklist()) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createSemanticARCOpts() { return new SemanticARCOpts(); }
