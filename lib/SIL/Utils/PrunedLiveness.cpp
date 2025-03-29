//===--- PrunedLiveness.cpp - Compute liveness from selected uses ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/PrunedLiveness.h"
#include "swift/AST/TypeExpansionContext.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/ScopedAddressUtils.h"
#include "swift/SIL/Test.h"

using namespace swift;

void PrunedLiveBlocks::computeUseBlockLiveness(SILBasicBlock *userBB) {
  // If, we are visiting this block, then it is not already LiveOut. Mark it
  // LiveWithin to indicate a liveness boundary within the block.
  markBlockLive(userBB, LiveWithin);

  BasicBlockWorklist worklist(userBB->getFunction());
  worklist.push(userBB);

  while (auto *block = worklist.pop()) {
    // The popped `bb` is live; now mark all its predecessors LiveOut.
    //
    // Traversal terminates at any previously visited block, including the
    // blocks initialized as definition blocks.
    for (auto *predBlock : block->getPredecessorBlocks()) {
      switch (getBlockLiveness(predBlock)) {
      case Dead:
        worklist.pushIfNotVisited(predBlock);
        LLVM_FALLTHROUGH;
      case LiveWithin:
        markBlockLive(predBlock, LiveOut);
        break;
      case LiveOut:
        break;
      }
    }
  }
}

//===----------------------------------------------------------------------===//
//                    PrunedLiveBlocks and PrunedLiveness
//===----------------------------------------------------------------------===//

llvm::StringRef PrunedLiveBlocks::getStringRef(IsLive isLive) const {
  switch (isLive) {
  case Dead:
    return "Dead";
  case LiveWithin:
    return "LiveWithin";
  case LiveOut:
    return "LiveOut";
  }
}

void PrunedLiveBlocks::print(llvm::raw_ostream &OS) const {
  if (!discoveredBlocks) {
    OS << "No deterministic live block list\n";
    return;
  }
  SmallVector<IsLive, 8> isLive;
  for (auto *block : *discoveredBlocks) {
    block->printAsOperand(OS);
    OS << ": " << getStringRef(this->getBlockLiveness(block)) << "\n";
  }
}

void PrunedLiveBlocks::dump() const {
  print(llvm::dbgs());
}

void PrunedLiveness::print(llvm::raw_ostream &OS) const {
  liveBlocks.print(OS);
  for (auto &userAndIsLifetimeEnding : users) {
    switch (userAndIsLifetimeEnding.second) {
    case LifetimeEnding::Value::NonUse:
      OS << "non-user: ";
      break;
    case LifetimeEnding::Value::Ending:
      OS << "lifetime-ending user: ";
      break;
    case LifetimeEnding::Value::NonEnding:
      OS << "regular user: ";
      break;
    }
    userAndIsLifetimeEnding.first->print(OS);
  }
}

void PrunedLiveness::dump() const {
  print(llvm::dbgs());
}

//===----------------------------------------------------------------------===//
//                           PrunedLivenessBoundary
//===----------------------------------------------------------------------===//

void PrunedLivenessBoundary::print(llvm::raw_ostream &OS) const {
  for (auto *user : lastUsers) {
    OS << "last user: " << *user;
  }
  for (auto *block : boundaryEdges) {
    OS << "boundary edge: ";
    block->printAsOperand(OS);
    OS << "\n";
  }
  if (!deadDefs.empty()) {
    for (auto *deadDef : deadDefs) {
      OS << "dead def: " << *deadDef;
    }
  }
}

void PrunedLivenessBoundary::dump() const {
  print(llvm::dbgs());
}

void PrunedLivenessBoundary::visitInsertionPoints(
    llvm::function_ref<void(SILBasicBlock::iterator insertPt)> visitor,
    DeadEndBlocks *deBlocks) {
  // Control flow merge blocks used as insertion points.
  SmallPtrSet<SILBasicBlock *, 4> mergeBlocks;

  for (SILInstruction *user : lastUsers) {
    if (!isa<TermInst>(user)) {
      visitor(std::next(user->getIterator()));
      continue;
    }
    auto *predBB = user->getParent();
    for (SILBasicBlock *succ : predBB->getSuccessors()) {
      if (!succ->getSinglePredecessorBlock()) {
        assert(predBB->getSingleSuccessorBlock() == succ);
        if (!mergeBlocks.insert(succ).second) {
          continue;
        }
      } else {
        assert(succ->getSinglePredecessorBlock() == predBB);
      }
      if (deBlocks && deBlocks->isDeadEnd(succ))
        continue;

      visitor(succ->begin());
    }
  }
  for (SILBasicBlock *edge : boundaryEdges) {
    if (deBlocks && deBlocks->isDeadEnd(edge))
      continue;

    visitor(edge->begin());
  }
  for (SILNode *deadDef : deadDefs) {
    if (auto *arg = dyn_cast<SILArgument>(deadDef))
      visitor(arg->getParent()->begin());
    else
      visitor(std::next(cast<SILInstruction>(deadDef)->getIterator()));
  }
}

namespace swift::test {
// Arguments:
// - variadic list of - instruction: a last user
// Dumps:
// - the insertion points
static FunctionTest
    PrunedLivenessBoundaryWithListOfLastUsersInsertionPointsTest(
        "pruned_liveness_boundary_with_list_of_last_users_insertion_points",
        [](auto &function, auto &arguments, auto &test) {
          PrunedLivenessBoundary boundary;
          while (arguments.hasUntaken()) {
            boundary.lastUsers.push_back(arguments.takeInstruction());
          }
          boundary.visitInsertionPoints([](SILBasicBlock::iterator point) {
            point->print(llvm::outs());
          });
        });
} // end namespace swift::test

//===----------------------------------------------------------------------===//
//                              PrunedLiveRange
//===----------------------------------------------------------------------===//

static PrunedLiveness::LifetimeEnding
branchMeet(PrunedLiveness::LifetimeEnding const lhs,
           PrunedLiveness::LifetimeEnding const rhs) {
  enum BranchLifetimeEnding {
    Ending,
    NonEnding,
    NonUse,
  };
  auto toBranch =
      [](PrunedLiveness::LifetimeEnding const ending) -> BranchLifetimeEnding {
    switch (ending) {
    case PrunedLiveness::LifetimeEnding::Value::NonEnding:
      return NonEnding;
    case PrunedLiveness::LifetimeEnding::Value::Ending:
      return Ending;
    case PrunedLiveness::LifetimeEnding::Value::NonUse:
      return NonUse;
    }
  };
  auto toRegular =
      [](BranchLifetimeEnding const ending) -> PrunedLiveness::LifetimeEnding {
    switch (ending) {
    case NonEnding:
      return PrunedLiveness::LifetimeEnding::Value::NonEnding;
    case Ending:
      return PrunedLiveness::LifetimeEnding::Value::Ending;
    case NonUse:
      return PrunedLiveness::LifetimeEnding::Value::NonUse;
    }
  };
  return toRegular(std::min(toBranch(lhs), toBranch(rhs)));
}
static void branchMeetInPlace(PrunedLiveness::LifetimeEnding &that,
                              PrunedLiveness::LifetimeEnding const other) {
  that = branchMeet(that, other);
}

template <typename LivenessWithDefs>
void PrunedLiveRange<LivenessWithDefs>::updateForUse(
    SILInstruction *user,
    PrunedLiveRange<LivenessWithDefs>::LifetimeEnding lifetimeEnding) {
  liveBlocks.updateForUse(user, asImpl().isUserBeforeDef(user));

  // Note that a user may use the current value from multiple operands. If any
  // of the uses are non-lifetime-ending, then we must consider the user
  // itself non-lifetime-ending; it cannot be a final destroy point because
  // the value of the non-lifetime-ending operand must be kept alive until the
  // end of the user. Consider a call that takes the same value using
  // different conventions:
  //
  //   apply %f(%val, %val) : $(@guaranteed, @owned) -> ()
  //
  // This call is not considered the end of %val's lifetime. The @owned
  // argument must be copied.
  auto iterAndSuccess = users.insert({user, lifetimeEnding});
  if (!iterAndSuccess.second) {
    if (isa<BranchInst>(user)) {
      branchMeetInPlace(iterAndSuccess.first->second, lifetimeEnding);
    } else {
      iterAndSuccess.first->second.meetInPlace(lifetimeEnding);
    }
  }
}
template <typename LivenessWithDefs>
void PrunedLiveRange<LivenessWithDefs>::updateForUse(SILInstruction *user,
                                                     bool lifetimeEnding) {
  updateForUse(user, LifetimeEnding::forUse(lifetimeEnding));
}

template <typename LivenessWithDefs>
void PrunedLiveRange<LivenessWithDefs>::extendToNonUse(SILInstruction *inst) {
  updateForUse(inst, LifetimeEnding::Value::NonUse);
}

template <typename LivenessWithDefs>
InnerBorrowKind
PrunedLiveRange<LivenessWithDefs>::updateForBorrowingOperand(Operand *operand) {
  assert(operand->getOperandOwnership() == OperandOwnership::Borrow);

  // A nested borrow scope is considered a use-point at each scope ending
  // instruction.
  //
  // Note: Ownership liveness should follow reborrows that are dominated by the
  // ownership definition.
  auto innerBorrowKind = InnerBorrowKind::Contained;
  BorrowingOperand(operand).visitScopeEndingUses(
    [&](Operand *end) {
      if (end->getOperandOwnership() == OperandOwnership::Reborrow) {
        innerBorrowKind = InnerBorrowKind::Reborrowed;
      }
      updateForUse(end->getUser(), /*lifetimeEnding*/ false);
      return true;
    }, [&](Operand *unknownUse) {
      updateForUse(unknownUse->getUser(), /*lifetimeEnding*/ false);
      innerBorrowKind = InnerBorrowKind::Escaped;
      return true;
    });
  return innerBorrowKind;
}

template <typename LivenessWithDefs>
AddressUseKind PrunedLiveRange<LivenessWithDefs>::checkAndUpdateInteriorPointer(
    Operand *operand) {
  assert(operand->getOperandOwnership() == OperandOwnership::InteriorPointer
         || operand->getOperandOwnership() == OperandOwnership::AnyInteriorPointer);

  if (auto scopedAddress = ScopedAddressValue::forUse(operand)) {
    scopedAddress.visitScopeEndingUses([this](Operand *end) {
      updateForUse(end->getUser(), /*lifetimeEnding*/ false);
      return true;
    });
    return AddressUseKind::NonEscaping;
  }
  // FIXME: findTransitiveUses should be a visitor so we're not recursively
  // allocating use vectors and potentially merging the use points.
  SmallVector<Operand *, 8> uses;
  auto useKind = InteriorPointerOperand(operand).findTransitiveUses(&uses);
  for (auto *use : uses) {
    updateForUse(use->getUser(), /*lifetimeEnding*/ false);
  }
  if (uses.empty()) {
    // Handle a dead address
    updateForUse(operand->getUser(), /*lifetimeEnding*/ false);
  }
  return useKind;
}

template <typename LivenessWithDefs>
void PrunedLiveRange<LivenessWithDefs>::extendAcrossLiveness(
    PrunedLiveness &otherLiveness) {
  // update this liveness for all the interesting users in otherLiveness.
  for (std::pair<SILInstruction *, LifetimeEnding> userAndEnd :
       otherLiveness.getAllUsers()) {
    updateForUse(userAndEnd.first, userAndEnd.second);
  }
}

template <typename LivenessWithDefs>
LiveRangeSummary PrunedLiveRange<LivenessWithDefs>::updateForDef(SILValue def) {
  ValueSet visited(def->getFunction());
  return recursivelyUpdateForDef(def, visited, def);
}

template <typename LivenessWithDefs>
LiveRangeSummary PrunedLiveRange<LivenessWithDefs>::recursivelyUpdateForDef(
    SILValue initialDef, ValueSet &visited, SILValue value) {
  LiveRangeSummary summary;

  if (!visited.insert(value))
    return summary;

  // Note: Uses with OperandOwnership::NonUse cannot be considered normal uses
  // for liveness. Otherwise, liveness would need to separately track non-uses
  // everywhere. Non-uses cannot be treated like normal non-lifetime-ending uses
  // because they can occur on both applies, which need to extend liveness to
  // the return point, and on forwarding instructions, like
  // init_existential_ref, which need to consume their use even when
  // type-dependent operands exist.
  for (Operand *use : value->getUses()) {
    switch (use->getOperandOwnership()) {
    case OperandOwnership::NonUse:
      break;
    case OperandOwnership::Borrow:
      summary.meet(updateForBorrowingOperand(use));
      break;
    case OperandOwnership::PointerEscape:
      summary.meet(AddressUseKind::PointerEscape);
      break;
    case OperandOwnership::InteriorPointer:
    case OperandOwnership::AnyInteriorPointer:
      summary.meet(checkAndUpdateInteriorPointer(use));
      break;
    case OperandOwnership::GuaranteedForwarding: {
      updateForUse(use->getUser(), /*lifetimeEnding*/false);
      if (auto phiOper = PhiOperand(use)) {
        SILValue phi = phiOper.getValue();
        // If 'def' is any of the enclosing defs, then it must dominate the phi
        // and all phi uses should be handled recursively.
        if (!visitEnclosingDefs(phi, [initialDef](SILValue enclosingDef) {
          return enclosingDef != initialDef;
        })) {
          // At least one enclosing def was 'def'.
          summary.meet(recursivelyUpdateForDef(initialDef, visited, phi));
        }
        // Otherwise all enclosing defs are protected by separate reborrow
        // scopes, which are not included in "simple" liveness.
        break;
      }
      ForwardingOperand(use).visitForwardedValues([&](SILValue result) {
        // Do not include transitive uses with 'none' ownership
        if (result->getOwnershipKind() != OwnershipKind::None) {
          summary.meet(recursivelyUpdateForDef(initialDef, visited, result));
        }
        return true;
      });
      break;
    }
    case OperandOwnership::TrivialUse: {
      if (auto scopedAddress = ScopedAddressValue::forUse(use)) {
        scopedAddress.visitScopeEndingUses([this](Operand *end) {
          updateForUse(end->getUser(), /*lifetimeEnding*/false);
          return true;
        });
      }
      updateForUse(use->getUser(), /*lifetimeEnding*/false);
      break;
    }
    default:
      // Note: An outer reborrow ends the outer lifetime here.
      updateForUse(use->getUser(), use->isLifetimeEnding());
      break;
    }
  }
  return summary;
}

namespace swift::test {
// Arguments:
// - SILValue: value to a analyze
// Dumps:
// - the liveness result and boundary
static FunctionTest SSALivenessTest("ssa_liveness", [](auto &function,
                                                       auto &arguments,
                                                       auto &test) {
  auto value = arguments.takeValue();
  assert(!arguments.hasUntaken());
  llvm::outs() << "SSA lifetime analysis: " << value;

  SmallVector<SILBasicBlock *, 8> discoveredBlocks;
  SSAPrunedLiveness liveness(value->getFunction(), &discoveredBlocks);
  liveness.initializeDef(value);
  LiveRangeSummary summary = liveness.computeSimple();
  if (summary.innerBorrowKind == InnerBorrowKind::Reborrowed)
    llvm::outs() << "Incomplete liveness: Reborrowed inner scope\n";

  if (summary.addressUseKind == AddressUseKind::PointerEscape)
    llvm::outs() << "Incomplete liveness: Escaping address\n";
  else if (summary.addressUseKind == AddressUseKind::Unknown)
    llvm::outs() << "Incomplete liveness: Unknown address use\n";

  liveness.print(llvm::outs());

  PrunedLivenessBoundary boundary;
  liveness.computeBoundary(boundary);
  boundary.print(llvm::outs());
});

// Arguments:
// - SILValue: def whose pruned liveness will be calculated
// - the string "uses:"
// - variadic list of live-range user instructions
// Dumps:
// -
static FunctionTest SSAUseLivenessTest("ssa_use_liveness", [](auto &function,
                                                              auto &arguments,
                                                              auto &test) {
  auto value = arguments.takeValue();
  SmallVector<SILBasicBlock *, 8> discoveredBlocks;
  SSAPrunedLiveness liveness(&function, &discoveredBlocks);
  liveness.initializeDef(value);

  auto argument = arguments.takeArgument();
  if (cast<StringArgument>(argument).getValue() != "uses:") {
    llvm::report_fatal_error("test specification expects the 'uses:' label\n");
  }

  while (arguments.hasUntaken()) {
    auto *inst = arguments.takeInstruction();
    auto kindString = arguments.takeString();
    enum Kind {
      NonUse,
      Ending,
      NonEnding,
    };
    auto kind = llvm::StringSwitch<std::optional<Kind>>(kindString)
                    .Case("non-use", Kind::NonUse)
                    .Case("ending", Kind::Ending)
                    .Case("non-ending", Kind::NonEnding)
                    .Default(std::nullopt);
    if (!kind.has_value()) {
      llvm::errs() << "Unknown kind: " << kindString << "\n";
      llvm::report_fatal_error("Bad user kind.  Value must be one of "
                               "'non-use', 'ending', 'non-ending'");
    }
    switch (kind.value()) {
    case Kind::NonUse:
      liveness.extendToNonUse(inst);
      break;
    case Kind::Ending:
      liveness.updateForUse(inst, /*lifetimeEnding*/ true);
      break;
    case Kind::NonEnding:
      liveness.updateForUse(inst, /*lifetimeEnding*/ false);
      break;
    }
  }

  liveness.print(llvm::outs());

  PrunedLivenessBoundary boundary;
  liveness.computeBoundary(boundary);
  boundary.print(llvm::outs());
});

} // end namespace swift::test

template <typename LivenessWithDefs>
bool PrunedLiveRange<LivenessWithDefs>::isWithinLivenessBoundary(
    SILInstruction *inst) const {
  assert(asImpl().isInitialized());

  auto *block = inst->getParent();
  auto blockLiveness = getBlockLiveness(block);
  if (blockLiveness == PrunedLiveBlocks::Dead)
    return false;

  bool isLive = blockLiveness == PrunedLiveBlocks::LiveOut;

  if (isLive && !asImpl().isDefBlock(block))
    return true;

  return isInstructionLive(inst, isLive);
}

template <typename LivenessWithDefs>
bool PrunedLiveRange<LivenessWithDefs>::isInstructionLive(SILInstruction *inst,
                                                          bool isLive) const {
  auto *block = inst->getParent();
  // Check if instruction is between a last use and a definition
  for (SILInstruction &it : llvm::reverse(*block)) {
    // the def itself is not within the boundary, so cancel liveness before
    // matching 'inst'.
    if (asImpl().isDef(&it)) {
      isLive = false;
    }
    if (&it == inst) {
      return isLive;
    }
    if (!isLive && isInterestingUser(&it)) {
      isLive = true;
    }
  }
  llvm_unreachable("instruction must be in its parent block");
}

template <typename LivenessWithDefs>
bool PrunedLiveRange<LivenessWithDefs>::isAvailableOut(
    SILBasicBlock *block, DeadEndBlocks &deadEndBlocks) const {
  assert(getBlockLiveness(block) == PrunedLiveBlocks::LiveWithin);
  assert(deadEndBlocks.isDeadEnd(block));
  for (SILInstruction &inst : llvm::reverse(*block)) {
    if (asImpl().isDef(&inst)) {
      return true;
    }
    switch (isInterestingUser(&inst)) {
    case PrunedLiveness::NonUser:
      continue;
    case PrunedLiveness::NonLifetimeEndingUse:
      return true;
    case PrunedLiveness::LifetimeEndingUse:
      return false;
    }
  }
  assert(asImpl().isDefBlock(block));
  assert(llvm::any_of(block->getArguments(), [this](SILArgument *arg) {
    return asImpl().isDef(arg);
  }));
  return true;
}

template <typename LivenessWithDefs>
bool PrunedLiveRange<LivenessWithDefs>::isInstructionAvailable(
    SILInstruction *user, DeadEndBlocks &deadEndBlocks) const {
  auto *parent = user->getParent();
  assert(getBlockLiveness(parent) == PrunedLiveBlocks::LiveWithin);
  assert(deadEndBlocks.isDeadEnd(parent));
  return isInstructionLive(user, isAvailableOut(parent, deadEndBlocks));
}

template <typename LivenessWithDefs>
bool PrunedLiveRange<LivenessWithDefs>::isWithinBoundary(
    SILInstruction *inst, DeadEndBlocks *deadEndBlocks) const {
  if (deadEndBlocks) {
    return asImpl().isWithinExtendedBoundary(inst, *deadEndBlocks);
  } else {
    return asImpl().isWithinLivenessBoundary(inst);
  }
}

template <typename LivenessWithDefs>
bool PrunedLiveRange<LivenessWithDefs>::isWithinExtendedBoundary(
    SILInstruction *inst, DeadEndBlocks &deadEndBlocks) const {
  // A value has a pruned live region, a live region and an available region.
  // (Note: PrunedLiveness does not distinguish between the pruned live region
  // and the live region; the pruned live region coincides with the live region
  // whenever consuming uses are considered.) This method refers to a FOURTH
  // region: the "extended region" which MAY be different from the others.
  // (Terminological note: this isn't intended to gain regular usage, hence its
  // lack of specificity.)
  //
  // Before _defining_ the extended region, consider the following example:
  //
  //   def = ...
  //   inst_1
  //   use %def // added to pruned liveness
  //   inst_2
  //   cond_br %c1, die, normal
  // die:
  //   inst_3
  //   unreachable
  // normal:
  //   inst_4
  //   destroy %def // NOT added to pruned liveness
  //   inst_5
  //
  // This table describes which regions the `inst_i`s are in:
  // +------+----+------+--------+---------+
  // |      |live|pruned|extended|available|
  // +------+----+------+--------+---------+
  // |inst_1| yes| yes  | yes    | yes     |
  // +------+----+------+--------+---------+
  // |inst_2| yes| no   | yes    | yes     |
  // +------+----+------+--------+---------+
  // |inst_3| no | no   | yes    | yes     |
  // +------+----+------+--------+---------+
  // |inst_4| yes| no   | no     | yes     |
  // +------+----+------+--------+---------+
  // |inst_5| no | no   | no     | no      |
  // +------+----+------+--------+---------+
  //
  // This example demonstrates that
  //     pruned live ≠ extended ≠ available
  // and indicates the fact that
  //     pruned live ⊆ extended ⊆ available
  //
  // The "extended region" is the pruned live region availability-extended into
  // dead-end regions.  In more detail, it's obtained by (1) unioning the
  // dead-end regions adjacent to the pruned live region (the portions of those
  // adjacent dead-end regions which are forward reachable from the pruned live
  // region) and (2) intersecting the result with the availability region.
  //
  // That this region is of interest is another result of lacking complete
  // OSSA lifetimes.

  if (asImpl().isWithinLivenessBoundary(inst)) {
    // The extended region is a superset of the pruned live region.
    return true;
  }

  SILBasicBlock *parent = inst->getParent();
  if (!deadEndBlocks.isDeadEnd(parent)) {
    // The extended region intersected with the non-dead-end region is equal to
    // the pruned live region.
    return false;
  }
  switch (liveBlocks.getBlockLiveness(parent)) {
  case PrunedLiveBlocks::Dead:
    break;
  case PrunedLiveBlocks::LiveWithin:
    // Dead defs may result in LiveWithin but AvailableOut blocks.
    return isInstructionAvailable(inst, deadEndBlocks);
  case PrunedLiveBlocks::LiveOut:
    // The instruction is not within the boundary, but its parent is LiveOut;
    // therefore it must be a def block.
    assert(asImpl().isDefBlock(parent));

    // Where within the block might the instruction be?
    // - before the first def: return false (outside the extended region).
    // - between a def and a use: unreachable (withinBoundary would have
    //   returned true).
    // - between a def and another def: unreachable (withinBoundary would have
    //   returned true)
    // - between a use and a def: return false (outside the extended region).
    // - after the final def: unreachable (withinBoundary would have returned
    //   true)
    return false;
  }
  // Check whether `parent` is in the extended region: walk backwards within
  // the dead portion of the dead-end region up _through_ the first block which
  // is either not dead or not dead-end.
  //
  // During the walk, if ANY reached block satisfies one of
  // (1) dead-end, LiveWithin, !AvailableOut
  // (2) NOT dead-end, NOT LiveOut
  // then the `parent` is not in the extended region.
  //
  // Otherwise, ALL reached blocks satisfied one of the following:
  // (a) dead-end, Dead
  // (b) dead-end, LiveWithin, AvailableOut
  // (b) MAYBE dead-end, LiveOut
  // In this case, `parent` is in the extended region.
  BasicBlockWorklist worklist(parent->getFunction());
  worklist.push(parent);
  while (auto *block = worklist.pop()) {
    auto isLive = liveBlocks.getBlockLiveness(block);
    if (!deadEndBlocks.isDeadEnd(block)) {
      // The first block beyond the dead-end region has been reached.
      if (isLive != PrunedLiveBlocks::LiveOut) {
        // Cases (2) above.
        return false;
      }
      // Stop walking.  (No longer in the dead portion of the dead-end region.)
      continue;
    }
    switch (isLive) {
    case PrunedLiveBlocks::Dead:
      // Still within the dead portion of the dead-end region.  Keep walking.
      for (auto *predecessor : block->getPredecessorBlocks()) {
        worklist.pushIfNotVisited(predecessor);
      }
      continue;
    case PrunedLiveBlocks::LiveWithin:
      // Availability may have ended in this block.  Check whether the block is
      // "AvailableOut".
      if (!isAvailableOut(block, deadEndBlocks)) {
        // Case (1) above.
        return false;
      }
      // Stop walking.  (No longer in the dead portion of the dead-end region.)
      continue;
    case PrunedLiveBlocks::LiveOut:
      // Stop walking.  (No longer in the dead portion of the dead-end region.)
      continue;
    }
  }
  return true;
}

namespace swift::test {
// Arguments:
// - string: "def:"
// - SILValue: value to be analyzed
// - string: "liveness-uses:"
// - variadic list of - SILInstruction: user to pass to updateForUse
//                    -         string: non-ending/ending/non-use
// - string: "uses:"
// - variadic list of - SILInstruction: the instruction to pass to
// areUsesWithinBoundary Dumps:
// - true/false
static FunctionTest SSAPrunedLiveness__areUsesWithinBoundary(
    "SSAPrunedLiveness__areUsesWithinBoundary",
    [](auto &function, auto &arguments, auto &test) {
      SmallVector<SILBasicBlock *, 8> discoveredBlocks;
      SSAPrunedLiveness liveness(&function, &discoveredBlocks);

      llvm::outs() << "SSAPrunedLiveness:\n";

      if (arguments.takeString() != "def:") {
        llvm::report_fatal_error("test expects the 'def:' label\n");
      }
      auto def = arguments.takeValue();
      liveness.initializeDef(def);
      llvm::outs() << "\tdef: " << def;
      if (arguments.takeString() != "liveness-uses:") {
        llvm::report_fatal_error("test expects the 'def:' label\n");
      }
      llvm::outs() << "\tuses:\n";
      while (true) {
        auto argument = arguments.takeArgument();
        if (isa<StringArgument>(argument)) {
          auto string = cast<StringArgument>(argument);
          if (string.getValue() != "uses:") {
            llvm::report_fatal_error("test expects the 'inst:' label\n");
          }
          break;
        }
        auto *instruction = cast<InstructionArgument>(argument).getValue();
        auto string = arguments.takeString();
        PrunedLiveness::LifetimeEnding::Value kind =
            llvm::StringSwitch<PrunedLiveness::LifetimeEnding::Value>(string)
                .Case("non-ending",
                      PrunedLiveness::LifetimeEnding::Value::NonEnding)
                .Case("ending", PrunedLiveness::LifetimeEnding::Value::Ending)
                .Case("non-use", PrunedLiveness::LifetimeEnding::Value::NonUse);

        llvm::outs() << "\t\t" << string << " " << *instruction;
        liveness.updateForUse(instruction, kind);
      }
      liveness.print(llvm::outs());

      PrunedLivenessBoundary boundary;
      liveness.computeBoundary(boundary);
      boundary.print(llvm::outs());

      llvm::outs() << "\noperands:\n";
      SmallVector<Operand *, 4> operands;
      while (arguments.hasUntaken()) {
        auto *operand = arguments.takeOperand();
        operands.push_back(operand);
        operand->print(llvm::outs());
      }

      auto result =
          liveness.areUsesWithinBoundary(operands, test.getDeadEndBlocks());

      llvm::outs() << "RESULT: " << StringRef(result ? "true" : "false")
                   << "\n";
    });
} // end namespace swift::test

template <typename LivenessWithDefs>
bool PrunedLiveRange<LivenessWithDefs>::areUsesWithinBoundary(
    ArrayRef<Operand *> uses, DeadEndBlocks *deadEndBlocks) const {
  assert(asImpl().isInitialized());

  for (auto *use : uses) {
    auto *user = use->getUser();
    if (!isWithinBoundary(user, deadEndBlocks))
      return false;
  }
  return true;
}

template <typename LivenessWithDefs>
bool PrunedLiveRange<LivenessWithDefs>::areUsesOutsideBoundary(
    ArrayRef<Operand *> uses, DeadEndBlocks *deadEndBlocks) const {
  assert(asImpl().isInitialized());

  for (auto *use : uses) {
    auto *user = use->getUser();
    if (isWithinBoundary(user, deadEndBlocks))
      return false;
  }
  return true;
}

template <typename LivenessWithDefs>
void PrunedLiveRange<LivenessWithDefs>::computeBoundary(
    AnyPrunedLivenessBoundary &boundary) const {
  assert(asImpl().isInitialized());

  for (SILBasicBlock *block : getDiscoveredBlocks()) {
    // Process each block that has not been visited and is not LiveOut.
    switch (getBlockLiveness(block)) {
    case PrunedLiveBlocks::LiveOut:
      for (SILBasicBlock *succBB : block->getSuccessors()) {
        if (getBlockLiveness(succBB) == PrunedLiveBlocks::Dead) {
          boundary.boundaryEdges.push_back(succBB);
        }
      }
      asImpl().findBoundariesInBlock(block, /*isLiveOut*/ true, boundary);
      break;
    case PrunedLiveBlocks::LiveWithin: {
      asImpl().findBoundariesInBlock(block, /*isLiveOut*/ false, boundary);
      break;
    }
    case PrunedLiveBlocks::Dead:
      llvm_unreachable("All discovered blocks must be live");
    }
  }
}

template <typename LivenessWithDefs>
void PrunedLiveRange<LivenessWithDefs>::computeBoundary(
    PrunedLivenessBoundary &boundary,
    ArrayRef<SILBasicBlock *> postDomBlocks) const {
  assert(asImpl().isInitialized());

  if (postDomBlocks.empty())
    return; // all paths must be dead-ends or infinite loops

  BasicBlockWorklist blockWorklist(postDomBlocks[0]->getParent());

  // Visit each post-dominating block as the starting point for a
  // backward CFG traversal.
  for (auto *block : postDomBlocks) {
    blockWorklist.pushIfNotVisited(block);
  }
  while (auto *block = blockWorklist.pop()) {
    // Process each block that has not been visited and is not LiveOut.
    switch (getBlockLiveness(block)) {
    case PrunedLiveBlocks::LiveOut:
      asImpl().findBoundariesInBlock(block, /*isLiveOut*/ true, boundary);
      break;
    case PrunedLiveBlocks::LiveWithin: {
      asImpl().findBoundariesInBlock(block, /*isLiveOut*/ false, boundary);
      break;
    }
    case PrunedLiveBlocks::Dead:
      // Continue searching upward to find the pruned liveness boundary.
      for (auto *predBB : block->getPredecessorBlocks()) {
        if (getBlockLiveness(predBB) == PrunedLiveBlocks::LiveOut) {
          boundary.boundaryEdges.push_back(block);
        } else {
          blockWorklist.pushIfNotVisited(predBB);
        }
      }
      break;
    }
  }
}

namespace swift {
template class PrunedLiveRange<SSAPrunedLiveness>;
template class PrunedLiveRange<MultiDefPrunedLiveness>;
} // namespace swift

//===----------------------------------------------------------------------===//
//                             SSAPrunedLiveness
//===----------------------------------------------------------------------===//

/// Given live-within (non-live-out) \p block, find the last user.
void PrunedLivenessBoundary::findBoundaryInNonDefBlock(
    SILBasicBlock *block, const PrunedLiveness &liveness) {
  assert(liveness.getBlockLiveness(block) == PrunedLiveBlocks::LiveWithin);

  for (SILInstruction &inst : llvm::reverse(*block)) {
    if (liveness.isInterestingUser(&inst)) {
      lastUsers.push_back(&inst);
      return;
    }
  }
  llvm_unreachable("live-within block must contain an interesting use");
}

void PrunedLivenessBlockBoundary::findBoundaryInNonDefBlock(
    SILBasicBlock *block, const PrunedLiveness &liveness) {
  assert(liveness.getBlockLiveness(block) == PrunedLiveBlocks::LiveWithin);

  endBlocks.push_back(block);
}

/// Given a live-within \p block that contains an SSA definition, and knowledge
/// that all live uses are dominated by that single definition, find either the
/// last user or a dead def.
///
/// A live range with a single definition cannot have any uses above that
/// definition in the same block. This even holds for unreachable self-loops.
void PrunedLivenessBoundary::findBoundaryInSSADefBlock(
    SILNode *ssaDef, const PrunedLiveness &liveness) {
  // defInst is null for argument defs.
  SILInstruction *defInst = dyn_cast<SILInstruction>(ssaDef);
  for (SILInstruction &inst : llvm::reverse(*ssaDef->getParentBlock())) {
    if (&inst == defInst) {
      deadDefs.push_back(cast<SILNode>(&inst));
      return;
    }
    if (liveness.isInterestingUser(&inst)) {
      lastUsers.push_back(&inst);
      return;
    }
  }
  auto *deadArg = dyn_cast<SILArgument>(ssaDef);
  assert(deadArg
         && "findBoundariesInBlock must be called on a live block");
  deadDefs.push_back(deadArg);
}

void PrunedLivenessBlockBoundary::findBoundaryInSSADefBlock(
    SILNode *ssaDef, const PrunedLiveness &liveness) {
  endBlocks.push_back(ssaDef->getParentBlock());
}

void SSAPrunedLiveness::findBoundariesInBlock(
    SILBasicBlock *block, bool isLiveOut,
    AnyPrunedLivenessBoundary &boundary) const {
  assert(isInitialized());

  // For SSA, a live-out block cannot have a boundary.
  if (isLiveOut)
    return;

  // Handle live-within block
  if (!isDefBlock(block)) {
    boundary.findBoundaryInNonDefBlock(block, *this);
    return;
  }
  // Find either the last user or a dead def
  auto *defInst = def->getDefiningInstruction();
  SILNode *defNode = defInst ? cast<SILNode>(defInst) : cast<SILArgument>(def);
  boundary.findBoundaryInSSADefBlock(defNode, *this);
}

//===----------------------------------------------------------------------===//
//                           MultiDefPrunedLiveness
//===----------------------------------------------------------------------===//

bool MultiDefPrunedLiveness::isUserBeforeDef(SILInstruction *user) const {
  auto *block = user->getParent();
  if (!isDefBlock(block))
    return false;

  if (llvm::any_of(block->getArguments(), [this](SILArgument *arg) {
    return isDef(arg);
  })) {
    return false;
  }

  auto *current = user;
  while (true) {
    // If user is also a def, then the use is considered before the def.
    current = current->getPreviousInstruction();
    if (!current)
      return true;

    if (isDef(current))
      return false;
  }
}

namespace swift::test {
// Arguments:
// - the string "defs:"
// - list of live-range defining values or instructions
// - the string "uses:"
// - variadic list of live-range user instructions
// Dumps:
// - the liveness result and boundary
//
// Computes liveness for the specified def nodes by considering only the
// specified uses. The actual uses of the def nodes are ignored.
//
// This is useful for testing non-ssa liveness, for example, of memory
// locations. In that case, the def nodes may be stores and the uses may be
// destroy_addrs.
static FunctionTest MultiDefUseLivenessTest(
    "multidefuse_liveness", [](auto &function, auto &arguments, auto &test) {
      SmallVector<SILBasicBlock *, 8> discoveredBlocks;
      MultiDefPrunedLiveness liveness(&function, &discoveredBlocks);

      llvm::outs() << "MultiDef lifetime analysis:\n";
      if (arguments.takeString() != "defs:") {
        llvm::report_fatal_error(
            "test specification expects the 'defs:' label\n");
      }
      while (true) {
        auto argument = arguments.takeArgument();
        if (isa<InstructionArgument>(argument)) {
          auto *instruction = cast<InstructionArgument>(argument).getValue();
          llvm::outs() << "  def instruction: " << *instruction;
          liveness.initializeDef(instruction);
          continue;
        }
        if (isa<ValueArgument>(argument)) {
          SILValue value = cast<ValueArgument>(argument).getValue();
          llvm::outs() << "  def value: " << value;
          liveness.initializeDef(value);
          continue;
        }
        if (cast<StringArgument>(argument).getValue() != "uses:") {
          llvm::report_fatal_error(
              "test specification expects the 'uses:' label\n");
        }
        break;
      }
      while (arguments.hasUntaken()) {
        auto *inst = arguments.takeInstruction();
        // lifetimeEnding has no effects on liveness, it's only a cache for the
        // caller.
        liveness.updateForUse(inst, /*lifetimeEnding*/ false);
      }
      liveness.print(llvm::outs());

      PrunedLivenessBoundary boundary;
      liveness.computeBoundary(boundary);
      boundary.print(llvm::outs());
    });
} // end namespace swift::test

void MultiDefPrunedLiveness::findBoundariesInBlock(
    SILBasicBlock *block, bool isLiveOut,
    AnyPrunedLivenessBoundary &boundary) const {
  assert(isInitialized());

  if (!isDefBlock(block)) {
    // A live-out block with no defs cannot have a boundary.
    if (!isLiveOut) {
      boundary.findBoundaryInNonDefBlock(block, *this);
    }
    return;
  }
  // Handle def blocks...
  //
  // First, check for an SSA live range
  if (++defs.begin() == defs.end()) {
    // For SSA, a live-out block cannot have a boundary.
    if (!isLiveOut) {
      boundary.findBoundaryInSSADefBlock(*defs.begin(), *this);
    }
    return;
  }
  boundary.findBoundaryInMultiDefBlock(block, isLiveOut, *this);
}

void PrunedLivenessBoundary::findBoundaryInMultiDefBlock(
    SILBasicBlock *block, bool isLiveOut,
    const MultiDefPrunedLiveness &liveness) {
  // Handle a live-out or live-within block with potentially multiple defs
  unsigned prevCount = deadDefs.size() + lastUsers.size();
  (void)prevCount;

  bool isLive = isLiveOut;
  for (auto &inst : llvm::reverse(*block)) {
    // Check if the instruction is a def before checking whether it is a
    // use. The same instruction can be both a dead def and boundary use.
    if (liveness.isDef(&inst)) {
      if (!isLive) {
        deadDefs.push_back(cast<SILNode>(&inst));
      }
      isLive = false;
    }
    // Note: the same instruction could potentially be both a dead def and last
    // user. The liveness boundary supports this, although it won't happen in
    // any context where we care about inserting code on the boundary.
    if (!isLive && liveness.isInterestingUser(&inst)) {
      lastUsers.push_back(&inst);
      isLive = true;
    }
  }
  if (!isLive) {
    for (SILArgument *deadArg : block->getArguments()) {
      if (liveness.defs.contains(deadArg)) {
        deadDefs.push_back(deadArg);
      }
    }
    if (auto *predBB = block->getSinglePredecessorBlock()) {
      if (liveness.getBlockLiveness(predBB) == PrunedLiveBlocks::LiveOut) {
        boundaryEdges.push_back(block);
      }
    }
  }
  // All live-within blocks must contain a boundary.
  assert(isLiveOut ||
         (prevCount < deadDefs.size() + lastUsers.size()) &&
             "findBoundariesInBlock must be called on a live block");
}

void PrunedLivenessBlockBoundary::findBoundaryInMultiDefBlock(
    SILBasicBlock *block, bool isLiveOut,
    const MultiDefPrunedLiveness &liveness) {
  bool isLive = isLiveOut;
  for (auto &inst : llvm::reverse(*block)) {
    // Check if the instruction is a def before checking whether it is a
    // use. The same instruction can be both a dead def and boundary use.
    if (liveness.isDef(&inst)) {
      if (!isLive) {
        endBlocks.push_back(block);
        return;
      }
      isLive = false;
    }
    if (!isLive && liveness.isInterestingUser(&inst)) {
      endBlocks.push_back(block);
      return;
    }
  }
  if (!isLive) {
    for (SILArgument *deadArg : block->getArguments()) {
      if (liveness.defs.contains(deadArg)) {
        endBlocks.push_back(block);
        return;
      }
    }
    if (auto *predBB = block->getSinglePredecessorBlock()) {
      if (liveness.getBlockLiveness(predBB) == PrunedLiveBlocks::LiveOut) {
        boundaryEdges.push_back(block);
        return;
      }
    }
  }
}

LiveRangeSummary MultiDefPrunedLiveness::computeSimple() {
  assert(isInitialized() && "defs uninitialized");

  LiveRangeSummary summary;
  for (SILNode *defNode : defs) {
    if (auto *arg = dyn_cast<SILArgument>(defNode))
      summary.meet(updateForDef(arg));
    else {
      for (auto result : cast<SILInstruction>(defNode)->getResults()) {
        summary.meet(updateForDef(result));
      }
    }
  }
  return summary;
}

namespace swift::test {
// Arguments:
// - variadic list of live-range defining values or instructions
// Dumps:
// - the liveness result and boundary
//
// Computes liveness for the specified def nodes by finding all their direct SSA
// uses. If the def is an instruction, then all results are considered.
static FunctionTest MultiDefLivenessTest(
    "multidef_liveness", [](auto &function, auto &arguments, auto &test) {
      SmallVector<SILBasicBlock *, 8> discoveredBlocks;
      MultiDefPrunedLiveness liveness(&function, &discoveredBlocks);

      llvm::outs() << "MultiDef lifetime analysis:\n";
      while (arguments.hasUntaken()) {
        auto argument = arguments.takeArgument();
        if (isa<InstructionArgument>(argument)) {
          auto *instruction = cast<InstructionArgument>(argument).getValue();
          llvm::outs() << "  def instruction: " << instruction;
          liveness.initializeDef(instruction);
        } else {
          SILValue value = cast<ValueArgument>(argument).getValue();
          llvm::outs() << "  def value: " << value;
          liveness.initializeDef(value);
        }
      }
      liveness.computeSimple();
      liveness.print(llvm::outs());

      PrunedLivenessBoundary boundary;
      liveness.computeBoundary(boundary);
      boundary.print(llvm::outs());
    });
} // end namespace swift::test

//===----------------------------------------------------------------------===//
//                       DiagnosticPrunedLiveness
//===----------------------------------------------------------------------===//

// FIXME: This is wrong. Why is nonLifetimeEndingUsesInLiveOut inside
// PrunedLiveness, and what does it mean? Blocks may transition to LiveOut
// later. Or they may already be LiveOut from a previous use. After computing
// liveness, clients should check uses that are in PrunedLivenessBoundary.
void DiagnosticPrunedLiveness::
updateForUse(SILInstruction *user, bool lifetimeEnding) {
  SSAPrunedLiveness::updateForUse(user, 0);

  auto useBlockLive = getBlockLiveness(user->getParent());
  // Record all uses of blocks on the liveness boundary. For blocks marked
  // LiveWithin, the boundary is considered to be the last use in the block.
  if (!lifetimeEnding && useBlockLive == PrunedLiveBlocks::LiveOut) {
    if (nonLifetimeEndingUsesInLiveOut)
      nonLifetimeEndingUsesInLiveOut->insert(user);
    return;
  }
}
