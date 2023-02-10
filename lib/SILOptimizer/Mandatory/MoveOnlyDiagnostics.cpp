//===--- MoveOnlyDiagnostics.cpp ------------------------------------------===//
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

#define DEBUG_TYPE "sil-move-only-checker"

#include "MoveOnlyDiagnostics.h"

#include "swift/AST/DiagnosticsSIL.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/FieldSensitivePrunedLiveness.h"
#include "swift/SIL/SILArgument.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace swift::siloptimizer;

static llvm::cl::opt<bool> SilentlyEmitDiagnostics(
    "move-only-diagnostics-silently-emit-diagnostics",
    llvm::cl::desc(
        "For testing purposes, emit the diagnostic silently so we can "
        "filecheck the result of emitting an error from the move checkers"),
    llvm::cl::init(false));

//===----------------------------------------------------------------------===//
//                              MARK: Utilities
//===----------------------------------------------------------------------===//

template <typename... T, typename... U>
static void diagnose(ASTContext &context, SILInstruction *inst, Diag<T...> diag,
                     U &&...args) {
  // See if the consuming use is an owned moveonly_to_copyable whose only
  // user is a return. In that case, use the return loc instead. We do this
  // b/c it is illegal to put a return value location on a non-return value
  // instruction... so we have to hack around this slightly.
  auto loc = inst->getLoc();
  if (auto *mtc = dyn_cast<MoveOnlyWrapperToCopyableValueInst>(inst)) {
    if (auto *ri = mtc->getSingleUserOfType<ReturnInst>()) {
      loc = ri->getLoc();
    }
  }

  // If for testing reasons we want to return that we emitted an error but not
  // emit the actual error itself, return early.
  if (SilentlyEmitDiagnostics)
    return;

  context.Diags.diagnose(loc.getSourceLoc(), diag, std::forward<U>(args)...);
}

static void getVariableNameForValue(MarkMustCheckInst *mmci,
                                    SmallString<64> &resultingString) {
  // Before we do anything, lets see if we have an exact debug_value on our
  // mmci. In such a case, we can end early and are done.
  if (auto *use = getSingleDebugUse(mmci)) {
    if (auto debugVar = DebugVarCarryingInst(use->getUser())) {
      assert(debugVar.getKind() == DebugVarCarryingInst::Kind::DebugValue);
      resultingString += debugVar.getName();
      return;
    }
  }

  // Otherwise, we need to look at our mark_must_check's operand.
  StackList<SILInstruction *> variableNamePath(mmci->getFunction());
  SILValue value = mmci->getOperand();
  while (true) {
    if (auto *allocInst = dyn_cast<AllocationInst>(value)) {
      variableNamePath.push_back(allocInst);
      break;
    }

    if (auto *globalAddrInst = dyn_cast<GlobalAddrInst>(value)) {
      variableNamePath.push_back(globalAddrInst);
      break;
    }

    if (auto *rei = dyn_cast<RefElementAddrInst>(value)) {
      variableNamePath.push_back(rei);
      value = rei->getOperand();
      continue;
    }

    // Single value instructions we should look through.
    if (isa<BeginBorrowInst>(value) || isa<LoadInst>(value) ||
        isa<BeginAccessInst>(value) || isa<MarkMustCheckInst>(value)) {
      value = cast<SingleValueInstruction>(value)->getOperand(0);
      continue;
    }

    // If we do not do an exact match, see if we can find a debug_var inst. If
    // we do, we always break since we have a root value.
    if (auto *use = getSingleDebugUse(value)) {
      if (auto debugVar = DebugVarCarryingInst(use->getUser())) {
        assert(debugVar.getKind() == DebugVarCarryingInst::Kind::DebugValue);
        variableNamePath.push_back(use->getUser());
        break;
      }
    }

    // If we do not pattern match successfully, just set resulting string to
    // unknown and return early.
    resultingString += "unknown";
    return;
  }

  // Walk backwards, constructing our string.
  while (true) {
    auto *next = variableNamePath.pop_back_val();

    if (auto i = DebugVarCarryingInst(next)) {
      resultingString += i.getName();
    } else if (auto i = VarDeclCarryingInst(next)) {
      resultingString += i.getName();
    }

    if (variableNamePath.empty())
      return;

    resultingString += '.';
  }
}

//===----------------------------------------------------------------------===//
//                           MARK: Misc Diagnostics
//===----------------------------------------------------------------------===//

void DiagnosticEmitter::emitCheckerDoesntUnderstandDiagnostic(
    MarkMustCheckInst *markedValue) {
  // If we failed to canonicalize ownership, there was something in the SIL
  // that copy propagation did not understand. Emit a we did not understand
  // error.
  if (markedValue->getType().isMoveOnlyWrapped()) {
    diagnose(
        fn->getASTContext(), markedValue,
        diag::sil_moveonlychecker_not_understand_consumable_and_assignable);
  } else {
    diagnose(fn->getASTContext(), markedValue,
             diag::sil_moveonlychecker_not_understand_moveonly);
  }
  registerDiagnosticEmitted(markedValue);
  emittedCheckerDoesntUnderstandDiagnostic = true;
}

//===----------------------------------------------------------------------===//
//                          MARK: Object Diagnostics
//===----------------------------------------------------------------------===//

void DiagnosticEmitter::emitObjectGuaranteedDiagnostic(
    MarkMustCheckInst *markedValue) {
  auto &astContext = fn->getASTContext();
  SmallString<64> varName;
  getVariableNameForValue(markedValue, varName);

  // See if we have any closure capture uses and emit a better diagnostic.
  if (getCanonicalizer().hasPartialApplyConsumingUse()) {
    diagnose(astContext, markedValue,
             diag::sil_moveonlychecker_guaranteed_value_captured_by_closure,
             varName);
    emitObjectDiagnosticsForPartialApplyUses();
    registerDiagnosticEmitted(markedValue);
  }

  // If we do not have any non-partial apply consuming uses... just exit early.
  if (!getCanonicalizer().hasNonPartialApplyConsumingUse())
    return;

  // Check if this value is closure captured. In such a case, emit a special
  // error.
  if (auto *fArg = dyn_cast<SILFunctionArgument>(
          lookThroughCopyValueInsts(markedValue->getOperand()))) {
    if (fArg->isClosureCapture()) {
      diagnose(astContext, markedValue,
               diag::sil_moveonlychecker_let_value_consumed_in_closure,
               varName);
      emitObjectDiagnosticsForGuaranteedUses(
          true /*ignore partial apply uses*/);
      registerDiagnosticEmitted(markedValue);
      return;
    }
  }

  diagnose(astContext, markedValue,
           diag::sil_moveonlychecker_guaranteed_value_consumed, varName);

  emitObjectDiagnosticsForGuaranteedUses(true /*ignore partial apply uses*/);
  registerDiagnosticEmitted(markedValue);
}

void DiagnosticEmitter::emitObjectOwnedDiagnostic(
    MarkMustCheckInst *markedValue) {
  auto &astContext = fn->getASTContext();
  SmallString<64> varName;
  getVariableNameForValue(markedValue, varName);

  // Ok we know that we are going to emit an error. Lets use a little more
  // compile time to emit a nice error.
  InstructionSet consumingUserSet(markedValue->getFunction());
  InstructionSet nonConsumingUserSet(markedValue->getFunction());
  llvm::SmallDenseMap<SILBasicBlock *, SILInstruction *, 8>
      consumingBlockToUserMap;
  llvm::SmallDenseMap<SILBasicBlock *, SILInstruction *, 8>
      nonConsumingBlockToUserMap;

  // NOTE: We use all lifetime ending and non-lifetime ending users to ensure
  // that we properly identify cases where the actual boundary use is in a loop
  // further down the loop nest from our original use. In such a case, it will
  // not be identified as part of the boundary and instead we will identify a
  // boundary edge which does not provide us with something that we want to
  // error upon.
  for (auto *user :
       getCanonicalizer().canonicalizer->getLifetimeEndingUsers()) {
    consumingUserSet.insert(user);
    consumingBlockToUserMap.try_emplace(user->getParent(), user);
  }
  for (auto *user :
       getCanonicalizer().canonicalizer->getNonLifetimeEndingUsers()) {
    nonConsumingUserSet.insert(user);
    nonConsumingBlockToUserMap.try_emplace(user->getParent(), user);
  }

  // Now for each consuming use that needs a copy...
  for (auto *user : getCanonicalizer().consumingUsesNeedingCopy) {
    // First search from user to the end of the block for one of our boundary
    // uses and if it is in the block, emit an error and continue.
    bool foundSingleBlockError = false;
    for (auto ii = std::next(user->getIterator()),
              ie = user->getParent()->end();
         ii != ie; ++ii) {
      if (consumingUserSet.contains(&*ii)) {
        foundSingleBlockError = true;
        diagnose(astContext, markedValue,
                 diag::sil_moveonlychecker_owned_value_consumed_more_than_once,
                 varName);
        diagnose(astContext, user,
                 diag::sil_moveonlychecker_consuming_use_here);
        diagnose(astContext, &*ii,
                 diag::sil_moveonlychecker_other_consuming_use_here);
        break;
      }

      if (nonConsumingUserSet.contains(&*ii)) {
        foundSingleBlockError = true;
        diagnose(astContext, markedValue,
                 diag::sil_moveonlychecker_value_used_after_consume, varName);
        diagnose(astContext, user,
                 diag::sil_moveonlychecker_consuming_use_here);
        diagnose(astContext, &*ii,
                 diag::sil_moveonlychecker_nonconsuming_use_here);
        break;
      }
    }

    // If we found a single block error for this user, continue.
    if (foundSingleBlockError)
      continue;

    // Otherwise, the reason why the consuming use needs to be copied is in a
    // successor block. Lets go look for that user.
    BasicBlockWorklist worklist(markedValue->getFunction());
    for (auto *succBlock : user->getParent()->getSuccessorBlocks())
      worklist.push(succBlock);
    while (auto *nextBlock = worklist.pop()) {
      // First, check if we are visiting the same block as our user block. In
      // such a case, we found a consuming use within a loop.
      if (nextBlock == user->getParent()) {
        diagnose(astContext, markedValue,
                 diag::sil_moveonlychecker_value_consumed_in_a_loop, varName);
        auto d =
            diag::sil_movekillscopyablevalue_value_cyclic_consumed_in_loop_here;
        diagnose(astContext, user, d);
        break;
      }

      {
        auto iter = consumingBlockToUserMap.find(nextBlock);
        if (iter != consumingBlockToUserMap.end()) {
          // We found it... emit the error and break.
          diagnose(
              astContext, markedValue,
              diag::sil_moveonlychecker_owned_value_consumed_more_than_once,
              varName);
          diagnose(astContext, user,
                   diag::sil_moveonlychecker_consuming_use_here);
          diagnose(astContext, iter->second,
                   diag::sil_moveonlychecker_other_consuming_use_here);
          break;
        }
      }

      {
        auto iter = nonConsumingBlockToUserMap.find(nextBlock);
        if (iter != nonConsumingBlockToUserMap.end()) {
          // We found it... emit the error and break.
          diagnose(astContext, markedValue,
                   diag::sil_moveonlychecker_value_used_after_consume, varName);
          diagnose(astContext, user,
                   diag::sil_moveonlychecker_consuming_use_here);
          diagnose(astContext, iter->second,
                   diag::sil_moveonlychecker_nonconsuming_use_here);
          break;
        }
      }

      // If we didn't break, keep walking successors we haven't seen yet.
      for (auto *succBlock : nextBlock->getSuccessorBlocks()) {
        worklist.pushIfNotVisited(succBlock);
      }
    }
  }

  registerDiagnosticEmitted(markedValue);
}

void DiagnosticEmitter::emitObjectDiagnosticsForGuaranteedUses(
    bool ignorePartialApplyUses) const {
  auto &astContext = fn->getASTContext();

  for (auto *consumingUser : getCanonicalizer().consumingUsesNeedingCopy) {
    if (ignorePartialApplyUses && isa<PartialApplyInst>(consumingUser))
      continue;
    diagnose(astContext, consumingUser,
             diag::sil_moveonlychecker_consuming_use_here);
  }

  for (auto *user : getCanonicalizer().consumingBoundaryUsers) {
    if (ignorePartialApplyUses && isa<PartialApplyInst>(user))
      continue;

    diagnose(astContext, user, diag::sil_moveonlychecker_consuming_use_here);
  }
}

void DiagnosticEmitter::emitObjectDiagnosticsForPartialApplyUses() const {
  auto &astContext = fn->getASTContext();

  for (auto *user : getCanonicalizer().consumingUsesNeedingCopy) {
    if (!isa<PartialApplyInst>(user))
      continue;
    diagnose(astContext, user,
             diag::sil_moveonlychecker_consuming_closure_use_here);
  }

  for (auto *user : getCanonicalizer().consumingBoundaryUsers) {
    if (!isa<PartialApplyInst>(user))
      continue;

    diagnose(astContext, user,
             diag::sil_moveonlychecker_consuming_closure_use_here);
  }
}

//===----------------------------------------------------------------------===//
//                         MARK: Address Diagnostics
//===----------------------------------------------------------------------===//

void DiagnosticEmitter::emitAddressExclusivityHazardDiagnostic(
    MarkMustCheckInst *markedValue, SILInstruction *consumingUser) {
  if (!useWithDiagnostic.insert(consumingUser).second)
    return;
  registerDiagnosticEmitted(markedValue);

  auto &astContext = markedValue->getFunction()->getASTContext();
  SmallString<64> varName;
  getVariableNameForValue(markedValue, varName);

  LLVM_DEBUG(llvm::dbgs() << "Emitting error for exclusivity!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Mark: " << *markedValue);
  LLVM_DEBUG(llvm::dbgs() << "    Consuming use: " << *consumingUser);

  diagnose(astContext, markedValue,
           diag::sil_moveonlychecker_exclusivity_violation, varName);
  diagnose(astContext, consumingUser,
           diag::sil_moveonlychecker_consuming_use_here);
}

void DiagnosticEmitter::emitAddressDiagnostic(MarkMustCheckInst *markedValue,
                                              SILInstruction *lastLiveUser,
                                              SILInstruction *violatingUser,
                                              bool isUseConsuming,
                                              bool isInOutEndOfFunction) {
  if (!useWithDiagnostic.insert(violatingUser).second)
    return;
  registerDiagnosticEmitted(markedValue);

  auto &astContext = markedValue->getFunction()->getASTContext();
  SmallString<64> varName;
  getVariableNameForValue(markedValue, varName);

  LLVM_DEBUG(llvm::dbgs() << "Emitting error!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Mark: " << *markedValue);
  LLVM_DEBUG(llvm::dbgs() << "    Last Live Use: " << *lastLiveUser);
  LLVM_DEBUG(llvm::dbgs() << "    Last Live Use Is Consuming? "
                          << (isUseConsuming ? "yes" : "no") << '\n');
  LLVM_DEBUG(llvm::dbgs() << "    Violating Use: " << *violatingUser);

  // If our liveness use is the same as our violating use, then we know that we
  // had a loop. Give a better diagnostic.
  if (lastLiveUser == violatingUser) {
    diagnose(astContext, markedValue,
             diag::sil_moveonlychecker_value_consumed_in_a_loop, varName);
    diagnose(astContext, violatingUser,
             diag::sil_moveonlychecker_consuming_use_here);
    return;
  }

  if (isInOutEndOfFunction) {
    if (auto *pbi = dyn_cast<ProjectBoxInst>(markedValue->getOperand())) {
      if (auto *fArg = dyn_cast<SILFunctionArgument>(pbi->getOperand())) {
        if (fArg->isClosureCapture()) {
          diagnose(
              astContext, markedValue,
              diag::
                  sil_moveonlychecker_inout_not_reinitialized_before_end_of_closure,
              varName);
          diagnose(astContext, violatingUser,
                   diag::sil_moveonlychecker_consuming_use_here);
          return;
        }
      }
    }
    if (auto *fArg = dyn_cast<SILFunctionArgument>(markedValue->getOperand())) {
      if (fArg->isClosureCapture()) {
        diagnose(
            astContext, markedValue,
            diag::
                sil_moveonlychecker_inout_not_reinitialized_before_end_of_closure,
            varName);
        diagnose(astContext, violatingUser,
                 diag::sil_moveonlychecker_consuming_use_here);
        return;
      }
    }
    diagnose(
        astContext, markedValue,
        diag::
            sil_moveonlychecker_inout_not_reinitialized_before_end_of_function,
        varName);
    diagnose(astContext, violatingUser,
             diag::sil_moveonlychecker_consuming_use_here);
    return;
  }

  // First if we are consuming emit an error for no implicit copy semantics.
  if (isUseConsuming) {
    diagnose(astContext, markedValue,
             diag::sil_moveonlychecker_owned_value_consumed_more_than_once,
             varName);
    diagnose(astContext, violatingUser,
             diag::sil_moveonlychecker_consuming_use_here);
    diagnose(astContext, lastLiveUser,
             diag::sil_moveonlychecker_consuming_use_here);
    return;
  }

  // Otherwise, use the "used after consuming use" error.
  diagnose(astContext, markedValue,
           diag::sil_moveonlychecker_value_used_after_consume, varName);
  diagnose(astContext, violatingUser,
           diag::sil_moveonlychecker_consuming_use_here);
  diagnose(astContext, lastLiveUser,
           diag::sil_moveonlychecker_nonconsuming_use_here);
}

void DiagnosticEmitter::emitInOutEndOfFunctionDiagnostic(
    MarkMustCheckInst *markedValue, SILInstruction *violatingUser) {
  if (!useWithDiagnostic.insert(violatingUser).second)
    return;
  registerDiagnosticEmitted(markedValue);

  assert(cast<SILFunctionArgument>(markedValue->getOperand())
             ->getArgumentConvention()
             .isInoutConvention() &&
         "Expected markedValue to be on an inout");

  auto &astContext = markedValue->getFunction()->getASTContext();
  SmallString<64> varName;
  getVariableNameForValue(markedValue, varName);

  LLVM_DEBUG(llvm::dbgs() << "Emitting inout error error!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Mark: " << *markedValue);
  LLVM_DEBUG(llvm::dbgs() << "    Violating Use: " << *violatingUser);

  // Otherwise, we need to do no implicit copy semantics. If our last use was
  // consuming message:
  if (auto *fArg = dyn_cast<SILFunctionArgument>(markedValue->getOperand())) {
    if (fArg->isClosureCapture()) {
      diagnose(
          astContext, markedValue,
          diag::
              sil_moveonlychecker_inout_not_reinitialized_before_end_of_closure,
          varName);
      diagnose(astContext, violatingUser,
               diag::sil_moveonlychecker_consuming_use_here);
      return;
    }
  }
  diagnose(
      astContext, markedValue,
      diag::sil_moveonlychecker_inout_not_reinitialized_before_end_of_function,
      varName);
  diagnose(astContext, violatingUser,
           diag::sil_moveonlychecker_consuming_use_here);
}

void DiagnosticEmitter::emitAddressDiagnosticNoCopy(
    MarkMustCheckInst *markedValue, SILInstruction *consumingUser) {
  if (!useWithDiagnostic.insert(consumingUser).second)
    return;

  auto &astContext = markedValue->getFunction()->getASTContext();
  SmallString<64> varName;
  getVariableNameForValue(markedValue, varName);

  LLVM_DEBUG(llvm::dbgs() << "Emitting no copy error!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Mark: " << *markedValue);
  LLVM_DEBUG(llvm::dbgs() << "    Consuming Use: " << *consumingUser);

  // Otherwise, we need to do no implicit copy semantics. If our last use was
  // consuming message:
  diagnose(astContext, markedValue,
           diag::sil_moveonlychecker_guaranteed_value_consumed, varName);
  diagnose(astContext, consumingUser,
           diag::sil_moveonlychecker_consuming_use_here);
  registerDiagnosticEmitted(markedValue);
}

void DiagnosticEmitter::emitObjectDestructureNeededWithinBorrowBoundary(
    MarkMustCheckInst *markedValue, SILInstruction *destructureNeedingUser,
    TypeTreeLeafTypeRange destructureSpan,
    FieldSensitivePrunedLivenessBoundary &boundary) {
  if (!useWithDiagnostic.insert(destructureNeedingUser).second)
    return;

  auto &astContext = markedValue->getFunction()->getASTContext();
  SmallString<64> varName;
  getVariableNameForValue(markedValue, varName);

  LLVM_DEBUG(llvm::dbgs() << "Emitting destructure can't be created error!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Mark: " << *markedValue);
  LLVM_DEBUG(llvm::dbgs() << "    Destructure Needing Use: "
                          << *destructureNeedingUser);

  diagnose(astContext, markedValue,
           diag::sil_moveonlychecker_moveonly_field_consumed, varName);
  diagnose(astContext, destructureNeedingUser,
           diag::sil_moveonlychecker_consuming_use_here);

  // Only emit errors for last users that overlap with our needed destructure
  // bits.
  for (auto pair : boundary.getLastUsers()) {
    if (llvm::any_of(destructureSpan.getRange(),
                     [&](unsigned index) { return pair.second.test(index); })) {
      LLVM_DEBUG(llvm::dbgs()
                 << "    Destructure Boundary Use: " << *pair.first);
      diagnose(astContext, pair.first, diag::sil_moveonlychecker_boundary_use);
    }
  }
  registerDiagnosticEmitted(markedValue);
}

void DiagnosticEmitter::emitObjectInstConsumesValueTwice(
    MarkMustCheckInst *markedValue, Operand *firstUse, Operand *secondUse) {
  assert(firstUse->getUser() == secondUse->getUser());
  assert(firstUse->isConsuming());
  assert(secondUse->isConsuming());

  LLVM_DEBUG(llvm::dbgs() << "Emitting object consumes value twice error!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Mark: " << *markedValue);
  LLVM_DEBUG(llvm::dbgs() << "    User: " << *firstUse->getUser());
  LLVM_DEBUG(llvm::dbgs() << "    First Conflicting Operand: "
                          << firstUse->getOperandNumber() << '\n');
  LLVM_DEBUG(llvm::dbgs() << "    Second Conflicting Operand: "
                          << secondUse->getOperandNumber() << '\n');

  auto &astContext = markedValue->getModule().getASTContext();
  SmallString<64> varName;
  getVariableNameForValue(markedValue, varName);
  diagnose(astContext, markedValue,
           diag::sil_moveonlychecker_owned_value_consumed_more_than_once,
           varName);
  diagnose(astContext, firstUse->getUser(),
           diag::sil_moveonlychecker_two_consuming_uses_here);
  registerDiagnosticEmitted(markedValue);
}

void DiagnosticEmitter::emitObjectInstConsumesAndUsesValue(
    MarkMustCheckInst *markedValue, Operand *consumingUse,
    Operand *nonConsumingUse) {
  assert(consumingUse->getUser() == nonConsumingUse->getUser());
  assert(consumingUse->isConsuming());
  assert(!nonConsumingUse->isConsuming());

  LLVM_DEBUG(llvm::dbgs() << "Emitting object consumeed and used error!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Mark: " << *markedValue);
  LLVM_DEBUG(llvm::dbgs() << "    User: " << *consumingUse->getUser());
  LLVM_DEBUG(llvm::dbgs() << "    Consuming Operand: "
                          << consumingUse->getOperandNumber() << '\n');
  LLVM_DEBUG(llvm::dbgs() << "    Non Consuming Operand: "
                          << nonConsumingUse->getOperandNumber() << '\n');

  auto &astContext = markedValue->getModule().getASTContext();
  SmallString<64> varName;
  getVariableNameForValue(markedValue, varName);
  diagnose(astContext, markedValue,
           diag::sil_moveonlychecker_owned_value_consumed_and_used_at_same_time,
           varName);
  diagnose(astContext, consumingUse->getUser(),
           diag::sil_moveonlychecker_consuming_and_non_consuming_uses_here);
  registerDiagnosticEmitted(markedValue);
}
