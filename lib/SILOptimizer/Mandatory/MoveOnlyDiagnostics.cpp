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
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILArgument.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace swift::siloptimizer;

//===----------------------------------------------------------------------===//
//                              MARK: Utilities
//===----------------------------------------------------------------------===//

template <typename... T, typename... U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

static StringRef getVariableNameForValue(MarkMustCheckInst *mmci) {
  if (auto *allocInst = dyn_cast<AllocationInst>(mmci->getOperand())) {
    DebugVarCarryingInst debugVar(allocInst);
    if (auto varInfo = debugVar.getVarInfo()) {
      return varInfo->Name;
    } else {
      if (auto *decl = debugVar.getDecl()) {
        return decl->getBaseName().userFacingName();
      }
    }
  }

  if (auto *use = getSingleDebugUse(mmci)) {
    DebugVarCarryingInst debugVar(use->getUser());
    if (auto varInfo = debugVar.getVarInfo()) {
      return varInfo->Name;
    } else {
      if (auto *decl = debugVar.getDecl()) {
        return decl->getBaseName().userFacingName();
      }
    }
  }

  return "unknown";
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
    diagnose(fn->getASTContext(), markedValue->getLoc().getSourceLoc(),
             diag::sil_moveonlychecker_not_understand_no_implicit_copy);
  } else {
    diagnose(fn->getASTContext(), markedValue->getLoc().getSourceLoc(),
             diag::sil_moveonlychecker_not_understand_moveonly);
  }
  valuesWithDiagnostics.insert(markedValue);
}

//===----------------------------------------------------------------------===//
//                          MARK: Object Diagnostics
//===----------------------------------------------------------------------===//

void DiagnosticEmitter::emitObjectGuaranteedDiagnostic(
    MarkMustCheckInst *markedValue) {
  auto &astContext = fn->getASTContext();
  StringRef varName = getVariableNameForValue(markedValue);

  // See if we have any closure capture uses and emit a better diagnostic.
  if (getCanonicalizer().hasPartialApplyConsumingUse()) {
    diagnose(astContext,
             markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
             diag::sil_moveonlychecker_guaranteed_value_captured_by_closure,
             varName);
    emitObjectDiagnosticsForPartialApplyUses();
    valuesWithDiagnostics.insert(markedValue);
  }

  // If we do not have any non-partial apply consuming uses... just exit early.
  if (!getCanonicalizer().hasNonPartialApplyConsumingUse())
    return;

  // Check if this value is closure captured. In such a case, emit a special
  // error.
  if (auto *fArg = dyn_cast<SILFunctionArgument>(
          lookThroughCopyValueInsts(markedValue->getOperand()))) {
    if (fArg->isClosureCapture()) {
      diagnose(astContext,
               markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
               diag::sil_moveonlychecker_let_value_consumed_in_closure,
               varName);
      emitObjectDiagnosticsForFoundUses(true /*ignore partial apply uses*/);
      valuesWithDiagnostics.insert(markedValue);
      return;
    }
  }

  diagnose(astContext,
           markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
           diag::sil_moveonlychecker_guaranteed_value_consumed, varName);

  emitObjectDiagnosticsForFoundUses(true /*ignore partial apply uses*/);
  valuesWithDiagnostics.insert(markedValue);
}

void DiagnosticEmitter::emitObjectOwnedDiagnostic(
    MarkMustCheckInst *markedValue) {
  auto &astContext = fn->getASTContext();
  StringRef varName = getVariableNameForValue(markedValue);

  diagnose(astContext,
           markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
           diag::sil_moveonlychecker_owned_value_consumed_more_than_once,
           varName);

  emitObjectDiagnosticsForFoundUses();
  valuesWithDiagnostics.insert(markedValue);
}

void DiagnosticEmitter::emitObjectDiagnosticsForFoundUses(
    bool ignorePartialApplyUses) const {
  auto &astContext = fn->getASTContext();

  for (auto *consumingUse : getCanonicalizer().consumingUsesNeedingCopy) {
    // See if the consuming use is an owned moveonly_to_copyable whose only
    // user is a return. In that case, use the return loc instead. We do this
    // b/c it is illegal to put a return value location on a non-return value
    // instruction... so we have to hack around this slightly.
    auto *user = consumingUse->getUser();
    auto loc = user->getLoc();
    if (auto *mtc = dyn_cast<MoveOnlyWrapperToCopyableValueInst>(user)) {
      if (auto *ri = mtc->getSingleUserOfType<ReturnInst>()) {
        loc = ri->getLoc();
      }
    }

    if (ignorePartialApplyUses &&
        isa<PartialApplyInst>(consumingUse->getUser()))
      continue;
    diagnose(astContext, loc.getSourceLoc(),
             diag::sil_moveonlychecker_consuming_use_here);
  }

  for (auto *consumingUse : getCanonicalizer().finalConsumingUses) {
    // See if the consuming use is an owned moveonly_to_copyable whose only
    // user is a return. In that case, use the return loc instead. We do this
    // b/c it is illegal to put a return value location on a non-return value
    // instruction... so we have to hack around this slightly.
    auto *user = consumingUse->getUser();
    auto loc = user->getLoc();
    if (auto *mtc = dyn_cast<MoveOnlyWrapperToCopyableValueInst>(user)) {
      if (auto *ri = mtc->getSingleUserOfType<ReturnInst>()) {
        loc = ri->getLoc();
      }
    }

    if (ignorePartialApplyUses &&
        isa<PartialApplyInst>(consumingUse->getUser()))
      continue;

    diagnose(astContext, loc.getSourceLoc(),
             diag::sil_moveonlychecker_consuming_use_here);
  }
}

void DiagnosticEmitter::emitObjectDiagnosticsForPartialApplyUses() const {
  auto &astContext = fn->getASTContext();

  for (auto *consumingUse : getCanonicalizer().consumingUsesNeedingCopy) {
    // See if the consuming use is an owned moveonly_to_copyable whose only
    // user is a return. In that case, use the return loc instead. We do this
    // b/c it is illegal to put a return value location on a non-return value
    // instruction... so we have to hack around this slightly.
    auto *user = consumingUse->getUser();
    auto loc = user->getLoc();
    if (auto *mtc = dyn_cast<MoveOnlyWrapperToCopyableValueInst>(user)) {
      if (auto *ri = mtc->getSingleUserOfType<ReturnInst>()) {
        loc = ri->getLoc();
      }
    }

    if (!isa<PartialApplyInst>(consumingUse->getUser()))
      continue;
    diagnose(astContext, loc.getSourceLoc(),
             diag::sil_moveonlychecker_consuming_closure_use_here);
  }

  for (auto *consumingUse : getCanonicalizer().finalConsumingUses) {
    // See if the consuming use is an owned moveonly_to_copyable whose only
    // user is a return. In that case, use the return loc instead. We do this
    // b/c it is illegal to put a return value location on a non-return value
    // instruction... so we have to hack around this slightly.
    auto *user = consumingUse->getUser();
    auto loc = user->getLoc();
    if (auto *mtc = dyn_cast<MoveOnlyWrapperToCopyableValueInst>(user)) {
      if (auto *ri = mtc->getSingleUserOfType<ReturnInst>()) {
        loc = ri->getLoc();
      }
    }

    if (!isa<PartialApplyInst>(consumingUse->getUser()))
      continue;

    diagnose(astContext, loc.getSourceLoc(),
             diag::sil_moveonlychecker_consuming_closure_use_here);
  }
}

//===----------------------------------------------------------------------===//
//                         MARK: Address Diagnostics
//===----------------------------------------------------------------------===//

void DiagnosticEmitter::emitAddressExclusivityHazardDiagnostic(
    MarkMustCheckInst *markedValue, SILInstruction *consumingUse) {
  if (!useWithDiagnostic.insert(consumingUse).second)
    return;

  valuesWithDiagnostics.insert(markedValue);

  auto &astContext = markedValue->getFunction()->getASTContext();
  StringRef varName = getVariableNameForValue(markedValue);

  LLVM_DEBUG(llvm::dbgs() << "Emitting error for exclusivity!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Mark: " << *markedValue);
  LLVM_DEBUG(llvm::dbgs() << "    Consuming use: " << *consumingUse);

  diagnose(astContext,
           markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
           diag::sil_moveonlychecker_exclusivity_violation, varName);
  diagnose(astContext, consumingUse->getLoc().getSourceLoc(),
           diag::sil_moveonlychecker_consuming_use_here);
}

void DiagnosticEmitter::emitAddressDiagnostic(MarkMustCheckInst *markedValue,
                                              SILInstruction *lastLiveUse,
                                              SILInstruction *violatingUse,
                                              bool isUseConsuming,
                                              bool isInOutEndOfFunction) {
  if (!useWithDiagnostic.insert(violatingUse).second)
    return;

  valuesWithDiagnostics.insert(markedValue);

  auto &astContext = markedValue->getFunction()->getASTContext();
  StringRef varName = getVariableNameForValue(markedValue);

  LLVM_DEBUG(llvm::dbgs() << "Emitting error!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Mark: " << *markedValue);
  LLVM_DEBUG(llvm::dbgs() << "    Last Live Use: " << *lastLiveUse);
  LLVM_DEBUG(llvm::dbgs() << "    Last Live Use Is Consuming? "
                          << (isUseConsuming ? "yes" : "no") << '\n');
  LLVM_DEBUG(llvm::dbgs() << "    Violating Use: " << *violatingUse);

  // If our liveness use is the same as our violating use, then we know that we
  // had a loop. Give a better diagnostic.
  if (lastLiveUse == violatingUse) {
    diagnose(astContext,
             markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
             diag::sil_moveonlychecker_value_consumed_in_a_loop, varName);
    diagnose(astContext, violatingUse->getLoc().getSourceLoc(),
             diag::sil_moveonlychecker_consuming_use_here);
    return;
  }

  if (isInOutEndOfFunction) {
    if (auto *fArg = dyn_cast<SILFunctionArgument>(markedValue->getOperand())) {
      if (fArg->isClosureCapture()) {
        diagnose(
            astContext,
            markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
            diag::
                sil_moveonlychecker_inout_not_reinitialized_before_end_of_closure,
            varName);
        diagnose(astContext, violatingUse->getLoc().getSourceLoc(),
                 diag::sil_moveonlychecker_consuming_use_here);
        return;
      }
    }
    diagnose(
        astContext,
        markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
        diag::
            sil_moveonlychecker_inout_not_reinitialized_before_end_of_function,
        varName);
    diagnose(astContext, violatingUse->getLoc().getSourceLoc(),
             diag::sil_moveonlychecker_consuming_use_here);
    return;
  }

  // First if we are consuming emit an error for no implicit copy semantics.
  if (isUseConsuming) {
    diagnose(astContext,
             markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
             diag::sil_moveonlychecker_owned_value_consumed_more_than_once,
             varName);
    diagnose(astContext, violatingUse->getLoc().getSourceLoc(),
             diag::sil_moveonlychecker_consuming_use_here);
    diagnose(astContext, lastLiveUse->getLoc().getSourceLoc(),
             diag::sil_moveonlychecker_consuming_use_here);
    return;
  }

  // Otherwise, use the "used after consuming use" error.
  diagnose(astContext,
           markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
           diag::sil_moveonlychecker_value_used_after_consume, varName);
  diagnose(astContext, violatingUse->getLoc().getSourceLoc(),
           diag::sil_moveonlychecker_consuming_use_here);
  diagnose(astContext, lastLiveUse->getLoc().getSourceLoc(),
           diag::sil_moveonlychecker_nonconsuming_use_here);
}

void DiagnosticEmitter::emitInOutEndOfFunctionDiagnostic(
    MarkMustCheckInst *markedValue, SILInstruction *violatingUse) {
  if (!useWithDiagnostic.insert(violatingUse).second)
    return;

  valuesWithDiagnostics.insert(markedValue);

  assert(cast<SILFunctionArgument>(markedValue->getOperand())
             ->getArgumentConvention()
             .isInoutConvention() &&
         "Expected markedValue to be on an inout");

  auto &astContext = markedValue->getFunction()->getASTContext();
  StringRef varName = getVariableNameForValue(markedValue);

  LLVM_DEBUG(llvm::dbgs() << "Emitting inout error error!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Mark: " << *markedValue);
  LLVM_DEBUG(llvm::dbgs() << "    Violating Use: " << *violatingUse);

  // Otherwise, we need to do no implicit copy semantics. If our last use was
  // consuming message:
  if (auto *fArg = dyn_cast<SILFunctionArgument>(markedValue->getOperand())) {
    if (fArg->isClosureCapture()) {
      diagnose(
          astContext,
          markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
          diag::
              sil_moveonlychecker_inout_not_reinitialized_before_end_of_closure,
          varName);
      diagnose(astContext, violatingUse->getLoc().getSourceLoc(),
               diag::sil_moveonlychecker_consuming_use_here);
      return;
    }
  }
  diagnose(
      astContext,
      markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
      diag::sil_moveonlychecker_inout_not_reinitialized_before_end_of_function,
      varName);
  diagnose(astContext, violatingUse->getLoc().getSourceLoc(),
           diag::sil_moveonlychecker_consuming_use_here);
}

void DiagnosticEmitter::emitAddressDiagnosticNoCopy(
    MarkMustCheckInst *markedValue, SILInstruction *consumingUse) {
  if (!useWithDiagnostic.insert(consumingUse).second)
    return;

  auto &astContext = markedValue->getFunction()->getASTContext();
  StringRef varName = getVariableNameForValue(markedValue);

  LLVM_DEBUG(llvm::dbgs() << "Emitting no copy error!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Mark: " << *markedValue);
  LLVM_DEBUG(llvm::dbgs() << "    Consuming Use: " << *consumingUse);

  // Otherwise, we need to do no implicit copy semantics. If our last use was
  // consuming message:
  diagnose(astContext,
           markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
           diag::sil_moveonlychecker_guaranteed_value_consumed, varName);
  diagnose(astContext, consumingUse->getLoc().getSourceLoc(),
           diag::sil_moveonlychecker_consuming_use_here);
  valuesWithDiagnostics.insert(markedValue);
}
