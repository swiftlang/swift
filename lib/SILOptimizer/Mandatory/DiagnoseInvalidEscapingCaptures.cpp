//===--- DiagnoseInvalidEscapingCaptures.cpp ------------------------------===//
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
// This file implements a diagnostic pass to diagnose escaping closures that
// capture mutable storage locations or noescape function values.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Types.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(ASTContext &Context, SILLocation loc,
                                   Diag<T...> diag, U &&... args) {
  return Context.Diags.diagnose(loc.getSourceLoc(), diag,
                                std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(ASTContext &Context, SourceLoc loc,
                                   Diag<T...> diag, U &&... args) {
  return Context.Diags.diagnose(loc, diag,
                                std::forward<U>(args)...);
}

// Check if a use of a value derived from a partial_apply can cause the
// closure to escape. For "pass-through" uses that build a new value from
// the partial_apply, followUses() is called to evaluate the uses of the
// derived value.
template <typename FollowUse>
static bool checkNoEscapePartialApplyUse(Operand *oper, FollowUse followUses) {
  SILInstruction *user = oper->getUser();

  if (isa<ConvertEscapeToNoEscapeInst>(user) ||
      isa<CopyBlockWithoutEscapingInst>(user))
    return false;

  // Ignore uses that are totally uninteresting. partial_apply [stack] is
  // terminated by a dealloc_stack instruction.
  if (isIncidentalUse(user) || onlyAffectsRefCount(user) ||
      isa<DeallocStackInst>(user))
    return false;

  // Before checking conversions in general below (getSingleValueCopyOrCast),
  // check for convert_function to [without_actually_escaping]. Assume such
  // conversion are not actually escaping without following their uses.
  if (auto *CFI = dyn_cast<ConvertFunctionInst>(user)) {
    if (CFI->withoutActuallyEscaping())
      return false;
  }

  // Look through copies, borrows, and conversions.
  if (SingleValueInstruction *copy = getSingleValueCopyOrCast(user)) {
    // Only follow the copied operand. Other operands are incidental,
    // as in the second operand of mark_dependence.
    if (oper->getOperandNumber() == 0)
      followUses(copy);

    return false;
  }

  // @noescape block storage can be passed as an Optional (Nullable).
  if (auto *EI = dyn_cast<EnumInst>(user)) {
    followUses(EI);
    return false;
  }

  // Look through Phis.
  if (auto *BI = dyn_cast<BranchInst>(user)) {
    const SILPhiArgument *arg = BI->getArgForOperand(oper);
    followUses(arg);
    return false;
  }

  if (auto *CBI = dyn_cast<CondBranchInst>(user)) {
    const SILPhiArgument *arg = CBI->getArgForOperand(oper);
    if (arg) // If the use isn't the branch condition, follow it.
      followUses(arg);
    return false;
  }

  // Look through ObjC closures.
  if (auto *SI = dyn_cast<StoreInst>(user)) {
    if (oper->getOperandNumber() == StoreInst::Src) {
      if (auto *PBSI = dyn_cast<ProjectBlockStorageInst>(
            SI->getDest())) {
        SILValue storageAddr = PBSI->getOperand();
        // The closure is stored to block storage. Recursively visit all
        // uses of any initialized block storage values derived from this
        // storage address..
        for (Operand *oper : storageAddr->getUses()) {
          if (auto *IBS = dyn_cast<InitBlockStorageHeaderInst>(oper->getUser()))
            followUses(IBS);
        }
        return false;
      }
    }
  }

  if (auto *PAI = dyn_cast<PartialApplyInst>(user)) {
    // Recurse through partial_apply chains.
    if (oper->get() == PAI->getCallee()) {
      followUses(PAI);
      return false;
    }

    // Look through re-abstraction thunks.
    if (isPartialApplyOfReabstractionThunk(PAI)) {
      // However, first check for withoutActuallyEscaping, which is always
      // a valid non-escaping use.
      SILFunction *thunkDef = PAI->getReferencedFunctionOrNull();
      if (!thunkDef)
        return true;
      if (!thunkDef->isWithoutActuallyEscapingThunk())
        followUses(PAI);
      return false;
    }
  }

  // Anything else is flagged as an escaping use.
  return true;
}

const ParamDecl *getParamDeclFromOperand(SILValue value) {
  if (auto *arg = dyn_cast<SILArgument>(value))
    if (auto *decl = dyn_cast_or_null<ParamDecl>(arg->getDecl()))
      return decl;

  return nullptr;
}

bool isUseOfSelfInInitializer(Operand *oper) {
  if (auto *PBI = dyn_cast<ProjectBoxInst>(oper->get())) {
    if (auto *MUI = dyn_cast<MarkUninitializedInst>(PBI->getOperand())) {
      switch (MUI->getKind()) {
      case MarkUninitializedInst::Kind::Var:
        return false;
      case MarkUninitializedInst::Kind::RootSelf:
      case MarkUninitializedInst::Kind::CrossModuleRootSelf:
      case MarkUninitializedInst::Kind::DerivedSelf:
      case MarkUninitializedInst::Kind::DerivedSelfOnly:
      case MarkUninitializedInst::Kind::DelegatingSelf:
      case MarkUninitializedInst::Kind::DelegatingSelfAllocated:
        return true;
      }

      llvm_unreachable("Bad MarkUninitializedInst::Kind");
    }
  }

  return false;
}

static bool checkForEscapingPartialApplyUses(PartialApplyInst *PAI) {
  // Avoid exponential path exploration.
  SmallVector<Operand *, 8> uses;
  llvm::SmallDenseSet<Operand *, 8> visited;
  auto uselistInsert = [&](Operand *operand) {
    if (visited.insert(operand).second)
      uses.push_back(operand);
  };

  for (Operand *use : PAI->getUses())
    uselistInsert(use);


  // Search for any uses of the closure that might potentially escape.
  bool foundEscapingUse = false;
  while (!uses.empty()) {
    Operand *oper = uses.pop_back_val();
    foundEscapingUse |= checkNoEscapePartialApplyUse(oper, [&](SILValue V) {
      for (Operand *use : V->getUses())
        uselistInsert(use);
    });
  }

  // If there aren't any, we're fine.
  return foundEscapingUse;
}

// Given a partial_apply forming a closure, together with one of its operands,
// find a usage of the corresponding argument inside the closure body, and
// diagnose it as a capture use.
//
// This makes a best-effort attempt at finding a "good" capture usage; it may
// not emit anything.
//
// The \c DC parameter is the DeclContext of the original function being
// analyzed by this diagnostic pass. We use it to distinguish calls of closures
// from calls of other unrelated functions, by checking the DeclContext of the
// called closure.
static void diagnoseCaptureLoc(ASTContext &Context, DeclContext *DC,
                               PartialApplyInst *PAI, Operand *oper) {
  assert(DC != nullptr &&
         "Invalid capture in function with no source location information");

  SmallVector<Operand *, 8> uses;
  llvm::SmallDenseSet<Operand *, 8> visited;
  auto uselistInsert = [&](Operand *operand) {
    if (visited.insert(operand).second)
      uses.push_back(operand);
  };

  auto lookInsideClosure = [&](ApplySite site, Operand *oper) -> bool {
    auto *F = site.getCalleeFunction();
    if (F == nullptr || F->empty())
      return false;

    auto *otherDC = F->getDeclContext();
    if (otherDC == nullptr || DC == nullptr ||
        !otherDC->isChildContextOf(DC))
      return false;

    // Map an operand of an apply instruction to an argument inside
    // the callee.
    auto args = F->getArguments();
    auto argIndex = site.getCalleeArgIndex(*oper);
    auto arg = args[argIndex];

    // Look for a usage of the callee argument.
    for (Operand *use : arg->getUses())
      uselistInsert(use);

    return true;
  };

  lookInsideClosure(PAI, oper);

  while (!uses.empty()) {
    Operand *oper = uses.pop_back_val();
    SILInstruction *user = oper->getUser();

    if (isIncidentalUse(user) || onlyAffectsRefCount(user))
      continue;

    // Look through copies, borrows, and conversions.
    if (SingleValueInstruction *copy = getSingleValueCopyOrCast(user)) {
      // Only follow the copied operand. Other operands are incidental,
      // as in the second operand of mark_dependence.
      if (oper->getOperandNumber() == 0) {
        for (auto *use : copy->getUses())
          uselistInsert(use);
        continue;
      }
    }

    // If the usage is a capture of the value by another closure, look inside
    // the body of that closure.
    if (auto site = ApplySite::isa(user)) {
      if (lookInsideClosure(site, oper)) {
        diagnose(Context, site.getLoc(), diag::value_captured_transitively);
        continue;
      }
    }

    // Otherwise, we might have found one of the "real" usages of the capture.
    // Diagnose it here.
    SILValue val = oper->get();
    SILLocation loc = val.getLoc();
    if (loc.isASTNode<VarDecl>())
      loc = user->getLoc();
    diagnose(Context, loc, diag::value_captured_here);
  }
}

// Diagnose this partial_apply if it captures a non-escaping value and has
// an escaping use.
static void checkPartialApply(ASTContext &Context, DeclContext *DC,
                              PartialApplyInst *PAI) {
  // Re-abstraction thunks are not useful to look at. We'll diagnose the
  // original closure instead.
  if (isPartialApplyOfReabstractionThunk(PAI))
    return;

  ApplySite apply(PAI);

  // Collect any non-escaping captures.
  SmallVector<Operand *, 2> inoutCaptures;
  SmallVector<Operand *, 2> noEscapeCaptures;

  for (auto &oper : apply.getArgumentOperands()) {
    SILValue value = oper.get();

    // Captures of inout parameters cannot escape.
    if (apply.getArgumentConvention(oper)
            == SILArgumentConvention::Indirect_InoutAliasable)
      inoutCaptures.push_back(&oper);

    // Captures of noescape function types or tuples containing noescape
    // function types cannot escape.
    if (value->getType().getASTType()->isNoEscape()) {
      noEscapeCaptures.push_back(&oper);
    }
  }

  // A partial_apply without non-escaping captures is always valid.
  if (inoutCaptures.empty() && noEscapeCaptures.empty())
    return;

  // A partial_apply without escaping uses is always valid.
  if (!checkForEscapingPartialApplyUses(PAI))
    return;

  // Otherwise, we have at least one escaping use of a partial_apply
  // capturing a non-escaping value. We need to emit diagnostics.

  // Should match SELECT_ESCAPING_CLOSURE_KIND in DiagnosticsSIL.def.
  enum {
    EscapingLocalFunction,
    EscapingClosure
  } functionKind = EscapingClosure;

  if (auto *F = PAI->getReferencedFunctionOrNull()) {
    if (auto loc = F->getLocation()) {
      if (loc.isASTNode<FuncDecl>())
        functionKind = EscapingLocalFunction;
    }
  }
  // First, diagnose the inout captures, if any.
  for (auto inoutCapture : inoutCaptures) {
    if (isUseOfSelfInInitializer(inoutCapture)) {
      diagnose(Context, PAI->getLoc(), diag::escaping_mutable_self_capture,
               functionKind);
    } else {
      auto *param = getParamDeclFromOperand(inoutCapture->get());
      if (param->isSelfParameter())
        diagnose(Context, PAI->getLoc(), diag::escaping_mutable_self_capture,
                 functionKind);
      else {
        diagnose(Context, PAI->getLoc(), diag::escaping_inout_capture,
                 functionKind, param->getName());
        diagnose(Context, param->getLoc(), diag::inout_param_defined_here,
                 param->getName());
      }
    }

    diagnoseCaptureLoc(Context, DC, PAI, inoutCapture);
  }

  // Finally, diagnose captures of values with noescape type.
  for (auto noEscapeCapture : noEscapeCaptures) {
    if (auto *param = getParamDeclFromOperand(noEscapeCapture->get())) {
      diagnose(Context, PAI->getLoc(), diag::escaping_noescape_param_capture,
               functionKind, param->getName());
      diagnose(Context, param->getLoc(), diag::noescape_param_defined_here,
               param->getName());
    } else {
      diagnose(Context, PAI->getLoc(), diag::escaping_noescape_var_capture,
               functionKind);
    }

    diagnoseCaptureLoc(Context, DC, PAI, noEscapeCapture);
  }
}

// Enforce exclusivity restrictions on recursive uses of non-escaping closures.
// Exclusivity requires a Non-Escaping Recursion Restriction rule (SE-0176):
// A non-escaping closure A may not be recursively invoked during the
// execution of a non-escaping closure B which captures the same local
// variable or inout parameter unless:
// - A is defined within B or
// - A is a local function declaration which is referenced directly by B.
//
// This is conservatively approximated with a Non-Escaping Parameter Call
// Restriction rule (NPCR), as implemented below:
// A function may not call a non-escaping function parameter passing a
// non-escaping function parameter as an argument.
// For the purposes of this rule, a closure which captures a non-escaping
// function parameter is treated the same as the parameter.
//
// Note: The compiler does not enforce recursion via
// withoutActuallyEscaping. This undefined behavior is exposed to programmers.
//
// TODO: Verify that all uses of noescaping function arguments are SIL patterns
// that are recognized below to prove that this diagnostic is complete.
static void checkApply(ASTContext &Context, FullApplySite site) {
  auto isNoEscapeParam = [&](SILValue value) -> const ParamDecl * {
    // If the value is an escaping, do not enforce any restrictions.
    if (!value->getType().getASTType()->isNoEscape())
      return nullptr;

    // If the value is not a function parameter, do not enforce any restrictions.
    return getParamDeclFromOperand(value);
  };

  // If the callee is not a no-escape parameter, there is nothing to check.
  auto callee = site.getCalleeOrigin();
  if (!isNoEscapeParam(callee))
    return;

  // See if any of our arguments are noescape parameters, or closures capturing
  // noescape parameters.
  SmallVector<std::pair<SILValue, bool>, 4> args;
  llvm::SmallDenseSet<SILValue, 4> visited;
  auto arglistInsert = [&](SILValue arg, bool capture) {
    if (visited.insert(arg).second)
      args.emplace_back(arg, capture);
  };

  for (auto arg : site.getArguments())
    arglistInsert(arg, /*capture=*/false);

  while (!args.empty()) {
    auto pair = args.pop_back_val();
    auto arg = pair.first;
    bool capture = pair.second;

    if (auto *CI = dyn_cast<ConversionInst>(arg)) {
      arglistInsert(CI->getConverted(), /*capture=*/false);
      continue;
    }

    // If one of our call arguments is a noescape parameter, diagnose the
    // violation.
    if (auto *param = isNoEscapeParam(arg)) {
      diagnose(Context, site.getLoc(), diag::err_noescape_param_call,
               param->getName(), capture);
      return;
    }

    // If one of our call arguments is a closure, recursively visit all of
    // the closure's captures.
    if (auto *PAI = dyn_cast<PartialApplyInst>(arg)) {
      ApplySite site(PAI);
      for (auto arg : site.getArguments())
        arglistInsert(arg, /*capture=*/true);
      continue;
    }
  }
}

static void checkForViolationsAtInstruction(ASTContext &Context,
                                            DeclContext *DC,
                                            SILInstruction *I) {
  if (auto *PAI = dyn_cast<PartialApplyInst>(I))
    checkPartialApply(Context, DC, PAI);

  if (isa<ApplyInst>(I) || isa<TryApplyInst>(I)) {
    FullApplySite site(I);
    checkApply(Context, site);
  }
}

static void checkEscapingCaptures(SILFunction *F) {
  if (F->empty())
    return;

  auto &Context =F->getASTContext();
  auto *DC = F->getDeclContext();

  for (auto &BB : *F) {
    for (auto &I : BB)
      checkForViolationsAtInstruction(Context, DC, &I);
  }
}

namespace {

class DiagnoseInvalidEscapingCaptures : public SILFunctionTransform {
public:
  DiagnoseInvalidEscapingCaptures() {}

private:
  void run() override {
    SILFunction *F = getFunction();

    // Don't rerun diagnostics on deserialized functions.
    if (F->wasDeserializedCanonical())
      return;

    checkEscapingCaptures(F);
  }
};

} // end anonymous namespace

SILTransform *swift::createDiagnoseInvalidEscapingCaptures() {
  return new DiagnoseInvalidEscapingCaptures();
}
