//===--- CSDisjunction.cpp - Disjunction pruning --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements optimizations which disable disjunction choices, ruling
// them out from further consideration by the solver. There are two main
// optimizations here:
//
// - When an applicable function constraint is first associated with a
//   disjunction, we filter choices by considering argument labels and
//   arity. This rules out choices which will never match, regardless
//   of type.
//
// - Every time the simplified type of an applicable function constraint
//   changes, we perform a further filtering step to disable choices
//   whose parameter and result types can never match the types at the
//   call site.
//
//===----------------------------------------------------------------------===//

#include "OpenedExistentials.h"
#include "TypeChecker.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericSignature.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Basic/Statistic.h"
#include "swift/Sema/ConstraintGraph.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/CSDisjunction.h"
#include "swift/Sema/CSBindings.h"
#include "swift/Sema/Subtyping.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/raw_ostream.h"
#include <cstddef>
#include <functional>

#define DEBUG_TYPE "CSLookahead"
#include "llvm/Support/Debug.h"

STATISTIC(NumDisjunctionsSkipped, "disjunctions skipped by pruning");
STATISTIC(NumDisjunctionsAnalyzed, "disjunction pruning rounds");
STATISTIC(NumDisjunctionsPruned, "disjunction pruning rounds");

using namespace swift;
using namespace constraints;

SolverDisjunction &ConstraintSystem::getRemainingDisjunction(Constraint *disjunction) {
  auto found = RemainingDisjunctions.find(disjunction);
  if (found != RemainingDisjunctions.end())
    return found->second;
  found = RemainingDisjunctions.insert(
      std::make_pair(disjunction, SolverDisjunction(disjunction))).first;
  return found->second;
}

static void forEachDisjunctionChoice(
    ConstraintSystem &cs, Constraint *disjunction,
    llvm::function_ref<void(Constraint *, ValueDecl *decl, FunctionType *)>
        callback) {
  for (auto constraint : disjunction->getNestedConstraints()) {
    if (constraint->isDisabled())
      continue;

    if (constraint->getKind() != ConstraintKind::BindOverload)
      continue;

    auto choice = constraint->getOverloadChoice();
    auto *decl = choice.getDeclOrNull();
    if (!decl)
      continue;

    Type overloadType = cs.getEffectiveOverloadType(
        disjunction->getLocator(), choice,
        /*allowMembers=*/true, constraint->getDeclContext());

    if (!overloadType || !overloadType->is<FunctionType>())
      continue;

    callback(constraint, decl, overloadType->castTo<FunctionType>());
  }
}

static const bool verifyIncrementalDisjunctionPruning = false;

/// Lazily prune a disjunction if the argument and result types at the call site
/// changed since last time.
void SolverDisjunction::pruneDisjunctionIfNeeded(ConstraintSystem &cs,
                                                 Constraint *applicableFn) {
  if (!cs.getASTContext().TypeCheckerOpts.SolverPruneDisjunctions)
    return;

  if (cs.shouldAttemptFixes())
    return;

  if (!applicableFn)
    return;

  auto PO = PrintOptions::forDebugging();

  // The below only depends on the overload choices and argument types, so
  // we can skip it if the argument type is already known.
  auto newFuncType =
      cs.simplifyType(applicableFn->getFirstType())->castTo<FunctionType>();
  if (newFuncType == argFuncType) {
    ++NumDisjunctionsSkipped;
    LLVM_DEBUG(llvm::dbgs() << "No change: " << newFuncType->getString(PO) << "\n");
    if (verifyIncrementalDisjunctionPruning)
      pruneDisjunction(cs, applicableFn, /*verify=*/true);
    return;
  }

  ++NumDisjunctionsAnalyzed;
  LLVM_DEBUG(llvm::dbgs() << "Apply function type change from: "
                          << argFuncType->getString(PO)
                          << " to "
                          << newFuncType->getString(PO) << "\n");

  // Save the old apply type in the trail. If we backtrack, we will
  // un-disable any choices we disabled, and also restore the previous
  // saved type for the disjunction.
  if (cs.solverState) {
    cs.recordChange(
      SolverTrail::Change::PrunedDisjunction(disjunction, argFuncType));
  }
  argFuncType = newFuncType;

  pruneDisjunction(cs, applicableFn, /*verify=*/false);
}

/// Prune a disjunction by considering the argument and result types at the
/// call site.
void SolverDisjunction::pruneDisjunction(ConstraintSystem &cs,
                                         Constraint *applicableFn,
                                         bool verify) {
  auto argumentList = cs.getArgumentList(applicableFn->getLocator());
  ASSERT(argumentList);

  for (const auto &argument : *argumentList) {
    if (auto *expr = argument.getExpr()) {
      // Directly `<#...#>` or has one inside.
      if (isa<CodeCompletionExpr>(expr) ||
          cs.containsIDEInspectionTarget(expr))
        return;
    }
  }

  auto matchArguments = [&](OverloadChoice choice, FunctionType *overloadType)
    -> std::optional<MatchCallArgumentResult> {
    auto *decl = choice.getDecl();

    SmallVector<FunctionType::Param, 8> argsWithLabels;
    argsWithLabels.append(argFuncType->getParams().begin(),
                          argFuncType->getParams().end());
    FunctionType::relabelParams(argsWithLabels, argumentList);

    auto hasAppliedSelf =
        decl->hasCurriedSelf() &&
        doesMemberRefApplyCurriedSelf(choice.getBaseType(), decl);

    ParameterListInfo paramListInfo(overloadType->getParams(), decl,
                                    hasAppliedSelf);

    MatchCallArgumentListener listener;
    return matchCallArguments(argsWithLabels, overloadType->getParams(),
                              paramListInfo,
                              argumentList->getFirstTrailingClosureIndex(),
                              /*allow fixes*/ false, listener, std::nullopt);
  };

  bool anyChanges = false;

  forEachDisjunctionChoice(
    cs, disjunction,
    [&](Constraint *choice, ValueDecl *decl, FunctionType *overloadType) {
      // Get the generic signature used for reasoning about type parameters
      // in the overload's parameter and result types.
      GenericSignature genericSig;
      {
        if (auto *GF = dyn_cast<AbstractFunctionDecl>(decl)) {
          genericSig = GF->getGenericSignature();
        } else if (auto *SD = dyn_cast<SubscriptDecl>(decl)) {
          genericSig = SD->getGenericSignature();
        }
      }

      auto matchings =
          matchArguments(choice->getOverloadChoice(), overloadType);
      if (!matchings) {
        if (cs.isDebugMode()) {
          llvm::errs().indent(cs.solverState->getCurrentIndent())
              << "<<< Matching failed with ";
          choice->print(llvm::errs(),
                        &cs.getASTContext().SourceMgr,
                        cs.solverState->getCurrentIndent());
          llvm::errs() << "\n";
        }
        return;
      }

      // This is important for SIMD operators in particular because
      // a lot of their overloads have same-type requires to a concrete
      // type:  `<Scalar == (U)Int*>(_: SIMD*<Scalar>, ...) -> ...`.
      if (genericSig) {
        overloadType = overloadType->getReducedType(genericSig)
                           ->castTo<FunctionType>();
      }

      ConflictReason reason;
      for (unsigned paramIdx = 0, n = overloadType->getNumParams();
           paramIdx != n; ++paramIdx) {
        const auto &param = overloadType->getParams()[paramIdx];
        const auto paramFlags = param.getParameterFlags();

        // If parameter is variadic we cannot compare because we don't know
        // real arity.
        if (paramFlags.isVariadic())
          continue;

        auto argIndices = matchings->parameterBindings[paramIdx];
        switch (argIndices.size()) {
        case 0:
          // Current parameter is defaulted, mark and continue.
          continue;

        case 1:
          // One-to-one match between argument and parameter.
          break;

        default:
          // Cannot deal with multiple possible matchings at the moment.
          continue;
        }

        auto argIdx = argIndices.front();
        ASSERT(argIdx < argFuncType->getNumParams());
        auto argParam = argFuncType->getParams()[argIdx];

        // FIXME: Get rid of the usage of InOutType here.
        auto argType = argParam.getOldType();
        auto paramType = param.getOldType();

        if (paramFlags.isAutoClosure())
          paramType = paramType->castTo<FunctionType>()->getResult();

        reason |= canPossiblyConvertTo(cs, argType, paramType, genericSig);
      }

      auto overloadResultType = overloadType->getResult();
      auto applyResultType = argFuncType->getResult();
      reason |= canPossiblyConvertTo(cs, overloadResultType,
                                     applyResultType, genericSig);

      if (reason) {
        if (cs.isDebugMode()) {
          llvm::errs().indent(cs.solverState->getCurrentIndent() + 4)
              << "(disabled choice ";
          choice->print(llvm::errs(),
                        &cs.getASTContext().SourceMgr,
                        cs.solverState->getCurrentIndent());
          llvm::errs() << " because ";
          simple_display(llvm::errs(), reason);
          llvm::errs() << ")\n";
        }
        ASSERT(!verify);

        if (cs.solverState)
          cs.solverState->disableConstraint(choice);
        else
          choice->setDisabled();

        if (!anyChanges) {
          ++NumDisjunctionsPruned;
          anyChanges = true;
        }
      }
    });
}
