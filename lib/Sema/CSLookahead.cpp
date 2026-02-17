//===--- CSLookahead.cpp - Experimental Optimization ----------------------===//
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
// This file implements FOO.
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
#include "swift/Sema/ConstraintGraph.h"
#include "swift/Sema/ConstraintSystem.h"
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

using namespace swift;
using namespace constraints;

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

static void pruneDisjunctionImpl(
    ConstraintSystem &cs, Constraint *disjunction,
    Constraint *applicableFn) {
  if (!cs.getASTContext().TypeCheckerOpts.SolverPruneDisjunctions)
    return;

  if (cs.shouldAttemptFixes())
    return;

  if (!applicableFn)
    return;

  auto argFuncType =
      applicableFn->getFirstType()->castTo<FunctionType>();

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

  SmallVector<FunctionType::Param, 8> argsWithLabels;
  {
    argsWithLabels.append(argFuncType->getParams().begin(),
                          argFuncType->getParams().end());
    FunctionType::relabelParams(argsWithLabels, argumentList);
  }

  SmallVector<Type, 2> argTypes;
  argTypes.resize(argFuncType->getNumParams());

  for (unsigned i = 0, n = argFuncType->getNumParams(); i != n; ++i) {
    const auto &param = argFuncType->getParams()[i];
    // FIXME: Get rid of the usage of InOutType here.
    auto argType = cs.simplifyType(param.getOldType());
    argTypes[i] = argType;
  }

  auto resultType = cs.simplifyType(argFuncType->getResult());

  auto matchArguments = [&](OverloadChoice choice, FunctionType *overloadType)
    -> std::optional<MatchCallArgumentResult> {
        auto *decl = choice.getDeclOrNull();
    assert(decl);

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

  forEachDisjunctionChoice(
    cs, disjunction,
    [&](Constraint *choice, ValueDecl *decl, FunctionType *overloadType) {
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
        auto argType = argTypes[argIdx];
        ASSERT(argType);

        const auto paramFlags = param.getParameterFlags();

        // If parameter is variadic we cannot compare because we don't know
        // real arity.
        if (paramFlags.isVariadic())
          continue;

        // FIXME: Get rid of the usage of InOutType here.
        auto paramType = param.getOldType();

        if (paramFlags.isAutoClosure())
          paramType = paramType->castTo<AnyFunctionType>()->getResult();

        reason |= canPossiblyConvertTo(cs, argType, paramType, genericSig);
      }

      auto overloadResultType = overloadType->getResult();
      reason |= canPossiblyConvertTo(cs, overloadResultType, resultType, genericSig);

      if (reason) {
        if (cs.isDebugMode()) {
          llvm::errs().indent(cs.solverState->getCurrentIndent() + 4)
              << "(disabled choice ";
          choice->print(llvm::errs(),
                        &cs.getASTContext().SourceMgr,
                        cs.solverState->getCurrentIndent());
          llvm::errs() << " because";
          if (reason.contains(ConflictFlag::Category))
            llvm::errs() << " category";
          if (reason.contains(ConflictFlag::Exact))
            llvm::errs() << " exact";
          if (reason.contains(ConflictFlag::Class))
            llvm::errs() << " class";
          if (reason.contains(ConflictFlag::Structural))
            llvm::errs() << " structural";
          if (reason.contains(ConflictFlag::Array))
            llvm::errs() << " array";
          if (reason.contains(ConflictFlag::DictionaryKey))
            llvm::errs() << " dictionary_key";
          if (reason.contains(ConflictFlag::DictionaryValue))
            llvm::errs() << " dictionary_value";
          if (reason.contains(ConflictFlag::Set))
            llvm::errs() << " set";
          if (reason.contains(ConflictFlag::Optional))
            llvm::errs() << " optional";
          if (reason.contains(ConflictFlag::Structural))
            llvm::errs() << " structural";
          if (reason.contains(ConflictFlag::Conformance))
            llvm::errs() << " conformance";
          llvm::errs() << ")\n";
        }

        if (cs.solverState)
          cs.solverState->disableConstraint(choice);
        else
          choice->setDisabled();
      }
    });
}

void ConstraintSystem::pruneDisjunction(Constraint *disjunction, Constraint *applicableFn) {
  pruneDisjunctionImpl(*this, disjunction, applicableFn);
}