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
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/raw_ostream.h"
#include <cstddef>
#include <functional>

using namespace swift;
using namespace constraints;

enum ConflictFlag : unsigned {
  Nominal = 1 << 1,
  NominalArchetype = 1 << 2,
  NominalFunction = 1 << 3,
  Conformance = 1 << 3,
  Mutability = 1 << 4
};
using ConflictReason = OptionSet<ConflictFlag>;

static bool shouldBeConservativeWithProto(ProtocolDecl *proto) {
  if (proto->isMarkerProtocol())
    return true;

  if (proto->isObjC())
    return true;

  return false;
}

static ConflictReason canPossiblyConvertTo(
    ConstraintSystem &cs,
    Type argType, Type paramType,
    GenericSignature sig) {
  if (paramType->is<FunctionType>() &&
      argType->getAnyNominal()) {
    return ConflictFlag::NominalFunction;
  }

  if (!inference::hasConversions(paramType)) {
    if (paramType->getAnyNominal()) {
      if (argType->is<ArchetypeType>())
        return ConflictFlag::NominalArchetype;

      if (argType->getAnyNominal() &&
          argType->getAnyNominal() != paramType->getAnyNominal())
        return ConflictFlag::Nominal;
    }
  }

  if (sig &&
      paramType->isTypeParameter() &&
      (argType->getAnyNominal() ||
       argType->is<ArchetypeType>())) {
    bool failed = llvm::any_of(
        sig->getRequiredProtocols(paramType),
        [&](ProtocolDecl *proto) {
          if (shouldBeConservativeWithProto(proto))
            return false;
          return !lookupConformance(argType, proto);
        });
    if (failed)
      return ConflictFlag::Conformance;
  }

  return std::nullopt;
}

static ConflictReason canPossiblyConvertFrom(
    ConstraintSystem &cs,
    Type argType, Type paramType,
    GenericSignature sig) {
  if (!inference::hasConversions(argType)) {
    if (paramType->getAnyNominal()) {
      if (argType->is<ArchetypeType>())
        return ConflictFlag::NominalArchetype;
      if (argType->getAnyNominal() &&
          argType->getAnyNominal() != paramType->getAnyNominal())
        return ConflictFlag::Nominal;
    }
  }

  return std::nullopt;
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

static NullablePtr<Constraint> getApplicableFnConstraint(ConstraintGraph &CG,
                                                         Constraint *disjunction) {
  auto *boundVar = disjunction->getNestedConstraints()[0]
                       ->getFirstType()
                       ->getAs<TypeVariableType>();
  if (!boundVar)
    return nullptr;

  auto constraints =
      CG.gatherNearbyConstraints(boundVar, [](Constraint *constraint) {
        return constraint->getKind() == ConstraintKind::ApplicableFunction;
      });

  if (constraints.size() != 1)
    return nullptr;

  auto *applicableFn = constraints.front();
  // Unapplied disjunction could appear as a argument to applicable function,
  // we are not interested in that.
  return applicableFn->getSecondType()->isEqual(boundVar) ? applicableFn
                                                          : nullptr;
}

static void pruneDisjunctionsImpl(
    ConstraintSystem &cs, SmallVectorImpl<Constraint *> &disjunctions) {
  if (false && !cs.getASTContext().TypeCheckerOpts.SolverPruneDisjunctions)
    return;

  if (cs.shouldAttemptFixes())
    return;

  for (auto *disjunction : disjunctions) {
    auto applicableFn =
        getApplicableFnConstraint(cs.getConstraintGraph(), disjunction);
    if (applicableFn.isNull())
      continue;

    auto argFuncType =
        applicableFn.get()->getFirstType()->getAs<FunctionType>();

    auto argumentList = cs.getArgumentList(applicableFn.get()->getLocator());
    if (!argumentList)
      return;

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
      auto argType = cs.simplifyType(param.getPlainType());
      ASSERT(argType);
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
            return;
          }

          auto argIdx = argIndices.front();
          ASSERT(argIdx < argFuncType->getNumParams());
          auto argType = argTypes[argIdx];
          ASSERT(argType);

          const auto paramFlags = param.getParameterFlags();

          // If parameter is variadic we cannot compare because we don't know
          // real arity.
          if (paramFlags.isVariadic())
            return;

          auto paramType = param.getPlainType();

          if (paramFlags.isAutoClosure())
            paramType = paramType->castTo<AnyFunctionType>()->getResult();

          // `inout` parameter accepts only l-value argument.
          if (paramFlags.isInOut() && !argType->is<LValueType>()) {
            // reason |= ConflictReason::Mutability;
          }

          reason |= canPossiblyConvertTo(cs, argType, paramType, genericSig);
        }

        auto overloadResultType = overloadType->getResult();
        reason |= canPossiblyConvertFrom(cs, resultType, overloadResultType, genericSig);

        if (reason) {
          if (cs.isDebugMode()) {
            llvm::errs().indent(cs.solverState->getCurrentIndent())
                << "<<< Disabling choice ";
            choice->print(llvm::errs(),
                          &cs.getASTContext().SourceMgr,
                          cs.solverState->getCurrentIndent());
            llvm::errs() << " because";
            if (reason.contains(ConflictFlag::Nominal))
              llvm::errs() << " nominal";
            if (reason.contains(ConflictFlag::NominalArchetype))
              llvm::errs() << " nominal_archetype";
            if (reason.contains(ConflictFlag::NominalFunction))
              llvm::errs() << " nominal_function";
            if (reason.contains(ConflictFlag::Conformance))
              llvm::errs() << " conformance";
            if (reason.contains(ConflictFlag::Mutability))
              llvm::errs() << " mutability";
            llvm::errs() << "\n";
          }

          if (cs.solverState)
            cs.solverState->disableConstraint(choice);
          else
            choice->setDisabled();
        }
      });
  }
}

void ConstraintSystem::pruneDisjunctions(SmallVectorImpl<Constraint *> &disjunctions) {
  pruneDisjunctionsImpl(*this, disjunctions);
}