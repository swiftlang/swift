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
#include "swift/Sema/TypeVariableType.h"
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

std::optional<std::pair<Constraint *, unsigned>>
ConstraintSystem::findConstraintThroughOptionals(
    TypeVariableType *typeVar, OptionalWrappingDirection optionalDirection,
    llvm::function_ref<bool(Constraint *, TypeVariableType *)> predicate) {
  unsigned numOptionals = 0;
  auto *rep = getRepresentative(typeVar);

  SmallPtrSet<TypeVariableType *, 4> visitedVars;
  while (visitedVars.insert(rep).second) {
    // Look for a disjunction that binds this type variable to an overload set.
    TypeVariableType *optionalObjectTypeVar = nullptr;
    auto constraints = getConstraintGraph().gatherNearbyConstraints(
        rep,
        [&](Constraint *match) {
          // If we have an "optional object of" constraint, we may need to
          // look through it to find the constraint we're looking for.
          if (match->getKind() != ConstraintKind::OptionalObject)
            return predicate(match, rep);

          switch (optionalDirection) {
          case OptionalWrappingDirection::Promote: {
            // We want to go from T to T?, so check if we're on the RHS, and
            // move over to the LHS if we can.
            auto rhsTypeVar = match->getSecondType()->getAs<TypeVariableType>();
            if (rhsTypeVar && getRepresentative(rhsTypeVar) == rep) {
              optionalObjectTypeVar =
                  match->getFirstType()->getAs<TypeVariableType>();
            }
            break;
          }
          case OptionalWrappingDirection::Unwrap: {
            // We want to go from T? to T, so check if we're on the LHS, and
            // move over to the RHS if we can.
            auto lhsTypeVar = match->getFirstType()->getAs<TypeVariableType>();
            if (lhsTypeVar && getRepresentative(lhsTypeVar) == rep) {
              optionalObjectTypeVar =
                  match->getSecondType()->getAs<TypeVariableType>();
            }
            break;
          }
          }
          // Don't include the optional constraint in the results.
          return false;
        });

    // If we found a result, return it.
    if (!constraints.empty())
      return std::make_pair(constraints[0], numOptionals);

    // If we found an "optional object of" constraint, follow it.
    if (optionalObjectTypeVar && !getFixedType(optionalObjectTypeVar)) {
      numOptionals += 1;
      rep = getRepresentative(optionalObjectTypeVar);
      continue;
    }

    // Otherwise we're done.
    return std::nullopt;
  }
  return std::nullopt;
}

Constraint *
ConstraintSystem::getApplicableFnConstraint(Constraint *disjunction) {
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

ConstraintSystem::SolutionKind
ConstraintSystem::filterDisjunction(
    Constraint *disjunction, bool restoreOnFail,
    llvm::function_ref<bool(Constraint *)> pred) {
  assert(disjunction->getKind() == ConstraintKind::Disjunction);

  SmallVector<Constraint *, 4> constraintsToRestoreOnFail;
  unsigned choiceIdx = 0;
  unsigned numEnabledTerms = 0;
  ASTContext &ctx = getASTContext();
  for (unsigned constraintIdx : indices(disjunction->getNestedConstraints())) {
    auto constraint = disjunction->getNestedConstraints()[constraintIdx];

    // Skip already-disabled constraints. Let's treat disabled
    // choices which have a fix as "enabled" ones here, so we can
    // potentially infer some type information from them.
    if (constraint->isDisabled() && !constraint->getFix())
      continue;

    if (pred(constraint)) {
      ++numEnabledTerms;
      choiceIdx = constraintIdx;
      continue;
    }

    if (isDebugMode()) {
      auto indent = (solverState ? solverState->getCurrentIndent() : 0) + 4;
      llvm::errs().indent(indent) << "(disabled disjunction term ";
      constraint->print(llvm::errs(), &ctx.SourceMgr, indent);
      llvm::errs().indent(indent) << ")\n";
    }

    if (!constraint->isDisabled()) {
      if (restoreOnFail)
        constraintsToRestoreOnFail.push_back(constraint);
      else if (solverState)
        solverState->disableConstraint(constraint);
      else
        constraint->setDisabled();
    }
  }

  if (numEnabledTerms == 0)
    return SolutionKind::Error;

  if (restoreOnFail) {
    for (auto constraint : constraintsToRestoreOnFail) {
      if (solverState)
        solverState->disableConstraint(constraint);
      else
        constraint->setDisabled();
    }
  }

  if (numEnabledTerms == 1) {
    // Only a single constraint remains. Retire the disjunction and make
    // the remaining constraint active.
    auto choice = disjunction->getNestedConstraints()[choiceIdx];

    // This can only happen when subscript syntax is used to lookup
    // something which doesn't exist in type marked with
    // `@dynamicMemberLookup`.
    // Since filtering currently runs as part of the `applicable function`
    // constraint processing, "keypath dynamic member lookup" choice can't
    // be attempted in-place because that would also try to operate on that
    // constraint, so instead let's keep the disjunction, but disable all
    // unviable choices.
    if (choice->getOverloadChoice().isKeyPathDynamicMemberLookup()) {
      for (auto *currentChoice : disjunction->getNestedConstraints()) {
        if (currentChoice->isDisabled())
          continue;

        if (currentChoice != choice)
          solverState->disableConstraint(currentChoice);
      }
      return SolutionKind::Solved;
    }

    // Retire the disjunction. It's been solved.
    retireConstraint(disjunction);

    // Note the choice we made and simplify it. This introduces the
    // new constraint into the system.
    if (disjunction->shouldRememberChoice()) {
      recordDisjunctionChoice(disjunction->getLocator(), choiceIdx);
    }

    if (isDebugMode()) {
      auto indent = (solverState ? solverState->getCurrentIndent() : 0) + 4;
      llvm::errs().indent(indent)
          << "(introducing single enabled disjunction term ";
      choice->print(llvm::errs(), &ctx.SourceMgr, indent);
      llvm::errs().indent(indent) << ")\n";
    }

    simplifyDisjunctionChoice(choice);

    return failedConstraint ? SolutionKind::Unsolved : SolutionKind::Solved;
  }

  return SolutionKind::Unsolved;
}

static bool isCodeCompletionTypeVar(Type type) {
  if (auto *TVT = type->getAs<TypeVariableType>()) {
    if (TVT->getImpl().isCodeCompletionToken()) {
      return true;
    }
  }
  return false;
}

static bool areConservativelyCompatibleArgumentLabels(
    ConstraintSystem &cs, OverloadChoice choice,
    SmallVectorImpl<FunctionType::Param> &args,
    MatchCallArgumentListener &listener,
    std::optional<unsigned> unlabeledTrailingClosureArgIndex) {
  ValueDecl *decl = nullptr;
  switch (choice.getKind()) {
  case OverloadChoiceKind::Decl:
  case OverloadChoiceKind::DeclViaBridge:
  case OverloadChoiceKind::DeclViaDynamic:
  case OverloadChoiceKind::DeclViaUnwrappedOptional:
    decl = choice.getDecl();
    break;

  // KeyPath application is not filtered in `performMemberLookup`.
  case OverloadChoiceKind::KeyPathApplication:
  case OverloadChoiceKind::DynamicMemberLookup:
  case OverloadChoiceKind::KeyPathDynamicMemberLookup:
  case OverloadChoiceKind::TupleIndex:
  case OverloadChoiceKind::MaterializePack:
  case OverloadChoiceKind::ExtractFunctionIsolation:
    return true;
  }

  // If this is a member lookup, the call arguments (if we have any) will
  // generally be applied to the second level of parameters, with the member
  // lookup applying the curried self at the first level. But there are cases
  // where we can get an unapplied declaration reference back.
  auto hasAppliedSelf =
      decl->hasCurriedSelf() &&
      doesMemberRefApplyCurriedSelf(choice.getBaseType(), decl);

  AnyFunctionType *fnType = nullptr;
  if (decl->hasParameterList()) {
    fnType = decl->getInterfaceType()->castTo<AnyFunctionType>();
    if (hasAppliedSelf) {
      fnType = fnType->getResult()->getAs<AnyFunctionType>();
      assert(fnType && "Parameter list curry level does not match type");
    }
  } else if (auto *VD = dyn_cast<VarDecl>(decl)) {
    // For variables, we can reject any type that we know cannot be callable.
    auto varTy = VD->getValueInterfaceType()->lookThroughAllOptionalTypes();
    if (!varTy->mayBeCallable(cs.DC))
      return false;
    fnType = varTy->getAs<AnyFunctionType>();
  } else if (auto *MD = dyn_cast<MacroDecl>(decl)) {
    fnType = MD->getInterfaceType()->getAs<AnyFunctionType>();
  }

  // Given we want to be conservative with this checking, if there's any case
  // we can't match arguments for (e.g callable nominals, type parameters),
  // default to returning true.
  if (!fnType)
    return true;

  auto params = fnType->getParams();
  ParameterListInfo paramInfo(params, decl, hasAppliedSelf);

  return matchCallArguments(args, params, paramInfo,
                            unlabeledTrailingClosureArgIndex,
                            /*allow fixes*/ false, listener, std::nullopt)
      .has_value();
}

namespace {

class AllowLabelMismatches : public MatchCallArgumentListener {
  SmallVector<Identifier, 4> NewLabels;
  bool HadLabelingIssues = false;

public:
  bool missingLabel(unsigned paramIndex) override {
    HadLabelingIssues = true;
    return false;
  }

  bool relabelArguments(ArrayRef<Identifier> newLabels) override {
    NewLabels.append(newLabels.begin(), newLabels.end());
    HadLabelingIssues = true;
    return false;
  }

  bool hadLabelingIssues() const { return HadLabelingIssues; }

  std::optional<ArrayRef<Identifier>> getLabelReplacements() const {
    if (!hadLabelingIssues() || NewLabels.empty())
      return std::nullopt;

    return {NewLabels};
  }
};

}

/// Entry point invoked when an applicable function constraint becomes
/// associated with a disjunction.
///
/// Prunes the disjunction by considering argument labels and arity at
/// the call site. Also, binds a common return type, if there is one.
bool ConstraintSystem::simplifyAppliedOverloadsImpl(
    Constraint *disjunction, TypeVariableType *fnTypeVar,
    FunctionType *argFnType, unsigned numOptionalUnwraps,
    ConstraintLocatorBuilder locator) {
  // Don't attempt to filter overloads when solving for code completion
  // because presence of code completion token means that any call
  // could be malformed e.g. missing arguments e.g. `foo([.#^MEMBER^#`
  if (isForCodeCompletion()) {
    bool ArgContainsCCTypeVar = Type(argFnType).findIf(isCodeCompletionTypeVar);
    if (ArgContainsCCTypeVar || isCodeCompletionTypeVar(fnTypeVar)) {
      return false;
    }
  }

  if (shouldAttemptFixes()) {
    auto arguments = argFnType->getParams();
    bool allHoles =
        arguments.size() > 0 &&
        llvm::all_of(arguments, [&](const AnyFunctionType::Param &arg) -> bool {
          auto argType = arg.getPlainType();
          if (argType->isPlaceholder())
            return true;

          if (auto *typeVar = argType->getAs<TypeVariableType>())
            return hasFixFor(typeVar->getImpl().getLocator());

          return false;
        });

    // If this is an operator application and all of the arguments are holes,
    // let's disable all but one overload to make sure holes don't cause
    // performance problems because hole could be bound to any type.
    //
    // Non-operator calls are exempted because they have fewer overloads,
    // and it's possible to filter them based on labels.
    if (allHoles && isOperatorDisjunction(disjunction)) {
      auto choices = disjunction->getNestedConstraints();
      for (auto *choice : choices.slice(1))
        choice->setDisabled();
    }
  }

  /// The common result type amongst all function overloads.
  Type commonResultType;

  auto markFailure = [&] {
    commonResultType = ErrorType::get(getASTContext());
  };

  auto updateCommonResultType = [&](Type choiceResultType) {
    // For now, don't attempt to establish a common result type when there
    // are type parameters.
    if (choiceResultType->hasTypeParameter())
      return markFailure();

    // If we haven't seen a common result type yet, record what we found.
    if (!commonResultType) {
      commonResultType = choiceResultType;
      return;
    }

    // If we found something different, fail.
    if (!commonResultType->isEqual(choiceResultType))
      return markFailure();
  };

  auto *argList = getArgumentList(getConstraintLocator(locator));

  // If argument list has trailing closures and this is `init` call to
  // a callable type, let's not filter anything since there is a possibility
  // that it needs an implicit `.callAsFunction` to work.
  if (argList && argList->hasAnyTrailingClosures()) {
    if (disjunction->getLocator()
            ->isLastElement<LocatorPathElt::ConstructorMember>()) {
      auto choice = disjunction->getNestedConstraints()[0]->getOverloadChoice();
      if (auto *decl = choice.getDeclOrNull()) {
        auto *dc = decl->getDeclContext();
        if (auto *parent = dc->getSelfNominalTypeDecl()) {
          auto type = parent->getDeclaredInterfaceType();
          if (type->isCallAsFunctionType(DC))
            return false;
        }
      }
    }
  }

  // Consider each of the constraints in the disjunction.
retry_after_fail:
  bool hasUnhandledConstraints = false;
  bool labelMismatch = false;
  auto filterResult =
      filterDisjunction(disjunction, /*restoreOnFail=*/shouldAttemptFixes(),
                         [&](Constraint *constraint) {
        assert(constraint->getKind() == ConstraintKind::BindOverload);

        auto choice = constraint->getOverloadChoice();

        // Determine whether the argument labels we have conflict with those of
        // this overload choice.
        if (argList) {
          auto args = argFnType->getParams();

          SmallVector<FunctionType::Param, 8> argsWithLabels;
          argsWithLabels.append(args.begin(), args.end());
          FunctionType::relabelParams(argsWithLabels, argList);

          auto labelsMatch = [&](MatchCallArgumentListener &listener) {
            if (areConservativelyCompatibleArgumentLabels(
                    *this, choice, argsWithLabels, listener,
                    argList->getFirstTrailingClosureIndex()))
              return true;

            labelMismatch = true;
            return false;
          };

          AllowLabelMismatches listener;

          // This overload has more problems than just missing/invalid labels.
          if (!labelsMatch(listener))
            return false;

          // If overload did match, let's check if it needs to be disabled
          // in "performance" mode because it has missing labels.
          if (listener.hadLabelingIssues()) {
            // In performance mode, let's just disable the choice,
            // this decision could be rolled back for diagnostics.
            if (!shouldAttemptFixes())
              return false;

            // Match expected vs. actual to see whether the only kind
            // of problem here is missing label(s).
            auto onlyMissingLabels =
                [argList](ArrayRef<Identifier> expectedLabels) {
                  if (argList->size() != expectedLabels.size())
                    return false;

                  for (auto i : indices(*argList)) {
                    auto actual = argList->getLabel(i);
                    auto expected = expectedLabels[i];

                    if (actual.compare(expected) != 0 && !actual.empty())
                      return false;
                  }

                  return true;
                };

            auto replacementLabels = listener.getLabelReplacements();
            // Either it's just one argument or all issues are missing labels.
            if (!replacementLabels || onlyMissingLabels(*replacementLabels)) {
              constraint->setDisabled(/*enableForDiagnostics=*/true);
              // Don't include this overload in "common result" computation
              // because it has issues.
              return true;
            }
          }
        }

        // Determine the type that this choice will have.
        Type choiceType = getEffectiveOverloadType(
            constraint->getLocator(), choice,
            constraint->getDeclContext());
        if (!choiceType) {
          hasUnhandledConstraints = true;
          return true;
        }

        if (getASTContext().TypeCheckerOpts.SolverEnablePerformanceHacks) {
          // If types of arguments/parameters and result lined up exactly,
          // let's favor this overload choice.
          //
          // Note this check ignores `ExtInfo` on purpose and only compares
          // types, if there are overloads that differ only in effects then
          // all of them are going to be considered and filtered as part of
          // "favored" group after forming a valid partial solution.
          if (auto *choiceFnType = choiceType->getAs<FunctionType>()) {
            if (FunctionType::equalParams(argFnType->getParams(),
                                          choiceFnType->getParams()) &&
                argFnType->getResult()->isEqual(choiceFnType->getResult()))
              constraint->setFavored();
          }
        }

        // Account for any optional unwrapping/binding
        for (unsigned i : range(numOptionalUnwraps)) {
          (void)i;
          if (Type objectType = choiceType->getOptionalObjectType())
            choiceType = objectType;
        }

        // FIXME: The !getSelfProtocolDecl() check is load-bearing, because
        // this optimization interacts poorly with existential opening
        // somehow. It should all be removed.
        if (auto *choiceFnType = choiceType->getAs<FunctionType>()) {
          if (isa<ConstructorDecl>(choice.getDecl()) &&
              !choice.getDecl()->getDeclContext()->getSelfProtocolDecl()) {
            auto choiceResultType = choice.getBaseType()
                                        ->getRValueType()
                                        ->getMetatypeInstanceType();

            if (choiceResultType->getOptionalObjectType()) {
              hasUnhandledConstraints = true;
              return true;
            }

            if (choiceFnType->getResult()->getOptionalObjectType())
              choiceResultType = OptionalType::get(choiceResultType);

            updateCommonResultType(choiceResultType);
          } else {
            updateCommonResultType(choiceFnType->getResult());
          }
        } else {
          markFailure();
        }

        return true;
      });

  switch (filterResult) {
  case SolutionKind::Error:
    if (labelMismatch && shouldAttemptFixes()) {
      argList = nullptr;
      goto retry_after_fail;
    }
    return true;
  case SolutionKind::Solved:
  case SolutionKind::Unsolved:
    break;
  }

  // If there was a constraint that we couldn't reason about, don't use the
  // results of any common-type computations.
  if (hasUnhandledConstraints)
    return false;

  // If we have a common result type, bind the expected result type to it.
  if (commonResultType && !commonResultType->is<ErrorType>()) {
    if (isDebugMode()) {
      llvm::errs().indent(solverState ? solverState->getCurrentIndent() : 0)
          << "(common result type for $T" << fnTypeVar->getID() << " is "
          << commonResultType.getString(PrintOptions::forDebugging()) << ")\n";
    }

    // Introduction of a `Bind` constraint here could result in the disconnect
    // in the constraint system with unintended consequences because e.g.
    // in case of key path application it could disconnect one of the
    // components like subscript from the rest of the context.
    addConstraint(ConstraintKind::Equal, argFnType->getResult(),
                  commonResultType, locator);
  }
  return false;
}

bool ConstraintSystem::simplifyAppliedOverloads(
    Constraint *disjunction, ConstraintLocatorBuilder locator) {
  auto choices = disjunction->getNestedConstraints();
  assert(choices.size() >= 2);
  assert(choices.front()->getKind() == ConstraintKind::BindOverload);

  // If we've already bound the overload type var, bail.
  auto *typeVar = choices.front()->getFirstType()->getAs<TypeVariableType>();
  if (!typeVar || getFixedType(typeVar))
    return false;

  // Try to find an applicable fn constraint that applies the overload choice.
  auto result = findConstraintThroughOptionals(
      typeVar, OptionalWrappingDirection::Unwrap,
      [&](Constraint *match, TypeVariableType *currentRep) {
        // Check to see if we have an applicable fn with a type var RHS that
        // matches the disjunction.
        if (match->getKind() != ConstraintKind::ApplicableFunction)
          return false;

        auto *rhsTyVar = match->getSecondType()->getAs<TypeVariableType>();
        return rhsTyVar && currentRep == getRepresentative(rhsTyVar);
      });

  if (!result)
    return false;

  auto *applicableFn = result->first;
  auto *fnTypeVar = applicableFn->getSecondType()->castTo<TypeVariableType>();
  auto argFnType = applicableFn->getFirstType()->castTo<FunctionType>();
  recordAppliedDisjunction(disjunction->getLocator(), argFnType);
  return simplifyAppliedOverloadsImpl(disjunction, fnTypeVar, argFnType,
                                      /*numOptionalUnwraps*/ result->second,
                                      applicableFn->getLocator());
}

Constraint *ConstraintSystem::getUnboundBindOverloadDisjunction(
    TypeVariableType *tyvar, unsigned *numOptionalUnwraps) {
  assert(!getFixedType(tyvar));
  auto result = findConstraintThroughOptionals(
      tyvar, OptionalWrappingDirection::Promote,
      [&](Constraint *match, TypeVariableType *currentRep) {
        // Check to see if we have a bind overload disjunction that binds the
        // type var we need.
        if (match->getKind() != ConstraintKind::Disjunction ||
            match->getNestedConstraints().front()->getKind() !=
                ConstraintKind::BindOverload)
          return false;

        auto lhsTy = match->getNestedConstraints().front()->getFirstType();
        auto *lhsTyVar = lhsTy->getAs<TypeVariableType>();
        return lhsTyVar && currentRep == getRepresentative(lhsTyVar);
      });
  if (!result)
    return nullptr;

  if (numOptionalUnwraps)
    *numOptionalUnwraps = result->second;

  return result->first;
}

bool ConstraintSystem::simplifyAppliedOverloads(
    Type fnType, FunctionType *argFnType, ConstraintLocatorBuilder locator) {
  // If we've already bound the function type, bail.
  auto *fnTypeVar = fnType->getAs<TypeVariableType>();
  if (!fnTypeVar || getFixedType(fnTypeVar))
    return false;

  // Try to find a corresponding bind overload disjunction.
  unsigned numOptionalUnwraps = 0;
  auto *disjunction =
      getUnboundBindOverloadDisjunction(fnTypeVar, &numOptionalUnwraps);
  if (!disjunction)
    return false;

  recordAppliedDisjunction(disjunction->getLocator(), argFnType);
  return simplifyAppliedOverloadsImpl(disjunction, fnTypeVar, argFnType,
                                      numOptionalUnwraps, locator);
}

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
        constraint->getDeclContext());

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
