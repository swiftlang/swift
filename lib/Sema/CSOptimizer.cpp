//===--- CSOptimizer.cpp - Constraint Optimizer ---------------------------===//
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
// This file implements disjunction and other constraint optimizations.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericSignature.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Sema/ConstraintGraph.h"
#include "swift/Sema/ConstraintSystem.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"
#include <cstddef>
#include <functional>

using namespace swift;
using namespace constraints;

namespace {

struct DisjunctionInfo {
  /// The score of the disjunction is the highest score from its choices.
  /// If the score is nullopt it means that the disjunction is not optimizable.
  std::optional<double> Score;
  /// The highest scoring choices that could be favored when disjunction
  /// is attempted.
  llvm::TinyPtrVector<Constraint *> FavoredChoices;

  DisjunctionInfo() = default;
  DisjunctionInfo(double score, ArrayRef<Constraint *> favoredChoices = {})
      : Score(score), FavoredChoices(favoredChoices) {}
};

// TODO: both `isIntegerType` and `isFloatType` should be available on Type
// as `isStdlib{Integer, Float}Type`.

static bool isIntegerType(Type type) {
  return type->isInt() || type->isInt8() || type->isInt16() ||
         type->isInt32() || type->isInt64() || type->isUInt() ||
         type->isUInt8() || type->isUInt16() || type->isUInt32() ||
         type->isUInt64();
}

static bool isFloatType(Type type) {
  return type->isFloat() || type->isDouble() || type->isFloat80();
}

static bool isUnboundArrayType(Type type) {
  if (auto *UGT = type->getAs<UnboundGenericType>())
    return UGT->getDecl() == type->getASTContext().getArrayDecl();
  return false;
}

static bool isSupportedOperator(Constraint *disjunction) {
  if (!isOperatorDisjunction(disjunction))
    return false;

  auto choices = disjunction->getNestedConstraints();
  auto *decl = getOverloadChoiceDecl(choices.front());

  auto name = decl->getBaseIdentifier();
  if (name.isArithmeticOperator() || name.isStandardComparisonOperator() ||
      name.isBitwiseOperator() || name.isNilCoalescingOperator()) {
    return true;
  }

  // Operators like &<<, &>>, &+, .== etc.
  if (llvm::any_of(choices, [](Constraint *choice) {
        return isSIMDOperator(getOverloadChoiceDecl(choice));
      })) {
    return true;
  }

  return false;
}

static bool isSupportedSpecialConstructor(ConstructorDecl *ctor) {
  if (auto *selfDecl = ctor->getImplicitSelfDecl()) {
    auto selfTy = selfDecl->getInterfaceType();
    /// Support `Int*`, `Float*` and `Double` initializers since their generic
    /// overloads are not too complicated.
    return selfTy && (isIntegerType(selfTy) || isFloatType(selfTy));
  }
  return false;
}

static bool isStandardComparisonOperator(ValueDecl *decl) {
  return decl->isOperator() &&
         decl->getBaseIdentifier().isStandardComparisonOperator();
}

static bool isArithmeticOperator(ValueDecl *decl) {
  return decl->isOperator() && decl->getBaseIdentifier().isArithmeticOperator();
}

static bool isSupportedDisjunction(Constraint *disjunction) {
  auto choices = disjunction->getNestedConstraints();

  if (isSupportedOperator(disjunction))
    return true;

  if (auto *ctor = dyn_cast_or_null<ConstructorDecl>(
          getOverloadChoiceDecl(choices.front()))) {
    if (isSupportedSpecialConstructor(ctor))
      return true;
  }

  // Non-operator disjunctions are supported only if they don't
  // have any generic choices.
  return llvm::all_of(choices, [&](Constraint *choice) {
    if (choice->isDisabled())
      return true;

    if (choice->getKind() != ConstraintKind::BindOverload)
      return false;

    if (auto *decl = getOverloadChoiceDecl(choice)) {
      // Cannot optimize declarations that return IUO because
      // they form a disjunction over a result type once attempted.
      if (decl->isImplicitlyUnwrappedOptional())
        return false;

      return decl->getInterfaceType()->is<FunctionType>();
    }

    return false;
  });
}

/// Given the type variable that represents a result type of a
/// function call, check whether that call is to an initializer
/// and based on that deduce possible type for the result.
///
/// @return A type and a flag that indicates whether there
/// are any viable failable overloads and empty pair if the
/// type variable isn't a result of an initializer call.
static llvm::PointerIntPair<Type, 1, bool>
inferTypeFromInitializerResultType(ConstraintSystem &cs,
                                   TypeVariableType *typeVar,
                                   ArrayRef<Constraint *> disjunctions) {
  assert(typeVar->getImpl().isFunctionResult());

  auto *resultLoc = typeVar->getImpl().getLocator();
  auto *call = getAsExpr<CallExpr>(resultLoc->getAnchor());
  if (!call)
    return {};

  auto *fn = call->getFn()->getSemanticsProvidingExpr();

  Type instanceTy;
  ConstraintLocator *ctorLocator = nullptr;
  if (auto *typeExpr = getAsExpr<TypeExpr>(fn)) {
    instanceTy = cs.getType(typeExpr)->getMetatypeInstanceType();
    ctorLocator =
        cs.getConstraintLocator(call, {LocatorPathElt::ApplyFunction(),
                                       LocatorPathElt::ConstructorMember()});
  } else if (auto *UDE = getAsExpr<UnresolvedDotExpr>(fn)) {
    if (!UDE->getName().getBaseName().isConstructor())
      return {};
    instanceTy = cs.getType(UDE->getBase())->getMetatypeInstanceType();
    ctorLocator = cs.getConstraintLocator(UDE, LocatorPathElt::Member());
  }

  if (!instanceTy || !ctorLocator)
    return {};

  auto initRef =
      llvm::find_if(disjunctions, [&ctorLocator](Constraint *disjunction) {
        return disjunction->getLocator() == ctorLocator;
      });

  if (initRef == disjunctions.end())
    return {};

  bool hasFailable =
      llvm::any_of((*initRef)->getNestedConstraints(), [](Constraint *choice) {
        if (choice->isDisabled())
          return false;
        auto *decl =
            dyn_cast_or_null<ConstructorDecl>(getOverloadChoiceDecl(choice));
        return decl && decl->isFailable();
      });

  return {instanceTy, hasFailable};
}

NullablePtr<Constraint> getApplicableFnConstraint(ConstraintGraph &CG,
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

void forEachDisjunctionChoice(
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

    // Ignore declarations that come from implicitly imported modules
    // when `MemberImportVisibility` feature is enabled otherwise
    // we might end up favoring an overload that would be diagnosed
    // as unavailable later.
    if (cs.getASTContext().LangOpts.hasFeature(
            Feature::MemberImportVisibility)) {
      if (auto *useDC = constraint->getDeclContext()) {
        if (!useDC->isDeclImported(decl))
          continue;
      }
    }

    // If disjunction choice is unavailable or disfavored we cannot
    // do anything with it.
    if (decl->getAttrs().hasAttribute<DisfavoredOverloadAttr>() ||
        cs.isDeclUnavailable(decl, disjunction->getLocator()))
      continue;

    Type overloadType =
        cs.getEffectiveOverloadType(disjunction->getLocator(), choice,
                                    /*allowMembers=*/true, cs.DC);

    if (!overloadType || !overloadType->is<FunctionType>())
      continue;

    callback(constraint, decl, overloadType->castTo<FunctionType>());
  }
}

static OverloadedDeclRefExpr *isOverloadedDeclRef(Constraint *disjunction) {
  assert(disjunction->getKind() == ConstraintKind::Disjunction);

  auto *locator = disjunction->getLocator();
  if (locator->getPath().empty())
    return getAsExpr<OverloadedDeclRefExpr>(locator->getAnchor());
  return nullptr;
}

static unsigned numOverloadChoicesMatchingOnArity(OverloadedDeclRefExpr *ODRE,
                                                  ArgumentList *arguments) {
  return llvm::count_if(ODRE->getDecls(), [&arguments](auto *choice) {
    if (auto *paramList = choice->getParameterList())
      return arguments->size() == paramList->size();
    return false;
  });
}

/// This maintains an "old hack" behavior where overloads  of some
/// `OverloadedDeclRef` calls were favored purely based on number of
/// argument and (non-defaulted) parameters matching.
static void findFavoredChoicesBasedOnArity(
    ConstraintSystem &cs, Constraint *disjunction, ArgumentList *argumentList,
    llvm::function_ref<void(Constraint *)> favoredChoice) {
  auto *ODRE = isOverloadedDeclRef(disjunction);
  if (!ODRE)
    return;

  if (numOverloadChoicesMatchingOnArity(ODRE, argumentList) > 1)
    return;

  auto isVariadicGenericOverload = [&](ValueDecl *choice) {
    auto genericContext = choice->getAsGenericContext();
    if (!genericContext)
      return false;

    auto *GPL = genericContext->getGenericParams();
    if (!GPL)
      return false;

    return llvm::any_of(GPL->getParams(), [&](const GenericTypeParamDecl *GP) {
      return GP->isParameterPack();
    });
  };

  bool hasVariadicGenerics = false;
  SmallVector<Constraint *> favored;

  forEachDisjunctionChoice(
      cs, disjunction,
      [&](Constraint *choice, ValueDecl *decl, FunctionType *overloadType) {
        if (isVariadicGenericOverload(decl))
          hasVariadicGenerics = true;

        if (overloadType->getNumParams() == argumentList->size() ||
            llvm::count_if(*decl->getParameterList(), [](auto *param) {
              return !param->isDefaultArgument();
            }) == argumentList->size())
          favored.push_back(choice);
      });

  if (hasVariadicGenerics)
    return;

  for (auto *choice : favored)
    favoredChoice(choice);
}

/// Preserves old behavior where, for unary calls, the solver would not previously
/// consider choices that didn't match on the number of parameters (regardless of
/// defaults and variadics) and only exact matches were favored.
static std::optional<DisjunctionInfo> preserveFavoringOfUnlabeledUnaryArgument(
    ConstraintSystem &cs, Constraint *disjunction, ArgumentList *argumentList) {
  if (!argumentList->isUnlabeledUnary())
    return std::nullopt;

  auto ODRE = isOverloadedDeclRef(disjunction);
  bool preserveFavoringOfUnlabeledUnaryArgument =
      !ODRE || numOverloadChoicesMatchingOnArity(ODRE, argumentList) < 2;

  if (!preserveFavoringOfUnlabeledUnaryArgument)
    return std::nullopt;

  auto *argument =
      argumentList->getUnlabeledUnaryExpr()->getSemanticsProvidingExpr();
  // The hack operated on "favored" types and only declaration references,
  // applications, and (dynamic) subscripts had them if they managed to
  // get an overload choice selected during constraint generation.
  if (!(isExpr<DeclRefExpr>(argument) || isExpr<ApplyExpr>(argument) ||
        isExpr<SubscriptExpr>(argument) ||
        isExpr<DynamicSubscriptExpr>(argument)))
    return {/*score=*/0};

  auto argumentType = cs.getType(argument);
  if (argumentType->hasTypeVariable() || argumentType->hasDependentMember())
    return {/*score=*/0};

  SmallVector<Constraint *, 2> favoredChoices;
  forEachDisjunctionChoice(
      cs, disjunction,
      [&argumentType, &favoredChoices](Constraint *choice, ValueDecl *decl,
                                       FunctionType *overloadType) {
        if (overloadType->getNumParams() != 1)
          return;

        auto paramType = overloadType->getParams()[0].getPlainType();
        if (paramType->isEqual(argumentType))
          favoredChoices.push_back(choice);
      });

  return DisjunctionInfo(/*score=*/favoredChoices.empty() ? 0 : 1,
                         favoredChoices);
}

} // end anonymous namespace

/// Given a set of disjunctions, attempt to determine
/// favored choices in the current context.
static void determineBestChoicesInContext(
    ConstraintSystem &cs, SmallVectorImpl<Constraint *> &disjunctions,
    llvm::DenseMap<Constraint *, DisjunctionInfo> &result) {
  double bestOverallScore = 0.0;

  auto recordResult = [&bestOverallScore, &result](Constraint *disjunction,
                                                   DisjunctionInfo &&info) {
    bestOverallScore = std::max(bestOverallScore, info.Score.value_or(0));
    result.try_emplace(disjunction, info);
  };

  for (auto *disjunction : disjunctions) {
    // If this is a compiler synthesized disjunction, mark it as supported
    // and record all of the previously favored choices. Such disjunctions
    // include - explicit coercions, IUO references,injected implicit
    // initializers for CGFloat<->Double conversions and restrictions with
    // multiple choices.
    if (disjunction->countFavoredNestedConstraints() > 0) {
      DisjunctionInfo info(/*score=*/2.0);
      llvm::copy_if(disjunction->getNestedConstraints(),
                    std::back_inserter(info.FavoredChoices),
                    [](Constraint *choice) { return choice->isFavored(); });
      recordResult(disjunction, std::move(info));
      continue;
    }

    auto applicableFn =
        getApplicableFnConstraint(cs.getConstraintGraph(), disjunction);

    if (applicableFn.isNull()) {
      auto *locator = disjunction->getLocator();
      if (auto expr = getAsExpr(locator->getAnchor())) {
        auto *parentExpr = cs.getParentExpr(expr);
        // Look through optional evaluation, so
        // we can cover expressions like `a?.b + 2`.
        if (isExpr<OptionalEvaluationExpr>(parentExpr))
          parentExpr = cs.getParentExpr(parentExpr);

        if (parentExpr) {
          // If this is a chained member reference or a direct operator
          // argument it could be prioritized since it helps to establish
          // context for other calls i.e. `(a.)b + 2` if `a` and/or `b`
          // are disjunctions they should be preferred over `+`.
          switch (parentExpr->getKind()) {
          case ExprKind::Binary:
          case ExprKind::PrefixUnary:
          case ExprKind::PostfixUnary:
          case ExprKind::UnresolvedDot: {
            llvm::SmallVector<Constraint *, 2> favoredChoices;
            // Favor choices that don't require application.
            llvm::copy_if(
                disjunction->getNestedConstraints(),
                std::back_inserter(favoredChoices), [](Constraint *choice) {
                  auto *decl = getOverloadChoiceDecl(choice);
                  return decl &&
                         !decl->getInterfaceType()->is<AnyFunctionType>();
                });
            recordResult(disjunction, {/*score=*/1.0, favoredChoices});
            continue;
          }

          default:
            break;
          }
        }
      }

      continue;
    }

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

    // This maintains an "old hack" behavior where overloads
    // of `OverloadedDeclRef` calls were favored purely
    // based on arity of arguments and parameters matching.
    {
      llvm::TinyPtrVector<Constraint *> favoredChoices;
      findFavoredChoicesBasedOnArity(cs, disjunction, argumentList,
                                     [&favoredChoices](Constraint *choice) {
                                       favoredChoices.push_back(choice);
                                     });

      if (!favoredChoices.empty()) {
        recordResult(disjunction, {/*score=*/0.01, favoredChoices});
        continue;
      }
    }

    // Preserves old behavior where, for unary calls, the solver
    // would not consider choices that didn't match on the number
    // of parameters (regardless of defaults) and only exact
    // matches were favored.
    if (auto info = preserveFavoringOfUnlabeledUnaryArgument(cs, disjunction,
                                                             argumentList)) {
      recordResult(disjunction, std::move(info.value()));
      continue;
    }

    if (!isSupportedDisjunction(disjunction))
      continue;

    SmallVector<FunctionType::Param, 8> argsWithLabels;
    {
      argsWithLabels.append(argFuncType->getParams().begin(),
                            argFuncType->getParams().end());
      FunctionType::relabelParams(argsWithLabels, argumentList);
    }

    struct ArgumentCandidate {
      Type type;
      // The candidate type is derived from a literal expression.
      bool fromLiteral : 1;
      // The candidate type is derived from a call to an
      // initializer i.e. `Double(...)`.
      bool fromInitializerCall : 1;

      ArgumentCandidate(Type type, bool fromLiteral = false,
                        bool fromInitializerCall = false)
          : type(type), fromLiteral(fromLiteral),
            fromInitializerCall(fromInitializerCall) {}
    };

    SmallVector<SmallVector<ArgumentCandidate, 2>, 2>
        argumentCandidates;
    argumentCandidates.resize(argFuncType->getNumParams());

    llvm::TinyPtrVector<Type> resultTypes;

    bool hasArgumentCandidates = false;
    for (unsigned i = 0, n = argFuncType->getNumParams(); i != n; ++i) {
      const auto &param = argFuncType->getParams()[i];
      auto argType = cs.simplifyType(param.getPlainType());

      SmallVector<Type, 2> optionals;
      // i.e. `??` operator could produce an optional type
      // so `test(<<something>> ?? 0) could result in an optional
      // argument that wraps a type variable. It should be possible
      // to infer bindings from underlying type variable and restore
      // optionality.
      if (argType->hasTypeVariable()) {
        if (auto *typeVar = argType->lookThroughAllOptionalTypes(optionals)
                                ->getAs<TypeVariableType>())
          argType = typeVar;
      }

      SmallVector<ArgumentCandidate, 2> types;
      if (auto *typeVar = argType->getAs<TypeVariableType>()) {
        auto bindingSet = cs.getBindingsFor(typeVar);

        auto restoreOptionality = [](Type type, unsigned numOptionals) {
          for (unsigned i = 0; i != numOptionals; ++i)
            type = type->wrapInOptionalType();
          return type;
        };

        for (const auto &binding : bindingSet.Bindings) {
          auto type = restoreOptionality(binding.BindingType, optionals.size());
          types.push_back({type});
        }

        for (const auto &literal : bindingSet.Literals) {
          if (literal.second.hasDefaultType()) {
            // Add primary default type
            auto type = restoreOptionality(literal.second.getDefaultType(),
                                           optionals.size());
            types.push_back({type,
                             /*fromLiteral=*/true});
          }
        }

        // Help situations like `1 + {Double, CGFloat}(...)` by inferring
        // a type for the second operand of `+` based on a type being
        // constructed.
        if (typeVar->getImpl().isFunctionResult()) {
          auto binding =
              inferTypeFromInitializerResultType(cs, typeVar, disjunctions);

          if (auto instanceTy = binding.getPointer()) {
            types.push_back({instanceTy,
                             /*fromLiteral=*/false,
                             /*fromInitializerCall=*/true});

            if (binding.getInt())
              types.push_back({instanceTy->wrapInOptionalType(),
                               /*fromLiteral=*/false,
                               /*fromInitializerCall=*/true});
          }
        }
      } else {
        types.push_back({argType, /*fromLiteral=*/false});
      }

      argumentCandidates[i].append(types);
      hasArgumentCandidates |= !types.empty();
    }

    auto resultType = cs.simplifyType(argFuncType->getResult());
    if (auto *typeVar = resultType->getAs<TypeVariableType>()) {
      auto bindingSet = cs.getBindingsFor(typeVar);

      for (const auto &binding : bindingSet.Bindings) {
        resultTypes.push_back(binding.BindingType);
      }

      // Infer bindings for each side of a ternary condition.
      bindingSet.forEachAdjacentVariable(
          [&cs, &resultTypes](TypeVariableType *adjacentVar) {
            auto *adjacentLoc = adjacentVar->getImpl().getLocator();
            // This is one of the sides of a ternary operator.
            if (adjacentLoc->directlyAt<TernaryExpr>()) {
              auto adjacentBindings = cs.getBindingsFor(adjacentVar);

              for (const auto &binding : adjacentBindings.Bindings)
                resultTypes.push_back(binding.BindingType);
            }
          });
    } else {
      resultTypes.push_back(resultType);
    }

    // Determine whether all of the argument candidates are inferred from literals.
    // This information is going to be used later on when we need to decide how to
    // score a matching choice.
    bool onlyLiteralCandidates =
        hasArgumentCandidates &&
        llvm::none_of(
            indices(argFuncType->getParams()), [&](const unsigned argIdx) {
              auto &candidates = argumentCandidates[argIdx];
              return llvm::any_of(candidates, [&](const auto &candidate) {
                return !candidate.fromLiteral;
              });
            });

    // Match arguments to the given overload choice.
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

    // Determine whether the candidate type is a subclass of the superclass
    // type.
    std::function<bool(Type, Type)> isSubclassOf = [&](Type candidateType,
                                                       Type superclassType) {
      // Conversion from a concrete type to its existential value.
      if (superclassType->isExistentialType() && !superclassType->isAny()) {
        auto layout = superclassType->getExistentialLayout();

        if (auto layoutConstraint = layout.getLayoutConstraint()) {
          if (layoutConstraint->isClass() &&
              !(candidateType->isClassExistentialType() ||
                candidateType->mayHaveSuperclass()))
            return false;
        }

        if (layout.explicitSuperclass &&
            !isSubclassOf(candidateType, layout.explicitSuperclass))
          return false;

        return llvm::all_of(layout.getProtocols(), [&](ProtocolDecl *P) {
          if (auto superclass = P->getSuperclassDecl()) {
            if (!isSubclassOf(candidateType,
                              superclass->getDeclaredInterfaceType()))
              return false;
          }

          auto result = TypeChecker::containsProtocol(candidateType, P,
                                                      /*allowMissing=*/false);
          return result.first || result.second;
        });
      }

      auto *subclassDecl = candidateType->getClassOrBoundGenericClass();
      auto *superclassDecl = superclassType->getClassOrBoundGenericClass();

      if (!(subclassDecl && superclassDecl))
        return false;

      return superclassDecl->isSuperclassOf(subclassDecl);
    };

    enum class MatchFlag {
      OnParam = 0x01,
      Literal = 0x02,
      ExactOnly = 0x04,
      DisableCGFloatDoubleConversion = 0x08,
    };

    using MatchOptions = OptionSet<MatchFlag>;

    // Perform a limited set of checks to determine whether the candidate
    // could possibly match the parameter type:
    //
    // - Equality
    // - Protocol conformance(s)
    // - Optional injection
    // - Superclass conversion
    // - Array-to-pointer conversion
    // - Value to existential conversion
    // - Exact match on top-level types
    //
    // In situations when it's not possible to determine whether a candidate
    // type matches a parameter type (i.e. when partially resolved generic
    // types are matched) this function is going to produce \c std::nullopt
    // instead of `0` that indicates "not a match".
    std::function<std::optional<double>(GenericSignature, ValueDecl *, Type,
                                        Type, MatchOptions)>
        scoreCandidateMatch =
            [&](GenericSignature genericSig, ValueDecl *choice,
                Type candidateType, Type paramType,
                MatchOptions options) -> std::optional<double> {
      auto areEqual = [&](Type a, Type b) {
        return a->getDesugaredType()->isEqual(b->getDesugaredType());
      };

      auto isCGFloatDoubleConversionSupported = [&options]() {
        // CGFloat <-> Double conversion is supposed only while
        // match argument candidates to parameters.
        return options.contains(MatchFlag::OnParam) &&
               !options.contains(MatchFlag::DisableCGFloatDoubleConversion);
      };

      // Allow CGFloat -> Double widening conversions between
      // candidate argument types and parameter types. This would
      // make sure that Double is always preferred over CGFloat
      // when using literals and ranking supported disjunction
      // choices. Narrowing conversion (Double -> CGFloat) should
      // be delayed as much as possible.
      if (isCGFloatDoubleConversionSupported()) {
        if (candidateType->isCGFloat() && paramType->isDouble()) {
          return options.contains(MatchFlag::Literal) ? 0.2 : 0.9;
        }
      }

      // Match `[...]` to Array<...> and/or `ExpressibleByArrayLiteral`
      // conforming types.
      if (options.contains(MatchFlag::OnParam) &&
          options.contains(MatchFlag::Literal) &&
          isUnboundArrayType(candidateType)) {
        // If an exact match is requested favor only `[...]` to `Array<...>`
        // since everything else is going to increase to score.
        if (options.contains(MatchFlag::ExactOnly))
          return paramType->isArray() ? 1 : 0;

        // Otherwise, check if the other side conforms to
        // `ExpressibleByArrayLiteral` protocol (in some way).
        // We want an overly optimistic result here to avoid
        // under-favoring.
        auto &ctx = cs.getASTContext();
        return checkConformanceWithoutContext(
                   paramType,
                   ctx.getProtocol(
                       KnownProtocolKind::ExpressibleByArrayLiteral),
                   /*allowMissing=*/true)
                   ? 0.3
                   : 0;
      }

      if (options.contains(MatchFlag::ExactOnly)) {
        if (!areEqual(candidateType, paramType))
          return 0;
        return options.contains(MatchFlag::Literal) ? 0.3 : 1;
      }

      // Exact match between candidate and parameter types.
      if (areEqual(candidateType, paramType)) {
        return options.contains(MatchFlag::Literal) ? 0.3 : 1;
      }

      if (options.contains(MatchFlag::Literal)) {
        if (candidateType->isInt() || candidateType->isDouble()) {
          if (paramType->hasTypeParameter() ||
              paramType->isAnyExistentialType()) {
            // Attempt to match literal default to generic parameter.
            // This helps to determine whether there are any generic
            // overloads that are a possible match.
            auto score =
                scoreCandidateMatch(genericSig, choice, candidateType,
                                    paramType, options - MatchFlag::Literal);
            if (score == 0)
              return 0;

            // Optional injection lowers the score for operators to match
            // pre-optimizer behavior.
            return choice->isOperator() && paramType->getOptionalObjectType()
                       ? 0.2
                       : 0.3;
          } else {
            // Integer and floating-point literals can match any parameter
            // type that conforms to `ExpressibleBy{Integer, Float}Literal`
            // protocol. Since this assessment is done in isolation we don't
            // lower the score even though this would be a non-default binding
            // for a literal.
            if (candidateType->isInt() &&
                TypeChecker::conformsToKnownProtocol(
                    paramType, KnownProtocolKind::ExpressibleByIntegerLiteral))
              return 0.3;

            if (candidateType->isDouble() &&
                TypeChecker::conformsToKnownProtocol(
                    paramType, KnownProtocolKind::ExpressibleByFloatLiteral))
              return 0.3;
          }
        }

        return 0;
      }

      // Check whether match would require optional injection.
      {
        SmallVector<Type, 2> candidateOptionals;
        SmallVector<Type, 2> paramOptionals;

        candidateType =
            candidateType->lookThroughAllOptionalTypes(candidateOptionals);
        paramType = paramType->lookThroughAllOptionalTypes(paramOptionals);

        if (!candidateOptionals.empty() || !paramOptionals.empty()) {
          if (paramOptionals.size() >= candidateOptionals.size()) {
            auto score = scoreCandidateMatch(genericSig, choice, candidateType,
                                             paramType, options);
            // Injection lowers the score slightly to comply with
            // old behavior where exact matches on operator parameter
            // types were always preferred.
            return score > 0 && choice->isOperator() ? score.value() - 0.1
                                                     : score;
          }

          // Optionality mismatch.
          return 0;
        }
      }

      // Candidate could be converted to a superclass.
      if (isSubclassOf(candidateType, paramType))
        return 1;

      // Possible Array<T> -> Unsafe*Pointer conversion.
      if (options.contains(MatchFlag::OnParam)) {
        if (candidateType->isArray() &&
            paramType->getAnyPointerElementType())
          return 1;
      }

      // If both argument and parameter are tuples of the same arity,
      // it's a match.
      {
        if (auto *candidateTuple = candidateType->getAs<TupleType>()) {
          auto *paramTuple = paramType->getAs<TupleType>();
          if (paramTuple &&
              candidateTuple->getNumElements() == paramTuple->getNumElements())
            return 1;
        }
      }

      // Check protocol requirement(s) if this parameter is a
      // generic parameter type.
      if (genericSig && paramType->isTypeParameter()) {
        // Light-weight check if cases where `checkRequirements` is not
        // applicable.
        auto checkProtocolRequirementsOnly = [&]() -> double {
          auto protocolRequirements =
              genericSig->getRequiredProtocols(paramType);
          if (llvm::all_of(protocolRequirements, [&](ProtocolDecl *protocol) {
                return bool(cs.lookupConformance(candidateType, protocol));
              })) {
            if (auto *GP = paramType->getAs<GenericTypeParamType>()) {
              auto *paramDecl = GP->getDecl();
              if (paramDecl && paramDecl->isOpaqueType())
                return 1.0;
            }
            return 0.7;
          }

          return 0;
        };

        // If candidate is not fully resolved or is matched against a
        // dependent member type (i.e. `Self.T`), let's check conformances
        // only and lower the score.
        if (candidateType->hasTypeVariable() ||
            paramType->is<DependentMemberType>()) {
          return checkProtocolRequirementsOnly();
        }

        // Cannot match anything but generic type parameters here.
        if (!paramType->is<GenericTypeParamType>())
          return std::nullopt;

        bool hasUnsatisfiableRequirements = false;
        SmallVector<Requirement, 4> requirements;

        for (const auto &requirement : genericSig.getRequirements()) {
          if (hasUnsatisfiableRequirements)
            break;

          llvm::SmallPtrSet<GenericTypeParamType *, 2> toExamine;

          auto recordReferencesGenericParams = [&toExamine](Type type) {
            type.visit([&toExamine](Type innerTy) {
              if (auto *GP = innerTy->getAs<GenericTypeParamType>())
                toExamine.insert(GP);
            });
          };

          recordReferencesGenericParams(requirement.getFirstType());

          if (requirement.getKind() != RequirementKind::Layout)
            recordReferencesGenericParams(requirement.getSecondType());

          if (llvm::any_of(toExamine, [&](GenericTypeParamType *GP) {
                return paramType->isEqual(GP);
              })) {
            requirements.push_back(requirement);
            // If requirement mentions other generic parameters
            // `checkRequirements` would because we don't have
            // candidate substitutions for anything but the current
            // parameter type.
            hasUnsatisfiableRequirements |= toExamine.size() > 1;
          }
        }

        // If some of the requirements cannot be satisfied, because
        // they reference other generic parameters, for example:
        // `<T, U, where T.Element == U.Element>`, let's perform a
        // light-weight check instead of skipping this overload choice.
        if (hasUnsatisfiableRequirements)
          return checkProtocolRequirementsOnly();

        // If the candidate type is fully resolved, let's check all of
        // the requirements that are associated with the corresponding
        // parameter, if all of them are satisfied this candidate is
        // an exact match.
        auto result = checkRequirements(
            requirements,
            [&paramType, &candidateType](SubstitutableType *type) -> Type {
              if (type->isEqual(paramType))
                return candidateType;
              return ErrorType::get(type);
            },
            SubstOptions(std::nullopt));

        // Concrete operator overloads are always more preferable to
        // generic ones if there are exact or subtype matches, for
        // everything else the solver should try both concrete and
        // generic and disambiguate during ranking.
        if (result == CheckRequirementsResult::Success)
          return choice->isOperator() ? 0.9 : 1.0;

        return 0;
      }

      // Parameter is generic, let's check whether top-level
      // types match i.e. Array<Element> as a parameter.
      //
      // This is slightly better than all of the conformances matching
      // because the parameter is concrete and could split the graph.      
      if (paramType->hasTypeParameter()) {
        auto *candidateDecl = candidateType->getAnyNominal();
        auto *paramDecl = paramType->getAnyNominal();

        if (candidateDecl && paramDecl && candidateDecl == paramDecl)
          return 0.8;
      }

      return 0;
    };

    // The choice with the best score.
    double bestScore = 0.0;
    SmallVector<std::pair<Constraint *, double>, 2> favoredChoices;

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
          if (!matchings)
            return;

          auto canUseContextualResultTypes = [&decl]() {
            return decl->isOperator() && !isStandardComparisonOperator(decl);
          };

          // Require exact matches only if all of the arguments
          // are literals and there are no usable contextual result
          // types that could help narrow favored choices.
          bool favorExactMatchesOnly =
              onlyLiteralCandidates &&
              (!canUseContextualResultTypes() || resultTypes.empty());

          // This is important for SIMD operators in particular because
          // a lot of their overloads have same-type requires to a concrete
          // type:  `<Scalar == (U)Int*>(_: SIMD*<Scalar>, ...) -> ...`.
          if (genericSig) {
            overloadType = overloadType->getReducedType(genericSig)
                               ->castTo<FunctionType>();
          }

          double score = 0.0;
          unsigned numDefaulted = 0;
          for (unsigned paramIdx = 0, n = overloadType->getNumParams();
               paramIdx != n; ++paramIdx) {
            const auto &param = overloadType->getParams()[paramIdx];

            auto argIndices = matchings->parameterBindings[paramIdx];
            switch (argIndices.size()) {
            case 0:
              // Current parameter is defaulted, mark and continue.
              ++numDefaulted;
              continue;

            case 1:
              // One-to-one match between argument and parameter.
              break;

            default:
              // Cannot deal with multiple possible matchings at the moment.
              return;
            }

            auto argIdx = argIndices.front();

            // Looks like there is nothing know about the argument.
            if (argumentCandidates[argIdx].empty())
              continue;

            const auto paramFlags = param.getParameterFlags();

            // If parameter is variadic we cannot compare because we don't know
            // real arity.
            if (paramFlags.isVariadic())
              continue;

            auto paramType = param.getPlainType();

            if (paramFlags.isAutoClosure())
              paramType = paramType->castTo<AnyFunctionType>()->getResult();

            // FIXME: Let's skip matching function types for now
            // because they have special rules for e.g. Concurrency
            // (around @Sendable) and @convention(c).
            if (paramType->is<FunctionType>())
              continue;

            // The idea here is to match the parameter type against
            // all of the argument candidate types and pick the best
            // match (i.e. exact equality one).
            //
            // If none of the candidates match exactly and they are
            // all bound concrete types, we consider this is mismatch
            // at this parameter position and remove the overload choice
            // from consideration.
            double bestCandidateScore = 0;
            llvm::BitVector mismatches(argumentCandidates[argIdx].size());

            for (unsigned candidateIdx :
                 indices(argumentCandidates[argIdx])) {
              // If one of the candidates matched exactly there is no reason
              // to continue checking.
              if (bestCandidateScore == 1)
                break;

              auto candidate = argumentCandidates[argIdx][candidateIdx];

              // `inout` parameter accepts only l-value argument.
              if (paramFlags.isInOut() && !candidate.type->is<LValueType>()) {
                mismatches.set(candidateIdx);
                continue;
              }

              MatchOptions options(MatchFlag::OnParam);
              if (candidate.fromLiteral)
                options |= MatchFlag::Literal;
              if (favorExactMatchesOnly)
                options |= MatchFlag::ExactOnly;

              // Disable CGFloat -> Double conversion for unary operators.
              //
              // Some of the unary operators, i.e. prefix `-`, don't have
              // CGFloat variants and expect generic `FloatingPoint` overload
              // to match CGFloat type. Let's not attempt `CGFloat` -> `Double`
              // conversion for unary operators because it always leads
              // to a worse solutions vs. generic overloads.
              if (n == 1 && decl->isOperator())
                options |= MatchFlag::DisableCGFloatDoubleConversion;

              // Disable implicit CGFloat -> Double widening conversion if
              // argument is an explicit call to `CGFloat` initializer.
              if (candidate.type->isCGFloat() &&
                  candidate.fromInitializerCall)
                options |= MatchFlag::DisableCGFloatDoubleConversion;

              // The specifier for a candidate only matters for `inout` check.
              auto candidateScore = scoreCandidateMatch(
                  genericSig, decl, candidate.type->getWithoutSpecifierType(),
                  paramType, options);

              if (!candidateScore)
                continue;

              if (candidateScore > 0) {
                bestCandidateScore =
                    std::max(bestCandidateScore, candidateScore.value());
                continue;
              }

              if (!candidate.type->hasTypeVariable())
                mismatches.set(candidateIdx);
            }

            // If none of the candidates for this parameter matched, let's
            // drop this overload from any further consideration.
            if (mismatches.all())
              return;

            score += bestCandidateScore;
          }

          // An overload whether all of the parameters are defaulted
          // that's called without arguments.
          if (numDefaulted == overloadType->getNumParams())
            return;

          // Average the score to avoid disfavoring disjunctions with fewer
          // parameters.
          score /= (overloadType->getNumParams() - numDefaulted);

          // If one of the result types matches exactly, that's a good
          // indication that overload choice should be favored.
          //
          // It's only safe to match result types of operators
          // because regular functions/methods/subscripts and
          // especially initializers could end up with a lot of
          // favored overloads because on the result type alone.
          if (canUseContextualResultTypes() &&
              (score > 0 || !hasArgumentCandidates)) {
            if (llvm::any_of(resultTypes, [&](const Type candidateResultTy) {
                  return scoreCandidateMatch(genericSig, decl,
                                             overloadType->getResult(),
                                             candidateResultTy,
                                             /*options=*/{}) > 0;
                })) {
              score += 1.0;
            }
          }

          if (score > 0) {
            // Nudge the score slightly to prefer concrete homogeneous
						// arithmetic operators.
            //
            // This is an opportunistic optimization based on the operator
            // use patterns where homogeneous operators are the most
            // heavily used ones.
            if (isArithmeticOperator(decl) &&
                overloadType->getNumParams() == 2) {
              auto resultTy = overloadType->getResult();
              if (!resultTy->hasTypeParameter() &&
                  llvm::all_of(overloadType->getParams(),
                               [&resultTy](const auto &param) {
                                 return param.getPlainType()->isEqual(resultTy);
                               }))
                score += 0.01;
            }

            favoredChoices.push_back({choice, score});
            bestScore = std::max(bestScore, score);
          }
        });

    if (cs.isDebugMode()) {
      PrintOptions PO;
      PO.PrintTypesForDebugging = true;

      llvm::errs().indent(cs.solverState->getCurrentIndent())
          << "<<< Disjunction "
          << disjunction->getNestedConstraints()[0]->getFirstType()->getString(
                 PO)
          << " with score " << bestScore << "\n";
    }

    bestOverallScore = std::max(bestOverallScore, bestScore);

    DisjunctionInfo info(/*score=*/bestScore);

    for (const auto &choice : favoredChoices) {
      if (choice.second == bestScore)
        info.FavoredChoices.push_back(choice.first);
    }

    recordResult(disjunction, std::move(info));
  }

  if (cs.isDebugMode() && bestOverallScore > 0) {
    PrintOptions PO;
    PO.PrintTypesForDebugging = true;

    auto getLogger = [&](unsigned extraIndent = 0) -> llvm::raw_ostream & {
      return llvm::errs().indent(cs.solverState->getCurrentIndent() +
                                 extraIndent);
    };

    {
      auto &log = getLogger();
      log << "(Optimizing disjunctions: [";

      interleave(
          disjunctions,
          [&](const auto *disjunction) {
            log << disjunction->getNestedConstraints()[0]
                       ->getFirstType()
                       ->getString(PO);
          },
          [&]() { log << ", "; });

      log << "]\n";
    }

    getLogger(/*extraIndent=*/4)
        << "Best overall score = " << bestOverallScore << '\n';

    for (auto *disjunction : disjunctions) {
      auto &entry = result[disjunction];
      getLogger(/*extraIndent=*/4)
          << "[Disjunction '"
          << disjunction->getNestedConstraints()[0]->getFirstType()->getString(
                 PO)
          << "' with score = " << entry.Score.value_or(0) << '\n';

      for (const auto *choice : entry.FavoredChoices) {
        auto &log = getLogger(/*extraIndent=*/6);

        log << "- ";
        choice->print(log, &cs.getASTContext().SourceMgr);
        log << '\n';
      }

      getLogger(/*extraIdent=*/4) << "]\n";
    }

    getLogger() << ")\n";
  }
}

// Attempt to find a disjunction of bind constraints where all options
// in the disjunction are binding the same type variable.
//
// Prefer disjunctions where the bound type variable is also the
// right-hand side of a conversion constraint, since having a concrete
// type that we're converting to can make it possible to split the
// constraint system into multiple ones.
static Constraint *
selectBestBindingDisjunction(ConstraintSystem &cs,
                             SmallVectorImpl<Constraint *> &disjunctions) {

  if (disjunctions.empty())
    return nullptr;

  auto getAsTypeVar = [&cs](Type type) {
    return cs.simplifyType(type)->getRValueType()->getAs<TypeVariableType>();
  };

  Constraint *firstBindDisjunction = nullptr;
  for (auto *disjunction : disjunctions) {
    auto choices = disjunction->getNestedConstraints();
    assert(!choices.empty());

    auto *choice = choices.front();
    if (choice->getKind() != ConstraintKind::Bind)
      continue;

    // We can judge disjunction based on the single choice
    // because all of choices (of bind overload set) should
    // have the same left-hand side.
    // Only do this for simple type variable bindings, not for
    // bindings like: ($T1) -> $T2 bind String -> Int
    auto *typeVar = getAsTypeVar(choice->getFirstType());
    if (!typeVar)
      continue;

    if (!firstBindDisjunction)
      firstBindDisjunction = disjunction;

    auto constraints = cs.getConstraintGraph().gatherNearbyConstraints(
        typeVar, [](Constraint *constraint) {
          return constraint->getKind() == ConstraintKind::Conversion;
        });

    for (auto *constraint : constraints) {
      if (typeVar == getAsTypeVar(constraint->getSecondType()))
        return disjunction;
    }
  }

  // If we had any binding disjunctions, return the first of
  // those. These ensure that we attempt to bind types earlier than
  // trying the elements of other disjunctions, which can often mean
  // we fail faster.
  return firstBindDisjunction;
}

/// Prioritize `build{Block, Expression, ...}` and any chained
/// members that are connected to individual builder elements
/// i.e. `ForEach(...) { ... }.padding(...)`, once `ForEach`
/// is resolved, `padding` should be prioritized because its
/// requirements can help prune the solution space before the
/// body is checked.
static Constraint *
selectDisjunctionInResultBuilderContext(ConstraintSystem &cs,
                                        ArrayRef<Constraint *> disjunctions) {
  auto context = AnyFunctionRef::fromDeclContext(cs.DC);
  if (!context)
    return nullptr;

  if (!cs.getAppliedResultBuilderTransform(context.value()))
    return nullptr;

  std::pair<Constraint *, unsigned> best{nullptr, 0};
  for (auto *disjunction : disjunctions) {
    auto *member =
        getAsExpr<UnresolvedDotExpr>(disjunction->getLocator()->getAnchor());
    if (!member)
      continue;

    // Attempt `build{Block, Expression, ...} first because they
    // provide contextual information for the inner calls.
    if (isResultBuilderMethodReference(cs.getASTContext(), member))
      return disjunction;

    Expr *curr = member;
    bool disqualified = false;
    // Walk up the parent expression chain and check whether this
    // disjunction represents one of the members in a chain that
    // leads up to `buildExpression` (if defined by the builder)
    // or to a pattern binding for `$__builderN` (the walk won't
    // find any argument position locations in that case).
    while (auto parent = cs.getParentExpr(curr)) {
      if (!(isExpr<CallExpr>(parent) || isExpr<UnresolvedDotExpr>(parent))) {
        disqualified = true;
        break;
      }

      if (auto *call = getAsExpr<CallExpr>(parent)) {
        // The current parent appears in an argument position.
        if (call->getFn() != curr) {
          // Allow expressions that appear in a argument position to
          // `build{Expression, Block, ...} methods.
          if (auto *UDE = getAsExpr<UnresolvedDotExpr>(call->getFn())) {
            disqualified =
                !isResultBuilderMethodReference(cs.getASTContext(), UDE);
          } else {
            disqualified = true;
          }
        }
      }

      if (disqualified)
        break;

      curr = parent;
    }

    if (disqualified)
      continue;

    if (auto depth = cs.getExprDepth(member)) {
      if (!best.first || best.second > depth)
        best = std::make_pair(disjunction, depth.value());
    }
  }

  return best.first;
}

std::optional<std::pair<Constraint *, llvm::TinyPtrVector<Constraint *>>>
ConstraintSystem::selectDisjunction() {
  if (performanceHacksEnabled()) {
    if (auto *disjunction = selectDisjunctionWithHacks())
      return std::make_pair(disjunction, llvm::TinyPtrVector<Constraint *>());
    return std::nullopt;
  }

  SmallVector<Constraint *, 4> disjunctions;

  collectDisjunctions(disjunctions);
  if (disjunctions.empty())
    return std::nullopt;

  if (auto *disjunction = selectBestBindingDisjunction(*this, disjunctions))
    return std::make_pair(disjunction, llvm::TinyPtrVector<Constraint *>());

  llvm::DenseMap<Constraint *, DisjunctionInfo> favorings;
  determineBestChoicesInContext(*this, disjunctions, favorings);

  if (auto *disjunction =
          selectDisjunctionInResultBuilderContext(*this, disjunctions)) {
    return std::make_pair(disjunction, favorings[disjunction].FavoredChoices);
  }

  // Pick the disjunction with the smallest number of favored, then active
  // choices.
  auto bestDisjunction = std::min_element(
      disjunctions.begin(), disjunctions.end(),
      [&](Constraint *first, Constraint *second) -> bool {
        unsigned firstActive = first->countActiveNestedConstraints();
        unsigned secondActive = second->countActiveNestedConstraints();

        auto &[firstScore, firstFavoredChoices] = favorings[first];
        auto &[secondScore, secondFavoredChoices] = favorings[second];

        // Rank based on scores only if both disjunctions are supported.
        if (firstScore && secondScore) {
          // If both disjunctions have the same score they should be ranked
          // based on number of favored/active choices.
          if (*firstScore != *secondScore)
            return *firstScore > *secondScore;
        }

        unsigned numFirstFavored = firstFavoredChoices.size();
        unsigned numSecondFavored = secondFavoredChoices.size();

        if (numFirstFavored == numSecondFavored) {
          if (firstActive != secondActive)
            return firstActive < secondActive;
        }

        numFirstFavored = numFirstFavored ? numFirstFavored : firstActive;
        numSecondFavored = numSecondFavored ? numSecondFavored : secondActive;

        return numFirstFavored < numSecondFavored;
      });

  if (bestDisjunction != disjunctions.end())
    return std::make_pair(*bestDisjunction,
                          favorings[*bestDisjunction].FavoredChoices);

  return std::nullopt;
}
