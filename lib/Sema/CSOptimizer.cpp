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
#include "swift/Basic/Defer.h"
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

  /// Whether the decisions were based on speculative information
  /// i.e. literal argument candidates or initializer type inference.
  bool IsSpeculative;

  DisjunctionInfo() = default;
  DisjunctionInfo(std::optional<double> score,
                  ArrayRef<Constraint *> favoredChoices, bool speculative)
      : Score(score), FavoredChoices(favoredChoices),
        IsSpeculative(speculative) {}

  static DisjunctionInfo none() { return {std::nullopt, {}, false}; }
};

class DisjunctionInfoBuilder {
  std::optional<double> Score;
  SmallVector<Constraint *, 2> FavoredChoices;
  bool IsSpeculative;

public:
  DisjunctionInfoBuilder(std::optional<double> score)
      : DisjunctionInfoBuilder(score, {}) {}

  DisjunctionInfoBuilder(std::optional<double> score,
                         ArrayRef<Constraint *> favoredChoices)
      : Score(score),
        FavoredChoices(favoredChoices.begin(), favoredChoices.end()),
        IsSpeculative(false) {}

  void setFavoredChoices(ArrayRef<Constraint *> choices) {
    FavoredChoices.clear();
    FavoredChoices.append(choices.begin(), choices.end());
  }

  void addFavoredChoice(Constraint *constraint) {
    FavoredChoices.push_back(constraint);
  }

  void setSpeculative(bool value = true) { IsSpeculative = value; }

  DisjunctionInfo build() { return {Score, FavoredChoices, IsSpeculative}; }
};

static DeclContext *getDisjunctionDC(Constraint *disjunction) {
  auto *choice = disjunction->getNestedConstraints()[0];
  switch (choice->getKind()) {
  case ConstraintKind::BindOverload:
  case ConstraintKind::ValueMember:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::ValueWitness:
    return choice->getDeclContext();
  default:
    return nullptr;
  }
}

/// Determine whether the given disjunction appears in a context
/// transformed by a result builder.
static bool isInResultBuilderContext(ConstraintSystem &cs,
                                     Constraint *disjunction) {
  auto *DC = getDisjunctionDC(disjunction);
  if (!DC)
    return false;

  do {
    auto fnContext = AnyFunctionRef::fromDeclContext(DC);
    if (!fnContext)
      return false;

    if (cs.getAppliedResultBuilderTransform(*fnContext))
      return true;

  } while ((DC = DC->getParent()));

  return false;
}

/// If the given operator disjunction appears in some position
// inside of a not yet resolved call i.e. `a.b(1 + c(4) - 1)`
// both `+` and `-` are "in" argument context of `b`.
static bool isOperatorPassedToUnresolvedCall(ConstraintSystem &cs,
                                             Constraint *disjunction) {
  ASSERT(isOperatorDisjunction(disjunction));

  auto *curr = castToExpr(disjunction->getLocator()->getAnchor());
  while (auto *parent = cs.getParentExpr(curr)) {
    SWIFT_DEFER { curr = parent; };

    switch (parent->getKind()) {
    case ExprKind::OptionalEvaluation:
    case ExprKind::Paren:
    case ExprKind::Binary:
    case ExprKind::PrefixUnary:
    case ExprKind::PostfixUnary:
      continue;

    // a.b(<<cond>> ? <<operator chain>> : <<...>>)
    case ExprKind::Ternary: {
      auto *T = cast<TernaryExpr>(parent);
      // If the operator is located in the condition it's
      // not tied to the context.
      if (T->getCondExpr() == curr)
        return false;

      // But the branches are connected to the context.
      continue;
    }

    // Handles `a(<<operator chain>>), `a[<<operator chain>>]`,
    // `.a(<<operator chain>>)` etc.
    case ExprKind::Call: {
      auto *call = cast<CallExpr>(parent);

      // Type(...)
      if (isa<TypeExpr>(call->getFn())) {
        auto *ctorLoc = cs.getConstraintLocator(
            call, {LocatorPathElt::ApplyFunction(),
                   LocatorPathElt::ConstructorMember()});
        return !cs.findSelectedOverloadFor(ctorLoc);
      }

      // Ignore injected result builder methods like `buildExpression`
      // and `buildBlock`.
      if (auto *UDE = dyn_cast<UnresolvedDotExpr>(call->getFn())) {
        if (isResultBuilderMethodReference(cs.getASTContext(), UDE))
          return false;
      }

      return !cs.findSelectedOverloadFor(call->getFn());
    }

    default:
      return false;
    }
  }

  return false;
}

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

static bool isUnboundDictionaryType(Type type) {
  if (auto *UGT = type->getAs<UnboundGenericType>())
    return UGT->getDecl() == type->getASTContext().getDictionaryDecl();
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

static bool isStandardComparisonOperator(Constraint *disjunction) {
  auto *choice = disjunction->getNestedConstraints()[0];
  if (auto *decl = getOverloadChoiceDecl(choice))
    return decl->isOperator() &&
           decl->getBaseIdentifier().isStandardComparisonOperator();
  return false;
}

static bool isStandardInfixLogicalOperator(Constraint *disjunction) {
  auto *choice = disjunction->getNestedConstraints()[0];
  if (auto *decl = getOverloadChoiceDecl(choice))
    return decl->isOperator() &&
           decl->getBaseIdentifier().isStandardInfixLogicalOperator();
  return false;
}

static bool isOperatorNamed(Constraint *disjunction, StringRef name) {
  auto *choice = disjunction->getNestedConstraints()[0];
  if (auto *decl = getOverloadChoiceDecl(choice))
    return decl->isOperator() && decl->getBaseIdentifier().is(name);
  return false;
}

static bool isArithmeticOperator(ValueDecl *decl) {
  return decl->isOperator() && decl->getBaseIdentifier().isArithmeticOperator();
}

/// Generic choices are supported only if they are not complex enough
/// that would they'd require solving to figure out whether they are a
/// potential match or not.
static bool isSupportedGenericOverloadChoice(ValueDecl *decl,
                                             GenericFunctionType *choiceType) {
  // Same type requirements cannot be handled because each
  // candidate-parameter pair is (currently) considered in isolation.
  if (llvm::any_of(choiceType->getRequirements(), [](const Requirement &req) {
        switch (req.getKind()) {
        case RequirementKind::SameType:
        case RequirementKind::SameShape:
          return true;

        case RequirementKind::Conformance:
        case RequirementKind::Superclass:
        case RequirementKind::Layout:
          return false;
        }
      }))
    return false;

  // If there are no same-type requirements, allow signatures
  // that use only concrete types or generic parameters directly
  // in their parameter positions i.e. `(T, Int)`.

  auto *paramList = decl->getParameterList();
  if (!paramList)
    return false;

  return llvm::all_of(paramList->getArray(), [](const ParamDecl *P) {
    auto paramType = P->getInterfaceType();
    return paramType->is<GenericTypeParamType>() ||
           !paramType->hasTypeParameter();
  });
}

static bool isSupportedDisjunction(Constraint *disjunction) {
  auto choices = disjunction->getNestedConstraints();

  if (isOperatorDisjunction(disjunction))
    return isSupportedOperator(disjunction);

  if (auto *ctor = dyn_cast_or_null<ConstructorDecl>(
          getOverloadChoiceDecl(choices.front()))) {
    if (isSupportedSpecialConstructor(ctor))
      return true;
  }

  // Non-operator disjunctions are supported only if they don't
  // have any complex generic choices.
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

      auto choiceType = decl->getInterfaceType()->getAs<AnyFunctionType>();
      if (!choiceType || choiceType->hasError())
        return false;

      // Non-generic choices are always supported.
      if (choiceType->is<FunctionType>())
        return true;

      if (auto *genericFn = choiceType->getAs<GenericFunctionType>())
        return isSupportedGenericOverloadChoice(decl, genericFn);

      return false;
    }

    return false;
  });
}

/// Determine whether the given overload choice constitutes a
/// valid choice that would be attempted during normal solving
/// without any score increases.
static ValueDecl *isViableOverloadChoice(ConstraintSystem &cs,
                                         Constraint *constraint,
                                         ConstraintLocator *locator) {
  if (constraint->isDisabled())
    return nullptr;

  if (constraint->getKind() != ConstraintKind::BindOverload)
    return nullptr;

  auto choice = constraint->getOverloadChoice();
  auto *decl = choice.getDeclOrNull();
  if (!decl)
    return nullptr;

  // Ignore declarations that come from implicitly imported modules
  // when `MemberImportVisibility` feature is enabled otherwise
  // we might end up favoring an overload that would be diagnosed
  // as unavailable later.
  if (cs.getASTContext().LangOpts.hasFeature(Feature::MemberImportVisibility)) {
    if (auto *useDC = constraint->getDeclContext()) {
      if (!useDC->isDeclImported(decl))
        return nullptr;
    }
  }

  // If disjunction choice is unavailable we cannot
  // do anything with it.
  if (cs.isDeclUnavailable(decl, locator))
    return nullptr;

  return decl;
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

  unsigned numFailable = 0;
  unsigned total = 0;
  for (auto *choice : (*initRef)->getNestedConstraints()) {
    auto *decl = isViableOverloadChoice(cs, choice, ctorLocator);
    if (!decl || !isa<ConstructorDecl>(decl))
      continue;

    auto *ctor = cast<ConstructorDecl>(decl);
    if (ctor->isFailable())
      ++numFailable;

    ++total;
  }

  if (numFailable > 0) {
    // If all of the active choices are failable, produce an optional
    // type only.
    if (numFailable == total)
      return {instanceTy->wrapInOptionalType(), /*hasFailable=*/false};
    // Otherwise there are two options.
    return {instanceTy, /*hasFailable*/ true};
  }

  return {instanceTy, /*hasFailable=*/false};
}

/// If the given expression represents a chain of operators that have
/// only declaration/member references and/or literals as arguments,
/// attempt to deduce a potential type of the chain. For example if
/// chain has only integral literals it's going to be `Int`, if there
/// are some floating-point literals mixed in - it's going to be `Double`.
static Type inferTypeOfArithmeticOperatorChain(ConstraintSystem &cs,
                                               ASTNode node) {
  class OperatorChainAnalyzer : public ASTWalker {
    ASTContext &C;
    DeclContext *DC;
    ConstraintSystem &CS;

    llvm::SmallPtrSet<llvm::PointerIntPair<Type, 1>, 2> candidates;

    bool unsupported = false;

    PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
      if (isa<BinaryExpr>(expr))
        return Action::Continue(expr);

      if (isa<PrefixUnaryExpr>(expr) || isa<PostfixUnaryExpr>(expr))
        return Action::Continue(expr);

      if (isa<ParenExpr>(expr))
        return Action::Continue(expr);

      // This inference works only with arithmetic operators
      // because we know the structure of their overloads.
      if (auto *ODRE = dyn_cast<OverloadedDeclRefExpr>(expr)) {
        if (auto *choice = ODRE->getDecls().front()) {
          if (choice->getBaseIdentifier().isArithmeticOperator())
            return Action::Continue(expr);
        }
      }

      if (auto *LE = dyn_cast<LiteralExpr>(expr)) {
        if (auto *P = TypeChecker::getLiteralProtocol(C, LE)) {
          if (auto defaultTy = TypeChecker::getDefaultType(P, DC)) {
            addCandidateType(defaultTy, /*literal=*/true);
            // String interpolation expressions have `TapExpr`
            // as their children, no reason to walk them.
            return Action::SkipChildren(expr);
          }
        }
      }

      if (auto *UDE = dyn_cast<UnresolvedDotExpr>(expr)) {
        auto memberTy = CS.getType(UDE);
        if (!memberTy->hasTypeVariable()) {
          addCandidateType(memberTy, /*literal=*/false);
          return Action::SkipChildren(expr);
        }
      }

      if (auto *DRE = dyn_cast<DeclRefExpr>(expr)) {
        auto declTy = CS.getType(DRE);
        if (!declTy->hasTypeVariable()) {
          addCandidateType(declTy, /*literal=*/false);
          return Action::SkipChildren(expr);
        }
      }

      unsupported = true;
      return Action::Stop();
    }

    void addCandidateType(Type type, bool literal) {
      if (literal) {
        if (type->isInt()) {
          // Floating-point types always subsume Int in operator chains.
          if (llvm::any_of(candidates, [](const auto &candidate) {
                auto ty = candidate.getPointer();
                return isFloatType(ty) || ty->isCGFloat();
              }))
            return;
        } else if (isFloatType(type) || type->isCGFloat()) {
          // A single use of a floating-point literal flips the
          // type of the entire chain to it.
          (void)candidates.erase({C.getIntType(), /*literal=*/true});
        }
      }

      candidates.insert({type, literal});
    }

  public:
    OperatorChainAnalyzer(ConstraintSystem &CS)
        : C(CS.getASTContext()), DC(CS.DC), CS(CS) {}

    Type chainType() const {
      if (unsupported)
        return Type();
      return candidates.size() != 1 ? Type()
                                    : (*candidates.begin()).getPointer();
    }
  };

  OperatorChainAnalyzer analyzer(cs);
  node.walk(analyzer);

  return analyzer.chainType();
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
    auto *decl =
        isViableOverloadChoice(cs, constraint, disjunction->getLocator());
    if (!decl)
      continue;

    Type overloadType = cs.getEffectiveOverloadType(
        disjunction->getLocator(), constraint->getOverloadChoice(),
        /*allowMembers=*/true, constraint->getDeclContext());

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

static bool isVariadicGenericOverload(ValueDecl *choice) {
  auto genericContext = choice->getAsGenericContext();
  if (!genericContext)
    return false;

  auto *GPL = genericContext->getGenericParams();
  if (!GPL)
    return false;

  return llvm::any_of(GPL->getParams(), [&](const GenericTypeParamDecl *GP) {
    return GP->isParameterPack();
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

  bool hasVariadicGenerics = false;
  SmallVector<Constraint *> favored;

  forEachDisjunctionChoice(
      cs, disjunction,
      [&](Constraint *choice, ValueDecl *decl, FunctionType *overloadType) {
        if (decl->getAttrs().hasAttribute<DisfavoredOverloadAttr>())
          return;

        if (isVariadicGenericOverload(decl)) {
          hasVariadicGenerics = true;
          return;
        }

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

  if (!isExpr<ApplyExpr>(
          cs.getParentExpr(argumentList->getUnlabeledUnaryExpr())))
    return std::nullopt;

  // The hack rolled back favoring choices if one of the overloads was a
  // protocol requirement or variadic generic.
  //
  // Note that it doesn't matter whether such overload choices are viable
  // or not, their presence disabled this "optimization".
  if (llvm::any_of(disjunction->getNestedConstraints(), [](Constraint *choice) {
        auto *decl = getOverloadChoiceDecl(choice);
        if (!decl)
          return false;

        return isa<ProtocolDecl>(decl->getDeclContext()) ||
               (!decl->getAttrs().hasAttribute<DisfavoredOverloadAttr>() &&
                isVariadicGenericOverload(decl));
      }))
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
  // It's sometimes possible to infer a type of a literal and an operator
  // chain, so it should be allowed as well.
  if (!(isExpr<DeclRefExpr>(argument) || isExpr<ApplyExpr>(argument) ||
        isExpr<SubscriptExpr>(argument) ||
        isExpr<DynamicSubscriptExpr>(argument) ||
        isExpr<LiteralExpr>(argument) || isExpr<BinaryExpr>(argument)))
    return DisjunctionInfo::none();

  auto argumentType = cs.getType(argument)->getRValueType();

  // For chains like `1 + 2 * 3` it's easy to deduce the type because
  // we know what literal types are preferred.
  if (isa<BinaryExpr>(argument)) {
    auto chainTy = inferTypeOfArithmeticOperatorChain(cs, argument);
    if (!chainTy)
      return DisjunctionInfo::none();

    argumentType = chainTy;
  }

  // Use default type of a literal (when available) to make a guess.
  // This is what old hack used to do as well.
  if (auto *LE = dyn_cast<LiteralExpr>(argument)) {
    auto *P = TypeChecker::getLiteralProtocol(cs.getASTContext(), LE);
    if (!P)
      return DisjunctionInfo::none();

    auto defaultTy = TypeChecker::getDefaultType(P, cs.DC);
    if (!defaultTy)
      return DisjunctionInfo::none();

    argumentType = defaultTy;
  }

  ASSERT(argumentType);

  if (argumentType->hasTypeVariable() || argumentType->hasDependentMember())
    return DisjunctionInfo::none();

  SmallVector<Constraint *, 2> favoredChoices;
  forEachDisjunctionChoice(
      cs, disjunction,
      [&argumentType, &favoredChoices, &argument](
          Constraint *choice, ValueDecl *decl, FunctionType *overloadType) {
        if (decl->getAttrs().hasAttribute<DisfavoredOverloadAttr>())
          return;

        if (overloadType->getNumParams() != 1)
          return;

        auto &param = overloadType->getParams()[0];

        // Literals are speculative, let's not attempt to apply them too
        // eagerly.
        if (!param.getParameterFlags().isNone() &&
            (isa<LiteralExpr>(argument) || isa<BinaryExpr>(argument)))
          return;

        if (argumentType->isEqual(param.getPlainType()))
          favoredChoices.push_back(choice);
      });

  return DisjunctionInfoBuilder(/*score=*/favoredChoices.empty() ? 0 : 1,
                                favoredChoices)
      .build();
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
      DisjunctionInfoBuilder info(/*score=*/2.0);
      for (auto *choice : disjunction->getNestedConstraints()) {
        if (choice->isFavored())
          info.addFavoredChoice(choice);
      }
      recordResult(disjunction, info.build());
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
            recordResult(
                disjunction,
                DisjunctionInfoBuilder(/*score=*/1.0, favoredChoices).build());
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
        recordResult(
            disjunction,
            DisjunctionInfoBuilder(/*score=*/0.01, favoredChoices).build());
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

    // Determine whether there are any non-speculative choices
    // in the given set of candidates. Speculative choices are
    // literals or types inferred from initializer calls.
    auto anyNonSpeculativeCandidates =
        [&](ArrayRef<ArgumentCandidate> candidates) {
          // If there is only one (non-CGFloat) candidate inferred from
          // an initializer call we don't consider this a speculation.
          //
          // CGFloat inference is always speculative because of the
          // implicit conversion between Double and CGFloat.
          if (llvm::count_if(candidates, [&](const auto &candidate) {
                return candidate.fromInitializerCall &&
                       !candidate.type->isCGFloat();
              }) == 1)
            return true;

          // If there are no non-literal and non-initializer-inferred types
          // in the list, consider this is a speculation.
          return llvm::any_of(candidates, [&](const auto &candidate) {
            return !candidate.fromLiteral && !candidate.fromInitializerCall;
          });
        };

    auto anyNonSpeculativeResultTypes = [](ArrayRef<Type> results) {
      return llvm::any_of(results, [](Type resultTy) {
        // Double and CGFloat are considered speculative because
        // there exists an implicit conversion between them and
        // preference is based on score impact in the overall solution.
        return !(resultTy->isDouble() || resultTy->isCGFloat());
      });
    };

    SmallVector<SmallVector<ArgumentCandidate, 2>, 2>
        argumentCandidates;
    argumentCandidates.resize(argFuncType->getNumParams());

    llvm::TinyPtrVector<Type> resultTypes;

    bool hasArgumentCandidates = false;
    bool isOperator = isOperatorDisjunction(disjunction);

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

        // We need to have a notion of "complete" binding set before
        // we can allow inference from generic parameters and ternary,
        // otherwise we'd make a favoring decision that might not be
        // correct i.e. `v ?? (<<cond>> ? nil : o)` where `o` is `Int`.
        // `getBindingsFor` doesn't currently infer transitive bindings
        // which means that for a ternary we'd only have a single
        // binding - `Int` which could lead to favoring overload of
        // `??` and has non-optional parameter on the right-hand side.
        if (typeVar->getImpl().getGenericParameter() ||
            typeVar->getImpl().isTernary())
          continue;

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
          } else if (literal.first ==
                         cs.getASTContext().getProtocol(
                             KnownProtocolKind::ExpressibleByNilLiteral) &&
                     literal.second.IsDirectRequirement) {
            // `==` and `!=` operators have special overloads that accept `nil`
            // as `_OptionalNilComparisonType` which is preferred over a
            // generic form `(T?, T?)`.
            if (isOperatorNamed(disjunction, "==") ||
                isOperatorNamed(disjunction, "!=")) {
              auto nilComparisonTy =
                  cs.getASTContext().get_OptionalNilComparisonTypeType();
              types.push_back({nilComparisonTy, /*fromLiteral=*/true});
            }
          }
        }

        // Help situations like `1 + {Double, CGFloat}(...)` by inferring
        // a type for the second operand of `+` based on a type being
        // constructed.
        if (typeVar->getImpl().isFunctionResult()) {
          auto *resultLoc = typeVar->getImpl().getLocator();

          if (auto type = inferTypeOfArithmeticOperatorChain(
                  cs, resultLoc->getAnchor())) {
            types.push_back({type, /*fromLiteral=*/true});
          }

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

    // Determine whether all of the argument candidates are speculative (i.e.
    // literals). This information is going to be used later on when we need to
    // decide how to score a matching choice.
    bool onlySpeculativeArgumentCandidates =
        hasArgumentCandidates &&
        llvm::none_of(
            indices(argFuncType->getParams()), [&](const unsigned argIdx) {
              return anyNonSpeculativeCandidates(argumentCandidates[argIdx]);
            });

    bool canUseContextualResultTypes =
        isOperator && !isStandardComparisonOperator(disjunction);

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

      if (auto *selfType = candidateType->getAs<DynamicSelfType>()) {
        candidateType = selfType->getSelfType();
      }

      if (auto *archetypeType = candidateType->getAs<ArchetypeType>()) {
        candidateType = archetypeType->getSuperclass();
        if (!candidateType)
          return false;
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
      StringInterpolation = 0x10,
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

      if (options.contains(MatchFlag::ExactOnly)) {
        // If an exact match is requested favor only `[...]` to `Array<...>`
        // since everything else is going to increase to score.
        if (options.contains(MatchFlag::Literal)) {
          if (isUnboundArrayType(candidateType))
            return paramType->isArray() ? 0.3 : 0;

          if (isUnboundDictionaryType(candidateType))
            return cs.isDictionaryType(paramType) ? 0.3 : 0;
        }

        if (!areEqual(candidateType, paramType))
          return 0;
        return options.contains(MatchFlag::Literal) ? 0.3 : 1;
      }

      // Exact match between candidate and parameter types.
      if (areEqual(candidateType, paramType)) {
        return options.contains(MatchFlag::Literal) ? 0.3 : 1;
      }

      if (options.contains(MatchFlag::Literal)) {
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

          if (candidateType->isBool() &&
              TypeChecker::conformsToKnownProtocol(
                  paramType, KnownProtocolKind::ExpressibleByBooleanLiteral))
            return 0.3;

          if (candidateType->isString()) {
            auto literalProtocol =
                options.contains(MatchFlag::StringInterpolation)
                    ? KnownProtocolKind::ExpressibleByStringInterpolation
                    : KnownProtocolKind::ExpressibleByStringLiteral;

            if (TypeChecker::conformsToKnownProtocol(paramType,
                                                     literalProtocol))
              return 0.3;
          }

          auto &ctx = cs.getASTContext();

          // Check if the other side conforms to `ExpressibleByArrayLiteral`
          // protocol (in some way). We want an overly optimistic result
          // here to avoid under-favoring.
          if (candidateType->isArray() &&
              checkConformanceWithoutContext(
                  paramType,
                  ctx.getProtocol(KnownProtocolKind::ExpressibleByArrayLiteral),
                  /*allowMissing=*/true))
            return 0.3;

          // Check if the other side conforms to
          // `ExpressibleByDictionaryLiteral` protocol (in some way).
          // We want an overly optimistic result here to avoid under-favoring.
          if (candidateType->isDictionary() &&
              checkConformanceWithoutContext(
                  paramType,
                  ctx.getProtocol(
                      KnownProtocolKind::ExpressibleByDictionaryLiteral),
                  /*allowMissing=*/true))
            return 0.3;
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
          // Can match i.e. Int? to Int or T to Int?
          if ((paramOptionals.empty() &&
               paramType->is<GenericTypeParamType>()) ||
              paramOptionals.size() >= candidateOptionals.size()) {
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
            candidateType->hasUnboundGenericType() ||
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

        // If there are no requirements associated with the generic
        // parameter or dependent member type it could match any type.
        if (requirements.empty())
          return 0.7;

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
      // because the parameter is concrete and could split the system.
      if (paramType->hasTypeParameter()) {
        auto *candidateDecl = candidateType->getAnyNominal();
        auto *paramDecl = paramType->getAnyNominal();

        // Conservatively we need to make sure that this is not worse
        // than matching against a generic parameter with or without
        // requirements.
        if (candidateDecl && paramDecl && candidateDecl == paramDecl) {
          // If the candidate it not yet fully resolved, let's lower the
          // score slightly to avoid over-favoring generic overload choices.
          if (candidateType->hasTypeVariable())
            return 0.8;

          // If the candidate is fully resolved we need to treat this
          // as we would generic parameter otherwise there is a risk
          // of skipping some of the valid choices.
          return choice->isOperator() ? 0.9 : 1.0;
        }
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

          // Require exact matches only if all of the arguments
          // are literals and there are no usable contextual result
          // types that could help narrow favored choices.
          bool favorExactMatchesOnly =
              onlySpeculativeArgumentCandidates &&
              (!canUseContextualResultTypes || resultTypes.empty());

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

              if (isExpr<InterpolatedStringLiteralExpr>(
                      argumentList->getExpr(argIdx)
                          ->getSemanticsProvidingExpr()))
                options |= MatchFlag::StringInterpolation;

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
          if (canUseContextualResultTypes &&
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

    // Determine if the score and favoring decisions here are
    // based only on "speculative" sources i.e. inference from
    // literals.
    //
    // This information is going to be used by the disjunction
    // selection algorithm to prevent over-eager selection of
    // the operators over unsupported non-operator declarations.
    bool isSpeculative = onlySpeculativeArgumentCandidates &&
                         (!canUseContextualResultTypes ||
                          !anyNonSpeculativeResultTypes(resultTypes));

    DisjunctionInfoBuilder info(/*score=*/bestScore);

    info.setSpeculative(isSpeculative);

    for (const auto &choice : favoredChoices) {
      if (choice.second == bestScore)
        info.addFavoredChoice(choice.first);
    }

    recordResult(disjunction, info.build());
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

static std::optional<bool> isPreferable(ConstraintSystem &cs,
                                        Constraint *disjunctionA,
                                        Constraint *disjunctionB) {
  // Consider only operator vs. non-operator situations.
  if (isOperatorDisjunction(disjunctionA) ==
      isOperatorDisjunction(disjunctionB))
    return std::nullopt;

  // Prevent operator selection if its passed as an argument
  // to not-yet resolved call. This helps to make sure that
  // in result builder context chained members and other
  // non-operator disjunctions are always selected first,
  // because they provide the context and help to prune the system.
  if (isInResultBuilderContext(cs, disjunctionA)) {
    if (isOperatorDisjunction(disjunctionA)) {
      if (isOperatorPassedToUnresolvedCall(cs, disjunctionA))
        return false;
    } else {
      if (isOperatorPassedToUnresolvedCall(cs, disjunctionB))
        return true;
    }
  }

  return std::nullopt;
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

  llvm::DenseMap<Constraint *, DisjunctionInfo> favorings;
  determineBestChoicesInContext(*this, disjunctions, favorings);

  // Pick the disjunction with the smallest number of favored, then active
  // choices.
  auto bestDisjunction = std::min_element(
      disjunctions.begin(), disjunctions.end(),
      [&](Constraint *first, Constraint *second) -> bool {
        unsigned firstActive = first->countActiveNestedConstraints();
        unsigned secondActive = second->countActiveNestedConstraints();

        if (firstActive == 1 || secondActive == 1)
          return secondActive != 1;

        if (auto preference = isPreferable(*this, first, second))
          return preference.value();

        auto &[firstScore, firstFavoredChoices, isFirstSpeculative] =
            favorings[first];
        auto &[secondScore, secondFavoredChoices, isSecondSpeculative] =
            favorings[second];

        bool isFirstOperator = isOperatorDisjunction(first);
        bool isSecondOperator = isOperatorDisjunction(second);

        // Infix logical operators are usually not overloaded and don't
        // form disjunctions, but when they do, let's prefer them over
        // other operators when they have fewer choices because it helps
        // to split operator chains.
        if (isFirstOperator && isSecondOperator) {
          if (isStandardInfixLogicalOperator(first) !=
              isStandardInfixLogicalOperator(second))
            return firstActive < secondActive;
        }

        // Not all of the non-operator disjunctions are supported by the
        // ranking algorithm, so to prevent eager selection of operators
        // when nothing concrete is known about them, let's reset the score
        // and compare purely based on number of choices.
        if (isFirstOperator != isSecondOperator) {
          if (isFirstOperator && isFirstSpeculative)
            firstScore = 0;

          if (isSecondOperator && isSecondSpeculative)
            secondScore = 0;
        }

        // Rank based on scores only if both disjunctions are supported.
        if (firstScore && secondScore) {
          // If both disjunctions have the same score they should be ranked
          // based on number of favored/active choices.
          if (*firstScore != *secondScore)
            return *firstScore > *secondScore;

          // If the scores are the same and both disjunctions are operators
          // they could be ranked purely based on whether the candidates
          // were speculative or not. The one with more context always wins.
          //
          // Consider the following situation:
          //
          // func test(_: Int) { ... }
          // func test(_: String) { ... }
          //
          // test("a" + "b" + "c")
          //
          // In this case we should always prefer ... + "c" over "a" + "b"
          // because it would fail and prune the other overload if parameter
          // type (aka contextual type) is `Int`.
          if (isFirstOperator && isSecondOperator &&
              isFirstSpeculative != isSecondSpeculative)
            return isSecondSpeculative;
        }

        // Use favored choices only if disjunction score is higher
        // than zero. This means that we can maintain favoring
        // choices without impacting selection decisions.
        unsigned numFirstFavored =
            firstScore.value_or(0) ? firstFavoredChoices.size() : 0;
        unsigned numSecondFavored =
            secondScore.value_or(0) ? secondFavoredChoices.size() : 0;

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
