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
  Category = 1 << 0,
  Exact = 1 << 1,
  Nominal = 1 << 2,
  Class = 1 << 3,
  Structural = 1 << 4,
  Array = 1 << 5,
  Dictionary = 1 << 6,
  Set = 1 << 7,
  Optional = 1 << 8,
  Double = 1 << 9,
  Conformance = 1 << 10,
  Mutability = 1 << 11
};
using ConflictReason = OptionSet<ConflictFlag>;

static bool shouldBeConservativeWithProto(ProtocolDecl *proto) {
  if (proto->isMarkerProtocol())
    return true;

  if (proto->isObjC())
    return true;

  return false;
}

static ClassDecl *getBridgedObjCClass(ClassDecl *classDecl) {
  return classDecl->getAttrs().getAttribute<ObjCBridgedAttr>()->getObjCClass();
}

static ConflictReason canPossiblyConvertTo(
    ConstraintSystem &cs,
    Type lhs, Type rhs,
    GenericSignature sig) {
  auto lhsKind = inference::getConversionBehavior(lhs);
  auto rhsKind = inference::getConversionBehavior(rhs);

  // Conversion between two types with the same conversion behavior.
  if (lhsKind == rhsKind) {
    switch (lhsKind) {
    case inference::ConversionBehavior::None:
      if (!lhs->hasTypeVariable() && !lhs->hasTypeParameter() &&
          !rhs->hasTypeVariable() && !rhs->hasTypeParameter()) {
        if (!lhs->isEqual(rhs))
          return ConflictFlag::Exact;
      }

      if (lhs->getAnyNominal() != rhs->getAnyNominal())
        return ConflictFlag::Nominal;
      break;

    case inference::ConversionBehavior::Class: {
      auto *lhsDecl = lhs->getClassOrBoundGenericClass();
      auto *rhsDecl = rhs->getClassOrBoundGenericClass();

      // Toll-free bridging CF -> ObjC.
      if (lhsDecl->getForeignClassKind() == ClassDecl::ForeignKind::CFType &&
          rhsDecl->getForeignClassKind() != ClassDecl::ForeignKind::CFType) {
        return canPossiblyConvertTo(getBridgedObjCClass(lhsDecl), rhsDecl);

      // Toll-free bridging ObjC -> CF.
      } else if (lhsDecl->getForeignClassKind() != ClassDecl::ForeignKind::CFType &&
                 rhsDecl->getForeignClassKind() == ClassDecl::ForeignKind::CFType) {
        return canPossiblyConvertTo(lhsDecl, getBridgedObjCClass(rhsDecl));

      // Subclassing relationships.
      } else if (!rhsDecl->isSuperclassOf(lhsDecl))
        return ConflictFlag::Class;

      break;
    }

    case inference::ConversionBehavior::Double:
      // There are only two types with this behavior, and they convert
      // to each other.
      break;

    case inference::ConversionBehavior::AnyHashable:
      // AnyHashable converts to AnyHashable.
      break;

    case inference::ConversionBehavior::Pointer:
      // FIXME: Implement
      break;

    case inference::ConversionBehavior::Array: {
      auto subResult = canPossiblyConvertTo(
          cs,
          lhs->getArrayElementType(),
          rhs->getArrayElementType(), sig);
      if (subResult)
        return subResult | ConflictFlag::Array;

      break;
    }
    case inference::ConversionBehavior::Dictionary: {
      // FIXME: Implement
      break;
    }
    case inference::ConversionBehavior::Set: {
      // FIXME: Implement
      break;
    }
    case inference::ConversionBehavior::Optional: {
      // Optional-to-optional conversion.
      auto argObjectType = lhs->getOptionalObjectType();
      auto objectType = rhs->getOptionalObjectType();
      auto optionalToOptional = canPossiblyConvertTo(
          cs, argObjectType, objectType, sig);

      if (optionalToOptional)
        return optionalToOptional | ConflictFlag::Optional;
      break;
    }
    case inference::ConversionBehavior::Structural:
      if (lhs->getCanonicalType()->getKind()
          != rhs->getCanonicalType()->getKind())
        return ConflictFlag::Structural;
      break;
    case inference::ConversionBehavior::Unknown:
      break;
    }

  // Handle case where the kinds don't match, and we're not converting
  // from an unknown type.
  } else if (lhsKind != inference::ConversionBehavior::Unknown) {
    switch (rhsKind) {
    case inference::ConversionBehavior::Class: {
      // Archetypes can convert to classes.
      if (lhs->is<ArchetypeType>()) {
        auto superclassType = lhs->getSuperclass();
        if (!superclassType)
          return ConflictFlag::Class;

        return canPossiblyConvertTo(cs, superclassType, rhs, sig);
      }

      // Protocol metatypes can convert to instances of the Protocol class
      // on Objective-C interop platforms.
      if (lhs->is<MetatypeType>())
        break;

      // Nothing else converts to a class except for existentials
      // (which are 'ConversionBehavior::Unknown').
      return ConflictFlag::Category;
    }

    case inference::ConversionBehavior::AnyHashable:
      // FIXME: Check if lhs definitely not Hashable
      break;

    case inference::ConversionBehavior::Pointer:
      // FIXME: Array, String, InOutType convert to pointers
      break;

    case inference::ConversionBehavior::Optional: {
      // We have a non-optional on the left. Try value-to-optional.
      auto objectType = rhs->getOptionalObjectType();
      auto valueToOptional = canPossiblyConvertTo(
          cs, lhs, objectType, sig);
      if (valueToOptional)
        return valueToOptional | ConflictFlag::Optional;

      break;
    }

    case inference::ConversionBehavior::None:
    case inference::ConversionBehavior::Double:
    case inference::ConversionBehavior::Array:
    case inference::ConversionBehavior::Dictionary:
    case inference::ConversionBehavior::Set:
    case inference::ConversionBehavior::Structural:
      return ConflictFlag::Category;

    case inference::ConversionBehavior::Unknown:
      break;
    }
  }

  if (sig) {
    // If '$LHS conv $RHS' and '$LHS conforms P', does it follow
    // that '$RHS conforms P'?
    auto isConformanceTransitiveOnLHS = [lhsKind, lhs]() -> bool {
      // FIXME: String converts to UnsafePointer<UInt8> which
      // can satisfy a conformance to P that String does not
      // satisfy. Encode this more thoroughly.
      if (lhsKind == inference::ConversionBehavior::None)
        return !lhs->isString();

      return false;
    };

    if (rhs->isTypeParameter() &&
        isConformanceTransitiveOnLHS()) {
      bool failed = llvm::any_of(
          sig->getRequiredProtocols(rhs),
          [&](ProtocolDecl *proto) {
            if (shouldBeConservativeWithProto(proto))
              return false;
            return !lookupConformance(lhs, proto);
          });
      if (failed)
        return ConflictFlag::Conformance;
    }

    // If '$LHS conv $RHS' and '$RHS conforms P', does it follow
    // that '$LHS conforms P'?
    auto isConformanceTransitiveOnRHS = [rhsKind]() -> bool {
      if (rhsKind == inference::ConversionBehavior::None ||
          rhsKind == inference::ConversionBehavior::Array ||
          rhsKind == inference::ConversionBehavior::Dictionary ||
          rhsKind == inference::ConversionBehavior::Set)
        return true;

      return false;
    };

    if (lhs->isTypeParameter() &&
        isConformanceTransitiveOnRHS()) {
      bool failed = llvm::any_of(
          sig->getRequiredProtocols(lhs),
          [&](ProtocolDecl *proto) {
            if (shouldBeConservativeWithProto(proto))
              return false;
            return !lookupConformance(rhs, proto);
          });
      if (failed)
        return ConflictFlag::Conformance;
    }
  }

  /*llvm::errs() << "unknown conversion:\n";
  lhs->dump(llvm::errs());
  rhs->dump(llvm::errs());*/
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
    auto argType = cs.simplifyType(param.getPlainType());
    // FIXME: This is gross, I know. The purpose is just to wrap the type
    // with something that will give it ConversionBehavior::Unknown.
    if (param.isInOut())
      argType = InOutType::get(argType);
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
          if (reason.contains(ConflictFlag::Nominal))
            llvm::errs() << " nominal";
          if (reason.contains(ConflictFlag::Class))
            llvm::errs() << " class";
          if (reason.contains(ConflictFlag::Structural))
            llvm::errs() << " structural";
          if (reason.contains(ConflictFlag::Array))
            llvm::errs() << " array";
          if (reason.contains(ConflictFlag::Dictionary))
            llvm::errs() << " dictionary";
          if (reason.contains(ConflictFlag::Set))
            llvm::errs() << " set";
          if (reason.contains(ConflictFlag::Optional))
            llvm::errs() << " optional";
          if (reason.contains(ConflictFlag::Structural))
            llvm::errs() << " structural";
          if (reason.contains(ConflictFlag::Conformance))
            llvm::errs() << " conformance";
          if (reason.contains(ConflictFlag::Mutability))
            llvm::errs() << " mutability";
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