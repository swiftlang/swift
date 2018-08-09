//===--- CSDiagnostics.cpp - Constraint Diagnostics -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements diagnostics for constraint system.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "CSDiagnostics.h"
#include "MiscDiagnostics.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/ArrayRef.h"

using namespace swift;
using namespace constraints;

FailureDiagnostic::~FailureDiagnostic() {}

Type FailureDiagnostic::getType(Expr *expr) const {
  auto &cs = getConstraintSystem();
  return solution.simplifyType(cs.getType(expr));
}

template <typename... ArgTypes>
InFlightDiagnostic
FailureDiagnostic::emitDiagnostic(ArgTypes &&... Args) const {
  auto &cs = getConstraintSystem();
  return cs.TC.diagnose(std::forward<ArgTypes>(Args)...);
}

Type RequirementFailure::getOwnerType() const {
  return getType(getAnchor())->getRValueInstanceType();
}

const Requirement &RequirementFailure::getRequirement() {
  auto *genericCtx = AffectedDecl->getAsGenericContext();
  return genericCtx->getGenericRequirements()[getRequirementIndex()];
}

ValueDecl *RequirementFailure::getDeclRef() const {
  auto &cs = getConstraintSystem();

  auto *anchor = getAnchor();
  auto *locator = cs.getConstraintLocator(anchor);
  if (auto *AE = dyn_cast<CallExpr>(anchor)) {
    assert(isa<TypeExpr>(AE->getFn()));
    ConstraintLocatorBuilder ctor(locator);
    locator = cs.getConstraintLocator(
        ctor.withPathElement(PathEltKind::ApplyFunction)
            .withPathElement(PathEltKind::ConstructorMember));
  } else if (auto *UDE = dyn_cast<UnresolvedDotExpr>(anchor)) {
    ConstraintLocatorBuilder member(locator);
    locator =
        cs.getConstraintLocator(member.withPathElement(PathEltKind::Member));
  }

  auto overload = getOverloadChoiceIfAvailable(locator);
  if (overload)
    return overload->choice.getDecl();

  auto ownerType = getOwnerType();
  if (auto *NA = dyn_cast<NameAliasType>(ownerType.getPointer()))
    return NA->getDecl();

  return ownerType->getAnyGeneric();
}

bool MissingConformanceFailure::diagnose() {
  auto *anchor = getAnchor();
  auto ownerType = getOwnerType();
  auto type = getNonConformingType();
  auto protocolType = getProtocolType();

  //  Find `ApplyExpr` based on a function expression attached to it.
  auto findApplyExpr = [](Expr *parent, Expr *fnExpr) -> ApplyExpr * {
    ApplyExpr *applyExpr = nullptr;
    parent->forEachChildExpr([&applyExpr, &fnExpr](Expr *subExpr) -> Expr * {
      auto *AE = dyn_cast<ApplyExpr>(subExpr);
      if (!AE || AE->getFn() != fnExpr)
        return subExpr;

      applyExpr = AE;
      return nullptr;
    });
    return applyExpr;
  };

  auto getArgumentAt = [](ApplyExpr *AE, unsigned index) -> Expr * {
    assert(AE);

    auto *arg = AE->getArg();
    if (auto *TE = dyn_cast<TupleExpr>(arg))
      return TE->getElement(index);

    assert(index == 0);
    if (auto *PE = dyn_cast<ParenExpr>(arg))
      return PE->getSubExpr();

    return arg;
  };

  auto *applyExpr = findApplyExpr(getParentExpr(), anchor);

  Optional<unsigned> atParameterPos;
  // Sometimes fix is recorded by type-checking sub-expression
  // during normal diagnostics, in such case call expression
  // is unavailable.
  if (applyExpr) {
    // If this is a static, initializer or operator call,
    // let's not try to diagnose it here, but refer to expression
    // diagnostics.
    if (isa<BinaryExpr>(applyExpr) || isa<TypeExpr>(anchor))
      return false;

    if (auto *fnType = ownerType->getAs<AnyFunctionType>()) {
      auto parameters = fnType->getParams();
      for (auto index : indices(parameters)) {
        if (parameters[index].getType()->isEqual(type)) {
          atParameterPos = index;
          break;
        }
      }
    }
  }

  if (type->isExistentialType()) {
    auto diagnostic = diag::protocol_does_not_conform_objc;
    if (type->isObjCExistentialType())
      diagnostic = diag::protocol_does_not_conform_static;

    emitDiagnostic(anchor->getLoc(), diagnostic, type, protocolType);
  } else if (atParameterPos) {
    // Requirement comes from one of the parameter types,
    // let's try to point diagnostic to the argument expression.
    auto *argExpr = getArgumentAt(applyExpr, *atParameterPos);
    emitDiagnostic(argExpr->getLoc(),
                   diag::cannot_convert_argument_value_protocol, type,
                   protocolType);
  } else {
    const auto &req = getRequirement();
    auto *genericCtx = AffectedDecl->getAsGenericContext();

    std::function<const DeclContext *(Type)> getAffectedCtx =
        [&](Type type) -> const DeclContext * {
      if (auto *DMT = type->getAs<DependentMemberType>())
        return getAffectedCtx(DMT->getBase());

      if (auto *GPT = type->getAs<GenericTypeParamType>())
        return GPT->getDecl()->getDeclContext();

      return genericCtx;
    };

    const auto *affected = getAffectedCtx(req.getFirstType());
    if (affected != genericCtx) {
      auto *NTD = affected->getAsNominalTypeOrNominalTypeExtensionContext();
      emitDiagnostic(anchor->getLoc(), diag::type_does_not_conform_in_decl_ref,
                     AffectedDecl->getDescriptiveKind(),
                     AffectedDecl->getFullName(), NTD->getDeclaredType(), type,
                     protocolType);
    } else {
      emitDiagnostic(anchor->getLoc(), diag::type_does_not_conform_decl_owner,
                     AffectedDecl->getDescriptiveKind(),
                     AffectedDecl->getFullName(), type, protocolType);
    }

    emitDiagnostic(affected->getAsDeclOrDeclExtensionContext(),
                   diag::where_type_does_not_conform_type, req.getFirstType(),
                   type);
  }
  return true;
}

bool LabelingFailure::diagnose() {
  auto &cs = getConstraintSystem();
  auto *call = cast<CallExpr>(getAnchor());
  return diagnoseArgumentLabelError(cs.getASTContext(), call->getArg(),
                                    CorrectLabels,
                                    isa<SubscriptExpr>(call->getFn()));
}

bool NoEscapeFuncToTypeConversionFailure::diagnose() {
  auto *anchor = getAnchor();

  if (ConvertTo) {
    emitDiagnostic(anchor->getLoc(), diag::converting_noescape_to_type,
                   ConvertTo);
    return true;
  }

  auto path = getLocator()->getPath();
  if (path.empty())
    return false;

  auto &last = path.back();
  if (last.getKind() != ConstraintLocator::Archetype)
    return false;

  auto *archetype = last.getArchetype();
  emitDiagnostic(anchor->getLoc(), diag::converting_noescape_to_type,
                 archetype);
  return true;
}

bool MissingForcedDowncastFailure::diagnose() {
  auto &TC = getTypeChecker();

  auto *coerceExpr = dyn_cast<CoerceExpr>(getAnchor());
  if (!coerceExpr)
    return false;

  auto *subExpr = coerceExpr->getSubExpr();
  auto fromType = getType(subExpr)->getRValueType();
  auto toType = resolveType(coerceExpr->getCastTypeLoc().getType());

  auto castKind =
      TC.typeCheckCheckedCast(fromType, toType, CheckedCastContextKind::None,
                              getDC(), coerceExpr->getLoc(), subExpr,
                              coerceExpr->getCastTypeLoc().getSourceRange());

  switch (castKind) {
  // Invalid cast.
  case CheckedCastKind::Unresolved:
    // Fix didn't work, let diagnoseFailureForExpr handle this.
    return false;
  case CheckedCastKind::Coercion:
  case CheckedCastKind::BridgingCoercion:
    llvm_unreachable("Coercions handled in other disjunction branch");

  // Valid casts.
  case CheckedCastKind::ArrayDowncast:
  case CheckedCastKind::DictionaryDowncast:
  case CheckedCastKind::SetDowncast:
  case CheckedCastKind::ValueCast:
    emitDiagnostic(coerceExpr->getLoc(), diag::missing_forced_downcast,
                   fromType, toType)
        .highlight(coerceExpr->getSourceRange())
        .fixItReplace(coerceExpr->getLoc(), "as!");
    return true;
  }
}
