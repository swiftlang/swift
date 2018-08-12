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

std::pair<Expr *, bool> FailureDiagnostic::computeAnchor() const {
  auto &cs = getConstraintSystem();

  auto *locator = getLocator();
  // Resolve the locator to a specific expression.
  SourceRange range;
  bool isSubscriptMember =
      (!locator->getPath().empty() && locator->getPath().back().getKind() ==
                                          ConstraintLocator::SubscriptMember);

  ConstraintLocator *resolved = simplifyLocator(cs, locator, range);
  if (!resolved || !resolved->getAnchor())
    return {locator->getAnchor(), true};

  Expr *anchor = resolved->getAnchor();
  // FIXME: Work around an odd locator representation that doesn't separate the
  // base of a subscript member from the member access.
  if (isSubscriptMember) {
    if (auto subscript = dyn_cast<SubscriptExpr>(anchor))
      anchor = subscript->getBase();
  }

  return {anchor, !resolved->getPath().empty()};
}

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
    if (isa<PrefixUnaryExpr>(applyExpr) || isa<PostfixUnaryExpr>(applyExpr) ||
        isa<BinaryExpr>(applyExpr) || isa<TypeExpr>(anchor))
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
  if (hasComplexLocator())
    return false;

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

bool MissingAddressOfFailure::diagnose() {
  if (hasComplexLocator())
    return false;

  auto *anchor = getAnchor();
  auto type = getType(anchor)->getRValueType();
  emitDiagnostic(anchor->getLoc(), diag::missing_address_of, type)
      .fixItInsert(anchor->getStartLoc(), "&");
  return true;
}

bool MissingExplicitConversionFailure::diagnose() {
  if (hasComplexLocator())
    return false;

  auto *DC = getDC();
  auto &TC = getTypeChecker();

  auto *anchor = getAnchor();
  if (auto *paren = dyn_cast<ParenExpr>(anchor))
    anchor = paren->getSubExpr();

  auto fromType = getType(anchor)->getRValueType();
  Type toType = resolveType(ConvertingTo);
  bool useAs = TC.isExplicitlyConvertibleTo(fromType, toType, DC);
  bool useAsBang = !useAs && TC.checkedCastMaySucceed(fromType, toType, DC);
  if (!useAs && !useAsBang)
    return false;

  auto *expr = getParentExpr();
  // If we're performing pattern matching,
  // "as" means something completely different...
  if (auto binOpExpr = dyn_cast<BinaryExpr>(expr)) {
    auto overloadedFn = dyn_cast<OverloadedDeclRefExpr>(binOpExpr->getFn());
    if (overloadedFn && !overloadedFn->getDecls().empty()) {
      ValueDecl *decl0 = overloadedFn->getDecls()[0];
      if (decl0->getBaseName() == decl0->getASTContext().Id_MatchOperator)
        return false;
    }
  }

  bool needsParensInside = exprNeedsParensBeforeAddingAs(anchor);
  bool needsParensOutside = exprNeedsParensAfterAddingAs(anchor, expr);

  llvm::SmallString<2> insertBefore;
  llvm::SmallString<32> insertAfter;
  if (needsParensOutside) {
    insertBefore += "(";
  }
  if (needsParensInside) {
    insertBefore += "(";
    insertAfter += ")";
  }
  insertAfter += useAs ? " as " : " as! ";
  insertAfter += toType->getWithoutParens()->getString();
  if (needsParensOutside)
    insertAfter += ")";

  auto diagID =
      useAs ? diag::missing_explicit_conversion : diag::missing_forced_downcast;
  auto diag = emitDiagnostic(anchor->getLoc(), diagID, fromType, toType);
  if (!insertBefore.empty()) {
    diag.fixItInsert(anchor->getStartLoc(), insertBefore);
  }
  diag.fixItInsertAfter(anchor->getEndLoc(), insertAfter);
  return true;
}

bool MemberAccessOnOptionalBaseFailure::diagnose() {
  if (hasComplexLocator())
    return false;

  auto *anchor = getAnchor();
  auto type = getType(anchor)->getRValueType();
  bool resultIsOptional = ResultTypeIsOptional;

  // If we've resolved the member overload to one that returns an optional
  // type, then the result of the expression is optional (and we want to offer
  // only a '?' fixit) even though the constraint system didn't need to add any
  // additional optionality.
  auto overload = getResolvedOverload(getLocator());
  if (overload && overload->ImpliedType->getOptionalObjectType())
    resultIsOptional = true;

  return diagnoseBaseUnwrapForMemberAccess(anchor, type, Member,
                                           resultIsOptional, SourceRange());
}

bool MissingOptionalUnwrapFailure::diagnose() {
  if (hasComplexLocator())
    return false;

  auto *anchor = getAnchor();
  auto *unwrapped = anchor->getValueProvidingExpr();
  auto type = getType(anchor)->getRValueType();

  auto *tryExpr = dyn_cast<OptionalTryExpr>(unwrapped);
  if (!tryExpr)
    return diagnoseUnwrap(getTypeChecker(), getDC(), unwrapped, type);

  emitDiagnostic(tryExpr->getTryLoc(), diag::missing_unwrap_optional_try, type)
      .fixItReplace({tryExpr->getTryLoc(), tryExpr->getQuestionLoc()}, "try!");
  return true;
}
