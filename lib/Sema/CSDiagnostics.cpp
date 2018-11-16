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

#include "CSDiagnostics.h"
#include "ConstraintSystem.h"
#include "MiscDiagnostics.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallString.h"

using namespace swift;
using namespace constraints;

FailureDiagnostic::~FailureDiagnostic() {}

bool FailureDiagnostic::diagnose(bool asNote) {
  return asNote ? diagnoseAsNote() : diagnoseAsError();
}

bool FailureDiagnostic::diagnoseAsNote() {
  return false;
}

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
  return resolveType(CS.getType(expr));
}

template <typename... ArgTypes>
InFlightDiagnostic
FailureDiagnostic::emitDiagnostic(ArgTypes &&... Args) const {
  auto &cs = getConstraintSystem();
  return cs.TC.diagnose(std::forward<ArgTypes>(Args)...);
}

Type RequirementFailure::getOwnerType() const {
  return getType(getAnchor())->getInOutObjectType()->getMetatypeInstanceType();
}

const GenericContext *RequirementFailure::getGenericContext() const {
  if (auto *genericCtx = AffectedDecl->getAsGenericContext())
    return genericCtx;
  return AffectedDecl->getDeclContext()->getAsDecl()->getAsGenericContext();
}

const Requirement &RequirementFailure::getRequirement() const {
  return getGenericContext()->getGenericRequirements()[getRequirementIndex()];
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

    if (UDE->getName().isSimpleName(DeclBaseName::createConstructor())) {
      member = member.withPathElement(PathEltKind::ConstructorMember);
    } else {
      member = member.withPathElement(PathEltKind::Member);
    }

    locator = cs.getConstraintLocator(member);
  } else if (auto *UME = dyn_cast<UnresolvedMemberExpr>(anchor)) {
    locator = cs.getConstraintLocator(locator, PathEltKind::UnresolvedMember);
  } else if (isa<SubscriptExpr>(anchor)) {
    ConstraintLocatorBuilder subscript(locator);
    locator = cs.getConstraintLocator(
        subscript.withPathElement(PathEltKind::SubscriptMember));
  }

  auto overload = getOverloadChoiceIfAvailable(locator);
  if (overload)
    return overload->choice.getDecl();

  auto ownerType = getOwnerType();
  if (auto *NA = dyn_cast<NameAliasType>(ownerType.getPointer()))
    return NA->getDecl();

  return ownerType->getAnyGeneric();
}

const DeclContext *RequirementFailure::getRequirementDC() const {
  const auto &req = getRequirement();
  auto *DC = AffectedDecl->getDeclContext();

  do {
    if (auto *sig = DC->getGenericSignatureOfContext()) {
      if (sig->isRequirementSatisfied(req))
        return DC;
    }
  } while ((DC = DC->getParent()));

  return AffectedDecl->getAsGenericContext();
}

bool RequirementFailure::diagnoseAsError() {
  if (!canDiagnoseFailure())
    return false;

  auto *anchor = getAnchor();
  const auto *reqDC = getRequirementDC();
  auto *genericCtx = getGenericContext();

  if (reqDC != genericCtx) {
    auto *NTD = reqDC->getSelfNominalTypeDecl();
    emitDiagnostic(anchor->getLoc(), getDiagnosticInRereference(),
                   AffectedDecl->getDescriptiveKind(),
                   AffectedDecl->getFullName(), NTD->getDeclaredType(),
                   getLHS(), getRHS());
  } else {
    emitDiagnostic(anchor->getLoc(), getDiagnosticOnDecl(),
                   AffectedDecl->getDescriptiveKind(),
                   AffectedDecl->getFullName(), getLHS(), getRHS());
  }

  emitRequirementNote(reqDC->getAsDecl());
  return true;
}

bool RequirementFailure::diagnoseAsNote() {
  const auto &req = getRequirement();
  const auto *reqDC = getRequirementDC();

  emitDiagnostic(reqDC->getAsDecl(), getDiagnosticAsNote(), getLHS(), getRHS(),
                 req.getFirstType(), req.getSecondType(), "");
  return true;
}

void RequirementFailure::emitRequirementNote(const Decl *anchor) const {
  auto &req = getRequirement();

  if (getRHS()->isEqual(req.getSecondType())) {
    emitDiagnostic(anchor, diag::where_requirement_failure_one_subst,
                   req.getFirstType(), getLHS());
    return;
  }

  if (getLHS()->isEqual(req.getFirstType())) {
    emitDiagnostic(anchor, diag::where_requirement_failure_one_subst,
                   req.getSecondType(), getRHS());
    return;
  }

  emitDiagnostic(anchor, diag::where_requirement_failure_both_subst,
                 req.getFirstType(), getLHS(), req.getSecondType(), getRHS());
}

bool MissingConformanceFailure::diagnoseAsError() {
  if (!canDiagnoseFailure())
    return false;

  auto *anchor = getAnchor();
  auto ownerType = getOwnerType();
  auto nonConformingType = getLHS();
  auto protocolType = getRHS();

  auto getArgumentAt = [](const ApplyExpr *AE, unsigned index) -> Expr * {
    assert(AE);

    auto *arg = AE->getArg();
    if (auto *TE = dyn_cast<TupleExpr>(arg))
      return TE->getElement(index);

    assert(index == 0);
    if (auto *PE = dyn_cast<ParenExpr>(arg))
      return PE->getSubExpr();

    return arg;
  };

  Optional<unsigned> atParameterPos;
  // Sometimes fix is recorded by type-checking sub-expression
  // during normal diagnostics, in such case call expression
  // is unavailable.
  if (Apply) {
    if (auto *fnType = ownerType->getAs<AnyFunctionType>()) {
      auto parameters = fnType->getParams();
      for (auto index : indices(parameters)) {
        if (parameters[index].getOldType()->isEqual(nonConformingType)) {
          atParameterPos = index;
          break;
        }
      }
    }
  }

  if (nonConformingType->isExistentialType()) {
    auto diagnostic = diag::protocol_does_not_conform_objc;
    if (nonConformingType->isObjCExistentialType())
      diagnostic = diag::protocol_does_not_conform_static;

    emitDiagnostic(anchor->getLoc(), diagnostic, nonConformingType,
                   protocolType);
    return true;
  }

  if (atParameterPos) {
    // Requirement comes from one of the parameter types,
    // let's try to point diagnostic to the argument expression.
    auto *argExpr = getArgumentAt(Apply, *atParameterPos);
    emitDiagnostic(argExpr->getLoc(),
                   diag::cannot_convert_argument_value_protocol,
                   nonConformingType, protocolType);
    return true;
  }

  // If none of the special cases could be diagnosed,
  // let's fallback to the most general diagnostic.
  return RequirementFailure::diagnoseAsError();
}

bool LabelingFailure::diagnoseAsError() {
  auto &cs = getConstraintSystem();
  auto *call = cast<CallExpr>(getAnchor());
  return diagnoseArgumentLabelError(cs.getASTContext(), call->getArg(),
                                    CorrectLabels,
                                    isa<SubscriptExpr>(call->getFn()));
}

bool NoEscapeFuncToTypeConversionFailure::diagnoseAsError() {
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
  if (last.getKind() != ConstraintLocator::GenericParameter)
    return false;

  auto *paramTy = last.getGenericParameter();
  emitDiagnostic(anchor->getLoc(), diag::converting_noescape_to_type,
                 paramTy);
  return true;
}

bool MissingForcedDowncastFailure::diagnoseAsError() {
  if (hasComplexLocator())
    return false;

  auto &TC = getTypeChecker();

  auto *expr = getAnchor();
  if (auto *assignExpr = dyn_cast<AssignExpr>(expr))
    expr = assignExpr->getSrc();
  auto *coerceExpr = dyn_cast<CoerceExpr>(expr);
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
  llvm_unreachable("unhandled cast kind");
}

bool MissingAddressOfFailure::diagnoseAsError() {
  if (hasComplexLocator())
    return false;

  auto *anchor = getAnchor();
  auto type = getType(anchor)->getRValueType();
  emitDiagnostic(anchor->getLoc(), diag::missing_address_of, type)
      .fixItInsert(anchor->getStartLoc(), "&");
  return true;
}

bool MissingExplicitConversionFailure::diagnoseAsError() {
  if (hasComplexLocator())
    return false;

  auto *DC = getDC();
  auto &TC = getTypeChecker();

  auto *anchor = getAnchor();
  if (auto *assign = dyn_cast<AssignExpr>(anchor))
    anchor = assign->getSrc();
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

bool MemberAccessOnOptionalBaseFailure::diagnoseAsError() {
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

// Suggest a default value via ?? <default value>
static void offerDefaultValueUnwrapFixit(TypeChecker &TC, DeclContext *DC, Expr *expr) {
  auto diag =
  TC.diagnose(expr->getLoc(), diag::unwrap_with_default_value);

  // Figure out what we need to parenthesize.
  bool needsParensInside =
  exprNeedsParensBeforeAddingNilCoalescing(TC, DC, expr);
  bool needsParensOutside =
  exprNeedsParensAfterAddingNilCoalescing(TC, DC, expr, expr);

  llvm::SmallString<2> insertBefore;
  llvm::SmallString<32> insertAfter;
  if (needsParensOutside) {
    insertBefore += "(";
  }
  if (needsParensInside) {
    insertBefore += "(";
    insertAfter += ")";
  }
  insertAfter += " ?? <" "#default value#" ">";
  if (needsParensOutside)
    insertAfter += ")";

  if (!insertBefore.empty()) {
    diag.fixItInsert(expr->getStartLoc(), insertBefore);
  }
  diag.fixItInsertAfter(expr->getEndLoc(), insertAfter);
}

// Suggest a force-unwrap.
static void offerForceUnwrapFixit(ConstraintSystem &CS, Expr *expr) {
  auto diag = CS.TC.diagnose(expr->getLoc(), diag::unwrap_with_force_value);

  // If expr is optional as the result of an optional chain and this last
  // dot isn't a member returning optional, then offer to force the last
  // link in the chain, rather than an ugly parenthesized postfix force.
  if (auto optionalChain = dyn_cast<OptionalEvaluationExpr>(expr)) {
    if (auto dotExpr =
        dyn_cast<UnresolvedDotExpr>(optionalChain->getSubExpr())) {
      auto bind = dyn_cast<BindOptionalExpr>(dotExpr->getBase());
      if (bind && !CS.getType(dotExpr)->getOptionalObjectType()) {
        diag.fixItReplace(SourceRange(bind->getLoc()), "!");
        return;
      }
    }
  }

  if (expr->canAppendPostfixExpression(true)) {
    diag.fixItInsertAfter(expr->getEndLoc(), "!");
  } else {
    diag.fixItInsert(expr->getStartLoc(), "(")
    .fixItInsertAfter(expr->getEndLoc(), ")!");
  }
}

class VarDeclMultipleReferencesChecker : public ASTWalker {
  VarDecl *varDecl;
  int count;

  std::pair<bool, Expr *> walkToExprPre(Expr *E) {
    if (auto *DRE = dyn_cast<DeclRefExpr>(E)) {
      if (DRE->getDecl() == varDecl)
        count++;
    }
    return { true, E };
  }

public:
  VarDeclMultipleReferencesChecker(VarDecl *varDecl) : varDecl(varDecl),count(0) {}
  int referencesCount() { return count; }
};

static bool diagnoseUnwrap(ConstraintSystem &CS, Expr *expr, Type type) {
  Type unwrappedType = type->getOptionalObjectType();
  if (!unwrappedType)
    return false;

  CS.TC.diagnose(expr->getLoc(), diag::optional_not_unwrapped, type,
                 unwrappedType);

  // If the expression we're unwrapping is the only reference to a
  // local variable whose type isn't explicit in the source, then
  // offer unwrapping fixits on the initializer as well.
  if (auto declRef = dyn_cast<DeclRefExpr>(expr)) {
    if (auto varDecl = dyn_cast<VarDecl>(declRef->getDecl())) {

      bool singleUse = false;
      AbstractFunctionDecl *AFD = nullptr;
      if (auto contextDecl = varDecl->getDeclContext()->getAsDecl()) {
        if ((AFD = dyn_cast<AbstractFunctionDecl>(contextDecl))) {
          auto checker = VarDeclMultipleReferencesChecker(varDecl);
          AFD->getBody()->walk(checker);
          singleUse = checker.referencesCount() == 1;
        }
      }

      PatternBindingDecl *binding = varDecl->getParentPatternBinding();
      if (singleUse && binding && binding->getNumPatternEntries() == 1 &&
          varDecl->getTypeSourceRangeForDiagnostics().isInvalid()) {

        Expr *initializer = varDecl->getParentInitializer();
        if (auto declRefExpr = dyn_cast<DeclRefExpr>(initializer)) {
          if (declRefExpr->getDecl()->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>()) {
            CS.TC.diagnose(declRefExpr->getLoc(), diag::unwrap_iuo_initializer, type);
          }
        }

        auto fnTy = AFD->getInterfaceType()->castTo<AnyFunctionType>();
        bool voidReturn = fnTy->getResult()->isEqual(TupleType::getEmpty(CS.DC->getASTContext()));

        auto diag = CS.TC.diagnose(varDecl->getLoc(), diag::unwrap_with_guard);
        diag.fixItInsert(binding->getStartLoc(), "guard ");
        if (voidReturn) {
          diag.fixItInsertAfter(binding->getEndLoc(), " else { return }");
        } else {
          diag.fixItInsertAfter(binding->getEndLoc(), " else { return <"
                                "#default value#" "> }");
        }
        diag.flush();

        offerDefaultValueUnwrapFixit(CS.TC, varDecl->getDeclContext(),
                                     initializer);
        offerForceUnwrapFixit(CS, initializer);
      }
    }
  }

  offerDefaultValueUnwrapFixit(CS.TC, CS.DC, expr);
  offerForceUnwrapFixit(CS, expr);
  return true;
}

bool MissingOptionalUnwrapFailure::diagnoseAsError() {
  if (hasComplexLocator())
    return false;

  auto *anchor = getAnchor();

  if (auto assignExpr = dyn_cast<AssignExpr>(anchor))
    anchor = assignExpr->getSrc();
  
  auto *unwrapped = anchor->getValueProvidingExpr();
  auto type = getType(anchor)->getRValueType();

  auto *tryExpr = dyn_cast<OptionalTryExpr>(unwrapped);
  if (!tryExpr)
    return diagnoseUnwrap(getConstraintSystem(), unwrapped, type);

  emitDiagnostic(tryExpr->getTryLoc(), diag::missing_unwrap_optional_try, type)
      .fixItReplace({tryExpr->getTryLoc(), tryExpr->getQuestionLoc()}, "try!");
  return true;
}

bool RValueTreatedAsLValueFailure::diagnoseAsError() {
  Diag<StringRef> subElementDiagID;
  Diag<Type> rvalueDiagID = diag::assignment_lhs_not_lvalue;
  Expr *diagExpr = getLocator()->getAnchor();
  SourceLoc loc = diagExpr->getLoc();

  if (auto assignExpr = dyn_cast<AssignExpr>(diagExpr)) {
    diagExpr = assignExpr->getDest();
  }

  if (auto callExpr = dyn_cast<ApplyExpr>(diagExpr)) {
    Expr *argExpr = callExpr->getArg();
    loc = callExpr->getFn()->getLoc();

    if (isa<PrefixUnaryExpr>(callExpr) || isa<PostfixUnaryExpr>(callExpr)) {
      subElementDiagID = diag::cannot_apply_lvalue_unop_to_subelement;
      rvalueDiagID = diag::cannot_apply_lvalue_unop_to_rvalue;
      diagExpr = argExpr;
    } else if (isa<BinaryExpr>(callExpr)) {
      subElementDiagID = diag::cannot_apply_lvalue_binop_to_subelement;
      rvalueDiagID = diag::cannot_apply_lvalue_binop_to_rvalue;
      auto argTuple = dyn_cast<TupleExpr>(argExpr);
      diagExpr = argTuple->getElement(0);
    } else if (getLocator()->getPath().size() > 0) {
      auto lastPathElement = getLocator()->getPath().back();
      assert(lastPathElement.getKind() ==
             ConstraintLocator::PathElementKind::ApplyArgToParam);

      subElementDiagID = diag::cannot_pass_rvalue_inout_subelement;
      rvalueDiagID = diag::cannot_pass_rvalue_inout;
      if (auto argTuple = dyn_cast<TupleExpr>(argExpr))
        diagExpr = argTuple->getElement(lastPathElement.getValue());
      else if (auto parens = dyn_cast<ParenExpr>(argExpr))
        diagExpr = parens->getSubExpr();
    } else {
      subElementDiagID = diag::assignment_lhs_is_apply_expression;
    }
  } else if (auto inoutExpr = dyn_cast<InOutExpr>(diagExpr)) {
    if (auto restriction = getRestrictionForType(getType(inoutExpr))) {
      PointerTypeKind pointerKind;
      if (restriction->second == ConversionRestrictionKind::ArrayToPointer &&
          restriction->first->getAnyPointerElementType(pointerKind) &&
          (pointerKind == PTK_UnsafePointer ||
           pointerKind == PTK_UnsafeRawPointer)) {
        // If we're converting to an UnsafePointer, then the programmer
        // specified an & unnecessarily. Produce a fixit hint to remove it.
        emitDiagnostic(inoutExpr->getLoc(),
                       diag::extra_address_of_unsafepointer, restriction->first)
            .highlight(inoutExpr->getSourceRange())
            .fixItRemove(inoutExpr->getStartLoc());
        return true;
      }
    }

    subElementDiagID = diag::cannot_pass_rvalue_inout_subelement;
    rvalueDiagID = diag::cannot_pass_rvalue_inout;
    diagExpr = inoutExpr->getSubExpr();
  } else if (isa<DeclRefExpr>(diagExpr)) {
    subElementDiagID = diag::assignment_lhs_is_immutable_variable;
  } else if (isa<ForceValueExpr>(diagExpr)) {
    subElementDiagID = diag::assignment_bang_has_immutable_subcomponent;
  } else if (isa<MemberRefExpr>(diagExpr)) {
    subElementDiagID = diag::assignment_lhs_is_immutable_property;
  } else if (auto member = dyn_cast<UnresolvedDotExpr>(diagExpr)) {
    subElementDiagID = diag::assignment_lhs_is_immutable_property;

    if (auto *ctor = dyn_cast<ConstructorDecl>(getDC())) {
      if (auto *baseRef = dyn_cast<DeclRefExpr>(member->getBase())) {
        if (baseRef->getDecl() == ctor->getImplicitSelfDecl() &&
            ctor->getDelegatingOrChainedInitKind(nullptr) ==
            ConstructorDecl::BodyInitKind::Delegating) {
          emitDiagnostic(loc, diag::assignment_let_property_delegating_init,
                      member->getName());
          if (auto *ref = getResolvedMemberRef(member)) {
            emitDiagnostic(ref, diag::decl_declared_here, member->getName());
          }
          return true;
        }
      }
    }

    if (auto resolvedOverload = getResolvedOverload(getLocator()))
      if (resolvedOverload->Choice.getKind() ==
          OverloadChoiceKind::DynamicMemberLookup)
        subElementDiagID = diag::assignment_dynamic_property_has_immutable_base;
  } else if (auto sub = dyn_cast<SubscriptExpr>(diagExpr)) {
      subElementDiagID = diag::assignment_subscript_has_immutable_base;
  } else {
    subElementDiagID = diag::assignment_lhs_is_immutable_variable;
  }

  AssignmentFailure failure(diagExpr, getConstraintSystem(), loc,
                            subElementDiagID, rvalueDiagID);
  return failure.diagnose();
}

bool TrailingClosureAmbiguityFailure::diagnoseAsNote() {
  const auto *expr = getParentExpr();
  auto *callExpr = dyn_cast<CallExpr>(expr);
  if (!callExpr)
    return false;
  if (!callExpr->hasTrailingClosure())
    return false;
  if (callExpr->getFn() != getAnchor())
    return false;

  llvm::SmallMapVector<Identifier, const ValueDecl *, 8> choicesByLabel;
  for (const auto &choice : Choices) {
    auto *callee = dyn_cast<AbstractFunctionDecl>(choice.getDecl());
    if (!callee)
      return false;

    const ParameterList *paramList = callee->getParameters();
    const ParamDecl *param = paramList->getArray().back();

    // Sanity-check that the trailing closure corresponds to this parameter.
    if (!param->hasValidSignature() ||
        !param->getInterfaceType()->is<AnyFunctionType>())
      return false;

    Identifier trailingClosureLabel = param->getArgumentName();
    auto &choiceForLabel = choicesByLabel[trailingClosureLabel];

    // FIXME: Cargo-culted from diagnoseAmbiguity: apparently the same decl can
    // appear more than once?
    if (choiceForLabel == callee)
      continue;

    // If just providing the trailing closure label won't solve the ambiguity,
    // don't bother offering the fix-it.
    if (choiceForLabel != nullptr)
      return false;

    choiceForLabel = callee;
  }

  // If we got here, then all of the choices have unique labels. Offer them in
  // order.
  for (const auto &choicePair : choicesByLabel) {
    auto diag = emitDiagnostic(
        expr->getLoc(), diag::ambiguous_because_of_trailing_closure,
        choicePair.first.empty(), choicePair.second->getFullName());
    swift::fixItEncloseTrailingClosure(getTypeChecker(), diag, callExpr,
                                       choicePair.first);
  }

  return true;
}

AssignmentFailure::AssignmentFailure(Expr *destExpr, ConstraintSystem &cs,
                                     SourceLoc diagnosticLoc)
    : FailureDiagnostic(destExpr, cs, cs.getConstraintLocator(destExpr)),
      Loc(diagnosticLoc),
      DeclDiagnostic(findDeclDiagonstic(cs.getASTContext(), destExpr)),
      TypeDiagnostic(diag::assignment_lhs_not_lvalue) {}

bool AssignmentFailure::diagnoseAsError() {
  auto &cs = getConstraintSystem();
  auto *DC = getDC();
  auto *destExpr = getParentExpr();

  // Walk through the destination expression, resolving what the problem is.  If
  // we find a node in the lvalue path that is problematic, this returns it.
  auto immInfo = resolveImmutableBase(destExpr);

  // Otherwise, we cannot resolve this because the available setter candidates
  // are all mutating and the base must be mutating.  If we dug out a
  // problematic decl, we can produce a nice tailored diagnostic.
  if (auto *VD = dyn_cast_or_null<VarDecl>(immInfo.second)) {
    std::string message = "'";
    message += VD->getName().str().str();
    message += "'";

    auto type = getType(immInfo.first);
    auto bgt = type ? type->getAs<BoundGenericType>() : nullptr;

    if (bgt && bgt->getDecl() == getASTContext().getKeyPathDecl())
      message += " is a read-only key path";
    else if (VD->isCaptureList())
      message += " is an immutable capture";
    else if (VD->isImplicit())
      message += " is immutable";
    else if (VD->isLet())
      message += " is a 'let' constant";
    else if (!VD->isSettable(DC))
      message += " is a get-only property";
    else if (!VD->isSetterAccessibleFrom(DC))
      message += " setter is inaccessible";
    else {
      message += " is immutable";
    }

    emitDiagnostic(Loc, DeclDiagnostic, message)
        .highlight(immInfo.first->getSourceRange());

    // If there is a masked instance variable of the same type, emit a
    // note to fixit prepend a 'self.'.
    if (auto typeContext = DC->getInnermostTypeContext()) {
      UnqualifiedLookup lookup(VD->getFullName(), typeContext,
                               getASTContext().getLazyResolver());
      for (auto &result : lookup.Results) {
        const VarDecl *typeVar = dyn_cast<VarDecl>(result.getValueDecl());
        if (typeVar && typeVar != VD && typeVar->isSettable(DC) &&
            typeVar->isSetterAccessibleFrom(DC) &&
            typeVar->getType()->isEqual(VD->getType())) {
          // But not in its own accessor.
          auto AD =
              dyn_cast_or_null<AccessorDecl>(DC->getInnermostMethodContext());
          if (!AD || AD->getStorage() != typeVar) {
            emitDiagnostic(Loc, diag::masked_instance_variable,
                           typeContext->getSelfTypeInContext())
                .fixItInsert(Loc, "self.");
          }
        }
      }
    }

    // If this is a simple variable marked with a 'let', emit a note to fixit
    // hint it to 'var'.
    VD->emitLetToVarNoteIfSimple(DC);
    return true;
  }

  // If the underlying expression was a read-only subscript, diagnose that.
  if (auto *SD = dyn_cast_or_null<SubscriptDecl>(immInfo.second)) {
    StringRef message;
    if (!SD->isSettable())
      message = "subscript is get-only";
    else if (!SD->isSetterAccessibleFrom(DC))
      message = "subscript setter is inaccessible";
    else
      message = "subscript is immutable";

    emitDiagnostic(Loc, DeclDiagnostic, message)
        .highlight(immInfo.first->getSourceRange());
    return true;
  }

  // If we're trying to set an unapplied method, say that.
  if (auto *VD = immInfo.second) {
    std::string message = "'";
    message += VD->getBaseName().getIdentifier().str();
    message += "'";

    auto diagID = DeclDiagnostic;
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(VD)) {
      if (AFD->hasImplicitSelfDecl()) {
        message += " is a method";
        diagID = diag::assignment_lhs_is_immutable_variable;
      } else {
        message += " is a function";
      }
    } else
      message += " is not settable";

    emitDiagnostic(Loc, diagID, message)
        .highlight(immInfo.first->getSourceRange());
    return true;
  }

  // If a keypath was the problem but wasn't resolved into a vardecl
  // it is ambiguous or unable to be used for setting.
  if (auto *KPE = dyn_cast_or_null<KeyPathExpr>(immInfo.first)) {
    emitDiagnostic(Loc, DeclDiagnostic, "immutable key path")
        .highlight(KPE->getSourceRange());
    return true;
  }

  // If the expression is the result of a call, it is an rvalue, not a mutable
  // lvalue.
  if (auto *AE = dyn_cast<ApplyExpr>(immInfo.first)) {
    // Handle literals, which are a call to the conversion function.
    auto argsTuple =
        dyn_cast<TupleExpr>(AE->getArg()->getSemanticsProvidingExpr());
    if (isa<CallExpr>(AE) && AE->isImplicit() && argsTuple &&
        argsTuple->getNumElements() == 1) {
      if (auto LE = dyn_cast<LiteralExpr>(
              argsTuple->getElement(0)->getSemanticsProvidingExpr())) {
        emitDiagnostic(Loc, DeclDiagnostic, "literals are not mutable")
            .highlight(LE->getSourceRange());
        return true;
      }
    }

    std::string name = "call";
    if (isa<PrefixUnaryExpr>(AE) || isa<PostfixUnaryExpr>(AE))
      name = "unary operator";
    else if (isa<BinaryExpr>(AE))
      name = "binary operator";
    else if (isa<CallExpr>(AE))
      name = "function call";
    else if (isa<DotSyntaxCallExpr>(AE) || isa<DotSyntaxBaseIgnoredExpr>(AE))
      name = "method call";

    if (auto *DRE = dyn_cast<DeclRefExpr>(AE->getFn()->getValueProvidingExpr()))
      name = std::string("'") +
             DRE->getDecl()->getBaseName().getIdentifier().str().str() + "'";

    emitDiagnostic(Loc, DeclDiagnostic, name + " returns immutable value")
        .highlight(AE->getSourceRange());
    return true;
  }

  if (auto contextualType = cs.getContextualType(immInfo.first)) {
    Type neededType = contextualType->getInOutObjectType();
    Type actualType = getType(immInfo.first)->getInOutObjectType();
    if (!neededType->isEqual(actualType)) {
      if (DeclDiagnostic.ID == diag::cannot_pass_rvalue_inout_subelement.ID) {
        // We have a special diagnostic with tailored wording for this
        // common case.
        emitDiagnostic(Loc, diag::cannot_pass_rvalue_inout_converted,
                       actualType, neededType)
            .highlight(immInfo.first->getSourceRange());

        if (auto inoutExpr = dyn_cast<InOutExpr>(immInfo.first))
          fixItChangeInoutArgType(inoutExpr->getSubExpr(), actualType,
                                  neededType);
      } else {
        emitDiagnostic(Loc, DeclDiagnostic,
                       "implicit conversion from '" + actualType->getString() +
                           "' to '" + neededType->getString() +
                           "' requires a temporary")
            .highlight(immInfo.first->getSourceRange());
      }
      return true;
    }
  }

  if (auto IE = dyn_cast<IfExpr>(immInfo.first)) {
    if (isLoadedLValue(IE)) {
      emitDiagnostic(Loc, DeclDiagnostic,
                     "result of conditional operator '? :' is never mutable")
          .highlight(IE->getQuestionLoc())
          .highlight(IE->getColonLoc());
      return true;
    }
  }

  emitDiagnostic(Loc, TypeDiagnostic, getType(destExpr))
      .highlight(immInfo.first->getSourceRange());
  return true;
}

void AssignmentFailure::fixItChangeInoutArgType(const Expr *arg,
                                                Type actualType,
                                                Type neededType) const {
  auto *DC = getDC();
  auto *DRE = dyn_cast<DeclRefExpr>(arg);
  if (!DRE)
    return;

  auto *VD = dyn_cast_or_null<VarDecl>(DRE->getDecl());
  if (!VD)
    return;

  // Don't emit for non-local variables.
  // (But in script-mode files, we consider module-scoped
  // variables in the same file to be local variables.)
  auto VDC = VD->getDeclContext();
  bool isLocalVar = VDC->isLocalContext();
  if (!isLocalVar && VDC->isModuleScopeContext()) {
    auto argFile = DC->getParentSourceFile();
    auto varFile = VDC->getParentSourceFile();
    isLocalVar = (argFile == varFile && argFile->isScriptMode());
  }
  if (!isLocalVar)
    return;

  SmallString<32> scratch;
  SourceLoc endLoc;   // Filled in if we decide to diagnose this
  SourceLoc startLoc; // Left invalid if we're inserting

  auto isSimpleTypelessPattern = [](Pattern *P) -> bool {
    if (auto VP = dyn_cast_or_null<VarPattern>(P))
      P = VP->getSubPattern();
    return P && isa<NamedPattern>(P);
  };

  auto typeRange = VD->getTypeSourceRangeForDiagnostics();
  if (typeRange.isValid()) {
    startLoc = typeRange.Start;
    endLoc = typeRange.End;
  } else if (isSimpleTypelessPattern(VD->getParentPattern())) {
    endLoc = VD->getNameLoc();
    scratch += ": ";
  }

  if (endLoc.isInvalid())
    return;

  scratch += neededType.getString();

  // Adjust into the location where we actually want to insert
  endLoc = Lexer::getLocForEndOfToken(getASTContext().SourceMgr, endLoc);

  // Since we already adjusted endLoc, this will turn an insertion
  // into a zero-character replacement.
  if (!startLoc.isValid())
    startLoc = endLoc;

  emitDiagnostic(VD->getLoc(), diag::inout_change_var_type_if_possible,
                 actualType, neededType)
      .fixItReplaceChars(startLoc, endLoc, scratch);
}

std::pair<Expr *, ValueDecl *>
AssignmentFailure::resolveImmutableBase(Expr *expr) const {
  auto &cs = getConstraintSystem();
  auto *DC = getDC();
  expr = expr->getValueProvidingExpr();

  // Provide specific diagnostics for assignment to subscripts whose base expr
  // is known to be an rvalue.
  if (auto *SE = dyn_cast<SubscriptExpr>(expr)) {
    // If we found a decl for the subscript, check to see if it is a set-only
    // subscript decl.
    SubscriptDecl *member = nullptr;
    if (SE->hasDecl())
      member = dyn_cast_or_null<SubscriptDecl>(SE->getDecl().getDecl());

    if (!member) {
      auto loc =
          cs.getConstraintLocator(SE, ConstraintLocator::SubscriptMember);
      member = dyn_cast_or_null<SubscriptDecl>(cs.findResolvedMemberRef(loc));
    }

    // If it isn't settable, return it.
    if (member) {
      if (!member->isSettable() || !member->isSetterAccessibleFrom(DC))
        return {expr, member};
    }

    if (auto tupleExpr = dyn_cast<TupleExpr>(SE->getIndex())) {
      if (tupleExpr->getNumElements() == 1 &&
          tupleExpr->getElementName(0).str() == "keyPath") {
        auto indexType = getType(tupleExpr->getElement(0));
        if (auto bgt = indexType->getAs<BoundGenericType>()) {
          if (bgt->getDecl() == getASTContext().getKeyPathDecl())
            return resolveImmutableBase(tupleExpr->getElement(0));
        }
      }
    }

    // If it is settable, then the base must be the problem, recurse.
    return resolveImmutableBase(SE->getBase());
  }

  // Look through property references.
  if (auto *UDE = dyn_cast<UnresolvedDotExpr>(expr)) {
    // If we found a decl for the UDE, check it.
    auto loc = cs.getConstraintLocator(UDE, ConstraintLocator::Member);

    // If we can resolve a member, we can determine whether it is settable in
    // this context.
    if (auto *member = cs.findResolvedMemberRef(loc)) {
      auto *memberVD = dyn_cast<VarDecl>(member);

      // If the member isn't a vardecl (e.g. its a funcdecl), or it isn't
      // settable, then it is the problem: return it.
      if (!memberVD || !member->isSettable(nullptr) ||
          !memberVD->isSetterAccessibleFrom(DC))
        return {expr, member};
    }

    // If we weren't able to resolve a member or if it is mutable, then the
    // problem must be with the base, recurse.
    return resolveImmutableBase(UDE->getBase());
  }

  if (auto *MRE = dyn_cast<MemberRefExpr>(expr)) {
    // If the member isn't settable, then it is the problem: return it.
    if (auto member = dyn_cast<AbstractStorageDecl>(MRE->getMember().getDecl()))
      if (!member->isSettable(nullptr) || !member->isSetterAccessibleFrom(DC))
        return {expr, member};

    // If we weren't able to resolve a member or if it is mutable, then the
    // problem must be with the base, recurse.
    return resolveImmutableBase(MRE->getBase());
  }

  if (auto *DRE = dyn_cast<DeclRefExpr>(expr))
    return {expr, DRE->getDecl()};

  // Look through x!
  if (auto *FVE = dyn_cast<ForceValueExpr>(expr))
    return resolveImmutableBase(FVE->getSubExpr());

  // Look through x?
  if (auto *BOE = dyn_cast<BindOptionalExpr>(expr))
    return resolveImmutableBase(BOE->getSubExpr());

  // Look through implicit conversions
  if (auto *ICE = dyn_cast<ImplicitConversionExpr>(expr))
    if (!isa<LoadExpr>(ICE->getSubExpr()))
      return resolveImmutableBase(ICE->getSubExpr());

  if (auto *SAE = dyn_cast<SelfApplyExpr>(expr))
    return resolveImmutableBase(SAE->getFn());

  return {expr, nullptr};
}

Diag<StringRef> AssignmentFailure::findDeclDiagonstic(ASTContext &ctx,
                                                      Expr *destExpr) {
  if (isa<ApplyExpr>(destExpr) || isa<SelfApplyExpr>(destExpr))
    return diag::assignment_lhs_is_apply_expression;

  if (isa<UnresolvedDotExpr>(destExpr) || isa<MemberRefExpr>(destExpr))
    return diag::assignment_lhs_is_immutable_property;

  if (auto *subscript = dyn_cast<SubscriptExpr>(destExpr)) {
    auto diagID = diag::assignment_subscript_has_immutable_base;
    // If the destination is a subscript with a 'dynamicLookup:' label and if
    // the tuple is implicit, then this was actually a @dynamicMemberLookup
    // access. Emit a more specific diagnostic.
    if (subscript->getIndex()->isImplicit() &&
        subscript->getArgumentLabels().size() == 1 &&
        subscript->getArgumentLabels().front() == ctx.Id_dynamicMember)
      diagID = diag::assignment_dynamic_property_has_immutable_base;

    return diagID;
  }

  return diag::assignment_lhs_is_immutable_variable;
}

bool ContextualFailure::diagnoseAsError() {
  auto *anchor = getAnchor();
  auto path = getLocator()->getPath();

  assert(!path.empty());

  if (diagnoseMissingFunctionCall())
    return true;

  Diag<Type, Type> diagnostic;
  switch (path.back().getKind()) {
  case ConstraintLocator::ClosureResult: {
    diagnostic = diag::cannot_convert_closure_result;
    break;
  }

  default:
    return false;
  }

  auto diag = emitDiagnostic(anchor->getLoc(), diagnostic, FromType, ToType);
  diag.highlight(anchor->getSourceRange());

  (void)trySequenceSubsequenceFixIts(diag, getConstraintSystem(), FromType,
                                     ToType, anchor);
  return true;
}

bool ContextualFailure::diagnoseMissingFunctionCall() const {
  auto &TC = getTypeChecker();

  auto *srcFT = FromType->getAs<FunctionType>();
  if (!srcFT || !srcFT->getParams().empty())
    return false;

  if (ToType->is<AnyFunctionType>() ||
      !TC.isConvertibleTo(srcFT->getResult(), ToType, getDC()))
    return false;

  auto *anchor = getAnchor();
  emitDiagnostic(anchor->getLoc(), diag::missing_nullary_call,
                 srcFT->getResult())
      .highlight(anchor->getSourceRange())
      .fixItInsertAfter(anchor->getEndLoc(), "()");
  return true;
}

bool ContextualFailure::trySequenceSubsequenceFixIts(InFlightDiagnostic &diag,
                                                     ConstraintSystem &CS,
                                                     Type fromType, Type toType,
                                                     Expr *expr) {
  if (!CS.TC.Context.getStdlibModule())
    return false;

  auto String = CS.TC.getStringType(CS.DC);
  auto Substring = CS.TC.getSubstringType(CS.DC);

  if (!String || !Substring)
    return false;

  /// FIXME: Remove this flag when void subscripts are implemented.
  /// Make this unconditional and remove the if statement.
  if (CS.TC.getLangOpts().FixStringToSubstringConversions) {
    // String -> Substring conversion
    // Add '[]' void subscript call to turn the whole String into a Substring
    if (fromType->isEqual(String)) {
      if (toType->isEqual(Substring)) {
        diag.fixItInsertAfter(expr->getEndLoc(), "[]");
        return true;
      }
    }
  }

  // Substring -> String conversion
  // Wrap in String.init
  if (fromType->isEqual(Substring)) {
    if (toType->isEqual(String)) {
      auto range = expr->getSourceRange();
      diag.fixItInsert(range.Start, "String(");
      diag.fixItInsertAfter(range.End, ")");
      return true;
    }
  }

  return false;
}
