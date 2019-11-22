//===--- DerivedConformanceComparable.cpp - Derived Comparable -===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements implicit derivation of the Comparable protocol.
//  (Most of this code is similar to code in `DerivedConformanceEquatableHashable.cpp`)
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "DerivedConformances.h"

using namespace swift;

/// Returns a generated guard statement that checks whether the given lhs and
/// rhs expressions are equal. If not equal, the else block for the guard
/// returns lhs < rhs.
/// \p C The AST context.
/// \p lhsExpr The first expression to compare for equality.
/// \p rhsExpr The second expression to compare for equality.
static GuardStmt *returnComparisonIfNotEqualGuard(ASTContext &C,
                                        Expr *lhsExpr,
                                        Expr *rhsExpr) {
  SmallVector<StmtConditionElement, 1> conditions;
  SmallVector<ASTNode, 1> statements;

  // First, generate the statement for the body of the guard.
  // return lhs < rhs
  auto ltFuncExpr = new (C) UnresolvedDeclRefExpr(
    DeclName(C.getIdentifier("<")), DeclRefKind::BinaryOperator,
    DeclNameLoc());
  auto ltArgsTuple = TupleExpr::create(C, SourceLoc(),
                                        { lhsExpr, rhsExpr },
                                        { }, { }, SourceLoc(),
                                        /*HasTrailingClosure*/false,
                                        /*Implicit*/true);
  auto ltExpr = new (C) BinaryExpr(ltFuncExpr, ltArgsTuple, /*Implicit*/true);
  auto returnStmt = new (C) ReturnStmt(SourceLoc(), ltExpr);
  statements.emplace_back(ASTNode(returnStmt));

  // Next, generate the condition being checked.
  // lhs == rhs
  auto cmpFuncExpr = new (C) UnresolvedDeclRefExpr(
    DeclName(C.getIdentifier("==")), DeclRefKind::BinaryOperator,
    DeclNameLoc());
  auto cmpArgsTuple = TupleExpr::create(C, SourceLoc(),
                                        { lhsExpr, rhsExpr },
                                        { }, { }, SourceLoc(),
                                        /*HasTrailingClosure*/false,
                                        /*Implicit*/true);
  auto cmpExpr = new (C) BinaryExpr(cmpFuncExpr, cmpArgsTuple,
                                    /*Implicit*/true);
  conditions.emplace_back(cmpExpr);

  // Build and return the complete guard statement.
  // guard lhs == rhs else { return lhs < rhs }
  auto body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
  return new (C) GuardStmt(SourceLoc(), C.AllocateCopy(conditions), body);
}

// how does this code ever even get invoked? you can’t compare uninhabited enums...
static std::pair<BraceStmt *, bool>
deriveBodyComparable_enum_uninhabited_lt(AbstractFunctionDecl *ltDecl, void *) {
  auto parentDC = ltDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto args = ltDecl->getParameters();
  auto aParam = args->get(0);
  auto bParam = args->get(1);

  assert(!cast<EnumDecl>(aParam->getType()->getAnyNominal())->hasCases());

  SmallVector<ASTNode, 1> statements;
  SmallVector<ASTNode, 0> cases;

  // switch (a, b) { }
  auto aRef = new (C) DeclRefExpr(aParam, DeclNameLoc(), /*implicit*/ true,
                                  AccessSemantics::Ordinary,
                                  aParam->getType());
  auto bRef = new (C) DeclRefExpr(bParam, DeclNameLoc(), /*implicit*/ true,
                                  AccessSemantics::Ordinary,
                                  bParam->getType());
  TupleTypeElt abTupleElts[2] = { aParam->getType(), bParam->getType() };
  auto abExpr = TupleExpr::create(C, SourceLoc(), {aRef, bRef}, {}, {},
                                  SourceLoc(), /*HasTrailingClosure*/ false,
                                  /*implicit*/ true,
                                  TupleType::get(abTupleElts, C));
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), abExpr,
                                       SourceLoc(), cases, SourceLoc(), C);
  statements.push_back(switchStmt);

  auto body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
  return { body, /*isTypeChecked=*/true };
}

/// Derive the body for a '<' operator for an enum that has no associated
/// values. This generates code that converts each value to its integer ordinal
/// and compares them, which produces an optimal single icmp instruction.
static std::pair<BraceStmt *, bool>
deriveBodyComparable_enum_noAssociatedValues_lt(AbstractFunctionDecl *ltDecl,
                                               void *) { 
  auto parentDC = ltDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto args = ltDecl->getParameters();
  auto aParam = args->get(0);
  auto bParam = args->get(1);

  auto enumDecl = cast<EnumDecl>(aParam->getType()->getAnyNominal());

  // Generate the conversion from the enums to integer indices.
  SmallVector<ASTNode, 6> statements;
  DeclRefExpr *aIndex = convertEnumToIndex(statements, parentDC, enumDecl,
                                           aParam, ltDecl, "index_a");
  DeclRefExpr *bIndex = convertEnumToIndex(statements, parentDC, enumDecl,
                                           bParam, ltDecl, "index_b");

  // Generate the compare of the indices.
  FuncDecl *cmpFunc = C.getLessThanIntDecl();
  assert(cmpFunc && "should have a < for int as we already checked for it");

  auto fnType = cmpFunc->getInterfaceType()->castTo<FunctionType>();

  Expr *cmpFuncExpr;
  if (cmpFunc->getDeclContext()->isTypeContext()) {
    auto contextTy = cmpFunc->getDeclContext()->getSelfInterfaceType();
    Expr *base = TypeExpr::createImplicitHack(SourceLoc(), contextTy, C);
    Expr *ref = new (C) DeclRefExpr(cmpFunc, DeclNameLoc(), /*Implicit*/ true,
                                    AccessSemantics::Ordinary, fnType);

    fnType = fnType->getResult()->castTo<FunctionType>();
    cmpFuncExpr = new (C) DotSyntaxCallExpr(ref, SourceLoc(), base, fnType);
    cmpFuncExpr->setImplicit();
  } else {
    cmpFuncExpr = new (C) DeclRefExpr(cmpFunc, DeclNameLoc(),
                                      /*implicit*/ true,
                                      AccessSemantics::Ordinary,
                                      fnType);
  }

  TupleTypeElt abTupleElts[2] = { aIndex->getType(), bIndex->getType() };
  TupleExpr *abTuple = TupleExpr::create(C, SourceLoc(), { aIndex, bIndex },
                                         { }, { }, SourceLoc(),
                                         /*HasTrailingClosure*/ false,
                                         /*Implicit*/ true,
                                         TupleType::get(abTupleElts, C));

  auto *cmpExpr = new (C) BinaryExpr(
      cmpFuncExpr, abTuple, /*implicit*/ true,
      fnType->castTo<FunctionType>()->getResult());
  statements.push_back(new (C) ReturnStmt(SourceLoc(), cmpExpr));

  BraceStmt *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
  return { body, /*isTypeChecked=*/true };
}

/// Derive the body for an '==' operator for an enum where at least one of the
/// cases has associated values.
static std::pair<BraceStmt *, bool>
deriveBodyComparable_enum_hasAssociatedValues_lt(AbstractFunctionDecl *ltDecl, void *) {
  auto parentDC = ltDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto args = ltDecl->getParameters();
  auto aParam = args->get(0);
  auto bParam = args->get(1);

  Type enumType = aParam->getType();
  auto enumDecl = cast<EnumDecl>(aParam->getType()->getAnyNominal());

  SmallVector<ASTNode, 6> statements;
  SmallVector<ASTNode, 4> cases;
  unsigned elementCount = 0;

  // For each enum element, generate a case statement matching a pair containing
  // the same case, binding variables for the left- and right-hand associated
  // values.
  for (auto elt : enumDecl->getAllElements()) {
    elementCount++;

    // .<elt>(let l0, let l1, ...)
    SmallVector<VarDecl*, 3> lhsPayloadVars;
    auto lhsSubpattern = enumElementPayloadSubpattern(elt, 'l', ltDecl,
                                                      lhsPayloadVars);
    auto lhsElemPat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                                 SourceLoc(), SourceLoc(),
                                                 Identifier(), elt,
                                                 lhsSubpattern);
    lhsElemPat->setImplicit();

    // .<elt>(let r0, let r1, ...)
    SmallVector<VarDecl*, 3> rhsPayloadVars;
    auto rhsSubpattern = enumElementPayloadSubpattern(elt, 'r', ltDecl,
                                                      rhsPayloadVars);
    auto rhsElemPat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                                 SourceLoc(), SourceLoc(),
                                                 Identifier(), elt,
                                                 rhsSubpattern);
    rhsElemPat->setImplicit();

    auto hasBoundDecls = !lhsPayloadVars.empty();
    Optional<MutableArrayRef<VarDecl *>> caseBodyVarDecls;
    if (hasBoundDecls) {
      // We allocated a direct copy of our lhs var decls for the case
      // body.
      auto copy = C.Allocate<VarDecl *>(lhsPayloadVars.size());
      for (unsigned i : indices(lhsPayloadVars)) {
        auto *vOld = lhsPayloadVars[i];
        auto *vNew = new (C) VarDecl(
            /*IsStatic*/ false, vOld->getIntroducer(), false /*IsCaptureList*/,
            vOld->getNameLoc(), vOld->getName(), vOld->getDeclContext());
        vNew->setHasNonPatternBindingInit();
        vNew->setImplicit();
        copy[i] = vNew;
      }
      caseBodyVarDecls.emplace(copy);
    }

    // case (.<elt>(let l0, let l1, ...), .<elt>(let r0, let r1, ...))
    auto caseTuplePattern = TuplePattern::create(C, SourceLoc(), {
      TuplePatternElt(lhsElemPat), TuplePatternElt(rhsElemPat) },
                                                 SourceLoc());
    caseTuplePattern->setImplicit();

    auto labelItem = CaseLabelItem(caseTuplePattern);

    // Generate a guard statement for each associated value in the payload,
    // breaking out early if any pair is unequal. (same as Equatable synthesis.)
    // the else statement performs the lexicographic comparison.
    SmallVector<ASTNode, 6> statementsInCase;
    for (size_t varIdx = 0; varIdx < lhsPayloadVars.size(); varIdx++) {
      auto lhsVar = lhsPayloadVars[varIdx];
      auto lhsExpr = new (C) DeclRefExpr(lhsVar, DeclNameLoc(),
                                         /*implicit*/true);
      auto rhsVar = rhsPayloadVars[varIdx];
      auto rhsExpr = new (C) DeclRefExpr(rhsVar, DeclNameLoc(),
                                         /*Implicit*/true);
      auto guardStmt = returnComparisonIfNotEqualGuard(C, lhsExpr, rhsExpr);
      statementsInCase.emplace_back(guardStmt);
    }

    // If none of the guard statements caused an early exit, then all the pairs
    // were true. (equal)
    // return false 
    auto falseExpr = new (C) BooleanLiteralExpr(false, SourceLoc(),
                                               /*Implicit*/true);
    auto returnStmt = new (C) ReturnStmt(SourceLoc(), falseExpr);
    statementsInCase.push_back(returnStmt);

    auto body = BraceStmt::create(C, SourceLoc(), statementsInCase,
                                  SourceLoc());
    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem, SourceLoc(),
                                     SourceLoc(), body, caseBodyVarDecls));
  }

  // default: result = <enum index>(lhs) < <enum index>(rhs)
  //
  // We only generate this if the enum has more than one case. If it has exactly
  // one case, then that single case statement is already exhaustive.
  if (elementCount > 1) {
    auto defaultPattern = new (C) AnyPattern(SourceLoc());
    defaultPattern->setImplicit();
    auto defaultItem = CaseLabelItem::getDefault(defaultPattern);
    auto body = deriveBodyComparable_enum_noAssociatedValues_lt(ltDecl, nullptr).first;
    cases.push_back(CaseStmt::create(C, SourceLoc(), defaultItem, SourceLoc(),
                                     SourceLoc(), body,
                                     /*case body var decls*/ None));
  }

  // switch (a, b) { <case statements> }
  auto aRef = new (C) DeclRefExpr(aParam, DeclNameLoc(), /*implicit*/true);
  auto bRef = new (C) DeclRefExpr(bParam, DeclNameLoc(), /*implicit*/true);
  auto abExpr = TupleExpr::create(C, SourceLoc(), { aRef, bRef }, {}, {},
                                  SourceLoc(), /*HasTrailingClosure*/ false,
                                  /*implicit*/ true);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), abExpr,
                                       SourceLoc(), cases, SourceLoc(), C);
  statements.push_back(switchStmt);

  auto body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
  return { body, /*isTypeChecked=*/false };
}

/// Derive an '<' operator implementation for an enum.
static ValueDecl *
deriveComparable_lt(
    DerivedConformance &derived,
    std::pair<BraceStmt *, bool> (*bodySynthesizer)(AbstractFunctionDecl *,
                                                    void *)) {
  ASTContext &C = derived.Context;

  auto parentDC = derived.getConformanceContext();
  auto selfIfaceTy = parentDC->getDeclaredInterfaceType();

  auto getParamDecl = [&](StringRef s) -> ParamDecl * {
    auto *param = new (C) ParamDecl(SourceLoc(),
                                    SourceLoc(), Identifier(), SourceLoc(),
                                    C.getIdentifier(s), parentDC);
    param->setSpecifier(ParamSpecifier::Default);
    param->setInterfaceType(selfIfaceTy);
    return param;
  };

  ParameterList *params = ParameterList::create(C, {
    getParamDecl("a"),
    getParamDecl("b")
  });

  auto boolTy = C.getBoolDecl()->getDeclaredType();

  Identifier generatedIdentifier;
  if (parentDC->getParentModule()->isResilient()) {
    generatedIdentifier = C.Id_LessThanOperator;
  } else {
    assert(selfIfaceTy->getEnumOrBoundGenericEnum());
    generatedIdentifier = C.Id_derived_enum_less_than;
  }

  DeclName name(C, generatedIdentifier, params);
  auto comparableDecl =
    FuncDecl::create(C, /*StaticLoc=*/SourceLoc(),
                     StaticSpellingKind::KeywordStatic,
                     /*FuncLoc=*/SourceLoc(), name, /*NameLoc=*/SourceLoc(),
                     /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                     /*GenericParams=*/nullptr,
                     params,
                     TypeLoc::withoutLoc(boolTy),
                     parentDC);
  comparableDecl->setImplicit();
  comparableDecl->setUserAccessible(false);

  // Add the @_implements(Comparable, < (_:_:)) attribute
  if (generatedIdentifier != C.Id_LessThanOperator) {
    auto comparable = C.getProtocol(KnownProtocolKind::Comparable);
    auto comparableType = comparable->getDeclaredType();
    auto comparableTypeLoc = TypeLoc::withoutLoc(comparableType);
    SmallVector<Identifier, 2> argumentLabels = { Identifier(), Identifier() };
    auto comparableDeclName = DeclName(C, DeclBaseName(C.Id_LessThanOperator),
                                   argumentLabels);
    comparableDecl->getAttrs().add(new (C) ImplementsAttr(SourceLoc(),
                                                          SourceRange(),
                                                          comparableTypeLoc,
                                                          comparableDeclName,
                                                          DeclNameLoc()));
  }

  if (!C.getLessThanIntDecl()) {
    derived.ConformanceDecl->diagnose(diag::no_less_than_overload_for_int);
    return nullptr;
  }

  comparableDecl->setBodySynthesizer(bodySynthesizer);

  comparableDecl->copyFormalAccessFrom(derived.Nominal, /*sourceIsParentContext*/ true);

  // Add the operator to the parent scope.
  derived.addMembersToConformanceContext({comparableDecl});

  return comparableDecl;
}

bool 
DerivedConformance::canDeriveComparable(DeclContext *context, NominalTypeDecl *declaration) {
  // The type must be an enum.
  if (EnumDecl *const enumeration = dyn_cast<EnumDecl>(declaration)) {
    // The cases must not have non-comparable associated values or raw backing
    auto comparable = context->getASTContext().getProtocol(KnownProtocolKind::Comparable);
    return allAssociatedValuesConformToProtocol(context, enumeration, comparable) && !enumeration->hasRawType();
  } else {
    return false;
  }
}

ValueDecl *DerivedConformance::deriveComparable(ValueDecl *requirement) {
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  // Build the necessary decl.
  if (requirement->getBaseName() == "<") {
    if (EnumDecl const *const enumeration = dyn_cast<EnumDecl>(this->Nominal)) {
      auto bodySynthesizer = !enumeration->hasCases() 
        ? &deriveBodyComparable_enum_uninhabited_lt 
        : enumeration->hasOnlyCasesWithoutAssociatedValues()
            ? &deriveBodyComparable_enum_noAssociatedValues_lt
            : &deriveBodyComparable_enum_hasAssociatedValues_lt; 
      return deriveComparable_lt(*this, bodySynthesizer);
    } else {
      llvm_unreachable("todo");
    }
  }
  requirement->diagnose(diag::broken_comparable_requirement);
  return nullptr;
}
