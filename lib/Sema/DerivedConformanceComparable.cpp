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

// how does this code ever even get invoked? you can’t compare uninhabited enums...
static void
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
  auto aRef = new (C) DeclRefExpr(aParam, DeclNameLoc(), /*implicit*/ true);
  auto bRef = new (C) DeclRefExpr(bParam, DeclNameLoc(), /*implicit*/ true);
  auto abExpr = TupleExpr::create(C, SourceLoc(), {aRef, bRef}, {}, {},
                                  SourceLoc(), /*HasTrailingClosure*/ false,
                                  /*implicit*/ true);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), abExpr,
                                       SourceLoc(), cases, SourceLoc(), C);
  statements.push_back(switchStmt);

  auto body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
  ltDecl->setBody(body);
}

/// Derive the body for a '<' operator for an enum that has no associated
/// values. This generates code that converts each value to its integer ordinal
/// and compares them, which produces an optimal single icmp instruction.
static void
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

  TupleExpr *abTuple = TupleExpr::create(C, SourceLoc(), { aIndex, bIndex },
                                         { }, { }, SourceLoc(),
                                         /*HasTrailingClosure*/ false,
                                         /*Implicit*/ true);

  auto *cmpExpr = new (C) BinaryExpr(cmpFuncExpr, abTuple, /*implicit*/ true);
  statements.push_back(new (C) ReturnStmt(SourceLoc(), cmpExpr));

  BraceStmt *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
  ltDecl->setBody(body);
}

/// Derive an '<' operator implementation for an enum.
static ValueDecl *
deriveComparable_lt(DerivedConformance &derived,
                   void (*bodySynthesizer)(AbstractFunctionDecl *, void *)) {
  ASTContext &C = derived.TC.Context;

  auto parentDC = derived.getConformanceContext();
  auto selfIfaceTy = parentDC->getDeclaredInterfaceType();

  auto getParamDecl = [&](StringRef s) -> ParamDecl * {
    auto *param = new (C) ParamDecl(VarDecl::Specifier::Default, SourceLoc(),
                                    SourceLoc(), Identifier(), SourceLoc(),
                                    C.getIdentifier(s), parentDC);
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
  comparableDecl->getAttrs().add(new (C) InfixAttr(/*implicit*/false));

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

  if (!C.getEqualIntDecl()) {
    derived.TC.diagnose(derived.ConformanceDecl->getLoc(),
                        diag::no_equal_overload_for_int);
    return nullptr;
  }

  comparableDecl->setBodySynthesizer(bodySynthesizer);

  // Compute the interface type.
  if (auto genericEnv = parentDC->getGenericEnvironmentOfContext())
    comparableDecl->setGenericEnvironment(genericEnv);
  comparableDecl->computeType();

  comparableDecl->copyFormalAccessFrom(derived.Nominal, /*sourceIsParentContext*/ true);
  comparableDecl->setValidationToChecked();

  C.addSynthesizedDecl(comparableDecl);

  // Add the operator to the parent scope.
  derived.addMembersToConformanceContext({comparableDecl});

  return comparableDecl;
}

bool 
DerivedConformance::canDeriveComparable(DeclContext *context, NominalTypeDecl *declaration) {
  // The type must be an enum.
  if (EnumDecl const *const enumeration = dyn_cast<EnumDecl>(declaration)) {
    // The cases must not have associated values or raw backing
    return enumeration->hasOnlyCasesWithoutAssociatedValues() && !enumeration->hasRawType();
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
      auto bodySynthesizer = enumeration->hasCases() 
        ? &deriveBodyComparable_enum_noAssociatedValues_lt 
        : &deriveBodyComparable_enum_uninhabited_lt;
      return deriveComparable_lt(*this, bodySynthesizer);
    } else {
      llvm_unreachable("todo");
    }
  }
  TC.diagnose(requirement->getLoc(), diag::broken_equatable_requirement);
  return nullptr;
}
