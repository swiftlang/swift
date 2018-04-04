//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements implicit derivation of the CaseIterable protocol.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "llvm/Support/raw_ostream.h"
#include "DerivedConformances.h"

using namespace swift;
using namespace DerivedConformance;

/// Common preconditions for CaseIterable.
static bool canDeriveConformance(NominalTypeDecl *type) {
  // The type must be an enum.
  auto enumDecl = dyn_cast<EnumDecl>(type);
  if (!enumDecl)
    return false;

  // "Simple" enums without availability attributes can derive
  // a CaseIterable conformance.
  //
  // FIXME: Lift the availability restriction.
  return !enumDecl->hasPotentiallyUnavailableCaseValue()
      && enumDecl->hasOnlyCasesWithoutAssociatedValues();
}

/// Derive the implementation of allCases for a "simple" no-payload enum.
void deriveCaseIterable_enum_getter(AbstractFunctionDecl *funcDecl) {
  auto *parentDC = funcDecl->getDeclContext();
  auto *parentEnum = parentDC->getAsEnumOrEnumExtensionContext();
  auto enumTy = parentEnum->getDeclaredTypeInContext();
  auto &C = parentDC->getASTContext();

  SmallVector<Expr *, 8> elExprs;
  for (EnumElementDecl *elt : parentEnum->getAllElements()) {
    auto *ref = new (C) DeclRefExpr(elt, DeclNameLoc(), /*implicit*/true);
    auto *base = TypeExpr::createImplicit(enumTy, C);
    auto *apply = new (C) DotSyntaxCallExpr(ref, SourceLoc(), base);
    elExprs.push_back(apply);
  }
  auto *arrayExpr = ArrayExpr::create(C, SourceLoc(), elExprs, {}, SourceLoc());

  auto *returnStmt = new (C) ReturnStmt(SourceLoc(), arrayExpr);
  auto *body = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt),
                                 SourceLoc());
  funcDecl->setBody(body);
}

static ArraySliceType *computeAllCasesType(NominalTypeDecl *enumType) {
  auto metaTy = enumType->getDeclaredInterfaceType();
  if (!metaTy || metaTy->hasError())
    return nullptr;

  return ArraySliceType::get(metaTy->getRValueInstanceType());
}

static Type deriveCaseIterable_AllCases(TypeChecker &tc, Decl *parentDecl,
                                        EnumDecl *enumDecl) {
  // enum SomeEnum : CaseIterable {
  //   @derived
  //   typealias AllCases = [SomeEnum]
  // }
  auto *rawInterfaceType = computeAllCasesType(enumDecl);
  return cast<DeclContext>(parentDecl)->mapTypeIntoContext(rawInterfaceType);
}

ValueDecl *DerivedConformance::deriveCaseIterable(TypeChecker &tc,
                                                  Decl *parentDecl,
                                                  NominalTypeDecl *targetDecl,
                                                  ValueDecl *requirement) {
  // Conformance can't be synthesized in an extension.
  auto caseIterableProto
      = tc.Context.getProtocol(KnownProtocolKind::CaseIterable);
  auto caseIterableType = caseIterableProto->getDeclaredType();
  if (targetDecl != parentDecl) {
    tc.diagnose(parentDecl->getLoc(), diag::cannot_synthesize_in_extension,
                caseIterableType);
    return nullptr;
  }

  // Check that we can actually derive CaseIterable for this type.
  if (!canDeriveConformance(targetDecl))
    return nullptr;

  // Build the necessary decl.
  if (requirement->getBaseName() != tc.Context.Id_allCases) {
    tc.diagnose(requirement->getLoc(),
                diag::broken_case_iterable_requirement);
    return nullptr;
  }

  auto enumDecl = cast<EnumDecl>(targetDecl);
  ASTContext &C = tc.Context;
  

  // Define the property.
  auto *returnTy = computeAllCasesType(targetDecl);

  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl)
    = declareDerivedProperty(tc, parentDecl, enumDecl, C.Id_allCases,
                             returnTy, returnTy,
                             /*isStatic=*/true, /*isFinal=*/true);

  // Define the getter.
  auto *getterDecl = addGetterToReadOnlyDerivedProperty(tc, propDecl, returnTy);

  getterDecl->setBodySynthesizer(&deriveCaseIterable_enum_getter);

  auto dc = cast<IterableDeclContext>(parentDecl);
  dc->addMember(getterDecl);
  dc->addMember(propDecl);
  dc->addMember(pbDecl);

  return propDecl;
}

Type DerivedConformance::deriveCaseIterable(TypeChecker &tc, Decl *parentDecl,
                                            NominalTypeDecl *targetDecl,
                                            AssociatedTypeDecl *assocType) {
  // Conformance can't be synthesized in an extension.
  auto caseIterableProto
      = tc.Context.getProtocol(KnownProtocolKind::CaseIterable);
  auto caseIterableType = caseIterableProto->getDeclaredType();
  if (targetDecl != parentDecl) {
    tc.diagnose(parentDecl->getLoc(), diag::cannot_synthesize_in_extension,
                caseIterableType);
    return nullptr;
  }

  // We can only synthesize CaseIterable for enums.
  auto enumDecl = dyn_cast<EnumDecl>(targetDecl);
  if (!enumDecl)
    return nullptr;

  // Check that we can actually derive CaseIterable for this type.
  if (!canDeriveConformance(targetDecl))
    return nullptr;

  if (assocType->getName() == tc.Context.Id_AllCases) {
    return deriveCaseIterable_AllCases(tc, parentDecl, enumDecl);
  }

  tc.diagnose(assocType->getLoc(),
              diag::broken_case_iterable_requirement);
  return nullptr;
}

