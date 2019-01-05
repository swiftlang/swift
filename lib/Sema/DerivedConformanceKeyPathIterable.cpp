//===--- DerivedConformanceKeyPathIterable.cpp ----------------------------===//
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
// This file implements explicit derivation of the KeyPathIterable protocol for
// a nominal type.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "DerivedConformances.h"

using namespace swift;

bool DerivedConformance::canDeriveKeyPathIterable(NominalTypeDecl *nominal) {
  // Note: we could extend synthesis to support classes.
  // Subclasses need to append `allKeyPaths` to `super.allKeyPaths`.
  return isa<StructDecl>(nominal);
}

// Compute `PartialKeyPathType<Nominal>`, bound to the given nominal
// declaration's type.
static Type computePartialKeyPathType(NominalTypeDecl *nominal) {
  auto &C = nominal->getASTContext();
  auto nominalType = nominal->getDeclaredInterfaceType();
  if (!nominalType || nominalType->hasError())
    return nullptr;
  auto *partialKeyPathDecl = cast<ClassDecl>(C.getPartialKeyPathDecl());
  return BoundGenericClassType::get(partialKeyPathDecl, /*parent*/ Type(),
                                    {nominal->getDeclaredInterfaceType()});
}

// Compute `AllKeyPaths` associated type for the given nominal declaration.
// It should be `[PartialKeyPath<Nominal>]`.
static ArraySliceType *computeAllKeyPathsType(NominalTypeDecl *nominal) {
  auto partialKeyPathType = computePartialKeyPathType(nominal);
  return ArraySliceType::get(partialKeyPathType);
}

// Synthesize body for the `allKeyPaths` computed property getter.
static void
deriveBodyKeyPathIterable_allKeyPaths(AbstractFunctionDecl *funcDecl) {
  auto *nominal = funcDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  auto *nominalTypeExpr = TypeExpr::createForDecl(SourceLoc(), nominal,
                                                  funcDecl, /*Implicit*/ true);

  // Create array of key path expressions to stored properties.
  llvm::SmallVector<Expr *, 2> keyPathExprs;
  for (auto member : nominal->getStoredProperties()) {
    auto *dotExpr = new (C)
        UnresolvedDotExpr(nominalTypeExpr, SourceLoc(), member->getFullName(),
                          DeclNameLoc(), /*Implicit*/ true);
    auto *keyPathExpr =
        new (C) KeyPathExpr(SourceLoc(), dotExpr, nullptr, /*Implicit*/ true);
    keyPathExprs.push_back(keyPathExpr);
  }
  // Return array of all key path expressions.
  auto keyPathsArrayExpr =
      ArrayExpr::create(C, SourceLoc(), keyPathExprs, {}, SourceLoc());
  auto *returnStmt = new (C) ReturnStmt(SourceLoc(), keyPathsArrayExpr);
  auto *body = BraceStmt::create(C, SourceLoc(), {returnStmt}, SourceLoc(),
                                 /*Implicit*/ true);
  funcDecl->setBody(BraceStmt::create(C, SourceLoc(), {body}, SourceLoc(),
                                      /*Implicit*/ true));
}

// Synthesize the `allKeyPaths` computed property declaration.
static ValueDecl *
deriveKeyPathIterable_allKeyPaths(DerivedConformance &derived) {
  auto nominal = derived.Nominal;
  auto &C = derived.TC.Context;

  auto returnInterfaceTy = computeAllKeyPathsType(nominal);
  auto returnTy =
      derived.getConformanceContext()->mapTypeIntoContext(returnInterfaceTy);

  // Create `allKeyPaths` property declaration.
  VarDecl *allKeyPathsDecl;
  PatternBindingDecl *pbDecl;
  std::tie(allKeyPathsDecl, pbDecl) = derived.declareDerivedProperty(
      C.Id_allKeyPaths, returnInterfaceTy, returnTy, /*isStatic*/ false,
      /*isFinal*/ true);

  // Add `@inlinable` to the `allKeyPaths` declaration.
  if (nominal->getEffectiveAccess() > AccessLevel::Internal)
    allKeyPathsDecl->getAttrs().add(new (C) InlinableAttr(/*implicit*/ true));

  // Create `allKeyPaths` getter.
  auto *getterDecl = derived.declareDerivedPropertyGetter(
      derived.TC, allKeyPathsDecl, returnTy);
  getterDecl->setBodySynthesizer(deriveBodyKeyPathIterable_allKeyPaths);
  allKeyPathsDecl->setAccessors(StorageImplInfo::getImmutableComputed(),
                                SourceLoc(), {getterDecl}, SourceLoc());
  derived.addMembersToConformanceContext({getterDecl, allKeyPathsDecl, pbDecl});

  return allKeyPathsDecl;
}

static Type deriveKeyPathIterable_AllKeyPaths(DerivedConformance &derived) {
  auto *rawInterfaceType = computeAllKeyPathsType(derived.Nominal);
  return derived.getConformanceContext()->mapTypeIntoContext(rawInterfaceType);
}

ValueDecl *DerivedConformance::deriveKeyPathIterable(ValueDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.Id_allKeyPaths) {
    return deriveKeyPathIterable_allKeyPaths(*this);
  }
  TC.diagnose(requirement->getLoc(),
              diag::broken_key_path_iterable_requirement);
  return nullptr;
}

Type DerivedConformance::deriveKeyPathIterable(
    AssociatedTypeDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.Id_AllKeyPaths) {
    return deriveKeyPathIterable_AllKeyPaths(*this);
  }
  TC.diagnose(requirement->getLoc(),
              diag::broken_key_path_iterable_requirement);
  return nullptr;
}
