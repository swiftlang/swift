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

// Mark the given `ValueDecl` as `@inlinable`, if the conformance context's
// module is not resilient and the `ValueDecl` is effectively public.
// TODO: Dedupe with DerivedConformanceRawRepresentable.cpp.
static void maybeMarkAsInlinable(DerivedConformance &derived, ValueDecl *decl) {
  ASTContext &C = derived.TC.Context;
  auto parentDC = derived.getConformanceContext();
  if (!parentDC->getParentModule()->isResilient()) {
    auto access = decl->getFormalAccessScope(
        nullptr, /*treatUsableFromInlineAsPublic*/ true);
    if (access.isPublic()) {
      decl->getAttrs().add(new (C) InlinableAttr(/*implicit*/ false));
      if (auto *attr = decl->getAttrs().getAttribute<UsableFromInlineAttr>())
        attr->setInvalid();
    }
  }
}

// Synthesize body for the `allKeyPaths` computed property getter.
static void
deriveBodyKeyPathIterable_allKeyPaths(AbstractFunctionDecl *funcDecl, void *) {
  auto *parentDC = funcDecl->getDeclContext();
  auto *nominal = parentDC->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();
  auto allKeyPathsInterfaceType = computeAllKeyPathsType(nominal);
  auto allKeyPathsType = parentDC->mapTypeIntoContext(allKeyPathsInterfaceType);

  auto *nominalTypeExpr = TypeExpr::createForDecl(SourceLoc(), nominal,
                                                  funcDecl, /*Implicit*/ true);

  // Create array of key path expressions to stored properties.
  llvm::SmallVector<Expr *, 2> keyPathExprs;
  for (auto member : nominal->getStoredProperties()) {
    // FIXME(TF-123): Skip generating keypaths to `@differentiable` functions
    // because of SILGen crash. Robust fix involves changing
    // `createAutoDiffThunk`.
    if (auto fnType = member->getType()->getAs<AnyFunctionType>())
      if (fnType->getExtInfo().isDifferentiable())
        continue;

    auto *dotExpr = new (C)
        UnresolvedDotExpr(nominalTypeExpr, SourceLoc(), member->getFullName(),
                          DeclNameLoc(), /*Implicit*/ true);
    auto *keyPathExpr =
        new (C) KeyPathExpr(SourceLoc(), dotExpr, nullptr, /*Implicit*/ true);
    keyPathExprs.push_back(keyPathExpr);
  }
  // Return array of all key path expressions.
  Expr *keyPathsArrayExpr =
      ArrayExpr::create(C, SourceLoc(), keyPathExprs, {}, SourceLoc());
  // NOTE(TF-575): Adding an explicit coercion expression here is necessary due
  // to a missing regression.
  keyPathsArrayExpr = new (C) CoerceExpr(
      keyPathsArrayExpr, SourceLoc(), TypeLoc::withoutLoc(allKeyPathsType));
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

  // Maybe add `@inlinable` to the `allKeyPaths` declaration.
  if (llvm::all_of(nominal->getStoredProperties(), [](VarDecl *vd) {
    return vd->getFormalAccessScope(
        nullptr, /*treatUsableFromInlineAsPublic*/ true).isPublic();
  })) {
    maybeMarkAsInlinable(derived, allKeyPathsDecl);
  }

  // Create `allKeyPaths` getter.
  auto *getterDecl = derived.declareDerivedPropertyGetter(
      derived.TC, allKeyPathsDecl, returnTy);
  getterDecl->setBodySynthesizer(
      deriveBodyKeyPathIterable_allKeyPaths, nullptr);
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
  // Diagnose conformances in disallowed contexts.
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  if (requirement->getBaseName() == TC.Context.Id_allKeyPaths)
    return deriveKeyPathIterable_allKeyPaths(*this);
  TC.diagnose(requirement->getLoc(),
              diag::broken_key_path_iterable_requirement);
  return nullptr;
}

Type DerivedConformance::deriveKeyPathIterable(
    AssociatedTypeDecl *requirement) {
  // Diagnose conformances in disallowed contexts.
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  if (requirement->getBaseName() == TC.Context.Id_AllKeyPaths)
    return deriveKeyPathIterable_AllKeyPaths(*this);
  TC.diagnose(requirement->getLoc(),
              diag::broken_key_path_iterable_requirement);
  return nullptr;
}
