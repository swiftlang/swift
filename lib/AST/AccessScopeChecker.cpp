//===--- AccessScopeChecker.cpp - Access calculation helpers ----- --------===//
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

#include "swift/AST/AccessScopeChecker.h"
#include "swift/AST/Module.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/Types.h"

using namespace swift;

//----------------------------------------------------------------------------//
// Access Scope Checkers
//----------------------------------------------------------------------------//

AccessScopeChecker::AccessScopeChecker(const DeclContext *useDC,
                                       bool treatUsableFromInlineAsPublic)
  : File(useDC->getParentSourceFile()),
    TreatUsableFromInlineAsPublic(treatUsableFromInlineAsPublic) {}

bool
AccessScopeChecker::visitDecl(const ValueDecl *VD) {
  if (isa<GenericTypeParamDecl>(VD))
    return true;

  auto AS = VD->getFormalAccessScope(File, TreatUsableFromInlineAsPublic);
  Scope = Scope->intersectWith(AS);
  return Scope.hasValue();
}

bool TypeReprIdentFinder::walkToTypeReprPost(TypeRepr *TR) {
  auto CITR = dyn_cast<ComponentIdentTypeRepr>(TR);
  if (!CITR || !CITR->getBoundDecl())
    return true;
  return Callback(CITR);
}

Optional<AccessScope>
AccessScopeChecker::getAccessScope(TypeRepr *TR, const DeclContext *useDC,
                                   bool treatUsableFromInlineAsPublic) {
  AccessScopeChecker checker(useDC, treatUsableFromInlineAsPublic);
  TR->walk(TypeReprIdentFinder([&](const ComponentIdentTypeRepr *typeRepr) {
    return checker.visitDecl(typeRepr->getBoundDecl());
  }));
  return checker.Scope;
}

TypeWalker::Action TypeDeclFinder::walkToTypePre(Type T) {
  if (auto *TAT = dyn_cast<TypeAliasType>(T.getPointer()))
    return visitTypeAliasType(TAT);

  // FIXME: We're looking through sugar here so that we visit, e.g.,
  // Swift.Array when we see `[Int]`. But that means we do redundant work when
  // we see sugar that's purely structural, like `(Int)`. Fortunately, paren
  // types are the only such purely structural sugar at the time this comment
  // was written, and they're not so common in the first place.
  if (auto *BGT = T->getAs<BoundGenericType>())
    return visitBoundGenericType(BGT);
  if (auto *NT = T->getAs<NominalType>())
    return visitNominalType(NT);

  return Action::Continue;
}

TypeWalker::Action
SimpleTypeDeclFinder::visitNominalType(NominalType *ty) {
  return Callback(ty->getDecl());
}

TypeWalker::Action
SimpleTypeDeclFinder::visitBoundGenericType(BoundGenericType *ty) {
  return Callback(ty->getDecl());
}

TypeWalker::Action
SimpleTypeDeclFinder::visitTypeAliasType(TypeAliasType *ty) {
  return Callback(ty->getDecl());
}


Optional<AccessScope>
AccessScopeChecker::getAccessScope(Type T, const DeclContext *useDC,
                                   bool treatUsableFromInlineAsPublic) {
  AccessScopeChecker checker(useDC, treatUsableFromInlineAsPublic);
  T.walk(SimpleTypeDeclFinder([&](const ValueDecl *VD) {
    if (checker.visitDecl(VD))
      return TypeWalker::Action::Continue;
    return TypeWalker::Action::Stop;
  }));
  return checker.Scope;
}
