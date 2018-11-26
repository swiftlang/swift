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
    TreatUsableFromInlineAsPublic(treatUsableFromInlineAsPublic),
    Context(File->getASTContext()) {}

bool
AccessScopeChecker::visitDecl(ValueDecl *VD) {
  if (!VD || isa<GenericTypeParamDecl>(VD))
    return true;

  auto AS = VD->getFormalAccessScope(File, TreatUsableFromInlineAsPublic);
  Scope = Scope->intersectWith(AS);
  return Scope.hasValue();
}

TypeReprAccessScopeChecker::TypeReprAccessScopeChecker(const DeclContext *useDC,
                                                       bool treatUsableFromInlineAsPublic)
  : AccessScopeChecker(useDC, treatUsableFromInlineAsPublic) {
}

bool
TypeReprAccessScopeChecker::walkToTypeReprPre(TypeRepr *TR) {
  if (auto CITR = dyn_cast<ComponentIdentTypeRepr>(TR))
    return visitDecl(CITR->getBoundDecl());
  return true;
}

bool
TypeReprAccessScopeChecker::walkToTypeReprPost(TypeRepr *TR) {
  return Scope.hasValue();
}

Optional<AccessScope>
TypeReprAccessScopeChecker::getAccessScope(TypeRepr *TR, const DeclContext *useDC,
                                           bool treatUsableFromInlineAsPublic) {
  TypeReprAccessScopeChecker checker(useDC, treatUsableFromInlineAsPublic);
  TR->walk(checker);
  return checker.Scope;
}

TypeAccessScopeChecker::TypeAccessScopeChecker(const DeclContext *useDC,
                                               bool treatUsableFromInlineAsPublic)
  : AccessScopeChecker(useDC, treatUsableFromInlineAsPublic) {}

TypeWalker::Action
TypeAccessScopeChecker::walkToTypePre(Type T) {
  ValueDecl *VD;
  if (auto *BNAD = dyn_cast<NameAliasType>(T.getPointer()))
    VD = BNAD->getDecl();
  else if (auto *NTD = T->getAnyNominal())
    VD = NTD;
  else
    VD = nullptr;

  if (!visitDecl(VD))
    return Action::Stop;

  return Action::Continue;
}

Optional<AccessScope>
TypeAccessScopeChecker::getAccessScope(Type T, const DeclContext *useDC,
                                       bool treatUsableFromInlineAsPublic) {
  TypeAccessScopeChecker checker(useDC, treatUsableFromInlineAsPublic);
  T.walk(checker);
  return checker.Scope;
}
