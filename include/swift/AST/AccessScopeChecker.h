//===--- AccessScopeChecker.h - Access calculation helpers -----*- C++ -*-===//
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
//  This file defines helpers for access-control calculation.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_ACCESS_SCOPE_CHECKER_H
#define SWIFT_ACCESS_SCOPE_CHECKER_H

#include "swift/AST/AccessScope.h"
#include "swift/AST/AttrKind.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/TypeWalker.h"

namespace swift {

class AbstractStorageDecl;
class ExtensionDecl;
class SourceFile;
class ValueDecl;

class AccessScopeChecker {
  const SourceFile *File;
  bool TreatUsableFromInlineAsPublic;

protected:
  ASTContext &Context;
  Optional<AccessScope> Scope = AccessScope::getPublic();

  AccessScopeChecker(const DeclContext *useDC,
                     bool treatUsableFromInlineAsPublic);
  bool visitDecl(ValueDecl *VD);
};

class TypeReprAccessScopeChecker : private ASTWalker, AccessScopeChecker {
  TypeReprAccessScopeChecker(const DeclContext *useDC,
                             bool treatUsableFromInlineAsPublic);

  bool walkToTypeReprPre(TypeRepr *TR) override;
  bool walkToTypeReprPost(TypeRepr *TR) override;

public:
  static Optional<AccessScope>
  getAccessScope(TypeRepr *TR, const DeclContext *useDC,
                 bool treatUsableFromInlineAsPublic = false);
};

class TypeAccessScopeChecker : private TypeWalker, AccessScopeChecker {
  TypeAccessScopeChecker(const DeclContext *useDC,
                         bool treatUsableFromInlineAsPublic);

  Action walkToTypePre(Type T);

public:
  static Optional<AccessScope>
  getAccessScope(Type T, const DeclContext *useDC,
                 bool treatUsableFromInlineAsPublic = false);
};

}

#endif
