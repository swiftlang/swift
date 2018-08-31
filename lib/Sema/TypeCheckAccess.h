//===--- TypeCheckAccess.h - Type Checking for Access Control --*- C++ -*-===//
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
// This file implements access control checking.
//
//===----------------------------------------------------------------------===//

#ifndef TYPECHECKACCESS_H
#define TYPECHECKACCESS_H

#include "swift/AST/AccessScope.h"
#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/STLExtras.h"

namespace swift {

class Decl;
class DeclContext;
class GenericParamList;
class RequirementRepr;
class TypeChecker;
class ValueDecl;
struct WhereClauseOwner;

/// A uniquely-typed boolean to reduce the chances of accidentally inverting
/// a check.
///
/// \see checkTypeAccess
enum class DowngradeToWarning: bool {
  No,
  Yes
};

/// \see checkTypeAccess
using CheckTypeAccessCallback =
    void(AccessScope, const TypeRepr *, DowngradeToWarning);

class AccessControlCheckerBase {
protected:
  TypeChecker &TC;
  bool checkUsableFromInline;

  void checkTypeAccessImpl(
      Type type, TypeRepr *typeRepr, AccessScope contextAccessScope,
      const DeclContext *useDC,
      llvm::function_ref<CheckTypeAccessCallback> diagnose);

  void checkTypeAccess(
      Type type, TypeRepr *typeRepr, const ValueDecl *context,
      llvm::function_ref<CheckTypeAccessCallback> diagnose);

  void checkTypeAccess(
      const TypeLoc &TL, const ValueDecl *context,
      llvm::function_ref<CheckTypeAccessCallback> diagnose) {
    return checkTypeAccess(TL.getType(), TL.getTypeRepr(), context, diagnose);
  }

  void checkRequirementAccess(
      WhereClauseOwner source,
      AccessScope accessScope,
      const DeclContext *useDC,
      llvm::function_ref<CheckTypeAccessCallback> diagnose);

  AccessControlCheckerBase(TypeChecker &TC, bool checkUsableFromInline)
    : TC(TC), checkUsableFromInline(checkUsableFromInline) {}

public:
  void checkGenericParamAccess(
    const GenericParamList *params,
    const Decl *owner,
    AccessScope accessScope,
    AccessLevel contextAccess);

  void checkGenericParamAccess(
    const GenericParamList *params,
    const ValueDecl *owner);
};

class AccessControlChecker : public AccessControlCheckerBase {
public:
  explicit AccessControlChecker(TypeChecker &TC)
    : AccessControlCheckerBase(TC, /*checkUsableFromInline=*/false) {}

  void check(Decl *D);

  static void checkAccessControl(TypeChecker &TC, Decl *D) {
    AccessControlChecker(TC).check(D);
  }
};

class UsableFromInlineChecker : public AccessControlCheckerBase {
public:
  explicit UsableFromInlineChecker(TypeChecker &TC)
    : AccessControlCheckerBase(TC, /*checkUsableFromInline=*/true) {}

  void check(Decl *D);

  static void checkUsableFromInline(TypeChecker &TC, Decl *D) {
    UsableFromInlineChecker(TC).check(D);
  }
};

} // end namespace swift

#endif
