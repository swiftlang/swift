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
class TypeChecker;
class ValueDecl;
class RequirementRepr;

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

class AccessControlChecker {
  TypeChecker &TC;

  void checkTypeAccessImpl(
      TypeLoc TL, AccessScope contextAccessScope,
      const DeclContext *useDC, bool checkUsableFromInline,
      llvm::function_ref<CheckTypeAccessCallback> diagnose);

  void checkTypeAccess(
      TypeLoc TL, const ValueDecl *context,
      bool checkUsableFromInline,
      llvm::function_ref<CheckTypeAccessCallback> diagnose);

  void checkRequirementAccess(
      ArrayRef<RequirementRepr> requirements,
      AccessScope accessScope,
      const DeclContext *useDC,
      bool checkUsableFromInline,
      llvm::function_ref<CheckTypeAccessCallback> diagnose);

public:
  explicit AccessControlChecker(TypeChecker &TC) : TC(TC) {}

  void checkAccessControl(Decl *D);
  void checkUsableFromInline(Decl *D);

  void checkGenericParamAccess(
    const GenericParamList *params,
    const Decl *owner,
    bool checkUsableFromInline,
    AccessScope accessScope,
    AccessLevel contextAccess);

  void checkGenericParamAccess(
    const GenericParamList *params,
    const ValueDecl *owner,
    bool checkUsableFromInline);
};

} // end namespace swift

#endif
