//===--- ITCDecl.cpp - Iterative Type Checker for Declarations ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the portions of the IterativeTypeChecker
//  class that involve declarations.
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
#include "swift/Sema/IterativeTypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
using namespace swift;

//----------------------------------------------------------------------------//
// Superclass handling
//----------------------------------------------------------------------------//
bool IterativeTypeChecker::isTypeCheckSuperclassSatisfied(ClassDecl *payload) {
  return payload->LazySemanticInfo.Superclass.getInt();
}

void IterativeTypeChecker::enumerateDependenciesOfTypeCheckSuperclass(
       ClassDecl *payload,
       llvm::function_ref<void(TypeCheckRequest)>) {
  // FIXME: depends on type checking the first thing in the
  // 'inherited' list.
}

void IterativeTypeChecker::satisfyTypeCheckSuperclass(ClassDecl *classDecl) {
  // FIXME: Declaration validation is overkill in many cases.
  TC.validateDecl(classDecl);
  
  // FIXME: We don't need to resolve the entire inheritance clause. Rather, we
  // need only resolve TypeReprs in the inherited() list until we find a class.
  TC.resolveInheritanceClause(classDecl);

  // Loop through the inheritance clause looking for a class type.
  Type superclassType;
  for (const auto &inherited : classDecl->getInherited()) {
    if (inherited.getType()->getClassOrBoundGenericClass()) {
      superclassType = inherited.getType();
      break;
    }
  }

  // Set the superclass type.
  classDecl->setSuperclass(superclassType);
}
