//===--- ITCNameLookup.cpp - Iterative Type Checker Name Lookup -----------===//
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
//  class that involve name lookup.
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
#include "swift/Sema/IterativeTypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include <tuple>
using namespace swift;

//----------------------------------------------------------------------------//
// Qualified name lookup handling
//----------------------------------------------------------------------------//
bool IterativeTypeChecker::isQualifiedLookupInTypeSatisfied(
       TypeCheckRequest::TypeLookupPayloadType payload) {
  auto type = payload.first;

  // One can always perform name lookup into a tuple type.
  if (type->is<TupleType>()) return true;

  // Module lookup is lookup within the module's DeclContext.
  if (auto moduleType = type->getAs<ModuleType>())
    return isSatisfied(TypeCheckRequest(
                         TypeCheckRequest::QualifiedLookupInDeclContext,
                         { moduleType->getModule(), payload.second }));

  // Nominal type lookup is lookup within the module's DeclContext.
  if (auto nominal = type->getAnyNominal())
    return isSatisfied(TypeCheckRequest(
                         TypeCheckRequest::QualifiedLookupInDeclContext,
                         { nominal, payload.second }));

  // For everything else, we can perform lookup.
  return true;
}

void IterativeTypeChecker::processQualifiedLookupInType(
       TypeCheckRequest::TypeLookupPayloadType payload,
       UnsatisfiedDependency unsatisfiedDependency) {
  // Everything is handled by the dependencies.
}

bool IterativeTypeChecker::isQualifiedLookupInDeclContextSatisfied(
       TypeCheckRequest::DeclContextLookupPayloadType payload) {
  auto dc = payload.first;

  NominalTypeDecl *nominal = nullptr;
  switch (dc->getContextKind()) {
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::Initializer:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::SerializedLocal:
    llvm_unreachable("not a DeclContext that supports name lookup");

  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
    // Modules and file units can always handle name lookup.
    return true;

  case DeclContextKind::NominalTypeDecl:
    // Get the nominal type.
    nominal = cast<NominalTypeDecl>(dc);
    break;

  case DeclContextKind::ExtensionDecl: {
    auto ext = cast<ExtensionDecl>(dc);
    // FIXME: bind the extension. We currently assume this is done.
    nominal = ext->isNominalTypeOrNominalTypeExtensionContext();
    if (!nominal) return true;
    break;
  }
  }

  assert(nominal && "Only nominal types are handled here");
  // FIXME: Cache a bit indicating when qualified lookup is possible.
  return false;
}

void IterativeTypeChecker::processQualifiedLookupInDeclContext(
       TypeCheckRequest::DeclContextLookupPayloadType payload,
       UnsatisfiedDependency unsatisfiedDependency) {
  auto nominal = payload.first->isNominalTypeOrNominalTypeExtensionContext();
  assert(nominal && "Only nominal types are handled here");

  // For classes, we need the superclass (if any) to support qualified lookup.
  if (auto classDecl = dyn_cast<ClassDecl>(nominal)) {
    if (unsatisfiedDependency(TypeCheckRequest(
                                TypeCheckRequest::TypeCheckSuperclass,
                                classDecl)))
      return;

    if (auto superclass = classDecl->getSuperclass()) {
      if (unsatisfiedDependency(TypeCheckRequest(
                                  TypeCheckRequest::QualifiedLookupInType,
                                  { superclass, payload.second })))
        return;
    }
  }

  // FIXME: we need to resolve the set of protocol conformances to do
  // unqualified lookup.

  // If we're looking for all names or for an initializer, resolve
  // implicitly-declared initializers.
  // FIXME: Recursion into old type checker.
  auto name = payload.second;
  if (!name || name.matchesRef(TC.Context.Id_init))
    TC.resolveImplicitConstructors(nominal);
}

//----------------------------------------------------------------------------//
// Qualified name lookup handling
//----------------------------------------------------------------------------//
bool IterativeTypeChecker::isUnqualifiedLookupInDeclContextSatisfied(
       TypeCheckRequest::DeclContextLookupPayloadType payload) {
  auto dc = payload.first;
  switch (dc->getContextKind()) {
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::Initializer:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::SerializedLocal:
    // FIXME: Actually do the lookup in these contexts, because if we find
    // something, we don't need to continue outward.
    return isUnqualifiedLookupInDeclContextSatisfied({dc->getParent(),
                                                      payload.second});

  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
    // Modules and file units can always handle name lookup.
    return true;

  case DeclContextKind::NominalTypeDecl:
  case DeclContextKind::ExtensionDecl:
    // Check whether we can perform qualified lookup into this
    // declaration context.
    if (!isSatisfied(TypeCheckRequest(
                       TypeCheckRequest::QualifiedLookupInDeclContext,
                       { dc, payload.second })))
      return false;
      
    // FIXME: If there is a name, actually perform qualified lookup
    // into this DeclContext. If it succeeds, there's nothing more to
    // do.
    return isUnqualifiedLookupInDeclContextSatisfied({dc->getParent(),
                                                      payload.second});
  }
}

void IterativeTypeChecker::processUnqualifiedLookupInDeclContext(
       TypeCheckRequest::DeclContextLookupPayloadType payload,
       UnsatisfiedDependency unsatisfiedDependency) {
  // Everything is handled by the dependencies.
}

