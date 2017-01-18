//===--- ITCType.cpp - Iterative Type Checker for Types -------------------===//
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
//  This file implements the portions of the IterativeTypeChecker
//  class that involve types.
//
//===----------------------------------------------------------------------===//
#include "GenericTypeResolver.h"
#include "TypeChecker.h"
#include "swift/Sema/IterativeTypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Type resolution.
//===----------------------------------------------------------------------===//
bool IterativeTypeChecker::isResolveTypeReprSatisfied(
       std::tuple<TypeRepr *, DeclContext *, unsigned> payload) {
  auto typeRepr = std::get<0>(payload);
  auto options = static_cast<TypeResolutionOptions>(std::get<2>(payload));

  // FIXME: Introduce a bit indicating when everything in the TypeRepr
  // has been name-bound, which indicates that we can always compute a
  // structural type.
  struct FindUnboundTypeRepr : public ASTWalker {
    TypeResolutionOptions Options;

    // Whether we've found an unbound type.
    bool HasUnbound = false;

    FindUnboundTypeRepr(TypeResolutionOptions options) : Options(options) { }

    bool walkToTypeReprPre(TypeRepr *T) override {
      // If we already found an unbound type, we're done.
      if (HasUnbound) return false;

      if (auto ident = dyn_cast<ComponentIdentTypeRepr>(T)) {
        if (!ident->isBound()) {
          HasUnbound = true;
          return false;
        }

        // If we're only looking to resolve the structure of the type,
        // don't walk into generic arguments. They don't affect the
        // structure.
        if (Options.contains(TR_ResolveStructure) &&
            isa<GenericIdentTypeRepr>(ident))
          return false;
      }

      // Keep walking.
      return true;
    }
  } findUnboundTypeRepr(options);

  typeRepr->walk(findUnboundTypeRepr);
  return findUnboundTypeRepr.HasUnbound;
}

void IterativeTypeChecker::processResolveTypeRepr(
       std::tuple<TypeRepr *, DeclContext *, unsigned> payload,
       UnsatisfiedDependency unsatisfiedDependency) {
  auto typeRepr = std::get<0>(payload);
  auto dc = std::get<1>(payload);
  auto options = static_cast<TypeResolutionOptions>(std::get<2>(payload));
  
  // FIXME: TypeChecker::resolveType() is mostly non-ad-hoc-recursive
  // when given an UnsatisfiedDependency.
  TC.resolveType(typeRepr, dc, options, nullptr, &unsatisfiedDependency);
}

bool IterativeTypeChecker::breakCycleForResolveTypeRepr(
       std::tuple<TypeRepr *, DeclContext *, unsigned> payload) {
  std::get<0>(payload)->setInvalid();
  return true;
}

