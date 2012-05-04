//===--- Attr.cpp - Swift Language Attr ASTs ------------------------------===//
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
//  This file implements routines relating to declaration attributes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Attr.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Component.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"

using namespace swift;

/// A statically-allocated empty set of attributes.
const DeclAttributes ValueDecl::EmptyAttrs;

DeclAttributes &ValueDecl::getMutableAttrs() {
  // If we don't have mutable attribute storage yet, allocate some.
  if (Attrs == &EmptyAttrs)
    Attrs = getASTContext().Allocate<DeclAttributes>(1);
  return *const_cast<DeclAttributes*>(Attrs);
}

/// getResilienceFrom - Find the resilience of this declaration from
/// the given component.
Resilience ValueDecl::getResilienceFrom(Component *C) const {
  const Resilience invalidResilience = Resilience(-1);
  Resilience explicitResilience = invalidResilience;

  DeclContext *DC = getDeclContext();
  while (true) {
    ValueDecl *D = nullptr;
    switch (DC->getContextKind()) {
    // All the builtin declarations are inherently fragile.
    case DeclContextKind::BuiltinModule:
      return Resilience::InherentlyFragile;

    // Global declarations are resilient according to whether the module
    // is resilient in this translation unit.
    case DeclContextKind::TranslationUnit:
      if (explicitResilience != invalidResilience)
        return explicitResilience;
      return C->isResilient(cast<TranslationUnit>(DC))
                  ? Resilience::Resilient : Resilience::Fragile;

    // Local declarations are always inherently fragile.
    case DeclContextKind::CapturingExpr:
    case DeclContextKind::TopLevelCodeDecl:
      return Resilience::InherentlyFragile;

    // For oneofs, we walk out through the oneof decl.
    case DeclContextKind::OneOfType:
      D = cast<OneOfType>(DC)->getDecl();
      goto HandleDecl;

    case DeclContextKind::ExtensionDecl:
      // FIXME: we can't use the ExtensionDecl, it has no attrs.
      return Resilience::Fragile;

    case DeclContextKind::ProtocolType:
      // FIXME: no attrs here, either.
      return Resilience::Fragile;
    }
    llvm_unreachable("bad decl context kind!");

  HandleDecl:
    if (explicitResilience == invalidResilience) {
      ResilienceData resil = D->getAttrs().getResilienceData();
      if (resil.isValid())
        explicitResilience = resil.getResilience();
    }
    DC = D->getDeclContext();
  }
}
