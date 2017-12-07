//===--- ResilienceDiagnostics.cpp - Resilience Inlineability Diagnostics -===//
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
// This file implements diagnostics for @inlineable.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/DeclContext.h"

using namespace swift;
using FragileFunctionKind = TypeChecker::FragileFunctionKind;

FragileFunctionKind TypeChecker::getFragileFunctionKind(const DeclContext *DC) {
  for (; DC->isLocalContext(); DC = DC->getParent()) {
    if (auto *DAI = dyn_cast<DefaultArgumentInitializer>(DC))
      if (DAI->getResilienceExpansion() == ResilienceExpansion::Minimal)
        return FragileFunctionKind::DefaultArgument;

    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(DC)) {
      // If the function is a nested function, we will serialize its body if
      // we serialize the parent's body.
      if (AFD->getDeclContext()->isLocalContext())
        continue;

      // Bodies of public transparent and always-inline functions are
      // serialized, so use conservative access patterns.
      if (AFD->isTransparent())
        return FragileFunctionKind::Transparent;

      if (AFD->getAttrs().hasAttribute<InlineableAttr>())
        return FragileFunctionKind::Inlineable;

      if (auto attr = AFD->getAttrs().getAttribute<InlineAttr>())
        if (attr->getKind() == InlineKind::Always)
          return FragileFunctionKind::InlineAlways;

      // If a property or subscript is @_inlineable, the accessors are
      // @_inlineable also.
      if (auto FD = dyn_cast<FuncDecl>(AFD))
        if (auto *ASD = FD->getAccessorStorageDecl())
          if (ASD->getAttrs().getAttribute<InlineableAttr>())
            return FragileFunctionKind::Inlineable;
    }
  }

  llvm_unreachable("Context is not nested inside a fragile function");
}

void TypeChecker::diagnoseInlineableLocalType(const NominalTypeDecl *NTD) {
  auto *DC = NTD->getDeclContext();
  auto expansion = DC->getResilienceExpansion();
  if (expansion == ResilienceExpansion::Minimal) {
    diagnose(NTD, diag::local_type_in_inlineable_function,
             NTD->getFullName(),
             static_cast<unsigned>(getFragileFunctionKind(DC)));
  }
}

bool TypeChecker::diagnoseInlineableDeclRef(SourceLoc loc,
                                            const ValueDecl *D,
                                            const DeclContext *DC) {
  auto expansion = DC->getResilienceExpansion();

  // Internal declarations referenced from non-inlineable contexts are OK.
  if (expansion == ResilienceExpansion::Maximal)
    return false;

  // Local declarations are OK.
  if (D->getDeclContext()->isLocalContext())
    return false;

  // Type parameters are OK.
  if (isa<AbstractTypeParamDecl>(D))
    return false;

  // Public declarations are OK.
  if (D->getFormalAccessScope(/*useDC=*/nullptr,
                              /*respectVersionedAttr=*/true).isPublic())
    return false;

  // Enum cases are handled as part of their containing enum.
  if (isa<EnumElementDecl>(D))
    return false;

  // Protocol requirements are not versioned because there's no
  // global entry point.
  if (isa<ProtocolDecl>(D->getDeclContext()) &&
      D->isProtocolRequirement())
    return false;

  // Dynamic declarations are not versioned because there's no
  // global entry point.
  if (D->isDynamic())
    return false;

  // FIXME: Figure out what to do with typealiases
  if (isa<TypeAliasDecl>(D))
    return false;

  diagnose(loc, diag::resilience_decl_unavailable,
           D->getDescriptiveKind(), D->getFullName(),
           D->getFormalAccessScope().accessLevelForDiagnostics(),
           static_cast<unsigned>(getFragileFunctionKind(DC)));

  bool isDefaultArgument = false;
  while (DC->isLocalContext()) {
    if (isa<DefaultArgumentInitializer>(DC)) {
      isDefaultArgument = true;
      break;
    }

    DC = DC->getParent();
  }

  if (isDefaultArgument) {
    diagnose(D, diag::resilience_decl_declared_here,
             D->getDescriptiveKind(), D->getFullName());
  } else {
    diagnose(D, diag::resilience_decl_declared_here_versioned,
             D->getDescriptiveKind(), D->getFullName());
  }

  return true;
}

