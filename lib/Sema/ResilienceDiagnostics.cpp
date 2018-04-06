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
// This file implements diagnostics for @inlinable.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/DeclContext.h"

using namespace swift;
using FragileFunctionKind = TypeChecker::FragileFunctionKind;

std::pair<FragileFunctionKind, bool>
TypeChecker::getFragileFunctionKind(const DeclContext *DC) {
  for (; DC->isLocalContext(); DC = DC->getParent()) {
    if (isa<DefaultArgumentInitializer>(DC)) {
      // Default argument generators of public functions cannot reference
      // @usableFromInline declarations; all other fragile function kinds
      // can.
      auto *VD = cast<ValueDecl>(DC->getInnermostDeclarationDeclContext());
      auto access =
        VD->getFormalAccessScope(/*useDC=*/nullptr,
                                 /*treatUsableFromInlineAsPublic=*/false);
      return std::make_pair(FragileFunctionKind::DefaultArgument,
                            !access.isPublic());
    }

    if (isa<PatternBindingInitializer>(DC))
      return std::make_pair(FragileFunctionKind::PropertyInitializer,
                            /*treatUsableFromInlineAsPublic=*/true);

    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(DC)) {
      // If the function is a nested function, we will serialize its body if
      // we serialize the parent's body.
      if (AFD->getDeclContext()->isLocalContext())
        continue;

      // Bodies of public transparent and always-inline functions are
      // serialized, so use conservative access patterns.
      if (AFD->isTransparent())
        return std::make_pair(FragileFunctionKind::Transparent,
                              /*treatUsableFromInlineAsPublic=*/true);

      if (AFD->getAttrs().hasAttribute<InlinableAttr>())
        return std::make_pair(FragileFunctionKind::Inlinable,
                              /*treatUsableFromInlineAsPublic=*/true);

      if (auto attr = AFD->getAttrs().getAttribute<InlineAttr>())
        if (attr->getKind() == InlineKind::Always)
          return std::make_pair(FragileFunctionKind::InlineAlways,
                                /*treatUsableFromInlineAsPublic=*/true);

      // If a property or subscript is @inlinable, the accessors are
      // @inlinable also.
      if (auto accessor = dyn_cast<AccessorDecl>(AFD))
        if (accessor->getStorage()->getAttrs().getAttribute<InlinableAttr>())
          return std::make_pair(FragileFunctionKind::Inlinable,
                                /*treatUsableFromInlineAsPublic=*/true);
    }
  }

  llvm_unreachable("Context is not nested inside a fragile function");
}

void TypeChecker::diagnoseInlinableLocalType(const NominalTypeDecl *NTD) {
  auto *DC = NTD->getDeclContext();
  auto expansion = DC->getResilienceExpansion();
  if (expansion == ResilienceExpansion::Minimal) {
    auto kind = getFragileFunctionKind(DC);
    diagnose(NTD, diag::local_type_in_inlinable_function,
             NTD->getFullName(),
             static_cast<unsigned>(kind.first));
  }
}

bool TypeChecker::diagnoseInlinableDeclRef(SourceLoc loc,
                                           const ValueDecl *D,
                                           const DeclContext *DC,
                                           FragileFunctionKind Kind,
                                           bool TreatUsableFromInlineAsPublic) {
  // Local declarations are OK.
  if (D->getDeclContext()->isLocalContext())
    return false;

  // Type parameters are OK.
  if (isa<AbstractTypeParamDecl>(D))
    return false;

  // Public declarations are OK.
  if (D->getFormalAccessScope(/*useDC=*/nullptr,
                              TreatUsableFromInlineAsPublic).isPublic())
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
           static_cast<unsigned>(Kind));

  if (TreatUsableFromInlineAsPublic) {
    diagnose(D, diag::resilience_decl_declared_here,
             D->getDescriptiveKind(), D->getFullName());
  } else {
    diagnose(D, diag::resilience_decl_declared_here_public,
             D->getDescriptiveKind(), D->getFullName());
  }

  return true;
}

