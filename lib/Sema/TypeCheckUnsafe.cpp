//===--- TypeCheckUnsafe.cpp - Unsafe Diagnostics -------------------------===//
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
// This file implements unsafe diagnostics.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/UnsafeUse.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/SourceFileExtras.h"
#include "TypeCheckAvailability.h"
#include "TypeCheckType.h"

using namespace swift;

static std::pair<const Decl *, bool /*inDefinition*/>
enclosingContextForUnsafe(const UnsafeUse &use) {
  return swift::enclosingContextForUnsafe(use.getLocation(),
                                          use.getDeclContext());
}

/// Whether this particular unsafe use occurs within the definition of an
/// entity, but not in its signature, meaning that the unsafety could be
/// encapsulated.
static bool isUnsafeUseInDefinition(const UnsafeUse &use) {
  switch (use.getKind()) {
  case UnsafeUse::Override:
  case UnsafeUse::Witness:
  case UnsafeUse::TypeWitness:
  case UnsafeUse::UnsafeConformance:
  case UnsafeUse::UnownedUnsafe:
  case UnsafeUse::NonisolatedUnsafe:
    // Never part of the definition. These are always part of the interface.
    return false;

  case UnsafeUse::ReferenceToUnsafe:
  case UnsafeUse::CallToUnsafe:
    return enclosingContextForUnsafe(use).second;
  }
}


static void suggestUnsafeOnEnclosingDecl(
    SourceLoc referenceLoc, const DeclContext *referenceDC) {
  const Decl *decl = nullptr;
  bool inDefinition = false;
  std::tie(decl, inDefinition) = enclosingContextForUnsafe(
      referenceLoc, referenceDC);
  if (!decl)
    return;

  if (inDefinition) {
    // The unsafe construct is inside the body of the entity, so suggest
    // @safe(unchecked) on the declaration.
    decl->diagnose(diag::encapsulate_unsafe_in_enclosing_context, decl)
      .fixItInsert(decl->getAttributeInsertionLoc(false),
                   "@safe(unchecked) ");
  } else {
    // The unsafe construct is not part of the body, so
    decl->diagnose(diag::make_enclosing_context_unsafe, decl)
      .fixItInsert(decl->getAttributeInsertionLoc(false), "@unsafe ");
  }
}

static void suggestUnsafeMarkerOnConformance(
    NormalProtocolConformance *conformance) {
  auto dc = conformance->getDeclContext();
  auto decl = dc->getAsDecl();
  if (!decl)
    return;

  decl->diagnose(
      diag::make_conforming_context_unsafe,
      decl->getDescriptiveKind(),
      conformance->getProtocol()->getName()
  ).fixItInsert(decl->getAttributeInsertionLoc(false), "@unsafe ");
}

static bool shouldDeferUnsafeUseIn(const Decl *decl) {
  if (auto func = dyn_cast_or_null<AbstractFunctionDecl>(decl)) {
    if (func->hasBody()) {
      return true;
    }
  }

  return false;
}

/// Retrieve the extra information
static SourceFileExtras &getSourceFileExtrasFor(const Decl *decl) {
  auto dc = decl->getDeclContext();
  return dc->getOutermostParentSourceFile()->getExtras();
}

void swift::diagnoseUnsafeUse(const UnsafeUse &use, bool asNote) {
  if (!asNote) {
    // If we can associate this unsafe use within a particular declaration, do so.
    // It will be diagnosed later, along with all other unsafe uses within this
    // same declaration.
    if (use.getKind() == UnsafeUse::ReferenceToUnsafe ||
        use.getKind() == UnsafeUse::CallToUnsafe) {
      auto [enclosingDecl, _] = enclosingContextForUnsafe(
          use.getLocation(), use.getDeclContext());
      if (enclosingDecl && shouldDeferUnsafeUseIn(enclosingDecl)) {
        getSourceFileExtrasFor(enclosingDecl).unsafeUses[enclosingDecl]
          .push_back(use);
        return;
      }
    }
  }

  switch (use.getKind()) {
  case UnsafeUse::Override: {
    auto override = use.getDecl();
    override->diagnose(
        diag::override_safe_withunsafe, override->getDescriptiveKind());
    if (auto overridingClass = override->getDeclContext()->getSelfClassDecl()) {
      overridingClass->diagnose(
          diag::make_subclass_unsafe, overridingClass->getName()
      ).fixItInsert(
          overridingClass->getAttributeInsertionLoc(false), "@unsafe ");
    }
    use.getOriginalDecl()->diagnose(diag::overridden_here);
    return;
  }

  case UnsafeUse::Witness: {
    auto witness = cast<ValueDecl>(use.getDecl());
    witness->diagnose(diag::witness_unsafe,
                      witness->getDescriptiveKind(),
                      witness->getName());
    suggestUnsafeMarkerOnConformance(use.getConformance());
    return;
  }

  case UnsafeUse::TypeWitness: {
    auto assocType = use.getAssociatedType();
    auto loc = use.getLocation();
    auto type = use.getType();
    auto conformance = use.getConformance();
    ASTContext &ctx = assocType->getASTContext();

    diagnoseUnsafeType(ctx, loc, type, conformance->getDeclContext(),
                       [&](Type specificType) {
      ctx.Diags.diagnose(
          loc, diag::type_witness_unsafe, specificType, assocType->getName());
    });
    suggestUnsafeMarkerOnConformance(conformance);
    assocType->diagnose(diag::decl_declared_here, assocType);

    return;
  }

  case UnsafeUse::UnsafeConformance: {
    auto conformance = use.getConformance();
    ASTContext &ctx = conformance->getProtocol()->getASTContext();
    ctx.Diags.diagnose(
        use.getLocation(), diag::unchecked_conformance_is_unsafe);
    suggestUnsafeMarkerOnConformance(conformance);
    return;
  }

  case UnsafeUse::UnownedUnsafe: {
    auto var = cast<VarDecl>(use.getDecl());
    var->diagnose(diag::unowned_unsafe_is_unsafe);
    var->diagnose(diag::make_enclosing_context_unsafe, var)
      .fixItInsert(var->getAttributeInsertionLoc(false), "@unsafe ");
    return;
  }

  case UnsafeUse::NonisolatedUnsafe: {
    auto decl = use.getDecl();
    ASTContext &ctx = decl->getASTContext();
    ctx.Diags.diagnose(use.getLocation(), diag::nonisolated_unsafe_is_unsafe);
    if (auto var = dyn_cast<VarDecl>(decl)) {
      var->diagnose(diag::make_enclosing_context_unsafe, var)
        .fixItInsert(var->getAttributeInsertionLoc(false), "@unsafe ");
    }
    return;
  }

  case UnsafeUse::ReferenceToUnsafe:
  case UnsafeUse::CallToUnsafe: {
    bool isCall = use.getKind() == UnsafeUse::CallToUnsafe;
    auto decl = cast<ValueDecl>(use.getDecl());
    auto loc = use.getLocation();
    Type type = use.getType();
    ASTContext &ctx = decl->getASTContext();
    if (type) {
      diagnoseUnsafeType(
          ctx, loc, type,
          use.getDeclContext(),
          [&](Type specificType) {
            ctx.Diags.diagnose(
                loc,
                asNote ? diag::note_reference_to_unsafe_typed_decl
                       : diag::reference_to_unsafe_typed_decl,
                isCall, decl, specificType);
          });
    } else {
      ctx.Diags.diagnose(
          loc,
          asNote ? diag::note_reference_to_unsafe_decl
                 : diag::reference_to_unsafe_decl,
          isCall, decl);
    }

    if (!asNote) {
      suggestUnsafeOnEnclosingDecl(loc, use.getDeclContext());
      decl->diagnose(diag::unsafe_decl_here, decl);
    }

    return;
  }
  }
}

void swift::diagnoseUnsafeUsesIn(const Decl *decl) {
  auto &extras = getSourceFileExtrasFor(decl);
  auto known = extras.unsafeUses.find(decl);
  if (known == extras.unsafeUses.end())
    return;

  // Take the unsafe uses.
  auto unsafeUses = std::move(known->second);
  extras.unsafeUses.erase(known);
  if (unsafeUses.empty())
    return;

  // Determine whether all of the uses are in the definition.
  bool canEncapsulateUnsafety = std::all_of(
      unsafeUses.begin(), unsafeUses.end(), isUnsafeUseInDefinition);
  StringRef fixItStr = canEncapsulateUnsafety
      ? "@safe(unchecked) "
      : "@unsafe ";
  decl->diagnose(diag::decl_involves_unsafe, decl, canEncapsulateUnsafety)
    .fixItInsert(decl->getAttributeInsertionLoc(/*forModifier=*/false),
                 fixItStr);

  for (const auto &unsafeUse : unsafeUses) {
    diagnoseUnsafeUse(unsafeUse, /*asNote=*/true);
  }
}
