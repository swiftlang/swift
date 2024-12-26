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

#include "TypeCheckAvailability.h"
#include "TypeCheckType.h"
#include "TypeCheckUnsafe.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/UnsafeUse.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/PackConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/SourceFileExtras.h"

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
    case UnsafeUse::PreconcurrencyImport:
    // Never part of the definition. These are always part of the interface.
    return false;

  case UnsafeUse::ReferenceToUnsafe:
  case UnsafeUse::ReferenceToUnsafeThroughTypealias:
  case UnsafeUse::CallToUnsafe:
  case UnsafeUse::NonisolatedUnsafe:
  case UnsafeUse::UnownedUnsafe:
  case UnsafeUse::UnsafeConformance:
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

/// Retrieve the extra information
static SourceFileExtras *getSourceFileExtrasFor(const Decl *decl) {
  auto dc = decl->getDeclContext();
  auto sf = dc->getOutermostParentSourceFile();
  if (!sf)
    return nullptr;

  return &sf->getExtras();
}

void swift::diagnoseUnsafeUse(const UnsafeUse &use, bool asNote) {
  if (!asNote) {
    // If we can associate this unsafe use within a particular declaration, do so.
    // It will be diagnosed later, along with all other unsafe uses within this
    // same declaration.
    if (use.getKind() == UnsafeUse::ReferenceToUnsafe ||
        use.getKind() == UnsafeUse::ReferenceToUnsafeThroughTypealias ||
        use.getKind() == UnsafeUse::CallToUnsafe ||
        use.getKind() == UnsafeUse::NonisolatedUnsafe ||
        use.getKind() == UnsafeUse::UnownedUnsafe ||
        use.getKind() == UnsafeUse::UnsafeConformance) {
      auto [enclosingDecl, _] = enclosingContextForUnsafe(
          use.getLocation(), use.getDeclContext());
      if (enclosingDecl) {
        if (auto extras = getSourceFileExtrasFor(enclosingDecl)) {
          extras->unsafeUses[enclosingDecl].push_back(use);
        }
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
    assert(asNote && "Can only be diagnosed with a note");
    auto witness = cast<ValueDecl>(use.getDecl());
    witness->diagnose(diag::note_witness_unsafe,
                      witness->getDescriptiveKind(),
                      witness->getName());
    return;
  }

  case UnsafeUse::TypeWitness: {
    assert(asNote && "Can only be diagnosed with a note");
    auto assocType = use.getAssociatedType();
    auto loc = use.getLocation();
    auto type = use.getType();
    auto conformance = use.getConformance().getConcrete();
    ASTContext &ctx = assocType->getASTContext();

    diagnoseUnsafeType(ctx, loc, type, conformance->getDeclContext(),
                       [&](Type specificType) {
      ctx.Diags.diagnose(
          loc, diag::note_type_witness_unsafe, specificType,
          assocType->getName());
    });
    return;
  }

  case UnsafeUse::UnsafeConformance: {
    auto conformance = use.getConformance();
    ASTContext &ctx = conformance.getRequirement()->getASTContext();
    ctx.Diags.diagnose(
        use.getLocation(),
        asNote ? diag::note_use_of_unsafe_conformance_is_unsafe
               : diag::use_of_unsafe_conformance_is_unsafe,
        use.getType(),
        conformance.getRequirement());
    return;
  }

  case UnsafeUse::UnownedUnsafe: {
    auto var = cast<VarDecl>(use.getDecl());
    ASTContext &ctx = var->getASTContext();
    ctx.Diags.diagnose(
        use.getLocation(),
        asNote ? diag::note_reference_unowned_unsafe
               : diag::reference_unowned_unsafe,
        var);
    if (!asNote) {
      var->diagnose(diag::make_enclosing_context_unsafe, var)
        .fixItInsert(var->getAttributeInsertionLoc(false), "@unsafe ");
    }
    return;
  }

  case UnsafeUse::NonisolatedUnsafe: {
    auto decl = use.getDecl();
    ASTContext &ctx = decl->getASTContext();
    ctx.Diags.diagnose(
        use.getLocation(),
        asNote ? diag::note_reference_to_nonisolated_unsafe
               : diag::note_reference_to_nonisolated_unsafe,
        cast<ValueDecl>(decl));
    if (!asNote) {
      if (auto var = dyn_cast<VarDecl>(decl)) {
        var->diagnose(diag::make_enclosing_context_unsafe, var)
          .fixItInsert(var->getAttributeInsertionLoc(false), "@unsafe ");
      }
    }
    return;
  }
  case UnsafeUse::ReferenceToUnsafeThroughTypealias: {
    auto typealias = cast<TypeAliasDecl>(use.getDecl());
    ASTContext &ctx = typealias->getASTContext();
    diagnoseUnsafeType(
        ctx, use.getLocation(), use.getType(),
        use.getDeclContext(),
        [&](Type specificType) {
          ctx.Diags.diagnose(
              use.getLocation(),
              asNote ? diag::note_reference_to_unsafe_through_typealias
                     : diag::reference_to_unsafe_through_typealias,
              typealias, specificType);
        });
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
    }

    return;
  }

  case UnsafeUse::PreconcurrencyImport: {
    auto importDecl = cast<ImportDecl>(use.getDecl());
    importDecl->diagnose(diag::preconcurrency_import_unsafe)
      .fixItInsert(importDecl->getAttributeInsertionLoc(false),
                   "@safe(unchecked) ");
    return;
  }
  }
}

void swift::diagnoseUnsafeUsesIn(const Decl *decl) {
  auto *extras = getSourceFileExtrasFor(decl);
  if (!extras)
    return;

  auto known = extras->unsafeUses.find(decl);
  if (known == extras->unsafeUses.end())
    return;

  // Take the unsafe uses.
  auto unsafeUses = std::move(known->second);
  extras->unsafeUses.erase(known);
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

bool swift::isUnsafe(ConcreteDeclRef declRef) {
  auto decl = declRef.getDecl();
  if (!decl)
    return false;

  // Is the declaration explicitly @unsafe?
  if (decl->isUnsafe())
    return true;

  auto type = decl->getInterfaceType();
  if (auto subs = declRef.getSubstitutions())
    type = type.subst(subs);
  if (type->isUnsafe())
    return true;

  return false;
}

bool swift::isUnsafeInConformance(const ValueDecl *requirement,
                                  const Witness &witness,
                                  NormalProtocolConformance *conformance) {
  if (requirement->isUnsafe())
    return true;

  Type requirementType = requirement->getInterfaceType();
  Type requirementTypeInContext;
  auto requirementSubs = witness.getRequirementToWitnessThunkSubs();
  if (auto genericFnType = requirementType->getAs<GenericFunctionType>()) {
    requirementTypeInContext =
        genericFnType->substGenericArgs(requirementSubs);
  } else {
    requirementTypeInContext = requirementType.subst(requirementSubs);
  }
  return requirementTypeInContext->isUnsafe();
}
