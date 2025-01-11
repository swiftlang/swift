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
#include "TypeCheckConcurrency.h"
#include "TypeCheckType.h"
#include "TypeCheckUnsafe.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Effects.h"
#include "swift/AST/UnsafeUse.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/PackConformance.h"
#include "swift/AST/SourceFile.h"

using namespace swift;

void swift::diagnoseUnsafeUse(const UnsafeUse &use) {
  switch (use.getKind()) {
  case UnsafeUse::Override: {
    auto override = use.getDecl();
    override->diagnose(
        diag::override_safe_with_unsafe, override->getDescriptiveKind());
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
    witness->diagnose(diag::note_witness_unsafe,
                      witness->getDescriptiveKind(),
                      witness->getName());
    return;
  }

  case UnsafeUse::TypeWitness: {
    auto assocType = use.getAssociatedType();
    auto loc = use.getLocation();
    auto type = use.getType();
    ASTContext &ctx = assocType->getASTContext();

    diagnoseUnsafeType(ctx, loc, type, [&](Type specificType) {
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
        diag::note_use_of_unsafe_conformance_is_unsafe,
        use.getType(),
        conformance.getRequirement());
    return;
  }

  case UnsafeUse::UnownedUnsafe: {
    auto var = cast<VarDecl>(use.getDecl());
    ASTContext &ctx = var->getASTContext();
    ctx.Diags.diagnose(
        use.getLocation(),
        diag::note_reference_unowned_unsafe,
        var);
    return;
  }

  case UnsafeUse::ExclusivityUnchecked: {
    auto var = cast<VarDecl>(use.getDecl());
    ASTContext &ctx = var->getASTContext();
    ctx.Diags.diagnose(
        use.getLocation(),
        diag::note_reference_exclusivity_unchecked,
        var);
    return;
  }
      
  case UnsafeUse::NonisolatedUnsafe: {
    auto decl = use.getDecl();
    ASTContext &ctx = decl->getASTContext();
    ctx.Diags.diagnose(
        use.getLocation(),
        diag::note_reference_to_nonisolated_unsafe,
        cast<ValueDecl>(decl));
    return;
  }
  case UnsafeUse::ReferenceToUnsafeThroughTypealias: {
    auto typealias = cast<TypeAliasDecl>(use.getDecl());
    ASTContext &ctx = typealias->getASTContext();
    diagnoseUnsafeType(
        ctx, use.getLocation(), use.getType(),
        [&](Type specificType) {
          ctx.Diags.diagnose(
              use.getLocation(),
              diag::note_reference_to_unsafe_through_typealias,
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
          [&](Type specificType) {
            ctx.Diags.diagnose(
                loc,
                diag::note_reference_to_unsafe_typed_decl,
                isCall, decl, specificType);
          });
    } else {
      ctx.Diags.diagnose(
          loc,
          diag::note_reference_to_unsafe_decl,
          isCall, decl);
    }

    return;
  }

  case UnsafeUse::PreconcurrencyImport: {
    auto importDecl = cast<ImportDecl>(use.getDecl());
    importDecl->diagnose(diag::preconcurrency_import_unsafe);
    return;
  }
  }
}

/// Determine whether a reference to the given variable is treated as
/// nonisolated(unsafe).
static bool isReferenceToNonisolatedUnsafe(ValueDecl *decl) {
  auto isolation = getActorIsolationForReference(
      decl, decl->getDeclContext());
  if (!isolation.isNonisolated())
    return false;

  auto attr = decl->getAttrs().getAttribute<NonisolatedAttr>();
  return attr && attr->isUnsafe();
}

bool swift::enumerateUnsafeUses(ConcreteDeclRef declRef,
                                SourceLoc loc,
                                bool isCall,
                                llvm::function_ref<bool(UnsafeUse)> fn) {
  // If the declaration is explicitly unsafe, note that.
  auto decl = declRef.getDecl();
  if (decl->isUnsafe()) {
    (void)fn(
        UnsafeUse::forReferenceToUnsafe(
          decl, isCall && !isa<ParamDecl>(decl), Type(), loc));

    // A declaration being explicitly marked @unsafe always stops enumerating
    // safety issues, because it causes too much noise.
    return true;
  }

  // If the type of this declaration involves unsafe types, diagnose that.
  ASTContext &ctx = decl->getASTContext();
  auto subs = declRef.getSubstitutions();
  {
    auto type = decl->getInterfaceType();
    if (subs)
      type = type.subst(subs);

    bool shouldReturnTrue = false;
    diagnoseUnsafeType(ctx, loc, type, [&](Type unsafeType) {
      if (fn(UnsafeUse::forReferenceToUnsafe(
               decl, isCall && !isa<ParamDecl>(decl), unsafeType, loc)))
        shouldReturnTrue = true;
    });

    if (shouldReturnTrue)
      return true;
  }

  // Check for any unsafe conformances in substitutions.
  if (enumerateUnsafeUses(subs, loc, fn))
    return true;

  // Additional checking for various declaration kinds.
  if (auto valueDecl = dyn_cast<ValueDecl>(decl)) {
    // Use of a nonisolated(unsafe) declaration is unsafe, but is only
    // diagnosed as such under strict concurrency.
    if (ctx.LangOpts.StrictConcurrencyLevel == StrictConcurrency::Complete &&
        isReferenceToNonisolatedUnsafe(valueDecl)) {
      if (fn(UnsafeUse::forNonisolatedUnsafe(valueDecl, loc)))
        return true;
    }

    if (auto var = dyn_cast<VarDecl>(valueDecl)) {
      // unowned(unsafe) is unsafe (duh).
      if (auto ownershipAttr =
              var->getAttrs().getAttribute<ReferenceOwnershipAttr>()) {
        if (ownershipAttr->get() == ReferenceOwnership::Unmanaged) {
          if (fn(UnsafeUse::forUnownedUnsafe(var, loc)))
            return true;
        }
      }

      // @exclusivity(unchecked) is unsafe.
      if (auto exclusivityAttr =
              var->getAttrs().getAttribute<ExclusivityAttr>()) {
        if (exclusivityAttr->getMode() == ExclusivityAttr::Unchecked) {
          if (fn(UnsafeUse::forExclusivityUnchecked(var, loc)))
            return true;
        }
      }
    }
  }

  return false;
}

/// Visit all of the unsafe conformances within this conformance.
static bool forEachUnsafeConformance(
    ProtocolConformanceRef conformance,
    llvm::function_ref<bool(ProtocolConformance *)> body) {
  if (conformance.isInvalid() || conformance.isAbstract())
    return false;

  if (conformance.isPack()) {
    for (auto packConf : conformance.getPack()->getPatternConformances()) {
      if (forEachUnsafeConformance(packConf, body))
        return true;
    }

    return false;
  }

  // Is this an unsafe conformance?
  ProtocolConformance *concreteConf = conformance.getConcrete();
  RootProtocolConformance *rootConf = concreteConf->getRootConformance();
  if (auto normalConf = dyn_cast<NormalProtocolConformance>(rootConf)) {
    if (normalConf->isUnsafe() && body(concreteConf))
      return true;
  }

  // Check conformances that are part of this conformance.
  auto subMap = concreteConf->getSubstitutionMap();
  for (auto subConf : subMap.getConformances()) {
    if (forEachUnsafeConformance(subConf, body))
      return true;
  }

  return false;
}

bool swift::enumerateUnsafeUses(SubstitutionMap subs,
                                SourceLoc loc,
                                llvm::function_ref<bool(UnsafeUse)> fn) {
  // FIXME: Check replacement types?
  for (auto conformance : subs.getConformances()) {
    if (!conformance.hasEffect(EffectKind::Unsafe))
      continue;

    if (forEachUnsafeConformance(
            conformance, [&](ProtocolConformance *unsafeConformance) {
              return fn(
                  UnsafeUse::forConformance(
                    unsafeConformance->getType(),
                    ProtocolConformanceRef(unsafeConformance), loc));
            })) {
      return true;
    }
  }

  return false;
}

bool swift::isUnsafe(ConcreteDeclRef declRef) {
  return enumerateUnsafeUses(
      declRef, SourceLoc(), /*isCall=*/false, [&](UnsafeUse) {
    return true;
  });
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

  bool hasUnsafeType = false;
  diagnoseUnsafeType(requirement->getASTContext(), conformance->getLoc(),
                     requirementTypeInContext, [&](Type unsafeType) {
    hasUnsafeType = true;
  });
  return hasUnsafeType;
}
