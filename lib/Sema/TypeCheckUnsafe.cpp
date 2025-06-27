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
#include "TypeCheckInvertible.h"
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
    override->diagnose(diag::override_safe_with_unsafe, override);
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
    witness->diagnose(diag::note_witness_unsafe, witness);
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
    ASTContext &ctx = conformance.getProtocol()->getASTContext();
    ctx.Diags.diagnose(
        use.getLocation(),
        diag::note_use_of_unsafe_conformance_is_unsafe,
        use.getType(),
        conformance.getProtocol());
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
  case UnsafeUse::ReferenceToUnsafeStorage:
  case UnsafeUse::CallToUnsafe: {
    bool isCall = use.getKind() == UnsafeUse::CallToUnsafe;
    auto decl = cast_or_null<ValueDecl>(use.getDecl());
    auto loc = use.getLocation();
    Type type = use.getType();
    ASTContext &ctx = decl ? decl->getASTContext() : type->getASTContext();
    if (type && decl) {
      diagnoseUnsafeType(
          ctx, loc, type,
          [&](Type specificType) {
            ctx.Diags.diagnose(
                loc,
                use.getKind() == UnsafeUse::ReferenceToUnsafeStorage                       ? diag::note_unsafe_storage
                    : diag::note_reference_to_unsafe_typed_decl,
                isCall, decl, specificType);
          });
    } else if (type) {
      ctx.Diags.diagnose(
          loc,
          diag::note_reference_to_unsafe_type,
          type);
    } else {
      ctx.Diags.diagnose(
          loc,
          diag::note_reference_to_unsafe_decl,
          isCall, decl);
    }

    return;
  }

  case UnsafeUse::CallArgument: {
    auto [argumentName, argumentIndex, argument] = use.getCallArgument();
    Type paramType = use.getType();
    ASTContext &ctx = paramType->getASTContext();

    SourceLoc loc = argument->getLoc();
    if (loc.isInvalid())
      loc = use.getLocation();

    if (auto calleeDecl = dyn_cast_or_null<ValueDecl>(use.getDecl())) {
      if (argumentName.empty()) {
        ctx.Diags.diagnose(
            loc,
            diag::note_unsafe_call_decl_argument_indexed,
            calleeDecl, argumentIndex, argument->getType())
          .highlight(argument->getSourceRange());
      } else {
        ctx.Diags.diagnose(
            loc,
            diag::note_unsafe_call_decl_argument_named,
            calleeDecl, argumentName, argument->getType())
          .highlight(argument->getSourceRange());
      }
    } else {
      ctx.Diags.diagnose(
          loc,
          diag::note_unsafe_call_argument_indexed,
          argumentIndex, argument->getType())
        .highlight(argument->getSourceRange());
    }

    return;
  }

  case UnsafeUse::TemporarilyEscaping: {
    Type type = use.getType();
    ASTContext &ctx = type->getASTContext();
    ctx.Diags.diagnose(
        use.getLocation(), diag::note_unsafe_temporarily_escaping, type);
    return;
  }

  case UnsafeUse::PreconcurrencyImport: {
    auto importDecl = cast<ImportDecl>(use.getDecl());
    importDecl->diagnose(diag::preconcurrency_import_unsafe)
      .fixItInsert(importDecl->getAttributeInsertionLoc(false), "@unsafe ");

    return;
  }
  }
}

/// Determine whether a reference to the given variable is treated as
/// nonisolated(unsafe).
static bool isReferenceToNonisolatedUnsafe(ValueDecl *decl) {
  auto attr = decl->getAttrs().getAttribute<NonisolatedAttr>();
  return attr && attr->isUnsafe();
}

bool swift::enumerateUnsafeUses(ConcreteDeclRef declRef,
                                SourceLoc loc,
                                bool isCall,
                                bool skipTypeCheck,
                                llvm::function_ref<bool(UnsafeUse)> fn) {
  // If the declaration is explicitly unsafe, note that.
  auto decl = declRef.getDecl();
  switch (decl->getExplicitSafety()) {
  case ExplicitSafety::Unspecified:
    // check based on the type, below
    break;

  case ExplicitSafety::Safe:
    // Nothing more to check. It's always safe.
    return false;

  case ExplicitSafety::Unsafe: {
    (void)fn(
        UnsafeUse::forReferenceToUnsafe(
          decl, isCall && !isa<ParamDecl>(decl), Type(), loc));

    // A declaration being explicitly marked @unsafe always stops enumerating
    // safety issues, because it causes too much noise.
    return true;
  }
  }

  // If the type of this declaration involves unsafe types, diagnose that.
  ASTContext &ctx = decl->getASTContext();
  auto subs = declRef.getSubstitutions();
  if (!skipTypeCheck) {
    auto type = decl->getInterfaceType();
    if (subs) {
      if (auto *genericFnType = type->getAs<GenericFunctionType>())
        type = genericFnType->substGenericArgs(subs);
      else
        type = type.subst(subs);
    }

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
    if (normalConf->getExplicitSafety() == ExplicitSafety::Unsafe &&
        body(concreteConf))
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

bool swift::enumerateUnsafeUses(ArrayRef<ProtocolConformanceRef> conformances,
                                SourceLoc loc,
                                llvm::function_ref<bool(UnsafeUse)> fn) {
  for (auto conformance : conformances) {
    if (conformance.isInvalid())
      continue;

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

bool swift::enumerateUnsafeUses(SubstitutionMap subs,
                                SourceLoc loc,
                                llvm::function_ref<bool(UnsafeUse)> fn) {
  // Replacement types.
  for (auto replacementType : subs.getReplacementTypes()) {
    if (replacementType->isUnsafe() &&
        fn(UnsafeUse::forReferenceToUnsafe(nullptr, false, replacementType, loc)))
      return true;
  }

  // Check conformances.
  if (enumerateUnsafeUses(subs.getConformances(), loc, fn))
    return true;

  return false;
}

bool swift::isUnsafe(ConcreteDeclRef declRef) {
  return enumerateUnsafeUses(
      declRef, SourceLoc(), /*isCall=*/false, /*skipTypeCheck=*/false,
      [&](UnsafeUse) {
    return true;
  });
}

bool swift::isUnsafeInConformance(const ValueDecl *requirement,
                                  const Witness &witness,
                                  NormalProtocolConformance *conformance) {
  if (requirement->getExplicitSafety() == ExplicitSafety::Unsafe)
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

void swift::diagnoseUnsafeType(ASTContext &ctx, SourceLoc loc, Type type,
                               llvm::function_ref<void(Type)> diagnose) {
  if (!type->isUnsafe())
    return;

  // Look for a specific @unsafe nominal type along the way.
  class Walker : public TypeWalker {
  public:
    Type specificType;

    Action walkToTypePre(Type type) override {
      if (specificType)
        return Action::Stop;

      // If this refers to a nominal type that is @unsafe, store that.
      if (auto typeDecl = type->getAnyNominal()) {
        if (typeDecl->getExplicitSafety() == ExplicitSafety::Unsafe) {
          specificType = type;
          return Action::Stop;
        }
      }

      // Do not recurse into nominal types, because we do not want to visit
      // their "parent" types.
      if (isa<NominalOrBoundGenericNominalType>(type.getPointer()) ||
          isa<UnboundGenericType>(type.getPointer())) {
        // Recurse into the generic arguments. This operation is recursive,
        // because we also need to see the generic arguments of parent types.
        walkGenericArguments(type);

        return Action::SkipNode;
      }

      return Action::Continue;
    }

  private:
    /// Recursively walk the generic arguments of this type and its parent
    /// types.
    void walkGenericArguments(Type type) {
      if (!type)
        return;

      // Walk the generic arguments.
      if (auto boundGeneric = type->getAs<BoundGenericType>()) {
        for (auto genericArg : boundGeneric->getGenericArgs())
          genericArg.walk(*this);
      }

      if (auto nominalOrBound = type->getAs<NominalOrBoundGenericNominalType>())
        return walkGenericArguments(nominalOrBound->getParent());

      if (auto unbound = type->getAs<UnboundGenericType>())
        return walkGenericArguments(unbound->getParent());
    }
  };

  // Look for a canonical unsafe type.
  Walker walker;
  type->getCanonicalType().walk(walker);
  Type specificType = walker.specificType;

  // Look for an unsafe type in the non-canonical type, which is a better answer
  // if we can find it.
  walker.specificType = Type();
  type.walk(walker);
  if (specificType && walker.specificType &&
      specificType->isEqual(walker.specificType))
    specificType = walker.specificType;

  diagnose(specificType ? specificType : type);
}

void swift::checkUnsafeStorage(NominalTypeDecl *nominal) {
  // If the type is marked explicitly with @safe or @unsafe, there's nothing
  // to check.
  switch (nominal->getExplicitSafety()) {
  case ExplicitSafety::Safe:
  case ExplicitSafety::Unsafe:
    return;

  case ExplicitSafety::Unspecified:
    break;
  }

  // Check whether the superclass is unsafe. If so, the only thing one can
  // do is mark the class unsafe.
  ASTContext &ctx = nominal->getASTContext();
  if (auto classDecl = dyn_cast<ClassDecl>(nominal)) {
    if (Type superclassType = classDecl->getSuperclass()) {
      superclassType = classDecl->mapTypeIntoContext(superclassType);
      bool diagnosed = false;
      diagnoseUnsafeType(ctx, classDecl->getLoc(), superclassType, [&](Type type) {
        if (diagnosed)
          return;

        classDecl->diagnose(diag::unsafe_superclass, classDecl, type)
          .fixItInsert(classDecl->getAttributeInsertionLoc(false), "@unsafe ");
        diagnosed = true;
      });

      if (diagnosed)
        return;
    }
  }

  // Visitor that finds unsafe storage.
  class UnsafeStorageVisitor: public StorageVisitor {
    ASTContext &ctx;
    SmallVectorImpl<UnsafeUse> &unsafeUses;

  public:
    UnsafeStorageVisitor(ASTContext &ctx, SmallVectorImpl<UnsafeUse> &unsafeUses)
      : ctx(ctx), unsafeUses(unsafeUses) { }

    bool operator()(VarDecl *property, Type propertyType) override {
      diagnoseUnsafeType(ctx, property->getLoc(), propertyType, [&](Type type) {
        unsafeUses.push_back(
            UnsafeUse::forReferenceToUnsafeStorage(
              property, propertyType, property->getLoc()));
      });
      return false;
    }

    bool operator()(EnumElementDecl *element, Type elementType) override {
      diagnoseUnsafeType(ctx, element->getLoc(), elementType, [&](Type type) {
        unsafeUses.push_back(
            UnsafeUse::forReferenceToUnsafeStorage(
            element, elementType, element->getLoc()));
      });
      return false;
    }
  };

  // Look for any unsafe storage in this nominal type.
  SmallVector<UnsafeUse, 4> unsafeUses;
  UnsafeStorageVisitor(ctx, unsafeUses).visit(nominal, nominal);

  // If we didn't find any unsafe storage, there's nothing to do.
  if (unsafeUses.empty())
    return;

  // Complain about this type needing @safe or @unsafe.
  nominal->diagnose(diag::decl_unsafe_storage, nominal);
  nominal->diagnose(diag::decl_storage_mark_unsafe)
    .fixItInsert(nominal->getAttributeInsertionLoc(false), "@unsafe ");
  nominal->diagnose(diag::decl_storage_mark_safe)
    .fixItInsert(nominal->getAttributeInsertionLoc(false), "@safe ");
  std::for_each(unsafeUses.begin(), unsafeUses.end(), diagnoseUnsafeUse);
}
