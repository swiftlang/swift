//===--- TypeCheckClassAllocator.cpp - Checking class allocators ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file checks the allocators of classes: allocators that classes
// inherit or get from `-default-class-allocator`, the flag
// `-default-class-allocator` itself, and the safety of conformances to
// `ClassAllocator`.
// The `@allocator` attribute itself is checked in TypeCheckAttr.cpp.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AvailabilityContext.h"
#include "swift/AST/AvailabilityRange.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

using namespace swift;

std::optional<unsigned>
TypeChecker::getClassAllocatorIneligibility(const ClassDecl *cls) {
  // Objective-C classes are allocated and freed by the Objective-C runtime.
  if (cls->isForeign() || cls->checkAncestry(AncestryFlags::ObjC))
    return 0;

  // Tail-allocated elements aren't supported.
  if (auto *managedBuffer = cls->getASTContext().getManagedBufferDecl())
    for (auto *superclass = cls->getSuperclassDecl(); superclass;
         superclass = superclass->getSuperclassDecl())
      if (superclass == managedBuffer)
        return 1;

  // The distributed actor runtime allocates remote proxies with the system
  // allocator, and at a smaller size; the runtime couldn't tell them from
  // instances that the allocator allocated.
  if (cls->isDistributedActor())
    return 3;

  // In a module built with library evolution, only classes that clients
  // outside the module's package can't see, and that aren't declared `open`,
  // may have allocators. Clients can inline the allocating entry points of
  // the designated initializers of public and `@usableFromInline` classes, so
  // their instances could be allocated by code compiled against an earlier
  // version of the module, which had no allocator or another one. Those of
  // package classes aren't serialized, except by Package CMO, which relies on
  // the modules of the package being built together, so this module allocates
  // their instances. The other modules of the package that access them
  // resiliently leave their frees to the runtime, which finds the allocator
  // in their metadata (see ClassDecl::getDirectlyCallableAllocatorType).
  // A class declared `open` can't have one even when it's nested in a package
  // or internal type, where other modules of the package (or `@testable`
  // importers) can still subclass it: a class in another module never
  // inherits an allocator from a library-evolution module, which could change
  // it without that module being rebuilt.
  if (cls->getModuleContext()->isResilient() &&
      (cls->getFormalAccess() == AccessLevel::Open ||
       cls->getFormalAccessScope(/*useDC=*/nullptr,
                                 /*treatUsableFromInlineAsPublic=*/true)
           .isPublic()))
    return 2;

  return std::nullopt;
}

bool TypeChecker::isClassVisibleOnlyInPackage(const ClassDecl *cls) {
  return cls
      ->getFormalAccessScope(/*useDC=*/nullptr,
                             /*treatUsableFromInlineAsPublic=*/true)
      .isPackage();
}

unsigned TypeChecker::getClassAllocatorVisibilityKind(const ClassDecl *cls) {
  switch (cls->getFormalAccess()) {
  case AccessLevel::Public:
    return 0;
  case AccessLevel::Open:
    // Declared `open`, whatever its effective visibility.
    return 1;
  default:
    // `@usableFromInline` (internal or package), or nested in a type that's
    // only `@usableFromInline`.
    return 2;
  }
}

const NominalTypeDecl *
TypeChecker::getUnexportedClassAllocatorType(const ClassDecl *cls,
                                             Type allocatorType) {
  // Clients of a public class subclass it, inheriting its allocator, and
  // allocate its instances in inlinable code, calling the allocator directly.
  // So do the other modules of the package of a package class, in which
  // Package CMO can inline its allocations, and whose package interface
  // prints the allocator. Every type that the allocator type names must be
  // visible to them.
  auto classScope = cls->getFormalAccessScope(
      /*useDC=*/nullptr, /*treatUsableFromInlineAsPublic=*/true);
  if (!classScope.isPublicOrPackage())
    return nullptr;
  bool isPackage = isClassVisibleOnlyInPackage(cls);

  const NominalTypeDecl *notExported = nullptr;
  allocatorType.findIf([&](Type type) {
    auto *nominal = type->getAnyNominal();
    if (!nominal)
      return false;
    auto scope = nominal->getFormalAccessScope(
        /*useDC=*/nullptr, /*treatUsableFromInlineAsPublic=*/true);
    if (scope.isPublic() || (isPackage && scope.isPackage()))
      return false;
    notExported = nominal;
    return true;
  });
  return notExported;
}

void TypeChecker::checkClassAllocator(ClassDecl *cls) {
  using Kind = ClassAllocatorSource::Kind;
  auto source = cls->getAllocatorSource();
  if (source.SourceKind != Kind::Inherited &&
      source.SourceKind != Kind::ModuleDefault)
    return;
  Type allocatorType = source.AllocatorType;
  if (!allocatorType || allocatorType->hasError() ||
      source.isSystemClassAllocator())
    return;

  auto &ctx = cls->getASTContext();
  bool isInherited = source.SourceKind == Kind::Inherited;
  if (auto reason = getClassAllocatorIneligibility(cls)) {
    // If the class that declares the allocator can't have it either, it's
    // diagnosed there.
    if (isInherited && getClassAllocatorIneligibility(source.DeclaringClass))
      return;
    cls->diagnose(diag::allocator_not_allowed, isInherited ? 1 : 2,
                  allocatorType, cls, *reason,
                  getClassAllocatorVisibilityKind(cls));
    ctx.Diags.diagnose(cls->getStartLoc(), diag::allocator_opt_out)
        .fixItInsert(cls->getAttributeInsertionLoc(/*forModifier=*/false),
                     "@allocator(SystemClassAllocator) ");
    return;
  }

  // An inherited allocator was checked where it was declared.
  if (isInherited)
    return;
  if (auto *notExported = getUnexportedClassAllocatorType(cls, allocatorType))
    cls->diagnose(diag::allocator_type_not_exported, /*default=*/1,
                  allocatorType, cls->getFormalAccess(), cls, notExported,
                  notExported->getFormalAccess(),
                  isClassVisibleOnlyInPackage(cls));
}

//===----------------------------------------------------------------------===//
// -default-class-allocator
//===----------------------------------------------------------------------===//

/// Resolve and check the class allocator named by `-default-class-allocator`
/// for \p module, diagnosing errors. The name is fully qualified,
/// `Module.Type`, and it is resolved as if `Module` were imported into every
/// file of \p module. The type must conform to `ClassAllocator`.
Type DefaultClassAllocatorRequest::evaluate(Evaluator &evaluator,
                                            ModuleDecl *module) const {
  auto &ctx = module->getASTContext();
  auto &diags = ctx.Diags;
  StringRef name = ctx.LangOpts.DefaultClassAllocator;
  if (!module->isMainModule() || name.empty())
    return Type();

  if (!ctx.LangOpts.hasFeature(Feature::ClassAllocators)) {
    diags.diagnose(SourceLoc(), diag::default_class_allocator_requires_feature);
    return Type();
  }
  if (ctx.LangOpts.hasFeature(Feature::Embedded)) {
    diags.diagnose(SourceLoc(), diag::default_class_allocator_in_embedded);
    return Type();
  }

  SmallVector<StringRef, 4> components;
  name.split(components, '.');
  if (components.size() < 2 || llvm::any_of(components, [](StringRef c) {
        return !Lexer::isIdentifier(c);
      })) {
    diags.diagnose(SourceLoc(), diag::default_class_allocator_invalid_name,
                   name);
    return Type();
  }

  Identifier moduleName = ctx.getIdentifier(components.front());
  ModuleDecl *typeModule = module->getName() == moduleName
                               ? module
                               : ctx.getModuleByIdentifier(moduleName);
  if (!typeModule) {
    diags.diagnose(SourceLoc(), diag::default_class_allocator_module_not_found,
                   moduleName, name);
    return Type();
  }

  // Look up the first component in the module, and each further component as
  // a member type of the previous one. Only public types of other modules are
  // visible.
  auto isVisible = [&](ValueDecl *decl) {
    return decl->getModuleContext() == module ||
           decl->getFormalAccessScope(/*useDC=*/nullptr).isPublic();
  };
  TypeDecl *typeDecl = nullptr;
  for (StringRef component : llvm::drop_begin(components)) {
    Identifier name = ctx.getIdentifier(component);
    SmallVector<ValueDecl *, 2> candidates;
    if (typeDecl) {
      auto *nominal = dyn_cast<NominalTypeDecl>(typeDecl);
      if (!nominal)
        break;
      for (auto *member : nominal->lookupDirect(name))
        candidates.push_back(member);
    } else {
      typeModule->lookupValue(name, NLKind::QualifiedLookup, candidates);
    }
    typeDecl = nullptr;
    for (auto *candidate : candidates) {
      auto *candidateType = dyn_cast<TypeDecl>(candidate);
      if (!candidateType || !isVisible(candidateType))
        continue;
      if (typeDecl) {
        // Ambiguous.
        typeDecl = nullptr;
        break;
      }
      typeDecl = candidateType;
    }
    if (!typeDecl)
      break;
  }
  if (!typeDecl) {
    diags.diagnose(SourceLoc(), diag::default_class_allocator_type_not_found,
                   name);
    return Type();
  }

  Type type = typeDecl->getDeclaredInterfaceType();
  if (!type || type->hasError()) {
    diags.diagnose(SourceLoc(), diag::default_class_allocator_type_not_found,
                   name);
    return Type();
  }
  if (type->hasTypeParameter() || type->hasUnboundGenericType()) {
    diags.diagnose(SourceLoc(), diag::default_class_allocator_type_generic,
                   type);
    return Type();
  }

  auto *proto = ctx.getProtocol(KnownProtocolKind::ClassAllocator);
  if (!proto || checkConformance(type, proto).isInvalid()) {
    diags.diagnose(SourceLoc(), diag::default_class_allocator_does_not_conform,
                   type);
    return Type();
  }

  // Every class of the module would need the runtime support for class
  // allocators.
  if (!ctx.LangOpts.DisableAvailabilityChecking) {
    auto required = ctx.getClassAllocatorsAvailability();
    if (!AvailabilityRange::forDeploymentTarget(ctx).isContainedIn(required)) {
      diags.diagnose(SourceLoc(), diag::default_class_allocator_unavailable,
                     ctx.getTargetAvailabilityDomain(), required);
      return Type();
    }
  }

  return type;
}

void TypeChecker::checkDefaultClassAllocatorFlag(ModuleDecl *module) {
  (void)module->getDefaultClassAllocator();
}

void TypeChecker::checkClassAllocatorConformanceSafety(
    NormalProtocolConformance *conformance) {
  auto *proto = conformance->getProtocol();
  if (!proto->isSpecificProtocol(KnownProtocolKind::ClassAllocator))
    return;

  ASTContext &ctx = proto->getASTContext();
  if (!ctx.LangOpts.hasFeature(Feature::StrictMemorySafety,
                               /*allowMigration=*/true))
    return;

  // Only conformances in the source being compiled.
  if (!conformance->getDeclContext()->getParentSourceFile())
    return;

  // The compiler trusts a class allocator to return valid memory and not to
  // reuse it too early, so a conformance to `ClassAllocator` is unsafe and
  // must be written `@unsafe`. The general check for unsafe witnesses of safe
  // requirements never catches it, because every requirement of an `@unsafe`
  // protocol is unsafe. A conformance implied by a conformance to a
  // protocol that refines it is acknowledged by writing that
  // explicit conformance `@unsafe` (`X: @unsafe MyAllocator`), which is also
  // where the Fix-It goes; marking the refining protocol doesn't.
  auto *explicitConformance = conformance;
  while (explicitConformance->getSourceKind() ==
         ConformanceEntryKind::Implied)
    explicitConformance = explicitConformance->getImplyingConformance();
  if (conformance->getExplicitSafety() == ExplicitSafety::Unsafe ||
      explicitConformance->getExplicitSafety() == ExplicitSafety::Unsafe)
    return;

  // The conformances of an unsafe type, such as one declared `@unsafe`, are
  // already unsafe, as for unsafe witnesses of safe requirements. An `@unsafe`
  // extension of a safe type doesn't make its conformances unsafe.
  if (conformance->getType()->isUnsafe())
    return;

  auto diag = ctx.Diags.diagnose(conformance->getLoc(),
                                 diag::class_allocator_conformance_not_unsafe,
                                 conformance->getType(), proto);
  if (explicitConformance->getSourceKind() == ConformanceEntryKind::Explicit)
    conformance->applyConformanceAttribute(diag, "@unsafe");
}
