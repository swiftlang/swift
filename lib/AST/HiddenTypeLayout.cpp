//===--- HiddenTypeLayout.cpp - Hidden type layout analysis ---------------===//
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

#include "swift/AST/HiddenTypeLayout.h"
#include "swift/AST/Decl.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/Module.h"
#include "swift/AST/TypeWalker.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include <functional>

using namespace swift;

void swift::forEachRequiredHiddenTypeLayout(
    const ModuleDecl *module, ArrayRef<const FileUnit *> files,
    llvm::function_ref<void(const HiddenTypeLayoutRequirement &)> callback) {
  // @_implementationOnly imported types and types imported via
  // -internal-import-bridging-header are not allowed to participate
  // in a module's API, but they can implicitly affect its ABI by
  // defining storage in another type that is part of the module's API.
  // Serialize hidden representations for types used to define storage on an
  // ABI-accessible struct or enum, or a public class. A stored type may also
  // refer to a hidden declaration without depending on its layout, such as a
  // parameter in a stored closure's function type. Preserve those references
  // so the stored type itself can be deserialized.

  // Clients need complete layout information for ABI-accessible value types in
  // order to copy them correctly. Public classes also need complete layout
  // information so clients can use direct field access. ABI-accessible classes
  // that are not public are intentionally excluded.

  llvm::SmallPtrSet<const TypeBase *, 16> visitedTypes;
  llvm::SmallMapVector<const Decl *, HiddenTypeLayoutRequirement, 16>
      requirements;

  auto reportHiddenType = [&](const Decl *layoutDecl, Type hiddenType,
                              HiddenTypeLayoutOrigin origin,
                              HiddenTypeRepresentationKind kind,
                              NominalTypeDecl *abiExposedType,
                              ValueDecl *layoutAffectingStorage) {
    assert(abiExposedType && layoutAffectingStorage);
    HiddenTypeLayoutRequirement requirement{layoutDecl, hiddenType, origin,
                                            kind, abiExposedType,
                                            layoutAffectingStorage};
    auto entry = requirements.find(layoutDecl);
    if (entry == requirements.end()) {
      requirements.insert({layoutDecl, std::move(requirement)});
      return;
    }
    if (kind == HiddenTypeRepresentationKind::AbstractTypeLayout)
      entry->second = std::move(requirement);
  };

  auto isInternalBridgingHeaderImportedType =
      [&](NominalTypeDecl *nominal, DeclContext *useDC) {
        auto importSource = nominal->getImportAccessFrom(useDC);
        assert(importSource &&
               "imported type should have import access information");
        if (importSource->accessLevel >= swift::AccessLevel::Public)
          return false;

        auto *importedModule = importSource->module.importedModule;
        assert(importedModule &&
               "import access should reference an imported module");
        return importedModule->isClangBridgingHeaderImportModule();
      };

  auto reportHiddenTypeIfNeeded =
      [&](Type type, HiddenTypeRepresentationKind representationKind,
          DeclContext *useDC, NominalTypeDecl *abiExposedType,
          ValueDecl *layoutAffectingStorage) {
        if (auto *hiddenType = type->getAs<HiddenType>()) {
          if (auto *layoutInfo = hiddenType->getLayoutInfoDecl())
            reportHiddenType(layoutInfo, type,
                             HiddenTypeLayoutOrigin::RecoveredHiddenType,
                             representationKind, abiExposedType,
                             layoutAffectingStorage);
          return true;
        }

        auto *nominal = type->getAnyNominal();
        if (!nominal)
          return false;

        // We schedule a hidden representation for types
        // that contribute to ABI and will not be visible to clients
        // for some reason.

        // @_implementationOnly imported types will not be provided to clients
        auto *typeModule = nominal->getModuleContext();
        if (module->isImportedImplementationOnly(typeModule,
                                                 /*assumeImported=*/false)) {
          reportHiddenType(nominal, type,
                           HiddenTypeLayoutOrigin::ImplementationOnly,
                           representationKind, abiExposedType,
                           layoutAffectingStorage);
          return true;
        }

        // Types from internally imported bridging headers will not be provided to clients
        if (nominal->hasClangNode()) {
          if (isInternalBridgingHeaderImportedType(nominal, useDC))
            reportHiddenType(
                nominal, type,
                HiddenTypeLayoutOrigin::InternalBridgingHeader,
                representationKind, abiExposedType, layoutAffectingStorage);
          return true;
        }

        return false;
      };

  // Process the declarations referenced by a type expression without
  // traversing the storage of those declarations.
  auto processTypeReferencedFromABIContributingType =
      [&](Type type, DeclContext *useDC,
          NominalTypeDecl *abiExposedType,
          ValueDecl *layoutAffectingStorage) {
        class ReferencedTypeWalker final : public TypeWalker {
          llvm::function_ref<void(Type)> Callback;

        public:
          explicit ReferencedTypeWalker(
              llvm::function_ref<void(Type)> callback)
              : Callback(callback) {}

          Action walkToTypePre(Type type) override {
            Callback(type);
            return Action::Continue;
          }
        };

        auto visitReferencedType = [&](Type referencedType) {
          reportHiddenTypeIfNeeded(
              referencedType, HiddenTypeRepresentationKind::Opaque,
              useDC, abiExposedType, layoutAffectingStorage);
        };
        ReferencedTypeWalker walker(visitReferencedType);
        type.walk(walker);
      };

  // Process a type whose layout contributes to ABI-accessible storage.
  std::function<void(Type, DeclContext *, NominalTypeDecl *, ValueDecl *)>
      processABIContributingType =
          [&](Type type, DeclContext *useDC,
              NominalTypeDecl *abiExposedType,
              ValueDecl *layoutAffectingStorage) {
        if (auto *tupleType = type->getAs<TupleType>()) {
          for (auto elt : tupleType->getElements())
            processABIContributingType(
                elt.getType(), useDC, abiExposedType,
                layoutAffectingStorage);
          return;
        }

        if (type->getAs<AnyFunctionType>() ||
            type->getAs<AnyMetatypeType>() ||
            type->getAs<ExistentialType>()) {
          processTypeReferencedFromABIContributingType(
              type, useDC, abiExposedType, layoutAffectingStorage);
          return;
        }

        if (reportHiddenTypeIfNeeded(
                type, HiddenTypeRepresentationKind::AbstractTypeLayout, useDC,
                abiExposedType, layoutAffectingStorage))
          return;

        NominalTypeDecl *nominal = type->getAnyNominal();
        if (!nominal)
          return;

        ModuleDecl *typeModule = nominal->getModuleContext();

        // We intentionally do not handle the other disallowed origin kinds that
        // sema's exportability analysis diagnoses:

        // SPIImported, SPILocal, SPIOnly:

        // SPI is about restricting access to APIs at the access control level,
        // but definitions of SPI declarations should still be accessible to the compiler,
        // so we don't need hidden representations I believe.

        // MissingImport:

        // MissingImport is just for diagnostics, it doesn't indicate a declaration won't be
        // visible to client compiles

        // FragileCxxAPI:

        // This is about non stable cxx abi being exposed in resilient contexts, not visibility
        // to the compiler.

        // NonPublicImport:

        // For the time being, non public import doesn't control compiler visibility, just
        // API visibility at the typechecking level. Eventually when @_implementationOnly is
        // retired we will need to handle these.

        // ImplementationOnlyMemoryLayout:

        // This should go away when @_implementationOnly is retired and we will need to handle
        // internal types separately

        // No need to recurse into a reslient type, clients don't need to know its
        // layout (by design) in order to manipulate it properly
        if (typeModule != module &&
            nominal->isResilient(const_cast<ModuleDecl *>(module),
                                 swift::ResilienceExpansion::Minimal))
          return;

        auto canonicalType = type->getCanonicalType();
        if (!visitedTypes.insert(canonicalType.getPointer()).second)
          return;

        auto substitutions = type->getContextSubstitutionMap();
        if (isa<StructDecl>(nominal) ||
            (isa<ClassDecl>(nominal) && nominal == abiExposedType)) {
          for (auto *prop : nominal->getStoredProperties()) {
            auto storedType = prop->getInterfaceType().subst(substitutions);
            auto *storage = prop->getModuleContext() == module
                                ? prop
                                : layoutAffectingStorage;
            processABIContributingType(storedType, prop->getDeclContext(),
                                       abiExposedType, storage);
          }
        } else if (auto *innerEnum = dyn_cast<EnumDecl>(nominal)) {
          for (auto *elt : innerEnum->getAllElements()) {
            if (elt->isIndirect() || innerEnum->isIndirect())
              continue;
            if (auto payloadType = elt->getPayloadInterfaceType()) {
              auto *storage = elt->getModuleContext() == module
                                  ? elt
                                  : layoutAffectingStorage;
              processABIContributingType(payloadType.subst(substitutions),
                                         elt->getDeclContext(), abiExposedType,
                                         storage);
            }
          }
        }
      };

  std::function<void(Decl *)> processABIAccessibleDecls = [&](Decl *decl) {
    if (auto *nominal = dyn_cast<NominalTypeDecl>(decl)) {
      auto abiAccess = nominal->getFormalAccessScope(
          /*useDC=*/nullptr, /*treatUsableFromInlineAsPublic=*/true);
      bool isABIAccessibleValueType =
          (abiAccess.isPublic() || abiAccess.isPackage()) &&
          isa<StructDecl, EnumDecl>(nominal);
      bool isPublicClass =
          isa<ClassDecl>(nominal) &&
          nominal
              ->getFormalAccessScope(
                  /*useDC=*/nullptr, /*treatUsableFromInlineAsPublic=*/false)
              .isPublic();
      if (isABIAccessibleValueType || isPublicClass) {
        processABIContributingType(
            nominal->getDeclaredInterfaceType(), nominal, nominal,
            /*layoutAffectingStorage=*/nullptr);
      }
    }

    if (auto *iterable = dyn_cast<IterableDeclContext>(decl)) {
      for (auto *member : iterable->getAllMembers())
        processABIAccessibleDecls(member);
    }
  };

  for (auto *nextFile : files) {
    SmallVector<Decl *, 32> fileDecls;
    nextFile->getTopLevelDeclsWithAuxiliaryDecls(fileDecls);

    for (auto *decl : fileDecls)
      processABIAccessibleDecls(decl);
  }

  for (const auto &entry : requirements)
    callback(entry.second);
}
