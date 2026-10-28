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
#include "swift/AST/Types.h"
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
  // ABI-accessible struct or enum, or a public class.

  // Clients need complete layout information for ABI-accessible value types in
  // order to copy them correctly. Public classes also need complete layout
  // information so clients can use direct field access. ABI-accessible classes
  // that are not public are intentionally excluded.

  llvm::SmallPtrSet<const TypeBase *, 16> visitedTypes;

  auto reportHiddenType = [&](const Decl *layoutDecl, Type hiddenType,
                              HiddenTypeLayoutOrigin origin,
                              NominalTypeDecl *abiExposedType,
                              ValueDecl *layoutAffectingStorage) {
    assert(abiExposedType && layoutAffectingStorage);
    callback({layoutDecl, hiddenType, origin, abiExposedType,
              layoutAffectingStorage});
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

  struct HiddenTypeReference {
    const Decl *LayoutDecl;
    HiddenTypeLayoutOrigin Origin;
  };

  auto getHiddenTypeReference =
      [&](Type type,
          DeclContext *useDC) -> std::optional<HiddenTypeReference> {
    if (auto *hiddenType = type->getAs<HiddenType>()) {
      if (auto *layoutInfo = hiddenType->getLayoutInfoDecl())
        return HiddenTypeReference{
            layoutInfo, HiddenTypeLayoutOrigin::RecoveredHiddenType};
      return std::nullopt;
    }

    auto *nominal = type->getAnyNominal();
    if (!nominal)
      return std::nullopt;

    auto *typeModule = nominal->getModuleContext();
    if (module->isImportedImplementationOnly(typeModule,
                                             /*assumeImported=*/false))
      return HiddenTypeReference{
          nominal, HiddenTypeLayoutOrigin::ImplementationOnly};

    if (nominal->hasClangNode() &&
        isInternalBridgingHeaderImportedType(nominal, useDC))
      return HiddenTypeReference{
          nominal, HiddenTypeLayoutOrigin::InternalBridgingHeader};

    return std::nullopt;
  };

  // Find declarations anywhere in the complete type expression that must be
  // available before a layout-bearing member can be deserialized. This is
  // deliberately broader than physical layout traversal: function parameters,
  // phantom generic arguments, and generic requirements can prevent the member
  // declaration from loading without contributing storage themselves.
  auto processTypeReferencedFromABIContributingType =
      [&](Type type, DeclContext *useDC, NominalTypeDecl *abiExposedType,
          ValueDecl *layoutAffectingStorage) {
        llvm::SmallPtrSet<const Decl *, 8> reportedHiddenDecls;
        llvm::SmallPtrSet<const NominalTypeDecl *, 8> visitedSignatures;

        std::function<void(Type, DeclContext *)> visitTypeReferences =
            [&](Type referencedType, DeclContext *referenceDC) {
          if (!referencedType)
            return;

          referencedType.visit([&](Type componentType) {
            if (auto hidden =
                    getHiddenTypeReference(componentType, referenceDC)) {
              if (reportedHiddenDecls.insert(hidden->LayoutDecl).second)
                reportHiddenType(hidden->LayoutDecl, componentType,
                                 hidden->Origin, abiExposedType,
                                 layoutAffectingStorage);
            }

            auto *nominal = componentType->getAnyNominal();
            if (!nominal || nominal->getModuleContext() != module ||
                !visitedSignatures.insert(nominal).second)
              return;

            auto signature = nominal->getGenericSignature();
            if (!signature)
              return;
            for (auto requirement : signature.getRequirements()) {
              visitTypeReferences(requirement.getFirstType(), nominal);
              if (requirement.getKind() != RequirementKind::Layout)
                visitTypeReferences(requirement.getSecondType(), nominal);
            }
          });
        };

        visitTypeReferences(type, useDC);
      };

  // Traverse a type that contributes to an ABI-accessible layout to find the
  // local stored members that clients may need to deserialize.
  std::function<void(Type, DeclContext *, NominalTypeDecl *, ValueDecl *)>
      processTypeForHiddenLayouts =
          [&](Type type, DeclContext *useDC,
              NominalTypeDecl *abiExposedType,
              ValueDecl *layoutAffectingStorage) {
        if (getHiddenTypeReference(type, useDC))
          return;

        if (auto *tupleType = type->getAs<TupleType>()) {
          for (auto elt : tupleType->getElements())
            processTypeForHiddenLayouts(elt.getType(), useDC, abiExposedType,
                                        layoutAffectingStorage);
          return;
        }

        NominalTypeDecl *nominal = type->getAnyNominal();
        if (!nominal)
          return;

        ModuleDecl *typeModule = nominal->getModuleContext();
        if (nominal->hasClangNode())
          return;

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
            auto declaredType = prop->getInterfaceType();
            auto *storage = prop->getModuleContext() == module
                                ? prop
                                : layoutAffectingStorage;
            processTypeReferencedFromABIContributingType(
                declaredType, prop->getDeclContext(), abiExposedType, storage);
            processTypeForHiddenLayouts(declaredType.subst(substitutions),
                                        prop->getDeclContext(), abiExposedType,
                                        storage);
          }
        } else if (auto *innerEnum = dyn_cast<EnumDecl>(nominal)) {
          for (auto *elt : innerEnum->getAllElements()) {
            if (auto payloadType = elt->getPayloadInterfaceType()) {
              auto *storage = elt->getModuleContext() == module
                                  ? elt
                                  : layoutAffectingStorage;
              processTypeReferencedFromABIContributingType(
                  payloadType, elt->getDeclContext(), abiExposedType, storage);
              if (!elt->isIndirect() && !innerEnum->isIndirect())
                processTypeForHiddenLayouts(
                    payloadType.subst(substitutions), elt->getDeclContext(),
                    abiExposedType, storage);
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
        processTypeForHiddenLayouts(
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
}
