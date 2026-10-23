//===--- ClientDeclarationRequirements.cpp - Client AST needs ------------===//
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

#include "swift/AST/ClientDeclarationRequirements.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTContextGlobalCache.h"
#include "swift/AST/Decl.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/Module.h"
#include "swift/AST/SynthesizedFileUnit.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/SmallPtrSet.h"
#include <functional>
#include <memory>

using namespace swift;

const ClientDeclarationRequirement &
ClientDeclarationRequirements::getRequirement(
    const Decl *declaration) const {
  auto entry = Requirements.find(declaration);
  if (entry == Requirements.end())
    llvm::report_fatal_error(
        "client declaration requirement was not computed");
  return entry->second;
}

void ClientDeclarationRequirements::recordRequirement(
    const Decl *declaration, ClientDeclarationRequirement requirement) {
  auto entry = Requirements.find(declaration);
  if (entry == Requirements.end()) {
    Requirements.insert({declaration, std::move(requirement)});
    return;
  }

  auto &existing = entry->second;
  if (requirement.getKind() <= existing.getKind())
    return;

  existing = std::move(requirement);
}

void ClientDeclarationRequirements::print(llvm::raw_ostream &out) const {
  auto printDecl = [&](const Decl *declaration) {
    auto *value = dyn_cast<ValueDecl>(declaration);
    if (!value) {
      out << Decl::getDescriptiveKindName(declaration->getDescriptiveKind());
      return;
    }

    SmallVector<const ValueDecl *, 4> path{value};
    for (auto *context = value->getDeclContext(); context;
         context = context->getParent()) {
      if (auto *parent = dyn_cast_or_null<ValueDecl>(context->getAsDecl()))
        path.push_back(parent);
    }

    out << value->getModuleContext()->getName();
    for (auto entry = path.rbegin(); entry != path.rend(); ++entry) {
      out << '.';
      (*entry)->getName().printPretty(out);
    }
  };

  auto printKind = [&](ClientDeclarationRequirement::Kind kind) {
    switch (kind) {
    case ClientDeclarationRequirement::Kind::None:
      out << "None";
      return;
    case ClientDeclarationRequirement::Kind::OpaqueDeclaration:
      out << "OpaqueDeclaration";
      return;
    case ClientDeclarationRequirement::Kind::AbstractTypeLayoutDeclaration:
      out << "AbstractTypeLayoutDeclaration";
      return;
    case ClientDeclarationRequirement::Kind::FullDeclaration:
      out << "FullDeclaration";
      return;
    }
    llvm_unreachable("unknown client declaration requirement");
  };

  out << "Client declaration requirements for module '"
      << AnalyzedModule->getName() << "':\n";
  for (auto &[declaration, requirement] : Requirements) {
    out << "  ";
    printDecl(declaration);
    out << ": ";
    printKind(requirement.getKind());
    out << '\n';
  }
}

class swift::ClientDeclarationRequirementsBuilder {
  const ModuleDecl *AnalyzedModule;
  ClientDeclarationRequirements &Result;

public:
  ClientDeclarationRequirementsBuilder(const ModuleDecl *module,
                                       ClientDeclarationRequirements &result)
      : AnalyzedModule(module), Result(result) {}

  void run();
};

void ClientDeclarationRequirementsBuilder::run() {
  llvm::SmallPtrSet<const TypeBase *, 16> visitedTypes;

  auto isInternalBridgingHeaderImportedType = [&](NominalTypeDecl *nominal,
                                                  DeclContext *useDC) {
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

  auto recordHiddenTypeRequirement =
      [&](Type type, DeclContext *useDC,
          ClientDeclarationRequirement::Kind kind,
          NominalTypeDecl *rootRequiringDeclaration,
          ValueDecl *leafRequiringDeclaration) {
        const Decl *requiredDecl = nullptr;
        std::optional<HiddenTypeLayoutOrigin> origin;

        if (auto *hiddenType = type->getAs<HiddenType>()) {
          requiredDecl = hiddenType->getLayoutInfoDecl();
          origin = HiddenTypeLayoutOrigin::RecoveredHiddenType;
        } else if (auto *nominal = type->getAnyNominal()) {
          auto *typeModule = nominal->getModuleContext();
          if (AnalyzedModule->isImportedImplementationOnly(
                  typeModule, /*assumeImported=*/false)) {
            requiredDecl = nominal;
            origin = HiddenTypeLayoutOrigin::ImplementationOnly;
          } else if (nominal->hasClangNode() &&
                     isInternalBridgingHeaderImportedType(nominal, useDC)) {
            requiredDecl = nominal;
            origin = HiddenTypeLayoutOrigin::InternalBridgingHeader;
          }
        }

        if (!requiredDecl)
          return false;

        assert(rootRequiringDeclaration && leafRequiringDeclaration);
        ClientDeclarationRequirementProvenance provenance{
            type, origin, rootRequiringDeclaration, leafRequiringDeclaration};
        switch (kind) {
        case ClientDeclarationRequirement::Kind::None:
          llvm_unreachable("hidden type must have a client requirement");
        case ClientDeclarationRequirement::Kind::OpaqueDeclaration:
          Result.recordRequirement(
              requiredDecl,
              ClientDeclarationRequirement::forOpaqueDeclaration(provenance));
          break;
        case ClientDeclarationRequirement::Kind::AbstractTypeLayoutDeclaration:
          Result.recordRequirement(
              requiredDecl,
              ClientDeclarationRequirement::forAbstractTypeLayoutDeclaration(
                  provenance));
          break;
        case ClientDeclarationRequirement::Kind::FullDeclaration:
          llvm_unreachable("hidden type cannot require a full declaration");
        }
        return true;
      };

  std::function<void(Type, ClientDeclarationRequirement::Kind, DeclContext *,
                     NominalTypeDecl *, ValueDecl *)>
      processTypeForClientRequirements =
          [&](Type type, ClientDeclarationRequirement::Kind requirementKind,
              DeclContext *useDC, NominalTypeDecl *rootRequiringDeclaration,
              ValueDecl *leafRequiringDeclaration) {
            bool isHidden = recordHiddenTypeRequirement(
                type, useDC, requirementKind, rootRequiringDeclaration,
                leafRequiringDeclaration);

            if (auto *tupleType = type->getAs<TupleType>()) {
              for (auto element : tupleType->getElements())
                processTypeForClientRequirements(
                    element.getType(), requirementKind, useDC,
                    rootRequiringDeclaration, leafRequiringDeclaration);
              return;
            }

            // Function values have fixed layout independent of their
            // signatures.
            if (auto *functionType = type->getAs<AnyFunctionType>()) {
              for (auto parameter : functionType->getParams())
                processTypeForClientRequirements(
                    parameter.getPlainType(),
                    ClientDeclarationRequirement::Kind::OpaqueDeclaration,
                    useDC, rootRequiringDeclaration, leafRequiringDeclaration);
              if (auto thrownError = functionType->getThrownError())
                processTypeForClientRequirements(
                    thrownError,
                    ClientDeclarationRequirement::Kind::OpaqueDeclaration,
                    useDC, rootRequiringDeclaration, leafRequiringDeclaration);
              processTypeForClientRequirements(
                  functionType->getResult(),
                  ClientDeclarationRequirement::Kind::OpaqueDeclaration, useDC,
                  rootRequiringDeclaration, leafRequiringDeclaration);
              return;
            }

            if (auto *metatype = type->getAs<AnyMetatypeType>()) {
              processTypeForClientRequirements(
                  metatype->getInstanceType(),
                  ClientDeclarationRequirement::Kind::OpaqueDeclaration, useDC,
                  rootRequiringDeclaration, leafRequiringDeclaration);
              return;
            }

            if (auto *existential = type->getAs<ExistentialType>()) {
              processTypeForClientRequirements(
                  existential->getConstraintType(),
                  ClientDeclarationRequirement::Kind::OpaqueDeclaration, useDC,
                  rootRequiringDeclaration, leafRequiringDeclaration);
              return;
            }

            if (auto *nominalType = type->getAs<NominalType>()) {
              if (auto parent = nominalType->getParent())
                processTypeForClientRequirements(
                    parent,
                    ClientDeclarationRequirement::Kind::OpaqueDeclaration,
                    useDC, rootRequiringDeclaration, leafRequiringDeclaration);
              for (auto argument : nominalType->getGenericArgs())
                processTypeForClientRequirements(
                    argument,
                    ClientDeclarationRequirement::Kind::OpaqueDeclaration,
                    useDC, rootRequiringDeclaration, leafRequiringDeclaration);
            }

            if (requirementKind ==
                    ClientDeclarationRequirement::Kind::OpaqueDeclaration ||
                isHidden)
              return;

            auto *nominal = type->getAnyNominal();
            if (!nominal || nominal->hasClangNode())
              return;

            auto *typeModule = nominal->getModuleContext();
            // A resilient type's declaration is sufficient for clients to
            // manipulate its values without knowing its component layouts.
            if (typeModule != AnalyzedModule &&
                nominal->isResilient(const_cast<ModuleDecl *>(AnalyzedModule),
                                     swift::ResilienceExpansion::Minimal))
              return;

            auto canonicalType = type->getCanonicalType();
            if (!visitedTypes.insert(canonicalType.getPointer()).second)
              return;

            if (isa<StructDecl>(nominal) ||
                (isa<ClassDecl>(nominal) &&
                 nominal == rootRequiringDeclaration)) {
              auto substitutions = type->getContextSubstitutionMap();
              for (auto *property : nominal->getStoredProperties()) {
                auto storedType =
                    property->getInterfaceType().subst(substitutions);
                auto *storage = property->getModuleContext() == AnalyzedModule
                                    ? property
                                    : leafRequiringDeclaration;
                processTypeForClientRequirements(
                    storedType,
                    ClientDeclarationRequirement::Kind::
                        AbstractTypeLayoutDeclaration,
                    property->getDeclContext(), rootRequiringDeclaration,
                    storage);
              }
            } else if (auto *enumDecl = dyn_cast<EnumDecl>(nominal)) {
              auto substitutions = type->getContextSubstitutionMap();
              for (auto *element : enumDecl->getAllElements()) {
                if (element->isIndirect() || enumDecl->isIndirect())
                  continue;
                if (auto payloadType = element->getPayloadInterfaceType()) {
                  auto *storage = element->getModuleContext() == AnalyzedModule
                                      ? element
                                      : leafRequiringDeclaration;
                  processTypeForClientRequirements(
                      payloadType.subst(substitutions),
                      ClientDeclarationRequirement::Kind::
                          AbstractTypeLayoutDeclaration,
                      element->getDeclContext(), rootRequiringDeclaration,
                      storage);
                }
              }
            }
          };

  std::function<void(Decl *)> processDeclaration = [&](Decl *declaration) {
    Result.recordRequirement(declaration,
                             ClientDeclarationRequirement::forNone());

    if (auto *value = dyn_cast<ValueDecl>(declaration)) {
      auto accessScope = value->getFormalAccessScope(
          /*useDC=*/nullptr, /*treatUsableFromInlineAsPublic=*/true);
      if (accessScope.isPublic() || accessScope.isPackage())
        Result.recordRequirement(
            declaration,
            ClientDeclarationRequirement::forFullDeclaration());
    }

    if (auto *nominal = dyn_cast<NominalTypeDecl>(declaration)) {
      auto abiAccess = nominal->getFormalAccessScope(
          /*useDC=*/nullptr, /*treatUsableFromInlineAsPublic=*/true);
      bool isABIAccessibleValueType =
          (abiAccess.isPublic() || abiAccess.isPackage()) &&
          isa<StructDecl, EnumDecl>(nominal);
      bool isPublicClass =
          isa<ClassDecl>(nominal) &&
          nominal
              ->getFormalAccessScope(
                  /*useDC=*/nullptr,
                  /*treatUsableFromInlineAsPublic=*/false)
              .isPublic();
      if (isABIAccessibleValueType || isPublicClass) {
        processTypeForClientRequirements(
            nominal->getDeclaredInterfaceType(),
            ClientDeclarationRequirement::Kind::
                AbstractTypeLayoutDeclaration,
            nominal, nominal,
            /*leafRequiringDeclaration=*/nullptr);
      }
    }

    if (auto *iterable = dyn_cast<IterableDeclContext>(declaration)) {
      for (auto *member : iterable->getAllMembers())
        processDeclaration(member);
    }
  };

  SmallVector<const FileUnit *, 8> files;
  for (auto *file : AnalyzedModule->getFiles()) {
    files.push_back(file);
    if (auto *synthesizedFile = file->getSynthesizedFile())
      files.push_back(synthesizedFile);
  }

  for (auto *file : files) {
    SmallVector<Decl *, 32> fileDeclarations;
    file->getTopLevelDeclsWithAuxiliaryDecls(fileDeclarations);
    for (auto *declaration : fileDeclarations)
      processDeclaration(declaration);
  }
}

const ClientDeclarationRequirements &
swift::getClientDeclarationRequirements(const ModuleDecl *module) {
  auto &cache = module->getASTContext()
                    .getGlobalCache()
                    .clientDeclarationRequirements;
  auto existing = cache.find(module);
  if (existing != cache.end())
    return *existing->second;

  auto requirements = std::shared_ptr<ClientDeclarationRequirements>(
      new ClientDeclarationRequirements(module));
  ClientDeclarationRequirementsBuilder(module, *requirements).run();
  auto [inserted, wasInserted] = cache.try_emplace(module, requirements);
  assert(wasInserted);
  return *inserted->second;
}
