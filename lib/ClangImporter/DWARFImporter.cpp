//===--- DWARFImporter.cpp - Import Clang modules from DWARF --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ImporterImpl.h"
#include "swift/AST/Module.h"

using namespace swift;

void DWARFImporterDelegate::anchor() {}

/// Represents a Clang module that was "imported" from debug info. Since all the
/// loading of types is done on demand, this class is effectively empty.
class DWARFModuleUnit final : public LoadedFile {
  ~DWARFModuleUnit() = default;
  ClangImporter::Implementation &owner;

public:
  DWARFModuleUnit(ModuleDecl &M, ClangImporter::Implementation &owner)
      : LoadedFile(FileUnitKind::DWARFModule, M), owner(owner) {}

  virtual bool isSystemModule() const override { return false; }

  /// Forwards the request to the ClangImporter, which forwards it to the
  /// DWARFimporterDelegate.
  virtual void
  lookupValue(ModuleDecl::AccessPathTy accessPath, DeclName name,
              NLKind lookupKind,
              SmallVectorImpl<ValueDecl *> &results) const override {
    owner.lookupValueDWARF(accessPath, name, lookupKind, results);
  }

  virtual TypeDecl *
  lookupNestedType(Identifier name,
                   const NominalTypeDecl *baseType) const override {
    return nullptr;
  }

  virtual void lookupVisibleDecls(ModuleDecl::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer,
                                  NLKind lookupKind) const override {}

  virtual void
  lookupClassMembers(ModuleDecl::AccessPathTy accessPath,
                     VisibleDeclConsumer &consumer) const override {}

  virtual void
  lookupClassMember(ModuleDecl::AccessPathTy accessPath, DeclName name,
                    SmallVectorImpl<ValueDecl *> &decls) const override {}

  void lookupObjCMethods(
      ObjCSelector selector,
      SmallVectorImpl<AbstractFunctionDecl *> &results) const override {}

  virtual void
  getTopLevelDecls(SmallVectorImpl<Decl *> &results) const override {}

  virtual void
  getDisplayDecls(SmallVectorImpl<Decl *> &results) const override {}

  virtual void
  getImportedModules(SmallVectorImpl<ModuleDecl::ImportedModule> &imports,
                     ModuleDecl::ImportFilter filter) const override {}

  virtual void getImportedModulesForLookup(
      SmallVectorImpl<ModuleDecl::ImportedModule> &imports) const override {};

  virtual void collectLinkLibraries(
      ModuleDecl::LinkLibraryCallback callback) const override {};

  Identifier
  getDiscriminatorForPrivateValue(const ValueDecl *D) const override {
    llvm_unreachable("no private decls in Clang modules");
  }

  virtual StringRef getFilename() const override { return ""; }

  virtual const clang::Module *getUnderlyingClangModule() const override {
    return nullptr;
  }

  static bool classof(const FileUnit *file) {
    return file->getKind() == FileUnitKind::DWARFModule;
  }
  static bool classof(const DeclContext *DC) {
    return isa<FileUnit>(DC) && classof(cast<FileUnit>(DC));
  }
};

ModuleDecl *ClangImporter::Implementation::loadModuleDWARF(
    SourceLoc importLoc, ArrayRef<std::pair<Identifier, SourceLoc>> path) {
  // There's no importing from debug info if no importer is installed.
  if (!DWARFImporter)
    return nullptr;

  // FIXME: Implement submodule support!
  Identifier name = path[0].first;
  auto it = DWARFModuleUnits.find(name);
  if (it != DWARFModuleUnits.end())
    return it->second->getParentModule();

  auto *decl = ModuleDecl::create(name, SwiftContext);
  decl->setIsNonSwiftModule();
  decl->setHasResolvedImports();
  auto wrapperUnit = new (SwiftContext) DWARFModuleUnit(*decl, *this);
  DWARFModuleUnits.insert({name, wrapperUnit});
  decl->addFile(*wrapperUnit);

  // Force load overlay modules for all imported modules.
  decl->forAllVisibleModules({}, [](ModuleDecl::ImportedModule import) {});

  // Register the module with the ASTContext so it is available for lookups.
  ModuleDecl *&loaded = SwiftContext.LoadedModules[name];
  if (!loaded)
    loaded = decl;

  return decl;
}

void ClangImporter::Implementation::lookupValueDWARF(
    ModuleDecl::AccessPathTy accessPath, DeclName name, NLKind lookupKind,
    SmallVectorImpl<ValueDecl *> &results) {
  if (!swift::ModuleDecl::matchesAccessPath(accessPath, name))
    return;

  if (lookupKind != NLKind::QualifiedLookup)
    return;

  if (!DWARFImporter)
    return;

  SmallVector<clang::Decl *, 4> decls;
  DWARFImporter->lookupValue(name.getBaseIdentifier().str(), llvm::None, decls);
  for (auto *clangDecl : decls) {
    auto *namedDecl = dyn_cast_or_null<clang::NamedDecl>(clangDecl);
    if (!namedDecl)
      continue;
    auto *swiftDecl = cast_or_null<ValueDecl>(
        importDeclReal(namedDecl->getMostRecentDecl(), CurrentVersion));
    if (!swiftDecl)
      continue;

    if (swiftDecl->getFullName().matchesRef(name) &&
        swiftDecl->getDeclContext()->isModuleScopeContext())
      results.push_back(swiftDecl);
  }
}

void ClangImporter::Implementation::lookupTypeDeclDWARF(
    StringRef rawName, ClangTypeKind kind,
    llvm::function_ref<void(TypeDecl *)> receiver) {
  if (!DWARFImporter)
    return;

  SmallVector<clang::Decl *, 1> decls;
  DWARFImporter->lookupValue(rawName, kind, decls);
  for (auto *clangDecl : decls) {
    if (!isa<clang::TypeDecl>(clangDecl) &&
        !isa<clang::ObjCContainerDecl>(clangDecl) &&
        !isa<clang::ObjCCompatibleAliasDecl>(clangDecl)) {
      continue;
    }
    auto *namedDecl = dyn_cast_or_null<clang::NamedDecl>(clangDecl);
    if (!namedDecl)
      continue;
    Decl *importedDecl = cast_or_null<ValueDecl>(
        importDeclReal(namedDecl->getMostRecentDecl(), CurrentVersion));

    if (auto *importedType = dyn_cast_or_null<TypeDecl>(importedDecl))
      receiver(importedType);
  }
}
