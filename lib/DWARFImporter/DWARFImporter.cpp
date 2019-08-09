//===--- DWARFImporter.cpp - Import Clang modules from DWARF --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/DWARFImporter/DWARFImporter.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "clang/CodeGen/ObjectFilePCHContainerOperations.h"
#include "clang/Frontend/FrontendActions.h"
#include "../ClangImporter/ImporterImpl.h"

using namespace swift;

std::unique_ptr<DWARFImporter>
DWARFImporter::create(ASTContext &ctx, const ClangImporterOptions &importerOpts,
                      std::unique_ptr<DWARFImporterDelegate> delegate,
                      DependencyTracker *tracker) {
  std::unique_ptr<DWARFImporter> importer{
    new DWARFImporter(ctx, importerOpts, std::move(delegate), tracker)
  };
  return importer;
}

class DWARFModuleUnit final : public LoadedFile {

  ~DWARFModuleUnit() = default;
  DWARFImporter::Implementation &owner;

public:
  DWARFModuleUnit(ModuleDecl &M, DWARFImporter::Implementation &owner)
      : LoadedFile(FileUnitKind::DWARFModule, M), owner(owner) {}

  virtual bool isSystemModule() const override { return false; }

  virtual void
  lookupValue(ModuleDecl::AccessPathTy accessPath, DeclName name,
              NLKind lookupKind,
              SmallVectorImpl<ValueDecl *> &results) const override;

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

class DWARFImporter::Implementation {
public:
  ASTContext &SwiftContext;

  std::unique_ptr<DWARFImporterDelegate> delegate;
  ClangImporter &clangImporter;

  llvm::DenseMap<Identifier, DWARFModuleUnit *> ModuleWrappers;
  Implementation(ASTContext &ctx, const ClangImporterOptions &clangImporterOpts,
                 std::unique_ptr<DWARFImporterDelegate> delegate,
                 DependencyTracker *tracker)
      : SwiftContext(ctx), delegate(std::move(delegate)),
        clangImporter(
            *static_cast<ClangImporter *>(ctx.getClangModuleLoader())) {}

  ModuleDecl *loadModule(SourceLoc importLoc,
                         ArrayRef<std::pair<Identifier, SourceLoc>> path) {
    // FIXME: Implement submodule support!
    Identifier name = path[0].first;

    auto it = ModuleWrappers.find(name);
    if (it != ModuleWrappers.end())
      return it->second->getParentModule();

    auto *decl = ModuleDecl::create(name, SwiftContext);
    decl->setIsNonSwiftModule();
    decl->setHasResolvedImports();
    auto wrapperUnit = new (SwiftContext) DWARFModuleUnit(*decl, *this);
    ModuleWrappers.insert({name, wrapperUnit});
    decl->addFile(*wrapperUnit);

    // Force load overlay modules for all imported modules.
    decl->forAllVisibleModules({}, [](ModuleDecl::ImportedModule import) {});

    // Register the module with the ASTContext so it is available for lookups.
    ModuleDecl *&loaded = SwiftContext.LoadedModules[name];
    if (!loaded)
      loaded = decl;

    return decl;
  }

public:
  ValueDecl *importDecl(clang::Decl *clangDecl) {
    auto *namedDecl = dyn_cast_or_null<clang::NamedDecl>(clangDecl);
    if (!namedDecl)
      return nullptr;
    return cast_or_null<ValueDecl>(clangImporter.Impl.importDeclReal(
        namedDecl->getMostRecentDecl(), clangImporter.Impl.CurrentVersion));
  }

  void lookupValue(ModuleDecl::AccessPathTy accessPath, DeclName name,
                   NLKind lookupKind, SmallVectorImpl<ValueDecl *> &results) {
    if (!swift::ModuleDecl::matchesAccessPath(accessPath, name))
      return;

    if (lookupKind != NLKind::QualifiedLookup)
      return;

    if (!delegate)
      return;

    SmallVector<clang::Decl *, 4> decls;
    delegate->lookupValue(name.getBaseIdentifier().str(), llvm::None, decls);
    for (auto *clangDecl : decls) {
      auto *decl = importDecl(clangDecl);
      if (!decl)
        continue;

      if (decl->getFullName().matchesRef(name) &&
          decl->getDeclContext()->isModuleScopeContext())
        results.push_back(decl);
    }
  }

  void lookupTypeDecl(StringRef rawName, Demangle::Node::Kind kind,
                      llvm::function_ref<void(TypeDecl *)> receiver) {
    SmallVector<clang::Decl *, 1> decls;
    delegate->lookupValue(rawName, kind, decls);
    for (auto *clangDecl : decls) {
      if (!isa<clang::TypeDecl>(clangDecl) &&
          !isa<clang::ObjCContainerDecl>(clangDecl) &&
          !isa<clang::ObjCCompatibleAliasDecl>(clangDecl)) {
        continue;
      }
      auto *imported = importDecl(clangDecl);
      if (auto *importedType = dyn_cast_or_null<TypeDecl>(imported))
        receiver(importedType);
    }
  }

  clang::ASTContext &getClangASTContext() const {
    return clangImporter.getClangASTContext();
  }
  clang::Preprocessor &getClangPreprocessor() const {
    return clangImporter.getClangPreprocessor();
  }
  clang::Sema &getClangSema() const { return clangImporter.getClangSema(); }
  const clang::CompilerInstance &getClangInstance() const {
    return clangImporter.getClangInstance();
  }
};

void DWARFModuleUnit::lookupValue(ModuleDecl::AccessPathTy accessPath,
                                  DeclName name, NLKind lookupKind,
                                  SmallVectorImpl<ValueDecl *> &results) const {
  owner.lookupValue(accessPath, name, lookupKind, results);
}

DWARFImporter::DWARFImporter(ASTContext &ctx,
                             const ClangImporterOptions &clangImporterOpts,
                             std::unique_ptr<DWARFImporterDelegate> delegate,
                             DependencyTracker *tracker)
    : ClangModuleLoader(tracker),
      Impl(*new Implementation(ctx, clangImporterOpts, std::move(delegate),
                               tracker)) {}

DWARFImporter::~DWARFImporter() { delete &Impl; }

void DWARFImporter::collectVisibleTopLevelModuleNames(
    SmallVectorImpl<Identifier> &names) const {}

bool DWARFImporter::canImportModule(std::pair<Identifier, SourceLoc> named) {
  return false;
}

bool DWARFImporter::addSearchPath(StringRef newSearchPath, bool isFramework,
                                  bool isSystem) {
  return false;
}

ModuleDecl *
DWARFImporter::loadModule(SourceLoc importLoc,
                          ArrayRef<std::pair<Identifier, SourceLoc>> path) {
  return Impl.loadModule(importLoc, path);
}

ValueDecl *DWARFImporter::importDecl(clang::Decl *clangDecl) {
  return Impl.importDecl(clangDecl);
}

void DWARFImporter::lookupValue(ModuleDecl::AccessPathTy accessPath,
                                DeclName name, NLKind lookupKind,
                                SmallVectorImpl<ValueDecl *> &results) {
  Impl.lookupValue(accessPath, name, lookupKind, results);
}

void DWARFImporter::lookupTypeDecl(
    StringRef rawName, Demangle::Node::Kind kind,
    llvm::function_ref<void(TypeDecl *)> receiver) {
  Impl.lookupTypeDecl(rawName, kind, receiver);
}

bool DWARFImporter::isInOverlayModuleForImportedModule(
    const DeclContext *overlayDC, const DeclContext *importedDC) {
  return false;
}

void DWARFImporter::loadExtensions(NominalTypeDecl *nominal,
                                   unsigned previousGeneration) {}

void DWARFImporter::loadObjCMethods(
    ClassDecl *classDecl, ObjCSelector selector, bool isInstanceMethod,
    unsigned previousGeneration,
    llvm::TinyPtrVector<AbstractFunctionDecl *> &methods) {}

ModuleDecl *DWARFImporter::getImportedHeaderModule() const { return nullptr; }

void DWARFImporter::verifyAllModules() {};

clang::ASTContext &DWARFImporter::getClangASTContext() const {
  return Impl.getClangASTContext();
}
clang::Preprocessor &DWARFImporter::getClangPreprocessor() const {
  return Impl.getClangPreprocessor();
}
clang::Sema &DWARFImporter::getClangSema() const { return Impl.getClangSema(); }
const clang::CompilerInstance &DWARFImporter::getClangInstance() const {
  return Impl.getClangInstance();
}

void DWARFImporter::printStatistics() const {}
