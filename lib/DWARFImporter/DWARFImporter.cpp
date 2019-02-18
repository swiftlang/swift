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

using namespace swift;

std::unique_ptr<DWARFImporter>
DWARFImporter::create(ASTContext &ctx,
                      const ClangImporterOptions &importerOpts,
                      DependencyTracker *tracker) {
  std::unique_ptr<DWARFImporter> importer{
    new DWARFImporter(ctx, importerOpts, tracker)
  };
  return importer;
}

class DWARFModuleUnit final : public LoadedFile {

  ~DWARFModuleUnit() = default;

public:
  DWARFModuleUnit(ModuleDecl &M)
      : LoadedFile(FileUnitKind::DWARFModule, M) {}

  virtual bool isSystemModule() const override { return false; }

  virtual void
  lookupValue(ModuleDecl::AccessPathTy accessPath, DeclName name,
              NLKind lookupKind,
              SmallVectorImpl<ValueDecl *> &results) const override {}

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
  clang::ASTContext *ClangASTContext = nullptr;
  clang::CompilerInstance *Instance = nullptr;
  clang::Preprocessor *PP = nullptr;
  clang::Sema *Sema = nullptr;

  llvm::DenseMap<Identifier, DWARFModuleUnit *> ModuleWrappers;
  Implementation(ASTContext &ctx, const ClangImporterOptions &clangImporterOpts)
      : SwiftContext(ctx) {}

  ModuleDecl *loadModule(SourceLoc importLoc,
                         ArrayRef<std::pair<Identifier, SourceLoc>> path) {
    // FIXME: Implement submodule support!
    Identifier name = path[0].first;

    auto it = ModuleWrappers.find(name);
    if (it != ModuleWrappers.end())
      return it->second->getParentModule();

    auto *decl = ModuleDecl::create(name, SwiftContext);
    // Silence error messages about testably importing a Clang module.
    decl->setTestingEnabled();
    decl->setHasResolvedImports();
    auto wrapperUnit = new (SwiftContext) DWARFModuleUnit(*decl);
    ModuleWrappers.insert({name, wrapperUnit});
    decl->addFile(*wrapperUnit);

    // Force load adapter modules for all imported modules.
    decl->forAllVisibleModules({}, [](ModuleDecl::ImportedModule import) {});

    return decl;
  }
};

DWARFImporter::DWARFImporter(ASTContext &ctx,
                             const ClangImporterOptions &clangImporterOpts,
                             DependencyTracker *tracker)
    : ClangModuleLoader(tracker),
      Impl(*new Implementation(ctx, clangImporterOpts)) {}

DWARFImporter::~DWARFImporter() { delete &Impl; }

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
  return *Impl.ClangASTContext;
}
clang::Preprocessor &DWARFImporter::getClangPreprocessor() const {
  return *Impl.PP;
}
clang::Sema &DWARFImporter::getClangSema() const { return *Impl.Sema; }
const clang::CompilerInstance &DWARFImporter::getClangInstance() const {
  return *Impl.Instance;
}

void DWARFImporter::printStatistics() const {}
