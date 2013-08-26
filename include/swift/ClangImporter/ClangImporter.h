//===--- ClangImporter.cpp - Import Clang Modules --------------*- C++ -*--===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements support for loading Clang modules into Swift.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_CLANG_IMPORTER_H
#define SWIFT_CLANG_IMPORTER_H

#include "swift/AST/ModuleLoader.h"

namespace clang {
  class VisibleDeclConsumer;
  class TargetInfo;
}

namespace swift {

class ASTContext;
class Module;
class NominalTypeDecl;

/// \brief Class that imports Clang modules into Swift, mapping directly
/// from Clang ASTs over to Swift ASTs.
class ClangImporter : public ModuleLoader {
public:
  struct Implementation;

private:
  Implementation &Impl;
  friend class ClangModule;

  ClangImporter(ASTContext &ctx);

public:
  /// \brief Create a new Clang importer that can import a suitable Clang
  /// module into the given ASTContext.
  ///
  /// \param ctx The ASTContext into which the module will be imported.
  ///
  /// \param sdkroot The path to the SDK from which modules will be imported.
  /// The SDK must support building modules with Clang.
  ///
  /// \param targetTriple The target triple to use for the import.
  ///
  /// \param moduleCachePath The module cache path.
  ///
  /// \param swiftRuntimeIncludePath The /path/to/lib/swift. Can be empty if
  /// \c overrideResourceDir is set instead.
  ///
  /// \param importSearchPaths Additional paths to search for user modules.
  ///
  /// \param frameworkSearchPaths Additional paths to search for user
  /// frameworks.
  ///
  /// \param overrideResourceDir If nonempty, the path to use to locate the
  /// resources for Clang.  This should at least have an include/ subdirectory
  /// containing Clang's built-in headers.  The default (empty) argument causes
  /// this function to assume that the headers are inside the
  /// \c swiftRuntimeIncludePath.
  ///
  /// \returns a new Clang module importer, or null (with a diagnostic) if
  /// an error occurred.
  static ClangImporter *create(ASTContext &ctx, StringRef sdkroot,
                               StringRef targetTriple,
                               StringRef swiftRuntimeIncludePath,
                               StringRef moduleCachePath,
                               ArrayRef<std::string> importSearchPaths = {},
                               ArrayRef<std::string> frameworkSearchPaths = {},
                               StringRef overrideResourceDir = StringRef());

  ClangImporter(const ClangImporter &) = delete;
  ClangImporter(ClangImporter &&) = delete;
  ClangImporter &operator=(const ClangImporter &) = delete;
  ClangImporter &operator=(ClangImporter &&) = delete;

  ~ClangImporter();

  /// \brief Import a module with the given module path.
  ///
  /// Clang modules will be imported using the Objective-C ARC dialect,
  /// with all warnings disabled.
  ///
  /// \param importLoc The location of the 'import' keyword.
  ///
  /// \param path A sequence of (identifier, location) pairs that denote
  /// the dotted module name to load, e.g., AppKit.NSWindow.
  ///
  /// \returns the module referenced, if it could be loaded. Otherwise,
  /// emits a diagnostic and returns NULL.
  virtual Module *loadModule(SourceLoc importLoc,
                             ArrayRef<std::pair<Identifier, SourceLoc>> path)
                                                                      override;

  /// \brief Look for declarations associated with the given name.
  ///
  /// \param module The module to search.
  ///
  /// \param accessPath The access path used to refer to the name within this
  /// (top-level) module.
  ///
  /// \param name The name we're searching for.
  ///
  /// \param lookupKind Whether we're performing qualified vs. unqualified
  /// lookup.
  ///
  /// \param result Will be populated with the results of name lookup.
  virtual void lookupValue(Module *module,
                           Module::AccessPathTy accessPath, Identifier name,
                           NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &result) override;
  
  /// \brief Look for visible declarations in the Clang translation unit.
  ///
  /// \param consumer The VisibleDeclConsumer that will be fed decls as they
  ///   are found.
  void lookupVisibleDecls(clang::VisibleDeclConsumer &consumer) const;

  /// \brief Look for visible declarations in the Clang translation unit and
  /// import them as Swift decls.
  ///
  /// \param Consumer The VisibleDeclConsumer that will be fed decls as they
  /// are found and imported.
  void lookupVisibleDecls(VisibleDeclConsumer &Consumer) const;

  void lookupVisibleDecls(const Module *M,
                          Module::AccessPathTy AccessPath,
                          VisibleDeclConsumer &Consumer,
                          NLKind LookupKind) override;

  /// \brief Load extensions to the given nominal type.
  ///
  /// \param nominal The nominal type whose extensions should be loaded.
  ///
  /// \param previousGeneration The previous generation number. The AST already
  /// contains extensions loaded from any generation up to and including this
  /// one.
  virtual void loadExtensions(NominalTypeDecl *nominal,
                              unsigned previousGeneration) override;

  virtual void getImportedModules(
    const Module *module,
    SmallVectorImpl<Module::ImportedModule> &exports,
    bool includePrivate) override;
  
  virtual void lookupClassMembers(const Module *module,
                                  Module::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer) override;
  
  virtual void lookupClassMember(const Module *module,
                                 Module::AccessPathTy accessPath,
                                 Identifier name,
                                 SmallVectorImpl<ValueDecl*> &results) override;

  virtual void getLinkLibraries(const Module *module,
                                Module::LinkLibraryCallback callback) override;

  virtual void getDisplayDecls(const Module *module,
                               SmallVectorImpl<Decl*> &results) override;
  
  clang::TargetInfo &getTargetInfo() const;
};

typedef decltype(&ClangImporter::create) ClangImporterCtorTy;
ClangImporterCtorTy getClangImporterCtor();

}

#endif
