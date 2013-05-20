//===--- ClangImporter.cpp - Import Clang Modules -------------------------===//
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
  /// \param searchPaths Additional paths to search for user modules.
  ///
  /// \returns a new Clang module importer, or null (with a diagnostic) if
  /// an error occurred.
  static ClangImporter *create(ASTContext &ctx, StringRef sdkroot,
                               StringRef targetTriple,
                               StringRef moduleCachePath,
                               ArrayRef<std::string> searchPaths = {});

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
                             ArrayRef<std::pair<Identifier, SourceLoc>> path);

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
                           SmallVectorImpl<ValueDecl*> &result);
  
  /// \brief Look for visible declarations in the Clang translation unit.
  ///
  /// \param consumer The VisibleDeclConsumer that will be fed decls as they
  ///   are found.
  void lookupVisibleDecls(clang::VisibleDeclConsumer &consumer);

  /// \brief Load extensions to the given nominal type.
  ///
  /// \param nominal The nominal type whose extensions should be loaded.
  ///
  /// \param previousGeneration The previous generation number. The AST already
  /// contains extensions loaded from any generation up to and including this
  /// one.
  virtual void loadExtensions(NominalTypeDecl *nominal,
                              unsigned previousGeneration);
};

}

#endif
