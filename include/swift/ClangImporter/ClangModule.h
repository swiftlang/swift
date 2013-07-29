//===--- ClangModule.h - An imported Clang module ---------------*- c++ -*-===//
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
#ifndef SWIFT_CLANGIMPORTER_CLANGMODULE_H
#define SWIFT_CLANGIMPORTER_CLANGMODULE_H

#include "swift/AST/Module.h"

namespace clang {
  class Module;
}

namespace swift {

class ASTContext;
class ClangImporter;
class Component;
class ModuleLoader;

/// \brief Represents a Clang module that has been imported into Swift.
class ClangModule : public LoadedModule {
  friend class ClangImporter;
  clang::Module *clangModule;

public:
  ClangModule(ASTContext &ctx, ModuleLoader &owner, Component *comp,
              clang::Module *clangModule);

  /// \brief Retrieve the underlying Clang module.
  // FIXME: Remove this.
  clang::Module *getClangModule() const { return clangModule; }

  /// Returns true if this is a top-level Clang module (not a submodule).
  bool isTopLevel() const;

  /// Returns the name of the enclosing top-level module without importing it.
  ///
  /// If this module is itself a top-level module, returns this module's name.
  StringRef getTopLevelModuleName() const;

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::ClangModule;
  }
};

}

#endif
