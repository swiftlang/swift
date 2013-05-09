//===--- SerializedModuleLoader.h - Import Swift modules --------*- c++ -*-===//
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

#ifndef SWIFT_SERIALIZATION_MODULELOADER_H
#define SWIFT_SERIALIZATION_MODULELOADER_H

#include "swift/AST/ModuleLoader.h"

namespace swift {

class ASTContext;
class Module;
  
/// \brief Imports serialized Swift modules into an ASTContext.
class SerializedModuleLoader : public ModuleLoader {
private:
  ASTContext &Ctx;

  explicit SerializedModuleLoader(ASTContext &ctx) : Ctx(ctx) {}

public:
  /// \brief Create a new importer that can load serialized Swift modules
  /// into the given ASTContext.
  static SerializedModuleLoader *create(ASTContext &ctx) {
    return new SerializedModuleLoader(ctx);
  }

  SerializedModuleLoader(const SerializedModuleLoader &) = delete;
  SerializedModuleLoader(SerializedModuleLoader &&) = delete;
  SerializedModuleLoader &operator=(const SerializedModuleLoader &) = delete;
  SerializedModuleLoader &operator=(SerializedModuleLoader &&) = delete;

  /// \brief Import a module with the given module path.
  ///
  /// \param importLoc The location of the 'import' keyword.
  ///
  /// \param path A sequence of (identifier, location) pairs that denote
  /// the dotted module name to load, e.g., AppKit.NSWindow.
  ///
  /// \returns the module referenced, if it could be loaded. Otherwise,
  /// emits a diagnostic and returns NULL.
  virtual Module *
  loadModule(SourceLoc importLoc,
             ArrayRef<std::pair<Identifier, SourceLoc>> path) override;
};

}

#endif
