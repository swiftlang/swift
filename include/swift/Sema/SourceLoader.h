//===--- SourceLoader.h - Import .swift files as modules --------*- c++ -*-===//
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

#ifndef SWIFT_SEMA_SOURCELOADER_H
#define SWIFT_SEMA_SOURCELOADER_H

#include "swift/AST/ModuleLoader.h"

namespace swift {

class ASTContext;
class ModuleDecl;
  
/// \brief Imports serialized Swift modules into an ASTContext.
class SourceLoader : public ModuleLoader {
private:
  ASTContext &Ctx;
  bool SkipBodies;

  explicit SourceLoader(ASTContext &ctx, bool skipBodies, DependencyTracker *tracker)
    : ModuleLoader(tracker), Ctx(ctx), SkipBodies(skipBodies) {}

public:
  static std::unique_ptr<SourceLoader>
  create(ASTContext &ctx, bool skipBodies,
         DependencyTracker *tracker = nullptr) {
    return std::unique_ptr<SourceLoader>{
      new SourceLoader(ctx, skipBodies, tracker)
    };
  }

  SourceLoader(const SourceLoader &) = delete;
  SourceLoader(SourceLoader &&) = delete;
  SourceLoader &operator=(const SourceLoader &) = delete;
  SourceLoader &operator=(SourceLoader &&) = delete;

  /// \brief Import a module with the given module path.
  ///
  /// \param importLoc The location of the 'import' keyword.
  ///
  /// \param path A sequence of (identifier, location) pairs that denote
  /// the dotted module name to load, e.g., AppKit.NSWindow.
  ///
  /// \returns the module referenced, if it could be loaded. Otherwise,
  /// returns NULL.
  virtual ModuleDecl *
  loadModule(SourceLoc importLoc,
             ArrayRef<std::pair<Identifier, SourceLoc>> path) override;

  /// \brief Load extensions to the given nominal type.
  ///
  /// \param nominal The nominal type whose extensions should be loaded.
  ///
  /// \param previousGeneration The previous generation number. The AST already
  /// contains extensions loaded from any generation up to and including this
  /// one.
  virtual void loadExtensions(NominalTypeDecl *nominal,
                              unsigned previousGeneration) override;

  virtual void loadObjCMethods(
                 ClassDecl *classDecl,
                 ObjCSelector selector,
                 bool isInstanceMethod,
                 unsigned previousGeneration,
                 llvm::TinyPtrVector<AbstractFunctionDecl *> &methods) override
  {
    // Parsing populates the Objective-C method tables.
  }
};

}

#endif
