//===--- ModuleLoader.h - Module Loader Interface ----------- -*- C++ -*- -===//
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
// This file implements an abstract interface for loading modules.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_MODULE_LOADER_H
#define SWIFT_AST_MODULE_LOADER_H

#include "swift/AST/Identifier.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
namespace swift {

class Module;
class NominalTypeDecl;

enum class KnownProtocolKind : uint8_t;
  
/// \brief Abstract interface that loads named modules into the AST.
class ModuleLoader : public llvm::RefCountedBaseVPTR {
  virtual void anchor();

public:
  virtual ~ModuleLoader() = default;

  /// \brief Import a module with the given module path.
  ///
  /// \param importLoc The location of the 'import' keyword.
  ///
  /// \param path A sequence of (identifier, location) pairs that denote
  /// the dotted module name to load, e.g., AppKit.NSWindow.
  ///
  /// \returns the module referenced, if it could be loaded. Otherwise,
  /// emits a diagnostic and returns NULL.
  virtual
  Module *loadModule(SourceLoc importLoc,
                     ArrayRef<std::pair<Identifier, SourceLoc>> path) = 0;

  /// \brief Load extensions to the given nominal type.
  ///
  /// \param nominal The nominal type whose extensions should be loaded.
  ///
  /// \param previousGeneration The previous generation number. The AST already
  /// contains extensions loaded from any generation up to and including this
  /// one.
  virtual void loadExtensions(NominalTypeDecl *nominal,
                              unsigned previousGeneration) { };
  
  /// \brief Load decls that provide conformances to the given compiler-known
  /// protocol.
  ///
  /// \param kind The known protocol whose decls should be loaded.
  ///
  /// \param previousGeneration The previous generation number. The AST already
  /// contains decls conforming to this protocol loaded from any generation up
  /// to and including this one.
  virtual void loadDeclsConformingTo(KnownProtocolKind kind,
                                     unsigned previousGeneration) { };

  /// \brief Verify all modules loaded by this loader.
  virtual void verifyAllModules() { }
};

}


#endif
