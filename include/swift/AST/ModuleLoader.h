//===--- ModuleLoader.h - Module Loader Interface ---------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/TinyPtrVector.h"

namespace clang {
class DependencyCollector;
}

namespace swift {

class AbstractFunctionDecl;
class ClangImporterOptions;
class ClassDecl;
class ModuleDecl;
class NominalTypeDecl;

enum class KnownProtocolKind : uint8_t;

enum class Bridgeability : unsigned {
  /// This context does not permit bridging at all.  For example, the
  /// target of a C pointer.
  None,

  /// This context permits all kinds of bridging.  For example, the
  /// imported result of a method declaration.
  Full
};

/// Records dependencies on files outside of the current module;
/// implemented in terms of a wrapped clang::DependencyCollector.
class DependencyTracker {
  std::shared_ptr<clang::DependencyCollector> clangCollector;
public:

  explicit DependencyTracker(bool TrackSystemDeps);

  /// Adds a file as a dependency.
  ///
  /// The contents of \p File are taken literally, and should be appropriate
  /// for appearing in a list of dependencies suitable for tooling like Make.
  /// No path canonicalization is done.
  void addDependency(StringRef File, bool IsSystem);

  /// Fetches the list of dependencies.
  ArrayRef<std::string> getDependencies() const;

  /// Return the underlying clang::DependencyCollector that this
  /// class wraps.
  std::shared_ptr<clang::DependencyCollector> getClangCollector();
};

/// Abstract interface that loads named modules into the AST.
class ModuleLoader {
  virtual void anchor();

protected:
  DependencyTracker * const dependencyTracker;
  ModuleLoader(DependencyTracker *tracker) : dependencyTracker(tracker) {}

public:
  virtual ~ModuleLoader() = default;

  /// Check whether the module with a given name can be imported without
  /// importing it.
  ///
  /// Note that even if this check succeeds, errors may still occur if the
  /// module is loaded in full.
  virtual bool canImportModule(std::pair<Identifier, SourceLoc> named) = 0;

  /// Import a module with the given module path.
  ///
  /// \param importLoc The location of the 'import' keyword.
  ///
  /// \param path A sequence of (identifier, location) pairs that denote
  /// the dotted module name to load, e.g., AppKit.NSWindow.
  ///
  /// \returns the module referenced, if it could be loaded. Otherwise,
  /// emits a diagnostic and returns NULL.
  virtual
  ModuleDecl *loadModule(SourceLoc importLoc,
                         ArrayRef<std::pair<Identifier, SourceLoc>> path) = 0;

  /// Load extensions to the given nominal type.
  ///
  /// \param nominal The nominal type whose extensions should be loaded.
  ///
  /// \param previousGeneration The previous generation number. The AST already
  /// contains extensions loaded from any generation up to and including this
  /// one.
  virtual void loadExtensions(NominalTypeDecl *nominal,
                              unsigned previousGeneration) { }

  /// Load the methods within the given class that produce
  /// Objective-C class or instance methods with the given selector.
  ///
  /// \param classDecl The class in which we are searching for @objc methods.
  /// The search only considers this class and its extensions; not any
  /// superclasses.
  ///
  /// \param selector The selector to search for.
  ///
  /// \param isInstanceMethod Whether we are looking for an instance method
  /// (vs. a class method).
  ///
  /// \param previousGeneration The previous generation with which this
  /// callback was invoked. The list of methods will already contain all of
  /// the results from generations up and including \c previousGeneration.
  ///
  /// \param methods The list of @objc methods in this class that have this
  /// selector and are instance/class methods as requested. This list will be
  /// extended with any methods found in subsequent generations.
  virtual void loadObjCMethods(
                 ClassDecl *classDecl,
                 ObjCSelector selector,
                 bool isInstanceMethod,
                 unsigned previousGeneration,
                 llvm::TinyPtrVector<AbstractFunctionDecl *> &methods) = 0;

  /// Verify all modules loaded by this loader.
  virtual void verifyAllModules() { }
};

} // namespace swift

#endif
