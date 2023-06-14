//===--- PluginLoader.h -----------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_PLUGIN_LOADER_H
#define SWIFT_AST_PLUGIN_LOADER_H

#include "swift/AST/ModuleLoader.h"
#include "swift/AST/PluginRegistry.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

class ASTContext;

/// Compiler plugin loader tied to an ASTContext. This is responsible for:
///  * Find plugins based on the module name
///  * Load plugins resolving VFS paths
///  * Track plugin dependencies
class PluginLoader {
  /// Plugin registry. Lazily populated by get/setRegistry().
  /// NOTE: Do not reference this directly. Use getRegistry().
  PluginRegistry *Registry = nullptr;

  /// `Registry` storage if this owns it.
  std::unique_ptr<PluginRegistry> OwnedRegistry = nullptr;

  ASTContext &Ctx;
  DependencyTracker *DepTracker;

  /// Map a module name to an executable plugin path that provides the module.
  llvm::DenseMap<swift::Identifier, llvm::StringRef> ExecutablePluginPaths;

  void createModuleToExecutablePluginMap();

public:
  PluginLoader(ASTContext &Ctx, DependencyTracker *DepTracker)
      : Ctx(Ctx), DepTracker(DepTracker) {
    createModuleToExecutablePluginMap();
  }

  void setRegistry(PluginRegistry *newValue);
  PluginRegistry *getRegistry();

  /// Lookup a plugin that can handle \p moduleName and return the path(s) to
  /// it. The path returned can be loaded by 'load(Library|Executable)Plugin()'.
  /// The return value is a pair of a "library path" and a "executable path".
  ///
  ///  * (libPath: empty, execPath: empty) - plugin not found.
  ///  * (libPath: some,  execPath: empty) - load the library path by
  ///    'loadLibraryPlugin()'.
  ///  * (libPath: empty, execPath: some) - load the executable path by
  ///    'loadExecutablePlugin()'.
  ///  * (libPath: some,  execPath: some) - load the executable path by
  ///    'loadExecutablePlugin()' and let the plugin load the libPath via IPC.
  std::pair<std::string, std::string>
  lookupPluginByModuleName(Identifier moduleName);

  /// Load the specified dylib plugin path resolving the path with the
  /// current VFS. If it fails to load the plugin, a diagnostic is emitted, and
  /// returns a nullptr.
  /// NOTE: This method is idempotent. If the plugin is already loaded, the same
  /// instance is simply returned.
  LoadedLibraryPlugin *loadLibraryPlugin(llvm::StringRef path);

  /// Launch the specified executable plugin path resolving the path with the
  /// current VFS. If it fails to load the plugin, a diagnostic is emitted, and
  /// returns a nullptr.
  /// NOTE: This method is idempotent. If the plugin is already loaded, the same
  /// instance is simply returned.
  LoadedExecutablePlugin *loadExecutablePlugin(llvm::StringRef path);
};

} // namespace swift

#endif // SWIFT_AST_PLUGIN_LOADER_H
