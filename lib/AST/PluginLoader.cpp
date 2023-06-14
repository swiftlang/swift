//===--- PluginLoader.cpp -------------------------------------------------===//
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

#include "swift/AST/PluginLoader.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/Config/config.h"

using namespace swift;

void PluginLoader::createModuleToExecutablePluginMap() {
  for (auto &elem : Ctx.SearchPathOpts.PluginSearchOpts) {
    if (auto *arg = elem.dyn_cast<PluginSearchOption::LoadPluginExecutable>()) {
      // Create a moduleName -> pluginPath mapping.
      assert(!arg->ExecutablePath.empty() && "empty plugin path");
      StringRef pathStr = Ctx.AllocateCopy(arg->ExecutablePath);
      for (auto moduleName : arg->ModuleNames) {
        ExecutablePluginPaths[Ctx.getIdentifier(moduleName)] = pathStr;
      }
    }
  }
}

void PluginLoader::setRegistry(PluginRegistry *newValue) {
  assert(Registry == nullptr && "Too late to set a new plugin registry");
  Registry = newValue;
}

PluginRegistry *PluginLoader::getRegistry() {
  // Create a new one if it hasn't been set.
  if (!Registry) {
    Registry = new PluginRegistry();
    OwnedRegistry.reset(Registry);
  }

  assert(Registry != nullptr);
  return Registry;
}

std::pair<std::string, std::string>
PluginLoader::lookupPluginByModuleName(Identifier moduleName) {
  auto fs = Ctx.SourceMgr.getFileSystem();

  // Look for 'lib${module name}(.dylib|.so)'.
  // FIXME: Shared library prefix might be different between platforms.
  SmallString<64> pluginLibBasename;
  pluginLibBasename.append("lib");
  pluginLibBasename.append(moduleName.str());
  pluginLibBasename.append(LTDL_SHLIB_EXT);

  // FIXME: Should we create a lookup table keyed by module name?
  for (auto &entry : Ctx.SearchPathOpts.PluginSearchOpts) {
    using namespace PluginSearchOption;
    // Try '-load-plugin-library'.
    if (auto *val = entry.dyn_cast<LoadPluginLibrary>()) {
      if (llvm::sys::path::filename(val->LibraryPath) == pluginLibBasename) {
        return {val->LibraryPath, ""};
      }
      continue;
    }

    // Try '-load-plugin-executable'.
    if (auto *v = entry.dyn_cast<LoadPluginExecutable>()) {
      auto found = ExecutablePluginPaths.find(moduleName);
      if (found != ExecutablePluginPaths.end()) {
        return {"", std::string(found->second)};
      }
      continue;
    }

    // Try '-plugin-path'.
    if (auto *v = entry.dyn_cast<PluginPath>()) {
      SmallString<128> fullPath(v->SearchPath);
      llvm::sys::path::append(fullPath, pluginLibBasename);
      if (fs->exists(fullPath)) {
        return {std::string(fullPath), ""};
      }
      continue;
    }

    // Try '-external-plugin-path'.
    if (auto *v = entry.dyn_cast<ExternalPluginPath>()) {
      SmallString<128> fullPath(v->SearchPath);
      llvm::sys::path::append(fullPath, pluginLibBasename);
      if (fs->exists(fullPath)) {
        return {std::string(fullPath), v->ServerPath};
      }
      continue;
    }
  }

  return {};
}

LoadedLibraryPlugin *PluginLoader::loadLibraryPlugin(StringRef path) {
  auto fs = Ctx.SourceMgr.getFileSystem();
  SmallString<128> resolvedPath;
  if (auto err = fs->getRealPath(path, resolvedPath)) {
    Ctx.Diags.diagnose(SourceLoc(), diag::compiler_plugin_not_loaded, path,
                       err.message());
    return nullptr;
  }

  // Track the dependency.
  if (DepTracker)
    DepTracker->addDependency(resolvedPath, /*IsSystem=*/false);

  // Load the plugin.
  auto plugin = getRegistry()->loadLibraryPlugin(resolvedPath);
  if (!plugin) {
    Ctx.Diags.diagnose(SourceLoc(), diag::compiler_plugin_not_loaded, path,
                       llvm::toString(plugin.takeError()));
    return nullptr;
  }

  return plugin.get();
}

LoadedExecutablePlugin *PluginLoader::loadExecutablePlugin(StringRef path) {
  auto fs = Ctx.SourceMgr.getFileSystem();
  SmallString<128> resolvedPath;
  if (auto err = fs->getRealPath(path, resolvedPath)) {
    Ctx.Diags.diagnose(SourceLoc(), diag::compiler_plugin_not_loaded, path,
                       err.message());
    return nullptr;
  }

  // Track the dependency.
  if (DepTracker)
    DepTracker->addDependency(resolvedPath, /*IsSystem=*/false);

  // Load the plugin.
  auto plugin = getRegistry()->loadExecutablePlugin(resolvedPath);
  if (!plugin) {
    Ctx.Diags.diagnose(SourceLoc(), diag::compiler_plugin_not_loaded, path,
                       llvm::toString(plugin.takeError()));
    return nullptr;
  }

  return plugin.get();
}
