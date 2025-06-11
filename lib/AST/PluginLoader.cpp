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
#include "swift/AST/DiagnosticsSema.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "llvm/Config/config.h"
#include "llvm/Support/PrefixMapper.h"
#include "llvm/Support/VirtualFileSystem.h"

using namespace swift;

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

/// Get plugin module name from \p path if the path looks like a shared library
/// path. Otherwise, returns an empty string.
static StringRef pluginModuleNameStringFromPath(StringRef path) {
  // Plugin library must be named 'lib${module name}(.dylib|.so|.dll)'.
  // FIXME: Shared library prefix might be different between platforms.
#if defined(_WIN32)
  constexpr StringRef libPrefix{};
  constexpr StringRef libSuffix = ".dll";
#else
  constexpr StringRef libPrefix = "lib";
  constexpr StringRef libSuffix = LTDL_SHLIB_EXT;
#endif

  StringRef filename = llvm::sys::path::filename(path);
  if (filename.starts_with(libPrefix) && filename.ends_with(libSuffix)) {
    // We don't check if the result it a valid identifier. Even if we put
    // invalid name in the lookup table, clients wound not be able to lookup
    // that name, thus harmless.
    return filename.drop_front(libPrefix.size()).drop_back(libSuffix.size());
  }
  return "";
}

static llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem>
getPluginLoadingFS(ASTContext &Ctx) {
  // If there is an immutable file system, using real file system to load plugin
  // as the FS in SourceMgr doesn't support directory iterator.
  if (Ctx.CASOpts.HasImmutableFileSystem)
    return llvm::vfs::getRealFileSystem();
  return Ctx.SourceMgr.getFileSystem();
}

llvm::DenseMap<Identifier, PluginLoader::PluginEntry> &
PluginLoader::getPluginMap() {
  if (PluginMap.has_value()) {
    return PluginMap.value();
  }

  // Create and populate the map.
  PluginMap.emplace();
  auto &map = PluginMap.value();

  // Helper function to try inserting an entry if there's no existing entry
  // associated with the module name.
  auto try_emplace = [&](StringRef moduleName, StringRef libPath,
                         StringRef execPath, bool overwrite = false) {
    auto moduleNameIdentifier = Ctx.getIdentifier(moduleName);
    if (map.find(moduleNameIdentifier) != map.end() && !overwrite) {
      // Specified module name is already in the map and no need to overwrite
      // the current value.
      return;
    }

    libPath = libPath.empty() ? "" : Ctx.AllocateCopy(libPath);
    execPath = execPath.empty() ? "" : Ctx.AllocateCopy(execPath);
    map[moduleNameIdentifier] = {libPath, execPath};
  };

  std::optional<llvm::PrefixMapper> mapper;
  if (!PathRemap.empty()) {
    SmallVector<llvm::MappedPrefix, 4> prefixes;
    llvm::MappedPrefix::transformJoinedIfValid(PathRemap, prefixes);
    mapper.emplace();
    mapper->addRange(prefixes);
    mapper->sort();
  }
  auto remapPath = [&mapper](StringRef path) {
    if (!mapper)
      return path.str();
    return mapper->mapToString(path);
  };

  auto fs = getPluginLoadingFS(Ctx);
  std::error_code ec;

  auto validateLibrary = [&](StringRef path) -> llvm::Expected<std::string> {
    auto remappedPath = remapPath(path);
    if (!Ctx.SearchPathOpts.ResolvedPluginVerification || path.empty())
      return remappedPath;

    auto currentStat = fs->status(remappedPath);
    if (!currentStat)
      return llvm::createFileError(remappedPath, currentStat.getError());

    auto goldStat = Ctx.SourceMgr.getFileSystem()->status(path);
    if (!goldStat)
      return llvm::createStringError(
          "cannot open gold reference library to compare");

    // Compare the size for difference for now.
    if (currentStat->getSize() != goldStat->getSize())
      return llvm::createStringError(
          "plugin has changed since dependency scanning");

    return remappedPath;
  };

  for (auto &entry : Ctx.SearchPathOpts.PluginSearchOpts) {
    switch (entry.getKind()) {

    // '-load-plugin-library <library path>'.
    case PluginSearchOption::Kind::LoadPluginLibrary: {
      auto &val = entry.get<PluginSearchOption::LoadPluginLibrary>();
      auto moduleName = pluginModuleNameStringFromPath(val.LibraryPath);
      if (!moduleName.empty()) {
        try_emplace(moduleName, val.LibraryPath, /*executablePath=*/"");
      }
      continue;
    }

    // '-load-plugin-executable <executable path>#<module name>, ...'.
    case PluginSearchOption::Kind::LoadPluginExecutable: {
      auto &val = entry.get<PluginSearchOption::LoadPluginExecutable>();
      assert(!val.ExecutablePath.empty() && "empty plugin path");
      for (auto &moduleName : val.ModuleNames) {
        try_emplace(moduleName, /*libraryPath=*/"", val.ExecutablePath);
      }
      continue;
    }

    // '-plugin-path <library search path>'.
    case PluginSearchOption::Kind::PluginPath: {
      auto &val = entry.get<PluginSearchOption::PluginPath>();
      for (auto i = fs->dir_begin(val.SearchPath, ec);
           i != llvm::vfs::directory_iterator(); i = i.increment(ec)) {
        auto libPath = i->path();
        auto moduleName = pluginModuleNameStringFromPath(libPath);
        if (!moduleName.empty()) {
          try_emplace(moduleName, libPath, /*executablePath=*/"");
        }
      }
      continue;
    }

    // '-external-plugin-path <library search path>#<server path>'.
    case PluginSearchOption::Kind::ExternalPluginPath: {
      auto &val = entry.get<PluginSearchOption::ExternalPluginPath>();
      for (auto i = fs->dir_begin(val.SearchPath, ec);
           i != llvm::vfs::directory_iterator(); i = i.increment(ec)) {
        auto libPath = i->path();
        auto moduleName = pluginModuleNameStringFromPath(libPath);
        if (!moduleName.empty()) {
          try_emplace(moduleName, libPath, val.ServerPath);
        }
      }
      continue;
    }

    // '-load-resolved-plugin <library path>#<server path>#<module name>,...'.
    case PluginSearchOption::Kind::ResolvedPluginConfig: {
      auto &val = entry.get<PluginSearchOption::ResolvedPluginConfig>();
      // Respect resolved plugin config above other search path, and it can
      // overwrite plugins found by other options or previous resolved
      // configuration.
      for (auto &moduleName : val.ModuleNames) {
        auto libPath = validateLibrary(val.LibraryPath);
        if (!libPath) {
          Ctx.Diags.diagnose(SourceLoc(), diag::resolved_macro_changed,
                             remapPath(val.LibraryPath),
                             toString(libPath.takeError()));
          continue;
        }
        try_emplace(moduleName, *libPath, remapPath(val.ExecutablePath),
                    /*overwrite*/ true);
      }
      continue;
    }
    }
    llvm_unreachable("unhandled PluginSearchOption::Kind");
  }

  return map;
}

const PluginLoader::PluginEntry &
PluginLoader::lookupPluginByModuleName(Identifier moduleName) {
  auto &map = getPluginMap();
  auto found = map.find(moduleName);
  if (found == map.end()) {
    static PluginEntry notFound{"", ""};
    return notFound;
  }

  // Track the dependency.
  recordDependency(found->second, moduleName);
  return found->second;
}

llvm::Expected<CompilerPlugin *> PluginLoader::getInProcessPlugins() {
  auto inProcPluginServerPath = Ctx.SearchPathOpts.InProcessPluginServerPath;
  if (inProcPluginServerPath.empty()) {
    return llvm::createStringError(
        llvm::inconvertibleErrorCode(),
        "library plugins require -in-process-plugin-server-path");
  }

  auto fs = getPluginLoadingFS(Ctx);
  SmallString<128> resolvedPath;
  if (auto err = fs->getRealPath(inProcPluginServerPath, resolvedPath)) {
    return llvm::createStringError(err, err.message());
  }

  return getRegistry()->getInProcessPlugins(resolvedPath);
}

llvm::Expected<CompilerPlugin *>
PluginLoader::loadExecutablePlugin(StringRef path) {
  auto fs = getPluginLoadingFS(Ctx);
  SmallString<128> resolvedPath;
  if (auto err = fs->getRealPath(path, resolvedPath)) {
    return llvm::createStringError(err, err.message());
  }

  // Load the plugin.
  auto plugin =
      getRegistry()->loadExecutablePlugin(resolvedPath, disableSandbox);
  if (!plugin) {
    resolvedPath.push_back(0);
    return llvm::handleErrors(
        plugin.takeError(), [&](const llvm::ErrorInfoBase &err) {
          return llvm::createStringError(
              err.convertToErrorCode(),
              "compiler plugin '%s' could not be loaded: %s",
              resolvedPath.data(), err.message().data());
        });
  }

  return plugin;
}

void PluginLoader::recordDependency(const PluginEntry &plugin,
                                    Identifier moduleName) {
  if (!DepTracker)
    return;

  // libraryPath: non-nil, executablePath: nil: in-process library plugin.
  // libraryPath: non-nil, executablePath: non-nil: external library plugin.
  // libraryPath: nil, executablePath: non-nil: executable plugin.
  StringRef path =
      !plugin.libraryPath.empty() ? plugin.libraryPath : plugin.executablePath;

  // NOTE: We don't track plugin-server path as a dependency because it doesn't
  // provide much value.

  assert(!path.empty());
  SmallString<128> resolvedPath;
  auto fs = Ctx.SourceMgr.getFileSystem();
  if (auto err = fs->getRealPath(path, resolvedPath)) {
    return;
  }

  DepTracker->addMacroPluginDependency(resolvedPath, moduleName);
}
