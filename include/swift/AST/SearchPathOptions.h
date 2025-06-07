//===--- SearchPathOptions.h ------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_AST_SEARCHPATHOPTIONS_H
#define SWIFT_AST_SEARCHPATHOPTIONS_H

#include "swift/Basic/ArrayRefView.h"
#include "swift/Basic/ExternalUnion.h"
#include "swift/Basic/PathRemapper.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/VersionTuple.h"
#include "llvm/Support/VirtualFileSystem.h"
#include <optional>

#include <string>
#include <vector>

namespace swift {

/// The kind of a module search path. The order of this enum is important
/// because import search paths should be considered before framework search
/// paths etc.
enum class ModuleSearchPathKind {
  Import,
  Framework,
  ImplicitFramework,
  RuntimeLibrary,
};

/// Specifies how to load modules when both a module interface and serialized
/// AST are present, or whether to disallow one format or the other altogether.
enum class ModuleLoadingMode {
  PreferInterface,
  PreferSerialized,
  OnlyInterface,
  OnlySerialized
};

/// A single module search path that can come from different sources, e.g.
/// framework search paths, import search path etc.
class ModuleSearchPath : public llvm::RefCountedBase<ModuleSearchPath> {
  /// The actual path of the module search path.
  std::string Path;

  /// The kind of the search path.
  ModuleSearchPathKind Kind;

  bool IsSystem;

  /// An index that describes the order this search path should be considered
  /// in within its \c ModuleSearchPathKind. This allows us to reconstruct the
  /// user-defined search path order when merging search paths containing
  /// different file names in \c searchPathsContainingFile.
  unsigned Index;

public:
  ModuleSearchPath(StringRef Path, ModuleSearchPathKind Kind, bool IsSystem,
                   unsigned Index)
      : Path(Path), Kind(Kind), IsSystem(IsSystem), Index(Index) {}

  StringRef getPath() const { return Path; }
  ModuleSearchPathKind getKind() const { return Kind; }

  bool isSystem() const { return IsSystem; }

  unsigned getIndex() const { return Index; }

  bool operator<(const ModuleSearchPath &Other) const {
    if (this->Kind == Other.Kind) {
      return this->Index < Other.Index;
    } else {
      return this->Kind < Other.Kind;
    }
  }
};

using ModuleSearchPathPtr = llvm::IntrusiveRefCntPtr<ModuleSearchPath>;

class SearchPathOptions;

/// Maintains a mapping of filenames to search paths that contain a file with
/// this name (non-recursively). E.g. if we have a directory structure as
/// follows.
///
/// \code
/// searchPath1/
///   Module1.framework
///
/// searchPath2/
///   Module1.framework
///   Module2.swiftmodule
/// \endcode
///
/// We have the following lookup table
///
/// \code
/// Module1.framework -> [searchPath1, searchPath2]
/// Module2.swiftmodule -> [searchPath2]
/// \endcode
///
/// When searching for a module this allows an efficient search of only those
/// search paths that are relevant. In a naive implementation, we would need
/// to scan all search paths for every module we import.
class ModuleSearchPathLookup {
  /// Parameters for which the \c LookupTable has been built. If one if these
  /// changes, the lookup table needs to be rebuilt. It is not expected that any
  /// of these change frequently.
  struct {
    llvm::vfs::FileSystem *FileSystem;
    bool IsOSDarwin;
    bool IsPopulated;
    const SearchPathOptions *Opts;
  } State;

  llvm::StringMap<SmallVector<ModuleSearchPathPtr, 4>> LookupTable;

  /// Scan the directory at \p SearchPath for files and add those files to the
  /// lookup table. \p Kind specifies the search path kind and \p Index the
  /// index of \p SearchPath within that search path kind. Search paths with
  /// lower indices are considered first.
  /// The \p SearchPath is stored by as a \c StringRef, so the string backing it
  /// must be alive as long as this lookup table is alive and not cleared.
  void addFilesInPathToLookupTable(llvm::vfs::FileSystem *FS,
                                   StringRef SearchPath,
                                   ModuleSearchPathKind Kind, bool IsSystem,
                                   unsigned Index);

  /// Discard the current lookup table and rebuild a new one.
  void rebuildLookupTable(const SearchPathOptions *Opts,
                          llvm::vfs::FileSystem *FS, bool IsOsDarwin);

  /// Discard the current lookup table.
  void clearLookupTable() {
    LookupTable.clear();
    State.IsPopulated = false;
    State.FileSystem = nullptr;
    State.IsOSDarwin = false;
    State.Opts = nullptr;
  }

public:
  /// Called by \p SearchPathOptions when search paths indexed by this \c
  /// SearchPathLookup have changed in an unknown way. Causes the lookup table
  /// to be rebuilt at the next request.
  void searchPathsDidChange() { clearLookupTable(); }

  /// Called by \p SearchPathOptions when an import or framework search path has
  /// been added.
  /// \p Index is the index of the search path within its kind and is used to
  /// make sure this search path is considered last (within its kind).
  void searchPathAdded(llvm::vfs::FileSystem *FS, StringRef SearchPath,
                       ModuleSearchPathKind Kind, bool IsSystem,
                       unsigned Index) {
    if (!State.IsPopulated) {
      // If the lookup table hasn't been built yet, we will scan the search path
      // once the lookup table is requested. Nothing to do yet.
      return;
    }
    if (State.FileSystem != FS) {
      // We would be using a different file system to augment the lookup table
      // than we initially used to build it. Discard everything to be safe.
      clearLookupTable();
      return;
    }
    addFilesInPathToLookupTable(FS, SearchPath, Kind, IsSystem, Index);
  }

  /// Returns all search paths that non-recursively contain a file whose name
  /// is in \p Filenames.
  SmallVector<const ModuleSearchPath *, 4>
  searchPathsContainingFile(const SearchPathOptions *Opts,
                            llvm::ArrayRef<std::string> Filenames,
                            llvm::vfs::FileSystem *FS, bool IsOSDarwin);
};

/// Pair of a plugin path and the module name that the plugin provides.
struct PluginExecutablePathAndModuleNames {
  std::string ExecutablePath;
  std::vector<std::string> ModuleNames;
};

/// Pair of a plugin search path and the corresponding plugin server executable
/// path.
struct ExternalPluginSearchPathAndServerPath {
  std::string SearchPath;
  std::string ServerPath;
};

class PluginSearchOption {
public:
  struct LoadPluginLibrary {
    std::string LibraryPath;
  };
  struct LoadPluginExecutable {
    std::string ExecutablePath;
    std::vector<std::string> ModuleNames;
  };
  struct PluginPath {
    std::string SearchPath;
  };
  struct ExternalPluginPath {
    std::string SearchPath;
    std::string ServerPath;
  };
  struct ResolvedPluginConfig {
    std::string LibraryPath;
    std::string ExecutablePath;
    std::vector<std::string> ModuleNames;
  };

  enum class Kind : uint8_t {
    LoadPluginLibrary,
    LoadPluginExecutable,
    PluginPath,
    ExternalPluginPath,
    ResolvedPluginConfig,
  };

private:
  using Members =
      ExternalUnionMembers<LoadPluginLibrary, LoadPluginExecutable, PluginPath,
                           ExternalPluginPath, ResolvedPluginConfig>;
  static Members::Index getIndexForKind(Kind kind) {
    switch (kind) {
    case Kind::LoadPluginLibrary:
      return Members::indexOf<LoadPluginLibrary>();
    case Kind::LoadPluginExecutable:
      return Members::indexOf<LoadPluginExecutable>();
    case Kind::PluginPath:
      return Members::indexOf<PluginPath>();
    case Kind::ExternalPluginPath:
      return Members::indexOf<ExternalPluginPath>();
    case Kind::ResolvedPluginConfig:
      return Members::indexOf<ResolvedPluginConfig>();
    }
  };
  using Storage = ExternalUnion<Kind, Members, getIndexForKind>;

  Kind kind;
  Storage storage;

public:
  PluginSearchOption(const LoadPluginLibrary &v)
      : kind(Kind::LoadPluginLibrary) {
    storage.emplace<LoadPluginLibrary>(kind, v);
  }
  PluginSearchOption(const LoadPluginExecutable &v)
      : kind(Kind::LoadPluginExecutable) {
    storage.emplace<LoadPluginExecutable>(kind, v);
  }
  PluginSearchOption(const PluginPath &v) : kind(Kind::PluginPath) {
    storage.emplace<PluginPath>(kind, v);
  }
  PluginSearchOption(const ExternalPluginPath &v)
      : kind(Kind::ExternalPluginPath) {
    storage.emplace<ExternalPluginPath>(kind, v);
  }
  PluginSearchOption(const ResolvedPluginConfig &v)
      : kind(Kind::ResolvedPluginConfig) {
    storage.emplace<ResolvedPluginConfig>(kind, v);
  }
  PluginSearchOption(const PluginSearchOption &o) : kind(o.kind) {
    storage.copyConstruct(o.kind, o.storage);
  }
  PluginSearchOption(PluginSearchOption &&o) : kind(o.kind) {
    storage.moveConstruct(o.kind, std::move(o.storage));
  }
  ~PluginSearchOption() { storage.destruct(kind); }
  PluginSearchOption &operator=(const PluginSearchOption &o) {
    storage.copyAssign(kind, o.kind, o.storage);
    kind = o.kind;
    return *this;
  }
  PluginSearchOption &operator=(PluginSearchOption &&o) {
    storage.moveAssign(kind, o.kind, std::move(o.storage));
    kind = o.kind;
    return *this;
  }

  Kind getKind() const { return kind; }

  template <typename T>
  const T *dyn_cast() const {
    if (Members::indexOf<T>() != getIndexForKind(kind))
      return nullptr;
    return &storage.get<T>(kind);
  }

  template <typename T>
  const T &get() const {
    return storage.get<T>(kind);
  }
};

/// Options for controlling search path behavior.
class SearchPathOptions {
  /// To call \c addImportSearchPath and \c addFrameworkSearchPath from
  /// \c ASTContext::addSearchPath.
  friend class ASTContext;

public:
  struct SearchPath {
    std::string Path;
    bool IsSystem = false;
    SearchPath(StringRef path, bool isSystem)
        : Path(path), IsSystem(isSystem) {}

    friend bool operator==(const SearchPath &LHS, const SearchPath &RHS) {
      return LHS.Path == RHS.Path && LHS.IsSystem == RHS.IsSystem;
    }
    friend bool operator!=(const SearchPath &LHS, const SearchPath &RHS) {
      return !(LHS == RHS);
    }
    friend llvm::hash_code
    hash_value(const SearchPath &searchPath) {
      return llvm::hash_combine(searchPath.Path, searchPath.IsSystem);
    }
  };

private:
  ModuleSearchPathLookup Lookup;

  /// Path to the SDK which is being built against.
  ///
  /// Must be modified through setter to keep \c Lookup in sync.
  std::string SDKPath;

  /// Path(s) which should be searched for modules.
  ///
  /// Must be modified through setter to keep \c Lookup in sync.
  std::vector<SearchPath> ImportSearchPaths;

  /// Path(s) which should be searched for frameworks.
  ///
  /// Must be modified through setter to keep \c Lookup in sync.
  std::vector<SearchPath> FrameworkSearchPaths;

  /// Paths to search for stdlib modules. One of these will be
  /// compiler-relative.
  ///
  /// Must be modified through setter to keep \c Lookup in sync.
  std::vector<std::string> RuntimeLibraryImportPaths;

  /// When on Darwin the framework paths that are implicitly imported.
  /// $SDKROOT/System/Library/Frameworks/ and $SDKROOT/Library/Frameworks/.
  ///
  /// Must be modified through setter to keep \c Lookup in sync.
  std::vector<std::string> ImplicitFrameworkSearchPaths;

  /// Compiler plugin library search paths.
  std::vector<std::string> CompilerPluginLibraryPaths;

  /// Compiler plugin executable paths and providing module names.
  std::vector<PluginExecutablePathAndModuleNames> CompilerPluginExecutablePaths;

  /// Add a single import search path. Must only be called from
  /// \c ASTContext::addSearchPath.
  void addImportSearchPath(SearchPath Path, llvm::vfs::FileSystem *FS) {
    ImportSearchPaths.push_back(Path);
    Lookup.searchPathAdded(FS, ImportSearchPaths.back().Path,
                           ModuleSearchPathKind::Import, Path.IsSystem,
                           ImportSearchPaths.size() - 1);
  }

  /// Add a single framework search path. Must only be called from
  /// \c ASTContext::addSearchPath.
  void addFrameworkSearchPath(SearchPath NewPath, llvm::vfs::FileSystem *FS) {
    FrameworkSearchPaths.push_back(NewPath);
    Lookup.searchPathAdded(FS, FrameworkSearchPaths.back().Path,
                           ModuleSearchPathKind::Framework, NewPath.IsSystem,
                           FrameworkSearchPaths.size() - 1);
  }

  std::optional<std::string> WinSDKRoot = std::nullopt;
  std::optional<std::string> WinSDKVersion = std::nullopt;
  std::optional<std::string> VCToolsRoot = std::nullopt;
  std::optional<std::string> VCToolsVersion = std::nullopt;

  std::optional<StringRef> SysRoot = std::nullopt;

  mutable std::optional<std::string> SDKPlatformPath = std::nullopt;

public:
  StringRef getSDKPath() const { return SDKPath; }

  void setSDKPath(std::string NewSDKPath) {
    SDKPath = NewSDKPath;
  }

  /// Retrieves the corresponding parent platform path for the SDK, or
  /// \c nullopt if there isn't one.
  /// NOTE: This computes and caches the result, and as such will not respect
  /// a different FileSystem being passed later.
  std::optional<StringRef> getSDKPlatformPath(llvm::vfs::FileSystem *FS) const;

  std::optional<StringRef> getWinSDKRoot() const { return WinSDKRoot; }
  void setWinSDKRoot(StringRef root) {
    WinSDKRoot = root;
  }

  std::optional<StringRef> getWinSDKVersion() const { return WinSDKVersion; }
  void setWinSDKVersion(StringRef version) {
    WinSDKVersion = version;
  }

  std::optional<StringRef> getVCToolsRoot() const { return VCToolsRoot; }
  void setVCToolsRoot(StringRef root) {
    VCToolsRoot = root;
  }

  std::optional<StringRef> getVCToolsVersion() const { return VCToolsVersion; }
  void setVCToolsVersion(StringRef version) {
    VCToolsVersion = version;
  }

  std::optional<StringRef> getSysRoot() const { return SysRoot; }
  void setSysRoot(StringRef sysroot) {
    SysRoot = sysroot;
  }

  ArrayRef<SearchPath> getImportSearchPaths() const {
    return ImportSearchPaths;
  }

  void setImportSearchPaths(std::vector<SearchPath> NewImportSearchPaths) {
    ImportSearchPaths = NewImportSearchPaths;
    Lookup.searchPathsDidChange();
  }

  ArrayRef<SearchPath> getFrameworkSearchPaths() const {
    return FrameworkSearchPaths;
  }

  void
  setFrameworkSearchPaths(std::vector<SearchPath> NewFrameworkSearchPaths) {
    FrameworkSearchPaths = NewFrameworkSearchPaths;
    Lookup.searchPathsDidChange();
  }

  /// The extra implicit framework search paths on Apple platforms:
  /// $SDKROOT/System/Library/Frameworks/ and $SDKROOT/Library/Frameworks/.
  ArrayRef<std::string> getImplicitFrameworkSearchPaths() const {
    return ImplicitFrameworkSearchPaths;
  }

  void setImplicitFrameworkSearchPaths(
      std::vector<std::string> NewImplicitFrameworkSearchPaths) {
    ImplicitFrameworkSearchPaths = NewImplicitFrameworkSearchPaths;
    Lookup.searchPathsDidChange();
  }

  ArrayRef<std::string> getRuntimeLibraryImportPaths() const {
    return RuntimeLibraryImportPaths;
  }

  void setRuntimeLibraryImportPaths(
      std::vector<std::string> NewRuntimeLibraryImportPaths) {
    RuntimeLibraryImportPaths = NewRuntimeLibraryImportPaths;
    Lookup.searchPathsDidChange();
  }

  /// Path(s) to virtual filesystem overlay YAML files.
  std::vector<std::string> VFSOverlayFiles;

  /// Path(s) which should be searched for libraries.
  ///
  /// This is used in immediate modes. It is safe to add paths to this directly.
  std::vector<std::string> LibrarySearchPaths;

  /// Path to search for compiler-relative header files.
  std::string RuntimeResourcePath;

  /// Paths to search for compiler-relative stdlib dylibs, in order of
  /// preference.
  std::vector<std::string> RuntimeLibraryPaths;

  /// Plugin search path options.
  std::vector<PluginSearchOption> PluginSearchOpts;

  /// Path to in-process plugin server shared library.
  std::string InProcessPluginServerPath;

  /// Don't automatically add any import paths.
  bool SkipAllImplicitImportPaths = false;

  /// Don't automatically add any import paths from the SDK.
  bool SkipSDKImportPaths = false;

  /// Scanner Prefix Mapper.
  std::vector<std::string> ScannerPrefixMapper;

  /// Verify resolved plugin is not changed.
  bool ResolvedPluginVerification = false;

  /// When set, don't validate module system dependencies.
  ///
  /// If a system header is modified and this is not set, the compiler will
  /// rebuild PCMs and compiled swiftmodules that depend on them, just like it
  /// would for a non-system header.
  bool DisableModulesValidateSystemDependencies = false;

  /// A set of compiled modules that may be ready to use.
  std::vector<std::string> CandidateCompiledModules;

  /// A map of explicit Swift module information.
  std::string ExplicitSwiftModuleMapPath;

  /// Module inputs specified with -swift-module-input,
  /// <ModuleName, Path to .swiftmodule file>
  std::vector<std::pair<std::string, std::string>> ExplicitSwiftModuleInputs;

  /// A map of placeholder Swift module dependency information.
  std::string PlaceholderDependencyModuleMap;

  /// A file containing a list of protocols whose conformances require const value extraction.
  std::string ConstGatherProtocolListFilePath;

  /// Path to the file that defines platform mapping for availability
  /// version inheritance.
  std::optional<std::string> PlatformAvailabilityInheritanceMapPath;

  /// Cross import module information. Map from module name to the list of cross
  /// import overlay files that associate with that module.
  using CrossImportMap = llvm::StringMap<std::vector<std::string>>;
  CrossImportMap CrossImportInfo;

  /// CanImport information passed from scanning.
  struct CanImportInfo {
    std::string ModuleName;
    llvm::VersionTuple Version;
    llvm::VersionTuple UnderlyingVersion;
  };
  std::vector<CanImportInfo> CanImportModuleInfo;

  /// Whether to search for cross import overlay on file system.
  bool DisableCrossImportOverlaySearch = false;

  /// Debug path mappings to apply to serialized search paths. These are
  /// specified in LLDB from the target.source-map entries.
  PathRemapper SearchPathRemapper;

  /// Recover the search paths deserialized from .swiftmodule files to their
  /// original form.
  PathObfuscator DeserializedPathRecoverer;

  /// Specify the module loading behavior of the compilation.
  ModuleLoadingMode ModuleLoadMode = ModuleLoadingMode::PreferSerialized;

  /// New scanner search behavior. Validate up-to-date existing Swift module
  /// dependencies in the scanner itself.
  bool ScannerModuleValidation = false;

  /// Whether this compilation should attempt to resolve in-package
  /// imports of its module dependencies.
  ///
  /// Source compilation and 'package' textual interface compilation both
  /// require that package-only imports of module dependencies be resolved.
  /// Otherwise, compilation of non-package textual interfaces, even if
  /// "in-package", must not require package-only module dependencies.
  bool ResolveInPackageModuleDependencies = false;

  /// Enable auto bridging header chaining.
  bool BridgingHeaderChaining = false;

  /// Return all module search paths that (non-recursively) contain a file whose
  /// name is in \p Filenames.
  SmallVector<const ModuleSearchPath *, 4>
  moduleSearchPathsContainingFile(llvm::ArrayRef<std::string> Filenames,
                                  llvm::vfs::FileSystem *FS, bool IsOSDarwin) {
    return Lookup.searchPathsContainingFile(this, Filenames, FS, IsOSDarwin);
  }

  /// Creates a filesystem taking into account any overlays specified in
  /// \c VFSOverlayFiles. Returns \p BaseFS if there were no overlays and
  /// \c FileError(s) if any error occurred while attempting to parse the
  /// overlay files.
  llvm::Expected<llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem>>
  makeOverlayFileSystem(
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> BaseFS) const;

public:
  /// Return a hash code of any components from these options that should
  /// contribute to a Swift Bridging PCH hash.
  llvm::hash_code getPCHHashComponents() const {
    using llvm::hash_combine;
    using llvm::hash_combine_range;
    return hash_combine(SDKPath,
                        hash_combine_range(ImportSearchPaths.begin(), ImportSearchPaths.end()),
                        hash_combine_range(VFSOverlayFiles.begin(), VFSOverlayFiles.end()),
                        hash_combine_range(FrameworkSearchPaths.begin(),
                                           FrameworkSearchPaths.end()),
                        hash_combine_range(LibrarySearchPaths.begin(),
                                           LibrarySearchPaths.end()),
                        RuntimeResourcePath,
                        hash_combine_range(RuntimeLibraryImportPaths.begin(),
                                           RuntimeLibraryImportPaths.end()),
                        hash_combine_range(ImplicitFrameworkSearchPaths.begin(),
                                           ImplicitFrameworkSearchPaths.end()),
                        DisableModulesValidateSystemDependencies,
                        ScannerModuleValidation,
                        ModuleLoadMode);
  }

  /// Return a hash code of any components from these options that should
  /// contribute to a Swift Dependency Scanning hash.
  llvm::hash_code getModuleScanningHashComponents() const {
    return getPCHHashComponents();
  }

  void dump(bool isDarwin) const;
};
}

#endif
