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
#include "swift/Basic/PathRemapper.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/VirtualFileSystem.h"

#include <string>
#include <vector>

namespace swift {

/// Options for controlling search path behavior.
class SearchPathOptions {
  /// To call \c addImportSearchPath and \c addFrameworkSearchPath from
  /// \c ASTContext::addSearchPath.
  friend class ASTContext;

public:
  struct FrameworkSearchPath {
    std::string Path;
    bool IsSystem = false;
    FrameworkSearchPath(StringRef path, bool isSystem)
    : Path(path), IsSystem(isSystem) {}

    friend bool operator ==(const FrameworkSearchPath &LHS,
                            const FrameworkSearchPath &RHS) {
      return LHS.Path == RHS.Path && LHS.IsSystem == RHS.IsSystem;
    }
    friend bool operator !=(const FrameworkSearchPath &LHS,
                            const FrameworkSearchPath &RHS) {
      return !(LHS == RHS);
    }
  };

private:
  /// Path to the SDK which is being built against.
  ///
  /// Must me modified through setter to keep \c SearchPathLookup in sync.
  std::string SDKPath;

  /// Path(s) which should be searched for modules.
  ///
  /// Must me modified through setter to keep \c SearchPathLookup in sync.
  std::vector<std::string> ImportSearchPaths;

  /// Path(s) which should be searched for frameworks.
  ///
  /// Must me modified through setter to keep \c SearchPathLookup in sync.
  std::vector<FrameworkSearchPath> FrameworkSearchPaths;

  /// Paths to search for stdlib modules. One of these will be
  /// compiler-relative.
  ///
  /// Must me modified through setter to keep \c SearchPathLookup in sync.
  std::vector<std::string> RuntimeLibraryImportPaths;

  /// When on Darwin the framework paths that are implicitly imported.
  /// $SDKROOT/System/Library/Frameworks/ and $SDKROOT/Library/Frameworks/.
  ///
  /// On non-Darwin platforms these are populated, but ignored.
  ///
  /// Computed when the SDK path is set and cached so we can reference the
  /// Darwin implicit framework search paths as \c StringRef from
  /// \c ModuleSearchPath.
  std::vector<std::string> DarwinImplicitFrameworkSearchPaths;

public:
  StringRef getSDKPath() const { return SDKPath; }

  void setSDKPath(std::string NewSDKPath) {
    SDKPath = NewSDKPath;

    // Compute Darwin implicit framework search paths.
    SmallString<128> systemFrameworksScratch(NewSDKPath);
    llvm::sys::path::append(systemFrameworksScratch, "System", "Library",
                            "Frameworks");
    SmallString<128> frameworksScratch(NewSDKPath);
    llvm::sys::path::append(frameworksScratch, "Library", "Frameworks");
    DarwinImplicitFrameworkSearchPaths = {systemFrameworksScratch.str().str(),
                                          frameworksScratch.str().str()};
  }

  ArrayRef<std::string> getImportSearchPaths() const {
    return ImportSearchPaths;
  }

  void setImportSearchPaths(std::vector<std::string> NewImportSearchPaths) {
    ImportSearchPaths = NewImportSearchPaths;
  }

  ArrayRef<FrameworkSearchPath> getFrameworkSearchPaths() const {
    return FrameworkSearchPaths;
  }

  void setFrameworkSearchPaths(
      std::vector<FrameworkSearchPath> NewFrameworkSearchPaths) {
    FrameworkSearchPaths = NewFrameworkSearchPaths;
  }

  /// The extra implicit framework search paths on Apple platforms:
  /// $SDKROOT/System/Library/Frameworks/ and $SDKROOT/Library/Frameworks/.
  ArrayRef<std::string> getDarwinImplicitFrameworkSearchPaths() const {
    return DarwinImplicitFrameworkSearchPaths;
  }

  ArrayRef<std::string> getRuntimeLibraryImportPaths() const {
    return RuntimeLibraryImportPaths;
  }

  void setRuntimeLibraryImportPaths(
      std::vector<std::string> NewRuntimeLibraryImportPaths) {
    RuntimeLibraryImportPaths = NewRuntimeLibraryImportPaths;
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

  /// Don't look in for compiler-provided modules.
  bool SkipRuntimeLibraryImportPaths = false;

  /// When set, don't validate module system dependencies.
  ///
  /// If a system header is modified and this is not set, the compiler will
  /// rebuild PCMs and compiled swiftmodules that depend on them, just like it
  /// would for a non-system header.
  bool DisableModulesValidateSystemDependencies = false;

  /// Enforce loading only serialized modules built with the same SDK
  /// as the context loading it.
  bool EnableSameSDKCheck = true;

  /// A set of compiled modules that may be ready to use.
  std::vector<std::string> CandidateCompiledModules;

  /// A map of explict Swift module information.
  std::string ExplicitSwiftModuleMap;

  /// A map of placeholder Swift module dependency information.
  std::string PlaceholderDependencyModuleMap;

  /// A file containing modules we should perform batch scanning.
  std::string BatchScanInputFilePath;

  /// Debug path mappings to apply to serialized search paths. These are
  /// specified in LLDB from the target.source-map entries.
  PathRemapper SearchPathRemapper;

  /// Recover the search paths deserialized from .swiftmodule files to their
  /// original form.
  PathObfuscator DeserializedPathRecoverer;

private:
  static StringRef
  pathStringFromFrameworkSearchPath(const FrameworkSearchPath &next) {
    return next.Path;
  };

public:
  /// Return a hash code of any components from these options that should
  /// contribute to a Swift Bridging PCH hash.
  llvm::hash_code getPCHHashComponents() const {
    using llvm::hash_combine;
    using llvm::hash_combine_range;

    using FrameworkPathView = ArrayRefView<FrameworkSearchPath, StringRef,
                                           pathStringFromFrameworkSearchPath>;
    FrameworkPathView frameworkPathsOnly{FrameworkSearchPaths};

    return hash_combine(SDKPath,
                        hash_combine_range(ImportSearchPaths.begin(),
                                           ImportSearchPaths.end()),
                        hash_combine_range(VFSOverlayFiles.begin(),
                                           VFSOverlayFiles.end()),
                        // FIXME: Should we include the system-ness of framework
                        // search paths too?
                        hash_combine_range(frameworkPathsOnly.begin(),
                                           frameworkPathsOnly.end()),
                        hash_combine_range(LibrarySearchPaths.begin(),
                                           LibrarySearchPaths.end()),
                        RuntimeResourcePath,
                        hash_combine_range(RuntimeLibraryImportPaths.begin(),
                                           RuntimeLibraryImportPaths.end()),
                        DisableModulesValidateSystemDependencies);
  }
};
}

#endif
