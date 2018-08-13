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

#include "llvm/ADT/Hashing.h"

#include <string>
#include <vector>

namespace swift {

/// Options for controlling search path behavior.
class SearchPathOptions {
public:
  /// Path to the SDK which is being built against.
  std::string SDKPath;

  /// Path(s) which should be searched for modules.
  ///
  /// Do not add values to this directly. Instead, use
  /// \c ASTContext::addSearchPath.
  std::vector<std::string> ImportSearchPaths;

  /// Path(s) to virtual filesystem overlay YAML files.
  std::vector<std::string> VFSOverlayFiles;

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
  /// Path(s) which should be searched for frameworks.
  ///
  /// Do not add values to this directly. Instead, use
  /// \c ASTContext::addSearchPath.
  std::vector<FrameworkSearchPath> FrameworkSearchPaths;

  /// Path(s) which should be searched for libraries.
  ///
  /// This is used in immediate modes. It is safe to add paths to this directly.
  std::vector<std::string> LibrarySearchPaths;

  /// Path to search for compiler-relative header files.
  std::string RuntimeResourcePath;

  /// Path to search for compiler-relative stdlib dylibs.
  std::string RuntimeLibraryPath;

  /// Path to search for compiler-relative stdlib modules.
  std::string RuntimeLibraryImportPath;

  /// Don't look in for compiler-provided modules.
  bool SkipRuntimeLibraryImportPath = false;

  /// Return a hash code of any components from these options that should
  /// contribute to a Swift Bridging PCH hash.
  llvm::hash_code getPCHHashComponents() const {
    using llvm::hash_value;
    using llvm::hash_combine;
    auto Code = hash_value(SDKPath);
    for (auto Import : ImportSearchPaths) {
      Code = hash_combine(Code, Import);
    }
    for (auto VFSFile : VFSOverlayFiles) {
      Code = hash_combine(Code, VFSFile);
    }
    for (const auto &FrameworkPath : FrameworkSearchPaths) {
      Code = hash_combine(Code, FrameworkPath.Path);
    }
    for (auto LibraryPath : LibrarySearchPaths) {
      Code = hash_combine(Code, LibraryPath);
    }
    Code = hash_combine(Code, RuntimeResourcePath);
    Code = hash_combine(Code, RuntimeLibraryImportPath);
    return Code;
  }
};

}

#endif
