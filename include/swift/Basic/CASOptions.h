//===--- CASOptions.h - CAS & caching options -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the CASOptions class, which provides various
//  CAS and caching flags.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_CASOPTIONS_H
#define SWIFT_BASIC_CASOPTIONS_H

#include "clang/CAS/CASOptions.h"
#include "llvm/ADT/Hashing.h"

namespace swift {

class CASOptions final {
public:
  /// Enable compiler caching.
  bool EnableCaching = false;

  /// Enable compiler caching remarks.
  bool EnableCachingRemarks = false;

  /// Skip replaying outputs from cache.
  bool CacheSkipReplay = false;

  /// Import modules from CAS.
  bool ImportModuleFromCAS = false;

  /// CASOptions
  clang::CASOptions CASOpts;

  /// Clang Include Trees.
  std::string ClangIncludeTree;

  /// Clang Include Tree FileList.
  std::string ClangIncludeTreeFileList;

  /// CacheKey for input file.
  std::string InputFileKey;

  /// Cache key for imported bridging header.
  std::string BridgingHeaderPCHCacheKey;

  /// Has immutable file system input.
  bool HasImmutableFileSystem = false;

  /// Get the CAS configuration flags.
  void enumerateCASConfigurationFlags(
      llvm::function_ref<void(llvm::StringRef)> Callback) const;

  /// Check to see if a CASFileSystem is required.
  bool requireCASFS() const {
    return EnableCaching &&
           (!ClangIncludeTree.empty() || !ClangIncludeTreeFileList.empty() ||
            !InputFileKey.empty() || !BridgingHeaderPCHCacheKey.empty());
  }

  /// Return a hash code of any components from these options that should
  /// contribute to a Swift Bridging PCH hash.
  llvm::hash_code getPCHHashComponents() const {
    // The CASIDs are generated from scanner, thus not part of the hash since
    // they will always be empty when requested.
    // TODO: Add frozen clang::CASOptions to the hash.
    return llvm::hash_combine(EnableCaching);
  }

  /// Return a hash code of any components from these options that should
  /// contribute to a Swift Dependency Scanning hash.
  llvm::hash_code getModuleScanningHashComponents() const {
    return getPCHHashComponents();
  }
};

} // namespace swift

#endif // SWIFT_BASIC_CASOPTIONS_H
