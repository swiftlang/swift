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

namespace swift {

class CASOptions final {
public:
  /// Enable compiler caching.
  bool EnableCaching = false;

  /// Enable compiler caching remarks.
  bool EnableCachingRemarks = false;

  /// Skip replaying outputs from cache.
  bool CacheSkipReplay = false;

  /// CASOptions
  clang::CASOptions CASOpts;

  /// CASFS Root.
  std::vector<std::string> CASFSRootIDs;

  /// Clang Include Trees.
  std::vector<std::string> ClangIncludeTrees;

  /// CacheKey for input file.
  std::string InputFileKey;

  /// Cache key for imported bridging header.
  std::string BridgingHeaderPCHCacheKey;

  /// Get the CAS configuration flags.
  void enumerateCASConfigurationFlags(
      llvm::function_ref<void(llvm::StringRef)> Callback) const;

  /// Check to see if a CASFileSystem is required.
  bool requireCASFS() const {
    return EnableCaching &&
           (!CASFSRootIDs.empty() || !ClangIncludeTrees.empty() ||
            !InputFileKey.empty() || !BridgingHeaderPCHCacheKey.empty());
  }
};

} // namespace swift

#endif // SWIFT_BASIC_CASOPTIONS_H
