//===--- TypeCheckedSnapshot.h - Per-file AST cache types -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the types and functions for per-file type-checked AST
// caching (.swiftast files). The cache stores the full type-checked AST for
// individual source files, allowing incremental builds to skip parsing and
// type-checking for unchanged files.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_TYPE_CHECKED_SNAPSHOT_H
#define SWIFT_AST_TYPE_CHECKED_SNAPSHOT_H

#include "swift/AST/FineGrainedDependencies.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include <string>

namespace swift {

class ASTContext;
class SourceFile;

/// The .swiftast file format magic header.
static constexpr const char *SWIFTAST_MAGIC = "SWIFTAST\0";
static constexpr uint32_t SWIFTAST_FORMAT_VERSION = 1;

/// Cache key for a .swiftast file. This is stored at the beginning of the
/// .swiftast file and validated on load.
struct ASTCacheKey {
  /// Magic header ("SWIFTAST\0").
  char magic[9];
  /// Format version of the .swiftast file.
  uint32_t formatVersion;
  /// Full Swift compiler version string hash.
  uint32_t compilerVersionHash;
  /// SHA-256 hash of the source file content.
  std::string sourceFileHash;
  /// Hash of language options (conditional compilation).
  uint32_t langOptsHash;
  /// SHA-256 hash of imported modules (name + content hash).
  std::string importedModulesHash;
  /// SHA-256 hash of macro plugins (path + content hash).
  std::string macroPluginsHash;
  /// SHA-256 hash of cross-import overlay modules.
  std::string crossImportOverlaysHash;
  /// SHA-256 hash of dependency provides (from .swiftdeps).
  std::string dependencyProvidesHash;

  /// Serialized import metadata: (module_name, import_attributes) pairs.
  /// Used to populate SourceFile::Imports on cache hit.
  std::string importsBlob;

  /// The private discriminator for this file.
  std::string privateDiscriminator;

  /// Serialized separately-imported overlays: (overlay_name, declaring_name) pairs.
  std::string overlaysBlob;

  /// Check if this cache key is valid for the given source file.
  bool isValid(ASTContext &ctx, const SourceFile &SF,
               const class SourceFileDepGraph &depGraph) const;
};

/// Compute the cache key for a source file.
ASTCacheKey computeASTCacheKey(ASTContext &ctx, const SourceFile &SF);

/// Validate whether a .swiftast cache file is valid for the given source file.
/// Returns true if the cache is valid (cache hit), false otherwise (cache miss).
bool validateASTCache(ASTContext &ctx, const SourceFile &SF,
                      const SourceFileDepGraph &depGraph,
                      const ASTCacheKey &cachedKey);

/// Per-file invalidation using .swiftdeps.
/// Returns true if the file's cache is still valid given the previous build's
/// dependency graph and the current build's provides sets.
bool isCacheValidForFile(const SourceFile &SF,
                         const SourceFileDepGraph &prevDepGraph,
                         const class ModuleDepGraph &currentDepGraph);

/// Serialize a type-checked SourceFile's AST to a .swiftast cache file.
/// \returns true on success.
bool writeTypeCheckedSnapshot(ASTContext &ctx, const SourceFile &SF,
                               StringRef outputPath);

} // namespace swift

#endif // SWIFT_AST_TYPE_CHECKED_SNAPSHOT_H
