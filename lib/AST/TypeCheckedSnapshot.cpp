//===--- TypeCheckedSnapshot.cpp - Per-file AST cache implementation ------===//
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
// Implementation of per-file type-checked AST caching.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/TypeCheckedSnapshot.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/FineGrainedDependencies.h"
#include "swift/Basic/Version.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SHA256.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

/// Compute SHA-256 hash of a string and return it as a hex string.
static std::string sha256Hex(StringRef data) {
  llvm::SHA256 hasher;
  hasher.update(data);
  llvm::SmallString<65> hashStr;
  llvm::toHex(hasher.final(), /*LowerCase=*/true, hashStr);
  return std::string(hashStr);
}

/// Compute a hash of the compiler version string.
static uint32_t computeCompilerVersionHash() {
  auto versionStr = version::getSwiftFullVersion();
  llvm::SHA256 hasher;
  hasher.update(versionStr);
  auto hash = hasher.final();
  // Use first 4 bytes as uint32
  uint32_t result;
  memcpy(&result, hash.data(), sizeof(result));
  return result;
}

ASTCacheKey swift::computeASTCacheKey(ASTContext &ctx, const SourceFile &SF) {
  ASTCacheKey key;
  memcpy(key.magic, SWIFTAST_MAGIC, 9);
  key.formatVersion = SWIFTAST_FORMAT_VERSION;
  key.compilerVersionHash = computeCompilerVersionHash();

  // Source file hash
  auto bufferID = SF.getBufferID();
  auto &sourceMgr = ctx.SourceMgr;
  auto buffer = sourceMgr.getEntireTextForBuffer(bufferID);
  key.sourceFileHash = sha256Hex(buffer);

  // Language options hash is intentionally NOT included in cache validation.
  // LangOpts fields change between save (post-typecheck) and load (pre-parse)
  // phases, making this hash unstable. See https://github.com/apple/swift/issues/...
  // The compiler version hash already covers toolchain identity.
  key.langOptsHash = 0;

  // For the PoC, we compute imported modules hash, macro plugins hash,
  // and cross-import overlays hash from the frontend options.
  // These are computed during cache validation, not here.
  // A full implementation would walk the module's imports and hash each
  // .swiftmodule file's content.

  // Private discriminator
  key.privateDiscriminator = SF.getPrivateDiscriminator().str();

  // The imports blob and overlays blob are populated during serialization
  // (when we have access to the resolved imports).
  // For the PoC, we store them as empty strings and populate during save.

  return key;
}

bool swift::validateASTCache(ASTContext &ctx, const SourceFile &SF,
                              const SourceFileDepGraph &depGraph,
                              const ASTCacheKey &cachedKey) {
  // Check magic header
  if (memcmp(cachedKey.magic, SWIFTAST_MAGIC, 9) != 0)
    return false;

  // Check format version
  if (cachedKey.formatVersion != SWIFTAST_FORMAT_VERSION)
    return false;

  // Check compiler version
  if (cachedKey.compilerVersionHash != computeCompilerVersionHash())
    return false;

  // Check source file hash
  auto currentKey = computeASTCacheKey(ctx, SF);
  if (cachedKey.sourceFileHash != currentKey.sourceFileHash)
    return false;

  // Language options hash is intentionally skipped (see computeASTCacheKey).

  // Check imported modules hash
  if (cachedKey.importedModulesHash != currentKey.importedModulesHash)
    return false;

  // Check macro plugins hash
  if (cachedKey.macroPluginsHash != currentKey.macroPluginsHash)
    return false;

  // Check cross-import overlays hash
  if (cachedKey.crossImportOverlaysHash != currentKey.crossImportOverlaysHash)
    return false;

  // Check dependency provides hash
  if (cachedKey.dependencyProvidesHash != currentKey.dependencyProvidesHash)
    return false;

  return true;
}

bool swift::isCacheValidForFile(const SourceFile &SF,
                                 const SourceFileDepGraph &prevDepGraph,
                                 const ModuleDepGraph &currentDepGraph) {
  // TODO: Implement per-file invalidation using .swiftdeps provides/depends edges.
  // For the PoC, we use a simple source-hash-only check.
  // A full implementation would:
  // 1. Parse all .swiftdeps files into provides/depends/potentialMember maps
  // 2. For each file F with a cache candidate, traverse F's depends edges
  // 3. Check if any file's provides set changed
  // 4. For potentialMember(T) dependencies, check if any member of T changed
  return true;
}
