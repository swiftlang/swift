//===--- SnapshotSerializer.cpp - Serialize type-checked AST to .swiftast -===//
//
// This source code is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Serializes a type-checked SourceFile's AST to a .swiftast cache file.
// The .swiftast file is a container with:
// 1. Cache key header (magic, version, hashes, import metadata)
// 2. Standard swiftmodule bitstream (per-SourceFile serialization)
// 3. .swiftdeps YAML blob (length-prefixed)
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/FineGrainedDependencies.h"
#include "swift/AST/TypeCheckedSnapshot.h"
#include "swift/Serialization/Serialization.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/Subsystems.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLTraits.h"
#include <unistd.h>

using namespace swift;

namespace {

/// A wrapper that writes the .swiftast container format.
class SnapshotSerializer {
  ASTContext &ctx;
  const SourceFile &SF;

public:
  SnapshotSerializer(ASTContext &ctx, const SourceFile &SF)
      : ctx(ctx), SF(SF) {}

  /// Serialize the type-checked AST to a .swiftast file.
  bool serialize(StringRef outputPath) {
    // 1. Force type-checking of all delayed function bodies
    const_cast<SourceFile &>(SF).typeCheckDelayedFunctions();

    // 2. Compute cache key
    ASTCacheKey key = computeASTCacheKey(ctx, SF);

    // 3. Populate import metadata in the key
    populateImportMetadata(key);

    // 4. Serialize the AST to a bitstream using the existing Serializer
    std::string bitstreamData;
    {
      llvm::raw_string_ostream bitstreamOS(bitstreamData);
      SerializationOptions opts;
      // Serialize only the source file (not the whole module)
      serialization::writeToStream(
          bitstreamOS, ModuleOrSourceFile(const_cast<SourceFile *>(&SF)),
          /*SILModule*/ nullptr, opts,
          /*DepGraph*/ nullptr);
    }

    // 5. Serialize the .swiftdeps content to YAML
    // For the PoC, we don't serialize the dependency graph.
    // A full implementation would get the SourceFileDepGraph and serialize it.
    std::string swiftdepsYAML;

    // 6. Write the .swiftast container
    // Write to temp file first, then atomic rename
    SmallString<256> tempPath;
    {
      SmallString<32> tmp;
      tmp = outputPath;
      tmp += ".tmp.";
      tmp += std::to_string(getpid());
      llvm::sys::path::append(tempPath, tmp);
    }

    // Create parent directory if needed
    auto parentDir = llvm::sys::path::parent_path(outputPath);
    if (!parentDir.empty()) {
      llvm::sys::fs::create_directories(parentDir);
    }

    std::error_code ec;
    llvm::raw_fd_ostream out(tempPath, ec, llvm::sys::fs::OF_None);
    if (ec) {
      return false;
    }

    // Write cache key header
    out.write(key.magic, 9);
    out.write(reinterpret_cast<const char *>(&key.formatVersion),
              sizeof(key.formatVersion));
    out.write(reinterpret_cast<const char *>(&key.compilerVersionHash),
              sizeof(key.compilerVersionHash));

    // Write string fields (length-prefixed)
    auto writeString = [&](const std::string &s) {
      uint32_t len = static_cast<uint32_t>(s.size());
      out.write(reinterpret_cast<const char *>(&len), sizeof(len));
      out.write(s.data(), len);
    };
    writeString(key.sourceFileHash);
    writeString(key.importedModulesHash);
    writeString(key.macroPluginsHash);
    writeString(key.crossImportOverlaysHash);
    writeString(key.dependencyProvidesHash);
    writeString(key.importsBlob);
    writeString(key.privateDiscriminator);
    writeString(key.overlaysBlob);

    // Write bitstream (length-prefixed)
    writeString(bitstreamData);

    // Write .swiftdeps YAML blob (length-prefixed)
    writeString(swiftdepsYAML);

    out.close();

    // Atomic rename
    auto renameErr = llvm::sys::fs::rename(tempPath, outputPath);
    if (renameErr) {
      // Non-atomic fallback: copy
      llvm::sys::fs::copy_file(tempPath, outputPath);
      llvm::sys::fs::remove(tempPath);
    }

    return true;
  }

private:
  void populateImportMetadata(ASTCacheKey &key) {
    // Serialize the resolved imports as module names (one per line)
    // This is used on cache hit to populate SourceFile::Imports
    // without running import resolution.
    if (SF.hasImports()) {
      llvm::raw_string_ostream importsOS(key.importsBlob);
      for (const auto &import : SF.getImports()) {
        importsOS << import.module.importedModule->getName().str() << "\n";
      }
    }

    // Serialize separately-imported overlays
    // TODO: Walk SF.getSeparatelyImportedOverlays() and serialize
  }
};

} // anonymous namespace

namespace swift {

/// Public entry point for serializing a type-checked AST to .swiftast.
bool writeTypeCheckedSnapshot(ASTContext &ctx, const SourceFile &SF,
                               StringRef outputPath) {
  SnapshotSerializer serializer(ctx, SF);
  return serializer.serialize(outputPath);
}

} // namespace swift
