//===--- SnapshotDeserializer.cpp - Deserialize .swiftast into SourceFile ===//
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
// Deserializes a .swiftast cache file into a SourceFile, populating Items,
// Imports, and other fields. Sets ASTStage = TypeChecked.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Import.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckedSnapshot.h"
#include "swift/AST/FineGrainedDependencies.h"
#include "ModuleFile.h"
#include "ModuleFileSharedCore.h"
#include "swift/Serialization/SerializationOptions.h"

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/Path.h"

using namespace swift;

namespace {

class SnapshotDeserializer {
  SourceFile &SF;
  ASTContext &ctx;

public:
  SnapshotDeserializer(SourceFile &SF, ASTContext &ctx)
      : SF(SF), ctx(ctx) {}

  /// Deserialize a .swiftast cache file into the SourceFile.
  /// Returns true on success, false on failure (caller should fall back to
  /// parsing).
  bool deserialize(const llvm::MemoryBuffer &cacheBuffer) {
    // 1. Parse the cache key header from the buffer
    ASTCacheKey key;
    size_t offset = 0;
    if (!parseCacheHeader(cacheBuffer, key, offset)) {
      return false;
    }

    // 2. Validate the cache key (source hash + compiler version hash)
    auto currentKey = computeASTCacheKey(ctx, SF);
    if (key.sourceFileHash != currentKey.sourceFileHash ||
        key.compilerVersionHash != currentKey.compilerVersionHash) {
      return false;
    }

    // 3. Parse the bitstream data
    std::string bitstreamData;
    if (!readString(cacheBuffer, bitstreamData, offset)) {
      return false;
    }

    // 4. Parse the .swiftdeps YAML blob (not used for PoC)
    std::string swiftdepsYAML;
    if (!readString(cacheBuffer, swiftdepsYAML, offset)) {
      return false;
    }

    if (!deserializeBitstream(bitstreamData, key)) {
      return false;
    }
    // 6. Populate Imports from cache header
    if (!populateImports(key.importsBlob)) {
      return false;
    }

    // 7. Set remaining fields
    SF.ASTStage = SourceFile::TypeChecked;
    SF.LoadedFromAstCache = true;

    return true;
  }

private:
  bool deserializeBitstream(StringRef bitstreamData,
                             const ASTCacheKey &key) {
    // Create a MemoryBuffer from the bitstream data
    auto bitstreamBuf = llvm::MemoryBuffer::getMemBufferCopy(
        bitstreamData, "cached_ast.swiftmodule");
    if (!bitstreamBuf) {
      return false;
    }

    // Load the ModuleFileSharedCore from the buffer
    std::shared_ptr<const ModuleFileSharedCore> core;
    PathObfuscator pathObfuscator;
    auto info = ModuleFileSharedCore::load(
        /*moduleInterfacePath*/ "", /*moduleInterfaceSourcePath*/ "",
        std::move(bitstreamBuf),
        /*moduleDocInputBuffer*/ nullptr,
        /*moduleSourceInfoInputBuffer*/ nullptr,
        /*isFramework*/ false,
        /*requiredSDK*/ ctx.LangOpts.SDKName,
        /*target*/ ctx.LangOpts.Target,
        /*isEmbedded*/ std::nullopt,
        pathObfuscator,
        core);
    if (info.status != serialization::Status::Valid) {
      return false;
    }

    // Create a ModuleFile from the core
    auto mf = std::make_unique<ModuleFile>(core);

    // Associate the ModuleFile with this SourceFile (which is a FileUnit)
    auto status = mf->associateWithFileContext(
        &SF, SourceLoc(), /*recoverFromIncompatibility*/ false);
    if (status != serialization::Status::Valid) {
      return false;
    }

    // Load top-level decls into the SourceFile's Items
    SmallVector<Decl *, 32> decls;
    mf->getTopLevelDecls(decls);

    // Populate SourceFile::Items with the deserialized decls
    SmallVector<ASTNode, 32> itemNodes;
    for (auto *D : decls) {
      itemNodes.push_back(ASTNode(D));
    }
    SF.setTopLevelItems(itemNodes);

    // Set the private discriminator from the cache key
    if (!key.privateDiscriminator.empty()) {
      SF.setPrivateDiscriminatorForCache(
          ctx.getIdentifier(key.privateDiscriminator));
    }
    // Keep the ModuleFile alive by storing it in the SourceFile.
    // This prevents the deserialized Decls from being freed.
    // TODO: Store the ModuleFile in SourceFile as a unique_ptr member.
    // For now, we leak it intentionally — the ASTContext owns the Decls
    // and the ModuleFile's core is shared_ptr so it will live as long as
    // the Decls reference it.
    (void)mf.release();

    return true;
  }

  bool parseCacheHeader(const llvm::MemoryBuffer &buf, ASTCacheKey &key,
                        size_t &offset) {
    auto data = buf.getBuffer();
    if (data.size() < 9 + sizeof(uint32_t) * 2) {
      return false;
    }

    // Read magic
    memcpy(key.magic, data.data(), 9);
    offset = 9;
    if (memcmp(key.magic, SWIFTAST_MAGIC, 9) != 0) {
      return false;
    }

    // Read format version
    memcpy(&key.formatVersion, data.data() + offset, sizeof(uint32_t));
    offset += sizeof(uint32_t);
    if (key.formatVersion != SWIFTAST_FORMAT_VERSION) {
      return false;
    }

    // Read compiler version hash
    memcpy(&key.compilerVersionHash, data.data() + offset, sizeof(uint32_t));
    offset += sizeof(uint32_t);

    // Read string fields
    if (!readString(buf, key.sourceFileHash, offset)) return false;
    if (!readString(buf, key.importedModulesHash, offset)) return false;
    if (!readString(buf, key.macroPluginsHash, offset)) return false;
    if (!readString(buf, key.crossImportOverlaysHash, offset)) return false;
    if (!readString(buf, key.dependencyProvidesHash, offset)) return false;
    if (!readString(buf, key.importsBlob, offset)) return false;
    if (!readString(buf, key.privateDiscriminator, offset)) return false;
    if (!readString(buf, key.overlaysBlob, offset)) return false;

    return true;
  }

  bool readString(const llvm::MemoryBuffer &buf, std::string &str,
                  size_t &offset) {
    auto data = buf.getBuffer();
    if (offset + sizeof(uint32_t) > data.size()) {
      return false;
    }
    uint32_t len;
    memcpy(&len, data.data() + offset, sizeof(uint32_t));
    offset += sizeof(uint32_t);
    if (offset + len > data.size()) {
      return false;
    }
    str.assign(data.data() + offset, len);
    offset += len;
    return true;
  }

  bool populateImports(StringRef importsBlob) {
    if (importsBlob.empty()) {
      // No imports in the cache header — set empty imports
      SF.setImports({});
      return true;
    }

    // Parse the imports blob: one module name per line
    SmallVector<AttributedImport<ImportedModule>, 4> imports;
    auto lines = llvm::split(importsBlob, '\n');
    for (auto line : lines) {
      if (line.empty()) continue;

      // Resolve the module name to a ModuleDecl*
      auto *mod = ctx.getModuleByName(line);
      if (!mod) {
        // Module not found — cache miss, fall back to parsing
        return false;
      }
      ImportedModule importedMod(mod);
      imports.push_back(AttributedImport<ImportedModule>(importedMod));
    }

    SF.setImports(imports);
    return true;
  }
};

} // anonymous namespace

namespace swift {

bool SourceFile::loadFromCache(ASTContext &Ctx,
                                llvm::MemoryBuffer &cacheBuffer) {
  SnapshotDeserializer deserializer(*this, Ctx);
  return deserializer.deserialize(cacheBuffer);
}

} // namespace swift
