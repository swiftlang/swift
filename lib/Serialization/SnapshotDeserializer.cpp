//===--- SnapshotDeserializer.cpp - Deserialize .swiftast into SourceFile ---===//
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
#include "swift/AST/SILOptions.h"
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
    // C4: Script-mode files are not cacheable (TopLevelCodeDecl is not
    // serialized by the stock serializer, so caching would silently drop
    // top-level executable code).
    if (SF.isScriptMode())
      return false;

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

    // 6. Set remaining fields
    SF.ASTStage = SourceFile::TypeChecked;
    SF.LoadedFromAstCache = true;

    // AST cache: disable SIL verification. Cached files have missing function
    // bodies and incomplete witness tables (by design), so SIL verification
    // would fail with "function must have a body" errors.
    const_cast<SILOptions &>(ctx.SILOpts).VerifyNone = true;
    return true;
  }

private:
  bool deserializeBitstream(StringRef bitstreamData,
                           const ASTCacheKey &key) {
 // The cached AST may have been serialized with AllowModuleWithCompilerErrors
 // (e.g. when types had errors due to whole-module compilation). Enable it
 // during deserialization so the deserializer skips invalid types instead
 // of crashing.
 auto &langOpts = const_cast<LangOptions &>(ctx.LangOpts);
 bool savedAllowErrors = langOpts.AllowModuleWithCompilerErrors;
 langOpts.AllowModuleWithCompilerErrors = true;

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

    // Pre-populate Items with an empty vector to prevent ParseSourceFileRequest
    // from re-parsing the source file if getTopLevelItems() is called during
    // decl deserialization (e.g., via loadDependenciesForFileContext or
    // handleInherited triggering type resolution). The real items are set
    // below via setTopLevelItems().
    SF.setTopLevelItems({});

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
    // Mark deserialized IterableDeclContexts as having already had their
    // parsed members added. Without this, addParsedMembers() sees
    // getParentSourceFile() != null and triggers ParseMembersRequest, which
    // tries to re-parse from source and crashes (invalid source ranges for
    // deserialized decls). SerializedASTFile decls avoid this because their
    // getParentSourceFile() returns null.
    for (auto *D : decls) {
      if (auto *IDC = dyn_cast<IterableDeclContext>(D)) {
        const_cast<IterableDeclContext *>(IDC)->setAddedParsedMembersForCache();
      }
    }

    // C3: Populate Imports from the ModuleFile's loaded dependencies.
    // associateWithFileContext calls loadDependenciesForFileContext which
    // loads the ModuleFile's Dependencies. We iterate over getDependencies()
    // to preserve import attributes (@_exported, @_implementationOnly, @_spi)
    // that getImportedModules() would drop.
    {
      SmallVector<AttributedImport<ImportedModule>, 8> attributedImports;
      for (auto &dep : mf->getDependencies()) {
        if (!dep.isLoaded())
          continue;

        ImportOptions options;
        if (dep.isExported())
          options |= ImportFlags::Exported;
        if (dep.isImplementationOnly())
          options |= ImportFlags::ImplementationOnly;
        if (!dep.spiGroups.empty())
          options |= ImportFlags::SPIAccessControl;

        AccessLevel accessLevel;
        if (dep.isExported())
          accessLevel = AccessLevel::Public;
        else if (dep.isPackageOnly())
          accessLevel = AccessLevel::Package;
        else if (dep.isInternalOrBelow())
          accessLevel = AccessLevel::Internal;
        else
          accessLevel = AccessLevel::Public;

        attributedImports.push_back(AttributedImport<ImportedModule>(
            *dep.Import, SourceLoc(), options, /*sourceFileArg*/ {},
            ArrayRef<Identifier>(dep.spiGroups), /*preconcurrencyRange*/ {},
            /*docVisibility*/ std::nullopt,
            /*accessLevel*/ accessLevel));
      }
      SF.setImports(attributedImports);
    }

    // Set the private discriminator from the cache key
    if (!key.privateDiscriminator.empty()) {
      SF.setPrivateDiscriminatorForCache(
          ctx.getIdentifier(key.privateDiscriminator));
    }

    // C1: Store the ModuleFile in the SourceFile to keep it alive.
    // The ASTContext arena doesn't call destructors, so we register a cleanup
    // to delete the ModuleFile when the ASTContext is destroyed.
    SF.CachedModuleFile = mf.release();
    ctx.addCleanup([sf = &SF]() {
      delete sf->CachedModuleFile;
      sf->CachedModuleFile = nullptr;
    });

 langOpts.AllowModuleWithCompilerErrors = savedAllowErrors;
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
};

} // anonymous namespace

namespace swift {

bool SourceFile::loadFromCache(ASTContext &Ctx,
                                llvm::MemoryBuffer &cacheBuffer) {
  SnapshotDeserializer deserializer(*this, Ctx);
  return deserializer.deserialize(cacheBuffer);
}


void SourceFile::clearCachedModuleFile() {
  delete CachedModuleFile;
  CachedModuleFile = nullptr;
}
} // namespace swift
