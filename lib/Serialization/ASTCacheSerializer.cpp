//===--- ASTCacheSerializer.cpp - Per-file AST cache serialization --------===//
//
// This source code is part of the Swift.org open source project
//
// Copyright (c) 2024-2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implements ASTCacheSerializer, a subclass of serialization::Serializer that
// overrides isDeclXRef() and writeCrossReference() to emit CrossFileDeclRef
// records for same-module cross-file decls instead of inlining them as copies.
//
//===----------------------------------------------------------------------===//

#include "Serialization.h"
#include "ModuleFormat.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckedSnapshot.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/Subsystems.h"
#include "llvm/Bitcode/BitcodeConvenience.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Path.h"
#include <unistd.h>

using namespace swift;
using namespace swift::serialization;
using llvm::BCBlockRAII;

namespace {

/// ASTCacheSerializer overrides the cross-reference behavior of Serializer
/// to emit CrossFileDeclRef records for same-module cross-file decls.
class ASTCacheSerializer : public Serializer {
public:
  ASTCacheSerializer(ModuleOrSourceFile DC,
                     const SerializationOptions &opts)
      : Serializer(SWIFTMODULE_SIGNATURE, DC, opts) {}
  /// Return true for same-module cross-file decls so they route through
  /// writeCrossReference instead of being inlined as copies.
  bool isDeclXRef(const Decl *D) const override {
    const DeclContext *topLevel = D->getDeclContext()->getModuleScopeContext();
    if (topLevel->getParentModule() != M)
      return true; // cross-module: use standard xref
    if (!SF || topLevel == SF || topLevel == SF->getSynthesizedFile())
      return false; // local to this file: inline
    // Same-module cross-file: route through writeCrossReference.
    // The deserializer resolves these via CachedNominalDeclRegistry
    // instead of lookupQualified, avoiding the parse-during-load crash.
    return true;
  }

  /// Use name-based DependentMemberType serialization so that associated
  /// types in xref'd protocols don't need AssociatedTypeDecl resolution.
  /// The deserialized type uses DependentMemberType::get(base, name) which
  /// resolves the AssociatedTypeDecl lazily during type checking.
  bool useNameBasedDependentMemberType() const override { return true; }
  /// Serialize body text for ALL functions (not just @inlinable) so the
  /// cache round-trips the full AST for structural verification.
  bool serializeAllBodyText() const override { return true; }

  /// Serialize the SourceFile to the given stream using the overridden
  /// isDeclXRef to route same-module cross-file decls through writeCrossReference.
  void writeToStream(raw_ostream &os, ModuleOrSourceFile DC) {
    writeBlockInfoBlock();
    {
      BCBlockRAII moduleBlock(Out, MODULE_BLOCK_ID, 2);
      writeHeader();
      writeInputBlock();
      writeSIL(nullptr);
      writeAST(DC);
      writeHiddenTypeLayoutsBlock();
    }
    SerializerBase::writeToStream(os);
  }
};

} // anonymous namespace

namespace swift::serialization {

/// Public entry point for serializing a SourceFile to a .swiftast cache file.
/// Writes the same container format as writeTypeCheckedSnapshot (ASTCacheKey
/// header + bitstream + swiftdeps YAML), but uses ASTCacheSerializer which
/// routes same-module cross-file decls through writeCrossReference (isDeclXRef=true)
/// instead of inlining them as copies (isDeclXRef=false).
bool writeASTCacheFile(ASTContext &ctx, const SourceFile &SF,
                       StringRef outputPath) {
  // Force type-checking of all delayed function bodies
  const_cast<SourceFile &>(SF).typeCheckDelayedFunctions();

  // 1. Compute cache key
  ASTCacheKey key = computeASTCacheKey(ctx, SF);

  // Populate importsBlob with explicit import module names (one per line).
  // Only ImportDecls from the source file's Items are explicit — implicit
  // imports (added by import resolution) are excluded. Used by the
  // deserializer to reconstruct ImportDecl nodes matching the parsed AST.
  {
    llvm::raw_string_ostream importsOS(key.importsBlob);
    for (auto item : const_cast<SourceFile &>(SF).getTopLevelItems()) {
      if (auto *importDecl = dyn_cast<ImportDecl>(item.dyn_cast<Decl *>())) {
        importsOS << importDecl->getModulePath().front().Item.str()
                  << "\n";
      }
    }
  }

  // 2. Serialize the AST to a bitstream using ASTCacheSerializer
  std::string bitstreamData;
  {
    llvm::raw_string_ostream bitstreamOS(bitstreamData);
    SerializationOptions opts;
    auto &langOpts = const_cast<LangOptions &>(ctx.LangOpts);
    bool savedAllowErrors = langOpts.AllowModuleWithCompilerErrors;
    langOpts.AllowModuleWithCompilerErrors = true;

    ASTCacheSerializer serializer(
        ModuleOrSourceFile(const_cast<SourceFile *>(&SF)), opts);
    serializer.writeToStream(bitstreamOS,
        ModuleOrSourceFile(const_cast<SourceFile *>(&SF)));

    langOpts.AllowModuleWithCompilerErrors = savedAllowErrors;
    langOpts.AllowModuleWithCompilerErrors = savedAllowErrors;

    // Collect decl source ranges for the deserializer to restore.
    // The deserialized decls have invalid SourceLocs; this blob provides
    // the original source offsets so getSourceRange() can return valid ranges.
    auto &sourceMgr = ctx.SourceMgr;
    unsigned bufferID = SF.getBufferID();
    if (bufferID != 0) {
      llvm::raw_string_ostream rangesOS(key.declRangesBlob);
      // Walk all decls in the SourceFile and write their source ranges.
      // Format: for each decl: (uint32_t startOffset, uint32_t endOffset)
      auto writeRange = [&](SourceRange range) {
        if (!range.isValid()) {
          uint32_t zero = 0;
          rangesOS.write(reinterpret_cast<const char *>(&zero), 4);
          rangesOS.write(reinterpret_cast<const char *>(&zero), 4);
          return;
        }
        uint32_t start = static_cast<uint32_t>(
            sourceMgr.getLocOffsetInBuffer(range.Start, bufferID));
        uint32_t end = static_cast<uint32_t>(
            sourceMgr.getLocOffsetInBuffer(range.End, bufferID));
        rangesOS.write(reinterpret_cast<const char *>(&start), 4);
        rangesOS.write(reinterpret_cast<const char *>(&end), 4);
      };
      auto writeDeclRange = [&](const Decl *D) {
        writeRange(D->getSourceRange());
        // Also write ranges for each semantic attr.
        for (auto *attr : D->getSemanticAttrs()) {
          writeRange(attr->getRange());
        }
      };
      for (auto item : const_cast<SourceFile &>(SF).getTopLevelItems()) {
        if (auto *D = item.dyn_cast<Decl *>()) {
          // Skip ImportDecls — they are not in the deserialized decls vector
          // (reconstructed separately by SnapshotDeserializer).
          if (isa<ImportDecl>(D))
            continue;
          writeDeclRange(D);
          if (auto *IDC = dyn_cast<IterableDeclContext>(D)) {
            for (auto *member : IDC->getMembers()) {
              // Skip EnumCaseDecl — it's a parser-internal grouping node
              // not present in the deserialized AST.
              if (isa<EnumCaseDecl>(member))
                continue;
              writeDeclRange(member);
              // Walk accessors of storage decls (not in getMembers()).
              if (auto *ASD = dyn_cast<AbstractStorageDecl>(member)) {
                for (auto *accessor : ASD->getAllAccessors())
                  writeDeclRange(accessor);
              }
              // Walk params of function decls.
              if (auto *AFD = dyn_cast<AbstractFunctionDecl>(member)) {
                if (auto *params = AFD->getParameters()) {
                  for (auto *param : *params)
                    writeDeclRange(param);
                }
                // Walk accessors' params too.
                if (auto *ASD = dyn_cast<AbstractStorageDecl>(member)) {
                  for (auto *accessor : ASD->getAllAccessors()) {
                    if (auto *accParams = accessor->getParameters()) {
                      for (auto *param : *accParams)
                        writeDeclRange(param);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  // 3. Write the .swiftast container (same format as SnapshotSerializer)
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
  writeString(key.declRangesBlob);
  // Write bitstream (length-prefixed)
  writeString(bitstreamData);

  // Write .swiftdeps YAML blob (length-prefixed) — empty for now
  std::string swiftdepsYAML;
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

} // namespace swift::serialization
