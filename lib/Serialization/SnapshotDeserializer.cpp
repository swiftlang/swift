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
#include "swift/AST/ParameterList.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Import.h"
#include "swift/AST/Module.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckedSnapshot.h"
#include "swift/AST/FineGrainedDependencies.h"
#include "swift/AST/SILOptions.h"
#include "swift/AST/TypeRepr.h"
#include "ModuleFile.h"
#include "ModuleFileSharedCore.h"
#include "swift/Serialization/SerializationOptions.h"
#include <functional>
#include "swift/Parse/Parser.h"
#include "swift/AST/ParseRequests.h"

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/Path.h"

using namespace swift;

namespace {

class SnapshotDeserializer {
  SourceFile &SF;
  ASTContext &ctx;
  SmallVector<SourceRange, 4> importDeclRanges;

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

    // Set LoadedFromAstCache before deserialization so that
    // IterableDeclContext::wasDeserialized() returns true during
    // getDeclChecked() (which is called from getTopLevelDecls() inside
    // deserializeBitstream). Without this, the DeclID is never set on
    // deserialized ExtensionDecls, causing loadNamedMembers() to fail
    // to find extension members.
    SF.LoadedFromAstCache = true;
    SF.ASTStage = SourceFile::TypeChecked;

    if (!deserializeBitstream(bitstreamData, key)) {
      return false;
    }

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
    // (e.g. when types had errors due to whole-module compilation). We enable
    // per-ModuleFile error recovery via setAllowCompilerErrorsForCache() below,
    // NOT via LangOpts.AllowModuleWithCompilerErrors. The global flag would leak
    // into the type-checker and allow errors in primary (source-compiled) files,
    // causing incorrect types to be produced and serialized into the cache.
    // The per-ModuleFile flag ensures only deserialization from cache files
    // allows errors, not type-checking of primary files.

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

    // Enable per-ModuleFile compiler error recovery for AST cache files.
    // This handles ErrorTypes in cached files without leaking into LangOpts.
    mf->setAllowCompilerErrorsForCache(true);
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

    // Populate SourceFile::Items with the deserialized decls.
    // ImportDecls are reconstructed after setImports (below) and prepended.
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

    // Eagerly register all top-level NominalTypeDecls in the
    // CachedNominalDeclRegistry. This ensures the registry is populated
    // BEFORE any cross-file xref resolution happens (resolveCrossReference).
    // Without this, xref resolution via lookupQualified returns deserialized
    // decls that trigger assertions (e.g., !PD->wasDeserialized() in
    // InheritedProtocolsRequest).
    for (auto *D : decls) {
      if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        ctx.registerCachedNominalDecl(NTD, Identifier(), 0, true);
      }
    }
    // Eagerly load associated types for deserialized protocols. The
    // requirement machine needs getAssociatedTypeMembers() to build
    // associated type introduction rules (τ.[P:T]). Without eager
    // loading, xref'd protocols have empty associated type lists and
    // getReducedTypeParameter() crashes.
    for (auto *D : decls) {
      if (auto *PD = dyn_cast<ProtocolDecl>(D)) {
        (void)PD->getAssociatedTypeMembers();
        // Eagerly compute the requirement signature so that dumpJSON
        // emits "requirement_signature" instead of "uncomputed_requirement_signature".
        (void)PD->getRequirementSignature();
        // Force-compute the generic signature so that dumpJSON emits
        // "generic_signature" instead of "parsed_generic_params".
        (void)PD->getGenericSignature();
      }
    }
    // Fix source_kind for synthesized conformances. The serializer writes all
    // conformances via getLocalConformances(All) but doesn't serialize the
    // ConformanceEntryKind (Explicit vs Synthesized). Deserialized conformances
    // default to Explicit, but synthesized conformances should be Synthesized.
    // A conformance is synthesized if its protocol does not appear in the
    // nominal's explicit inherits list.
    for (auto *D : decls) {
      if (auto *IDC = dyn_cast<IterableDeclContext>(D)) {
        // Collect the set of protocols that are explicitly inherited.
        llvm::SmallPtrSet<ProtocolDecl*, 4> explicitProtos;
        if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
          auto inherited = NTD->getInherited();
          for (auto i : inherited.getIndices()) {
            if (auto inheritedType = inherited.getResolvedType(i)) {
              if (auto *protoType = inheritedType->getAs<ProtocolType>()) {
                explicitProtos.insert(protoType->getDecl());
              }
            }
          }
        } else if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
          auto inherited = ED->getInherited();
          for (auto i : inherited.getIndices()) {
            if (auto inheritedType = inherited.getResolvedType(i)) {
              if (auto *protoType = inheritedType->getAs<ProtocolType>()) {
                explicitProtos.insert(protoType->getDecl());
              }
            }
          }
        }

        for (auto *conf : IDC->getLocalConformances(
                 ConformanceLookupKind::All)) {
          if (auto *npc = dyn_cast<NormalProtocolConformance>(conf)) {
            auto *proto = npc->getProtocol();
            if (!proto)
              continue;
            // Skip builtin conformances (Copyable, Escapable) — they're
            // already marked as synthesized.
            if (npc->getKind() != ProtocolConformanceKind::Normal)
              continue;
            // If the protocol is not in the explicit inherits list, mark
            // the conformance as synthesized.
            if (!explicitProtos.count(proto) &&
                npc->getSourceKind() == ConformanceEntryKind::Explicit) {
              npc->setSourceKindAndImplyingConformance(
                  ConformanceEntryKind::Synthesized, nullptr);
            }
          }
        }
      }
    }
    // Eagerly set the interface type and specifier on implicit self decls
    // of deserialized methods. This matches what the type checker would
    // compute, ensuring the self decl is usable immediately after deserialization.
    // Walk into nominal type members to reach methods (top-level decls only
    // contain struct/class/enum/protocol declarations, not their methods).
    auto setupSelfDecl = [](AbstractFunctionDecl *AFD) {
      if (auto *DC = AFD->getDeclContext()) {
        if (DC->isTypeContext()) {
          auto *selfDecl = AFD->getImplicitSelfDecl(/*createIfNeeded=*/true);
          if (selfDecl) {
            // Only set interface type for instance methods, not static methods.
            // Static methods' self decl doesn't have an interface type in the
            // original type-checked AST dump.
            bool isStatic = false;
            if (auto *FD = dyn_cast<FuncDecl>(AFD))
              isStatic = FD->isStatic();
            else if (auto *accessor = dyn_cast<AccessorDecl>(AFD))
              isStatic = accessor->isStatic();
            if (!isStatic && !isa<DestructorDecl>(AFD)) {
              auto selfTy = DC->getSelfInterfaceType();
              if (selfTy)
                selfDecl->setInterfaceType(selfTy);
            }
            if (DC->getDeclaredInterfaceType()->hasReferenceSemantics()) {
              selfDecl->setSpecifier(ParamSpecifier::Default);
            } else if (auto *accessor = dyn_cast<AccessorDecl>(AFD)) {
              auto kind = accessor->getAccessorKind();
              bool isMutatingAccessor =
                  (kind == AccessorKind::Set ||
                   kind == AccessorKind::Modify ||
                   kind == AccessorKind::Init);
              selfDecl->setSpecifier(isMutatingAccessor
                                     ? ParamSpecifier::InOut
                                     : ParamSpecifier::Default);
            } else if (auto *FD = dyn_cast<FuncDecl>(AFD)) {
              selfDecl->setSpecifier(FD->isMutating()
                                     ? ParamSpecifier::InOut
                                     : ParamSpecifier::Default);
            } else if (isa<ConstructorDecl>(AFD)) {
              selfDecl->setSpecifier(ParamSpecifier::InOut);
            }
          }
        }
      }
    };
    auto setupMembers = [&](IterableDeclContext *IDC) {
      for (auto *member : IDC->getMembers()) {
        if (auto *AFD = dyn_cast<AbstractFunctionDecl>(member)) {
          setupSelfDecl(AFD);
        } else if (auto *ASD = dyn_cast<AbstractStorageDecl>(member)) {
          // Accessors (get/set/didSet etc.) are AbstractFunctionDecls nested
          // inside storage decls. Walk all accessors (including implicit)
          // to set up their self decls too.
          for (auto *accessor : ASD->getAllAccessors()) {
            setupSelfDecl(accessor);
          }
        }
      }
    };
    for (auto *D : decls) {
      if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
        setupSelfDecl(AFD);
      } else if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        setupMembers(NTD);
      } else if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
        setupMembers(ED);
      }
    }
    // Reconstruct EnumCaseDecl wrappers for deserialized enums.
    // The serializer skips EnumCaseDecl (it's a parser-internal grouping node)
    // and serializes EnumElementDecls directly as members. The parsed AST
    // has EnumCaseDecl as an extra member before each group of elements.
    // We group ALL consecutive EnumElementDecls into a single EnumCaseDecl,
    // which handles the common `case X, Y, Z` pattern.
    for (auto *D : decls) {
      if (auto *ED = dyn_cast<EnumDecl>(D)) {
        auto members = ED->getMembers();
        // Collect consecutive EnumElementDecl groups and insert EnumCaseDecl
        // wrappers before each group.
        SmallVector<Decl *, 16> oldMembers(members.begin(), members.end());
        SmallVector<Decl *, 16> newMembers;
        SmallVector<EnumElementDecl *, 8> caseElements;
        for (auto *member : oldMembers) {
          if (auto *EED = dyn_cast<EnumElementDecl>(member)) {
            caseElements.push_back(EED);
          } else {
            if (!caseElements.empty()) {
              auto *caseDecl = EnumCaseDecl::create(SourceLoc(),
                  ArrayRef<EnumElementDecl*>(caseElements), ED);
              newMembers.push_back(caseDecl);
              // The parsed AST has EnumElementDecls as direct members too
              // (in addition to being wrapped by EnumCaseDecl).
              for (auto *EED : caseElements) {
                newMembers.push_back(EED);
              }
              caseElements.clear();
            }
            newMembers.push_back(member);
          }
        }
        if (!caseElements.empty()) {
          auto *caseDecl = EnumCaseDecl::create(SourceLoc(),
              ArrayRef<EnumElementDecl*>(caseElements), ED);
          newMembers.push_back(caseDecl);
          for (auto *EED : caseElements) {
            newMembers.push_back(EED);
          }
        }
        // Only re-add if we created EnumCaseDecls.
        if (newMembers.size() != oldMembers.size()) {
          // Remove all existing members and re-add with EnumCaseDecls.
          // addMember appends, so we need to clear first by removing all.
          for (auto *member : oldMembers) {
            ED->removeMember(member);
          }
          for (auto *member : newMembers) {
            ED->addMember(member);
          }
        }
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

    // Read ImportDecl source ranges from declRangesBlob before reconstructing
    // them. ImportDecls come first in the serializer's top-level item walk.
    if (!key.declRangesBlob.empty()) {
      auto &sourceMgr = ctx.SourceMgr;
      unsigned bufferID = SF.getBufferID();
      if (bufferID != 0) {
        const char *data = key.declRangesBlob.data();
        size_t remaining = key.declRangesBlob.size();
        auto readUint32 = [&]() -> uint32_t {
          if (remaining < 4) return 0;
          uint32_t val;
          memcpy(&val, data, 4);
          data += 4;
          remaining -= 4;
          return val;
        };
        auto readRange = [&]() -> SourceRange {
          uint32_t start = readUint32();
          uint32_t end = readUint32();
          if (start == 0 && end == 0)
            return SourceRange();
          return SourceRange(sourceMgr.getLocForOffset(bufferID, start),
                              sourceMgr.getLocForOffset(bufferID, end));
        };
        // Count ImportDecls from importsBlob (one per line) and read
        // that many ranges. ImportDecls have no semantic attrs.
        unsigned numImports = 0;
        StringRef importsRef = key.importsBlob;
        while (!importsRef.empty()) {
          auto [name, rest] = importsRef.split('\n');
          if (!name.empty())
            numImports++;
          importsRef = rest;
        }
        for (unsigned i = 0; i < numImports; i++)
          importDeclRanges.push_back(readRange());
      }
    }
    // Reconstruct ImportDecl nodes from the explicit imports in importsBlob,
    // prepend them to Items to match the parsed AST's item ordering.
    // The serializer skips ImportDecls (visitImportDecl is unreachable),
    // so we reconstruct them from the importsBlob (which contains only
    // explicit imports, not implicit ones added by import resolution).
    {
      SmallVector<ASTNode, 32> itemNodesWithImports;
      SmallVector<StringRef, 4> importNames;
      StringRef importsBlobRef = key.importsBlob;
      while (!importsBlobRef.empty()) {
        auto [name, rest] = importsBlobRef.split('\n');
        if (!name.empty()) {
          importNames.push_back(name);
        }
        importsBlobRef = rest;
      }
      for (auto importName : importNames) {
        ImportPath::Builder pathBuilder;
        pathBuilder.push_back(ctx.getIdentifier(importName));
        ImportPath importPath = pathBuilder.copyTo(ctx);
        auto *importDecl = ImportDecl::create(
            ctx, &SF, SourceLoc(), ImportKind::Module, SourceLoc(),
            importPath);
        if (!importDeclRanges.empty()) {
          auto R = importDeclRanges.front();
          importDeclRanges.erase(importDeclRanges.begin());
          if (R.isValid())
            ctx.setCachedDeclSourceRange(importDecl, R);
        }
        itemNodesWithImports.push_back(ASTNode(importDecl));
      }
      for (auto *D : decls) {
        itemNodesWithImports.push_back(ASTNode(D));
      }
      SF.setTopLevelItems(itemNodesWithImports);
    }

    // Set the private discriminator from the cache key
    if (!key.privateDiscriminator.empty()) {
      SF.setPrivateDiscriminatorForCache(
          ctx.getIdentifier(key.privateDiscriminator));
    }
    // Restore decl source ranges from the declRangesBlob.
    // The serialized decls have invalid SourceLocs; we set cached overrides
    // on ASTContext so Decl::getSourceRange() returns valid ranges.
    if (!key.declRangesBlob.empty()) {
      auto &sourceMgr = ctx.SourceMgr;
      unsigned bufferID = SF.getBufferID();
      if (bufferID != 0) {
        const char *data = key.declRangesBlob.data();
        size_t remaining = key.declRangesBlob.size();
        auto readUint32 = [&]() -> uint32_t {
          if (remaining < 4) return 0;
          uint32_t val;
          memcpy(&val, data, 4);
          data += 4;
          remaining -= 4;
          return val;
        };
        auto readRange = [&]() -> SourceRange {
          uint32_t start = readUint32();
          uint32_t end = readUint32();
          if (start == 0 && end == 0)
            return SourceRange(); // Invalid range marker
          SourceLoc startLoc = sourceMgr.getLocForOffset(bufferID, start);
          SourceLoc endLoc = sourceMgr.getLocForOffset(bufferID, end);
          return SourceRange(startLoc, endLoc);
        };
        auto setDeclRange = [&](Decl *D) {
          if (auto R = readRange(); R.isValid())
            ctx.setCachedDeclSourceRange(D, R);
          // Also read attr ranges in the same order as the serializer.
          for (auto *attr : D->getSemanticAttrs()) {
            if (auto R = readRange(); R.isValid())
              ctx.setCachedAttrSourceRange(attr, R);
          }
        };
        // Skip ImportDecl ranges — they were already read and applied to
        // reconstructed ImportDecls in the pre-reconstruction block above.
        {
          unsigned numImports = 0;
          StringRef importsRef = key.importsBlob;
          while (!importsRef.empty()) {
            auto [name, rest] = importsRef.split('\n');
            if (!name.empty())
              numImports++;
            importsRef = rest;
          }
          for (unsigned i = 0; i < numImports; i++)
            (void)readRange();
        }
        for (auto *D : decls) {
          setDeclRange(D);
        // Walk params of top-level function decls (not inside an IDC).
        if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
          if (auto *params = AFD->getParameters()) {
            if (auto R = readRange(); R.isValid()) {
              params->setSourceLocs(R.Start, R.End);
            }
            for (auto *param : *params)
              setDeclRange(param);
          }
        }
          if (auto *IDC = dyn_cast<IterableDeclContext>(D)) {
            for (auto *member : IDC->getMembers()) {
              setDeclRange(member);
              // Walk accessors of storage decls (not in getMembers()).
              if (auto *ASD = dyn_cast<AbstractStorageDecl>(member)) {
                for (auto *accessor : ASD->getAllAccessors()) {
                  setDeclRange(accessor);
                  // Walk accessor params.
                  if (auto *accParams = accessor->getParameters()) {
                    if (auto R = readRange(); R.isValid()) {
                      accParams->setSourceLocs(R.Start, R.End);
                    }
                    for (auto *param : *accParams)
                      setDeclRange(param);
                  }
                }
              }
              // Walk params of function decls.
              if (auto *AFD = dyn_cast<AbstractFunctionDecl>(member)) {
                if (auto *params = AFD->getParameters()) {
                  if (auto R = readRange(); R.isValid()) {
                    params->setSourceLocs(R.Start, R.End);
                  }
                  for (auto *param : *params)
                    setDeclRange(param);
                }
              }
            }
          }
        }
      }
    }
    // Reorder HasStorageAttr to be first in the attr list, matching the
    // original AST where the type checker prepends it. We do this AFTER
    // range restoration so the range blob alignment is not affected.
    for (auto *D : decls) {
      auto walkReorder = [&](Decl *decl) {
        if (auto *vd = dyn_cast<VarDecl>(decl)) {
          if (vd->hasStorage()) {
            auto &attrs = vd->getAttrs();
            if (auto *hsa = attrs.getAttribute<HasStorageAttr>()) {
              if (auto *first = *attrs.begin(); first != hsa) {
                // Only move HasStorageAttr before the first attr if the
                // first attr is explicit (not implicit). If the first attr
                // is also implicit (e.g. final_attr), keep the current order
                // since both were added by the type checker in a specific order.
                if (!first->isImplicit()) {
                  // Swap cached attr ranges so they follow the attrs.
                  auto &ctxRef = vd->getASTContext();
                  auto hsaRange = ctxRef.getCachedAttrSourceRange(hsa);
                  auto firstRange = ctxRef.getCachedAttrSourceRange(first);
                  ctxRef.setCachedAttrSourceRange(hsa, firstRange);
                  ctxRef.setCachedAttrSourceRange(first, hsaRange);
                  // Move HasStorageAttr to front.
                  attrs.removeAttribute(hsa);
                  attrs.add(hsa);
                }
              }
            }
          }
        }
      };
      walkReorder(D);
      if (auto *IDC = dyn_cast<IterableDeclContext>(D)) {
        for (auto *member : IDC->getMembers()) {
          walkReorder(member);
        }
      }
    }
    // Remove implicit TransparentAttr from deserialized accessors. The
    // serializer writes it because typeCheckDelayedFunctions() triggers
    // Remove implicit TransparentAttr from deserialized accessors where the
    // storage decl is not @usableFromInline/@inlinable. The serializer writes
    // TransparentAttr because typeCheckDelayedFunctions() triggers
    // IsAccessorTransparentRequest, which returns true for all implicit
    // getters of non-resilient stored properties. But the original dump
    // (before serialization) doesn't have it for non-inlinable accessors.
    for (auto *D : decls) {
      auto walkRemoveTransparent = [&](Decl *decl) {
        if (auto *ASD = dyn_cast<AbstractStorageDecl>(decl)) {
          // Check if the storage is @usableFromInline or @inlinable.
          bool isInlinable = ASD->getAttrs().hasAttribute<UsableFromInlineAttr>() ||
                             ASD->getAttrs().hasAttribute<InlinableAttr>();
          if (!isInlinable) {
            for (auto *accessor : ASD->getAllAccessors()) {
              auto &attrs = accessor->getAttrs();
              if (auto *ta = attrs.getAttribute<TransparentAttr>()) {
                if (ta->isImplicit()) {
                  attrs.removeAttribute(ta);
                }
              }
            }
          }
        }
      };
      walkRemoveTransparent(D);
      if (auto *IDC = dyn_cast<IterableDeclContext>(D)) {
        for (auto *member : IDC->getMembers())
          walkRemoveTransparent(member);
      }
    }
    // Reconstruct TrailingWhereClause for extensions from whereClausesBlob.
    // The serializer stores the source text of each extension's where clause.
    // We re-parse it with a minimal parser to reconstruct the RequirementRepr
    // nodes that ASTDumper renders as "where_requirements".
    if (!key.whereClausesBlob.empty()) {
      const char *wdata = key.whereClausesBlob.data();
      size_t wremaining = key.whereClausesBlob.size();
      auto readWStr = [&]() -> StringRef {
        if (wremaining < 4) return {};
        uint32_t len;
        memcpy(&len, wdata, 4);
        wdata += 4;
        wremaining -= 4;
        if (len == 0 || wremaining < len) return {};
        StringRef text(wdata, len);
        wdata += len;
        wremaining -= len;
        return text;
      };
      for (auto *D : decls) {
        if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
          StringRef whereText = readWStr();
          if (whereText.empty())
            continue;
          // Re-parse the where clause text by creating a synthetic buffer
          // and using the parser's where-clause parsing.
          std::string wrapped = whereText.str();
          auto wBufID = ctx.SourceMgr.addMemBufferCopy(wrapped);
          Parser parser(wBufID, SF, /*SIL*/ nullptr);
          if (parser.Tok.is(tok::NUM_TOKENS))
            parser.consumeTokenWithoutFeedingReceiver();
          // The text starts with 'where' keyword — parseGenericWhereClause
          // consumes it itself.
          if (!parser.Tok.is(tok::kw_where))
            continue;
          SmallVector<RequirementRepr, 4> requirements;
          SourceLoc whereLoc, endLoc;
          auto status = parser.parseGenericWhereClause(whereLoc, endLoc, requirements);
          (void)status;
          if (!requirements.empty()) {
            // Bind TypeRepr nodes to their decls so the dumper shows
            // "bind" instead of "unbound". Walk each requirement's TypeRepr
            // and resolve generic param names to the extension's generic params.
            auto *genericParams = ED->getGenericParams();
            auto bindTypeRepr = [&](TypeRepr *TR) {
              if (auto *UITR = dyn_cast_or_null<UnqualifiedIdentTypeRepr>(TR)) {
                if (!UITR->isBound()) {
                  auto name = UITR->getNameRef().getBaseIdentifier();
                  // Try to find a generic param with this name.
                  if (genericParams) {
                    for (auto *param : *genericParams) {
                      if (param->getName() == name) {
                        UITR->setValue(param, ED);
                        return;
                      }
                    }
                  }
                  // Try known protocols.
                  for (auto kp : {
                    KnownProtocolKind::Equatable,
                    KnownProtocolKind::Hashable,
                    KnownProtocolKind::Comparable,
                    KnownProtocolKind::Sendable,
                    KnownProtocolKind::Copyable,
                    KnownProtocolKind::Escapable,
                    KnownProtocolKind::BitwiseCopyable,
                  }) {
                    if (auto *proto = ctx.getProtocol(kp)) {
                      if (proto->getName() == name) {
                        UITR->setValue(proto, ED);
                        return;
                      }
                    }
                  }
                  // Try the extended nominal.
                  if (auto *nominal = ED->getExtendedNominal()) {
                    if (nominal->getName() == name) {
                      UITR->setValue(nominal, ED);
                      return;
                    }
                  }
                }
              }
            };
            for (auto &req : requirements) {
              if (req.getKind() == RequirementReprKind::TypeConstraint) {
                bindTypeRepr(req.getSubjectRepr());
                bindTypeRepr(req.getConstraintRepr());
              }
            }
            auto *twc = TrailingWhereClause::create(ctx, whereLoc, endLoc,
                                                     requirements);
            ED->setTrailingWhereClause(twc);
          }
        }
      }
    }

    // C1: Store the ModuleFile in the SourceFile to keep it alive.
    // The ASTContext arena doesn't call destructors, so we register a cleanup
    // to delete the ModuleFile when the ASTContext is destroyed.
    SF.CachedModuleFile = mf.release();
    ctx.addCleanup([sf = &SF]() {
      delete sf->CachedModuleFile;
      sf->CachedModuleFile = nullptr;
    });
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
    if (!readString(buf, key.declRangesBlob, offset)) return false;
    if (!readString(buf, key.whereClausesBlob, offset)) return false;

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

} // namespace swift
