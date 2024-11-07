//===--- ClangImporter.cpp - Import Clang Modules -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements support for loading Clang modules into Swift.
//
//===----------------------------------------------------------------------===//
#include "swift/ClangImporter/ClangImporter.h"
#include "CFTypeInfo.h"
#include "ClangDerivedConformances.h"
#include "ClangDiagnosticConsumer.h"
#include "ClangIncludePaths.h"
#include "ImporterImpl.h"
#include "SwiftDeclSynthesizer.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleNameLookup.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Platform.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Basic/Version.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Parse/ParseVersion.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Mangle.h"
#include "clang/AST/Type.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/FileEntry.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/LangStandard.h"
#include "clang/Basic/Module.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/CAS/CASOptions.h"
#include "clang/CAS/IncludeTree.h"
#include "clang/CodeGen/ObjectFilePCHContainerOperations.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/FrontendOptions.h"
#include "clang/Frontend/IncludeTreePPActions.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Frontend/Utils.h"
#include "clang/Index/IndexingAction.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Parse/Parser.h"
#include "clang/Rewrite/Frontend/Rewriters.h"
#include "clang/Sema/DelayedDiagnostic.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTWriter.h"
#include "clang/Tooling/DependencyScanning/ScanAndUpdateArgs.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/CAS/CASReference.h"
#include "llvm/CAS/ObjectStore.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CrashRecoveryContext.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileCollector.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Memory.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrefixMapper.h"
#include "llvm/Support/VirtualFileSystem.h"
#include "llvm/TextAPI/InterfaceFile.h"
#include "llvm/TextAPI/TextAPIReader.h"
#include <algorithm>
#include <memory>
#include <optional>
#include <string>

using namespace swift;
using namespace importer;

// Commonly-used Clang classes.
using clang::CompilerInstance;
using clang::CompilerInvocation;

#pragma mark Internal data structures

namespace {
  class HeaderImportCallbacks : public clang::PPCallbacks {
    ClangImporter::Implementation &Impl;
  public:
    HeaderImportCallbacks(ClangImporter::Implementation &impl)
      : Impl(impl) {}

    void handleImport(const clang::Module *imported) {
      if (!imported)
        return;
      Impl.ImportedHeaderExports.push_back(
          const_cast<clang::Module *>(imported));
    }

    void InclusionDirective(
        clang::SourceLocation HashLoc, const clang::Token &IncludeTok,
        StringRef FileName, bool IsAngled, clang::CharSourceRange FilenameRange,
        clang::OptionalFileEntryRef File, StringRef SearchPath,
        StringRef RelativePath, const clang::Module *SuggestedModule,
        bool ModuleImported,
        clang::SrcMgr::CharacteristicKind FileType) override {
      handleImport(ModuleImported ? SuggestedModule : nullptr);
    }

    void moduleImport(clang::SourceLocation ImportLoc,
                              clang::ModuleIdPath Path,
                              const clang::Module *Imported) override {
      handleImport(Imported);
    }
  };

  class PCHDeserializationCallbacks : public clang::ASTDeserializationListener {
    ClangImporter::Implementation &Impl;
  public:
    explicit PCHDeserializationCallbacks(ClangImporter::Implementation &impl)
      : Impl(impl) {}
    void ModuleImportRead(clang::serialization::SubmoduleID ID,
                          clang::SourceLocation ImportLoc) override {
      if (Impl.IsReadingBridgingPCH) {
        Impl.PCHImportedSubmodules.push_back(ID);
      }
    }
  };

  class HeaderParsingASTConsumer : public clang::ASTConsumer {
    SmallVector<clang::DeclGroupRef, 4> DeclGroups;
    PCHDeserializationCallbacks PCHCallbacks;
  public:
    explicit HeaderParsingASTConsumer(ClangImporter::Implementation &impl)
      : PCHCallbacks(impl) {}
    void
    HandleTopLevelDeclInObjCContainer(clang::DeclGroupRef decls) override {
      DeclGroups.push_back(decls);
    }

    ArrayRef<clang::DeclGroupRef> getAdditionalParsedDecls() {
      return DeclGroups;
    }

    clang::ASTDeserializationListener *GetASTDeserializationListener() override {
      return &PCHCallbacks;
    }

    void reset() {
      DeclGroups.clear();
    }
  };

  class ParsingAction : public clang::ASTFrontendAction {
    ASTContext &Ctx;
    ClangImporter &Importer;
    ClangImporter::Implementation &Impl;
    const ClangImporterOptions &ImporterOpts;
    std::string SwiftPCHHash;
  public:
    explicit ParsingAction(ASTContext &ctx,
                           ClangImporter &importer,
                           ClangImporter::Implementation &impl,
                           const ClangImporterOptions &importerOpts,
                           std::string swiftPCHHash)
      : Ctx(ctx), Importer(importer), Impl(impl), ImporterOpts(importerOpts),
        SwiftPCHHash(swiftPCHHash) {}
    std::unique_ptr<clang::ASTConsumer>
    CreateASTConsumer(clang::CompilerInstance &CI, StringRef InFile) override {
      return std::make_unique<HeaderParsingASTConsumer>(Impl);
    }
    bool BeginSourceFileAction(clang::CompilerInstance &CI) override {
      // Prefer frameworks over plain headers.
      // We add search paths here instead of when building the initial invocation
      // so that (a) we use the same code as search paths for imported modules,
      // and (b) search paths are always added after -Xcc options.
      SearchPathOptions &searchPathOpts = Ctx.SearchPathOpts;
      for (const auto &framepath : searchPathOpts.getFrameworkSearchPaths()) {
        Importer.addSearchPath(framepath.Path, /*isFramework*/true,
                               framepath.IsSystem);
      }

      for (const auto &path : searchPathOpts.getImportSearchPaths()) {
        Importer.addSearchPath(path, /*isFramework*/false, /*isSystem=*/false);
      }

      auto PCH =
          Importer.getOrCreatePCH(ImporterOpts, SwiftPCHHash, /*Cached=*/true);
      if (PCH.has_value()) {
        Impl.getClangInstance()->getPreprocessorOpts().ImplicitPCHInclude =
            PCH.value();
        Impl.IsReadingBridgingPCH = true;
        Impl.setSinglePCHImport(PCH.value());
      }

      return true;
    }
  };

  class StdStringMemBuffer : public llvm::MemoryBuffer {
    const std::string storage;
    const std::string name;
  public:
    StdStringMemBuffer(std::string &&source, StringRef name)
        : storage(std::move(source)), name(name.str()) {
      init(storage.data(), storage.data() + storage.size(),
           /*null-terminated=*/true);
    }

    StringRef getBufferIdentifier() const override {
      return name;
    }

    BufferKind getBufferKind() const override {
      return MemoryBuffer_Malloc;
    }
  };

  class ZeroFilledMemoryBuffer : public llvm::MemoryBuffer {
    const std::string name;
  public:
    explicit ZeroFilledMemoryBuffer(size_t size, StringRef name)
        : name(name.str()) {
      assert(size > 0);
      std::error_code error;
      llvm::sys::MemoryBlock memory =
          llvm::sys::Memory::allocateMappedMemory(size, nullptr,
                                                  llvm::sys::Memory::MF_READ,
                                                  error);
      assert(!error && "failed to allocated read-only zero-filled memory");
      init(static_cast<char *>(memory.base()),
           static_cast<char *>(memory.base()) + memory.allocatedSize() - 1,
           /*null-terminated*/true);
    }

    ~ZeroFilledMemoryBuffer() override {
      llvm::sys::MemoryBlock memory{const_cast<char *>(getBufferStart()),
        getBufferSize()};
      std::error_code error = llvm::sys::Memory::releaseMappedMemory(memory);
      assert(!error && "failed to deallocate read-only zero-filled memory");
      (void)error;
    }

    ZeroFilledMemoryBuffer(const ZeroFilledMemoryBuffer &) = delete;
    ZeroFilledMemoryBuffer(ZeroFilledMemoryBuffer &&) = delete;
    void operator=(const ZeroFilledMemoryBuffer &) = delete;
    void operator=(ZeroFilledMemoryBuffer &&) = delete;

    StringRef getBufferIdentifier() const override {
      return name;
    }
    BufferKind getBufferKind() const override {
      return MemoryBuffer_MMap;
    }
  };
} // end anonymous namespace

namespace {
class BridgingPPTracker : public clang::PPCallbacks {
  ClangImporter::Implementation &Impl;

public:
  BridgingPPTracker(ClangImporter::Implementation &Impl)
    : Impl(Impl) {}

private:
  static unsigned getNumModuleIdentifiers(const clang::Module *Mod) {
    unsigned Result = 1;
    while (Mod->Parent) {
      Mod = Mod->Parent;
      ++Result;
    }
    return Result;
  }

  void InclusionDirective(clang::SourceLocation HashLoc,
                          const clang::Token &IncludeTok, StringRef FileName,
                          bool IsAngled, clang::CharSourceRange FilenameRange,
                          clang::OptionalFileEntryRef File,
                          StringRef SearchPath, StringRef RelativePath,
                          const clang::Module *SuggestedModule,
                          bool ModuleImported,
                          clang::SrcMgr::CharacteristicKind FileType) override {
    if (!ModuleImported) {
      if (File)
        Impl.BridgeHeaderFiles.insert(*File);
      return;
    }
    // Synthesize identifier locations.
    SmallVector<clang::SourceLocation, 4> IdLocs;
    for (unsigned I = 0, E = getNumModuleIdentifiers(SuggestedModule); I != E; ++I)
      IdLocs.push_back(HashLoc);
    handleImport(HashLoc, IdLocs, SuggestedModule);
  }

  void moduleImport(clang::SourceLocation ImportLoc,
                    clang::ModuleIdPath Path,
                    const clang::Module *Imported) override {
    if (!Imported)
      return;
    SmallVector<clang::SourceLocation, 4> IdLocs;
    for (auto &P : Path)
      IdLocs.push_back(P.second);
    handleImport(ImportLoc, IdLocs, Imported);
  }

  void handleImport(clang::SourceLocation ImportLoc,
                    ArrayRef<clang::SourceLocation> IdLocs,
                    const clang::Module *Imported) {
    clang::ASTContext &ClangCtx = Impl.getClangASTContext();
    clang::ImportDecl *ClangImport = clang::ImportDecl::Create(ClangCtx,
                                            ClangCtx.getTranslationUnitDecl(),
                                            ImportLoc,
                                           const_cast<clang::Module*>(Imported),
                                            IdLocs);
    Impl.BridgeHeaderTopLevelImports.push_back(ClangImport);
  }

  void MacroDefined(const clang::Token &MacroNameTok,
                    const clang::MacroDirective *MD) override {
    Impl.BridgeHeaderMacros.push_back(MacroNameTok.getIdentifierInfo());
  }
};

class ClangImporterDependencyCollector : public clang::DependencyCollector
{
  llvm::StringSet<> ExcludedPaths;
  /// The FileCollector is used by LLDB to generate reproducers. It's not used
  /// by Swift to track dependencies.
  std::shared_ptr<llvm::FileCollectorBase> FileCollector;
  const IntermoduleDepTrackingMode Mode;

public:
  ClangImporterDependencyCollector(
      IntermoduleDepTrackingMode Mode,
      std::shared_ptr<llvm::FileCollectorBase> FileCollector)
      : FileCollector(FileCollector), Mode(Mode) {}

  void excludePath(StringRef filename) {
    ExcludedPaths.insert(filename);
  }

  bool isClangImporterSpecialName(StringRef Filename) {
    using ImporterImpl = ClangImporter::Implementation;
    return (Filename == ImporterImpl::moduleImportBufferName
            || Filename == ImporterImpl::bridgingHeaderBufferName);
  }

  bool needSystemDependencies() override {
    return Mode == IntermoduleDepTrackingMode::IncludeSystem;
  }

  bool sawDependency(StringRef Filename, bool FromClangModule,
                     bool IsSystem, bool IsClangModuleFile,
                     bool IsMissing) override {
    if (!clang::DependencyCollector::sawDependency(Filename, FromClangModule,
                                                   IsSystem, IsClangModuleFile,
                                                   IsMissing))
      return false;
    // Currently preserving older ClangImporter behavior of ignoring .pcm
    // file dependencies, but possibly revisit?
    if (IsClangModuleFile
        || isClangImporterSpecialName(Filename)
        || ExcludedPaths.count(Filename))
      return false;
    return true;
  }

  void maybeAddDependency(StringRef Filename, bool FromModule, bool IsSystem,
                          bool IsModuleFile, bool IsMissing) override {
    if (FileCollector)
      FileCollector->addFile(Filename);
    clang::DependencyCollector::maybeAddDependency(
        Filename, FromModule, IsSystem, IsModuleFile, IsMissing);
  }
};
} // end anonymous namespace

std::shared_ptr<clang::DependencyCollector>
ClangImporter::createDependencyCollector(
    IntermoduleDepTrackingMode Mode,
    std::shared_ptr<llvm::FileCollectorBase> FileCollector) {
  return std::make_shared<ClangImporterDependencyCollector>(Mode,
                                                            FileCollector);
}

bool ClangImporter::isKnownCFTypeName(llvm::StringRef name) {
  return CFPointeeInfo::isKnownCFTypeName(name);
}

void ClangImporter::Implementation::addBridgeHeaderTopLevelDecls(
    clang::Decl *D) {
  if (shouldIgnoreBridgeHeaderTopLevelDecl(D))
    return;

  BridgeHeaderTopLevelDecls.push_back(D);
}

bool importer::isForwardDeclOfType(const clang::Decl *D) {
  if (auto *ID = dyn_cast<clang::ObjCInterfaceDecl>(D)) {
    if (!ID->isThisDeclarationADefinition())
      return true;
  } else if (auto PD = dyn_cast<clang::ObjCProtocolDecl>(D)) {
    if (!PD->isThisDeclarationADefinition())
      return true;
  } else if (auto TD = dyn_cast<clang::TagDecl>(D)) {
    if (!TD->isThisDeclarationADefinition())
      return true;
  }
  return false;
}

bool ClangImporter::Implementation::shouldIgnoreBridgeHeaderTopLevelDecl(
    clang::Decl *D) {
  return importer::isForwardDeclOfType(D);
}

ClangImporter::ClangImporter(ASTContext &ctx,
                             DependencyTracker *tracker,
                             DWARFImporterDelegate *dwarfImporterDelegate)
    : ClangModuleLoader(tracker),
      Impl(*new Implementation(ctx, tracker, dwarfImporterDelegate)) {
}

ClangImporter::~ClangImporter() {
  delete &Impl;
}

#pragma mark Module loading

static bool clangSupportsPragmaAttributeWithSwiftAttr() {
  clang::AttributeCommonInfo swiftAttrInfo(clang::SourceRange(),
     clang::AttributeCommonInfo::AT_SwiftAttr,
     clang::AttributeCommonInfo::Form::GNU());
  auto swiftAttrParsedInfo = clang::ParsedAttrInfo::get(swiftAttrInfo);
  return swiftAttrParsedInfo.IsSupportedByPragmaAttribute;
}

static inline bool isPCHFilenameExtension(StringRef path) {
  return llvm::sys::path::extension(path)
    .ends_with(file_types::getExtension(file_types::TY_PCH));
}

void importer::getNormalInvocationArguments(
    std::vector<std::string> &invocationArgStrs, ASTContext &ctx,
    bool ignoreClangTarget) {
  const auto &LangOpts = ctx.LangOpts;
  llvm::Triple triple = LangOpts.Target;
  // Use clang specific target triple if given.
  if (LangOpts.ClangTarget.has_value() && !ignoreClangTarget) {
    triple = LangOpts.ClangTarget.value();
  }
  SearchPathOptions &searchPathOpts = ctx.SearchPathOpts;
  ClangImporterOptions &importerOpts = ctx.ClangImporterOpts;
  auto languageVersion = ctx.LangOpts.EffectiveLanguageVersion;

  if (isPCHFilenameExtension(importerOpts.BridgingHeader)) {
    invocationArgStrs.insert(invocationArgStrs.end(), {
        "-include-pch", importerOpts.BridgingHeader
    });
  }

  // If there are no shims in the resource dir, add a search path in the SDK.
  SmallString<128> shimsPath(searchPathOpts.RuntimeResourcePath);
  llvm::sys::path::append(shimsPath, "shims");
  if (!llvm::sys::fs::exists(shimsPath)) {
    shimsPath = searchPathOpts.getSDKPath();
    llvm::sys::path::append(shimsPath, "usr", "lib", "swift", "shims");
    invocationArgStrs.insert(invocationArgStrs.end(),
                             {"-isystem", std::string(shimsPath.str())});
  }

  // Construct the invocation arguments for the current target.
  // Add target-independent options first.
  invocationArgStrs.insert(invocationArgStrs.end(), {
      // Don't emit LLVM IR.
      "-fsyntax-only",

      // Enable block support.
      "-fblocks",

      languageVersion.preprocessorDefinition("__swift__", {10000, 100, 1}),

      "-fretain-comments-from-system-headers",

      "-isystem", searchPathOpts.RuntimeResourcePath,
  });

  // Enable Position Independence.  `-fPIC` is not supported on Windows, which
  // is implicitly position independent.
  if (!triple.isOSWindows())
    invocationArgStrs.insert(invocationArgStrs.end(), {"-fPIC"});

  // Enable modules.
  invocationArgStrs.insert(invocationArgStrs.end(), {
      "-fmodules",
      "-Xclang", "-fmodule-feature", "-Xclang", "swift"
  });

  bool EnableCXXInterop = LangOpts.EnableCXXInterop;

  if (LangOpts.EnableObjCInterop) {
    invocationArgStrs.insert(invocationArgStrs.end(), {"-fobjc-arc"});
    // TODO: Investigate whether 7.0 is a suitable default version.
    if (!triple.isOSDarwin())
      invocationArgStrs.insert(invocationArgStrs.end(),
                               {"-fobjc-runtime=ios-7.0"});

    invocationArgStrs.insert(invocationArgStrs.end(), {
      "-x", EnableCXXInterop ? "objective-c++" : "objective-c",
    });
  } else {
    invocationArgStrs.insert(invocationArgStrs.end(), {
      "-x", EnableCXXInterop ? "c++" : "c",
    });
  }

  {
    const clang::LangStandard &stdcxx =
#if defined(CLANG_DEFAULT_STD_CXX)
        *clang::LangStandard::getLangStandardForName(CLANG_DEFAULT_STD_CXX);
#else
        clang::LangStandard::getLangStandardForKind(
            clang::LangStandard::lang_gnucxx17);
#endif

    const clang::LangStandard &stdc =
#if defined(CLANG_DEFAULT_STD_C)
        *clang::LangStandard::getLangStandardForName(CLANG_DEFAULT_STD_C);
#else
        clang::LangStandard::getLangStandardForKind(
            clang::LangStandard::lang_gnu11);
#endif

    invocationArgStrs.insert(invocationArgStrs.end(), {
      (Twine("-std=") + StringRef(EnableCXXInterop ? stdcxx.getName()
                                                   : stdc.getName())).str()
    });
  }

  if (LangOpts.EnableCXXInterop) {
    if (auto path = getCxxShimModuleMapPath(searchPathOpts, triple)) {
      invocationArgStrs.push_back((Twine("-fmodule-map-file=") + *path).str());
    }
  }

  // Set C language options.
  if (triple.isOSDarwin()) {
    invocationArgStrs.insert(invocationArgStrs.end(), {
      // Avoid including the iso646.h header because some headers from OS X
      // frameworks are broken by it.
      "-D_ISO646_H_", "-D__ISO646_H",

      // Request new APIs from AppKit.
      "-DSWIFT_SDK_OVERLAY_APPKIT_EPOCH=2",

      // Request new APIs from Foundation.
      "-DSWIFT_SDK_OVERLAY_FOUNDATION_EPOCH=8",

      // Request new APIs from SceneKit.
      "-DSWIFT_SDK_OVERLAY2_SCENEKIT_EPOCH=3",

      // Request new APIs from GameplayKit.
      "-DSWIFT_SDK_OVERLAY_GAMEPLAYKIT_EPOCH=1",

      // Request new APIs from SpriteKit.
      "-DSWIFT_SDK_OVERLAY_SPRITEKIT_EPOCH=1",

      // Request new APIs from CoreImage.
      "-DSWIFT_SDK_OVERLAY_COREIMAGE_EPOCH=2",

      // Request new APIs from libdispatch.
      "-DSWIFT_SDK_OVERLAY_DISPATCH_EPOCH=2",

      // Request new APIs from libpthread
      "-DSWIFT_SDK_OVERLAY_PTHREAD_EPOCH=1",

      // Request new APIs from CoreGraphics.
      "-DSWIFT_SDK_OVERLAY_COREGRAPHICS_EPOCH=0",

      // Request new APIs from UIKit.
      "-DSWIFT_SDK_OVERLAY_UIKIT_EPOCH=2",

      // Backwards compatibility for headers that were checking this instead of
      // '__swift__'.
      "-DSWIFT_CLASS_EXTRA=",
    });

    // Indicate that using '__attribute__((swift_attr))' with '@Sendable' and
    // '@_nonSendable' on Clang declarations is fully supported, including the
    // 'attribute push' pragma.
    if (clangSupportsPragmaAttributeWithSwiftAttr())
      invocationArgStrs.push_back("-D__SWIFT_ATTR_SUPPORTS_SENDABLE_DECLS=1");

    if (triple.isXROS()) {
      // FIXME: This is a gnarly hack until some macros get adjusted in the SDK.
      invocationArgStrs.insert(invocationArgStrs.end(), {
        "-DOS_OBJECT_HAVE_OBJC_SUPPORT=1",
      });
    }

    // Get the version of this compiler and pass it to C/Objective-C
    // declarations.
    auto V = version::getCurrentCompilerVersion();
    if (!V.empty()) {
      // Note: Prior to Swift 5.7, the "Y" version component was omitted and the
      // "X" component resided in its digits.
      invocationArgStrs.insert(invocationArgStrs.end(), {
        V.preprocessorDefinition("__SWIFT_COMPILER_VERSION",
                                 {1000000000000,   // X
                                     1000000000,   // Y
                                        1000000,   // Z
                                           1000,   // a
                                              1}), // b
      });
    }
  } else {
    // Ideally we should turn this on for all Glibc targets that are actually
    // using Glibc or a libc that respects that flag. This will cause some
    // source breakage however (specifically with strerror_r()) on Linux
    // without a workaround.
    if (triple.isOSFuchsia() || triple.isAndroid() || triple.isMusl()) {
      // Many of the modern libc features are hidden behind feature macros like
      // _GNU_SOURCE or _XOPEN_SOURCE.
      invocationArgStrs.insert(invocationArgStrs.end(), {
        "-D_GNU_SOURCE",
      });
    }

    if (triple.isOSWindows()) {
      switch (triple.getArch()) {
      default: llvm_unreachable("unsupported Windows architecture");
      case llvm::Triple::arm:
      case llvm::Triple::thumb:
        invocationArgStrs.insert(invocationArgStrs.end(), {"-D_ARM_"});
        break;
      case llvm::Triple::aarch64:
      case llvm::Triple::aarch64_32:
        invocationArgStrs.insert(invocationArgStrs.end(), {"-D_ARM64_"});
        break;
      case llvm::Triple::x86:
        invocationArgStrs.insert(invocationArgStrs.end(), {"-D_X86_"});
        break;
      case llvm::Triple::x86_64:
        invocationArgStrs.insert(invocationArgStrs.end(), {"-D_AMD64_"});
        break;
      }
    }
  }

  // If we support SendingArgsAndResults, set the -D flag to signal that it
  // is supported.
  if (LangOpts.hasFeature(Feature::SendingArgsAndResults))
    invocationArgStrs.push_back("-D__SWIFT_ATTR_SUPPORTS_SENDING=1");

  // Indicate that the compiler will respect macros applied to imported
  // declarations via '__attribute__((swift_attr("@...")))'.
  if (LangOpts.hasFeature(Feature::MacrosOnImports))
    invocationArgStrs.push_back("-D__SWIFT_ATTR_SUPPORTS_MACROS=1");

  if (searchPathOpts.getSDKPath().empty()) {
    invocationArgStrs.push_back("-Xclang");
    invocationArgStrs.push_back("-nostdsysteminc");
  } else {
    if (triple.isWindowsMSVCEnvironment()) {
      llvm::SmallString<261> path; // MAX_PATH + 1
      path = searchPathOpts.getSDKPath();
      llvm::sys::path::append(path, "usr", "include");
      llvm::sys::path::native(path);

      invocationArgStrs.push_back("-isystem");
      invocationArgStrs.push_back(std::string(path.str()));
    } else {
      // On Darwin, Clang uses -isysroot to specify the include
      // system root. On other targets, it seems to use --sysroot.
      if (triple.isOSDarwin()) {
        invocationArgStrs.push_back("-isysroot");
        invocationArgStrs.push_back(searchPathOpts.getSDKPath().str());
      } else {
        if (auto sysroot = searchPathOpts.getSysRoot()) {
          invocationArgStrs.push_back("--sysroot");
          invocationArgStrs.push_back(sysroot->str());
        } else {
          invocationArgStrs.push_back("--sysroot");
          invocationArgStrs.push_back(searchPathOpts.getSDKPath().str());
        }
      }
    }
  }

  const std::string &moduleCachePath = importerOpts.ModuleCachePath;
  const std::string &scannerCachePath = importerOpts.ClangScannerModuleCachePath;
  // If a scanner cache is specified, this must be a scanning action. Prefer this
  // path for the Clang scanner to cache its Scanning PCMs.
  if (!scannerCachePath.empty()) {
    invocationArgStrs.push_back("-fmodules-cache-path=");
    invocationArgStrs.back().append(scannerCachePath);
  } else if (!moduleCachePath.empty() && !importerOpts.DisableImplicitClangModules) {
    invocationArgStrs.push_back("-fmodules-cache-path=");
    invocationArgStrs.back().append(moduleCachePath);
  }

  if (importerOpts.DisableImplicitClangModules) {
    invocationArgStrs.push_back("-fno-implicit-modules");
    invocationArgStrs.push_back("-fno-implicit-module-maps");
  }

  if (ctx.SearchPathOpts.DisableModulesValidateSystemDependencies) {
    invocationArgStrs.push_back("-fno-modules-validate-system-headers");
  } else {
    invocationArgStrs.push_back("-fmodules-validate-system-headers");
  }

  if (importerOpts.DetailedPreprocessingRecord) {
    invocationArgStrs.insert(invocationArgStrs.end(), {
      "-Xclang", "-detailed-preprocessing-record",
      "-Xclang", "-fmodule-format=raw",
    });
  } else {
    invocationArgStrs.insert(invocationArgStrs.end(), {
      "-Xclang", "-fmodule-format=obj",
    });
  }

  // Enable API notes alongside headers/in frameworks.
  invocationArgStrs.push_back("-fapinotes-modules");
  invocationArgStrs.push_back("-fapinotes-swift-version=" +
                              languageVersion.asAPINotesVersionString());

  // Prefer `-sdk` paths.
  if (!searchPathOpts.getSDKPath().empty()) {
    llvm::SmallString<261> path{searchPathOpts.getSDKPath()};
    llvm::sys::path::append(path, "usr", "lib", "swift", "apinotes");

    invocationArgStrs.push_back("-iapinotes-modules");
    invocationArgStrs.push_back(path.str().str());
  }

  // Fallback to "legacy" `-resource-dir` paths.
  {
    llvm::SmallString<261> path{searchPathOpts.RuntimeResourcePath};
    llvm::sys::path::append(path, "apinotes");

    invocationArgStrs.push_back("-iapinotes-modules");
    invocationArgStrs.push_back(path.str().str());
  }
}

static void
getEmbedBitcodeInvocationArguments(std::vector<std::string> &invocationArgStrs,
                                   ASTContext &ctx) {
  invocationArgStrs.insert(invocationArgStrs.end(), {
    // Backend mode.
    "-fembed-bitcode",

    // ...but Clang isn't doing the emission.
    "-fsyntax-only",

    "-x", "ir",
  });
}

void
importer::addCommonInvocationArguments(
    std::vector<std::string> &invocationArgStrs,
    ASTContext &ctx, bool ignoreClangTarget) {
  using ImporterImpl = ClangImporter::Implementation;
  llvm::Triple triple = ctx.LangOpts.Target;
  // Use clang specific target triple if given.
  if (ctx.LangOpts.ClangTarget.has_value() && !ignoreClangTarget) {
    triple = ctx.LangOpts.ClangTarget.value();
  }
  SearchPathOptions &searchPathOpts = ctx.SearchPathOpts;
  const ClangImporterOptions &importerOpts = ctx.ClangImporterOpts;

  invocationArgStrs.push_back("-target");
  invocationArgStrs.push_back(triple.str());

  if (ctx.LangOpts.SDKVersion) {
    invocationArgStrs.push_back("-Xclang");
    invocationArgStrs.push_back(
        "-target-sdk-version=" + ctx.LangOpts.SDKVersion->getAsString());
  }

  invocationArgStrs.push_back(ImporterImpl::moduleImportBufferName);

  if (ctx.LangOpts.EnableAppExtensionRestrictions) {
    invocationArgStrs.push_back("-fapplication-extension");
  }

  if (!importerOpts.TargetCPU.empty()) {
    switch (triple.getArch()) {
    case llvm::Triple::x86:
    case llvm::Triple::x86_64:
      // `-mcpu` is deprecated and an alias for `-mtune`. We need to pass
      // `-march` and `-mtune` to behave identically to the `apple-a\d+` cases
      // below.
      invocationArgStrs.push_back("-march=" + importerOpts.TargetCPU);
      invocationArgStrs.push_back("-mtune=" + importerOpts.TargetCPU);
      break;
    default:
      invocationArgStrs.push_back("-mcpu=" + importerOpts.TargetCPU);
      break;
    }
  } else if (triple.isOSDarwin()) {
    // Special case CPU based on known deployments:
    //   - arm64 deploys to apple-a7
    //   - arm64 on macOS
    //   - arm64 for iOS/tvOS/watchOS simulators
    //   - arm64e deploys to apple-a12
    // and arm64e (everywhere) and arm64e macOS defaults to the "apple-a12" CPU
    // for Darwin, but Clang only detects this if we use -arch.
    if (triple.getArchName() == "arm64e")
      invocationArgStrs.push_back("-mcpu=apple-a12");
    else if (triple.isAArch64() && triple.isMacOSX())
      invocationArgStrs.push_back("-mcpu=apple-a12");
    else if (triple.isAArch64() && triple.isSimulatorEnvironment() &&
             (triple.isiOS() || triple.isWatchOS()))
      invocationArgStrs.push_back("-mcpu=apple-a12");
    else if (triple.isAArch64() && triple.isSimulatorEnvironment() &&
             triple.isXROS())
      invocationArgStrs.push_back("-mcpu=apple-a12");
    else if (triple.getArch() == llvm::Triple::aarch64 ||
             triple.getArch() == llvm::Triple::aarch64_32 ||
             triple.getArch() == llvm::Triple::aarch64_be) {
      invocationArgStrs.push_back("-mcpu=apple-a7");
    }
  } else if (triple.getArch() == llvm::Triple::systemz) {
    invocationArgStrs.push_back("-march=z13");
  }

  if (triple.getArch() == llvm::Triple::x86_64) {
    // Enable double wide atomic intrinsics on every x86_64 target.
    // (This is the default on Darwin, but not so on other platforms.)
    invocationArgStrs.push_back("-mcx16");
  }

  if (triple.isOSDarwin()) {
    if (auto variantTriple = ctx.LangOpts.TargetVariant) {
      // Passing the -target-variant along to clang causes clang's
      // CodeGenerator to emit zippered .o files.
      invocationArgStrs.push_back("-darwin-target-variant");
      invocationArgStrs.push_back(variantTriple->str());
    }

    if (ctx.LangOpts.VariantSDKVersion) {
      invocationArgStrs.push_back("-Xclang");
      invocationArgStrs.push_back(
        ("-darwin-target-variant-sdk-version=" +
         ctx.LangOpts.VariantSDKVersion->getAsString()));
    }
  }

  if (std::optional<StringRef> R = ctx.SearchPathOpts.getWinSDKRoot()) {
    invocationArgStrs.emplace_back("-Xmicrosoft-windows-sdk-root");
    invocationArgStrs.emplace_back(*R);
  }
  if (std::optional<StringRef> V = ctx.SearchPathOpts.getWinSDKVersion()) {
    invocationArgStrs.emplace_back("-Xmicrosoft-windows-sdk-version");
    invocationArgStrs.emplace_back(*V);
  }
  if (std::optional<StringRef> R = ctx.SearchPathOpts.getVCToolsRoot()) {
    invocationArgStrs.emplace_back("-Xmicrosoft-visualc-tools-root");
    invocationArgStrs.emplace_back(*R);
  }
  if (std::optional<StringRef> V = ctx.SearchPathOpts.getVCToolsVersion()) {
    invocationArgStrs.emplace_back("-Xmicrosoft-visualc-tools-version");
    invocationArgStrs.emplace_back(*V);
  }

  if (!importerOpts.Optimization.empty()) {
    invocationArgStrs.push_back(importerOpts.Optimization);
  }

  const std::string &overrideResourceDir = importerOpts.OverrideResourceDir;
  if (overrideResourceDir.empty()) {
    llvm::SmallString<128> resourceDir(searchPathOpts.RuntimeResourcePath);

    // Adjust the path to refer to our copy of the Clang resource directory
    // under 'lib/swift/clang', which is either a real resource directory or a
    // symlink to one inside of a full Clang installation.
    //
    // The rationale for looking under the Swift resource directory and not
    // assuming that the Clang resource directory is located next to it is that
    // Swift, when installed separately, should not need to install files in
    // directories that are not "owned" by it.
    llvm::sys::path::append(resourceDir, "clang");

    // Set the Clang resource directory to the path we computed.
    invocationArgStrs.push_back("-resource-dir");
    invocationArgStrs.push_back(std::string(resourceDir.str()));
  } else {
    invocationArgStrs.push_back("-resource-dir");
    invocationArgStrs.push_back(overrideResourceDir);
  }

  if (!importerOpts.IndexStorePath.empty()) {
    invocationArgStrs.push_back("-index-store-path");
    invocationArgStrs.push_back(importerOpts.IndexStorePath);
  }

  invocationArgStrs.push_back("-fansi-escape-codes");

  if (importerOpts.ValidateModulesOnce) {
    invocationArgStrs.push_back("-fmodules-validate-once-per-build-session");
    invocationArgStrs.push_back("-fbuild-session-file=" + importerOpts.BuildSessionFilePath);
  }

  for (auto extraArg : importerOpts.ExtraArgs) {
    invocationArgStrs.push_back(extraArg);
  }
}

bool ClangImporter::canReadPCH(StringRef PCHFilename) {
  if (!llvm::sys::fs::exists(PCHFilename))
    return false;

  // FIXME: The following attempts to do an initial ReadAST invocation to verify
  // the PCH, without causing trouble for the existing CompilerInstance.
  // Look into combining creating the ASTReader along with verification + update
  // if necessary, so that we can create and use one ASTReader in the common case
  // when there is no need for update.
  clang::CompilerInstance CI(Impl.Instance->getPCHContainerOperations(),
                             &Impl.Instance->getModuleCache());
  auto invocation =
      std::make_shared<clang::CompilerInvocation>(*Impl.Invocation);
  invocation->getPreprocessorOpts().DisablePCHOrModuleValidation =
      clang::DisableValidationForModuleKind::None;
  invocation->getHeaderSearchOpts().ModulesValidateSystemHeaders = true;
  invocation->getLangOpts().NeededByPCHOrCompilationUsesPCH = true;
  invocation->getLangOpts().CacheGeneratedPCH = true;

  // ClangImporter::create adds a remapped MemoryBuffer that we don't need
  // here.  Moreover, it's a raw pointer owned by the preprocessor options; if
  // we don't clear the range then both the original and new CompilerInvocation
  // will try to free it.
  invocation->getPreprocessorOpts().RemappedFileBuffers.clear();

  CI.setInvocation(std::move(invocation));
  CI.setTarget(&Impl.Instance->getTarget());
  CI.setDiagnostics(
      &*clang::CompilerInstance::createDiagnostics(new clang::DiagnosticOptions()));

  // Note: Reusing the file manager is safe; this is a component that's already
  // reused when building PCM files for the module cache.
  CI.createSourceManager(Impl.Instance->getFileManager());
  auto &clangSrcMgr = CI.getSourceManager();
  auto FID = clangSrcMgr.createFileID(
                        std::make_unique<ZeroFilledMemoryBuffer>(1, "<main>"));
  clangSrcMgr.setMainFileID(FID);
  auto &diagConsumer = CI.getDiagnosticClient();
  diagConsumer.BeginSourceFile(CI.getLangOpts());
  SWIFT_DEFER {
    diagConsumer.EndSourceFile();
  };

  // Pass in TU_Complete, which is the default mode for the Preprocessor
  // constructor and the right one for reading a PCH.
  CI.createPreprocessor(clang::TU_Complete);
  CI.createASTContext();
  CI.createASTReader();
  clang::ASTReader &Reader = *CI.getASTReader();

  auto failureCapabilities =
    clang::ASTReader::ARR_Missing |
    clang::ASTReader::ARR_OutOfDate |
    clang::ASTReader::ARR_VersionMismatch;

  // If a PCH was output with errors, it may not have serialized all its
  // inputs. If there was a change to the search path or a headermap now
  // exists where it didn't previously, it's possible those inputs will now be
  // found. Ideally we would only rebuild in this particular case rather than
  // any error in general, but explicit module builds are the real solution
  // there. For now, just treat PCH with errors as out of date.
  failureCapabilities |= clang::ASTReader::ARR_TreatModuleWithErrorsAsOutOfDate;

  auto result = Reader.ReadAST(PCHFilename, clang::serialization::MK_PCH,
                               clang::SourceLocation(), failureCapabilities);
  switch (result) {
  case clang::ASTReader::Success:
    return true;
  case clang::ASTReader::Failure:
  case clang::ASTReader::Missing:
  case clang::ASTReader::OutOfDate:
  case clang::ASTReader::VersionMismatch:
    return false;
  case clang::ASTReader::ConfigurationMismatch:
  case clang::ASTReader::HadErrors:
    assert(0 && "unexpected ASTReader failure for PCH validation");
    return false;
  }
  llvm_unreachable("unhandled result");
}

std::string ClangImporter::getOriginalSourceFile(StringRef PCHFilename) {
  return clang::ASTReader::getOriginalSourceFile(
      PCHFilename.str(), Impl.Instance->getFileManager(),
      Impl.Instance->getPCHContainerReader(), Impl.Instance->getDiagnostics());
}

void ClangImporter::addClangInvovcationDependencies(
    std::vector<std::string> &files) {
  auto addFiles = [&files](const auto &F) {
    files.insert(files.end(), F.begin(), F.end());
  };
  auto &invocation = *Impl.Invocation;
  // FIXME: Add file dependencies that are not accounted. The long term solution
  // is to do a dependency scanning for clang importer and use that directly.
  SmallVector<std::string, 4> HeaderMapFileNames;
  Impl.Instance->getPreprocessor().getHeaderSearchInfo().getHeaderMapFileNames(
      HeaderMapFileNames);
  addFiles(HeaderMapFileNames);
  addFiles(invocation.getHeaderSearchOpts().VFSOverlayFiles);
  // FIXME: Should not depend on working directory. Build system/swift driver
  // should not pass working directory here but if that option is passed,
  // repect that and add that into CASFS.
  auto CWD = invocation.getFileSystemOpts().WorkingDir;
  if (!CWD.empty())
    files.push_back(CWD);
}

std::optional<std::string>
ClangImporter::getPCHFilename(const ClangImporterOptions &ImporterOptions,
                              StringRef SwiftPCHHash, bool &isExplicit) {
  if (isPCHFilenameExtension(ImporterOptions.BridgingHeader)) {
    isExplicit = true;
    return ImporterOptions.BridgingHeader;
  }
  isExplicit = false;

  const auto &BridgingHeader = ImporterOptions.BridgingHeader;
  const auto &PCHOutputDir = ImporterOptions.PrecompiledHeaderOutputDir;
  if (SwiftPCHHash.empty() || BridgingHeader.empty() || PCHOutputDir.empty()) {
    return std::nullopt;
  }

  SmallString<256> PCHBasename { llvm::sys::path::filename(BridgingHeader) };
  llvm::sys::path::replace_extension(PCHBasename, "");
  PCHBasename.append("-swift_");
  PCHBasename.append(SwiftPCHHash);
  PCHBasename.append("-clang_");
  PCHBasename.append(getClangModuleHash());
  PCHBasename.append(".pch");
  SmallString<256> PCHFilename { PCHOutputDir };
  llvm::sys::path::append(PCHFilename, PCHBasename);
  return PCHFilename.str().str();
}

std::optional<std::string>
ClangImporter::getOrCreatePCH(const ClangImporterOptions &ImporterOptions,
                              StringRef SwiftPCHHash, bool Cached) {
  bool isExplicit;
  auto PCHFilename = getPCHFilename(ImporterOptions, SwiftPCHHash,
                                    isExplicit);
  if (!PCHFilename.has_value()) {
    return std::nullopt;
  }
  if (!isExplicit && !ImporterOptions.PCHDisableValidation &&
      !canReadPCH(PCHFilename.value())) {
    StringRef parentDir = llvm::sys::path::parent_path(PCHFilename.value());
    std::error_code EC = llvm::sys::fs::create_directories(parentDir);
    if (EC) {
      llvm::errs() << "failed to create directory '" << parentDir << "': "
        << EC.message();
      return std::nullopt;
    }
    auto FailedToEmit = emitBridgingPCH(ImporterOptions.BridgingHeader,
                                        PCHFilename.value(), Cached);
    if (FailedToEmit) {
      return std::nullopt;
    }
  }

  return PCHFilename.value();
}

std::vector<std::string>
ClangImporter::getClangDriverArguments(ASTContext &ctx, bool ignoreClangTarget) {
  assert(!ctx.ClangImporterOpts.DirectClangCC1ModuleBuild &&
         "direct-clang-cc1-module-build should not call this function");
  std::vector<std::string> invocationArgStrs;
  // When creating from driver commands, clang expects this to be like an actual
  // command line. So we need to pass in "clang" for argv[0]
  invocationArgStrs.push_back(ctx.ClangImporterOpts.clangPath);
  switch (ctx.ClangImporterOpts.Mode) {
  case ClangImporterOptions::Modes::Normal:
  case ClangImporterOptions::Modes::PrecompiledModule:
    getNormalInvocationArguments(invocationArgStrs, ctx, ignoreClangTarget);
    break;
  case ClangImporterOptions::Modes::EmbedBitcode:
    getEmbedBitcodeInvocationArguments(invocationArgStrs, ctx);
    break;
  }
  addCommonInvocationArguments(invocationArgStrs, ctx, ignoreClangTarget);
  return invocationArgStrs;
}

std::optional<std::vector<std::string>> ClangImporter::getClangCC1Arguments(
    ClangImporter *importer, ASTContext &ctx,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> VFS,
    bool ignoreClangTarget) {
  std::unique_ptr<clang::CompilerInvocation> CI;

  // Set up a temporary diagnostic client to report errors from parsing the
  // command line, which may be important for Swift clients if, for example,
  // they're using -Xcc options. Unfortunately this diagnostic engine has to
  // use the default options because the /actual/ options haven't been parsed
  // yet.
  //
  // The long-term client for Clang diagnostics is set up afterwards, after the
  // clang::CompilerInstance is created.
  llvm::IntrusiveRefCntPtr<clang::DiagnosticOptions> tempDiagOpts{
      new clang::DiagnosticOptions};
  auto *tempDiagClient =
      new ClangDiagnosticConsumer(importer->Impl, *tempDiagOpts,
                                  ctx.ClangImporterOpts.DumpClangDiagnostics);
  auto clangDiags = clang::CompilerInstance::createDiagnostics(
      tempDiagOpts.get(), tempDiagClient,
      /*owned*/ true);

  // If using direct cc1 module build, use extra args to setup ClangImporter.
  if (ctx.ClangImporterOpts.DirectClangCC1ModuleBuild) {
    llvm::SmallVector<const char *> clangArgs;
    clangArgs.reserve(ctx.ClangImporterOpts.ExtraArgs.size());
    llvm::for_each(
        ctx.ClangImporterOpts.ExtraArgs,
        [&](const std::string &Arg) { clangArgs.push_back(Arg.c_str()); });

    // Try parse extra args, if failed, return nullopt.
    CI = std::make_unique<clang::CompilerInvocation>();
    if (!clang::CompilerInvocation::CreateFromArgs(*CI, clangArgs,
                                                   *clangDiags))
      return std::nullopt;

    // Forwards some options from swift to clang even using direct mode. This is
    // to reduce the number of argument passing on the command-line and swift
    // compiler can be more efficient to compute swift cache key without having
    // the knowledge about clang command-line options.
    if (ctx.CASOpts.EnableCaching) {
      CI->getCASOpts() = ctx.CASOpts.CASOpts;
      // When clangImporter is used to compile (generate .pcm or .pch), need to
      // inherit the include tree from swift args (last one wins) and clear the
      // input file.
      if ((CI->getFrontendOpts().ProgramAction ==
               clang::frontend::ActionKind::GenerateModule ||
           CI->getFrontendOpts().ProgramAction ==
               clang::frontend::ActionKind::GeneratePCH) &&
          ctx.ClangImporterOpts.HasClangIncludeTreeRoot) {
        CI->getFrontendOpts().CASIncludeTreeID =
            ctx.CASOpts.ClangIncludeTrees.back();
        CI->getFrontendOpts().Inputs.clear();
      }
    }

    // If clang target is ignored, using swift target.
    if (ignoreClangTarget)
      CI->getTargetOpts().Triple = ctx.LangOpts.Target.str();

    // Forward the index store path. That information is not passed to scanner
    // and it is cached invariant so we don't want to re-scan if that changed.
    CI->getFrontendOpts().IndexStorePath = ctx.ClangImporterOpts.IndexStorePath;
  } else {
    // Otherwise, create cc1 arguments from driver args.
    auto driverArgs = getClangDriverArguments(ctx, ignoreClangTarget);

    llvm::SmallVector<const char *> invocationArgs;
    invocationArgs.reserve(driverArgs.size());
    llvm::for_each(driverArgs, [&](const std::string &Arg) {
      invocationArgs.push_back(Arg.c_str());
    });

    if (ctx.ClangImporterOpts.DumpClangDiagnostics) {
      llvm::errs() << "clang importer driver args: '";
      llvm::interleave(
          invocationArgs, [](StringRef arg) { llvm::errs() << arg; },
          [] { llvm::errs() << "' '"; });
      llvm::errs() << "'\n\n";
    }

    clang::CreateInvocationOptions CIOpts;
    CIOpts.VFS = VFS;
    CIOpts.Diags = clangDiags;
    CIOpts.RecoverOnError = false;
    CIOpts.ProbePrecompiled = true;
    CI = clang::createInvocation(invocationArgs, std::move(CIOpts));
    if (!CI)
      return std::nullopt;
  }

  // FIXME: clang fails to generate a module if there is a `-fmodule-map-file`
  // argument pointing to a missing file.
  // Such missing module files occur frequently in SourceKit. If the files are
  // missing, SourceKit fails to build SwiftShims (which wouldn't have required
  // the missing module file), thus fails to load the stdlib and hence looses
  // all semantic functionality.
  // To work around this issue, drop all `-fmodule-map-file` arguments pointing
  // to missing files and report the error that clang would throw manually.
  // rdar://77516546 is tracking that the clang importer should be more
  // resilient and provide a module even if there were building it.
  auto TempVFS = clang::createVFSFromCompilerInvocation(
      *CI, *clangDiags,
      VFS ? VFS : importer->Impl.SwiftContext.SourceMgr.getFileSystem());

  std::vector<std::string> FilteredModuleMapFiles;
  for (auto ModuleMapFile : CI->getFrontendOpts().ModuleMapFiles) {
    if (ctx.ClangImporterOpts.HasClangIncludeTreeRoot) {
      // There is no need to add any module map file here. Issue a warning and
      // drop the option.
      importer->Impl.diagnose(SourceLoc(), diag::module_map_ignored,
                              ModuleMapFile);
    } else if (TempVFS->exists(ModuleMapFile)) {
      FilteredModuleMapFiles.push_back(ModuleMapFile);
    } else {
      importer->Impl.diagnose(SourceLoc(), diag::module_map_not_found,
                              ModuleMapFile);
    }
  }
  CI->getFrontendOpts().ModuleMapFiles = FilteredModuleMapFiles;

  return CI->getCC1CommandLine();
}

std::unique_ptr<clang::CompilerInvocation> ClangImporter::createClangInvocation(
    ClangImporter *importer, const ClangImporterOptions &importerOpts,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> VFS,
    const std::vector<std::string> &CC1Args) {
  std::vector<const char *> invocationArgs;
  invocationArgs.reserve(CC1Args.size());
  llvm::for_each(CC1Args, [&](const std::string &Arg) {
    invocationArgs.push_back(Arg.c_str());
  });

  // Create a diagnostics engine for creating clang compiler invocation. The
  // option here is either generated by dependency scanner or just round tripped
  // from `getClangCC1Arguments` so we don't expect it to fail. Use a simple
  // printing diagnostics consumer for debugging any unexpected error.
  auto diagOpts = llvm::makeIntrusiveRefCnt<clang::DiagnosticOptions>();
  clang::DiagnosticsEngine clangDiags(
      new clang::DiagnosticIDs(), diagOpts,
      new clang::TextDiagnosticPrinter(llvm::errs(), diagOpts.get()));

  // Finally, use the CC1 command-line and the diagnostic engine
  // to instantiate our Invocation.
  auto CI = std::make_unique<clang::CompilerInvocation>();
  if (!clang::CompilerInvocation::CreateFromArgs(
          *CI, invocationArgs, clangDiags, importerOpts.clangPath.c_str()))
    return nullptr;

  return CI;
}

std::unique_ptr<ClangImporter>
ClangImporter::create(ASTContext &ctx,
                      std::string swiftPCHHash, DependencyTracker *tracker,
                      DWARFImporterDelegate *dwarfImporterDelegate,
                      bool ignoreFileMapping) {
  std::unique_ptr<ClangImporter> importer{
      new ClangImporter(ctx, tracker, dwarfImporterDelegate)};
  auto &importerOpts = ctx.ClangImporterOpts;

  if (isPCHFilenameExtension(importerOpts.BridgingHeader)) {
    importer->Impl.setSinglePCHImport(importerOpts.BridgingHeader);
    importer->Impl.IsReadingBridgingPCH = true;
    if (tracker) {
      // Currently ignoring dependency on bridging .pch files because they are
      // temporaries; if and when they are no longer temporaries, this condition
      // should be removed.
      auto &coll = static_cast<ClangImporterDependencyCollector &>(
        *tracker->getClangCollector());
      coll.excludePath(importerOpts.BridgingHeader);
    }
  }

  llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> VFS =
      ctx.SourceMgr.getFileSystem();

  ClangInvocationFileMapping fileMapping =
    getClangInvocationFileMapping(ctx, nullptr, ignoreFileMapping);

  // Avoid creating indirect file system when using include tree.
  if (!ctx.ClangImporterOpts.HasClangIncludeTreeRoot) {
    // Wrap Swift's FS to allow Clang to override the working directory
    VFS = llvm::vfs::RedirectingFileSystem::create(
        fileMapping.redirectedFiles, true, *ctx.SourceMgr.getFileSystem());
    if (importerOpts.DumpClangDiagnostics) {
      llvm::errs() << "clang importer redirected file mappings:\n";
      for (const auto &mapping : fileMapping.redirectedFiles) {
        llvm::errs() << "   mapping real file '" << mapping.second
                     << "' to virtual file '" << mapping.first << "'\n";
      }
      llvm::errs() << "\n";
    }

    if (!fileMapping.overridenFiles.empty()) {
      llvm::IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> overridenVFS =
          new llvm::vfs::InMemoryFileSystem();
      for (const auto &file : fileMapping.overridenFiles) {
        if (importerOpts.DumpClangDiagnostics) {
          llvm::errs() << "clang importer overriding file '" << file.first
                       << "' with the following contents:\n";
          llvm::errs() << file.second << "\n";
        }
        auto contents = ctx.Allocate<char>(file.second.size() + 1);
        std::copy(file.second.begin(), file.second.end(), contents.begin());
        // null terminate the buffer.
        contents[contents.size() - 1] = '\0';
        overridenVFS->addFile(file.first, 0,
                              llvm::MemoryBuffer::getMemBuffer(StringRef(
                                  contents.begin(), contents.size() - 1)));
      }
      llvm::IntrusiveRefCntPtr<llvm::vfs::OverlayFileSystem> overlayVFS =
          new llvm::vfs::OverlayFileSystem(VFS);
      VFS = overlayVFS;
      overlayVFS->pushOverlay(overridenVFS);
    }
  }

  // Create a new Clang compiler invocation.
  {
    if (auto ClangArgs = getClangCC1Arguments(importer.get(), ctx, VFS))
      importer->Impl.ClangArgs = *ClangArgs;
    else
      return nullptr;

    if (fileMapping.requiresBuiltinHeadersInSystemModules)
      importer->Impl.ClangArgs.push_back("-fbuiltin-headers-in-system-modules");

    ArrayRef<std::string> invocationArgStrs = importer->Impl.ClangArgs;
    if (importerOpts.DumpClangDiagnostics) {
      llvm::errs() << "clang importer cc1 args: '";
      llvm::interleave(
                       invocationArgStrs, [](StringRef arg) { llvm::errs() << arg; },
                       [] { llvm::errs() << "' '"; });
      llvm::errs() << "'\n";
    }
    importer->Impl.Invocation = createClangInvocation(
        importer.get(), importerOpts, VFS, importer->Impl.ClangArgs);
    if (!importer->Impl.Invocation)
      return nullptr;
  }

  {
    // Create an almost-empty memory buffer.
    auto sourceBuffer = llvm::MemoryBuffer::getMemBuffer(
      "extern int __swift __attribute__((unavailable));",
      Implementation::moduleImportBufferName);
    clang::PreprocessorOptions &ppOpts =
        importer->Impl.Invocation->getPreprocessorOpts();
    ppOpts.addRemappedFile(Implementation::moduleImportBufferName,
                           sourceBuffer.release());
  }

  // Install a Clang module file extension to build Swift name lookup tables.
  importer->Impl.Invocation->getFrontendOpts().ModuleFileExtensions.push_back(
      std::make_shared<SwiftNameLookupExtension>(
          importer->Impl.BridgingHeaderLookupTable,
          importer->Impl.LookupTables, importer->Impl.SwiftContext,
          importer->Impl.getBufferImporterForDiagnostics(),
          importer->Impl.platformAvailability));

  // Create a compiler instance.
  {
    // The Clang modules produced by ClangImporter are always embedded in an
    // ObjectFilePCHContainer and contain -gmodules debug info.
    importer->Impl.Invocation->getCodeGenOpts().DebugTypeExtRefs = true;

    auto PCHContainerOperations =
      std::make_shared<clang::PCHContainerOperations>();
    PCHContainerOperations->registerWriter(
        std::make_unique<clang::ObjectFilePCHContainerWriter>());
    PCHContainerOperations->registerReader(
        std::make_unique<clang::ObjectFilePCHContainerReader>());
    importer->Impl.Instance.reset(
        new clang::CompilerInstance(std::move(PCHContainerOperations)));
  }
  auto &instance = *importer->Impl.Instance;
  instance.setInvocation(importer->Impl.Invocation);

  if (tracker)
    instance.addDependencyCollector(tracker->getClangCollector());

  {
    // Now set up the real client for Clang diagnostics---configured with proper
    // options---as opposed to the temporary one we made above.
    auto actualDiagClient = std::make_unique<ClangDiagnosticConsumer>(
        importer->Impl, instance.getDiagnosticOpts(),
        importerOpts.DumpClangDiagnostics);
    instance.createDiagnostics(actualDiagClient.release());
  }

  // Set up the file manager.
  {
    VFS = clang::createVFSFromCompilerInvocation(
        instance.getInvocation(), instance.getDiagnostics(), std::move(VFS));
    instance.createFileManager(VFS);
  }

  // Don't stop emitting messages if we ever can't load a module.
  // FIXME: This is actually a general problem: any "fatal" error could mess up
  // the CompilerInvocation when we're not in "show diagnostics after fatal
  // error" mode.
  clang::DiagnosticsEngine &clangDiags = instance.getDiagnostics();
  clangDiags.setSeverity(clang::diag::err_module_not_found,
                         clang::diag::Severity::Error,
                         clang::SourceLocation());
  clangDiags.setSeverity(clang::diag::err_module_not_built,
                         clang::diag::Severity::Error,
                         clang::SourceLocation());
  clangDiags.setFatalsAsError(ctx.Diags.getShowDiagnosticsAfterFatalError());

  // Use Clang to configure/save options for Swift IRGen/CodeGen
  if (ctx.LangOpts.ClangTarget.has_value()) {
    // If '-clang-target' is set, create a mock invocation with the Swift triple
    // to configure CodeGen and Target options for Swift compilation.
    auto swiftTargetClangArgs =
        getClangCC1Arguments(importer.get(), ctx, VFS, true);
    if (!swiftTargetClangArgs)
      return nullptr;
    auto swiftTargetClangInvocation = createClangInvocation(
        importer.get(), importerOpts, VFS, *swiftTargetClangArgs);
    if (!swiftTargetClangInvocation)
      return nullptr;
    importer->Impl.setSwiftTargetInfo(clang::TargetInfo::CreateTargetInfo(
        clangDiags, swiftTargetClangInvocation->TargetOpts));
    importer->Impl.setSwiftCodeGenOptions(new clang::CodeGenOptions(
        swiftTargetClangInvocation->getCodeGenOpts()));
  } else {
    // Just use the existing Invocation's directly
    importer->Impl.setSwiftTargetInfo(clang::TargetInfo::CreateTargetInfo(
        clangDiags, importer->Impl.Invocation->TargetOpts));
    importer->Impl.setSwiftCodeGenOptions(
        new clang::CodeGenOptions(importer->Impl.Invocation->getCodeGenOpts()));
  }

  // Create the associated action.
  importer->Impl.Action.reset(new ParsingAction(ctx, *importer,
                                                importer->Impl,
                                                importerOpts,
                                                swiftPCHHash));
  auto *action = importer->Impl.Action.get();

  // Execute the action. We effectively inline most of
  // CompilerInstance::ExecuteAction here, because we need to leave the AST
  // open for future module loading.
  // FIXME: This has to be cleaned up on the Clang side before we can improve
  // things here.

  // Create the target instance.
  instance.setTarget(
    clang::TargetInfo::CreateTargetInfo(clangDiags,
                                        instance.getInvocation().TargetOpts));
  if (!instance.hasTarget())
    return nullptr;

  // Inform the target of the language options.
  //
  // FIXME: We shouldn't need to do this, the target should be immutable once
  // created. This complexity should be lifted elsewhere.
  instance.getTarget().adjust(clangDiags, instance.getLangOpts());

  if (importerOpts.Mode == ClangImporterOptions::Modes::EmbedBitcode)
    return importer;

  // ClangImporter always sets this in Normal mode, so we need to make sure to
  // set it before bailing out early when configuring ClangImporter for
  // precompiled modules. This is not a benign langopt, so forgetting this (for
  // example, if we combined the early exit below with the one above) would make
  // the compiler instance used to emit PCMs incompatible with the one used to
  // read them later.
  instance.getLangOpts().NeededByPCHOrCompilationUsesPCH = true;

  // Clang implicitly enables this by default in C++20 mode.
  instance.getLangOpts().ModulesLocalVisibility = false;

  if (importerOpts.Mode == ClangImporterOptions::Modes::PrecompiledModule)
    return importer;

  instance.initializeDelayedInputFileFromCAS();
  if (instance.getFrontendOpts().Inputs.empty())
    return nullptr; // no inputs available.

  bool canBegin = action->BeginSourceFile(instance,
                                          instance.getFrontendOpts().Inputs[0]);
  if (!canBegin)
    return nullptr; // there was an error related to the compiler arguments.

  clang::Preprocessor &clangPP = instance.getPreprocessor();
  clangPP.enableIncrementalProcessing();

  // Setup Preprocessor callbacks before initialing the parser to make sure
  // we catch implicit includes.
  auto ppTracker = std::make_unique<BridgingPPTracker>(importer->Impl);
  clangPP.addPPCallbacks(std::move(ppTracker));

  instance.createASTReader();

  // Manually run the action, so that the TU stays open for additional parsing.
  instance.createSema(action->getTranslationUnitKind(), nullptr);
  importer->Impl.Parser.reset(new clang::Parser(clangPP, instance.getSema(),
                                                /*SkipFunctionBodies=*/false));

  clangPP.EnterMainSourceFile();
  importer->Impl.Parser->Initialize();

  importer->Impl.nameImporter.reset(new NameImporter(
      importer->Impl.SwiftContext, importer->Impl.platformAvailability,
      importer->Impl.getClangSema()));

  // FIXME: These decls are not being parsed correctly since (a) some of the
  // callbacks are still being added, and (b) the logic to parse them has
  // changed.
  clang::Parser::DeclGroupPtrTy parsed;
  clang::Sema::ModuleImportState importState =
      clang::Sema::ModuleImportState::NotACXX20Module;
  while (!importer->Impl.Parser->ParseTopLevelDecl(parsed, importState)) {
    for (auto *D : parsed.get()) {
      importer->Impl.addBridgeHeaderTopLevelDecls(D);

      if (auto named = dyn_cast<clang::NamedDecl>(D)) {
        addEntryToLookupTable(*importer->Impl.BridgingHeaderLookupTable, named,
                              *importer->Impl.nameImporter);
      }
    }
  }

  // FIXME: This is missing implicit includes.
  auto *CB = new HeaderImportCallbacks(importer->Impl);
  clangPP.addPPCallbacks(std::unique_ptr<clang::PPCallbacks>(CB));

  // Create the selectors we'll be looking for.
  auto &clangContext = importer->Impl.Instance->getASTContext();
  importer->Impl.objectAtIndexedSubscript
    = clangContext.Selectors.getUnarySelector(
        &clangContext.Idents.get("objectAtIndexedSubscript"));
  const clang::IdentifierInfo *setObjectAtIndexedSubscriptIdents[2] = {
      &clangContext.Idents.get("setObject"),
      &clangContext.Idents.get("atIndexedSubscript"),
  };
  importer->Impl.setObjectAtIndexedSubscript
    = clangContext.Selectors.getSelector(2, setObjectAtIndexedSubscriptIdents);
  importer->Impl.objectForKeyedSubscript
    = clangContext.Selectors.getUnarySelector(
        &clangContext.Idents.get("objectForKeyedSubscript"));
  const clang::IdentifierInfo *setObjectForKeyedSubscriptIdents[2] = {
      &clangContext.Idents.get("setObject"),
      &clangContext.Idents.get("forKeyedSubscript"),
  };
  importer->Impl.setObjectForKeyedSubscript
    = clangContext.Selectors.getSelector(2, setObjectForKeyedSubscriptIdents);

  // Set up the imported header module.
  auto *importedHeaderModule =
      ModuleDecl::create(ctx.getIdentifier(CLANG_HEADER_MODULE_NAME), ctx);
  importer->Impl.ImportedHeaderUnit =
    new (ctx) ClangModuleUnit(*importedHeaderModule, importer->Impl, nullptr);
  importedHeaderModule->addFile(*importer->Impl.ImportedHeaderUnit);
  importedHeaderModule->setHasResolvedImports();
  importedHeaderModule->setIsNonSwiftModule(true);

  importer->Impl.IsReadingBridgingPCH = false;

  return importer;
}

bool ClangImporter::addSearchPath(StringRef newSearchPath, bool isFramework,
                                  bool isSystem) {
  clang::FileManager &fileMgr = Impl.Instance->getFileManager();
  auto optionalEntry = fileMgr.getOptionalDirectoryRef(newSearchPath);
  if (!optionalEntry)
    return true;
  auto entry = *optionalEntry;

  auto &headerSearchInfo = Impl.getClangPreprocessor().getHeaderSearchInfo();
  auto exists = std::any_of(headerSearchInfo.search_dir_begin(),
                            headerSearchInfo.search_dir_end(),
                            [&](const clang::DirectoryLookup &lookup) -> bool {
    if (isFramework)
      return lookup.getFrameworkDir() == &entry.getDirEntry();
    return lookup.getDir() == &entry.getDirEntry();
  });
  if (exists) {
    // Don't bother adding a search path that's already there. Clang would have
    // removed it via deduplication at the time the search path info gets built.
    return false;
  }

  auto kind = isSystem ? clang::SrcMgr::C_System : clang::SrcMgr::C_User;
  headerSearchInfo.AddSearchPath({entry, kind, isFramework},
                                 /*isAngled=*/true);

  // In addition to changing the current preprocessor directly, we still need
  // to change the options structure for future module-building.
  Impl.Instance->getHeaderSearchOpts().AddPath(newSearchPath,
                   isSystem ? clang::frontend::System : clang::frontend::Angled,
                                               isFramework,
                                               /*IgnoreSysRoot=*/true);
  return false;
}

clang::SourceLocation
ClangImporter::Implementation::getNextIncludeLoc() {
  clang::SourceManager &srcMgr = getClangInstance()->getSourceManager();

  if (!DummyIncludeBuffer.isValid()) {
    clang::SourceLocation includeLoc =
        srcMgr.getLocForStartOfFile(srcMgr.getMainFileID());
    // Picking the beginning of the main FileID as include location is also what
    // the clang PCH mechanism is doing (see
    // clang::ASTReader::getImportLocation()). Choose the next source location
    // here to avoid having the exact same import location as the clang PCH.
    // Otherwise, if we are using a PCH for bridging header, we'll have
    // problems with source order comparisons of clang source locations not
    // being deterministic.
    includeLoc = includeLoc.getLocWithOffset(1);
    DummyIncludeBuffer = srcMgr.createFileID(
        std::make_unique<ZeroFilledMemoryBuffer>(
          256*1024, StringRef(moduleImportBufferName)),
        clang::SrcMgr::C_User, /*LoadedID*/0, /*LoadedOffset*/0, includeLoc);
  }

  clang::SourceLocation clangImportLoc =
      srcMgr.getLocForStartOfFile(DummyIncludeBuffer)
            .getLocWithOffset(IncludeCounter++);
  assert(srcMgr.isInFileID(clangImportLoc, DummyIncludeBuffer) &&
         "confused Clang's source manager with our fake locations");
  return clangImportLoc;
}

bool ClangImporter::Implementation::importHeader(
    ModuleDecl *adapter, StringRef headerName, SourceLoc diagLoc,
    bool trackParsedSymbols,
    std::unique_ptr<llvm::MemoryBuffer> sourceBuffer,
    bool implicitImport) {

  // Progress update for the debugger.
  SwiftContext.PreModuleImportHook(
      headerName, ASTContext::ModuleImportKind::BridgingHeader);

  // Don't even try to load the bridging header if the Clang AST is in a bad
  // state. It could cause a crash.
  auto &clangDiags = getClangASTContext().getDiagnostics();
  if (clangDiags.hasUnrecoverableErrorOccurred() &&
      !getClangInstance()->getPreprocessorOpts().AllowPCHWithCompilerErrors)
    return true;

  assert(adapter);
  ImportedHeaderOwners.push_back(adapter);

  bool hadError = clangDiags.hasErrorOccurred();

  clang::SourceManager &sourceMgr = getClangInstance()->getSourceManager();
  clang::FileID bufferID = sourceMgr.createFileID(std::move(sourceBuffer),
                                                  clang::SrcMgr::C_User,
                                                  /*LoadedID=*/0,
                                                  /*LoadedOffset=*/0,
                                                  getNextIncludeLoc());
  auto &consumer =
      static_cast<HeaderParsingASTConsumer &>(Instance->getASTConsumer());
  consumer.reset();

  clang::Preprocessor &pp = getClangPreprocessor();
  pp.EnterSourceFile(bufferID, /*Dir=*/nullptr, /*Loc=*/{});
  // Force the import to occur.
  pp.LookAhead(0);

  SmallVector<clang::DeclGroupRef, 16> allParsedDecls;
  auto handleParsed = [&](clang::DeclGroupRef parsed) {
    if (trackParsedSymbols) {
      for (auto *D : parsed) {
        addBridgeHeaderTopLevelDecls(D);
      }
    }

    allParsedDecls.push_back(parsed);
  };

  clang::Parser::DeclGroupPtrTy parsed;
  clang::Sema::ModuleImportState importState =
      clang::Sema::ModuleImportState::NotACXX20Module;
  while (!Parser->ParseTopLevelDecl(parsed, importState)) {
    if (parsed)
      handleParsed(parsed.get());
    for (auto additionalParsedGroup : consumer.getAdditionalParsedDecls())
      handleParsed(additionalParsedGroup);
    consumer.reset();
  }

  // We're trying to discourage (and eventually deprecate) the use of implicit
  // bridging-header imports triggered by IMPORTED_HEADER blocks in
  // modules. There are two sub-cases to consider:
  //
  //   #1 The implicit import actually occurred.
  //
  //   #2 The user explicitly -import-objc-header'ed some header or PCH that
  //      makes the implicit import redundant.
  //
  // It's not obvious how to exactly differentiate these cases given the
  // interface clang gives us, but we only want to warn on case #1, and the
  // non-emptiness of allParsedDecls is a _definite_ sign that we're in case
  // #1. So we treat that as an approximation of the condition we're after, and
  // accept that we might fail to warn in the odd case where "the import
  // occurred" but didn't introduce any new decls.
  //
  // We also want to limit (for now) the warning in case #1 to invocations that
  // requested an explicit bridging header, because otherwise the warning will
  // complain in a very common scenario (unit test w/o bridging header imports
  // application w/ bridging header) that we don't yet have Xcode automation
  // to correct. The fix would be explicitly importing on the command line.
  if (implicitImport && !allParsedDecls.empty() &&
    BridgingHeaderExplicitlyRequested) {
    diagnose(
      diagLoc, diag::implicit_bridging_header_imported_from_module,
      llvm::sys::path::filename(headerName), adapter->getName());
  }

  // We can't do this as we're parsing because we may want to resolve naming
  // conflicts between the things we've parsed.

  std::function<void(clang::Decl *)> visit = [&](clang::Decl *decl) {
    // Iterate into extern "C" {} type declarations.
    if (auto linkageDecl = dyn_cast<clang::LinkageSpecDecl>(decl)) {
      for (auto *decl : linkageDecl->noload_decls()) {
        visit(decl);
      }
    }
    if (auto named = dyn_cast<clang::NamedDecl>(decl)) {
      addEntryToLookupTable(*BridgingHeaderLookupTable, named,
                              getNameImporter());
    }
  };
  for (auto group : allParsedDecls) {
    for (auto *D : group) {
      visit(D);
    }
  }

  pp.EndSourceFile();
  bumpGeneration();

  // Add any defined macros to the bridging header lookup table.
  addMacrosToLookupTable(*BridgingHeaderLookupTable, getNameImporter());

  // Finish loading any extra modules that were (transitively) imported.
  handleDeferredImports(diagLoc);

  // Wrap all Clang imports under a Swift import decl.
  for (auto &Import : BridgeHeaderTopLevelImports) {
    if (auto *ClangImport = Import.dyn_cast<clang::ImportDecl*>()) {
      Import = createImportDecl(SwiftContext, adapter, ClangImport, {});
    }
  }

  // Finalize the lookup table, which may fail.
  finalizeLookupTable(*BridgingHeaderLookupTable, getNameImporter(),
                      getBufferImporterForDiagnostics());

  // FIXME: What do we do if there was already an error?
  if (!hadError && clangDiags.hasErrorOccurred() &&
      !getClangInstance()->getPreprocessorOpts().AllowPCHWithCompilerErrors) {
    diagnose(diagLoc, diag::bridging_header_error, headerName);
    return true;
  }

  return false;
}

bool ClangImporter::importHeader(StringRef header, ModuleDecl *adapter,
                                 off_t expectedSize, time_t expectedModTime,
                                 StringRef cachedContents, SourceLoc diagLoc) {
  clang::FileManager &fileManager = Impl.Instance->getFileManager();
  auto headerFile = fileManager.getFile(header, /*OpenFile=*/true);
  // Prefer importing the header directly if the header content matches by
  // checking size and mod time. This allows correct import if some no-modular
  // headers are already imported into clang importer. If mod time is zero, then
  // the module should be built from CAS and there is no mod time to verify.
  if (headerFile && (*headerFile)->getSize() == expectedSize &&
      (expectedModTime == 0 ||
       (*headerFile)->getModificationTime() == expectedModTime)) {
    return importBridgingHeader(header, adapter, diagLoc, false, true);
  }

  // If we've made it to here, this is some header other than the bridging
  // header, which means we can no longer rely on one file's modification time
  // to invalidate code completion caches. :-(
  Impl.setSinglePCHImport(std::nullopt);

  if (!cachedContents.empty() && cachedContents.back() == '\0')
    cachedContents = cachedContents.drop_back();
  std::unique_ptr<llvm::MemoryBuffer> sourceBuffer{
    llvm::MemoryBuffer::getMemBuffer(cachedContents, header)
  };
  return Impl.importHeader(adapter, header, diagLoc, /*trackParsedSymbols=*/false,
                           std::move(sourceBuffer), true);
}

bool ClangImporter::importBridgingHeader(StringRef header, ModuleDecl *adapter,
                                         SourceLoc diagLoc,
                                         bool trackParsedSymbols,
                                         bool implicitImport) {
  if (isPCHFilenameExtension(header)) {
    Impl.ImportedHeaderOwners.push_back(adapter);
    // We already imported this with -include-pch above, so we should have
    // collected a bunch of PCH-encoded module imports that we just need to
    // replay in handleDeferredImports.
    Impl.handleDeferredImports(diagLoc);
    return false;
  }

  clang::FileManager &fileManager = Impl.Instance->getFileManager();
  auto headerFile = fileManager.getFile(header, /*OpenFile=*/true);
  if (!headerFile) {
    Impl.diagnose(diagLoc, diag::bridging_header_missing, header);
    return true;
  }

  llvm::SmallString<128> importLine;
  if (Impl.SwiftContext.LangOpts.EnableObjCInterop)
    importLine = "#import \"";
  else
    importLine = "#include \"";

  importLine += header;
  importLine += "\"\n";

  std::unique_ptr<llvm::MemoryBuffer> sourceBuffer{
    llvm::MemoryBuffer::getMemBufferCopy(
      importLine, Implementation::bridgingHeaderBufferName)
  };
  return Impl.importHeader(adapter, header, diagLoc, trackParsedSymbols,
                           std::move(sourceBuffer), implicitImport);
}

static llvm::Expected<llvm::cas::ObjectRef>
setupIncludeTreeInput(clang::CompilerInvocation &invocation,
                      StringRef headerPath, StringRef pchIncludeTree) {
  auto DB = invocation.getCASOpts().getOrCreateDatabases();
  if (!DB)
    return DB.takeError();
  auto CAS = DB->first;
  auto ID = CAS->parseID(pchIncludeTree);
  if (!ID)
    return ID.takeError();
  auto includeTreeRef = CAS->getReference(*ID);
  if (!includeTreeRef)
    return llvm::cas::ObjectStore::createUnknownObjectError(*ID);

  invocation.getFrontendOpts().Inputs.push_back(clang::FrontendInputFile(
      *includeTreeRef, headerPath, clang::Language::ObjC));

  return *includeTreeRef;
}

std::string ClangImporter::getBridgingHeaderContents(
    StringRef headerPath, off_t &fileSize, time_t &fileModTime,
    StringRef pchIncludeTree) {
  auto invocation =
      std::make_shared<clang::CompilerInvocation>(*Impl.Invocation);

  invocation->getFrontendOpts().DisableFree = false;
  invocation->getFrontendOpts().Inputs.clear();

  std::optional<llvm::cas::ObjectRef> includeTreeRef;
  if (pchIncludeTree.empty())
    invocation->getFrontendOpts().Inputs.push_back(
        clang::FrontendInputFile(headerPath, clang::Language::ObjC));
  else if (auto err =
               setupIncludeTreeInput(*invocation, headerPath, pchIncludeTree)
                   .moveInto(includeTreeRef)) {
    Impl.diagnose({}, diag::err_rewrite_bridging_header,
                  toString(std::move(err)));
    return "";
  }

  invocation->getPreprocessorOpts().resetNonModularOptions();

  clang::CompilerInstance rewriteInstance(
    Impl.Instance->getPCHContainerOperations(),
    &Impl.Instance->getModuleCache());
  rewriteInstance.setInvocation(invocation);
  rewriteInstance.createDiagnostics(new clang::IgnoringDiagConsumer);

  clang::FileManager &fileManager = Impl.Instance->getFileManager();
  rewriteInstance.setFileManager(&fileManager);
  rewriteInstance.createSourceManager(fileManager);
  rewriteInstance.setTarget(&Impl.Instance->getTarget());

  std::string result;
  bool success = llvm::CrashRecoveryContext().RunSafelyOnThread([&] {
    // A much simpler version of clang::RewriteIncludesAction that lets us
    // write to an in-memory buffer.
    class RewriteIncludesAction : public clang::PreprocessorFrontendAction {
      raw_ostream &OS;
      std::optional<llvm::cas::ObjectRef> includeTreeRef;

      void ExecuteAction() override {
        clang::CompilerInstance &compiler = getCompilerInstance();
        // If the input is include tree, setup the IncludeTreePPAction.
        if (includeTreeRef) {
          auto IncludeTreeRoot = clang::cas::IncludeTreeRoot::get(
              compiler.getOrCreateObjectStore(), *includeTreeRef);
          if (!IncludeTreeRoot)
            llvm::report_fatal_error(IncludeTreeRoot.takeError());
          auto PPCachedAct =
              clang::createPPActionsFromIncludeTree(*IncludeTreeRoot);
          if (!PPCachedAct)
            llvm::report_fatal_error(PPCachedAct.takeError());
          compiler.getPreprocessor().setPPCachedActions(
              std::move(*PPCachedAct));
        }

        clang::RewriteIncludesInInput(compiler.getPreprocessor(), &OS,
                                      compiler.getPreprocessorOutputOpts());
      }

    public:
      explicit RewriteIncludesAction(
          raw_ostream &os, std::optional<llvm::cas::ObjectRef> includeTree)
          : OS(os), includeTreeRef(includeTree) {}
    };

    llvm::raw_string_ostream os(result);
    RewriteIncludesAction action(os, includeTreeRef);
    rewriteInstance.ExecuteAction(action);
  });

  success |= !rewriteInstance.getDiagnostics().hasErrorOccurred();
  if (!success) {
    Impl.diagnose({}, diag::could_not_rewrite_bridging_header);
    return "";
  }

  if (auto fileInfo = fileManager.getFile(headerPath)) {
    fileSize = (*fileInfo)->getSize();
    fileModTime = (*fileInfo)->getModificationTime();
  }
  return result;
}

/// Returns the appropriate source input language based on language options.
static clang::Language getLanguageFromOptions(
    const clang::LangOptions &LangOpts) {
  if (LangOpts.OpenCL)
    return clang::Language::OpenCL;
  if (LangOpts.CUDA)
    return clang::Language::CUDA;
  if (LangOpts.ObjC)
    return LangOpts.CPlusPlus ?
        clang::Language::ObjCXX : clang::Language::ObjC;
  return LangOpts.CPlusPlus ? clang::Language::CXX : clang::Language::C;
}

/// Wraps the given frontend action in an index data recording action if the
/// frontend options have an index store path specified.
static
std::unique_ptr<clang::FrontendAction> wrapActionForIndexingIfEnabled(
    const clang::FrontendOptions &FrontendOpts,
    std::unique_ptr<clang::FrontendAction> action) {
  if (!FrontendOpts.IndexStorePath.empty()) {
    return clang::index::createIndexDataRecordingAction(
        FrontendOpts, std::move(action));
  }
  return action;
}

std::unique_ptr<clang::CompilerInstance>
ClangImporter::cloneCompilerInstanceForPrecompiling() {
  auto invocation =
      std::make_shared<clang::CompilerInvocation>(*Impl.Invocation);

  auto &PPOpts = invocation->getPreprocessorOpts();
  PPOpts.resetNonModularOptions();

  auto &FrontendOpts = invocation->getFrontendOpts();
  FrontendOpts.DisableFree = false;
  if (FrontendOpts.CASIncludeTreeID.empty())
    FrontendOpts.Inputs.clear();

  auto clonedInstance = std::make_unique<clang::CompilerInstance>(
    Impl.Instance->getPCHContainerOperations(),
    &Impl.Instance->getModuleCache());
  clonedInstance->setInvocation(std::move(invocation));
  clonedInstance->createDiagnostics(&Impl.Instance->getDiagnosticClient(),
                                    /*ShouldOwnClient=*/false);

  clang::FileManager &fileManager = Impl.Instance->getFileManager();
  clonedInstance->setFileManager(&fileManager);
  clonedInstance->createSourceManager(fileManager);
  clonedInstance->setTarget(&Impl.Instance->getTarget());
  clonedInstance->setOutputBackend(Impl.SwiftContext.OutputBackend);

  return clonedInstance;
}

bool ClangImporter::emitBridgingPCH(
    StringRef headerPath, StringRef outputPCHPath, bool cached) {
  auto emitInstance = cloneCompilerInstanceForPrecompiling();
  auto &invocation = emitInstance->getInvocation();

  auto &LangOpts = invocation.getLangOpts();
  LangOpts.NeededByPCHOrCompilationUsesPCH = true;
  LangOpts.CacheGeneratedPCH = cached;

  auto language = getLanguageFromOptions(LangOpts);
  auto inputFile = clang::FrontendInputFile(headerPath, language);

  auto &FrontendOpts = invocation.getFrontendOpts();
  if (invocation.getFrontendOpts().CASIncludeTreeID.empty())
    FrontendOpts.Inputs = {inputFile};
  FrontendOpts.OutputFile = outputPCHPath.str();
  FrontendOpts.ProgramAction = clang::frontend::GeneratePCH;

  auto action = wrapActionForIndexingIfEnabled(
      FrontendOpts, std::make_unique<clang::GeneratePCHAction>());
  emitInstance->ExecuteAction(*action);

  if (emitInstance->getDiagnostics().hasErrorOccurred() &&
      !emitInstance->getPreprocessorOpts().AllowPCHWithCompilerErrors) {
    Impl.diagnose({}, diag::bridging_header_pch_error,
                  outputPCHPath, headerPath);
    return true;
  }
  return false;
}

bool ClangImporter::runPreprocessor(
    StringRef inputPath, StringRef outputPath) {
  auto emitInstance = cloneCompilerInstanceForPrecompiling();
  auto &invocation = emitInstance->getInvocation();
  auto &LangOpts = invocation.getLangOpts();
  auto &OutputOpts = invocation.getPreprocessorOutputOpts();
  OutputOpts.ShowCPP = 1;
  OutputOpts.ShowComments = 0;
  OutputOpts.ShowLineMarkers = 0;
  OutputOpts.ShowMacros = 0;
  OutputOpts.ShowMacroComments = 0;
  auto language = getLanguageFromOptions(LangOpts);
  auto inputFile = clang::FrontendInputFile(inputPath, language);

  auto &FrontendOpts = invocation.getFrontendOpts();
  if (invocation.getFrontendOpts().CASIncludeTreeID.empty())
    FrontendOpts.Inputs = {inputFile};
  FrontendOpts.OutputFile = outputPath.str();
  FrontendOpts.ProgramAction = clang::frontend::PrintPreprocessedInput;

  auto action = wrapActionForIndexingIfEnabled(
      FrontendOpts, std::make_unique<clang::PrintPreprocessedAction>());
  emitInstance->ExecuteAction(*action);
  return emitInstance->getDiagnostics().hasErrorOccurred();
}

bool ClangImporter::emitPrecompiledModule(
    StringRef moduleMapPath, StringRef moduleName, StringRef outputPath) {
  auto emitInstance = cloneCompilerInstanceForPrecompiling();
  auto &invocation = emitInstance->getInvocation();

  auto &LangOpts = invocation.getLangOpts();
  LangOpts.setCompilingModule(clang::LangOptions::CMK_ModuleMap);
  LangOpts.ModuleName = moduleName.str();
  LangOpts.CurrentModule = LangOpts.ModuleName;

  auto language = getLanguageFromOptions(LangOpts);

  auto &FrontendOpts = invocation.getFrontendOpts();
  if (invocation.getFrontendOpts().CASIncludeTreeID.empty()) {
    auto inputFile = clang::FrontendInputFile(
        moduleMapPath,
        clang::InputKind(language, clang::InputKind::ModuleMap, false),
        FrontendOpts.IsSystemModule);
    FrontendOpts.Inputs = {inputFile};
  }
  FrontendOpts.OriginalModuleMap = moduleMapPath.str();
  FrontendOpts.OutputFile = outputPath.str();
  FrontendOpts.ProgramAction = clang::frontend::GenerateModule;

  auto action = wrapActionForIndexingIfEnabled(
      FrontendOpts,
      std::make_unique<clang::GenerateModuleFromModuleMapAction>());
  emitInstance->ExecuteAction(*action);

  if (emitInstance->getDiagnostics().hasErrorOccurred() &&
      !FrontendOpts.AllowPCMWithCompilerErrors) {
    Impl.diagnose({}, diag::emit_pcm_error, outputPath, moduleMapPath);
    return true;
  }
  return false;
}

bool ClangImporter::dumpPrecompiledModule(
    StringRef modulePath, StringRef outputPath) {
  auto dumpInstance = cloneCompilerInstanceForPrecompiling();
  auto &invocation = dumpInstance->getInvocation();

  auto inputFile = clang::FrontendInputFile(
      modulePath, clang::InputKind(
          clang::Language::Unknown, clang::InputKind::Precompiled, false));

  auto &FrontendOpts = invocation.getFrontendOpts();
  if (invocation.getFrontendOpts().CASIncludeTreeID.empty())
    FrontendOpts.Inputs = {inputFile};
  FrontendOpts.OutputFile = outputPath.str();

  auto action = std::make_unique<clang::DumpModuleInfoAction>();
  dumpInstance->ExecuteAction(*action);

  if (dumpInstance->getDiagnostics().hasErrorOccurred()) {
    Impl.diagnose({}, diag::dump_pcm_error, modulePath);
    return true;
  }
  return false;
}

void ClangImporter::collectVisibleTopLevelModuleNames(
    SmallVectorImpl<Identifier> &names) const {
  SmallVector<clang::Module *, 32> Modules;
  Impl.getClangPreprocessor().getHeaderSearchInfo().collectAllModules(Modules);
  for (auto &M : Modules) {
    if (!M->isAvailable())
      continue;

    names.push_back(
        Impl.SwiftContext.getIdentifier(M->getTopLevelModuleName()));
  }
}

void ClangImporter::collectSubModuleNames(
    ImportPath::Module path,
    std::vector<std::string> &names) const {
  auto &clangHeaderSearch = Impl.getClangPreprocessor().getHeaderSearchInfo();

  // Look up the top-level module first.
  clang::Module *clangModule = clangHeaderSearch.lookupModule(
      path.front().Item.str(), /*ImportLoc=*/clang::SourceLocation(),
      /*AllowSearch=*/true, /*AllowExtraModuleMapSearch=*/true);
  if (!clangModule)
    return;
  clang::Module *submodule = clangModule;
  for (auto component : path.getSubmodulePath()) {
    submodule = submodule->findSubmodule(component.Item.str());
    if (!submodule)
      return;
  }
  for (auto sub : submodule->submodules())
    names.push_back(sub->Name);
}

bool ClangImporter::isModuleImported(const clang::Module *M) {
  return M->NameVisibility == clang::Module::NameVisibilityKind::AllVisible;
}

static llvm::VersionTuple getCurrentVersionFromTBD(llvm::vfs::FileSystem &FS,
                                                   StringRef path,
                                                   StringRef moduleName) {
  std::string fwName = (moduleName + ".framework").str();
  auto pos = path.find(fwName);
  if (pos == StringRef::npos)
    return {};
  llvm::SmallString<256> buffer(path.substr(0, pos + fwName.size()));
  llvm::sys::path::append(buffer, moduleName + ".tbd");
  auto tbdPath = buffer.str();
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> tbdBufOrErr =
      FS.getBufferForFile(tbdPath);
  // .tbd file doesn't exist, exit.
  if (!tbdBufOrErr)
    return {};
  auto tbdFileOrErr =
      llvm::MachO::TextAPIReader::get(tbdBufOrErr.get()->getMemBufferRef());
  if (auto err = tbdFileOrErr.takeError()) {
    consumeError(std::move(err));
    return {};
  }
  auto tbdCV = (*tbdFileOrErr)->getCurrentVersion();
  return llvm::VersionTuple(tbdCV.getMajor(), tbdCV.getMinor(),
                            tbdCV.getSubminor());
}

bool ClangImporter::canImportModule(ImportPath::Module modulePath,
                                    SourceLoc loc,
                                    ModuleVersionInfo *versionInfo,
                                    bool isTestableDependencyLookup) {
  // Look up the top-level module to see if it exists.
  auto topModule = modulePath.front();
  clang::Module *clangModule = Impl.lookupModule(topModule.Item.str());
  if (!clangModule) {
    return false;
  }

  clang::Module::Requirement r;
  clang::Module::UnresolvedHeaderDirective mh;
  clang::Module *m;
  auto &ctx = Impl.getClangASTContext();
  auto &lo = ctx.getLangOpts();
  auto &ti = getModuleAvailabilityTarget();

  auto available = clangModule->isAvailable(lo, ti, r, mh, m);
  if (!available)
    return false;

  if (modulePath.hasSubmodule()) {
    for (auto &component : modulePath.getSubmodulePath()) {
      clangModule = clangModule->findSubmodule(component.Item.str());

      // Special case: a submodule named "Foo.Private" can be moved to a
      // top-level module named "Foo_Private". Clang has special support for
      // this.
      if (!clangModule && component.Item.str() == "Private" &&
          (&component) == (&modulePath.getRaw()[1])) {
        clangModule =
            Impl.lookupModule((topModule.Item.str() + "_Private").str());
      }
      if (!clangModule || !clangModule->isAvailable(lo, ti, r, mh, m)) {
        return false;
      }
    }
  }

  if (!versionInfo)
    return true;

  assert(available);
  StringRef path = getClangASTContext().getSourceManager()
    .getFilename(clangModule->DefinitionLoc);

  // Look for the .tbd file inside .framework dir to get the project version
  // number.
  llvm::VersionTuple currentVersion = getCurrentVersionFromTBD(
      Impl.Instance->getVirtualFileSystem(), path, topModule.Item.str());
  versionInfo->setVersion(currentVersion,
                          ModuleVersionSourceKind::ClangModuleTBD);
  return true;
}

clang::Module *
ClangImporter::Implementation::lookupModule(StringRef moduleName) {
  auto &clangHeaderSearch = getClangPreprocessor().getHeaderSearchInfo();
  if (getClangASTContext().getLangOpts().ImplicitModules)
    return clangHeaderSearch.lookupModule(
        moduleName, /*ImportLoc=*/clang::SourceLocation(),
        /*AllowSearch=*/true, /*AllowExtraModuleMapSearch=*/true);

  // Explicit module. Try load from modulemap.
  auto &PP = Instance->getPreprocessor();
  auto &MM = PP.getHeaderSearchInfo().getModuleMap();
  auto loadFromMM = [&]() -> clang::Module * {
    auto *II = PP.getIdentifierInfo(moduleName);
    if (auto clangModule = MM.getCachedModuleLoad(*II))
      return *clangModule;
    return nullptr;
  };
  // Check if it is already loaded.
  if (auto *clangModule = loadFromMM())
    return clangModule;

  // If not, try load it.
  auto &PrebuiltModules = Instance->getHeaderSearchOpts().PrebuiltModuleFiles;
  auto moduleFile = PrebuiltModules.find(moduleName);
  if (moduleFile == PrebuiltModules.end())
    return nullptr;

  clang::serialization::ModuleFile *Loaded = nullptr;
  if (!Instance->loadModuleFile(moduleFile->second, Loaded))
    return nullptr; // error loading, return not found.
  return loadFromMM();
}

ModuleDecl *ClangImporter::Implementation::loadModuleClang(
    SourceLoc importLoc, ImportPath::Module path) {
  auto &clangHeaderSearch = getClangPreprocessor().getHeaderSearchInfo();
  auto realModuleName = SwiftContext.getRealModuleName(path.front().Item).str();

  // For explicit module build, module should always exist but module map might
  // not be exist. Go straight to module loader.
  if (Instance->getInvocation().getLangOpts().ImplicitModules) {
    // Look up the top-level module first, to see if it exists at all.
    clang::Module *clangModule = clangHeaderSearch.lookupModule(
        realModuleName, /*ImportLoc=*/clang::SourceLocation(),
        /*AllowSearch=*/true, /*AllowExtraModuleMapSearch=*/true);
    if (!clangModule)
      return nullptr;
  }

  // Convert the Swift import path over to a Clang import path.
  SmallVector<std::pair<clang::IdentifierInfo *, clang::SourceLocation>, 4>
      clangPath;
  bool isTopModuleComponent = true;
  for (auto component : path) {
    StringRef item = isTopModuleComponent? realModuleName:
                                           component.Item.str();
    isTopModuleComponent = false;

    clangPath.emplace_back(
        getClangPreprocessor().getIdentifierInfo(item),
        exportSourceLoc(component.Loc));
  }

  auto &diagEngine = Instance->getDiagnostics();
  auto &rawDiagClient = *diagEngine.getClient();
  auto &diagClient = static_cast<ClangDiagnosticConsumer &>(rawDiagClient);

  auto loadModule = [&](clang::ModuleIdPath path,
                        clang::Module::NameVisibilityKind visibility)
      -> clang::ModuleLoadResult {
    auto importRAII =
        diagClient.handleImport(clangPath.front().first, diagEngine,
                                importLoc);

    std::string preservedIndexStorePathOption;
    auto &clangFEOpts = Instance->getFrontendOpts();
    if (!clangFEOpts.IndexStorePath.empty()) {
      StringRef moduleName = path[0].first->getName();
      // Ignore the SwiftShims module for the index data.
      if (moduleName == SwiftContext.SwiftShimsModuleName.str()) {
        preservedIndexStorePathOption = clangFEOpts.IndexStorePath;
        clangFEOpts.IndexStorePath.clear();
      }
    }

    clang::SourceLocation clangImportLoc = getNextIncludeLoc();
    clang::ModuleLoadResult result =
        Instance->loadModule(clangImportLoc, path, visibility,
                             /*IsInclusionDirective=*/false);

    if (!preservedIndexStorePathOption.empty()) {
      // Restore the -index-store-path option.
      clangFEOpts.IndexStorePath = preservedIndexStorePathOption;
    }

    if (result && (visibility == clang::Module::AllVisible)) {
      getClangPreprocessor().makeModuleVisible(result, clangImportLoc);
    }
    return result;
  };

  // Now load the top-level module, so that we can check if the submodule
  // exists without triggering a fatal error.
  auto clangModule = loadModule(clangPath.front(), clang::Module::AllVisible);
  if (!clangModule)
    return nullptr;

  // If we're asked to import the top-level module then we're done here.
  auto *topSwiftModule = finishLoadingClangModule(clangModule, importLoc);
  if (path.size() == 1) {
    return topSwiftModule;
  }

  // Verify that the submodule exists.
  clang::Module *submodule = clangModule;
  for (auto &component : path.getSubmodulePath()) {
    submodule = submodule->findSubmodule(component.Item.str());

    // Special case: a submodule named "Foo.Private" can be moved to a top-level
    // module named "Foo_Private". Clang has special support for this.
    // We're limiting this to just submodules named "Private" because this will
    // put the Clang AST in a fatal error state if it /doesn't/ exist.
    if (!submodule && component.Item.str() == "Private" &&
        (&component) == (&path.getRaw()[1])) {
      submodule = loadModule(llvm::ArrayRef(clangPath).slice(0, 2),
                             clang::Module::Hidden);
    }

    if (!submodule) {
      // FIXME: Specialize the error for a missing submodule?
      return nullptr;
    }
  }

  // Finally, load the submodule and make it visible.
  clangModule = loadModule(clangPath, clang::Module::AllVisible);
  if (!clangModule)
    return nullptr;

  return finishLoadingClangModule(clangModule, importLoc);
}

ModuleDecl *
ClangImporter::loadModule(SourceLoc importLoc,
                          ImportPath::Module path,
                          bool AllowMemoryCache) {
  return Impl.loadModule(importLoc, path);
}

ModuleDecl *ClangImporter::Implementation::loadModule(
    SourceLoc importLoc, ImportPath::Module path) {
  ModuleDecl *MD = nullptr;
  ASTContext &ctx = getNameImporter().getContext();

  // `CxxStdlib` is the only accepted spelling of the C++ stdlib module name.
  if (path.front().Item.is("std") ||
      path.front().Item.str().starts_with("std_"))
    return nullptr;
  if (path.front().Item == ctx.Id_CxxStdlib) {
    ImportPath::Builder adjustedPath(ctx.getIdentifier("std"), importLoc);
    adjustedPath.append(path.getSubmodulePath());
    path = adjustedPath.copyTo(ctx).getModulePath(ImportKind::Module);
  }

  if (!DisableSourceImport)
    MD = loadModuleClang(importLoc, path);
  if (!MD)
    MD = loadModuleDWARF(importLoc, path);
  return MD;
}

ModuleDecl *ClangImporter::Implementation::finishLoadingClangModule(
    const clang::Module *clangModule, SourceLoc importLoc) {
  assert(clangModule);

  // Bump the generation count.
  bumpGeneration();

  // Force load overlays for all imported modules.
  // FIXME: This forces the creation of wrapper modules for all imports as
  // well, and may do unnecessary work.
  ClangModuleUnit *wrapperUnit = getWrapperForModule(clangModule, importLoc);
  ModuleDecl *result = wrapperUnit->getParentModule();
  if (!ModuleWrappers[clangModule].getInt()) {
    ModuleWrappers[clangModule].setInt(true);
    (void) namelookup::getAllImports(result);
  }

  // Register '.h' inputs of each Clang module dependency with
  // the dependency tracker. In implicit builds such dependencies are registered
  // during the on-demand construction of Clang module. In Explicit Module
  // Builds, since we load pre-built PCMs directly, we do not get to do so. So
  // instead, manually register all `.h` inputs of Clang module dependnecies.
  if (SwiftDependencyTracker &&
      !Instance->getInvocation().getLangOpts().ImplicitModules) {
    if (auto moduleRef = clangModule->getASTFile()) {
      auto *moduleFile = Instance->getASTReader()->getModuleManager().lookup(
          *moduleRef);
      Instance->getASTReader()->visitInputFileInfos(
          *moduleFile, /*IncludeSystem=*/true,
          [&](const clang::serialization::InputFileInfo &IFI, bool isSystem) {
            SwiftDependencyTracker->addDependency(IFI.Filename, isSystem);
          });
    }
  }

  if (clangModule->isSubModule()) {
    finishLoadingClangModule(clangModule->getTopLevelModule(), importLoc);
  } else {

    if (!SwiftContext.getLoadedModule(result->getName()))
      SwiftContext.addLoadedModule(result);
  }

  return result;
}

// Run through the set of deferred imports -- either those referenced by
// submodule ID from a bridging PCH, or those already loaded as clang::Modules
// in response to an import directive in a bridging header -- and call
// finishLoadingClangModule on each.
void ClangImporter::Implementation::handleDeferredImports(SourceLoc diagLoc) {
  clang::ASTReader &R = *Instance->getASTReader();
  llvm::SmallSet<clang::serialization::SubmoduleID, 32> seenSubmodules;
  for (clang::serialization::SubmoduleID ID : PCHImportedSubmodules) {
    if (!seenSubmodules.insert(ID).second)
      continue;
    ImportedHeaderExports.push_back(R.getSubmodule(ID));
  }
  PCHImportedSubmodules.clear();

  // Avoid a for-in loop because in unusual situations we can end up pulling in
  // another bridging header while we finish loading the modules that are
  // already here. This is a brittle situation but it's outside what's
  // officially supported with bridging headers: app targets and unit tests
  // only. Unfortunately that's not enforced.
  for (size_t i = 0; i < ImportedHeaderExports.size(); ++i) {
    (void)finishLoadingClangModule(ImportedHeaderExports[i], diagLoc);
  }
}

ModuleDecl *ClangImporter::getImportedHeaderModule() const {
  return Impl.ImportedHeaderUnit->getParentModule();
}

ModuleDecl *
ClangImporter::getWrapperForModule(const clang::Module *mod,
                                   bool returnOverlayIfPossible) const {
  auto clangUnit = Impl.getWrapperForModule(mod);
  if (returnOverlayIfPossible && clangUnit->getOverlayModule())
    return clangUnit->getOverlayModule();
  return clangUnit->getParentModule();
}

PlatformAvailability::PlatformAvailability(const LangOptions &langOpts)
    : platformKind(targetPlatform(langOpts)) {
  switch (platformKind) {
  case PlatformKind::iOS:
  case PlatformKind::iOSApplicationExtension:
  case PlatformKind::macCatalyst:
  case PlatformKind::macCatalystApplicationExtension:
  case PlatformKind::tvOS:
  case PlatformKind::tvOSApplicationExtension:
    deprecatedAsUnavailableMessage =
        "APIs deprecated as of iOS 7 and earlier are unavailable in Swift";
    asyncDeprecatedAsUnavailableMessage =
      "APIs deprecated as of iOS 12 and earlier are not imported as 'async'";
    break;

  case PlatformKind::watchOS:
  case PlatformKind::watchOSApplicationExtension:
    deprecatedAsUnavailableMessage = "";
    asyncDeprecatedAsUnavailableMessage =
      "APIs deprecated as of watchOS 5 and earlier are not imported as "
      "'async'";
    break;

  case PlatformKind::macOS:
  case PlatformKind::macOSApplicationExtension:
    deprecatedAsUnavailableMessage =
        "APIs deprecated as of macOS 10.9 and earlier are unavailable in Swift";
    asyncDeprecatedAsUnavailableMessage =
      "APIs deprecated as of macOS 10.14 and earlier are not imported as "
      "'async'";
    break;

  case PlatformKind::visionOS:
  case PlatformKind::visionOSApplicationExtension:
    break;

  case PlatformKind::OpenBSD:
    deprecatedAsUnavailableMessage = "";
    break;

  case PlatformKind::Windows:
    deprecatedAsUnavailableMessage = "";
    break;

  case PlatformKind::none:
    break;
  }
}

bool PlatformAvailability::isPlatformRelevant(StringRef name) const {
  switch (platformKind) {
  case PlatformKind::macOS:
    return name == "macos";
  case PlatformKind::macOSApplicationExtension:
    return name == "macos" || name == "macos_app_extension";

  case PlatformKind::iOS:
    return name == "ios";
  case PlatformKind::iOSApplicationExtension:
    return name == "ios" || name == "ios_app_extension";

  case PlatformKind::macCatalyst:
    return name == "ios" || name == "maccatalyst";
  case PlatformKind::macCatalystApplicationExtension:
    return name == "ios" || name == "ios_app_extension" ||
           name == "maccatalyst" || name == "maccatalyst_app_extension";

  case PlatformKind::tvOS:
    return name == "tvos";
  case PlatformKind::tvOSApplicationExtension:
    return name == "tvos" || name == "tvos_app_extension";

  case PlatformKind::watchOS:
    return name == "watchos";
  case PlatformKind::watchOSApplicationExtension:
    return name == "watchos" || name == "watchos_app_extension";

  case PlatformKind::visionOS:
    return name == "xros" || name == "visionos";
  case PlatformKind::visionOSApplicationExtension:
    return name == "xros" || name == "xros_app_extension" ||
           name == "visionos" || name == "visionos_app_extension";

  case PlatformKind::OpenBSD:
    return name == "openbsd";

  case PlatformKind::Windows:
    return name == "windows";

  case PlatformKind::none:
    return false;
  }

  llvm_unreachable("Unexpected platform");
}

bool PlatformAvailability::treatDeprecatedAsUnavailable(
    const clang::Decl *clangDecl, const llvm::VersionTuple &version,
    bool isAsync) const {
  assert(!version.empty() && "Must provide version when deprecated");
  unsigned major = version.getMajor();
  std::optional<unsigned> minor = version.getMinor();

  switch (platformKind) {
  case PlatformKind::none:
    llvm_unreachable("version but no platform?");

  case PlatformKind::macOS:
  case PlatformKind::macOSApplicationExtension:
    // Anything deprecated by macOS 10.14 is unavailable for async import
    // in Swift.
    if (isAsync && !clangDecl->hasAttr<clang::SwiftAsyncAttr>()) {
      return major < 10 ||
          (major == 10 && (!minor.has_value() || minor.value() <= 14));
    }

    // Anything deprecated in OSX 10.9.x and earlier is unavailable in Swift.
    return major < 10 ||
           (major == 10 && (!minor.has_value() || minor.value() <= 9));

  case PlatformKind::iOS:
  case PlatformKind::iOSApplicationExtension:
  case PlatformKind::tvOS:
  case PlatformKind::tvOSApplicationExtension:
    // Anything deprecated by iOS 12 is unavailable for async import
    // in Swift.
    if (isAsync && !clangDecl->hasAttr<clang::SwiftAsyncAttr>()) {
      return major <= 12;
    }

    // Anything deprecated in iOS 7.x and earlier is unavailable in Swift.
    return major <= 7;

  case PlatformKind::macCatalyst:
  case PlatformKind::macCatalystApplicationExtension:
    // ClangImporter does not yet support macCatalyst.
    return false;

  case PlatformKind::watchOS:
  case PlatformKind::watchOSApplicationExtension:
    // Anything deprecated by watchOS 5.0 is unavailable for async import
    // in Swift.
    if (isAsync && !clangDecl->hasAttr<clang::SwiftAsyncAttr>()) {
      return major <= 5;
    }

    // No deprecation filter on watchOS
    return false;

  case PlatformKind::visionOS:
  case PlatformKind::visionOSApplicationExtension:
    // No deprecation filter on xrOS
    return false;

  case PlatformKind::OpenBSD:
    // No deprecation filter on OpenBSD
    return false;

  case PlatformKind::Windows:
    // No deprecation filter on Windows
    return false;
  }

  llvm_unreachable("Unexpected platform");
}

ClangImporter::Implementation::Implementation(
    ASTContext &ctx, DependencyTracker *dependencyTracker,
    DWARFImporterDelegate *dwarfImporterDelegate)
    : SwiftContext(ctx), ImportForwardDeclarations(
                             ctx.ClangImporterOpts.ImportForwardDeclarations),
      DisableSwiftBridgeAttr(ctx.ClangImporterOpts.DisableSwiftBridgeAttr),
      BridgingHeaderExplicitlyRequested(
          !ctx.ClangImporterOpts.BridgingHeader.empty()),
      DisableOverlayModules(ctx.ClangImporterOpts.DisableOverlayModules),
      EnableClangSPI(ctx.ClangImporterOpts.EnableClangSPI),
      UseClangIncludeTree(ctx.ClangImporterOpts.UseClangIncludeTree),
      importSymbolicCXXDecls(
          ctx.LangOpts.hasFeature(Feature::ImportSymbolicCXXDecls)),
      IsReadingBridgingPCH(false),
      CurrentVersion(ImportNameVersion::fromOptions(ctx.LangOpts)),
      Walker(DiagnosticWalker(*this)), BuffersForDiagnostics(ctx.SourceMgr),
      BridgingHeaderLookupTable(new SwiftLookupTable(nullptr)),
      platformAvailability(ctx.LangOpts), nameImporter(),
      DisableSourceImport(ctx.ClangImporterOpts.DisableSourceImport),
      SwiftDependencyTracker(dependencyTracker),
      DWARFImporter(dwarfImporterDelegate) {}

ClangImporter::Implementation::~Implementation() {
#ifndef NDEBUG
  SwiftContext.SourceMgr.verifyAllBuffers();
#endif
}

ClangImporter::Implementation::DiagnosticWalker::DiagnosticWalker(
    ClangImporter::Implementation &Impl)
    : Impl(Impl) {}

bool ClangImporter::Implementation::DiagnosticWalker::TraverseDecl(
    clang::Decl *D) {
  if (!D)
    return true;
  // In some cases, diagnostic notes about types (ex: built-in types) do not
  // have an obvious source location at which to display diagnostics. We
  // provide the location of the closest decl as a reasonable choice.
  llvm::SaveAndRestore<clang::SourceLocation> sar{TypeReferenceSourceLocation,
                                                  D->getBeginLoc()};
  return clang::RecursiveASTVisitor<DiagnosticWalker>::TraverseDecl(D);
}

bool ClangImporter::Implementation::DiagnosticWalker::TraverseParmVarDecl(
    clang::ParmVarDecl *D) {
  // When the ClangImporter imports functions / methods, the return
  // type is first imported, followed by parameter types in order of
  // declaration. If any type fails to import, the import of the function /
  // method is aborted. This means any parameters after the first to fail to
  // import (the first could be the return type) will not have diagnostics
  // attached. Even though these remaining parameters may have unimportable
  // types, we avoid diagnosing these types as a type diagnosis without a
  // "parameter not imported" note on the referencing param decl is inconsistent
  // behaviour and could be confusing.
  if (Impl.ImportDiagnostics[D].size()) {
    // Since the parameter decl in question has been diagnosed (we didn't bail
    // before importing this param) continue the traversal as normal.
    return clang::RecursiveASTVisitor<DiagnosticWalker>::TraverseParmVarDecl(D);
  }

  // If the decl in question has not been diagnosed, traverse "as normal" except
  // avoid traversing to the referenced typed. Note the traversal has been
  // simplified greatly and may need to be modified to support some future
  // diagnostics.
  if (!getDerived().shouldTraversePostOrder())
    if (!WalkUpFromParmVarDecl(D))
      return false;

  if (clang::DeclContext *declContext = dyn_cast<clang::DeclContext>(D)) {
    for (auto *Child : declContext->decls()) {
      if (!canIgnoreChildDeclWhileTraversingDeclContext(Child))
        if (!TraverseDecl(Child))
          return false;
    }
  }
  if (getDerived().shouldTraversePostOrder())
    if (!WalkUpFromParmVarDecl(D))
      return false;
  return true;
}

bool ClangImporter::Implementation::DiagnosticWalker::VisitDecl(
    clang::Decl *D) {
  Impl.emitDiagnosticsForTarget(D);
  return true;
}

bool ClangImporter::Implementation::DiagnosticWalker::VisitMacro(
    const clang::MacroInfo *MI) {
  Impl.emitDiagnosticsForTarget(MI);
  for (const clang::Token &token : MI->tokens()) {
    Impl.emitDiagnosticsForTarget(&token);
  }
  return true;
}

bool ClangImporter::Implementation::DiagnosticWalker::
    VisitObjCObjectPointerType(clang::ObjCObjectPointerType *T) {
  // If an ObjCInterface is pointed to, diagnose it.
  if (const clang::ObjCInterfaceDecl *decl = T->getInterfaceDecl()) {
    Impl.emitDiagnosticsForTarget(decl);
  }
  // Diagnose any protocols the pointed to type conforms to.
  for (auto cp = T->qual_begin(), cpEnd = T->qual_end(); cp != cpEnd; ++cp) {
    Impl.emitDiagnosticsForTarget(*cp);
  }
  return true;
}

bool ClangImporter::Implementation::DiagnosticWalker::VisitType(
    clang::Type *T) {
  if (TypeReferenceSourceLocation.isValid())
    Impl.emitDiagnosticsForTarget(T, TypeReferenceSourceLocation);
  return true;
}

ClangModuleUnit *ClangImporter::Implementation::getWrapperForModule(
    const clang::Module *underlying, SourceLoc diagLoc) {
  auto &cacheEntry = ModuleWrappers[underlying];
  if (ClangModuleUnit *cached = cacheEntry.getPointer())
    return cached;

  // FIXME: Handle hierarchical names better.
  Identifier name = underlying->Name == "std"
                        ? SwiftContext.Id_CxxStdlib
                        : SwiftContext.getIdentifier(underlying->Name);
  auto wrapper = ModuleDecl::create(name, SwiftContext);
  wrapper->setIsSystemModule(underlying->IsSystem);
  wrapper->setIsNonSwiftModule();
  wrapper->setHasResolvedImports();
  if (!underlying->ExportAsModule.empty())
    wrapper->setExportAsName(
        SwiftContext.getIdentifier(underlying->ExportAsModule));

  auto file = new (SwiftContext) ClangModuleUnit(*wrapper, *this,
                                                 underlying);
  wrapper->addFile(*file);
  SwiftContext.getClangModuleLoader()->findOverlayFiles(diagLoc, wrapper, file);
  cacheEntry.setPointer(file);

  return file;
}

ClangModuleUnit *ClangImporter::Implementation::getClangModuleForDecl(
    const clang::Decl *D,
    bool allowForwardDeclaration) {
  auto maybeModule = getClangSubmoduleForDecl(D, allowForwardDeclaration);
  if (!maybeModule)
    return nullptr;
  if (!maybeModule.value())
    return ImportedHeaderUnit;

  // Get the parent module because currently we don't represent submodules with
  // ClangModuleUnit.
  auto *M = maybeModule.value()->getTopLevelModule();

  return getWrapperForModule(M);
}

void ClangImporter::Implementation::addImportDiagnostic(
    ImportDiagnosticTarget target, Diagnostic &&diag,
    clang::SourceLocation loc) {
  ImportDiagnostic importDiag = ImportDiagnostic(target, diag, loc);
  if (SwiftContext.LangOpts.DisableExperimentalClangImporterDiagnostics ||
      CollectedDiagnostics.count(importDiag))
    return;

  CollectedDiagnostics.insert(importDiag);
  ImportDiagnostics[target].push_back(importDiag);
}

#pragma mark Source locations
clang::SourceLocation
ClangImporter::Implementation::exportSourceLoc(SourceLoc loc) {
  // FIXME: Implement!
  return clang::SourceLocation();
}

SourceLoc
ClangImporter::Implementation::importSourceLoc(clang::SourceLocation loc) {
  return BuffersForDiagnostics.resolveSourceLocation(Instance->getSourceManager(), loc);
}

SourceRange
ClangImporter::Implementation::importSourceRange(clang::SourceRange range) {
  return SourceRange(importSourceLoc(range.getBegin()), importSourceLoc(range.getEnd()));
}

#pragma mark Importing names

clang::DeclarationName
ClangImporter::Implementation::exportName(Identifier name) {
  // FIXME: When we start dealing with C++, we can map over some operator
  // names.
  if (name.empty() || name.isOperator())
    return clang::DeclarationName();

  // Map the identifier. If it's some kind of keyword, it can't be mapped.
  auto ident = &Instance->getASTContext().Idents.get(name.str());
  if (ident->getTokenID() != clang::tok::identifier)
    return clang::DeclarationName();

  return ident;
}

Identifier
ClangImporter::Implementation::importIdentifier(
  const clang::IdentifierInfo *identifier,
  StringRef removePrefix)
{
  if (!identifier) return Identifier();

  StringRef name = identifier->getName();
  // Remove the prefix, if any.
  if (!removePrefix.empty()) {
    if (name.starts_with(removePrefix)) {
      name = name.slice(removePrefix.size(), name.size());
    }
  }

  // Get the Swift identifier.
  return SwiftContext.getIdentifier(name);
}

ObjCSelector ClangImporter::Implementation::importSelector(
               clang::Selector selector) {
  auto &ctx = SwiftContext;

  // Handle zero-argument selectors directly.
  if (selector.isUnarySelector()) {
    Identifier name;
    if (auto id = selector.getIdentifierInfoForSlot(0))
      name = ctx.getIdentifier(id->getName());
    return ObjCSelector(ctx, 0, name);
  }

  SmallVector<Identifier, 2> pieces;
  for (auto i = 0u, n = selector.getNumArgs(); i != n; ++i) {
    Identifier piece;
    if (auto id = selector.getIdentifierInfoForSlot(i))
      piece = ctx.getIdentifier(id->getName());
    pieces.push_back(piece);
  }

  return ObjCSelector(ctx, pieces.size(), pieces);
}

clang::Selector
ClangImporter::Implementation::exportSelector(DeclName name,
                                              bool allowSimpleName) {
  if (!allowSimpleName && name.isSimpleName())
    return {};

  clang::ASTContext &ctx = getClangASTContext();

  SmallVector<const clang::IdentifierInfo *, 8> pieces;
  pieces.push_back(exportName(name.getBaseIdentifier()).getAsIdentifierInfo());

  auto argNames = name.getArgumentNames();
  if (argNames.empty())
    return ctx.Selectors.getNullarySelector(pieces.front());

  if (!argNames.front().empty())
    return {};
  argNames = argNames.slice(1);

  for (Identifier argName : argNames)
    pieces.push_back(exportName(argName).getAsIdentifierInfo());

  return ctx.Selectors.getSelector(pieces.size(), pieces.data());
}

clang::Selector
ClangImporter::Implementation::exportSelector(ObjCSelector selector) {
  SmallVector<const clang::IdentifierInfo *, 4> pieces;
  for (auto piece : selector.getSelectorPieces())
    pieces.push_back(exportName(piece).getAsIdentifierInfo());
  return getClangASTContext().Selectors.getSelector(selector.getNumArgs(),
                                                    pieces.data());
}

/// Determine whether the given method potentially conflicts with the
/// setter for a property in the given protocol.
static bool
isPotentiallyConflictingSetter(const clang::ObjCProtocolDecl *proto,
                               const clang::ObjCMethodDecl *method) {
  auto sel = method->getSelector();
  if (sel.getNumArgs() != 1)
    return false;

  const clang::IdentifierInfo *setterID = sel.getIdentifierInfoForSlot(0);
  if (!setterID || !setterID->getName().starts_with("set"))
    return false;

  for (auto *prop : proto->properties()) {
    if (prop->getSetterName() == sel)
      return true;
  }

  return false;
}

bool importer::shouldSuppressDeclImport(const clang::Decl *decl) {
  if (auto objcMethod = dyn_cast<clang::ObjCMethodDecl>(decl)) {
    // First check if we're actually in a Swift class.
    auto dc = decl->getDeclContext();
    if (hasNativeSwiftDecl(cast<clang::ObjCContainerDecl>(dc)))
      return true;

    // If this member is a method that is a getter or setter for a
    // property, don't add it into the table. property names and
    // getter names (by choosing to only have a property).
    //
    // Note that this is suppressed for certain accessibility declarations,
    // which are imported as getter/setter pairs and not properties.
    if (objcMethod->isPropertyAccessor()) {
      // Suppress the import of this method when the corresponding
      // property is not suppressed.
      return !shouldSuppressDeclImport(
               objcMethod->findPropertyDecl(/*CheckOverrides=*/false));
    }

    // If the method was declared within a protocol, check that it
    // does not conflict with the setter of a property.
    if (auto proto = dyn_cast<clang::ObjCProtocolDecl>(dc))
      return isPotentiallyConflictingSetter(proto, objcMethod);


    return false;
  }

  if (auto objcProperty = dyn_cast<clang::ObjCPropertyDecl>(decl)) {
    // First check if we're actually in a Swift class.
    auto dc = objcProperty->getDeclContext();
    if (hasNativeSwiftDecl(cast<clang::ObjCContainerDecl>(dc)))
      return true;

    // Suppress certain properties; import them as getter/setter pairs instead.
    if (shouldImportPropertyAsAccessors(objcProperty))
      return true;

    // Check whether there is a superclass method for the getter that
    // is *not* suppressed, in which case we will need to suppress
    // this property.
    auto objcClass = dyn_cast<clang::ObjCInterfaceDecl>(dc);
    if (!objcClass) {
      if (auto objcCategory = dyn_cast<clang::ObjCCategoryDecl>(dc)) {
        // If the enclosing category is invalid, suppress this declaration.
        if (objcCategory->isInvalidDecl()) return true;

        objcClass = objcCategory->getClassInterface();
      }
    }

    if (objcClass) {
      if (auto objcSuperclass = objcClass->getSuperClass()) {
        auto getterMethod =
            objcSuperclass->lookupMethod(objcProperty->getGetterName(),
                                         objcProperty->isInstanceProperty());
        if (getterMethod && !shouldSuppressDeclImport(getterMethod))
          return true;
      }
    }

    return false;
  }

  if (isa<clang::BuiltinTemplateDecl>(decl)) {
    return true;
  }

  return false;
}

#pragma mark Name lookup
const clang::TypedefNameDecl *
ClangImporter::Implementation::lookupTypedef(clang::DeclarationName name) {
  clang::Sema &sema = Instance->getSema();
  clang::LookupResult lookupResult(sema, name,
                                   clang::SourceLocation(),
                                   clang::Sema::LookupOrdinaryName);

  if (sema.LookupName(lookupResult, sema.TUScope)) {
    for (auto decl : lookupResult) {
      if (auto typedefDecl =
          dyn_cast<clang::TypedefNameDecl>(decl->getUnderlyingDecl()))
        return typedefDecl;
    }
  }

  return nullptr;
}

static bool isDeclaredInModule(const ClangModuleUnit *ModuleFilter,
                               const Decl *VD) {
  // Sometimes imported decls get put into the clang header module. If we
  // found one of these decls, don't filter it out.
  if (VD->getModuleContext()->getName().str() == CLANG_HEADER_MODULE_NAME) {
    return true;
  }
  auto ContainingUnit = VD->getDeclContext()->getModuleScopeContext();
  return ModuleFilter == ContainingUnit;
}

static const clang::Module *
getClangOwningModule(ClangNode Node, const clang::ASTContext &ClangCtx) {
  assert(!Node.getAsModule() && "not implemented for modules");

  if (const clang::Decl *D = Node.getAsDecl()) {
    auto ExtSource = ClangCtx.getExternalSource();
    assert(ExtSource);

    auto originalDecl = D;
    if (auto functionDecl = dyn_cast<clang::FunctionDecl>(D)) {
      if (auto pattern = functionDecl->getTemplateInstantiationPattern()) {
        // Function template instantiations don't have an owning Clang module.
        // Let's use the owning module of the template pattern.
        originalDecl = pattern;
      }
    }
    if (!originalDecl->hasOwningModule()) {
      if (auto cxxRecordDecl = dyn_cast<clang::CXXRecordDecl>(D)) {
        if (auto pattern = cxxRecordDecl->getTemplateInstantiationPattern()) {
          // Class template instantiations sometimes don't have an owning Clang
          // module, if the instantiation is not typedef-ed.
          originalDecl = pattern;
        }
      }
    }

    return ExtSource->getModule(originalDecl->getOwningModuleID());
  }

  if (const clang::ModuleMacro *M = Node.getAsModuleMacro())
    return M->getOwningModule();

  // A locally-defined MacroInfo does not have an owning module.
  assert(Node.getAsMacroInfo());
  return nullptr;
}

static const clang::Module *
getClangTopLevelOwningModule(ClangNode Node,
                             const clang::ASTContext &ClangCtx) {
  const clang::Module *OwningModule = getClangOwningModule(Node, ClangCtx);
  if (!OwningModule)
    return nullptr;
  return OwningModule->getTopLevelModule();
}

static bool isVisibleFromModule(const ClangModuleUnit *ModuleFilter,
                                ValueDecl *VD) {
  assert(ModuleFilter);

  auto ContainingUnit = VD->getDeclContext()->getModuleScopeContext();
  if (ModuleFilter == ContainingUnit)
    return true;

  // The rest of this function is looking to see if the Clang entity that
  // caused VD to be imported has redeclarations in the filter module.
  auto Wrapper = dyn_cast<ClangModuleUnit>(ContainingUnit);
  if (!Wrapper)
    return false;

  ASTContext &Ctx = ContainingUnit->getASTContext();
  auto *Importer = static_cast<ClangImporter *>(Ctx.getClangModuleLoader());
  auto ClangNode = Importer->getEffectiveClangNode(VD);

  // Macros can be "redeclared" by putting an equivalent definition in two
  // different modules. (We don't actually check the equivalence.)
  // FIXME: We're also not checking if the redeclaration is in /this/ module.
  if (ClangNode.getAsMacro())
    return true;

  const clang::Decl *D = ClangNode.castAsDecl();
  auto &ClangASTContext = ModuleFilter->getClangASTContext();
  // We don't handle Clang submodules; pop everything up to the top-level
  // module.
  auto OwningClangModule = getClangTopLevelOwningModule(ClangNode,
                                                        ClangASTContext);
  if (OwningClangModule == ModuleFilter->getClangModule())
    return true;

  // If this decl was implicitly synthesized by the compiler, and is not
  // supposed to be owned by any module, return true.
  if (Importer->isSynthesizedAndVisibleFromAllModules(D)) {
    return true;
  }

  // Friends from class templates don't have an owning module. Just return true.
  if (isa<clang::FunctionDecl>(D) &&
      cast<clang::FunctionDecl>(D)->isThisDeclarationInstantiatedFromAFriendDefinition())
    return true;

  // Handle redeclarable Clang decls by checking each redeclaration.
  bool IsTagDecl = isa<clang::TagDecl>(D);
  if (!(IsTagDecl || isa<clang::FunctionDecl>(D) || isa<clang::VarDecl>(D) ||
        isa<clang::TypedefNameDecl>(D) || isa<clang::NamespaceDecl>(D))) {
    return false;
  }

  for (auto Redeclaration : D->redecls()) {
    if (Redeclaration == D)
      continue;

    // For enums, structs, and unions, only count definitions when looking to
    // see what other modules they appear in.
    if (IsTagDecl) {
      auto TD = cast<clang::TagDecl>(Redeclaration);
      if (!TD->isCompleteDefinition() &&
          !TD->isThisDeclarationADemotedDefinition())
        continue;
    }

    auto OwningClangModule = getClangTopLevelOwningModule(Redeclaration,
                                                          ClangASTContext);
    if (OwningClangModule == ModuleFilter->getClangModule())
      return true;
  }

  return false;
}


namespace {
class ClangVectorDeclConsumer : public clang::VisibleDeclConsumer {
  std::vector<clang::NamedDecl *> results;
public:
  ClangVectorDeclConsumer() = default;

  void FoundDecl(clang::NamedDecl *ND, clang::NamedDecl *Hiding,
                 clang::DeclContext *Ctx, bool InBaseClass) override {
    if (!ND->getIdentifier())
      return;

    if (ND->isModulePrivate())
      return;

    results.push_back(ND);
  }

  llvm::MutableArrayRef<clang::NamedDecl *> getResults() {
    return results;
  }
};

class FilteringVisibleDeclConsumer : public swift::VisibleDeclConsumer {
  swift::VisibleDeclConsumer &NextConsumer;
  const ClangModuleUnit *ModuleFilter;

public:
  FilteringVisibleDeclConsumer(swift::VisibleDeclConsumer &consumer,
                               const ClangModuleUnit *CMU)
      : NextConsumer(consumer), ModuleFilter(CMU) {
    assert(CMU);
  }

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason,
                 DynamicLookupInfo dynamicLookupInfo) override {
    if (!VD->hasClangNode() || isVisibleFromModule(ModuleFilter, VD))
      NextConsumer.foundDecl(VD, Reason, dynamicLookupInfo);
  }
};

class FilteringDeclaredDeclConsumer : public swift::VisibleDeclConsumer {
  swift::VisibleDeclConsumer &NextConsumer;
  const ClangModuleUnit *ModuleFilter;

public:
  FilteringDeclaredDeclConsumer(swift::VisibleDeclConsumer &consumer,
                                const ClangModuleUnit *CMU)
      : NextConsumer(consumer), ModuleFilter(CMU) {
    assert(CMU && CMU->isTopLevel() && "Only top-level modules supported");
  }

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason,
                 DynamicLookupInfo dynamicLookupInfo) override {
    if (isDeclaredInModule(ModuleFilter, VD)) {
      NextConsumer.foundDecl(VD, Reason, dynamicLookupInfo);
    }
  }
};

/// A hack to hide particular types in the "Darwin" module on Apple platforms.
class DarwinLegacyFilterDeclConsumer : public swift::VisibleDeclConsumer {
  swift::VisibleDeclConsumer &NextConsumer;
  clang::ASTContext &ClangASTContext;

  bool shouldDiscard(ValueDecl *VD) {
    if (!VD->hasClangNode())
      return false;

    const clang::Module *clangModule = getClangOwningModule(VD->getClangNode(),
                                                            ClangASTContext);
    if (!clangModule)
      return false;

    if (clangModule->Name == "MacTypes") {
      if (!VD->hasName() || VD->getBaseName().isSpecial())
        return true;
      return llvm::StringSwitch<bool>(VD->getBaseName().userFacingName())
          .Cases("OSErr", "OSStatus", "OptionBits", false)
          .Cases("FourCharCode", "OSType", false)
          .Case("Boolean", false)
          .Case("kUnknownType", false)
          .Cases("UTF32Char", "UniChar", "UTF16Char", "UTF8Char", false)
          .Case("ProcessSerialNumber", false)
          .Default(true);
    }

    if (clangModule->Parent &&
        clangModule->Parent->Name == "CarbonCore") {
      return llvm::StringSwitch<bool>(clangModule->Name)
          .Cases("BackupCore", "DiskSpaceRecovery", "MacErrors", false)
          .Case("UnicodeUtilities", false)
          .Default(true);
    }

    if (clangModule->Parent &&
        clangModule->Parent->Name == "OSServices") {
      // Note that this is a list of things to /drop/ rather than to /keep/.
      // We're more likely to see new, modern headers added to OSServices.
      return llvm::StringSwitch<bool>(clangModule->Name)
          .Cases("IconStorage", "KeychainCore", "Power", true)
          .Cases("SecurityCore", "SystemSound", true)
          .Cases("WSMethodInvocation", "WSProtocolHandler", "WSTypes", true)
          .Default(false);
    }

    return false;
  }

public:
  DarwinLegacyFilterDeclConsumer(swift::VisibleDeclConsumer &consumer,
                                 clang::ASTContext &clangASTContext)
      : NextConsumer(consumer), ClangASTContext(clangASTContext) {}

  static bool needsFiltering(const clang::Module *topLevelModule) {
    return topLevelModule && (topLevelModule->Name == "Darwin" ||
                              topLevelModule->Name == "CoreServices");
  }

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason,
                 DynamicLookupInfo dynamicLookupInfo) override {
    if (!shouldDiscard(VD))
      NextConsumer.foundDecl(VD, Reason, dynamicLookupInfo);
  }
};

} // unnamed namespace

/// Translate a MacroDefinition to a ClangNode, either a ModuleMacro for
/// a definition imported from a module or a MacroInfo for a macro defined
/// locally.
ClangNode getClangNodeForMacroDefinition(clang::MacroDefinition &M) {
  if (!M.getModuleMacros().empty())
    return ClangNode(M.getModuleMacros().back()->getMacroInfo());
  if (auto *MD = M.getLocalDirective())
    return ClangNode(MD->getMacroInfo());
  return ClangNode();
}

void ClangImporter::lookupBridgingHeaderDecls(
                              llvm::function_ref<bool(ClangNode)> filter,
                              llvm::function_ref<void(Decl*)> receiver) const {
  for (auto &Import : Impl.BridgeHeaderTopLevelImports) {
    auto ImportD = Import.get<ImportDecl*>();
    if (filter(ImportD->getClangDecl()))
      receiver(ImportD);
  }
  for (auto *ClangD : Impl.BridgeHeaderTopLevelDecls) {
    if (filter(ClangD)) {
      if (auto *ND = dyn_cast<clang::NamedDecl>(ClangD)) {
        if (Decl *imported = Impl.importDeclReal(ND, Impl.CurrentVersion))
          receiver(imported);
      }
    }
  }

  auto &ClangPP = Impl.getClangPreprocessor();
  for (clang::IdentifierInfo *II : Impl.BridgeHeaderMacros) {
    auto MD = ClangPP.getMacroDefinition(II);
    if (auto macroNode = getClangNodeForMacroDefinition(MD)) {
      if (filter(macroNode)) {
        auto MI = macroNode.getAsMacro();
        Identifier Name = Impl.getNameImporter().importMacroName(II, MI);
        if (Decl *imported = Impl.importMacro(Name, macroNode))
          receiver(imported);
      }
    }
  }
}

bool ClangImporter::lookupDeclsFromHeader(StringRef Filename,
                              llvm::function_ref<bool(ClangNode)> filter,
                              llvm::function_ref<void(Decl*)> receiver) const {
  llvm::Expected<clang::FileEntryRef> ExpectedFile =
      getClangPreprocessor().getFileManager().getFileRef(Filename);
  if (!ExpectedFile)
    return true;
  clang::FileEntryRef File = *ExpectedFile;

  auto &ClangCtx = getClangASTContext();
  auto &ClangSM = ClangCtx.getSourceManager();
  auto &ClangPP = getClangPreprocessor();

  // Look up the header in the includes of the bridging header.
  if (Impl.BridgeHeaderFiles.count(File)) {
    auto headerFilter = [&](ClangNode ClangN) -> bool {
      if (ClangN.isNull())
        return false;

      auto ClangLoc = ClangSM.getFileLoc(ClangN.getLocation());
      if (ClangLoc.isInvalid())
        return false;

      clang::OptionalFileEntryRef LocRef =
          ClangSM.getFileEntryRefForID(ClangSM.getFileID(ClangLoc));
      if (!LocRef || *LocRef != File)
        return false;

      return filter(ClangN);
    };

    lookupBridgingHeaderDecls(headerFilter, receiver);
    return false;
  }

  clang::FileID FID = ClangSM.translateFile(File);
  if (FID.isInvalid())
    return false;

  // Look up the header in the ASTReader.
  if (ClangSM.isLoadedFileID(FID)) {
    // Decls.
    SmallVector<clang::Decl *, 32> Decls;
    unsigned Length = ClangSM.getFileIDSize(FID);
    ClangCtx.getExternalSource()->FindFileRegionDecls(FID, 0, Length, Decls);
    for (auto *ClangD : Decls) {
      if (Impl.shouldIgnoreBridgeHeaderTopLevelDecl(ClangD))
        continue;
      if (filter(ClangD)) {
        if (auto *ND = dyn_cast<clang::NamedDecl>(ClangD)) {
          if (Decl *imported = Impl.importDeclReal(ND, Impl.CurrentVersion))
            receiver(imported);
        }
      }
    }

    // Macros.
    for (const auto &Iter : ClangPP.macros()) {
      auto *II = Iter.first;
      auto MD = ClangPP.getMacroDefinition(II);
      MD.forAllDefinitions([&](clang::MacroInfo *Info) {
        if (Info->isBuiltinMacro())
          return;

        auto Loc = Info->getDefinitionLoc();
        if (Loc.isInvalid() || ClangSM.getFileID(Loc) != FID)
          return;

        ClangNode MacroNode = Info;
        if (filter(MacroNode)) {
          auto Name = Impl.getNameImporter().importMacroName(II, Info);
          if (auto *Imported = Impl.importMacro(Name, MacroNode))
            receiver(Imported);
        }
      });
    }
    // FIXME: Module imports inside that header.
    return false;
  }

  return true; // no info found about that header.
}

void ClangImporter::lookupValue(DeclName name, VisibleDeclConsumer &consumer) {
  Impl.forEachLookupTable([&](SwiftLookupTable &table) -> bool {
    Impl.lookupValue(table, name, consumer);
    return false;
  });
}

ClangNode ClangImporter::getEffectiveClangNode(const Decl *decl) const {
  // Directly...
  if (auto clangNode = decl->getClangNode())
    return clangNode;

  // Or via the nested "Code" enum.
  if (auto *errorWrapper = dyn_cast<StructDecl>(decl)) {
    if (auto *code = Impl.lookupErrorCodeEnum(errorWrapper))
      if (auto clangNode = code->getClangNode())
        return clangNode;
  }

  return ClangNode();
}

void ClangImporter::lookupTypeDecl(
    StringRef rawName, ClangTypeKind kind,
    llvm::function_ref<void(TypeDecl *)> receiver) {
  clang::DeclarationName clangName(
      &Impl.Instance->getASTContext().Idents.get(rawName));

  SmallVector<clang::Sema::LookupNameKind, 1> lookupKinds;
  switch (kind) {
  case ClangTypeKind::Typedef:
    lookupKinds.push_back(clang::Sema::LookupOrdinaryName);
    break;
  case ClangTypeKind::Tag:
    lookupKinds.push_back(clang::Sema::LookupTagName);
    lookupKinds.push_back(clang::Sema::LookupNamespaceName);
    break;
  case ClangTypeKind::ObjCProtocol:
    lookupKinds.push_back(clang::Sema::LookupObjCProtocolName);
    break;
  }

  // Perform name lookup into the global scope.
  auto &sema = Impl.Instance->getSema();
  bool foundViaClang = false;

  for (auto lookupKind : lookupKinds) {
    clang::LookupResult lookupResult(sema, clangName, clang::SourceLocation(),
                                     lookupKind);
    if (!Impl.DisableSourceImport &&
        sema.LookupName(lookupResult, /*Scope=*/ sema.TUScope)) {
      for (auto clangDecl : lookupResult) {
        if (!isa<clang::TypeDecl>(clangDecl) &&
            !isa<clang::NamespaceDecl>(clangDecl) &&
            !isa<clang::ObjCContainerDecl>(clangDecl) &&
            !isa<clang::ObjCCompatibleAliasDecl>(clangDecl)) {
          continue;
        }
        Decl *imported = Impl.importDecl(clangDecl, Impl.CurrentVersion);

        // Namespaces are imported as extensions for enums.
        if (auto ext = dyn_cast_or_null<ExtensionDecl>(imported)) {
          imported = ext->getExtendedNominal();
        }
        if (auto *importedType = dyn_cast_or_null<TypeDecl>(imported)) {
          foundViaClang = true;
          receiver(importedType);
        }
      }
    }
  }

  // If Clang couldn't find the type, query the DWARFImporterDelegate.
  if (!foundViaClang)
    Impl.lookupTypeDeclDWARF(rawName, kind, receiver);
}

void ClangImporter::lookupRelatedEntity(
    StringRef rawName, ClangTypeKind kind, StringRef relatedEntityKind,
    llvm::function_ref<void(TypeDecl *)> receiver) {
  using CISTAttr = ClangImporterSynthesizedTypeAttr;
  if (relatedEntityKind ==
        CISTAttr::manglingNameForKind(CISTAttr::Kind::NSErrorWrapper) ||
      relatedEntityKind ==
        CISTAttr::manglingNameForKind(CISTAttr::Kind::NSErrorWrapperAnon)) {
    auto underlyingKind = ClangTypeKind::Tag;
    if (relatedEntityKind ==
          CISTAttr::manglingNameForKind(CISTAttr::Kind::NSErrorWrapperAnon)) {
      underlyingKind = ClangTypeKind::Typedef;
    }
    lookupTypeDecl(rawName, underlyingKind,
                   [this, receiver] (const TypeDecl *foundType) {
      auto *enumDecl =
          dyn_cast_or_null<clang::EnumDecl>(foundType->getClangDecl());
      if (!enumDecl)
        return;
      if (!Impl.getEnumInfo(enumDecl).isErrorEnum())
        return;
      auto *enclosingType =
          dyn_cast<NominalTypeDecl>(foundType->getDeclContext());
      if (!enclosingType)
        return;
      receiver(enclosingType);
    });
  }
}

void ClangModuleUnit::lookupVisibleDecls(ImportPath::Access accessPath,
                                         VisibleDeclConsumer &consumer,
                                         NLKind lookupKind) const {
  // FIXME: Ignore submodules, which are empty for now.
  if (clangModule && clangModule->isSubModule())
    return;

  // FIXME: Respect the access path.
  FilteringVisibleDeclConsumer filterConsumer(consumer, this);

  DarwinLegacyFilterDeclConsumer darwinFilterConsumer(filterConsumer,
                                                      getClangASTContext());

  swift::VisibleDeclConsumer *actualConsumer = &filterConsumer;
  if (lookupKind == NLKind::UnqualifiedLookup &&
      DarwinLegacyFilterDeclConsumer::needsFiltering(clangModule)) {
    actualConsumer = &darwinFilterConsumer;
  }

  // Find the corresponding lookup table.
  if (auto lookupTable = owner.findLookupTable(clangModule)) {
    // Search it.
    owner.lookupVisibleDecls(*lookupTable, *actualConsumer);
  }
}

namespace {
class VectorDeclPtrConsumer : public swift::VisibleDeclConsumer {
public:
  SmallVectorImpl<Decl *> &Results;
  explicit VectorDeclPtrConsumer(SmallVectorImpl<Decl *> &Decls)
    : Results(Decls) {}

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason,
                 DynamicLookupInfo) override {
    Results.push_back(VD);
  }
};
} // unnamed namespace

// FIXME(https://github.com/apple/swift-docc/issues/190): Should submodules still be crawled for the symbol graph?
bool ClangModuleUnit::shouldCollectDisplayDecls() const { return isTopLevel(); }

void ClangModuleUnit::getTopLevelDecls(SmallVectorImpl<Decl*> &results) const {
  VectorDeclPtrConsumer consumer(results);
  FilteringDeclaredDeclConsumer filterConsumer(consumer, this);
  DarwinLegacyFilterDeclConsumer darwinFilterConsumer(filterConsumer,
                                                      getClangASTContext());

  const clang::Module *topLevelModule =
    clangModule ? clangModule->getTopLevelModule() : nullptr;

  swift::VisibleDeclConsumer *actualConsumer = &filterConsumer;
  if (DarwinLegacyFilterDeclConsumer::needsFiltering(topLevelModule))
    actualConsumer = &darwinFilterConsumer;

  // Find the corresponding lookup table.
  if (auto lookupTable = owner.findLookupTable(topLevelModule)) {
    // Search it.
    owner.lookupVisibleDecls(*lookupTable, *actualConsumer);

    // Add the extensions produced by importing categories.
    for (auto category : lookupTable->categories()) {
      if (auto extension = cast_or_null<ExtensionDecl>(
              owner.importDecl(category, owner.CurrentVersion,
                               /*UseCanonical*/false))) {
        results.push_back(extension);
      }
    }

    auto findEnclosingExtension = [](Decl *importedDecl) -> ExtensionDecl * {
      for (auto importedDC = importedDecl->getDeclContext();
           !importedDC->isModuleContext();
           importedDC = importedDC->getParent()) {
        if (auto ext = dyn_cast<ExtensionDecl>(importedDC))
          return ext;
      }
      return nullptr;
    };
    // Retrieve all of the globals that will be mapped to members.

    // FIXME: Since we don't represent Clang submodules as Swift
    // modules, we're getting everything.
    llvm::SmallPtrSet<ExtensionDecl *, 8> knownExtensions;
    for (auto entry : lookupTable->allGlobalsAsMembers()) {
      auto decl = entry.get<clang::NamedDecl *>();
      Decl *importedDecl = owner.importDecl(decl, owner.CurrentVersion);
      if (!importedDecl) continue;

      // Find the enclosing extension, if there is one.
      ExtensionDecl *ext = findEnclosingExtension(importedDecl);
      if (ext && knownExtensions.insert(ext).second)
        results.push_back(ext);

      // If this is a compatibility typealias, the canonical type declaration
      // may exist in another extension.
      auto alias = dyn_cast<TypeAliasDecl>(importedDecl);
      if (!alias || !alias->isCompatibilityAlias()) continue;

      auto aliasedTy = alias->getUnderlyingType();
      ext = nullptr;
      importedDecl = nullptr;

      // Note: We can't use getAnyGeneric() here because `aliasedTy`
      // might be typealias.
      if (auto Ty = dyn_cast<TypeAliasType>(aliasedTy.getPointer()))
        importedDecl = Ty->getDecl();
      else if (auto Ty = dyn_cast<AnyGenericType>(aliasedTy.getPointer()))
        importedDecl = Ty->getDecl();
      if (!importedDecl) continue;

      ext = findEnclosingExtension(importedDecl);
      if (ext && knownExtensions.insert(ext).second)
        results.push_back(ext);
    }
  }
}

ImportDecl *swift::createImportDecl(ASTContext &Ctx,
                                    DeclContext *DC,
                                    ClangNode ClangN,
                                    ArrayRef<clang::Module *> Exported) {
  auto *ImportedMod = ClangN.getClangModule();
  assert(ImportedMod);

  ImportPath::Builder importPath;
  auto *TmpMod = ImportedMod;
  while (TmpMod) {
    // If this is a C++ stdlib module, print its name as `CxxStdlib` instead of
    // `std`. `CxxStdlib` is the only accepted spelling of the C++ stdlib module
    // name in Swift.
    Identifier moduleName = !TmpMod->isSubModule() && TmpMod->Name == "std"
                                ? Ctx.Id_CxxStdlib
                                : Ctx.getIdentifier(TmpMod->Name);
    importPath.push_back(moduleName);
    TmpMod = TmpMod->Parent;
  }
  std::reverse(importPath.begin(), importPath.end());

  bool IsExported = false;
  for (auto *ExportedMod : Exported) {
    if (ImportedMod == ExportedMod) {
      IsExported = true;
      break;
    }
  }

  auto *ID = ImportDecl::create(Ctx, DC, SourceLoc(),
                                ImportKind::Module, SourceLoc(),
                                importPath.get(), ClangN);
  if (IsExported)
    ID->getAttrs().add(new (Ctx) ExportedAttr(/*IsImplicit=*/false));
  return ID;
}

static void getImportDecls(ClangModuleUnit *ClangUnit, const clang::Module *M,
                           SmallVectorImpl<Decl *> &Results) {
  assert(M);
  SmallVector<clang::Module *, 1> Exported;
  M->getExportedModules(Exported);

  ASTContext &Ctx = ClangUnit->getASTContext();

  for (auto *ImportedMod : M->Imports) {
    auto *ID = createImportDecl(Ctx, ClangUnit, ImportedMod, Exported);
    Results.push_back(ID);
  }
}

void ClangModuleUnit::getDisplayDecls(SmallVectorImpl<Decl*> &results, bool recursive) const {
  if (clangModule)
    getImportDecls(const_cast<ClangModuleUnit *>(this), clangModule, results);
  getTopLevelDecls(results);
}

void ClangModuleUnit::lookupValue(DeclName name, NLKind lookupKind,
                                  OptionSet<ModuleLookupFlags> flags,
                                  SmallVectorImpl<ValueDecl*> &results) const {
  // FIXME: Ignore submodules, which are empty for now.
  if (clangModule && clangModule->isSubModule())
    return;

  VectorDeclConsumer vectorWriter(results);
  FilteringVisibleDeclConsumer filteringConsumer(vectorWriter, this);

  DarwinLegacyFilterDeclConsumer darwinFilterConsumer(filteringConsumer,
                                                      getClangASTContext());

  swift::VisibleDeclConsumer *consumer = &filteringConsumer;
  if (lookupKind == NLKind::UnqualifiedLookup &&
      DarwinLegacyFilterDeclConsumer::needsFiltering(clangModule)) {
    consumer = &darwinFilterConsumer;
  }

  // Find the corresponding lookup table.
  if (auto lookupTable = owner.findLookupTable(clangModule)) {
    // Search it.
    owner.lookupValue(*lookupTable, name, *consumer);
  }
}

bool ClangImporter::Implementation::isVisibleClangEntry(
    const clang::NamedDecl *clangDecl) {
  // For a declaration, check whether the declaration is hidden.
  clang::Sema &clangSema = getClangSema();
  if (clangSema.isVisible(clangDecl)) return true;

  // Is any redeclaration visible?
  for (auto redecl : clangDecl->redecls()) {
    if (clangSema.isVisible(cast<clang::NamedDecl>(redecl))) return true;
  }

  return false;
}

bool ClangImporter::Implementation::isVisibleClangEntry(
  SwiftLookupTable::SingleEntry entry) {
  if (auto clangDecl = entry.dyn_cast<clang::NamedDecl *>()) {
    return isVisibleClangEntry(clangDecl);
  }

  // If it's a macro from a module, check whether the module has been imported.
  if (auto moduleMacro = entry.dyn_cast<clang::ModuleMacro *>()) {
    clang::Module *module = moduleMacro->getOwningModule();
    return module->NameVisibility == clang::Module::AllVisible;
  }

  return true;
}

TypeDecl *
ClangModuleUnit::lookupNestedType(Identifier name,
                                  const NominalTypeDecl *baseType) const {
  // Special case for error code enums: try looking directly into the struct
  // first. But only if it looks like a synthesized error wrapped struct.
  if (name == getASTContext().Id_Code &&
      !baseType->hasClangNode() &&
      isa<StructDecl>(baseType)) {
    auto *wrapperStruct = cast<StructDecl>(baseType);
    if (auto *codeEnum = owner.lookupErrorCodeEnum(wrapperStruct))
      return codeEnum;

    // Otherwise, fall back and try via lookup table.
  }

  auto lookupTable = owner.findLookupTable(clangModule);
  if (!lookupTable)
    return nullptr;

  auto baseTypeContext = owner.getEffectiveClangContext(baseType);
  if (!baseTypeContext)
    return nullptr;

  // FIXME: This is very similar to what's in Implementation::lookupValue and
  // Implementation::loadAllMembers.
  SmallVector<TypeDecl *, 2> results;
  for (auto entry : lookupTable->lookup(SerializedSwiftName(name.str()),
                                        baseTypeContext)) {
    // If the entry is not visible, skip it.
    if (!owner.isVisibleClangEntry(entry)) continue;

    auto *clangDecl = entry.dyn_cast<clang::NamedDecl *>();
    if (!clangDecl)
      continue;

    const auto *clangTypeDecl = clangDecl->getMostRecentDecl();

    bool anyMatching = false;
    TypeDecl *originalDecl = nullptr;
    owner.forEachDistinctName(clangTypeDecl,
                              [&](ImportedName newName,
                                  ImportNameVersion nameVersion) -> bool {
      if (anyMatching)
        return true;
      if (!newName.getDeclName().isSimpleName(name))
        return true;

      auto decl = dyn_cast_or_null<TypeDecl>(
          owner.importDeclReal(clangTypeDecl, nameVersion));
      if (!decl)
        return false;

      if (!originalDecl)
        originalDecl = decl;
      else if (originalDecl == decl)
        return true;

      auto *importedContext = decl->getDeclContext()->getSelfNominalTypeDecl();
      if (importedContext != baseType)
        return true;

      assert(decl->getName() == name &&
             "importFullName behaved differently from importDecl");
      results.push_back(decl);
      anyMatching = true;
      return true;
    });
  }

  if (results.size() != 1) {
    // It's possible that two types were import-as-member'd onto the same base
    // type with the same name. In this case, fall back to regular lookup.
    return nullptr;
  }

  return results.front();
}

void ClangImporter::loadExtensions(NominalTypeDecl *nominal,
                                   unsigned previousGeneration) {
  // Determine the effective Clang context for this Swift nominal type.
  auto effectiveClangContext = Impl.getEffectiveClangContext(nominal);
  if (!effectiveClangContext) return;

  // For an Objective-C class, import all of the visible categories.
  if (auto objcClass = dyn_cast_or_null<clang::ObjCInterfaceDecl>(
                         effectiveClangContext.getAsDeclContext())) {
    SmallVector<clang::NamedDecl *, 4> DelayedCategories;

    // Simply importing the categories adds them to the list of extensions.
    for (const auto *Cat : objcClass->known_categories()) {
      if (getClangSema().isVisible(Cat)) {
        Impl.importDeclReal(Cat, Impl.CurrentVersion);
      }
    }
  }

  // Dig through each of the Swift lookup tables, creating extensions
  // where needed.
  (void)Impl.forEachLookupTable([&](SwiftLookupTable &table) -> bool {
      // FIXME: If we already looked at this for this generation,
      // skip.

      for (auto entry : table.allGlobalsAsMembersInContext(effectiveClangContext)) {
        // If the entry is not visible, skip it.
        if (!Impl.isVisibleClangEntry(entry)) continue;

        if (auto decl = entry.dyn_cast<clang::NamedDecl *>()) {
          // Import the context of this declaration, which has the
          // side effect of creating instantiations.
          (void)Impl.importDeclContextOf(decl, effectiveClangContext);
        } else {
          llvm_unreachable("Macros cannot be imported as members.");
        }
      }

      return false;
    });
}

void ClangImporter::loadObjCMethods(
       NominalTypeDecl *typeDecl,
       ObjCSelector selector,
       bool isInstanceMethod,
       unsigned previousGeneration,
       llvm::TinyPtrVector<AbstractFunctionDecl *> &methods) {
  // TODO: We don't currently need to load methods from imported ObjC protocols.
  auto classDecl = dyn_cast<ClassDecl>(typeDecl);
  if (!classDecl)
    return;

  const auto *objcClass =
      dyn_cast_or_null<clang::ObjCInterfaceDecl>(classDecl->getClangDecl());
  if (!objcClass)
    return;

  // Collect the set of visible Objective-C methods with this selector.
  clang::Selector clangSelector = Impl.exportSelector(selector);

  AbstractFunctionDecl *method = nullptr;
  auto *objcMethod = objcClass->lookupMethod(
      clangSelector, isInstanceMethod,
      /*shallowCategoryLookup=*/false,
      /*followSuper=*/false);

  if (objcMethod) {
    // If we found a property accessor, import the property.
    if (objcMethod->isPropertyAccessor())
      (void)Impl.importDecl(objcMethod->findPropertyDecl(true),
                            Impl.CurrentVersion);

    method = dyn_cast_or_null<AbstractFunctionDecl>(
        Impl.importDecl(objcMethod, Impl.CurrentVersion));
  }

  // If we didn't find anything, we're done.
  if (method == nullptr)
    return;

  // If we did find something, it might be a duplicate of something we found
  // earlier, because we aren't tracking generation counts for Clang modules.
  // Filter out the duplicates.
  // FIXME: We shouldn't need to do this.
  if (!llvm::is_contained(methods, method))
    methods.push_back(method);
}

void
ClangModuleUnit::lookupClassMember(ImportPath::Access accessPath,
                                   DeclName name,
                                   SmallVectorImpl<ValueDecl*> &results) const {
  // FIXME: Ignore submodules, which are empty for now.
  if (clangModule && clangModule->isSubModule())
    return;

  VectorDeclConsumer consumer(results);

  // Find the corresponding lookup table.
  if (auto lookupTable = owner.findLookupTable(clangModule)) {
    // Search it.
    owner.lookupObjCMembers(*lookupTable, name, consumer);
  }
}

void ClangModuleUnit::lookupClassMembers(ImportPath::Access accessPath,
                                         VisibleDeclConsumer &consumer) const {
  // FIXME: Ignore submodules, which are empty for now.
  if (clangModule && clangModule->isSubModule())
    return;

  // Find the corresponding lookup table.
  if (auto lookupTable = owner.findLookupTable(clangModule)) {
    // Search it.
    owner.lookupAllObjCMembers(*lookupTable, consumer);
  }
}

void ClangModuleUnit::lookupObjCMethods(
       ObjCSelector selector,
       SmallVectorImpl<AbstractFunctionDecl *> &results) const {
  // FIXME: Ignore submodules, which are empty for now.
  if (clangModule && clangModule->isSubModule())
    return;

  // Map the selector into a Clang selector.
  auto clangSelector = owner.exportSelector(selector);
  if (clangSelector.isNull()) return;

  // Collect all of the Objective-C methods with this selector.
  SmallVector<clang::ObjCMethodDecl *, 8> objcMethods;
  auto &clangSema = owner.getClangSema();
  auto &clangObjc = clangSema.ObjC();
  clangObjc.CollectMultipleMethodsInGlobalPool(clangSelector,
                                               objcMethods,
                                               /*InstanceFirst=*/true,
                                               /*CheckTheOther=*/false);
  clangObjc.CollectMultipleMethodsInGlobalPool(clangSelector,
                                               objcMethods,
                                               /*InstanceFirst=*/false,
                                               /*CheckTheOther=*/false);

  // Import the methods.
  auto &clangCtx = clangSema.getASTContext();
  for (auto objcMethod : objcMethods) {
    // Verify that this method came from this module.
    auto owningClangModule = getClangTopLevelOwningModule(objcMethod, clangCtx);
    if (owningClangModule != clangModule) continue;

    if (shouldSuppressDeclImport(objcMethod))
      continue;

    // If we found a property accessor, import the property.
    if (objcMethod->isPropertyAccessor())
      (void)owner.importDecl(objcMethod->findPropertyDecl(true),
                             owner.CurrentVersion);
    Decl *imported = owner.importDecl(objcMethod, owner.CurrentVersion);
    if (!imported) continue;

    if (auto func = dyn_cast<AbstractFunctionDecl>(imported))
      results.push_back(func);

    // If there is an alternate declaration, also look at it.
    for (auto alternate : owner.getAlternateDecls(imported)) {
      if (auto func = dyn_cast<AbstractFunctionDecl>(alternate))
        results.push_back(func);
    }
  }
}

void ClangModuleUnit::collectLinkLibraries(
    ModuleDecl::LinkLibraryCallback callback) const {
  if (!clangModule)
    return;

  // Skip this lib name in favor of export_as name.
  if (clangModule->UseExportAsModuleLinkName)
    return;

  for (auto clangLinkLib : clangModule->LinkLibraries) {
    LibraryKind kind;
    if (clangLinkLib.IsFramework)
      kind = LibraryKind::Framework;
    else
      kind = LibraryKind::Library;

    callback(LinkLibrary(clangLinkLib.Library, kind));
  }
}

StringRef ClangModuleUnit::getFilename() const {
  if (!clangModule) {
    StringRef SinglePCH = owner.getSinglePCHImport();
    if (SinglePCH.empty())
      return "<imports>";
    else
      return SinglePCH;
  }
  if (auto F = clangModule->getASTFile())
    return F->getName();
  return StringRef();
}

StringRef ClangModuleUnit::getLoadedFilename() const {
  if (auto F = clangModule->getASTFile())
    return F->getName();
  return StringRef();
}

clang::TargetInfo &ClangImporter::getModuleAvailabilityTarget() const {
  return Impl.Instance->getTarget();
}

clang::TargetInfo &ClangImporter::getTargetInfo() const {
  return *Impl.getSwiftTargetInfo();
}

clang::ASTContext &ClangImporter::getClangASTContext() const {
  return Impl.getClangASTContext();
}

clang::Preprocessor &ClangImporter::getClangPreprocessor() const {
  return Impl.getClangPreprocessor();
}

const clang::CompilerInstance &ClangImporter::getClangInstance() const {
  return *Impl.Instance;
}

const clang::Module *ClangImporter::getClangOwningModule(ClangNode Node) const {
  return Impl.getClangOwningModule(Node);
}

const clang::Module *
ClangImporter::Implementation::getClangOwningModule(ClangNode Node) const {
  return ::getClangOwningModule(Node, getClangASTContext());
}

bool ClangImporter::hasTypedef(const clang::Decl *typeDecl) const {
  return Impl.DeclsWithSuperfluousTypedefs.count(typeDecl);
}

clang::Sema &ClangImporter::getClangSema() const {
  return Impl.getClangSema();
}

clang::CodeGenOptions &ClangImporter::getCodeGenOpts() const {
  return *Impl.getSwiftCodeGenOptions();
}

std::string ClangImporter::getClangModuleHash() const {
  return Impl.Invocation->getModuleHash(Impl.Instance->getDiagnostics());
}

std::vector<std::string>
ClangImporter::getSwiftExplicitModuleDirectCC1Args() const {
  llvm::SmallVector<const char*> clangArgs;
  clangArgs.reserve(Impl.ClangArgs.size());
  llvm::for_each(Impl.ClangArgs, [&](const std::string &Arg) {
    clangArgs.push_back(Arg.c_str());
  });

  clang::CompilerInvocation instance;
  clang::DiagnosticsEngine clangDiags(new clang::DiagnosticIDs(),
                                      new clang::DiagnosticOptions(),
                                      new clang::IgnoringDiagConsumer());
  bool success = clang::CompilerInvocation::CreateFromArgs(instance, clangArgs,
                                                           clangDiags);
  (void)success;
  assert(success && "clang options from clangImporter failed to parse");

  if (!Impl.SwiftContext.CASOpts.EnableCaching)
    return instance.getCC1CommandLine();

  // Clear some options that are not needed.
  instance.clearImplicitModuleBuildOptions();

  // CASOpts are forwarded from swift arguments.
  instance.getCASOpts() = clang::CASOptions();

  // HeaderSearchOptions.
  // Clang search options are only used by scanner and clang importer from main
  // module should not using search paths to find modules.
  auto &HSOpts = instance.getHeaderSearchOpts();
  HSOpts.VFSOverlayFiles.clear();
  HSOpts.UserEntries.clear();
  HSOpts.SystemHeaderPrefixes.clear();

  // FrontendOptions.
  auto &FEOpts = instance.getFrontendOpts();
  FEOpts.IncludeTimestamps = false;
  FEOpts.ModuleMapFiles.clear();

  // IndexStorePath is forwarded from swift.
  FEOpts.IndexStorePath.clear();

  // PreprocessorOptions.
  // Cannot clear macros as the main module clang importer doesn't have clang
  // include tree created and it has to be created from command-line. However,
  // include files are no collected into CASFS so they will not be found so
  // clear them to avoid problem.
  auto &PPOpts = instance.getPreprocessorOpts();
  PPOpts.MacroIncludes.clear();
  PPOpts.Includes.clear();

  if (Impl.SwiftContext.ClangImporterOpts.UseClangIncludeTree) {
    // FileSystemOptions.
    auto &FSOpts = instance.getFileSystemOpts();
    FSOpts.WorkingDir.clear();
  }

  if (!Impl.SwiftContext.SearchPathOpts.ScannerPrefixMapper.empty()) {
    // Remap all the paths if requested.
    llvm::PrefixMapper Mapper;
    clang::tooling::dependencies::DepscanPrefixMapping::configurePrefixMapper(
        Impl.SwiftContext.SearchPathOpts.ScannerPrefixMapper, Mapper);
    clang::tooling::dependencies::DepscanPrefixMapping::remapInvocationPaths(
        instance, Mapper);
    instance.getFrontendOpts().PathPrefixMappings.clear();
  }

  return instance.getCC1CommandLine();
}

std::optional<Decl *>
ClangImporter::importDeclCached(const clang::NamedDecl *ClangDecl) {
  return Impl.importDeclCached(ClangDecl, Impl.CurrentVersion);
}

void ClangImporter::printStatistics() const {
  Impl.Instance->getASTReader()->PrintStats();
}

void ClangImporter::verifyAllModules() {
#ifndef NDEBUG
  if (Impl.VerifiedDeclsCounter == Impl.ImportedDecls.size())
    return;

  // Collect the Decls before verifying them; the act of verifying may cause
  // more decls to be imported and modify the map while we are iterating it.
  size_t verifiedCounter = Impl.ImportedDecls.size();
  SmallVector<Decl *, 8> Decls;
  for (auto &I : Impl.ImportedDecls)
    if (I.first.second == Impl.CurrentVersion)
      if (Decl *D = I.second)
        Decls.push_back(D);

  for (auto D : Decls)
    verify(D);

  Impl.VerifiedDeclsCounter = verifiedCounter;
#endif
}

const clang::Type *
ClangImporter::parseClangFunctionType(StringRef typeStr,
                                      SourceLoc loc) const {
  auto &sema = Impl.getClangSema();
  StringRef filename = Impl.SwiftContext.SourceMgr.getDisplayNameForLoc(loc);
  // TODO: Obtain a clang::SourceLocation from the swift::SourceLoc we have
  auto parsedType = sema.ParseTypeFromStringCallback(typeStr, filename, {});
  if (!parsedType.isUsable())
    return nullptr;
  clang::QualType resultType = clang::Sema::GetTypeFromParser(parsedType.get());
  auto *typePtr = resultType.getTypePtrOrNull();
  if (typePtr && (typePtr->isFunctionPointerType()
                  || typePtr->isBlockPointerType()))
      return typePtr;
  return nullptr;
}

void ClangImporter::printClangType(const clang::Type *type,
                                   llvm::raw_ostream &os) const {
  auto policy = clang::PrintingPolicy(getClangASTContext().getLangOpts());
  clang::QualType(type, 0).print(os, policy);
}

//===----------------------------------------------------------------------===//
// ClangModule Implementation
//===----------------------------------------------------------------------===//

static_assert(IsTriviallyDestructible<ClangModuleUnit>::value,
              "ClangModuleUnits are BumpPtrAllocated; the d'tor is not called");

ClangModuleUnit::ClangModuleUnit(ModuleDecl &M,
                                 ClangImporter::Implementation &owner,
                                 const clang::Module *clangModule)
  : LoadedFile(FileUnitKind::ClangModule, M), owner(owner),
    clangModule(clangModule) {
  // Capture the file metadata before it goes away.
  if (clangModule)
    ASTSourceDescriptor = {*const_cast<clang::Module *>(clangModule)};
}

StringRef ClangModuleUnit::getModuleDefiningPath() const {
  if (!clangModule || clangModule->DefinitionLoc.isInvalid())
    return "";

  auto &clangSourceMgr = owner.getClangASTContext().getSourceManager();
  return clangSourceMgr.getFilename(clangModule->DefinitionLoc);
}

std::optional<clang::ASTSourceDescriptor>
ClangModuleUnit::getASTSourceDescriptor() const {
  if (clangModule) {
    assert(ASTSourceDescriptor.getModuleOrNull() == clangModule);
    return ASTSourceDescriptor;
  }
  return std::nullopt;
}

bool ClangModuleUnit::hasClangModule(ModuleDecl *M) {
  for (auto F : M->getFiles()) {
    if (isa<ClangModuleUnit>(F))
      return true;
  }
  return false;
}

bool ClangModuleUnit::isTopLevel() const {
  return !clangModule || !clangModule->isSubModule();
}

bool ClangModuleUnit::isSystemModule() const {
  return clangModule && clangModule->IsSystem;
}

clang::ASTContext &ClangModuleUnit::getClangASTContext() const {
  return owner.getClangASTContext();
}

StringRef ClangModuleUnit::getExportedModuleName() const {
  if (clangModule && !clangModule->ExportAsModule.empty())
    return clangModule->ExportAsModule;

  // Return module real name (see FileUnit::getExportedModuleName)
  return getParentModule()->getRealName().str();
}

ModuleDecl *ClangModuleUnit::getOverlayModule() const {
  if (!clangModule)
    return nullptr;

  if (owner.DisableOverlayModules)
    return nullptr;

  if (!isTopLevel()) {
    // FIXME: Is this correct for submodules?
    auto topLevel = clangModule->getTopLevelModule();
    auto wrapper = owner.getWrapperForModule(topLevel);
    return wrapper->getOverlayModule();
  }

  if (!overlayModule.getInt()) {
    // FIXME: Include proper source location.
    ModuleDecl *M = getParentModule();
    ASTContext &Ctx = M->getASTContext();
    auto overlay = Ctx.getOverlayModule(this);
    if (overlay) {
      Ctx.addLoadedModule(overlay);
    } else {
      // FIXME: This is the awful legacy of the old implementation of overlay
      // loading laid bare. Because the previous implementation used
      // ASTContext::getModuleByIdentifier, it consulted the clang importer
      // recursively which forced the current module, its dependencies, and
      // the overlays of those dependencies to load and
      // become visible in the current context. All of the callers of
      // ClangModuleUnit::getOverlayModule are relying on this behavior, and
      // untangling them is going to take a heroic amount of effort.
      // Clang module loading should *never* *ever* be allowed to load unrelated
      // Swift modules.
      ImportPath::Module::Builder builder(M->getName());
      (void) owner.loadModule(SourceLoc(), std::move(builder).get());
    }
    // If this Clang module is a part of the C++ stdlib, and we haven't loaded
    // the overlay for it so far, it is a split libc++ module (e.g. std_vector).
    // Load the CxxStdlib overlay explicitly, if building with the
    // platform-default C++ stdlib.
    if (!overlay && importer::isCxxStdModule(clangModule) &&
        Ctx.LangOpts.isUsingPlatformDefaultCXXStdlib()) {
      ImportPath::Module::Builder builder(Ctx.Id_CxxStdlib);
      overlay = owner.loadModule(SourceLoc(), std::move(builder).get());
    }
    auto mutableThis = const_cast<ClangModuleUnit *>(this);
    mutableThis->overlayModule.setPointerAndInt(overlay, true);
  }

  return overlayModule.getPointer();
}

void ClangModuleUnit::getImportedModules(
    SmallVectorImpl<ImportedModule> &imports,
    ModuleDecl::ImportFilter filter) const {
  // Bail out if we /only/ want ImplementationOnly imports; Clang modules never
  // have any of these.
  if (filter.containsOnly(ModuleDecl::ImportFilterKind::ImplementationOnly))
    return;

  // [NOTE: Pure-Clang-modules-privately-import-stdlib]:
  // Needed for implicitly synthesized conformances.
  if (filter.contains(ModuleDecl::ImportFilterKind::Default))
    if (auto stdlib = owner.getStdlibModule())
      imports.push_back({ImportPath::Access(), stdlib});

  SmallVector<clang::Module *, 8> imported;
  if (!clangModule) {
    // This is the special "imported headers" module.
    if (filter.contains(ModuleDecl::ImportFilterKind::Exported)) {
      imported.append(owner.ImportedHeaderExports.begin(),
                      owner.ImportedHeaderExports.end());
    }

  } else {
    clangModule->getExportedModules(imported);

    if (filter.contains(ModuleDecl::ImportFilterKind::Default)) {
      // Copy in any modules that are imported but not exported.
      llvm::SmallPtrSet<clang::Module *, 8> knownModules(imported.begin(),
                                                         imported.end());
      if (!filter.contains(ModuleDecl::ImportFilterKind::Exported)) {
        // Remove the exported ones now that we're done with them.
        imported.clear();
      }
      llvm::copy_if(clangModule->Imports, std::back_inserter(imported),
                    [&](clang::Module *mod) {
                     return !knownModules.insert(mod).second;
                    });

      // FIXME: The parent module isn't exactly a private import, but it is
      // needed for link dependencies.
      if (clangModule->Parent)
        imported.push_back(clangModule->Parent);
    }
  }

  auto topLevelOverlay = getOverlayModule();
  for (auto importMod : imported) {
    auto wrapper = owner.getWrapperForModule(importMod);

    auto actualMod = wrapper->getOverlayModule();
    if (!actualMod) {
      // HACK: Deal with imports of submodules by importing the top-level module
      // as well.
      auto importTopLevel = importMod->getTopLevelModule();
      if (importTopLevel != importMod) {
        if (!clangModule || importTopLevel != clangModule->getTopLevelModule()){
          auto topLevelWrapper = owner.getWrapperForModule(importTopLevel);
          imports.push_back({ ImportPath::Access(),
                              topLevelWrapper->getParentModule() });
        }
      }
      actualMod = wrapper->getParentModule();
    } else if (actualMod == topLevelOverlay) {
      actualMod = wrapper->getParentModule();
    }

    assert(actualMod && "Missing imported overlay");
    imports.push_back({ImportPath::Access(), actualMod});
  }
}

void ClangModuleUnit::getImportedModulesForLookup(
    SmallVectorImpl<ImportedModule> &imports) const {

  // Reuse our cached list of imports if we have one.
  if (importedModulesForLookup.has_value()) {
    imports.append(importedModulesForLookup->begin(),
                   importedModulesForLookup->end());
    return;
  }

  size_t firstImport = imports.size();

  SmallVector<clang::Module *, 8> imported;
  const clang::Module *topLevel;
  ModuleDecl *topLevelOverlay = getOverlayModule();
  if (!clangModule) {
    // This is the special "imported headers" module.
    imported.append(owner.ImportedHeaderExports.begin(),
                    owner.ImportedHeaderExports.end());
    topLevel = nullptr;
  } else {
    clangModule->getExportedModules(imported);
    topLevel = clangModule->getTopLevelModule();

    // If this is a C++ module, implicitly import the Cxx module, which contains
    // definitions of Swift protocols that C++ types might conform to, such as
    // CxxSequence.
    if (owner.SwiftContext.LangOpts.EnableCXXInterop &&
        requiresCPlusPlus(clangModule) && clangModule->Name != CXX_SHIM_NAME) {
      auto *cxxModule =
          owner.SwiftContext.getModuleByIdentifier(owner.SwiftContext.Id_Cxx);
      if (cxxModule)
        imports.push_back({ImportPath::Access(), cxxModule});
    }
  }

  if (imported.empty()) {
    importedModulesForLookup = ArrayRef<ImportedModule>();
    return;
  }

  SmallPtrSet<clang::Module *, 32> seen{imported.begin(), imported.end()};
  SmallVector<clang::Module *, 8> tmpBuf;
  llvm::SmallSetVector<clang::Module *, 8> topLevelImported;

  // Get the transitive set of top-level imports. That is, if a particular
  // import is a top-level import, add it. Otherwise, keep searching.
  while (!imported.empty()) {
    clang::Module *next = imported.pop_back_val();

    // HACK: Deal with imports of submodules by importing the top-level module
    // as well, unless it's the top-level module we're currently in.
    clang::Module *nextTopLevel = next->getTopLevelModule();
    if (nextTopLevel != topLevel) {
      topLevelImported.insert(nextTopLevel);

      // Don't continue looking through submodules of modules that have
      // overlays. The overlay might shadow things.
      auto wrapper = owner.getWrapperForModule(nextTopLevel);
      if (wrapper->getOverlayModule())
        continue;
    }

    // Only look through the current module if it's not top-level.
    if (nextTopLevel == next)
      continue;

    next->getExportedModules(tmpBuf);
    for (clang::Module *nextImported : tmpBuf) {
      if (seen.insert(nextImported).second)
        imported.push_back(nextImported);
    }
    tmpBuf.clear();
  }

  for (auto importMod : topLevelImported) {
    auto wrapper = owner.getWrapperForModule(importMod);

    ModuleDecl *actualMod = nullptr;
    if (owner.SwiftContext.LangOpts.EnableCXXInterop && topLevel &&
        isCxxStdModule(topLevel) && wrapper->clangModule &&
        isCxxStdModule(wrapper->clangModule)) {
      // The CxxStdlib overlay re-exports the clang module std, which in recent
      // libc++ versions re-exports top-level modules for different std headers
      // (std_string, std_vector, etc). The overlay module for each of the std
      // modules is the CxxStdlib module itself. Make sure we return the actual
      // clang modules (std_xyz) as transitive dependencies instead of just
      // CxxStdlib itself.
      actualMod = wrapper->getParentModule();
    } else {
      actualMod = wrapper->getOverlayModule();
      if (!actualMod || actualMod == topLevelOverlay)
        actualMod = wrapper->getParentModule();
    }

    assert(actualMod && "Missing imported overlay");
    imports.push_back({ImportPath::Access(), actualMod});
  }

  // Cache our results for use next time.
  auto importsToCache = llvm::ArrayRef(imports).slice(firstImport);
  importedModulesForLookup = getASTContext().AllocateCopy(importsToCache);
}

void ClangImporter::getMangledName(raw_ostream &os,
                                   const clang::NamedDecl *clangDecl) const {
  if (!Impl.Mangler)
    Impl.Mangler.reset(getClangASTContext().createMangleContext());

  return Impl.getMangledName(Impl.Mangler.get(), clangDecl, os);
}

void ClangImporter::Implementation::getMangledName(
    clang::MangleContext *mangler, const clang::NamedDecl *clangDecl,
    raw_ostream &os) {
  if (auto ctor = dyn_cast<clang::CXXConstructorDecl>(clangDecl)) {
    auto ctorGlobalDecl =
        clang::GlobalDecl(ctor, clang::CXXCtorType::Ctor_Complete);
    mangler->mangleCXXName(ctorGlobalDecl, os);
  } else {
    mangler->mangleName(clangDecl, os);
  }
}

// ---------------------------------------------------------------------------
// Swift lookup tables
// ---------------------------------------------------------------------------

SwiftLookupTable *ClangImporter::Implementation::findLookupTable(
                    const clang::Module *clangModule) {
  // If the Clang module is null, use the bridging header lookup table.
  if (!clangModule)
    return BridgingHeaderLookupTable.get();

  // Submodules share lookup tables with their parents.
  if (clangModule->isSubModule())
    return findLookupTable(clangModule->getTopLevelModule());

  // Look for a Clang module with this name.
  auto known = LookupTables.find(clangModule->Name);
  if (known == LookupTables.end()) return nullptr;

  return known->second.get();
}

SwiftLookupTable *
ClangImporter::Implementation::findLookupTable(const clang::Decl *decl) {
  // Contents of a C++ namespace are added to the __ObjC module.
  bool isWithinNamespace = false;
  auto declContext = decl->getDeclContext();
  while (!declContext->isTranslationUnit()) {
    if (declContext->isNamespace()) {
      isWithinNamespace = true;
      break;
    }
    declContext = declContext->getParent();
  }

  clang::Module *owningModule = nullptr;
  if (!isWithinNamespace) {
    // Members of class template specializations don't have an owning module.
    if (auto spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(decl))
      owningModule = spec->getSpecializedTemplate()->getOwningModule();
    else
      owningModule = decl->getOwningModule();
  }
  return findLookupTable(owningModule);
}

bool ClangImporter::Implementation::forEachLookupTable(
       llvm::function_ref<bool(SwiftLookupTable &table)> fn) {
  // Visit the bridging header's lookup table.
  if (fn(*BridgingHeaderLookupTable)) return true;

  // Collect and sort the set of module names.
  SmallVector<StringRef, 4> moduleNames;
  for (const auto &entry : LookupTables) {
    moduleNames.push_back(entry.first);
  }
  llvm::array_pod_sort(moduleNames.begin(), moduleNames.end());

  // Visit the lookup tables.
  for (auto moduleName : moduleNames) {
    if (fn(*LookupTables[moduleName])) return true;
  }

  return false;
}

bool ClangImporter::Implementation::lookupValue(SwiftLookupTable &table,
                                                DeclName name,
                                                VisibleDeclConsumer &consumer) {

  auto &clangCtx = getClangASTContext();
  auto clangTU = clangCtx.getTranslationUnitDecl();
  auto *importer =
      static_cast<ClangImporter *>(SwiftContext.getClangModuleLoader());

  bool declFound = false;

  if (name.isOperator()) {
    for (auto entry : table.lookupMemberOperators(name.getBaseName())) {
      if (isVisibleClangEntry(entry)) {
        if (auto decl = dyn_cast_or_null<ValueDecl>(
                importDeclReal(entry->getMostRecentDecl(), CurrentVersion))) {
          consumer.foundDecl(decl, DeclVisibilityKind::VisibleAtTopLevel);
          declFound = true;
        }
      }
    }

    // If CXXInterop is enabled we need to check the modified operator name as
    // well
    if (SwiftContext.LangOpts.EnableCXXInterop) {
      auto funcBaseName =
          DeclBaseName(SwiftContext.getIdentifier(getPrivateOperatorName(
              name.getBaseName().getIdentifier().str().str())));
      for (auto entry : table.lookupMemberOperators(funcBaseName)) {
        if (isVisibleClangEntry(entry)) {
          if (auto func = dyn_cast_or_null<FuncDecl>(
                  importDeclReal(entry->getMostRecentDecl(), CurrentVersion))) {
            if (auto synthesizedOperator =
                    importer->getCXXSynthesizedOperatorFunc(func)) {
              consumer.foundDecl(synthesizedOperator,
                                 DeclVisibilityKind::VisibleAtTopLevel);
              declFound = true;
            }
          }
        }
      }
    }
  }

  for (auto entry : table.lookup(name.getBaseName(), clangTU)) {
    // If the entry is not visible, skip it.
    if (!isVisibleClangEntry(entry)) continue;

    ValueDecl *decl = nullptr;
    // If it's a Clang declaration, try to import it.
    if (auto clangDecl = entry.dyn_cast<clang::NamedDecl *>()) {
      bool isNamespace = isa<clang::NamespaceDecl>(clangDecl);
      Decl *realDecl =
          importDeclReal(clangDecl->getMostRecentDecl(), CurrentVersion,
                         /*useCanonicalDecl*/ !isNamespace);

      if (!realDecl)
        continue;
      decl = cast<ValueDecl>(realDecl);
      if (!decl) continue;
    } else if (!name.isSpecial()) {
      // Try to import a macro.
      if (auto modMacro = entry.dyn_cast<clang::ModuleMacro *>())
        decl = importMacro(name.getBaseIdentifier(), modMacro);
      else if (auto clangMacro = entry.dyn_cast<clang::MacroInfo *>())
        decl = importMacro(name.getBaseIdentifier(), clangMacro);
      else
        llvm_unreachable("new kind of lookup table entry");
      if (!decl) continue;
    } else {
      continue;
    }

    // If we found a declaration from the standard library, make sure
    // it does not show up in the lookup results for the imported
    // module.
    if (decl->getDeclContext()->isModuleScopeContext() &&
        decl->getModuleContext() == getStdlibModule())
      continue;

    // If the name matched, report this result.
    bool anyMatching = false;

    // Use the base name for operators; they likely won't have parameters.
    auto foundDeclName = decl->getName();
    if (foundDeclName.isOperator())
      foundDeclName = foundDeclName.getBaseName();

    if (foundDeclName.matchesRef(name) &&
        decl->getDeclContext()->isModuleScopeContext()) {
      consumer.foundDecl(decl, DeclVisibilityKind::VisibleAtTopLevel);
      anyMatching = true;
    }

    // If there is an alternate declaration and the name matches,
    // report this result.
    for (auto alternate : getAlternateDecls(decl)) {
      if (alternate->getName().matchesRef(name) &&
          alternate->getDeclContext()->isModuleScopeContext()) {
        consumer.foundDecl(alternate, DeclVisibilityKind::VisibleAtTopLevel);
        anyMatching = true;
      }
    }

    // Visit auxiliary declarations to check for name matches.
    decl->visitAuxiliaryDecls([&](Decl *aux) {
      if (auto auxValue = dyn_cast<ValueDecl>(aux)) {
        if (auxValue->getName().matchesRef(name) &&
            auxValue->getDeclContext()->isModuleScopeContext()) {
          consumer.foundDecl(auxValue, DeclVisibilityKind::VisibleAtTopLevel);
          anyMatching = true;
        }
      }
    });

    // If we have a declaration and nothing matched so far, try the names used
    // in other versions of Swift.
    if (auto clangDecl = entry.dyn_cast<clang::NamedDecl *>()) {
      const clang::NamedDecl *recentClangDecl =
          clangDecl->getMostRecentDecl();

      CurrentVersion.forEachOtherImportNameVersion(
          [&](ImportNameVersion nameVersion) {
        if (anyMatching)
          return;

        // Check to see if the name and context match what we expect.
        ImportedName newName = importFullName(recentClangDecl, nameVersion);
        if (!newName.getDeclName().matchesRef(name))
          return;

        // If we asked for an async import and didn't find one, skip this.
        // This filters out duplicates.
        if (nameVersion.supportsConcurrency() &&
            !newName.getAsyncInfo())
          return;

        const clang::DeclContext *clangDC =
            newName.getEffectiveContext().getAsDeclContext();
        if (!clangDC || !clangDC->isFileContext())
          return;

        // Then try to import the decl under the alternate name.
        auto alternateNamedDecl =
            cast_or_null<ValueDecl>(importDeclReal(recentClangDecl,
                                                   nameVersion));
        if (!alternateNamedDecl || alternateNamedDecl == decl)
          return;
        assert(alternateNamedDecl->getName().matchesRef(name) &&
               "importFullName behaved differently from importDecl");
        if (alternateNamedDecl->getDeclContext()->isModuleScopeContext()) {
          consumer.foundDecl(alternateNamedDecl,
                             DeclVisibilityKind::VisibleAtTopLevel);
          anyMatching = true;
        }
      });
    }
    declFound = declFound || anyMatching;
  }
  return declFound;
}

void ClangImporter::Implementation::lookupVisibleDecls(
       SwiftLookupTable &table,
       VisibleDeclConsumer &consumer) {
  // Retrieve and sort all of the base names in this particular table.
  auto baseNames = table.allBaseNames();
  llvm::array_pod_sort(baseNames.begin(), baseNames.end());

  // Look for namespace-scope entities with each base name.
  for (auto baseName : baseNames) {
    DeclBaseName name = baseName.toDeclBaseName(SwiftContext);
    if (!lookupValue(table, name, consumer) &&
        SwiftContext.LangOpts.EnableExperimentalEagerClangModuleDiagnostics) {
      diagnoseTopLevelValue(name);
    }
  }
}

void ClangImporter::Implementation::lookupObjCMembers(
       SwiftLookupTable &table,
       DeclName name,
       VisibleDeclConsumer &consumer) {
  for (auto clangDecl : table.lookupObjCMembers(name.getBaseName())) {
    // If the entry is not visible, skip it.
    if (!isVisibleClangEntry(clangDecl)) continue;

    forEachDistinctName(clangDecl,
                        [&](ImportedName importedName,
                            ImportNameVersion nameVersion) -> bool {
      // Import the declaration.
      auto decl =
          cast_or_null<ValueDecl>(importDeclReal(clangDecl, nameVersion));
      if (!decl)
        return false;

      // If the name we found matches, report the declaration.
      // FIXME: If we didn't need to check alternate decls here, we could avoid
      // importing the member at all by checking importedName ahead of time.
      if (decl->getName().matchesRef(name)) {
        consumer.foundDecl(decl, DeclVisibilityKind::DynamicLookup,
                           DynamicLookupInfo::AnyObject);
      }

      // Check for an alternate declaration; if its name matches,
      // report it.
      for (auto alternate : getAlternateDecls(decl)) {
        if (alternate->getName().matchesRef(name)) {
          consumer.foundDecl(alternate, DeclVisibilityKind::DynamicLookup,
                             DynamicLookupInfo::AnyObject);
        }
      }
      return true;
    });
  }
}

void ClangImporter::Implementation::lookupAllObjCMembers(
       SwiftLookupTable &table,
       VisibleDeclConsumer &consumer) {
  // Retrieve and sort all of the base names in this particular table.
  auto baseNames = table.allBaseNames();
  llvm::array_pod_sort(baseNames.begin(), baseNames.end());

  // Look for Objective-C members with each base name.
  for (auto baseName : baseNames) {
    lookupObjCMembers(table, baseName.toDeclBaseName(SwiftContext), consumer);
  }
}

void ClangImporter::Implementation::diagnoseTopLevelValue(
    const DeclName &name) {
  forEachLookupTable([&](SwiftLookupTable &table) -> bool {
    for (const auto &entry :
         table.lookup(name.getBaseName(),
                      EffectiveClangContext(
                          getClangASTContext().getTranslationUnitDecl()))) {
      diagnoseTargetDirectly(importDiagnosticTargetFromLookupTableEntry(entry));
    }
    return false;
  });
}

void ClangImporter::Implementation::diagnoseMemberValue(
    const DeclName &name, const clang::DeclContext *container) {
  forEachLookupTable([&](SwiftLookupTable &table) -> bool {
    for (const auto &entry :
         table.lookup(name.getBaseName(), EffectiveClangContext(container))) {
      if (clang::NamedDecl *nd = entry.get<clang::NamedDecl *>()) {
        // We are only interested in members of a particular context,
        // skip other contexts.
        if (nd->getDeclContext() != container)
          continue;

        diagnoseTargetDirectly(
            importDiagnosticTargetFromLookupTableEntry(entry));
      }
      // If the entry is not a NamedDecl, it is a form of macro, which cannot be
      // a member value.
    }
    return false;
  });
}

void ClangImporter::Implementation::diagnoseTargetDirectly(
    ImportDiagnosticTarget target) {
  if (const clang::Decl *decl = target.dyn_cast<const clang::Decl *>()) {
    Walker.TraverseDecl(const_cast<clang::Decl *>(decl));
  } else if (const clang::MacroInfo *macro =
                 target.dyn_cast<const clang::MacroInfo *>()) {
    Walker.VisitMacro(macro);
  }
}

ImportDiagnosticTarget
ClangImporter::Implementation::importDiagnosticTargetFromLookupTableEntry(
    SwiftLookupTable::SingleEntry entry) {
  if (clang::NamedDecl *decl = entry.dyn_cast<clang::NamedDecl *>()) {
    return decl;
  } else if (const clang::MacroInfo *macro =
                 entry.dyn_cast<clang::MacroInfo *>()) {
    return macro;
  } else if (const clang::ModuleMacro *macro =
                 entry.dyn_cast<clang::ModuleMacro *>()) {
    return macro->getMacroInfo();
  }
  llvm_unreachable("SwiftLookupTable::Single entry must be a NamedDecl, "
                   "MacroInfo or ModuleMacro pointer");
}

static void diagnoseForeignReferenceTypeFixit(ClangImporter::Implementation &Impl,
                                              HeaderLoc loc, Diagnostic diag) {
  auto importedLoc =
    Impl.SwiftContext.getClangModuleLoader()->importSourceLocation(loc.clangLoc);
  Impl.diagnose(loc, diag).fixItInsert(
      importedLoc, "SWIFT_SHARED_REFERENCE(<#retain#>, <#release#>) ");
}

bool ClangImporter::Implementation::emitDiagnosticsForTarget(
    ImportDiagnosticTarget target, clang::SourceLocation fallbackLoc) {
  for (auto it = ImportDiagnostics[target].rbegin();
       it != ImportDiagnostics[target].rend(); ++it) {
    HeaderLoc loc = HeaderLoc(it->loc.isValid() ? it->loc : fallbackLoc);
    if (it->diag.getID() == diag::record_not_automatically_importable.ID) {
      diagnoseForeignReferenceTypeFixit(*this, loc, it->diag);
    } else {
      diagnose(loc, it->diag);
    }
  }
  return ImportDiagnostics[target].size();
}

static SmallVector<SwiftLookupTable::SingleEntry, 4>
lookupInClassTemplateSpecialization(
    ASTContext &ctx, const clang::ClassTemplateSpecializationDecl *clangDecl,
    DeclName name) {
  // TODO: we could make this faster if we can cache class templates in the
  // lookup table as well.
  // Import all the names to figure out which ones we're looking for.
  SmallVector<SwiftLookupTable::SingleEntry, 4> found;
  for (auto member : clangDecl->decls()) {
    auto namedDecl = dyn_cast<clang::NamedDecl>(member);
    if (!namedDecl)
      continue;

    auto memberName = ctx.getClangModuleLoader()->importName(namedDecl);
    if (!memberName)
      continue;

    // Use the base names here because *sometimes* our input name won't have
    // any arguments.
    if (name.getBaseName().compare(memberName.getBaseName()) == 0)
      found.push_back(namedDecl);
  }

  return found;
}

static bool isDirectLookupMemberContext(const clang::Decl *foundClangDecl,
                                        const clang::Decl *memberContext,
                                        const clang::Decl *parent) {
  if (memberContext->getCanonicalDecl() == parent->getCanonicalDecl())
    return true;
  if (auto namespaceDecl = dyn_cast<clang::NamespaceDecl>(memberContext)) {
    if (namespaceDecl->isInline()) {
      if (auto memberCtxParent =
              dyn_cast<clang::Decl>(namespaceDecl->getParent()))
        return isDirectLookupMemberContext(foundClangDecl, memberCtxParent,
                                           parent);
    }
  }
  // Enum constant decl can be found in the parent context of the enum decl.
  if (auto *ED = dyn_cast<clang::EnumDecl>(memberContext)) {
    if (isa<clang::EnumConstantDecl>(foundClangDecl)) {
      if (auto *firstDecl = dyn_cast<clang::Decl>(ED->getDeclContext()))
        return firstDecl->getCanonicalDecl() == parent->getCanonicalDecl();
    }
  }
  return false;
}

SmallVector<SwiftLookupTable::SingleEntry, 4>
ClangDirectLookupRequest::evaluate(Evaluator &evaluator,
                                   ClangDirectLookupDescriptor desc) const {
  auto &ctx = desc.decl->getASTContext();
  auto *clangDecl = desc.clangDecl;
  // Class templates aren't in the lookup table.
  if (auto spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(clangDecl))
    return lookupInClassTemplateSpecialization(ctx, spec, desc.name);

  SwiftLookupTable *lookupTable = nullptr;
  if (isa<clang::NamespaceDecl>(clangDecl)) {
    // DeclContext of a namespace imported into Swift is the __ObjC module.
    lookupTable = ctx.getClangModuleLoader()->findLookupTable(nullptr);
  } else {
    auto *clangModule =
        getClangOwningModule(clangDecl, clangDecl->getASTContext());
    lookupTable = ctx.getClangModuleLoader()->findLookupTable(clangModule);
  }

  auto foundDecls = lookupTable->lookup(
      SerializedSwiftName(desc.name.getBaseName()), EffectiveClangContext());
  // Make sure that `clangDecl` is the parent of all the members we found.
  SmallVector<SwiftLookupTable::SingleEntry, 4> filteredDecls;
  llvm::copy_if(foundDecls, std::back_inserter(filteredDecls),
                [clangDecl](SwiftLookupTable::SingleEntry decl) {
                  auto foundClangDecl = decl.dyn_cast<clang::NamedDecl *>();
                  if (!foundClangDecl)
                    return false;
                  auto first = foundClangDecl->getDeclContext();
                  auto second = cast<clang::DeclContext>(clangDecl);
                  if (auto firstDecl = dyn_cast<clang::Decl>(first)) {
                    if (auto secondDecl = dyn_cast<clang::Decl>(second))
                      return isDirectLookupMemberContext(foundClangDecl,
                                                         firstDecl, secondDecl);
                    else
                      return false;
                  }
                  return first == second;
                });
  return filteredDecls;
}

TinyPtrVector<ValueDecl *> CXXNamespaceMemberLookup::evaluate(
    Evaluator &evaluator, CXXNamespaceMemberLookupDescriptor desc) const {
  EnumDecl *namespaceDecl = desc.namespaceDecl;
  DeclName name = desc.name;
  auto *clangNamespaceDecl =
      cast<clang::NamespaceDecl>(namespaceDecl->getClangDecl());
  auto &ctx = namespaceDecl->getASTContext();

  TinyPtrVector<ValueDecl *> result;
  llvm::SmallPtrSet<clang::NamedDecl *, 8> importedDecls;
  for (auto redecl : clangNamespaceDecl->redecls()) {
    auto allResults = evaluateOrDefault(
        ctx.evaluator, ClangDirectLookupRequest({namespaceDecl, redecl, name}),
        {});

    for (auto found : allResults) {
      auto clangMember = found.get<clang::NamedDecl *>();
      auto it = importedDecls.insert(clangMember);
      // Skip over members already found during lookup in
      // prior redeclarations.
      if (!it.second)
        continue;
      if (auto import =
              ctx.getClangModuleLoader()->importDeclDirectly(clangMember))
        result.push_back(cast<ValueDecl>(import));
    }
  }

  return result;
}

CxxEscapability
ClangTypeEscapability::evaluate(Evaluator &evaluator,
                                EscapabilityLookupDescriptor desc) const {
  auto desugared = desc.type->getUnqualifiedDesugaredType();
  if (const auto *recordType = desugared->getAs<clang::RecordType>()) {
    if (importer::hasNonEscapableAttr(recordType->getDecl()))
      return CxxEscapability::NonEscapable;
    if (importer::hasEscapableAttr(recordType->getDecl()))
      return CxxEscapability::Escapable;
    auto recordDecl = recordType->getDecl();
    auto cxxRecordDecl = dyn_cast<clang::CXXRecordDecl>(recordDecl);
    if (!cxxRecordDecl || cxxRecordDecl->isAggregate()) {
      bool hadUnknown = false;
      auto evaluateEscapability = [&](const clang::Type *type) {
        auto escapability = evaluateOrDefault(
            evaluator, ClangTypeEscapability({type}), CxxEscapability::Unknown);
        if (escapability == CxxEscapability::Unknown)
          hadUnknown = true;
        return escapability;
      };

      if (cxxRecordDecl) {
        for (auto base : cxxRecordDecl->bases()) {
          auto baseEscapability = evaluateEscapability(
              base.getType()->getUnqualifiedDesugaredType());
          if (baseEscapability == CxxEscapability::NonEscapable)
            return CxxEscapability::NonEscapable;
        }
      }

      for (auto field : recordDecl->fields()) {
        auto fieldEscapability = evaluateEscapability(
            field->getType()->getUnqualifiedDesugaredType());
        if (fieldEscapability == CxxEscapability::NonEscapable)
          return CxxEscapability::NonEscapable;
      }

      return hadUnknown ? CxxEscapability::Unknown : CxxEscapability::Escapable;
    }
  }
  if (desugared->isArrayType()) {
    auto elemTy = cast<clang::ArrayType>(desugared)
                      ->getElementType()
                      ->getUnqualifiedDesugaredType();
    return evaluateOrDefault(evaluator, ClangTypeEscapability({elemTy}),
                             CxxEscapability::Unknown);
  }

  // Base cases
  if (desugared->isAnyPointerType() || desugared->isBlockPointerType() ||
      desugared->isMemberPointerType() || desugared->isReferenceType())
    return CxxEscapability::NonEscapable;
  if (desugared->isScalarType())
    return CxxEscapability::Escapable;
  return CxxEscapability::Unknown;
}

void swift::simple_display(llvm::raw_ostream &out,
                           EscapabilityLookupDescriptor desc) {
  out << "Computing escapability for type '";
  out << clang::QualType(desc.type, 0).getAsString();
  out << "'";
}

SourceLoc swift::extractNearestSourceLoc(EscapabilityLookupDescriptor) {
  return SourceLoc();
}

// Just create a specialized function decl for "__swift_interopStaticCast"
// using the types base and derived.
static
DeclRefExpr *getInteropStaticCastDeclRefExpr(ASTContext &ctx,
                                             const clang::Module *owningModule,
                                             Type base, Type derived) {
  if (base->isForeignReferenceType() && derived->isForeignReferenceType()) {
    base = base->wrapInPointer(PTK_UnsafePointer);
    derived = derived->wrapInPointer(PTK_UnsafePointer);
  }

  // Lookup our static cast helper function in the C++ shim module.
  auto wrapperModule = ctx.getLoadedModule(ctx.getIdentifier(CXX_SHIM_NAME));
  assert(wrapperModule &&
         "CxxShim module is required when using members of a base class. "
         "Make sure you `import CxxShim`.");

  SmallVector<ValueDecl *, 1> results;
  ctx.lookupInModule(wrapperModule, "__swift_interopStaticCast", results);
  assert(
      results.size() == 1 &&
      "Did you forget to define a __swift_interopStaticCast helper function?");
  FuncDecl *staticCastFn = cast<FuncDecl>(results.back());

  // Now we have to force instantiate this. We can't let the type checker do
  // this yet because it can't infer the "To" type.
  auto subst =
      SubstitutionMap::get(staticCastFn->getGenericSignature(), {derived, base},
                           LookUpConformanceInModule());
  auto functionTemplate = const_cast<clang::FunctionTemplateDecl *>(
      cast<clang::FunctionTemplateDecl>(staticCastFn->getClangDecl()));
  auto spec = ctx.getClangModuleLoader()->instantiateCXXFunctionTemplate(
      ctx, functionTemplate, subst);
  auto specializedStaticCastFn =
      cast<FuncDecl>(ctx.getClangModuleLoader()->importDeclDirectly(spec));

  auto staticCastRefExpr = new (ctx)
      DeclRefExpr(ConcreteDeclRef(specializedStaticCastFn), DeclNameLoc(),
                  /*implicit*/ true);
  staticCastRefExpr->setType(specializedStaticCastFn->getInterfaceType());

  return staticCastRefExpr;
}

// Create the following expressions:
// %0 = Builtin.addressof(&self)
// %1 = Builtin.reinterpretCast<UnsafeMutablePointer<Derived>>(%0)
// %2 = __swift_interopStaticCast<UnsafeMutablePointer<Base>?>(%1)
// %3 = %2!
// return %3.pointee
static
MemberRefExpr *getSelfInteropStaticCast(FuncDecl *funcDecl,
                                        NominalTypeDecl *baseStruct,
                                        NominalTypeDecl *derivedStruct) {
  auto &ctx = funcDecl->getASTContext();

  auto mutableSelf = [&ctx](FuncDecl *funcDecl) {
    auto selfDecl = funcDecl->getImplicitSelfDecl();

    auto selfRef =
        new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(), /*implicit*/ true);
    selfRef->setType(LValueType::get(selfDecl->getInterfaceType()));

    return selfRef;
  }(funcDecl);

  auto createCallToBuiltin = [&](Identifier name, ArrayRef<Type> substTypes,
                                 Argument arg) {
    auto builtinFn = cast<FuncDecl>(getBuiltinValueDecl(ctx, name));
    auto substMap =
        SubstitutionMap::get(builtinFn->getGenericSignature(), substTypes,
                             LookUpConformanceInModule());
    ConcreteDeclRef builtinFnRef(builtinFn, substMap);
    auto builtinFnRefExpr =
        new (ctx) DeclRefExpr(builtinFnRef, DeclNameLoc(), /*implicit*/ true);

    auto fnType = builtinFn->getInterfaceType();
    if (auto genericFnType = dyn_cast<GenericFunctionType>(fnType.getPointer()))
      fnType = genericFnType->substGenericArgs(substMap);
    builtinFnRefExpr->setType(fnType);
    auto *argList = ArgumentList::createImplicit(ctx, {arg});
    auto callExpr = CallExpr::create(ctx, builtinFnRefExpr, argList, /*implicit*/ true);
    callExpr->setThrows(nullptr);
    return callExpr;
  };

  auto rawSelfPointer = createCallToBuiltin(
      ctx.getIdentifier("addressof"), {derivedStruct->getSelfInterfaceType()},
      Argument::implicitInOut(ctx, mutableSelf));
  rawSelfPointer->setType(ctx.TheRawPointerType);

  auto derivedPtrType = derivedStruct->getSelfInterfaceType()->wrapInPointer(
      PTK_UnsafeMutablePointer);
  auto selfPointer =
      createCallToBuiltin(ctx.getIdentifier("reinterpretCast"),
                          {ctx.TheRawPointerType, derivedPtrType},
                          Argument::unlabeled(rawSelfPointer));
  selfPointer->setType(derivedPtrType);

  auto staticCastRefExpr = getInteropStaticCastDeclRefExpr(
      ctx, baseStruct->getClangDecl()->getOwningModule(),
      baseStruct->getSelfInterfaceType()->wrapInPointer(
          PTK_UnsafeMutablePointer),
      derivedStruct->getSelfInterfaceType()->wrapInPointer(
          PTK_UnsafeMutablePointer));
  auto *argList = ArgumentList::forImplicitUnlabeled(ctx, {selfPointer});
  auto casted = CallExpr::createImplicit(ctx, staticCastRefExpr, argList);
  // This will be "Optional<UnsafeMutablePointer<Base>>"
  casted->setType(cast<FunctionType>(staticCastRefExpr->getType().getPointer())
                      ->getResult());
  casted->setThrows(nullptr);

  SubstitutionMap pointeeSubst = SubstitutionMap::get(
      ctx.getUnsafeMutablePointerDecl()->getGenericSignature(),
      {baseStruct->getSelfInterfaceType()},
      LookUpConformanceInModule());
  VarDecl *pointeePropertyDecl =
      ctx.getPointerPointeePropertyDecl(PTK_UnsafeMutablePointer);
  auto pointeePropertyRefExpr = new (ctx) MemberRefExpr(
      casted, SourceLoc(),
      ConcreteDeclRef(pointeePropertyDecl, pointeeSubst), DeclNameLoc(),
      /*implicit=*/true);
  pointeePropertyRefExpr->setType(
      LValueType::get(baseStruct->getSelfInterfaceType()));

  return pointeePropertyRefExpr;
}

// Find the base C++ method called by the base function we want to synthesize
// the derived thunk for.
// The base C++ method is either the original C++ method that corresponds
// to the imported base member, or it's the synthesized C++ method thunk
// used in another synthesized derived thunk that acts as a base member here.
const clang::CXXMethodDecl *getCalledBaseCxxMethod(FuncDecl *baseMember) {
  if (baseMember->getClangDecl())
    return dyn_cast<clang::CXXMethodDecl>(baseMember->getClangDecl());
  // Another synthesized derived thunk is used as a base member here,
  // so extract its synthesized C++ method.
  auto body = baseMember->getBody();
  if (body->getElements().empty())
    return nullptr;
  ReturnStmt *returnStmt = dyn_cast_or_null<ReturnStmt>(
      body->getElements().front().dyn_cast<Stmt *>());
  if (!returnStmt)
    return nullptr;
  Expr *returnExpr = returnStmt->getResult();
  // Look through a potential 'reinterpretCast' that can be used
  // to cast UnsafeMutablePointer to UnsafePointer in the synthesized
  // Swift body for `.pointee`.
  if (auto *ce = dyn_cast<CallExpr>(returnExpr)) {
    if (auto *v = ce->getCalledValue()) {
      if (v->getModuleContext() ==
              baseMember->getASTContext().TheBuiltinModule &&
          v->getBaseName().userFacingName() == "reinterpretCast") {
        returnExpr = ce->getArgs()->get(0).getExpr();
      }
    }
  }
  // A member ref expr for `.pointee` access can be wrapping a call
  // when looking through the synthesized Swift body for `.pointee`
  // accessor.
  if (MemberRefExpr *mre = dyn_cast<MemberRefExpr>(returnExpr))
    returnExpr = mre->getBase();
  auto *callExpr = dyn_cast<CallExpr>(returnExpr);
  if (!callExpr)
    return nullptr;
  auto *cv = callExpr->getCalledValue();
  if (!cv)
    return nullptr;
  if (!cv->getClangDecl())
    return nullptr;
  return dyn_cast<clang::CXXMethodDecl>(cv->getClangDecl());
}

// Construct a Swift method that represents the synthesized C++ method
// that invokes the base C++ method.
FuncDecl *synthesizeBaseFunctionDeclCall(ClangImporter &impl, ASTContext &ctx,
                                         NominalTypeDecl *derivedStruct,
                                         NominalTypeDecl *baseStruct,
                                         FuncDecl *baseMember) {
  auto *cxxMethod = getCalledBaseCxxMethod(baseMember);
  if (!cxxMethod)
    return nullptr;
  auto *newClangMethod =
      SwiftDeclSynthesizer(&impl).synthesizeCXXForwardingMethod(
          cast<clang::CXXRecordDecl>(derivedStruct->getClangDecl()),
          cast<clang::CXXRecordDecl>(baseStruct->getClangDecl()), cxxMethod,
          ForwardingMethodKind::Base);
  if (!newClangMethod)
    return nullptr;
  return cast_or_null<FuncDecl>(
      ctx.getClangModuleLoader()->importDeclDirectly(newClangMethod));
}

// Generates the body of a derived method, that invokes the base
// method.
// The method's body takes the following form:
//   return self.__synthesizedBaseCall_fn(args...)
static std::pair<BraceStmt *, bool>
synthesizeBaseClassMethodBody(AbstractFunctionDecl *afd, void *context) {

  ASTContext &ctx = afd->getASTContext();

  auto funcDecl = cast<FuncDecl>(afd);
  auto derivedStruct =
      cast<NominalTypeDecl>(funcDecl->getDeclContext()->getAsDecl());
  auto baseMember = static_cast<FuncDecl *>(context);
  auto baseStruct =
      cast<NominalTypeDecl>(baseMember->getDeclContext()->getAsDecl());

  auto forwardedFunc = synthesizeBaseFunctionDeclCall(
      *static_cast<ClangImporter *>(ctx.getClangModuleLoader()), ctx,
      derivedStruct, baseStruct, baseMember);
  if (!forwardedFunc) {
    ctx.Diags.diagnose(SourceLoc(), diag::failed_base_method_call_synthesis,
                         funcDecl, baseStruct);
    auto body = BraceStmt::create(ctx, SourceLoc(), {}, SourceLoc(),
                                  /*implicit=*/true);
    return {body, /*isTypeChecked=*/true};
  }

  SmallVector<Expr *, 8> forwardingParams;
  for (auto param : *funcDecl->getParameters()) {
    auto paramRefExpr = new (ctx) DeclRefExpr(param, DeclNameLoc(),
                                              /*Implicit=*/true);
    paramRefExpr->setType(param->getTypeInContext());
    forwardingParams.push_back(paramRefExpr);
  }

  Argument selfArg = [&]() {
    auto *selfDecl = funcDecl->getImplicitSelfDecl();
    auto selfExpr = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                          /*implicit*/ true);
    if (funcDecl->isMutating()) {
      selfExpr->setType(LValueType::get(selfDecl->getInterfaceType()));
      return Argument::implicitInOut(ctx, selfExpr);
    }
    selfExpr->setType(selfDecl->getTypeInContext());
    return Argument::unlabeled(selfExpr);
  }();

  auto *baseMemberExpr =
      new (ctx) DeclRefExpr(ConcreteDeclRef(forwardedFunc), DeclNameLoc(),
                            /*Implicit=*/true);
  baseMemberExpr->setType(forwardedFunc->getInterfaceType());

  auto baseMemberDotCallExpr =
      DotSyntaxCallExpr::create(ctx, baseMemberExpr, SourceLoc(), selfArg);
  baseMemberDotCallExpr->setType(baseMember->getMethodInterfaceType());
  baseMemberDotCallExpr->setThrows(nullptr);

  auto *argList = ArgumentList::forImplicitUnlabeled(ctx, forwardingParams);
  auto *baseMemberCallExpr = CallExpr::createImplicit(
      ctx, baseMemberDotCallExpr, argList);
  baseMemberCallExpr->setType(baseMember->getResultInterfaceType());
  baseMemberCallExpr->setThrows(nullptr);

  auto *returnStmt = ReturnStmt::createImplicit(ctx, baseMemberCallExpr);

  auto body = BraceStmt::create(ctx, SourceLoc(), {returnStmt}, SourceLoc(),
                                /*implicit=*/true);
  return {body, /*isTypeChecked=*/true};
}

// How should the synthesized C++ method that returns the field of interest
// from the base class should return the value - by value, or by reference.
enum ReferenceReturnTypeBehaviorForBaseAccessorSynthesis {
  ReturnByValue,
  ReturnByReference,
  ReturnByMutableReference
};

// Synthesize a C++ method that returns the field of interest from the base
// class. This lets Clang take care of the cast from the derived class
// to the base class while the field is accessed.
static clang::CXXMethodDecl *synthesizeCxxBaseGetterAccessorMethod(
    ClangImporter &impl, const clang::CXXRecordDecl *derivedClass,
    const clang::CXXRecordDecl *baseClass, const clang::FieldDecl *field,
    ValueDecl *retainOperationFn,
    ReferenceReturnTypeBehaviorForBaseAccessorSynthesis behavior) {
  auto &clangCtx = impl.getClangASTContext();
  auto &clangSema = impl.getClangSema();

  // Create a new method in the derived class that calls the base method.
  auto name = field->getDeclName();
  if (name.isIdentifier()) {
    std::string newName;
    llvm::raw_string_ostream os(newName);
    os << (behavior == ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::
                           ReturnByMutableReference
               ? "__synthesizedBaseSetterAccessor_"
               : "__synthesizedBaseGetterAccessor_")
       << name.getAsIdentifierInfo()->getName();
    name = clang::DeclarationName(
        &impl.getClangPreprocessor().getIdentifierTable().get(os.str()));
  }
  auto returnType = field->getType();
  if (returnType->isReferenceType())
    returnType = returnType->getPointeeType();
  auto valueReturnType = returnType;
  if (behavior !=
      ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::ReturnByValue) {
    returnType = clangCtx.getRValueReferenceType(
        behavior == ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::
                        ReturnByReference
            ? returnType.withConst()
            : returnType);
  }
  clang::FunctionProtoType::ExtProtoInfo info;
  if (behavior != ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::
                      ReturnByMutableReference)
    info.TypeQuals.addConst();
  info.ExceptionSpec.Type = clang::EST_NoThrow;
  auto ftype = clangCtx.getFunctionType(returnType, {}, info);
  auto newMethod = clang::CXXMethodDecl::Create(
      clangCtx, const_cast<clang::CXXRecordDecl *>(derivedClass),
      field->getSourceRange().getBegin(),
      clang::DeclarationNameInfo(name, clang::SourceLocation()), ftype,
      clangCtx.getTrivialTypeSourceInfo(ftype), clang::SC_None,
      /*UsesFPIntrin=*/false, /*isInline=*/true,
      clang::ConstexprSpecKind::Unspecified, field->getSourceRange().getEnd());
  newMethod->setImplicit();
  newMethod->setImplicitlyInline();
  newMethod->setAccess(clang::AccessSpecifier::AS_public);
  if (retainOperationFn) {
    // Return an FRT field at +1.
    newMethod->addAttr(clang::CFReturnsRetainedAttr::CreateImplicit(clangCtx));
  }

  // Create a new Clang diagnostic pool to capture any diagnostics
  // emitted during the construction of the method.
  clang::sema::DelayedDiagnosticPool diagPool{
      clangSema.DelayedDiagnostics.getCurrentPool()};
  auto diagState = clangSema.DelayedDiagnostics.push(diagPool);

  // Returns the expression that accesses the base field from derived type.
  auto createFieldAccess = [&]() -> clang::Expr * {
    auto *thisExpr = clang::CXXThisExpr::Create(
        clangCtx, clang::SourceLocation(), newMethod->getThisType(),
        /*IsImplicit=*/false);
    clang::QualType baseClassPtr = clangCtx.getRecordType(baseClass);
    baseClassPtr.addConst();
    baseClassPtr = clangCtx.getPointerType(baseClassPtr);

    clang::CastKind Kind;
    clang::CXXCastPath Path;
    clangSema.CheckPointerConversion(thisExpr, baseClassPtr, Kind, Path,
                                     /*IgnoreBaseAccess=*/false,
                                     /*Diagnose=*/true);
    auto conv = clangSema.ImpCastExprToType(thisExpr, baseClassPtr, Kind,
                                            clang::VK_PRValue, &Path);
    if (!conv.isUsable())
      return nullptr;
    auto memberExpr = clangSema.BuildMemberExpr(
        conv.get(), /*isArrow=*/true, clang::SourceLocation(),
        clang::NestedNameSpecifierLoc(), clang::SourceLocation(),
        const_cast<clang::FieldDecl *>(field),
        clang::DeclAccessPair::make(const_cast<clang::FieldDecl *>(field),
                                    clang::AS_public),
        /*HadMultipleCandidates=*/false,
        clang::DeclarationNameInfo(field->getDeclName(),
                                   clang::SourceLocation()),
        valueReturnType, clang::VK_LValue, clang::OK_Ordinary);
    auto returnCast = clangSema.ImpCastExprToType(memberExpr, valueReturnType,
                                                  clang::CK_LValueToRValue,
                                                  clang::VK_PRValue);
    if (!returnCast.isUsable())
      return nullptr;
    return returnCast.get();
  };

  llvm::SmallVector<clang::Stmt *, 2> body;
  if (retainOperationFn) {
    // Check if the returned value needs to be retained. This might occur if the
    // field getter is returning a shared reference type using, as it needs to
    // perform the retain to match the expected @owned convention.
    auto *retainClangFn =
        dyn_cast<clang::FunctionDecl>(retainOperationFn->getClangDecl());
    if (!retainClangFn) {
      return nullptr;
    }
    auto *fnRef = new (clangCtx) clang::DeclRefExpr(
        clangCtx, const_cast<clang::FunctionDecl *>(retainClangFn), false,
        retainClangFn->getType(), clang::ExprValueKind::VK_LValue,
        clang::SourceLocation());
    auto fieldExpr = createFieldAccess();
    if (!fieldExpr)
      return nullptr;
    auto retainCall = clangSema.BuildResolvedCallExpr(
        fnRef, const_cast<clang::FunctionDecl *>(retainClangFn),
        clang::SourceLocation(), {fieldExpr}, clang::SourceLocation());
    if (!retainCall.isUsable())
      return nullptr;
    body.push_back(retainCall.get());
  }

  // Construct the method's body.
  auto fieldExpr = createFieldAccess();
  if (!fieldExpr)
    return nullptr;
  auto returnStmt = clang::ReturnStmt::Create(clangCtx, clang::SourceLocation(),
                                              fieldExpr, nullptr);
  body.push_back(returnStmt);

  // Check if there were any Clang errors during the construction
  // of the method body.
  clangSema.DelayedDiagnostics.popWithoutEmitting(diagState);
  if (!diagPool.empty())
    return nullptr;
  newMethod->setBody(body.size() > 1
                         ? clang::CompoundStmt::Create(
                               clangCtx, body, clang::FPOptionsOverride(),
                               clang::SourceLocation(), clang::SourceLocation())
                         : body[0]);
  return newMethod;
}

// Generates the body of a derived method, that invokes the base
// field getter or the base subscript.
// The method's body takes the following form:
//   return self.__synthesizedBaseCall_fn(args...)
static std::pair<BraceStmt *, bool>
synthesizeBaseClassFieldGetterOrAddressGetterBody(AbstractFunctionDecl *afd,
                                                  void *context,
                                                  AccessorKind kind) {
  assert(kind == AccessorKind::Get || kind == AccessorKind::Address ||
         kind == AccessorKind::MutableAddress);
  ASTContext &ctx = afd->getASTContext();

  AccessorDecl *getterDecl = cast<AccessorDecl>(afd);
  AbstractStorageDecl *baseClassVar = static_cast<AbstractStorageDecl *>(context);
  NominalTypeDecl *baseStruct =
      cast<NominalTypeDecl>(baseClassVar->getDeclContext()->getAsDecl());
  NominalTypeDecl *derivedStruct =
      cast<NominalTypeDecl>(getterDecl->getDeclContext()->getAsDecl());

  const clang::Decl *baseClangDecl;
  if (baseClassVar->getClangDecl())
    baseClangDecl = baseClassVar->getClangDecl();
  else
    baseClangDecl = getCalledBaseCxxMethod(baseClassVar->getAccessor(kind));

  clang::CXXMethodDecl *baseGetterCxxMethod = nullptr;
  if (auto *md = dyn_cast_or_null<clang::CXXMethodDecl>(baseClangDecl)) {
    // Subscript operator, or `.pointee` wrapper is represented through a
    // generated C++ method call that calls the base operator.
    baseGetterCxxMethod =
        SwiftDeclSynthesizer(
            static_cast<ClangImporter *>(ctx.getClangModuleLoader()))
            .synthesizeCXXForwardingMethod(
                cast<clang::CXXRecordDecl>(derivedStruct->getClangDecl()),
                cast<clang::CXXRecordDecl>(baseStruct->getClangDecl()), md,
                ForwardingMethodKind::Base,
                getterDecl->getResultInterfaceType()->isForeignReferenceType()
                    ? ReferenceReturnTypeBehaviorForBaseMethodSynthesis::
                          RemoveReferenceIfPointer
                    : (kind != AccessorKind::Get
                           ? ReferenceReturnTypeBehaviorForBaseMethodSynthesis::
                                 KeepReference
                           : ReferenceReturnTypeBehaviorForBaseMethodSynthesis::
                                 RemoveReference),
                /*forceConstQualifier=*/kind != AccessorKind::MutableAddress);
  } else if (auto *fd = dyn_cast_or_null<clang::FieldDecl>(baseClangDecl)) {
    ValueDecl *retainOperationFn = nullptr;
    // Check if this field getter is returning a retainable FRT.
    if (getterDecl->getResultInterfaceType()->isForeignReferenceType()) {
      auto retainOperation = evaluateOrDefault(
          ctx.evaluator,
          CustomRefCountingOperation({getterDecl->getResultInterfaceType()
                                          ->lookThroughAllOptionalTypes()
                                          ->getClassOrBoundGenericClass(),
                                      CustomRefCountingOperationKind::retain}),
          {});
      if (retainOperation.kind ==
          CustomRefCountingOperationResult::foundOperation) {
        retainOperationFn = retainOperation.operation;
      }
    }
    // Field getter is represented through a generated
    // C++ method call that returns the value of the base field.
    baseGetterCxxMethod = synthesizeCxxBaseGetterAccessorMethod(
        *static_cast<ClangImporter *>(ctx.getClangModuleLoader()),
        cast<clang::CXXRecordDecl>(derivedStruct->getClangDecl()),
        cast<clang::CXXRecordDecl>(baseStruct->getClangDecl()), fd,
        retainOperationFn,
        kind == AccessorKind::Get
            ? ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::ReturnByValue
            : (kind == AccessorKind::Address
                   ? ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::
                         ReturnByReference
                   : ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::
                         ReturnByMutableReference));
  }

  if (!baseGetterCxxMethod) {
    ctx.Diags.diagnose(SourceLoc(), diag::failed_base_method_call_synthesis,
                       getterDecl, baseStruct);
    auto body = BraceStmt::create(ctx, SourceLoc(), {}, SourceLoc(),
                                  /*implicit=*/true);
    return {body, true};
  }
  auto *baseGetterMethod = cast<FuncDecl>(
      ctx.getClangModuleLoader()->importDeclDirectly(baseGetterCxxMethod));

  Argument selfArg = [&]() {
    auto selfDecl = getterDecl->getImplicitSelfDecl();
    auto selfExpr = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                          /*implicit*/ true);
    if (kind == AccessorKind::MutableAddress) {
      selfExpr->setType(LValueType::get(selfDecl->getInterfaceType()));
      return Argument::implicitInOut(ctx, selfExpr);
    }
    selfExpr->setType(selfDecl->getTypeInContext());
    return Argument::unlabeled(selfExpr);
  }();

  auto *baseMemberExpr =
      new (ctx) DeclRefExpr(ConcreteDeclRef(baseGetterMethod), DeclNameLoc(),
                            /*Implicit=*/true);
  baseMemberExpr->setType(baseGetterMethod->getInterfaceType());

  auto baseMemberDotCallExpr =
      DotSyntaxCallExpr::create(ctx, baseMemberExpr, SourceLoc(), selfArg);
  baseMemberDotCallExpr->setType(baseGetterMethod->getMethodInterfaceType());
  baseMemberDotCallExpr->setThrows(nullptr);

  ArgumentList *argumentList;
  if (auto subscript = dyn_cast<SubscriptDecl>(baseClassVar)) {
    auto paramDecl = getterDecl->getParameters()->get(0);
    auto paramRefExpr = new (ctx) DeclRefExpr(paramDecl, DeclNameLoc(),
                                              /*Implicit=*/true);
    paramRefExpr->setType(paramDecl->getTypeInContext());
    argumentList = ArgumentList::forImplicitUnlabeled(ctx, {paramRefExpr});
  } else {
    argumentList = ArgumentList::forImplicitUnlabeled(ctx, {});
  }

  auto *baseMemberCallExpr =
      CallExpr::createImplicit(ctx, baseMemberDotCallExpr, argumentList);
  Type resultType = baseGetterMethod->getResultInterfaceType();
  baseMemberCallExpr->setType(resultType);
  baseMemberCallExpr->setThrows(nullptr);

  Expr *returnExpr = baseMemberCallExpr;
  // Cast an 'address' result from a mutable pointer if needed.
  if (kind == AccessorKind::Address &&
      baseGetterMethod->getResultInterfaceType()->isUnsafeMutablePointer()) {
    auto finalResultType = getterDecl->getResultInterfaceType();
    returnExpr = SwiftDeclSynthesizer::synthesizeReturnReinterpretCast(
        ctx, baseGetterMethod->getResultInterfaceType(), finalResultType,
        returnExpr);
  }

  auto *returnStmt = ReturnStmt::createImplicit(ctx, returnExpr);

  auto body = BraceStmt::create(ctx, SourceLoc(), {returnStmt}, SourceLoc(),
                                /*implicit=*/true);
  return {body, /*isTypeChecked=*/true};
}

static std::pair<BraceStmt *, bool>
synthesizeBaseClassFieldGetterBody(AbstractFunctionDecl *afd, void *context) {
  return synthesizeBaseClassFieldGetterOrAddressGetterBody(afd, context,
                                                           AccessorKind::Get);
}

static std::pair<BraceStmt *, bool>
synthesizeBaseClassFieldAddressGetterBody(AbstractFunctionDecl *afd,
                                          void *context) {
  return synthesizeBaseClassFieldGetterOrAddressGetterBody(
      afd, context, AccessorKind::Address);
}

// For setters we have to pass self as a pointer and then emit an assign:
//   %0 = Builtin.addressof(&self)
//   %1 = Builtin.reinterpretCast<UnsafeMutablePointer<Derived>>(%0)
//   %2 = __swift_interopStaticCast<UnsafeMutablePointer<Base>?>(%1)
//   %3 = %2!
//   %4 = %3.pointee
//   assign newValue to %4
static std::pair<BraceStmt *, bool>
synthesizeBaseClassFieldSetterBody(AbstractFunctionDecl *afd, void *context) {
  auto setterDecl = cast<AccessorDecl>(afd);
  AbstractStorageDecl *baseClassVar = static_cast<AbstractStorageDecl *>(context);
  ASTContext &ctx = setterDecl->getASTContext();

  NominalTypeDecl *baseStruct =
      cast<NominalTypeDecl>(baseClassVar->getDeclContext()->getAsDecl());
  NominalTypeDecl *derivedStruct =
      cast<NominalTypeDecl>(setterDecl->getDeclContext()->getAsDecl());

  auto *pointeePropertyRefExpr =
      getSelfInteropStaticCast(setterDecl, baseStruct, derivedStruct);

  Expr *storedRef = nullptr;
  if (auto subscript = dyn_cast<SubscriptDecl>(baseClassVar)) {
    auto paramDecl = setterDecl->getParameters()->get(1);
    auto paramRefExpr = new (ctx) DeclRefExpr(paramDecl,
                                              DeclNameLoc(),
                                              /*Implicit=*/ true);
    paramRefExpr->setType(paramDecl->getTypeInContext());

    auto *argList = ArgumentList::forImplicitUnlabeled(ctx, {paramRefExpr});
    storedRef = SubscriptExpr::create(ctx, pointeePropertyRefExpr, argList, subscript);
    storedRef->setType(LValueType::get(subscript->getElementInterfaceType()));
  } else {
    // If the base class var has a clang decl, that means it's an access into a
    // stored field. Otherwise, we're looking into another base class, so it's a
    // another synthesized accessor.
    AccessSemantics accessKind = baseClassVar->getClangDecl()
                                     ? AccessSemantics::DirectToStorage
                                     : AccessSemantics::DirectToImplementation;

    storedRef =
        new (ctx) MemberRefExpr(pointeePropertyRefExpr, SourceLoc(), baseClassVar,
                                DeclNameLoc(), /*Implicit=*/true, accessKind);
    storedRef->setType(LValueType::get(cast<VarDecl>(baseClassVar)->getTypeInContext()));
  }

  auto newValueParamRefExpr =
      new (ctx) DeclRefExpr(setterDecl->getParameters()->get(0), DeclNameLoc(),
                            /*Implicit=*/true);
  newValueParamRefExpr->setType(setterDecl->getParameters()->get(0)->getTypeInContext());

  auto assignExpr =
      new (ctx) AssignExpr(storedRef, SourceLoc(), newValueParamRefExpr,
                           /*implicit*/ true);
  assignExpr->setType(TupleType::getEmpty(ctx));

  auto body = BraceStmt::create(ctx, SourceLoc(), {assignExpr}, SourceLoc(),
                                /*implicit*/ true);
  return {body, /*isTypeChecked=*/true};
}

static std::pair<BraceStmt *, bool>
synthesizeBaseClassFieldAddressSetterBody(AbstractFunctionDecl *afd,
                                          void *context) {
  return synthesizeBaseClassFieldGetterOrAddressGetterBody(
      afd, context, AccessorKind::MutableAddress);
}

static SmallVector<AccessorDecl *, 2>
makeBaseClassMemberAccessors(DeclContext *declContext,
                             AbstractStorageDecl *computedVar,
                             AbstractStorageDecl *baseClassVar) {
  auto &ctx = declContext->getASTContext();
  auto computedType = computedVar->getInterfaceType();
  auto contextTy = declContext->mapTypeIntoContext(computedType);

  // Use 'address' or 'mutableAddress' accessors for non-copyable
  // types, unless the base accessor returns it by value.
  bool useAddress = contextTy->isNoncopyable() &&
                    (baseClassVar->getReadImpl() == ReadImplKind::Stored ||
                     baseClassVar->getAccessor(AccessorKind::Address));

  ParameterList *bodyParams = nullptr;
  if (auto subscript = dyn_cast<SubscriptDecl>(baseClassVar)) {
    computedType = computedType->getAs<FunctionType>()->getResult();

    auto idxParam = subscript->getIndices()->get(0);
    bodyParams = ParameterList::create(ctx, { idxParam });
  } else {
    bodyParams = ParameterList::createEmpty(ctx);
  }

  auto getterDecl = AccessorDecl::create(
      ctx,
      /*FuncLoc=*/SourceLoc(),
      /*AccessorKeywordLoc=*/SourceLoc(),
      useAddress ? AccessorKind::Address : AccessorKind::Get, computedVar,
      /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
      /*Throws=*/false,
      /*ThrowsLoc=*/SourceLoc(), /*ThrownType=*/TypeLoc(), bodyParams,
      useAddress ? computedType->wrapInPointer(PTK_UnsafePointer)
                 : computedType,
      declContext);
  getterDecl->setIsTransparent(true);
  getterDecl->setAccess(AccessLevel::Public);
  getterDecl->setBodySynthesizer(useAddress
                                     ? synthesizeBaseClassFieldAddressGetterBody
                                     : synthesizeBaseClassFieldGetterBody,
                                 baseClassVar);
  if (baseClassVar->getWriteImpl() == WriteImplKind::Immutable)
    return {getterDecl};

  auto newValueParam =
      new (ctx) ParamDecl(SourceLoc(), SourceLoc(), Identifier(), SourceLoc(),
                          ctx.getIdentifier("newValue"), declContext);
  newValueParam->setSpecifier(ParamSpecifier::Default);
  newValueParam->setInterfaceType(computedType);

  SmallVector<ParamDecl *, 2> setterParamDecls;
  if (!useAddress)
    setterParamDecls.push_back(newValueParam);
  if (auto subscript = dyn_cast<SubscriptDecl>(baseClassVar))
    setterParamDecls.push_back(subscript->getIndices()->get(0));
  ParameterList *setterBodyParams =
      ParameterList::create(ctx, setterParamDecls);

  auto setterDecl = AccessorDecl::create(
      ctx,
      /*FuncLoc=*/SourceLoc(),
      /*AccessorKeywordLoc=*/SourceLoc(),
      useAddress ? AccessorKind::MutableAddress : AccessorKind::Set,
      computedVar,
      /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
      /*Throws=*/false,
      /*ThrowsLoc=*/SourceLoc(), /*ThrownType=*/TypeLoc(), setterBodyParams,
      useAddress ? computedType->wrapInPointer(PTK_UnsafeMutablePointer)
                 : TupleType::getEmpty(ctx),
      declContext);
  setterDecl->setIsTransparent(true);
  setterDecl->setAccess(AccessLevel::Public);
  setterDecl->setBodySynthesizer(useAddress
                                     ? synthesizeBaseClassFieldAddressSetterBody
                                     : synthesizeBaseClassFieldSetterBody,
                                 baseClassVar);
  setterDecl->setSelfAccessKind(SelfAccessKind::Mutating);

  return {getterDecl, setterDecl};
}

// Clone attributes that have been imported from Clang.
void cloneImportedAttributes(ValueDecl *fromDecl, ValueDecl* toDecl) {
  ASTContext& context = fromDecl->getASTContext();
  DeclAttributes& attrs = toDecl->getAttrs();
  for (auto attr : fromDecl->getAttrs()) {
    switch (attr->getKind()) {
    case DeclAttrKind::Available: {
      attrs.add(cast<AvailableAttr>(attr)->clone(context, true));
      break;
    }
    case DeclAttrKind::Custom: {
      CustomAttr *cAttr = cast<CustomAttr>(attr);
      attrs.add(CustomAttr::create(context, SourceLoc(), cAttr->getTypeExpr(),
                                   cAttr->getInitContext(), cAttr->getArgs(),
                                   true));
      break;
    }
    case DeclAttrKind::DiscardableResult: {
      attrs.add(new (context) DiscardableResultAttr(true));
      break;
    }
    case DeclAttrKind::Effects: {
      attrs.add(cast<EffectsAttr>(attr)->clone(context));
      break;
    }
    case DeclAttrKind::Final: {
      attrs.add(new (context) FinalAttr(true));
      break;
    }
    case DeclAttrKind::Transparent: {
      attrs.add(new (context) TransparentAttr(true));
      break;
    }
    case DeclAttrKind::WarnUnqualifiedAccess: {
      attrs.add(new (context) WarnUnqualifiedAccessAttr(true));
      break;
    }
    default:
      break;
    }
  }
}

static ValueDecl *
cloneBaseMemberDecl(ValueDecl *decl, DeclContext *newContext) {
  ASTContext &context = decl->getASTContext();

  if (auto fn = dyn_cast<FuncDecl>(decl)) {
    // TODO: function templates are specialized during type checking so to
    // support these we need to tell Swift to type check the synthesized bodies.
    // TODO: we also currently don't support static functions. That shouldn't be
    // too hard.
    if (fn->isStatic() ||
        isa_and_nonnull<clang::FunctionTemplateDecl>(fn->getClangDecl()))
      return nullptr;
    if (auto cxxMethod =
            dyn_cast_or_null<clang::CXXMethodDecl>(fn->getClangDecl())) {
      // FIXME: if this function has rvalue this, we won't be able to synthesize
      // the accessor correctly (https://github.com/apple/swift/issues/69745).
      if (cxxMethod->getRefQualifier() == clang::RefQualifierKind::RQ_RValue)
        return nullptr;
    }

    auto out = FuncDecl::createImplicit(
        context, fn->getStaticSpelling(), fn->getName(),
        fn->getNameLoc(), fn->hasAsync(), fn->hasThrows(),
        fn->getThrownInterfaceType(),
        fn->getGenericParams(), fn->getParameters(),
        fn->getResultInterfaceType(), newContext);
    cloneImportedAttributes(decl, out);
    out->copyFormalAccessFrom(fn);
    out->setBodySynthesizer(synthesizeBaseClassMethodBody, fn);
    out->setSelfAccessKind(fn->getSelfAccessKind());
    return out;
  }

  if (auto subscript = dyn_cast<SubscriptDecl>(decl)) {
    auto contextTy =
        newContext->mapTypeIntoContext(subscript->getElementInterfaceType());
    // Subscripts that return non-copyable types are not yet supported.
    // See: https://github.com/apple/swift/issues/70047.
    if (contextTy->isNoncopyable())
      return nullptr;
    auto out = SubscriptDecl::create(
        subscript->getASTContext(), subscript->getName(), subscript->getStaticLoc(),
        subscript->getStaticSpelling(), subscript->getSubscriptLoc(),
        subscript->getIndices(), subscript->getNameLoc(), subscript->getElementInterfaceType(),
        newContext, subscript->getGenericParams());
    out->copyFormalAccessFrom(subscript);
    out->setAccessors(SourceLoc(),
                      makeBaseClassMemberAccessors(newContext, out, subscript),
                      SourceLoc());
    out->setImplInfo(subscript->getImplInfo());
    return out;
  }

  if (auto var = dyn_cast<VarDecl>(decl)) {
    auto oldContext = var->getDeclContext();
    auto oldTypeDecl = oldContext->getSelfNominalTypeDecl();
    // FIXME: this is a workaround for rdar://128013193
    if (oldTypeDecl->getAttrs().hasAttribute<MoveOnlyAttr>() &&
        context.LangOpts.CxxInteropUseOpaquePointerForMoveOnly)
      return nullptr;

    auto rawMemory = allocateMemoryForDecl<VarDecl>(var->getASTContext(),
                                                    sizeof(VarDecl), false);
    auto out =
        new (rawMemory) VarDecl(var->isStatic(), var->getIntroducer(),
                                var->getLoc(), var->getName(), newContext);
    out->setInterfaceType(var->getInterfaceType());
    out->setIsObjC(var->isObjC());
    out->setIsDynamic(var->isDynamic());
    out->copyFormalAccessFrom(var);
    out->getASTContext().evaluator.cacheOutput(HasStorageRequest{out}, false);
    auto accessors = makeBaseClassMemberAccessors(newContext, out, var);
    out->setAccessors(SourceLoc(), accessors, SourceLoc());
    auto isMutable = var->getWriteImpl() == WriteImplKind::Immutable
                         ? StorageIsNotMutable : StorageIsMutable;
    out->setImplInfo(
        accessors[0]->getAccessorKind() == AccessorKind::Address
            ? (accessors.size() > 1
                   ? StorageImplInfo(ReadImplKind::Address,
                                     WriteImplKind::MutableAddress,
                                     ReadWriteImplKind::MutableAddress)
                   : StorageImplInfo(ReadImplKind::Address))
            : StorageImplInfo::getComputed(isMutable));
    out->setIsSetterMutating(true);
    return out;
  }

  if (auto typeAlias = dyn_cast<TypeAliasDecl>(decl)) {
    auto rawMemory = allocateMemoryForDecl<TypeAliasDecl>(
        typeAlias->getASTContext(), sizeof(TypeAliasDecl), false);
    auto out = new (rawMemory)
        TypeAliasDecl(typeAlias->getStartLoc(), typeAlias->getEqualLoc(),
                      typeAlias->getName(), typeAlias->getNameLoc(),
                      typeAlias->getGenericParams(), newContext);
    out->setUnderlyingType(typeAlias->getUnderlyingType());
    out->copyFormalAccessFrom(typeAlias);
    return out;
  }

  if (auto typeDecl = dyn_cast<TypeDecl>(decl)) {
    auto rawMemory = allocateMemoryForDecl<TypeAliasDecl>(
        typeDecl->getASTContext(), sizeof(TypeAliasDecl), false);
    auto out = new (rawMemory) TypeAliasDecl(
        typeDecl->getLoc(), typeDecl->getLoc(), typeDecl->getName(),
        typeDecl->getLoc(), nullptr, newContext);
    out->setUnderlyingType(typeDecl->getInterfaceType());
    out->copyFormalAccessFrom(typeDecl);
    return out;
  }

  return nullptr;
}

TinyPtrVector<ValueDecl *> ClangRecordMemberLookup::evaluate(
    Evaluator &evaluator, ClangRecordMemberLookupDescriptor desc) const {
  NominalTypeDecl *recordDecl = desc.recordDecl;
  DeclName name = desc.name;

  auto &ctx = recordDecl->getASTContext();
  auto allResults = evaluateOrDefault(
      ctx.evaluator,
      ClangDirectLookupRequest({recordDecl, recordDecl->getClangDecl(), name}),
      {});

  // Find the results that are actually a member of "recordDecl".
  TinyPtrVector<ValueDecl *> result;
  ClangModuleLoader *clangModuleLoader = ctx.getClangModuleLoader();
  for (auto found : allResults) {
    auto named = found.get<clang::NamedDecl *>();
    if (dyn_cast<clang::Decl>(named->getDeclContext()) ==
        recordDecl->getClangDecl()) {
      // Don't import constructors on foreign reference types.
      if (isa<clang::CXXConstructorDecl>(named) && isa<ClassDecl>(recordDecl))
        continue;

      if (auto import = clangModuleLoader->importDeclDirectly(named))
        result.push_back(cast<ValueDecl>(import));
    }
  }

  // If this is a C++ record, look through any base classes.
  if (auto cxxRecord =
          dyn_cast<clang::CXXRecordDecl>(recordDecl->getClangDecl())) {
    // Capture the arity of already found members in the
    // current record, to avoid adding ambiguous members
    // from base classes.
    const auto getArity =
        ClangImporter::Implementation::getImportedBaseMemberDeclArity;
    llvm::SmallSet<size_t, 4> foundNameArities;
    for (const auto *valueDecl : result)
      foundNameArities.insert(getArity(valueDecl));

    for (auto base : cxxRecord->bases()) {
      if (base.getAccessSpecifier() != clang::AccessSpecifier::AS_public)
        continue;

      clang::QualType baseType = base.getType();
      if (auto spectType = dyn_cast<clang::TemplateSpecializationType>(baseType))
        baseType = spectType->desugar();
      if (!isa<clang::RecordType>(baseType.getCanonicalType()))
        continue;

      auto *baseRecord = baseType->getAs<clang::RecordType>()->getDecl();
      if (auto import = clangModuleLoader->importDeclDirectly(baseRecord)) {
        // If we are looking up the base class, go no further. We will have
        // already found it during the other lookup.
        if (cast<ValueDecl>(import)->getName() == name)
          continue;

        // Add Clang members that are imported lazily.
        auto baseResults = evaluateOrDefault(
            ctx.evaluator,
            ClangRecordMemberLookup({cast<NominalTypeDecl>(import), name}), {});
        // Add members that are synthesized eagerly, such as subscripts.
        for (auto member :
             cast<NominalTypeDecl>(import)->getCurrentMembersWithoutLoading()) {
          if (auto namedMember = dyn_cast<ValueDecl>(member)) {
            if (namedMember->hasName() &&
                namedMember->getName().getBaseName() == name &&
                // Make sure we don't add duplicate entries, as that would
                // wrongly imply that lookup is ambiguous.
                !llvm::is_contained(baseResults, namedMember)) {
              baseResults.push_back(namedMember);
            }
          }
        }
        for (auto foundInBase : baseResults) {
          // Do not add duplicate entry with the same arity,
          // as that would cause an ambiguous lookup.
          if (foundNameArities.count(getArity(foundInBase)))
            continue;
          if (auto newDecl = clangModuleLoader->importBaseMemberDecl(
                  foundInBase, recordDecl)) {
            result.push_back(newDecl);
          }
        }
      }
    }
  }

  return result;
}

IterableDeclContext *IterableDeclContext::getImplementationContext() {
  if (auto implDecl = getDecl()->getObjCImplementationDecl())
    if (auto implExt = dyn_cast<ExtensionDecl>(implDecl))
      return implExt;

  return this;
}

namespace {
struct OrderDecls {
  bool operator () (Decl *lhs, Decl *rhs) const {
    if (lhs->getDeclContext()->getModuleScopeContext()
          == rhs->getDeclContext()->getModuleScopeContext()) {
      auto &SM = lhs->getASTContext().SourceMgr;
      return SM.isBeforeInBuffer(lhs->getLoc(), rhs->getLoc());
    }

    auto lhsFile =
        dyn_cast<SourceFile>(lhs->getDeclContext()->getModuleScopeContext());
    auto rhsFile =
        dyn_cast<SourceFile>(rhs->getDeclContext()->getModuleScopeContext());

    if (!lhsFile)
      return false;
    if (!rhsFile)
      return true;

    return lhsFile->getFilename() < rhsFile->getFilename();
  }
};
}

static ObjCInterfaceAndImplementation
constructResult(const llvm::TinyPtrVector<Decl *> &interfaces,
                llvm::TinyPtrVector<Decl *> &impls,
                Decl *diagnoseOn, Identifier categoryName) {
  if (interfaces.empty() || impls.empty())
    return ObjCInterfaceAndImplementation();

  if (impls.size() > 1) {
    llvm::sort(impls, OrderDecls());

    auto &diags = interfaces.front()->getASTContext().Diags;
    for (auto extraImpl : llvm::ArrayRef<Decl *>(impls).drop_front()) {
      auto attr = extraImpl->getAttrs().getAttribute<ObjCImplementationAttr>();
      attr->setInvalid();

      // @objc @implementations for categories are diagnosed as category
      // conflicts, so we're only concerned with main class bodies and
      // non-category implementations here.
      if (categoryName.empty() || !isa<ExtensionDecl>(impls.front())) {
        diags.diagnose(attr->getLocation(), diag::objc_implementation_two_impls,
                       diagnoseOn)
          .fixItRemove(attr->getRangeWithAt());
        diags.diagnose(impls.front(), diag::previous_objc_implementation);
      }
    }
  }

  return ObjCInterfaceAndImplementation(interfaces, impls.front());
}

static bool isImplValid(ExtensionDecl *ext) {
  auto attr = ext->getAttrs().getAttribute<ObjCImplementationAttr>();

  if (!attr)
    return false;

  // Clients using the stable syntax shouldn't have a category name on the attr.
  // This is diagnosed in AttributeChecker::visitObjCImplementationAttr().
  if (!attr->isEarlyAdopter() && !attr->CategoryName.empty())
    return false;
  
  return !attr->isCategoryNameInvalid();
}

static ObjCInterfaceAndImplementation
findContextInterfaceAndImplementation(DeclContext *dc) {
  if (!dc)
    return {};

  ClassDecl *classDecl = dc->getSelfClassDecl();
  if (!classDecl || !classDecl->hasClangNode())
    // Only extensions of ObjC classes can have @_objcImplementations.
    return {};

  // We know the class we're trying to work with. Next, the category name.
  Identifier categoryName;

  if (auto ext = dyn_cast<ExtensionDecl>(dc)) {
    assert(ext);
    if (!ext->hasClangNode() && !isImplValid(ext))
      return {};

    categoryName = ext->getObjCCategoryName();
  } else {
    // Must be an imported class. Look for its main implementation.
    assert(isa_and_nonnull<ClassDecl>(dc));
    categoryName = Identifier();
  }

  // Now let's look up the interfaces for this...
  auto interfaceDecls = classDecl->getImportedObjCCategory(categoryName);

  // And the implementations.
  llvm::TinyPtrVector<Decl *> implDecls;
  for (ExtensionDecl *ext : classDecl->getExtensions()) {
    if (ext->isObjCImplementation()
          && ext->getObjCCategoryName() == categoryName
          && isImplValid(ext))
      implDecls.push_back(ext);
  }

  return constructResult(interfaceDecls, implDecls, classDecl, categoryName);
}

static void lookupRelatedFuncs(AbstractFunctionDecl *func,
                               SmallVectorImpl<ValueDecl *> &results) {
  DeclName swiftName;
  if (auto accessor = dyn_cast<AccessorDecl>(func))
    swiftName = accessor->getStorage()->getName();
  else
    swiftName = func->getName();

  NLOptions options = NL_IgnoreAccessControl | NL_IgnoreMissingImports;
  if (auto ty = func->getDeclContext()->getSelfNominalTypeDecl()) {
    ty->lookupQualified({ ty }, DeclNameRef(swiftName), func->getLoc(),
                        NL_QualifiedDefault | options, results);
  }
  else {
    auto mod = func->getDeclContext()->getParentModule();
    mod->lookupQualified(mod, DeclNameRef(swiftName), func->getLoc(),
                         NL_RemoveOverridden | options, results);
  }
}

static ObjCInterfaceAndImplementation
findFunctionInterfaceAndImplementation(AbstractFunctionDecl *func) {
  if (!func)
    return {};

  // If this isn't either a clang import or an implementation, there's no point
  // doing any work here.
  if (!func->hasClangNode() && !func->isObjCImplementation())
    return {};

  OptionalEnum<AccessorKind> accessorKind;
  if (auto accessor = dyn_cast<AccessorDecl>(func))
    accessorKind = accessor->getAccessorKind();

  StringRef clangName = func->getCDeclName();
  if (clangName.empty())
    return {};

  SmallVector<ValueDecl *, 4> results;
  lookupRelatedFuncs(func, results);

  // Classify the `results` as either the interface or an implementation.
  // (Multiple implementations are invalid but utterable.)
  Decl *interface = nullptr;
  TinyPtrVector<Decl *> impls;

  for (ValueDecl *result : results) {
    AbstractFunctionDecl *resultFunc = nullptr;
    if (accessorKind) {
      if (auto resultStorage = dyn_cast<AbstractStorageDecl>(result))
        resultFunc = resultStorage->getAccessor(*accessorKind);
    }
    else
      resultFunc = dyn_cast<AbstractFunctionDecl>(result);

    if (!resultFunc)
      continue;

    if (resultFunc->getCDeclName() != clangName)
      continue;

    if (resultFunc->hasClangNode()) {
      if (interface) {
        // This clang name is overloaded. That should only happen with C++
        // functions/methods, which aren't currently supported.
        return {};
      }
      interface = result;
    } else if (resultFunc->isObjCImplementation()) {
      impls.push_back(result);
    }
  }

  // If we found enough decls to construct a result, `func` should be among them
  // somewhere.
  assert(interface == nullptr || impls.empty() ||
         interface == func || llvm::is_contained(impls, func));

  return constructResult({ interface }, impls, interface,
                         /*categoryName=*/Identifier());
}

ObjCInterfaceAndImplementation ObjCInterfaceAndImplementationRequest::
evaluate(Evaluator &evaluator, Decl *decl) const {
  // Types and extensions have direct links to their counterparts through the
  // `@_objcImplementation` attribute. Let's resolve that.
  // (Also directing nulls here, where they'll early-return.)
  if (auto ty = dyn_cast_or_null<NominalTypeDecl>(decl))
    return findContextInterfaceAndImplementation(ty);
  else if (auto ext = dyn_cast<ExtensionDecl>(decl))
    return findContextInterfaceAndImplementation(ext);
  // Abstract functions have to be matched through their @_cdecl attributes.
  else if (auto func = dyn_cast<AbstractFunctionDecl>(decl))
    return findFunctionInterfaceAndImplementation(func);

  return {};
}

void swift::simple_display(llvm::raw_ostream &out,
                           const ObjCInterfaceAndImplementation &pair) {
  if (pair.empty()) {
    out << "no clang interface or @_objcImplementation";
    return;
  }

  out << "@implementation ";
  simple_display(out, pair.implementationDecl);
  out << " matches clang interfaces ";
  simple_display(out, pair.interfaceDecls);
}

SourceLoc
swift::extractNearestSourceLoc(const ObjCInterfaceAndImplementation &pair) {
  if (pair.implementationDecl)
    return SourceLoc();
  return extractNearestSourceLoc(pair.implementationDecl);
}

llvm::TinyPtrVector<Decl *> Decl::getAllImplementedObjCDecls() const {
  if (hasClangNode())
    // This *is* the interface, if there is one.
    return {};

  ObjCInterfaceAndImplementationRequest req{const_cast<Decl *>(this)};
  auto result = evaluateOrDefault(getASTContext().evaluator, req, {});
  return result.interfaceDecls;
}

DeclContext *DeclContext::getImplementedObjCContext() const {
  if (auto ED = dyn_cast<ExtensionDecl>(this))
    if (auto impl = dyn_cast_or_null<DeclContext>(ED->getImplementedObjCDecl()))
      return impl;
  return const_cast<DeclContext *>(this);
}

Decl *Decl::getObjCImplementationDecl() const {
  if (!hasClangNode())
    // This *is* the implementation, if it has one.
    return nullptr;

  ObjCInterfaceAndImplementationRequest req{const_cast<Decl *>(this)};
  auto result = evaluateOrDefault(getASTContext().evaluator, req, {});
  return result.implementationDecl;
}

llvm::TinyPtrVector<Decl *>
ClangCategoryLookupRequest::evaluate(Evaluator &evaluator,
                                     ClangCategoryLookupDescriptor desc) const {
  const ClassDecl *CD = desc.classDecl;
  Identifier categoryName = desc.categoryName;

  auto clangClass =
      dyn_cast_or_null<clang::ObjCInterfaceDecl>(CD->getClangDecl());
  if (!clangClass)
    return {};

  auto importCategory = [&](const clang::ObjCCategoryDecl *clangCat) -> Decl * {
    return CD->getASTContext().getClangModuleLoader()
                  ->importDeclDirectly(clangCat);
  };

  if (categoryName.empty()) {
    // No category name, so we want the decl for the `@interface` in
    // `clangClass`, as well as any class extensions.
    llvm::TinyPtrVector<Decl *> results;
    results.push_back(const_cast<ClassDecl *>(CD));

    auto importer =
       static_cast<ClangImporter *>(CD->getASTContext().getClangModuleLoader());
    ClangImporter::Implementation &impl = importer->Impl;

    for (auto clangExt : clangClass->known_extensions()) {
      if (impl.getClangSema().isVisible(clangExt))
        results.push_back(importCategory(clangExt));
    }

    return results;
  }

  auto ident = &clangClass->getASTContext().Idents.get(categoryName.str());
  auto clangCategory = clangClass->FindCategoryDeclaration(ident);
  if (!clangCategory)
    return {};

  return { importCategory(clangCategory) };
}

llvm::TinyPtrVector<Decl *>
ClassDecl::getImportedObjCCategory(Identifier name) const {
  ClangCategoryLookupDescriptor desc{this, name};
  return evaluateOrDefault(getASTContext().evaluator,
                           ClangCategoryLookupRequest(desc),
                           {});
}

void swift::simple_display(llvm::raw_ostream &out,
                           const ClangCategoryLookupDescriptor &desc) {
  out << "Looking up @interface for ";
  if (!desc.categoryName.empty()) {
    out << "category ";
    simple_display(out, desc.categoryName);
  }
  else {
    out << "main body";
  }
  out << " of ";
  simple_display(out, desc.classDecl);
}

SourceLoc
swift::extractNearestSourceLoc(const ClangCategoryLookupDescriptor &desc) {
  return extractNearestSourceLoc(desc.classDecl);
}

TinyPtrVector<ValueDecl *>
ClangImporter::Implementation::loadNamedMembers(
    const IterableDeclContext *IDC, DeclBaseName N, uint64_t extra) {
  auto *D = IDC->getDecl();
  auto *DC = D->getInnermostDeclContext();
  auto *CD = D->getClangDecl();
  auto *CDC = cast_or_null<clang::DeclContext>(CD);

  auto *nominal = DC->getSelfNominalTypeDecl();
  auto effectiveClangContext = getEffectiveClangContext(nominal);

  // There are 3 cases:
  //
  //  - The decl is from a bridging header, CMO is Some(nullptr)
  //    which denotes the __ObjC Swift module and its associated
  //    BridgingHeaderLookupTable.
  //
  //  - The decl is from a clang module, CMO is Some(M) for non-null
  //    M and we can use the table for that module.
  //
  //  - The decl is a forward declaration, CMO is None, which should
  //    never be the case if we got here (someone is asking for members).
  //
  // findLookupTable, below, handles the first two cases; we assert on the
  // third.

  std::optional<clang::Module *> CMO;
  if (CD)
    CMO = getClangSubmoduleForDecl(CD);
  else {
    // IDC is an extension containing globals imported as members, so it doesn't
    // have a clang node but the submodule pointer has been stashed in `extra`.
    CMO = reinterpret_cast<clang::Module *>(static_cast<uintptr_t>(extra));
  }
  assert(CMO && "loadNamedMembers on a forward-declared Decl");

  auto table = findLookupTable(*CMO);
  assert(table && "clang module without lookup table");

  assert(!isa_and_nonnull<clang::NamespaceDecl>(CD)
            && "Namespace members should be loaded via a request.");
  assert(!CD || isa<clang::ObjCContainerDecl>(CD));

  // Force the members of the entire inheritance hierarchy to be loaded and
  // deserialized before loading the named member of a class. This warms up
  // ClangImporter::Implementation::MembersForNominal, used for computing
  // property overrides.
  //
  // FIXME: If getOverriddenDecl() kicked off a request for imported decls,
  // we could postpone this until overrides are actually requested.
  if (auto *classDecl = dyn_cast<ClassDecl>(D))
    if (auto *superclassDecl = classDecl->getSuperclassDecl())
      (void) const_cast<ClassDecl *>(superclassDecl)->lookupDirect(N);

  // TODO: update this to use the requestified lookup.
  TinyPtrVector<ValueDecl *> Members;

  // Lookup actual, factual clang-side members of the context. No need to do
  // this if we're handling an import-as-member extension.
  if (CD) {
    for (auto entry : table->lookup(SerializedSwiftName(N),
                                    effectiveClangContext)) {
      if (!entry.is<clang::NamedDecl *>()) continue;
      auto member = entry.get<clang::NamedDecl *>();
      if (!isVisibleClangEntry(member)) continue;

      // Skip Decls from different clang::DeclContexts
      if (member->getDeclContext() != CDC) continue;

      SmallVector<Decl*, 4> tmp;
      insertMembersAndAlternates(member, tmp, DC);
      for (auto *TD : tmp) {
        if (auto *V = dyn_cast<ValueDecl>(TD)) {
          // Skip ValueDecls if they import under different names.
          if (V->getBaseName() == N) {
            Members.push_back(V);
          }
        }

        // If the property's accessors have alternate decls, we might have
        // to import those too.
        if (auto *ASD = dyn_cast<AbstractStorageDecl>(TD)) {
          for (auto *AD : ASD->getAllAccessors()) {
            for (auto *D : getAlternateDecls(AD)) {
              if (D->getBaseName() == N)
                Members.push_back(D);
            }
          }
        }
      }
    }
  }

  for (auto entry : table->lookupGlobalsAsMembers(SerializedSwiftName(N),
                                                  effectiveClangContext)) {
    if (!entry.is<clang::NamedDecl *>()) continue;
    auto member = entry.get<clang::NamedDecl *>();
    if (!isVisibleClangEntry(member)) continue;

    // Skip Decls from different clang::DeclContexts. We don't do this for
    // import-as-member extensions because we don't know what decl context to
    // expect; for instance, an enum constant is inside the enum decl, not in
    // the translation unit.
    if (CDC && member->getDeclContext() != CDC) continue;

    SmallVector<Decl*, 4> tmp;
    insertMembersAndAlternates(member, tmp, DC);
    for (auto *TD : tmp) {
      if (auto *V = dyn_cast<ValueDecl>(TD)) {
        // Skip ValueDecls if they import under different names.
        if (V->getBaseName() == N) {
          Members.push_back(V);
        }
      }
    }
  }

  if (CD && N.isConstructor()) {
    if (auto *classDecl = dyn_cast<ClassDecl>(D)) {
      SmallVector<Decl *, 4> ctors;
      importInheritedConstructors(cast<clang::ObjCInterfaceDecl>(CD),
                                  classDecl, ctors);
      for (auto ctor : ctors)
        Members.push_back(cast<ValueDecl>(ctor));
    }
  }

  if (CD && !isa<ProtocolDecl>(D)) {
    if (auto *OCD = dyn_cast<clang::ObjCContainerDecl>(CD)) {
      SmallVector<Decl *, 1> newMembers;
      importMirroredProtocolMembers(OCD, DC, N, newMembers);
      for (auto member : newMembers)
          Members.push_back(cast<ValueDecl>(member));
    }
  }

  return Members;
}

EffectiveClangContext ClangImporter::Implementation::getEffectiveClangContext(
    const NominalTypeDecl *nominal) {
  // If we have a Clang declaration, look at it to determine the
  // effective Clang context.
  if (auto constClangDecl = nominal->getClangDecl()) {
    auto clangDecl = const_cast<clang::Decl *>(constClangDecl);
    if (auto dc = dyn_cast<clang::DeclContext>(clangDecl))
      return EffectiveClangContext(dc);
    if (auto typedefName = dyn_cast<clang::TypedefNameDecl>(clangDecl))
      return EffectiveClangContext(typedefName);

    return EffectiveClangContext();
  }

  // If it's an @objc entity, go look for it.
  // Note that we're stepping lightly here to avoid computing isObjC()
  // too early.
  if (isa<ClassDecl>(nominal) &&
      (nominal->getAttrs().hasAttribute<ObjCAttr>() ||
       (!nominal->getParentSourceFile() && nominal->isObjC()))) {
    // Map the name. If we can't represent the Swift name in Clang.
    Identifier name = nominal->getName();
    if (auto objcAttr = nominal->getAttrs().getAttribute<ObjCAttr>()) {
      if (auto objcName = objcAttr->getName()) {
        if (objcName->getNumArgs() == 0) {
          // This is an error if not 0, but it should be caught later.
          name = objcName->getSimpleName();
        }
      }
    }
    auto clangName = exportName(name);
    if (!clangName)
      return EffectiveClangContext();

    // Perform name lookup into the global scope.
    auto &sema = Instance->getSema();
    clang::LookupResult lookupResult(sema, clangName,
                                     clang::SourceLocation(),
                                     clang::Sema::LookupOrdinaryName);
    if (sema.LookupName(lookupResult, /*Scope=*/nullptr)) {
      // FIXME: Filter based on access path? C++ access control?
      for (auto clangDecl : lookupResult) {
        if (auto objcClass = dyn_cast<clang::ObjCInterfaceDecl>(clangDecl))
          return EffectiveClangContext(objcClass);

        /// FIXME: Other type declarations should also be okay?
      }
    }

    // For source compatibility reasons, fall back to the Swift name.
    //
    // This is how people worked around not being able to import-as-member onto
    // Swift types by their ObjC name before the above code to handle ObjCAttr
    // was added.
    if (name != nominal->getName())
      clangName = exportName(nominal->getName());

    lookupResult.clear();
    lookupResult.setLookupName(clangName);
    // FIXME: This loop is duplicated from above, but doesn't obviously factor
    // out in a nice way.
    if (sema.LookupName(lookupResult, /*Scope=*/nullptr)) {
      // FIXME: Filter based on access path? C++ access control?
      for (auto clangDecl : lookupResult) {
        if (auto objcClass = dyn_cast<clang::ObjCInterfaceDecl>(clangDecl))
          return EffectiveClangContext(objcClass);

        /// FIXME: Other type declarations should also be okay?
      }
    }
  }

  return EffectiveClangContext();
}

void ClangImporter::dumpSwiftLookupTables() const {
  Impl.dumpSwiftLookupTables();
}

void ClangImporter::Implementation::dumpSwiftLookupTables() {
  // Sort the module names so we can print in a deterministic order.
  SmallVector<StringRef, 4> moduleNames;
  for (const auto &lookupTable : LookupTables) {
    moduleNames.push_back(lookupTable.first);
  }
  array_pod_sort(moduleNames.begin(), moduleNames.end());

  // Print out the lookup tables for the various modules.
  for (auto moduleName : moduleNames) {
    llvm::errs() << "<<" << moduleName << " lookup table>>\n";
    LookupTables[moduleName]->deserializeAll();
    LookupTables[moduleName]->dump(llvm::errs());
  }

  llvm::errs() << "<<Bridging header lookup table>>\n";
  BridgingHeaderLookupTable->dump(llvm::errs());
}

DeclName ClangImporter::
importName(const clang::NamedDecl *D,
           clang::DeclarationName preferredName) {
  return Impl.importFullName(D, Impl.CurrentVersion, preferredName).
    getDeclName();
}

std::optional<Type>
ClangImporter::importFunctionReturnType(const clang::FunctionDecl *clangDecl,
                                        DeclContext *dc) {
  bool isInSystemModule =
      cast<ClangModuleUnit>(dc->getModuleScopeContext())->isSystemModule();
  bool allowNSUIntegerAsInt =
      Impl.shouldAllowNSUIntegerAsInt(isInSystemModule, clangDecl);
  if (auto imported =
          Impl.importFunctionReturnType(dc, clangDecl, allowNSUIntegerAsInt)
              .getType())
    return imported;
  return {};
}

Type ClangImporter::importVarDeclType(
    const clang::VarDecl *decl, VarDecl *swiftDecl, DeclContext *dc) {
  if (decl->getTemplateInstantiationPattern())
    Impl.getClangSema().InstantiateVariableDefinition(
        decl->getLocation(),
        const_cast<clang::VarDecl *>(decl));

  // If the declaration is const, consider it audited.
  // We can assume that loading a const global variable doesn't
  // involve an ownership transfer.
  bool isAudited = decl->getType().isConstQualified();

  auto declType = decl->getType();

  // Special case: NS Notifications
  if (isNSNotificationGlobal(decl))
    if (auto newtypeDecl = findSwiftNewtype(decl, Impl.getClangSema(),
                                            Impl.CurrentVersion))
      declType = Impl.getClangASTContext().getTypedefType(newtypeDecl);

  bool isInSystemModule =
      cast<ClangModuleUnit>(dc->getModuleScopeContext())->isSystemModule();

  // Note that we deliberately don't bridge most globals because we want to
  // preserve pointer identity.
  auto importedType =
      Impl.importType(declType,
                      (isAudited ? ImportTypeKind::AuditedVariable
                                 : ImportTypeKind::Variable),
                      ImportDiagnosticAdder(Impl, decl, decl->getLocation()),
                      isInSystemModule, Bridgeability::None,
                      getImportTypeAttrs(decl));

  if (!importedType)
    return ErrorType::get(Impl.SwiftContext);

  if (importedType.isImplicitlyUnwrapped())
    swiftDecl->setImplicitlyUnwrappedOptional(true);

  return importedType.getType();
}

bool ClangImporter::isInOverlayModuleForImportedModule(
                                               const DeclContext *overlayDC,
                                               const DeclContext *importedDC) {
  overlayDC = overlayDC->getModuleScopeContext();
  importedDC = importedDC->getModuleScopeContext();

  auto importedClangModuleUnit = dyn_cast<ClangModuleUnit>(importedDC);
  if (!importedClangModuleUnit || !importedClangModuleUnit->getClangModule())
    return false;

  auto overlayModule = overlayDC->getParentModule();
  if (overlayModule == importedClangModuleUnit->getOverlayModule())
    return true;

  // Is this a private module that's re-exported to the public (overlay) name?
  auto clangModule =
  importedClangModuleUnit->getClangModule()->getTopLevelModule();
  return !clangModule->ExportAsModule.empty() &&
    clangModule->ExportAsModule == overlayModule->getName().str();
}

/// Extract the specified-or-defaulted -module-cache-path that winds up in
/// the clang importer, for reuse as the .swiftmodule cache path when
/// building a ModuleInterfaceLoader.
std::string
swift::getModuleCachePathFromClang(const clang::CompilerInstance &Clang) {
  if (!Clang.hasPreprocessor())
    return "";
  std::string SpecificModuleCachePath =
      Clang.getPreprocessor().getHeaderSearchInfo().getModuleCachePath().str();

  // The returned-from-clang module cache path includes a suffix directory
  // that is specific to the clang version and invocation; we want the
  // directory above that.
  return llvm::sys::path::parent_path(SpecificModuleCachePath).str();
}

clang::FunctionDecl *ClangImporter::instantiateCXXFunctionTemplate(
    ASTContext &ctx, clang::FunctionTemplateDecl *func, SubstitutionMap subst) {
  SmallVector<clang::TemplateArgument, 4> templateSubst;
  std::unique_ptr<TemplateInstantiationError> error =
      ctx.getClangTemplateArguments(func->getTemplateParameters(),
                                    subst.getReplacementTypes(), templateSubst);

  auto getFuncName = [&]() -> std::string {
    std::string funcName;
    llvm::raw_string_ostream funcNameStream(funcName);
    func->printQualifiedName(funcNameStream);
    return funcName;
  };

  if (error) {
    std::string failedTypesStr;
    llvm::raw_string_ostream failedTypesStrStream(failedTypesStr);
    llvm::interleaveComma(error->failedTypes, failedTypesStrStream);

    // TODO: Use the location of the apply here.
    // TODO: This error message should not reference implementation details.
    // See: https://github.com/apple/swift/pull/33053#discussion_r477003350
    ctx.Diags.diagnose(SourceLoc(), diag::unable_to_convert_generic_swift_types,
                       getFuncName(), failedTypesStr);
    return nullptr;
  }

  // Instantiate a specialization of this template using the substitution map.
  auto *templateArgList = clang::TemplateArgumentList::CreateCopy(
      func->getASTContext(), templateSubst);
  auto &sema = getClangInstance().getSema();
  auto *spec = sema.InstantiateFunctionDeclaration(func, templateArgList,
                                                   clang::SourceLocation());
  if (!spec) {
    std::string templateParams;
    llvm::raw_string_ostream templateParamsStream(templateParams);
    llvm::interleaveComma(templateArgList->asArray(), templateParamsStream,
                          [&](const clang::TemplateArgument &arg) {
                            arg.print(func->getASTContext().getPrintingPolicy(),
                                      templateParamsStream,
                                      /*IncludeType*/ true);
                          });
    ctx.Diags.diagnose(SourceLoc(),
                       diag::unable_to_substitute_cxx_function_template,
                       getFuncName(), templateParams);
    return nullptr;
  }
  sema.InstantiateFunctionDefinition(clang::SourceLocation(), spec);
  return spec;
}

StructDecl *
ClangImporter::instantiateCXXClassTemplate(
    clang::ClassTemplateDecl *decl,
    ArrayRef<clang::TemplateArgument> arguments) {
  void *InsertPos = nullptr;
  auto *ctsd = decl->findSpecialization(arguments, InsertPos);
  if (!ctsd) {
    ctsd = clang::ClassTemplateSpecializationDecl::Create(
        decl->getASTContext(), decl->getTemplatedDecl()->getTagKind(),
        decl->getDeclContext(), decl->getTemplatedDecl()->getBeginLoc(),
        decl->getLocation(), decl, arguments, nullptr);
    decl->AddSpecialization(ctsd, InsertPos);
  }

  auto CanonType = decl->getASTContext().getTypeDeclType(ctsd);
  assert(isa<clang::RecordType>(CanonType) &&
          "type of non-dependent specialization is not a RecordType");

  return dyn_cast_or_null<StructDecl>(
      Impl.importDecl(ctsd, Impl.CurrentVersion));
}

// On Windows and 32-bit platforms we need to force "Int" to actually be
// re-imported as "Int." This is needed because otherwise, we cannot round-trip
// "Int" and "UInt". For example, on Windows, "Int" will be imported into C++ as
// "long long" and then back into Swift as "Int64" not "Int."
static ValueDecl *rewriteIntegerTypes(SubstitutionMap subst, ValueDecl *oldDecl,
                                      AbstractFunctionDecl *newDecl) {
  auto originalFnSubst = cast<AbstractFunctionDecl>(oldDecl)
                             ->getInterfaceType()
                             ->getAs<GenericFunctionType>()
                             ->substGenericArgs(subst);
  // The constructor type is a function type as follows:
  //   (CType.Type) -> (Generic) -> CType
  // And a method's function type is as follows:
  //   (inout CType) -> (Generic) -> Void
  // In either case, we only want the result of that function type because that
  // is the function type with the generic params that need to be substituted:
  //   (Generic) -> CType
  if (isa<ConstructorDecl>(oldDecl) || oldDecl->isInstanceMember() ||
      oldDecl->isStatic())
    originalFnSubst = cast<FunctionType>(originalFnSubst->getResult().getPointer());

  SmallVector<ParamDecl *, 4> fixedParameters;
  unsigned parameterIndex = 0;
  for (auto *newFnParam : *newDecl->getParameters()) {
    // If the user substituted this param with an (U)Int, use (U)Int.
    auto substParamType =
        originalFnSubst->getParams()[parameterIndex].getParameterType();
    if (substParamType->isEqual(newDecl->getASTContext().getIntType()) ||
        substParamType->isEqual(newDecl->getASTContext().getUIntType())) {
      auto intParam =
          ParamDecl::cloneWithoutType(newDecl->getASTContext(), newFnParam);
      intParam->setInterfaceType(substParamType);
      fixedParameters.push_back(intParam);
    } else {
      fixedParameters.push_back(newFnParam);
    }
    parameterIndex++;
  }

  auto fixedParams =
      ParameterList::create(newDecl->getASTContext(), fixedParameters);
  newDecl->setParameters(fixedParams);

  // Now fix the result type:
  if (originalFnSubst->getResult()->isEqual(
          newDecl->getASTContext().getIntType()) ||
      originalFnSubst->getResult()->isEqual(
          newDecl->getASTContext().getUIntType())) {
    // Constructors don't have a result.
    if (auto func = dyn_cast<FuncDecl>(newDecl)) {
      // We have to rebuild the whole function.
      auto newFnDecl = FuncDecl::createImported(
          func->getASTContext(), func->getNameLoc(),
          func->getName(), func->getNameLoc(),
          func->hasAsync(), func->hasThrows(),
          func->getThrownInterfaceType(),
          fixedParams, originalFnSubst->getResult(),
          /*genericParams=*/nullptr, func->getDeclContext(), newDecl->getClangDecl());
      if (func->isStatic()) newFnDecl->setStatic();
      if (func->isImportAsStaticMember()) newFnDecl->setImportAsStaticMember();
      if (func->getImportAsMemberStatus().isInstance()) {
        newFnDecl->setSelfAccessKind(func->getSelfAccessKind());
        newFnDecl->setSelfIndex(func->getSelfIndex());
      }

      return newFnDecl;
    }
  }

  return newDecl;
}

static Argument createSelfArg(FuncDecl *fnDecl) {
  ASTContext &ctx = fnDecl->getASTContext();

  auto selfDecl = fnDecl->getImplicitSelfDecl();
  auto selfRefExpr = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                           /*implicit*/ true);

  if (!fnDecl->isMutating()) {
    selfRefExpr->setType(selfDecl->getInterfaceType());
    return Argument::unlabeled(selfRefExpr);
  }
  selfRefExpr->setType(LValueType::get(selfDecl->getInterfaceType()));
  return Argument::implicitInOut(ctx, selfRefExpr);
}

// Synthesize a thunk body for the function created in
// "addThunkForDependentTypes". This will just cast all params and forward them
// along to the specialized function. It will also cast the result before
// returning it.
static std::pair<BraceStmt *, bool>
synthesizeDependentTypeThunkParamForwarding(AbstractFunctionDecl *afd, void *context) {
  ASTContext &ctx = afd->getASTContext();

  auto thunkDecl = cast<FuncDecl>(afd);
  auto specializedFuncDecl = static_cast<FuncDecl *>(context);

  SmallVector<Argument, 8> forwardingParams;
  unsigned paramIndex = 0;
  for (auto param : *thunkDecl->getParameters()) {
    if (isa<MetatypeType>(param->getInterfaceType().getPointer())) {
      paramIndex++;
      continue;
    }
    auto paramTy = param->getTypeInContext();
    auto isInOut = param->isInOut();
    auto specParamTy =
        specializedFuncDecl->getParameters()->get(paramIndex)
          ->getTypeInContext();

    Expr *paramRefExpr = new (ctx) DeclRefExpr(param, DeclNameLoc(),
                                               /*Implicit=*/true);
    paramRefExpr->setType(isInOut ? LValueType::get(paramTy) : paramTy);

    Argument arg = [&]() {
      if (isInOut) {
        assert(specParamTy->isEqual(paramTy));
        return Argument::implicitInOut(ctx, paramRefExpr);
      }
      Expr *argExpr = nullptr;
      if (specParamTy->isEqual(paramTy)) {
        argExpr = paramRefExpr;
      } else {
        argExpr = ForcedCheckedCastExpr::createImplicit(ctx, paramRefExpr,
                                                        specParamTy);
      }
      return Argument::unlabeled(argExpr);
    }();
    forwardingParams.push_back(arg);
    paramIndex++;
  }

  Expr *specializedFuncDeclRef = new (ctx) DeclRefExpr(ConcreteDeclRef(specializedFuncDecl),
                                                       DeclNameLoc(), true);
  specializedFuncDeclRef->setType(specializedFuncDecl->getInterfaceType());

  if (specializedFuncDecl->isInstanceMember()) {
    auto selfArg = createSelfArg(thunkDecl);
    auto *memberCall = DotSyntaxCallExpr::create(ctx, specializedFuncDeclRef,
                                                 SourceLoc(), selfArg);
    memberCall->setThrows(nullptr);
    auto resultType = specializedFuncDecl->getInterfaceType()->getAs<FunctionType>()->getResult();
    specializedFuncDeclRef = memberCall;
    specializedFuncDeclRef->setType(resultType);
  } else if (specializedFuncDecl->isStatic()) {
    auto resultType = specializedFuncDecl->getInterfaceType()->getAs<FunctionType>()->getResult();
    auto selfType = cast<NominalTypeDecl>(thunkDecl->getDeclContext()->getAsDecl())->getDeclaredInterfaceType();
    auto selfTypeExpr = TypeExpr::createImplicit(selfType, ctx);
    auto *memberCall =
        DotSyntaxCallExpr::create(ctx, specializedFuncDeclRef, SourceLoc(),
                                  Argument::unlabeled(selfTypeExpr));
    memberCall->setThrows(nullptr);
    specializedFuncDeclRef = memberCall;
    specializedFuncDeclRef->setType(resultType);
  }

  auto argList = ArgumentList::createImplicit(ctx, forwardingParams);
  auto *specializedFuncCallExpr = CallExpr::createImplicit(ctx, specializedFuncDeclRef, argList);
  specializedFuncCallExpr->setType(specializedFuncDecl->getResultInterfaceType());
  specializedFuncCallExpr->setThrows(nullptr);

  Expr *resultExpr = nullptr;
  if (specializedFuncCallExpr->getType()->isEqual(
        thunkDecl->getResultInterfaceType())) {
    resultExpr = specializedFuncCallExpr;
  } else {
    resultExpr = ForcedCheckedCastExpr::createImplicit(
        ctx, specializedFuncCallExpr, thunkDecl->getResultInterfaceType());
  }

  auto *returnStmt = ReturnStmt::createImplicit(ctx, resultExpr);
  auto body = BraceStmt::create(ctx, SourceLoc(), {returnStmt}, SourceLoc(),
                                /*implicit=*/true);
  return {body, /*isTypeChecked=*/true};
}

// Create a thunk to map functions with dependent types to their specialized
// version. For example, create a thunk with type (Any) -> Any to wrap a
// specialized function template with type (Dependent<T>) -> Dependent<T>.
static ValueDecl *addThunkForDependentTypes(FuncDecl *oldDecl,
                                            FuncDecl *newDecl) {
  bool updatedAnyParams = false;

  SmallVector<ParamDecl *, 4> fixedParameters;
  unsigned parameterIndex = 0;
  for (auto *newFnParam : *newDecl->getParameters()) {
    // If the un-specialized function had a parameter with type "Any" preserve
    // that parameter. Otherwise, use the new function parameter.
    auto oldParamType = oldDecl->getParameters()->get(parameterIndex)->getInterfaceType();
    if (oldParamType->isEqual(newDecl->getASTContext().getAnyExistentialType())) {
      updatedAnyParams = true;
      auto newParam =
          ParamDecl::cloneWithoutType(newDecl->getASTContext(), newFnParam);
      newParam->setInterfaceType(oldParamType);
      fixedParameters.push_back(newParam);
    } else {
      fixedParameters.push_back(newFnParam);
    }
    parameterIndex++;
  }

  // If we don't need this thunk, bail out.
  if (!updatedAnyParams &&
      !oldDecl->getResultInterfaceType()->isEqual(
          oldDecl->getASTContext().getAnyExistentialType()))
    return newDecl;

  auto fixedParams =
      ParameterList::create(newDecl->getASTContext(), fixedParameters);

  Type fixedResultType;
  if (oldDecl->getResultInterfaceType()->isEqual(
          oldDecl->getASTContext().getAnyExistentialType()))
    fixedResultType = oldDecl->getASTContext().getAnyExistentialType();
  else
    fixedResultType = newDecl->getResultInterfaceType();

  // We have to rebuild the whole function.
  auto newFnDecl = FuncDecl::createImplicit(
      newDecl->getASTContext(), newDecl->getStaticSpelling(),
      newDecl->getName(), newDecl->getNameLoc(), newDecl->hasAsync(),
      newDecl->hasThrows(), newDecl->getThrownInterfaceType(),
      /*genericParams=*/nullptr, fixedParams,
      fixedResultType, newDecl->getDeclContext());
  newFnDecl->copyFormalAccessFrom(newDecl);
  newFnDecl->setBodySynthesizer(synthesizeDependentTypeThunkParamForwarding, newDecl);
  newFnDecl->setSelfAccessKind(newDecl->getSelfAccessKind());
  if (newDecl->isStatic()) newFnDecl->setStatic();
  newFnDecl->getAttrs().add(
      new (newDecl->getASTContext()) TransparentAttr(/*IsImplicit=*/true));
  return newFnDecl;
}

// Synthesizes the body of a thunk that takes extra metatype arguments and
// skips over them to forward them along to the FuncDecl contained by context.
// This is used when importing a C++ templated function where the template params
// are not used in the function signature. We supply the type params as explicit
// metatype arguments to aid in typechecking, but they shouldn't be forwarded to
// the corresponding C++ function.
static std::pair<BraceStmt *, bool>
synthesizeForwardingThunkBody(AbstractFunctionDecl *afd, void *context) {
  ASTContext &ctx = afd->getASTContext();

  auto thunkDecl = cast<FuncDecl>(afd);
  auto specializedFuncDecl = static_cast<FuncDecl *>(context);

  SmallVector<Argument, 8> forwardingParams;
  for (auto param : *thunkDecl->getParameters()) {
    if (isa<MetatypeType>(param->getInterfaceType().getPointer())) {
      continue;
    }
    auto paramTy = param->getTypeInContext();
    auto isInOut = param->isInOut();

    Expr *paramRefExpr = new (ctx) DeclRefExpr(param, DeclNameLoc(),
                                               /*Implicit=*/true);
    paramRefExpr->setType(isInOut ? LValueType::get(paramTy) : paramTy);

    auto arg = isInOut ? Argument::implicitInOut(ctx, paramRefExpr)
                       : Argument::unlabeled(paramRefExpr);
    forwardingParams.push_back(arg);
  }

  Expr *specializedFuncDeclRef = new (ctx) DeclRefExpr(ConcreteDeclRef(specializedFuncDecl),
                                                       DeclNameLoc(), true);
  specializedFuncDeclRef->setType(specializedFuncDecl->getInterfaceType());

  if (specializedFuncDecl->isInstanceMember()) {
    auto selfArg = createSelfArg(thunkDecl);
    auto *memberCall = DotSyntaxCallExpr::create(ctx, specializedFuncDeclRef,
                                                 SourceLoc(), selfArg);
    memberCall->setThrows(nullptr);
    auto resultType = specializedFuncDecl->getInterfaceType()->getAs<FunctionType>()->getResult();
    specializedFuncDeclRef = memberCall;
    specializedFuncDeclRef->setType(resultType);
  } else if (specializedFuncDecl->isStatic()) {
    auto resultType = specializedFuncDecl->getInterfaceType()->getAs<FunctionType>()->getResult();
    auto selfType = cast<NominalTypeDecl>(thunkDecl->getDeclContext()->getAsDecl())->getDeclaredInterfaceType();
    auto selfTypeExpr = TypeExpr::createImplicit(selfType, ctx);
    auto *memberCall =
        DotSyntaxCallExpr::create(ctx, specializedFuncDeclRef, SourceLoc(),
                                  Argument::unlabeled(selfTypeExpr));
    memberCall->setThrows(nullptr);
    specializedFuncDeclRef = memberCall;
    specializedFuncDeclRef->setType(resultType);
  }

  auto argList = ArgumentList::createImplicit(ctx, forwardingParams);
  auto *specializedFuncCallExpr = CallExpr::createImplicit(ctx, specializedFuncDeclRef, argList);
  specializedFuncCallExpr->setType(thunkDecl->getResultInterfaceType());
  specializedFuncCallExpr->setThrows(nullptr);

  auto *returnStmt = ReturnStmt::createImplicit(ctx, specializedFuncCallExpr);

  auto body = BraceStmt::create(ctx, SourceLoc(), {returnStmt}, SourceLoc(),
                                /*implicit=*/true);
  return {body, /*isTypeChecked=*/true};
}

static ValueDecl *generateThunkForExtraMetatypes(SubstitutionMap subst,
                                                 FuncDecl *oldDecl,
                                                 FuncDecl *newDecl) {
  // We added additional metatype parameters to aid template
  // specialization, which are no longer now that we've specialized
  // this function. Create a thunk that only forwards the original
  // parameters along to the clang function.
  SmallVector<ParamDecl *, 4> newParams;

  for (auto param : *newDecl->getParameters()) {
    auto *newParamDecl = ParamDecl::clone(newDecl->getASTContext(), param);
    newParams.push_back(newParamDecl);
  }

  auto originalFnSubst = cast<AbstractFunctionDecl>(oldDecl)
                             ->getInterfaceType()
                             ->getAs<GenericFunctionType>()
                             ->substGenericArgs(subst);
  // The constructor type is a function type as follows:
  //   (CType.Type) -> (Generic) -> CType
  // And a method's function type is as follows:
  //   (inout CType) -> (Generic) -> Void
  // In either case, we only want the result of that function type because that
  // is the function type with the generic params that need to be substituted:
  //   (Generic) -> CType
  if (isa<ConstructorDecl>(oldDecl) || oldDecl->isInstanceMember() ||
      oldDecl->isStatic())
    originalFnSubst = cast<FunctionType>(originalFnSubst->getResult().getPointer());

  for (auto paramTy : originalFnSubst->getParams()) {
    if (!paramTy.getPlainType()->is<MetatypeType>())
      continue;

    auto dc = newDecl->getDeclContext();
    auto paramVarDecl =
        new (newDecl->getASTContext()) ParamDecl(
            SourceLoc(), SourceLoc(), Identifier(), SourceLoc(),
            newDecl->getASTContext().getIdentifier("_"), dc);
    paramVarDecl->setInterfaceType(paramTy.getPlainType());
    paramVarDecl->setSpecifier(ParamSpecifier::Default);
    newParams.push_back(paramVarDecl);
  }

  auto *newParamList =
      ParameterList::create(newDecl->getASTContext(), SourceLoc(), newParams, SourceLoc());

  auto thunk = FuncDecl::createImplicit(
      newDecl->getASTContext(), newDecl->getStaticSpelling(), oldDecl->getName(),
      newDecl->getNameLoc(), newDecl->hasAsync(), newDecl->hasThrows(),
      newDecl->getThrownInterfaceType(),
      /*genericParams=*/nullptr, newParamList,
      newDecl->getResultInterfaceType(), newDecl->getDeclContext());
  thunk->copyFormalAccessFrom(newDecl);
  thunk->setBodySynthesizer(synthesizeForwardingThunkBody, newDecl);
  thunk->setSelfAccessKind(newDecl->getSelfAccessKind());
  if (newDecl->isStatic()) thunk->setStatic();
  thunk->getAttrs().add(
      new (newDecl->getASTContext()) TransparentAttr(/*IsImplicit=*/true));

  return thunk;
}

ConcreteDeclRef
ClangImporter::getCXXFunctionTemplateSpecialization(SubstitutionMap subst,
                                                    ValueDecl *decl) {
  PrettyStackTraceDeclAndSubst trace("specializing", subst, decl);

  assert(isa<clang::FunctionTemplateDecl>(decl->getClangDecl()) &&
         "This API should only be used with function templates.");

  auto *newFn =
      decl->getASTContext()
          .getClangModuleLoader()
          ->instantiateCXXFunctionTemplate(
              decl->getASTContext(),
              const_cast<clang::FunctionTemplateDecl *>(
                  cast<clang::FunctionTemplateDecl>(decl->getClangDecl())),
              subst);
  // We failed to specialize this function template. The compiler is going to
  // exit soon. Return something valid in the meantime.
  if (!newFn)
    return ConcreteDeclRef(decl);

  if (Impl.specializedFunctionTemplates.count(newFn))
    return ConcreteDeclRef(Impl.specializedFunctionTemplates[newFn]);

  auto newDecl = cast_or_null<ValueDecl>(
      decl->getASTContext().getClangModuleLoader()->importDeclDirectly(
          newFn));

  if (auto fn = dyn_cast<AbstractFunctionDecl>(newDecl)) {
    if (!subst.empty()) {
      newDecl = rewriteIntegerTypes(subst, decl, fn);
    }
  }

  if (auto fn = dyn_cast<FuncDecl>(decl)) {
    newDecl = addThunkForDependentTypes(fn, cast<FuncDecl>(newDecl));
  }

  if (auto fn = dyn_cast<FuncDecl>(decl)) {
    if (newFn->getNumParams() != fn->getParameters()->size()) {
      newDecl = generateThunkForExtraMetatypes(subst, fn,
                                               cast<FuncDecl>(newDecl));
    }
  }

  Impl.specializedFunctionTemplates[newFn] = newDecl;
  return ConcreteDeclRef(newDecl);
}

FuncDecl *ClangImporter::getCXXSynthesizedOperatorFunc(FuncDecl *decl) {
  // `decl` is not an operator, it is a regular function which has a
  // name that starts with `__operator`. We were asked for a
  // corresponding synthesized Swift operator, so let's retrieve it.

  // The synthesized Swift operator was added as an alternative decl
  // for `func`.
  auto alternateDecls = Impl.getAlternateDecls(decl);
  // Did we actually synthesize an operator for `func`?
  if (alternateDecls.empty())
    return nullptr;
  // If we did, then we should have only synthesized one.
  assert(alternateDecls.size() == 1 &&
         "expected only the synthesized operator as an alternative");

  auto synthesizedOperator = alternateDecls.front();
  assert(synthesizedOperator->isOperator() &&
         "expected the alternative to be a synthesized operator");
  return cast<FuncDecl>(synthesizedOperator);
}

bool ClangImporter::isSynthesizedAndVisibleFromAllModules(
    const clang::Decl *decl) {
  return Impl.synthesizedAndAlwaysVisibleDecls.contains(decl);
}

bool ClangImporter::isCXXMethodMutating(const clang::CXXMethodDecl *method) {
  if (isa<clang::CXXConstructorDecl>(method) || !method->isConst())
    return true;
  if (isAnnotatedWith(method, "mutating"))
    return true;
  if (method->getParent()->hasMutableFields()) {
    if (isAnnotatedWith(method, "nonmutating"))
      return false;
    // FIXME(rdar://91961524): figure out a way to handle mutable fields
    // without breaking classes from the C++ standard library (e.g.
    // `std::string` which has a mutable member in old libstdc++ version used on
    // CentOS 7)
    return false;
  }
  return false;
}

bool ClangImporter::isUnsafeCXXMethod(const FuncDecl *func) {
  if (!func->hasClangNode())
    return false;
  auto clangDecl = func->getClangNode().getAsDecl();
  if (!clangDecl)
    return false;
  auto cxxMethod = dyn_cast<clang::CXXMethodDecl>(clangDecl);
  if (!cxxMethod)
    return false;
  if (!func->hasName())
    return false;
  auto id = func->getBaseName().userFacingName();
  return id.starts_with("__") && id.ends_with("Unsafe");
}

bool ClangImporter::isAnnotatedWith(const clang::CXXMethodDecl *method,
                                    StringRef attr) {
  return method->hasAttrs() &&
         llvm::any_of(method->getAttrs(), [attr](clang::Attr *a) {
           if (auto swiftAttr = dyn_cast<clang::SwiftAttrAttr>(a)) {
             return swiftAttr->getAttribute() == attr;
           }
           return false;
         });
}

FuncDecl *
ClangImporter::getDefaultArgGenerator(const clang::ParmVarDecl *param) {
  auto it = Impl.defaultArgGenerators.find(param);
  if (it != Impl.defaultArgGenerators.end())
    return it->second;
  return nullptr;
}

SwiftLookupTable *
ClangImporter::findLookupTable(const clang::Module *clangModule) {
  return Impl.findLookupTable(clangModule);
}

/// Determine the effective Clang context for the given Swift nominal type.
EffectiveClangContext
ClangImporter::getEffectiveClangContext(const NominalTypeDecl *nominal) {
  return Impl.getEffectiveClangContext(nominal);
}

Decl *ClangImporter::importDeclDirectly(const clang::NamedDecl *decl) {
  return Impl.importDecl(decl, Impl.CurrentVersion);
}

ValueDecl *ClangImporter::Implementation::importBaseMemberDecl(
    ValueDecl *decl, DeclContext *newContext) {
  // Make sure we don't clone the decl again for this class, as that would
  // result in multiple definitions of the same symbol.
  std::pair<ValueDecl *, DeclContext *> key = {decl, newContext};
  auto known = clonedBaseMembers.find(key);
  if (known == clonedBaseMembers.end()) {
    ValueDecl *cloned = cloneBaseMemberDecl(decl, newContext);
    known = clonedBaseMembers.insert({key, cloned}).first;
  }

  return known->second;
}

size_t ClangImporter::Implementation::getImportedBaseMemberDeclArity(
    const ValueDecl *valueDecl) {
  if (auto *func = dyn_cast<FuncDecl>(valueDecl)) {
    if (auto *params = func->getParameters()) {
      return params->size();
    }
  }
  return 0;
}

ValueDecl *ClangImporter::importBaseMemberDecl(ValueDecl *decl,
                                               DeclContext *newContext) {
  return Impl.importBaseMemberDecl(decl, newContext);
}

void ClangImporter::diagnoseTopLevelValue(const DeclName &name) {
  Impl.diagnoseTopLevelValue(name);
}

void ClangImporter::diagnoseMemberValue(const DeclName &name,
                                        const Type &baseType) {

  // Return early for any type that namelookup::extractDirectlyReferencedNominalTypes
  // does not know how to handle.
  if (!(baseType->getAnyNominal() ||
        baseType->is<ExistentialType>() ||
        baseType->is<UnboundGenericType>() ||
        baseType->is<ArchetypeType>() ||
        baseType->is<ProtocolCompositionType>() ||
        baseType->is<TupleType>()))
    return;

  SmallVector<NominalTypeDecl *, 4> nominalTypesToLookInto;
  namelookup::extractDirectlyReferencedNominalTypes(baseType,
                                                    nominalTypesToLookInto);
  for (auto containerDecl : nominalTypesToLookInto) {
    const clang::Decl *clangContainerDecl = containerDecl->getClangDecl();
    if (isa_and_nonnull<clang::DeclContext>(clangContainerDecl)) {
      Impl.diagnoseMemberValue(name,
                               cast<clang::DeclContext>(clangContainerDecl));
    }

    if (Impl.ImportForwardDeclarations) {
      const clang::Decl *clangContainerDecl = containerDecl->getClangDecl();
      if (const clang::ObjCInterfaceDecl *objCInterfaceDecl =
              llvm::dyn_cast_or_null<clang::ObjCInterfaceDecl>(
                  clangContainerDecl); objCInterfaceDecl && !objCInterfaceDecl->hasDefinition()) {
        // Emit a diagnostic about how the base type represents a forward
        // declared ObjC interface and is in all likelihood missing members.
        // We only attach this diagnostic in diagnoseMemberValue rather than
        // in SwiftDeclConverter because it is only relevant when the user
        // tries to access an unavailable member.
        Impl.addImportDiagnostic(
            objCInterfaceDecl,
            Diagnostic(
                diag::
                    placeholder_for_forward_declared_interface_member_access_failure,
                objCInterfaceDecl->getName()),
            objCInterfaceDecl->getSourceRange().getBegin());
        // Emit any diagnostics attached to the source Clang node (ie. forward
        // declaration here note)
        Impl.diagnoseTargetDirectly(clangContainerDecl);
      } else if (const clang::ObjCProtocolDecl *objCProtocolDecl =
                     llvm::dyn_cast_or_null<clang::ObjCProtocolDecl>(
                         clangContainerDecl); objCProtocolDecl && !objCProtocolDecl->hasDefinition()) {
        // Same as above but for protocols
        Impl.addImportDiagnostic(
            objCProtocolDecl,
            Diagnostic(
                diag::
                    placeholder_for_forward_declared_protocol_member_access_failure,
                objCProtocolDecl->getName()),
            objCProtocolDecl->getSourceRange().getBegin());
        Impl.diagnoseTargetDirectly(clangContainerDecl);
      }
    }
  }
}

SourceLoc ClangImporter::importSourceLocation(clang::SourceLocation loc) {
  auto &bufferImporter = Impl.getBufferImporterForDiagnostics();
  return bufferImporter.resolveSourceLocation(
      getClangASTContext().getSourceManager(), loc);
}

static bool hasImportAsRefAttr(const clang::RecordDecl *decl) {
  return decl->hasAttrs() && llvm::any_of(decl->getAttrs(), [](auto *attr) {
           if (auto swiftAttr = dyn_cast<clang::SwiftAttrAttr>(attr))
             return swiftAttr->getAttribute() == "import_reference" ||
                    // TODO: Remove this once libSwift hosttools no longer
                    // requires it.
                    swiftAttr->getAttribute() == "import_as_ref";
           return false;
         });
}

// TODO: Move all these utility functions in a new file ClangImporterUtils.h
// rdar://138803759
static bool hasImmortalAtts(const clang::RecordDecl *decl) {
  return decl->hasAttrs() && llvm::any_of(decl->getAttrs(), [](auto *attr) {
           if (auto swiftAttr = dyn_cast<clang::SwiftAttrAttr>(attr))
             return swiftAttr->getAttribute() == "retain:immortal" ||
                    swiftAttr->getAttribute() == "release:immortal";
           return false;
         });
}

// Is this a pointer to a foreign reference type.
bool importer::isForeignReferenceTypeWithoutImmortalAttrs(const clang::QualType type) {
  if (!type->isPointerType())
    return false;

  auto pointeeType =
      dyn_cast<clang::RecordType>(type->getPointeeType().getCanonicalType());
  if (pointeeType == nullptr)
    return false;

  return hasImportAsRefAttr(pointeeType->getDecl()) &&
         !hasImmortalAtts(pointeeType->getDecl());
}

// Is this a pointer to a foreign reference type.
static bool isForeignReferenceType(const clang::QualType type) {
  if (!type->isPointerType())
    return false;

  auto pointeeType =
      dyn_cast<clang::RecordType>(type->getPointeeType().getCanonicalType());
  if (pointeeType == nullptr)
    return false;

  return hasImportAsRefAttr(pointeeType->getDecl());
}

static bool hasSwiftAttribute(const clang::Decl *decl, StringRef attr) {
  if (decl->hasAttrs() && llvm::any_of(decl->getAttrs(), [&](auto *A) {
        if (auto swiftAttr = dyn_cast<clang::SwiftAttrAttr>(A))
          return swiftAttr->getAttribute() == attr;
        return false;
      }))
    return true;

  if (auto *P = dyn_cast<clang::ParmVarDecl>(decl)) {
    bool found = false;
    findSwiftAttributes(P->getOriginalType(),
                        [&](const clang::SwiftAttrAttr *swiftAttr) {
                          found |= swiftAttr->getAttribute() == attr;
                        });
    return found;
  }

  return false;
}

static bool hasOwnedValueAttr(const clang::RecordDecl *decl) {
  return hasSwiftAttribute(decl, "import_owned");
}

bool importer::hasUnsafeAPIAttr(const clang::Decl *decl) {
  return hasSwiftAttribute(decl, "import_unsafe");
}

static bool hasIteratorAPIAttr(const clang::Decl *decl) {
  return hasSwiftAttribute(decl, "import_iterator");
}

static bool hasNonCopyableAttr(const clang::RecordDecl *decl) {
  return hasSwiftAttribute(decl, "~Copyable");
}

bool importer::hasNonEscapableAttr(const clang::RecordDecl *decl) {
  return hasSwiftAttribute(decl, "~Escapable");
}

bool importer::hasEscapableAttr(const clang::RecordDecl *decl) {
  return hasSwiftAttribute(decl, "Escapable");
}

/// Recursively checks that there are no pointers in any fields or base classes.
/// Does not check C++ records with specific API annotations.
static bool hasPointerInSubobjects(const clang::CXXRecordDecl *decl) {
  clang::PrettyStackTraceDecl trace(decl, clang::SourceLocation(),
                                    decl->getASTContext().getSourceManager(),
                                    "looking for pointers in subobjects of");

  // Probably a class template that has not yet been specialized:
  if (!decl->getDefinition())
    return false;

  auto checkType = [](clang::QualType t) {
    if (t->isPointerType())
      return true;

    if (auto recordType = dyn_cast<clang::RecordType>(t.getCanonicalType())) {
      if (auto cxxRecord =
              dyn_cast<clang::CXXRecordDecl>(recordType->getDecl())) {
        if (hasImportAsRefAttr(cxxRecord) || hasOwnedValueAttr(cxxRecord) ||
            hasUnsafeAPIAttr(cxxRecord))
          return false;

        if (hasIteratorAPIAttr(cxxRecord) || isIterator(cxxRecord))
          return true;

        if (hasPointerInSubobjects(cxxRecord))
          return true;
      }
    }

    return false;
  };

  for (auto field : decl->fields()) {
    if (checkType(field->getType()))
      return true;
  }

  for (auto base : decl->bases()) {
    if (checkType(base.getType()))
      return true;
  }

  return false;
}

bool importer::isViewType(const clang::CXXRecordDecl *decl) {
  return !hasOwnedValueAttr(decl) && hasPointerInSubobjects(decl);
}

static bool copyConstructorIsDefaulted(const clang::CXXRecordDecl *decl) {
  auto ctor = llvm::find_if(decl->ctors(), [](clang::CXXConstructorDecl *ctor) {
    return ctor->isCopyConstructor();
  });

  assert(ctor != decl->ctor_end());
  return ctor->isDefaulted();
}

static bool copyAssignOperatorIsDefaulted(const clang::CXXRecordDecl *decl) {
  auto copyAssignOp = llvm::find_if(decl->decls(), [](clang::Decl *member) {
    if (auto method = dyn_cast<clang::CXXMethodDecl>(member))
      return method->isCopyAssignmentOperator();
    return false;
  });

  assert(copyAssignOp != decl->decls_end());
  return cast<clang::CXXMethodDecl>(*copyAssignOp)->isDefaulted();
}

/// Recursively checks that there are no user-provided copy constructors or
/// destructors in any fields or base classes.
/// Does not check C++ records with specific API annotations.
static bool isSufficientlyTrivial(const clang::CXXRecordDecl *decl) {
  // Probably a class template that has not yet been specialized:
  if (!decl->getDefinition())
    return true;

  if ((decl->hasUserDeclaredCopyConstructor() &&
       !copyConstructorIsDefaulted(decl)) ||
      (decl->hasUserDeclaredCopyAssignment() &&
       !copyAssignOperatorIsDefaulted(decl)) ||
      (decl->hasUserDeclaredDestructor() && decl->getDestructor() &&
       !decl->getDestructor()->isDefaulted()))
    return false;

  auto checkType = [](clang::QualType t) {
    if (auto recordType = dyn_cast<clang::RecordType>(t.getCanonicalType())) {
      if (auto cxxRecord =
              dyn_cast<clang::CXXRecordDecl>(recordType->getDecl())) {
        if (hasImportAsRefAttr(cxxRecord) || hasOwnedValueAttr(cxxRecord) ||
            hasUnsafeAPIAttr(cxxRecord))
          return true;

        if (!isSufficientlyTrivial(cxxRecord))
          return false;
      }
    }

    return true;
  };

  for (auto field : decl->fields()) {
    if (!checkType(field->getType()))
      return false;
  }

  for (auto base : decl->bases()) {
    if (!checkType(base.getType()))
      return false;
  }

  return true;
}

/// Checks if a record provides the required value type lifetime operations
/// (copy and destroy).
static bool hasCopyTypeOperations(const clang::CXXRecordDecl *decl) {
  // Hack for a base type of std::optional from the Microsoft standard library.
  if (decl->isInStdNamespace() && decl->getIdentifier() &&
      decl->getName() == "_Optional_construct_base")
    return true;
  // Hack for std::vector::const_iterator from libstdc++, which uses an extra
  // parameter on its copy constructor, which has a defaulted enable_if value.
  auto namespaceContext = dyn_cast_or_null<clang::NamespaceDecl>(
      decl->getEnclosingNamespaceContext());
  if (namespaceContext && namespaceContext->getIdentifier() &&
      namespaceContext->getName() == "__gnu_cxx" && decl->getIdentifier() &&
      decl->getName() == "__normal_iterator")
    return true;

  // If we have no way of copying the type we can't import the class
  // at all because we cannot express the correct semantics as a swift
  // struct.
  return llvm::any_of(decl->ctors(), [](clang::CXXConstructorDecl *ctor) {
    return ctor->isCopyConstructor() && !ctor->isDeleted() &&
           ctor->getAccess() == clang::AccessSpecifier::AS_public;
  });
}

static bool hasMoveTypeOperations(const clang::CXXRecordDecl *decl) {
  // If we have no way of copying the type we can't import the class
  // at all because we cannot express the correct semantics as a swift
  // struct.
  if (llvm::any_of(decl->ctors(), [](clang::CXXConstructorDecl *ctor) {
        return ctor->isMoveConstructor() &&
               (ctor->isDeleted() || ctor->getAccess() != clang::AS_public);
      }))
    return false;

  return llvm::any_of(decl->ctors(), [](clang::CXXConstructorDecl *ctor) {
    return ctor->isMoveConstructor();
  });
}

static bool hasDestroyTypeOperations(const clang::CXXRecordDecl *decl) {
  if (auto dtor = decl->getDestructor()) {
    if (dtor->isDeleted() || dtor->getAccess() != clang::AS_public) {
      return false;
    }
    return true;
  }
  return false;
}

static bool hasCustomCopyOrMoveConstructor(const clang::CXXRecordDecl *decl) {
  return decl->hasUserDeclaredCopyConstructor() ||
         decl->hasUserDeclaredMoveConstructor();
}

static bool isSwiftClassType(const clang::CXXRecordDecl *decl) {
  // Swift type must be annotated with external_source_symbol attribute.
  auto essAttr = decl->getAttr<clang::ExternalSourceSymbolAttr>();
  if (!essAttr || essAttr->getLanguage() != "Swift" ||
      essAttr->getDefinedIn().empty() || essAttr->getUSR().empty())
    return false;

  // Ensure that the baseclass is swift::RefCountedClass.
  auto baseDecl = decl;
  do {
    if (baseDecl->getNumBases() != 1)
      return false;
    auto baseClassSpecifier = *baseDecl->bases_begin();
    auto Ty = baseClassSpecifier.getType();
    auto nextBaseDecl = Ty->getAsCXXRecordDecl();
    if (!nextBaseDecl)
      return false;
    baseDecl = nextBaseDecl;
  } while (baseDecl->getName() != "RefCountedClass");

  return true;
}

CxxRecordSemanticsKind
CxxRecordSemantics::evaluate(Evaluator &evaluator,
                             CxxRecordSemanticsDescriptor desc) const {
  const auto *decl = desc.decl;

  if (hasImportAsRefAttr(decl)) {
    return CxxRecordSemanticsKind::Reference;
  }

  auto cxxDecl = dyn_cast<clang::CXXRecordDecl>(decl);
  if (!cxxDecl) {
    return CxxRecordSemanticsKind::Trivial;
  }

  if (isSwiftClassType(cxxDecl))
    return CxxRecordSemanticsKind::SwiftClassType;

  if (!hasDestroyTypeOperations(cxxDecl) ||
      (!hasCopyTypeOperations(cxxDecl) && !hasMoveTypeOperations(cxxDecl))) {
    if (hasUnsafeAPIAttr(cxxDecl))
      desc.ctx.Diags.diagnose({}, diag::api_pattern_attr_ignored,
                              "import_unsafe", decl->getNameAsString());
    if (hasOwnedValueAttr(cxxDecl))
      desc.ctx.Diags.diagnose({}, diag::api_pattern_attr_ignored,
                              "import_owned", decl->getNameAsString());
    if (hasIteratorAPIAttr(cxxDecl))
      desc.ctx.Diags.diagnose({}, diag::api_pattern_attr_ignored,
                              "import_iterator", decl->getNameAsString());

    return CxxRecordSemanticsKind::MissingLifetimeOperation;
  }

  if (hasNonCopyableAttr(cxxDecl) && hasMoveTypeOperations(cxxDecl)) {
    return CxxRecordSemanticsKind::MoveOnly;
  }

  if (hasOwnedValueAttr(cxxDecl)) {
    return CxxRecordSemanticsKind::Owned;
  }

  if (hasIteratorAPIAttr(cxxDecl) || isIterator(cxxDecl)) {
    return CxxRecordSemanticsKind::Iterator;
  }
  
  if (hasCopyTypeOperations(cxxDecl)) {
    return CxxRecordSemanticsKind::Owned;
  }

  if (hasMoveTypeOperations(cxxDecl)) {
    return CxxRecordSemanticsKind::MoveOnly;
  }

  if (isSufficientlyTrivial(cxxDecl)) {
    return CxxRecordSemanticsKind::Trivial;
  }

  llvm_unreachable("Could not classify C++ type.");
}

ValueDecl *
CxxRecordAsSwiftType::evaluate(Evaluator &evaluator,
                               CxxRecordSemanticsDescriptor desc) const {
  auto cxxDecl = dyn_cast<clang::CXXRecordDecl>(desc.decl);
  if (!cxxDecl)
    return nullptr;
  if (!isSwiftClassType(cxxDecl))
    return nullptr;

  SmallVector<ValueDecl *, 1> results;
  auto *essaAttr = cxxDecl->getAttr<clang::ExternalSourceSymbolAttr>();
  auto *mod = desc.ctx.getModuleByName(essaAttr->getDefinedIn());
  if (!mod) {
    // TODO: warn about missing 'import'.
    return nullptr;
  }
  // FIXME: Support renamed declarations.
  auto swiftName = cxxDecl->getName();
  // FIXME: handle nested Swift types once they're supported.
  mod->lookupValue(desc.ctx.getIdentifier(swiftName), NLKind::UnqualifiedLookup,
                   results);
  if (results.size() == 1) {
    if (dyn_cast<ClassDecl>(results[0]))
      return results[0];
  }
  return nullptr;
}

bool anySubobjectsSelfContained(const clang::CXXRecordDecl *decl) {
  // std::pair and std::tuple might have copy and move constructors, or base
  // classes with copy and move constructors, but they are not self-contained
  // types, e.g. `std::pair<UnsafeType, T>`.
  if (decl->isInStdNamespace() &&
      (decl->getName() == "pair" || decl->getName() == "tuple"))
    return false;

  if (!decl->getDefinition())
    return false;

  if (hasCustomCopyOrMoveConstructor(decl) || hasOwnedValueAttr(decl))
    return true;
  
  auto checkType = [](clang::QualType t) {
    if (auto recordType = dyn_cast<clang::RecordType>(t.getCanonicalType())) {
      if (auto cxxRecord =
              dyn_cast<clang::CXXRecordDecl>(recordType->getDecl())) {
        return anySubobjectsSelfContained(cxxRecord);
      }
    }

    return false;
  };

  for (auto field : decl->fields()) {
    if (checkType(field->getType()))
      return true;
  }

  for (auto base : decl->bases()) {
    if (checkType(base.getType()))
      return true;
  }
  
  return false;
}

bool IsSafeUseOfCxxDecl::evaluate(Evaluator &evaluator,
                                  SafeUseOfCxxDeclDescriptor desc) const {
  const clang::Decl *decl = desc.decl;

  if (auto method = dyn_cast<clang::CXXMethodDecl>(decl)) {
    // The user explicitly asked us to import this method.
    if (hasUnsafeAPIAttr(method))
      return true;

    // If it's a static method, it cannot project anything. It's fine.
    if (method->isOverloadedOperator() || method->isStatic() ||
        isa<clang::CXXConstructorDecl>(decl))
      return true;

    if (isForeignReferenceType(method->getReturnType()))
      return true;

    // begin and end methods likely return an interator, so they're unsafe. This
    // is required so that automatic the conformance to RAC works properly.
    if (method->getNameAsString() == "begin" ||
        method->getNameAsString() == "end")
      return false;

    auto parentQualType = method
      ->getParent()->getTypeForDecl()->getCanonicalTypeUnqualified();

    bool parentIsSelfContained =
      !isForeignReferenceType(parentQualType) &&
      anySubobjectsSelfContained(method->getParent());

    // If it returns a pointer or reference from an owned parent, that's a
    // projection (unsafe).
    if (method->getReturnType()->isPointerType() ||
        method->getReturnType()->isReferenceType())
      return !parentIsSelfContained;

    // Check if it's one of the known unsafe methods we currently
    // mark as safe by default.
    if (isUnsafeStdMethod(method))
      return false;

    // Try to figure out the semantics of the return type. If it's a
    // pointer/iterator, it's unsafe.
    if (auto returnType = dyn_cast<clang::RecordType>(
            method->getReturnType().getCanonicalType())) {
      if (auto cxxRecordReturnType =
              dyn_cast<clang::CXXRecordDecl>(returnType->getDecl())) {
        if (isSwiftClassType(cxxRecordReturnType))
          return true;

        if (hasIteratorAPIAttr(cxxRecordReturnType) ||
            isIterator(cxxRecordReturnType))
          return false;

        // Mark this as safe to help our diganostics down the road.
        if (!cxxRecordReturnType->getDefinition()) {
          return true;
        }

        // A projection of a view type (such as a string_view) from a self
        // contained parent is a proejction (unsafe).
        if (!anySubobjectsSelfContained(cxxRecordReturnType) &&
            isViewType(cxxRecordReturnType)) {
          return !parentIsSelfContained;
        }
      }
    }
  }

  // Otherwise, it's safe.
  return true;
}

void swift::simple_display(llvm::raw_ostream &out,
                           CxxRecordSemanticsDescriptor desc) {
  out << "Matching API semantics of C++ record '"
      << desc.decl->getNameAsString() << "'.\n";
}

SourceLoc swift::extractNearestSourceLoc(CxxRecordSemanticsDescriptor desc) {
  return SourceLoc();
}

void swift::simple_display(llvm::raw_ostream &out,
                           SafeUseOfCxxDeclDescriptor desc) {
  out << "Checking if '";
  if (auto namedDecl = dyn_cast<clang::NamedDecl>(desc.decl))
    out << namedDecl->getNameAsString();
  else
    out << "<invalid decl>";
  out << "' is safe to use in context.\n";
}

SourceLoc swift::extractNearestSourceLoc(SafeUseOfCxxDeclDescriptor desc) {
  return SourceLoc();
}

CustomRefCountingOperationResult CustomRefCountingOperation::evaluate(
    Evaluator &evaluator, CustomRefCountingOperationDescriptor desc) const {
  auto swiftDecl = desc.decl;
  auto operation = desc.kind;
  auto &ctx = swiftDecl->getASTContext();

  std::string operationStr = operation == CustomRefCountingOperationKind::retain
                                 ? "retain:"
                                 : "release:";

  auto decl = cast<clang::RecordDecl>(swiftDecl->getClangDecl());
  if (!decl->hasAttrs())
    return {CustomRefCountingOperationResult::noAttribute, nullptr, ""};

  llvm::SmallVector<const clang::SwiftAttrAttr *, 1> retainReleaseAttrs;
  for (auto *attr : decl->getAttrs()) {
    if (auto swiftAttr = llvm::dyn_cast<clang::SwiftAttrAttr>(attr)) {
      if (swiftAttr->getAttribute().starts_with(operationStr)) {
        retainReleaseAttrs.push_back(swiftAttr);
      }
    }
  }

  if (retainReleaseAttrs.empty()) {
    return {CustomRefCountingOperationResult::noAttribute, nullptr, ""};
  } else if (retainReleaseAttrs.size() > 1) {
    return {CustomRefCountingOperationResult::tooManyAttributes, nullptr, ""};
  }

  auto name = retainReleaseAttrs.front()
                  ->getAttribute()
                  .drop_front(StringRef(operationStr).size())
                  .str();

  if (name == "immortal")
    return {CustomRefCountingOperationResult::immortal, nullptr, name};

  llvm::SmallVector<ValueDecl *, 1> results;
  auto *clangMod = swiftDecl->getClangDecl()->getOwningModule();
  if (clangMod && clangMod->isSubModule())
    clangMod = clangMod->getTopLevelModule();
  if (clangMod) {
    auto parentModule = ctx.getClangModuleLoader()->getWrapperForModule(clangMod);
    ctx.lookupInModule(parentModule, name, results);
  } else {
    // There is no Clang module for this declaration, so perform lookup from
    // the main module. This will find declarations from the bridging header.
    namelookup::lookupInModule(
        ctx.MainModule, ctx.getIdentifier(name), results,
        NLKind::UnqualifiedLookup, namelookup::ResolutionKind::Overloadable,
        ctx.MainModule, SourceLoc(), NL_UnqualifiedDefault);

    // Filter out any declarations that didn't come from Clang.
    auto newEnd = std::remove_if(results.begin(), results.end(), [&](ValueDecl *decl) {
      return !decl->getClangDecl();
    });
    results.erase(newEnd, results.end());
  }
  if (results.size() == 1)
    return {CustomRefCountingOperationResult::foundOperation, results.front(),
            name};

  if (results.empty())
    return {CustomRefCountingOperationResult::notFound, nullptr, name};

  return {CustomRefCountingOperationResult::tooManyFound, nullptr, name};
}

void ClangImporter::withSymbolicFeatureEnabled(
    llvm::function_ref<void(void)> callback) {
  llvm::SaveAndRestore<bool> oldImportSymbolicCXXDecls(
      Impl.importSymbolicCXXDecls, true);
  Impl.nameImporter->enableSymbolicImportFeature(true);
  auto importedDeclsCopy = Impl.ImportedDecls;
  Impl.ImportedDecls.clear();
  callback();
  Impl.ImportedDecls = std::move(importedDeclsCopy);
  Impl.nameImporter->enableSymbolicImportFeature(
      oldImportSymbolicCXXDecls.get());
}

bool ClangImporter::isSymbolicImportEnabled() const {
  return Impl.importSymbolicCXXDecls;
}

const clang::TypedefType *ClangImporter::getTypeDefForCXXCFOptionsDefinition(
    const clang::Decl *candidateDecl) {

  if (!Impl.SwiftContext.LangOpts.EnableCXXInterop)
    return nullptr;

  auto enumDecl = dyn_cast<clang::EnumDecl>(candidateDecl);
  if (!enumDecl)
    return nullptr;

  if (!enumDecl->getDeclName().isEmpty())
    return nullptr;

  const clang::ElaboratedType *elaboratedType =
      dyn_cast<clang::ElaboratedType>(enumDecl->getIntegerType().getTypePtr());
  if (auto typedefType =
          elaboratedType
              ? dyn_cast<clang::TypedefType>(elaboratedType->desugar())
              : dyn_cast<clang::TypedefType>(
                    enumDecl->getIntegerType().getTypePtr())) {
    auto enumExtensibilityAttr =
        elaboratedType
            ? enumDecl->getAttr<clang::EnumExtensibilityAttr>()
            : typedefType->getDecl()->getAttr<clang::EnumExtensibilityAttr>();
    const bool hasFlagEnumAttr =
        elaboratedType ? enumDecl->hasAttr<clang::FlagEnumAttr>()
                       : typedefType->getDecl()->hasAttr<clang::FlagEnumAttr>();

    if (enumExtensibilityAttr &&
        enumExtensibilityAttr->getExtensibility() ==
            clang::EnumExtensibilityAttr::Open &&
        hasFlagEnumAttr) {
      return Impl.isUnavailableInSwift(typedefType->getDecl()) ? typedefType
                                                               : nullptr;
    }
  }

  return nullptr;
}

bool importer::requiresCPlusPlus(const clang::Module *module) {
  // The libc++ modulemap doesn't currently declare the requirement.
  if (isCxxStdModule(module))
    return true;

  // Modulemaps often declare the requirement for the top-level module only.
  if (auto parent = module->Parent) {
    if (requiresCPlusPlus(parent))
      return true;
  }

  return llvm::any_of(module->Requirements, [](clang::Module::Requirement req) {
    return req.FeatureName == "cplusplus";
  });
}

bool importer::isCxxStdModule(const clang::Module *module) {
  return isCxxStdModule(module->getTopLevelModuleName(),
                        module->getTopLevelModule()->IsSystem);
}

bool importer::isCxxStdModule(StringRef moduleName, bool IsSystem) {
  if (moduleName == "std")
    return true;
  // In recent libc++ versions the module is split into multiple top-level
  // modules (std_vector, std_utility, etc).
  if (IsSystem && moduleName.starts_with("std_")) {
    if (moduleName == "std_errno_h")
      return false;
    return true;
  }
  return false;
}

std::optional<clang::QualType>
importer::getCxxReferencePointeeTypeOrNone(const clang::Type *type) {
  if (type->isReferenceType())
    return type->getPointeeType();
  return {};
}

bool importer::isCxxConstReferenceType(const clang::Type *type) {
  auto pointeeType = getCxxReferencePointeeTypeOrNone(type);
  return pointeeType && pointeeType->isConstQualified();
}
