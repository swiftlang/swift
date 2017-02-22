//===--- ClangImporter.cpp - Import Clang Modules -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
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
#include "swift/ClangImporter/ClangModule.h"
#include "IAMInference.h"
#include "ImporterImpl.h"
#include "ClangDiagnosticConsumer.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Platform.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Basic/Version.h"
#include "swift/ClangImporter/ClangImporterOptions.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"
#include "swift/Config.h"
#include "swift/Strings.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Mangle.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/Module.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/Version.h"
#include "clang/CodeGen/ObjectFilePCHContainerOperations.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/Utils.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTWriter.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Parse/Parser.h"
#include "clang/Rewrite/Frontend/FrontendActions.h"
#include "clang/Rewrite/Frontend/Rewriters.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/CrashRecoveryContext.h"
#include "llvm/Support/Path.h"
#include <algorithm>
#include <memory>

using namespace swift;
using namespace importer;

// Commonly-used Clang classes.
using clang::CompilerInstance;
using clang::CompilerInvocation;

#pragma mark Internal data structures

namespace {
  class HeaderImportCallbacks : public clang::PPCallbacks {
    ClangImporter &Importer;
    ClangImporter::Implementation &Impl;
  public:
    HeaderImportCallbacks(ClangImporter &importer,
                          ClangImporter::Implementation &impl)
      : Importer(importer), Impl(impl) {}

    void handleImport(const clang::Module *imported) {
      if (!imported)
        return;
      ModuleDecl *nativeImported = Impl.finishLoadingClangModule(Importer, imported,
        /*preferAdapter=*/true);
      Impl.ImportedHeaderExports.push_back({ /*filter=*/{}, nativeImported });
    }

    void InclusionDirective(clang::SourceLocation HashLoc,
                                    const clang::Token &IncludeTok,
                                    StringRef FileName,
                                    bool IsAngled,
                                    clang::CharSourceRange FilenameRange,
                                    const clang::FileEntry *File,
                                    StringRef SearchPath,
                                    StringRef RelativePath,
                                    const clang::Module *Imported) override {
      handleImport(Imported);
      if (!Imported && File)
        Importer.addDependency(File->getName());
    }

    void moduleImport(clang::SourceLocation ImportLoc,
                              clang::ModuleIdPath Path,
                              const clang::Module *Imported) override {
      handleImport(Imported);
    }
  };

  class ASTReaderCallbacks : public clang::ASTReaderListener {
    ClangImporter &Importer;
  public:
    explicit ASTReaderCallbacks(ClangImporter &importer) : Importer(importer) {}

    bool needsInputFileVisitation() override { return true; }

    bool visitInputFile(StringRef file, bool isSystem,
                        bool isOverridden, bool isExplicitModule) override {
      if (!isOverridden)
        Importer.addDependency(file);
      return true;
    }
  };

  class PCHDeserializationCallbacks : public clang::ASTDeserializationListener {
    ClangImporter::Implementation &Impl;
  public:
    explicit PCHDeserializationCallbacks(ClangImporter::Implementation &impl)
      : Impl(impl) {}
    void ModuleImportRead(clang::serialization::SubmoduleID ID,
                          clang::SourceLocation ImportLoc) {
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
  public:
    explicit ParsingAction(ASTContext &ctx,
                           ClangImporter &importer,
                           ClangImporter::Implementation &impl)
      : Ctx(ctx), Importer(importer), Impl(impl) {}
    std::unique_ptr<clang::ASTConsumer>
    CreateASTConsumer(clang::CompilerInstance &CI, StringRef InFile) override {
      return llvm::make_unique<HeaderParsingASTConsumer>(Impl);
    }
    bool BeginSourceFileAction(CompilerInstance &CI,
                               StringRef Filename) override {
      // Prefer frameworks over plain headers.
      // We add search paths here instead of when building the initial invocation
      // so that (a) we use the same code as search paths for imported modules,
      // and (b) search paths are always added after -Xcc options.
      SearchPathOptions &searchPathOpts = Ctx.SearchPathOpts;
      for (auto path : searchPathOpts.FrameworkSearchPaths)
        Importer.addSearchPath(path, /*isFramework*/true);
      for (auto path : searchPathOpts.ImportSearchPaths)
        Importer.addSearchPath(path, /*isFramework*/false);
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
                          const clang::Token &IncludeTok,
                          StringRef FileName,
                          bool IsAngled,
                          clang::CharSourceRange FilenameRange,
                          const clang::FileEntry *File,
                          StringRef SearchPath,
                          StringRef RelativePath,
                          const clang::Module *Imported) override {
    if (!Imported) {
      if (File)
        Impl.BridgeHeaderFiles.insert(File);
      return;
    }
    // Synthesize identifier locations.
    SmallVector<clang::SourceLocation, 4> IdLocs;
    for (unsigned I = 0, E = getNumModuleIdentifiers(Imported); I != E; ++I)
      IdLocs.push_back(HashLoc);
    handleImport(HashLoc, IdLocs, Imported);
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
} // end anonymous namespace

void ClangImporter::Implementation::addBridgeHeaderTopLevelDecls(
    clang::Decl *D) {
  if (shouldIgnoreBridgeHeaderTopLevelDecl(D))
    return;

  BridgeHeaderTopLevelDecls.push_back(D);
}

bool ClangImporter::Implementation::shouldIgnoreBridgeHeaderTopLevelDecl(
    clang::Decl *D) {
  // Ignore forward references;
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

ClangImporter::ClangImporter(ASTContext &ctx,
                             const ClangImporterOptions &clangImporterOpts,
                             DependencyTracker *tracker)
  : ClangModuleLoader(tracker),
    Impl(*new Implementation(ctx, clangImporterOpts))
{
}

ClangImporter::~ClangImporter() {
  delete &Impl;
}

void ClangImporter::setTypeResolver(LazyResolver &resolver) {
  Impl.setTypeResolver(&resolver);
}

void ClangImporter::clearTypeResolver() {
  Impl.setTypeResolver(nullptr);
}

#pragma mark Module loading

#ifdef NDEBUG
#define SHIMS_INCLUDE_FLAG "-isystem"
#else
#define SHIMS_INCLUDE_FLAG "-I"
#endif

static StringRef
getMinVersionOptNameForDarwinTriple(const llvm::Triple &triple) {
  switch(getDarwinPlatformKind(triple)) {
    case DarwinPlatformKind::MacOS:
      return "-mmacosx-version-min=";
    case DarwinPlatformKind::IPhoneOS:
      return "-mios-version-min=";
    case DarwinPlatformKind::IPhoneOSSimulator:
      return "-mios-simulator-version-min=";
    case DarwinPlatformKind::TvOS:
      return "-mtvos-version-min=";
    case DarwinPlatformKind::TvOSSimulator:
      return "-mtvos-simulator-version-min=";
    case DarwinPlatformKind::WatchOS:
      return "-mwatchos-version-min=";
    case DarwinPlatformKind::WatchOSSimulator:
      return "-mwatchos-simulator-version-min=";
  }
  llvm_unreachable("Unsupported Darwin platform");
}

static void
getNormalInvocationArguments(std::vector<std::string> &invocationArgStrs,
                             ASTContext &ctx,
                             const ClangImporterOptions &importerOpts) {
  const llvm::Triple &triple = ctx.LangOpts.Target;
  SearchPathOptions &searchPathOpts = ctx.SearchPathOpts;

  auto languageVersion = ctx.LangOpts.EffectiveLanguageVersion;

  if (llvm::sys::path::extension(importerOpts.BridgingHeader).endswith(
        PCH_EXTENSION)) {
    invocationArgStrs.insert(
      invocationArgStrs.end(),
        { "-include-pch", importerOpts.BridgingHeader }
    );
  }

  // Construct the invocation arguments for the current target.
  // Add target-independent options first.
  invocationArgStrs.insert(
      invocationArgStrs.end(),
      {

          // Enable modules
          "-fmodules",
          "-Werror=non-modular-include-in-framework-module",
          "-Xclang", "-fmodule-feature", "-Xclang", "swift",

          // Don't emit LLVM IR.
          "-fsyntax-only",

          // Enable block support.
          "-fblocks",

          languageVersion.preprocessorDefinition("__swift__", {10000, 100, 1}),

          "-fretain-comments-from-system-headers",

          SHIMS_INCLUDE_FLAG, searchPathOpts.RuntimeResourcePath,
      });

  // Set C language options.
  if (triple.isOSDarwin()) {
    invocationArgStrs.insert(invocationArgStrs.end(), {
      // Darwin uses Objective-C ARC.
      "-x", "objective-c", "-std=gnu11", "-fobjc-arc",

      // Define macros that Swift bridging headers use.
      "-DSWIFT_CLASS_EXTRA=__attribute__((annotate(\""
        SWIFT_NATIVE_ANNOTATION_STRING "\")))",
      "-DSWIFT_PROTOCOL_EXTRA=__attribute__((annotate(\""
        SWIFT_NATIVE_ANNOTATION_STRING "\")))",
      "-DSWIFT_EXTENSION_EXTRA=__attribute__((annotate(\""
        SWIFT_NATIVE_ANNOTATION_STRING "\")))",
      "-DSWIFT_ENUM_EXTRA=__attribute__((annotate(\""
        SWIFT_NATIVE_ANNOTATION_STRING "\")))",

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
    });

    // Get the version of this compiler and pass it to
    // C/Objective-C declarations.
    auto V = version::Version::getCurrentCompilerVersion();
    if (!V.empty()) {
      invocationArgStrs.insert(invocationArgStrs.end(), {
        V.preprocessorDefinition("__SWIFT_COMPILER_VERSION",
                                 {1000000000, /*ignored*/0, 1000000, 1000, 1}),
      });
    }
  } else {
    invocationArgStrs.insert(invocationArgStrs.end(), {
      // Non-Darwin platforms don't use the Objective-C runtime, so they can
      // not import Objective-C modules.
      //
      // Just use the most feature-rich C language mode.
      "-x", "c", "-std=gnu11",
    });

    // The module map used for Glibc depends on the target we're compiling for,
    // and is not included in the resource directory with the other implicit
    // module maps. It's at {freebsd|linux}/{arch}/glibc.modulemap.
    SmallString<128> GlibcModuleMapPath;
    GlibcModuleMapPath = searchPathOpts.RuntimeResourcePath;

    // Running without a resource directory is not a supported configuration.
    assert(!GlibcModuleMapPath.empty());

    llvm::sys::path::append(
      GlibcModuleMapPath,
      swift::getPlatformNameForTriple(triple),
      swift::getMajorArchitectureName(triple),
      "glibc.modulemap");

    // Only specify the module map if that file actually exists.
    // It may not--for example in the case that
    // `swiftc -target x86_64-unknown-linux-gnu -emit-ir` is invoked using
    // a Swift compiler not built for Linux targets.
    if (llvm::sys::fs::exists(GlibcModuleMapPath)) {
      invocationArgStrs.push_back(
        (Twine("-fmodule-map-file=") + GlibcModuleMapPath).str());
    } else {
      // FIXME: Emit a warning of some kind.
    }
  }

  if (triple.isOSDarwin()) {
    std::string minVersionBuf;
    llvm::raw_string_ostream minVersionOpt{minVersionBuf};
    minVersionOpt << getMinVersionOptNameForDarwinTriple(triple);

    unsigned major, minor, micro;
    if (triple.isiOS()) {
      triple.getiOSVersion(major, minor, micro);
    } else if (triple.isWatchOS()) {
      triple.getWatchOSVersion(major, minor, micro);
    } else {
      assert(triple.isMacOSX());
      triple.getMacOSXVersion(major, minor, micro);
    }
    minVersionOpt << clang::VersionTuple(major, minor, micro);
    invocationArgStrs.push_back(std::move(minVersionOpt.str()));
  }

  if (searchPathOpts.SDKPath.empty()) {
    invocationArgStrs.push_back("-Xclang");
    invocationArgStrs.push_back("-nostdsysteminc");
  } else {
    // On Darwin, Clang uses -isysroot to specify the include
    // system root. On other targets, it seems to use --sysroot.
    if (triple.isOSDarwin()) {
      invocationArgStrs.push_back("-isysroot");
    } else {
      invocationArgStrs.push_back("--sysroot");
    }
    invocationArgStrs.push_back(searchPathOpts.SDKPath);
  }

  const std::string &moduleCachePath = importerOpts.ModuleCachePath;

  // Set the module and API notes cache paths to the same location.
  if (!moduleCachePath.empty()) {
    invocationArgStrs.push_back("-fmodules-cache-path=");
    invocationArgStrs.back().append(moduleCachePath);

    invocationArgStrs.push_back("-fapinotes-cache-path=");
    invocationArgStrs.back().append(moduleCachePath);
  }

  if (!importerOpts.DisableModulesValidateSystemHeaders) {
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
}

static void
getEmbedBitcodeInvocationArguments(std::vector<std::string> &invocationArgStrs,
                                   ASTContext &ctx,
                                   const ClangImporterOptions &importerOpts) {
  invocationArgStrs.insert(invocationArgStrs.end(), {
    // Backend mode.
    "-fembed-bitcode",

    // ...but Clang isn't doing the emission.
    "-fsyntax-only",

    "-x", "ir",
  });
}

static void
addCommonInvocationArguments(std::vector<std::string> &invocationArgStrs,
                             ASTContext &ctx,
                             const ClangImporterOptions &importerOpts) {
  using ImporterImpl = ClangImporter::Implementation;
  const llvm::Triple &triple = ctx.LangOpts.Target;
  SearchPathOptions &searchPathOpts = ctx.SearchPathOpts;

  invocationArgStrs.push_back("-target");
  invocationArgStrs.push_back(triple.str());

  invocationArgStrs.push_back(ImporterImpl::moduleImportBufferName);

  if (ctx.LangOpts.EnableAppExtensionRestrictions) {
    invocationArgStrs.push_back("-fapplication-extension");
  }

  if (!importerOpts.TargetCPU.empty()) {
    invocationArgStrs.push_back("-mcpu=" + importerOpts.TargetCPU);

  } else if (triple.isOSDarwin()) {
    // Special case: arm64 defaults to the "cyclone" CPU for Darwin,
    // but Clang only detects this if we use -arch.
    if (triple.getArch() == llvm::Triple::aarch64 ||
        triple.getArch() == llvm::Triple::aarch64_be) {
      invocationArgStrs.push_back("-mcpu=cyclone");
    }
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
    invocationArgStrs.push_back(resourceDir.str());
  } else {
    invocationArgStrs.push_back("-resource-dir");
    invocationArgStrs.push_back(overrideResourceDir);
  }

  for (auto extraArg : importerOpts.ExtraArgs) {
    invocationArgStrs.push_back(extraArg);
  }

  // Enable API notes alongside headers/in frameworks.
  invocationArgStrs.push_back("-fapinotes-modules");

  // Add API notes paths.
  for (const auto &searchPath : searchPathOpts.ImportSearchPaths) {
    invocationArgStrs.push_back("-iapinotes-modules");
    invocationArgStrs.push_back(searchPath);
  }
  invocationArgStrs.push_back("-iapinotes-modules");
  invocationArgStrs.push_back(searchPathOpts.RuntimeLibraryImportPath);

  // Map the Swift major version into the API notes version for Swift. This
  // has the effect of allowing API notes to effect changes only on Swift
  // major versions, not minor versions.
  invocationArgStrs.push_back("-fapinotes-swift-version=" +
    llvm::itostr(ctx.LangOpts.EffectiveLanguageVersion[0]));
}

std::unique_ptr<ClangImporter>
ClangImporter::create(ASTContext &ctx,
                      const ClangImporterOptions &importerOpts,
                      DependencyTracker *tracker) {

  std::unique_ptr<ClangImporter> importer{
    new ClangImporter(ctx, importerOpts, tracker)
  };

  std::vector<std::string> invocationArgStrs;

  // Clang expects this to be like an actual command line. So we need to pass in
  // "clang" for argv[0]
  invocationArgStrs.push_back("clang");

  switch (importerOpts.Mode) {
  case ClangImporterOptions::Modes::Normal:
    getNormalInvocationArguments(invocationArgStrs, ctx, importerOpts);
    break;
  case ClangImporterOptions::Modes::EmbedBitcode:
    getEmbedBitcodeInvocationArguments(invocationArgStrs, ctx, importerOpts);
    break;
  }
  addCommonInvocationArguments(invocationArgStrs, ctx, importerOpts);

  if (importerOpts.DumpClangDiagnostics) {
    llvm::errs() << "clang '";
    interleave(invocationArgStrs,
               [](StringRef arg) { llvm::errs() << arg; },
               [] { llvm::errs() << "' '"; });
    llvm::errs() << "'\n";
  }

  std::vector<const char *> invocationArgs;
  for (auto &argStr : invocationArgStrs)
    invocationArgs.push_back(argStr.c_str());

  if (llvm::sys::path::extension(importerOpts.BridgingHeader).endswith(
        PCH_EXTENSION)) {
    importer->Impl.IsReadingBridgingPCH = true;
  }

  // FIXME: These can't be controlled from the command line.
  llvm::IntrusiveRefCntPtr<clang::DiagnosticOptions> diagnosticOpts{
    new clang::DiagnosticOptions
  };

  std::unique_ptr<ClangDiagnosticConsumer> diagClient{
    new ClangDiagnosticConsumer(importer->Impl, *diagnosticOpts,
                                importerOpts.DumpClangDiagnostics)
  };
  auto clangDiags = CompilerInstance::createDiagnostics(diagnosticOpts.get(),
                                                        diagClient.release());

  // Create a new Clang compiler invocation.
  importer->Impl.Invocation =
      clang::createInvocationFromCommandLine(invocationArgs, clangDiags);
  if (!importer->Impl.Invocation)
    return nullptr;

  // Don't stop emitting messages if we ever can't load a module.
  // FIXME: This is actually a general problem: any "fatal" error could mess up
  // the CompilerInvocation.
  clangDiags->setSeverity(clang::diag::err_module_not_found,
                          clang::diag::Severity::Error,
                          clang::SourceLocation());
  clangDiags->setSeverity(clang::diag::err_module_not_built,
                          clang::diag::Severity::Error,
                          clang::SourceLocation());

  // Create an almost-empty memory buffer.
  auto sourceBuffer = llvm::MemoryBuffer::getMemBuffer(
    "extern int __swift __attribute__((unavailable));",
    Implementation::moduleImportBufferName);
  clang::PreprocessorOptions &ppOpts =
      importer->Impl.Invocation->getPreprocessorOpts();
  ppOpts.addRemappedFile(Implementation::moduleImportBufferName,
                         sourceBuffer.release());

  // Install a Clang module file extension to build Swift name lookup tables.
  importer->Impl.Invocation->getFrontendOpts().ModuleFileExtensions.push_back(
      std::make_shared<SwiftNameLookupExtension>(
          importer->Impl.BridgingHeaderLookupTable,
          importer->Impl.LookupTables, importer->Impl.SwiftContext,
          importer->Impl.platformAvailability,
          importer->Impl.InferImportAsMember));

  // Create a compiler instance.
  auto PCHContainerOperations =
    std::make_shared<clang::PCHContainerOperations>();
  PCHContainerOperations->registerWriter(
      llvm::make_unique<clang::ObjectFilePCHContainerWriter>());
  PCHContainerOperations->registerReader(
      llvm::make_unique<clang::ObjectFilePCHContainerReader>());
  importer->Impl.Instance.reset(new CompilerInstance(PCHContainerOperations));
  auto &instance = *importer->Impl.Instance;
  instance.setDiagnostics(&*clangDiags);
  instance.setInvocation(importer->Impl.Invocation);

  // Create the associated action.
  importer->Impl.Action.reset(new ParsingAction(ctx, *importer,
                                                importer->Impl));
  auto *action = importer->Impl.Action.get();

  // Execute the action. We effectively inline most of
  // CompilerInstance::ExecuteAction here, because we need to leave the AST
  // open for future module loading.
  // FIXME: This has to be cleaned up on the Clang side before we can improve
  // things here.

  // Create the target instance.
  instance.setTarget(
    clang::TargetInfo::CreateTargetInfo(*clangDiags,
                                        instance.getInvocation().TargetOpts));
  if (!instance.hasTarget())
    return nullptr;

  // Inform the target of the language options.
  //
  // FIXME: We shouldn't need to do this, the target should be immutable once
  // created. This complexity should be lifted elsewhere.
  instance.getTarget().adjust(instance.getLangOpts());

  if (importerOpts.Mode == ClangImporterOptions::Modes::EmbedBitcode)
    return importer;

  bool canBegin = action->BeginSourceFile(instance,
                                          instance.getFrontendOpts().Inputs[0]);
  if (!canBegin)
    return nullptr; // there was an error related to the compiler arguments.

  clang::Preprocessor &clangPP = instance.getPreprocessor();
  clangPP.enableIncrementalProcessing();

  // Setup Preprocessor callbacks before initialing the parser to make sure
  // we catch implicit includes.
  auto ppTracker = llvm::make_unique<BridgingPPTracker>(importer->Impl);
  clangPP.addPPCallbacks(std::move(ppTracker));

  instance.createModuleManager();
  instance.getModuleManager()->addListener(
         std::unique_ptr<clang::ASTReaderListener>(
                 new ASTReaderCallbacks(*importer)));

  // Manually run the action, so that the TU stays open for additional parsing.
  instance.createSema(action->getTranslationUnitKind(), nullptr);
  importer->Impl.Parser.reset(new clang::Parser(clangPP, instance.getSema(),
                                                /*SkipFunctionBodies=*/false));

  clangPP.EnterMainSourceFile();
  importer->Impl.Parser->Initialize();

  importer->Impl.nameImporter.reset(new NameImporter(
      importer->Impl.SwiftContext, importer->Impl.platformAvailability,
      importer->Impl.getClangSema(), importer->Impl.InferImportAsMember));

  // FIXME: These decls are not being parsed correctly since (a) some of the
  // callbacks are still being added, and (b) the logic to parse them has
  // changed.
  clang::Parser::DeclGroupPtrTy parsed;
  while (!importer->Impl.Parser->ParseTopLevelDecl(parsed)) {
    for (auto *D : parsed.get()) {
      importer->Impl.addBridgeHeaderTopLevelDecls(D);

      if (auto named = dyn_cast<clang::NamedDecl>(D)) {
        addEntryToLookupTable(*importer->Impl.BridgingHeaderLookupTable, named,
                              *importer->Impl.nameImporter);
      }
    }
  }

  // FIXME: This is missing implicit includes.
  auto *CB = new HeaderImportCallbacks(*importer, importer->Impl);
  clangPP.addPPCallbacks(std::unique_ptr<clang::PPCallbacks>(CB));

  // Create the selectors we'll be looking for.
  auto &clangContext = importer->Impl.Instance->getASTContext();
  importer->Impl.objectAtIndexedSubscript
    = clangContext.Selectors.getUnarySelector(
        &clangContext.Idents.get("objectAtIndexedSubscript"));
  clang::IdentifierInfo *setObjectAtIndexedSubscriptIdents[2] = {
    &clangContext.Idents.get("setObject"),
    &clangContext.Idents.get("atIndexedSubscript")
  };
  importer->Impl.setObjectAtIndexedSubscript
    = clangContext.Selectors.getSelector(2, setObjectAtIndexedSubscriptIdents);
  importer->Impl.objectForKeyedSubscript
    = clangContext.Selectors.getUnarySelector(
        &clangContext.Idents.get("objectForKeyedSubscript"));
  clang::IdentifierInfo *setObjectForKeyedSubscriptIdents[2] = {
    &clangContext.Idents.get("setObject"),
    &clangContext.Idents.get("forKeyedSubscript")
  };
  importer->Impl.setObjectForKeyedSubscript
    = clangContext.Selectors.getSelector(2, setObjectForKeyedSubscriptIdents);

  // Set up the imported header module.
  auto *importedHeaderModule = ModuleDecl::create(ctx.getIdentifier("__ObjC"), ctx);
  importer->Impl.ImportedHeaderUnit =
    new (ctx) ClangModuleUnit(*importedHeaderModule, *importer, nullptr);
  importedHeaderModule->addFile(*importer->Impl.ImportedHeaderUnit);

  importer->Impl.IsReadingBridgingPCH = false;

  return importer;
}

bool ClangImporter::addSearchPath(StringRef newSearchPath, bool isFramework) {
  clang::FileManager &fileMgr = Impl.Instance->getFileManager();
  const clang::DirectoryEntry *entry = fileMgr.getDirectory(newSearchPath);
  if (!entry)
    return true;

  auto &headerSearchInfo = Impl.getClangPreprocessor().getHeaderSearchInfo();
  headerSearchInfo.AddSearchPath({entry, clang::SrcMgr::C_User, isFramework},
                                 /*isAngled=*/true);

  // In addition to changing the current preprocessor directly, we still need
  // to change the options structure for future module-building.
  Impl.Instance->getHeaderSearchOpts().AddPath(newSearchPath,
                                               clang::frontend::Angled,
                                               isFramework,
                                               /*IgnoreSysRoot=*/true);
  return false;
}

bool ClangImporter::Implementation::importHeader(
    ModuleDecl *adapter, StringRef headerName, SourceLoc diagLoc,
    bool trackParsedSymbols,
    std::unique_ptr<llvm::MemoryBuffer> sourceBuffer,
    bool implicitImport) {
  // Don't even try to load the bridging header if the Clang AST is in a bad
  // state. It could cause a crash.
  auto &clangDiags = getClangASTContext().getDiagnostics();
  if (clangDiags.hasFatalErrorOccurred())
    return true;

  assert(adapter);
  ImportedHeaderOwners.push_back(adapter);

  bool hadError = clangDiags.hasErrorOccurred();

  clang::ASTContext &ClangCtx = getClangASTContext();
  clang::Preprocessor &pp = getClangPreprocessor();

  clang::SourceManager &sourceMgr = ClangCtx.getSourceManager();

  clang::SourceLocation includeLoc =
    sourceMgr.getLocForStartOfFile(sourceMgr.getMainFileID());
  clang::FileID bufferID = sourceMgr.createFileID(std::move(sourceBuffer),
                                                  clang::SrcMgr::C_User,
                                                  /*LoadedID=*/0,
                                                  /*LoadedOffset=*/0,
                                                  includeLoc);
  auto &consumer =
      static_cast<HeaderParsingASTConsumer &>(Instance->getASTConsumer());
  consumer.reset();

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
  while (!Parser->ParseTopLevelDecl(parsed)) {
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
    SwiftContext.Diags.diagnose(
      diagLoc, diag::implicit_bridging_header_imported_from_module,
      llvm::sys::path::filename(headerName), adapter->getName());
  }

  // We can't do this as we're parsing because we may want to resolve naming
  // conflicts between the things we've parsed.
  for (auto group : allParsedDecls)
    for (auto *D : group)
      if (auto named = dyn_cast<clang::NamedDecl>(D))
        addEntryToLookupTable(*BridgingHeaderLookupTable, named,
                              getNameImporter());

  pp.EndSourceFile();
  bumpGeneration();

  // Add any defined macros to the bridging header lookup table.
  addMacrosToLookupTable(*BridgingHeaderLookupTable, getNameImporter());

  // Wrap all Clang imports under a Swift import decl.
  for (auto &Import : BridgeHeaderTopLevelImports) {
    if (auto *ClangImport = Import.dyn_cast<clang::ImportDecl*>()) {
      Import = createImportDecl(SwiftContext, adapter, ClangImport, {});
    }
  }

  // Finalize the lookup table, which may fail.
  finalizeLookupTable(*BridgingHeaderLookupTable, getNameImporter());

  // FIXME: What do we do if there was already an error?
  if (!hadError && clangDiags.hasErrorOccurred()) {
    SwiftContext.Diags.diagnose(diagLoc, diag::bridging_header_error,
                                headerName);
    return true;
  }

  return false;
}

bool ClangImporter::importHeader(StringRef header, ModuleDecl *adapter,
                                 off_t expectedSize, time_t expectedModTime,
                                 StringRef cachedContents, SourceLoc diagLoc) {
  clang::FileManager &fileManager = Impl.Instance->getFileManager();
  const clang::FileEntry *headerFile = fileManager.getFile(header,
                                                           /*OpenFile=*/true);
  if (headerFile && headerFile->getSize() == expectedSize &&
      headerFile->getModificationTime() == expectedModTime) {
    return importBridgingHeader(header, adapter, diagLoc, false, true);
  }

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
  if (llvm::sys::path::extension(header).endswith(PCH_EXTENSION)) {
    // We already imported this with -include-pch above, so we should have
    // collected a bunch of PCH-encoded module imports that we need to
    // replay to the HeaderImportCallbacks for processing.
    Impl.ImportedHeaderOwners.push_back(adapter);
    clang::ASTReader &R = *Impl.Instance->getModuleManager();
    HeaderImportCallbacks CB(*this, Impl);
    for (auto ID : Impl.PCHImportedSubmodules) {
      CB.handleImport(R.getSubmodule(ID));
    }
    Impl.PCHImportedSubmodules.clear();
    return false;
  }
  clang::FileManager &fileManager = Impl.Instance->getFileManager();
  const clang::FileEntry *headerFile = fileManager.getFile(header,
                                                           /*OpenFile=*/true);
  if (!headerFile) {
    Impl.SwiftContext.Diags.diagnose(diagLoc, diag::bridging_header_missing,
                                     header);
    return true;
  }

  llvm::SmallString<128> importLine{"#import \""};
  importLine += header;
  importLine += "\"\n";

  std::unique_ptr<llvm::MemoryBuffer> sourceBuffer{
    llvm::MemoryBuffer::getMemBufferCopy(
      importLine, Implementation::bridgingHeaderBufferName)
  };

  return Impl.importHeader(adapter, header, diagLoc, trackParsedSymbols,
                           std::move(sourceBuffer), implicitImport);
}

std::string ClangImporter::getBridgingHeaderContents(StringRef headerPath,
                                                     off_t &fileSize,
                                                     time_t &fileModTime) {
  auto invocation =
      std::make_shared<clang::CompilerInvocation>(*Impl.Invocation);

  invocation->getFrontendOpts().DisableFree = false;
  invocation->getFrontendOpts().Inputs.clear();
  invocation->getFrontendOpts().Inputs.push_back(
      clang::FrontendInputFile(headerPath, clang::IK_ObjC));

  invocation->getPreprocessorOpts().resetNonModularOptions();

  clang::CompilerInstance rewriteInstance(
    Impl.Instance->getPCHContainerOperations());
  rewriteInstance.setInvocation(invocation);
  rewriteInstance.createDiagnostics(new clang::IgnoringDiagConsumer);

  clang::FileManager &fileManager = Impl.Instance->getFileManager();
  rewriteInstance.setFileManager(&fileManager);
  rewriteInstance.createSourceManager(fileManager);
  rewriteInstance.setTarget(&Impl.Instance->getTarget());

  std::string result;
  bool success = llvm::CrashRecoveryContext().RunSafelyOnThread([&] {
    clang::RewriteIncludesAction action;
    action.BeginSourceFile(rewriteInstance,
                           invocation->getFrontendOpts().Inputs.front());
    llvm::raw_string_ostream os(result);
    clang::RewriteIncludesInInput(rewriteInstance.getPreprocessor(), &os,
                                  rewriteInstance.getPreprocessorOutputOpts());
    action.EndSourceFile();
  });

  success |= !rewriteInstance.getDiagnostics().hasErrorOccurred();
  if (!success) {
    Impl.SwiftContext.Diags.diagnose({},
                                     diag::could_not_rewrite_bridging_header);
    return "";
  }

  const clang::FileEntry *fileInfo = fileManager.getFile(headerPath);
  fileSize = fileInfo->getSize();
  fileModTime = fileInfo->getModificationTime();
  return result;
}

bool
ClangImporter::emitBridgingPCH(StringRef headerPath,
                               StringRef outputPCHPath) {
  auto invocation = std::make_shared<clang::CompilerInvocation>
    (clang::CompilerInvocation(*Impl.Invocation));
  invocation->getFrontendOpts().DisableFree = false;
  invocation->getFrontendOpts().Inputs.clear();
  invocation->getFrontendOpts().Inputs.push_back(
      clang::FrontendInputFile(headerPath, clang::IK_ObjC));
  invocation->getFrontendOpts().OutputFile = outputPCHPath;
  invocation->getPreprocessorOpts().resetNonModularOptions();
  invocation->getLangOpts()->NeededByPCHOrCompilationUsesPCH = true;

  clang::CompilerInstance emitInstance(
    Impl.Instance->getPCHContainerOperations());
  emitInstance.setInvocation(std::move(invocation));
  emitInstance.createDiagnostics(&Impl.Instance->getDiagnosticClient(),
                                 false);

  clang::FileManager &fileManager = Impl.Instance->getFileManager();
  emitInstance.setFileManager(&fileManager);
  emitInstance.createSourceManager(fileManager);
  emitInstance.setTarget(&Impl.Instance->getTarget());

  clang::GeneratePCHAction action;
  emitInstance.ExecuteAction(action);
  if (emitInstance.getDiagnostics().hasErrorOccurred()) {
    Impl.SwiftContext.Diags.diagnose({},
                                     diag::bridging_header_pch_error,
                                     outputPCHPath, headerPath);
    return true;
  }
  return false;
}

void ClangImporter::collectSubModuleNames(
    ArrayRef<std::pair<Identifier, SourceLoc>> path,
    std::vector<std::string> &names) {
  auto &clangHeaderSearch = Impl.getClangPreprocessor().getHeaderSearchInfo();

  // Look up the top-level module first.
  clang::Module *clangModule =
  clangHeaderSearch.lookupModule(path.front().first.str());
  if (!clangModule)
    return;
  clang::Module *submodule = clangModule;
  for (auto component : path.slice(1)) {
    submodule = submodule->findSubmodule(component.first.str());
    if (!submodule)
      return;
  }
  auto submoduleNameLength = submodule->getFullModuleName().length();
  for (auto sub : submodule->submodules()) {
    std::string full = sub->getFullModuleName();
    full.erase(0, submoduleNameLength + 1);
    names.push_back(std::move(full));
  }
}

bool ClangImporter::isModuleImported(const clang::Module *M) {
  return M->NameVisibility == clang::Module::NameVisibilityKind::AllVisible;
}

bool ClangImporter::canImportModule(std::pair<Identifier, SourceLoc> moduleID) {
  // Look up the top-level module to see if it exists.
  // FIXME: This only works with top-level modules.
  auto &clangHeaderSearch = Impl.getClangPreprocessor().getHeaderSearchInfo();
  clang::Module *clangModule =
    clangHeaderSearch.lookupModule(moduleID.first.str());
  if (!clangModule) {
    return false;
  }

  clang::Module::Requirement r;
  clang::Module::UnresolvedHeaderDirective mh;
  clang::Module *m;
  auto &ctx = Impl.getClangASTContext();
  return clangModule->isAvailable(ctx.getLangOpts(), getTargetInfo(), r, mh, m);
}

ModuleDecl *ClangImporter::loadModule(
    SourceLoc importLoc,
    ArrayRef<std::pair<Identifier, SourceLoc>> path) {
  auto &clangContext = Impl.getClangASTContext();
  auto &clangHeaderSearch = Impl.getClangPreprocessor().getHeaderSearchInfo();

  // Look up the top-level module first, to see if it exists at all.
  clang::Module *clangModule =
    clangHeaderSearch.lookupModule(path.front().first.str());
  if (!clangModule)
    return nullptr;

  // Convert the Swift import path over to a Clang import path.
  SmallVector<std::pair<clang::IdentifierInfo *, clang::SourceLocation>, 4>
    clangPath;
  for (auto component : path) {
    clangPath.push_back({ &clangContext.Idents.get(component.first.str()),
                          Impl.exportSourceLoc(component.second) } );
  }

  auto &srcMgr = clangContext.getSourceManager();
  auto &rawDiagClient = Impl.Instance->getDiagnosticClient();
  auto &diagClient = static_cast<ClangDiagnosticConsumer &>(rawDiagClient);

  auto loadModule = [&](clang::ModuleIdPath path,
                        bool makeVisible) -> clang::ModuleLoadResult {
    clang::Module::NameVisibilityKind visibility =
      makeVisible ? clang::Module::AllVisible : clang::Module::Hidden;

    auto importRAII = diagClient.handleImport(clangPath.front().first,
                                              importLoc);

    // FIXME: The source location here is completely bogus. It can't be
    // invalid, and it can't be the same thing twice in a row, so we just use
    // a counter. Having real source locations would be far, far better.
    clang::SourceLocation clangImportLoc
      = srcMgr.getLocForStartOfFile(srcMgr.getMainFileID())
              .getLocWithOffset(Impl.ImportCounter++);
    clang::ModuleLoadResult result =
        Impl.Instance->loadModule(clangImportLoc, path, visibility,
                                  /*IsInclusionDirective=*/false);
    if (result && makeVisible)
      Impl.getClangPreprocessor().makeModuleVisible(result, clangImportLoc);
    return result;
  };

  // Now load the top-level module, so that we can check if the submodule
  // exists without triggering a fatal error.
  clangModule = loadModule(clangPath.front(), false);
  if (!clangModule)
    return nullptr;

  // Verify that the submodule exists.
  clang::Module *submodule = clangModule;
  for (auto component : path.slice(1)) {
    submodule = submodule->findSubmodule(component.first.str());
    // FIXME: Specialize the error for a missing submodule?
    if (!submodule)
      return nullptr;
  }

  // Finally, load the submodule and make it visible.
  clangModule = loadModule(clangPath, true);
  if (!clangModule)
    return nullptr;

  return Impl.finishLoadingClangModule(*this, clangModule,
                                       /*preferAdapter=*/false);
}

ModuleDecl *ClangImporter::Implementation::finishLoadingClangModule(
    ClangImporter &owner,
    const clang::Module *clangModule,
    bool findAdapter) {
  assert(clangModule);

  // Bump the generation count.
  bumpGeneration();

  auto &cacheEntry = ModuleWrappers[clangModule];
  ModuleDecl *result;
  ClangModuleUnit *wrapperUnit;
  if ((wrapperUnit = cacheEntry.getPointer())) {
    result = wrapperUnit->getParentModule();
    if (!cacheEntry.getInt()) {
      // Force load adapter modules for all imported modules.
      // FIXME: This forces the creation of wrapper modules for all imports as
      // well, and may do unnecessary work.
      cacheEntry.setInt(true);
      result->forAllVisibleModules({}, [&](ModuleDecl::ImportedModule import) {});
    }
  } else {
    // Build the representation of the Clang module in Swift.
    // FIXME: The name of this module could end up as a key in the ASTContext,
    // but that's not correct for submodules.
    Identifier name = SwiftContext.getIdentifier((*clangModule).Name);
    result = ModuleDecl::create(name, SwiftContext);
    // Silence error messages about testably importing a Clang module.
    result->setTestingEnabled();

    wrapperUnit =
      new (SwiftContext) ClangModuleUnit(*result, owner, clangModule);
    result->addFile(*wrapperUnit);
    cacheEntry.setPointerAndInt(wrapperUnit, true);

    // Force load adapter modules for all imported modules.
    // FIXME: This forces the creation of wrapper modules for all imports as
    // well, and may do unnecessary work.
    result->forAllVisibleModules({}, [](ModuleDecl::ImportedModule import) {});
  }

  if (clangModule->isSubModule()) {
    finishLoadingClangModule(owner, clangModule->getTopLevelModule(), true);
  } else {
    ModuleDecl *&loaded = SwiftContext.LoadedModules[result->getName()];
    if (!loaded)
      loaded = result;
  }

  if (findAdapter)
    if (ModuleDecl *adapter = wrapperUnit->getAdapterModule())
      result = adapter;

  return result;
}

ModuleDecl *ClangImporter::getImportedHeaderModule() const {
  return Impl.ImportedHeaderUnit->getParentModule();
}

PlatformAvailability::PlatformAvailability(LangOptions &langOpts) {
  // Add filters to determine if a Clang availability attribute
  // applies in Swift, and if so, what is the cutoff for deprecated
  // declarations that are now considered unavailable in Swift.

  if (langOpts.Target.isiOS() && !langOpts.Target.isTvOS()) {
    if (!langOpts.EnableAppExtensionRestrictions) {
      filter = [](StringRef Platform) { return Platform == "ios"; };
    } else {
      filter = [](StringRef Platform) {
        return Platform == "ios" || Platform == "ios_app_extension";
      };
    }
    // Anything deprecated in iOS 7.x and earlier is unavailable in Swift.
    deprecatedAsUnavailableFilter = [](
        unsigned major, llvm::Optional<unsigned> minor) { return major <= 7; };
    deprecatedAsUnavailableMessage =
        "APIs deprecated as of iOS 7 and earlier are unavailable in Swift";
  } else if (langOpts.Target.isTvOS()) {
    if (!langOpts.EnableAppExtensionRestrictions) {
      filter = [](StringRef Platform) { return Platform == "tvos"; };
    } else {
      filter = [](StringRef Platform) {
        return Platform == "tvos" || Platform == "tvos_app_extension";
      };
    }
    // Anything deprecated in iOS 7.x and earlier is unavailable in Swift.
    deprecatedAsUnavailableFilter = [](
        unsigned major, llvm::Optional<unsigned> minor) { return major <= 7; };
    deprecatedAsUnavailableMessage =
        "APIs deprecated as of iOS 7 and earlier are unavailable in Swift";
  } else if (langOpts.Target.isWatchOS()) {
    if (!langOpts.EnableAppExtensionRestrictions) {
      filter = [](StringRef Platform) { return Platform == "watchos"; };
    } else {
      filter = [](StringRef Platform) {
        return Platform == "watchos" || Platform == "watchos_app_extension";
      };
    }
    // No deprecation filter on watchOS
    deprecatedAsUnavailableFilter = [](
        unsigned major, llvm::Optional<unsigned> minor) { return false; };
    deprecatedAsUnavailableMessage = "";
  } else if (langOpts.Target.isMacOSX()) {
    if (!langOpts.EnableAppExtensionRestrictions) {
      filter = [](StringRef Platform) { return Platform == "macos"; };
    } else {
      filter = [](StringRef Platform) {
        return Platform == "macos" || Platform == "macos_app_extension";
      };
    }
    // Anything deprecated in OSX 10.9.x and earlier is unavailable in Swift.
    deprecatedAsUnavailableFilter = [](unsigned major,
                                       llvm::Optional<unsigned> minor) {
      return major < 10 ||
             (major == 10 && (!minor.hasValue() || minor.getValue() <= 9));
    };
    deprecatedAsUnavailableMessage =
        "APIs deprecated as of OS X 10.9 and earlier are unavailable in Swift";
  }
}

ClangImporter::Implementation::Implementation(ASTContext &ctx,
                                              const ClangImporterOptions &opts)
    : SwiftContext(ctx),
      ImportForwardDeclarations(opts.ImportForwardDeclarations),
      InferImportAsMember(opts.InferImportAsMember),
      DisableSwiftBridgeAttr(opts.DisableSwiftBridgeAttr),
      BridgingHeaderExplicitlyRequested(!opts.BridgingHeader.empty()),
      IsReadingBridgingPCH(false),
      CurrentVersion(nameVersionFromOptions(ctx.LangOpts)),
      BridgingHeaderLookupTable(new SwiftLookupTable(nullptr)),
      platformAvailability(ctx.LangOpts),
      nameImporter() {}

ClangImporter::Implementation::~Implementation() {
  assert(NumCurrentImportingEntities == 0);
#ifndef NDEBUG
  SwiftContext.SourceMgr.verifyAllBuffers();
#endif
}

ClangModuleUnit *ClangImporter::Implementation::getWrapperForModule(
    ClangImporter &importer,
    const clang::Module *underlying) {
  auto &cacheEntry = ModuleWrappers[underlying];
  if (ClangModuleUnit *cached = cacheEntry.getPointer())
    return cached;

  // FIXME: Handle hierarchical names better.
  Identifier name = SwiftContext.getIdentifier(underlying->Name);
  auto wrapper = ModuleDecl::create(name, SwiftContext);
  // Silence error messages about testably importing a Clang module.
  wrapper->setTestingEnabled();

  auto file = new (SwiftContext) ClangModuleUnit(*wrapper, importer,
                                                 underlying);
  wrapper->addFile(*file);
  cacheEntry.setPointer(file);

  return file;
}

ClangModuleUnit *ClangImporter::Implementation::getClangModuleForDecl(
    const clang::Decl *D,
    bool allowForwardDeclaration) {
  auto maybeModule = getClangSubmoduleForDecl(D, allowForwardDeclaration);
  if (!maybeModule)
    return nullptr;
  if (!maybeModule.getValue())
    return ImportedHeaderUnit;

  // Get the parent module because currently we don't represent submodules with
  // ClangModuleUnit.
  auto *M = maybeModule.getValue()->getTopLevelModule();

  auto &importer =
    static_cast<ClangImporter &>(*SwiftContext.getClangModuleLoader());
  return getWrapperForModule(importer, M);
}

#pragma mark Source locations
clang::SourceLocation
ClangImporter::Implementation::exportSourceLoc(SourceLoc loc) {
  // FIXME: Implement!
  return clang::SourceLocation();
}

SourceLoc
ClangImporter::Implementation::importSourceLoc(clang::SourceLocation loc) {
  // FIXME: Implement!
  return SourceLoc();
}

SourceRange
ClangImporter::Implementation::importSourceRange(clang::SourceRange loc) {
  // FIXME: Implement!
  return SourceRange();
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
    if (name.startswith(removePrefix)) {
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

  SmallVector<clang::IdentifierInfo *, 8> pieces;
  pieces.push_back(exportName(name.getBaseName()).getAsIdentifierInfo());

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
  SmallVector<clang::IdentifierInfo *, 4> pieces;
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

  clang::IdentifierInfo *setterID = sel.getIdentifierInfoForSlot(0);
  if (!setterID || !setterID->getName().startswith("set"))
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

  return false;
}

#pragma mark Name lookup
const clang::TypedefNameDecl *
ClangImporter::Implementation::lookupTypedef(clang::DeclarationName name) {
  clang::Sema &sema = Instance->getSema();
  clang::LookupResult lookupResult(sema, name,
                                   clang::SourceLocation(),
                                   clang::Sema::LookupOrdinaryName);

  if (sema.LookupName(lookupResult, /*scope=*/nullptr)) {
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
  auto ContainingUnit = VD->getDeclContext()->getModuleScopeContext();
  return ModuleFilter == ContainingUnit;
}

static const clang::Module *getClangOwningModule(ClangNode Node,
                                            const clang::ASTContext &ClangCtx) {
  auto ExtSource = ClangCtx.getExternalSource();
  assert(ExtSource);
  if (const clang::Decl *D = Node.getAsDecl())
    return ExtSource->getModule(D->getOwningModuleID());
  if (const clang::MacroInfo *MI = Node.getAsMacro())
    return ExtSource->getModule(MI->getOwningModuleID());

  return nullptr;
}

static bool isVisibleFromModule(const ClangModuleUnit *ModuleFilter,
                                const ValueDecl *VD) {
  // Include a value from module X if:
  // * no particular module was requested, or
  // * module X was specifically requested.
  if (!ModuleFilter)
    return true;

  auto ContainingUnit = VD->getDeclContext()->getModuleScopeContext();
  if (ModuleFilter == ContainingUnit)
    return true;

  auto Wrapper = dyn_cast<ClangModuleUnit>(ContainingUnit);
  if (!Wrapper)
    return false;

  auto ClangNode = VD->getClangNode();
  assert(ClangNode);

  auto &ClangASTContext = ModuleFilter->getClangASTContext();
  auto OwningClangModule = getClangOwningModule(ClangNode, ClangASTContext);

  // We don't handle Clang submodules; pop everything up to the top-level
  // module.
  if (OwningClangModule)
    OwningClangModule = OwningClangModule->getTopLevelModule();

  if (OwningClangModule == ModuleFilter->getClangModule())
    return true;

  if (auto D = ClangNode.getAsDecl()) {
    // Handle redeclared decls.
    if (isa<clang::FunctionDecl>(D) || isa<clang::VarDecl>(D) ||
        isa<clang::TypedefNameDecl>(D)) {
      for (auto Redeclaration : D->redecls()) {
        if (Redeclaration == D)
          continue;
        auto OwningClangModule = getClangOwningModule(Redeclaration,
                                                      ClangASTContext);
        if (OwningClangModule)
          OwningClangModule = OwningClangModule->getTopLevelModule();

        if (OwningClangModule == ModuleFilter->getClangModule())
          return true;
      }
    } else if (isa<clang::TagDecl>(D)) {
      for (auto Redeclaration : D->redecls()) {
        if (Redeclaration == D)
          continue;
        if (!cast<clang::TagDecl>(Redeclaration)->isCompleteDefinition())
          continue;
        auto OwningClangModule = getClangOwningModule(Redeclaration,
                                                      ClangASTContext);
        if (OwningClangModule)
          OwningClangModule = OwningClangModule->getTopLevelModule();

        if (OwningClangModule == ModuleFilter->getClangModule())
          return true;
      }
    }
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
  const ClangModuleUnit *ModuleFilter = nullptr;

public:
  FilteringVisibleDeclConsumer(swift::VisibleDeclConsumer &consumer,
                               const ClangModuleUnit *CMU)
      : NextConsumer(consumer), ModuleFilter(CMU) {}

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
    if (isVisibleFromModule(ModuleFilter, VD))
      NextConsumer.foundDecl(VD, Reason);
  }
};

class FilteringDeclaredDeclConsumer : public swift::VisibleDeclConsumer {
  swift::VisibleDeclConsumer &NextConsumer;
  const ClangModuleUnit *ModuleFilter = nullptr;

public:
  FilteringDeclaredDeclConsumer(swift::VisibleDeclConsumer &consumer,
                                const ClangModuleUnit *CMU)
      : NextConsumer(consumer),
        ModuleFilter(CMU) {}

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
    if (isDeclaredInModule(ModuleFilter, VD))
      NextConsumer.foundDecl(VD, Reason);
  }
};

/// A hack to blacklist particular types in the Darwin module on
/// Apple platforms.
class DarwinBlacklistDeclConsumer : public swift::VisibleDeclConsumer {
  swift::VisibleDeclConsumer &NextConsumer;
  clang::ASTContext &ClangASTContext;

  bool isBlacklisted(ValueDecl *VD) {
    if (!VD->hasClangNode())
      return false;

    const clang::Module *clangModule = getClangOwningModule(VD->getClangNode(),
                                                            ClangASTContext);
    if (!clangModule)
      return false;

    if (clangModule->Name == "MacTypes") {
      return llvm::StringSwitch<bool>(VD->getNameStr())
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
      // Note that this is a blacklist rather than a whitelist.
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
  DarwinBlacklistDeclConsumer(swift::VisibleDeclConsumer &consumer,
                              clang::ASTContext &clangASTContext)
      : NextConsumer(consumer), ClangASTContext(clangASTContext) {}

  static bool needsBlacklist(const clang::Module *topLevelModule) {
    if (!topLevelModule)
      return false;
    if (topLevelModule->Name == "Darwin")
      return true;
    if (topLevelModule->Name == "CoreServices")
      return true;
    return false;
  }

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
    if (!isBlacklisted(VD))
      NextConsumer.foundDecl(VD, Reason);
  }
};

} // unnamed namespace

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
    if (auto *MI = ClangPP.getMacroInfo(II)) {
      if (filter(MI)) {
        Identifier Name = Impl.getNameImporter().importMacroName(II, MI);
        if (Decl *imported = Impl.importMacro(Name, MI))
          receiver(imported);
      }
    }
  }
}

bool ClangImporter::lookupDeclsFromHeader(StringRef Filename,
                              llvm::function_ref<bool(ClangNode)> filter,
                              llvm::function_ref<void(Decl*)> receiver) const {
  const clang::FileEntry *File =
    getClangPreprocessor().getFileManager().getFile(Filename);
  if (!File)
    return true;

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

      if (ClangSM.getFileEntryForID(ClangSM.getFileID(ClangLoc)) != File)
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
    if (auto *ppRec = ClangPP.getPreprocessingRecord()) {
      clang::SourceLocation B = ClangSM.getLocForStartOfFile(FID);
      clang::SourceLocation E = ClangSM.getLocForEndOfFile(FID);
      clang::SourceRange R(B, E);
      const auto &Entities = ppRec->getPreprocessedEntitiesInRange(R);
      for (auto I = Entities.begin(), E = Entities.end(); I != E; ++I) {
        if (!ppRec->isEntityInFileID(I, FID))
          continue;
        clang::PreprocessedEntity *PPE = *I;
        if (!PPE)
          continue;
        if (auto *MD = dyn_cast<clang::MacroDefinitionRecord>(PPE)) {
          auto *II = const_cast<clang::IdentifierInfo*>(MD->getName());
          if (auto *MI = ClangPP.getMacroInfo(II)) {
            if (filter(MI)) {
              Identifier Name = Impl.getNameImporter().importMacroName(II, MI);
              if (Decl *imported = Impl.importMacro(Name, MI))
                receiver(imported);
            }
          }
        }
      }

      // FIXME: Module imports inside that header.
    }
    return false;
  }

  return true; // no info found about that header.
}

void ClangImporter::lookupValue(DeclName name, VisibleDeclConsumer &consumer){
  Impl.forEachLookupTable([&](SwiftLookupTable &table) -> bool {
      Impl.lookupValue(table, name, consumer);
      return false;
    });
}

void ClangModuleUnit::lookupVisibleDecls(ModuleDecl::AccessPathTy accessPath,
                                         VisibleDeclConsumer &consumer,
                                         NLKind lookupKind) const {
  // FIXME: Ignore submodules, which are empty for now.
  if (clangModule && clangModule->isSubModule())
    return;

  // FIXME: Respect the access path.
  FilteringVisibleDeclConsumer filterConsumer(consumer, this);

  DarwinBlacklistDeclConsumer darwinBlacklistConsumer(filterConsumer,
                                                      getClangASTContext());

  swift::VisibleDeclConsumer *actualConsumer = &filterConsumer;
  if (lookupKind == NLKind::UnqualifiedLookup &&
      DarwinBlacklistDeclConsumer::needsBlacklist(clangModule)) {
    actualConsumer = &darwinBlacklistConsumer;
  }

  // Find the corresponding lookup table.
  if (auto lookupTable = owner.Impl.findLookupTable(clangModule)) {
    // Search it.
    owner.Impl.lookupVisibleDecls(*lookupTable, *actualConsumer);
  }
}

namespace {
class VectorDeclPtrConsumer : public swift::VisibleDeclConsumer {
public:
  SmallVectorImpl<Decl *> &Results;
  explicit VectorDeclPtrConsumer(SmallVectorImpl<Decl *> &Decls)
    : Results(Decls) {}

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
    Results.push_back(VD);
  }
};
} // unnamed namespace

void ClangModuleUnit::getTopLevelDecls(SmallVectorImpl<Decl*> &results) const {
  VectorDeclPtrConsumer consumer(results);
  FilteringDeclaredDeclConsumer filterConsumer(consumer, this);
  DarwinBlacklistDeclConsumer blacklistConsumer(filterConsumer,
                                                getClangASTContext());

  const clang::Module *topLevelModule =
    clangModule ? clangModule->getTopLevelModule() : nullptr;

  swift::VisibleDeclConsumer *actualConsumer = &filterConsumer;
  if (DarwinBlacklistDeclConsumer::needsBlacklist(topLevelModule))
    actualConsumer = &blacklistConsumer;

  // Find the corresponding lookup table.
  if (auto lookupTable = owner.Impl.findLookupTable(topLevelModule)) {
    // Search it.
    owner.Impl.lookupVisibleDecls(*lookupTable, *actualConsumer);

    // Add the extensions produced by importing categories.
    for (auto category : lookupTable->categories()) {
      if (auto extension = cast_or_null<ExtensionDecl>(
              owner.Impl.importDecl(category, owner.Impl.CurrentVersion)))
        results.push_back(extension);
    }

    // Retrieve all of the globals that will be mapped to members.

    // FIXME: Since we don't represent Clang submodules as Swift
    // modules, we're getting everything.
    llvm::SmallPtrSet<ExtensionDecl *, 8> knownExtensions;
    for (auto entry : lookupTable->allGlobalsAsMembers()) {
      auto decl = entry.get<clang::NamedDecl *>();
      auto importedDecl =
          owner.Impl.importDecl(decl, owner.Impl.CurrentVersion);
      if (!importedDecl) continue;

      auto ext = dyn_cast<ExtensionDecl>(importedDecl->getDeclContext());
      if (!ext) continue;

      if (knownExtensions.insert(ext).second)
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
  SmallVector<std::pair<swift::Identifier, swift::SourceLoc>, 4> AccessPath;
  auto *TmpMod = ImportedMod;
  while (TmpMod) {
    AccessPath.push_back({ Ctx.getIdentifier(TmpMod->Name), SourceLoc() });
    TmpMod = TmpMod->Parent;
  }
  std::reverse(AccessPath.begin(), AccessPath.end());

  bool IsExported = false;
  for (auto *ExportedMod : Exported) {
    if (ImportedMod == ExportedMod) {
      IsExported = true;
      break;
    }
  }

  auto *ID = ImportDecl::create(Ctx, DC, SourceLoc(),
                                ImportKind::Module, SourceLoc(), AccessPath,
                                ClangN);
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

void ClangModuleUnit::getDisplayDecls(SmallVectorImpl<Decl*> &results) const {
  if (clangModule)
    getImportDecls(const_cast<ClangModuleUnit *>(this), clangModule, results);
  getTopLevelDecls(results);
}

void ClangModuleUnit::lookupValue(ModuleDecl::AccessPathTy accessPath,
                                  DeclName name, NLKind lookupKind,
                                  SmallVectorImpl<ValueDecl*> &results) const {
  if (!ModuleDecl::matchesAccessPath(accessPath, name))
    return;

  // FIXME: Ignore submodules, which are empty for now.
  if (clangModule && clangModule->isSubModule())
    return;

  VectorDeclConsumer vectorWriter(results);
  FilteringVisibleDeclConsumer filteringConsumer(vectorWriter, this);

  DarwinBlacklistDeclConsumer darwinBlacklistConsumer(filteringConsumer,
                                                      getClangASTContext());

  swift::VisibleDeclConsumer *consumer = &filteringConsumer;
  if (lookupKind == NLKind::UnqualifiedLookup &&
      DarwinBlacklistDeclConsumer::needsBlacklist(clangModule)) {
    consumer = &darwinBlacklistConsumer;
  }

  // Find the corresponding lookup table.
  if (auto lookupTable = owner.Impl.findLookupTable(clangModule)) {
    // Search it.
    owner.Impl.lookupValue(*lookupTable, name, *consumer);
  }
}

/// Determine whether the given Clang entry is visible.
///
/// FIXME: this is an elaborate hack to badly reflect Clang's
/// submodule visibility into Swift.
static bool isVisibleClangEntry(clang::ASTContext &ctx,
                                SwiftLookupTable::SingleEntry entry) {
  if (auto clangDecl = entry.dyn_cast<clang::NamedDecl *>()) {
    // For a declaration, check whether the declaration is hidden.
    if (!clangDecl->isHidden()) return true;

    // Is any redeclaration visible?
    for (auto redecl : clangDecl->redecls()) {
      if (!cast<clang::NamedDecl>(redecl)->isHidden()) return true;
    }

    return false;
  }

  // Check whether the macro is defined.
  auto clangMacro = entry.get<clang::MacroInfo *>();
  if (auto moduleID = clangMacro->getOwningModuleID()) {
    if (auto module = ctx.getExternalSource()->getModule(moduleID))
      return module->NameVisibility == clang::Module::AllVisible;
  }

  return true;
}

void ClangImporter::loadExtensions(NominalTypeDecl *nominal,
                                   unsigned previousGeneration) {
  // Determine the effective Clang context for this Swift nominal type.
  auto effectiveClangContext = Impl.getEffectiveClangContext(nominal);
  if (!effectiveClangContext) return;

  // For an Objective-C class, import all of the visible categories.
  if (auto objcClass = dyn_cast_or_null<clang::ObjCInterfaceDecl>(
                         effectiveClangContext.getAsDeclContext())) {
    // Simply importing the categories adds them to the list of extensions.
    for (auto I = objcClass->visible_categories_begin(),
           E = objcClass->visible_categories_end();
         I != E; ++I) {
      Impl.importDeclReal(*I, Impl.CurrentVersion);
    }
  }

  // Dig through each of the Swift lookup tables, creating extensions
  // where needed.
  auto &clangCtx = Impl.getClangASTContext();
  (void)Impl.forEachLookupTable([&](SwiftLookupTable &table) -> bool {
      // FIXME: If we already looked at this for this generation,
      // skip.

      for (auto entry : table.lookupGlobalsAsMembers(effectiveClangContext)) {
        // If the entry is not visible, skip it.
        if (!isVisibleClangEntry(clangCtx, entry)) continue;

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
       ClassDecl *classDecl,
       ObjCSelector selector,
       bool isInstanceMethod,
       unsigned previousGeneration,
       llvm::TinyPtrVector<AbstractFunctionDecl *> &methods) {
  // If we're currently looking for this selector, don't load any Objective-C
  // methods.
  if (Impl.ActiveSelectors.count({selector, isInstanceMethod}))
    return;

  const auto *objcClass =
      dyn_cast_or_null<clang::ObjCInterfaceDecl>(classDecl->getClangDecl());
  if (!objcClass)
    return;

  // Collect the set of visible Objective-C methods with this selector.
  clang::Selector clangSelector = Impl.exportSelector(selector);
  SmallVector<clang::ObjCMethodDecl *, 4> objcMethods;
  auto &sema = Impl.Instance->getSema();
  sema.CollectMultipleMethodsInGlobalPool(clangSelector, objcMethods,
                                          isInstanceMethod,
                                          /*CheckTheOther=*/false);

  // Check whether this method is in the class we care about.
  SmallVector<AbstractFunctionDecl *, 4> foundMethods;
  for (auto objcMethod : objcMethods) {
    // Find the owner of this method and determine whether it is the class
    // we're looking for.
    if (objcMethod->getClassInterface() != objcClass)
      continue;

    if (auto method = dyn_cast_or_null<AbstractFunctionDecl>(
                        Impl.importDecl(objcMethod, Impl.CurrentVersion))) {
      foundMethods.push_back(method);
    }
  }

  // If we didn't find anything, we're done.
  if (foundMethods.empty())
    return;

  // If we did find something, it might be a duplicate of something we found
  // earlier, because we aren't tracking generation counts for Clang modules.
  // Filter out the duplicates.
  // FIXME: We shouldn't need to do this.
  llvm::SmallPtrSet<AbstractFunctionDecl *, 4> known;
  known.insert(methods.begin(), methods.end());
  for (auto method : foundMethods) {
    if (known.insert(method).second)
      methods.push_back(method);
  }
}

void
ClangModuleUnit::lookupClassMember(ModuleDecl::AccessPathTy accessPath,
                                   DeclName name,
                                   SmallVectorImpl<ValueDecl*> &results) const {
  // FIXME: Ignore submodules, which are empty for now.
  if (clangModule && clangModule->isSubModule())
    return;

  VectorDeclConsumer consumer(results);

  // Find the corresponding lookup table.
  if (auto lookupTable = owner.Impl.findLookupTable(clangModule)) {
    // Search it.
    owner.Impl.lookupObjCMembers(*lookupTable, name, consumer);
  }
}

void ClangModuleUnit::lookupClassMembers(ModuleDecl::AccessPathTy accessPath,
                                         VisibleDeclConsumer &consumer) const {
  // FIXME: Ignore submodules, which are empty for now.
  if (clangModule && clangModule->isSubModule())
    return;

  // Find the corresponding lookup table.
  if (auto lookupTable = owner.Impl.findLookupTable(clangModule)) {
    // Search it.
    owner.Impl.lookupAllObjCMembers(*lookupTable, consumer);
  }
}

void ClangModuleUnit::lookupObjCMethods(
       ObjCSelector selector,
       SmallVectorImpl<AbstractFunctionDecl *> &results) const {
  // FIXME: Ignore submodules, which are empty for now.
  if (clangModule && clangModule->isSubModule())
    return;

  // Map the selector into a Clang selector.
  auto clangSelector = owner.Impl.exportSelector(selector);
  if (clangSelector.isNull()) return;

  // Collect all of the Objective-C methods with this selector.
  SmallVector<clang::ObjCMethodDecl *, 8> objcMethods;
  auto &clangSema = owner.Impl.getClangSema();
  clangSema.CollectMultipleMethodsInGlobalPool(clangSelector,
                                               objcMethods,
                                               /*InstanceFirst=*/true,
                                               /*CheckTheOther=*/false);
  clangSema.CollectMultipleMethodsInGlobalPool(clangSelector,
                                               objcMethods,
                                               /*InstanceFirst=*/false,
                                               /*CheckTheOther=*/false);

  // Import the methods.
  auto &clangCtx = clangSema.getASTContext();
  for (auto objcMethod : objcMethods) {
    // Verify that this method came from this module.
    auto owningClangModule = getClangOwningModule(objcMethod, clangCtx);
    if (owningClangModule)
      owningClangModule = owningClangModule->getTopLevelModule();

    if (owningClangModule != clangModule) continue;

    // If we found a property accessor, import the property.
    if (objcMethod->isPropertyAccessor())
      (void)owner.Impl.importDecl(objcMethod->findPropertyDecl(true),
                                  owner.Impl.CurrentVersion);

    // Import it.
    // FIXME: Retrying a failed import works around recursion bugs in the Clang
    // importer.
    auto imported =
        owner.Impl.importDecl(objcMethod, owner.Impl.CurrentVersion);
    if (!imported)
      imported = owner.Impl.importDecl(objcMethod, owner.Impl.CurrentVersion);
    if (!imported) continue;

    if (auto func = dyn_cast<AbstractFunctionDecl>(imported))
      results.push_back(func);

    // If there is an alternate declaration, also look at it.
    for (auto alternate : owner.Impl.getAlternateDecls(imported)) {
      if (auto func = dyn_cast<AbstractFunctionDecl>(alternate))
        results.push_back(func);
    }
  }
}

void ClangModuleUnit::collectLinkLibraries(
    ModuleDecl::LinkLibraryCallback callback) const {
  if (!clangModule)
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
  if (!clangModule)
    return "<imports>";
  if (const clang::FileEntry *F = clangModule->getASTFile())
    if (!F->getName().empty())
      return F->getName();
  return StringRef();
}

clang::TargetInfo &ClangImporter::getTargetInfo() const {
  return Impl.Instance->getTarget();
}

clang::ASTContext &ClangImporter::getClangASTContext() const {
  return Impl.getClangASTContext();
}

clang::Preprocessor &ClangImporter::getClangPreprocessor() const {
  return Impl.getClangPreprocessor();
}
const clang::Module *ClangImporter::getClangOwningModule(ClangNode Node) const {
  return ::getClangOwningModule(Node, getClangASTContext());
}

bool ClangImporter::hasTypedef(const clang::Decl *typeDecl) const {
  return Impl.DeclsWithSuperfluousTypedefs.count(typeDecl);
}

clang::Sema &ClangImporter::getClangSema() const {
  return Impl.getClangSema();
}

clang::CodeGenOptions &ClangImporter::getClangCodeGenOpts() const {
  return Impl.getClangCodeGenOpts();
}

std::string ClangImporter::getClangModuleHash() const {
  return Impl.Invocation->getModuleHash();
}

Decl *ClangImporter::importDeclCached(const clang::NamedDecl *ClangDecl) {
  return Impl.importDeclCached(ClangDecl, Impl.CurrentVersion);
}

void ClangImporter::printStatistics() const {
  Impl.Instance->getModuleManager()->PrintStats();
}

void ClangImporter::verifyAllModules() {
#ifndef NDEBUG
  if (Impl.ImportCounter == Impl.VerifiedImportCounter)
    return;

  // Collect the Decls before verifying them; the act of verifying may cause
  // more decls to be imported and modify the map while we are iterating it.
  SmallVector<Decl *, 8> Decls;
  for (auto &I : Impl.ImportedDecls)
    if (I.first.second == Impl.CurrentVersion)
      if (Decl *D = I.second)
        Decls.push_back(D);

  for (auto D : Decls)
    verify(D);

  Impl.VerifiedImportCounter = Impl.ImportCounter;
#endif
}

//===----------------------------------------------------------------------===//
// ClangModule Implementation
//===----------------------------------------------------------------------===//

ClangModuleUnit::ClangModuleUnit(ModuleDecl &M, ClangImporter &owner,
                                 const clang::Module *clangModule)
  : LoadedFile(FileUnitKind::ClangModule, M), owner(owner),
    clangModule(clangModule) {
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

ModuleDecl *ClangModuleUnit::getAdapterModule() const {
  if (!clangModule)
    return nullptr;

  if (!isTopLevel()) {
    // FIXME: Is this correct for submodules?
    auto topLevel = clangModule->getTopLevelModule();
    auto wrapper = owner.Impl.getWrapperForModule(owner, topLevel);
    return wrapper->getAdapterModule();
  }

  if (!adapterModule.getInt()) {
    // FIXME: Include proper source location.
    ModuleDecl *M = getParentModule();
    ASTContext &Ctx = M->getASTContext();
    auto adapter = Ctx.getModule(ModuleDecl::AccessPathTy({M->getName(),
                                                       SourceLoc()}));
    if (adapter == M) {
      adapter = nullptr;
    } else {
      auto &sharedModuleRef = Ctx.LoadedModules[M->getName()];
      assert(!sharedModuleRef || sharedModuleRef == adapter ||
             sharedModuleRef == M);
      sharedModuleRef = adapter;
    }

    auto mutableThis = const_cast<ClangModuleUnit *>(this);
    mutableThis->adapterModule.setPointerAndInt(adapter, true);
  }

  return adapterModule.getPointer();
}

void ClangModuleUnit::getImportedModules(
    SmallVectorImpl<ModuleDecl::ImportedModule> &imports,
    ModuleDecl::ImportFilter filter) const {
  if (filter != ModuleDecl::ImportFilter::Public)
    imports.push_back({ModuleDecl::AccessPathTy(),
                       getASTContext().getStdlibModule()});

  if (!clangModule) {
    // This is the special "imported headers" module.
    if (filter != ModuleDecl::ImportFilter::Private) {
      imports.append(owner.Impl.ImportedHeaderExports.begin(),
                     owner.Impl.ImportedHeaderExports.end());
    }
    return;
  }

  auto topLevelAdapter = getAdapterModule();

  SmallVector<clang::Module *, 8> imported;
  clangModule->getExportedModules(imported);
  if (filter != ModuleDecl::ImportFilter::Public) {
    if (filter == ModuleDecl::ImportFilter::All) {
      llvm::SmallPtrSet<clang::Module *, 8> knownModules;
      imported.append(clangModule->Imports.begin(), clangModule->Imports.end());
      imported.erase(std::remove_if(imported.begin(), imported.end(),
                                    [&](clang::Module *mod) -> bool {
                                      return !knownModules.insert(mod).second;
                                    }),
                     imported.end());
    } else {
      llvm::SmallPtrSet<clang::Module *, 8> knownModules(imported.begin(),
                                                         imported.end());
      SmallVector<clang::Module *, 8> privateImports;
      std::copy_if(clangModule->Imports.begin(), clangModule->Imports.end(),
                   std::back_inserter(privateImports), [&](clang::Module *mod) {
                     return knownModules.count(mod) == 0;
      });
      imported.swap(privateImports);
    }

    // FIXME: The parent module isn't exactly a private import, but it is
    // needed for link dependencies.
    if (clangModule->Parent)
      imported.push_back(clangModule->Parent);
  }

  for (auto importMod : imported) {
    auto wrapper = owner.Impl.getWrapperForModule(owner, importMod);

    auto actualMod = wrapper->getAdapterModule();
    if (!actualMod) {
      // HACK: Deal with imports of submodules by importing the top-level module
      // as well.
      auto importTopLevel = importMod->getTopLevelModule();
      if (importTopLevel != importMod &&
          importTopLevel != clangModule->getTopLevelModule()) {
        auto topLevelWrapper = owner.Impl.getWrapperForModule(owner,
                                                              importTopLevel);
        imports.push_back({ ModuleDecl::AccessPathTy(),
                            topLevelWrapper->getParentModule() });
      }
      actualMod = wrapper->getParentModule();
    } else if (actualMod == topLevelAdapter) {
      actualMod = wrapper->getParentModule();
    }

    assert(actualMod && "Missing imported adapter module");
    imports.push_back({ModuleDecl::AccessPathTy(), actualMod});
  }
}

void ClangModuleUnit::getImportedModulesForLookup(
    SmallVectorImpl<ModuleDecl::ImportedModule> &imports) const {

  if (!clangModule) {
    // This is the special "imported headers" module.
    imports.append(owner.Impl.ImportedHeaderExports.begin(),
                   owner.Impl.ImportedHeaderExports.end());
    return;
  }

  // Reuse our cached list of imports if we have one.
  if (!importedModulesForLookup.empty()) {
    imports.append(importedModulesForLookup.begin(),
                   importedModulesForLookup.end());
    return;
  }

  size_t firstImport = imports.size();
  auto topLevel = clangModule->getTopLevelModule();
  auto topLevelAdapter = getAdapterModule();

  SmallVector<clang::Module *, 8> imported;
  clangModule->getExportedModules(imported);
  if (imported.empty())
    return;

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
      auto wrapper = owner.Impl.getWrapperForModule(owner, nextTopLevel);
      if (wrapper->getAdapterModule())
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
    auto wrapper = owner.Impl.getWrapperForModule(owner, importMod);

    auto actualMod = wrapper->getAdapterModule();
    if (!actualMod || actualMod == topLevelAdapter)
      actualMod = wrapper->getParentModule();

    assert(actualMod && "Missing imported adapter module");
    imports.push_back({ModuleDecl::AccessPathTy(), actualMod});
  }

  // Cache our results for use next time.
  auto importsToCache = llvm::makeArrayRef(imports).slice(firstImport);
  importedModulesForLookup = getASTContext().AllocateCopy(importsToCache);
}

void ClangImporter::getMangledName(raw_ostream &os,
                                   const clang::NamedDecl *clangDecl) const {
  if (!Impl.Mangler)
    Impl.Mangler.reset(Impl.getClangASTContext().createMangleContext());

  Impl.Mangler->mangleName(clangDecl, os);
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

bool ClangImporter::Implementation::forEachLookupTable(
       llvm::function_ref<bool(SwiftLookupTable &table)> fn) {
  // Visit the bridging header's lookup table.
  if (fn(*BridgingHeaderLookupTable)) return true;

  // Collect and sort the set of module names.
  SmallVector<StringRef, 4> moduleNames;
  for (const auto &entry : LookupTables) {
    moduleNames.push_back(entry.first());
  }
  llvm::array_pod_sort(moduleNames.begin(), moduleNames.end());

  // Visit the lookup tables.
  for (auto moduleName : moduleNames) {
    if (fn(*LookupTables[moduleName])) return true;
  }

  return false;
}

void ClangImporter::Implementation::lookupValue(
       SwiftLookupTable &table, DeclName name,
       VisibleDeclConsumer &consumer) {
  auto &clangCtx = getClangASTContext();
  auto clangTU = clangCtx.getTranslationUnitDecl();

  for (auto entry : table.lookup(name.getBaseName().str(), clangTU)) {
    // If the entry is not visible, skip it.
    if (!isVisibleClangEntry(clangCtx, entry)) continue;

    ValueDecl *decl;
    // If it's a Clang declaration, try to import it.
    if (auto clangDecl = entry.dyn_cast<clang::NamedDecl *>()) {
      decl = cast_or_null<ValueDecl>(
          importDeclReal(clangDecl->getMostRecentDecl(), CurrentVersion));
      if (!decl) continue;
    } else {
      // Try to import a macro.
      auto clangMacro = entry.get<clang::MacroInfo *>();
      decl = importMacro(name.getBaseName(), clangMacro);
      if (!decl) continue;
    }

    // If we found a declaration from the standard library, make sure
    // it does not show up in the lookup results for the imported
    // module.
    if (decl->getDeclContext()->isModuleScopeContext() &&
        decl->getModuleContext() == getStdlibModule())
      continue;

    // If the name matched, report this result.
    bool anyMatching = false;
    if (decl->getFullName().matchesRef(name) &&
        decl->getDeclContext()->isModuleScopeContext()) {
      consumer.foundDecl(decl, DeclVisibilityKind::VisibleAtTopLevel);
      anyMatching = true;
    }

    // If there is an alternate declaration and the name matches,
    // report this result.
    for (auto alternate : getAlternateDecls(decl)) {
      if (alternate->getFullName().matchesRef(name) &&
          alternate->getDeclContext()->isModuleScopeContext()) {
        consumer.foundDecl(alternate, DeclVisibilityKind::VisibleAtTopLevel);
        anyMatching = true;
      }
    }

    // If we have a declaration and nothing matched so far, try the Swift 2
    // name.
    if (!anyMatching) {
      if (auto clangDecl = entry.dyn_cast<clang::NamedDecl *>()) {
        if (auto swift2Decl = cast_or_null<ValueDecl>(importDeclReal(
                clangDecl->getMostRecentDecl(), ImportNameVersion::Swift2))) {
          if (swift2Decl->getFullName().matchesRef(name) &&
              swift2Decl->getDeclContext()->isModuleScopeContext()) {
            consumer.foundDecl(swift2Decl,
                               DeclVisibilityKind::VisibleAtTopLevel);
          }
        }
      }
    }
  }
}

void ClangImporter::Implementation::lookupVisibleDecls(
       SwiftLookupTable &table,
       VisibleDeclConsumer &consumer) {
  // Retrieve and sort all of the base names in this particular table.
  auto baseNames = table.allBaseNames();
  llvm::array_pod_sort(baseNames.begin(), baseNames.end());

  // Look for namespace-scope entities with each base name.
  for (auto baseName : baseNames) {
    lookupValue(table, SwiftContext.getIdentifier(baseName), consumer);
  }
}

void ClangImporter::Implementation::lookupObjCMembers(
       SwiftLookupTable &table,
       DeclName name,
       VisibleDeclConsumer &consumer) {
  auto &clangCtx = getClangASTContext();
  auto baseName = name.getBaseName().str();

  for (auto clangDecl : table.lookupObjCMembers(baseName)) {
    // If the entry is not visible, skip it.
    if (!isVisibleClangEntry(clangCtx, clangDecl)) continue;

    // Import the declaration.
    auto decl =
        cast_or_null<ValueDecl>(importDeclReal(clangDecl, CurrentVersion));
    if (!decl)
      continue;

    // If the name we found matches, report the declaration.
    bool matchedAny = false;
    if (decl->getFullName().matchesRef(name)) {
      consumer.foundDecl(decl, DeclVisibilityKind::DynamicLookup);
      matchedAny = true;
    }

    // Check for an alternate declaration; if it's name matches,
    // report it.
    for (auto alternate : getAlternateDecls(decl)) {
      if (alternate->getFullName().matchesRef(name)) {
        consumer.foundDecl(alternate, DeclVisibilityKind::DynamicLookup);
        matchedAny = true;
      }
    }

    // If we didn't find anything, try under the Swift 2 name.
    if (!matchedAny) {
      if (auto swift2Decl = cast_or_null<ValueDecl>(
                              importDeclReal(clangDecl, Version::Swift2))) {
        if (swift2Decl->getFullName().matchesRef(name)) {
          consumer.foundDecl(swift2Decl, DeclVisibilityKind::DynamicLookup);
        }
      }
    }
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
    lookupObjCMembers(table, SwiftContext.getIdentifier(baseName), consumer);
  }
}

EffectiveClangContext ClangImporter::Implementation::getEffectiveClangContext(
                        NominalTypeDecl *nominal) {
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

  // Resolve the type.
  if (auto typeResolver = getTypeResolver())
    typeResolver->resolveDeclSignature(nominal);

  // If it's an @objc entity, go look for it.
  if (nominal->isObjC()) {
    // Map the name. If we can't represent the Swift name in Clang.
    // FIXME: We should be using the Objective-C name here!
    auto clangName = exportName(nominal->getName());
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
  }

  return EffectiveClangContext();
}

void ClangImporter::dumpSwiftLookupTables() {
  Impl.dumpSwiftLookupTables();
}

void ClangImporter::Implementation::dumpSwiftLookupTables() {
  // Sort the module names so we can print in a deterministic order.
  SmallVector<StringRef, 4> moduleNames;
  for (const auto &lookupTable : LookupTables) {
    moduleNames.push_back(lookupTable.first());
  }
  array_pod_sort(moduleNames.begin(), moduleNames.end());

  // Print out the lookup tables for the various modules.
  for (auto moduleName : moduleNames) {
    llvm::errs() << "<<" << moduleName << " lookup table>>\n";
    LookupTables[moduleName]->deserializeAll();
    LookupTables[moduleName]->dump();
  }

  llvm::errs() << "<<Bridging header lookup table>>\n";
  BridgingHeaderLookupTable->dump();
}
