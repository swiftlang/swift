//===--- ClangImporter.cpp - Import Clang Modules -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
      Module *nativeImported = Impl.finishLoadingClangModule(Importer, imported,
                                                             /*adapter=*/true);
      Impl.ImportedHeaderExports.push_back({ /*filter=*/{}, nativeImported });
    }

    virtual void InclusionDirective(clang::SourceLocation HashLoc,
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

    virtual void moduleImport(clang::SourceLocation ImportLoc,
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

  class HeaderParsingASTConsumer : public clang::ASTConsumer {
    SmallVector<clang::DeclGroupRef, 4> DeclGroups;
  public:
    void
    HandleTopLevelDeclInObjCContainer(clang::DeclGroupRef decls) override {
      DeclGroups.push_back(decls);
    }

    ArrayRef<clang::DeclGroupRef> getAdditionalParsedDecls() {
      return DeclGroups;
    }

    void reset() {
      DeclGroups.clear();
    }
  };

  class ParsingAction : public clang::ASTFrontendAction {
    std::unique_ptr<clang::ASTConsumer>
    CreateASTConsumer(clang::CompilerInstance &CI, StringRef InFile) override {
      return llvm::make_unique<HeaderParsingASTConsumer>();
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

    const char *getBufferIdentifier() const override {
      return name.c_str();
    }

    BufferKind getBufferKind() const override {
      return MemoryBuffer_Malloc;
    }
  };
}

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
}

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

  // Construct the invocation arguments for the current target.
  // Add target-independent options first.
  invocationArgStrs.insert(
      invocationArgStrs.end(),
      {

          // Enable modules
          "-fmodules",
          "-fmodules-validate-system-headers",
          "-Werror=non-modular-include-in-framework-module",
          // Enable implicit module maps (implied by "-fmodules")
          "-fimplicit-module-maps",
          "-Xclang", "-fmodule-feature", "-Xclang", "swift",

          // Don't emit LLVM IR.
          "-fsyntax-only",

          "-fretain-comments-from-system-headers",

          SHIMS_INCLUDE_FLAG, searchPathOpts.RuntimeResourcePath,
      });

  // Set C language options.
  if (triple.isOSDarwin()) {
    invocationArgStrs.insert(invocationArgStrs.end(), {
      // Darwin uses Objective-C ARC.
      "-x", "objective-c", "-std=gnu11", "-fobjc-arc", "-fblocks",

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
        V.preprocessorDefinition(),
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

  // Set the module cache path.
  if (!moduleCachePath.empty()) {
    invocationArgStrs.push_back("-fmodules-cache-path=");
    invocationArgStrs.back().append(moduleCachePath);
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

  // Add API notes paths.
  for (const auto &searchPath : searchPathOpts.ImportSearchPaths) {
    invocationArgStrs.push_back("-iapinotes-modules");
    invocationArgStrs.push_back(searchPath);
  }
  invocationArgStrs.push_back("-iapinotes-modules");
  invocationArgStrs.push_back(searchPathOpts.RuntimeLibraryImportPath);
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
  llvm::IntrusiveRefCntPtr<CompilerInvocation> invocation{
    clang::createInvocationFromCommandLine(invocationArgs, clangDiags)
  };
  if (!invocation)
    return nullptr;
  importer->Impl.Invocation = invocation;

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
  clang::PreprocessorOptions &ppOpts = invocation->getPreprocessorOpts();
  ppOpts.addRemappedFile(Implementation::moduleImportBufferName,
                         sourceBuffer.release());

  // Install a Clang module file extension to build Swift name lookup tables.
  invocation->getFrontendOpts().ModuleFileExtensions.push_back(
    new Implementation::SwiftNameLookupExtension(importer->Impl));

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
  instance.setInvocation(&*invocation);

  // Create the associated action.
  importer->Impl.Action.reset(new ParsingAction);
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
                                                /*skipFunctionBodies=*/false));

  clangPP.EnterMainSourceFile();
  importer->Impl.Parser->Initialize();

  // Prefer frameworks over plain headers.
  // We add search paths here instead of when building the initial invocation
  // so that (a) we use the same code as search paths for imported modules,
  // and (b) search paths are always added after -Xcc options.
  SearchPathOptions &searchPathOpts = ctx.SearchPathOpts;
  for (auto path : searchPathOpts.FrameworkSearchPaths)
    importer->addSearchPath(path, /*isFramework*/true);
  for (auto path : searchPathOpts.ImportSearchPaths)
    importer->addSearchPath(path, /*isFramework*/false);

  // FIXME: These decls are not being parsed correctly since (a) some of the
  // callbacks are still being added, and (b) the logic to parse them has
  // changed.
  clang::Parser::DeclGroupPtrTy parsed;
  while (!importer->Impl.Parser->ParseTopLevelDecl(parsed)) {
    for (auto *D : parsed.get()) {
      importer->Impl.addBridgeHeaderTopLevelDecls(D);

      if (auto named = dyn_cast<clang::NamedDecl>(D)) {
        importer->Impl.addEntryToLookupTable(
          instance.getSema(),
          importer->Impl.BridgingHeaderLookupTable,
          named);
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
  auto *importedHeaderModule = Module::create(ctx.getIdentifier("__ObjC"), ctx);
  importer->Impl.ImportedHeaderUnit =
    new (ctx) ClangModuleUnit(*importedHeaderModule, *importer, nullptr);
  importedHeaderModule->addFile(*importer->Impl.ImportedHeaderUnit);

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
                                               /*ignoreSysroot=*/true);
  return false;
}

void ClangImporter::Implementation::addEntryToLookupTable(
       clang::Sema &clangSema,
       SwiftLookupTable &table,
       clang::NamedDecl *named)
{
  // Determine whether this declaration is suppressed in Swift.
  if (shouldSuppressDeclImport(named)) return;

  // Leave incomplete struct/enum/union types out of the table; Swift only
  // handles pointers to them.
  // FIXME: At some point we probably want to be importing incomplete types,
  // so that pointers to different incomplete types themselves have distinct
  // types. At that time it will be necessary to make the decision of whether
  // or not to import an incomplete type declaration based on whether it's
  // actually the struct backing a CF type:
  //
  //    typedef struct CGColor *CGColorRef;
  //
  // The best way to do this is probably to change CFDatabase.def to include
  // struct names when relevant, not just pointer names. That way we can check
  // both CFDatabase.def and the objc_bridge attribute and cover all our bases.
  if (auto *tagDecl = dyn_cast<clang::TagDecl>(named)) {
    if (!tagDecl->getDefinition())
      return;
  }

  // If we have a name to import as, add this entry to the table.
  if (auto importedName = importFullName(named, None, &clangSema)) {
    table.addEntry(importedName.Imported, named, importedName.EffectiveContext);

    // Also add the subscript entry, if needed.
    if (importedName.isSubscriptAccessor())
      table.addEntry(DeclName(SwiftContext, SwiftContext.Id_subscript,
                              ArrayRef<Identifier>()),
                     named, importedName.EffectiveContext);

    // Import the Swift 2 name of this entity, and record it as well if it is
    // different.
    if (auto swift2Name = importFullName(named, ImportNameFlags::Swift2Name,
                                         &clangSema)) {
      if (swift2Name.Imported != importedName.Imported)
        table.addEntry(swift2Name.Imported, named, swift2Name.EffectiveContext);
    }
  } else if (auto category = dyn_cast<clang::ObjCCategoryDecl>(named)) {
    // If the category is invalid, don't add it.
    if (category->isInvalidDecl()) return;

    table.addCategory(category);
  }

  // Walk the members of any context that can have nested members.
  if (isa<clang::TagDecl>(named) ||
      isa<clang::ObjCInterfaceDecl>(named) ||
      isa<clang::ObjCProtocolDecl>(named) ||
      isa<clang::ObjCCategoryDecl>(named)) {
    clang::DeclContext *dc = cast<clang::DeclContext>(named);
    for (auto member : dc->decls()) {
      if (auto namedMember = dyn_cast<clang::NamedDecl>(member))
        addEntryToLookupTable(clangSema, table, namedMember);
    }
  }
}

void ClangImporter::Implementation::addMacrosToLookupTable(
       clang::ASTContext &clangCtx,
       clang::Preprocessor &pp,
       SwiftLookupTable &table) {
  for (const auto &macro : pp.macros(false)) {
    // Find the local history of this macro directive.
    clang::MacroDirective *MD = pp.getLocalMacroDirectiveHistory(macro.first);

    // Walk the history.
    for (; MD; MD = MD->getPrevious()) {
      // Don't look at any definitions that are followed by undefs.
      // FIXME: This isn't quite correct across explicit submodules -- one
      // submodule might define a macro, while another defines and then
      // undefines the same macro. If they are processed in that order, the
      // history will have the undef at the end, and we'll miss the first
      // definition.
      if (isa<clang::UndefMacroDirective>(MD))
        break;

      // Only interested in macro definitions.
      auto *defMD = dyn_cast<clang::DefMacroDirective>(MD);
      if (!defMD) continue;

      // Is this definition from this module?
      auto info = defMD->getInfo();
      if (!info || info->isFromASTFile()) continue;

      // If we hit a builtin macro, we're done.
      if (info->isBuiltinMacro()) break;

      // If we hit a macro with invalid or predefined location, we're done.
      auto loc = defMD->getLocation();
      if (loc.isInvalid()) break;
      if (pp.getSourceManager().getFileID(loc) == pp.getPredefinesFileID())
        break;

      // Add this entry.
      auto name = importMacroName(macro.first, info, clangCtx);
      if (name.empty()) continue;
      table.addEntry(name, info, clangCtx.getTranslationUnitDecl(), &pp);
    }
  }
}

void ClangImporter::Implementation::finalizeLookupTable(
       clang::ASTContext &clangCtx,
       clang::Preprocessor &pp,
       SwiftLookupTable &table) {
  // Resolve any unresolved entries.
  SmallVector<SwiftLookupTable::SingleEntry, 4> unresolved;
  if (table.resolveUnresolvedEntries(unresolved)) {
    // Complain about unresolved entries that remain.
    for (auto entry : unresolved) {
      auto decl = entry.get<clang::NamedDecl *>();
      auto swiftName = decl->getAttr<clang::SwiftNameAttr>();

      SwiftContext.Diags.diagnose(SourceLoc(), diag::unresolvable_clang_decl,
                                  decl->getNameAsString(),
                                  swiftName->getName());
    }
  }
}

bool ClangImporter::Implementation::importHeader(
    Module *adapter, StringRef headerName, SourceLoc diagLoc,
    bool trackParsedSymbols,
    std::unique_ptr<llvm::MemoryBuffer> sourceBuffer) {
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

  pp.EnterSourceFile(bufferID, /*directoryLookup=*/nullptr, /*loc=*/{});
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

  // We can't do this as we're parsing because we may want to resolve naming
  // conflicts between the things we've parsed.
  for (auto group : allParsedDecls)
    for (auto *D : group)
      if (auto named = dyn_cast<clang::NamedDecl>(D))
        addEntryToLookupTable(getClangSema(), BridgingHeaderLookupTable, named);

  pp.EndSourceFile();
  bumpGeneration();

  // Add any defined macros to the bridging header lookup table.
  addMacrosToLookupTable(getClangASTContext(), getClangPreprocessor(),
                         BridgingHeaderLookupTable);

  // Wrap all Clang imports under a Swift import decl.
  for (auto &Import : BridgeHeaderTopLevelImports) {
    if (auto *ClangImport = Import.dyn_cast<clang::ImportDecl*>()) {
      Import = createImportDecl(SwiftContext, adapter, ClangImport, {});
    }
  }

  // Finalize the lookup table, which may fail.
  finalizeLookupTable(getClangASTContext(), getClangPreprocessor(),
                      BridgingHeaderLookupTable);

  // FIXME: What do we do if there was already an error?
  if (!hadError && clangDiags.hasErrorOccurred()) {
    SwiftContext.Diags.diagnose(diagLoc, diag::bridging_header_error,
                                headerName);
    return true;
  }

  return false;
}

bool ClangImporter::importHeader(StringRef header, Module *adapter,
                                 off_t expectedSize, time_t expectedModTime,
                                 StringRef cachedContents, SourceLoc diagLoc) {
  clang::FileManager &fileManager = Impl.Instance->getFileManager();
  const clang::FileEntry *headerFile = fileManager.getFile(header,
                                                           /*open=*/true);
  if (headerFile && headerFile->getSize() == expectedSize &&
      headerFile->getModificationTime() == expectedModTime) {
    return importBridgingHeader(header, adapter, diagLoc);
  }

  if (!cachedContents.empty() && cachedContents.back() == '\0')
    cachedContents = cachedContents.drop_back();
  std::unique_ptr<llvm::MemoryBuffer> sourceBuffer{
    llvm::MemoryBuffer::getMemBuffer(cachedContents, header)
  };
  return Impl.importHeader(adapter, header, diagLoc, /*trackParsedSymbols=*/false,
                           std::move(sourceBuffer));
}

bool ClangImporter::importBridgingHeader(StringRef header, Module *adapter,
                                         SourceLoc diagLoc,
                                         bool trackParsedSymbols) {
  clang::FileManager &fileManager = Impl.Instance->getFileManager();
  const clang::FileEntry *headerFile = fileManager.getFile(header,
                                                           /*open=*/true);
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
                           std::move(sourceBuffer));
}

std::string ClangImporter::getBridgingHeaderContents(StringRef headerPath,
                                                     off_t &fileSize,
                                                     time_t &fileModTime) {
  llvm::IntrusiveRefCntPtr<clang::CompilerInvocation> invocation{
    new clang::CompilerInvocation(*Impl.Invocation)
  };
  invocation->getFrontendOpts().DisableFree = false;
  invocation->getFrontendOpts().Inputs.clear();
  invocation->getFrontendOpts().Inputs.push_back(
      clang::FrontendInputFile(headerPath, clang::IK_ObjC));

  invocation->getPreprocessorOpts().resetNonModularOptions();

  clang::CompilerInstance rewriteInstance(
    Impl.Instance->getPCHContainerOperations());
  rewriteInstance.setInvocation(&*invocation);
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
    StringRef full = sub->getFullModuleName();
    names.push_back(full.substr(submoduleNameLength + 1).str());
  }
}

bool ClangImporter::isModuleImported(const clang::Module *M) {
  return M->NameVisibility == clang::Module::NameVisibilityKind::AllVisible;
}

Module *ClangImporter::loadModule(
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

  return Impl.finishLoadingClangModule(*this, clangModule, /*adapter=*/false);
}

Module *ClangImporter::Implementation::finishLoadingClangModule(
    ClangImporter &owner,
    const clang::Module *clangModule,
    bool findAdapter) {
  assert(clangModule);

  // Bump the generation count.
  bumpGeneration();

  auto &cacheEntry = ModuleWrappers[clangModule];
  Module *result;
  ClangModuleUnit *wrapperUnit;
  if ((wrapperUnit = cacheEntry.getPointer())) {
    result = wrapperUnit->getParentModule();
    if (!cacheEntry.getInt()) {
      // Force load adapter modules for all imported modules.
      // FIXME: This forces the creation of wrapper modules for all imports as
      // well, and may do unnecessary work.
      cacheEntry.setInt(true);
      result->forAllVisibleModules({}, [&](Module::ImportedModule import) {});
    }
  } else {
    // Build the representation of the Clang module in Swift.
    // FIXME: The name of this module could end up as a key in the ASTContext,
    // but that's not correct for submodules.
    Identifier name = SwiftContext.getIdentifier((*clangModule).Name);
    result = Module::create(name, SwiftContext);
    // Silence error messages about testably importing a Clang module.
    result->setTestingEnabled();

    wrapperUnit =
      new (SwiftContext) ClangModuleUnit(*result, owner, clangModule);
    result->addFile(*wrapperUnit);
    cacheEntry.setPointerAndInt(wrapperUnit, true);

    // Force load adapter modules for all imported modules.
    // FIXME: This forces the creation of wrapper modules for all imports as
    // well, and may do unnecessary work.
    result->forAllVisibleModules({}, [](Module::ImportedModule import) {});
  }

  if (clangModule->isSubModule()) {
    finishLoadingClangModule(owner, clangModule->getTopLevelModule(), true);
  } else {
    Module *&loaded = SwiftContext.LoadedModules[result->getName()];
    if (!loaded)
      loaded = result;
  }

  if (findAdapter)
    if (Module *adapter = wrapperUnit->getAdapterModule())
      result = adapter;

  return result;
}

Module *ClangImporter::getImportedHeaderModule() const {
  return Impl.ImportedHeaderUnit->getParentModule();
}

ClangImporter::Implementation::Implementation(ASTContext &ctx,
                                              const ClangImporterOptions &opts)
  : SwiftContext(ctx),
    ImportForwardDeclarations(opts.ImportForwardDeclarations),
    InferImportAsMember(opts.InferImportAsMember),
    DisableSwiftBridgeAttr(opts.DisableSwiftBridgeAttr),
    HonorSwiftNewtypeAttr(opts.HonorSwiftNewtypeAttr),
    BridgingHeaderLookupTable(nullptr)
{
  // Add filters to determine if a Clang availability attribute
  // applies in Swift, and if so, what is the cutoff for deprecated
  // declarations that are now considered unavailable in Swift.

  if (ctx.LangOpts.Target.isiOS() && !ctx.LangOpts.Target.isTvOS()) {
    if (!ctx.LangOpts.EnableAppExtensionRestrictions) {
      PlatformAvailabilityFilter =
        [](StringRef Platform) { return Platform == "ios"; };
    } else {
      PlatformAvailabilityFilter =
        [](StringRef Platform) {
          return Platform == "ios" ||
                 Platform == "ios_app_extension"; };
    }
    // Anything deprecated in iOS 7.x and earlier is unavailable in Swift.
    DeprecatedAsUnavailableFilter =
      [](unsigned major, llvm::Optional<unsigned> minor) { return major <= 7; };
    DeprecatedAsUnavailableMessage =
      "APIs deprecated as of iOS 7 and earlier are unavailable in Swift";
  } else if (ctx.LangOpts.Target.isTvOS()) {
    if (!ctx.LangOpts.EnableAppExtensionRestrictions) {
      PlatformAvailabilityFilter =
        [](StringRef Platform) { return Platform == "tvos"; };
    } else {
      PlatformAvailabilityFilter =
        [](StringRef Platform) {
          return Platform == "tvos" ||
                 Platform == "tvos_app_extension"; };
    }
    // Anything deprecated in iOS 7.x and earlier is unavailable in Swift.
    DeprecatedAsUnavailableFilter =
      [](unsigned major, llvm::Optional<unsigned> minor) { return major <= 7; };
    DeprecatedAsUnavailableMessage =
      "APIs deprecated as of iOS 7 and earlier are unavailable in Swift";
  } else if (ctx.LangOpts.Target.isWatchOS()) {
    if (!ctx.LangOpts.EnableAppExtensionRestrictions) {
      PlatformAvailabilityFilter =
        [](StringRef Platform) { return Platform == "watchos"; };
    } else {
      PlatformAvailabilityFilter =
        [](StringRef Platform) {
          return Platform == "watchos" ||
                 Platform == "watchos_app_extension"; };
    }
    // No deprecation filter on watchOS
    DeprecatedAsUnavailableFilter =
      [](unsigned major, llvm::Optional<unsigned> minor) { return false; };
    DeprecatedAsUnavailableMessage = "";
  }
  else if (ctx.LangOpts.Target.isMacOSX()) {
    if (!ctx.LangOpts.EnableAppExtensionRestrictions) {
      PlatformAvailabilityFilter =
      [](StringRef Platform) { return Platform == "macos"; };
    } else {
      PlatformAvailabilityFilter =
      [](StringRef Platform) {
        return Platform == "macos" ||
               Platform == "macos_app_extension"; };
    }
    // Anything deprecated in OSX 10.9.x and earlier is unavailable in Swift.
    DeprecatedAsUnavailableFilter =
      [](unsigned major, llvm::Optional<unsigned> minor) {
        return major < 10 ||
               (major == 10 && (!minor.hasValue() || minor.getValue() <= 9));
    };
    DeprecatedAsUnavailableMessage =
      "APIs deprecated as of OS X 10.9 and earlier are unavailable in Swift";
  }
}


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
  auto wrapper = Module::create(name, SwiftContext);
  // Silence error messages about testably importing a Clang module.
  wrapper->setTestingEnabled();

  auto file = new (SwiftContext) ClangModuleUnit(*wrapper, importer,
                                                 underlying);
  wrapper->addFile(*file);
  cacheEntry.setPointer(file);

  return file;
}

Optional<const clang::Decl *>
ClangImporter::Implementation::getDefinitionForClangTypeDecl(
    const clang::Decl *D) {
  if (auto OID = dyn_cast<clang::ObjCInterfaceDecl>(D))
    return OID->getDefinition();

  if (auto TD = dyn_cast<clang::TagDecl>(D))
    return TD->getDefinition();

  if (auto OPD = dyn_cast<clang::ObjCProtocolDecl>(D))
    return OPD->getDefinition();

  return None;
}

Optional<clang::Module *>
ClangImporter::Implementation::getClangSubmoduleForDecl(
    const clang::Decl *D,
    bool allowForwardDeclaration) {
  const clang::Decl *actual = nullptr;

  // Put an Objective-C class into the module that contains the @interface
  // definition, not just some @class forward declaration.
  if (auto maybeDefinition = getDefinitionForClangTypeDecl(D)) {
    actual = maybeDefinition.getValue();
    if (!actual && !allowForwardDeclaration)
      return None;
  }

  if (!actual)
    actual = D->getCanonicalDecl();

  return actual->getImportedOwningModule();
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

/// \brief Determine whether the given name is reserved for Swift.
bool ClangImporter::Implementation::isSwiftReservedName(StringRef name) {
  tok kind = Lexer::kindOfIdentifier(name, /*InSILMode=*/false);
  return (kind != tok::identifier);
}

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

/// Determine whether the given Clang selector matches the given
/// selector pieces.
static bool isNonNullarySelector(clang::Selector selector,
                                 ArrayRef<StringRef> pieces) {
  unsigned n = selector.getNumArgs();
  if (n == 0) return false;
  if (n != pieces.size()) return false;

  for (unsigned i = 0; i != n; ++i) {
    if (selector.getNameForSlot(i) != pieces[i]) return false;
  }

  return true;
}

/// Whether we should make a variadic method with the given selector
/// non-variadic.
static bool shouldMakeSelectorNonVariadic(clang::Selector selector) {
  // This is UIActionSheet's designated initializer.
  if (isNonNullarySelector(selector,
                           { "initWithTitle",
                             "delegate",
                             "cancelButtonTitle",
                             "destructiveButtonTitle",
                             "otherButtonTitles" }))
    return true;

  // This is UIAlertView's designated initializer.
  if (isNonNullarySelector(selector,
                           { "initWithTitle",
                             "message",
                             "delegate",
                             "cancelButtonTitle",
                             "otherButtonTitles" }))
    return true;

  // Nothing else for now.
  return false;
}

static bool isBlockParameter(const clang::ParmVarDecl *param) {
  return param->getType()->isBlockPointerType();
}

static bool isErrorOutParameter(const clang::ParmVarDecl *param,
                         ForeignErrorConvention::IsOwned_t &isErrorOwned) {
  clang::QualType type = param->getType();

  // Must be a pointer.
  auto ptrType = type->getAs<clang::PointerType>();
  if (!ptrType) return false;
  type = ptrType->getPointeeType();

  // For NSError**, take ownership from the qualifier.
  if (auto objcPtrType = type->getAs<clang::ObjCObjectPointerType>()) {
    auto iface = objcPtrType->getInterfaceDecl();
    if (iface && iface->getName() == "NSError") {
      switch (type.getObjCLifetime()) {
      case clang::Qualifiers::OCL_None:
        llvm_unreachable("not in ARC?");

      case clang::Qualifiers::OCL_ExplicitNone:
      case clang::Qualifiers::OCL_Autoreleasing:
        isErrorOwned = ForeignErrorConvention::IsNotOwned;
        return true;

      case clang::Qualifiers::OCL_Weak:
        // We just don't know how to handle this.
        return false;

      case clang::Qualifiers::OCL_Strong:
        isErrorOwned = ForeignErrorConvention::IsOwned;
        return false;
      }
      llvm_unreachable("bad error ownership");
    }
  }
  return false;
}

static bool isBoolType(clang::ASTContext &ctx, clang::QualType type) {
  do {
    // Check whether we have a typedef for "BOOL" or "Boolean".
    if (auto typedefType = dyn_cast<clang::TypedefType>(type.getTypePtr())) {
      auto typedefDecl = typedefType->getDecl();
      if (typedefDecl->getName() == "BOOL" ||
          typedefDecl->getName() == "Boolean")
        return true;

      type = typedefDecl->getUnderlyingType();
      continue;
    }

    // Try to desugar one level...
    clang::QualType desugared = type.getSingleStepDesugaredType(ctx);
    if (desugared.getTypePtr() == type.getTypePtr())
      break;

    type = desugared;
  } while (!type.isNull());

  return false;
}

static bool isIntegerType(clang::QualType clangType) {
  if (auto builtinTy = clangType->getAs<clang::BuiltinType>()) {
    return (builtinTy->getKind() >= clang::BuiltinType::Bool &&
            builtinTy->getKind() <= clang::BuiltinType::UInt128) ||
           (builtinTy->getKind() >= clang::BuiltinType::SChar &&
            builtinTy->getKind() <= clang::BuiltinType::Int128);
  }

  return false;
}

/// Whether the given Objective-C type can be imported as an optional type.
static bool canImportAsOptional(clang::ASTContext &ctx, clang::QualType type) {
  // Note: this mimics ImportHint::canImportAsOptional.

  // Objective-C object pointers.
  if (type->getAs<clang::ObjCObjectPointerType>()) return true;

  // Block and C pointers, including CF types.
  if (type->isBlockPointerType() || type->isPointerType()) return true;

  return false;
}

static Optional<ForeignErrorConvention::Kind>
classifyMethodErrorHandling(const clang::ObjCMethodDecl *clangDecl,
                            OptionalTypeKind resultOptionality) {
  // TODO: opt out any non-standard methods here?
  clang::ASTContext &clangCtx = clangDecl->getASTContext();

  // Check for an explicit attribute.
  if (auto attr = clangDecl->getAttr<clang::SwiftErrorAttr>()) {
    switch (attr->getConvention()) {
    case clang::SwiftErrorAttr::None:
      return None;

    case clang::SwiftErrorAttr::NonNullError:
      return ForeignErrorConvention::NonNilError;

    // Only honor null_result if we actually imported as a
    // non-optional type.
    case clang::SwiftErrorAttr::NullResult:
      if (resultOptionality != OTK_None &&
          canImportAsOptional(clangCtx, clangDecl->getReturnType()))
        return ForeignErrorConvention::NilResult;
      return None;

    // Preserve the original result type on a zero_result unless we
    // imported it as Bool.
    case clang::SwiftErrorAttr::ZeroResult:
      if (isBoolType(clangCtx, clangDecl->getReturnType())) {
        return ForeignErrorConvention::ZeroResult;
      } else if (isIntegerType(clangDecl->getReturnType())) {
        return ForeignErrorConvention::ZeroPreservedResult;
      }
      return None;

    // There's no reason to do the same for nonzero_result because the
    // only meaningful value remaining would be zero.
    case clang::SwiftErrorAttr::NonZeroResult:
      if (isIntegerType(clangDecl->getReturnType()))
        return ForeignErrorConvention::NonZeroResult;
      return None;
    }
    llvm_unreachable("bad swift_error kind");
  }

  // Otherwise, apply the default rules.

  // For bool results, a zero value is an error.
  if (isBoolType(clangCtx, clangDecl->getReturnType())) {
    return ForeignErrorConvention::ZeroResult;
  }

  // For optional reference results, a nil value is normally an error.
  if (resultOptionality != OTK_None &&
      canImportAsOptional(clangCtx, clangDecl->getReturnType())) {
    return ForeignErrorConvention::NilResult;
  }

  return None;
}

static const char ErrorSuffix[] = "AndReturnError";
static const char AltErrorSuffix[] = "WithError";

/// Look for a method that will import to have the same name as the
/// given method after importing the Nth parameter as an elided error
/// parameter.
static bool hasErrorMethodNameCollision(ClangImporter::Implementation &importer,
                                        const clang::ObjCMethodDecl *method,
                                        unsigned paramIndex,
                                        StringRef suffixToStrip) {
  // Copy the existing selector pieces into an array.
  auto selector = method->getSelector();
  unsigned numArgs = selector.getNumArgs();
  assert(numArgs > 0);

  SmallVector<clang::IdentifierInfo *, 4> chunks;
  for (unsigned i = 0, e = selector.getNumArgs(); i != e; ++i) {
    chunks.push_back(selector.getIdentifierInfoForSlot(i));
  }

  auto &ctx = method->getASTContext();
  if (paramIndex == 0 && !suffixToStrip.empty()) {
    StringRef name = chunks[0]->getName();
    assert(name.endswith(suffixToStrip));
    name = name.drop_back(suffixToStrip.size());
    chunks[0] = &ctx.Idents.get(name);
  } else if (paramIndex != 0) {
    chunks.erase(chunks.begin() + paramIndex);
  }

  auto newSelector = ctx.Selectors.getSelector(numArgs - 1, chunks.data());
  const clang::ObjCMethodDecl *conflict;
  if (auto iface = method->getClassInterface()) {
    conflict = iface->lookupMethod(newSelector, method->isInstanceMethod());
  } else {
    auto protocol = cast<clang::ObjCProtocolDecl>(method->getDeclContext());
    conflict = protocol->getMethod(newSelector, method->isInstanceMethod());
  }

  if (conflict == nullptr)
    return false;

  // Look to see if the conflicting decl is unavailable, either because it's
  // been marked NS_SWIFT_UNAVAILABLE, because it's actually marked unavailable,
  // or because it was deprecated before our API sunset. We can handle
  // "conflicts" where one form is unavailable.
  return !importer.isUnavailableInSwift(conflict);
}

/// Determine the optionality of the given Objective-C method.
///
/// \param method The Clang method.
static OptionalTypeKind getResultOptionality(
                          const clang::ObjCMethodDecl *method) {
  auto &clangCtx = method->getASTContext();

  // If nullability is available on the type, use it.
  if (auto nullability = method->getReturnType()->getNullability(clangCtx)) {
    return ClangImporter::Implementation::translateNullability(*nullability);
  }

  // If there is a returns_nonnull attribute, non-null.
  if (method->hasAttr<clang::ReturnsNonNullAttr>())
    return OTK_None;

  // Default to implicitly unwrapped optionals.
  return OTK_ImplicitlyUnwrappedOptional;
}

static Optional<ClangImporter::Implementation::ImportedErrorInfo>
considerErrorImport(ClangImporter::Implementation &importer,
                    const clang::ObjCMethodDecl *clangDecl,
                    StringRef &baseName,
                    SmallVectorImpl<StringRef> &paramNames,
                    ArrayRef<const clang::ParmVarDecl *> params,
                    bool isInitializer,
                    bool hasCustomName) {
  // If the declaration name isn't parallel to the actual parameter
  // list (e.g. if the method has C-style parameter declarations),
  // don't try to apply error conventions.
  bool expectsToRemoveError =
      hasCustomName && paramNames.size() + 1 == params.size();
  if (!expectsToRemoveError && paramNames.size() != params.size())
    return None;

  for (unsigned index = params.size(); index-- != 0; ) {
    // Allow an arbitrary number of trailing blocks.
    if (isBlockParameter(params[index]))
      continue;

    // Otherwise, require the last parameter to be an out-parameter.
    auto isErrorOwned = ForeignErrorConvention::IsNotOwned;
    if (!isErrorOutParameter(params[index], isErrorOwned))
      break;

    auto errorKind =
      classifyMethodErrorHandling(clangDecl,
                                  getResultOptionality(clangDecl));
    if (!errorKind) return None;

    // Consider adjusting the imported declaration name to remove the
    // parameter.
    bool adjustName = !hasCustomName;

    // Never do this if it's the first parameter of a constructor.
    if (isInitializer && index == 0) {
      adjustName = false;
    }

    // If the error parameter is the first parameter, try removing the
    // standard error suffix from the base name.
    StringRef suffixToStrip;
    StringRef origBaseName = baseName;
    if (adjustName && index == 0 && paramNames[0].empty()) {
      if (baseName.endswith(ErrorSuffix))
        suffixToStrip = ErrorSuffix;
      else if (baseName.endswith(AltErrorSuffix))
        suffixToStrip = AltErrorSuffix;

      if (!suffixToStrip.empty()) {
        StringRef newBaseName = baseName.drop_back(suffixToStrip.size());
        if (newBaseName.empty() || importer.isSwiftReservedName(newBaseName)) {
          adjustName = false;
          suffixToStrip = {};
        } else {
          baseName = newBaseName;
        }
      }
    }

    // Also suppress name changes if there's a collision.
    // TODO: this logic doesn't really work with init methods
    // TODO: this privileges the old API over the new one
    if (adjustName &&
        hasErrorMethodNameCollision(importer, clangDecl, index,
                                    suffixToStrip)) {
      // If there was a conflict on the first argument, and this was
      // the first argument and we're not stripping error suffixes, just
      // give up completely on error import.
      if (index == 0 && suffixToStrip.empty()) {
        return None;

      // If there was a conflict stripping an error suffix, adjust the
      // name but don't change the base name.  This avoids creating a
      // spurious _: () argument.
      } else if (index == 0 && !suffixToStrip.empty()) {
        suffixToStrip = {};
        baseName = origBaseName;

      // Otherwise, give up on adjusting the name.
      } else {
        adjustName = false;
        baseName = origBaseName;
      }
    }

    // If we're adjusting the name, erase the error parameter.
    if (adjustName) {
      paramNames.erase(paramNames.begin() + index);
    }

    bool replaceParamWithVoid = !adjustName && !expectsToRemoveError;
    ClangImporter::Implementation::ImportedErrorInfo errorInfo {
      *errorKind, isErrorOwned, index, replaceParamWithVoid
    };
    return errorInfo;
  }

  // Didn't find an error parameter.
  return None;
}

// Determine whether we should strip the module prefix from a global-scope
// entity in the given module and with the given base name.
static unsigned stripModulePrefixLength(
                  const llvm::StringMap<std::string> &modulePrefixes,
                  StringRef moduleName,
                  StringRef baseName) {
  // Do we have a module prefix for this module?
  auto prefixPos = modulePrefixes.find(moduleName);
  if (prefixPos == modulePrefixes.end()) return 0;

  // Is the prefix actually there?
  if (!baseName.startswith(prefixPos->second)) return 0;

  // Check whether this is a known Foundation entity that conflicts with the
  // standard library.
  if (auto known = getKnownFoundationEntity(baseName))
    return 0;

  // If the character following the prefix is a '_', eat that, too.
  unsigned prefixLen = prefixPos->second.size();
  if (prefixLen < baseName.size() && baseName[prefixLen] == '_')
    ++prefixLen;

  // Make sure we're left with an identifier.
  if (prefixLen == baseName.size() ||
      !Lexer::isIdentifier(baseName.substr(prefixLen)))
    return 0;

  // We can strip the module prefix.
  return prefixLen;
}

/// Determine whether we should lowercase the first word of the given value
/// name.
static bool shouldLowercaseValueName(StringRef name) {
  // If we see any lowercase characters, we can lowercase.
  for (auto c : name) {
    if (clang::isLowercase(c)) return true;
  }

  // Otherwise, lowercasing will either be a no-op or we have ALL_CAPS.
  return false;
}

/// Returns true if it is expected that the macro is ignored.
static bool shouldIgnoreMacro(StringRef name, const clang::MacroInfo *macro) {
  // Ignore include guards.
  if (macro->isUsedForHeaderGuard())
    return true;

  // If there are no tokens, there is nothing to convert.
  if (macro->tokens_empty())
    return true;

  // Currently we only convert non-function-like macros.
  if (macro->isFunctionLike())
    return true;

  // Consult the blacklist of macros to suppress.
  auto suppressMacro =
    llvm::StringSwitch<bool>(name)
#define SUPPRESS_MACRO(NAME) .Case(#NAME, true)
#include "MacroTable.def"
    .Default(false);

  if (suppressMacro)
    return true;

  return false;
}

bool ClangImporter::shouldIgnoreMacro(StringRef Name,
                                      const clang::MacroInfo *Macro) {
  return ::shouldIgnoreMacro(Name, Macro);
}

Identifier ClangImporter::Implementation::importMacroName(
             const clang::IdentifierInfo *clangIdentifier,
             const clang::MacroInfo *macro,
             clang::ASTContext &clangCtx) {
  // If we're supposed to ignore this macro, return an empty identifier.
  if (::shouldIgnoreMacro(clangIdentifier->getName(), macro))
    return Identifier();

  // No transformation is applied to the name.
  StringRef name = clangIdentifier->getName();
  return SwiftContext.getIdentifier(name);
}

namespace {
  typedef ClangImporter::Implementation::ImportedName ImportedName;
}

/// Will recursively print out the fully qualified context for the given name.
/// Ends with a trailing "."
static void
printFullContextPrefix(ClangImporter::Implementation::ImportedName name,
                       llvm::raw_ostream &os,
                       ClangImporter::Implementation &Impl) {
  const clang::NamedDecl *newDeclContextNamed = nullptr;
  switch (name.EffectiveContext.getKind()) {
  case EffectiveClangContext::UnresolvedContext:
    os << name.EffectiveContext.getUnresolvedName() << ".";
    // And we're done!
    return;

  case EffectiveClangContext::DeclContext: {
    auto namedDecl =
        dyn_cast<clang::NamedDecl>(name.EffectiveContext.getAsDeclContext());
    if (!namedDecl) {
      // We're done
      return;
    }
    newDeclContextNamed = cast<clang::NamedDecl>(namedDecl);
    break;
  }

  case EffectiveClangContext::TypedefContext:
    newDeclContextNamed = name.EffectiveContext.getTypedefName();
    break;
  }

  // Now, let's print out the parent
  assert(newDeclContextNamed && "should of been set");
  auto parentName = Impl.importFullName(newDeclContextNamed);
  printFullContextPrefix(parentName, os, Impl);
  os << parentName.Imported << ".";
}

void ClangImporter::Implementation::printSwiftName(ImportedName name,
                                                   bool fullyQualified,
                                                   llvm::raw_ostream &os) {
  // Property accessors.
  bool isGetter = false;
  bool isSetter = false;
  switch (name.AccessorKind) {
  case ImportedAccessorKind::None:
    break;

  case ImportedAccessorKind::PropertyGetter:
  case ImportedAccessorKind::SubscriptGetter:
    os << "getter:";
    isGetter = true;
    break;

  case ImportedAccessorKind::PropertySetter:
  case ImportedAccessorKind::SubscriptSetter:
    os << "setter:";
    isSetter = true;
    break;
  }

  if (fullyQualified)
    printFullContextPrefix(name, os, *this);

  // Base name.
  os << name.Imported.getBaseName().str();

  // Determine the number of argument labels we'll be producing.
  auto argumentNames = name.Imported.getArgumentNames();
  unsigned numArguments = argumentNames.size();
  if (name.SelfIndex) ++numArguments;
  if (isSetter) ++numArguments;

  // If the result is a simple name that is not a getter, we're done.
  if (numArguments == 0 && name.Imported.isSimpleName() && !isGetter) return;

  // We need to produce a function name.
  os << "(";
  unsigned currentArgName = 0;
  for (unsigned i = 0; i != numArguments; ++i) {
    // The "self" parameter.
    if (name.SelfIndex && *name.SelfIndex == i) {
      os << "self:";
      continue;
    }

    if (currentArgName < argumentNames.size()) {
      if (argumentNames[currentArgName].empty())
        os << "_";
      else
        os << argumentNames[currentArgName].str();
      os << ":";
      ++currentArgName;
      continue;
    }

    // We don't have a name for this argument.
    os << "_:";
  }
  os << ")";
}

namespace llvm {
  // An Identifier is "pointer like".
  template<typename T> class PointerLikeTypeTraits;
  template<>
  class PointerLikeTypeTraits<swift::DeclName> {
  public:
    static inline void *getAsVoidPointer(swift::DeclName name) {
      return name.getOpaqueValue();
    }
    static inline swift::DeclName getFromVoidPointer(void *ptr) {
      return swift::DeclName::getFromOpaqueValue(ptr);
    }
    enum { NumLowBitsAvailable = 0 };
  };
}

/// Retrieve the name of the given Clang declaration context for
/// printing.
static StringRef getClangDeclContextName(const clang::DeclContext *dc) {
  auto type = ClangImporter::Implementation::getClangDeclContextType(dc);
  if (type.isNull()) return StringRef();

  return ClangImporter::Implementation::getClangTypeNameForOmission(
           dc->getParentASTContext(),
           type).Name;
}

namespace {
  /// Merge the a set of imported names produced for the overridden
  /// declarations of a given method or property.
  template<typename DeclType>
  void mergeOverriddenNames(ASTContext &ctx,
                            const DeclType *decl,
                            SmallVectorImpl<std::pair<const DeclType *,
                                                      ImportedName>>
                              &overriddenNames) {
    typedef std::pair<const DeclType *, ImportedName> OverriddenName;
    llvm::SmallPtrSet<DeclName, 4> known;
    (void)known.insert(DeclName());
    overriddenNames.erase(std::remove_if(overriddenNames.begin(),
                                         overriddenNames.end(),
                                         [&](OverriddenName overridden) {
                                           return !known.insert(
                                                     overridden.second.Imported)
                                             .second;
                                         }),
                          overriddenNames.end());

    if (overriddenNames.size() < 2)
      return;

    // Complain about inconsistencies.
    std::string nameStr;
    auto method = dyn_cast<clang::ObjCMethodDecl>(decl);
    if (method)
      nameStr = method->getSelector().getAsString();
    else
      nameStr = cast<clang::ObjCPropertyDecl>(decl)->getName().str();
    for (unsigned i = 1, n = overriddenNames.size(); i != n; ++i) {
      ctx.Diags.diagnose(SourceLoc(), diag::inconsistent_swift_name,
                         method == nullptr,
                         nameStr,
                         getClangDeclContextName(decl->getDeclContext()),
                         overriddenNames[0].second,
                         getClangDeclContextName(
                           overriddenNames[0].first->getDeclContext()),
                         overriddenNames[i].second,
                         getClangDeclContextName(
                           overriddenNames[i].first->getDeclContext()));
    }
  }
}

/// Determine whether the given Objective-C class, or any of its
/// superclasses, either has or inherits a swift_bridge attribute.
static bool
hasOrInheritsSwiftBridgeAttr(const clang::ObjCInterfaceDecl *objcClass) {
  do {
    // Look at the definition, if there is one.
    if (auto def = objcClass->getDefinition())
      objcClass = def;

    // Check for the swift_bridge attribute.
    if (objcClass->hasAttr<clang::SwiftBridgeAttr>())
      return true;

    // Follow the superclass chain.
    objcClass = objcClass->getSuperClass();
  } while (objcClass);

  return false;
}

/// Skip a leading 'k' in a 'kConstant' pattern
static StringRef stripLeadingK(StringRef name) {
  if (name.size() >= 2 && name[0] == 'k' &&
      clang::isUppercase(name[1]))
    return name.drop_front(1);
  return name;
}

/// Strips a trailing "Notification", if present. Returns {} if name doesn't end
/// in "Notification", or it there would be nothing left.
static StringRef stripNotification(StringRef name) {
  name = stripLeadingK(name);
  StringRef notification = "Notification";
  if (name.size() <= notification.size() || !name.endswith(notification))
    return {};
  return name.drop_back(notification.size());
}

bool ClangImporter::Implementation::isNSNotificationGlobal(
    const clang::NamedDecl *decl) {
  // Looking for: extern NSString *fooNotification;

  // Must be extern global variable
  auto vDecl = dyn_cast<clang::VarDecl>(decl);
  if (!vDecl || !vDecl->hasExternalFormalLinkage())
    return false;

  // No explicit swift_name
  if (decl->getAttr<clang::SwiftNameAttr>())
    return false;

  // Must end in Notification
  if (!vDecl->getDeclName().isIdentifier())
    return false;
  if (stripNotification(vDecl->getName()).empty())
    return false;

  // Must be NSString *
  if (!isNSString(vDecl->getType()))
    return false;

  // We're a match!
  return true;
}


/// Whether the decl is from a module who requested import-as-member inference
static bool moduleIsInferImportAsMember(const clang::NamedDecl *decl,
                                        clang::Sema &clangSema) {
  clang::Module *submodule;
  if (auto m = decl->getImportedOwningModule()) {
    submodule = m;
  } else if (auto m = decl->getLocalOwningModule()) {
    submodule = m;
  } else if (auto m = clangSema.getPreprocessor().getCurrentModule()) {
    submodule = m;
  } else if (auto m = clangSema.getPreprocessor().getCurrentSubmodule()) {
    submodule = m;
  } else {
    return false;
  }

  while (submodule) {
    if (submodule->IsSwiftInferImportAsMember) {
      // HACK HACK HACK: This is a workaround for some module invalidation issue
      // and inconsistency. This will go away soon.
      if (submodule->Name != "CoreGraphics")
        return false;
      return true;
    }
    submodule = submodule->Parent;
  }

  return false;
}

// If this decl is associated with a swift_newtype typedef, return it, otherwise
// null
clang::TypedefNameDecl *ClangImporter::Implementation::findSwiftNewtype(
    const clang::NamedDecl *decl, clang::Sema &clangSema, bool useSwift2Name) {
  // If we aren't honoring the swift_newtype attribute, don't even
  // bother looking. Similarly for swift2 names
  if (!HonorSwiftNewtypeAttr || useSwift2Name)
    return nullptr;

  auto varDecl = dyn_cast<clang::VarDecl>(decl);
  if (!varDecl)
    return nullptr;

  if (auto typedefTy = varDecl->getType()->getAs<clang::TypedefType>())
    if (getSwiftNewtypeAttr(typedefTy->getDecl(), false))
      return typedefTy->getDecl();

  // Special case: "extern NSString * fooNotification" adopts
  // NSNotificationName type, and is a member of NSNotificationName
  if (ClangImporter::Implementation::isNSNotificationGlobal(decl)) {
    clang::IdentifierInfo *notificationName =
        &clangSema.getASTContext().Idents.get("NSNotificationName");
    clang::LookupResult lookupResult(clangSema, notificationName,
                                     clang::SourceLocation(),
                                     clang::Sema::LookupOrdinaryName);
    if (!clangSema.LookupName(lookupResult, nullptr))
      return nullptr;
    auto nsDecl = lookupResult.getAsSingle<clang::TypedefNameDecl>();
    if (!nsDecl)
      return nullptr;

    // Make sure it also has a newtype decl on it
    if (getSwiftNewtypeAttr(nsDecl, false))
      return nsDecl;

    return nullptr;
  }

  return nullptr;
}

/// Match the name of the given Objective-C method to its enclosing class name
/// to determine the name prefix that would be stripped if the class method
/// were treated as an initializer.
static Optional<unsigned> matchFactoryAsInitName(
                            const clang::ObjCMethodDecl *method){
  // Only class methods can be mapped to initializers in this way.
  if (!method->isClassMethod()) return None;

  // Said class methods must be in an actual class.
  auto objcClass = method->getClassInterface();
  if (!objcClass) return None;

  // See if we can match the class name to the beginning of the first
  // selector piece.
  auto firstPiece = method->getSelector().getNameForSlot(0);
  StringRef firstArgLabel = matchLeadingTypeName(firstPiece,
                                                 objcClass->getName());
  if (firstArgLabel.size() == firstPiece.size())
    return None;

  // FIXME: Factory methods cannot have dummy parameters added for
  // historical reasons.
  if (!firstArgLabel.empty() && method->getSelector().getNumArgs() == 0)
    return None;

  // Return the prefix length.
  return firstPiece.size() - firstArgLabel.size();
}

/// Determine the kind of initializer the given factory method could be mapped
/// to, or produce \c None.
static Optional<CtorInitializerKind>
determineCtorInitializerKind(const clang::ObjCMethodDecl *method) {
  // Determine whether we have a suitable return type.
  if (method->hasRelatedResultType()) {
    // When the factory method has an "instancetype" result type, we
    // can import it as a convenience factory method.
    return CtorInitializerKind::ConvenienceFactory;
  }

  if (auto objcPtr = method->getReturnType()
                       ->getAs<clang::ObjCObjectPointerType>()) {
    auto objcClass = method->getClassInterface();
    if (!objcClass) return None;

    if (objcPtr->getInterfaceDecl() != objcClass) {
      // FIXME: Could allow a subclass here, but the rest of the compiler
      // isn't prepared for that yet.
      return None;
    }

    // Factory initializer.
    return CtorInitializerKind::Factory;
  }

  // Not imported as an initializer.
  return None;
}

/// Find the swift_name attribute associated with this declaration, if
/// any.
///
/// \param swift2Name When true, restrict the results to those that were
/// present in Swift 2.
static clang::SwiftNameAttr *findSwiftNameAttr(const clang::Decl *decl,
                                               bool swift2Name) {
  // Find the attribute.
  auto attr = decl->getAttr<clang::SwiftNameAttr>();
  if (!attr) return nullptr;

  // If we're not emulating the Swift 2 behavior, return what we got.
  if (!swift2Name) return attr;

  // API notes produce implicit attributes; ignore them because they weren't
  // used for naming in Swift 2.
  if (attr->isImplicit()) return nullptr;

  // Whitelist certain explicitly-written Swift names that were
  // permitted and used in Swift 2. All others are ignored, so that we are
  // assuming a more direct translation from the Objective-C APIs into Swift.

  if (auto enumerator = dyn_cast<clang::EnumConstantDecl>(decl)) {
    // Foundation's NSXMLDTDKind had an explicit swift_name attribute in
    // Swift 2. Honor it.
    if (enumerator->getName() == "NSXMLDTDKind") return attr;
    return nullptr;
  }

  if (auto method = dyn_cast<clang::ObjCMethodDecl>(decl)) {
    // Special case: mapping to an initializer.
    if (attr->getName().startswith("init(")) {
      // If we have a class method, honor the annotation to turn a class
      // method into an initializer.
      if (method->isClassMethod()) return attr;

      return nullptr;
    }

    // Special case: preventing a mapping to an initializer.
    if (matchFactoryAsInitName(method) && determineCtorInitializerKind(method))
      return attr;

    return nullptr;
  }

  return nullptr;
}

/// Prepare global name for importing onto a swift_newtype.
static StringRef determineSwiftNewtypeBaseName(StringRef baseName,
                                               StringRef newtypeName,
                                               bool &strippedPrefix) {
  StringRef newBaseName = stripLeadingK(baseName);
  if (newBaseName != baseName) {
    baseName = newBaseName;
    strippedPrefix = true;
  }

  // Special case: Strip Notification for NSNotificationName
  auto stripped = stripNotification(baseName);
  if (!stripped.empty())
    return stripped;

  bool nonIdentifier = false;
  auto pre = getCommonWordPrefix(newtypeName, baseName, nonIdentifier);
  if (pre.size()) {
    baseName = baseName.drop_front(pre.size());
    strippedPrefix = true;
  }

  return baseName;
}

auto ClangImporter::Implementation::importFullName(
       const clang::NamedDecl *D,
       ImportNameOptions options,
       clang::Sema *clangSemaOverride) -> ImportedName {
  clang::Sema &clangSema = clangSemaOverride ? *clangSemaOverride
                                             : getClangSema();
  ImportedName result;

  /// Whether we want the Swift 2.0 name.
  bool swift2Name = options.contains(ImportNameFlags::Swift2Name);

  // Objective-C categories and extensions don't have names, despite
  // being "named" declarations.
  if (isa<clang::ObjCCategoryDecl>(D))
    return result;

  // Dig out the definition, if there is one.
  if (auto def = getDefinitionForClangTypeDecl(D)) {
    if (*def)
      D = static_cast<const clang::NamedDecl *>(*def);
  }

  // Compute the effective context.
  auto dc = const_cast<clang::DeclContext *>(D->getDeclContext());

  // Enumerators can end up within their enclosing enum or in the global
  // scope, depending how their enclosing enumeration is imported.
  if (isa<clang::EnumConstantDecl>(D)) {
    auto enumDecl = cast<clang::EnumDecl>(dc);
    switch (getEnumKind(enumDecl, &clangSema.getPreprocessor())) {
    case EnumKind::Enum:
    case EnumKind::Options:
      // Enums are mapped to Swift enums, Options to Swift option sets.
      result.EffectiveContext = cast<clang::DeclContext>(enumDecl);
      break;

    case EnumKind::Constants:
    case EnumKind::Unknown:
      // The enum constant goes into the redeclaration context of the
      // enum.
      result.EffectiveContext = enumDecl->getRedeclContext();
      break;
    }
  // Import onto a swift_newtype if present
  } else if (auto newtypeDecl = findSwiftNewtype(D, clangSema, swift2Name)) {
    result.EffectiveContext = newtypeDecl;
    result.ImportAsMember = true;
  // Everything else goes into its redeclaration context.
  } else {
    result.EffectiveContext = dc->getRedeclContext();
  }

  // Anything in an Objective-C category or extension is adjusted to the
  // class context.
  if (auto category = dyn_cast_or_null<clang::ObjCCategoryDecl>(
                        result.EffectiveContext.getAsDeclContext())) {
    // If the enclosing category is invalid, we cannot import the declaration.
    if (category->isInvalidDecl()) return result;

    result.EffectiveContext = category->getClassInterface();
  }

  // Find the original method/property declaration and retrieve the
  // name from there.
  if (auto method = dyn_cast<clang::ObjCMethodDecl>(D)) {
    // Inherit the name from the "originating" declarations, if
    // there are any.
    SmallVector<std::pair<const clang::ObjCMethodDecl *, ImportedName>, 4>
      overriddenNames;
    SmallVector<const clang::ObjCMethodDecl *, 4> overriddenMethods;
    method->getOverriddenMethods(overriddenMethods);
    for (auto overridden : overriddenMethods) {
      const auto overriddenName = importFullName(overridden, options,
                                                 clangSemaOverride);
      if (overriddenName.Imported)
        overriddenNames.push_back({overridden, overriddenName});
    }

    // If we found any names of overridden methods, return those names.
    if (!overriddenNames.empty()) {
      if (overriddenNames.size() > 1)
        mergeOverriddenNames(SwiftContext, method, overriddenNames);
      overriddenNames[0].second.EffectiveContext = result.EffectiveContext;
      return overriddenNames[0].second;
    }
  } else if (auto property = dyn_cast<clang::ObjCPropertyDecl>(D)) {
    // Inherit the name from the "originating" declarations, if
    // there are any.
    if (auto getter = property->getGetterMethodDecl()) {
      SmallVector<std::pair<const clang::ObjCPropertyDecl *, ImportedName>, 4>
        overriddenNames;
      SmallVector<const clang::ObjCMethodDecl *, 4> overriddenMethods;
      SmallPtrSet<const clang::ObjCPropertyDecl *, 4> knownProperties;
      (void)knownProperties.insert(property);

      getter->getOverriddenMethods(overriddenMethods);
      for (auto overridden : overriddenMethods) {
        if (!overridden->isPropertyAccessor()) continue;
        auto overriddenProperty = overridden->findPropertyDecl(true);
        if (!overriddenProperty) continue;
        if (!knownProperties.insert(overriddenProperty).second) continue;

        const auto overriddenName = importFullName(overriddenProperty,
                                                   options,
                                                   clangSemaOverride);
        if (overriddenName.Imported)
          overriddenNames.push_back({overriddenProperty, overriddenName});
      }

      // If we found any names of overridden methods, return those names.
      if (!overriddenNames.empty()) {
        if (overriddenNames.size() > 1)
          mergeOverriddenNames(SwiftContext, property, overriddenNames);
        overriddenNames[0].second.EffectiveContext = result.EffectiveContext;
        return overriddenNames[0].second;
      }
    }
  }

  // If we have a swift_name attribute, use that.
  if (auto *nameAttr = findSwiftNameAttr(D, swift2Name)) {
    bool skipCustomName = false;

    // Parse the name.
    ParsedDeclName parsedName = parseDeclName(nameAttr->getName());
    if (!parsedName || parsedName.isOperator()) return result;

    // If we have an Objective-C method that is being mapped to an
    // initializer (e.g., a factory method whose name doesn't fit the
    // convention for factory methods), make sure that it can be
    // imported as an initializer.
    bool isInitializer = false;
    auto method = dyn_cast<clang::ObjCMethodDecl>(D);
    if (method) {
      unsigned initPrefixLength;
      if (parsedName.BaseName == "init" && parsedName.IsFunctionName) {
        if (!shouldImportAsInitializer(method, initPrefixLength,
                                       result.InitKind)) {
          // We cannot import this as an initializer anyway.
          return { };
        }

        // If this swift_name attribute maps a factory method to an
        // initializer and we were asked not to do so, ignore the
        // custom name.
        if (options.contains(ImportNameFlags::SuppressFactoryMethodAsInit) &&
            (result.InitKind == CtorInitializerKind::Factory ||
             result.InitKind == CtorInitializerKind::ConvenienceFactory)) {
          skipCustomName = true;
        } else {
          // Note that this is an initializer.
          isInitializer = true;
        }
      }
    }

    if (!skipCustomName) {
      result.HasCustomName = true;
      result.Imported = parsedName.formDeclName(SwiftContext);

      // Handle globals treated as members.
      if (parsedName.isMember()) {
        // FIXME: Make sure this thing is global.
        result.EffectiveContext = parsedName.ContextName;
        if (parsedName.SelfIndex)
          result.SelfIndex = parsedName.SelfIndex;
        result.ImportAsMember = true;

        if (parsedName.BaseName == "init")
          result.InitKind = CtorInitializerKind::Factory;
      }

      // Map property getters/setters.
      if (parsedName.IsGetter)
        result.AccessorKind = ImportedAccessorKind::PropertyGetter;
      else if (parsedName.IsSetter)
        result.AccessorKind = ImportedAccessorKind::PropertySetter;

      if (method && parsedName.IsFunctionName) {
        // Get the parameters.
        ArrayRef<const clang::ParmVarDecl *> params{
          method->param_begin(),
          method->param_end()
        };

        result.ErrorInfo = considerErrorImport(*this, method,
                                               parsedName.BaseName,
                                               parsedName.ArgumentLabels,
                                               params,
                                               isInitializer,
                                               /*hasCustomName=*/true);
      }

      return result;
    }
  } else if (!swift2Name &&
             (InferImportAsMember ||
              moduleIsInferImportAsMember(D, clangSema)) &&
             (isa<clang::VarDecl>(D) || isa<clang::FunctionDecl>(D)) &&
             dc->isTranslationUnit()) {
    auto inference = IAMResult::infer(SwiftContext, clangSema, D);
    if (inference.isImportAsMember()) {
      result.ImportAsMember = true;
      result.Imported = inference.name;
      result.EffectiveContext = inference.effectiveDC;

      // Instance or static
      if (inference.selfIndex)
        result.SelfIndex = inference.selfIndex;

      // Property
      if (inference.isGetter())
        result.AccessorKind = ImportedAccessorKind::PropertyGetter;
      else if (inference.isSetter())
        result.AccessorKind = ImportedAccessorKind::PropertySetter;

      return result;
    }
  }

  // For empty names, there is nothing to do.
  if (D->getDeclName().isEmpty()) return result;

  /// Whether the result is a function name.
  bool isFunction = false;
  bool isInitializer = false;
  unsigned initializerPrefixLen;
  StringRef baseName;
  SmallVector<StringRef, 4> argumentNames;
  SmallString<16> selectorSplitScratch;
  ArrayRef<const clang::ParmVarDecl *> params;
  switch (D->getDeclName().getNameKind()) {
  case clang::DeclarationName::CXXConstructorName:
  case clang::DeclarationName::CXXConversionFunctionName:
  case clang::DeclarationName::CXXDestructorName:
  case clang::DeclarationName::CXXLiteralOperatorName:
  case clang::DeclarationName::CXXOperatorName:
  case clang::DeclarationName::CXXUsingDirective:
    // Handling these is part of C++ interoperability.
    llvm_unreachable("unhandled C++ interoperability");

  case clang::DeclarationName::Identifier:
    // Map the identifier.
    baseName = D->getDeclName().getAsIdentifierInfo()->getName();

    // For Objective-C BOOL properties, use the name of the getter
    // which, conventionally, has an "is" prefix.
    if (!swift2Name) {
      if (auto property = dyn_cast<clang::ObjCPropertyDecl>(D)) {
        if (isBoolType(clangSema.Context, property->getType()))
          baseName = property->getGetterName().getNameForSlot(0);
      }
    }

    // For C functions, create empty argument names.
    if (auto function = dyn_cast<clang::FunctionDecl>(D)) {
      isFunction = true;
      params = { function->param_begin(), function->param_end() };
      for (auto param : params) {
        (void)param;
        argumentNames.push_back(StringRef());
      }
      if (function->isVariadic())
        argumentNames.push_back(StringRef());
    }
    break;

  case clang::DeclarationName::ObjCMultiArgSelector:
  case clang::DeclarationName::ObjCOneArgSelector:
  case clang::DeclarationName::ObjCZeroArgSelector: {
    auto objcMethod = cast<clang::ObjCMethodDecl>(D);
    isInitializer = shouldImportAsInitializer(objcMethod, initializerPrefixLen,
                                              result.InitKind);

    // If we would import a factory method as an initializer but were
    // asked not to, don't consider this as an initializer.
    if (isInitializer &&
        options.contains(ImportNameFlags::SuppressFactoryMethodAsInit) &&
        (result.InitKind == CtorInitializerKind::Factory ||
         result.InitKind == CtorInitializerKind::ConvenienceFactory)) {
      isInitializer = false;
    }

    // Map the Objective-C selector directly.
    auto selector = D->getDeclName().getObjCSelector();
    if (isInitializer)
      baseName = "init";
    else
      baseName = selector.getNameForSlot(0);

    // Get the parameters.
    params = { objcMethod->param_begin(), objcMethod->param_end() };

    // If we have a variadic method for which we need to drop the last
    // selector piece, do so now.
    unsigned numArgs = selector.getNumArgs();
    if (objcMethod->isVariadic() && shouldMakeSelectorNonVariadic(selector)) {
      --numArgs;
      result.DroppedVariadic = true;
      params = params.drop_back(1);
    }

    for (unsigned index = 0; index != numArgs; ++index) {
      if (index == 0) {
        argumentNames.push_back(StringRef());
      } else {
        StringRef argName = selector.getNameForSlot(index);
        argumentNames.push_back(argName);
      }
    }

    // For initializers, compute the first argument name.
    if (isInitializer) {
      // Skip over the prefix.
      auto argName = selector.getNameForSlot(0).substr(initializerPrefixLen);

      // Drop "With" if present after the "init".
      bool droppedWith = false;
      if (argName.startswith("With")) {
        argName = argName.substr(4);
        droppedWith = true;
      }

      // Lowercase the remaining argument name.
      argName = camel_case::toLowercaseWord(argName, selectorSplitScratch);

      // If we dropped "with" and ended up with a reserved name,
      // put "with" back.
      if (droppedWith && isSwiftReservedName(argName)) {
        selectorSplitScratch = "with";
        selectorSplitScratch += selector.getNameForSlot(0).substr(
                                  initializerPrefixLen + 4);
        argName = selectorSplitScratch;
      }

      // Set the first argument name to be the name we computed. If
      // there is no first argument, create one for this purpose.
      if (argumentNames.empty()) {
        if (!argName.empty()) {
          // FIXME: Record what happened here for the caller?
          argumentNames.push_back(argName);
        }
      } else {
        argumentNames[0] = argName;
      }
    }

    result.ErrorInfo = considerErrorImport(*this, objcMethod, baseName,
                                           argumentNames, params, isInitializer,
                                           /*hasCustomName=*/false);

    isFunction = true;

    // Is this one of the accessors for subscripts?
    if (objcMethod->getMethodFamily() == clang::OMF_None &&
        objcMethod->isInstanceMethod()) {
      if (isNonNullarySelector(objcMethod->getSelector(),
                               { "objectAtIndexedSubscript" }) ||
          isNonNullarySelector(objcMethod->getSelector(),
                               { "objectForKeyedSubscript" }))
        result.AccessorKind = ImportedAccessorKind::SubscriptGetter;
      else if (isNonNullarySelector(objcMethod->getSelector(),
                                    { "setObject", "atIndexedSubscript" }) ||
               isNonNullarySelector(objcMethod->getSelector(),
                                    { "setObject", "forKeyedSubscript" }))
        result.AccessorKind = ImportedAccessorKind::SubscriptSetter;
    }

    break;
  }
  }

  // Perform automatic name transformations.

  // Enumeration constants may have common prefixes stripped.
  bool strippedPrefix = false;
  if (isa<clang::EnumConstantDecl>(D)) {
    auto enumDecl = cast<clang::EnumDecl>(D->getDeclContext());
    auto enumInfo = getEnumInfo(enumDecl, &clangSema.getPreprocessor());

    StringRef removePrefix = enumInfo.getConstantNamePrefix();
    if (!removePrefix.empty() && baseName.startswith(removePrefix)) {
      baseName = baseName.substr(removePrefix.size());
      strippedPrefix = true;
    }
  }

  // If the error is an error enum, it will be mapped to the 'Code'
  // enum nested within an NSError-containing struct. Strip the word
  // "Code" off the end of the name, if it's there, because it's
  // redundant.
  if (auto enumDecl = dyn_cast<clang::EnumDecl>(D)) {
    auto enumInfo = getEnumInfo(enumDecl, &clangSema.getPreprocessor());
    if (enumInfo.isErrorEnum() && baseName.size() > 4 &&
        camel_case::getLastWord(baseName) == "Code")
      baseName = baseName.substr(0, baseName.size() - 4);
  }

  auto hasConflict = [&](const clang::IdentifierInfo *proposedName,
                         const clang::TypedefNameDecl *cfTypedef) -> bool {
    // Test to see if there is a value with the same name as 'proposedName'
    // in the same module as the decl
    // FIXME: This will miss macros.
    auto clangModule = getClangSubmoduleForDecl(D);
    if (clangModule.hasValue() && clangModule.getValue())
      clangModule = clangModule.getValue()->getTopLevelModule();

    auto conflicts = [&](const clang::Decl *OtherD) -> bool {
      // If these are simply redeclarations, they do not conflict.
      if (D->getCanonicalDecl() == OtherD->getCanonicalDecl()) return false;

      // If we have a CF typedef, check whether the "other"
      // declaration we found is just the opaque type behind it. If
      // so, it does not conflict.
      if (cfTypedef) {
        if (auto cfPointerTy =
              cfTypedef->getUnderlyingType()->getAs<clang::PointerType>()) {
          if (auto tagDecl = cfPointerTy->getPointeeType()->getAsTagDecl()) {
            if (tagDecl->getCanonicalDecl() == OtherD)
              return false;
          }
        }
      }

      auto declModule = getClangSubmoduleForDecl(OtherD);
      if (!declModule.hasValue())
        return false;

      // Handle the bridging header case. This is pretty nasty since things
      // can get added to it *later*, but there's not much we can do.
      if (!declModule.getValue())
        return *clangModule == nullptr;
      return *clangModule == declModule.getValue()->getTopLevelModule();
    };

    // Allow this lookup to find hidden names.  We don't want the
    // decision about whether to rename the decl to depend on
    // what exactly the user has imported.  Indeed, if we're being
    // asked to resolve a serialization cross-reference, the user
    // may not have imported this module at all, which means a
    // normal lookup wouldn't even find the decl!
    //
    // Meanwhile, we don't need to worry about finding unwanted
    // hidden declarations from different modules because we do a
    // module check before deciding that there's a conflict.
    clang::LookupResult lookupResult(clangSema, proposedName,
                                     clang::SourceLocation(),
                                     clang::Sema::LookupOrdinaryName);
    lookupResult.setAllowHidden(true);
    lookupResult.suppressDiagnostics();

    if (clangSema.LookupName(lookupResult, /*scope=*/nullptr)) {
      if (std::any_of(lookupResult.begin(), lookupResult.end(), conflicts))
        return true;
    }

    lookupResult.clear(clang::Sema::LookupTagName);
    if (clangSema.LookupName(lookupResult, /*scope=*/nullptr)) {
      if (std::any_of(lookupResult.begin(), lookupResult.end(), conflicts))
        return true;
    }

    return false;
  };

  // Objective-C protocols may have the suffix "Protocol" appended if
  // the non-suffixed name would conflict with another entity in the
  // same top-level module.
  SmallString<16> baseNameWithProtocolSuffix;
  if (auto objcProto = dyn_cast<clang::ObjCProtocolDecl>(D)) {
    if (objcProto->hasDefinition()) {
      if (hasConflict(objcProto->getIdentifier(), nullptr)) {
        baseNameWithProtocolSuffix = baseName;
        baseNameWithProtocolSuffix += SWIFT_PROTOCOL_SUFFIX;
        baseName = baseNameWithProtocolSuffix;
      }
    }
  }

  // Typedef declarations might be CF types that will drop the "Ref"
  // suffix.
  clang::ASTContext &clangCtx = clangSema.Context;
  if (!swift2Name) {
    if (auto typedefNameDecl = dyn_cast<clang::TypedefNameDecl>(D)) {
      auto swiftName = getCFTypeName(typedefNameDecl);
      if (!swiftName.empty() &&
          !hasConflict(&clangCtx.Idents.get(swiftName), typedefNameDecl)) {
        // Adopt the requested name.
        baseName = swiftName;
      }
    }
  }

  // Omit needless words.
  StringScratchSpace omitNeedlessWordsScratch;

  // swift_newtype-ed declarations may have common words with the type name
  // stripped.
  if (auto newtypeDecl = findSwiftNewtype(D, clangSema, swift2Name)) {
    baseName = determineSwiftNewtypeBaseName(baseName, newtypeDecl->getName(),
                                             strippedPrefix);
  }

  if (!result.isSubscriptAccessor() && !swift2Name) {
    // Check whether the module in which the declaration resides has a
    // module prefix and will map into Swift as a type. If so, strip
    // that prefix off when present.
    if (D->getDeclContext()->getRedeclContext()->isFileContext() &&
        (isa<clang::TypeDecl>(D) ||
         (isa<clang::ObjCInterfaceDecl>(D) &&
          !hasOrInheritsSwiftBridgeAttr(cast<clang::ObjCInterfaceDecl>(D))) ||
         isa<clang::ObjCProtocolDecl>(D)) &&
        !isUnavailableInSwift(D)) {
      // Find the original declaration, from which we can determine
      // the owning module.
      const clang::Decl *owningD = D->getCanonicalDecl();
      if (auto def = getDefinitionForClangTypeDecl(D)) {
        if (*def)
          owningD = *def;
      }

      SmallString<32> moduleName;
      if (auto module = owningD->getImportedOwningModule())
        moduleName = module->getTopLevelModuleName();
      else
        moduleName = owningD->getASTContext().getLangOpts().CurrentModule;
      if (unsigned prefixLen = stripModulePrefixLength(ModulePrefixes,
                                                       moduleName, baseName)) {
        // Strip off the prefix.
        baseName = baseName.substr(prefixLen);
        strippedPrefix = true;
      }
    }

    // Objective-C properties.
    if (auto objcProperty = dyn_cast<clang::ObjCPropertyDecl>(D)) {
      auto contextType = getClangDeclContextType(D->getDeclContext());
      if (!contextType.isNull()) {
        auto contextTypeName = getClangTypeNameForOmission(clangCtx,
                                                           contextType);
        auto propertyTypeName = getClangTypeNameForOmission(
                                  clangCtx, objcProperty->getType());
        // Find the property names.
        const InheritedNameSet *allPropertyNames = nullptr;
        if (!contextType.isNull()) {
          if (auto objcPtrType = contextType->getAsObjCInterfacePointerType())
            if (auto objcClassDecl = objcPtrType->getInterfaceDecl())
              allPropertyNames = SwiftContext.getAllPropertyNames(
                                   objcClassDecl,
                                   /*forInstance=*/true);
        }

        (void)omitNeedlessWords(baseName, { }, "", propertyTypeName,
                                contextTypeName, { }, /*returnsSelf=*/false,
                                /*isProperty=*/true, allPropertyNames,
                                omitNeedlessWordsScratch);
      }
    }

    // Objective-C methods.
    if (auto method = dyn_cast<clang::ObjCMethodDecl>(D)) {
      (void)omitNeedlessWordsInFunctionName(
        clangSema,
        baseName,
        argumentNames,
        params,
        method->getReturnType(),
        method->getDeclContext(),
        getNonNullArgs(method, params),
        result.ErrorInfo ? Optional<unsigned>(result.ErrorInfo->ParamIndex)
                         : None,
        method->hasRelatedResultType(),
        method->isInstanceMethod(),
        omitNeedlessWordsScratch);
    }

    // If the result is a value, lowercase it.
    if (strippedPrefix && isa<clang::ValueDecl>(D) &&
        shouldLowercaseValueName(baseName)) {
      baseName =
        camel_case::toLowercaseInitialisms(baseName,
                                           omitNeedlessWordsScratch);
    }
  }

  // Local function to determine whether the given declaration is subject to
  // a swift_private attribute.
  auto hasSwiftPrivate = [&clangSema, this](const clang::NamedDecl *D) {
    if (D->hasAttr<clang::SwiftPrivateAttr>())
      return true;

    // Enum constants that are not imported as members should be considered
    // private if the parent enum is marked private.
    if (auto *ECD = dyn_cast<clang::EnumConstantDecl>(D)) {
      auto *ED = cast<clang::EnumDecl>(ECD->getDeclContext());
      switch (getEnumKind(ED, &clangSema.getPreprocessor())) {
        case EnumKind::Constants:
        case EnumKind::Unknown:
          if (ED->hasAttr<clang::SwiftPrivateAttr>())
            return true;
          if (auto *enumTypedef = ED->getTypedefNameForAnonDecl())
            if (enumTypedef->hasAttr<clang::SwiftPrivateAttr>())
              return true;
          break;

        case EnumKind::Enum:
        case EnumKind::Options:
          break;
      }
    }

    return false;
  };

  // If this declaration has the swift_private attribute, prepend "__" to the
  // appropriate place.
  SmallString<16> swiftPrivateScratch;
  if (hasSwiftPrivate(D)) {
    // Make the given name private.
    //
    // Returns true if this is not possible.
    auto makeNamePrivate = [](bool isInitializer,
                              StringRef &baseName,
                              SmallVectorImpl<StringRef> &argumentNames,
                              CtorInitializerKind initKind,
                              SmallString<16> &scratch) -> bool {
      scratch = "__";

      if (isInitializer) {
        // For initializers, prepend "__" to the first argument name.
        if (argumentNames.empty()) {
          // FIXME: ... unless it was from a factory method, for historical
          // reasons.
          if (initKind == CtorInitializerKind::Factory ||
              initKind == CtorInitializerKind::ConvenienceFactory)
            return true;

          // FIXME: Record that we did this.
          argumentNames.push_back("__");
        } else {
          scratch += argumentNames[0];
          argumentNames[0] = scratch;
        }
      } else {
        // For all other entities, prepend "__" to the base name.
        scratch += baseName;
        baseName = scratch;
      }

      return false;
    };

    // Make the name private.
    if (makeNamePrivate(isInitializer, baseName, argumentNames,
                        result.InitKind, swiftPrivateScratch))
      return result;
  }

  result.Imported = formDeclName(SwiftContext, baseName, argumentNames,
                                 isFunction);
  return result;
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

/// Translate the "nullability" notion from API notes into an optional type
/// kind.
OptionalTypeKind ClangImporter::Implementation::translateNullability(
                   clang::NullabilityKind kind) {
  switch (kind) {
  case clang::NullabilityKind::NonNull:
    return OptionalTypeKind::OTK_None;

  case clang::NullabilityKind::Nullable:
    return OptionalTypeKind::OTK_Optional;

  case clang::NullabilityKind::Unspecified:
    return OptionalTypeKind::OTK_ImplicitlyUnwrappedOptional;
  }
}

bool ClangImporter::Implementation::hasDesignatedInitializers(
       const clang::ObjCInterfaceDecl *classDecl) {
  if (classDecl->hasDesignatedInitializers())
    return true;

  return false;
}

bool ClangImporter::Implementation::isDesignatedInitializer(
       const clang::ObjCInterfaceDecl *classDecl,
       const clang::ObjCMethodDecl *method) {
  // If the information is on the AST, use it.
  if (classDecl->hasDesignatedInitializers()) {
    auto *methodParent = method->getClassInterface();
    if (!methodParent ||
        methodParent->getCanonicalDecl() == classDecl->getCanonicalDecl()) {
      return method->hasAttr<clang::ObjCDesignatedInitializerAttr>();
    }
  }

  return false;
}

bool ClangImporter::Implementation::isRequiredInitializer(
       const clang::ObjCMethodDecl *method) {
  // FIXME: No way to express this in Objective-C.
  return false;
}

FactoryAsInitKind ClangImporter::Implementation::getFactoryAsInit(
                    const clang::ObjCInterfaceDecl *classDecl,
                    const clang::ObjCMethodDecl *method) {
  if (auto *customNameAttr = method->getAttr<clang::SwiftNameAttr>()) {
    if (customNameAttr->getName().startswith("init("))
      return FactoryAsInitKind::AsInitializer;
    else
      return FactoryAsInitKind::AsClassMethod;
  }

  if (method->hasAttr<clang::SwiftSuppressFactoryAsInitAttr>()) {
    return FactoryAsInitKind::AsClassMethod;
  }

  return FactoryAsInitKind::Infer;
}

/// Check if this method is declared in the context that conforms to
/// NSAccessibility.
static bool
isAccessibilityConformingContext(const clang::DeclContext *ctx) {
  const clang::ObjCProtocolList *protocols = nullptr;

  if (auto protocol = dyn_cast<clang::ObjCProtocolDecl>(ctx)) {
    if (protocol->getName() == "NSAccessibility")
      return true;
    return false;
  } else if (auto interface = dyn_cast<clang::ObjCInterfaceDecl>(ctx))
    protocols = &interface->getReferencedProtocols();
  else if (auto category = dyn_cast<clang::ObjCCategoryDecl>(ctx))
    protocols = &category->getReferencedProtocols();
  else
    return false;

  for (auto pi : *protocols) {
    if (pi->getName() == "NSAccessibility")
      return true;
  }
  return false;

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

bool
ClangImporter::Implementation::hasNativeSwiftDecl(const clang::Decl *decl) {
  for (auto annotation : decl->specific_attrs<clang::AnnotateAttr>()) {
    if (annotation->getAnnotation() == SWIFT_NATIVE_ANNOTATION_STRING) {
      return true;
    }
  }

  if (auto *category = dyn_cast<clang::ObjCCategoryDecl>(decl)) {
    clang::SourceLocation categoryNameLoc = category->getCategoryNameLoc();
    if (categoryNameLoc.isMacroID()) {
      // Climb up to the top-most macro invocation.
      clang::ASTContext &clangCtx = category->getASTContext();
      clang::SourceManager &SM = clangCtx.getSourceManager();

      clang::SourceLocation macroCaller =
          SM.getImmediateMacroCallerLoc(categoryNameLoc);
      while (macroCaller.isMacroID()) {
        categoryNameLoc = macroCaller;
        macroCaller = SM.getImmediateMacroCallerLoc(categoryNameLoc);
      }

      StringRef macroName =
          clang::Lexer::getImmediateMacroName(categoryNameLoc, SM,
                                              clangCtx.getLangOpts());
      if (macroName == "SWIFT_EXTENSION")
        return true;
    }
  }

  return false;
}

bool ClangImporter::Implementation::shouldSuppressDeclImport(
       const clang::Decl *decl) {
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
               objcMethod->findPropertyDecl(/*checkOverrides=*/false));
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

    // Suppress certain accessibility properties; they're imported as
    // getter/setter pairs instead.
    if (isAccessibilityDecl(objcProperty))
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

bool
ClangImporter::Implementation::shouldSuppressGenericParamsImport(
    const clang::ObjCInterfaceDecl *decl) {
  while (decl) {
    StringRef name = decl->getName();
    if (name == "NSArray" || name == "NSDictionary" || name == "NSSet" ||
        name == "NSOrderedSet" || name == "NSEnumerator" ||
        name == "NSMeasurement") {
      return true;
    }
    decl = decl->getSuperClass();
  }
  return false;
}

bool
ClangImporter::Implementation::isAccessibilityDecl(const clang::Decl *decl) {

  if (auto objCMethod = dyn_cast<clang::ObjCMethodDecl>(decl)) {
    StringRef name = objCMethod->getSelector().getNameForSlot(0);
    if (!(objCMethod->getSelector().getNumArgs() <= 1 &&
          (name.startswith("accessibility") ||
           name.startswith("setAccessibility") ||
           name.startswith("isAccessibility")))) {
      return false;
    }

  } else if (auto objCProperty = dyn_cast<clang::ObjCPropertyDecl>(decl)) {
    if (!objCProperty->getName().startswith("accessibility"))
      return false;

  } else {
    llvm_unreachable("The declaration is not an ObjC property or method.");
  }

  if (isAccessibilityConformingContext(decl->getDeclContext()))
    return true;

  return false;
}

bool ClangImporter::Implementation::isInitMethod(
       const clang::ObjCMethodDecl *method) {
  // init methods are always instance methods.
  if (!method->isInstanceMethod()) return false;

  // init methods must be classified as such by Clang.
  if (method->getMethodFamily() != clang::OMF_init) return false;

  // Swift restriction: init methods must start with the word "init".
  auto selector = method->getSelector();
  return camel_case::getFirstWord(selector.getNameForSlot(0)) == "init";
}

bool ClangImporter::Implementation::shouldImportAsInitializer(
       const clang::ObjCMethodDecl *method,
       unsigned &prefixLength,
       CtorInitializerKind &kind) {
  /// Is this an initializer?
  if (isInitMethod(method)) {
    prefixLength = 4;
    kind = CtorInitializerKind::Designated;
    return true;
  }

  // It must be a class method.
  if (!method->isClassMethod()) return false;

  // Said class methods must be in an actual class.
  auto objcClass = method->getClassInterface();
  if (!objcClass) return false;

  // Check whether we should try to import this factory method as an
  // initializer.
  switch (getFactoryAsInit(objcClass, method)) {
  case FactoryAsInitKind::AsInitializer:
    // Okay; check for the correct result type below.
    prefixLength = 0;
    break;

  case FactoryAsInitKind::Infer:
    // See if we can match the class name to the beginning of the first
    // selector piece.
    if (auto matchedLength = matchFactoryAsInitName(method)) {
      prefixLength = *matchedLength;
      break;
    }

    return false;

  case FactoryAsInitKind::AsClassMethod:
    return false;
  }

  // Determine what kind of initializer we're creating.
  if (auto initKind = determineCtorInitializerKind(method)) {
    kind = *initKind;
    return true;
  }

  // Not imported as an initializer.
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
        if (Decl *imported = Impl.importDeclReal(ND, /*useSwift2Name=*/false))
          receiver(imported);
      }
    }
  }

  auto &ClangCtx = Impl.getClangASTContext();
  auto &ClangPP = Impl.getClangPreprocessor();
  for (clang::IdentifierInfo *II : Impl.BridgeHeaderMacros) {
    if (auto *MI = ClangPP.getMacroInfo(II)) {
      if (filter(MI)) {
        Identifier Name = Impl.importMacroName(II, MI, ClangCtx);
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
          if (Decl *imported = Impl.importDeclReal(ND, /*useSwift2Name=*/false))
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
              Identifier Name = Impl.importMacroName(II, MI, ClangCtx);
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

void ClangModuleUnit::lookupVisibleDecls(Module::AccessPathTy accessPath,
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

  virtual void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
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
                            owner.Impl.importDecl(category, false)))
        results.push_back(extension);
    }

    // Retrieve all of the globals that will be mapped to members.

    // FIXME: Since we don't represent Clang submodules as Swift
    // modules, we're getting everything.
    llvm::SmallPtrSet<ExtensionDecl *, 8> knownExtensions;
    for (auto entry : lookupTable->allGlobalsAsMembers()) {
      auto decl = entry.get<clang::NamedDecl *>();
      auto importedDecl = owner.Impl.importDecl(decl, false);
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

void ClangModuleUnit::lookupValue(Module::AccessPathTy accessPath,
                                  DeclName name, NLKind lookupKind,
                                  SmallVectorImpl<ValueDecl*> &results) const {
  if (!Module::matchesAccessPath(accessPath, name))
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
      Impl.importDeclReal(*I, /*useSwift2Name=*/false);
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
                        Impl.importDecl(objcMethod, false))) {
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
ClangModuleUnit::lookupClassMember(Module::AccessPathTy accessPath,
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

void ClangModuleUnit::lookupClassMembers(Module::AccessPathTy accessPath,
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
                                               /*instance=*/true,
                                               /*CheckTheOther=*/false);
  clangSema.CollectMultipleMethodsInGlobalPool(clangSelector,
                                               objcMethods,
                                               /*instance=*/false,
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
                                  false);

    // Import it.
    // FIXME: Retrying a failed import works around recursion bugs in the Clang
    // importer.
    auto imported = owner.Impl.importDecl(objcMethod, false);
    if (!imported) imported = owner.Impl.importDecl(objcMethod, false);
    if (!imported) continue;

    if (auto func = dyn_cast<AbstractFunctionDecl>(imported))
      results.push_back(func);

    // If there is an alternate declaration, also look at it.
    if (auto alternate = owner.Impl.getAlternateDecl(imported)) {
      if (auto func = dyn_cast<AbstractFunctionDecl>(alternate))
        results.push_back(func);
    }
  }
}

void ClangModuleUnit::collectLinkLibraries(
    Module::LinkLibraryCallback callback) const {
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
    if (F->getName())
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
  return Impl.importDeclCached(ClangDecl, /*useSwift2Name=*/false);
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
    if (!I.first.second)
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

ClangModuleUnit::ClangModuleUnit(Module &M, ClangImporter &owner,
                                 const clang::Module *clangModule)
  : LoadedFile(FileUnitKind::ClangModule, M), owner(owner),
    clangModule(clangModule) {
}

bool ClangModuleUnit::hasClangModule(Module *M) {
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

Module *ClangModuleUnit::getAdapterModule() const {
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
    Module *M = getParentModule();
    ASTContext &Ctx = M->getASTContext();
    auto adapter = Ctx.getModule(Module::AccessPathTy({M->getName(),
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
    SmallVectorImpl<Module::ImportedModule> &imports,
    Module::ImportFilter filter) const {
  if (filter != Module::ImportFilter::Public)
    imports.push_back({Module::AccessPathTy(),
                       getASTContext().getStdlibModule()});

  if (!clangModule) {
    // This is the special "imported headers" module.
    if (filter != Module::ImportFilter::Private) {
      imports.append(owner.Impl.ImportedHeaderExports.begin(),
                     owner.Impl.ImportedHeaderExports.end());
    }
    return;
  }

  auto topLevelAdapter = getAdapterModule();

  SmallVector<clang::Module *, 8> imported;
  clangModule->getExportedModules(imported);
  if (filter != Module::ImportFilter::Public) {
    if (filter == Module::ImportFilter::All) {
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
        imports.push_back({ Module::AccessPathTy(),
                            topLevelWrapper->getParentModule() });
      }
      actualMod = wrapper->getParentModule();
    } else if (actualMod == topLevelAdapter) {
      actualMod = wrapper->getParentModule();
    }

    assert(actualMod && "Missing imported adapter module");
    imports.push_back({Module::AccessPathTy(), actualMod});
  }
}

void ClangModuleUnit::getImportedModulesForLookup(
    SmallVectorImpl<Module::ImportedModule> &imports) const {

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
    imports.push_back({Module::AccessPathTy(), actualMod});
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

clang::ModuleFileExtensionMetadata
ClangImporter::Implementation::SwiftNameLookupExtension::
getExtensionMetadata() const {
  clang::ModuleFileExtensionMetadata metadata;
  metadata.BlockName = "swift.lookup";
  metadata.MajorVersion = SWIFT_LOOKUP_TABLE_VERSION_MAJOR;
  metadata.MinorVersion = SWIFT_LOOKUP_TABLE_VERSION_MINOR;
  metadata.UserInfo = version::getSwiftFullVersion();
  return metadata;
}

llvm::hash_code
ClangImporter::Implementation::SwiftNameLookupExtension::hashExtension(
   llvm::hash_code code) const {
  return llvm::hash_combine(code, StringRef("swift.lookup"),
                            SWIFT_LOOKUP_TABLE_VERSION_MAJOR,
                            SWIFT_LOOKUP_TABLE_VERSION_MINOR,
                            Impl.InferImportAsMember,
                            Impl.HonorSwiftNewtypeAttr);
}

std::unique_ptr<clang::ModuleFileExtensionWriter>
ClangImporter::Implementation::SwiftNameLookupExtension::createExtensionWriter(
    clang::ASTWriter &writer) {
    // Local function to populate the lookup table.
  auto populateTable = [this](clang::Sema &sema, SwiftLookupTable &table) {
    for (auto decl
           : sema.Context.getTranslationUnitDecl()->noload_decls()) {
      // Skip anything from an AST file.
      if (decl->isFromASTFile()) continue;

      // Skip non-named declarations.
      auto named = dyn_cast<clang::NamedDecl>(decl);
      if (!named) continue;

      // Add this entry to the lookup table.
      Impl.addEntryToLookupTable(sema, table, named);
    }

    // Add macros to the lookup table.
    Impl.addMacrosToLookupTable(sema.Context, sema.getPreprocessor(), table);

    // Finalize the lookup table, which may fail.
    Impl.finalizeLookupTable(sema.Context, sema.getPreprocessor(), table);
  };

  return std::unique_ptr<clang::ModuleFileExtensionWriter>(
           new SwiftLookupTableWriter(this, populateTable, writer));
}

std::unique_ptr<clang::ModuleFileExtensionReader>
ClangImporter::Implementation::SwiftNameLookupExtension::createExtensionReader(
  const clang::ModuleFileExtensionMetadata &metadata,
  clang::ASTReader &reader,
  clang::serialization::ModuleFile &mod,
  const llvm::BitstreamCursor &stream)
{
  // Make sure we have a compatible block. Since these values are part
  // of the hash, it should never be wrong.
  assert(metadata.BlockName == "swift.lookup");
  assert(metadata.MajorVersion == SWIFT_LOOKUP_TABLE_VERSION_MAJOR);
  assert(metadata.MinorVersion == SWIFT_LOOKUP_TABLE_VERSION_MINOR);

  // Check whether we already have an entry in the set of lookup tables.
  auto &entry = Impl.LookupTables[mod.ModuleName];
  if (entry) return nullptr;

  // Local function used to remove this entry when the reader goes away.
  std::string moduleName = mod.ModuleName;
  auto onRemove = [this, moduleName]() {
    Impl.LookupTables.erase(moduleName);
  };

  // Create the reader.
  auto tableReader = SwiftLookupTableReader::create(this, reader, mod, onRemove,
                                                    stream);
  if (!tableReader) return nullptr;

  // Create the lookup table.
  entry.reset(new SwiftLookupTable(tableReader.get()));

  // Return the new reader.
  return std::move(tableReader);
}

SwiftLookupTable *ClangImporter::Implementation::findLookupTable(
                    const clang::Module *clangModule) {
  // If the Clang module is null, use the bridging header lookup table.
  if (!clangModule)
    return &BridgingHeaderLookupTable;

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
  if (fn(BridgingHeaderLookupTable)) return true;

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
               importDeclReal(clangDecl->getMostRecentDecl(), false));
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
    if (auto alternate = getAlternateDecl(decl)) {
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
        if (auto swift2Decl = cast_or_null<ValueDecl>(
                                importDeclReal(clangDecl->getMostRecentDecl(),
                                               true))) {
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
    auto decl = cast_or_null<ValueDecl>(importDeclReal(clangDecl, false));
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
    if (auto alternate = getAlternateDecl(decl)) {
      if (alternate->getFullName().matchesRef(name)) {
        consumer.foundDecl(alternate, DeclVisibilityKind::DynamicLookup);
        matchedAny = true;
      }
    }

    // If we didn't find anything, try under the Swift 2 name.
    if (!matchedAny) {
      if (auto swift2Decl = cast_or_null<ValueDecl>(
                              importDeclReal(clangDecl, true))) {
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
    if (sema.LookupName(lookupResult, /*Scope=*/0)) {
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
  BridgingHeaderLookupTable.dump();
}
