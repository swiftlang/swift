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
#include "ImporterImpl.h"
#include "ClangDiagnosticConsumer.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
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
  Impl.Retain();
}

ClangImporter::~ClangImporter() {
  Impl.Release();
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

static void
getNormalInvocationArguments(std::vector<std::string> &invocationArgStrs,
                             ASTContext &ctx,
                             const ClangImporterOptions &importerOpts) {
  const llvm::Triple &triple = ctx.LangOpts.Target;
  SearchPathOptions &searchPathOpts = ctx.SearchPathOpts;

  // Construct the invocation arguments for the current target.
  // Add target-independent options first.
  invocationArgStrs.insert(invocationArgStrs.end(), {
    // Enable modules.
    "-fmodules",

    // Don't emit LLVM IR.
    "-fsyntax-only",

    "-femit-all-decls",
    SHIMS_INCLUDE_FLAG, searchPathOpts.RuntimeResourcePath,
    "-fretain-comments-from-system-headers",
    "-fmodules-validate-system-headers",
    "-Werror=non-modular-include-in-framework-module",
    "-Xclang", "-fmodule-feature", "-Xclang", "swift",
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

      // Request new APIs from Foundation.
      "-DSWIFT_SDK_OVERLAY_FOUNDATION_EPOCH=5",

      // Request new APIs from SceneKit.
      "-DSWIFT_SDK_OVERLAY2_SCENEKIT_EPOCH=1",

      // Request new APIs from SpriteKit.
      "-DSWIFT_SDK_OVERLAY2_SPRITEKIT_EPOCH=1",

      // Request new APIs from CoreImage.
      "-DSWIFT_SDK_OVERLAY_COREIMAGE_EPOCH=1",
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
  }

  if (triple.isOSDarwin()) {
    std::string minVersionBuf;
    llvm::raw_string_ostream minVersionOpt{minVersionBuf};
    unsigned major, minor, micro;
    if (triple.isiOS()) {
      bool isiOSSimulator = swift::tripleIsiOSSimulator(triple);
      if (triple.isTvOS()) {
        if (isiOSSimulator)
          minVersionOpt << "-mtvos-simulator-version-min=";
        else
          minVersionOpt << "-mtvos-version-min=";
      } else {
        if (isiOSSimulator)
          minVersionOpt << "-mios-simulator-version-min=";
        else
          minVersionOpt << "-mios-version-min=";
      }

      triple.getiOSVersion(major, minor, micro);
    } else if(triple.isWatchOS()) {
      if (tripleIsWatchSimulator(triple))
          minVersionOpt << "-mwatchos-simulator-version-min=";
      else
          minVersionOpt << "-mwatchos-version-min=";
      triple.getOSVersion(major, minor, micro);
    } else {
      assert(triple.isMacOSX());
      minVersionOpt << "-mmacosx-version-min=";
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
}

std::unique_ptr<ClangImporter>
ClangImporter::create(ASTContext &ctx,
                      const ClangImporterOptions &importerOpts,
                      DependencyTracker *tracker) {

  std::unique_ptr<ClangImporter> importer{
    new ClangImporter(ctx, importerOpts, tracker)
  };

  std::vector<std::string> invocationArgStrs;

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
  invocation->getFrontendOpts().ModuleFileExtensions.push_back(&importer->Impl);

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
  importer->Impl.Action.reset(new clang::SyntaxOnlyAction);
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

  // If we have a name to import as, add this entry to the table.
  clang::DeclContext *effectiveContext;
  if (auto importedName = importFullName(named, None, &effectiveContext,
                                         &clangSema)) {
    table.addEntry(importedName.Imported, named, effectiveContext);

    // Also add the alias, if needed.
    if (importedName.Alias)
      table.addEntry(importedName.Alias, named, effectiveContext);

    // Also add the subscript entry, if needed.
    if (importedName.IsSubscriptAccessor)
      table.addEntry(DeclName(SwiftContext, SwiftContext.Id_subscript,
                              ArrayRef<Identifier>()),
                     named, effectiveContext);
  } else if (auto category = dyn_cast<clang::ObjCCategoryDecl>(named)) {
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
    if (!MD) continue;

    // Import the name.
    auto name = importIdentifier(macro.first);
    if (name.empty()) continue;

    // Walk the history.
    for (; MD; MD = MD->getPrevious()) {
      // Check whether we have a macro defined in this module.
      auto info = pp.getMacroInfo(macro.first);
      if (!info || info->isFromASTFile() || info->isBuiltinMacro()) continue;
      
      // Only interested in macro definitions.
      auto *defMD = dyn_cast<clang::DefMacroDirective>(MD);
      if (!defMD) continue;

      // If we hit a builtin macro, we're done.
      if (auto info = defMD->getInfo()) {
        if (info->isBuiltinMacro()) break;
      }

      // If we hit a macro with invalid or predefined location, we're done.
      auto loc = defMD->getLocation();
      if (loc.isInvalid()) break;
      if (pp.getSourceManager().getFileID(loc) == pp.getPredefinesFileID())
        break;

      // Add this entry.
      table.addEntry(name, info, clangCtx.getTranslationUnitDecl());
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

  pp.EnterSourceFile(bufferID, /*directoryLookup=*/nullptr, /*loc=*/{});
  // Force the import to occur.
  pp.LookAhead(0);

  clang::Parser::DeclGroupPtrTy parsed;
  while (!Parser->ParseTopLevelDecl(parsed)) {
    if (!parsed) continue;

    for (auto *D : parsed.get()) {
      if (trackParsedSymbols)
        addBridgeHeaderTopLevelDecls(D);

      if (auto named = dyn_cast<clang::NamedDecl>(D)) {
        addEntryToLookupTable(getClangSema(), BridgingHeaderLookupTable,
                              named);
      }
    }
  }
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

void ClangImporter::collectSubModuleNamesAndVisibility(
    ArrayRef<std::pair<Identifier, SourceLoc>> path,
    std::vector<std::pair<std::string, bool>> &namesVisiblePairs) {
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
    namesVisiblePairs.push_back(
      std::make_pair(full.substr(submoduleNameLength + 1).str(),
      isModuleImported(sub)));
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

  // Try to load the API notes for this module.
  (void)getAPINotesForModule(clangModule->getTopLevelModule());

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
    OmitNeedlessWords(opts.OmitNeedlessWords),
    InferDefaultArguments(opts.InferDefaultArguments),
    UseSwiftLookupTables(opts.UseSwiftLookupTables),
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
      [](StringRef Platform) { return Platform == "macosx"; };
    } else {
      PlatformAvailabilityFilter =
      [](StringRef Platform) {
        return Platform == "macosx" ||
               Platform == "macosx_app_extension"; };
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

api_notes::APINotesReader *
ClangImporter::Implementation::getAPINotesForModule(
    const clang::Module *underlying) {
  assert(underlying == underlying->getTopLevelModule() &&
         "Only allowed on a top-level module");
  // Check whether we already have an API notes reader.
  auto known = APINotesReaders.find(underlying);
  if (known != APINotesReaders.end())
    return known->second.get();

  /// Determine the name of the API notes we're looking for.
  llvm::SmallString<64> notesFilename(underlying->Name);
  notesFilename += '.';
  notesFilename += api_notes::BINARY_APINOTES_EXTENSION;

  // Look for a compiled API notes file in at the given search path. Sets
  // the reader and returns true if one was found.
  llvm::SmallString<64> scratch;
  std::unique_ptr<api_notes::APINotesReader> reader;
  auto findAPINotes = [&](StringRef searchPath) -> bool {
    // Compute the file name we're looking for.
    scratch.clear();
    llvm::sys::path::append(scratch, searchPath, notesFilename.str());

    // Try to open the file.
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> bufferOrErr
      = llvm::MemoryBuffer::getFile(scratch.str());
    if (!bufferOrErr)
      return false;

    // We found the API notes file; try to load it.
    reader = api_notes::APINotesReader::get(std::move(bufferOrErr.get()));
    return true;
  };

  // Look for a ModuleName.apinotes file in the import search paths.
  // FIXME: Good thing we have no notion of layering for these paths.
  bool foundAny = false;
  for (const auto& searchPath : SwiftContext.SearchPathOpts.ImportSearchPaths) {
    if (findAPINotes(searchPath)) {
      foundAny = true;
      break;
    }
  }

  // If we didn't find anything in the user-provided import search paths, look
  // in the runtime library import path.
  if (!foundAny)
    findAPINotes(SwiftContext.SearchPathOpts.RuntimeLibraryImportPath);

  // Add the reader we formed (if any) to the table.
  return APINotesReaders.insert({underlying, std::move(reader)})
           .first->second.get();
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

/// Parse a stringified Swift declaration name, e.g. "init(frame:)".
static StringRef parseDeclName(StringRef Name,
                               SmallVectorImpl<StringRef> &ArgNames,
                               bool &IsFunctionName) {
  if (Name.back() != ')') {
    IsFunctionName = false;
    if (Lexer::isIdentifier(Name) && Name != "_")
      return Name;

    return "";
  }

  IsFunctionName = true;

  StringRef BaseName, Parameters;
  std::tie(BaseName, Parameters) = Name.split('(');
  if (!Lexer::isIdentifier(BaseName) || BaseName == "_")
    return "";

  if (Parameters.empty())
    return "";
  Parameters = Parameters.drop_back(); // ')'

  if (Parameters.empty())
    return BaseName;

  if (Parameters.back() != ':')
    return "";

  do {
    StringRef NextParam;
    std::tie(NextParam, Parameters) = Parameters.split(':');

    if (!Lexer::isIdentifier(NextParam))
      return "";
    Identifier NextParamID;
    if (NextParam == "_")
      ArgNames.push_back("");
    else
      ArgNames.push_back(NextParam);
  } while (!Parameters.empty());

  return BaseName;
}

/// \brief Returns the common prefix of two strings at camel-case word
/// granularity.
///
/// For example, given "NSFooBar" and "NSFooBas", returns "NSFoo"
/// (not "NSFooBa"). The returned StringRef is a slice of the "a" argument.
///
/// If either string has a non-identifier character immediately after the
/// prefix, \p followedByNonIdentifier will be set to \c true. If both strings
/// have identifier characters after the prefix, \p followedByNonIdentifier will
/// be set to \c false. Otherwise, \p followedByNonIdentifier will not be
/// changed from its initial value.
///
/// This is used to derive the common prefix of enum constants so we can elide
/// it from the Swift interface.
static StringRef getCommonWordPrefix(StringRef a, StringRef b,
                                     bool &followedByNonIdentifier) {
  auto aWords = camel_case::getWords(a), bWords = camel_case::getWords(b);
  auto aI = aWords.begin(), aE = aWords.end(),
       bI = bWords.begin(), bE = bWords.end();

  unsigned prevLength = 0;
  unsigned prefixLength = 0;
  for ( ; aI != aE && bI != bE; ++aI, ++bI) {
    if (*aI != *bI) {
      followedByNonIdentifier = false;
      break;
    }

    prevLength = prefixLength;
    prefixLength = aI.getPosition() + aI->size();
  }

  // Avoid creating a prefix where the rest of the string starts with a number.
  if ((aI != aE && !Lexer::isIdentifier(*aI)) ||
      (bI != bE && !Lexer::isIdentifier(*bI))) {
    followedByNonIdentifier = true;
    prefixLength = prevLength;
  }

  return a.slice(0, prefixLength);
}

/// Returns the common word-prefix of two strings, allowing the second string
/// to be a common English plural form of the first.
///
/// For example, given "NSProperty" and "NSProperties", the full "NSProperty"
/// is returned. Given "NSMagicArmor" and "NSMagicArmory", only
/// "NSMagic" is returned.
///
/// The "-s", "-es", and "-ies" patterns cover every plural NS_OPTIONS name
/// in Cocoa and Cocoa Touch.
///
/// \see getCommonWordPrefix
static StringRef getCommonPluralPrefix(StringRef singular, StringRef plural) {
  assert(!plural.empty());

  if (singular.empty())
    return singular;

  bool ignored;
  StringRef commonPrefix = getCommonWordPrefix(singular, plural, ignored);
  if (commonPrefix.size() == singular.size() || plural.back() != 's')
    return commonPrefix;

  StringRef leftover = singular.substr(commonPrefix.size());
  StringRef firstLeftoverWord = camel_case::getFirstWord(leftover);
  StringRef commonPrefixPlusWord =
      singular.substr(0, commonPrefix.size() + firstLeftoverWord.size());

  // Is the plural string just "[singular]s"?
  plural = plural.drop_back();
  if (plural.endswith(firstLeftoverWord))
    return commonPrefixPlusWord;

  if (plural.empty() || plural.back() != 'e')
    return commonPrefix;

  // Is the plural string "[singular]es"?
  plural = plural.drop_back();
  if (plural.endswith(firstLeftoverWord))
    return commonPrefixPlusWord;

  if (plural.empty() || !(plural.back() == 'i' && singular.back() == 'y'))
    return commonPrefix;

  // Is the plural string "[prefix]ies" and the singular "[prefix]y"?
  plural = plural.drop_back();
  firstLeftoverWord = firstLeftoverWord.drop_back();
  if (plural.endswith(firstLeftoverWord))
    return commonPrefixPlusWord;

  return commonPrefix;
}

StringRef ClangImporter::Implementation::getEnumConstantNamePrefix(
            clang::Sema &sema,
            const clang::EnumDecl *decl) {
  switch (classifyEnum(sema.getPreprocessor(), decl)) {
  case EnumKind::Enum:
  case EnumKind::Options:
    // Enums are mapped to Swift enums, Options to Swift option sets, both
    // of which attempt prefix-stripping.
    break;

  case EnumKind::Constants:
  case EnumKind::Unknown:
    // Nothing to do.
    return StringRef();
  }

  // If there are no enumers, there is no prefix to compute.
  auto ec = decl->enumerator_begin(), ecEnd = decl->enumerator_end();
  if (ec == ecEnd)
    return StringRef();

  // Determine whether we can cache the result.
  // FIXME: Pass in a cache?
  bool useCache = &sema == &getClangSema();

  // If we've already computed the prefix, return it.
  auto known = useCache ? EnumConstantNamePrefixes.find(decl)
                        : EnumConstantNamePrefixes.end();
  if (known != EnumConstantNamePrefixes.end())
    return known->second;

  // Determine whether the given enumerator is non-deprecated and has no
  // specifically-provided name.
  auto isNonDeprecatedWithoutCustomName =
    [](const clang::EnumConstantDecl *elem) -> bool {
      if (elem->hasAttr<clang::SwiftNameAttr>())
        return false;

      clang::VersionTuple maxVersion{~0U, ~0U, ~0U};
      switch (elem->getAvailability(nullptr, maxVersion)) {
      case clang::AR_Available:
      case clang::AR_NotYetIntroduced:
        for (auto attr : elem->attrs()) {
          if (auto annotate = dyn_cast<clang::AnnotateAttr>(attr)) {
            if (annotate->getAnnotation() == "swift1_unavailable")
              return false;
          }
          if (auto avail = dyn_cast<clang::AvailabilityAttr>(attr)) {
            if (avail->getPlatform()->getName() == "swift")
              return false;
          }
        }
        return true;

      case clang::AR_Deprecated:
      case clang::AR_Unavailable:
        return false;
      }
    };

  // Move to the first non-deprecated enumerator, or non-swift_name'd
  // enumerator, if present.
  auto firstNonDeprecated = std::find_if(ec, ecEnd,
                                         isNonDeprecatedWithoutCustomName);
  bool hasNonDeprecated = (firstNonDeprecated != ecEnd);
  if (hasNonDeprecated) {
    ec = firstNonDeprecated;
  } else {
    // Advance to the first case without a custom name, deprecated or not.
    while (ec != ecEnd && (*ec)->hasAttr<clang::SwiftNameAttr>())
      ++ec;
    if (ec == ecEnd) {
      if (useCache)
        EnumConstantNamePrefixes.insert({decl, StringRef()});
      return StringRef();
    }
  }

  // Compute the common prefix.
  StringRef commonPrefix = (*ec)->getName();
  bool followedByNonIdentifier = false;
  for (++ec; ec != ecEnd; ++ec) {
    // Skip deprecated or swift_name'd enumerators.
    const clang::EnumConstantDecl *elem = *ec;
    if (hasNonDeprecated) {
      if (!isNonDeprecatedWithoutCustomName(elem))
        continue;
    } else {
      if (elem->hasAttr<clang::SwiftNameAttr>())
        continue;
    }

    commonPrefix = getCommonWordPrefix(commonPrefix, elem->getName(),
                                       followedByNonIdentifier);
    if (commonPrefix.empty())
      break;
  }

  if (!commonPrefix.empty()) {
    StringRef checkPrefix = commonPrefix;

    // Account for the 'kConstant' naming convention on enumerators.
    if (checkPrefix[0] == 'k') {
      bool canDropK;
      if (checkPrefix.size() >= 2)
        canDropK = clang::isUppercase(checkPrefix[1]);
      else
        canDropK = !followedByNonIdentifier;

      if (canDropK)
        checkPrefix = checkPrefix.drop_front();
    }

    // Don't use importFullName() here, we want to ignore the swift_name
    // and swift_private attributes.
    StringRef enumNameStr = decl->getName();
    StringRef commonWithEnum = getCommonPluralPrefix(checkPrefix,
                                                     enumNameStr);
    size_t delta = commonPrefix.size() - checkPrefix.size();

    // Account for the 'EnumName_Constant' convention on enumerators.
    if (commonWithEnum.size() < checkPrefix.size() &&
        checkPrefix[commonWithEnum.size()] == '_' &&
        !followedByNonIdentifier) {
      delta += 1;
    }

    commonPrefix = commonPrefix.slice(0, commonWithEnum.size() + delta);
  }

  if (useCache)
    EnumConstantNamePrefixes.insert({decl, commonPrefix});
  return commonPrefix;
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

  // Block and function pointers.
  if (type->isBlockPointerType() || type->isFunctionPointerType()) return true;

  // CF types.
  do {
    // Check whether we have a typedef that refers to a CoreFoundation type.
    if (auto typedefType = dyn_cast<clang::TypedefType>(type.getTypePtr())) {
      if (ClangImporter::Implementation::isCFTypeDecl(typedefType->getDecl()))
        return true;

      type = typedefType->getDecl()->getUnderlyingType();
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
  // FIXME: Somewhat duplicated from Implementation::importAttributes.
  clang::AvailabilityResult availability = conflict->getAvailability();
  if (availability != clang::AR_Unavailable &&
      importer.DeprecatedAsUnavailableFilter) {
    for (auto *attr : conflict->specific_attrs<clang::AvailabilityAttr>()) {
      if (attr->getPlatform()->getName() == "swift") {
        availability = clang::AR_Unavailable;
        break;
      }
      if (importer.PlatformAvailabilityFilter &&
          !importer.PlatformAvailabilityFilter(attr->getPlatform()->getName())){
        continue;
      }
      clang::VersionTuple version = attr->getDeprecated();
      if (version.empty())
        continue;
      if (importer.DeprecatedAsUnavailableFilter(version.getMajor(),
                                                 version.getMinor())) {
        availability = clang::AR_Unavailable;
        break;
      }
    }
  }
  return availability != clang::AR_Unavailable;
}

/// Determine the optionality of the given Objective-C method.
///
/// \param method The Clang method.
///
/// \param knownNullability When API notes describe the nullability of this
/// parameter, that nullability.
static OptionalTypeKind getResultOptionality(
                          const clang::ObjCMethodDecl *method,
                          Optional<clang::NullabilityKind> knownNullability) {
  auto &clangCtx = method->getASTContext();

  // If nullability is available on the type, use it.
  if (auto nullability = method->getReturnType()->getNullability(clangCtx)) {
    return ClangImporter::Implementation::translateNullability(*nullability);
  }

  // If there is a returns_nonnull attribute, non-null.
  if (method->hasAttr<clang::ReturnsNonNullAttr>())
    return OTK_None;

  // If API notes gives us nullability, use that.
  if (knownNullability) {
    return ClangImporter::Implementation::translateNullability(
             *knownNullability);
  }

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

    // Determine the nullability of the result.
    Optional<clang::NullabilityKind> knownResultNullability;
    if (auto knownMethod = importer.getKnownObjCMethod(clangDecl)) {
      if (knownMethod->NullabilityAudited)
        knownResultNullability = knownMethod->getReturnTypeInfo();
    }

    auto errorKind =
      classifyMethodErrorHandling(clangDecl,
                                  getResultOptionality(clangDecl,
                                                       knownResultNullability));
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

auto ClangImporter::Implementation::importFullName(
       const clang::NamedDecl *D,
       ImportNameOptions options,
       clang::DeclContext **effectiveContext,
       clang::Sema *clangSemaOverride) -> ImportedName {
  clang::Sema &clangSema = clangSemaOverride ? *clangSemaOverride
                                             : getClangSema();
  ImportedName result;

  // Objective-C categories and extensions don't have names, despite
  // being "named" declarations.
  if (isa<clang::ObjCCategoryDecl>(D))
    return result;

  // Compute the effective context, if requested.
  if (effectiveContext) {
    auto dc = const_cast<clang::DeclContext *>(D->getDeclContext());

    // Enumerators can end up within their enclosing enum or in the global
    // scope, depending how their enclosing enumeration is imported.
    if (isa<clang::EnumConstantDecl>(D)) {
      auto enumDecl = cast<clang::EnumDecl>(dc);
      switch (classifyEnum(clangSema.getPreprocessor(), enumDecl)) {
      case EnumKind::Enum:
      case EnumKind::Options:
        // Enums are mapped to Swift enums, Options to Swift option sets.
        *effectiveContext = enumDecl;
        break;

      case EnumKind::Constants:
      case EnumKind::Unknown:
        // The enum constant goes into the redeclaration context of the
        // enum.
        *effectiveContext = enumDecl->getRedeclContext();
        break;
      }
    } else {
      // Everything else goes into its redeclaration context.
      *effectiveContext = dc->getRedeclContext();
    }

    // Anything in an Objective-C category or extension is adjusted to the
    // class context.
    if (auto category = dyn_cast<clang::ObjCCategoryDecl>(*effectiveContext)) {
      *effectiveContext = category->getClassInterface();
    }
  }

  // Local function that forms a DeclName from the given strings.
  auto formDeclName = [&](StringRef baseName,
                          ArrayRef<StringRef> argumentNames,
                          bool isFunction) -> DeclName {
    // We cannot import when the base name is not an identifier.
    if (!Lexer::isIdentifier(baseName))
      return DeclName();

    // Get the identifier for the base name.
    Identifier baseNameId = SwiftContext.getIdentifier(baseName);

    // For non-functions, just use the base name.
    if (!isFunction) return baseNameId;

    // For functions, we need to form a complete name.

    // Convert the argument names.
    SmallVector<Identifier, 4> argumentNameIds;
    for (auto argName : argumentNames) {
      if (argumentNames.empty() || !Lexer::isIdentifier(argName)) {
        argumentNameIds.push_back(Identifier());
        continue;
      }
      
      argumentNameIds.push_back(SwiftContext.getIdentifier(argName));
    }

    // Build the result.
    return DeclName(SwiftContext, baseNameId, argumentNameIds);
  };

  // If we have a swift_name attribute, use that.
  if (auto *nameAttr = D->getAttr<clang::SwiftNameAttr>()) {
    bool skipCustomName = false;

    // If we have an Objective-C method that is being mapped to an
    // initializer (e.g., a factory method whose name doesn't fit the
    // convention for factory methods), make sure that it can be
    // imported as an initializer.
    bool isInitializer = false;
    auto method = dyn_cast<clang::ObjCMethodDecl>(D);
    if (method) {
      unsigned initPrefixLength;
      if (nameAttr->getName().startswith("init(")) {
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
      SmallVector<StringRef, 4> argumentNames;
      bool isFunctionName;
      StringRef baseName = parseDeclName(nameAttr->getName(), argumentNames,
                                         isFunctionName);
      if (baseName.empty()) return result;
      
      result.HasCustomName = true;
      result.Imported = formDeclName(baseName, argumentNames, isFunctionName);

      if (method) {
        // Get the parameters.
        ArrayRef<const clang::ParmVarDecl *> params{
          method->param_begin(),
          method->param_end()
        };

        result.ErrorInfo = considerErrorImport(*this, method, baseName,
                                               argumentNames, params,
                                               isInitializer,
                                               /*hasCustomName=*/true);
      }


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
  StringScratchSpace stringScratch;
  ArrayRef<const clang::ParmVarDecl *> params;
  switch (D->getDeclName().getNameKind()) {
  case clang::DeclarationName::CXXConstructorName:
  case clang::DeclarationName::CXXConversionFunctionName:
  case clang::DeclarationName::CXXDestructorName:
  case clang::DeclarationName::CXXLiteralOperatorName:
  case clang::DeclarationName::CXXOperatorName:
  case clang::DeclarationName::CXXUsingDirective:
    // Handling these is part of C++ interoperability.
    return result;

  case clang::DeclarationName::Identifier:
    // Map the identifier.
    baseName = D->getDeclName().getAsIdentifierInfo()->getName();

    if (OmitNeedlessWords) {
      // For Objective-C BOOL properties, use the name of the getter
      // which, conventionally, has an "is" prefix.
      if (auto property = dyn_cast<clang::ObjCPropertyDecl>(D)) {
        if (isBoolType(clangSema.Context, property->getType()))
          baseName = property->getGetterName().getNameForSlot(0);
      }
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

        // Swift 2 lowercased all subsequent argument names.
        // Swift 3 may handle this as part of omitting needless words, below,
        // but don't preempt that here.
        if (!OmitNeedlessWords)
          argName = camel_case::toLowercaseWord(argName, stringScratch);

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
        objcMethod->isInstanceMethod() &&
        (isNonNullarySelector(objcMethod->getSelector(),
                              { "objectAtIndexedSubscript" }) ||
         isNonNullarySelector(objcMethod->getSelector(),
                              { "setObject", "atIndexedSubscript" }) ||
         isNonNullarySelector(objcMethod->getSelector(),
                              { "objectForKeyedSubscript" }) ||
         isNonNullarySelector(objcMethod->getSelector(),
                              { "setObject", "forKeyedSubscript" })))
      result.IsSubscriptAccessor = true;

    break;
  }
  }

  // Perform automatic name transformations.

  // Enumeration constants may have common prefixes stripped.
  if (isa<clang::EnumConstantDecl>(D)) {
    auto enumDecl = cast<clang::EnumDecl>(D->getDeclContext());
    StringRef removePrefix = getEnumConstantNamePrefix(clangSema, enumDecl);
    if (baseName.startswith(removePrefix))
      baseName = baseName.substr(removePrefix.size());
  }

  // Objective-C protocols may have the suffix "Protocol" appended if
  // the non-suffixed name would conflict with another entity in the
  // same top-level module.
  SmallString<16> baseNameWithProtocolSuffix;
  if (auto objcProto = dyn_cast<clang::ObjCProtocolDecl>(D)) {
    if (objcProto->hasDefinition()) {
      // Test to see if there is a value with the same name as the protocol
      // in the same module.
      // FIXME: This will miss macros.
      auto clangModule = getClangSubmoduleForDecl(objcProto);
      if (clangModule.hasValue() && clangModule.getValue())
        clangModule = clangModule.getValue()->getTopLevelModule();

      auto isInSameModule = [&](const clang::Decl *D) -> bool {
        auto declModule = getClangSubmoduleForDecl(D);
        if (!declModule.hasValue())
          return false;
        // Handle the bridging header case. This is pretty nasty since things
        // can get added to it *later*, but there's not much we can do.
        if (!declModule.getValue())
          return *clangModule == nullptr;
        return *clangModule == declModule.getValue()->getTopLevelModule();
      };

      // Allow this lookup to find hidden names.  We don't want the
      // decision about whether to rename the protocol to depend on
      // what exactly the user has imported.  Indeed, if we're being
      // asked to resolve a serialization cross-reference, the user
      // may not have imported this module at all, which means a
      // normal lookup wouldn't even find the protocol!
      //
      // Meanwhile, we don't need to worry about finding unwanted
      // hidden declarations from different modules because we do a
      // module check before deciding that there's a conflict.
      bool hasConflict = false;
      clang::LookupResult lookupResult(clangSema, D->getDeclName(),
                                       clang::SourceLocation(),
                                       clang::Sema::LookupOrdinaryName);
      lookupResult.setAllowHidden(true);
      lookupResult.suppressDiagnostics();

      if (clangSema.LookupName(lookupResult, /*scope=*/nullptr)) {
        hasConflict = std::any_of(lookupResult.begin(), lookupResult.end(),
                                  isInSameModule);
      }
      if (!hasConflict) {
        lookupResult.clear(clang::Sema::LookupTagName);
        if (clangSema.LookupName(lookupResult, /*scope=*/nullptr)) {
          hasConflict = std::any_of(lookupResult.begin(), lookupResult.end(),
                                    isInSameModule);
        }
      }

      if (hasConflict) {
        baseNameWithProtocolSuffix = baseName;
        baseNameWithProtocolSuffix += SWIFT_PROTOCOL_SUFFIX;
        baseName = baseNameWithProtocolSuffix;
      }
    }
  }

  // Typedef declarations might be CF types that will drop the "Ref"
  // suffix.
  bool aliasIsFunction = false;
  bool aliasIsInitializer = false;
  StringRef aliasBaseName;
  SmallVector<StringRef, 4> aliasArgumentNames;
  if (auto typedefNameDecl = dyn_cast<clang::TypedefNameDecl>(D)) {
    auto swiftName = getCFTypeName(typedefNameDecl, &aliasBaseName);
    if (!swiftName.empty()) {
      baseName = swiftName;
    }
  }

  // Local function to determine whether the given declaration is subject to
  // a swift_private attribute.
  auto clangSemaPtr = &clangSema;
  auto hasSwiftPrivate = [clangSemaPtr](const clang::NamedDecl *D) {
    if (D->hasAttr<clang::SwiftPrivateAttr>())
      return true;

    // Enum constants that are not imported as members should be considered
    // private if the parent enum is marked private.
    if (auto *ECD = dyn_cast<clang::EnumConstantDecl>(D)) {
      auto *ED = cast<clang::EnumDecl>(ECD->getDeclContext());
      switch (classifyEnum(clangSemaPtr->getPreprocessor(), ED)) {
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

  // Omit needless words.
  clang::ASTContext &clangCtx = clangSema.Context;
  StringScratchSpace omitNeedlessWordsScratch;
  if (OmitNeedlessWords) {
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
        getKnownObjCMethod(method),
        result.ErrorInfo ? Optional<unsigned>(result.ErrorInfo->ParamIndex)
                         : None,
        method->hasRelatedResultType(),
        method->isInstanceMethod(),
        omitNeedlessWordsScratch);
    }
  }

  // If this declaration has the swift_private attribute, prepend "__" to the
  // appropriate place.
  SmallString<16> swiftPrivateScratch;
  SmallString<16> swiftPrivateAliasScratch;
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

    // If we have an alias name, make it private as well.
    if (!aliasBaseName.empty()) {
      (void)makeNamePrivate(aliasIsInitializer, aliasBaseName,
                            aliasArgumentNames, CtorInitializerKind::Designated,
                            swiftPrivateAliasScratch);
    }
  }

  result.Imported = formDeclName(baseName, argumentNames, isFunction);
  result.Alias = formDeclName(aliasBaseName, aliasArgumentNames,
                              aliasIsFunction);
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

api_notes::APINotesReader*
ClangImporter::Implementation::getAPINotesForDecl(const clang::Decl *decl) {
  if (auto module = getClangSubmoduleForDecl(decl))
    if (*module)
      return getAPINotesForModule((*module)->getTopLevelModule());
  return nullptr;
}

std::tuple<StringRef, api_notes::APINotesReader *, api_notes::APINotesReader *>
ClangImporter::Implementation::getAPINotesForContext(
    const clang::ObjCContainerDecl *container) {
  // Find the primary set of API notes, in the module where this container
  // was declared.
  api_notes::APINotesReader *primary = getAPINotesForDecl(container);

  StringRef name;

  // Find the secondary set of API notes, in the module where the original
  // class was declared.
  api_notes::APINotesReader *secondary = nullptr;
  if (auto category = dyn_cast<clang::ObjCCategoryDecl>(container)) {
    auto *objcClass = category->getClassInterface();
    secondary = getAPINotesForDecl(objcClass);

    // For categories, use the name of the class itself.
    name = objcClass->getName();
  } else {
    // For protocols and classes, we just need the class name.
    name = container->getName();
  }

  // If the primary and secondary are the same.
  if (primary == secondary) {
    // Drop the secondary; we only care about the primary.
    secondary = nullptr;
  }

  return std::make_tuple(name, primary, secondary);
}

void ClangImporter::Implementation::mergePropInfoIntoAccessor(
    const clang::ObjCMethodDecl *method, api_notes::ObjCMethodInfo &methodInfo){

  if (!method->isPropertyAccessor())
    return;

  const clang::ObjCPropertyDecl *pDecl = method->findPropertyDecl();
  if (!pDecl)
    return;

  if (auto pInfo = getKnownObjCProperty(pDecl)) {
    if (method->param_size() == 0) {
      methodInfo.mergePropInfoIntoGetter(*pInfo);
    } else {
      assert(method->param_size() == 1);
      methodInfo.mergePropInfoIntoSetter(*pInfo);
    }
  }
}

static Optional<std::pair<api_notes::ContextID, api_notes::ObjCContextInfo>> lookupObjCContext(api_notes::APINotesReader *reader,
                    StringRef contextName,
                    const clang::ObjCContainerDecl *contextDecl) {
  Optional<std::pair<api_notes::ContextID, api_notes::ObjCContextInfo>>
    contextInfo;

  // Look for context information in the primary source.
  if (isa<clang::ObjCProtocolDecl>(contextDecl))
    contextInfo = reader->lookupObjCProtocol(contextName);
  else
    contextInfo = reader->lookupObjCClass(contextName);

  return contextInfo;
}


Optional<api_notes::ObjCMethodInfo>
ClangImporter::Implementation::getKnownObjCMethod(
    const clang::ObjCMethodDecl *method,
    const clang::ObjCContainerDecl *container) {
  if (!container)
    container = cast<clang::ObjCContainerDecl>(method->getDeclContext());

  // Figure out where to look for context information.
  StringRef contextName;
  api_notes::APINotesReader *primary;
  api_notes::APINotesReader *secondary;
  std::tie(contextName, primary, secondary) = getAPINotesForContext(container);

  // Map the selector.
  SmallVector<StringRef, 2> selectorPieces;
  api_notes::ObjCSelectorRef selectorRef;
  selectorRef.NumPieces = method->getSelector().getNumArgs();
  for (unsigned i = 0, n = std::max(1u, selectorRef.NumPieces); i != n; ++i) {
    selectorPieces.push_back(method->getSelector().getNameForSlot(i));
  }
  selectorRef.Identifiers = selectorPieces;

  // Look for method and context information in the primary source.
  Optional<api_notes::ObjCMethodInfo> methodInfo;
  Optional<std::pair<api_notes::ContextID, api_notes::ObjCContextInfo>>
    contextInfo;

  if (primary) {
    // Look for context information in the primary source.
    contextInfo = lookupObjCContext(primary, contextName, container);

    // Look for method information in the primary source.
    if (contextInfo) {
      methodInfo = primary->lookupObjCMethod(contextInfo->first, selectorRef,
                                             method->isInstanceMethod());
    }
  }

  // If we found method or class information in the primary source, return what
  // we found.
  if (methodInfo || contextInfo) {
    if (!methodInfo)
      methodInfo = api_notes::ObjCMethodInfo();

    // If accessor, merge the property info in.
    mergePropInfoIntoAccessor(method, *methodInfo);

    // Merge class information into the method.
    *methodInfo |= contextInfo->second;

    return methodInfo;
  }

  // Look for method information in the secondary source.
  if (secondary) {
    // Look for the context information in the secondary source.
    contextInfo = lookupObjCContext(secondary, contextName, container);

    // Look for the method in the secondary source. We don't merge context
    // information from the secondary source.
    if (contextInfo) {
      methodInfo = secondary->lookupObjCMethod(contextInfo->first, selectorRef,
                                               method->isInstanceMethod());

      if (!methodInfo)
        methodInfo = api_notes::ObjCMethodInfo();

      // If accessor, merge the property info in.
      mergePropInfoIntoAccessor(method, *methodInfo);

      return methodInfo;
    }
  }

  return None;
}

Optional<api_notes::ObjCContextInfo>
ClangImporter::Implementation::getKnownObjCContext(
    const clang::ObjCContainerDecl *container) {
  // Figure out where to look for context information.
  StringRef name;
  api_notes::APINotesReader *primary;
  api_notes::APINotesReader *secondary;
  std::tie(name, primary, secondary) = getAPINotesForContext(container);

  Optional<std::pair<api_notes::ContextID, api_notes::ObjCContextInfo>>
    primaryInfo;
  if (primary)
    primaryInfo = lookupObjCContext(primary, name, container);

  Optional<std::pair<api_notes::ContextID, api_notes::ObjCContextInfo>>
    secondaryInfo;
  if (secondary)
    secondaryInfo = lookupObjCContext(secondary, name, container);

  // If neither place had information about this class, we're done.
  if (!primaryInfo && !secondaryInfo)
    return None;

  api_notes::ObjCContextInfo info;

  // Merge in primary information, if available.
  if (primaryInfo) {
    info |= primaryInfo->second;
  }

  // Merge in secondary information after stripping out anything that is not
  // propagated from the context's defining module.
  if (secondaryInfo) {
    secondaryInfo->second.stripModuleLocalInfo();
    info |= secondaryInfo->second;
  }

  return info;
}

Optional<api_notes::ObjCPropertyInfo>
ClangImporter::Implementation::getKnownObjCProperty(
    const clang::ObjCPropertyDecl *property) {
  auto *container = cast<clang::ObjCContainerDecl>(property->getDeclContext());

  // Figure out where to look for context information.
  StringRef contextName;
  api_notes::APINotesReader *primary;
  api_notes::APINotesReader *secondary;
  std::tie(contextName, primary, secondary) = getAPINotesForContext(container);

  // Look for property and context information in the primary source.
  Optional<api_notes::ObjCPropertyInfo> propertyInfo;
  Optional<std::pair<api_notes::ContextID, api_notes::ObjCContextInfo>>
    contextInfo;

  if (primary) {
    // Look for context information in the primary source.
    contextInfo = lookupObjCContext(primary, contextName, container);

    // Look for property information in the primary source.
    if (contextInfo) {
      propertyInfo = primary->lookupObjCProperty(contextInfo->first,
                                                 property->getName());
    }
  }

  // If we found property or class information in the primary source, return what
  // we found.
  if (propertyInfo || contextInfo) {
    if (!propertyInfo)
      propertyInfo = api_notes::ObjCPropertyInfo();

    // Merge class information into the property.
    *propertyInfo |= contextInfo->second;

    return propertyInfo;
  }

  // Look for property information in the secondary source.
  if (secondary) {
    // Look for the context information in the secondary source.
    contextInfo = lookupObjCContext(secondary, contextName, container);

    // Look for the property in the secondary source. We don't merge context
    // information from the secondary source.
    if (contextInfo) {
      return secondary->lookupObjCProperty(contextInfo->first,
                                           property->getName());
    }
  }

  return None;
}

Optional<api_notes::GlobalVariableInfo>
ClangImporter::Implementation::getKnownGlobalVariable(
    const clang::VarDecl *global) {
  if (auto notesReader = getAPINotesForDecl(global))
    return notesReader->lookupGlobalVariable(global->getName());
  
  return None;
}

Optional<api_notes::GlobalFunctionInfo>
ClangImporter::Implementation::getKnownGlobalFunction(
    const clang::FunctionDecl *fn) {
  if (auto notesReader = getAPINotesForDecl(fn))
    return notesReader->lookupGlobalFunction(fn->getName());
  
  return None;
}

bool ClangImporter::Implementation::hasDesignatedInitializers(
       const clang::ObjCInterfaceDecl *classDecl) {
  if (classDecl->hasDesignatedInitializers())
    return true;

  if (auto info = getKnownObjCContext(classDecl))
    return info->hasDesignatedInits();

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

  if (auto info = getKnownObjCMethod(method, classDecl))
    return info->DesignatedInit;

  return false;
}

bool ClangImporter::Implementation::isRequiredInitializer(
       const clang::ObjCMethodDecl *method) {
  // FIXME: No way to express this in Objective-C.

  if (auto info = getKnownObjCMethod(method))
    return info->Required;

  return false;
}

FactoryAsInitKind ClangImporter::Implementation::getFactoryAsInit(
                    const clang::ObjCInterfaceDecl *classDecl,
                    const clang::ObjCMethodDecl *method) {
  if (auto info = getKnownObjCMethod(method, classDecl))
    return info->getFactoryAsInitKind();

  if (auto *customNameAttr = method->getAttr<clang::SwiftNameAttr>()) {
    if (customNameAttr->getName().startswith("init("))
      return FactoryAsInitKind::AsInitializer;
    else
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

bool ClangImporter::Implementation::shouldSuppressDeclImport(
       const clang::Decl *decl) {
  if (auto objcMethod = dyn_cast<clang::ObjCMethodDecl>(decl)) {
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
    if (auto proto = dyn_cast<clang::ObjCProtocolDecl>(decl->getDeclContext()))
      return isPotentiallyConflictingSetter(proto, objcMethod);

    return false;
  }

  if (auto objcProperty = dyn_cast<clang::ObjCPropertyDecl>(decl)) {
    // Suppress certain accessibility properties; they're imported as
    // getter/setter pairs instead.
    if (isAccessibilityDecl(objcProperty))
      return true;

    // Check whether there is a superclass method for the getter that
    // is *not* suppressed, in which case we will need to suppress
    // this property.
    auto dc = objcProperty->getDeclContext();
    auto objcClass = dyn_cast<clang::ObjCInterfaceDecl>(dc);
    if (!objcClass) {
      if (auto objcCategory = dyn_cast<clang::ObjCCategoryDecl>(dc))
        objcClass = objcCategory->getClassInterface();
    }

    if (objcClass) {
      if (auto objcSuperclass = objcClass->getSuperClass()) {
        if (auto getterMethod
              = objcSuperclass->lookupInstanceMethod(
                  objcProperty->getGetterName())) {
          if (!shouldSuppressDeclImport(getterMethod))
            return true;
        }
      }
    }

    return false;
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

  case FactoryAsInitKind::Infer: {
    // See if we can match the class name to the beginning of the first
    // selector piece.
    auto firstPiece = method->getSelector().getNameForSlot(0);
    StringRef firstArgLabel = matchLeadingTypeName(firstPiece,
                                                   objcClass->getName());
    if (firstArgLabel.size() == firstPiece.size())
      return false;

    // FIXME: Factory methods cannot have dummy parameters added for
    // historical reasons.
    if (!firstArgLabel.empty() && method->getSelector().getNumArgs() == 0)
      return false;

    // Store the prefix length.
    prefixLength = firstPiece.size() - firstArgLabel.size();

    // Continue checking the result type, below.
    break;
  }

  case FactoryAsInitKind::AsClassMethod:
    return false;
  }

  // Determine whether we have a suitable return type.
  if (method->hasRelatedResultType()) {
    // When the factory method has an "instancetype" result type, we
    // can import it as a convenience factory method.
    kind = CtorInitializerKind::ConvenienceFactory;
  } else if (auto objcPtr = method->getReturnType()
                              ->getAs<clang::ObjCObjectPointerType>()) {
    if (objcPtr->getInterfaceDecl() != objcClass) {
      // FIXME: Could allow a subclass here, but the rest of the compiler
      // isn't prepared for that yet.
      return false;
    }

    // Factory initializer.
    kind = CtorInitializerKind::Factory;
  } else {
    // Not imported as an initializer.
    return false;
  }

  return true;
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
        if (Decl *imported = Impl.importDeclReal(ND))
          receiver(imported);
      }
    }
  }
  ASTContext &Ctx = Impl.SwiftContext;
  auto &ClangPP = Impl.getClangPreprocessor();
  for (clang::IdentifierInfo *II : Impl.BridgeHeaderMacros) {
    if (auto *MI = ClangPP.getMacroInfo(II)) {
      if (filter(MI)) {
        Identifier Name = Ctx.getIdentifier(II->getName());
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

  ASTContext &Ctx = Impl.SwiftContext;
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
          if (Decl *imported = Impl.importDeclReal(ND))
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
              Identifier Name = Ctx.getIdentifier(II->getName());
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
  // Look for values in the bridging header's lookup table.
  Impl.lookupValue(Impl.BridgingHeaderLookupTable, name, consumer);

  // Collect and sort the set of module names.
  SmallVector<StringRef, 4> moduleNames;
  for (const auto &entry : Impl.LookupTables) {
    moduleNames.push_back(entry.first());
  }
  llvm::array_pod_sort(moduleNames.begin(), moduleNames.end());

  // Look for values in the module lookup tables.
  for (auto moduleName : moduleNames) {
    Impl.lookupValue(*Impl.LookupTables[moduleName].get(), name, consumer);
  }
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

  const clang::Module *topLevelModule = clangModule->getTopLevelModule();

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
                            owner.Impl.importDecl(category)))
        results.push_back(extension);
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

void ClangImporter::loadExtensions(NominalTypeDecl *nominal,
                                   unsigned previousGeneration) {
  auto objcClass = dyn_cast_or_null<clang::ObjCInterfaceDecl>(
                     nominal->getClangDecl());
  if (!objcClass) {
    if (auto typeResolver = Impl.getTypeResolver())
      typeResolver->resolveDeclSignature(nominal);
    if (nominal->isObjC()) {
      // Map the name. If we can't represent the Swift name in Clang, bail out
      // now.
      auto clangName = Impl.exportName(nominal->getName());
      if (!clangName)
        return;

      auto &sema = Impl.Instance->getSema();

      // Perform name lookup into the global scope.
      // FIXME: Map source locations over.
      clang::LookupResult lookupResult(sema, clangName,
                                       clang::SourceLocation(),
                                       clang::Sema::LookupOrdinaryName);
      if (sema.LookupName(lookupResult, /*Scope=*/0)) {
        // FIXME: Filter based on access path? C++ access control?
        for (auto clangDecl : lookupResult) {
          objcClass = dyn_cast<clang::ObjCInterfaceDecl>(clangDecl);
          if (objcClass)
            break;
        }
      }
    }
  }

  if (!objcClass)
    return;

  // Import all of the visible categories. Simply loading them adds them to
  // the list of extensions.
  for (auto I = objcClass->visible_categories_begin(),
            E = objcClass->visible_categories_end();
       I != E; ++I) {
    Impl.importDeclReal(*I);
  }
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
                                          isInstanceMethod);

  // Check whether this method is in the class we care about.
  SmallVector<AbstractFunctionDecl *, 4> foundMethods;
  for (auto objcMethod : objcMethods) {
    // Find the owner of this method and determine whether it is the class
    // we're looking for.
    if (objcMethod->getClassInterface() != objcClass)
      continue;

    if (auto method = dyn_cast_or_null<AbstractFunctionDecl>(
                        Impl.importDecl(objcMethod))) {
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
  if (!clangModule) {
    return "<imports>";
  }
  return clangModule->getASTFile()
    ? clangModule->getASTFile()->getName() : StringRef();
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
  return Impl.importDeclCached(ClangDecl);
}

bool ClangImporter::shouldIgnoreMacro(StringRef Name,
                                      const clang::MacroInfo *Macro) {
  return Impl.shouldIgnoreMacro(Name, Macro);
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
ClangImporter::Implementation::getExtensionMetadata() const {
  clang::ModuleFileExtensionMetadata metadata;
  metadata.BlockName = "swift.lookup";
  metadata.MajorVersion = SWIFT_LOOKUP_TABLE_VERSION_MAJOR;
  metadata.MinorVersion = SWIFT_LOOKUP_TABLE_VERSION_MINOR;
  metadata.UserInfo = version::getSwiftFullVersion();
  return metadata;
}

llvm::hash_code ClangImporter::Implementation::hashExtension(
                  llvm::hash_code code) const {
  return llvm::hash_combine(code, StringRef("swift.lookup"),
                            SWIFT_LOOKUP_TABLE_VERSION_MAJOR,
                            SWIFT_LOOKUP_TABLE_VERSION_MINOR,
                            OmitNeedlessWords,
                            InferDefaultArguments);
}

std::unique_ptr<clang::ModuleFileExtensionWriter>
ClangImporter::Implementation::createExtensionWriter(clang::ASTWriter &writer) {
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
      addEntryToLookupTable(sema, table, named);
    }

    // Add macros to the lookup table.
    addMacrosToLookupTable(sema.Context, sema.getPreprocessor(), table);
  };

  return std::unique_ptr<clang::ModuleFileExtensionWriter>(
           new SwiftLookupTableWriter(this, populateTable, writer));
}

std::unique_ptr<clang::ModuleFileExtensionReader>
ClangImporter::Implementation::createExtensionReader(
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
  auto &entry = LookupTables[mod.ModuleName];
  if (entry) return nullptr;

  // Local function used to remove this entry when the reader goes away.
  std::string moduleName = mod.ModuleName;
  auto onRemove = [this, moduleName]() {
    LookupTables.erase(moduleName);
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

/// Determine whether the given Clang entry is visible.
///
/// FIXME: this is an elaborate hack to badly reflect Clang's
/// submodule visibility into Swift.
static bool isVisibleClangEntry(clang::Preprocessor &pp,
                                StringRef name,
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
  // FIXME: We could get the wrong macro definition here.
  return pp.isMacroDefined(name);
}

void ClangImporter::Implementation::lookupValue(
       SwiftLookupTable &table, DeclName name,
       VisibleDeclConsumer &consumer) {
  auto clangTU = getClangASTContext().getTranslationUnitDecl();
  auto &clangPP = getClangPreprocessor();
  auto baseName = name.getBaseName().str();

  for (auto entry : table.lookup(name.getBaseName().str(), clangTU)) {
    // If the entry is not visible, skip it.
    if (!isVisibleClangEntry(clangPP, baseName, entry)) continue;

    ValueDecl *decl;

    // If it's a Clang declaration, try to import it.
    if (auto clangDecl = entry.dyn_cast<clang::NamedDecl *>()) {
      decl = cast_or_null<ValueDecl>(
               importDeclReal(clangDecl->getMostRecentDecl()));
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
    if (decl->getFullName().matchesRef(name))
      consumer.foundDecl(decl, DeclVisibilityKind::VisibleAtTopLevel);

    // If there is an alternate declaration and the name matches,
    // report this result.
    if (auto alternate = getAlternateDecl(decl)) {
      if (alternate->getFullName().matchesRef(name))
        consumer.foundDecl(alternate, DeclVisibilityKind::VisibleAtTopLevel);
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
  auto &clangPP = getClangPreprocessor();
  auto baseName = name.getBaseName().str();

  for (auto clangDecl : table.lookupObjCMembers(baseName)) {
    // If the entry is not visible, skip it.
    if (!isVisibleClangEntry(clangPP, baseName, clangDecl)) continue;

    // Import the declaration.
    auto decl = cast_or_null<ValueDecl>(importDeclReal(clangDecl));
    if (!decl)
      continue;

    // If the name we found matches, report the declaration.
    if (decl->getFullName().matchesRef(name))
      consumer.foundDecl(decl, DeclVisibilityKind::DynamicLookup);

    // Check for an alternate declaration; if it's name matches,
    // report it.
    if (auto alternate = getAlternateDecl(decl)) {
      if (alternate->getFullName().matchesRef(name))
        consumer.foundDecl(alternate, DeclVisibilityKind::DynamicLookup);
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
