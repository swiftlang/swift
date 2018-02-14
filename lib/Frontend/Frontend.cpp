//===--- Frontend.cpp - frontend utility methods --------------------------===//
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
// This file contains utility methods for parsing and performing semantic
// on modules.
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/Frontend.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Module.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
#include "swift/Parse/DelayedParsingCallbacks.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/SILModule.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"

using namespace swift;

std::string CompilerInvocation::getPCHHash() const {
  using llvm::hash_code;
  using llvm::hash_value;
  using llvm::hash_combine;

  auto Code = hash_value(LangOpts.getPCHHashComponents());
  Code = hash_combine(Code, FrontendOpts.getPCHHashComponents());
  Code = hash_combine(Code, ClangImporterOpts.getPCHHashComponents());
  Code = hash_combine(Code, SearchPathOpts.getPCHHashComponents());
  Code = hash_combine(Code, DiagnosticOpts.getPCHHashComponents());
  Code = hash_combine(Code, SILOpts.getPCHHashComponents());
  Code = hash_combine(Code, IRGenOpts.getPCHHashComponents());

  return llvm::APInt(64, Code).toString(36, /*Signed=*/false);
}

void CompilerInstance::createSILModule() {
  assert(MainModule && "main module not created yet");
  // Assume WMO if a -primary-file option was not provided.
  TheSILModule = SILModule::createEmptyModule(
      getMainModule(), Invocation.getSILOptions(),
      Invocation.getFrontendOptions().Inputs.isWholeModule());
}

void CompilerInstance::setPrimarySourceFile(SourceFile *SF) {
  assert(SF);
  assert(MainModule && "main module not created yet");
  assert(!PrimarySourceFile && "already has a primary source file");
  assert(PrimaryBufferID == NO_SUCH_BUFFER || !SF->getBufferID().hasValue() ||
         SF->getBufferID().getValue() == PrimaryBufferID);
  PrimarySourceFile = SF;
  PrimarySourceFile->setReferencedNameTracker(NameTracker);
}

bool CompilerInstance::setup(const CompilerInvocation &Invok) {
  Invocation = Invok;

  // Honor -Xllvm.
  if (!Invok.getFrontendOptions().LLVMArgs.empty()) {
    llvm::SmallVector<const char *, 4> Args;
    Args.push_back("swift (LLVM option parsing)");
    for (unsigned i = 0, e = Invok.getFrontendOptions().LLVMArgs.size(); i != e;
         ++i)
      Args.push_back(Invok.getFrontendOptions().LLVMArgs[i].c_str());
    Args.push_back(nullptr);
    llvm::cl::ParseCommandLineOptions(Args.size()-1, Args.data());
  }

  if (Invocation.getDiagnosticOptions().ShowDiagnosticsAfterFatalError) {
    Diagnostics.setShowDiagnosticsAfterFatalError();
  }
  if (Invocation.getDiagnosticOptions().SuppressWarnings) {
    Diagnostics.setSuppressWarnings(true);
  }
  if (Invocation.getDiagnosticOptions().WarningsAsErrors) {
    Diagnostics.setWarningsAsErrors(true);
  }

  // If we are asked to emit a module documentation file, configure lexing and
  // parsing to remember comments.
  if (!Invocation.getFrontendOptions().ModuleDocOutputPath.empty())
    Invocation.getLangOptions().AttachCommentsToDecls = true;

  // If we are doing index-while-building, configure lexing and parsing to
  // remember comments.
  if (!Invocation.getFrontendOptions().IndexStorePath.empty()) {
    Invocation.getLangOptions().AttachCommentsToDecls = true;
  }

  Context.reset(new ASTContext(Invocation.getLangOptions(),
                               Invocation.getSearchPathOptions(),
                               SourceMgr, Diagnostics));

  if (Invocation.getFrontendOptions().EnableSourceImport) {
    bool immediate = Invocation.getFrontendOptions().actionIsImmediate();
    bool enableResilience = Invocation.getFrontendOptions().EnableResilience;
    Context->addModuleLoader(SourceLoader::create(*Context,
                                                  !immediate,
                                                  enableResilience,
                                                  DepTracker));
  }

  auto SML = SerializedModuleLoader::create(*Context, DepTracker);
  this->SML = SML.get();
  Context->addModuleLoader(std::move(SML));

  // Wire up the Clang importer. If the user has specified an SDK, use it.
  // Otherwise, we just keep it around as our interface to Clang's ABI
  // knowledge.
  auto clangImporter =
    ClangImporter::create(*Context, Invocation.getClangImporterOptions(),
                          Invocation.getPCHHash(),
                          DepTracker);
  if (!clangImporter) {
    Diagnostics.diagnose(SourceLoc(), diag::error_clang_importer_create_fail);
    return true;
  }

  Context->addModuleLoader(std::move(clangImporter), /*isClang*/true);

  assert(Lexer::isIdentifier(Invocation.getModuleName()));

  Optional<unsigned> CodeCompletionBufferID;
  auto CodeCompletePoint = Invocation.getCodeCompletionPoint();
  if (CodeCompletePoint.first) {
    auto MemBuf = CodeCompletePoint.first;
    // CompilerInvocation doesn't own the buffers, copy to a new buffer.
    CodeCompletionBufferID = SourceMgr.addMemBufferCopy(MemBuf);
    InputSourceCodeBufferIDs.push_back(*CodeCompletionBufferID);
    SourceMgr.setCodeCompletionPoint(*CodeCompletionBufferID,
                                     CodeCompletePoint.second);
  }

  bool MainMode = (Invocation.getInputKind() == InputFileKind::IFK_Swift);
  bool SILMode = (Invocation.getInputKind() == InputFileKind::IFK_SIL);

  if (SILMode)
    Invocation.getLangOptions().EnableAccessControl = false;

  const Optional<SelectedInput> &PrimaryInput =
      Invocation.getFrontendOptions().Inputs.getOptionalPrimaryInput();

  // Add the memory buffers first, these will be associated with a filename
  // and they can replace the contents of an input filename.
  for (unsigned i = 0,
                e = Invocation.getFrontendOptions().Inputs.inputBufferCount();
       i != e; ++i) {
    // CompilerInvocation doesn't own the buffers, copy to a new buffer.
    auto *InputBuffer =
        Invocation.getFrontendOptions().Inputs.getInputBuffers()[i];
    auto Copy = std::unique_ptr<llvm::MemoryBuffer>(
        llvm::MemoryBuffer::getMemBufferCopy(
            InputBuffer->getBuffer(), InputBuffer->getBufferIdentifier()));
    if (serialization::isSerializedAST(Copy->getBuffer())) {
      PartialModules.push_back({ std::move(Copy), nullptr });
    } else {
      unsigned BufferID = SourceMgr.addNewSourceBuffer(std::move(Copy));
      InputSourceCodeBufferIDs.push_back(BufferID);

      if (SILMode)
        MainBufferID = BufferID;

      if (PrimaryInput && PrimaryInput->isBuffer() && PrimaryInput->Index == i)
        PrimaryBufferID = BufferID;
    }
  }

  for (unsigned i = 0,
                e = Invocation.getFrontendOptions().Inputs.inputFilenameCount();
       i != e; ++i) {
    bool hasError = setUpForFileAt(i);
    if (hasError) {
      return true;
    }
  }

  // Set the primary file to the code-completion point if one exists.
  if (CodeCompletionBufferID.hasValue())
    PrimaryBufferID = *CodeCompletionBufferID;

  if (MainMode && MainBufferID == NO_SUCH_BUFFER &&
      InputSourceCodeBufferIDs.size() == 1)
    MainBufferID = InputSourceCodeBufferIDs.front();

  return false;
}

ModuleDecl *CompilerInstance::getMainModule() {
  if (!MainModule) {
    Identifier ID = Context->getIdentifier(Invocation.getModuleName());
    MainModule = ModuleDecl::create(ID, *Context);
    if (Invocation.getFrontendOptions().EnableTesting)
      MainModule->setTestingEnabled();

    if (Invocation.getFrontendOptions().EnableResilience)
      MainModule->setResilienceStrategy(ResilienceStrategy::Resilient);
  }
  return MainModule;
}

static void addAdditionalInitialImportsTo(
    SourceFile *SF, const CompilerInstance::ImplicitImports &implicitImports) {
  using ImportPair =
      std::pair<ModuleDecl::ImportedModule, SourceFile::ImportOptions>;
  SmallVector<ImportPair, 4> additionalImports;

  if (implicitImports.objCModuleUnderlyingMixedFramework)
    additionalImports.push_back(
        {{/*accessPath=*/{},
          implicitImports.objCModuleUnderlyingMixedFramework},
         SourceFile::ImportFlags::Exported});
  if (implicitImports.headerModule)
    additionalImports.push_back(
        {{/*accessPath=*/{}, implicitImports.headerModule},
         SourceFile::ImportFlags::Exported});
  if (!implicitImports.modules.empty()) {
    for (auto &importModule : implicitImports.modules) {
      additionalImports.push_back({{/*accessPath=*/{}, importModule}, {}});
    }
  }

  SF->addImports(additionalImports);
}

static bool shouldImportSwiftOnoneModuleIfNoneOrImplicitOptimization(
    FrontendOptions::ActionType RequestedAction) {
  return RequestedAction == FrontendOptions::ActionType::EmitObject ||
         RequestedAction == FrontendOptions::ActionType::Immediate ||
         RequestedAction == FrontendOptions::ActionType::EmitSIL;
}

/// Implicitly import the SwiftOnoneSupport module in non-optimized
/// builds. This allows for use of popular specialized functions
/// from the standard library, which makes the non-optimized builds
/// execute much faster.
static bool
shouldImplicityImportSwiftOnoneSupportModule(CompilerInvocation &Invocation) {
  if (Invocation.getImplicitModuleImportKind() !=
      SourceFile::ImplicitModuleImportKind::Stdlib)
    return false;
  if (Invocation.getSILOptions().shouldOptimize())
    return false;

  if (shouldImportSwiftOnoneModuleIfNoneOrImplicitOptimization(
          Invocation.getFrontendOptions().RequestedAction)) {
    return true;
  }
  return Invocation.getFrontendOptions().isCreatingSIL();
}

void CompilerInstance::performSema() {
  SharedTimer timer("performSema");
  Context->LoadedModules[MainModule->getName()] = getMainModule();

  if (Invocation.getInputKind() == InputFileKind::IFK_SIL) {
    assert(InputSourceCodeBufferIDs.size() == 1);
    assert(MainBufferID != NO_SUCH_BUFFER);
    createSILModule();
  }

  if (Invocation.getImplicitModuleImportKind() ==
      SourceFile::ImplicitModuleImportKind::Stdlib) {
    if (!loadStdlib())
      return;
  }
  if (shouldImplicityImportSwiftOnoneSupportModule(Invocation)) {
    Invocation.getFrontendOptions().ImplicitImportModuleNames.push_back(
        SWIFT_ONONE_SUPPORT);
  }

  const ImplicitImports implicitImports(*this);

  if (Invocation.getInputKind() == InputFileKind::IFK_Swift_REPL) {
    createREPLFile(implicitImports);
    return;
  }

  // Make sure the main file is the first file in the module, so do this now.
  if (MainBufferID != NO_SUCH_BUFFER)
    addMainFileToModule(implicitImports);

  parseAndCheckTypes(implicitImports);
}

CompilerInstance::ImplicitImports::ImplicitImports(CompilerInstance &compiler) {
  kind = compiler.Invocation.getImplicitModuleImportKind();

  objCModuleUnderlyingMixedFramework =
      compiler.Invocation.getFrontendOptions().ImportUnderlyingModule
          ? compiler.importUnderlyingModule()
          : nullptr;

  compiler.getImplicitlyImportedModules(modules);

  headerModule = compiler.importBridgingHeader();
}

bool CompilerInstance::loadStdlib() {
  SharedTimer timer("performSema-loadStdlib");
  ModuleDecl *M = Context->getStdlibModule(true);

  if (!M) {
    Diagnostics.diagnose(SourceLoc(), diag::error_stdlib_not_found,
                         Invocation.getTargetTriple());
    return false;
  }

  // If we failed to load, we should have already diagnosed
  if (M->failedToLoad()) {
    assert(Diagnostics.hadAnyError() &&
           "Module failed to load but nothing was diagnosed?");
    return false;
  }
  return true;
}

ModuleDecl *CompilerInstance::importUnderlyingModule() {
  SharedTimer timer("performSema-importUnderlyingModule");
  ModuleDecl *objCModuleUnderlyingMixedFramework =
      static_cast<ClangImporter *>(Context->getClangModuleLoader())
          ->loadModule(SourceLoc(),
                       std::make_pair(MainModule->getName(), SourceLoc()));
  if (objCModuleUnderlyingMixedFramework)
    return objCModuleUnderlyingMixedFramework;
  Diagnostics.diagnose(SourceLoc(), diag::error_underlying_module_not_found,
                       MainModule->getName());
  return nullptr;
}

ModuleDecl *CompilerInstance::importBridgingHeader() {
  SharedTimer timer("performSema-importBridgingHeader");
  const StringRef implicitHeaderPath =
      Invocation.getFrontendOptions().ImplicitObjCHeaderPath;
  auto clangImporter =
      static_cast<ClangImporter *>(Context->getClangModuleLoader());
  if (implicitHeaderPath.empty() ||
      clangImporter->importBridgingHeader(implicitHeaderPath, MainModule))
    return nullptr;
  ModuleDecl *importedHeaderModule = clangImporter->getImportedHeaderModule();
  assert(importedHeaderModule);
  return importedHeaderModule;
}

void CompilerInstance::getImplicitlyImportedModules(
    SmallVectorImpl<ModuleDecl *> &importModules) {
  SharedTimer timer("performSema-getImplicitlyImportedModules");
  for (auto &ImplicitImportModuleName :
       Invocation.getFrontendOptions().ImplicitImportModuleNames) {
    if (Lexer::isIdentifier(ImplicitImportModuleName)) {
      auto moduleID = Context->getIdentifier(ImplicitImportModuleName);
      ModuleDecl *importModule =
          Context->getModule(std::make_pair(moduleID, SourceLoc()));
      if (importModule) {
        importModules.push_back(importModule);
      } else {
        Diagnostics.diagnose(SourceLoc(), diag::sema_no_import,
                             ImplicitImportModuleName);
        if (Invocation.getSearchPathOptions().SDKPath.empty() &&
            llvm::Triple(llvm::sys::getProcessTriple()).isMacOSX()) {
          Diagnostics.diagnose(SourceLoc(), diag::sema_no_import_no_sdk);
          Diagnostics.diagnose(SourceLoc(), diag::sema_no_import_no_sdk_xcrun);
        }
      }
    } else {
      Diagnostics.diagnose(SourceLoc(), diag::error_bad_module_name,
                           ImplicitImportModuleName, false);
    }
  }
}

void CompilerInstance::createREPLFile(
    const ImplicitImports &implicitImports) const {
  auto *SingleInputFile = new (*Context) SourceFile(
      *MainModule, Invocation.getSourceFileKind(), None, implicitImports.kind,
      Invocation.getLangOptions().CollectParsedToken,
      Invocation.getLangOptions().BuildSyntaxTree);
  MainModule->addFile(*SingleInputFile);
  addAdditionalInitialImportsTo(SingleInputFile, implicitImports);
}

std::unique_ptr<DelayedParsingCallbacks>
CompilerInstance::computeDelayedParsingCallback(bool isPrimary) {
  if (Invocation.isCodeCompletion())
    return llvm::make_unique<CodeCompleteDelayedCallbacks>(
        SourceMgr.getCodeCompletionLoc());
  if (!isPrimary)
    return llvm::make_unique<AlwaysDelayedCallbacks>();
  return nullptr;
}

void CompilerInstance::addMainFileToModule(
    const ImplicitImports &implicitImports) {
  const InputFileKind Kind = Invocation.getInputKind();
  assert(Kind == InputFileKind::IFK_Swift || Kind == InputFileKind::IFK_SIL);

  if (Kind == InputFileKind::IFK_Swift)
    SourceMgr.setHashbangBufferID(MainBufferID);

  auto *MainFile = new (*Context) SourceFile(
      *MainModule, Invocation.getSourceFileKind(), MainBufferID,
      implicitImports.kind, Invocation.getLangOptions().CollectParsedToken,
      Invocation.getLangOptions().BuildSyntaxTree);
  MainModule->addFile(*MainFile);
  addAdditionalInitialImportsTo(MainFile, implicitImports);

  if (MainBufferID == PrimaryBufferID)
    setPrimarySourceFile(MainFile);
}

void CompilerInstance::parseAndCheckTypes(
    const ImplicitImports &implicitImports) {
  SharedTimer timer("performSema-parseAndCheckTypes");
  // Delayed parsing callback for the primary file, or all files
  // in non-WMO mode.
  std::unique_ptr<DelayedParsingCallbacks> PrimaryDelayedCB{
      computeDelayedParsingCallback(true)};

  // Delayed parsing callback for non-primary files. Not used in
  // WMO mode.
  std::unique_ptr<DelayedParsingCallbacks> SecondaryDelayedCB{
      computeDelayedParsingCallback(false)};

  PersistentParserState PersistentState;

  bool hadLoadError = parsePartialModulesAndLibraryFiles(
      implicitImports, PersistentState,
      PrimaryDelayedCB.get(),
      SecondaryDelayedCB.get());
  if (Invocation.isCodeCompletion()) {
    // When we are doing code completion, make sure to emit at least one
    // diagnostic, so that ASTContext is marked as erroneous.  In this case
    // various parts of the compiler (for example, AST verifier) have less
    // strict assumptions about the AST.
    Diagnostics.diagnose(SourceLoc(), diag::error_doing_code_completion);
  }
  if (hadLoadError)
    return;

  OptionSet<TypeCheckingFlags> TypeCheckOptions = computeTypeCheckingOptions();

  // Type-check main file after parsing all other files so that
  // it can use declarations from other files.
  // In addition, the main file has parsing and type-checking
  // interwined.
  if (MainBufferID != NO_SUCH_BUFFER) {
    parseAndTypeCheckMainFile(PersistentState, PrimaryDelayedCB.get(),
                              TypeCheckOptions);
  }

  const auto &options = Invocation.getFrontendOptions();
  forEachFileToTypeCheck([&](SourceFile &SF) {
    performTypeChecking(SF, PersistentState.getTopLevelContext(),
                        TypeCheckOptions, /*curElem*/ 0,
                        options.WarnLongFunctionBodies,
                        options.WarnLongExpressionTypeChecking,
                        options.SolverExpressionTimeThreshold);
  });

  // Even if there were no source files, we should still record known
  // protocols.
  if (auto *stdlib = Context->getStdlibModule())
    Context->recordKnownProtocols(stdlib);

  if (Invocation.isCodeCompletion()) {
    performDelayedParsing(MainModule, PersistentState,
                          Invocation.getCodeCompletionFactory());
  }
  finishTypeChecking(TypeCheckOptions);
}

void CompilerInstance::parseLibraryFile(
    unsigned BufferID, const ImplicitImports &implicitImports,
    PersistentParserState &PersistentState,
    DelayedParsingCallbacks *PrimaryDelayedCB,
    DelayedParsingCallbacks *SecondaryDelayedCB) {
  SharedTimer timer("performSema-parseLibraryFile");

  auto *NextInput = new (*Context) SourceFile(
      *MainModule, SourceFileKind::Library, BufferID, implicitImports.kind,
      Invocation.getLangOptions().CollectParsedToken,
      Invocation.getLangOptions().BuildSyntaxTree);
  MainModule->addFile(*NextInput);
  addAdditionalInitialImportsTo(NextInput, implicitImports);

  auto *DelayedCB = SecondaryDelayedCB;
  if (BufferID == PrimaryBufferID) {
    setPrimarySourceFile(NextInput);
    DelayedCB = PrimaryDelayedCB;
  }
  if (isWholeModuleCompilation())
    DelayedCB = PrimaryDelayedCB;

  auto &Diags = NextInput->getASTContext().Diags;
  auto DidSuppressWarnings = Diags.getSuppressWarnings();
  auto IsPrimary = isWholeModuleCompilation() || BufferID == PrimaryBufferID;
  Diags.setSuppressWarnings(DidSuppressWarnings || !IsPrimary);

  bool Done;
  do {
    // Parser may stop at some erroneous constructions like #else, #endif
    // or '}' in some cases, continue parsing until we are done
    parseIntoSourceFile(*NextInput, BufferID, &Done, nullptr, &PersistentState,
                        DelayedCB);
  } while (!Done);

  Diags.setSuppressWarnings(DidSuppressWarnings);

  performNameBinding(*NextInput);
}

OptionSet<TypeCheckingFlags> CompilerInstance::computeTypeCheckingOptions() {
  OptionSet<TypeCheckingFlags> TypeCheckOptions;
  if (isWholeModuleCompilation()) {
    TypeCheckOptions |= TypeCheckingFlags::DelayWholeModuleChecking;
  }
  const auto &options = Invocation.getFrontendOptions();
  if (options.DebugTimeFunctionBodies) {
    TypeCheckOptions |= TypeCheckingFlags::DebugTimeFunctionBodies;
  }
  if (options.actionIsImmediate()) {
    TypeCheckOptions |= TypeCheckingFlags::ForImmediateMode;
  }
  if (options.DebugTimeExpressionTypeChecking) {
    TypeCheckOptions |= TypeCheckingFlags::DebugTimeExpressions;
  }
  return TypeCheckOptions;
}

bool CompilerInstance::parsePartialModulesAndLibraryFiles(
    const ImplicitImports &implicitImports,
    PersistentParserState &PersistentState,
    DelayedParsingCallbacks *PrimaryDelayedCB,
    DelayedParsingCallbacks *SecondaryDelayedCB) {
  SharedTimer timer("performSema-parsePartialModulesAndLibraryFiles");
  bool hadLoadError = false;
  // Parse all the partial modules first.
  for (auto &PM : PartialModules) {
    assert(PM.ModuleBuffer);
    if (!SML->loadAST(*MainModule, SourceLoc(), std::move(PM.ModuleBuffer),
                      std::move(PM.ModuleDocBuffer)))
      hadLoadError = true;
  }

  // Then parse all the library files.
  for (auto BufferID : InputSourceCodeBufferIDs) {
    if (BufferID != MainBufferID) {
      parseLibraryFile(BufferID, implicitImports, PersistentState,
                       PrimaryDelayedCB, SecondaryDelayedCB);
    }
  }
  return hadLoadError;
}

void CompilerInstance::parseAndTypeCheckMainFile(
    PersistentParserState &PersistentState,
    DelayedParsingCallbacks *DelayedParseCB,
    OptionSet<TypeCheckingFlags> TypeCheckOptions) {
  SharedTimer timer(
      "performSema-checkTypesWhileParsingMain-parseAndTypeCheckMainFile");
  bool mainIsPrimary =
      (isWholeModuleCompilation() || MainBufferID == PrimaryBufferID);

  SourceFile &MainFile =
      MainModule->getMainSourceFile(Invocation.getSourceFileKind());

  auto &Diags = MainFile.getASTContext().Diags;
  auto DidSuppressWarnings = Diags.getSuppressWarnings();
  Diags.setSuppressWarnings(DidSuppressWarnings || !mainIsPrimary);

  SILParserState SILContext(TheSILModule.get());
  unsigned CurTUElem = 0;
  bool Done;
  do {
    // Pump the parser multiple times if necessary.  It will return early
    // after parsing any top level code in a main module, or in SIL mode when
    // there are chunks of swift decls (e.g. imports and types) interspersed
    // with 'sil' definitions.
    parseIntoSourceFile(MainFile, MainFile.getBufferID().getValue(), &Done,
                        TheSILModule ? &SILContext : nullptr, &PersistentState,
                        DelayedParseCB);
    if (mainIsPrimary) {
      const auto &options = Invocation.getFrontendOptions();
      performTypeChecking(MainFile, PersistentState.getTopLevelContext(),
                          TypeCheckOptions, CurTUElem,
                          options.WarnLongFunctionBodies,
                          options.WarnLongExpressionTypeChecking,
                          options.SolverExpressionTimeThreshold);
    }
    CurTUElem = MainFile.Decls.size();
  } while (!Done);

  Diags.setSuppressWarnings(DidSuppressWarnings);

  if (mainIsPrimary && !Context->hadError() &&
      Invocation.getFrontendOptions().PCMacro) {
    performPCMacro(MainFile, PersistentState.getTopLevelContext());
  }

  // Playground transform knows to look out for PCMacro's changes and not
  // to playground log them.
  if (mainIsPrimary && !Context->hadError() &&
      Invocation.getFrontendOptions().PlaygroundTransform)
    performPlaygroundTransform(
        MainFile, Invocation.getFrontendOptions().PlaygroundHighPerformance);
  if (!mainIsPrimary) {
    performNameBinding(MainFile);
  }
}

static void
forEachSourceFileIn(ModuleDecl *module,
                    llvm::function_ref<void(SourceFile &)> fn) {
  for (auto File : module->getFiles()) {
    if (auto SF = dyn_cast<SourceFile>(File))
      fn(*SF);
  }
}

void CompilerInstance::forEachFileToTypeCheck(
    llvm::function_ref<void(SourceFile &)> fn) {
  if (isWholeModuleCompilation()) {
    forEachSourceFileIn(MainModule, [&](SourceFile &SF) { fn(SF); });
  } else {
    fn(*PrimarySourceFile);
  }
}

void CompilerInstance::finishTypeChecking(
    OptionSet<TypeCheckingFlags> TypeCheckOptions) {
  if (TypeCheckOptions & TypeCheckingFlags::DelayWholeModuleChecking) {
    forEachSourceFileIn(MainModule, [&](SourceFile &SF) {
      performWholeModuleTypeChecking(SF);
    });
  }
}

void CompilerInstance::performParseOnly(bool EvaluateConditionals) {
  const InputFileKind Kind = Invocation.getInputKind();
  ModuleDecl *MainModule = getMainModule();
  Context->LoadedModules[MainModule->getName()] = MainModule;

  assert((Kind == InputFileKind::IFK_Swift ||
          Kind == InputFileKind::IFK_Swift_Library) &&
         "only supports parsing .swift files");
  (void)Kind;

  auto implicitModuleImportKind = SourceFile::ImplicitModuleImportKind::None;

  // Make sure the main file is the first file in the module but parse it last,
  // to match the parsing logic used when performing Sema.
  if (MainBufferID != NO_SUCH_BUFFER) {
    assert(Kind == InputFileKind::IFK_Swift);
    SourceMgr.setHashbangBufferID(MainBufferID);

    auto *MainFile = new (*Context)
        SourceFile(*MainModule, Invocation.getSourceFileKind(), MainBufferID,
                   implicitModuleImportKind,
                   Invocation.getLangOptions().CollectParsedToken,
                   Invocation.getLangOptions().BuildSyntaxTree);
    MainModule->addFile(*MainFile);

    if (MainBufferID == PrimaryBufferID)
      setPrimarySourceFile(MainFile);
  }

  PersistentParserState PersistentState;
  PersistentState.PerformConditionEvaluation = EvaluateConditionals;
  // Parse all the library files.
  for (auto BufferID : InputSourceCodeBufferIDs) {
    if (BufferID == MainBufferID)
      continue;

    auto *NextInput = new (*Context)
        SourceFile(*MainModule, SourceFileKind::Library, BufferID,
                   implicitModuleImportKind,
                   Invocation.getLangOptions().CollectParsedToken,
                   Invocation.getLangOptions().BuildSyntaxTree);
    MainModule->addFile(*NextInput);
    if (BufferID == PrimaryBufferID)
      setPrimarySourceFile(NextInput);

    bool Done;
    do {
      // Parser may stop at some erroneous constructions like #else, #endif
      // or '}' in some cases, continue parsing until we are done
      parseIntoSourceFile(*NextInput, BufferID, &Done, nullptr,
                          &PersistentState, nullptr);
    } while (!Done);
  }

  // Now parse the main file.
  if (MainBufferID != NO_SUCH_BUFFER) {
    SourceFile &MainFile =
        MainModule->getMainSourceFile(Invocation.getSourceFileKind());

    bool Done;
    do {
      parseIntoSourceFile(MainFile, MainFile.getBufferID().getValue(), &Done,
                          nullptr, &PersistentState, nullptr);
    } while (!Done);
  }

  assert(Context->LoadedModules.size() == 1 &&
         "Loaded a module during parse-only");
}

void CompilerInstance::freeContextAndSIL() {
  Context.reset();
  TheSILModule.reset();
  MainModule = nullptr;
  SML = nullptr;
  PrimarySourceFile = nullptr;
}

bool CompilerInstance::setUpForFileAt(unsigned i) {
  bool MainMode = (Invocation.getInputKind() == InputFileKind::IFK_Swift);
  bool SILMode = (Invocation.getInputKind() == InputFileKind::IFK_SIL);

  auto &File = Invocation.getFrontendOptions().Inputs.getInputFilenames()[i];

  // FIXME: Working with filenames is fragile, maybe use the real path
  // or have some kind of FileManager.
  using namespace llvm::sys::path;
  if (Optional<unsigned> ExistingBufferID =
          SourceMgr.getIDForBufferIdentifier(File)) {
    if (SILMode || (MainMode && filename(File) == "main.swift"))
      MainBufferID = ExistingBufferID.getValue();

    if (Invocation.getFrontendOptions().Inputs.isPrimaryInputAFileAt(i))
      PrimaryBufferID = ExistingBufferID.getValue();

    return false; // replaced by a memory buffer.
  }

  // Open the input file.
  using FileOrError = llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>;
  FileOrError InputFileOrErr = llvm::MemoryBuffer::getFileOrSTDIN(File);
  if (!InputFileOrErr) {
    Diagnostics.diagnose(SourceLoc(), diag::error_open_input_file, File,
                         InputFileOrErr.getError().message());
    return true;
  }

  if (serialization::isSerializedAST(InputFileOrErr.get()->getBuffer())) {
    llvm::SmallString<128> ModuleDocFilePath(File);
    llvm::sys::path::replace_extension(ModuleDocFilePath,
                                       SERIALIZED_MODULE_DOC_EXTENSION);
    FileOrError ModuleDocOrErr =
        llvm::MemoryBuffer::getFileOrSTDIN(ModuleDocFilePath.str());
    if (!ModuleDocOrErr &&
        ModuleDocOrErr.getError() != std::errc::no_such_file_or_directory) {
      Diagnostics.diagnose(SourceLoc(), diag::error_open_input_file, File,
                           ModuleDocOrErr.getError().message());
      return true;
    }
    PartialModules.push_back(
        {std::move(InputFileOrErr.get()),
         ModuleDocOrErr ? std::move(ModuleDocOrErr.get()) : nullptr});
    return false;
  }

  // Transfer ownership of the MemoryBuffer to the SourceMgr.
  unsigned BufferID =
      SourceMgr.addNewSourceBuffer(std::move(InputFileOrErr.get()));

  InputSourceCodeBufferIDs.push_back(BufferID);

  if (SILMode || (MainMode && filename(File) == "main.swift"))
    MainBufferID = BufferID;

  if (Invocation.getFrontendOptions().Inputs.isPrimaryInputAFileAt(i))
    PrimaryBufferID = BufferID;

  return false;
}
