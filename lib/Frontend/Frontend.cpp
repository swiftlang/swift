//===-- Frontend.cpp - frontend utility methods ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains utility methods for parsing and performing semantic
// on modules.
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/Frontend.h"
#include "swift/Subsystems.h"
#include "swift/Strings.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/Module.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/DelayedParsingCallbacks.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/SILModule.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"

using namespace swift;

void CompilerInstance::createSILModule() {
  assert(MainModule && "main module not created yet");
  TheSILModule = SILModule::createEmptyModule(getMainModule(),
                                              Invocation.getSILOptions());
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

  // If we are asked to emit a module documentation file, configure lexing and
  // parsing to remember comments.
  if (!Invocation.getFrontendOptions().ModuleDocOutputPath.empty())
    Invocation.getLangOptions().AttachCommentsToDecls = true;

  Context.reset(new ASTContext(Invocation.getLangOptions(),
                               Invocation.getSearchPathOptions(),
                               SourceMgr, Diagnostics));

  if (Invocation.getFrontendOptions().EnableSourceImport) {
    bool immediate = Invocation.getFrontendOptions().actionIsImmediate();
    Context->addModuleLoader(SourceLoader::create(*Context, !immediate,
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
                          Invocation.getIRGenOptions(), DepTracker);
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
    BufferIDs.push_back(*CodeCompletionBufferID);
    SourceMgr.setCodeCompletionPoint(*CodeCompletionBufferID,
                                     CodeCompletePoint.second);
  }

  bool MainMode = (Invocation.getInputKind() == SourceFileKind::Main);
  bool SILMode = (Invocation.getInputKind() == SourceFileKind::SIL);

  if (SILMode)
    Invocation.getLangOptions().EnableAccessControl = false;

  const Optional<SelectedInput> &PrimaryInput =
    Invocation.getFrontendOptions().PrimaryInput;

  // Add the memory buffers first, these will be associated with a filename
  // and they can replace the contents of an input filename.
  for (unsigned i = 0, e = Invocation.getInputBuffers().size(); i != e; ++i) {
    // CompilerInvocation doesn't own the buffers, copy to a new buffer.
    auto *InputBuffer = Invocation.getInputBuffers()[i];
    auto Copy = std::unique_ptr<llvm::MemoryBuffer>(
        llvm::MemoryBuffer::getMemBufferCopy(
            InputBuffer->getBuffer(), InputBuffer->getBufferIdentifier()));
    if (SerializedModuleLoader::isSerializedAST(Copy->getBuffer())) {
      PartialModules.push_back({ std::move(Copy), nullptr });
    } else {
      unsigned BufferID = SourceMgr.addNewSourceBuffer(std::move(Copy));
      BufferIDs.push_back(BufferID);

      if (SILMode)
        MainBufferID = BufferID;

      if (PrimaryInput && PrimaryInput->isBuffer() && PrimaryInput->Index == i)
        PrimaryBufferID = BufferID;
    }
  }

  for (unsigned i = 0, e = Invocation.getInputFilenames().size(); i != e; ++i) {
    auto &File = Invocation.getInputFilenames()[i];

    // FIXME: Working with filenames is fragile, maybe use the real path
    // or have some kind of FileManager.
    using namespace llvm::sys::path;
    if (Optional<unsigned> ExistingBufferID =
            SourceMgr.getIDForBufferIdentifier(File)) {
      if (SILMode || (MainMode && filename(File) == "main.swift"))
        MainBufferID = ExistingBufferID.getValue();

      if (PrimaryInput && PrimaryInput->isFilename() &&
          PrimaryInput->Index == i)
        PrimaryBufferID = ExistingBufferID.getValue();

      continue; // replaced by a memory buffer.
    }

    // Open the input file.
    using FileOrError = llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>;
    FileOrError InputFileOrErr = llvm::MemoryBuffer::getFileOrSTDIN(File);
    if (!InputFileOrErr) {
      Diagnostics.diagnose(SourceLoc(), diag::error_open_input_file,
                           File, InputFileOrErr.getError().message());
      return true;
    }

    if (SerializedModuleLoader::isSerializedAST(
                                          InputFileOrErr.get()->getBuffer())) {
      llvm::SmallString<128> ModuleDocFilePath(File);
      llvm::sys::path::replace_extension(ModuleDocFilePath,
                                         SERIALIZED_MODULE_DOC_EXTENSION);
      FileOrError ModuleDocOrErr =
        llvm::MemoryBuffer::getFileOrSTDIN(ModuleDocFilePath.str());
      if (!ModuleDocOrErr &&
          ModuleDocOrErr.getError() != std::errc::no_such_file_or_directory) {
        Diagnostics.diagnose(SourceLoc(), diag::error_open_input_file,
                             File, ModuleDocOrErr.getError().message());
        return true;
      }
      PartialModules.push_back({ std::move(InputFileOrErr.get()),
                                 ModuleDocOrErr? std::move(ModuleDocOrErr.get())
                                               : nullptr });
      continue;
    }

    // Transfer ownership of the MemoryBuffer to the SourceMgr.
    unsigned BufferID =
      SourceMgr.addNewSourceBuffer(std::move(InputFileOrErr.get()));

    BufferIDs.push_back(BufferID);

    if (SILMode || (MainMode && filename(File) == "main.swift"))
      MainBufferID = BufferID;

    if (PrimaryInput && PrimaryInput->isFilename() && PrimaryInput->Index == i)
      PrimaryBufferID = BufferID;
  }

  // Set the primary file to the code-completion point if one exists.
  if (CodeCompletionBufferID.hasValue())
    PrimaryBufferID = *CodeCompletionBufferID;

  if (MainMode && MainBufferID == NO_SUCH_BUFFER && BufferIDs.size() == 1)
    MainBufferID = BufferIDs.front();

  return false;
}

Module *CompilerInstance::getMainModule() {
  if (!MainModule) {
    Identifier ID = Context->getIdentifier(Invocation.getModuleName());
    MainModule = Module::create(ID, *Context);
  }
  return MainModule;
}

void CompilerInstance::performSema() {
  const SourceFileKind Kind = Invocation.getInputKind();
  Module *MainModule = getMainModule();
  Context->LoadedModules[MainModule->Name] = MainModule;

  auto modImpKind = SourceFile::ImplicitModuleImportKind::Stdlib;

  if (Kind == SourceFileKind::SIL) {
    assert(BufferIDs.size() == 1);
    assert(MainBufferID != NO_SUCH_BUFFER);
    createSILModule();
    modImpKind = SourceFile::ImplicitModuleImportKind::None;
  } else if (Invocation.getParseStdlib()) {
    modImpKind = SourceFile::ImplicitModuleImportKind::Builtin;
  }

  switch (modImpKind) {
  case SourceFile::ImplicitModuleImportKind::None:
  case SourceFile::ImplicitModuleImportKind::Builtin:
    break;
  case SourceFile::ImplicitModuleImportKind::Stdlib:
    if (!Context->getStdlibModule(true)) {
      Diagnostics.diagnose(SourceLoc(), diag::error_stdlib_not_found,
                           Invocation.getTargetTriple());
      return;
    }
    break;
  }

  auto clangImporter =
    static_cast<ClangImporter *>(Context->getClangModuleLoader());

  Module *underlying = nullptr;
  if (Invocation.getFrontendOptions().ImportUnderlyingModule) {
    underlying = clangImporter->loadModule(SourceLoc(),
                                           std::make_pair(MainModule->Name,
                                                          SourceLoc()));
    if (!underlying) {
      Diagnostics.diagnose(SourceLoc(), diag::error_underlying_module_not_found,
                           MainModule->Name);
    }
  }

  Module *importedHeaderModule = nullptr;
  StringRef implicitHeaderPath =
    Invocation.getFrontendOptions().ImplicitObjCHeaderPath;
  if (!implicitHeaderPath.empty()) {
    clangImporter->importBridgingHeader(implicitHeaderPath, MainModule);
    importedHeaderModule = clangImporter->getImportedHeaderModule();
    assert(importedHeaderModule);
  }

  auto addAdditionalInitialImports = [&](SourceFile *SF) {
    if (!underlying && !importedHeaderModule)
      return;

    auto initialImports = SF->getImports(/*allowUnparsed=*/true);

    using ImportPair = std::pair<Module::ImportedModule, bool>;
    SmallVector<ImportPair, 4> initialImportsBuf{
      initialImports.begin(), initialImports.end()
    };
    if (underlying)
      initialImportsBuf.push_back({ { /*accessPath=*/{}, underlying },
                                    /*exported=*/false });
    if (importedHeaderModule)
      initialImportsBuf.push_back({ { /*accessPath=*/{}, importedHeaderModule },
                                    /*exported=*/true });
    SF->setImports(Context->AllocateCopy(initialImportsBuf));
  };

  if (Kind == SourceFileKind::REPL) {
    auto *SingleInputFile =
      new (*Context) SourceFile(*MainModule, Kind, None, modImpKind);
    MainModule->addFile(*SingleInputFile);
    addAdditionalInitialImports(SingleInputFile);
    return;
  }

  std::unique_ptr<DelayedParsingCallbacks> DelayedCB;
  if (Invocation.isCodeCompletion()) {
    DelayedCB.reset(
        new CodeCompleteDelayedCallbacks(SourceMgr.getCodeCompletionLoc()));
  } else if (Invocation.isDelayedFunctionBodyParsing()) {
    DelayedCB.reset(new AlwaysDelayedCallbacks);
  }

  PersistentParserState PersistentState;

  // Make sure the main file is the first file in the module. This may only be
  // a source file, or it may be a SIL file, which requires pumping the parser.
  // We parse it last, though, to make sure that it can use decls from other
  // files in the module.
  if (MainBufferID != NO_SUCH_BUFFER) {
    assert(Kind == SourceFileKind::Main || Kind == SourceFileKind::SIL);

    if (Kind == SourceFileKind::Main)
      SourceMgr.setHashbangBufferID(MainBufferID);

    auto *MainFile = new (*Context) SourceFile(*MainModule, Kind, MainBufferID,
                                               modImpKind);
    MainModule->addFile(*MainFile);
    addAdditionalInitialImports(MainFile);

    if (MainBufferID == PrimaryBufferID)
      setPrimarySourceFile(MainFile);
  }

  bool hadLoadError = false;

  // Parse all the partial modules first.
  for (auto &PM : PartialModules) {
    assert(PM.ModuleBuffer);
    if (!SML->loadAST(*MainModule, SourceLoc(), std::move(PM.ModuleBuffer),
                      std::move(PM.ModuleDocBuffer)))
      hadLoadError = true;
  }

  // Then parse all the library files.
  for (auto BufferID : BufferIDs) {
    if (BufferID == MainBufferID)
      continue;

    auto *NextInput = new (*Context) SourceFile(*MainModule,
                                                SourceFileKind::Library,
                                                BufferID,
                                                modImpKind);
    MainModule->addFile(*NextInput);
    addAdditionalInitialImports(NextInput);

    if (BufferID == PrimaryBufferID)
      setPrimarySourceFile(NextInput);

    bool Done;
    parseIntoSourceFile(*NextInput, BufferID, &Done, nullptr,
                        &PersistentState, DelayedCB.get());
    assert(Done && "Parser returned early?");
    (void) Done;

    performNameBinding(*NextInput);
  }

  if (Invocation.isCodeCompletion()) {
    // When we are doing code completion, make sure to emit at least one
    // diagnostic, so that ASTContext is marked as erroneous.  In this case
    // various parts of the compiler (for example, AST verifier) have less
    // strict assumptions about the AST.
    Diagnostics.diagnose(SourceLoc(), diag::error_doing_code_completion);
  }

  if (hadLoadError)
    return;

  // Compute the options we want to use for type checking.
  OptionSet<TypeCheckingFlags> TypeCheckOptions;
  if (PrimaryBufferID == NO_SUCH_BUFFER) {
    TypeCheckOptions |= TypeCheckingFlags::DelayWholeModuleChecking;
  }
  if (Invocation.getFrontendOptions().DebugTimeFunctionBodies) {
    TypeCheckOptions |= TypeCheckingFlags::DebugTimeFunctionBodies;
  }

  // Parse the main file last.
  if (MainBufferID != NO_SUCH_BUFFER) {
    bool mainIsPrimary =
      (PrimaryBufferID == NO_SUCH_BUFFER || MainBufferID == PrimaryBufferID);

    SourceFile &MainFile = MainModule->getMainSourceFile(Kind);
    SILParserState SILContext(TheSILModule.get());
    unsigned CurTUElem = 0;
    bool Done;
    do {
      // Pump the parser multiple times if necessary.  It will return early
      // after parsing any top level code in a main module, or in SIL mode when
      // there are chunks of swift decls (e.g. imports and types) interspersed
      // with 'sil' definitions.
      parseIntoSourceFile(MainFile, MainFile.getBufferID().getValue(), &Done,
                          TheSILModule ? &SILContext : nullptr,
                          &PersistentState, DelayedCB.get());
      if (mainIsPrimary) {
        performTypeChecking(MainFile, PersistentState.getTopLevelContext(),
                            TypeCheckOptions, CurTUElem);
      }
      CurTUElem = MainFile.Decls.size();
    } while (!Done);
    
    if (mainIsPrimary && Invocation.getFrontendOptions().PlaygroundTransform)
      performPlaygroundTransform(MainFile);
    if (!mainIsPrimary)
      performNameBinding(MainFile);
  }

  // Type-check each top-level input besides the main source file.
  for (auto File : MainModule->getFiles())
    if (auto SF = dyn_cast<SourceFile>(File))
      if (PrimaryBufferID == NO_SUCH_BUFFER || SF == PrimarySourceFile)
        performTypeChecking(*SF, PersistentState.getTopLevelContext(),
                            TypeCheckOptions);

  // Even if there were no source files, we should still record known
  // protocols.
  if (auto *stdlib = Context->getStdlibModule())
    Context->recordKnownProtocols(stdlib);

  if (DelayedCB) {
    performDelayedParsing(MainModule, PersistentState,
                          Invocation.getCodeCompletionFactory());
  }

  // Perform whole-module type checking.
  if (TypeCheckOptions & TypeCheckingFlags::DelayWholeModuleChecking) {
    for (auto File : MainModule->getFiles())
      if (auto SF = dyn_cast<SourceFile>(File))
        performWholeModuleTypeChecking(*SF);
  }
}

void CompilerInstance::performParseOnly() {
  const SourceFileKind Kind = Invocation.getInputKind();
  Module *MainModule = getMainModule();
  Context->LoadedModules[MainModule->Name] = MainModule;

  assert(Kind == SourceFileKind::Main || Kind == SourceFileKind::Library);
  assert(BufferIDs.size() == 1 && "only supports parsing a single file");

  if (Kind == SourceFileKind::Main)
    SourceMgr.setHashbangBufferID(BufferIDs[0]);

  auto *Input = new (*Context) SourceFile(*MainModule,
                                          Kind,
                                          BufferIDs[0],
                                    SourceFile::ImplicitModuleImportKind::None);
  MainModule->addFile(*Input);
  setPrimarySourceFile(Input);

  PersistentParserState PersistentState;
  bool Done;
  do {
    // Pump the parser multiple times if necessary.  It will return early
    // after parsing any top level code in a main module.
    parseIntoSourceFile(*Input, Input->getBufferID().getValue(), &Done,
                        nullptr, &PersistentState, nullptr);
  } while (!Done);

  assert(Context->LoadedModules.size() == 1 &&
         "Loaded a module during parse-only");
}
