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

#include "swift/AST/ASTContext.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Module.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Parse/DelayedParsingCallbacks.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/SILModule.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Subsystems.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"

using namespace swift;

void swift::CompilerInstance::createSILModule() {
  assert(getMainModule());
  TheSILModule = SILModule::createEmptyModule(getMainModule());
}

bool swift::CompilerInstance::setup(const CompilerInvocation &Invok) {
  Invocation = Invok;

  Context.reset(new ASTContext(Invocation.getLangOptions(),
                               Invocation.getSearchPathOptions(),
                               SourceMgr, Diagnostics));

  Context->addModuleLoader(SourceLoader::create(*Context,
                                                !Invocation.isImmediate()));
  SML = SerializedModuleLoader::create(*Context);
  Context->addModuleLoader(SML);

  // If the user has specified an SDK, wire up the Clang module importer
  // and point it at that SDK.
  if (!Invocation.getSDKPath().empty()) {
    auto ImporterCtor = swift::getClangImporterCtor();
    if (!ImporterCtor) {
      Diagnostics.diagnose(SourceLoc(),
                           diag::error_clang_importer_not_linked_in);
      return true;
    }
    auto clangImporter =
        ImporterCtor(*Context, Invocation.getTargetTriple(),
                     Invocation.getClangImporterOptions());
    if (!clangImporter) {
      Diagnostics.diagnose(SourceLoc(), diag::error_clang_importer_create_fail);
      return true;
    }

    Context->addModuleLoader(clangImporter, /*isClang*/true);
  }

  assert(Lexer::isIdentifier(Invocation.getModuleName()));

  auto CodeCompletePoint = Invocation.getCodeCompletionPoint();
  if (CodeCompletePoint.first) {
    auto MemBuf = CodeCompletePoint.first;
    // CompilerInvocation doesn't own the buffers, copy to a new buffer.
    llvm::MemoryBuffer *CodeCompletionBuffer =
        llvm::MemoryBuffer::getMemBufferCopy(MemBuf->getBuffer(),
                                             MemBuf->getBufferIdentifier());
    unsigned CodeCompletionBufferID =
        SourceMgr.addNewSourceBuffer(CodeCompletionBuffer);
    BufferIDs.push_back(CodeCompletionBufferID);
    SourceMgr.setCodeCompletionPoint(CodeCompletionBufferID,
                                     CodeCompletePoint.second);
  }

  bool MainMode = (Invocation.getInputKind() == SourceFileKind::Main);
  bool SILMode = (Invocation.getInputKind() == SourceFileKind::SIL);

  const Optional<SelectedInput> &PrimaryInput =
    Invocation.getFrontendOptions().PrimaryInput;

  // Add the memory buffers first, these will be associated with a filename
  // and they can replace the contents of an input filename.
  for (unsigned i = 0, e = Invocation.getInputBuffers().size(); i != e; ++i) {
    auto Buf = Invocation.getInputBuffers()[i];
    unsigned BufferID = SourceMgr.addNewSourceBuffer(
      llvm::MemoryBuffer::getMemBufferCopy(Buf->getBuffer(),
                                           Buf->getBufferIdentifier()));

    // CompilerInvocation doesn't own the buffers, copy to a new buffer.
    BufferIDs.push_back(BufferID);

    if (SILMode)
      MainBufferID = BufferID;

    if (PrimaryInput && PrimaryInput->isBuffer() && PrimaryInput->Index == i)
      PrimaryBufferID = BufferID;
  }

  for (unsigned i = 0, e = Invocation.getInputFilenames().size(); i != e; ++i) {
    auto &File = Invocation.getInputFilenames()[i];

    // FIXME: Working with filenames is fragile, maybe use the real path
    // or have some kind of FileManager.
    using namespace llvm::sys::path;
    {
      Optional<unsigned> ExistingBufferID =
        SourceMgr.getIDForBufferIdentifier(File);
      if (ExistingBufferID.hasValue()) {
        if (SILMode || (MainMode && filename(File) == "main.swift"))
          MainBufferID = ExistingBufferID.getValue();

        if (PrimaryInput && PrimaryInput->isFilename() &&
            PrimaryInput->Index == i)
          PrimaryBufferID = ExistingBufferID.getValue();

        continue; // replaced by a memory buffer.
      }
    }

    // Open the input file.
    llvm::OwningPtr<llvm::MemoryBuffer> InputFile;
    if (llvm::error_code Err =
          llvm::MemoryBuffer::getFileOrSTDIN(File, InputFile)) {
      Diagnostics.diagnose(SourceLoc(), diag::error_open_input_file,
                           File, Err.message());
      return true;
    }

    unsigned BufferID = SourceMgr.addNewSourceBuffer(InputFile.take());

    // Transfer ownership of the MemoryBuffer to the SourceMgr.
    BufferIDs.push_back(BufferID);

    if (SILMode || (MainMode && filename(File) == "main.swift"))
      MainBufferID = BufferID;

    if (PrimaryInput && PrimaryInput->isFilename() && PrimaryInput->Index == i)
      PrimaryBufferID = BufferID;
  }

  if (MainMode && MainBufferID == NO_SUCH_BUFFER && BufferIDs.size() == 1)
    MainBufferID = BufferIDs.front();

  return false;
}

void CompilerInstance::performParse() {
  const SourceFileKind Kind = Invocation.getInputKind();
  Identifier ID = Context->getIdentifier(Invocation.getModuleName());
  MainModule = new (*Context) Module(ID, *Context);
  Context->LoadedModules[ID.str()] = MainModule;

  if (Kind == SourceFileKind::SIL) {
    assert(BufferIDs.size() == 1);
    assert(MainBufferID != NO_SUCH_BUFFER);
    createSILModule();
  }

  if (Kind == SourceFileKind::REPL) {
    auto *SingleInputFile =
      new (*Context) SourceFile(*MainModule, Kind, {},
                                Invocation.getParseStdlib());
    MainModule->addFile(*SingleInputFile);
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

    auto *SingleInputFile =
      new (*Context) SourceFile(*MainModule, Kind, MainBufferID,
                                Invocation.getParseStdlib());
    MainModule->addFile(*SingleInputFile);

    if (MainBufferID == PrimaryBufferID)
      PrimarySourceFile = SingleInputFile;
  }

  bool hadLoadError = false;

  // Parse all the library files first.
  for (size_t i = 0, e = BufferIDs.size(); i < e; ++i) {
    auto BufferID = BufferIDs[i];
    if (BufferID == MainBufferID)
      continue;

    auto Buffer = SourceMgr.getLLVMSourceMgr().getMemoryBuffer(BufferID);
    if (SerializedModuleLoader::isValidSerializedAST(*Buffer)) {
      std::unique_ptr<llvm::MemoryBuffer> Input(
        llvm::MemoryBuffer::getMemBuffer(Buffer->getBuffer(),
                                         Buffer->getBufferIdentifier(),
                                         false));
      if (!SML->loadAST(*MainModule, SourceLoc(), std::move(Input)))
        hadLoadError = true;
      continue;
    }

    auto *NextInput = new (*Context) SourceFile(*MainModule,
                                                SourceFileKind::Library,
                                                BufferID,
                                                Invocation.getParseStdlib());
    MainModule->addFile(*NextInput);

    if (BufferID == PrimaryBufferID)
      PrimarySourceFile = NextInput;

    bool Done;
    parseIntoSourceFile(*NextInput, BufferID, &Done, nullptr,
                        &PersistentState, DelayedCB.get());
    assert(Done && "Parser returned early?");
    (void) Done;

    performNameBinding(*NextInput);
  }

  if (hadLoadError)
    return;

  // Parse the main file last.
  if (MainBufferID != NO_SUCH_BUFFER) {
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
      if (!Invocation.getParseOnly())
        performTypeChecking(MainFile, PersistentState.getTopLevelContext(),
                            CurTUElem);
      CurTUElem = MainFile.Decls.size();
    } while (!Done);
  }

  if (!Invocation.getParseOnly()) {
    // Type-check each top-level input besides the main source file.
    auto InputSourceFiles = MainModule->getFiles().slice(0, BufferIDs.size());
    for (auto File : InputSourceFiles)
      if (auto SF = dyn_cast<SourceFile>(File))
        performTypeChecking(*SF, PersistentState.getTopLevelContext());

    // If there were no source files, we should still record known protocols.
    if (Context->getStdlibModule())
      Context->recordKnownProtocols(Context->getStdlibModule());
  }

  if (DelayedCB) {
    performDelayedParsing(MainModule, PersistentState,
                          Invocation.getCodeCompletionFactory());
  }
}

