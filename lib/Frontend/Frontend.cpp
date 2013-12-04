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
  assert(getTU());
  TheSILModule = SILModule::createEmptyModule(getTU());
}

bool swift::CompilerInstance::setup(const CompilerInvocation &Invok) {
  Invocation = Invok;

  Context.reset(new ASTContext(Invocation.getLangOptions(), SourceMgr, Diagnostics));

  // Give the context the list of search paths to use for modules.
  Context->ImportSearchPaths = Invocation.getImportSearchPaths();
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
        ImporterCtor(*Context, Invocation.getSDKPath(),
                     Invocation.getTargetTriple(),
                     Invocation.getRuntimeIncludePath(),
                     Invocation.getClangModuleCachePath(),
                     Invocation.getImportSearchPaths(),
                     Invocation.getFrameworkSearchPaths(),
                     StringRef(), Invocation.getExtraClangArgs());
    if (!clangImporter) {
      Diagnostics.diagnose(SourceLoc(), diag::error_clang_importer_create_fail);
      return true;
    }

    Context->addModuleLoader(clangImporter, /*isClang*/true);
  }

  // Add the runtime include path (which contains swift.swift)
  Context->ImportSearchPaths.push_back(Invocation.getRuntimeIncludePath());

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

  bool MainMode = (Invocation.getInputKind() == SourceFile::Main);
  bool SILMode = (Invocation.getInputKind() == SourceFile::SIL);

  // Add the memory buffers first, these will be associated with a filename
  // and they can replace the contents of an input filename.
  for (auto Buf : Invocation.getInputBuffers()) {
    if (SILMode)
      MainBufferIndex = BufferIDs.size();

    // CompilerInvocation doesn't own the buffers, copy to a new buffer.
    BufferIDs.push_back(SourceMgr.addNewSourceBuffer(
        llvm::MemoryBuffer::getMemBufferCopy(Buf->getBuffer(),
                                             Buf->getBufferIdentifier())));
  }

  for (auto &File : Invocation.getInputFilenames()) {
    // FIXME: Working with filenames is fragile, maybe use the real path
    // or have some kind of FileManager.
    if (SourceMgr.getIDForBufferIdentifier(File).hasValue())
      continue; // replaced by a memory buffer.

    // Open the input file.
    llvm::OwningPtr<llvm::MemoryBuffer> InputFile;
    if (llvm::error_code Err =
          llvm::MemoryBuffer::getFileOrSTDIN(File, InputFile)) {
      Diagnostics.diagnose(SourceLoc(), diag::error_open_input_file,
                           File, Err.message());
      return true;
    }

    using namespace llvm::sys::path;
    if (SILMode || (MainMode && filename(File) == "main.swift"))
      MainBufferIndex = BufferIDs.size();

    // Transfer ownership of the MemoryBuffer to the SourceMgr.
    BufferIDs.push_back(SourceMgr.addNewSourceBuffer(InputFile.take()));
  }

  if (MainMode && MainBufferIndex == NO_SUCH_BUFFER && BufferIDs.size() == 1)
    MainBufferIndex = 0;

  return false;
}

void swift::CompilerInstance::doIt() {
  const SourceFile::SourceKind Kind = Invocation.getInputKind();
  Identifier ID = Context->getIdentifier(Invocation.getModuleName());
  TU = new (*Context) TranslationUnit(ID, *Context);
  Context->LoadedModules[ID.str()] = TU;

  if (Kind == SourceFile::SIL) {
    assert(BufferIDs.size() == 1);
    assert(MainBufferIndex != NO_SUCH_BUFFER);
    createSILModule();
  }

  if (Kind == SourceFile::REPL) {
    auto *SingleInputFile =
      new (*Context) SourceFile(*TU, Kind, {}, Invocation.getParseStdlib());
    TU->addSourceFile(*SingleInputFile);
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
  if (MainBufferIndex != NO_SUCH_BUFFER) {
    assert(Kind == SourceFile::Main || Kind == SourceFile::SIL);

    unsigned BufferID = BufferIDs[MainBufferIndex];
    if (Kind == SourceFile::Main)
      SourceMgr.setHashbangBufferID(BufferID);

    auto *SingleInputFile =
      new (*Context) SourceFile(*TU, Kind, BufferID,
                                Invocation.getParseStdlib());
    TU->addSourceFile(*SingleInputFile);
  }

  // Parse all the library files first.
  for (size_t i = 0, e = BufferIDs.size(); i < e; ++i) {
    if (i == MainBufferIndex)
      continue;
    auto BufferID = BufferIDs[i];

    auto *NextInput = new (*Context) SourceFile(*TU, SourceFile::Library,
                                                BufferID,
                                                Invocation.getParseStdlib());
    TU->addSourceFile(*NextInput);

    bool Done;
    parseIntoTranslationUnit(*NextInput, BufferID, &Done, nullptr,
                             &PersistentState, DelayedCB.get());
    assert(Done && "Parser returned early?");
    (void) Done;

    performNameBinding(*NextInput);
  }

  // Parse the main file last.
  if (MainBufferIndex != NO_SUCH_BUFFER) {
    SourceFile &MainFile = *TU->getSourceFiles().front();
    SILParserState SILContext(TheSILModule.get());

    unsigned CurTUElem = 0;
    bool Done;
    do {
      // Pump the parser multiple times if necessary.  It will return early
      // after parsing any top level code in a main module, or in SIL mode when
      // there are chunks of swift decls (e.g. imports and types) interspersed
      // with 'sil' definitions.
      parseIntoTranslationUnit(MainFile, MainFile.getBufferID().getValue(),
                               &Done, TheSILModule ? &SILContext : nullptr,
                               &PersistentState, DelayedCB.get());
      if (!Invocation.getParseOnly())
        performTypeChecking(MainFile, CurTUElem);
      CurTUElem = MainFile.Decls.size();
    } while (!Done);
  }

  if (!Invocation.getParseOnly()) {
    // Type-check each top-level input besides the main source file.
    auto InputSourceFiles = TU->getSourceFiles().slice(0, BufferIDs.size());
    for (auto SF : InputSourceFiles)
      performTypeChecking(*SF);
  }

  if (DelayedCB) {
    performDelayedParsing(TU, PersistentState,
                          Invocation.getCodeCompletionFactory());
  }
}

