//===--- CI_Inputs.cpp - CompilerInstance input methods
//--------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//

#include "swift/Frontend/CI_Inputs.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Module.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
#include "swift/Frontend/Frontend.h"
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

SourceManager &CI_Inputs::getSourceMgr() { return SourceMgr; }

DiagnosticEngine &CI_Inputs::getDiags() { return Diagnostics; }

//
//
// Main files
//

unsigned CI_Inputs::getMainBufferID() const { return MainBufferID; }

bool CI_Inputs::hasMainBufferID() const {
  return getMainBufferID() != NO_SUCH_BUFFER;
}

void CI_Inputs::setMainBufferID(unsigned id) { MainBufferID = id; }

//
//
// Primary files
//

const llvm::SetVector<unsigned> CI_Inputs::getPrimaryBufferIDs() const {
  return PrimaryBufferIDs;
}

bool CI_Inputs::isPrimaryInput(unsigned BufID) const {
  return getPrimaryBufferIDs().count(BufID) != 0;
}

bool CI_Inputs::isWholeModuleCompilation() const {
  return PrimaryBufferIDs.empty();
}

bool CI_Inputs::isMainPrimary() const {
  return isWholeModuleCompilation() || isPrimaryInput(getMainBufferID());
}

const llvm::SetVector<SourceFile *> &CI_Inputs::getPrimarySourceFiles() const {
  return PrimarySourceFiles;
}

SourceFile *CI_Inputs::getPrimarySourceFile() const {
  if (PrimarySourceFiles.empty()) {
    return nullptr;
  }
  assert(PrimarySourceFiles.size() == 1);
  return *PrimarySourceFiles.begin();
}

bool CI_Inputs::hasPrimarySourceFiles() const {
  return !PrimarySourceFiles.empty();
}

void CI_Inputs::addPrimarySourceFile(SourceFile *sf) {
  PrimarySourceFiles.insert(sf);
}

void CI_Inputs::recordPrimaryInputBuffer(unsigned BufID) {
  PrimaryBufferIDs.insert(BufID);
}

//
//
// All source files:
//

ArrayRef<unsigned> CI_Inputs::getInputSourceCodeBufferIDs() const {
  return InputSourceCodeBufferIDs;
}

void CI_Inputs::addInputSourceCodeBufferID(unsigned id) {
  InputSourceCodeBufferIDs.push_back(id);
}

//
//
// Module[Doc] memory buffers:
//

void CI_Inputs::consumePartialModules(
    llvm::function_ref<
        void(std::unique_ptr<llvm::MemoryBuffer> moduleBuffer,
             std::unique_ptr<llvm::MemoryBuffer> moduleDocBuffer)>
        fn) {
  for (auto &PM : PartialModules) {
    fn(std::move(PM.ModuleBuffer), std::move(PM.ModuleDocBuffer));
  }
}

void CI_Inputs::addPartialModule(
    std::unique_ptr<llvm::MemoryBuffer> moduleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> moduleDocBuffer) {
  PartialModules.push_back(
      {std::move(moduleBuffer), std::move(moduleDocBuffer)});
}

//
//
// Initialization & clean-up
//

bool CI_Inputs::setUpInputs(
    const std::pair<llvm::MemoryBuffer *, unsigned> codeCompletionPoint,
    FrontendOptions &opts) {
  // Adds to InputSourceCodeBufferIDs, so may need to happen before the
  // per-input setup.
  const Optional<unsigned> codeCompletionBufferID =
      setUpCodeCompletionBuffer(codeCompletionPoint);

  const InputFileKind inputKind = opts.InputKind;
  {
    bool hadError = false;
    opts.InputsAndOutputs.forEachInput([&](const InputFile &input) {
      hadError = setUpForInput(input, inputKind) || hadError;
    });
    if (hadError)
      return true;
  }

  // Set the primary file to the code-completion point if one exists.
  if (codeCompletionBufferID.hasValue() &&
      !isPrimaryInput(*codeCompletionBufferID)) {
    assert(PrimaryBufferIDs.empty() && "re-setting PrimaryBufferID");
    recordPrimaryInputBuffer(*codeCompletionBufferID);
  }

  if (inputKind == InputFileKind::IFK_Swift &&
      getMainBufferID() == NO_SUCH_BUFFER &&
      InputSourceCodeBufferIDs.size() == 1)
    setMainBufferID(InputSourceCodeBufferIDs.front());

  return false;
}

void CI_Inputs::clearPrimarySourceFilesAndBuffers() {
  PrimaryBufferIDs.clear();
  PrimarySourceFiles.clear();
}

bool CI_Inputs::setUpForInput(const InputFile &input,
                              const InputFileKind inputKind) {
  bool failed = false;
  Optional<unsigned> bufferID = getRecordedBufferID(input, failed);
  if (failed)
    return true;
  if (!bufferID)
    return false;

  if (inputKind == InputFileKind::IFK_SIL ||
      (input.buffer() == nullptr && inputKind == InputFileKind::IFK_Swift &&
       llvm::sys::path::filename(input.file()) == "main.swift")) {
    assert(MainBufferID == NO_SUCH_BUFFER && "re-setting MainBufferID");
    setMainBufferID(*bufferID);
  }

  if (input.isPrimary()) {
    assert(PrimaryBufferIDs.empty() && "re-setting PrimaryBufferID");
    recordPrimaryInputBuffer(*bufferID);
  }
  return false;
}

Optional<unsigned> CI_Inputs::setUpCodeCompletionBuffer(
    std::pair<llvm::MemoryBuffer *, unsigned> codeCompletionPoint) {
  Optional<unsigned> codeCompletionBufferID;
  if (codeCompletionPoint.first) {
    auto memBuf = codeCompletionPoint.first;
    // CompilerInvocation doesn't own the buffers, copy to a new buffer.
    codeCompletionBufferID = SourceMgr.addMemBufferCopy(memBuf);
    InputSourceCodeBufferIDs.push_back(*codeCompletionBufferID);
    SourceMgr.setCodeCompletionPoint(*codeCompletionBufferID,
                                     codeCompletionPoint.second);
  }
  return codeCompletionBufferID;
}

Optional<unsigned> CI_Inputs::getRecordedBufferID(const InputFile &input,
                                                  bool &failed) {
  if (!input.buffer()) {
    if (Optional<unsigned> existingBufferID =
            SourceMgr.getIDForBufferIdentifier(input.file())) {
      return existingBufferID;
    }
  }
  std::pair<std::unique_ptr<llvm::MemoryBuffer>,
            std::unique_ptr<llvm::MemoryBuffer>>
      buffers = getInputBufferAndModuleDocBufferIfPresent(input);

  if (!buffers.first) {
    failed = true;
    return None;
  }

  // FIXME: The fact that this test happens twice, for some cases,
  // suggests that setupInputs could use another round of refactoring.
  if (serialization::isSerializedAST(buffers.first->getBuffer())) {
    addPartialModule(std::move(buffers.first), std::move(buffers.second));
    return None;
  }
  assert(buffers.second.get() == nullptr);
  // Transfer ownership of the MemoryBuffer to the SourceMgr.
  unsigned bufferID = SourceMgr.addNewSourceBuffer(std::move(buffers.first));

  addInputSourceCodeBufferID(bufferID);
  return bufferID;
}

std::pair<std::unique_ptr<llvm::MemoryBuffer>,
          std::unique_ptr<llvm::MemoryBuffer>>
CI_Inputs::getInputBufferAndModuleDocBufferIfPresent(const InputFile &input) {
  if (auto b = input.buffer()) {
    return std::make_pair(llvm::MemoryBuffer::getMemBufferCopy(
                              b->getBuffer(), b->getBufferIdentifier()),
                          nullptr);
  }
  // FIXME: Working with filenames is fragile, maybe use the real path
  // or have some kind of FileManager.
  using FileOrError = llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>;
  FileOrError inputFileOrErr = llvm::MemoryBuffer::getFileOrSTDIN(input.file());
  if (!inputFileOrErr) {
    Diagnostics.diagnose(SourceLoc(), diag::error_open_input_file, input.file(),
                         inputFileOrErr.getError().message());
    return std::make_pair(nullptr, nullptr);
  }
  if (!serialization::isSerializedAST((*inputFileOrErr)->getBuffer()))
    return std::make_pair(std::move(*inputFileOrErr), nullptr);

  if (Optional<std::unique_ptr<llvm::MemoryBuffer>> moduleDocBuffer =
          openModuleDoc(input)) {
    return std::make_pair(std::move(*inputFileOrErr),
                          std::move(*moduleDocBuffer));
  }
  return std::make_pair(nullptr, nullptr);
}

Optional<std::unique_ptr<llvm::MemoryBuffer>>
CI_Inputs::openModuleDoc(const InputFile &input) {
  llvm::SmallString<128> moduleDocFilePath(input.file());
  llvm::sys::path::replace_extension(moduleDocFilePath,
                                     SERIALIZED_MODULE_DOC_EXTENSION);
  using FileOrError = llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>;
  FileOrError moduleDocFileOrErr =
      llvm::MemoryBuffer::getFileOrSTDIN(moduleDocFilePath);
  if (moduleDocFileOrErr)
    return std::move(*moduleDocFileOrErr);

  if (moduleDocFileOrErr.getError() == std::errc::no_such_file_or_directory)
    return std::unique_ptr<llvm::MemoryBuffer>();

  Diagnostics.diagnose(SourceLoc(), diag::error_open_input_file,
                       moduleDocFilePath,
                       moduleDocFileOrErr.getError().message());
  return None;
}
