//===--- CI_Inputs.h - CompilerInstance input state & behavior
//------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//

#ifndef CI_Inputs_h
#define CI_Inputs_h

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Module.h"
#include "swift/Basic/InputFile.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/FrontendOptions.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/MemoryBuffer.h"

namespace swift {
class CI_Inputs {

private:
  SourceManager SourceMgr;
  DiagnosticEngine Diagnostics{SourceMgr};

public:
  enum : unsigned { NO_SUCH_BUFFER = ~0U };

public:
  SourceManager &getSourceMgr();
  DiagnosticEngine &getDiags();

  //
  //
  // Main files
  //

private:
  unsigned MainBufferID = NO_SUCH_BUFFER;

public:
  unsigned getMainBufferID() const;
  bool hasMainBufferID() const;

private:
  void setMainBufferID(unsigned);

  //
  //
  // Primary files
  //

private:
  /// Identifies the set of input buffers in the SourceManager that are
  /// considered primaries.
  llvm::SetVector<unsigned> PrimaryBufferIDs;

public:
  const llvm::SetVector<unsigned> getPrimaryBufferIDs() const;

  /// Return whether there is an entry in PrimaryInputs for buffer \BufID.
  bool isPrimaryInput(unsigned BufID) const;

  bool isWholeModuleCompilation() const;
  bool isMainPrimary() const;
  /// Gets the set of SourceFiles which are the primary inputsAndOutputs for
  /// this CompilerInstance.
  const llvm::SetVector<SourceFile *> &getPrimarySourceFiles() const;

  /// Gets the SourceFile which is the primary input for this CompilerInstance.
  /// \returns the primary SourceFile, or nullptr if there is no primary input;
  /// if there are _multiple_ primary inputsAndOutputs, fails with an assertion.
  ///
  /// FIXME: This should be removed eventually, once there are no longer any
  /// codepaths that rely on a single primary file.
  SourceFile *getPrimarySourceFile() const;

  bool hasPrimarySourceFiles() const;

private:
  /// Identifies the set of SourceFiles that are considered primaries. An
  /// invariant is that any SourceFile in this set with an associated
  /// buffer will also have its buffer ID in PrimaryBufferIDs.
  llvm::SetVector<SourceFile *> PrimarySourceFiles;

public:
  void addPrimarySourceFile(SourceFile *);

  /// Record in PrimaryBufferIDs the fact that \BufID is a primary.
  /// If \BufID is already in the set, do nothing.
  void recordPrimaryInputBuffer(unsigned BufID);

  //
  //
  // All source files:
  //

private:
  /// Contains buffer IDs for all input source code files.
  std::vector<unsigned> InputSourceCodeBufferIDs;

public:
  ArrayRef<unsigned> getInputSourceCodeBufferIDs() const;

private:
  void addInputSourceCodeBufferID(unsigned);

  //
  //
  // Module[Doc] memory buffers:
  //

private:
  struct PartialModuleInputs {
    std::unique_ptr<llvm::MemoryBuffer> ModuleBuffer;
    std::unique_ptr<llvm::MemoryBuffer> ModuleDocBuffer;
  };

  /// Contains \c MemoryBuffers for partial serialized module files and
  /// corresponding partial serialized module documentation files.
  std::vector<PartialModuleInputs> PartialModules;

public:
  void consumePartialModules(
      llvm::function_ref<
          void(std::unique_ptr<llvm::MemoryBuffer> moduleBuffer,
               std::unique_ptr<llvm::MemoryBuffer> moduleDocBuffer)>
          fn);

private:
  void addPartialModule(std::unique_ptr<llvm::MemoryBuffer> moduleBuffer,
                        std::unique_ptr<llvm::MemoryBuffer> moduleDocBuffer);

  //
  //
  // Initialization & clean-up
  //

public:
  bool setUpInputs(
      const std::pair<llvm::MemoryBuffer *, unsigned> codeCompletionPoint,
      FrontendOptions &opts);
  void clearPrimarySourceFilesAndBuffers();

private:
  /// Set up all state in the CompilerInstance to process the given input file.
  /// Return true on error.
  bool setUpForInput(const InputFile &input, InputFileKind inputKind);

  Optional<unsigned> setUpCodeCompletionBuffer(
      std::pair<llvm::MemoryBuffer *, unsigned> codeCompletionPoint);

  /// Find a buffer for a given input file and ensure it is recorded in
  /// SourceMgr, PartialModules, or InputSourceCodeBufferIDs as appropriate.
  /// Return the buffer ID if it is not already compiled, or None if so.
  /// Set failed on failure.

  Optional<unsigned> getRecordedBufferID(const InputFile &input, bool &failed);

  /// Given an input file, return a buffer to use for its contents,
  /// and a buffer for the corresponding module doc file if one exists.
  /// On failure, return a null pointer for the first element of the returned
  /// pair.
  std::pair<std::unique_ptr<llvm::MemoryBuffer>,
            std::unique_ptr<llvm::MemoryBuffer>>
  getInputBufferAndModuleDocBufferIfPresent(const InputFile &input);

  /// Try to open the module doc file corresponding to the input parameter.
  /// Return None for error, nullptr if no such file exists, or the buffer if
  /// one was found.
  Optional<std::unique_ptr<llvm::MemoryBuffer>>
  openModuleDoc(const InputFile &input);
};
} // namespace swift

#endif /* CI_Inputs_h */
