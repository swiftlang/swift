//===--- InputFile.h --------------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_FRONTEND_INPUTFILE_H
#define SWIFT_FRONTEND_INPUTFILE_H

#include "swift/Basic/PrimarySpecificPaths.h"
#include "swift/Basic/SupplementaryOutputPaths.h"
#include "llvm/Support/MemoryBuffer.h"
#include <string>
#include <vector>

namespace swift {

enum class InputFileKind {
  None,
  Swift,
  SwiftLibrary,
  SwiftREPL,
  SwiftModuleInterface,
  SIL,
  LLVM
};

// Inputs may include buffers that override contents, and eventually should
// always include a buffer.
class InputFile {
  std::string Filename;
  bool IsPrimary;
  /// Points to a buffer overriding the file's contents, or nullptr if there is
  /// none.
  llvm::MemoryBuffer *Buffer;

  /// If there are explicit primary inputs (i.e. designated with -primary-input
  /// or -primary-filelist), the paths specific to those inputs (other than the
  /// input file path itself) are kept here. If there are no explicit primary
  /// inputs (for instance for whole module optimization), the corresponding
  /// paths are kept in the first input file.
  PrimarySpecificPaths PSPs;

public:
  /// Does not take ownership of \p buffer. Does take ownership of (copy) a
  /// string.
  InputFile(StringRef name, bool isPrimary,
            llvm::MemoryBuffer *buffer = nullptr,
            StringRef outputFilename = StringRef())
      : Filename(
            convertBufferNameFromLLVM_getFileOrSTDIN_toSwiftConventions(name)),
        IsPrimary(isPrimary), Buffer(buffer), PSPs(PrimarySpecificPaths()) {
    assert(!name.empty());
  }

  bool isPrimary() const { return IsPrimary; }
  llvm::MemoryBuffer *buffer() const { return Buffer; }
  const std::string &file() const {
    assert(!Filename.empty());
    return Filename;
  }

  /// Return Swift-standard file name from a buffer name set by
  /// llvm::MemoryBuffer::getFileOrSTDIN, which uses "<stdin>" instead of "-".
  static StringRef convertBufferNameFromLLVM_getFileOrSTDIN_toSwiftConventions(
      StringRef filename) {
    return filename.equals("<stdin>") ? "-" : filename;
  }

  std::string outputFilename() const { return PSPs.OutputFilename; }

  const PrimarySpecificPaths &getPrimarySpecificPaths() const { return PSPs; }

  void setPrimarySpecificPaths(const PrimarySpecificPaths &PSPs) {
    this->PSPs = PSPs;
  }

  // The next set of functions provides access to those primary-specific paths
  // accessed directly from an InputFile, as opposed to via
  // FrontendInputsAndOutputs. They merely make the call sites
  // a bit shorter. Add more forwarding methods as needed.

  std::string dependenciesFilePath() const {
    return getPrimarySpecificPaths().SupplementaryOutputs.DependenciesFilePath;
  }
  std::string loadedModuleTracePath() const {
    return getPrimarySpecificPaths().SupplementaryOutputs.LoadedModuleTracePath;
  }
  std::string serializedDiagnosticsPath() const {
    return getPrimarySpecificPaths().SupplementaryOutputs
        .SerializedDiagnosticsPath;
  }
  std::string fixItsOutputPath() const {
    return getPrimarySpecificPaths().SupplementaryOutputs.FixItsOutputPath;
  }
};
} // namespace swift

#endif /* SWIFT_FRONTEND_INPUTFILE_H */
