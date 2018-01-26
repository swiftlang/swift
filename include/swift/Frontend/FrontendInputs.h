//===--- FrontendInputs.h ---------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_FRONTEND_FRONTENDINPUTS_H
#define SWIFT_FRONTEND_FRONTENDINPUTS_H

#include "swift/AST/Module.h"
#include "swift/Frontend/FrontendInputs.h"
#include "swift/Frontend/InputFile.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/MapVector.h"

#include <string>
#include <vector>

namespace llvm {
class MemoryBuffer;
}

namespace swift {

/// Information about all the inputs to the frontend.
class FrontendInputs {
  friend class ArgsToFrontendInputsConverter;

  std::vector<InputFile> AllInputs;
  typedef llvm::StringMap<unsigned> InputFileMap;
  InputFileMap PrimaryInputs;

public:
  FrontendInputs() = default;

  FrontendInputs(const FrontendInputs &other);

  FrontendInputs &operator=(const FrontendInputs &other);

  // Readers:

  ArrayRef<InputFile> getAllInputs() const { return AllInputs; }
  
  std::vector<std::string> getInputFilenames() const;

  unsigned inputCount() const { return AllInputs.size(); }

  bool hasInputs() const { return !AllInputs.empty(); }

  bool hasSingleInput() const { return inputCount() == 1; }

  StringRef getFilenameOfFirstInput() const;

  bool isReadingFromStdin() const;

  // If we have exactly one input filename, and its extension is "bc" or "ll",
  // treat the input as LLVM_IR.
  bool shouldTreatAsLLVM() const;

  // Primary input readers

private:
  void assertMustNotBeMoreThanOnePrimaryInput() const;

  bool areAllNonPrimariesSIB() const;

public:
  unsigned primaryInputCount() const { return PrimaryInputs.size(); }

  // Primary count readers:

  bool hasUniquePrimaryInput() const { return primaryInputCount() == 1; }

  bool hasPrimaryInputs() const { return primaryInputCount() > 0; }

  bool isWholeModule() const { return !hasPrimaryInputs(); }

  // Count-dependend readers:

  /// \return the unique primary input, if one exists.
  const InputFile *getUniquePrimaryInput() const;

  const InputFile &getRequiredUniquePrimaryInput() const;

  /// \return the name of the unique primary input, or an empty StrinRef if
  /// there isn't one.
  StringRef getNameOfUniquePrimaryInputFile() const;

  bool isInputPrimary(StringRef file) const;

  unsigned numberOfPrimaryInputsEndingWith(const char *extension) const;

  // Multi-facet readers

  bool shouldTreatAsSIL() const;

  /// \return true for error
  bool verifyInputs(DiagnosticEngine &diags, bool treatAsSIL,
                    bool isREPLRequested, bool isNoneRequested) const;

  // Writers

  void addInputFile(StringRef file, llvm::MemoryBuffer *buffer = nullptr) {
    addInput(InputFile(file, false, buffer));
  }
  void addPrimaryInputFile(StringRef file,
                           llvm::MemoryBuffer *buffer = nullptr) {
    addInput(InputFile(file, true, buffer));
  }

  void addInput(const InputFile &input);

  void clearInputs() {
    AllInputs.clear();
    PrimaryInputs.clear();
  }
};

} // namespace swift

#endif /* SWIFT_FRONTEND_FRONTENDINPUTS_H */
