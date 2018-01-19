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
#include "swift/Basic/InputFile.h"
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

/// Information about all the inputs and outputs to the frontend.

class FrontendInputs {
  friend class ArgsToFrontendInputsConverter;

  std::vector<InputFile> AllFiles;
  typedef llvm::StringMap<unsigned> InputFileMap;
  InputFileMap PrimaryInputs;
  bool IsSingleThreadedWMO = false;

public:
  FrontendInputs() = default;

  FrontendInputs(const FrontendInputs &other) {
    for (InputFile input : other.getAllFiles())
      addInput(input);
    IsSingleThreadedWMO = other.IsSingleThreadedWMO;
  }

  FrontendInputs &operator=(const FrontendInputs &other) {
    clearInputs();
    for (InputFile input : other.getAllFiles())
      addInput(input);
    IsSingleThreadedWMO = other.IsSingleThreadedWMO;
    return *this;
  }

  // Single-threaded WMO routines:

  //  SingleThreadedWMO mode needs only one of each output file for the entire
  //  invocation. WMO can get away with that because it doesn't even attempt to
  //  be incremental, and so it doesn't need per-file intermediates that
  //  wouldn't be generated otherwise.
  //
  //    (A few of the outputs might make more sense to be generated for every
  //    input—.d files in particular—but it wasn't natural when passing them on
  //    the command line, and it hasn't been critical. So right now there's only
  //    one of everything in WMO, always, except for the actual object files in
  //    threaded mode.)
  const InputFile *getSingleThreadedWMOInput() const {
    return isSingleThreadedWMO() ? &firstInput() : nullptr;
  }
  InputFile *getSingleThreadedWMOInput() {
    return isSingleThreadedWMO() ? &firstInput() : nullptr;
  }

  void setIsSingleThreadedWMO(bool istw) { IsSingleThreadedWMO = istw; }
  bool isSingleThreadedWMO() const { return IsSingleThreadedWMO; }

  // Readers:

  ArrayRef<InputFile> getAllFiles() const { return AllFiles; }
  std::vector<InputFile> &getAllFiles() { return AllFiles; }

  InputFile &firstPrimaryInput();
  const InputFile &firstPrimaryInput() const;

  std::vector<std::string> getInputFilenames() const;

  unsigned inputCount() const { return getAllFiles().size(); }

  bool hasInputs() const { return !getAllFiles().empty(); }

  bool hasSingleInput() const { return inputCount() == 1; }

  const InputFile &firstInput() const { return getAllFiles()[0]; }
  InputFile &firstInput() { return getAllFiles()[0]; }

  StringRef getFilenameOfFirstInput() const;

  bool isReadingFromStdin() const {
    return hasSingleInput() && getFilenameOfFirstInput() == "-";
  }

  // If we have exactly one input filename, and its extension is "bc" or "ll",
  // treat the input as LLVM_IR.
  bool shouldTreatAsLLVM() const;

  // Primary input readers

  bool areAllNonPrimariesSIB() const;

public:
  unsigned countOfFilesProducingOutput() const;

  const InputFile &firstInputProducingOutput() const;

  bool forEachInputProducingOutput(
      llvm::function_ref<bool(const InputFile &, unsigned)> fn) const;

  bool
  forEachInput(llvm::function_ref<bool(const InputFile &, unsigned)> fn) const;

  bool forEachPrimaryInput(
      llvm::function_ref<bool(const InputFile &, unsigned)> fn) const;

  InputFile &firstInputProducingOutput();

  bool forEachInputProducingOutput(
      llvm::function_ref<bool(InputFile &, unsigned)> fn);
  bool forEachInput(llvm::function_ref<bool(InputFile &, unsigned)> fn);

  bool
  forEachPrimaryInput(llvm::function_ref<bool(InputFile &input, unsigned)> fn);

  unsigned primaryInputCount() const { return PrimaryInputs.size(); }

  // Primary count readers:

  bool hasUniquePrimaryInput() const { return primaryInputCount() == 1; }

  bool hasPrimaryInputs() const { return primaryInputCount() > 0; }

  bool isWholeModule() const { return !hasPrimaryInputs(); }

  // Count-dependend readers:

  /// Return the unique primary input, if one exists.
  const InputFile *getUniquePrimaryInput() const;

  const InputFile &getRequiredUniquePrimaryInput() const;

  /// Return the name of the unique primary input, or an empty StringRef if
  /// there isn't one.
  StringRef getNameOfUniquePrimaryInputFile() const;

  bool isFilePrimary(StringRef file);

  unsigned numberOfPrimaryInputsEndingWith(const char *extension) const;

  // Multi-facet readers

  bool shouldTreatAsSIL() const;

  /// \return true for error
  bool verifyInputs(DiagnosticEngine &diags, bool treatAsSIL,
                    bool isREPLRequested, bool isNoneRequested) const;

  // Writers

  void addInputFile(StringRef file, llvm::MemoryBuffer *buffer = nullptr);
  void addPrimaryInputFile(StringRef file,
                           llvm::MemoryBuffer *buffer = nullptr);

  void addInput(const InputFile &input);

  void clearInputs();

  // FIXME: dmu fix uses / remove these when batch mode works
  void assertMustNotBeMoreThanOnePrimaryInput() const;
};

} // namespace swift

#endif /* SWIFT_FRONTEND_FRONTENDINPUTS_H */
