//===--- Job.h - Commands to Execute ----------------------------*- C++ -*-===//
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

#ifndef SWIFT_DRIVER_JOB_H
#define SWIFT_DRIVER_JOB_H

#include "swift/Basic/LLVM.h"
#include "swift/Driver/Types.h"
#include "swift/Driver/Util.h"
#include "llvm/Option/Option.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/TimeValue.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>

namespace swift {
namespace driver {

class Action;
class InputAction;
class Job;
class Tool;

class CommandOutput {
  types::ID PrimaryOutputType;
  
  /// The primary output files of the command.
  /// Usually a command has only a single output file. Only the compiler in
  /// multi-threaded compilation produces multiple output files.
  SmallVector<std::string, 1> PrimaryOutputFilenames;

  /// For each primary output file there is a base input. This is the input file
  /// from which the output file is derived.
  SmallVector<StringRef, 1> BaseInputs;

  llvm::SmallDenseMap<types::ID, std::string, 4> AdditionalOutputsMap;

public:
  CommandOutput(types::ID PrimaryOutputType)
      : PrimaryOutputType(PrimaryOutputType) { }

  types::ID getPrimaryOutputType() const { return PrimaryOutputType; }

  void addPrimaryOutput(StringRef FileName, StringRef BaseInput) {
    PrimaryOutputFilenames.push_back(FileName);
    BaseInputs.push_back(BaseInput);
  }
  
  // This returns a std::string instead of a StringRef so that users can rely
  // on the data buffer being null-terminated.
  const std::string &getPrimaryOutputFilename() const {
    assert(PrimaryOutputFilenames.size() == 1);
    return PrimaryOutputFilenames[0];
  }

  ArrayRef<std::string> getPrimaryOutputFilenames() const {
    return PrimaryOutputFilenames;
  }
  
  void setAdditionalOutputForType(types::ID type, StringRef OutputFilename);
  const std::string &getAdditionalOutputForType(types::ID type) const;

  const std::string &getAnyOutputForType(types::ID type) const;

  StringRef getBaseInput(int Index) const { return BaseInputs[Index]; }
};

class Job {
public:
  enum class Condition {
    Always,
    RunWithoutCascading,
    CheckDependencies,
    NewlyAdded
  };

private:
  /// The action which caused the creation of this Job.
  const Action &Source;

  /// The tool which created this Job, and the conditions under which it must
  /// be run.
  llvm::PointerIntPair<const Tool *, 2, Condition> CreatorAndCondition;

  /// The list of other Jobs which are inputs to this Job.
  SmallVector<const Job *, 4> Inputs;

  /// The output of this command.
  std::unique_ptr<CommandOutput> Output;

  /// The executable to run.
  const char *Executable;

  /// The list of program arguments (not including the implicit first argument,
  /// which will be the Executable).
  llvm::opt::ArgStringList Arguments;

  /// The modification time of the main input file, if any.
  llvm::sys::TimeValue InputModTime = llvm::sys::TimeValue::MaxTime();

public:
  Job(const Action &Source, const Tool &Creator,
      SmallVectorImpl<const Job *> &&Inputs,
      std::unique_ptr<CommandOutput> Output,
      const char *Executable,
      llvm::opt::ArgStringList Arguments)
      : Source(Source), CreatorAndCondition(&Creator, Condition::Always),
        Inputs(std::move(Inputs)), Output(std::move(Output)),
        Executable(Executable), Arguments(std::move(Arguments)) {}

  const Action &getSource() const { return Source; }
  const Tool &getCreator() const { return *CreatorAndCondition.getPointer(); }

  const char *getExecutable() const { return Executable; }
  const llvm::opt::ArgStringList &getArguments() const { return Arguments; }

  ArrayRef<const Job *> getInputs() const { return Inputs; }
  const CommandOutput &getOutput() const { return *Output; }

  Condition getCondition() const {
    return CreatorAndCondition.getInt();
  }
  void setCondition(Condition Cond) {
    CreatorAndCondition.setInt(Cond);
  }

  void setInputModTime(llvm::sys::TimeValue time) {
    InputModTime = time;
  }

  llvm::sys::TimeValue getInputModTime() const {
    return InputModTime;
  }

  /// Print the command line for this Job to the given \p stream,
  /// terminating output with the given \p terminator.
  void printCommandLine(raw_ostream &Stream, StringRef Terminator = "\n") const;

  void dump() const LLVM_ATTRIBUTE_USED;

  static void printArguments(raw_ostream &Stream,
                             const llvm::opt::ArgStringList &Args);
};

} // end namespace driver
} // end namespace swift

#endif
