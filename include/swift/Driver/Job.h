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
#include "swift/Basic/Optional.h"
#include "swift/Driver/Types.h"
#include "swift/Driver/Util.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Option/Option.h"

#include <memory>

namespace swift {
namespace driver {
  class Action;
  class InputAction;
  class Tool;

class Job {
public:
  enum JobClass {
    CommandClass,
    JobListClass
  };

private:
  JobClass Kind;

protected:
  Job(JobClass Kind) : Kind(Kind) {}
public:
  virtual ~Job() = default;

  JobClass getKind() const { return Kind; }
};

class JobList : public Job {
  typedef SmallVector<Job *, 4> list_type;
    
public:
  typedef list_type::size_type size_type;
  typedef list_type::iterator iterator;
  typedef list_type::const_iterator const_iterator;

private:
  list_type Jobs;
  bool OwnsJobs;

public:
  JobList() : Job(JobListClass), OwnsJobs(true) {}
  virtual ~JobList();

  bool getOwnsJobs() const { return OwnsJobs; }
  void setOwnsJobs(bool Value) { OwnsJobs = Value; }

  void addJob(Job *J) { Jobs.push_back(J); }

  void clear();

  ArrayRef<Job *> getJobs() const { return Jobs; }

  size_type size() const { return Jobs.size(); }
  bool empty() const { return Jobs.empty(); }
  iterator begin() { return Jobs.begin(); }
  const_iterator begin() const { return Jobs.begin(); }
  iterator end() { return Jobs.end(); }
  const_iterator end() const { return Jobs.end(); }

  static bool classof(const Job *J) {
    return J->getKind() == JobListClass;
  }
};

class CommandOutput {
  types::ID PrimaryOutputType;
  std::string PrimaryOutputFilename;

  llvm::SmallDenseMap<types::ID, std::string, 4> AdditionalOutputsMap;

  StringRef BaseInput;

public:
  CommandOutput(StringRef BaseInput)
      : CommandOutput(types::ID::TY_Nothing, StringRef(), BaseInput) {}

  CommandOutput(types::ID PrimaryOutputType, StringRef PrimaryOutputFilename,
                StringRef BaseInput)
    : PrimaryOutputType(PrimaryOutputType),
      PrimaryOutputFilename(PrimaryOutputFilename), BaseInput(BaseInput) {}

  types::ID getPrimaryOutputType() const { return PrimaryOutputType; }

  // This returns a std::string instead of a StringRef so that users can rely
  // on the data buffer being null-terminated.
  const std::string &getPrimaryOutputFilename() const {
    return PrimaryOutputFilename;
  }

  void setAdditionalOutputForType(types::ID type, StringRef OutputFilename);
  const std::string &getAdditionalOutputForType(types::ID type) const;

  const std::string &getAnyOutputForType(types::ID type) const;

  StringRef getBaseInput() const { return BaseInput; }
};

class Command : public Job {
  /// The action which caused the creation of this Job.
  const Action &Source;

  /// The tool which created this Job.
  const Tool &Creator;

  /// The list of other Jobs which are inputs to this Job.
  std::unique_ptr<JobList> Inputs;

  /// The output of this command.
  std::unique_ptr<CommandOutput> Output;

  /// The executable to run.
  const char *Executable;

  /// The list of program arguments (not including the implicit first argument,
  /// which will be the Executable).
  llvm::opt::ArgStringList Arguments;

public:
  Command(const Action &Source, const Tool &Creator,
          std::unique_ptr<JobList> Inputs,
          std::unique_ptr<CommandOutput> Output, const char *Executable,
          llvm::opt::ArgStringList &Arguments)
      : Job(CommandClass), Source(Source), Creator(Creator),
        Inputs(std::move(Inputs)), Output(std::move(Output)),
        Executable(Executable), Arguments(Arguments) {}
  virtual ~Command() = default;

  const Action &getSource() const { return Source; }
  const Tool &getCreator() const { return Creator; }

  const char *getExecutable() const { return Executable; }
  const llvm::opt::ArgStringList &getArguments() const { return Arguments; }

  const JobList &getInputs() const { return *Inputs; }
  const CommandOutput &getOutput() const { return *Output; }

  /// Print the command line for this Command to the given \p stream.
  void printCommandLine(llvm::raw_ostream &Stream) const;

  static bool classof(const Job *J) {
    return J->getKind() == CommandClass;
  }
};

} // end namespace driver
} // end namespace swift

#endif
