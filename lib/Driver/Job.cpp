//===--- Job.cpp - Command to Execute -------------------------------------===//
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

#include "swift/Driver/Job.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::driver;

JobList::~JobList() {
  if (OwnsJobs) {
    llvm::DeleteContainerPointers(Jobs);
  }
}

bool JobList::needsToRun() {
  for (Job *J : *this) {
    if (J->needsToRun()) {
      return true;
    }
  }

  // None of the jobs in this list need to run, so the list doesn't need to run.
  return false;
}

int JobList::run() {
  int result = 0;
  for (Job *J : *this) {
    if (J->needsToRun() && ((result = J->run()) != 0)) {
      return result;
    }
  }
  return result;
}

void JobList::clear() {
  if (OwnsJobs) {
    llvm::DeleteContainerPointers(Jobs);
  } else {
    // We don't own the pointers, so just clear the list.
    Jobs.clear();
  }
}

bool Command::needsToRun() {
  if (Inputs->empty()) {
    // If a command has no inputs, it must be run.
    // TODO: refine this check, though possibly only in subclasses of Command.
    return true;
  }

  // If this command has inputs, then we fall back to whether or not they need
  // to run.
  return Inputs->needsToRun();
}

int Command::run() {
  int result = 0;
  for (Job *J : *Inputs) {
    if (J->needsToRun()) {
      result = J->run();
      if (result != 0) {
        // This Job failed (returned a non-zero result code), so stop here.
        return result;
      }
    }
  }

  return execute(nullptr, nullptr, nullptr);
}

int Command::execute(const StringRef **Redirects, std::string *ErrMsg,
                     bool *ExecutionFailed) const {
  // TODO: this needs to be replaced with something which we can dispatch
  // multiple at a time.
  
  llvm::errs() << Executable << ' ';
  for (const char *Arg : Arguments) {
    llvm::errs() << Arg << ' ';
  }
  llvm::errs() << '\n';

  SmallVector<const char *, 128> Argv;
  Argv.push_back(Executable);
  Argv.append(Arguments.begin(), Arguments.end());
  Argv.push_back(0);

  return llvm::sys::ExecuteAndWait(Executable, Argv.data(), 0,
                                   Redirects, 0, 0, ErrMsg, ExecutionFailed);
}
