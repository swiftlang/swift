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

void JobList::clear() {
  if (OwnsJobs) {
    llvm::DeleteContainerPointers(Jobs);
  } else {
    // We don't own the pointers, so just clear the list.
    Jobs.clear();
  }
}

void CommandOutput::setAdditionalOutputForType(types::ID type,
                                               StringRef OutputFilename) {
  AdditionalOutputsMap[type] = OutputFilename;
}

const std::string &
CommandOutput::getAdditionalOutputForType(types::ID type) const {
  auto iter = AdditionalOutputsMap.find(type);
  if (iter != AdditionalOutputsMap.end())
    return iter->second;

  static const std::string empty;
  return empty;
}

const std::string &
CommandOutput::getAnyOutputForType(types::ID type) const {
  if (PrimaryOutputType == type)
    return PrimaryOutputFilename;
  return getAdditionalOutputForType(type);
}

void Command::printCommandLine(llvm::raw_ostream &os) const {
  os << Executable;
  for (const char *Arg : Arguments) {
    llvm::outs() << ' ' << Arg;
  }
  os << '\n';
}
