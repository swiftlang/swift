//===--- Compilation.cpp - Compilation Task Data Structure ----------------===//
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

#include "swift/Driver/Compilation.h"

#include "swift/Driver/Job.h"
#include "llvm/Option/ArgList.h"

using namespace swift;
using namespace swift::driver;
using namespace llvm::opt;

Compilation::Compilation(const Driver &D, const ToolChain &DefaultToolChain,
                         std::unique_ptr<InputArgList> InputArgs,
                         std::unique_ptr<DerivedArgList> TranslatedArgs,
                         unsigned NumberOfParallelCommands)
  : TheDriver(D), DefaultToolChain(DefaultToolChain), Jobs(new JobList),
    InputArgs(std::move(InputArgs)), TranslatedArgs(std::move(TranslatedArgs)),
    NumberOfParallelCommands(NumberOfParallelCommands) {
};

Compilation::~Compilation() = default;

void Compilation::addJob(Job *J) {
  Jobs->addJob(J);
}

int Compilation::performJobs() {
  return Jobs->run();
}
