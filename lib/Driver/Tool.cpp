//===--- Tool.cpp - Compilation Tools -------------------------------------===//
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

#include "swift/Driver/Tool.h"
#include "swift/Driver/Action.h"
#include "swift/Driver/Job.h"
#include "llvm/ADT/STLExtras.h"

using namespace swift;
using namespace swift::driver;

std::unique_ptr<Job>
Tool::constructJob(const JobAction &JA,
                   SmallVectorImpl<const Job *> &&inputs,
                   std::unique_ptr<CommandOutput> output,
                   const ActionList &inputActions,
                   const llvm::opt::ArgList &args,
                   const OutputInfo &OI) const {
  auto arguments = constructArgumentList(JA, inputs, output.get(),
                                         inputActions, args, OI);
  return llvm::make_unique<Job>(JA, *this, std::move(inputs), std::move(output),
                                getPath(args, OI), std::move(arguments));
}
