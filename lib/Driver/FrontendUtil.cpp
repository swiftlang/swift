//===--- FrontendUtil.cpp - Driver Utilities for Frontend -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Driver/FrontendUtil.h"

#include "swift/AST/DiagnosticsDriver.h"
#include "swift/Driver/Action.h"
#include "swift/Driver/Compilation.h"
#include "swift/Driver/Driver.h"
#include "swift/Driver/Job.h"
#include "swift/Driver/ToolChain.h"
#include "llvm/Option/ArgList.h"

using namespace swift;
using namespace swift::driver;

bool swift::driver::getSingleFrontendInvocationFromDriverArguments(
    ArrayRef<const char *> Argv, DiagnosticEngine &Diags,
    llvm::function_ref<bool(ArrayRef<const char *> FrontendArgs)> Action) {
  SmallVector<const char *, 16> Args;
  Args.push_back("<swiftc>"); // FIXME: Remove dummy argument.
  Args.insert(Args.end(), Argv.begin(), Argv.end());

  // When creating a CompilerInvocation, ensure that the driver creates a single
  // frontend command.
  Args.push_back("-force-single-frontend-invocation");

  // Explictly disable batch mode to avoid a spurious warning when combining
  // -enable-batch-mode with -force-single-frontend-invocation.  This is an
  // implementation detail.
  Args.push_back("-disable-batch-mode");

  // Avoid using filelists
  std::string neverThreshold =
      std::to_string(Compilation::NEVER_USE_FILELIST);
  Args.push_back("-driver-filelist-threshold");
  Args.push_back(neverThreshold.c_str());

  // Force the driver into batch mode by specifying "swiftc" as the name.
  Driver TheDriver("swiftc", "swiftc", Args, Diags);

  // Don't check for the existence of input files, since the user of the
  // CompilerInvocation may wish to remap inputs to source buffers.
  TheDriver.setCheckInputFilesExist(false);

  std::unique_ptr<llvm::opt::InputArgList> ArgList =
    TheDriver.parseArgStrings(ArrayRef<const char *>(Args).slice(1));
  if (Diags.hadAnyError())
    return true;

  std::unique_ptr<ToolChain> TC = TheDriver.buildToolChain(*ArgList);
  if (Diags.hadAnyError())
    return true;

  std::unique_ptr<Compilation> C =
      TheDriver.buildCompilation(*TC, std::move(ArgList));
  if (!C || C->getJobs().empty())
    return true; // Don't emit an error; one should already have been emitted

  SmallPtrSet<const Job *, 4> CompileCommands;
  for (const Job *Cmd : C->getJobs())
    if (isa<CompileJobAction>(Cmd->getSource()))
      CompileCommands.insert(Cmd);

  if (CompileCommands.size() != 1) {
    // TODO: include Jobs in the diagnostic.
    Diags.diagnose(SourceLoc(), diag::error_expected_one_frontend_job);
    return true;
  }

  const Job *Cmd = *CompileCommands.begin();
  if (StringRef("-frontend") != Cmd->getArguments().front()) {
    Diags.diagnose(SourceLoc(), diag::error_expected_frontend_command);
    return true;
  }

  const llvm::opt::ArgStringList &BaseFrontendArgs = Cmd->getArguments();
  return Action(llvm::makeArrayRef(BaseFrontendArgs).drop_front());
}
