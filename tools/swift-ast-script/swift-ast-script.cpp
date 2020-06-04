//===--- swift-ast-script.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This utility is a command line tool that searches Swift code for
/// declarations matching the given requirements.
///
//===----------------------------------------------------------------------===//

#include "ASTScript.h"
#include "ASTScriptConfiguration.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/FrontendTool/FrontendTool.h"

using namespace swift;
using namespace scripting;

// ISO C++ does not allow 'main' to be used by a program [-Wmain]
int main2(int argc, const char *argv[]) {
  PROGRAM_START(argc, argv);
  INITIALIZE_LLVM();

  // Look for the first "--" in the arguments.
  auto argBegin = argv + 1;
  auto argEnd = argv + argc;
  auto dashDash = std::find(argBegin, argEnd, StringRef("--"));
  if (dashDash == argEnd) {
    llvm::errs() << "error: missing '--' in arguments to separate "
                    "script configuration from compiler arguments\n"
                    "usage:\n"
                    "  swift-grep <script_file> -- <compiler flags>\n";
    return 1;
  }

  PrintingDiagnosticConsumer PDC;
  CompilerInstance CI;
  CI.addDiagnosticConsumer(&PDC);

  CompilerInvocation Invocation;
  Invocation.setMainExecutablePath(llvm::sys::fs::getMainExecutable(
      argv[0], reinterpret_cast<void *>(&main2)));

  // Set up the frontend arguments.
  SmallVector<const char *, 8> frontendArgs;
  frontendArgs.append(dashDash + 1, argEnd);
  Invocation.parseArgs(frontendArgs, CI.getDiags());

  if (CI.setup(Invocation))
    return 1;

  auto configArgs = llvm::makeArrayRef(argBegin, dashDash);
  auto config = ASTScriptConfiguration::parse(CI, configArgs);
  if (!config)
    return 1;

  auto script = ASTScript::parse(*config);
  if (!script)
    return 1;

  if (CI.loadStdlibIfNeeded())
    return 1;

  CI.performSema();

  if (CI.getASTContext().hadError())
    return 1;

  if (script->execute())
    return 1;

  return 0;
}

int main(int argc, const char *argv[]) {
  return main2(argc, argv);
}
