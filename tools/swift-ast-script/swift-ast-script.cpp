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

#include "swift/Frontend/Frontend.h"
#include "swift/FrontendTool/FrontendTool.h"
#include "swift/Basic/LLVMInitialize.h"
#include "ASTScript.h"
#include "ASTScriptConfiguration.h"

using namespace swift;
using namespace scripting;

namespace {

class Observer : public FrontendObserver {
  ArrayRef<const char *> Args;
  std::unique_ptr<ASTScriptConfiguration> Config;
  std::unique_ptr<ASTScript> Script;
  bool HadError = false;

public:
  Observer(ArrayRef<const char *> args) : Args(args) {}

  void configuredCompiler(CompilerInstance &instance) override {
    Config = ASTScriptConfiguration::parse(instance, Args);
    if (!Config) return flagError();

    Script = ASTScript::parse(*Config);
    if (!Script) return flagError();
  }

  void performedSemanticAnalysis(CompilerInstance &instance) override {
    if (Script) {
      if (Script->execute())
        return flagError();
    }
  }

  bool hadError() const {
    return HadError;
  }

private:
  void flagError() {
    HadError = true;
  }
};

}

// ISO C++ does not allow 'main' to be used by a program [-Wmain]
int main2(int argc, const char *argv[]) {
  PROGRAM_START(argc, argv);

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

  Observer observer(llvm::ArrayRef(argBegin, dashDash));

  // Set up the frontend arguments.
  unsigned numFrontendArgs = argEnd - (dashDash + 1);
  SmallVector<const char *, 8> frontendArgs;
  frontendArgs.reserve(numFrontendArgs + 1);
  frontendArgs.append(dashDash + 1, argEnd);
  frontendArgs.push_back("-typecheck");

  int frontendResult =
    performFrontend(frontendArgs, argv[0], (void*) &main2, &observer);

  return (observer.hadError() ? 1 : frontendResult);
}

int main(int argc, const char *argv[]) {
  return main2(argc, argv);
}
