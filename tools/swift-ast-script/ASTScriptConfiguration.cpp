//===--- ASTScriptConfiguration.cpp ---------------------------------------===//
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
/// Configuration parsing for an AST script.
///
//===----------------------------------------------------------------------===//

#include "ASTScriptConfiguration.h"

#include "swift/Basic/QuotedString.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace scripting;

std::unique_ptr<ASTScriptConfiguration>
ASTScriptConfiguration::parse(CompilerInstance &compiler,
                              ArrayRef<const char *> args) {
  bool hadError = false;

  std::unique_ptr<ASTScriptConfiguration> result(
    new ASTScriptConfiguration(compiler));

#define emitError(WHAT) do {                                                  \
    llvm::errs() << "error: " << WHAT << "\n";                                \
    hadError = true;                                                          \
  } while (0)

  auto popArg = [&](const char *what) -> StringRef {
    // Gracefully handle the case of running out of arguments.
    if (args.empty()) {
      assert(what && "expected explanation here!");
      emitError(what);
      return "";
    }

    auto arg = args.front();
    args = args.slice(1);
    return arg;
  };

  auto setScriptFile = [&](StringRef filename) {
    if (!result->ScriptFile.empty()) {
      emitError("multiple script files ("
                  << QuotedString(result->ScriptFile)
                  << ", "
                  << QuotedString(filename)
                  << ")");
    }
    result->ScriptFile = filename;
  };

  // Parse the arguments.
  while (!hadError && !args.empty()) {
    StringRef arg = popArg(nullptr);
    if (!arg.startswith("-")) {
      setScriptFile(arg);
    } else if (arg == "-f") {
      StringRef filename = popArg("expected path after -f");
      if (!hadError)
        setScriptFile(filename);
    } else if (arg == "-h" || arg == "--help") {
      llvm::errs()
        << "usage: swift-ast-script <script-args> -- <compiler-args\n"
           "accepted script arguments:\n"
           "  <filename>\n"
           "  -f <filename>\n"
           "    Specify the file to use as the script; required argument\n";
      hadError = true;
    } else {
      emitError("unknown argument " << QuotedString(arg));
    }
  }

  // Check well-formedness.
  if (!hadError) {
    if (result->ScriptFile.empty()) {
      emitError("script file is required");
    }
  }

  if (hadError)
    result.reset();
  return result;
}
