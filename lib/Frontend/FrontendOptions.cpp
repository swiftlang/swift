//===--- FrontendOptions.cpp ----------------------------------------------===//
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

#include "swift/Frontend/FrontendOptions.h"

#include "llvm/Support/ErrorHandling.h"

using namespace swift;

bool FrontendOptions::actionHasOutput() const {
  switch (RequestedAction) {
  case NoneAction:
  case Parse:
  case DumpParse:
  case DumpAST:
  case PrintAST:
    return false;
  case EmitSILGen:
  case EmitSIL:
  case EmitSIBGen:
  case EmitSIB:
  case EmitModuleOnly:
    return true;
  case Immediate:
  case REPL:
    return false;
  case EmitAssembly:
  case EmitIR:
  case EmitBC:
  case EmitObject:
    return true;
  }
  llvm_unreachable("Unknown ActionType");
}

bool FrontendOptions::actionIsImmediate() const {
  switch (RequestedAction) {
  case NoneAction:
  case Parse:
  case DumpParse:
  case DumpAST:
  case PrintAST:
  case EmitSILGen:
  case EmitSIL:
  case EmitSIBGen:
  case EmitSIB:
  case EmitModuleOnly:
    return false;
  case Immediate:
  case REPL:
    return true;
  case EmitAssembly:
  case EmitIR:
  case EmitBC:
  case EmitObject:
    return false;
  }
  llvm_unreachable("Unknown ActionType");
}

void FrontendOptions::forAllOutputPaths(
    std::function<void(const std::string &)> fn) const {
  if (RequestedAction != FrontendOptions::EmitModuleOnly) {
    for (const std::string &OutputFileName : OutputFilenames) {
      fn(OutputFileName);
    }
  }
  const std::string *outputs[] = {
    &ModuleOutputPath,
    &ModuleDocOutputPath,
    &ObjCHeaderOutputPath
  };
  for (const std::string *next : outputs) {
    if (!next->empty())
      fn(*next);
  }
}
