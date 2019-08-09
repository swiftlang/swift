//===--- ASTScriptConfiguration.h - AST script configuration ----*- C++ -*-===//
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
//
// Types for configuring an AST script invocation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SCRIPTING_ASTSCRIPTCONFIGURATION_H
#define SWIFT_SCRIPTING_ASTSCRIPTCONFIGURATION_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
class CompilerInstance;

namespace scripting {

/// A configuration for working with an ASTScript.
class ASTScriptConfiguration {
  ASTScriptConfiguration(CompilerInstance &compiler) : Compiler(compiler) {}
public:
  CompilerInstance &Compiler;
  StringRef ScriptFile;

  /// Attempt to parse this configuration.
  ///
  /// Returns null if there's a problem.
  static std::unique_ptr<ASTScriptConfiguration>
  parse(CompilerInstance &compiler, ArrayRef<const char *> args);
};

}
}

#endif
