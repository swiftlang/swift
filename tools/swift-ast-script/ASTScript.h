//===--- ASTScript.h - AST script type --------------------------*- C++ -*-===//
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
// The AST for a swift-ast-script script.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SCRIPTING_ASTSCRIPT_H
#define SWIFT_SCRIPTING_ASTSCRIPT_H

#include "swift/Basic/LLVM.h"

namespace swift {
namespace scripting {
class ASTScriptConfiguration;

class ASTScript {
  ASTScriptConfiguration &Config;

public:
  ASTScript(ASTScriptConfiguration &config) : Config(config) {}

  static std::unique_ptr<ASTScript> parse(ASTScriptConfiguration &config);

  bool execute() const;
};

}
}

#endif
