//===--- Format.cpp - Declaration Syntax Formatting Interface ---*- C++ -*-===//
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
//
// This file defines the interface for textually formatting Swift Syntax nodes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_FORMAT_H
#define SWIFT_SYNTAX_FORMAT_H

#include "swift/Syntax/Rewriter.h"

namespace swift {
namespace syntax {

/// A SyntaxRewriter for applying a set of formatting rules to a Syntax tree.
struct FormatSyntaxRewriter : public SyntaxRewriter {
  virtual StructDeclSyntax
  rewriteStructDecl(StructDeclSyntax Struct) override;
};

/// Format a Syntax tree with the given rules.
Syntax format(Syntax Tree);
// TODO: Represenation for formatting rules, etc. This is just a figment
// for now.

}
}
#endif
