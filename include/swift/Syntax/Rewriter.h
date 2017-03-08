//===--- Rewriter.h - Swift Syntax Rewriter Interface -----------*- C++ -*-===//
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
// This file defines the interface for Swift Syntax rewriters: classes that
// provide replacements for certain kinds of Syntax nodes that meet the
// specific rewriter's criteria.
//
// Use these for heavyweight, widespread, or complex Syntax
// tree transformations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_REWRITER_H
#define SWIFT_SYNTAX_REWRITER_H

#include "swift/Syntax/References.h"
#include "swift/Syntax/DeclSyntax.h"
#include "swift/Syntax/GenericSyntax.h"
#include "swift/Syntax/ExprSyntax.h"
#include "swift/Syntax/StmtSyntax.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/UnknownSyntax.h"

namespace swift {
namespace syntax {

/// The base class for creating Swift Syntax rewriters, for more complex
/// or formal syntactic transformations.
struct SyntaxRewriter {
#define SYNTAX(Id, Parent) \
  virtual Id##Syntax rewrite##Id(Id##Syntax Node) { \
    return Node; \
  }
#include "swift/Syntax/SyntaxKinds.def"
  virtual ~SyntaxRewriter() = default;
};

}
}

#endif
