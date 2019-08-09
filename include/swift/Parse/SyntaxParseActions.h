//===--- SyntaxParseActions.h - Syntax Parsing Actions ----------*- C++ -*-===//
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
//  This file defines the interface between the parser and a receiver of
//  raw syntax nodes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_SYNTAXPARSEACTIONS_H
#define SWIFT_PARSE_SYNTAXPARSEACTIONS_H

#include "swift/Basic/LLVM.h"

namespace swift {

class CharSourceRange;
class ParsedTriviaPiece;
class SourceLoc;
enum class tok;

namespace syntax {
  enum class SyntaxKind;
}

typedef void *OpaqueSyntaxNode;

// todo [gsoc]: remove when possible
enum class OpaqueSyntaxNodeKind {
  SwiftSyntax,
  LibSyntax,
};

class SyntaxParseActions {
  virtual void _anchor();

public:
  virtual ~SyntaxParseActions() = default;

  virtual OpaqueSyntaxNode recordToken(tok tokenKind,
                                    ArrayRef<ParsedTriviaPiece> leadingTrivia,
                                    ArrayRef<ParsedTriviaPiece> trailingTrivia,
                                    CharSourceRange range) = 0;

  /// Record a missing token. \c loc can be invalid or an approximate location
  /// of where the token would be if not missing.
  virtual OpaqueSyntaxNode recordMissingToken(tok tokenKind, SourceLoc loc) = 0;

  /// The provided \c elements are an exact layout appropriate for the syntax
  /// \c kind. Missing optional elements are represented with a null
  /// OpaqueSyntaxNode object.
  virtual OpaqueSyntaxNode recordRawSyntax(syntax::SyntaxKind kind,
                                           ArrayRef<OpaqueSyntaxNode> elements,
                                           CharSourceRange range) = 0;

  /// Used for incremental re-parsing.
  virtual std::pair<size_t, OpaqueSyntaxNode>
  lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) {
    return std::make_pair(0, nullptr);
  }

  /// Returns what kind of OpaqueSyntaxNode is created by `recordXXX` methods.
  virtual OpaqueSyntaxNodeKind getOpaqueKind() = 0;
};

} // end namespace swift

#endif
