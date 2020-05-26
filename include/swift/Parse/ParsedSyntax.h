//===--- ParsedSyntax.h - Base class for ParsedSyntax hierarchy -*- C++ -*-===//
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

#ifndef SWIFT_PARSE_PARSEDSYNTAX_H
#define SWIFT_PARSE_PARSEDSYNTAX_H

#include "swift/Parse/ParsedRawSyntaxNode.h"
#include "swift/Syntax/SyntaxKind.h"

namespace swift {

class ParsedSyntax {
  ParsedRawSyntaxNode RawNode;

public:
  explicit ParsedSyntax(ParsedRawSyntaxNode rawNode)
    : RawNode(std::move(rawNode)) {}

  const ParsedRawSyntaxNode &getRaw() const { return RawNode; }
  ParsedRawSyntaxNode takeRaw() { return std::move(RawNode); }
  syntax::SyntaxKind getKind() const { return RawNode.getKind(); }

  /// Returns true if the syntax node is of the given type.
  template <typename T>
  bool is() const {
    return T::classof(this);
  }

  /// Cast this Syntax node to a more specific type, asserting it's of the
  /// right kind.
  template <typename T>
  T castTo() const {
    assert(is<T>() && "castTo<T>() node of incompatible type!");
    return T { RawNode.copyDeferred() };
  }

  /// If this Syntax node is of the right kind, cast and return it,
  /// otherwise return None.
  template <typename T>
  llvm::Optional<T> getAs() const {
    if (is<T>()) {
      return castTo<T>();
    }
    return llvm::None;
  }

  ParsedSyntax copyDeferred() const {
    return ParsedSyntax { RawNode.copyDeferred() };
  }

  static bool kindof(syntax::SyntaxKind Kind) {
    return true;
  }

  static bool classof(const ParsedSyntax *S) {
    // Trivially true.
    return true;
  }
};

class ParsedTokenSyntax final : public ParsedSyntax {
public:
  explicit ParsedTokenSyntax(ParsedRawSyntaxNode rawNode)
    : ParsedSyntax(std::move(rawNode)) {}

  tok getTokenKind() const {
    return getRaw().getTokenKind();
  }

  static bool kindof(syntax::SyntaxKind Kind) {
    return isTokenKind(Kind);
  }

  static bool classof(const ParsedSyntax *S) {
    return kindof(S->getKind());
  }
};

/// A generic unbounded collection of syntax nodes
template <syntax::SyntaxKind CollectionKind>
class ParsedSyntaxCollection : public ParsedSyntax {

public:
  explicit ParsedSyntaxCollection(ParsedRawSyntaxNode rawNode)
    : ParsedSyntax(std::move(rawNode)) {}

  static bool kindof(syntax::SyntaxKind Kind) {
    return Kind == CollectionKind;
  }

  static bool classof(const ParsedSyntax *S) {
    return kindof(S->getKind());
  }
};

} // end namespace swift

#endif
