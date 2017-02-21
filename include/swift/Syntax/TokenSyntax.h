//===--- TokenSyntax.h - Swift Token Interface ------------------*- C++ -*-===//
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
// This file contains the interface for a `TokenSyntax`, which is a token that
// includes full-fidelity leading and trailing trivia.
//
// A TokenSyntax is an instance of `RawSyntax`, meaning it is immutable,
// reference counted, and able to be shared.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_TokenSyntax_H
#define SWIFT_SYNTAX_TokenSyntax_H

#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/TokenKinds.h"
#include "swift/Syntax/Trivia.h"

namespace swift {
namespace syntax {

class AbsolutePosition;

struct TokenSyntax final : public RawSyntax {
  friend struct SyntaxFactory;
  const tok TokenKind;
  const OwnedString Text;
  const Trivia LeadingTrivia;
  const Trivia TrailingTrivia;

private:
  TokenSyntax();

  TokenSyntax(tok TokenKind, OwnedString Text, const SourcePresence Presence);

  TokenSyntax(tok TokenKind, OwnedString Text, const SourcePresence Presence,
            const Trivia &LeadingTrivia, const Trivia &TrailingTrivia);

public:
  /// Make a new token.
  static RC<TokenSyntax> make(tok TokenKind, OwnedString Text,
                           const SourcePresence Presence,
                           const Trivia &LeadingTrivia,
                           const Trivia &TrailingTrivia) {
    return RC<TokenSyntax> {
      new TokenSyntax {
        TokenKind, Text, Presence,
        LeadingTrivia, TrailingTrivia
      }
    };
  }

  /// Return a token with the specified kind and text, but marked as missing.
  static RC<TokenSyntax> missingToken(const tok Kind, OwnedString Text) {
    return make(Kind, Text, SourcePresence::Missing, {}, {});
  }

  /// Return a new token like this one, but with the given leading
  /// trivia instead.
  RC<TokenSyntax> withLeadingTrivia(const Trivia &NewLeadingTrivia) const {
    return make(TokenKind, Text, Presence, NewLeadingTrivia, TrailingTrivia);
  }

  /// Return a new token like this one, but with the given trailing
  /// trivia instead.
  RC<TokenSyntax> withTrailingTrivia(const Trivia &NewTrailingTrivia) const {
    return make(TokenKind, Text, Presence, LeadingTrivia, NewTrailingTrivia);
  }

  /// Returns true if the token is of the expected kind and has the given
  /// expected text.
  bool is(tok ExpectedKind, StringRef ExpectedText) const {
    return getTokenKind() == ExpectedKind && ExpectedText == getText();
  }

  /// Returns the kind of token this is.
  tok getTokenKind() const { return TokenKind; }

  /// Returns a reference to the text of this token.
  StringRef getText() const { return Text.str(); }

  /// True if the token is any keyword.
  bool isKeyword() const {
    switch (TokenKind) {
#define KEYWORD(X) case tok::kw_##X: return true;
#include "swift/Syntax/TokenKinds.def"
      default: return false;
    }
  }

  /// Returns true if the token is any literal.
  bool isLiteral() const {
    switch(TokenKind) {
      case tok::integer_literal:
      case tok::floating_literal:
      case tok::string_literal:
      case tok::pound_fileLiteral:
      case tok::pound_colorLiteral:
      case tok::pound_imageLiteral:
        return true;
      default:
        return false;
    }
  }

  /// Returns true if this token is of the specified kind.
  bool is(tok K) const { return TokenKind == K; }

  /// Returns true if this token is not of the specified kind.
  bool isNot(tok K) const { return TokenKind != K; }

  /// Base case for variadic `isAny`
  bool isAny(tok K1) const {
    return is(K1);
  }

  /// Returns true if this token is any of the provided kinds.
  template <typename ...T>
  bool isAny(tok K1, tok K2, T... K) const {
    if (is(K1))
      return true;
    return isAny(K2, K...);
  }

  /// Returns true if this token is not any of the provided kinds.
  template <typename ...T>
  bool isNot(tok K1, T... K) const { return !isAny(K1, K...); }

  bool isPunctuation() const {
    switch (TokenKind) {
#define PUNCTUATOR(Name, Str) case tok::Name: return true;
#include "swift/Syntax/TokenKinds.def"
      default: return false;
    }
  }

  /// Returns true if this token is a binary operator.
  bool isBinaryOperator() const {
    return TokenKind == tok::oper_binary_spaced ||
           TokenKind == tok::oper_binary_unspaced;
  }

  /// Returns true if this token is any kind of operator.
  bool isOperator() const {
    return isBinaryOperator() ||
    TokenKind == tok::oper_postfix ||
    TokenKind == tok::oper_prefix;
  }

  /// Returns true if this token is not an operator.
  bool isNotOperator() const {
    return !isOperator();
  }

  /// Print the leading trivia, text, and trailing trivia of this token to
  /// the provided output stream.
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const {
    for (const auto &Leader : LeadingTrivia) {
      Leader.print(OS);
    }

    if (!isMissing()) {
      OS << getText();
    }

    for (const auto &Trailer : TrailingTrivia) {
      Trailer.print(OS);
    }
  }

  /// Advance the provided AbsolutePosition by the token's full width and
  /// return the AbsolutePosition of the start of the token's nontrivial text.
  AbsolutePosition accumulateAbsolutePosition(AbsolutePosition &Pos) const;

  /// Dump the textual representation of this token's kind.
  void dumpKind(llvm::raw_ostream &OS) const;

  /// Dump the layout of this token: its leading trivia, kind, text, and
  /// trailing trivia.
  void dump(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  static bool classof(const RawSyntax *RS) {
    return RS->Kind == SyntaxKind::Token;
  }
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_TokenSyntax_H
