//===----------- TokenSyntax.h - Swift Token Interface ----------*- C++ -*-===//
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
// This file contains the interface for a `TokenSyntax`, which is a token
// that includes full-fidelity leading and trailing trivia.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_TOKENSYNTAX_H
#define SWIFT_SYNTAX_TOKENSYNTAX_H

#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/TokenKinds.h"
#include "swift/Syntax/Trivia.h"

namespace swift {
namespace syntax {

class TokenSyntax final : public Syntax {
protected:
  void validate() const {
    assert(getRaw()->isToken());
  }
public:
  TokenSyntax(const RC<const SyntaxData> &Data) : Syntax(Data) {
    validate();
  }

  static TokenSyntax missingToken(const tok Kind, StringRef Text,
                                  const RC<SyntaxArena> &Arena) {
    return makeRoot<TokenSyntax>(RawSyntax::missing(Kind, Text, Arena));
  }

  StringRef getLeadingTrivia() const { return getRaw()->getLeadingTrivia(); }
  Trivia getLeadingTriviaPieces() const { return getRaw()->getLeadingTriviaPieces(); }

  StringRef getTrailingTrivia() const { return getRaw()->getTrailingTrivia(); }
  Trivia getTrailingTriviaPieces() const {
    return getRaw()->getTrailingTriviaPieces();
  }

  TokenSyntax withLeadingTrivia(StringRef Trivia) const {
    auto NewRaw = getRaw()->withLeadingTrivia(Trivia);
    return TokenSyntax(getData()->replacingSelf(NewRaw));
  }

  TokenSyntax withTrailingTrivia(StringRef Trivia) const {
    auto NewRaw = getRaw()->withTrailingTrivia(Trivia);
    return TokenSyntax(getData()->replacingSelf(NewRaw));
  }

  bool isMissing() const {
    return getRaw()->isMissing();
  }

  tok getTokenKind() const {
    return getRaw()->getTokenKind();
  }

  StringRef getText() const {
    return getRaw()->getTokenText();
  }

  StringRef getIdentifierText() const {
    StringRef text = getText();
    if (text.front() == '`') {
      assert(text.back() == '`');
      return text.slice(1, text.size() - 1);
    }
    return text;
  }

  static bool kindof(SyntaxKind Kind) {
    return isTokenKind(Kind);
  }

  static bool classof(const Syntax *S) {
    return kindof(S->getKind());
  }
};

/// See comment in \c SyntaxRef.
/// 
/// This duplicates some code from \c TokenSyntax, but since \c TokenSyntaxRef
/// needs to inherit from \c SyntaxRef and \c TokenSyntax needs to inherit from
/// \c Syntax, there's no easy way to share the code. And the duplicate is
/// fairly small.
class TokenSyntaxRef final : public SyntaxRef {
public:
  TokenSyntaxRef(const SyntaxDataRef *Data) : SyntaxRef(Data) {
    validate();
  }
  TokenSyntaxRef(const SyntaxDataRef *Data, no_validation_t)
      : SyntaxRef(Data) {}

  void validate() const { assert(getRaw()->isToken()); }

  /// Demote a \c TokenSyntax to a \c TokenSyntaxRef
  TokenSyntaxRef(const TokenSyntax &Token) : SyntaxRef(Token.getData().get()) {}

  StringRef getLeadingTrivia() const { return getRaw()->getLeadingTrivia(); }
  Trivia getLeadingTriviaPieces() const {
    return getRaw()->getLeadingTriviaPieces();
  }

  StringRef getTrailingTrivia() const { return getRaw()->getTrailingTrivia(); }
  Trivia getTrailingTriviaPieces() const {
    return getRaw()->getTrailingTriviaPieces();
  }

  bool isMissing() const { return getRaw()->isMissing(); }

  tok getTokenKind() const { return getRaw()->getTokenKind(); }

  StringRef getText() const { return getRaw()->getTokenText(); }

  StringRef getIdentifierText() const {
    StringRef text = getText();
    if (text.front() == '`') {
      assert(text.back() == '`');
      return text.slice(1, text.size() - 1);
    }
    return text;
  }

  static bool kindof(SyntaxKind Kind) { return isTokenKind(Kind); }

  static bool classof(const SyntaxRef *S) { return kindof(S->getKind()); }
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_TOKENSYNTAX_H
