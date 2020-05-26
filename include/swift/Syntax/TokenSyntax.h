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
  TokenSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : Syntax(Root, Data) {}

  static TokenSyntax missingToken(const tok Kind, OwnedString Text) {
    return make<TokenSyntax>(RawSyntax::missing(Kind, Text));
  }

  Trivia getLeadingTrivia() const {
    return Trivia { getRaw()->getLeadingTrivia().vec() };
  }

  Trivia getTrailingTrivia() const {
    return Trivia { getRaw()->getTrailingTrivia().vec() };
  }

  TokenSyntax withLeadingTrivia(const Trivia &Trivia) const {
    auto NewRaw = getRaw()->withLeadingTrivia(Trivia.Pieces);
    return Data->replaceSelf<TokenSyntax>(NewRaw);
  }

  TokenSyntax withTrailingTrivia(const Trivia &Trivia) const {
    auto NewRaw = getRaw()->withTrailingTrivia(Trivia.Pieces);
    return Data->replaceSelf<TokenSyntax>(NewRaw);
  }

  /* TODO: If we really need them.
  bool isKeyword() const;

  bool isPunctuation() const;

  bool isOperator() const;

  bool isLiteral() const;
  */

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

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_TOKENSYNTAX_H
