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

#include "swift/Syntax/RawTokenSyntax.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/TokenKinds.h"
#include "swift/Syntax/Trivia.h"

namespace swift {
namespace syntax {

class TokenSyntax final : public Syntax {
protected:
  virtual void validate() const override {
    assert(getRaw()->isToken());
  }
public:
  TokenSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : Syntax(Root, Data) {}

  RC<RawTokenSyntax> getRawToken() const {
    return cast<RawTokenSyntax>(getRaw());
  }

  static TokenSyntax missingToken(const tok Kind, OwnedString Text) {
    return make<TokenSyntax>(RawTokenSyntax::missingToken(Kind, Text));
  }

  const Trivia &getLeadingTrivia() const {
    return getRawToken()->LeadingTrivia;
  }

  const Trivia &getTrailingTrivia() const {
    return getRawToken()->TrailingTrivia;
  }

  TokenSyntax withLeadingTrivia(const Trivia &Trivia) const {
    auto NewRaw = getRawToken()->withLeadingTrivia(Trivia);
    return Data->replaceSelf<TokenSyntax>(NewRaw);
  }

  TokenSyntax withTrailingTrivia(const Trivia &Trivia) const {
    auto NewRaw = getRawToken()->withTrailingTrivia(Trivia);
    return Data->replaceSelf<TokenSyntax>(NewRaw);
  }

  bool isKeyword() const {
    return getRawToken()->isKeyword();
  }

  bool isMissing() const {
    return getRawToken()->isMissing();
  }

  bool isPunctuation() const {
    return getRawToken()->isPunctuation();
  }

  bool isOperator() const {
    return getRawToken()->isOperator();
  }

  bool isLiteral() const {
    return getRawToken()->isLiteral();
  }

  tok getTokenKind() const {
    return getRawToken()->getTokenKind();
  }

  StringRef getText() const {
    return getRawToken()->getText();
  }
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_TOKENSYNTAX_H
