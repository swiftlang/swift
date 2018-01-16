//===--- RawTokenSyntax.cpp - Swift Token Syntax Implementation -----------===//
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


#include "swift/Syntax/TokenSyntax.h"

using namespace swift;
using namespace swift::syntax;

RawTokenSyntax::RawTokenSyntax()
  : RawSyntax(SyntaxKind::Token, {}, SourcePresence::Missing),
    TokenKind(tok::NUM_TOKENS), Text(OwnedString()),
    LeadingTrivia({}), TrailingTrivia({}) {}

RawTokenSyntax::RawTokenSyntax(tok TokenKind, OwnedString Text,
                               const SourcePresence Presence)
  : RawSyntax(SyntaxKind::Token, {}, Presence),
    TokenKind(TokenKind),
    Text(Text),
    LeadingTrivia({}), TrailingTrivia({}) {}

RawTokenSyntax::RawTokenSyntax(tok TokenKind, OwnedString Text,
                               const SourcePresence Presence,
                               const Trivia &LeadingTrivia,
                               const Trivia &TrailingTrivia)
    : RawSyntax(SyntaxKind::Token, {}, Presence),
                TokenKind(TokenKind), Text(Text),
                LeadingTrivia(LeadingTrivia), TrailingTrivia(TrailingTrivia) {}

AbsolutePosition
RawTokenSyntax::accumulateAbsolutePosition(AbsolutePosition &Pos) const {
  for (auto Leader : LeadingTrivia) {
    Leader.accumulateAbsolutePosition(Pos);
  }

  auto Start = Pos;

  Pos.addText(getText());

  for (auto Trailer : TrailingTrivia) {
    Trailer.accumulateAbsolutePosition(Pos);
  }

  return Start;
}

void RawTokenSyntax::dumpKind(llvm::raw_ostream &OS) const {
  switch (getTokenKind()) {
#define TOKEN(X)                                                               \
  case tok::X:                                                                 \
    OS << #X;                                                                  \
    break;
#include "swift/Syntax/TokenKinds.def"
  case tok::NUM_TOKENS:
    OS << "NUM_TOKENS (unset)";
    break;
  }
}

void RawTokenSyntax::dump(llvm::raw_ostream &OS, unsigned Indent) const {
  auto indent = [&](unsigned Amount) {
    for (decltype(Amount) i = 0; i < Amount; ++i) {
      OS << ' ';
    }
  };

  indent(Indent);

  OS << "(token ";
  dumpKind(OS);
  if (isMissing())
    OS << " [missing] ";

  for (auto Leader : LeadingTrivia) {
    OS << "\n";
    Leader.dump(OS, Indent + 1);
  }

  OS << "\n";
  indent(Indent + 1);
  OS << "(text=\"" << getText() << "\")";

  for (auto Trailer : TrailingTrivia) {
    OS << "\n";
    Trailer.dump(OS, Indent + 1);
  }
  OS << ')';
}
