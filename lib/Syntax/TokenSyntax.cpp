//===--- TokenSyntax.cpp - Swift Token Syntax Implementation --------------===//
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

TokenSyntax::TokenSyntax()
  : RawSyntax(SyntaxKind::Token, {}, SourcePresence::Missing),
    TokenKind(tok::NUM_TOKENS), Text(OwnedString()),
    LeadingTrivia({}), TrailingTrivia({}) {}

TokenSyntax::TokenSyntax(tok TokenKind, OwnedString Text,
                     const SourcePresence Presence)
  : RawSyntax(SyntaxKind::Token, {}, Presence),
    TokenKind(TokenKind),
    Text(Text),
    LeadingTrivia({}), TrailingTrivia({}) {}

TokenSyntax::TokenSyntax(tok TokenKind, OwnedString Text,
                     const SourcePresence Presence,
                     const Trivia &LeadingTrivia,
                     const Trivia &TrailingTrivia)
    : RawSyntax(SyntaxKind::Token, {}, Presence),
                TokenKind(TokenKind), Text(Text),
                LeadingTrivia(LeadingTrivia), TrailingTrivia(TrailingTrivia) {}

AbsolutePosition
TokenSyntax::accumulateAbsolutePosition(AbsolutePosition &Pos) const {
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

void TokenSyntax::dumpKind(llvm::raw_ostream &OS) const {
  switch (getTokenKind()) {
  case tok::unknown:
    OS << "unknown";
    break;
  case tok::eof:
    OS << "eof";
    break;
  case tok::code_complete:
    OS << "code_complete";
    break;
  case tok::identifier:
    OS << "identifier";
    break;
  case tok::oper_binary_unspaced:
    OS << "oper_binary_unspaced";
    break;
  case tok::oper_binary_spaced:
    OS << "oper_binary_spaced";
    break;
  case tok::oper_postfix:
    OS << "oper_postfix";
    break;
  case tok::oper_prefix:
    OS << "oper_prefix";
    break;
  case tok::dollarident:
    OS << "dollarident";
    break;
  case tok::integer_literal:
    OS << "integer_literal";
    break;
  case tok::floating_literal:
    OS << "floating_literal";
    break;
  case tok::string_literal:
    OS << "string_literal";
    break;
  case tok::sil_local_name:
    OS << "sil_local_name";
    break;
  case tok::comment:
    OS << "comment";
    break;
  case tok::NUM_TOKENS:
    OS << "NUM_TOKENS (unset)";
    break;
#define KEYWORD(X)                                                             \
  case tok::kw_##X:                                                            \
    OS << "kw_" << #X;                                                         \
    break;
#define PUNCTUATOR(X, Y)                                                       \
  case tok::X:                                                                 \
    OS << #X;                                                                  \
    break;
#define POUND_KEYWORD(X)                                                       \
  case tok::pound_##X:                                                         \
    OS << "pound_" << #X;                                                      \
    break;
#include "swift/Syntax/TokenKinds.def"
  }
}

void TokenSyntax::dump(llvm::raw_ostream &OS, unsigned Indent) const {
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
