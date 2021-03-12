//===--- RawSyntax.cpp - Swift Raw Syntax Implementation ------------------===//
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

#include "swift/Syntax/RawSyntax.h"
#include "swift/Basic/ColorUtils.h"
#include "swift/Parse/Lexer.h"
#include "swift/Syntax/SyntaxArena.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>

using llvm::dyn_cast;
using namespace swift;
using namespace swift::syntax;

namespace {
static bool isTrivialSyntaxKind(SyntaxKind Kind) {
  if (isUnknownKind(Kind))
    return true;
  if (isCollectionKind(Kind))
    return true;
  switch(Kind) {
  case SyntaxKind::SourceFile:
  case SyntaxKind::CodeBlockItem:
  case SyntaxKind::ExpressionStmt:
  case SyntaxKind::DeclarationStmt:
    return true;
  default:
    return false;
  }
}

static void printSyntaxKind(SyntaxKind Kind, llvm::raw_ostream &OS,
                            SyntaxPrintOptions Opts, bool Open) {
  std::unique_ptr<swift::OSColor> Color;
  if (Opts.Visual) {
    Color.reset(new swift::OSColor(OS, llvm::raw_ostream::GREEN));
  }
  OS << "<";
  if (!Open)
    OS << "/";
  dumpSyntaxKind(OS, Kind);
  OS << ">";
}

} // end of anonymous namespace

void swift::dumpTokenKind(llvm::raw_ostream &OS, tok Kind) {
  switch (Kind) {
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

/// Lex the given trivia string into its pieces
Trivia lexTrivia(StringRef TriviaStr) {
  // FIXME: The trivia lexer should directly create TriviaPieces so we don't
  // need the conversion from ParsedTriviaPiece to TriviaPiece

  // Lex the trivia into ParsedTriviaPiece
  auto TriviaPieces = TriviaLexer::lexTrivia(TriviaStr).Pieces;

  /// Convert the ParsedTriviaPiece to TriviaPiece
  Trivia SyntaxTrivia;
  size_t Offset = 0;
  for (auto Piece : TriviaPieces) {
    StringRef Text = TriviaStr.substr(Offset, Piece.getLength());
    SyntaxTrivia.push_back(TriviaPiece::fromText(Piece.getKind(), Text));
    Offset += Piece.getLength();
  }
  return SyntaxTrivia;
}

Trivia RawSyntax::getLeadingTriviaPieces() const {
  return lexTrivia(getLeadingTrivia());
}

Trivia RawSyntax::getTrailingTriviaPieces() const {
  return lexTrivia(getTrailingTrivia());
}

const RawSyntax *RawSyntax::append(const RawSyntax *NewLayoutElement) const {
  auto Layout = getLayout();
  std::vector<const RawSyntax *> NewLayout;
  NewLayout.reserve(Layout.size() + 1);
  std::copy(Layout.begin(), Layout.end(), std::back_inserter(NewLayout));
  NewLayout.push_back(NewLayoutElement);
  return RawSyntax::make(getKind(), NewLayout, SourcePresence::Present, Arena);
}

const RawSyntax *
RawSyntax::replacingChild(CursorIndex Index,
                          const RawSyntax *NewLayoutElement) const {
  auto Layout = getLayout();
  std::vector<const RawSyntax *> NewLayout;
  NewLayout.reserve(Layout.size());

  std::copy(Layout.begin(), Layout.begin() + Index,
            std::back_inserter(NewLayout));

  NewLayout.push_back(NewLayoutElement);

  std::copy(Layout.begin() + Index + 1, Layout.end(),
            std::back_inserter(NewLayout));

  return RawSyntax::make(getKind(), NewLayout, getPresence(), Arena);
}

void RawSyntax::print(llvm::raw_ostream &OS, SyntaxPrintOptions Opts) const {
  if (isMissing())
    return;

  if (isToken()) {
    OS << getLeadingTrivia();
    OS << getTokenText();
    OS << getTrailingTrivia();
  } else {
    auto Kind = getKind();
    const bool PrintKind = Opts.PrintSyntaxKind && (Opts.PrintTrivialNodeKind ||
                                                    !isTrivialSyntaxKind(Kind));
    if (PrintKind)
      printSyntaxKind(Kind, OS, Opts, true);

    for (const auto &LE : getLayout())
      if (LE)
        LE->print(OS, Opts);

    if (PrintKind)
      printSyntaxKind(Kind, OS, Opts, false);
  }
}

void RawSyntax::dump() const {
  dump(llvm::errs(), /*Indent*/ 0);
  llvm::errs() << '\n';
}

void RawSyntax::dump(llvm::raw_ostream &OS, unsigned Indent) const {
  auto indent = [&](unsigned Amount) {
    for (decltype(Amount) i = 0; i < Amount; ++i) {
      OS << ' ';
    }
  };

  indent(Indent);
  OS << '(';
  dumpSyntaxKind(OS, getKind());

  if (isMissing())
    OS << " [missing] ";

  if (isToken()) {
    OS << " ";
    dumpTokenKind(OS, getTokenKind());

    for (auto &Leader : getLeadingTriviaPieces()) {
      OS << "\n";
      Leader.dump(OS, Indent + 1);
    }

    OS << "\n";
    indent(Indent + 1);
    OS << "(text=\"";
    OS.write_escaped(getTokenText(), /*UseHexEscapes=*/true);
    OS << "\")";

    for (auto &Trailer : getTrailingTriviaPieces()) {
      OS << "\n";
      Trailer.dump(OS, Indent + 1);
    }
  } else {
    for (auto &Child : getLayout()) {
      if (!Child)
        continue;
      OS << "\n";
      Child->dump(OS, Indent + 1);
    }
  }
  OS << ')';
}

void RawSyntax::Profile(llvm::FoldingSetNodeID &ID, tok TokKind, StringRef Text,
                        StringRef LeadingTrivia, StringRef TrailingTrivia) {
  ID.AddInteger(unsigned(TokKind));
  ID.AddInteger(LeadingTrivia.size());
  ID.AddInteger(TrailingTrivia.size());
  switch (TokKind) {
#define TOKEN_DEFAULT(NAME) case tok::NAME:
#define PUNCTUATOR(NAME, X) TOKEN_DEFAULT(NAME)
#define KEYWORD(KW) TOKEN_DEFAULT(kw_##KW)
#define POUND_KEYWORD(KW) TOKEN_DEFAULT(pound_##KW)
#include "swift/Syntax/TokenKinds.def"
    break;
  default:
    ID.AddString(Text);
    break;
  }
  ID.AddString(LeadingTrivia);
  ID.AddString(TrailingTrivia);
}
