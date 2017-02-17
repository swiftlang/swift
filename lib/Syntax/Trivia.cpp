//===--- Syntax.cpp - Swift Syntax Trivia Implementation --------*- C++ -*-===//
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
#include "swift/Syntax/Trivia.h"

using namespace swift;
using namespace swift::syntax;

namespace {
void printRepeated(llvm::raw_ostream &OS, char c, size_t Count) {
  for (decltype(Count) i = 0; i < Count; ++i)
    OS << c;
}

void escapeNewlines(std::string &S) {
  size_t Index = 0;
  while (true) {
    Index = S.find("\n", Index);
    if (Index == std::string::npos)
      break;
    S.erase(Index, 1);
    S.insert(Index, "\\n");
    Index += 3;
  }
}
}

void TriviaPiece::dump(llvm::raw_ostream &OS, unsigned Indent) const {
  printRepeated(OS, ' ', Indent);
  OS << "(trivia ";

  switch (Kind) {
  case TriviaKind::Space:
    OS << "space " << Count;
    break;
  case TriviaKind::Tab:
    OS << "tab " << Count;
    break;
  case TriviaKind::VerticalTab:
    OS << "vertical_tab " << Count;
    break;
  case TriviaKind::Formfeed:
    OS << "form_feed " << Count;
    break;
  case TriviaKind::Newline:
    OS << "newline " << Count;
    break;
  case TriviaKind::LineComment:
    OS << "line_comment" << Text.str();
    break;
  case TriviaKind::BlockComment: {
    // Make this fit a little more nicely with indentation
    // and the lispy nature of the dump by escaping the newlines.
    auto s = Text.str().str();
    escapeNewlines(s);
    OS << "block_comment" << Text.str();
    break;
  }
  case TriviaKind::DocLineComment:
    OS << "doc_line_comment" << Text.str();
    break;
  case TriviaKind::DocBlockComment: {
    // Make this fit a little more nicely with indentation
    // and the lispy nature of the dump by escaping the newlines.
    auto s = Text.str().str();
    escapeNewlines(s);
    OS << "doc_block_comment" << Text.str();
    break;
  }
  case TriviaKind::Backtick:
    OS << "backtick " << Count;
    break;
  case TriviaKind::Semicolon:
    OS << "semicolon " << Count;
    break;
  }
  OS << ')';
}

void TriviaPiece::accumulateAbsolutePosition(AbsolutePosition &Pos) const {
  switch (Kind) {
  case TriviaKind::LineComment:
  case TriviaKind::BlockComment:
  case TriviaKind::DocBlockComment:
  case TriviaKind::DocLineComment:
    Pos.addText(Text.str());
    break;
  case TriviaKind::Newline:
    Pos.addNewlines(Count);
    break;
  case TriviaKind::Space:
  case TriviaKind::Backtick:
  case TriviaKind::Semicolon:
  case TriviaKind::Tab:
  case TriviaKind::VerticalTab:
  case TriviaKind::Formfeed:
    Pos.addColumns(Count);
    break;
  }
}

void TriviaPiece::print(llvm::raw_ostream &OS) const {
  switch (Kind) {
  case TriviaKind::Space:
    printRepeated(OS, ' ', Count);
    break;
  case TriviaKind::Tab:
    printRepeated(OS, '\t', Count);
    break;
  case TriviaKind::VerticalTab:
    printRepeated(OS, '\v', Count);
    break;
  case TriviaKind::Formfeed:
    printRepeated(OS, '\f', Count);
    break;
  case TriviaKind::Newline:
    printRepeated(OS, '\n', Count);
    break;
  case TriviaKind::LineComment:
  case TriviaKind::BlockComment:
  case TriviaKind::DocLineComment:
  case TriviaKind::DocBlockComment:
    OS << Text.str();
    break;
  case TriviaKind::Backtick:
    printRepeated(OS, '`', Count);
    break;
  case TriviaKind::Semicolon:
    printRepeated(OS, ';', Count);
  }
}

#pragma mark - Trivia collection

Trivia Trivia::appending(const Trivia &Other) const {
  auto NewPieces = Pieces;
  std::copy(Other.begin(), Other.end(), std::back_inserter(NewPieces));
  return { NewPieces };
}

void Trivia::dump(llvm::raw_ostream &OS, unsigned Indent) const {
  for (const auto &Piece : Pieces) {
    Piece.dump(OS, Indent);
  }
}

void Trivia::dump() const {
  dump(llvm::errs());
}

void Trivia::print(llvm::raw_ostream &OS) const {
  for (const auto &Piece : Pieces) {
    Piece.print(OS);
  }
}

TriviaList::const_iterator Trivia::find(const TriviaKind DesiredKind) const {
  return std::find_if(Pieces.begin(), Pieces.end(),
                      [=](const TriviaPiece &Piece) -> bool {
                        return Piece.Kind == DesiredKind;
                      });
}

Trivia Trivia::operator+(const Trivia &Other) const {
  auto NewPieces = Pieces;
  std::copy(Other.Pieces.begin(), Other.Pieces.end(),
            std::back_inserter(NewPieces));
  return { NewPieces };
}
