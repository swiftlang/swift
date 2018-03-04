//===--- Trivia.cpp - Swift Syntax Trivia Implementation ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
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
} // end anonymous namespace

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
  case TriviaKind::CarriageReturn:
    OS << "carriage_return " << Count;
    break;
  case TriviaKind::CarriageReturnLineFeed:
    OS << "carriage_return_line_feed " << Count;
    break;
  case TriviaKind::LineComment:
    OS << "line_comment ";
    OS.write_escaped(Text.str());
    break;
  case TriviaKind::BlockComment:
    OS << "block_comment ";
    OS.write_escaped(Text.str());
    break;
  case TriviaKind::DocLineComment:
    OS << "doc_line_comment ";
    OS.write_escaped(Text.str());
    break;
  case TriviaKind::DocBlockComment:
    OS << "doc_block_comment ";
    OS.write_escaped(Text.str());
    break;
  case TriviaKind::GarbageText:
    OS << "garbage_text ";
    OS.write_escaped(Text.str());
    break;
  case TriviaKind::Backtick:
    OS << "backtick " << Count;
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
  case TriviaKind::GarbageText:
    Pos.addText(Text.str());
    break;
  case TriviaKind::Newline:
  case TriviaKind::CarriageReturn:
    Pos.addNewlines(Count, 1);
    break;
  case TriviaKind::CarriageReturnLineFeed:
    Pos.addNewlines(Count, 2);
    break;
  case TriviaKind::Space:
  case TriviaKind::Backtick:
  case TriviaKind::Tab:
  case TriviaKind::VerticalTab:
  case TriviaKind::Formfeed:
    Pos.addColumns(Count);
    break;
  }
}

bool TriviaPiece::trySquash(const TriviaPiece &Next) {
  if (Kind != Next.Kind) { return false; }
  
  switch (Kind) {
    case TriviaKind::Space:
    case TriviaKind::Tab:
    case TriviaKind::VerticalTab:
    case TriviaKind::Formfeed:
    case TriviaKind::Newline:
    case TriviaKind::CarriageReturn:
    case TriviaKind::CarriageReturnLineFeed:
      Count += Next.Count;
      return true;
    case TriviaKind::LineComment:
    case TriviaKind::BlockComment:
    case TriviaKind::DocLineComment:
    case TriviaKind::DocBlockComment:
    case TriviaKind::GarbageText:
    case TriviaKind::Backtick:
      return false;
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
  case TriviaKind::CarriageReturn:
    printRepeated(OS, '\r', Count);
    break;
  case TriviaKind::CarriageReturnLineFeed:
    for (unsigned i = 0; i < Count; i++) {
      OS << "\r\n";
    }
    break;
  case TriviaKind::LineComment:
  case TriviaKind::BlockComment:
  case TriviaKind::DocLineComment:
  case TriviaKind::DocBlockComment:
  case TriviaKind::GarbageText:
    OS << Text.str();
    break;
  case TriviaKind::Backtick:
    printRepeated(OS, '`', Count);
    break;
  }
}

#pragma mark - Trivia collection

void Trivia::appendOrSquash(const TriviaPiece &Next) {
  if (Pieces.size() > 0) {
    TriviaPiece &last = Pieces.back();
    if (last.trySquash(Next)) {
      return;
    }
  }
  
  push_back(Next);
}

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
                        return Piece.getKind() == DesiredKind;
                      });
}

Trivia Trivia::operator+(const Trivia &Other) const {
  auto NewPieces = Pieces;
  std::copy(Other.Pieces.begin(), Other.Pieces.end(),
            std::back_inserter(NewPieces));
  return { NewPieces };
}
