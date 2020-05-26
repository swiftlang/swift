//===--- ParsedTrivia.cpp - ParsedTrivia API ----------------------------*-===//
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

#include "swift/Parse/ParsedTrivia.h"
#include "swift/Syntax/Trivia.h"
#include "swift/Basic/SourceManager.h"

using namespace swift;
using namespace swift::syntax;

Trivia
ParsedTriviaPiece::convertToSyntaxTrivia(ArrayRef<ParsedTriviaPiece> pieces,
                                         SourceLoc loc,
                                         const SourceManager &SM,
                                         unsigned bufferID) {
  Trivia trivia;
  SourceLoc curLoc = loc;
  for (const auto &piece : pieces) {
    CharSourceRange range{curLoc, piece.getLength()};
    StringRef text = SM.extractText(range, bufferID);
    trivia.push_back(TriviaPiece::fromText(piece.getKind(), text));
    curLoc = curLoc.getAdvancedLoc(piece.getLength());
  }
  return trivia;
}

Trivia
ParsedTrivia::convertToSyntaxTrivia(SourceLoc loc, const SourceManager &SM,
                                    unsigned bufferID) const {
  return ParsedTriviaPiece::convertToSyntaxTrivia(Pieces, loc, SM, bufferID);
}
