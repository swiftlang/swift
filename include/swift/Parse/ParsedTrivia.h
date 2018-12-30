//===--- ParsedTrivia.h - ParsedTrivia API ----------------------*- C++ -*-===//
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

#ifndef SWIFT_PARSE_PARSEDTRIVIA_H
#define SWIFT_PARSE_PARSEDTRIVIA_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
  class SourceLoc;
  class SourceManager;

namespace syntax {
  enum class TriviaKind;
  struct Trivia;
}

class ParsedTriviaPiece {
  syntax::TriviaKind Kind;
  unsigned Length;

public:
  ParsedTriviaPiece(syntax::TriviaKind kind, unsigned length)
      : Kind(kind), Length(length) {}

  syntax::TriviaKind getKind() const { return Kind; }

  /// Return the text of the trivia.
  unsigned getLength() const { return Length; }

  void extendLength(unsigned len) {
    Length += len;
  }
};

using ParsedTriviaList = SmallVector<ParsedTriviaPiece, 3>;

struct ParsedTrivia {
  ParsedTriviaList Pieces;

  /// Get the begin iterator of the pieces.
  ParsedTriviaList::const_iterator begin() const {
    return Pieces.begin();
  }

  /// Get the end iterator of the pieces.
  ParsedTriviaList::const_iterator end() const {
    return Pieces.end();
  }

  /// Clear pieces.
  void clear() {
    Pieces.clear();
  }

  /// Returns true if there are no pieces in this Trivia collection.
  bool empty() const {
    return Pieces.empty();
  }

  size_t getLength() const {
    size_t Len = 0;
    for (auto &P : Pieces)
      Len += P.getLength();
    return Len;
  }

  void push_back(syntax::TriviaKind kind, unsigned length) {
    Pieces.emplace_back(kind, length);
  }

  void appendOrSquash(syntax::TriviaKind kind, unsigned length) {
    if (empty() || Pieces.back().getKind() != kind) {
      push_back(kind, length);
    } else {
      Pieces.back().extendLength(length);
    }
  }

  syntax::Trivia convertToSyntaxTrivia(SourceLoc loc, const SourceManager &SM,
                                       unsigned bufferID) const;
};

} // end namespace swift

#endif
