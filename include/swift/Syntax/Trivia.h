//===--- Trivia.h - Swift Source Trivia -------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines syntax that doesn't affect semantics, such as whitespace
//  and comments. These are important to get printing of syntax trees with
//  full formatting fidelity.
//
//===----------------------------------------------------------------------===//


#ifndef SWIFT_PARSE_TRIVIA_H
#define SWIFT_PARSE_TRIVIA_H

#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/String.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"

#include <deque>

namespace swift {
namespace syntax {

enum class TriviaKind : uint8_t {
  StartOfFile,
  Space,
  Tab,
  Newline,
  DocComment,
  Comment,
  Backtick,
};

class Trivia {
  TriviaKind Kind;
  String Text;
  SourceLoc Loc;
public:
  Trivia(TriviaKind Kind, SourceLoc Loc) : Kind(Kind), Loc(Loc) {}

  Trivia(TriviaKind Kind, String Text, SourceLoc Loc)
    : Kind(Kind), Text(Text), Loc(Loc) {}

  TriviaKind getKind() const {
    return Kind;
  }

  StringRef str() const {
    return Text.str();
  }

  size_t size() const {
    return Text.str().size();
  }

  bool isWhitespace() const {
    return Kind < TriviaKind::Backtick;
  }

  bool isAnyComment() const {
    return Kind == TriviaKind::DocComment || Kind == TriviaKind::Comment;
  }

  bool isSingleLineComment() const {
    return isAnyComment() && Text.str().startswith("//");
  }

  bool isGYBComment() const {
    return Text.str().startswith("// ###");
  }

  /// Return the buffer-backed source location for the start of this trivia,
  /// if it was parsed from a source buffer. Implicitly created trivia does not
  /// have a SourceLoc.
  SourceLoc getLoc() const {
    return Loc;
  }

  CharSourceRange getRange() const {
    return CharSourceRange(getLoc(), size());
  }

private:
  static Trivia repeatedCharacter(TriviaKind Kind, size_t Count, char c) {
    llvm::SmallString<80> Spaces;
    llvm::raw_svector_ostream OS(Spaces);
    for (decltype(Count) i = 0; i < Count; ++i)
      OS << c;
    return Trivia { Kind, String::createManaged(OS.str()), SourceLoc() };
  }
public:
  static Trivia spaces(size_t Count) {
     return repeatedCharacter(TriviaKind::Space, Count, ' ');
  }

  static Trivia newlines(size_t Count) {
    return repeatedCharacter(TriviaKind::Newline, Count, '\n');
  }

  static Trivia startOfFile(SourceLoc Loc) {
    return Trivia { TriviaKind::StartOfFile, Loc };
  }

  bool trailingWhitespaceContainsNewline() const;

  CharSourceRange getCharSourceRange() const {
    return CharSourceRange { Loc, static_cast<unsigned int>(size()) };
  }

  void dump(llvm::raw_ostream &OS = llvm::errs()) const {
    switch (Kind) {
      case TriviaKind::Comment:
        OS << "Comment: ";
        break;
      case TriviaKind::DocComment:
        OS << "DocComment: ";
        break;
      case TriviaKind::Space:
        OS << "Spaces: " << size();
        break;
      case TriviaKind::Tab:
        OS << "Tabs: " << size();
        break;
      case TriviaKind::Newline:
        OS << "Newlines: " << size();
        break;
      case TriviaKind::StartOfFile:
        OS << "Start of file.";
        break;
      case TriviaKind::Backtick:
        OS << '`';
        break;
    }
    OS << '\'' << str() << '\'' << "\n";
  }

  void print(llvm::raw_ostream &OS) const {
    OS << str();
  }

  bool operator==(const Trivia &Other) const {
    return Kind == Other.getKind() && Text == Other.Text &&
           Loc == Other.getLoc();
  }
};

using TriviaList = std::deque<Trivia>;

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_PARSE_TRIVIA_H
