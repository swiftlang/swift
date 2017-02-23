//===--- Trivia.h - Swift Syntax Trivia Interface ---------------*- C++ -*-===//
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
// This file defines a data structure representing "trivia" in the Swift
// language, such as formatting text like whitespace, or other pieces of
// syntax that don't affect program behavior, like comments.
//
// All source trivia except for comments are kind of "run-length encoded".
// For example, a token might follow 2 newlines and 2 spaces, like so:
//
// func foo() {
//   var x = 2
// }
//
// Here, the 'var' keyword would have the following "leading" trivia:
// [ Newlines(2), Spaces(2) ]
//
// and the following "trailing" trivia:
// [ Spaces(1) ]
//
// Every terminal token in the tree has "leading" and "trailing" trivia.
//
// There is one basic rule to follow when attaching trivia:
//
// 1. A token owns all of its trailing trivia up to, but not including,
//    the next newline character.
//
// 2. Looking backward in the text, a token owns all of the leading trivia
//    up to and including the first contiguous sequence of newlines characters.
//
// For this example again:
//
// func foo() {
//   var x = 2
// }
//
// 'func'
// - Has no leading trivia.
// - Takes up the space after because of rule 1.
// - Leading: [] Trailing: [ Space(1) ]
//
// 'foo'
// - Has no leading trivia. 'func' ate it as its trailing trivia.
// - Has no trailing trivia, because it is butted up against the next '('.
// - Leading: [] Trailing: []
//
// '('
// - Has no leading or trailing trivia.
// - Leading: [] Trailing: []
//
// ')'
// - Has no leading trivia.
// - Takes up the space after because of rule 1.
// - Leading: [] Trailing: [ Space(1) ]
//
// '{'
// - Has no leading trivia. ')' ate it as its trailing trivia.
// - Has no trailing trivia. Because of Rule 1, it doesn't take the newline.
// - Leading: [] Trailing: []
//
// 'var'
// - Takes the newline and preceding two spaces because of Rule 2.
// - Takes the single space that follows because of Rule 1.
// - Leading: [ Newline(1), Space(2) ] Trailing: [ Space(1) ]
//
// ... and so on.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_TRIVIA_H
#define SWIFT_SYNTAX_TRIVIA_H

#include "swift/Basic/OwnedString.h"
#include "llvm/Support/raw_ostream.h"

#include <deque>

namespace swift {
namespace syntax {

class AbsolutePosition;

/// The kind of source trivia, such as spaces, newlines, or comments.
enum class TriviaKind {
  /// A space ' ' character.
  Space,

  /// A tab '\t' character.
  Tab,

  /// A vertical tab '\v' character.
  VerticalTab,

  /// A form-feed '\f' character.
  Formfeed,

  /// A newline '\n' character.
  Newline,

  /// A developer line comment, starting with '//'
  LineComment,

  /// A developer block comment, starting with '/*' and ending with '*/'.
  BlockComment,

  /// A documentation line comment, starting with '///'.
  DocLineComment,

  /// A documentation block comment, starting with '/**' and ending with '*/.
  DocBlockComment,

  /// A backtick '`' character, used to escape identifiers.
  Backtick,

  /// A semicolon ';' character, used for delimiting statements.
  Semicolon
};

/// A contiguous stretch of a single kind of trivia. The constituent part of
/// a `Trivia` collection.
///
/// For example, four spaces would be represented by
/// { TriviaKind::Space, 4, "" }.
///
/// All trivia except for comments don't need to store text, since they can be
/// reconstituted using their Kind and Count.
///
/// In general, you should deal with the actual Trivia collection instead
/// of individual pieces whenever possible.
struct TriviaPiece {
  TriviaKind Kind;
  unsigned Count;
  OwnedString Text;

  TriviaPiece(const TriviaKind Kind, const unsigned Count,
              const OwnedString Text)
      : Kind(Kind), Count(Count), Text(Text) {}

  /// Return a piece of trivia for some number of space characters in a row.
  static TriviaPiece spaces(unsigned Count) {
    return TriviaPiece {TriviaKind::Space, Count, OwnedString{}};
  }

  /// Return a piece of trivia for some number of tab characters in a row.
  static TriviaPiece tabs(unsigned Count) {
    return TriviaPiece {TriviaKind::Tab, Count, OwnedString{}};
  }

  /// Return a piece of trivia for some number of newline characters
  /// in a row.
  static TriviaPiece newlines(unsigned Count) {
    return TriviaPiece {TriviaKind::Newline, Count, OwnedString{}};
  }

  /// Return a piece of trivia for a single line of ('//') developer comment.
  static TriviaPiece lineComment(const OwnedString Text) {
    return TriviaPiece {TriviaKind::LineComment, 1, Text};
  }

  /// Return a piece of trivia for a block comment ('/* ... */')
  static TriviaPiece blockComment(const OwnedString Text) {
    return TriviaPiece {TriviaKind::BlockComment, 1, Text};
  }

  /// Return a piece of trivia for a single line of ('///') doc comment.
  static TriviaPiece docLineComment(const OwnedString Text) {
    return TriviaPiece {TriviaKind::DocLineComment, 1, Text};
  }

  /// Return a piece of trivia for a documentation block comment ('/** ... */')
  static TriviaPiece docBlockComment(const OwnedString Text) {
    return TriviaPiece {TriviaKind::DocBlockComment, 1, Text};
  }

  /// Return a piece of trivia for a single backtick '`' for escaping
  /// an identifier.
  static TriviaPiece backtick() {
    return TriviaPiece {TriviaKind::Backtick, 1, OwnedString{}};
  }

  /// Return a piece of trivia for some number of semicolons in a row.
  static TriviaPiece semicolon(unsigned Count) {
    return TriviaPiece {TriviaKind::Semicolon, Count, OwnedString{}};
  }

  void accumulateAbsolutePosition(AbsolutePosition &Pos) const;

  /// Print a debug representation of this trivia piece to the provided output
  /// stream and indentation level.
  void dump(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  /// Print this piece of trivia to the provided output stream.
  void print(llvm::raw_ostream &OS) const;
};

using TriviaList = std::deque<TriviaPiece>;

/// A collection of leading or trailing trivia. This is the main data structure
/// for thinking about trivia.
struct Trivia {
  TriviaList Pieces;

  /// Get the begin iterator of the pieces.
  TriviaList::const_iterator begin() const {
    return Pieces.begin();
  }

  /// Get the end iterator of the pieces.
  TriviaList::const_iterator end() const {
    return Pieces.end();
  }

  /// Add a piece to the end of the collection.
  void push_back(const TriviaPiece &Piece) {
    Pieces.push_back(Piece);
  }

  /// Add a piece to the beginning of the collection.
  void push_front(const TriviaPiece &Piece) {
    Pieces.push_front(Piece);
  }

  /// Return a reference to the first piece.
  ///
  /// Precondition: !empty()
  const TriviaPiece &front() const {
    return Pieces.front();
  }

  /// Return a reference to the last piece.
  ///
  /// Precondition: !empty()
  const TriviaPiece &back() const  {
    return Pieces.back();
  }

  /// Remove the last piece from the Trivia collection.
  ///
  /// Precondition: !empty()
  void pop_back() {
    Pieces.pop_back();
  }

  /// Returns true if there are no pieces in this Trivia collection.
  bool empty() const {
    return Pieces.empty();
  }

  /// Return the number of pieces in this Trivia collection.
  size_t size() const {
    return Pieces.size();
  }

  /// Dump a debug representation of this Trivia collection to standard error.
  void dump() const;

  /// Dump a debug representation of this Trivia collection to the provided
  /// stream and indentation level.
  void dump(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  /// Print all of the pieces to the provided output stream in source order.
  void print(llvm::raw_ostream &OS) const;

  /// Return a new Trivia collection by appending pieces from `Other`.
  Trivia appending(const Trivia &Other) const;
  Trivia operator+(const Trivia &Other) const;

  /// Look for the first TriviaPiece with the DesiredKind. If not found,
  /// returns the end iterator.
  TriviaList::const_iterator find(const TriviaKind DesiredKind) const;

  /// Returns true if the Trivia collection contains a piece of the given Kind.
  bool contains(const TriviaKind Kind) const {
    return find(Kind) != end();
  }

  /// Return a collection of trivia of some number of space characters in a row.
  static Trivia spaces(unsigned Count) {
    return {{ TriviaPiece {TriviaKind::Space, Count, OwnedString{}} }};
  }

  /// Return a collection of trivia of some number of tab characters in a row.
  static Trivia tabs(unsigned Count) {
    return {{ TriviaPiece {TriviaKind::Tab, Count, OwnedString{}} }};
  }

  /// Return a collection of trivia of some number of newline characters
  // in a row.
  static Trivia newlines(unsigned Count) {
    return {{ TriviaPiece {TriviaKind::Newline, Count, OwnedString{}} }};
  }

  /// Return a collection of trivia with a single line of ('//')
  // developer comment.
  static Trivia lineComment(const OwnedString Text) {
    return {{ TriviaPiece {TriviaKind::LineComment, 1, Text} }};
  }

  /// Return a collection of trivia with a block comment ('/* ... */')
  static Trivia blockComment(const OwnedString Text) {
    return {{ TriviaPiece {TriviaKind::BlockComment, 1, Text} }};
  }

  /// Return a collection of trivia with a single line of ('///') doc comment.
  static Trivia docLineComment(const OwnedString Text) {
    return {{ TriviaPiece {TriviaKind::DocLineComment, 1, Text} }};
  }

  /// Return a collection of trivia with a documentation block
  // comment ('/** ... */')
  static Trivia docBlockComment(const OwnedString Text) {
    return {{ TriviaPiece {TriviaKind::DocBlockComment, 1, Text} }};
  }

  /// Return a piece of trivia for a single backtick '`' for escaping
  /// an identifier.
  static Trivia backtick() {
    return {{ TriviaPiece {TriviaKind::Backtick, 1, OwnedString{}} }};
  }

  /// Return a piece of trivia for some number of semicolons in a row.
  static Trivia semicolon() {
    return {{ TriviaPiece {TriviaKind::Semicolon, 1, OwnedString{}} }};
  }
};
}
}
#endif // SWIFT_SYNTAX_TRIVIA_H
