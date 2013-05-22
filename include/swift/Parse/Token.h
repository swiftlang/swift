//===--- Token.h - Token interface ------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Token interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TOKEN_H
#define SWIFT_TOKEN_H

#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

enum class tok {
  unknown = 0,
  eof,
  identifier,
  oper_binary,
  oper_postfix,
  oper_prefix,
  dollarident,
  integer_literal,
  floating_literal,
  string_literal,
  character_literal,
  sil_local_name,      // %42 in SIL mode.
  
#define KEYWORD(X) kw_ ## X,
#define PUNCTUATOR(X, Y) X,
#include "swift/Parse/Tokens.def"
  
  NUM_TOKENS
};

/// Token - This structure provides full information about a lexed token.
/// It is not intended to be space efficient, it is intended to return as much
/// information as possible about each returned token.  This is expected to be
/// compressed into a smaller form if memory footprint is important.
///
class Token {
  /// Kind - The actual flavor of token this is.
  ///
  tok Kind;

  /// \brief Whether this token is the first token on the line.
  bool AtStartOfLine;

  /// Text - The actual string covered by the token in the source buffer.
  StringRef Text;
  
public:
  Token() : Kind(tok::unknown), AtStartOfLine(false) {}
  
  tok getKind() const { return Kind; }
  void setKind(tok K) { Kind = K; }
  
  /// is/isNot - Predicates to check if this token is a specific kind, as in
  /// "if (Tok.is(tok::l_brace)) {...}".
  bool is(tok K) const { return Kind == K; }
  bool isNot(tok K) const { return Kind != K; }
  
  bool isAnyOperator() const {
    return Kind == tok::oper_binary
        || Kind == tok::oper_postfix
        || Kind == tok::oper_prefix;
  }
  bool isNotAnyOperator() const {
    return !isAnyOperator();
  }

  /// \brief Determine whether this token occurred at the start of a line.
  bool isAtStartOfLine() const { return AtStartOfLine; }

  /// \brief Set whether this token occurred at the start of a line.
  void setAtStartOfLine(bool value) { AtStartOfLine = value; }

  bool isContextualKeyword(StringRef ContextKW) const {
    return is(tok::identifier) && Text == ContextKW;
  }

  bool isContextualPunctuator(StringRef ContextPunc) const {
    return isAnyOperator() && Text == ContextPunc;
  }

  /// True if the token is an l_paren token that does not start a new line.
  bool isFollowingLParen() const {
    return !isAtStartOfLine() && Kind == tok::l_paren;
  }
  
  /// True if the token is an l_square token that does not start a new line.
  bool isFollowingLSquare() const {
    return !isAtStartOfLine() && Kind == tok::l_square;
  }
  
  /// getLoc - Return a source location identifier for the specified
  /// offset in the current file.
  SourceLoc getLoc() const {
    return SourceLoc(llvm::SMLoc::getFromPointer(Text.begin()));
  }

  StringRef getText() const { return Text; }
  void setText(StringRef T) { Text = T; }
  
  unsigned getLength() const { return Text.size(); }
  
  /// setToken - Set the token to the specified kind and source range.
  ///
  void setToken(tok K, StringRef T) {
    Kind = K;
    Text = T;
  }
};
  
} // end namespace swift


namespace llvm {
  template <typename T> struct isPodLike;
  template <>
  struct isPodLike<swift::Token> { static const bool value = true; };
}  // end namespace llvm

#endif
