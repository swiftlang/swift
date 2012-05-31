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
#include "swift/AST/LLVM.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

enum class tok {
  unknown = 0,
  eof,
  identifier,
  oper,
  dollarident,
  integer_literal,
  floating_literal,
  string_literal,
  character_literal,
  
#define KEYWORD(X) kw_ ## X,
  // Decl and type keywords.
  KEYWORD(class)
  KEYWORD(extension)
  KEYWORD(func)
  KEYWORD(import)
  KEYWORD(oneof)
  KEYWORD(protocol)
  KEYWORD(struct)
  KEYWORD(typealias)
  KEYWORD(var)
  KEYWORD(static)
  KEYWORD(subscript)
  
  // Statements
  KEYWORD(if)
  KEYWORD(in)
  KEYWORD(else)
  KEYWORD(for)
  KEYWORD(while)
  KEYWORD(return)
  KEYWORD(break)
  KEYWORD(continue)

  // Expressions
  KEYWORD(new)
#undef KEYWORD
  
#define PUNCTUATOR(X, Y) X,
  PUNCTUATOR(l_paren_space, "(")
  PUNCTUATOR(l_paren,     "(")
  PUNCTUATOR(r_paren,     ")")
  PUNCTUATOR(l_brace,     "{")
  PUNCTUATOR(r_brace,     "}")
  PUNCTUATOR(l_square_space, "[")
  PUNCTUATOR(l_square,    "[")
  PUNCTUATOR(r_square,    "]")

  PUNCTUATOR(period,      ".")
  PUNCTUATOR(comma,       ",")
  PUNCTUATOR(colon,       ":")
  PUNCTUATOR(semi,        ";")
  PUNCTUATOR(equal,       "=")
  PUNCTUATOR(ellipsis,    "...")
  
  PUNCTUATOR(arrow,       "->")
#undef PUNCTUATOR
  
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
  
  /// Text - The actual string covered by the token in the source buffer.
  StringRef Text;
  
public:
  Token() : Kind(tok::unknown) {}
  
  tok getKind() const { return Kind; }
  void setKind(tok K) { Kind = K; }
  
  /// is/isNot - Predicates to check if this token is a specific kind, as in
  /// "if (Tok.is(tok::l_brace)) {...}".
  bool is(tok K) const { return Kind == K; }
  bool isNot(tok K) const { return Kind != K; }
  
  
  bool isAnyLParen() const {
    return Kind == tok::l_paren || Kind == tok::l_paren_space;
  }
  bool isNotAnyLParen() const { return !isAnyLParen(); }

  bool isAnyLSquare() const {
    return Kind == tok::l_square || Kind == tok::l_square_space;
  }
  bool isNotAnyLSquare() const { return !isAnyLSquare(); }
  
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
