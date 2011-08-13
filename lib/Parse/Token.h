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

#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/StringRef.h"
#include "swift/AST/LLVM.h"

namespace swift {

enum class tok {
  unknown = 0,
  eof,
  identifier,
  oper,
  dollarident,
  numeric_constant,
  
#define KEYWORD(X) kw_ ## X,
  // Types.  No uint version should be added here, these should always be
  // wrapped with UDT's that have the operations.  These correspond to LLVM IR
  // types.
  KEYWORD(__builtin_int1_type)
  KEYWORD(__builtin_int8_type)
  KEYWORD(__builtin_int16_type)
  KEYWORD(__builtin_int32_type)
  KEYWORD(__builtin_int64_type)
//    KEYWORD(__builtin_float_type)
//    KEYWORD(__builtin_double_type)

  // Deck and type keywords.
  KEYWORD(import)
  KEYWORD(oneof)
  KEYWORD(struct)
  KEYWORD(func)
  KEYWORD(var)
  KEYWORD(typealias)
  
  // Statements
  KEYWORD(if)
  KEYWORD(else)
  KEYWORD(while)
  KEYWORD(return)
#undef KEYWORD
  
#define PUNCTUATOR(X, Y) X,
  PUNCTUATOR(l_paren_space, "(")
  PUNCTUATOR(l_paren,     "(")
  PUNCTUATOR(r_paren,     ")")
  PUNCTUATOR(l_brace,     "{")
  PUNCTUATOR(r_brace,     "}")
  PUNCTUATOR(l_square,    "[")
  PUNCTUATOR(r_square,    "]")

  PUNCTUATOR(period,      ".")
  PUNCTUATOR(comma,       ",")
  PUNCTUATOR(colon,       ":")
  PUNCTUATOR(coloncolon,  "::")
  PUNCTUATOR(semi,        ";")
  PUNCTUATOR(equal,       "=")
  
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
  
  /// getLoc - Return a source location identifier for the specified
  /// offset in the current file.
  SMLoc getLoc() const {
    return SMLoc::getFromPointer(Text.begin());
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
