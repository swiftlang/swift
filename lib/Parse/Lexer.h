//===--- Lexer.h - Swift Language Lexer -------------------------*- C++ -*-===//
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
//  This file defines the Lexer interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LEXER_H
#define SWIFT_LEXER_H

#include "Token.h"

namespace llvm {
  class MemoryBuffer;
  class SourceMgr;
}

namespace swift {
  class Token;
  class ASTContext;

class Lexer {
  llvm::SourceMgr &SourceMgr;
  const llvm::MemoryBuffer *Buffer;
  const char *CurPtr;
  ASTContext &Context;

  Token NextToken;
  
  Lexer(const Lexer&) = delete;
  void operator=(const Lexer&) = delete;
public:
  Lexer(unsigned BufferID, ASTContext &Context);
  
  void lex(Token &Result) {
    Result = NextToken;
    if (Result.isNot(tok::eof))
      lexImpl();
  }

  /// peekNextToken - Return the next token to be returned by Lex without
  /// actually lexing it.
  const Token &peekNextToken() const { return NextToken; }
  
private:
  void note(const char *Loc, const Twine &Message);
  void warning(const char *Loc, const Twine &Message);
  void error(const char *Loc, const Twine &Message);
  void lexImpl();
  void formToken(tok Kind, const char *TokStart);
  
  void skipSlashSlashComment();
  void skipSlashStarComment();
  void lexIdentifier();
  void lexDollarIdent();
  void lexPunctuationIdentifier();
  void lexDigit();
};
  
  
} // end namespace swift

#endif
