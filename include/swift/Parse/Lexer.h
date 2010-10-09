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

#include "swift/Parse/Token.h"

namespace llvm {
  class MemoryBuffer;
  class SourceMgr;
  class Twine;
}

namespace swift {
  class Token;

class Lexer {
  llvm::SourceMgr &SourceMgr;
  const llvm::MemoryBuffer *Buffer;
  const char *CurPtr;

  Lexer(const Lexer&);          // DO NOT IMPLEMENT
  void operator=(const Lexer&); // DO NOT IMPLEMENT
public:
  Lexer(unsigned BufferID, llvm::SourceMgr &SM);
  
  void Lex(Token &Result);
  
  
private:
  void Warning(const char *Loc, const llvm::Twine &Message);
  void Error(const char *Loc, const llvm::Twine &Message);
  void FormToken(tok::TokenKind Kind, const char *TokStart, Token &Result);
  
  void SkipSlashSlashComment();
  void LexIdentifier(Token &Result);
  void LexPunctuationIdentifier(Token &Result);
  void LexDigit(Token &Result);
};
  
  
} // end namespace swift

#endif
