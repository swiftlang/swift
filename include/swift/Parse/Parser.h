//===--- Parser.h - Swift Language Parser -----------------------*- C++ -*-===//
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
//  This file defines the Parser interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSER_H
#define SWIFT_PARSER_H

#include "swift/Parse/Token.h"

namespace llvm {
  class SourceMgr;
}

namespace swift {
  class Lexer;
  
class Parser {
  llvm::SourceMgr &SourceMgr;
  Lexer *L;
  
  /// Tok - This is the current token being considered by the parser.
  Token Tok;
  
  Parser(const Parser&);         // DO NOT IMPLEMENT
  void operator=(const Parser&); // DO NOT IMPLEMENT
public:
  Parser(unsigned BufferID, llvm::SourceMgr &SM);
  ~Parser();
  
  void ParseTranslationUnit();
  
private:
  // Utilities.
  void ConsumeToken();
  void ConsumeToken(tok::TokenKind K) {
    assert(Tok.is(K) && "Consuming wrong token kind");
    ConsumeToken();
  }
  
  // ExpectAndConsume
  
  /// SkipUntil - Read tokens until we get to the specified token, then return
  /// without consuming it.  Because we cannot guarantee that the token will
  /// ever occur, this skips to some likely good stopping point.
  ///
  void SkipUntil(tok::TokenKind T);
  
  void Warning(llvm::SMLoc Loc, const char *Message);
  void Error(llvm::SMLoc Loc, const char *Message);
  
  // Parser Implementation
  void ParseDeclTopLevel();
  void ParseDeclVar();
};
  
} // end namespace swift

#endif
