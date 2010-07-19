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
  class Sema;
  class ASTContext;
  
class Parser {
  llvm::SourceMgr &SourceMgr;
  Lexer &L;
  Sema &S;
  
  /// Tok - This is the current token being considered by the parser.
  Token Tok;
  
  Parser(const Parser&);         // DO NOT IMPLEMENT
  void operator=(const Parser&); // DO NOT IMPLEMENT
public:
  Parser(unsigned BufferID, llvm::SourceMgr &SM, ASTContext &Context);
  ~Parser();
  
  void ParseTranslationUnit();
  
private:
  // Utilities.
  void ConsumeToken();
  void ConsumeToken(tok::TokenKind K) {
    assert(Tok.is(K) && "Consuming wrong token kind");
    ConsumeToken();
  }
  
  /// ConsumeIf - If the current token is the specified kind, consume it and
  /// return true.  Otherwise, return false without consuming it.
  bool ConsumeIf(tok::TokenKind K) {
    if (Tok.isNot(K)) return false;
    ConsumeToken(K);
    return true;
  }
  
  /// SkipUntil - Read tokens until we get to the specified token, then return
  /// without consuming it.  Because we cannot guarantee that the token will
  /// ever occur, this skips to some likely good stopping point.
  ///
  void SkipUntil(tok::TokenKind T);
  
  void Note(llvm::SMLoc Loc, const char *Message);
  void Warning(llvm::SMLoc Loc, const char *Message);
  void Error(llvm::SMLoc Loc, const char *Message);
  
  // Primitive Parsing
  bool ParseIdentifier(llvm::StringRef &Result, const char *Message = 0,
                       tok::TokenKind SkipToTok = tok::unknown);

  /// ParseToken - The parser expects that 'K' is next in the input.  If so, it
  /// is consumed and false is returned.
  ///
  /// If the input is malformed, this emits the specified error diagnostic.
  /// Next, if SkipToTok is specified, it calls SkipUntil(SkipToTok).  Finally,
  /// true is returned.
  bool ParseToken(tok::TokenKind K, const char *Message,
                  tok::TokenKind SkipToTok = tok::unknown);
  
  
  // Decl Parsing
  void ParseDeclTopLevel();
  void ParseDeclVar();
  
  // Type Parsing
  bool ParseType(const char *Message = 0);

  // Expression Parsing
  bool ParseExpr(const char *Message = 0);
  bool ParseExprPrimary(const char *Message = 0);
  bool ParseExprBinaryRHS(unsigned MinPrecedence = 1);
};
  
} // end namespace swift

#endif
