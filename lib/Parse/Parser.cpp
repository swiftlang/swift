//===--- Parser.cpp - Swift Language Parser -------------------------------===//
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
//  This file implements the Swift parser.
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/Parse/Lexer.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Setup and Helper Methods
//===----------------------------------------------------------------------===//

Parser::Parser(unsigned BufferID, llvm::SourceMgr &SM) : SourceMgr(SM) {
  L = new Lexer(BufferID, SM);
}

Parser::~Parser() {
  delete L; 
}

void Parser::Warning(llvm::SMLoc Loc, const char *Message) {
  SourceMgr.PrintMessage(Loc, Message, "warning");
}

void Parser::Error(llvm::SMLoc Loc, const char *Message) {
  SourceMgr.PrintMessage(Loc, Message, "error");
}

void Parser::ConsumeToken() {
  assert(Tok.isNot(tok::eof) && "Lexing past eof!");
  L->Lex(Tok);
}

/// SkipUntil - Read tokens until we get to the specified token, then return.
/// Because we cannot guarantee that the token will ever occur, this skips to
/// some likely good stopping point.
///
void Parser::SkipUntil(tok::TokenKind T) {
  if (Tok.is(T)) return;
  
  while (Tok.isNot(tok::eof) && Tok.isNot(T)) {
    switch (Tok.getKind()) {
    default: ConsumeToken(); break;
    }
  }
}


//===----------------------------------------------------------------------===//
// Decl Parsing
//===----------------------------------------------------------------------===//

/// ParseTranslationUnit
///   translation-unit:
///     decl-top-level*
void Parser::ParseTranslationUnit() {
  // Prime the lexer.
  ConsumeToken();
  
  while (Tok.isNot(tok::eof))
    ParseDeclTopLevel();
}

/// ParseDeclTopLevel
///   decl-top-level:
///     ';'
///     decl-var
void Parser::ParseDeclTopLevel() {
  switch (Tok.getKind()) {
  default:
    Error(Tok.getLocation(), "expected a top level declaration");
    SkipUntil(tok::semi);
    return;
  case tok::semi:   return ConsumeToken(tok::semi); 
  case tok::kw_var: return ParseDeclVar();
  }
}

/// ParseDeclVar
///   decl-var:
///      'var' 
void Parser::ParseDeclVar() {
  ConsumeToken(tok::kw_var);
  
}
