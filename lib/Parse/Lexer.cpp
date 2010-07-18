//===--- Lexer.cpp - Swift Language Lexer ---------------------------------===//
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
//  This file implements the Lexer and Token interfaces.
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Lexer.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;


Lexer::Lexer(unsigned BufferID, llvm::SourceMgr &SM) : SourceMgr(SM) {
  Buffer = SM.getMemoryBuffer(BufferID);
  CurPtr = Buffer->getBufferStart();
}

void Lexer::FormToken(tok::TokenKind Kind, const char *TokStart, Token &Result){
  Result.setToken(Kind, llvm::StringRef(TokStart, CurPtr-TokStart));
}


void Lexer::Lex(Token &Result) {
  assert(CurPtr >= Buffer->getBufferStart() &&
         CurPtr <= Buffer->getBufferEnd() && "Cur Char Pointer out of range!");
Restart:
  // Remember the start of the token so we can form the text range.
  const char *TokStart = CurPtr;
  
  switch (*CurPtr++) {
  default:
    // Error!
    return FormToken(tok::unknown, TokStart, Result);

  case ' ':
  case '\t':
  case '\n':
    goto Restart;  // Skip whitespace.
  case 0:
    // If this is a random nul character in the middle of a buffer, skip it as
    // whitespace.
    if (CurPtr-1 != Buffer->getBufferEnd())
      // TODO: WARN.
      goto Restart;
      
    // Otherwise, this is the end of the buffer.  Return EOF.
    return FormToken(tok::eof, TokStart, Result);
      
  case ',': return FormToken(tok::comma, TokStart, Result);
  case ':': return FormToken(tok::colon, TokStart, Result);
  }
}

