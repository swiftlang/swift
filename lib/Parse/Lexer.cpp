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
#include "swift/AST/ASTContext.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Identifier.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Setup and Helper Methods
//===----------------------------------------------------------------------===//

Lexer::Lexer(StringRef Buffer, llvm::SourceMgr &SourceMgr,
             DiagnosticEngine *Diags, const char *CurrentPosition)
  : SourceMgr(SourceMgr), Diags(Diags) {
  BufferStart = Buffer.begin();
  BufferEnd = Buffer.end();
  CurPtr = CurrentPosition;
  assert(CurPtr >= BufferStart && CurPtr <= BufferEnd &&
         "Current position is out-of-range");
    
  // Prime the lexer.
  lexImpl();
}

InFlightDiagnostic Lexer::diagnose(const char *Loc, Diag<> ID) {
  if (Diags)
    Diags->diagnose(getSourceLoc(Loc), ID);
  
  return InFlightDiagnostic();
}

void Lexer::formToken(tok Kind, const char *TokStart) {
  NextToken.setToken(Kind, StringRef(TokStart, CurPtr-TokStart));
}

//===----------------------------------------------------------------------===//
// Lexer Subroutines
//===----------------------------------------------------------------------===//

/// skipSlashSlashComment - Skip to the end of the line of a // comment.
void Lexer::skipSlashSlashComment() {
  assert(CurPtr[-1] == '/' && CurPtr[0] == '/' && "Not a // comment");
  while (1) {
    switch (*CurPtr++) {
    case '\n':
    case '\r':
      return;  // If we found the end of the line, return.
    default:
      break;   // Otherwise, eat other characters.
    case 0:
      // If this is a random nul character in the middle of a buffer, skip it as
      // whitespace.
      if (CurPtr-1 != BufferEnd) {
        diagnose(CurPtr-1, diag::lex_nul_character);
        break;
      }
        
      // Otherwise, we have a // comment at end of file.
      --CurPtr;
      return;
    }
  }
}

void Lexer::skipSlashStarComment() {
  const char *StartPtr = CurPtr-1;
  assert(CurPtr[-1] == '/' && CurPtr[0] == '*' && "Not a /* comment");
  // Make sure to advance over the * so that we don't incorrectly handle /*/ as
  // the beginning and end of the comment.
  ++CurPtr;
  
  // /**/ comments can be nested, keep track of how deep we've gone.
  unsigned Depth = 1;
  
  while (1) {
    switch (*CurPtr++) {
    case '*':
      // Check for a '*/'
      if (*CurPtr == '/') {
        ++CurPtr;
        if (--Depth == 0)
          return;
      }
      break;
    case '/':
      // Check for a '/*'
      if (*CurPtr == '*') {
        ++CurPtr;
        ++Depth;
      }
      break;
    default:
      break;   // Otherwise, eat other characters.
    case 0:
      // If this is a random nul character in the middle of a buffer, skip it as
      // whitespace.
      if (CurPtr-1 != BufferEnd) {
        diagnose(CurPtr-1, diag::lex_nul_character);
        break;
      }
      
      // Otherwise, we have an unterminated /* comment.
      --CurPtr;
      diagnose(CurPtr-(CurPtr[-1] == '\n'),
               diag::lex_unterminated_block_comment);
      diagnose(StartPtr, diag::lex_comment_start);
      return;
    }
  }
}


/// lexIdentifier - Match [a-zA-Z_][a-zA-Z_$0-9]*
void Lexer::lexIdentifier() {
  const char *TokStart = CurPtr-1;
  assert((isalpha(*TokStart) || *TokStart == '_') && "Unexpected start");
  
  // Lex [a-zA-Z_$0-9]*
  while (isalnum(*CurPtr) || *CurPtr == '_' || *CurPtr == '$')
    ++CurPtr;
  
  tok Kind =
  llvm::StringSwitch<tok>(StringRef(TokStart, CurPtr-TokStart))
    // decl and type keywords
    .Case("extension", tok::kw_extension)
    .Case("import", tok::kw_import)
    .Case("func", tok::kw_func)
    .Case("oneof", tok::kw_oneof)
    .Case("protocol", tok::kw_protocol)
    .Case("struct", tok::kw_struct)
    .Case("typealias", tok::kw_typealias)
    .Case("var", tok::kw_var)
  
    // Statements
    .Case("if", tok::kw_if)
    .Case("else", tok::kw_else)
    .Case("while", tok::kw_while)
    .Case("return", tok::kw_return)
    .Default(tok::identifier);
  
  return formToken(Kind, TokStart);
}

/// lexOperatorIdentifier - Match identifiers formed out of punctuation.
void Lexer::lexOperatorIdentifier() {
  const char *TokStart = CurPtr-1;

  while (Identifier::isOperatorChar(*CurPtr))
    ++CurPtr;
  
  // Match various reserved words.
  if (CurPtr-TokStart == 1) {
    switch (TokStart[0]) {
    case '=': return formToken(tok::equal, TokStart);
    }
  } else if (CurPtr-TokStart == 2) {
    switch ((TokStart[0] << 8) | TokStart[1]) {
    case ('-' << 8) | '>': // ->
      return formToken(tok::arrow, TokStart);
    }
  }
  
  return formToken(tok::oper, TokStart);
}

/// lexDollarIdent - Match $[0-9a-zA-Z_$]*
void Lexer::lexDollarIdent() {
  const char *TokStart = CurPtr-1;
  assert(*TokStart == '$');
  
  // Lex [a-zA-Z_$0-9]*
  while (isalnum(*CurPtr) || *CurPtr == '_' || *CurPtr == '$')
    ++CurPtr;
  
  return formToken(tok::dollarident, TokStart);
}


/// lexNumber - Match ([0-9]|[.][0-9])*
void Lexer::lexNumber() {
  const char *TokStart = CurPtr-1;
  assert((isdigit(*TokStart) || *TokStart == '.') && "Unexpected start");

  while (1) {
    if (isdigit(*CurPtr))
      ++CurPtr;
    else if (*CurPtr == '.' && isdigit(CurPtr[1]))
      CurPtr += 2;
    else
      break;
  }
  
  return formToken(tok::numeric_constant, TokStart);
}


//===----------------------------------------------------------------------===//
// Main Lexer Loop
//===----------------------------------------------------------------------===//

void Lexer::lexImpl() {
  assert(CurPtr >= BufferStart &&
         CurPtr <= BufferEnd && "Cur Char Pointer out of range!");
  
Restart:
  // Remember the start of the token so we can form the text range.
  const char *TokStart = CurPtr;
  
  switch (*CurPtr++) {
  default:
    diagnose(CurPtr-1, diag::lex_invalid_character);
    return formToken(tok::unknown, TokStart);

  case ' ':
  case '\t':
  case '\n':
  case '\r':
    goto Restart;  // Skip whitespace.
  case 0:
    // If this is a random nul character in the middle of a buffer, skip it as
    // whitespace.
    if (CurPtr-1 != BufferEnd) {
      diagnose(CurPtr-1, diag::lex_nul_character);
      goto Restart;
    }
      
    // Otherwise, this is the end of the buffer.  Return EOF.
    return formToken(tok::eof, TokStart);

  case '(': {
    // This is either l_paren or l_paren_space depending on whether there is
    // whitespace before it.
    bool PrecededBySpace;

    // For these purposes, the start of the file is considered to be
    // preceeded by infinite whitespace.
    if (CurPtr - 1 == BufferStart) {
      PrecededBySpace = true;

    // Otherwise, our list of whitespace characters is pretty short.
    } else {
      char LastChar = *(CurPtr - 2);
      PrecededBySpace = (isspace(LastChar) || LastChar == '\0');
    }

    if (PrecededBySpace)
      return formToken(tok::l_paren_space, TokStart);

    return formToken(tok::l_paren, TokStart);
  }
  case ')': return formToken(tok::r_paren,  TokStart);
  case '{': return formToken(tok::l_brace,  TokStart);
  case '}': return formToken(tok::r_brace,  TokStart);
  case '[': return formToken(tok::l_square, TokStart);
  case ']': return formToken(tok::r_square, TokStart);

  case '.':
    if (isdigit(CurPtr[0]))   // .42
      return lexNumber();
      
    return formToken(tok::period, TokStart);
  case ',': return formToken(tok::comma,    TokStart);
  case ';': return formToken(tok::semi,     TokStart);
      
  case ':':
    if (CurPtr[0] != ':')
      return formToken(tok::colon, TokStart);
    ++CurPtr;
    return formToken(tok::coloncolon, TokStart);
      
  // Punctuator identifier characters.
  case '/':
    if (CurPtr[0] == '/') {  // "//"
      skipSlashSlashComment();
      goto Restart;
    }
      
    if (CurPtr[0] == '*') { // "/*"
      skipSlashStarComment();
      goto Restart;
    }
      
    // '/' starts an identifier.
    return lexOperatorIdentifier();

  case '=':
  case '-':
  case '+':
  case '*': 
  case '%':
  case '<':
  case '>':
  case '!':
  case '&':
  case '|':
  case '^':
    return lexOperatorIdentifier();

  case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
  case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
  case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
  case 'V': case 'W': case 'X': case 'Y': case 'Z':
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
  case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
  case 'v': case 'w': case 'x': case 'y': case 'z':
  case '_':
    return lexIdentifier();
  
  case '$':
    return lexDollarIdent();
      
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    return lexNumber();
  }
}
