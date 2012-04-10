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

/// skipSlashStarComment - /**/ comments are skipped (treated as whitespace).
/// Note that (unlike in C) block comments can be nested.
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

static bool isValidStartOfIdentifier(char c) {
  return isalpha(c) || c == '_';
}
static bool isValidContinuationOfIdentifier(char c) {
  return isalnum(c) || c == '_' || c == '$';
}

/// isIdentifier - Checks whether a string matches the identifier regex.
bool Lexer::isIdentifier(llvm::StringRef string) {
  if (string.empty()) return false;
  if (!isValidStartOfIdentifier(string[0])) return false;
  for (unsigned i = 1, e = string.size(); i != e; ++i)
    if (!isValidContinuationOfIdentifier(string[i]))
      return false;
  return true;
}

/// lexIdentifier - Match [a-zA-Z_][a-zA-Z_$0-9]*
void Lexer::lexIdentifier() {
  const char *TokStart = CurPtr-1;
  assert(isValidStartOfIdentifier(*TokStart) && "Unexpected start");
  
  // Lex [a-zA-Z_$0-9]*
  while (isValidContinuationOfIdentifier(*CurPtr))
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
    .Case("static", tok::kw_static)
  
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


// Return true if the string starts with "[eE][+-][0-9]"
static bool isValidExponent(const char *P) {
  if (*P != 'e' && *P != 'E')
    return false;
  ++P;
  if (*P != '+' && *P != '-')
    return false;
  ++P;
  return isdigit(*P);
}

/// lexNumber:
///   integer_literal  ::= [0-9]+
///   integer_literal  ::= 0x[0-9a-fA-F]+
///   integer_literal  ::= 0o[0-7]+
///   integer_literal  ::= 0b[01]+
///   floating_literal ::= [0-9]+(\.[0-9]+)?
///   floating_literal ::= [0-9]+(\.[0-9]*)?[eE][+-][0-9]+
///   floating_literal ::= \.[0-9]+([eE][+-][0-9]+)?
void Lexer::lexNumber() {
  const char *TokStart = CurPtr-1;
  assert((isdigit(*TokStart) || *TokStart == '.') && "Unexpected start");

  if (*TokStart == '0' && *CurPtr == 'x') {
    // 0x[0-9a-fA-F]+
    ++CurPtr;
    while (isdigit(*CurPtr) ||
           (*CurPtr >= 'a' && *CurPtr <= 'f') ||
           (*CurPtr >= 'A' && *CurPtr <= 'F'))
      ++CurPtr;
    if (CurPtr - TokStart == 2) {
      diagnose(CurPtr, diag::lex_expected_digit_in_int_literal);
      return formToken(tok::unknown, TokStart);
    }
    return formToken(tok::integer_literal, TokStart);
  } else if (*TokStart == '0' && *CurPtr == 'o') {
    // 0o[0-7]+
    ++CurPtr;
    while (*CurPtr >= '0' && *CurPtr <= '7')
      ++CurPtr;
    if (CurPtr - TokStart == 2) {
      diagnose(CurPtr, diag::lex_expected_digit_in_int_literal);
      return formToken(tok::unknown, TokStart);
    }
    return formToken(tok::integer_literal, TokStart);
  } else if (*TokStart == '0' && *CurPtr == 'b') {
    // 0b[01]+
    ++CurPtr;
    while (*CurPtr == '0' || *CurPtr == '1')
      ++CurPtr;
    if (CurPtr - TokStart == 2) {
      diagnose(CurPtr, diag::lex_expected_digit_in_int_literal);
      return formToken(tok::unknown, TokStart);
    }
    return formToken(tok::integer_literal, TokStart);
  }

  // Handle the leading character here as well.
  --CurPtr;

  // Handle a leading [0-9]+, lexing an integer or falling through if we have a
  // floating point value.
  if (isdigit(*CurPtr)) {
    while (isdigit(*CurPtr))
      ++CurPtr;
    
    // Floating literals must have '.', 'e', or 'E' after digits.  If it is
    // something else, then this is the end of the token.
    if (*CurPtr != '.' && *CurPtr != 'e' && *CurPtr != 'E')
      return formToken(tok::integer_literal, TokStart);
    
    // Lex things like 4.x as '4' followed by a tok::period.
    if (*CurPtr == '.' && !isdigit(CurPtr[1]) && !isValidExponent(CurPtr+1))
      return formToken(tok::integer_literal, TokStart);
  }
  
  // Lex decimal point.
  if (*CurPtr == '.') {
    ++CurPtr;
   
    // Lex any digits after the decimal point.
    while (isdigit(*CurPtr))
      ++CurPtr;
  }
  
  // Lex exponent.
  if (*CurPtr == 'e' || *CurPtr == 'E') {
    ++CurPtr;  // Eat the 'e'
    if (*CurPtr != '+' && *CurPtr != '-') {
      diagnose(CurPtr, diag::lex_expected_sign_in_fp);
      return formToken(tok::unknown, TokStart);
    }
    ++CurPtr;  // Eat the sign.
    
    if (!isdigit(*CurPtr)) {
      diagnose(CurPtr, diag::lex_expected_digit_in_fp_exponent);
      return formToken(tok::unknown, TokStart);
    }
    
    while (isdigit(*CurPtr))
      ++CurPtr;
  }
  
  return formToken(tok::floating_literal, TokStart);
}

/// lexStringLiteral:
///   string_literal  ::= ["][^"\\\n\r]*["]
void Lexer::lexStringLiteral() {
  const char *TokStart = CurPtr-1;
  assert(*TokStart == '"' && "Unexpected start");
  
EatString:
  // String literals cannot have \n or \r in them, and \ is an escape.
  while (*CurPtr != '\0' && *CurPtr != '"' && *CurPtr != '\\' &&
         *CurPtr != '\n' && *CurPtr != '\r')
    ++CurPtr;

  // If we found the closing " character, we're done.
  if (*CurPtr == '"') {
    ++CurPtr;
    return formToken(tok::string_literal, TokStart);
  }

  // If we got a nul, we're either at the end of file, or have an embedded
  // nul.
  if (*CurPtr == 0 && CurPtr-1 != BufferEnd) {
    diagnose(CurPtr-1, diag::lex_nul_character);
    goto EatString;
  }
    
  diagnose(TokStart, diag::lex_unterminated_string);
  --CurPtr;
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
  case ':': return formToken(tok::colon,    TokStart);
      
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
      
    // '/' starts an operator identifier.
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
  case '"':
    return lexStringLiteral();
  }
}

SourceLoc Lexer::getLocForEndOfToken(llvm::SourceMgr &SM, SourceLoc Loc) {
  // Don't try to do anything with an invalid location.
  if (!Loc.isValid())
    return Loc;

  // Figure out which buffer contains this location.
  int BufferID = SM.FindBufferContainingLoc(Loc.Value);
  if (BufferID < 0)
    return SourceLoc();
  
  const llvm::MemoryBuffer *Buffer = SM.getMemoryBuffer(BufferID);
  if (!Buffer)
    return SourceLoc();
  
  Lexer L(Buffer->getBuffer(), SM, 0, Loc.Value.getPointer());
  unsigned Length = L.peekNextToken().getLength();
  return Loc.getAdvancedLoc(Length);
}

