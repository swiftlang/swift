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
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// UTF8 Validation/Encoding/Decoding helper functions
//===----------------------------------------------------------------------===//

/// EncodeToUTF8 - Encode the specified code point into a UTF8 stream.  Return
/// true if it is an erroneous code point.
static bool EncodeToUTF8(unsigned CharValue,
                         llvm::SmallVectorImpl<char> &Result) {
  assert(CharValue >= 0x80 && "Single-byte encoding should be already handled");
  // Number of bits in the value, ignoring leading zeros.
  unsigned NumBits = 32-llvm::CountLeadingZeros_32(CharValue);
  unsigned LowHalf = CharValue & 0xFFFF;

  // Reserved values in each plane
  if (LowHalf == 0xFFFE || LowHalf == 0xFFFF)
    return true;

  // Handle the leading byte, based on the number of bits in the value.
  unsigned NumTrailingBytes;
  if (NumBits <= 5+6) {
    // Encoding is 0x110aaaaa 10bbbbbb
    Result.push_back(char(0xC0 | (CharValue >> 6)));
    NumTrailingBytes = 1;
  } else if (NumBits <= 4+6+6) {
    // Encoding is 0x1110aaaa 10bbbbbb 10cccccc
    Result.push_back(char(0xE0 | (CharValue >> (6+6))));
    NumTrailingBytes = 2;

    // UTF-16 surrogate pair values are not valid code points.
    if (CharValue >= 0xD800 && CharValue <= 0xDFFF)
      return true;
    // U+FDD0...U+FDEF are also reserved
    if (CharValue >= 0xFDD0 && CharValue <= 0xFDEF)
      return true;
  } else if (NumBits <= 3+6+6+6) {
    // Encoding is 0x11110aaa 10bbbbbb 10cccccc 10dddddd
    Result.push_back(char(0xF0 | (CharValue >> (6+6+6))));
    NumTrailingBytes = 3;
    // Reject over-large code points.  These cannot be encoded as UTF-16
    // surrogate pairs, so UTF-32 doesn't allow them.
    if (CharValue > 0x10FFFF)
      return true;
  } else {
    return true;  // UTF8 can encode these, but they aren't valid code points.
  }
  
  // Emit all of the trailing bytes.
  while (NumTrailingBytes--)
    Result.push_back(char(0x80 | (0x3F & (CharValue >> (NumTrailingBytes*6)))));
  return false;
}


/// CLO8 - Return the number of leading ones in the specified 8-bit value.
static unsigned CLO8(unsigned char C) {
  return llvm::CountLeadingOnes_32(uint32_t(C) << 24);
}

/// isStartOfUTF8Character - Return true if this isn't a UTF8 continuation
/// character, which will be of the form 0b10XXXXXX
static bool isStartOfUTF8Character(unsigned char C) {
  return (signed char)C >= 0 || C >= 0xC0;  // C0 = 0b11000000
}

/// validateUTF8CharacterAndAdvance - Given a pointer to the starting byte of a
/// UTF8 character, validate it and advance the lexer past it.  This returns the
/// encoded character or ~0U if the encoding is invalid.
static uint32_t validateUTF8CharacterAndAdvance(const char *&Ptr) {
  assert((signed char)(*Ptr) < 0 && "Not the start of an encoded letter");
  
  unsigned char CurByte = *Ptr++;
  
  // Read the number of high bits set, which indicates the number of bytes in
  // the character.
  unsigned EncodedBytes = CLO8(CurByte);
  
  // If this is 0b10XXXXXX, then it is a continuation character.
  if (EncodedBytes == 1 ||
      // If the number of encoded bytes is > 4, then this is an invalid
      // character in the range of 0xF5 and above.  These would start an
      // encoding for something that couldn't be represented with UTF16
      // digraphs, so Unicode rejects them.
      EncodedBytes > 4) {
    // Skip until we get the start of another character.  This is guaranteed to
    // at least stop at the nul at the end of the buffer.
    while (!isStartOfUTF8Character(*Ptr))
      ++Ptr;
    return ~0U;
  }
  
  // Drop the high bits indicating the # bytes of the result.
  unsigned CharValue = (unsigned char)(CurByte << EncodedBytes) >> EncodedBytes;
  
  // Read and validate the continuation bytes.
  for (unsigned i = 1; i != EncodedBytes; ++i) {
    CurByte = *Ptr;
    // If the high bit isn't set or the second bit isn't clear, then this is not
    // a continuation byte!
    if (CurByte < 0x80 || CurByte >= 0xC0) return ~0U;
    
    // Accumulate our result.
    CharValue <<= 6;
    CharValue |= CurByte & 0x3F;
    ++Ptr;
  }
  
  // UTF-16 surrogate pair values are not valid code points.
  if (CharValue >= 0xD800 && CharValue <= 0xDFFF)
    return ~0U;
  
  // If we got here, we read the appropriate number of accumulated bytes.
  // Verify that the encoding was actually minimal.
  // Number of bits in the value, ignoring leading zeros.
  unsigned NumBits = 32-llvm::CountLeadingZeros_32(CharValue);
  
  if (NumBits <= 5+6)
    return EncodedBytes == 2 ? CharValue : ~0U;
  if (NumBits <= 4+6+6)
    return EncodedBytes == 3 ? CharValue : ~0U;
  return EncodedBytes == 4 ? CharValue : ~0U;
}


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
    return Diags->diagnose(getSourceLoc(Loc), ID);
  
  return InFlightDiagnostic();
}

tok Lexer::getTokenKind(StringRef Text) {
  assert(Text.data() >= BufferStart && Text.data() <= BufferEnd &&
         "Text string does not fall within lexer's buffer");
  Lexer L(StringRef(BufferStart, BufferEnd - BufferStart), SourceMgr, Diags,
          Text.data());
  Token Result;
  L.lex(Result);
  return Result.getKind();
}

void Lexer::formToken(tok Kind, const char *TokStart) {
  NextToken.setToken(Kind, StringRef(TokStart, CurPtr-TokStart));
}

void Lexer::formStartingToken(tok Kind,const char *TokStart,tok FollowingKind) {
  if (isStartingToken(TokStart)) {
    NextToken.setToken(Kind, StringRef(TokStart, CurPtr-TokStart));
  } else {
    if (FollowingKind == tok::unknown)
      diagnose(TokStart, diag::lex_missing_whitespace);
    NextToken.setToken(FollowingKind, StringRef(TokStart, CurPtr-TokStart));
  }
}

bool Lexer::isStartingToken(const char *TokStart) {
  // Note: "NextToken" is actually the soon to be previous token.
  switch (NextToken.getKind()) {
#define IDENTIFIER_KEYWORD(kw) case tok::kw_##kw:
#include "swift/Parse/Tokens.def"
  case tok::identifier:
  case tok::dollarident:
  case tok::integer_literal:
  case tok::floating_literal:
  case tok::string_literal:
  case tok::character_literal:
  case tok::r_paren:
  case tok::r_square:
  case tok::r_brace: {
    // If there is whitespace between the above tokens and this one,
    // then the current token is a literal.
    char LastChar = *(TokStart - 1);
    return isspace(LastChar) || LastChar == '\0';
  }
  default:
    return true;
  }
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
      // If this is a "high" UTF-8 character, validate it.
      if ((signed char)(CurPtr[-1]) < 0) {
        --CurPtr;
        const char *CharStart = CurPtr;
        if (validateUTF8CharacterAndAdvance(CurPtr) == ~0U)
          diagnose(CharStart, diag::lex_invalid_utf8_character);
      }
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
      // If this is a "high" UTF-8 character, validate it.
      if ((signed char)(CurPtr[-1]) < 0) {
        --CurPtr;
        const char *CharStart = CurPtr;
        if (validateUTF8CharacterAndAdvance(CurPtr) == ~0U)
          diagnose(CharStart, diag::lex_invalid_utf8_character);
      }

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
///
/// FIXME: We should also allow unicode characters in identifiers.
void Lexer::lexIdentifier() {
  const char *TokStart = CurPtr-1;
  assert(isValidStartOfIdentifier(*TokStart) && "Unexpected start");

  // Lex [a-zA-Z_$0-9]*
  while (isValidContinuationOfIdentifier(*CurPtr))
    ++CurPtr;

  tok Kind =
  llvm::StringSwitch<tok>(StringRef(TokStart, CurPtr-TokStart))
    // Declarations and Type Keywords
    .Case("class", tok::kw_class)
    .Case("constructor", tok::kw_constructor)
    .Case("destructor", tok::kw_destructor)
    .Case("extension", tok::kw_extension)
    .Case("func", tok::kw_func)
    .Case("import", tok::kw_import)
    .Case("oneof", tok::kw_oneof)
    .Case("protocol", tok::kw_protocol)
    .Case("requires", tok::kw_requires)
    .Case("struct", tok::kw_struct)
    .Case("typealias", tok::kw_typealias)
    .Case("var", tok::kw_var)
    .Case("static", tok::kw_static) // inside-out attribute that implies a decl
    .Case("subscript", tok::kw_subscript)

    // Statements
    .Case("if", tok::kw_if)
    .Case("else", tok::kw_else)
    .Case("for", tok::kw_for)
    .Case("do", tok::kw_do)
    .Case("while", tok::kw_while)
    .Case("return", tok::kw_return)
    .Case("break", tok::kw_break)
    .Case("continue", tok::kw_continue)

    // Expressions
    .Case("new", tok::kw_new)

    // Reserved Identifiers
    .Case("metatype", tok::kw_metatype)
    .Case("super", tok::kw_super)
    .Case("this", tok::kw_this)
    .Case("This", tok::kw_This)

    .Default(tok::identifier);

  return formStartingToken(Kind, TokStart);
}

/// Is the operator beginning at the given character "left-bound"?
static bool isLeftBound(const char *tokBegin, const char *bufferBegin) {
  // The first character in the file is not left-bound.
  if (tokBegin == bufferBegin) return false;

  switch (tokBegin[-1]) {
  case ' ': case '\r': case '\n': case '\t': // whitespace
  case '(': case '[': case '{':              // opening delimiters
  case ',': case ';':                        // expression separators
  case '\0':                                 // whitespace / last char in file
    return false;

  default:
    return true;
  }
}

/// Is the operator ending at the given character (actually one past the end)
/// "right-bound"?
static bool isRightBound(const char *tokEnd) {
  switch (*tokEnd) {
  case ' ': case '\r': case '\n': case '\t': // whitespace
  case ')': case ']': case '}':              // closing delimiters
  case ',': case ';':                        // expression separators
  case '\0':                                 // whitespace / last char in file
    return false;

  default:
    return true;
  }
}

/// lexOperatorIdentifier - Match identifiers formed out of punctuation.
void Lexer::lexOperatorIdentifier() {
  const char *TokStart = CurPtr-1;

  // We only allow '.' in a series
  if (*TokStart == '.') {
    while (*CurPtr == '.')
      ++CurPtr;

    if (CurPtr-TokStart > 3) {
      diagnose(TokStart, diag::lex_unexpected_long_period_series);
      return formToken(tok::unknown, TokStart);
    }
  } else {
    while (Identifier::isOperatorChar(*CurPtr) && *CurPtr != '.')
      ++CurPtr;
  }

  // Decide between the binary, prefix, and postfix cases.
  // It's binary if either both sides are bound or both sides are not bound.
  // Otherwise, it's postfix if left-bound and prefix if right-bound.
  bool leftBound = isLeftBound(TokStart, BufferStart);
  bool rightBound = isRightBound(CurPtr);

  // Match various reserved words.
  if (CurPtr-TokStart == 1) {
    switch (TokStart[0]) {
    case '=':
      return formToken(tok::equal, TokStart);
    case '&':
      if (leftBound == rightBound || leftBound)
        break;
      return formToken(tok::make_ref, TokStart);
    case '.':
      // Parsing the '.' in ".5" in "3.14*.5" breaks the rules we have for
      // operators. It becomes a false-positive binary operator given our
      // simple left bound versus right bound rule.
      if (isdigit(CurPtr[0]) && isStartingToken(TokStart))
        return lexNumber();
      if (leftBound == rightBound)
        return formToken(tok::period, TokStart);
      if (rightBound)
        return formToken(tok::unresolved_member, TokStart);
      diagnose(TokStart, diag::lex_unary_postfix_dot_is_reserved);
      return formToken(tok::unknown, TokStart);
    }
  } else if (CurPtr-TokStart == 2) {
    switch ((TokStart[0] << 8) | TokStart[1]) {
    case ('-' << 8) | '>': // ->
      return formToken(tok::arrow, TokStart);
    case ('*' << 8) | '/': // */
      diagnose(TokStart, diag::lex_unexpected_block_comment_end);
      return formToken(tok::unknown, TokStart);
    }
  } else {
    if (CurPtr-TokStart == 3) {
      switch ((TokStart[0] << 16) | (TokStart[1] << 8) | TokStart[0]) {
      case ('.' << 16) | ('.' << 8) | '.':
        return formToken(tok::ellipsis, TokStart);
      }
    }
    // If there is a "//" in the middle of an identifier token, it starts
    // a single-line comment.
    auto Pos = StringRef(TokStart, CurPtr-TokStart).find("//");
    if (Pos != StringRef::npos)
      CurPtr = TokStart+Pos;

    // If there is a "/*" in the middle of an identifier token, it starts
    // a multi-line comment.
    Pos = StringRef(TokStart, CurPtr-TokStart).find("/*");
    if (Pos != StringRef::npos)
      CurPtr = TokStart+Pos;

    // Verify there is no "*/" in the middle of the identifier token, we reject
    // it as potentially ending a block comment.
    Pos = StringRef(TokStart, CurPtr-TokStart).find("*/");
    if (Pos != StringRef::npos) {
      diagnose(TokStart+Pos, diag::lex_unexpected_block_comment_end);
      return formToken(tok::unknown, TokStart);
    }
  }

  if (leftBound == rightBound)
    return formToken(tok::oper_binary, TokStart);

  return formToken(leftBound ? tok::oper_postfix : tok::oper_prefix, TokStart);
}

/// lexDollarIdent - Match $[0-9a-zA-Z_$]*
void Lexer::lexDollarIdent() {
  const char *TokStart = CurPtr-1;
  assert(*TokStart == '$');
  
  // Lex [a-zA-Z_$0-9]*
  while (isalnum(*CurPtr) || *CurPtr == '_' || *CurPtr == '$')
    ++CurPtr;
  
  return formStartingToken(tok::dollarident, TokStart);
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

static bool isxdigit(char c) {
  return isdigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

void Lexer::lexHexNumber() {
  // We assume we're starting from the 'x' in a '0x...' floating-point literal.
  assert(*CurPtr == 'x' && "not a hex literal");
  const char *TokStart = CurPtr-1;
  assert(*TokStart == '0' && "not a hex literal");

  // 0x[0-9a-fA-F]+
  ++CurPtr;
  while (isxdigit(*CurPtr))
    ++CurPtr;
  if (CurPtr - TokStart == 2) {
    diagnose(CurPtr, diag::lex_expected_digit_in_int_literal);
    return formToken(tok::unknown, TokStart);
  }
  
  if (*CurPtr != '.' && *CurPtr != 'p' && *CurPtr != 'P')
    return formStartingToken(tok::integer_literal, TokStart);
  
  // (\.[0-9A-Fa-f]+)?
  if (*CurPtr == '.') {
    ++CurPtr;
    
    // If the character after the '.' is not a digit, assume we have an int
    // literal followed by a dot expression.
    if (!isxdigit(*CurPtr)) {
      --CurPtr;
      return formStartingToken(tok::integer_literal, TokStart);
    }
    
    while (isxdigit(*CurPtr))
      ++CurPtr;
    if (*CurPtr != 'p' && *CurPtr != 'P') {
      diagnose(CurPtr, diag::lex_expected_binary_exponent_in_hex_float_literal);
      return formToken(tok::unknown, TokStart);
    }
  }
  
  // [pP][+-]?[0-9]+
  assert(*CurPtr == 'p' || *CurPtr == 'P' && "not at a hex float exponent?!");
  ++CurPtr;
  
  if (*CurPtr == '+' || *CurPtr == '-')
    ++CurPtr;  // Eat the sign.

  if (!isdigit(*CurPtr)) {
    diagnose(CurPtr, diag::lex_expected_digit_in_fp_exponent);
    return formToken(tok::unknown, TokStart);
  }
  
  while (isdigit(*CurPtr))
    ++CurPtr;

  return formStartingToken(tok::floating_literal, TokStart);
}

/// lexNumber:
///   integer_literal  ::= [0-9]+
///   integer_literal  ::= 0x[0-9a-fA-F]+
///   integer_literal  ::= 0o[0-7]+
///   integer_literal  ::= 0b[01]+
///   floating_literal ::= [0-9]+\.[0-9]+
///   floating_literal ::= [0-9]+(\.[0-9]*)?[eE][+-]?[0-9]+
///   floating_literal ::= \.[0-9]+([eE][+-]?[0-9]+)?
///   floating_literal ::= 0x[0-9A-Fa-f]+(\.[0-9A-Fa-f]+)?[pP][+-]?[0-9]+
void Lexer::lexNumber() {
  const char *TokStart = CurPtr-1;
  assert((isdigit(*TokStart) || *TokStart == '.') && "Unexpected start");

  if (*TokStart == '0' && *CurPtr == 'x') {
    return lexHexNumber();
  } else if (*TokStart == '0' && *CurPtr == 'o') {
    // 0o[0-7]+
    ++CurPtr;
    while (*CurPtr >= '0' && *CurPtr <= '7')
      ++CurPtr;
    if (CurPtr - TokStart == 2) {
      diagnose(CurPtr, diag::lex_expected_digit_in_int_literal);
      return formToken(tok::unknown, TokStart);
    }
    return formStartingToken(tok::integer_literal, TokStart);
  } else if (*TokStart == '0' && *CurPtr == 'b') {
    // 0b[01]+
    ++CurPtr;
    while (*CurPtr == '0' || *CurPtr == '1')
      ++CurPtr;
    if (CurPtr - TokStart == 2) {
      diagnose(CurPtr, diag::lex_expected_digit_in_int_literal);
      return formToken(tok::unknown, TokStart);
    }
    return formStartingToken(tok::integer_literal, TokStart);
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
      return formStartingToken(tok::integer_literal, TokStart);
    
    // Lex things like 4.x as '4' followed by a tok::period.
    if (*CurPtr == '.' && !isdigit(CurPtr[1]) && !isValidExponent(CurPtr+1))
      return formStartingToken(tok::integer_literal, TokStart);
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
    if (*CurPtr == '+' || *CurPtr == '-')
      ++CurPtr;  // Eat the sign.
      
    if (!isdigit(*CurPtr)) {
      diagnose(CurPtr, diag::lex_expected_digit_in_fp_exponent);
      return formToken(tok::unknown, TokStart);
    }
    
    while (isdigit(*CurPtr))
      ++CurPtr;
  }
  
  return formStartingToken(tok::floating_literal, TokStart);
}

/// lexCharacter - Read a character and return its UTF32 code.  If this is the
/// end of enclosing string/character sequence, this returns ~0U.
/// 
///   character_escape  ::= [\][\] | [\]t | [\]n | [\]r | [\]" | [\]'
///   character_escape  ::= [\]x hex hex  
///   character_escape  ::= [\]u hex hex hex hex  
///   character_escape  ::= [\]U hex hex hex hex hex hex hex hex
///   hex               ::= [0-9a-fA-F]
unsigned Lexer::lexCharacter(const char *&CurPtr, bool StopAtDoubleQuote,
                             bool EmitDiagnostics) {
  const char *CharStart = CurPtr;

  switch (*CurPtr++) {
  default: {// Normal characters are part of the string.
    // If this is a "high" UTF-8 character, validate it.
    if ((signed char)(CurPtr[-1]) >= 0)
      return CurPtr[-1];
    --CurPtr;
    unsigned CharValue = validateUTF8CharacterAndAdvance(CurPtr);
    if (CharValue != ~0U) return CharValue;
    if (EmitDiagnostics)
      diagnose(CharStart, diag::lex_invalid_utf8_character);
    return 0;
  }
  case '"':
    // If we found the closing " character, we're done.
    if (StopAtDoubleQuote) {
      --CurPtr;
      return ~0U;
    }
    // In a single quoted string, this is just a character.
    return CurPtr[-1];
  case '\'':
    if (!StopAtDoubleQuote) {
      --CurPtr;  
      return ~0U;
    }
    // In a double quoted string, this is just a character.
    return CurPtr[-1];
      
  case 0:
    if (CurPtr-2 != BufferEnd) {
      if (EmitDiagnostics)
        diagnose(CurPtr-2, diag::lex_nul_character);
      return 0;
    }
    // FALL THROUGH.
  case '\n':  // String literals cannot have \n or \r in them.
  case '\r':
    --CurPtr;
    return ~0U;
  case '\\':  // Escapes.
    break;
  }
  
  unsigned CharValue = 0;
  // Escape processing.  We already ate the "\".
  switch (*CurPtr) {
  default:  // Invalid escape.
    diagnose(CurPtr, diag::lex_invalid_escape);
    return 0;
      
  // Simple single-character escapes.
  case '0': ++CurPtr; return '\0';
  case 'a': ++CurPtr; return '\a';
  case 'b': ++CurPtr; return '\b';
  case 'f': ++CurPtr; return '\f';
  case 'n': ++CurPtr; return '\n';
  case 'r': ++CurPtr; return '\r';
  case 't': ++CurPtr; return '\t';
  case 'v': ++CurPtr; return '\v';
  case '"': ++CurPtr; return '"';
  case '\'': ++CurPtr; return '\'';
  case '\\': ++CurPtr; return '\\';
  // Unicode escapes of various lengths.
  case 'x':  //  \x HEX HEX
    if (!isxdigit(CurPtr[1]) || !isxdigit(CurPtr[2])) {
      if (EmitDiagnostics)
        diagnose(CurPtr, diag::lex_invalid_x_escape);
      return 0;
    }
    
    StringRef(CurPtr+1, 2).getAsInteger(16, CharValue);
    
    // Reject \x80 and above, since it is going to encode into a multibyte
    // unicode encoding, which is something that C folks may not expect.
    if (CharValue >= 0x80)
      diagnose(CurPtr, diag::lex_invalid_hex_escape);
    
    CurPtr += 3;
    break;
    
  case 'u':  //  \u HEX HEX HEX HEX 
    if (!isxdigit(CurPtr[1]) || !isxdigit(CurPtr[2]) ||
        !isxdigit(CurPtr[3]) || !isxdigit(CurPtr[4])) {
      if (EmitDiagnostics)
        diagnose(CurPtr, diag::lex_invalid_u_escape);
      return 0;
    }
    
    StringRef(CurPtr+1, 4).getAsInteger(16, CharValue);
    CurPtr += 5;
    break;
  case 'U':  //  \U HEX HEX HEX HEX HEX HEX HEX HEX 
    if (!isxdigit(CurPtr[1]) || !isxdigit(CurPtr[2]) || 
        !isxdigit(CurPtr[3]) || !isxdigit(CurPtr[4]) ||
        !isxdigit(CurPtr[5]) || !isxdigit(CurPtr[6]) || 
        !isxdigit(CurPtr[7]) || !isxdigit(CurPtr[8])) {
      if (EmitDiagnostics)
        diagnose(CurPtr, diag::lex_invalid_U_escape);
      return 0;
    }
    StringRef(CurPtr+1, 8).getAsInteger(16, CharValue);
    CurPtr += 9;
    break;
  }
  
  // Check to see if the encoding is valid.
  llvm::SmallString<64> TempString;
  if (CharValue >= 0x80 && EncodeToUTF8(CharValue, TempString)) {
    if (EmitDiagnostics)
      diagnose(CharStart, diag::lex_invalid_unicode_code_point);
    return 0;
  }
  
  return CharValue;
}


/// lexCharacterLiteral:
///   character_literal ::= '([^'\\\n\r]|character_escape)'
void Lexer::lexCharacterLiteral() {
  const char *TokStart = CurPtr-1;
  assert(*TokStart == '\'' && "Unexpected start");
  
  unsigned CharValue = lexCharacter(CurPtr, false, true);
    
  // If this wasn't a normal character, then this is a malformed character.
  if (CharValue == ~0U) {
    diagnose(TokStart, diag::lex_invalid_character_literal);
    return formToken(tok::unknown, TokStart);
  }
    
  if (*CurPtr != '\'') {
    diagnose(TokStart, diag::lex_invalid_character_literal);
    return formToken(tok::unknown, TokStart);;
  }
  ++CurPtr;
  return formStartingToken(tok::character_literal, TokStart);
}

/// getEncodedCharacterLiteral - Return the UTF32 codepoint for the specified
/// character literal.
uint32_t Lexer::getEncodedCharacterLiteral(const Token &Str) {
  const char *CharStart = Str.getText().data()+1;
  return lexCharacter(CharStart, false, false);
}

/// skipToEndOfInterpolatedExpression - Given the first character after a \(
/// sequence in a string literal (the start of an interpolated expression), 
/// scan forward to the end of the interpolated expression and return the end.
/// On success, the returned pointer will point to a ')'.  On failure, it will
/// point to something else.  This basically just does brace matching.
static const char *skipToEndOfInterpolatedExpression(const char *CurPtr,
                                                     Lexer *L) {
  SourceLoc InterpStart = Lexer::getSourceLoc(CurPtr-1);
  unsigned ParenCount = 1;
  while (true) {
    // This is a very simple scanner.  The implications of this include not
    // being able to use string literals in an interpolated string, and not
    // being able to break an expression over multiple lines in an interpolated
    // string.  Both of these limitations make this simple and allow us to
    // recover from common errors though.
    //
    // On success scanning the expression body, the real lexer will be used to
    // relex the body when parsing the expressions.  We let it diagnose any
    // issues with malformed tokens or other problems.
    switch (*CurPtr++) {
    // String literals in general cannot be split across multiple lines,
    // interpolated ones are no exception.
    case '\n':
    case '\r':
      // Will be diagnosed as an unterminated string literal.
      return CurPtr-1;

    // String literals cannot be used in interpolated string literals.
    case '"':
      L->diagnose(CurPtr - 1, diag::lex_unexpected_quote_string_interpolation)
        << SourceRange(InterpStart, Lexer::getSourceLoc(CurPtr-1));
      return CurPtr-1;
    case 0:
      // If we hit EOF, we fail.
      if (CurPtr-1 == L->getBufferEnd()) {
        L->diagnose(CurPtr-1, diag::lex_unterminated_string);
        return CurPtr-1;
      }
      continue;
        
    // Paren nesting deeper to support "foo = \((a+b)-(c*d)) bar".
    case '(':
      ++ParenCount;
      continue;
    case ')':
      // If this is the last level of nesting, then we're done!
      if (--ParenCount == 0)
        return CurPtr-1;
      continue;
    default:
      // Normal token character.
      continue;
    }
  }
}

/// lexStringLiteral:
///   string_literal ::= ["]([^"\\\n\r]|character_escape)*["]
void Lexer::lexStringLiteral() {
  const char *TokStart = CurPtr-1;
  assert(*TokStart == '"' && "Unexpected start");

  bool wasErroneous = false;
  
  while (1) {
    if (*CurPtr == '\\' && *(CurPtr + 1) == '(') {
      // Consume tokens until we hit the corresponding ')'.
      CurPtr += 2;
      const char *EndPtr = skipToEndOfInterpolatedExpression(CurPtr, this);
      
      if (*EndPtr == ')') {
        // Successfully scanned the body of the expression literal.
        CurPtr = EndPtr+1;
      } else {
        wasErroneous = true;
      }
      continue;
    }
    
    unsigned CharValue = lexCharacter(CurPtr, true, true);
    
    // If this is a normal character, just munch it.
    if (CharValue != ~0U)
      continue;
    
    switch (*CurPtr) {
    default: assert(0 && "Unknown reason to stop lexing character");
    // If we found the closing " character, we're done.
    case '"':
      ++CurPtr;
      if (wasErroneous) return formToken(tok::unknown, TokStart);
      return formStartingToken(tok::string_literal, TokStart);
    case 0:
    case '\n':  // String literals cannot have \n or \r in them.
    case '\r':
      diagnose(TokStart, diag::lex_unterminated_string);
      return formToken(tok::unknown, TokStart);
    }
  }
}

/// getEncodedStringLiteral - Given a string literal token, return the bytes
/// that the actual string literal should codegen to.  If a copy needs to be
/// made, it will be allocated out of the ASTContext allocator.
void Lexer::getEncodedStringLiteral(const Token &Str, ASTContext &Ctx,
              llvm::SmallVectorImpl<StringSegment> &Segments) {
  // Get the bytes behind the string literal, dropping the double quotes.
  StringRef Bytes = Str.getText().drop_front().drop_back();
  llvm::SmallString<64> TempString;

  // Note that it is always safe to read one over the end of "Bytes" because
  // we know that there is a terminating " character.  Use BytesPtr to avoid a
  // range check subscripting on the StringRef.
  const char *BytesPtr = Bytes.begin();
  while (BytesPtr != Bytes.end()) {
    char CurChar = *BytesPtr++;
    if (CurChar != '\\') {
      TempString += CurChar;
      continue;
    }
    
    // Invalid escapes are accepted by the lexer but diagnosed as an error.  We
    // just ignore them here.
    unsigned CharValue; // Unicode character value for \x, \u, \U.
    switch (*BytesPtr++) {
    default:
      continue;   // Invalid escape, ignore it.
          
      // Simple single-character escapes.
    case 'a': TempString += '\a'; continue;
    case 'b': TempString += '\b'; continue;
    case 'f': TempString += '\f'; continue;
    case 'n': TempString += '\n'; continue;
    case 'r': TempString += '\r'; continue;
    case 't': TempString += '\t'; continue;
    case 'v': TempString += '\v'; continue;
    case '"': TempString += '"'; continue;
    case '\'': TempString += '\''; continue;
    case '\\': TempString += '\\'; continue;
      
        
    // String interpolation.
    case '(': {
      // Flush the current string.
      if (!TempString.empty()) {
        auto Res = Ctx.AllocateCopy(TempString);
        Segments.push_back(StringSegment::getLiteral(StringRef(Res.data(),
                                                               Res.size())));
        TempString.clear();
      }
      
      // Find the closing ')'.
      const char *End = skipToEndOfInterpolatedExpression(BytesPtr, this);
      assert(*End == ')' && "invalid string literal interpolations should"
             " not be returned as string literals");
      ++End;
      
      // Add an expression segment.
      Segments.push_back(
                 StringSegment::getExpr(StringRef(BytesPtr, End-BytesPtr-1)));
      
      // Reset the input bytes to the string that remains to be consumed.
      Bytes = StringRef(End, Bytes.end() - End);
      BytesPtr = End;
      continue;
    }
        
      // Unicode escapes of various lengths.
    case 'x':  //  \x HEX HEX
      if (!isxdigit(BytesPtr[0]) || !isxdigit(BytesPtr[1]))
        continue;  // Ignore invalid escapes.
      
      StringRef(BytesPtr, 2).getAsInteger(16, CharValue);
      BytesPtr += 2;
      break;
    case 'u':  //  \u HEX HEX HEX HEX 
      if (!isxdigit(BytesPtr[0]) || !isxdigit(BytesPtr[1]) ||
          !isxdigit(BytesPtr[2]) || !isxdigit(BytesPtr[3]))
        continue;  // Ignore invalid escapes.
      
      StringRef(BytesPtr, 4).getAsInteger(16, CharValue);
      BytesPtr += 4;
      break;
    case 'U':  //  \U HEX HEX HEX HEX HEX HEX HEX HEX 
      if (!isxdigit(BytesPtr[0]) || !isxdigit(BytesPtr[1]) ||
          !isxdigit(BytesPtr[2]) || !isxdigit(BytesPtr[3]) ||
          !isxdigit(BytesPtr[4]) || !isxdigit(BytesPtr[5]) ||
          !isxdigit(BytesPtr[6]) || !isxdigit(BytesPtr[7]))
        continue;  // Ignore invalid escapes.
      
      StringRef(BytesPtr, 8).getAsInteger(16, CharValue);
      BytesPtr += 8;
      break;
    }
    
    if (CharValue < 0x80) 
      TempString += (char)CharValue;
    else
      EncodeToUTF8(CharValue, TempString);
  }
  
  // If we didn't escape or reprocess anything, then we don't need to reallocate
  // a copy of the string, just point to the lexer's version.  We know that this
  // is safe because unescaped strings are always shorter than their escaped
  // forms (in a valid string).
  if (Segments.empty() && TempString.size() == Bytes.size())
    Segments.push_back(StringSegment::getLiteral(Bytes));
  else if (Segments.empty() || !TempString.empty()) {
    auto Res = Ctx.AllocateCopy(TempString);
    Segments.push_back(StringSegment::getLiteral(StringRef(Res.data(),
                                                           Res.size())));
  }
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
    if (CurPtr[-2] != '\n' && CurPtr[-2] != '\r') {
      // While we are not C, we should not ignore the strong Unix command-line
      // tool conventions that motivate this warning.
      diagnose(CurPtr-1, diag::lex_missing_newline_eof);
    }
    return formToken(tok::eof, TokStart);

  case '{': return formStartingToken(tok::l_brace, TokStart);
  case '[': return formStartingToken(tok::l_square_starting, TokStart,
                                     tok::l_square_following);
  case '(': return formStartingToken(tok::l_paren_starting, TokStart,
                                     tok::l_paren_following);
  case '}': return formToken(tok::r_brace,  TokStart);
  case ']': return formToken(tok::r_square, TokStart);
  case ')':
    // When lexing an interpolated string literal, the buffer will terminate
    // with a ')'.
    if (CurPtr-1 == BufferEnd)
      return formToken(tok::eof, TokStart);
    return formToken(tok::r_paren,  TokStart);

  case ',': return formToken(tok::comma,    TokStart);
  case ';': return formToken(tok::semi,     TokStart);
  case ':': return formToken(tok::colon,    TokStart);
      
  // Operator characters.
  case '/':
    if (CurPtr[0] == '/') {  // "//"
      skipSlashSlashComment();
      goto Restart;
    }
    if (CurPtr[0] == '*') { // "/*"
      skipSlashStarComment();
      goto Restart;
    }
    // FALL THROUGH
  case '=': case '-': case '+': case '*': case '%': case '<': case '>':
  case '!': case '&': case '|': case '^': case '~': case '.':
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

  case '\'':
    return lexCharacterLiteral();
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

