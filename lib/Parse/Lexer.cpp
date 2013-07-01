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
#include "swift/Basic/Fallthrough.h"
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
  unsigned NumBits = 32-llvm::countLeadingZeros(CharValue);
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
static uint32_t validateUTF8CharacterAndAdvance(const char *&Ptr,
                                                const char *End) {
  if (Ptr >= End)
    return ~0U;
  
  unsigned char CurByte = *Ptr++;
  if (CurByte < 0x80)
    return CurByte;
  
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
    while (Ptr < End && !isStartOfUTF8Character(*Ptr))
      ++Ptr;
    return ~0U;
  }
  
  // Drop the high bits indicating the # bytes of the result.
  unsigned CharValue = (unsigned char)(CurByte << EncodedBytes) >> EncodedBytes;
  
  // Read and validate the continuation bytes.
  for (unsigned i = 1; i != EncodedBytes; ++i) {
    if (Ptr >= End)
      return ~0U;
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
  unsigned NumBits = 32-llvm::countLeadingZeros(CharValue);
  
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
             DiagnosticEngine *Diags, const char *CurrentPosition,
             bool InSILMode)
  : SourceMgr(SourceMgr), Diags(Diags), InSILMode(InSILMode) {
  BufferStart = Buffer.begin();
  BufferEnd = Buffer.end();
  ArtificialEOF = BufferEnd;
  CurPtr = CurrentPosition;
  assert(CurPtr >= BufferStart && CurPtr <= BufferEnd &&
         "Current position is out-of-range");
    
  // Prime the lexer.
  lexImpl();
  assert((NextToken.isAtStartOfLine() || CurrentPosition != BufferStart) &&
         "The token should be at the beginning of the line, "
         "or we should be lexing from the middle of the buffer");
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
          Text.data(), /*not SIL mode*/ false);
  Token Result;
  L.lex(Result);
  return Result.getKind();
}

void Lexer::formToken(tok Kind, const char *TokStart) {
  // When we are lexing a subrange from the middle of a file buffer, we will
  // run past the end of the range, but will stay within the file.  Check if
  // we are past the imaginary EOF, and synthesize a tok::eof in this case.
  if (Kind != tok::eof && TokStart >= ArtificialEOF) {
    formToken(tok::eof, TokStart);
    return;
  }
  NextToken.setToken(Kind, StringRef(TokStart, CurPtr-TokStart));
}

//===----------------------------------------------------------------------===//
// Lexer Subroutines
//===----------------------------------------------------------------------===//

static void diagnoseEmbeddedNul(DiagnosticEngine *Diags, const char *Ptr) {
  assert(Ptr && "invalid source location");
  assert(*Ptr == '\0' && "not an embedded null");

  if (!Diags)
    return;

  SourceLoc NulLoc = Lexer::getSourceLoc(Ptr);
  SourceLoc NulEndLoc = Lexer::getSourceLoc(Ptr+1);
  Diags->diagnose(NulLoc, diag::lex_nul_character)
    .fixItRemove(DiagnosticInfo::Range(NulLoc, NulEndLoc));
}

/// skipSlashSlashComment - Skip to the end of the line of a // comment.
void Lexer::skipSlashSlashComment() {
  assert(CurPtr[-1] == '/' && CurPtr[0] == '/' && "Not a // comment");
  while (1) {
    switch (*CurPtr++) {
    case '\n':
    case '\r':
      NextToken.setAtStartOfLine(true);
      return;  // If we found the end of the line, return.
    default:
      // If this is a "high" UTF-8 character, validate it.
      if ((signed char)(CurPtr[-1]) < 0) {
        --CurPtr;
        const char *CharStart = CurPtr;
        if (validateUTF8CharacterAndAdvance(CurPtr, BufferEnd) == ~0U)
          diagnose(CharStart, diag::lex_invalid_utf8_character);
      }
      break;   // Otherwise, eat other characters.
    case 0:
      // If this is a random nul character in the middle of a buffer, skip it as
      // whitespace.
      if (CurPtr-1 != BufferEnd) {
        diagnoseEmbeddedNul(Diags, CurPtr-1);
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

    case '\n':
    case '\r':
      NextToken.setAtStartOfLine(true);
      break;

    default:
      // If this is a "high" UTF-8 character, validate it.
      if ((signed char)(CurPtr[-1]) < 0) {
        --CurPtr;
        const char *CharStart = CurPtr;
        if (validateUTF8CharacterAndAdvance(CurPtr, BufferEnd) == ~0U)
          diagnose(CharStart, diag::lex_invalid_utf8_character);
      }

      break;   // Otherwise, eat other characters.
    case 0:
      // If this is a random nul character in the middle of a buffer, skip it as
      // whitespace.
      if (CurPtr-1 != BufferEnd) {
        diagnoseEmbeddedNul(Diags, CurPtr-1);
        break;
      }
      
      // Otherwise, we have an unterminated /* comment.
      --CurPtr;

      // Count how many levels deep we are.
      llvm::SmallString<8> Terminator("*/");
      while (--Depth != 0)
        Terminator += "*/";

      const char *EOL = (CurPtr[-1] == '\n') ? (CurPtr - 1) : CurPtr;
      diagnose(EOL, diag::lex_unterminated_block_comment)
        .fixItInsert(getSourceLoc(EOL), Terminator);
      diagnose(StartPtr, diag::lex_comment_start);
      return;
    }
  }
}

static bool isValidIdentifierContinuationCodePoint(uint32_t c) {
  if (c < 0x80)
    return isalnum(c) || c == '_' || c == '$';
  
  // N1518: Recommendations for extended identifier characters for C and C++
  // Proposed Annex X.1: Ranges of characters allowed
  return c == 0x00A8 || c == 0x00AA || c == 0x00AD || c == 0x00AF
    || (c >= 0x00B2 && c <= 0x00B5) || (c >= 0x00B7 && c <= 0x00BA)
    || (c >= 0x00BC && c <= 0x00BE) || (c >= 0x00C0 && c <= 0x00D6)
    || (c >= 0x00D8 && c <= 0x00F6) || (c >= 0x00F8 && c <= 0x00FF)
  
    || (c >= 0x0100 && c <= 0x167F)
    || (c >= 0x1681 && c <= 0x180D)
    || (c >= 0x180F && c <= 0x1FFF)
  
    || (c >= 0x200B && c <= 0x200D)
    || (c >= 0x202A && c <= 0x202E)
    || (c >= 0x203F && c <= 0x2040)
    || c == 0x2054
    || (c >= 0x2060 && c <= 0x206F)
  
    || (c >= 0x2070 && c <= 0x218F)
    || (c >= 0x2460 && c <= 0x24FF)
    || (c >= 0x2776 && c <= 0x2793)
    || (c >= 0x2C00 && c <= 0x2DFF)
    || (c >= 0x2E80 && c <= 0x2FFF)
  
    || (c >= 0x3004 && c <= 0x3007)
    || (c >= 0x3021 && c <= 0x302F)
    || (c >= 0x3031 && c <= 0x303F)
  
    || (c >= 0x3040 && c <= 0xD7FF)
  
    || (c >= 0xF900 && c <= 0xFD3D)
    || (c >= 0xFD40 && c <= 0xFDCF)
    || (c >= 0xFDF0 && c <= 0xFE44)
    || (c >= 0xFE47 && c <= 0xFFFD)
  
    || (c >= 0x10000 && c <= 0x1FFFD)
    || (c >= 0x20000 && c <= 0x2FFFD)
    || (c >= 0x30000 && c <= 0x3FFFD)
    || (c >= 0x40000 && c <= 0x4FFFD)
    || (c >= 0x50000 && c <= 0x5FFFD)
    || (c >= 0x60000 && c <= 0x6FFFD)
    || (c >= 0x70000 && c <= 0x7FFFD)
    || (c >= 0x80000 && c <= 0x8FFFD)
    || (c >= 0x90000 && c <= 0x9FFFD)
    || (c >= 0xA0000 && c <= 0xAFFFD)
    || (c >= 0xB0000 && c <= 0xBFFFD)
    || (c >= 0xC0000 && c <= 0xCFFFD)
    || (c >= 0xD0000 && c <= 0xDFFFD)
    || (c >= 0xE0000 && c <= 0xEFFFD);
}
static bool isValidIdentifierStartCodePoint(uint32_t c) {
  if (!isValidIdentifierContinuationCodePoint(c))
    return false;
  if (c < 0x80 && (isnumber(c) || c == '$'))
    return false;

  // N1518: Recommendations for extended identifier characters for C and C++
  // Proposed Annex X.2: Ranges of characters disallowed initially
  if ((c >= 0x0300 && c <= 0x036F)
      || (c >= 0x1DC0 && c <= 0x1DFF)
      || (c >= 0x20D0 && c <= 0x20FF)
      || (c >= 0xFE20 && c <= 0xFE2F))
    return false;
  
  return true;
}

static bool advanceIf(char const *&ptr,
                      char const *end,
                      bool (*predicate)(uint32_t)) {
  char const *next = ptr;
  uint32_t c = validateUTF8CharacterAndAdvance(next, end);
  if (c == ~0U)
    return false;
  if (predicate(c)) {
    ptr = next;
    return true;
  }
  return false;

}

static bool advanceIfValidStartOfIdentifier(char const *&ptr,
                                            char const *end) {
  return advanceIf(ptr, end, isValidIdentifierStartCodePoint);
}

static bool advanceIfValidContinuationOfIdentifier(char const *&ptr,
                                                   char const *end) {
  return advanceIf(ptr, end, isValidIdentifierContinuationCodePoint);
}

static bool advanceIfValidStartOfOperator(char const *&ptr,
                                          char const *end) {
  return advanceIf(ptr, end, Identifier::isOperatorStartCodePoint);
}

static bool advanceIfValidContinuationOfOperator(char const *&ptr,
                                                 char const *end) {
  return advanceIf(ptr, end, Identifier::isOperatorContinuationCodePoint);
}

/// isIdentifier - Checks whether a string matches the identifier regex.
bool Lexer::isIdentifier(llvm::StringRef string) {
  if (string.empty()) return false;
  char const *p = string.data(), *end = string.end();
  if (!advanceIfValidStartOfIdentifier(p, end))
    return false;
  while (p < end && advanceIfValidContinuationOfIdentifier(p, end));
  return p == end;
}

/// lexIdentifier - Match [a-zA-Z_][a-zA-Z_$0-9]*
///
/// FIXME: We should also allow unicode characters in identifiers.
void Lexer::lexIdentifier() {
  const char *TokStart = CurPtr-1;
  CurPtr = TokStart;
  bool didStart = advanceIfValidStartOfIdentifier(CurPtr, BufferEnd);
  assert(didStart && "Unexpected start");
  (void) didStart;

  // Lex [a-zA-Z_$0-9[[:XID_Continue:]]]*
  while (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd));

  tok Kind =
  llvm::StringSwitch<tok>(StringRef(TokStart, CurPtr-TokStart))
#define KEYWORD(kw) \
    .Case(#kw, tok::kw_##kw)
#include "swift/Parse/Tokens.def"
    .Default(tok::identifier);

  // "sil" is only a keyword in SIL mode.
  if (Kind == tok::kw_sil && !InSILMode)
    Kind = tok::identifier;
  
  return formToken(Kind, TokStart);
}

/// Is the operator beginning at the given character "left-bound"?
static bool isLeftBound(const char *tokBegin, const char *bufferBegin) {
  // The first character in the file is not left-bound.
  if (tokBegin == bufferBegin) return false;

  switch (tokBegin[-1]) {
  case ' ': case '\r': case '\n': case '\t': // whitespace
  case '(': case '[': case '{':              // opening delimiters
  case ',': case ';': case ':':              // expression separators
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
  case ',': case ';': case ':':              // expression separators
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
    CurPtr = TokStart;
    bool didStart = advanceIfValidStartOfOperator(CurPtr, BufferEnd);
    assert(didStart && "unexpected operator start");
    (void) didStart;
    
    while (advanceIfValidContinuationOfOperator(CurPtr, BufferEnd));
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
      if (leftBound != rightBound)
        diagnose(TokStart, diag::lex_unary_equal_is_reserved);
      // always emit 'tok::equal' to avoid trickle down parse errors
      return formToken(tok::equal, TokStart);
    case '&':
      if (leftBound == rightBound || leftBound)
        break;
      return formToken(tok::amp_prefix, TokStart);
    case '.':
      if (leftBound == rightBound)
        return formToken(tok::period, TokStart);
      if (rightBound)
        return formToken(tok::period_prefix, TokStart);
      diagnose(TokStart, diag::lex_unary_postfix_dot_is_reserved);
      // always emit 'tok::period' to avoid trickle down parse errors
      return formToken(tok::period, TokStart);
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

  // In a SIL function body, '$' is a token by itself.
  if (InSILBody)
    return formToken(tok::sil_dollar, TokStart);

  // Lex [a-zA-Z_$0-9]*
  while (isalnum(*CurPtr) || *CurPtr == '_' || *CurPtr == '$')
    ++CurPtr;
  
  return formToken(tok::dollarident, TokStart);
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
    while (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd));
    return formToken(tok::unknown, TokStart);
  }
  
  if (*CurPtr != '.' && *CurPtr != 'p' && *CurPtr != 'P')
    return formToken(tok::integer_literal, TokStart);
  
  // (\.[0-9A-Fa-f]+)?
  if (*CurPtr == '.') {
    ++CurPtr;
    
    // If the character after the '.' is not a digit, assume we have an int
    // literal followed by a dot expression.
    if (!isxdigit(*CurPtr)) {
      --CurPtr;
      return formToken(tok::integer_literal, TokStart);
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

  return formToken(tok::floating_literal, TokStart);
}

/// lexNumber:
///   integer_literal  ::= [0-9]+
///   integer_literal  ::= 0x[0-9a-fA-F]+
///   integer_literal  ::= 0o[0-7]+
///   integer_literal  ::= 0b[01]+
///   floating_literal ::= [0-9]+\.[0-9]+
///   floating_literal ::= [0-9]+\.[0-9]+[eE][+-]?[0-9]+
///   floating_literal ::= [0-9][eE][+-]?[0-9]+
///   floating_literal ::= 0x[0-9A-Fa-f]+(\.[0-9A-Fa-f]+)?[pP][+-]?[0-9]+
void Lexer::lexNumber() {
  const char *TokStart = CurPtr-1;
  assert((isdigit(*TokStart) || *TokStart == '.') && "Unexpected start");

  if (*TokStart == '0' && *CurPtr == 'x')
    return lexHexNumber();
  
  if (*TokStart == '0' && *CurPtr == 'o') {
    // 0o[0-7]+
    ++CurPtr;
    while (*CurPtr >= '0' && *CurPtr <= '7')
      ++CurPtr;
    if (CurPtr - TokStart == 2) {
      diagnose(CurPtr, diag::lex_expected_digit_in_int_literal);
      while (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd));
      return formToken(tok::unknown, TokStart);
    }
    return formToken(tok::integer_literal, TokStart);
  }
  
  if (*TokStart == '0' && *CurPtr == 'b') {
    // 0b[01]+
    ++CurPtr;
    while (*CurPtr == '0' || *CurPtr == '1')
      ++CurPtr;
    if (CurPtr - TokStart == 2) {
      diagnose(CurPtr, diag::lex_expected_digit_in_int_literal);
      while (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd));
      return formToken(tok::unknown, TokStart);
    }
    return formToken(tok::integer_literal, TokStart);
  }

  // Handle a leading [0-9]+, lexing an integer or falling through if we have a
  // floating point value.
  while (isdigit(*CurPtr))
    ++CurPtr;

  // Lex things like 4.x as '4' followed by a tok::period.
  if (*CurPtr == '.') {
    // NextToken is the soon to be previous token
    // Therefore: x.0.1 is sub-tuple access, not x.float_literal
    if (!isdigit(CurPtr[1]) || NextToken.is(tok::period))
      return formToken(tok::integer_literal, TokStart);
  } else {
    // Floating literals must have '.', 'e', or 'E' after digits.  If it is
    // something else, then this is the end of the token.
    if (*CurPtr != 'e' && *CurPtr != 'E') {
      char const *tmp = CurPtr;
      if (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd)) {
        diagnose(tmp, diag::lex_expected_digit_in_int_literal);
        while (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd));
        return formToken(tok::unknown, TokStart);
      }
      return formToken(tok::integer_literal, TokStart);
    }
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
      while (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd));
      return formToken(tok::unknown, TokStart);
    }
    
    while (isdigit(*CurPtr))
      ++CurPtr;
  }
  
  return formToken(tok::floating_literal, TokStart);
}

/// lexCharacter - Read a character and return its UTF32 code.  If this is the
/// end of enclosing string/character sequence, this returns ~0U.  If this is a
/// malformed character sequence, it emits a diagnostic (when EmitDiagnostics is
/// true) and returns ~1U.
/// 
///   character_escape  ::= [\][\] | [\]t | [\]n | [\]r | [\]" | [\]' | [\]0
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
    if ((signed char)(CurPtr[-1]) >= 0) {
      if (isprint(CurPtr[-1]) == 0)
        diagnose(CharStart, diag::lex_unprintable_ascii_character);
      return CurPtr[-1];
    }
    --CurPtr;
    unsigned CharValue = validateUTF8CharacterAndAdvance(CurPtr, BufferEnd);
    if (CharValue != ~0U) return CharValue;
    if (EmitDiagnostics)
      diagnose(CharStart, diag::lex_invalid_utf8_character);
    return ~1U;
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
    SWIFT_FALLTHROUGH;
  case '\n':  // String literals cannot have \n or \r in them.
  case '\r':
    diagnose(CurPtr-1, diag::lex_unterminated_string);
    return ~1U;
  case '\\':  // Escapes.
    break;
  }
  
  unsigned CharValue = 0;
  // Escape processing.  We already ate the "\".
  switch (*CurPtr) {
  default:  // Invalid escape.
    diagnose(CurPtr, diag::lex_invalid_escape);
    // If this looks like a plausible escape character, recover as though this
    // is an invalid escape.
    if (isalnum(*CurPtr)) ++CurPtr;
    return ~1U;
      
  // Simple single-character escapes.
  case '0': ++CurPtr; return '\0';
  case 'n': ++CurPtr; return '\n';
  case 'r': ++CurPtr; return '\r';
  case 't': ++CurPtr; return '\t';
  case '"': ++CurPtr; return '"';
  case '\'': ++CurPtr; return '\'';
  case '\\': ++CurPtr; return '\\';
  // Unicode escapes of various lengths.
  case 'x': {  //  \x HEX HEX
    unsigned ValidChars = !!isxdigit(CurPtr[1]) + !!isxdigit(CurPtr[2]);
    if (ValidChars != 2) {
      if (EmitDiagnostics)
        diagnose(CurPtr, diag::lex_invalid_x_escape);
      CurPtr += 1 + ValidChars;
      return ~1U;
    }

    StringRef(CurPtr+1, ValidChars).getAsInteger(16, CharValue);
    CurPtr += 1 + ValidChars;

    // Reject \x80 and above, since it is going to encode into a multibyte
    // unicode encoding, which is something that C folks may not expect.
    if (CharValue >= 0x80) {
      if (EmitDiagnostics)
        diagnose(CurPtr, diag::lex_invalid_hex_escape);
      return ~1U;
    }

    break;
  }
  case 'u': {  //  \u HEX HEX HEX HEX 
    unsigned ValidChars = !!isxdigit(CurPtr[1]) + !!isxdigit(CurPtr[2])
                        + !!isxdigit(CurPtr[3]) + !!isxdigit(CurPtr[4]);
    StringRef(CurPtr+1, ValidChars).getAsInteger(16, CharValue);

    if (ValidChars != 4) {
      if (EmitDiagnostics)
        diagnose(CurPtr, diag::lex_invalid_u_escape);
      CurPtr += 1 + ValidChars;
      return ~1U;
    }
    CurPtr += 1 + ValidChars;
    break;
  }
  case 'U': {  //  \U HEX HEX HEX HEX HEX HEX HEX HEX 
    unsigned ValidChars = !!isxdigit(CurPtr[1]) + !!isxdigit(CurPtr[2])
                        + !!isxdigit(CurPtr[3]) + !!isxdigit(CurPtr[4])
                        + !!isxdigit(CurPtr[5]) + !!isxdigit(CurPtr[6])
                        + !!isxdigit(CurPtr[7]) + !!isxdigit(CurPtr[8]);
    StringRef(CurPtr+1, ValidChars).getAsInteger(16, CharValue);

    if (ValidChars != 8) {
      if (EmitDiagnostics)
        diagnose(CurPtr, diag::lex_invalid_U_escape);
      CurPtr += 1 + ValidChars;
      return ~1U;
    }

    CurPtr += 1 + ValidChars;
    break;
  }
  }

  // Check to see if the encoding is valid.
  llvm::SmallString<64> TempString;
  if (CharValue >= 0x80 && EncodeToUTF8(CharValue, TempString)) {
    if (EmitDiagnostics)
      diagnose(CharStart, diag::lex_invalid_unicode_code_point);
    return ~1U;
  }
  
  return CharValue;
}


/// lexCharacterLiteral:
///   character_literal ::= '([^'\\\n\r]|character_escape)'
void Lexer::lexCharacterLiteral() {
  const char *TokStart = CurPtr-1;
  assert(*TokStart == '\'' && "Unexpected start");
  
  unsigned CharValue = lexCharacter(CurPtr, false, true);

  // If we have '', we have an invalid character literal.
  if (CharValue == ~0U) {
    diagnose(TokStart, diag::lex_invalid_character_literal);
    return formToken(tok::unknown, TokStart);
  }

  // If this wasn't a normal character, then this is a malformed character.
  if (CharValue == ~1U) {
    // If we successfully skipped to the ending ', eat it now to avoid confusing
    // error recovery.
    if (*CurPtr == '\'') ++CurPtr;
    return formToken(tok::unknown, TokStart);
  }

  if (*CurPtr != '\'') {
    diagnose(TokStart, diag::lex_invalid_character_literal);
    return formToken(tok::unknown, TokStart);
  }
  ++CurPtr;
  return formToken(tok::character_literal, TokStart);
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
        .highlight(Diagnostic::Range(InterpStart,
                                     Lexer::getSourceLoc(CurPtr-1)));
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

    // String literals cannot have \n or \r in them.
    if (*CurPtr == '\r' || *CurPtr == '\n') {
      diagnose(TokStart, diag::lex_unterminated_string);
      return formToken(tok::unknown, TokStart);
    }
    
    unsigned CharValue = lexCharacter(CurPtr, true, true);
    wasErroneous |= CharValue == ~1U;

    // If this is the end of string, we are done.  If it is a normal character
    // or an already-diagnosed error, just munch it.
    if (CharValue == ~0U) {
      ++CurPtr;
      if (wasErroneous) return formToken(tok::unknown, TokStart);
      return formToken(tok::string_literal, TokStart);
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
    unsigned CharValue = 0; // Unicode character value for \x, \u, \U.
    switch (*BytesPtr++) {
    default:
      continue;   // Invalid escape, ignore it.
          
      // Simple single-character escapes.
    case '0': TempString += '\0'; continue;
    case 'n': TempString += '\n'; continue;
    case 'r': TempString += '\r'; continue;
    case 't': TempString += '\t'; continue;
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

  NextToken.setAtStartOfLine(CurPtr == BufferStart);

Restart:
  // Remember the start of the token so we can form the text range.
  const char *TokStart = CurPtr;
  
  switch (*CurPtr++) {
  default: {
    char const *tmp = CurPtr-1;
    if (advanceIfValidStartOfIdentifier(tmp, BufferEnd))
      return lexIdentifier();
    
    if (advanceIfValidStartOfOperator(tmp, BufferEnd))
      return lexOperatorIdentifier();
    
    if (advanceIfValidContinuationOfIdentifier(tmp, BufferEnd)) {
      // If this is a valid identifier continuation, but not a valid identifier
      // start, attempt to recover by eating more continuation characters.
      diagnose(CurPtr-1, diag::lex_invalid_identifier_start_character);
      while (advanceIfValidContinuationOfIdentifier(tmp, BufferEnd));
    } else {
      // This character isn't allowed in Swift source.
      validateUTF8CharacterAndAdvance(tmp, BufferEnd);
      diagnose(CurPtr-1, diag::lex_invalid_character);
    }

    CurPtr = tmp;
    return formToken(tok::unknown, TokStart);
  }

  case '\n':
  case '\r':
    NextToken.setAtStartOfLine(true);
    goto Restart;  // Skip whitespace.

  case ' ':
  case '\t':
    goto Restart;  // Skip whitespace.
  case 0:
    // If this is a random nul character in the middle of a buffer, skip it as
    // whitespace.
    if (CurPtr-1 != BufferEnd) {
      diagnoseEmbeddedNul(Diags, CurPtr-1);
      goto Restart;
    }

    // Otherwise, this is the end of the buffer.  Return EOF.
    if (BufferStart != BufferEnd && CurPtr[-2] != '\n' && CurPtr[-2] != '\r') {
      // While we are not C, we should not ignore the strong Unix command-line
      // tool conventions that motivate this warning.
      diagnose(CurPtr-1, diag::lex_missing_newline_eof)
        .fixItInsert(getSourceLoc(CurPtr-1), "\n");
    }
    return formToken(tok::eof, TokStart);

  case '{': return formToken(tok::l_brace, TokStart);
  case '[': return formToken(tok::l_square, TokStart);
  case '(': return formToken(tok::l_paren, TokStart);
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
  case '?': return formToken(tok::question, TokStart);

  case '@':
    // @ is only a token in SIL mode.
    if (InSILMode)
      return formToken(tok::sil_at_sign, TokStart);
    return formToken(tok::unknown, TokStart);

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
    return lexOperatorIdentifier();
  case '%':
    // Lex %[0-9a-zA-Z]+ as a local SIL value
    if (InSILBody && isalnum(CurPtr[0])) {
      do {
        ++CurPtr;
      } while (isdigit(CurPtr[0]));
      
      return formToken(tok::sil_local_name, TokStart);
    }
    return lexOperatorIdentifier();
      
  case '=': case '-': case '+': case '*': case '<': case '>':
  case '!': case '&': case '|': case '^': case '~':
    return lexOperatorIdentifier();
  
  case '.':
    // When lexing a completion context, the buffer will terminate with a '.'.
    if (CurPtr-1 == BufferEnd)
      return formToken(tok::eof, TokStart);
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
  
  Lexer L(Buffer->getBuffer(), SM, nullptr, Loc.Value.getPointer(), false);
  unsigned Length = L.peekNextToken().getLength();
  return Loc.getAdvancedLoc(Length);
}

