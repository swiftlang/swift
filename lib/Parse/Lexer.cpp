//===--- Lexer.cpp - Swift Language Lexer ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Lexer and Token interfaces.
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Confusables.h"
#include "swift/Parse/Lexer.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
// FIXME: Figure out if this can be migrated to LLVM.
#include "clang/Basic/CharInfo.h"

#include <limits>

using namespace swift;
using namespace swift::syntax;

// clang::isIdentifierHead and clang::isIdentifierBody are deliberately not in
// this list as a reminder that they are using C rules for identifiers.
// (Admittedly these are the same as Swift's right now.)
using clang::isAlphanumeric;
using clang::isDigit;
using clang::isHexDigit;
using clang::isHorizontalWhitespace;
using clang::isPrintable;
using clang::isWhitespace;

//===----------------------------------------------------------------------===//
// UTF8 Validation/Encoding/Decoding helper functions
//===----------------------------------------------------------------------===//

/// EncodeToUTF8 - Encode the specified code point into a UTF8 stream.  Return
/// true if it is an erroneous code point.
static bool EncodeToUTF8(unsigned CharValue,
                         SmallVectorImpl<char> &Result) {
  // Number of bits in the value, ignoring leading zeros.
  unsigned NumBits = 32-llvm::countLeadingZeros(CharValue);

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
  return llvm::countLeadingOnes(uint32_t(C) << 24);
}

/// isStartOfUTF8Character - Return true if this isn't a UTF8 continuation
/// character, which will be of the form 0b10XXXXXX
static bool isStartOfUTF8Character(unsigned char C) {
  // RFC 2279: The octet values FE and FF never appear.
  // RFC 3629: The octet values C0, C1, F5 to FF never appear.
  return C <= 0x80 || (C >= 0xC2 && C < 0xF5);
}

/// validateUTF8CharacterAndAdvance - Given a pointer to the starting byte of a
/// UTF8 character, validate it and advance the lexer past it.  This returns the
/// encoded character or ~0U if the encoding is invalid.
uint32_t swift::validateUTF8CharacterAndAdvance(const char *&Ptr,
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
      !isStartOfUTF8Character(CurByte)) {
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

Lexer::Lexer(const PrincipalTag &, const LangOptions &LangOpts,
             const SourceManager &SourceMgr, unsigned BufferID,
             DiagnosticEngine *Diags, bool InSILMode,
             HashbangMode HashbangAllowed, CommentRetentionMode RetainComments,
             TriviaRetentionMode TriviaRetention)
    : LangOpts(LangOpts), SourceMgr(SourceMgr), BufferID(BufferID),
      Diags(Diags), InSILMode(InSILMode),
      IsHashbangAllowed(HashbangAllowed == HashbangMode::Allowed),
      RetainComments(RetainComments), TriviaRetention(TriviaRetention) {}

void Lexer::initialize(unsigned Offset, unsigned EndOffset) {
  assert(Offset <= EndOffset);

  // Initialize buffer pointers.
  StringRef contents =
      SourceMgr.extractText(SourceMgr.getRangeForBuffer(BufferID));
  BufferStart = contents.data();
  BufferEnd = contents.data() + contents.size();
  assert(*BufferEnd == 0);
  assert(BufferStart + Offset <= BufferEnd);
  assert(BufferStart + EndOffset <= BufferEnd);

  // Check for Unicode BOM at start of file (Only UTF-8 BOM supported now).
  size_t BOMLength = contents.startswith("\xEF\xBB\xBF") ? 3 : 0;

  // Keep information about existance of UTF-8 BOM for transparency source code
  // editing with libSyntax.
  ContentStart = BufferStart + BOMLength;

  // Initialize code completion.
  if (BufferID == SourceMgr.getCodeCompletionBufferID()) {
    const char *Ptr = BufferStart + SourceMgr.getCodeCompletionOffset();
    if (Ptr >= BufferStart && Ptr <= BufferEnd)
      CodeCompletionPtr = Ptr;
  }

  ArtificialEOF = BufferStart + EndOffset;
  CurPtr = BufferStart + Offset;

  assert(NextToken.is(tok::NUM_TOKENS));
  lexImpl();
  assert((NextToken.isAtStartOfLine() || CurPtr != BufferStart) &&
         "The token should be at the beginning of the line, "
         "or we should be lexing from the middle of the buffer");
}

Lexer::Lexer(const LangOptions &Options, const SourceManager &SourceMgr,
             unsigned BufferID, DiagnosticEngine *Diags, bool InSILMode,
             HashbangMode HashbangAllowed, CommentRetentionMode RetainComments,
             TriviaRetentionMode TriviaRetention)
    : Lexer(PrincipalTag(), Options, SourceMgr, BufferID, Diags, InSILMode,
            HashbangAllowed, RetainComments, TriviaRetention) {
  unsigned EndOffset = SourceMgr.getRangeForBuffer(BufferID).getByteLength();
  initialize(/*Offset=*/0, EndOffset);
}

Lexer::Lexer(const LangOptions &Options, const SourceManager &SourceMgr,
             unsigned BufferID, DiagnosticEngine *Diags, bool InSILMode,
             HashbangMode HashbangAllowed, CommentRetentionMode RetainComments,
             TriviaRetentionMode TriviaRetention, unsigned Offset,
             unsigned EndOffset)
    : Lexer(PrincipalTag(), Options, SourceMgr, BufferID, Diags, InSILMode,
            HashbangAllowed, RetainComments, TriviaRetention) {
  initialize(Offset, EndOffset);
}

Lexer::Lexer(Lexer &Parent, State BeginState, State EndState)
    : Lexer(PrincipalTag(), Parent.LangOpts, Parent.SourceMgr, Parent.BufferID,
            Parent.Diags, Parent.InSILMode,
            Parent.IsHashbangAllowed
                ? HashbangMode::Allowed
                : HashbangMode::Disallowed,
            Parent.RetainComments, Parent.TriviaRetention) {
  assert(BufferID == SourceMgr.findBufferContainingLoc(BeginState.Loc) &&
         "state for the wrong buffer");
  assert(BufferID == SourceMgr.findBufferContainingLoc(EndState.Loc) &&
         "state for the wrong buffer");

  unsigned Offset = SourceMgr.getLocOffsetInBuffer(BeginState.Loc, BufferID);
  unsigned EndOffset = SourceMgr.getLocOffsetInBuffer(EndState.Loc, BufferID);
  initialize(Offset, EndOffset);
}

InFlightDiagnostic Lexer::diagnose(const char *Loc, Diagnostic Diag) {
  if (Diags)
    return Diags->diagnose(getSourceLoc(Loc), Diag);
  
  return InFlightDiagnostic();
}

Token Lexer::getTokenAt(SourceLoc Loc) {
  assert(BufferID == static_cast<unsigned>(
                         SourceMgr.findBufferContainingLoc(Loc)) &&
         "location from the wrong buffer");

  Lexer L(LangOpts, SourceMgr, BufferID, Diags, InSILMode,
          HashbangMode::Allowed, CommentRetentionMode::None,
          TriviaRetentionMode::WithoutTrivia);
  L.restoreState(State(Loc));
  Token Result;
  L.lex(Result);
  return Result;
}

void Lexer::formToken(tok Kind, const char *TokStart, bool MultilineString) {
  assert(CurPtr >= BufferStart &&
         CurPtr <= BufferEnd && "Current pointer out of range!");

  // When we are lexing a subrange from the middle of a file buffer, we will
  // run past the end of the range, but will stay within the file.  Check if
  // we are past the imaginary EOF, and synthesize a tok::eof in this case.
  if (Kind != tok::eof && TokStart >= ArtificialEOF) {
    Kind = tok::eof;
  }
  unsigned CommentLength = 0;
  if (RetainComments == CommentRetentionMode::AttachToNextToken) {
    // 'CommentLength' here is the length from the *first* comment to the
    // token text (or its backtick if exist).
    auto Iter = llvm::find_if(LeadingTrivia, [](const TriviaPiece &Piece) {
      return Piece.isComment();
    });
    for (auto End = LeadingTrivia.end(); Iter != End; Iter++) {
      if (Iter->getKind() == TriviaKind::Backtick)
        // Since Token::getCommentRange() doesn't take backtick into account,
        // we cannot include length of backtick.
        break;
      CommentLength += Iter->getTextLength();
    }
  }

  StringRef TokenText { TokStart, static_cast<size_t>(CurPtr - TokStart) };

  if (TriviaRetention == TriviaRetentionMode::WithTrivia) {
    lexTrivia(TrailingTrivia, /* IsForTrailingTrivia */ true);
  }

  NextToken.setToken(Kind, TokenText, CommentLength, MultilineString);
}

void Lexer::formEscapedIdentifierToken(const char *TokStart) {
  assert(CurPtr - TokStart >= 3 && "escaped identifier must be longer than or equal 3 bytes");
  assert(TokStart[0] == '`' && "escaped identifier starts with backtick");
  assert(CurPtr[-1] == '`' && "escaped identifier ends with backtick");

  LeadingTrivia.push_back(TriviaPiece::backtick());
  assert(TrailingTrivia.empty() && "TrailingTrivia is empty here");
  TrailingTrivia.push_back(TriviaPiece::backtick());

  formToken(tok::identifier, TokStart);
  // If this token is at ArtificialEOF, it's forced to be tok::eof. Don't mark
  // this as escaped-identifier in this case.
  if (NextToken.is(tok::eof))
    return;
  NextToken.setEscapedIdentifier(true);
}

Lexer::State Lexer::getStateForBeginningOfTokenLoc(SourceLoc Loc) const {
  const char *Ptr = getBufferPtrForSourceLoc(Loc);
  // Skip whitespace backwards until we hit a newline.  This is needed to
  // correctly lex the token if it is at the beginning of the line.
  while (Ptr >= ContentStart + 1) {
    char C = Ptr[-1];
    if (C == ' ' || C == '\t') {
      --Ptr;
      continue;
    }
    if (C == 0) {
      // A NUL character can be either whitespace we diagnose or a code
      // completion token.
      if (Ptr - 1 == CodeCompletionPtr)
        break;
      --Ptr;
      continue;
    }
    if (C == '\n' || C == '\r') {
      --Ptr;
      break;
    }
    break;
  }
  return State(SourceLoc(llvm::SMLoc::getFromPointer(Ptr)));
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
      .fixItRemoveChars(NulLoc, NulEndLoc);
}

void Lexer::skipToEndOfLine(bool EatNewline) {
  while (1) {
    switch (*CurPtr++) {
    case '\n':
    case '\r':
      if (EatNewline) {
        NextToken.setAtStartOfLine(true);
      } else {
        --CurPtr;
      }
      return;  // If we found the end of the line, return.
    default:
      // If this is a "high" UTF-8 character, validate it.
      if ((signed char)(CurPtr[-1]) < 0) {
        --CurPtr;
        const char *CharStart = CurPtr;
        if (validateUTF8CharacterAndAdvance(CurPtr, BufferEnd) == ~0U)
          diagnose(CharStart, diag::lex_invalid_utf8);
      }
      break;   // Otherwise, eat other characters.
    case 0:
      switch (getNulCharacterKind(CurPtr - 1)) {
      case NulCharacterKind::Embedded:
        // If this is a random nul character in the middle of a buffer, skip it
        // as whitespace.
        diagnoseEmbeddedNul(Diags, CurPtr-1);
        LLVM_FALLTHROUGH;
      case NulCharacterKind::CodeCompletion:
        continue;
      case NulCharacterKind::BufferEnd:
        // Otherwise, the last line of the file does not have a newline.
        --CurPtr;
        return;
      }
    }
  }
}

void Lexer::skipSlashSlashComment(bool EatNewline) {
  assert(CurPtr[-1] == '/' && CurPtr[0] == '/' && "Not a // comment");
  skipToEndOfLine(EatNewline);
}

void Lexer::skipHashbang(bool EatNewline) {
  assert(CurPtr == ContentStart && CurPtr[0] == '#' && CurPtr[1] == '!' &&
         "Not a hashbang");
  skipToEndOfLine(EatNewline);
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
          diagnose(CharStart, diag::lex_invalid_utf8);
      }

      break;   // Otherwise, eat other characters.
    case 0:
      switch (getNulCharacterKind(CurPtr - 1)) {
      case NulCharacterKind::Embedded:
        // If this is a random nul character in the middle of a buffer, skip it
        // as whitespace.
        diagnoseEmbeddedNul(Diags, CurPtr - 1);
        LLVM_FALLTHROUGH;
      case NulCharacterKind::CodeCompletion:
        continue;
      case NulCharacterKind::BufferEnd: {
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
  }
}

static bool isValidIdentifierContinuationCodePoint(uint32_t c) {
  if (c < 0x80)
    return clang::isIdentifierBody(c, /*dollar*/true);
  
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
    || (c >= 0xFE47 && c <= 0xFFF8)
  
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
  if (c < 0x80 && (isDigit(c) || c == '$'))
    return false;

  // N1518: Recommendations for extended identifier characters for C and C++
  // Proposed Annex X.2: Ranges of characters disallowed initially
  if ((c >= 0x0300 && c <= 0x036F) ||
      (c >= 0x1DC0 && c <= 0x1DFF) ||
      (c >= 0x20D0 && c <= 0x20FF) ||
      (c >= 0xFE20 && c <= 0xFE2F))
    return false;
  
  return true;
}

static bool advanceIf(char const *&ptr, char const *end,
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

bool Lexer::isIdentifier(StringRef string) {
  if (string.empty()) return false;
  char const *p = string.data(), *end = string.end();
  if (!advanceIfValidStartOfIdentifier(p, end))
    return false;
  while (p < end && advanceIfValidContinuationOfIdentifier(p, end));
  return p == end;
}

/// \brief Determines if the given string is a valid operator identifier,
/// without escaping characters.
bool Lexer::isOperator(StringRef string) {
  if (string.empty()) return false;
  char const *p = string.data(), *end = string.end();
  if (!advanceIfValidStartOfOperator(p, end))
    return false;
  while (p < end && advanceIfValidContinuationOfOperator(p, end));
  return p == end;
}


tok Lexer::kindOfIdentifier(StringRef Str, bool InSILMode) {
#define SIL_KEYWORD(kw)
#define KEYWORD(kw) if (Str == #kw) return tok::kw_##kw;
#include "swift/Syntax/TokenKinds.def"

  // SIL keywords are only active in SIL mode.
  if (InSILMode) {
#define SIL_KEYWORD(kw) if (Str == #kw) return tok::kw_##kw;
#include "swift/Syntax/TokenKinds.def"
  }
  return tok::identifier;
}

/// lexIdentifier - Match [a-zA-Z_][a-zA-Z_$0-9]*
void Lexer::lexIdentifier() {
  const char *TokStart = CurPtr-1;
  CurPtr = TokStart;
  bool didStart = advanceIfValidStartOfIdentifier(CurPtr, BufferEnd);
  assert(didStart && "Unexpected start");
  (void) didStart;

  // Lex [a-zA-Z_$0-9[[:XID_Continue:]]]*
  while (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd));

  tok Kind = kindOfIdentifier(StringRef(TokStart, CurPtr-TokStart), InSILMode);
  return formToken(Kind, TokStart);
}

/// lexHash - Handle #], #! for shebangs, and the family of #identifiers.
void Lexer::lexHash() {
  const char *TokStart = CurPtr-1;

  // Scan for [a-zA-Z]+ to see what we match.
  const char *tmpPtr = CurPtr;
  if (clang::isIdentifierHead(*tmpPtr)) {
    do {
      ++tmpPtr;
    } while (clang::isIdentifierBody(*tmpPtr));
  }

  // Map the character sequence onto
  tok Kind = llvm::StringSwitch<tok>(StringRef(CurPtr, tmpPtr-CurPtr))
#define POUND_KEYWORD(id) \
  .Case(#id, tok::pound_##id)
#include "swift/Syntax/TokenKinds.def"
  .Default(tok::pound);

  // If we didn't find a match, then just return tok::pound.  This is highly
  // dubious in terms of error recovery, but is useful for code completion and
  // SIL parsing.
  if (Kind == tok::pound)
    return formToken(tok::pound, TokStart);

  // If we found something specific, return it.
  CurPtr = tmpPtr;
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

  case '/':
    if (tokBegin - 1 != bufferBegin && tokBegin[-2] == '*')
      return false; // End of a slash-star comment, so whitespace.
    else
      return true;

  case '\xA0':
    if (tokBegin - 1 != bufferBegin && tokBegin[-2] == '\xC2')
      return false; // Non-breaking whitespace (U+00A0)
    else
      return true;

  default:
    return true;
  }
}

/// Is the operator ending at the given character (actually one past the end)
/// "right-bound"?
///
/// The code-completion point is considered right-bound.
static bool isRightBound(const char *tokEnd, bool isLeftBound,
                         const char *codeCompletionPtr) {
  switch (*tokEnd) {
  case ' ': case '\r': case '\n': case '\t': // whitespace
  case ')': case ']': case '}':              // closing delimiters
  case ',': case ';': case ':':              // expression separators
    return false;

  case '\0':
    if (tokEnd == codeCompletionPtr)         // code-completion
      return true;
    return false;                            // whitespace / last char in file

  case '.':
    // Prefer the '^' in "x^.y" to be a postfix op, not binary, but the '^' in
    // "^.y" to be a prefix op, not binary.
    return !isLeftBound;

  case '/':
    // A following comment counts as whitespace, so this token is not right bound.
    if (tokEnd[1] == '/' || tokEnd[1] == '*')
      return false;
    else
      return true;

  case '\xC2':
    if (tokEnd[1] == '\xA0')
      return false; // Non-breaking whitespace (U+00A0)
    else
      return true;

  default:
    return true;
  }
}

static bool rangeContainsPlaceholderEnd(const char *CurPtr,
                                        const char *End) {
  for (auto SubStr = CurPtr; SubStr != End - 1; ++SubStr) {
    if (SubStr[0] == '\n') {
      return false;
    }
    if (SubStr[0] == '#' && SubStr[1] == '>') {
      return true;
    }
  }
  return false;
}

/// lexOperatorIdentifier - Match identifiers formed out of punctuation.
void Lexer::lexOperatorIdentifier() {
  const char *TokStart = CurPtr-1;
  CurPtr = TokStart;
  bool didStart = advanceIfValidStartOfOperator(CurPtr, BufferEnd);
  assert(didStart && "unexpected operator start");
  (void) didStart;
  
  do {
    if (CurPtr != BufferEnd && InSILBody &&
        (*CurPtr == '!' || *CurPtr == '?'))
      // When parsing SIL body, '!' and '?' are special token and can't be
      // in the middle of an operator.
      break;

    // '.' cannot appear in the middle of an operator unless the operator
    // started with a '.'.
    if (*CurPtr == '.' && *TokStart != '.')
      break;
    if (Identifier::isEditorPlaceholder(StringRef(CurPtr, BufferEnd-CurPtr)) &&
        rangeContainsPlaceholderEnd(CurPtr + 2, BufferEnd)) {
      break;
    }
  } while (advanceIfValidContinuationOfOperator(CurPtr, BufferEnd));

  if (CurPtr-TokStart > 2) {
    // If there is a "//" or "/*" in the middle of an identifier token, 
    // it starts a comment.
    for (auto Ptr = TokStart+1; Ptr != CurPtr-1; ++Ptr) {
      if (Ptr[0] == '/' && (Ptr[1] == '/' || Ptr[1] == '*')) {
        CurPtr = Ptr;
        break;
      }
    }
  }

  // Decide between the binary, prefix, and postfix cases.
  // It's binary if either both sides are bound or both sides are not bound.
  // Otherwise, it's postfix if left-bound and prefix if right-bound.
  bool leftBound = isLeftBound(TokStart, ContentStart);
  bool rightBound = isRightBound(CurPtr, leftBound, CodeCompletionPtr);

  // Match various reserved words.
  if (CurPtr-TokStart == 1) {
    switch (TokStart[0]) {
    case '=':
      if (leftBound != rightBound) {
        auto d = diagnose(TokStart, diag::lex_unary_equal);
        if (leftBound)
          d.fixItInsert(getSourceLoc(TokStart), " ");
        else
          d.fixItInsert(getSourceLoc(TokStart+1), " ");
      }
      // always emit 'tok::equal' to avoid trickle down parse errors
      return formToken(tok::equal, TokStart);
    case '&':
      if (leftBound == rightBound || leftBound)
        break;
      return formToken(tok::amp_prefix, TokStart);
    case '.': {
      if (leftBound == rightBound)
        return formToken(tok::period, TokStart);
      if (rightBound)
        return formToken(tok::period_prefix, TokStart);
      
      // If left bound but not right bound, handle some likely situations.
      
      // If there is just some horizontal whitespace before the next token, its
      // addition is probably incorrect.
      const char *AfterHorzWhitespace = CurPtr;
      while (*AfterHorzWhitespace == ' ' || *AfterHorzWhitespace == '\t')
        ++AfterHorzWhitespace;

      // First, when we are code completing "x. <ESC>", then make sure to return
      // a tok::period, since that is what the user is wanting to know about.
      if (*AfterHorzWhitespace == '\0' &&
          AfterHorzWhitespace == CodeCompletionPtr) {
        diagnose(TokStart, diag::expected_member_name);
        return formToken(tok::period, TokStart);
      }

      if (isRightBound(AfterHorzWhitespace, leftBound, CodeCompletionPtr) &&
          // Don't consider comments to be this.  A leading slash is probably
          // either // or /* and most likely occurs just in our testsuite for
          // expected-error lines.
          *AfterHorzWhitespace != '/') {
        diagnose(TokStart, diag::extra_whitespace_period)
          .fixItRemoveChars(getSourceLoc(CurPtr),
                            getSourceLoc(AfterHorzWhitespace));
        return formToken(tok::period, TokStart);
      }

      // Otherwise, it is probably a missing member.
      diagnose(TokStart, diag::expected_member_name);
      return formToken(tok::unknown, TokStart);
    }
    case '?':
      if (leftBound)
        return formToken(tok::question_postfix, TokStart);
      return formToken(tok::question_infix, TokStart);
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
    // Verify there is no "*/" in the middle of the identifier token, we reject
    // it as potentially ending a block comment.
    auto Pos = StringRef(TokStart, CurPtr-TokStart).find("*/");
    if (Pos != StringRef::npos) {
      diagnose(TokStart+Pos, diag::lex_unexpected_block_comment_end);
      return formToken(tok::unknown, TokStart);
    }
  }

  if (leftBound == rightBound)
    return formToken(leftBound ? tok::oper_binary_unspaced :
                                 tok::oper_binary_spaced, TokStart);

  return formToken(leftBound ? tok::oper_postfix : tok::oper_prefix, TokStart);
}

/// lexDollarIdent - Match $[0-9a-zA-Z_$]+
void Lexer::lexDollarIdent() {
  const char *tokStart = CurPtr-1;
  assert(*tokStart == '$');

  // In a SIL function body, '$' is a token by itself, except it's a SIL global
  // name. SIL global identifiers may start with a '$', e.g. @$S1m3fooyyF.
  if (InSILBody && NextToken.getKind() != tok::at_sign)
    return formToken(tok::sil_dollar, tokStart);

  bool isAllDigits = true;
  for (;; ++CurPtr) {
    if (isDigit(*CurPtr)) {
      // continue
    } else if (clang::isIdentifierHead(*CurPtr, /*dollar*/true)) {
      isAllDigits = false;
      // continue
    } else {
      break;
    }
  }

  if (CurPtr == tokStart + 1) {
    // It is an error to see a standalone '$'. Offer to replace '$' with '`$`'.
    diagnose(tokStart, diag::standalone_dollar_identifier)
      .fixItReplaceChars(getSourceLoc(tokStart), getSourceLoc(CurPtr), "`$`");
    return formToken(tok::identifier, tokStart);
  }

  // We reserve $nonNumeric for persistent bindings in the debugger.
  if (!isAllDigits) {
    if (!LangOpts.EnableDollarIdentifiers && !InSILBody)
      diagnose(tokStart, diag::expected_dollar_numeric);

    // Even if we diagnose, we go ahead and form an identifier token,
    // in part to ensure that the basic behavior of the lexer is
    // independent of language mode.
    return formToken(tok::identifier, tokStart);
  } else {
    return formToken(tok::dollarident, tokStart);
  }
}

enum class ExpectedDigitKind : unsigned { Binary, Octal, Decimal, Hex };

void Lexer::lexHexNumber() {
  // We assume we're starting from the 'x' in a '0x...' floating-point literal.
  assert(*CurPtr == 'x' && "not a hex literal");
  const char *TokStart = CurPtr-1;
  assert(*TokStart == '0' && "not a hex literal");

  auto expected_digit = [&]() {
    while (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd));
    return formToken(tok::unknown, TokStart);
  };

  auto expected_hex_digit = [&](const char *loc) {
    diagnose(loc, diag::lex_invalid_digit_in_int_literal, StringRef(loc, 1),
             (unsigned)ExpectedDigitKind::Hex);
    return expected_digit();
  };

  // 0x[0-9a-fA-F][0-9a-fA-F_]*
  ++CurPtr;
  if (!isHexDigit(*CurPtr))
    return expected_hex_digit(CurPtr);

  while (isHexDigit(*CurPtr) || *CurPtr == '_')
    ++CurPtr;

  if (*CurPtr != '.' && *CurPtr != 'p' && *CurPtr != 'P') {
    auto tmp = CurPtr;
    if (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd))
      return expected_hex_digit(tmp);
    else
      return formToken(tok::integer_literal, TokStart);
  }

  const char *PtrOnDot = nullptr;

  // (\.[0-9A-Fa-f][0-9A-Fa-f_]*)?
  if (*CurPtr == '.') {
    PtrOnDot = CurPtr;
    ++CurPtr;
    
    // If the character after the '.' is not a digit, assume we have an int
    // literal followed by a dot expression.
    if (!isHexDigit(*CurPtr)) {
      --CurPtr;
      return formToken(tok::integer_literal, TokStart);
    }
    
    while (isHexDigit(*CurPtr) || *CurPtr == '_')
      ++CurPtr;

    if (*CurPtr != 'p' && *CurPtr != 'P') {
      if (!isDigit(PtrOnDot[1])) {
        // e.g: 0xff.description
        CurPtr = PtrOnDot;
        return formToken(tok::integer_literal, TokStart);
      }
      diagnose(CurPtr, diag::lex_expected_binary_exponent_in_hex_float_literal);
      return formToken(tok::unknown, TokStart);
    }
  }
  
  // [pP][+-]?[0-9][0-9_]*
  assert(*CurPtr == 'p' || *CurPtr == 'P' && "not at a hex float exponent?!");
  ++CurPtr;
  
  bool signedExponent = false;
  if (*CurPtr == '+' || *CurPtr == '-') {
    ++CurPtr;  // Eat the sign.
    signedExponent = true;
  }

  if (!isDigit(*CurPtr)) {
    if (PtrOnDot && !isDigit(PtrOnDot[1]) && !signedExponent) {
      // e.g: 0xff.fpValue, 0xff.fp
      CurPtr = PtrOnDot;
      return formToken(tok::integer_literal, TokStart);
    }
    // Note: 0xff.fp+otherExpr can be valid expression. But we don't accept it.

    // There are 3 cases to diagnose if the exponent starts with a non-digit:
    // identifier (invalid character), underscore (invalid first character),
    // non-identifier (empty exponent)
    auto tmp = CurPtr;
    if (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd))
      diagnose(tmp, diag::lex_invalid_digit_in_fp_exponent, StringRef(tmp, 1),
               *tmp == '_');
    else
      diagnose(CurPtr, diag::lex_expected_digit_in_fp_exponent);

    return expected_digit();
  }
  
  while (isDigit(*CurPtr) || *CurPtr == '_')
    ++CurPtr;

  auto tmp = CurPtr;
  if (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd)) {
    diagnose(tmp, diag::lex_invalid_digit_in_fp_exponent, StringRef(tmp, 1),
             false);
    return expected_digit();
  }

  return formToken(tok::floating_literal, TokStart);
}

/// lexNumber:
///   integer_literal  ::= [0-9][0-9_]*
///   integer_literal  ::= 0x[0-9a-fA-F][0-9a-fA-F_]*
///   integer_literal  ::= 0o[0-7][0-7_]*
///   integer_literal  ::= 0b[01][01_]*
///   floating_literal ::= [0-9][0-9]_*\.[0-9][0-9_]*
///   floating_literal ::= [0-9][0-9]*\.[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*
///   floating_literal ::= [0-9][0-9_]*[eE][+-]?[0-9][0-9_]*
///   floating_literal ::= 0x[0-9A-Fa-f][0-9A-Fa-f_]*
///                          (\.[0-9A-Fa-f][0-9A-Fa-f_]*)?[pP][+-]?[0-9][0-9_]*
void Lexer::lexNumber() {
  const char *TokStart = CurPtr-1;
  assert((isDigit(*TokStart) || *TokStart == '.') && "Unexpected start");

  auto expected_digit = [&]() {
    while (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd));
    return formToken(tok::unknown, TokStart);
  };

  auto expected_int_digit = [&](const char *loc, ExpectedDigitKind kind) {
    diagnose(loc, diag::lex_invalid_digit_in_int_literal, StringRef(loc, 1),
             (unsigned)kind);
    return expected_digit();
  };

  if (*TokStart == '0' && *CurPtr == 'x')
    return lexHexNumber();
  
  if (*TokStart == '0' && *CurPtr == 'o') {
    // 0o[0-7][0-7_]*
    ++CurPtr;
    if (*CurPtr < '0' || *CurPtr > '7')
      return expected_int_digit(CurPtr, ExpectedDigitKind::Octal);

    while ((*CurPtr >= '0' && *CurPtr <= '7') || *CurPtr == '_')
      ++CurPtr;

    auto tmp = CurPtr;
    if (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd))
      return expected_int_digit(tmp, ExpectedDigitKind::Octal);

    return formToken(tok::integer_literal, TokStart);
  }
  
  if (*TokStart == '0' && *CurPtr == 'b') {
    // 0b[01][01_]*
    ++CurPtr;
    if (*CurPtr != '0' && *CurPtr != '1')
      return expected_int_digit(CurPtr, ExpectedDigitKind::Binary);

    while (*CurPtr == '0' || *CurPtr == '1' || *CurPtr == '_')
      ++CurPtr;

    auto tmp = CurPtr;
    if (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd))
      return expected_int_digit(tmp, ExpectedDigitKind::Binary);

    return formToken(tok::integer_literal, TokStart);
  }

  // Handle a leading [0-9]+, lexing an integer or falling through if we have a
  // floating point value.
  while (isDigit(*CurPtr) || *CurPtr == '_')
    ++CurPtr;

  // Lex things like 4.x as '4' followed by a tok::period.
  if (*CurPtr == '.') {
    // NextToken is the soon to be previous token
    // Therefore: x.0.1 is sub-tuple access, not x.float_literal
    if (!isDigit(CurPtr[1]) || NextToken.is(tok::period))
      return formToken(tok::integer_literal, TokStart);
  } else {
    // Floating literals must have '.', 'e', or 'E' after digits.  If it is
    // something else, then this is the end of the token.
    if (*CurPtr != 'e' && *CurPtr != 'E') {
      auto tmp = CurPtr;
      if (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd))
        return expected_int_digit(tmp, ExpectedDigitKind::Decimal);

      return formToken(tok::integer_literal, TokStart);
    }
  }

  // Lex decimal point.
  if (*CurPtr == '.') {
    ++CurPtr;
   
    // Lex any digits after the decimal point.
    while (isDigit(*CurPtr) || *CurPtr == '_')
      ++CurPtr;
  }
  
  // Lex exponent.
  if (*CurPtr == 'e' || *CurPtr == 'E') {
    ++CurPtr;  // Eat the 'e'
    if (*CurPtr == '+' || *CurPtr == '-')
      ++CurPtr;  // Eat the sign.

    if (!isDigit(*CurPtr)) {
      // There are 3 cases to diagnose if the exponent starts with a non-digit:
      // identifier (invalid character), underscore (invalid first character),
      // non-identifier (empty exponent)
      auto tmp = CurPtr;
      if (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd))
        diagnose(tmp, diag::lex_invalid_digit_in_fp_exponent, StringRef(tmp, 1),
                 *tmp == '_');
      else
        diagnose(CurPtr, diag::lex_expected_digit_in_fp_exponent);

      return expected_digit();
    }

    while (isDigit(*CurPtr) || *CurPtr == '_')
      ++CurPtr;

    auto tmp = CurPtr;
    if (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd)) {
      diagnose(tmp, diag::lex_invalid_digit_in_fp_exponent, StringRef(tmp, 1),
               false);
      return expected_digit();
    }
  }
  
  return formToken(tok::floating_literal, TokStart);
}

///   unicode_character_escape ::= [\]u{hex+}
///   hex                      ::= [0-9a-fA-F]
unsigned Lexer::lexUnicodeEscape(const char *&CurPtr, Lexer *Diags) {
  assert(CurPtr[0] == '{' && "Invalid unicode escape");
  ++CurPtr;

  const char *DigitStart = CurPtr;

  unsigned NumDigits = 0;
  for (; isHexDigit(CurPtr[0]); ++NumDigits)
    ++CurPtr;

  if (CurPtr[0] != '}') {
    if (Diags)
      Diags->diagnose(CurPtr, diag::lex_invalid_u_escape_rbrace);
    return ~1U;
  }
  ++CurPtr;

  if (NumDigits < 1 || NumDigits > 8) {
    if (Diags)
      Diags->diagnose(CurPtr, diag::lex_invalid_u_escape);
    return ~1U;
  }

  unsigned CharValue = 0;
  StringRef(DigitStart, NumDigits).getAsInteger(16, CharValue);
  return CharValue;
}

/// maybeConsumeNewlineEscape - Check for valid elided newline escape and
/// move pointer passed in to the character after the end of the line.
static bool maybeConsumeNewlineEscape(const char *&CurPtr, ssize_t Offset) {
  const char *TmpPtr = CurPtr + Offset;
  while (true) {
    switch (*TmpPtr++) {
    case ' ': case '\t':
      continue;
    case '\r':
      if (*TmpPtr == '\n')
        ++TmpPtr;
      LLVM_FALLTHROUGH;
    case '\n':
      CurPtr = TmpPtr;
      return true;
    case 0:
    default:
      return false;
    }
  }
}

/// lexCharacter - Read a character and return its UTF32 code.  If this is the
/// end of enclosing string/character sequence (i.e. the character is equal to
/// 'StopQuote'), this returns ~0U and leaves 'CurPtr' pointing to the terminal
/// quote.  If this is a malformed character sequence, it emits a diagnostic
/// (when EmitDiagnostics is true) and returns ~1U.
/// 
///   character_escape  ::= [\][\] | [\]t | [\]n | [\]r | [\]" | [\]' | [\]0
///   character_escape  ::= unicode_character_escape
unsigned Lexer::lexCharacter(const char *&CurPtr, char StopQuote,
                             bool EmitDiagnostics, bool MultilineString) {
  const char *CharStart = CurPtr;

  switch (*CurPtr++) {
  default: {// Normal characters are part of the string.
    // If this is a "high" UTF-8 character, validate it.
    if ((signed char)(CurPtr[-1]) >= 0) {
      if (isPrintable(CurPtr[-1]) == 0)
        if (!(MultilineString && (CurPtr[-1] == '\t')))
          if (EmitDiagnostics)
            diagnose(CharStart, diag::lex_unprintable_ascii_character);
      return CurPtr[-1];
    }
    --CurPtr;
    unsigned CharValue = validateUTF8CharacterAndAdvance(CurPtr, BufferEnd);
    if (CharValue != ~0U) return CharValue;
    if (EmitDiagnostics)
      diagnose(CharStart, diag::lex_invalid_utf8);
    return ~1U;
  }
  case '"':
  case '\'':
    // If we found a closing quote character, we're done.
    if (CurPtr[-1] == StopQuote) {
      --CurPtr;
      return ~0U;
    }
    // Otherwise, this is just a character.
    return CurPtr[-1];
      
  case 0:
    if (CurPtr-1 != BufferEnd) {
      if (EmitDiagnostics)
        diagnose(CurPtr-1, diag::lex_nul_character);
      return CurPtr[-1];
    }
    // Move the pointer back to EOF.
    --CurPtr;
    if (EmitDiagnostics)
      diagnose(CurPtr-1, diag::lex_unterminated_string);
    return ~1U;
  case '\n':  // String literals cannot have \n or \r in them.
  case '\r':
    if (MultilineString) // ... unless they are multiline
      return CurPtr[-1];
    if (EmitDiagnostics)
      diagnose(CurPtr-1, diag::lex_unterminated_string);
    return ~1U;
  case '\\':  // Escapes.
    break;
  }
  
  unsigned CharValue = 0;
  // Escape processing.  We already ate the "\".
  switch (*CurPtr) {
  case ' ': case '\t': case '\n': case '\r':
    if (MultilineString && maybeConsumeNewlineEscape(CurPtr, 0))
      return '\n';
    LLVM_FALLTHROUGH;
  default:  // Invalid escape.
    if (EmitDiagnostics)
      diagnose(CurPtr, diag::lex_invalid_escape);
    // If this looks like a plausible escape character, recover as though this
    // is an invalid escape.
    if (isAlphanumeric(*CurPtr)) ++CurPtr;
    return ~1U;
      
  // Simple single-character escapes.
  case '0': ++CurPtr; return '\0';
  case 'n': ++CurPtr; return '\n';
  case 'r': ++CurPtr; return '\r';
  case 't': ++CurPtr; return '\t';
  case '"': ++CurPtr; return '"';
  case '\'': ++CurPtr; return '\'';
  case '\\': ++CurPtr; return '\\';

  case 'u': {  //  \u HEX HEX HEX HEX
    ++CurPtr;
    if (*CurPtr != '{') {
      if (EmitDiagnostics)
        diagnose(CurPtr-1, diag::lex_unicode_escape_braces);
      return ~1U;
    }

    CharValue = lexUnicodeEscape(CurPtr, EmitDiagnostics ? this : nullptr);
    if (CharValue == ~1U) return ~1U;
    break;
  }
  }

  // Check to see if the encoding is valid.
  llvm::SmallString<64> TempString;
  if (CharValue >= 0x80 && EncodeToUTF8(CharValue, TempString)) {
    if (EmitDiagnostics)
      diagnose(CharStart, diag::lex_invalid_unicode_scalar);
    return ~1U;
  }
  
  return CharValue;
}

/// skipToEndOfInterpolatedExpression - Given the first character after a \(
/// sequence in a string literal (the start of an interpolated expression),
/// scan forward to the end of the interpolated expression and return the end.
/// On success, the returned pointer will point to the ')' at the end of the
/// interpolated expression.  On failure, it will point to the first character
/// that cannot be lexed as part of the interpolated expression; this character
/// will never be ')'.
///
/// This function performs brace and quote matching, keeping a stack of
/// outstanding delimiters as it scans the string.
static const char *skipToEndOfInterpolatedExpression(const char *CurPtr,
                                                     const char *EndPtr,
                                                     DiagnosticEngine *Diags,
                                                     bool MultilineString) {
  llvm::SmallVector<char, 4> OpenDelimiters;
  llvm::SmallVector<bool, 4> AllowNewline;
  AllowNewline.push_back(MultilineString);

  auto inStringLiteral = [&]() {
    return !OpenDelimiters.empty() &&
           (OpenDelimiters.back() == '"' || OpenDelimiters.back() == '\'');
  };
  while (true) {
    // This is a simple scanner, capable of recognizing nested parentheses and
    // string literals but not much else.  The implications of this include not
    // being able to break an expression over multiple lines in an interpolated
    // string.  This limitation allows us to recover from common errors though.
    //
    // On success scanning the expression body, the real lexer will be used to
    // relex the body when parsing the expressions.  We let it diagnose any
    // issues with malformed tokens or other problems.
    switch (*CurPtr++) {
    // String literals in general cannot be split across multiple lines;
    // interpolated ones are no exception - unless multiline literals.
    case '\n':
    case '\r':
      if (AllowNewline.back())
        continue;
      // Will be diagnosed as an unterminated string literal.
      return CurPtr-1;

    case '"':
    case '\'': {
      if (!AllowNewline.back() && inStringLiteral()) {
        if (OpenDelimiters.back() == CurPtr[-1]) {
          // Closing single line string literal.
          OpenDelimiters.pop_back();
          AllowNewline.pop_back();
        }
        // Otherwise, it's just a quote in string literal. e.g. "foo's".
        continue;
      }

      bool isMultilineQuote = (
          *CurPtr == '"' && *(CurPtr + 1) == '"' && *(CurPtr - 1) == '"');
      if (isMultilineQuote)
        CurPtr += 2;

      if (!inStringLiteral()) {
        // Open string literal
        OpenDelimiters.push_back(CurPtr[-1]);
        AllowNewline.push_back(isMultilineQuote);
        continue;
      }

      // We are in multiline string literal.
      assert(AllowNewline.back() && "other cases must be handled above");
      if (isMultilineQuote) {
        // Close multiline string literal.
        OpenDelimiters.pop_back();
        AllowNewline.pop_back();
      }

      // Otherwise, it's just a normal character in multiline string.
      continue;
    }
    case '\\':
      if (inStringLiteral()) {
        char escapedChar = *CurPtr++;
        switch (escapedChar) {
        case '(':
          // Entering a recursive interpolated expression
          OpenDelimiters.push_back('(');
          continue;
        case '\n': case '\r':
          if (AllowNewline.back())
            continue;
          LLVM_FALLTHROUGH;
        case 0:
          // Don't jump over newline/EOF due to preceding backslash!
          return CurPtr-1;
        default:
          continue;
        }
      }
      continue;
    case 0:
      // If we hit EOF, we fail.
      if (CurPtr-1 == EndPtr) {
        if (Diags)
          Diags->diagnose(Lexer::getSourceLoc(CurPtr-1),
                          diag::lex_unterminated_string);
        return CurPtr-1;
      }
      continue;
        
    // Paren nesting deeper to support "foo = \((a+b)-(c*d)) bar".
    case '(':
      if (!inStringLiteral()) {
        OpenDelimiters.push_back('(');
      }
      continue;
    case ')':
      if (OpenDelimiters.empty()) {
        // No outstanding open delimiters; we're done.
        return CurPtr-1;
      } else if (OpenDelimiters.back() == '(') {
        // Pop the matching bracket and keep going.
        OpenDelimiters.pop_back();
        continue;
      } else {
        // It's a right parenthesis in a string literal.
        assert(inStringLiteral());
        continue;
      }
    default:
      // Normal token character.
      continue;
    }
  }
}

/// getStringLiteralContent:
/// Extract content of string literal from inside quotes.
static StringRef getStringLiteralContent(const Token &Str) {
  StringRef Bytes = Str.getText();

  if (Str.IsMultilineString())
    Bytes = Bytes.drop_front(3).drop_back(3);
  else
    Bytes = Bytes.drop_front().drop_back();

  return Bytes;
}

static size_t commonPrefixLength(StringRef shorter, StringRef longer) {
  size_t offset = 0;
  while (offset < shorter.size() && offset < longer.size() && shorter[offset] == longer[offset]) {
    ++offset;
  }
  
  return offset;
}

/// getMultilineTrailingIndent:
/// Determine trailing indent to be used for multiline literal indent stripping.
static std::tuple<StringRef, SourceLoc>
getMultilineTrailingIndent(const Token &Str, DiagnosticEngine *Diags) {
  StringRef Bytes = getStringLiteralContent(Str);
  const char *begin = Bytes.begin(), *end = Bytes.end(), *start = end;
  bool sawNonWhitespace = false;

  // Work back from the end to find whitespace to strip.
  while (!sawNonWhitespace && start > begin) {
    switch (*--start) {
    case ' ':
    case '\t':
      continue;
    case '\n':
    case '\r': {
      ++start;
      auto startLoc = Lexer::getSourceLoc(start);
      auto string = StringRef(start, end - start);

      // Disallow escaped newline in the last line.
      if (Diags) {
        auto *Ptr = start - 1;
        if (*Ptr == '\n') --Ptr;
        if (*Ptr == '\r') --Ptr;
        auto *LineEnd = Ptr + 1;
        while (Ptr > begin && (*Ptr == ' ' || *Ptr == '\t')) --Ptr;
        if (*Ptr == '\\') {
          auto escapeLoc = Lexer::getSourceLoc(Ptr);
          bool invalid = true;
          while (*--Ptr == '\\') invalid = !invalid;
          if (invalid)
            Diags->diagnose(escapeLoc, diag::lex_escaped_newline_at_lastline)
              .fixItRemoveChars(escapeLoc, Lexer::getSourceLoc(LineEnd));
        }
      }

      return std::make_tuple(string, startLoc);
    }
    default:
      sawNonWhitespace = true;
    }
  }
  
  if (sawNonWhitespace && Diags) {
    auto loc = Lexer::getSourceLoc(start + 1);
    Diags->diagnose(loc, diag::lex_illegal_multiline_string_end)
    // FIXME: Should try to suggest indentation.
      .fixItInsert(loc, "\n");
  }

  return std::make_tuple("", Lexer::getSourceLoc(end - 1));
}

/// diagnoseInvalidMultilineIndents:
/// Emit errors for a group of multiline indents with the same MistakeOffset.
/// Note: Does not emit an error if MistakeOffset does not lie within 
/// ExpectedIndent.
static void diagnoseInvalidMultilineIndents(
                                            DiagnosticEngine *Diags, 
                                            StringRef ExpectedIndent,
                                            SourceLoc IndentLoc,
                                            StringRef Bytes,
                                            SmallVector<size_t, 4> LineStarts,
                                            size_t MistakeOffset,
                                            StringRef ActualIndent) {
  if (MistakeOffset >= ExpectedIndent.size()) {
    // These lines were valid; there's nothing to correct.
    return;
  }

  assert(!LineStarts.empty());

  auto getLoc = [&](size_t offset) -> SourceLoc {
    return Lexer::getSourceLoc((const char *)Bytes.bytes_begin() + offset);
  };
  auto classify = [&](unsigned char ch) -> unsigned {
    switch (ch) {
    case ' ':
      return 0;
    case '\t':
      return 1;
    default:
      return 2;
    }
  };
  
  Diags->diagnose(getLoc(LineStarts[0] + MistakeOffset),
                  diag::lex_multiline_string_indent_inconsistent,
                  LineStarts.size() != 1, LineStarts.size(),
                  classify(Bytes[LineStarts[0] + MistakeOffset]));
  
  Diags->diagnose(IndentLoc.getAdvancedLoc(MistakeOffset), 
                  diag::lex_multiline_string_indent_should_match_here, 
                  classify(ExpectedIndent[MistakeOffset]));
  
  auto fix = Diags->diagnose(getLoc(LineStarts[0] + MistakeOffset),
                             diag::lex_multiline_string_indent_change_line,
                             LineStarts.size() != 1);
  
  assert(MistakeOffset <= ActualIndent.size());
  assert(ExpectedIndent.substr(0, MistakeOffset) == 
         ActualIndent.substr(0, MistakeOffset));
  
  for (auto line : LineStarts) {
    fix.fixItReplaceChars(getLoc(line + MistakeOffset), 
                          getLoc(line + ActualIndent.size()),
                          ExpectedIndent.substr(MistakeOffset));
  }
}

/// validateMultilineIndents:
/// Diagnose contents of string literal that have inconsistent indentation.
static void validateMultilineIndents(const Token &Str,
                                     DiagnosticEngine *Diags) {
  StringRef Indent;
  SourceLoc IndentStartLoc;
  std::tie(Indent, IndentStartLoc) = getMultilineTrailingIndent(Str, Diags);
  if (Indent.empty())
    return;
  
  // The offset into the previous line where it experienced its first indentation 
  // error, or Indent.size() if every character matched.
  size_t lastMistakeOffset = std::numeric_limits<size_t>::max();
  // Offsets for each consecutive previous line with its first error at 
  // lastMatchLength.
  SmallVector<size_t, 4> linesWithLastMistakeOffset = {};
  // Prefix of indentation that's present on all lines in linesWithLastMatchLength.
  StringRef commonIndentation = "";
  
  StringRef Bytes = getStringLiteralContent(Str);
  for (size_t pos = Bytes.find('\n'); pos != StringRef::npos; pos = Bytes.find('\n', pos + 1)) {
    size_t nextpos = pos + 1;
    auto restOfBytes = Bytes.substr(nextpos);
    
    // Ignore blank lines.
    if (restOfBytes[0] == '\n' || restOfBytes[0] == '\r') {
      continue;
    }
    
    // Where is the first difference?
    auto errorOffset = commonPrefixLength(Indent, restOfBytes);
    
    // Are we starting a new run?
    if (errorOffset != lastMistakeOffset) {
      // Diagnose problems in the just-finished run of lines.
      diagnoseInvalidMultilineIndents(Diags, Indent, IndentStartLoc, Bytes, 
                                      linesWithLastMistakeOffset, lastMistakeOffset, 
                                      commonIndentation);
      
      // Set up for a new run.
      lastMistakeOffset = errorOffset;
      linesWithLastMistakeOffset = {};
      
      // To begin with, all whitespace is part of the common indentation.
      auto prefixLength = restOfBytes.find_first_not_of(" \t");
      commonIndentation = restOfBytes.substr(0, prefixLength);
    }
    else {
      // We're continuing the run, so include this line in the common prefix.
      auto prefixLength = commonPrefixLength(commonIndentation, restOfBytes);
      commonIndentation = commonIndentation.substr(0, prefixLength);
    }
    
    // Either way, add this line to the run.
    linesWithLastMistakeOffset.push_back(nextpos);
  }
  
  // Handle the last run.
  diagnoseInvalidMultilineIndents(Diags, Indent, IndentStartLoc, Bytes, 
                                  linesWithLastMistakeOffset, lastMistakeOffset, 
                                  commonIndentation);
}

/// lexStringLiteral:
///   string_literal ::= ["]([^"\\\n\r]|character_escape)*["]
///   string_literal ::= ["]["]["].*["]["]["] - approximately
void Lexer::lexStringLiteral() {
  const char *TokStart = CurPtr-1;
  assert((*TokStart == '"' || *TokStart == '\'') && "Unexpected start");
  // NOTE: We only allow single-quote string literals so we can emit useful
  // diagnostics about changing them to double quotes.

  bool wasErroneous = false, MultilineString = false;

  // Is this the start of a multiline string literal?
  if (*TokStart == '"' && *CurPtr == '"' && *(CurPtr + 1) == '"') {
    MultilineString = true;
    CurPtr += 2;
    if (*CurPtr != '\n' && *CurPtr != '\r')
      diagnose(CurPtr, diag::lex_illegal_multiline_string_start)
        .fixItInsert(Lexer::getSourceLoc(CurPtr), "\n");
  }

  while (true) {
    if (*CurPtr == '\\' && *(CurPtr + 1) == '(') {
      // Consume tokens until we hit the corresponding ')'.
      CurPtr += 2;
      const char *EndPtr =
          skipToEndOfInterpolatedExpression(CurPtr, BufferEnd,
                                            Diags, MultilineString);
      
      if (*EndPtr == ')') {
        // Successfully scanned the body of the expression literal.
        CurPtr = EndPtr+1;
      } else {
        CurPtr = EndPtr;
        wasErroneous = true;
      }
      continue;
    }

    // String literals cannot have \n or \r in them (unless multiline).
    if (((*CurPtr == '\r' || *CurPtr == '\n') && !MultilineString)
        || CurPtr == BufferEnd) {
      diagnose(TokStart, diag::lex_unterminated_string);
      return formToken(tok::unknown, TokStart);
    }

    unsigned CharValue = lexCharacter(CurPtr, *TokStart, true, MultilineString);
    wasErroneous |= CharValue == ~1U;

    // If this is the end of string, we are done.  If it is a normal character
    // or an already-diagnosed error, just munch it.
    if (CharValue == ~0U) {
      ++CurPtr;
      if (wasErroneous)
        return formToken(tok::unknown, TokStart);

      if (*TokStart == '\'') {
        // Complain about single-quote string and suggest replacement with
        // double-quoted equivalent.
        StringRef orig(TokStart, CurPtr - TokStart);
        llvm::SmallString<32> replacement;
        replacement += '"';
        std::string str = orig.slice(1, orig.size() - 1).str();
        std::string quot = "\"";
        size_t pos = 0;
        while (pos != str.length()) {
          if (str.at(pos) == '\\') {
            if (str.at(pos + 1) == '\'') {
                // Un-escape escaped single quotes.
                str.replace(pos, 2, "'");
                ++pos;
            } else {
                // Skip over escaped characters.
                pos += 2;
            }
          } else if (str.at(pos) == '"') {
            str.replace(pos, 1, "\\\"");
            // Advance past the newly added ["\""].
            pos += 2;
          } else {
            ++pos;
          }
        }
        replacement += StringRef(str);
        replacement += '"';
        diagnose(TokStart, diag::lex_single_quote_string)
          .fixItReplaceChars(getSourceLoc(TokStart), getSourceLoc(CurPtr),
                             replacement);
      }

      // Is this the end of a multiline string literal?
      if (MultilineString) {
        if (*CurPtr == '"' && *(CurPtr + 1) == '"' && *(CurPtr + 2) != '"') {
          CurPtr += 2;
          formToken(tok::string_literal, TokStart, MultilineString);
          if (Diags)
            validateMultilineIndents(NextToken, Diags);
          return;
        }
        else
          continue;
      }

      return formToken(tok::string_literal, TokStart, MultilineString);
    }
  }
}


/// We found an opening curly quote in the source file.  Scan ahead until we
/// find and end-curly-quote (or straight one).  If we find what looks to be a
/// string literal, diagnose the problem and return a pointer to the end of the
/// entire string literal.  This helps us avoid parsing the body of the string
/// as program tokens, which will only lead to massive confusion.
const char *Lexer::findEndOfCurlyQuoteStringLiteral(const char *Body,
                                                    bool EmitDiagnostics) {

  while (true) {
    // Don't bother with string interpolations.
    if (*Body == '\\' && *(Body + 1) == '(')
      return nullptr;

    // We didn't find the end of the string literal if we ran to end of line.
    if (*Body == '\r' || *Body == '\n' || Body == BufferEnd)
      return nullptr;

    // Get the next character.
    const char *CharStart = Body;
    unsigned CharValue = lexCharacter(Body, '\0', /*EmitDiagnostics=*/false);
    // If the character was incorrectly encoded, give up.
    if (CharValue == ~1U) return nullptr;
    
    // If we found a straight-quote, then we're done.  Just return the spot
    // to continue.
    if (CharValue == '"')
      return Body;
    
    // If we found an ending curly quote (common since this thing started with
    // an opening curly quote) diagnose it with a fixit and then return.
    if (CharValue == 0x0000201D) {
      if (EmitDiagnostics) {
        diagnose(CharStart, diag::lex_invalid_curly_quote)
            .fixItReplaceChars(getSourceLoc(CharStart), getSourceLoc(Body),
                               "\"");
      }
      return Body;
    }
    
    // Otherwise, keep scanning.
  }
}


/// lexEscapedIdentifier:
///   identifier ::= '`' identifier '`'
///
/// If it doesn't match this production, the leading ` is a punctuator.
void Lexer::lexEscapedIdentifier() {
  assert(CurPtr[-1] == '`' && "Unexpected start of escaped identifier");
  
  const char *Quote = CurPtr-1;

  // Check whether we have an identifier followed by another backtick, in which
  // case this is an escaped identifier.
  const char *IdentifierStart = CurPtr;
  if (advanceIfValidStartOfIdentifier(CurPtr, BufferEnd)) {
    // Keep continuing the identifier.
    while (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd));

    // If we have the terminating "`", it's an escaped identifier.
    if (*CurPtr == '`') {
      ++CurPtr;
      formEscapedIdentifierToken(Quote);
      return;
    }
  }

  // Special case; allow '`$`'.
  if (Quote[1] == '$' && Quote[2] == '`') {
    CurPtr = Quote + 3;
    formEscapedIdentifierToken(Quote);
    return;
  }

  // The backtick is punctuation.
  CurPtr = IdentifierStart;
  formToken(tok::backtick, Quote);
}

/// Find the end of a version control conflict marker.
static const char *findConflictEnd(const char *CurPtr, const char *BufferEnd,
                                   ConflictMarkerKind CMK) {
  StringRef terminator = CMK == ConflictMarkerKind::Perforce ? "<<<<\n"
                                                             : ">>>>>>> ";
  size_t termLen = terminator.size();
  
  // Get a reference to the rest of the buffer minus the length of the start
  // of the conflict marker.
  auto restOfBuffer = StringRef(CurPtr, BufferEnd - CurPtr).substr(termLen);
  size_t endPos = restOfBuffer.find(terminator);
  while (endPos != StringRef::npos) {
    // Must occur at start of line.
    if (endPos != 0 &&
        (restOfBuffer[endPos - 1] == '\r' || restOfBuffer[endPos - 1] == '\n'))
    {
      return restOfBuffer.data() + endPos;
    }
    restOfBuffer = restOfBuffer.substr(endPos + termLen);
    endPos = restOfBuffer.find(terminator);
  }
  return nullptr;
}

bool Lexer::tryLexConflictMarker(bool EatNewline) {
  const char *Ptr = CurPtr - 1;

  // Only a conflict marker if it starts at the beginning of a line.
  if (Ptr != ContentStart && Ptr[-1] != '\n' && Ptr[-1] != '\r')
    return false;
  
  // Check to see if we have <<<<<<< or >>>>.
  StringRef restOfBuffer(Ptr, BufferEnd - Ptr);
  if (!restOfBuffer.startswith("<<<<<<< ") && !restOfBuffer.startswith(">>>> "))
    return false;
  
  ConflictMarkerKind Kind = *Ptr == '<' ? ConflictMarkerKind::Normal
                                        : ConflictMarkerKind::Perforce;
  if (const char *End = findConflictEnd(Ptr, BufferEnd, Kind)) {
    // Diagnose at the conflict marker, then jump ahead to the end.
    diagnose(CurPtr, diag::lex_conflict_marker_in_file);
    CurPtr = End;
    
    // Skip ahead to the end of the marker.
    if (CurPtr != BufferEnd)
      skipToEndOfLine(EatNewline);
    
    return true;
  }
  
  // No end of conflict marker found.
  return false;
}

bool Lexer::lexUnknown(bool EmitDiagnosticsIfToken) {
  const char *Tmp = CurPtr - 1;

  if (advanceIfValidContinuationOfIdentifier(Tmp, BufferEnd)) {
    // If this is a valid identifier continuation, but not a valid identifier
    // start, attempt to recover by eating more continuation characters.
    if (EmitDiagnosticsIfToken) {
      diagnose(CurPtr - 1, diag::lex_invalid_identifier_start_character);
    }
    while (advanceIfValidContinuationOfIdentifier(Tmp, BufferEnd))
      ;
    CurPtr = Tmp;
    return true;
  }

  // This character isn't allowed in Swift source.
  uint32_t Codepoint = validateUTF8CharacterAndAdvance(Tmp, BufferEnd);
  if (Codepoint == ~0U) {
    diagnose(CurPtr - 1, diag::lex_invalid_utf8)
        .fixItReplaceChars(getSourceLoc(CurPtr - 1), getSourceLoc(Tmp), " ");
    CurPtr = Tmp;
    return false; // Skip presumed whitespace.
  } else if (Codepoint == 0x000000A0) {
      // Non-breaking whitespace (U+00A0)
      while (Tmp[0] == '\xC2' && Tmp[1] == '\xA0')
        Tmp += 2;
      SmallString<8> Spaces;
      Spaces.assign((Tmp - CurPtr + 1) / 2, ' ');
      diagnose(CurPtr - 1, diag::lex_nonbreaking_space)
          .fixItReplaceChars(getSourceLoc(CurPtr - 1), getSourceLoc(Tmp),
                             Spaces);
      CurPtr = Tmp;
      return false;
  } else if (Codepoint == 0x0000201D) {
    // If this is an end curly quote, just diagnose it with a fixit hint.
    if (EmitDiagnosticsIfToken) {
      diagnose(CurPtr - 1, diag::lex_invalid_curly_quote)
          .fixItReplaceChars(getSourceLoc(CurPtr - 1), getSourceLoc(Tmp), "\"");
    }
    CurPtr = Tmp;
    return true;
  } else if (Codepoint == 0x0000201C) {
    auto EndPtr = Tmp;
    // If this is a start curly quote, do a fuzzy match of a string literal
    // to improve recovery.
    if (auto Tmp2 =
            findEndOfCurlyQuoteStringLiteral(Tmp, EmitDiagnosticsIfToken))
      Tmp = Tmp2;

    // Note, we intentionally diagnose the end quote before the start quote,
    // so that the IDE suggests fixing the end quote before the start quote.
    // This, in turn, works better with our error recovery because we won't
    // diagnose an end curly quote in the middle of a straight quoted
    // literal.
    if (EmitDiagnosticsIfToken) {
      diagnose(CurPtr - 1, diag::lex_invalid_curly_quote)
          .fixItReplaceChars(getSourceLoc(CurPtr - 1), getSourceLoc(EndPtr),
                             "\"");
    }
    CurPtr = Tmp;
    return true;
  }

  diagnose(CurPtr - 1, diag::lex_invalid_character)
      .fixItReplaceChars(getSourceLoc(CurPtr - 1), getSourceLoc(Tmp), " ");

  char ExpectedCodepoint;
  if ((ExpectedCodepoint =
           confusable::tryConvertConfusableCharacterToASCII(Codepoint))) {

    llvm::SmallString<4> ConfusedChar;
    EncodeToUTF8(Codepoint, ConfusedChar);
    llvm::SmallString<1> ExpectedChar;
    ExpectedChar += ExpectedCodepoint;
    diagnose(CurPtr - 1, diag::lex_confusable_character, ConfusedChar,
             ExpectedChar)
        .fixItReplaceChars(getSourceLoc(CurPtr - 1), getSourceLoc(Tmp),
                           ExpectedChar);
  }

  CurPtr = Tmp;
  return false; // Skip presumed whitespace.
}

Lexer::NulCharacterKind Lexer::getNulCharacterKind(const char *Ptr) const {
  assert(Ptr != nullptr && *Ptr == 0);
  if (Ptr == CodeCompletionPtr) {
    return NulCharacterKind::CodeCompletion;
  }
  if (Ptr == BufferEnd) {
    return NulCharacterKind::BufferEnd;
  }
  return NulCharacterKind::Embedded;
}

void Lexer::tryLexEditorPlaceholder() {
  assert(CurPtr[-1] == '<' && CurPtr[0] == '#');
  const char *TokStart = CurPtr-1;
  for (const char *Ptr = CurPtr+1; Ptr < BufferEnd-1; ++Ptr) {
    if (*Ptr == '\n')
      break;
    if (Ptr[0] == '<' && Ptr[1] == '#')
      break;
    if (Ptr[0] == '#' && Ptr[1] == '>') {
      // Found it. Flag it as error (or warning, if in playground mode) for the
      // rest of the compiler pipeline and lex it as an identifier.
      if (LangOpts.Playground) {
        diagnose(TokStart, diag::lex_editor_placeholder_in_playground);
      } else {
        diagnose(TokStart, diag::lex_editor_placeholder);
      }
      CurPtr = Ptr+2;
      formToken(tok::identifier, TokStart);
      return;
    }
  }

  // Not a well-formed placeholder.
  lexOperatorIdentifier();
}

StringRef Lexer::getEncodedStringSegment(StringRef Bytes,
                                         SmallVectorImpl<char> &TempString,
                                         bool IsFirstSegment,
                                         bool IsLastSegment,
                                         unsigned IndentToStrip) {

  TempString.clear();
  // Note that it is always safe to read one over the end of "Bytes" because
  // we know that there is a terminating " character.  Use BytesPtr to avoid a
  // range check subscripting on the StringRef.
  const char *BytesPtr = Bytes.begin();
  bool IsEscapedNewline = false;
  while (BytesPtr < Bytes.end()) {
    char CurChar = *BytesPtr++;

    // Multiline string line ending normalization and indent stripping.
    if (CurChar == '\r' || CurChar == '\n') {
      bool stripNewline = IsEscapedNewline ||
        (IsFirstSegment && BytesPtr - 1 == Bytes.begin());
      if (CurChar == '\r' && *BytesPtr == '\n')
        ++BytesPtr;
      if (*BytesPtr != '\r' && *BytesPtr != '\n')
        BytesPtr += IndentToStrip;
      if (IsLastSegment && BytesPtr == Bytes.end())
        stripNewline = true;
      if (!stripNewline)
        TempString.push_back('\n');
      IsEscapedNewline = false;
      continue;
    }

    if (CurChar != '\\') {
      TempString.push_back(CurChar);
      continue;
    }
    
    // Invalid escapes are accepted by the lexer but diagnosed as an error.  We
    // just ignore them here.
    unsigned CharValue = 0; // Unicode character value for \x, \u, \U.
    switch (*BytesPtr++) {
    default:
      continue;   // Invalid escape, ignore it.
          
      // Simple single-character escapes.
    case '0': TempString.push_back('\0'); continue;
    case 'n': TempString.push_back('\n'); continue;
    case 'r': TempString.push_back('\r'); continue;
    case 't': TempString.push_back('\t'); continue;
    case '"': TempString.push_back('"'); continue;
    case '\'': TempString.push_back('\''); continue;
    case '\\': TempString.push_back('\\'); continue;

    case ' ': case '\t': case '\n': case '\r':
      if (maybeConsumeNewlineEscape(BytesPtr, -1)) {
        IsEscapedNewline = true;
        --BytesPtr;
      }
      continue;

    // String interpolation.
    case '(':
      llvm_unreachable("string contained interpolated segments");
        
      // Unicode escapes of various lengths.
    case 'u':  //  \u HEX HEX HEX HEX
      if (BytesPtr[0] != '{')
        continue;       // Ignore invalid escapes.

      CharValue = lexUnicodeEscape(BytesPtr, /*no diagnostics*/nullptr);
      // Ignore invalid escapes.
      if (CharValue == ~1U) continue;
      break;
    }
    
    if (CharValue < 0x80) 
      TempString.push_back(CharValue);
    else
      EncodeToUTF8(CharValue, TempString);
  }
  
  // If we didn't escape or reprocess anything, then we don't need to use the
  // temporary string, just point to the original one. We know that this
  // is safe because unescaped strings are always shorter than their escaped
  // forms (in a valid string).
  if (TempString.size() == Bytes.size()) {
    TempString.clear();
    return Bytes;
  }
  return StringRef(TempString.begin(), TempString.size());
}

void Lexer::getStringLiteralSegments(
              const Token &Str,
              SmallVectorImpl<StringSegment> &Segments,
              DiagnosticEngine *Diags) {
  assert(Str.is(tok::string_literal));
  // Get the bytes behind the string literal, dropping any double quotes.
  StringRef Bytes = getStringLiteralContent(Str);

  // Are substitutions required either for indent stripping or line ending
  // normalization?
  bool MultilineString = Str.IsMultilineString(), IsFirstSegment = true;
  unsigned IndentToStrip = 0;
  if (MultilineString)
    IndentToStrip = 
      std::get<0>(getMultilineTrailingIndent(Str, /*Diags=*/nullptr)).size();

  // Note that it is always safe to read one over the end of "Bytes" because
  // we know that there is a terminating " character.  Use BytesPtr to avoid a
  // range check subscripting on the StringRef.
  const char *SegmentStartPtr = Bytes.begin();
  const char *BytesPtr = SegmentStartPtr;
  // FIXME: Use SSE to scan for '\'.
  while (BytesPtr != Bytes.end()) {
    char CurChar = *BytesPtr++;
    if (CurChar != '\\')
      continue;

    if (*BytesPtr++ != '(')
      continue;

    // String interpolation.

    // Push the current segment.
    Segments.push_back(
        StringSegment::getLiteral(getSourceLoc(SegmentStartPtr),
                                  BytesPtr-SegmentStartPtr-2,
                                  IsFirstSegment, false, IndentToStrip));
    IsFirstSegment = false;

    // Find the closing ')'.
    const char *End = skipToEndOfInterpolatedExpression(BytesPtr,
                                                        Str.getText().end(),
                                                        Diags, MultilineString);
    assert(*End == ')' && "invalid string literal interpolations should"
           " not be returned as string literals");
    ++End;

    // Add an expression segment.
    Segments.push_back(
        StringSegment::getExpr(getSourceLoc(BytesPtr-1), End-BytesPtr+1));

    // Reset the beginning of the segment to the string that remains to be
    // consumed.
    SegmentStartPtr = BytesPtr = End;
  }

  Segments.push_back(
      StringSegment::getLiteral(getSourceLoc(SegmentStartPtr),
                                Bytes.end()-SegmentStartPtr,
                                IsFirstSegment, true, IndentToStrip));
}


//===----------------------------------------------------------------------===//
// Main Lexer Loop
//===----------------------------------------------------------------------===//

void Lexer::lexImpl() {
  assert(CurPtr >= BufferStart &&
         CurPtr <= BufferEnd && "Current pointer out of range!");

  LeadingTrivia.clear();
  TrailingTrivia.clear();

  if (CurPtr == BufferStart) {
    if (BufferStart < ContentStart) {
      size_t BOMLen = ContentStart - BufferStart;
      assert(BOMLen == 3 && "UTF-8 BOM is 3 bytes");
      // Add UTF-8 BOM to LeadingTrivia.
      auto Text = OwnedString::makeRefCounted(StringRef(CurPtr, BOMLen));
      LeadingTrivia.push_back(TriviaPiece::garbageText(Text));
      CurPtr += BOMLen;
    }
    NextToken.setAtStartOfLine(true);
  } else {
    NextToken.setAtStartOfLine(false);
  }

  lexTrivia(LeadingTrivia, /* IsForTrailingTrivia */ false);

  // Remember the start of the token so we can form the text range.
  const char *TokStart = CurPtr;
  
  switch ((signed char)*CurPtr++) {
  default: {
    char const *Tmp = CurPtr-1;
    if (advanceIfValidStartOfIdentifier(Tmp, BufferEnd))
      return lexIdentifier();
    
    if (advanceIfValidStartOfOperator(Tmp, BufferEnd))
      return lexOperatorIdentifier();

    bool ShouldTokenize = lexUnknown(/*EmitDiagnosticsIfToken=*/true);
    assert(
        ShouldTokenize &&
        "Invalid UTF-8 sequence should be eaten by lexTrivia as LeadingTrivia");
    (void)ShouldTokenize;
    return formToken(tok::unknown, TokStart);
  }

  case '\n':
  case '\r':
    llvm_unreachable("Newlines should be eaten by lexTrivia as LeadingTrivia");

  case ' ':
  case '\t':
  case '\f':
  case '\v':
    llvm_unreachable(
        "Whitespaces should be eaten by lexTrivia as LeadingTrivia");

  case -1:
  case -2:
    diagnose(CurPtr-1, diag::lex_utf16_bom_marker);
    CurPtr = BufferEnd;
    return formToken(tok::unknown, TokStart);

  case 0:
    switch (getNulCharacterKind(CurPtr - 1)) {
    case NulCharacterKind::CodeCompletion:
      return formToken(tok::code_complete, TokStart);

    case NulCharacterKind::BufferEnd:
      // This is the real end of the buffer.
      // Put CurPtr back into buffer bounds.
      --CurPtr;
      // Return EOF.
      return formToken(tok::eof, TokStart);

    case NulCharacterKind::Embedded:
      llvm_unreachable(
          "Embedded nul should be eaten by lexTrivia as LeadingTrivia");
    }

  case '@': return formToken(tok::at_sign, TokStart);
  case '{': return formToken(tok::l_brace, TokStart);
  case '[': return formToken(tok::l_square, TokStart);
  case '(': return formToken(tok::l_paren, TokStart);
  case '}': return formToken(tok::r_brace, TokStart);
  case ']': return formToken(tok::r_square, TokStart);
  case ')': return formToken(tok::r_paren, TokStart);

  case ',': return formToken(tok::comma, TokStart);
  case ';': return formToken(tok::semi, TokStart);
  case ':': return formToken(tok::colon, TokStart);
  case '\\': return formToken(tok::backslash, TokStart);

  case '#':
    return lexHash();

      // Operator characters.
  case '/':
    if (CurPtr[0] == '/') {  // "//"
      skipSlashSlashComment(/*EatNewline=*/true);
      assert(isKeepingComments() &&
             "Non token comment should be eaten by lexTrivia as LeadingTrivia");
      return formToken(tok::comment, TokStart);
    }
    if (CurPtr[0] == '*') { // "/*"
      skipSlashStarComment();
      assert(isKeepingComments() &&
             "Non token comment should be eaten by lexTrivia as LeadingTrivia");
      return formToken(tok::comment, TokStart);
    }
    return lexOperatorIdentifier();
  case '%':
    // Lex %[0-9a-zA-Z_]+ as a local SIL value
    if (InSILBody && clang::isIdentifierBody(CurPtr[0])) {
      do {
        ++CurPtr;
      } while (clang::isIdentifierBody(CurPtr[0]));
      
      return formToken(tok::sil_local_name, TokStart);
    }
    return lexOperatorIdentifier();

  case '!':
    if (InSILBody)
      return formToken(tok::sil_exclamation, TokStart);
    if (isLeftBound(TokStart, ContentStart))
      return formToken(tok::exclaim_postfix, TokStart);
    return lexOperatorIdentifier();
  
  case '?':
    if (isLeftBound(TokStart, ContentStart))
      return formToken(tok::question_postfix, TokStart);
    return lexOperatorIdentifier();

  case '<':
    if (CurPtr[0] == '#')
      return tryLexEditorPlaceholder();

    return lexOperatorIdentifier();
  case '>':
    return lexOperatorIdentifier();
 
  case '=': case '-': case '+': case '*':
  case '&': case '|':  case '^': case '~': case '.':
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
  case '\'':
    return lexStringLiteral();
      
  case '`':
    return lexEscapedIdentifier();
  }
}

Token Lexer::getTokenAtLocation(const SourceManager &SM, SourceLoc Loc) {
  // Don't try to do anything with an invalid location.
  if (!Loc.isValid())
    return Token();

  // Figure out which buffer contains this location.
  int BufferID = SM.findBufferContainingLoc(Loc);
  if (BufferID < 0)
    return Token();
  
  // Use fake language options; language options only affect validity
  // and the exact token produced.
  LangOptions FakeLangOpts;

  // Here we return comments as tokens because either the caller skipped
  // comments and normally we won't be at the beginning of a comment token
  // (making this option irrelevant), or the caller lexed comments and
  // we need to lex just the comment token.
  Lexer L(FakeLangOpts, SM, BufferID, nullptr, /*InSILMode=*/ false,
          HashbangMode::Allowed, CommentRetentionMode::ReturnAsTokens);
  L.restoreState(State(Loc));
  return L.peekNextToken();
}

void Lexer::lexTrivia(syntax::Trivia &Pieces, bool IsForTrailingTrivia) {
Restart:
  const char *TriviaStart = CurPtr;

  switch ((signed char)*CurPtr++) {
  case '\n':
    if (IsForTrailingTrivia)
      break;
    NextToken.setAtStartOfLine(true);
    Pieces.appendOrSquash(TriviaPiece::newlines(1));
    goto Restart;
  case '\r':
    if (IsForTrailingTrivia)
      break;
    NextToken.setAtStartOfLine(true);
    if (CurPtr[0] == '\n') {
      Pieces.appendOrSquash(TriviaPiece::carriageReturnLineFeeds(1));
      ++CurPtr;
    } else {
      Pieces.appendOrSquash(TriviaPiece::carriageReturns(1));
    }
    goto Restart;
  case ' ':
    Pieces.appendOrSquash(TriviaPiece::spaces(1));
    goto Restart;
  case '\t':
    Pieces.appendOrSquash(TriviaPiece::tabs(1));
    goto Restart;
  case '\v':
    Pieces.appendOrSquash(TriviaPiece::verticalTabs(1));
    goto Restart;
  case '\f':
    Pieces.appendOrSquash(TriviaPiece::formfeeds(1));
    goto Restart;
  case '/':
    if (IsForTrailingTrivia || isKeepingComments()) {
      // Don't lex comments as trailing trivias (for now).
      // Don't try to lex comments here if we are lexing comments as Tokens.
      break;
    } else if (*CurPtr == '/') {
      // '// ...' comment.
      bool isDocComment = CurPtr[1] == '/';
      skipSlashSlashComment(/*EatNewline=*/false);
      size_t Length = CurPtr - TriviaStart;
      auto Text = OwnedString::makeRefCounted(StringRef(TriviaStart, Length));
      Pieces.push_back(isDocComment ? TriviaPiece::docLineComment(Text)
                                    : TriviaPiece::lineComment(Text));
      goto Restart;
    } else if (*CurPtr == '*') {
      // '/* ... */' comment.
      bool isDocComment = CurPtr[1] == '*';
      skipSlashStarComment();
      size_t Length = CurPtr - TriviaStart;
      auto Text = OwnedString::makeRefCounted(StringRef(TriviaStart, Length));
      Pieces.push_back(isDocComment ? TriviaPiece::docBlockComment(Text)
                                    : TriviaPiece::blockComment(Text));
      goto Restart;
    }
    break;
  case '#':
    if (TriviaStart == ContentStart && *CurPtr == '!') {
      // Hashbang '#!/path/to/swift'.
      --CurPtr;
      if (!IsHashbangAllowed)
        diagnose(TriviaStart, diag::lex_hashbang_not_allowed);
      skipHashbang(/*EatNewline=*/false);
      size_t Length = CurPtr - TriviaStart;
      auto Text = OwnedString::makeRefCounted(StringRef(TriviaStart, Length));
      Pieces.push_back(TriviaPiece::garbageText(Text));
      goto Restart;
    }
    break;
  case '<':
  case '>':
    if (tryLexConflictMarker(/*EatNewline=*/false)) {
      // Conflict marker.
      size_t Length = CurPtr - TriviaStart;
      auto Text = OwnedString::makeRefCounted(StringRef(TriviaStart, Length));
      Pieces.push_back(TriviaPiece::garbageText(Text));
      goto Restart;
    }
    break;
  case 0:
    switch (getNulCharacterKind(CurPtr - 1)) {
    case NulCharacterKind::Embedded: {
      diagnoseEmbeddedNul(Diags, CurPtr - 1);
      size_t Length = CurPtr - TriviaStart;
      auto Text = OwnedString::makeRefCounted(StringRef(TriviaStart, Length));
      Pieces.push_back(TriviaPiece::garbageText(Text));
      goto Restart;
    }
    case NulCharacterKind::CodeCompletion:
    case NulCharacterKind::BufferEnd:
      break;
    }
    break;
  // Start character of tokens.
  case -1: case -2:
  case '@': case '{': case '[': case '(': case '}': case ']': case ')':
  case ',': case ';': case ':': case '\\': case '$':
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
  case '"': case '\'': case '`':
  // Start of identifiers.
  case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
  case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
  case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
  case 'V': case 'W': case 'X': case 'Y': case 'Z':
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
  case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
  case 'v': case 'w': case 'x': case 'y': case 'z':
  case '_':
  // Start of operators.
  case '%': case '!': case '?': case '=':
  case '-': case '+': case '*':
  case '&': case '|': case '^': case '~': case '.':
    break;
  default:
    const char *Tmp = CurPtr - 1;
    if (advanceIfValidStartOfIdentifier(Tmp, BufferEnd)) {
      break;
    }
    if (advanceIfValidStartOfOperator(Tmp, BufferEnd)) {
      break;
    }

    bool ShouldTokenize = lexUnknown(/*EmitDiagnosticsIfToken=*/false);
    if (ShouldTokenize) {
      CurPtr = Tmp;
      return;
    }

    size_t Length = CurPtr - TriviaStart;
    auto Text = OwnedString::makeRefCounted(StringRef(TriviaStart, Length));
    Pieces.push_back(TriviaPiece::garbageText(Text));
    goto Restart;
  }
  // Reset the cursor.
  --CurPtr;
}

SourceLoc Lexer::getLocForEndOfToken(const SourceManager &SM, SourceLoc Loc) {
  return Loc.getAdvancedLocOrInvalid(getTokenAtLocation(SM, Loc).getLength());
}


static SourceLoc getLocForStartOfTokenInBuf(SourceManager &SM,
                                            unsigned BufferID,
                                            unsigned Offset,
                                            unsigned BufferStart,
                                            unsigned BufferEnd) {
  // Use fake language options; language options only affect validity
  // and the exact token produced.
  LangOptions FakeLangOptions;

  Lexer L(FakeLangOptions, SM, BufferID, nullptr, /*InSILMode=*/false,
          HashbangMode::Allowed, CommentRetentionMode::None,
          TriviaRetentionMode::WithoutTrivia, BufferStart, BufferEnd);

  // Lex tokens until we find the token that contains the source location.
  Token Tok;
  do {
    L.lex(Tok);

    unsigned TokOffs = SM.getLocOffsetInBuffer(Tok.getLoc(), BufferID);
    if (TokOffs > Offset) {
      // We ended up skipping over the source location entirely, which means
      // that it points into whitespace. We are done here.
      break;
    }

    if (Offset < TokOffs+Tok.getLength()) {
      // Current token encompasses our source location.

      if (Tok.is(tok::string_literal)) {
        SmallVector<Lexer::StringSegment, 4> Segments;
        Lexer::getStringLiteralSegments(Tok, Segments, /*Diags=*/nullptr);
        for (auto &Seg : Segments) {
          unsigned SegOffs = SM.getLocOffsetInBuffer(Seg.Loc, BufferID);
          unsigned SegEnd = SegOffs+Seg.Length;
          if (SegOffs > Offset)
            break;

          // If the offset is inside an interpolated expr segment, re-lex.
          if (Seg.Kind == Lexer::StringSegment::Expr && Offset < SegEnd)
            return getLocForStartOfTokenInBuf(SM, BufferID, Offset,
                                              /*BufferStart=*/SegOffs,
                                              /*BufferEnd=*/SegEnd);
        }
      }

      return Tok.getLoc();
    }
  } while (Tok.isNot(tok::eof));

  // We've passed our source location; just return the original source location.
  return SM.getLocForOffset(BufferID, Offset);
}

// Find the start of the given line.
static const char *findStartOfLine(const char *bufStart, const char *current) {
  while (current != bufStart) {
    if (current[0] == '\n' || current[0] == '\r') {
      ++current;
      break;
    }

    --current;
  }

  return current;
}

SourceLoc Lexer::getLocForStartOfToken(SourceManager &SM, SourceLoc Loc) {
  Optional<unsigned> BufferIdOp = SM.getIDForBufferIdentifier(SM.
    getBufferIdentifierForLoc(Loc));
  if (!BufferIdOp.hasValue())
    return SourceLoc();
  return getLocForStartOfToken(SM, BufferIdOp.getValue(),
    SM.getLocOffsetInBuffer(Loc, BufferIdOp.getValue()));
}

SourceLoc Lexer::getLocForStartOfToken(SourceManager &SM, unsigned BufferID,
                                       unsigned Offset) {
  CharSourceRange entireRange = SM.getRangeForBuffer(BufferID);
  StringRef Buffer = SM.extractText(entireRange);

  const char *BufStart = Buffer.data();
  if (Offset > Buffer.size())
    return SourceLoc();

  const char *StrData = BufStart+Offset;
  // If it points to whitespace return the SourceLoc for it.
  if (StrData[0] == '\n' || StrData[0] == '\r' ||
      StrData[0] == ' ' || StrData[0] == '\t')
    return SM.getLocForOffset(BufferID, Offset);

  // Back up from the current location until we hit the beginning of a line
  // (or the buffer). We'll relex from that point.
  const char *LexStart = findStartOfLine(BufStart, StrData);

  return getLocForStartOfTokenInBuf(SM, BufferID, Offset,
                                    /*BufferStart=*/LexStart-BufStart,
                                    /*BufferEnd=*/Buffer.size());
}

SourceLoc Lexer::getLocForStartOfLine(SourceManager &SM, SourceLoc Loc) {
  // Don't try to do anything with an invalid location.
  if (Loc.isInvalid())
    return Loc;

  // Figure out which buffer contains this location.
  int BufferID = SM.findBufferContainingLoc(Loc);
  if (BufferID < 0)
    return SourceLoc();

  CharSourceRange entireRange = SM.getRangeForBuffer(BufferID);
  StringRef Buffer = SM.extractText(entireRange);

  const char *BufStart = Buffer.data();
  unsigned Offset = SM.getLocOffsetInBuffer(Loc, BufferID);

  const char *StartOfLine = findStartOfLine(BufStart, BufStart + Offset);
  return getSourceLoc(StartOfLine);
}

SourceLoc Lexer::getLocForEndOfLine(SourceManager &SM, SourceLoc Loc) {
  // Don't try to do anything with an invalid location.
  if (Loc.isInvalid())
    return Loc;

  // Figure out which buffer contains this location.
  int BufferID = SM.findBufferContainingLoc(Loc);
  if (BufferID < 0)
    return SourceLoc();

  // Use fake language options; language options only affect validity
  // and the exact token produced.
  LangOptions FakeLangOpts;

  // Here we return comments as tokens because either the caller skipped
  // comments and normally we won't be at the beginning of a comment token
  // (making this option irrelevant), or the caller lexed comments and
  // we need to lex just the comment token.
  Lexer L(FakeLangOpts, SM, BufferID, nullptr, /*InSILMode=*/ false,
          HashbangMode::Allowed, CommentRetentionMode::ReturnAsTokens);
  L.restoreState(State(Loc));
  L.skipToEndOfLine(/*EatNewline=*/true);
  return getSourceLoc(L.CurPtr);
}

StringRef Lexer::getIndentationForLine(SourceManager &SM, SourceLoc Loc,
                                       StringRef *ExtraIndentation) {
  // FIXME: do something more intelligent here.
  //
  // Four spaces is the typical indentation in Swift code, so for now just use
  // that directly here, but if someone was to do something better, updating
  // here will update everyone.

  if (ExtraIndentation)
    *ExtraIndentation = "    ";

  // Don't try to do anything with an invalid location.
  if (Loc.isInvalid())
    return "";

  // Figure out which buffer contains this location.
  int BufferID = SM.findBufferContainingLoc(Loc);
  if (BufferID < 0)
    return "";

  CharSourceRange entireRange = SM.getRangeForBuffer(BufferID);
  StringRef Buffer = SM.extractText(entireRange);

  const char *BufStart = Buffer.data();
  unsigned Offset = SM.getLocOffsetInBuffer(Loc, BufferID);

  const char *StartOfLine = findStartOfLine(BufStart, BufStart + Offset);
  const char *EndOfIndentation = StartOfLine;
  while (*EndOfIndentation && isHorizontalWhitespace(*EndOfIndentation))
    ++EndOfIndentation;

  return StringRef(StartOfLine, EndOfIndentation - StartOfLine);
}

ArrayRef<Token> swift::
slice_token_array(ArrayRef<Token> AllTokens, SourceLoc StartLoc,
                  SourceLoc EndLoc) {
  assert(StartLoc.isValid() && EndLoc.isValid());
  auto StartIt = token_lower_bound(AllTokens, StartLoc);
  auto EndIt = token_lower_bound(AllTokens, EndLoc);
  assert(StartIt->getLoc() == StartLoc && EndIt->getLoc() == EndLoc);
  return AllTokens.slice(StartIt - AllTokens.begin(), EndIt - StartIt + 1);
}
