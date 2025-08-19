#ifndef SWIFTC_LEXER_LEXER_H
#define SWIFTC_LEXER_LEXER_H

#include "swiftc/Basic/LLVM.h"
#include "swiftc/Basic/SourceLoc.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/Lexer/Token.h"

namespace swiftc {

class SourceManager;

class Lexer {
  const char* BufferStart;
  const char* BufferEnd;
  const char* CurPtr;
  SourceLoc BufferStartLoc;
  DiagnosticEngine& Diags;

public:
  Lexer(StringRef buffer, SourceLoc startLoc, DiagnosticEngine& diags);

  /// Lex the next token.
  Token lex();

  /// Peek at the next token without consuming it.
  Token peek();

  /// Check if we're at the end of the buffer.
  bool isAtEnd() const { return CurPtr >= BufferEnd; }

private:
  /// Get the current source location.
  SourceLoc getCurrentLoc() const {
    return SourceLoc(BufferStartLoc.getRawValue() + (CurPtr - BufferStart));
  }

  /// Advance the current pointer by one character.
  void advance() {
    if (CurPtr < BufferEnd) ++CurPtr;
  }

  /// Peek at the current character without consuming it.
  char peekChar() const {
    return CurPtr < BufferEnd ? *CurPtr : '\0';
  }

  /// Peek at the character at the given offset without consuming it.
  char peekChar(int offset) const {
    const char* ptr = CurPtr + offset;
    return ptr < BufferEnd ? *ptr : '\0';
  }

  /// Skip whitespace and return the next non-whitespace token.
  Token lexImpl();

  /// Lex an identifier or keyword.
  Token lexIdentifier();

  /// Lex a numeric literal.
  Token lexNumber();

  /// Lex a string literal.
  Token lexString();

  /// Lex an operator.
  Token lexOperator();

  /// Lex a comment (and skip it).
  void lexComment();

  /// Skip whitespace.
  void skipWhitespace();

  /// Check if a character is a valid identifier start.
  static bool isIdentifierStart(char c);

  /// Check if a character is a valid identifier continuation.
  static bool isIdentifierContinue(char c);

  /// Check if a character is a digit.
  static bool isDigit(char c);

  /// Check if a character is whitespace.
  static bool isWhitespace(char c);

  /// Check if a character can be part of an operator.
  static bool isOperatorChar(char c);

  /// Create a token with the given kind and text.
  Token makeToken(TokenKind kind, const char* start, const char* end) const {
    SourceLoc loc(BufferStartLoc.getRawValue() + (start - BufferStart));
    return Token(kind, loc, StringRef(start, end - start));
  }

  /// Create a token with the given kind starting from the given position.
  Token makeToken(TokenKind kind, const char* start) const {
    return makeToken(kind, start, CurPtr);
  }
};

} // namespace swiftc

#endif // SWIFTC_LEXER_LEXER_H