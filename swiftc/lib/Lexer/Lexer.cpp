#include "swiftc/Lexer/Lexer.h"
#include "swiftc/Basic/SourceManager.h"
#include <cctype>
#include <string>

using namespace swiftc;

Lexer::Lexer(StringRef buffer, SourceLoc startLoc, DiagnosticEngine& diags)
    : BufferStart(buffer.data()),
      BufferEnd(buffer.data() + buffer.size()),
      CurPtr(buffer.data()),
      BufferStartLoc(startLoc),
      Diags(diags) {}

Token Lexer::lex() {
  return lexImpl();
}

Token Lexer::peek() {
  const char* savedPtr = CurPtr;
  Token token = lexImpl();
  CurPtr = savedPtr;
  return token;
}

Token Lexer::lexImpl() {
  while (true) {
    skipWhitespace();
    
    if (isAtEnd()) {
      return makeToken(TokenKind::Eof, CurPtr);
    }
    
    const char* tokenStart = CurPtr;
    char c = peekChar();
    
    // Identifiers and keywords
    if (isIdentifierStart(c)) {
      return lexIdentifier();
    }
    
    // Numbers
    if (isDigit(c)) {
      return lexNumber();
    }
    
    // String literals
    if (c == '"') {
      return lexString();
    }
    
    // Comments - skip them and continue lexing
    if (c == '/' && peekChar(1) == '/') {
      lexComment();
      continue; // Skip comment and continue lexing
    }
    
    if (c == '/' && peekChar(1) == '*') {
      lexComment();
      continue; // Skip comment and continue lexing
    }
    
    // Single character tokens
    advance();
    switch (c) {
    case '(': return makeToken(TokenKind::LeftParen, tokenStart);
    case ')': return makeToken(TokenKind::RightParen, tokenStart);
    case '{': 
      if (!isAtEnd() && peekChar() == '{') {
        advance();
        return makeToken(TokenKind::LeftBraceBrace, tokenStart);
      }
      return makeToken(TokenKind::LeftBrace, tokenStart);
    case '}':
      if (!isAtEnd() && peekChar() == '}') {
        advance();
        return makeToken(TokenKind::RightBraceBrace, tokenStart);
      }
      return makeToken(TokenKind::RightBrace, tokenStart);
    case '[': return makeToken(TokenKind::LeftBracket, tokenStart);
    case ']': return makeToken(TokenKind::RightBracket, tokenStart);
    case ',': return makeToken(TokenKind::Comma, tokenStart);
    case ';': return makeToken(TokenKind::Semicolon, tokenStart);
    case ':': return makeToken(TokenKind::Colon, tokenStart);
    case '~': return makeToken(TokenKind::Tilde, tokenStart);
    case '^': return makeToken(TokenKind::Caret, tokenStart);
    case '@': return makeToken(TokenKind::At, tokenStart);
    case '#': return makeToken(TokenKind::Hash, tokenStart);
    case '$': return makeToken(TokenKind::Dollar, tokenStart);
    case '`': return makeToken(TokenKind::Backtick, tokenStart);
    case '\\': return makeToken(TokenKind::Backslash, tokenStart);
    }
    
    // Check for Unicode characters (treat as operators for now)
    if ((unsigned char)c > 127) {
      // Simple Unicode handling - consume the Unicode sequence
      while (!isAtEnd() && (unsigned char)peekChar() > 127) {
        advance();
      }
      return makeToken(TokenKind::Identifier, tokenStart); // Treat as identifier for now
    }
    
    // Multi-character operators
    CurPtr = tokenStart;
    return lexOperator();
  }
}

Token Lexer::lexIdentifier() {
  const char* start = CurPtr;
  
  // Consume the first character
  advance();
  
  // Consume remaining identifier characters
  while (!isAtEnd() && isIdentifierContinue(peekChar())) {
    advance();
  }
  
  StringRef text(start, CurPtr - start);
  TokenKind kind = getKeywordKind(text);
  return makeToken(kind, start);
}

Token Lexer::lexNumber() {
  const char* start = CurPtr;
  bool isFloat = false;
  
  // Consume digits
  while (!isAtEnd() && isDigit(peekChar())) {
    advance();
  }
  
  // Check for decimal point
  if (!isAtEnd() && peekChar() == '.' && isDigit(peekChar(1))) {
    isFloat = true;
    advance(); // consume '.'
    
    // Consume fractional digits
    while (!isAtEnd() && isDigit(peekChar())) {
      advance();
    }
  }
  
  // Check for exponent
  if (!isAtEnd() && (peekChar() == 'e' || peekChar() == 'E')) {
    isFloat = true;
    advance(); // consume 'e' or 'E'
    
    // Optional sign
    if (!isAtEnd() && (peekChar() == '+' || peekChar() == '-')) {
      advance();
    }
    
    // Consume exponent digits
    while (!isAtEnd() && isDigit(peekChar())) {
      advance();
    }
  }
  
  TokenKind kind = isFloat ? TokenKind::FloatingPointLiteral : TokenKind::IntegerLiteral;
  return makeToken(kind, start);
}

Token Lexer::lexString() {
  const char* start = CurPtr;
  
  // Consume opening quote
  advance();
  
  while (!isAtEnd() && peekChar() != '"') {
    if (peekChar() == '\\') {
      advance(); // consume backslash
      if (!isAtEnd()) {
        char escaped = peekChar();
        if (escaped == '(') {
          // String interpolation - consume until matching )
          advance(); // consume (
          int parenCount = 1;
          while (!isAtEnd() && parenCount > 0) {
            char ch = peekChar();
            advance();
            if (ch == '(') parenCount++;
            else if (ch == ')') parenCount--;
            // Stop if we hit a quote (malformed interpolation)
            else if (ch == '"') break;
          }
        } else {
          advance(); // consume escaped character
        }
      }
    } else {
      advance();
    }
  }
  
  if (isAtEnd()) {
    Diags.diagnoseError(getCurrentLoc(), "unterminated string literal");
    return makeToken(TokenKind::Unknown, start);
  }
  
  // Consume closing quote
  advance();
  
  return makeToken(TokenKind::StringLiteral, start);
}

Token Lexer::lexOperator() {
  const char* start = CurPtr;
  char c = peekChar();
  
  // Check if this could be a multi-character custom operator
  if (isOperatorChar(c)) {
    advance();
    
    // Continue consuming operator characters to form custom operators
    while (!isAtEnd() && isOperatorChar(peekChar()) && 
           !isWhitespace(peekChar()) && peekChar() != '(' && peekChar() != ')' &&
           peekChar() != '[' && peekChar() != ']' && peekChar() != '{' && peekChar() != '}' &&
           peekChar() != ',' && peekChar() != ';' && peekChar() != ':') {
      advance();
    }
    
    // If we consumed more than one character, it's likely a custom operator
    if (CurPtr - start > 1) {
      StringRef operatorText(start, CurPtr - start);
      // Check for known multi-character operators first
      if (operatorText == "**+") {
        return makeToken(TokenKind::Identifier, start);
      }
      // Fall through to handle other operators
    }
    
    // Reset to handle single character operators
    CurPtr = start + 1;
  } else {
    advance();
  }
  
  switch (c) {
  case '+':
    if (!isAtEnd() && peekChar() == '=') {
      advance();
      return makeToken(TokenKind::PlusEqual, start);
    }
    return makeToken(TokenKind::Plus, start);
    
  case '-':
    if (!isAtEnd() && peekChar() == '=') {
      advance();
      return makeToken(TokenKind::MinusEqual, start);
    }
    if (!isAtEnd() && peekChar() == '>') {
      advance();
      return makeToken(TokenKind::Arrow, start);
    }
    return makeToken(TokenKind::Minus, start);
    
  case '*':
    if (!isAtEnd() && peekChar() == '=') {
      advance();
      return makeToken(TokenKind::StarEqual, start);
    }
    return makeToken(TokenKind::Star, start);
    
  case '/':
    if (!isAtEnd() && peekChar() == '=') {
      advance();
      return makeToken(TokenKind::SlashEqual, start);
    }
    return makeToken(TokenKind::Slash, start);
    
  case '%':
    if (!isAtEnd() && peekChar() == '=') {
      advance();
      return makeToken(TokenKind::PercentEqual, start);
    }
    return makeToken(TokenKind::Percent, start);
    
  case '=':
    if (!isAtEnd() && peekChar() == '=') {
      advance();
      return makeToken(TokenKind::EqualEqual, start);
    }
    if (!isAtEnd() && peekChar() == '>') {
      advance();
      return makeToken(TokenKind::FatArrow, start);
    }
    return makeToken(TokenKind::Equal, start);
    
  case '!':
    if (!isAtEnd() && peekChar() == '=') {
      advance();
      return makeToken(TokenKind::ExclaimEqual, start);
    }
    return makeToken(TokenKind::Exclaim, start);
    
  case '<':
    if (!isAtEnd() && peekChar() == '=') {
      advance();
      return makeToken(TokenKind::LessEqual, start);
    }
    if (!isAtEnd() && peekChar() == '<') {
      advance();
      return makeToken(TokenKind::LessLess, start);
    }
    return makeToken(TokenKind::Less, start);
    
  case '>':
    if (!isAtEnd() && peekChar() == '=') {
      advance();
      return makeToken(TokenKind::GreaterEqual, start);
    }
    if (!isAtEnd() && peekChar() == '>') {
      advance();
      return makeToken(TokenKind::GreaterGreater, start);
    }
    return makeToken(TokenKind::Greater, start);
    
  case '&':
    if (!isAtEnd() && peekChar() == '&') {
      advance();
      return makeToken(TokenKind::AmpAmp, start);
    }
    return makeToken(TokenKind::Amp, start);
    
  case '|':
    if (!isAtEnd() && peekChar() == '|') {
      advance();
      return makeToken(TokenKind::PipePipe, start);
    }
    return makeToken(TokenKind::Pipe, start);
    
  case '?':
    if (!isAtEnd() && peekChar() == '?') {
      advance();
      return makeToken(TokenKind::QuestionQuestion, start);
    }
    return makeToken(TokenKind::Question, start);
    
  case '.':
    if (!isAtEnd() && peekChar() == '.') {
      advance();
      if (!isAtEnd() && peekChar() == '.') {
        advance();
        return makeToken(TokenKind::DotDotDot, start);
      }
      if (!isAtEnd() && peekChar() == '<') {
        advance();
        return makeToken(TokenKind::DotDotLess, start);
      }
      // Put back the second dot
      CurPtr--;
    }
    return makeToken(TokenKind::Dot, start);
    
  default:
    std::string msg = "unexpected character '";
    msg += c;
    msg += "' (ASCII: ";
    msg += std::to_string((unsigned char)c);
    msg += ")";
    Diags.diagnoseError(getCurrentLoc(), msg);
    return makeToken(TokenKind::Unknown, start);
  }
}

void Lexer::lexComment() {
  if (peekChar() == '/' && peekChar(1) == '/') {
    // Single-line comment
    advance(); // consume first '/'
    advance(); // consume second '/'
    
    // Consume until end of line
    while (!isAtEnd() && peekChar() != '\n') {
      advance();
    }
  } else if (peekChar() == '/' && peekChar(1) == '*') {
    // Multi-line comment
    advance(); // consume '/'
    advance(); // consume '*'
    
    // Consume until */
    while (!isAtEnd()) {
      if (peekChar() == '*' && peekChar(1) == '/') {
        advance(); // consume '*'
        advance(); // consume '/'
        break;
      }
      advance();
    }
  }
}

void Lexer::skipWhitespace() {
  while (!isAtEnd() && isWhitespace(peekChar())) {
    advance();
  }
}

bool Lexer::isIdentifierStart(char c) {
  return std::isalpha(c) || c == '_';
}

bool Lexer::isIdentifierContinue(char c) {
  return std::isalnum(c) || c == '_';
}

bool Lexer::isDigit(char c) {
  return std::isdigit(c);
}

bool Lexer::isWhitespace(char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

bool Lexer::isOperatorChar(char c) {
  return c == '+' || c == '-' || c == '*' || c == '/' || c == '%' ||
         c == '=' || c == '!' || c == '<' || c == '>' || c == '&' ||
         c == '|' || c == '^' || c == '~' || c == '?' || c == '.';
}