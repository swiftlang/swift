#ifndef SWIFTC_LEXER_TOKEN_H
#define SWIFTC_LEXER_TOKEN_H

#include "swiftc/Basic/LLVM.h"
#include "swiftc/Basic/SourceLoc.h"

namespace swiftc {

enum class TokenKind {
  // EOF and Unknown
  Eof,
  Unknown,
  
  // Identifiers and literals
  Identifier,
  IntegerLiteral,
  FloatingPointLiteral,
  StringLiteral,
  
  // Keywords
  Let,
  Var,
  Func,
  Class,
  Struct,
  Enum,
  Protocol,
  Extension,
  If,
  Else,
  For,
  While,
  Repeat,
  Switch,
  Case,
  Default,
  Break,
  Continue,
  Return,
  Throw,
  Try,
  Catch,
  Guard,
  Do,
  Import,
  Public,
  Private,
  Internal,
  Fileprivate,
  Open,
  Static,
  Final,
  Override,
  Mutating,
  Nonmutating,
  Lazy,
  Weak,
  Unowned,
  Optional,
  Required,
  Convenience,
  Dynamic,
  Infix,
  Prefix,
  Postfix,
  Operator,
  Precedencegroup,
  Associatedtype,
  Typealias,
  Init,
  Deinit,
  Subscript,
  Willset,
  Didset,
  Get,
  Set,
  Where,
  Self_,
  Super,
  Nil,
  True,
  False,
  As,
  Is,
  In,
  Inout,
  Some,
  Any,
  
  // Operators
  Plus,              // +
  Minus,             // -
  Star,              // *
  Slash,             // /
  Percent,           // %
  Equal,             // =
  PlusEqual,         // +=
  MinusEqual,        // -=
  StarEqual,         // *=
  SlashEqual,        // /=
  PercentEqual,      // %=
  EqualEqual,        // ==
  ExclaimEqual,      // !=
  Less,              // <
  Greater,           // >
  LessEqual,         // <=
  GreaterEqual,      // >=
  AmpAmp,            // &&
  PipePipe,          // ||
  Exclaim,           // !
  Question,          // ?
  QuestionQuestion,  // ??
  Amp,               // &
  Pipe,              // |
  Caret,             // ^
  Tilde,             // ~
  LessLess,          // <<
  GreaterGreater,    // >>
  Arrow,             // ->
  FatArrow,          // =>
  At,                // @
  Hash,              // #
  Dollar,            // $
  Backtick,          // `
  Backslash,         // \
  
  // Punctuation
  LeftParen,         // (
  RightParen,        // )
  LeftBrace,         // {
  RightBrace,        // }
  LeftBracket,       // [
  RightBracket,      // ]
  Comma,             // ,
  Semicolon,         // ;
  Colon,             // :
  Dot,               // .
  DotDotDot,         // ...
  DotDotLess,        // ..<
  LeftBraceBrace,    // {{
  RightBraceBrace,   // }}
  
  // Whitespace and comments (usually skipped)
  Whitespace,
  Comment,
  
  NUM_TOKENS
};

class Token {
  TokenKind Kind;
  SourceLoc Loc;
  StringRef Text;

public:
  Token() : Kind(TokenKind::Unknown), Loc(SourceLoc::getInvalidLoc()) {}
  Token(TokenKind kind, SourceLoc loc, StringRef text)
      : Kind(kind), Loc(loc), Text(text) {}

  TokenKind getKind() const { return Kind; }
  SourceLoc getLoc() const { return Loc; }
  StringRef getText() const { return Text; }
  
  bool is(TokenKind kind) const { return Kind == kind; }
  bool isNot(TokenKind kind) const { return Kind != kind; }
  
  bool isKeyword() const;
  bool isOperator() const;
  bool isLiteral() const;
  
  /// Get the source range of this token.
  SourceRange getRange() const {
    return SourceRange(Loc, SourceLoc(Loc.getRawValue() + Text.size()));
  }
  
  /// Check if this token is at the start of a line.
  bool isAtStartOfLine() const;
};

/// Get the string representation of a token kind.
StringRef getTokenKindName(TokenKind kind);

/// Check if a string is a Swift keyword.
TokenKind getKeywordKind(StringRef text);

} // namespace swiftc

#endif // SWIFTC_LEXER_TOKEN_H