//===--- Token.h - Token interface ------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Token interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_TOKEN_H
#define SWIFT_SYNTAX_TOKEN_H

#include "swift/AST/RawComment.h"
#include "swift/Basic/String.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/Trivia.h"

#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallString.h"

#include <string>
#include <vector>

namespace swift {

class ASTContext;
struct RawComment;

enum class tok {
  unknown = 0,
  eof,
  code_complete,
  identifier,
  oper_binary_unspaced,   // "x+y"
  oper_binary_spaced,     // "x + y"
  oper_postfix,
  oper_prefix,
  dollarident,
  integer_literal,
  floating_literal,
  string_literal,
  sil_local_name,      // %42 in SIL mode.
  comment,

#define KEYWORD(X) kw_ ## X,
#define PUNCTUATOR(X, Y) X,
#define POUND_KEYWORD(X) pound_ ## X,
#include "swift/Parse/Tokens.def"
  
  NUM_TOKENS
};

namespace syntax {

class Token  {
protected:
  tok Kind;
  String Text;
  TriviaList LeadingTrivia;
  TriviaList TrailingTrivia;

  // FIXME: Reconcile source buffers with synthesized nodes
  SourceLoc Loc;

public:

  Token() : Kind(tok::NUM_TOKENS) {}

  Token(tok Kind) : Kind(Kind) {}

  Token(tok Kind,
        String Text,
        TriviaList LeadingTrivia = {},
        TriviaList TrailingTrivia = {},
        SourceLoc Loc = SourceLoc())
    : Kind(Kind),
      Text(Text),
      LeadingTrivia(LeadingTrivia), TrailingTrivia(TrailingTrivia),
      Loc(Loc) {}

  Token &operator=(const Token &Other) {
    Kind = Other.Kind;
    Text = Other.Text;
    LeadingTrivia = Other.LeadingTrivia;
    TrailingTrivia = Other.TrailingTrivia;
    Loc = Other.Loc;
    return *this;
  }

  // Getters

  tok getKind() const {
    return Kind;
  }

  /// Returns the text of the node, without leading and trailing trivia.
  StringRef getText() const {
    return Text.str();
  }

  /// Returns the absolute source location based on an in-memory buffer,
  /// if the token was created from source text.
  SourceLoc getLoc() const {
    if (isEscapedIdentifier())
      return LeadingTrivia.back().getLoc();
    return Loc;
  }

  /// Return the width of this token without trivia.
  size_t getWidth() const {
    return Text.size();
  }

  /// Return the width of this token, including its leading and
  /// trailing trivia.
  size_t getFullWidth() const {
    auto Width = getWidth();
    for (auto Leader : LeadingTrivia)
      Width += Leader.size();
    for (auto Trailer : TrailingTrivia)
      Width += Trailer.size();
    return Width;
  }

  /// Returns the absolute source range based on an in-memory buffer,
  /// if the token was created from source text.
  CharSourceRange getRange() const {
    return CharSourceRange(getLoc(), getWidth());
  }

  /// Returns the text of the node, including leading and trailing trivia.
  const std::string getFullText() const {
    SmallString<256> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    for (auto Leader : LeadingTrivia)
      Leader.print(OS);

    OS << getText();

    for (auto Trailer : TrailingTrivia)
      Trailer.print(OS);

    return OS.str();
  }

  const TriviaList &getLeadingTrivia() const {
    return LeadingTrivia;
  }

  const TriviaList &getTrailingTrivia() const {
    return TrailingTrivia;
  }

  size_t getIndentation(size_t SpacesPerTab = 4) const {
    if (LeadingTrivia.empty())
      return 0;

    size_t Indentation = 0;
    for (auto i = LeadingTrivia.rbegin(); i != LeadingTrivia.rend(); ++i) {
      auto T = *i;
      if (T.getKind() == TriviaKind::Tab)
        Indentation = T.size() * SpacesPerTab;
      else if (T.getKind() == TriviaKind::Space)
        Indentation += T.size();
      else
        break;
    }
    return Indentation;
  }

  // Modification APIs

  Token withKind(tok NewKind) {
    return Token { NewKind, Text, LeadingTrivia, TrailingTrivia, Loc };
  }

  /// Return a new token with the supplied leading trivia.
  Token withLeadingTrivia(const TriviaList &Leading) {
    return Token { Kind, Text, Leading, TrailingTrivia, Loc };
  }

  /// Return a new token with the supplied trailing trivia.
  Token withTrailingTrivia(const TriviaList &Trailing) {
    return Token { Kind, Text, LeadingTrivia, Trailing, Loc };
  }

  /// Return a new token with the supplied leading and trailing trivia.
  Token withTrivia(const TriviaList &Leading,
                      const TriviaList &Trailing) {
    return Token { Kind, Text, Leading, Trailing, Loc };
  }

  /// If this token was parsed from a source buffer, return the absolute source
  /// location to the first character of the first leading trivia that was
  /// lexed with this token.
  SourceLoc getAbsoluteTriviaStart() const {
    assert(getLoc().isValid() &&
           "This token was not parsed from a source buffer!");
    if (LeadingTrivia.empty())
      return getLoc();

    return LeadingTrivia.front().getLoc();
  }

  // Identity Queries

  bool isAny(tok K1) const {
    return is(K1);
  }

  /// Returns true if the token's kind is any of those proposed.
  template <typename ...T>
  bool isAny(tok K1, tok K2, T... K) const {
    if (is(K1))
      return true;
    return isAny(K2, K...);
  }

  /// Returns true if the token's kind is the one proposed.
  bool is(tok ProposedKind) const {
    return Kind == ProposedKind;
  }

  /// Returns true if the token's kind is not the one proposed.
  bool isNot(tok ProposedKind) const {
    return !is(ProposedKind);
  }

  // Predicates to check to see if the token is not the same as any of a list.
  template <typename ...T>
  bool isNot(tok K1, T... K) const {
    return !isAny(K1, K...);
  }

  bool isBinaryOperator() const {
    return Kind == tok::oper_binary_spaced ||
           Kind == tok::oper_binary_unspaced;
  }

  bool isAnyOperator() const {
    return isBinaryOperator() || Kind == tok::oper_postfix ||
           Kind == tok::oper_prefix;
  }

  bool isNotAnyOperator() const {
    return !isAnyOperator();
  }

  bool isEllipsis() const {
    return isAnyOperator() && getText() == "...";
  }

  bool isNotEllipsis() const {
    return !isEllipsis();
  }

  /// True if the token is an l_paren token that does not start a new line.
  bool isFollowingLParen() const {
    return !isAtStartOfLine() && Kind == tok::l_paren;
  }

  /// True if the token is an l_square token that does not start a new line.
  bool isFollowingLSquare() const {
    return !isAtStartOfLine() && Kind == tok::l_square;
  }

  /// True if the token is any keyword.
  bool isKeyword() const {
    switch (Kind) {
#define KEYWORD(X) case tok::kw_##X: return true;
#include "swift/Parse/Tokens.def"
    default: return false;
    }
  }

  bool isContextualKeyword(StringRef ContextKW) const {
    return is(tok::identifier) && !isEscapedIdentifier() &&
           getText().str() == ContextKW;
  }

  /// Return true if this is a contextual keyword that could be the start of a
  /// decl.
  bool isContextualDeclKeyword() const {
    auto Str = getText();
    if (isNot(tok::identifier) || isEscapedIdentifier() || getText().empty())
      return false;

    switch (Str[0]) {
      case 'c':
        return Str == "convenience";
      case 'd':
        return Str == "dynamic";
      case 'f':
        return Str == "final";
      case 'i':
        return Str == "indirect" || Str == "infix";
      case 'l':
        return Str == "lazy";
      case 'm':
        return Str == "mutating";
      case 'n':
        return Str == "nonmutating";
      case 'o':
        return Str == "open" || Str == "override" || Str == "optional";
      case 'p':
        return Str == "prefix" || Str == "postfix";
      case 'r':
        return Str == "required";
      case 'u':
        return Str == "unowned";
      case 'w':
        return Str == "weak";
      default:
        return false;
    }
  }

  /// Determine whether the token can be an argument label.
  ///
  /// This covers all identifiers and keywords except those keywords
  /// used
  bool canBeArgumentLabel() const {
    // Identifiers, escaped identifiers, and '_' can be argument labels.
    if (is(tok::identifier) || isEscapedIdentifier() || is(tok::kw__))
      return true;

    // 'let', 'var', and 'inout' cannot be argument labels.
    if (isAny(tok::kw_let, tok::kw_var, tok::kw_inout)) return false;

    // All other keywords can be argument labels.
    return isKeyword();
  }

  bool isContextualPunctuator(StringRef ContextPunc) const {
    return isAnyOperator() && getText() == ContextPunc;
  }

  /// \brief Determine whether this token occurred at the start of a line.
  bool isAtStartOfLine() const {
    if (LeadingTrivia.empty())
      return false;

    for (auto Trivia = LeadingTrivia.rbegin(); Trivia != LeadingTrivia.rend();
         ++Trivia)
      if (Trivia->getKind() == TriviaKind::StartOfFile ||
          Trivia->trailingWhitespaceContainsNewline())
        return true;

    return false;
  }

  /// True if the token is an identifier or '_'.
  bool isIdentifierOrUnderscore() const {
    return isAny(tok::identifier, tok::kw__);
  }

  /// \brief True if this token is an escaped identifier token.
  bool isEscapedIdentifier() const {
    if (LeadingTrivia.empty() || TrailingTrivia.empty())
      return false;

    return LeadingTrivia.back().getKind() == TriviaKind::Backtick &&
           TrailingTrivia.front().getKind() == TriviaKind::Backtick;
  }

  std::vector<SingleRawComment>
  getRawCommentPieces(SourceManager &SourceMgr) const;

  bool operator==(const Token &Other) const {
    return Kind == Other.Kind && Text == Other.Text &&
           LeadingTrivia == Other.LeadingTrivia &&
           TrailingTrivia == Other.TrailingTrivia &&
           Loc == Other.Loc;
  }

  bool operator!=(const Token &Other) const {
    return !(*this == Other);
  }

  bool isKnown() const {
    return Kind != tok::unknown && Kind != tok::NUM_TOKENS;
  }

  static Token unknown() {
    return Token { tok::unknown };
  }

  /// Dump the structure of the raw syntax tree for debugging.
  void dump(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  /// Print the raw syntax tree with full formatting fidelity.
  void print(llvm::raw_ostream &OS) const;
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_TOKEN_H

