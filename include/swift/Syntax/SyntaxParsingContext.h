//===----------- SyntaxParsingContext.h -==============----------*- C++ -*-===//
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

#ifndef SWIFT_SYNTAX_PARSING_CONTEXT_H
#define SWIFT_SYNTAX_PARSING_CONTEXT_H

#include "swift/Basic/SourceLoc.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/TokenSyntax.h"

namespace swift {
class SourceLoc;
class SourceFile;
class Token;

namespace syntax {
struct RawTokenSyntax;
struct RawSyntax;
enum class SyntaxKind;

enum class SyntaxContextKind {
  Decl,
  Stmt,
  Expr,
  Type,
  Pattern,
  Syntax,
};

/// Indicates what action should be performed on the destruction of
///  SyntaxParsingContext
enum class AccumulationMode {
  // Coerece the result to one of ContextKind.
  // E.g. for ContextKind::Expr, passthroug if the result is CallExpr, whereas
  // <UnknownExpr><SomeDecl /></UnknownDecl> for non Exprs.
  CoerceKind,

  // Construct a result Syntax with specified SyntaxKind.
  CreateSyntax,

  // Pass through all parts to the parent context.
  Transparent,

  // Discard all parts in the context.
  Discard,

  // Construct SourceFile syntax to the specified SF.
  Root,

  // Invalid.
  NotSet,
};

/// RAII object which receive RawSyntax parts. On destruction, this constructs
/// a specified syntax node from received parts and propagate it to the parent
/// context.
///
/// e.g.
///   parseExprParen() {
///     SyntaxParsingContext LocalCtxt(SyntaxKind::ParenExpr, SyntaxContext);
///     consumeToken(tok::l_paren) // In consumeToken(), a RawTokenSyntax is
///                                // added to the context.
///     parseExpr(); // On returning from parseExpr(), a Expr Syntax node is
///                  // created and added to the context.
///     consumeToken(tok::r_paren)
///     // Now the context holds { '(' Expr ')' }.
///     // From these parts, it creates ParenExpr node and add it to the parent.
///   }
class SyntaxParsingContext {
  // Parent context. Only the root context has nullptr.
  SyntaxParsingContext *Parent;

  // Reference to the
  SyntaxParsingContext *&CtxtHolder;

  // Collected parts.
  std::vector<RC<RawSyntax>> Parts;

  // Operation on destruction.
  AccumulationMode Mode = AccumulationMode::NotSet;

  // Additional info depending on \c Mode.
  union {
    // For AccumulationMode::CreateSyntax; desired syntax node kind.
    SyntaxKind SynKind;
    // For AccumulationMode::CoerceKind; desired syntax node category.
    SyntaxContextKind CtxtKind;
    // For AccumulationMode::Root; the parsing source file.
    SourceFile *SF;
  };

  // If false, context does nothing.
  bool Enabled;

  /// Create a syntax node using the tail \c N elements of collected parts and
  /// replace those parts with the single result.
  void createNodeInPlace(SyntaxKind Kind, size_t N);

public:
  /// Construct root context.
  SyntaxParsingContext(SyntaxParsingContext *&CtxtHolder, SourceFile &SF);

  /// Designated constructor for child context.
  SyntaxParsingContext(SyntaxParsingContext *&CtxtHolder)
      : Parent(CtxtHolder), CtxtHolder(CtxtHolder),
        Enabled(Parent->isEnabled()) {
    assert(CtxtHolder->isTopOfContextStack() &&
           "SyntaxParsingContext cannot have multiple children");
    CtxtHolder = this;
  }

  SyntaxParsingContext(SyntaxParsingContext *&CtxtHolder, SyntaxContextKind Kind)
      : SyntaxParsingContext(CtxtHolder) {
    setCoerceKind(Kind);
  }

  SyntaxParsingContext(SyntaxParsingContext *&CtxtHolder, SyntaxKind Kind)
      : SyntaxParsingContext(CtxtHolder) {
    setCreateSyntax(Kind);
  }

  ~SyntaxParsingContext();

  void disable() { Enabled = false; }
  bool isEnabled() const { return Enabled; }
  bool isRoot() const { return !Parent; }
  bool isTopOfContextStack() const { return this == CtxtHolder; }

  SyntaxParsingContext *getRoot();

  /// Add RawSyntax to the parts.
  void addRawSyntax(RC<RawSyntax> Raw);

  /// Add Token with Trivia to the parts.
  void addToken(Token &Tok, Trivia &LeadingTrivia, Trivia &TrailingTrivia);

  /// Add Syntax to the parts.
  void addSyntax(Syntax Node);

  RC<RawSyntax> popBack() {
    auto Raw = std::move(Parts.back());
    Parts.pop_back();
    return Raw;
  }

  template<typename SyntaxNode>
  llvm::Optional<SyntaxNode> popIf() {
    if (auto Node = make<Syntax>(Parts.back()).getAs<SyntaxNode>()) {
      Parts.pop_back();
      return Node;
    }
    return None;
  }

  TokenSyntax popToken() {
    assert(Parts.back()->Kind == SyntaxKind::Token);
    return make<TokenSyntax>(popBack());
  }

  /// Create a node using the tail of the collected parts. The number of parts
  /// is automatically determined from \c Kind. Node: limited number of \c Kind
  /// are supported. See the implementation.
  void createNodeInPlace(SyntaxKind Kind);

  /// On destruction, construct a specified kind of RawSyntax node consuming the
  /// collected parts, then append it to the parent context.
  void setCreateSyntax(SyntaxKind Kind) {
    Mode = AccumulationMode::CreateSyntax;
    SynKind = Kind;
  }

  /// On destruction, if the parts size is 1 and it's kind of \c Kind, just
  /// append it to the parent context. Otherwise, create Unknown{Kind} node from
  /// the collected parts.
  void setCoerceKind(SyntaxContextKind Kind) {
    Mode = AccumulationMode::CoerceKind;
    CtxtKind = Kind;
  }

  /// Move the collected parts to the tail of parent context.
  void setTransparent() { Mode = AccumulationMode::Transparent; }

  /// Discard collected parts on this context.
  void setDiscard() { Mode = AccumulationMode::Discard; }

};

} // namespace syntax
} // namespace swift
#endif // SWIFT_SYNTAX_PARSING_CONTEXT_H
