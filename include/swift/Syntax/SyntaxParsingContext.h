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

namespace swift {
  class SourceLoc;
  class SourceFile;
  class Token;

namespace syntax {
  struct RawTokenSyntax;
  struct RawSyntax;
  enum class SyntaxKind;

enum class SyntaxContextKind: uint8_t{
  Expr,
  Decl,
  Stmt,
};

/// The handler for parser to generate libSyntax entities.
struct RawSyntaxInfo {
  /// Start and end location of this syntax node.
  SourceRange SyntaxRange;

  /// This location must be valid if this node is an implicit node, e.g.
  /// an empty statement list collection.
  /// This location indicates the implicit node should appear before the token
  /// on the location.
  SourceLoc BeforeLoc;

  /// The raw node.
  RC<RawSyntax> RawNode;
  RawSyntaxInfo(RC<RawSyntax> RawNode): RawNode(RawNode) {}
  RawSyntaxInfo(SourceLoc StartLoc, RC<RawSyntax> RawNode):
    SyntaxRange(StartLoc), RawNode(RawNode) {}
  RawSyntaxInfo(SourceRange SyntaxRange, RC<RawSyntax> RawNode):
    SyntaxRange(SyntaxRange), RawNode(RawNode) {}

  bool isImplicit() const { return SyntaxRange.isInvalid(); }
  SourceLoc getStartLoc() const { return SyntaxRange.Start; }
  SourceLoc getEndLoc() const { return SyntaxRange.End; }

  template <typename SyntaxNode>
  SyntaxNode makeSyntax() const { return make<SyntaxNode>(RawNode); }

  template <typename RawSyntaxNode>
  RC<RawSyntaxNode> getRaw() const {
    return RC<RawSyntaxNode>(cast<RawSyntaxNode>(RawNode));
  }
  void brigeWithContext(SyntaxContextKind Kind);
  void setBeforeLoc(SourceLoc Loc) {
    assert(isImplicit());
    BeforeLoc = Loc;
  }
};

enum class SyntaxParsingContextKind: uint8_t {
  Root,
  Child,
};

/// The base class of different kinds of Syntax context that Parser should use to
/// create syntax nodes.
class SyntaxParsingContext {
protected:
  SyntaxParsingContext(SourceFile &SF, unsigned BufferID, Token &Tok);
  SyntaxParsingContext(SyntaxParsingContext &Another);
public:
  struct ContextInfo;
  ContextInfo &ContextData;
  const Token &Tok;

  // Get the context kind.
  virtual SyntaxParsingContextKind getKind() = 0;

  // Create a syntax node of the given kind.
  virtual void makeNode(SyntaxKind Kind, SourceLoc LastTokLoc) = 0;
  virtual ~SyntaxParsingContext();
  virtual void setSyntaxKind(SyntaxKind Kind) = 0;
  virtual void setContextKind(SyntaxContextKind CKind) = 0;
  virtual void finalize() = 0;

  // Disable the building of syntax tree in the current context.
  void disable();
};

// The start point of syntax tree parsing. This context is the root
// of all other entity-specific contexts. This is the context Parser
// has when the parser instance is firstly created.
class SyntaxParsingContextRoot: public SyntaxParsingContext {
  SourceFile &File;
public:
  SyntaxParsingContextRoot(SourceFile &File, unsigned BufferID, Token &Tok):
    SyntaxParsingContext(File, BufferID, Tok), File(File) {}
  ~SyntaxParsingContextRoot();
  void makeNode(SyntaxKind Kind, SourceLoc LastTokLoc) override {};
  void setSyntaxKind(SyntaxKind Kind) override {};
  void setContextKind(SyntaxContextKind CKind) override {};
  void finalize() override {};
  SyntaxParsingContextKind getKind() override {
    return SyntaxParsingContextKind::Root;
  };
};

// The base class for contexts that are created from a parent context.
// The stack instance will set the context holder when the context
// is firstly created and reset the context holder to the parent when
// it's destructed.
class SyntaxParsingContextChild: public SyntaxParsingContext {
  SyntaxParsingContext *Parent;
  SyntaxParsingContext *&ContextHolder;
  Optional<SyntaxContextKind> ContextKind;
  Optional<SyntaxKind> KnownSyntax;
  void makeNodeWhole(SyntaxKind Kind);
  SyntaxParsingContextChild(SyntaxParsingContext *&ContextHolder,
                            Optional<SyntaxContextKind> Kind,
                            Optional<SyntaxKind> KnownSyntax);
  bool isTopOfContextStack() const { return this == ContextHolder; }
public:
  SyntaxParsingContextChild(SyntaxParsingContext *&ContextHolder,
    SyntaxContextKind Kind): SyntaxParsingContextChild(ContextHolder,
                                                       Kind, None) {}

  SyntaxParsingContextChild(SyntaxParsingContext *&ContextHolder,
    SyntaxKind KnownSyntax): SyntaxParsingContextChild(ContextHolder,
                             None, KnownSyntax) {};

  SyntaxParsingContextChild(SyntaxParsingContext *&ContextHolder,
                            bool Disable = false);

  ~SyntaxParsingContextChild();
  void makeNode(SyntaxKind Kind, SourceLoc LastTokLoc) override;
  void finalize() override;
  SyntaxParsingContext* getParent() { return Parent; }
  void setSyntaxKind(SyntaxKind SKind) override;
  void setContextKind(SyntaxContextKind CKind) override;
  SyntaxParsingContextRoot &getRoot();
  SyntaxParsingContextKind getKind() override {
    return SyntaxParsingContextKind::Child;
  };
};
}
}
#endif // SWIFT_SYNTAX_PARSING_CONTEXT_H

