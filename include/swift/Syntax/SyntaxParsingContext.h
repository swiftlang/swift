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

 #include "swift/Syntax/Syntax.h"

namespace swift {
  class SourceLoc;
  class SourceFile;
  class Token;

namespace syntax {
  struct RawTokenSyntax;
  struct RawSyntax;
  enum class SyntaxKind;

/// The handler for parser to generate libSyntax entities.
struct RawSyntaxInfo {
  /// Start location of this syntax node.
  SourceLoc StartLoc;

  /// The number of tokens belong to the syntax node.
  unsigned TokCount;

  /// The raw node.
  RC<RawSyntax> RawNode;
  RawSyntaxInfo(SourceLoc StartLoc, RC<RawSyntax> RawNode):
    RawSyntaxInfo(StartLoc, 1, RawNode) {}
  RawSyntaxInfo(SourceLoc StartLoc, unsigned TokCount, RC<RawSyntax> RawNode);

  template <typename SyntaxNode>
  SyntaxNode makeSyntax() const { return make<SyntaxNode>(RawNode); }

  template <typename RawSyntaxNode>
  RC<RawSyntaxNode> getRaw() const {
    return RC<RawSyntaxNode>(cast<RawSyntaxNode>(RawNode));
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

  // Add a token syntax at the given source location to the context; this
  // token node can be used to build more complex syntax nodes in later call
  // back.
  virtual void addTokenSyntax(SourceLoc Loc) = 0;

  // Get the context kind.
  virtual SyntaxParsingContextKind getKind() = 0;

  // Create a syntax node of the given kind.
  virtual void makeNode(SyntaxKind Kind) = 0;
  virtual ~SyntaxParsingContext();

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
  void addTokenSyntax(SourceLoc Loc) override {};
  void makeNode(SyntaxKind Kind) override {};
  SyntaxParsingContextKind getKind() override {
    return SyntaxParsingContextKind::Root;
  };
};

enum class SyntaxContextKind: uint8_t{
  Expr,
  Decl,
};

// The base class for contexts that are created from a parent context.
// The stack instance will set the context holder when the context
// is firstly created and reset the context holder to the parent when
// it's destructed.
class SyntaxParsingContextChild: public SyntaxParsingContext {
  SyntaxParsingContext *Parent;
  SyntaxParsingContext *&ContextHolder;
  const SyntaxContextKind Kind;
public:
  SyntaxParsingContextChild(SyntaxParsingContext *&ContextHolder,
                            SyntaxContextKind Kind);
  ~SyntaxParsingContextChild();
  void makeNode(SyntaxKind Kind) override;
  void addTokenSyntax(SourceLoc Loc) override;
  SyntaxParsingContext* getParent() { return Parent; }
  SyntaxParsingContextRoot &getRoot();
  SyntaxParsingContextKind getKind() override {
    return SyntaxParsingContextKind::Child;
  };
};
}
}
#endif // SWIFT_SYNTAX_PARSING_CONTEXT_H

