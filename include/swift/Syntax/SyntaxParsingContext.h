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

#include "swift/Syntax/RawTokenSyntax.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/TokenKinds.h"
#include "swift/Syntax/Trivia.h"

namespace swift {
  class SourceFile;

namespace syntax {

struct RawTokenInfo {
  SourceLoc Loc;
  RC<RawTokenSyntax> Token;
};

enum class SyntaxParsingContextKind: uint8_t {
  Root,
  Expr,
};

class SyntaxParsingContext {
protected:
  SyntaxParsingContext(SourceFile &File, bool Enabled);
  SyntaxParsingContext(SyntaxParsingContext &Another);
public:
  struct Implementation;
  Implementation &Impl;
  void addTokenSyntax(SourceLoc Loc);
  virtual SyntaxParsingContextKind getKind() = 0;
  virtual void makeNode(SyntaxKind Kind) = 0;
  virtual ~SyntaxParsingContext();
  void disable();
};

class SyntaxParsingContextRoot: public SyntaxParsingContext {
public:
  SyntaxParsingContextRoot(SourceFile &SF, unsigned BufferID);
  ~SyntaxParsingContextRoot();
  void makeNode(SyntaxKind Kind) override {};
  SyntaxParsingContextKind getKind() override {
    return SyntaxParsingContextKind::Root;
  };
};

class SyntaxParsingContextChild: public SyntaxParsingContext {
  SyntaxParsingContext *Parent;
  SyntaxParsingContext *&ContextHolder;
protected:
  SyntaxParsingContextChild(SyntaxParsingContext *&ContextHolder):
    SyntaxParsingContext(*ContextHolder), Parent(ContextHolder),
    ContextHolder(ContextHolder) {
      ContextHolder = this;
  }
  ~SyntaxParsingContextChild();
};

class SyntaxParsingContextExpr: public SyntaxParsingContextChild {
public:
  SyntaxParsingContextExpr(SyntaxParsingContext *&ContextHolder):
    SyntaxParsingContextChild(ContextHolder) {}
  ~SyntaxParsingContextExpr();
  void makeNode(SyntaxKind Kind) override;
  SyntaxParsingContextKind getKind() override {
    return SyntaxParsingContextKind::Expr;
  };
};
}
}
#endif // SWIFT_SYNTAX_PARSING_CONTEXT_H

