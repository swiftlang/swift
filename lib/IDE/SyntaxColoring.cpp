//===- SyntaxColoring.cpp - Routines for syntax coloring ------------------===//
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

#include "swift/IDE/SyntaxColoring.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Parse/Token.h"
#include "swift/Subsystems.h"
#include <vector>

using namespace swift;
using namespace ide;

void SyntaxColorWalker::anchor() {}

struct SyntaxColoringContext::Implementation {
  std::vector<SyntaxNode> TokenNodes;
};

SyntaxColoringContext::SyntaxColoringContext(SourceManager &SM,
                                             unsigned BufferID,
                                             TranslationUnit &TU)
  : Impl(*new Implementation()),
    TU(TU) {
  std::vector<Token> Tokens = swift::tokenize(SM, BufferID, /*Offset=*/0,
                                              /*EndOffset=*/0,
                                              /*KeepComments=*/true);
  std::vector<SyntaxNode> Nodes;
  for (auto &Tok : Tokens) {
    SyntaxColor Kind;
    switch(Tok.getKind()) {
#define KEYWORD(X) case tok::kw_##X: Kind = SyntaxColor::Keyword; break;
#include "swift/Parse/Tokens.def"
#undef KEYWORD

    case tok::dollarident: Kind = SyntaxColor::DollarIdent; break;
    case tok::integer_literal: Kind = SyntaxColor::Integer; break;
    case tok::floating_literal: Kind = SyntaxColor::Floating; break;
    case tok::string_literal: Kind = SyntaxColor::String; break;
    case tok::character_literal: Kind = SyntaxColor::Character; break;
    case tok::comment:
      if (Tok.getText().startswith("//"))
        Kind = SyntaxColor::CommentLine;
      else
        Kind = SyntaxColor::CommentBlock;
      break;

    default:
      continue;
    }

    assert(Tok.getLoc().isValid());
    assert(Nodes.empty() ||
           Nodes.back().Range.End.Value.getPointer()<Tok.getLoc().Value.getPointer());
    Nodes.emplace_back(Kind, Tok.getLoc());
  }

  Impl.TokenNodes = std::move(Nodes);
}

SyntaxColoringContext::~SyntaxColoringContext() {
  delete &Impl;
}

namespace {

class ColorASTWalker : public ASTWalker {
public:
  SyntaxColorWalker &SCWalker;
  ArrayRef<SyntaxNode> TokenNodes;

  ColorASTWalker(SyntaxColorWalker &SCWalker)
    : SCWalker(SCWalker) { }

  void visitTranslationUnit(TranslationUnit &TU, ArrayRef<SyntaxNode> Tokens);

  virtual bool walkToTypeReprPre(TypeRepr *T);

private:
  bool passTokenNodesUntil(SourceLoc Loc, bool Inclusive);
  bool passNonTokenNode(const SyntaxNode &Node);
  bool passNode(const SyntaxNode &Node);
};

} // anonymous namespace

bool SyntaxColoringContext::walk(SyntaxColorWalker &Walker) {
  ColorASTWalker ASTWalk(Walker);
  ASTWalk.visitTranslationUnit(TU, Impl.TokenNodes);
  return true;
}

void ColorASTWalker::visitTranslationUnit(TranslationUnit &TU,
                                          ArrayRef<SyntaxNode> Tokens) {
  TokenNodes = Tokens;
  for (Decl *D : TU.Decls)
    D->walk(*this);

  // Pass the rest of the token nodes.
  for (auto &TokNode : TokenNodes)
    passNode(TokNode);
}

bool ColorASTWalker::walkToTypeReprPre(TypeRepr *T) {
  if (IdentTypeRepr *IdT = dyn_cast<IdentTypeRepr>(T)) {
    for (auto &comp : IdT->Components) {
      if (!passNonTokenNode({ SyntaxColor::TypeId, comp.getIdLoc() }))
        return false;
    }
  }
  return true;
}

bool ColorASTWalker::passTokenNodesUntil(SourceLoc Loc, bool Inclusive) {
  assert(Loc.isValid());
  unsigned I = 0;
  for (unsigned E = TokenNodes.size(); I != E; ++I) {
    SourceLoc TokLoc = TokenNodes[I].Range.Start;
    if (TokLoc.Value.getPointer() > Loc.Value.getPointer() ||
        (!Inclusive && TokLoc.Value.getPointer() == Loc.Value.getPointer())) {
      break;
    }
    if (!passNode(TokenNodes[I]))
      return false;
  }

  TokenNodes = TokenNodes.slice(I);
  return true;
}

bool ColorASTWalker::passNonTokenNode(const SyntaxNode &Node) {
  if (!passTokenNodesUntil(Node.Range.Start, /*Inclusive=*/false))
    return false;
  if (!passNode(Node))
    return false;
  return true;
}

bool ColorASTWalker::passNode(const SyntaxNode &Node) {
  SCWalker.walkToNodePre(Node);
  if (!SCWalker.walkToNodePost(Node))
    return false;
  return true;
}
