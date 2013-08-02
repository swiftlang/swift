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
                                             TranslationUnit *TU)
  : Impl(*new Implementation()) {
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

bool SyntaxColoringContext::walk(SyntaxColorWalker &Walker) {
  for (auto &Node : Impl.TokenNodes) {
    Walker.walkToNodePre(Node);
    if (!Walker.walkToNodePost(Node))
      return false;
  }

  return true;
}
