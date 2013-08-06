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
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Token.h"
#include "swift/Subsystems.h"
#include "llvm/Support/MemoryBuffer.h"
#include <vector>

using namespace swift;
using namespace ide;

void SyntaxColorWalker::anchor() {}

struct SyntaxColoringContext::Implementation {
  std::vector<SyntaxNode> TokenNodes;
};

/// \brief Tokenizes a string literal, taking into account string interpolation.
static std::vector<Token>
getStringPartTokens(const Token &Tok, SourceManager &SM, int BufID,
                    ASTContext &Ctx) {
  assert(Tok.is(tok::string_literal));
  llvm::SmallVector<Lexer::StringSegment, 4> Segments;
  Lexer::getStringLiteralSegments(Tok, Segments, /*Diags=*/0);
  std::vector<Token> Toks;
  for (unsigned i = 0, e = Segments.size(); i != e; ++i) {
    Lexer::StringSegment &Seg = Segments[i];
    bool isFirst = i == 0;
    bool isLast = i == e-1;
    if (Seg.Kind == Lexer::StringSegment::Literal) {
      SourceLoc Loc = Seg.Loc;
      unsigned Len = Seg.Length;
      if (isFirst) {
        // Include the quote.
        Loc = Loc.getAdvancedLoc(-1);
        ++Len;
      }
      if (isLast) {
        // Include the quote.
        ++Len;
      }
      StringRef Text(Loc.Value.getPointer(), Len);
      Token NewTok;
      NewTok.setToken(tok::string_literal, Text);
      Toks.push_back(NewTok);

    } else {
      const llvm::MemoryBuffer *Buffer = SM->getMemoryBuffer(BufID);
      unsigned Offset = Seg.Loc.Value.getPointer() -
                        Buffer->getBufferStart();
      unsigned EndOffset = Offset + Seg.Length;
      std::vector<Token> NewTokens = swift::tokenize(SM, BufID, Offset,
                                                     EndOffset,
                                                     /*KeepComments=*/true);
      Toks.insert(Toks.end(), NewTokens.begin(), NewTokens.end());
    }
  }

  return Toks;
}

SyntaxColoringContext::SyntaxColoringContext(SourceManager &SM,
                                             unsigned BufferID,
                                             TranslationUnit &TU)
  : Impl(*new Implementation()),
    TU(TU) {
  std::vector<Token> Tokens = swift::tokenize(SM, BufferID, /*Offset=*/0,
                                              /*EndOffset=*/0,
                                              /*KeepComments=*/true);
  // Handle string interpolation.
  for (unsigned i = 0; i != Tokens.size(); ++i) {
    Token Tok = Tokens[i];
    if (Tok.is(tok::string_literal)) {
      std::vector<Token> NewToks = getStringPartTokens(Tok, SM,BufferID,TU.Ctx);
      assert(!NewToks.empty());
      Tokens[i] = NewToks[0];
      Tokens.insert(Tokens.begin()+i+1, NewToks.begin()+1, NewToks.end());
      i += NewToks.size()-1;
    }
  }

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
           Nodes.back().Loc.Value.getPointer()<Tok.getLoc().Value.getPointer());
    Nodes.emplace_back(Kind, Tok.getLoc(), Tok.getLength());
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
      if (!passNonTokenNode({ SyntaxColor::TypeId, comp.getIdLoc(),
                              comp.getIdentifier().getLength() }))
        return false;
    }
  }
  return true;
}

bool ColorASTWalker::passTokenNodesUntil(SourceLoc Loc, bool Inclusive) {
  assert(Loc.isValid());
  unsigned I = 0;
  for (unsigned E = TokenNodes.size(); I != E; ++I) {
    SourceLoc TokLoc = TokenNodes[I].Loc;
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
  if (!passTokenNodesUntil(Node.Loc, /*Inclusive=*/false))
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
