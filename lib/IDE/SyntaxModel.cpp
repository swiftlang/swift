//===- SyntaxModel.cpp - Routines for IDE syntax model --------------------===//
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

#include "swift/IDE/SyntaxModel.h"
#include "swift/AST/ASTContext.h"
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

void SyntaxModelWalker::anchor() {}

struct SyntaxModelContext::Implementation {
  std::vector<SyntaxNode> TokenNodes;
};

SyntaxModelContext::SyntaxModelContext(SourceManager &SM,
                                             unsigned BufferID,
                                             TranslationUnit &TU)
  : Impl(*new Implementation()),
    TU(TU) {
  std::vector<Token> Tokens = swift::tokenize(SM, BufferID, /*Offset=*/0,
                                              /*EndOffset=*/0,
                                              /*KeepComments=*/true,
                                           /*TokenizeInterpolatedString=*/true);
  std::vector<SyntaxNode> Nodes;
  for (auto &Tok : Tokens) {
    SyntaxNodeKind Kind;
    switch(Tok.getKind()) {
#define KEYWORD(X) case tok::kw_##X: Kind = SyntaxNodeKind::Keyword; break;
#include "swift/Parse/Tokens.def"
#undef KEYWORD

    case tok::identifier: Kind = SyntaxNodeKind::Identifier; break;
    case tok::dollarident: Kind = SyntaxNodeKind::DollarIdent; break;
    case tok::integer_literal: Kind = SyntaxNodeKind::Integer; break;
    case tok::floating_literal: Kind = SyntaxNodeKind::Floating; break;
    case tok::string_literal: Kind = SyntaxNodeKind::String; break;
    case tok::character_literal: Kind = SyntaxNodeKind::Character; break;
    case tok::comment:
      if (Tok.getText().startswith("//"))
        Kind = SyntaxNodeKind::CommentLine;
      else
        Kind = SyntaxNodeKind::CommentBlock;
      break;

    default:
      continue;
    }

    assert(Tok.getLoc().isValid());
    assert(Nodes.empty() || SM.isBeforeInBuffer(Nodes.back().Range.getStart(),
                                                Tok.getLoc()));
    Nodes.emplace_back(Kind, CharSourceRange(Tok.getLoc(), Tok.getLength()));
  }

  Impl.TokenNodes = std::move(Nodes);
}

SyntaxModelContext::~SyntaxModelContext() {
  delete &Impl;
}

namespace {

class ModelASTWalker : public ASTWalker {
  const SourceManager &SM;
  std::vector<SyntaxStructureNode> SubStructureStack;
#ifndef NDEBUG
  SourceLoc LastLoc;
#endif

public:
  SyntaxModelWalker &Walker;
  ArrayRef<SyntaxNode> TokenNodes;

  ModelASTWalker(const SourceManager &SM, SyntaxModelWalker &Walker)
      : SM(SM), Walker(Walker) { }

  void visitTranslationUnit(TranslationUnit &TU, ArrayRef<SyntaxNode> Tokens);

  bool walkToDeclPre(Decl *D) override;
  bool walkToDeclPost(Decl *D) override;
  bool walkToTypeReprPre(TypeRepr *T) override;

private:
  enum PassNodesBehavior {
    /// Pass all nodes up to but not including the location.
    ExcludeNodeAtLocation,
    /// Pass all nodes up to and including the location.
    IncludeNodeAtLocation,
    /// Like ExcludeNodeAtLocation, and skip past any node at the location.
    DisplaceNodeAtLocation
  };
  bool passTokenNodesUntil(SourceLoc Loc, PassNodesBehavior Behavior);
  bool passNonTokenNode(const SyntaxNode &Node);
  bool passNode(const SyntaxNode &Node);
  bool pushStructureNode(const SyntaxStructureNode &Node);
  bool popStructureNode();
};

SyntaxStructureKind syntaxStructureKindFromNominalTypeDecl(NominalTypeDecl *N) {
  if (isa<ClassDecl>(N))
    return SyntaxStructureKind::Class;
  else if (isa<StructDecl>(N))
    return SyntaxStructureKind::Struct;
  else if (isa<ProtocolDecl>(N))
    return SyntaxStructureKind::Protocol;
  else {
    // All other known NominalTypeDecl derived classes covered, so assert() here.
    assert(isa<EnumDecl>(N));
    return SyntaxStructureKind::Enum;
  }
}

} // anonymous namespace

bool SyntaxModelContext::walk(SyntaxModelWalker &Walker) {
  ModelASTWalker ASTWalk(TU.Ctx.SourceMgr, Walker);
  ASTWalk.visitTranslationUnit(TU, Impl.TokenNodes);
  return true;
}

void ModelASTWalker::visitTranslationUnit(TranslationUnit &TU,
                                          ArrayRef<SyntaxNode> Tokens) {
  TokenNodes = Tokens;
  TU.walk(*this);

  // Pass the rest of the token nodes.
  for (auto &TokNode : TokenNodes)
    passNode(TokNode);
}

bool ModelASTWalker::walkToDeclPre(Decl *D) {
  if (FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
    if (FD->isGetterOrSetter()) {
      // Pass get / set context sensitive keyword token.
      SourceLoc SL = FD->getDefLoc();
      if (!passNonTokenNode({ SyntaxNodeKind::Keyword,
                              CharSourceRange(SL, 3)
                            }))
        return false;
    }
    else {
      SourceRange SR = FD->getSourceRange();
      SourceLoc NL = FD->getNameLoc();
      SyntaxStructureKind Kind;
      const DeclContext *DC = FD->getDeclContext();
      if (DC->isTypeContext()) {
        if (FD->isStatic())
          Kind = SyntaxStructureKind::StaticFunction;
        else
          Kind = SyntaxStructureKind::InstanceFunction;
      }
      else
         Kind = SyntaxStructureKind::FreeFunction;

      pushStructureNode({Kind, CharSourceRange(SM, SR.Start, SR.End),
                         CharSourceRange(SM, NL, NL.getAdvancedLoc(
                                                  FD->getName().getLength()))});
    }
  }
  else if (NominalTypeDecl *NTD = dyn_cast<NominalTypeDecl>(D)) {
    SyntaxStructureKind Kind = syntaxStructureKindFromNominalTypeDecl(NTD);
    SourceRange SR = NTD->getSourceRange();
    SourceLoc NL = NTD->getNameLoc();
    pushStructureNode({Kind, CharSourceRange(SM, SR.Start, SR.End),
                       CharSourceRange(SM, NL, NL.getAdvancedLoc(
                                                 NTD->getName().getLength()))});
  }
  else if (VarDecl *VD = dyn_cast<VarDecl>(D)) {
    const DeclContext *DC = VD->getDeclContext();
    if (DC->isTypeContext()) {
      SourceRange SR = VD->getSourceRange();
      SourceLoc NL = VD->getNameLoc();
      pushStructureNode({SyntaxStructureKind::InstanceVariable,
                         CharSourceRange(SM, SR.Start, SR.End),
                         CharSourceRange(SM, NL, NL.getAdvancedLoc(
                                                  VD->getName().getLength()))});
    }
  }
  return true;
}

bool ModelASTWalker::walkToDeclPost(swift::Decl *D) {
  if (FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
    if (!FD->isGetterOrSetter()) {
      popStructureNode();
    }
  }
  else if (VarDecl *VD = dyn_cast<VarDecl>(D)) {
    const DeclContext *DC = VD->getDeclContext();
    if (DC->isTypeContext()) {
      popStructureNode();
    }
  }
  else if (isa<NominalTypeDecl>(D)) {
    popStructureNode();
  }
  return true;
}

bool ModelASTWalker::walkToTypeReprPre(TypeRepr *T) {
  if (IdentTypeRepr *IdT = dyn_cast<IdentTypeRepr>(T)) {
    for (auto &comp : IdT->Components) {
      if (!passNonTokenNode({ SyntaxNodeKind::TypeId,
                              CharSourceRange(comp.getIdLoc(),
                                              comp.getIdentifier().getLength())
                            }))
        return false;
    }
  }
  return true;
}

bool ModelASTWalker::passTokenNodesUntil(SourceLoc Loc,
                                         PassNodesBehavior Behavior) {
  assert(Loc.isValid());
  unsigned I = 0;
  for (unsigned E = TokenNodes.size(); I != E; ++I) {
    SourceLoc TokLoc = TokenNodes[I].Range.getStart();
    if (SM.isBeforeInBuffer(Loc, TokLoc)) {
      break;
    }
    if (TokLoc == Loc && Behavior != IncludeNodeAtLocation) {
      if (Behavior == DisplaceNodeAtLocation) {
        // Skip past the node directly at the specified location, allowing the
        // caller to effectively replace it.
        ++I;
      }
      break;
    }
    if (!passNode(TokenNodes[I]))
      return false;
  }

  TokenNodes = TokenNodes.slice(I);
  return true;
}

bool ModelASTWalker::passNonTokenNode(const SyntaxNode &Node) {
  if (!passTokenNodesUntil(Node.Range.getStart(), DisplaceNodeAtLocation))
    return false;
  if (!passNode(Node))
    return false;
  return true;
}

bool ModelASTWalker::passNode(const SyntaxNode &Node) {
#ifndef NDEBUG
  assert(!SM.isBeforeInBuffer(Node.Range.getStart(), LastLoc));
  LastLoc = Node.Range.getStart();
#endif
  Walker.walkToNodePre(Node);
  if (!Walker.walkToNodePost(Node))
    return false;
  return true;
}

bool ModelASTWalker::pushStructureNode(const SyntaxStructureNode &Node) {
  SubStructureStack.push_back(Node);

  if (!passTokenNodesUntil(Node.Range.getStart(), ExcludeNodeAtLocation))
    return false;
  if (!Walker.walkToSubStructurePre(Node))
    return false;

  return true;
}

bool ModelASTWalker::popStructureNode() {
  assert(!SubStructureStack.empty());
  SyntaxStructureNode Node = SubStructureStack.back();
  SubStructureStack.pop_back();

  if (!passTokenNodesUntil(Node.Range.getEnd(), ExcludeNodeAtLocation))
    return false;
  if (!Walker.walkToSubStructurePost(Node))
    return false;

  return true;
}
