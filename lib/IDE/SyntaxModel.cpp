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

SyntaxModelContext::SyntaxModelContext(SourceManager &SM, unsigned BufferID,
                                       Module &M)
  : Impl(*new Implementation()),
    M(M) {
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
  SourceLoc LastLoc;

public:
  SyntaxModelWalker &Walker;
  ArrayRef<SyntaxNode> TokenNodes;

  ModelASTWalker(const SourceManager &SM, SyntaxModelWalker &Walker)
      : SM(SM), Walker(Walker) { }

  void visitModule(Module &M, ArrayRef<SyntaxNode> Tokens);

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

CharSourceRange charSourceRangeFromSourceRange(SourceManager &SM,
                                               const SourceRange &SR) {
  SourceLoc SRE = Lexer::getLocForEndOfToken(SM, SR.End);
  return CharSourceRange(SM, SR.Start, SRE);
}

} // anonymous namespace

bool SyntaxModelContext::walk(SyntaxModelWalker &Walker) {
  ModelASTWalker ASTWalk(M.Ctx.SourceMgr, Walker);
  ASTWalk.visitModule(M, Impl.TokenNodes);
  return true;
}

void ModelASTWalker::visitModule(Module &M, ArrayRef<SyntaxNode> Tokens) {
  TokenNodes = Tokens;
  M.walk(*this);

  // Pass the rest of the token nodes.
  for (auto &TokNode : TokenNodes)
    passNode(TokNode);
}

bool ModelASTWalker::walkToDeclPre(Decl *D) {
  if (AbstractFunctionDecl *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
    FuncDecl *FD = dyn_cast<FuncDecl>(AFD);
    if (FD && FD->isGetterOrSetter()) {
      // Pass get / set context sensitive keyword token.
      SourceLoc SL = FD->getFuncLoc();
      if (!passNonTokenNode({ SyntaxNodeKind::Keyword, CharSourceRange(SL, 3)}))
        return false;
    }
    else {
      // Pass Function / Method structure node.
      SyntaxStructureNode SN;
      const DeclContext *DC = AFD->getDeclContext();
      if (DC->isTypeContext()) {
        if (FD && FD->isStatic())
          SN.Kind = SyntaxStructureKind::StaticFunction;
        else
          SN.Kind = SyntaxStructureKind::InstanceFunction;
      }
      else
        SN.Kind = SyntaxStructureKind::FreeFunction;
      ASTContext &AC = AFD->getASTContext();
      SN.Range = charSourceRangeFromSourceRange(AC.SourceMgr,
                                                          AFD->getSourceRange());
      SourceLoc NRStart = AFD->getNameLoc();
      SourceLoc NREnd = NRStart.getAdvancedLoc(AFD->getName().getLength());
      SN.NameRange = CharSourceRange(SM, NRStart, NREnd);
      SN.Attrs = AFD->getAttrs();
      pushStructureNode(SN);
    }
  }
  else if (NominalTypeDecl *NTD = dyn_cast<NominalTypeDecl>(D)) {
    SyntaxStructureNode SN;
    SN.Kind = syntaxStructureKindFromNominalTypeDecl(NTD);
    ASTContext &AC = NTD->getASTContext();
    SN.Range = charSourceRangeFromSourceRange(AC.SourceMgr,
                                                  NTD->getSourceRange());
    SourceLoc NRStart = NTD->getNameLoc();
    SourceLoc NREnd = NRStart.getAdvancedLoc(NTD->getName().getLength());
    SN.NameRange = CharSourceRange(SM, NRStart, NREnd);

    for (const TypeLoc &TL : NTD->getInherited()) {
      CharSourceRange TR = charSourceRangeFromSourceRange(AC.SourceMgr,
                                                          TL.getSourceRange());
      SN.InheritedTypeRanges.push_back(TR);
    }

    SN.Attrs = NTD->getAttrs();
    pushStructureNode(SN);
  }
  else if (VarDecl *VD = dyn_cast<VarDecl>(D)) {
    const DeclContext *DC = VD->getDeclContext();
    if (DC->isTypeContext()) {
      SyntaxStructureNode SN;
      SourceRange SR;
      if (PatternBindingDecl *PD = VD->getParentPattern())
        SR = PD->getSourceRange();
      else
        SR = VD->getSourceRange();
      ASTContext &AC = VD->getASTContext();
      SN.Range = charSourceRangeFromSourceRange(AC.SourceMgr, SR);
      SourceLoc NRStart = VD->getNameLoc();
      SourceLoc NREnd = NRStart.getAdvancedLoc(VD->getName().getLength());
      SN.NameRange = CharSourceRange(SM, NRStart, NREnd);
      SN.Kind = SyntaxStructureKind::InstanceVariable;
      SN.Attrs = VD->getAttrs();
      pushStructureNode(SN);
    }
  }
  return true;
}

bool ModelASTWalker::walkToDeclPost(swift::Decl *D) {
  if (isa<AbstractFunctionDecl>(D)) {
    FuncDecl *FD = dyn_cast<FuncDecl>(D);
    if (!(FD && FD->isGetterOrSetter())) {
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
  // Skip out of order non-token nodes.
  // Ideally this shouldn't happen, but the AST can contain overlapping nodes,
  // such as multiple PatternBindingDecl in code like: var a, b : Int. Which
  // would cause us to report the TypeRepr twice.
  if (!SM.isBeforeInBuffer(LastLoc, Node.Range.getStart()))
    return false;

  if (!passTokenNodesUntil(Node.Range.getStart(), DisplaceNodeAtLocation))
    return false;
  if (!passNode(Node))
    return false;
  return true;
}

bool ModelASTWalker::passNode(const SyntaxNode &Node) {
  assert(!SM.isBeforeInBuffer(Node.Range.getStart(), LastLoc));
  LastLoc = Node.Range.getStart();

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

  if (!Walker.walkToSubStructurePost(Node))
    return false;

  return true;
}
