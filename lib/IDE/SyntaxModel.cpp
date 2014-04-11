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
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Token.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/MemoryBuffer.h"
#include <vector>

using namespace swift;
using namespace ide;

void SyntaxModelWalker::anchor() {}

struct SyntaxModelContext::Implementation {
  SourceFile &SrcFile;
  const LangOptions &LangOpts;
  const SourceManager &SrcMgr;
  std::vector<SyntaxNode> TokenNodes;

  Implementation(SourceFile &SrcFile)
    : SrcFile(SrcFile),
      LangOpts(SrcFile.getASTContext().LangOpts),
      SrcMgr(SrcFile.getASTContext().SourceMgr) {}
};

SyntaxModelContext::SyntaxModelContext(SourceFile &SrcFile)
  : Impl(*new Implementation(SrcFile)) {
  const SourceManager &SM = Impl.SrcMgr;
  std::vector<Token> Tokens = swift::tokenize(Impl.LangOpts, SM,
                                              *Impl.SrcFile.getBufferID(),
                                              /*Offset=*/0,
                                              /*EndOffset=*/0,
                                              /*KeepComments=*/true,
                                           /*TokenizeInterpolatedString=*/true);
  std::vector<SyntaxNode> Nodes;
  SourceLoc AttrLoc;
  for (auto &Tok : Tokens) {
    SyntaxNodeKind Kind;
    SourceLoc Loc;
    unsigned Length;
    if (AttrLoc.isValid()) {
      // This token is following @, see if it's a known attribute name.
      bool IsAttr = llvm::StringSwitch<bool>(Tok.getText())
#define ATTR(X) .Case(#X, true)
#define TYPE_ATTR(X) .Case(#X, true)
#define IB_ATTR(X) .Case(#X, true)
#include "swift/AST/Attr.def"
      .Default(false);
      if (IsAttr) {
        // It's a known attribute, so treat it as a syntactic attribute node for
        // syntax coloring. If swift gets user attributes then all identifiers
        // will be treated as syntactic attribute nodes.
        Loc = AttrLoc;
        Length = SM.getByteDistance(Loc, Tok.getLoc()) + Tok.getLength();
        Kind = SyntaxNodeKind::AttributeId;
      }
      AttrLoc = SourceLoc();
    }
    
    if (!Loc.isValid()) {
      Loc = Tok.getLoc();
      Length = Tok.getLength();
    
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
      case tok::at_sign:
        // Set the location of @ and continue. Next token should be the
        // attribute name.
        AttrLoc = Tok.getLoc();
        continue;
      default:
        continue;
      }
    }

    assert(Loc.isValid());
    assert(Nodes.empty() || SM.isBeforeInBuffer(Nodes.back().Range.getStart(),
                                                Loc));
    Nodes.emplace_back(Kind, CharSourceRange(Loc, Length));
  }

  Impl.TokenNodes = std::move(Nodes);
}

SyntaxModelContext::~SyntaxModelContext() {
  delete &Impl;
}

namespace {

class ModelASTWalker : public ASTWalker {
  const LangOptions &LangOpts;
  const SourceManager &SM;
  unsigned BufferID;
  std::vector<SyntaxStructureNode> SubStructureStack;
  SourceLoc LastLoc;

public:
  SyntaxModelWalker &Walker;
  ArrayRef<SyntaxNode> TokenNodes;

  ModelASTWalker(const LangOptions &LangOpts, const SourceManager &SM,
                 unsigned BufferID, SyntaxModelWalker &Walker)
      : LangOpts(LangOpts), SM(SM), BufferID(BufferID), Walker(Walker) { }

  void visitSourceFile(SourceFile &SrcFile, ArrayRef<SyntaxNode> Tokens);

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override;
  Stmt *walkToStmtPost(Stmt *S) override;
  bool walkToDeclPre(Decl *D) override;
  bool walkToDeclPost(Decl *D) override;
  bool walkToTypeReprPre(TypeRepr *T) override;

private:
  bool annotateIfConfigConditionIdentifiers(Expr *Cond);
  bool handleAttrs(const DeclAttributes &Attrs);
  bool handleAttrs(const TypeAttributes &Attrs);
  bool handleAttrLocs(SourceLoc AtLoc, ArrayRef<SourceLoc> Locs);

  bool shouldPassBraceStructureNode(BraceStmt *S);

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

CharSourceRange charSourceRangeFromSourceRange(const SourceManager &SM,
                                               const SourceRange &SR) {
  SourceLoc SRE = Lexer::getLocForEndOfToken(SM, SR.End);
  return CharSourceRange(SM, SR.Start, SRE);
}

CharSourceRange innerCharSourceRangeFromSourceRange(const SourceManager &SM,
                                                    const SourceRange &SR) {
  if (SR.isInvalid())
    return CharSourceRange();

  SourceLoc SRS = Lexer::getLocForEndOfToken(SM, SR.Start);
  return CharSourceRange(SM, SRS, (SR.End != SR.Start) ? SR.End : SRS);
}

} // anonymous namespace

bool SyntaxModelContext::walk(SyntaxModelWalker &Walker) {
  ModelASTWalker ASTWalk(Impl.LangOpts, Impl.SrcMgr,
                         *Impl.SrcFile.getBufferID(), Walker);
  ASTWalk.visitSourceFile(Impl.SrcFile, Impl.TokenNodes);
  return true;
}

void ModelASTWalker::visitSourceFile(SourceFile &SrcFile,
                                     ArrayRef<SyntaxNode> Tokens) {
  TokenNodes = Tokens;
  SrcFile.walk(*this);

  // Pass the rest of the token nodes.
  for (auto &TokNode : TokenNodes)
    passNode(TokNode);
}

std::pair<bool, Stmt *> ModelASTWalker::walkToStmtPre(Stmt *S) {
  if (isa<BraceStmt>(S) && shouldPassBraceStructureNode(cast<BraceStmt>(S))) {
    // Pass BraceStatement structure node.
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::BraceStatement;
    SN.Range = charSourceRangeFromSourceRange(SM, S->getSourceRange());
    SN.BodyRange = innerCharSourceRangeFromSourceRange(SM,
                                                       S->getSourceRange());
    pushStructureNode(SN);
  }
  else if (SwitchStmt *SW = dyn_cast<SwitchStmt>(S)) {
    if (SW->getLBraceLoc().isValid() && SW->getRBraceLoc().isValid()) {
      SourceRange BraceRange(SW->getLBraceLoc(), SW->getRBraceLoc());
      SyntaxStructureNode SN;
      SN.Kind = SyntaxStructureKind::BraceStatement;
      SN.Range = charSourceRangeFromSourceRange(SM, BraceRange);
      SN.BodyRange = innerCharSourceRangeFromSourceRange(SM, BraceRange);
      pushStructureNode(SN);
    }

  } else if (auto ConfigS = dyn_cast<IfConfigStmt>(S)) {
    if (!passNonTokenNode({ SyntaxNodeKind::BuildConfigKeyword,
          CharSourceRange(ConfigS->getIfLoc(), 3/*'#if'*/) }))
      return { false, nullptr };
    if (!annotateIfConfigConditionIdentifiers(ConfigS->getCond()))
      return { false, nullptr };
    if (Stmt *S = ConfigS->getThenStmt())
      if (!S->walk(*this))
        return { false, nullptr };
    if (ConfigS->hasElse()) {
      if (!passNonTokenNode({ SyntaxNodeKind::BuildConfigKeyword,
            CharSourceRange(ConfigS->getElseLoc(), 5/*'#else'*/) }))
        return { false, nullptr };

      if (Stmt *S = ConfigS->getElseStmt())
        if (!S->walk(*this))
          return { false, nullptr };
    }
    if (ConfigS->getEndLoc().isValid())
      if (!passNonTokenNode({ SyntaxNodeKind::BuildConfigKeyword,
            CharSourceRange(ConfigS->getEndLoc(), 6/*'#endif'*/) }))
        return { false, nullptr };
  }

  return { true, S };
}

Stmt *ModelASTWalker::walkToStmtPost(Stmt *S) {
  if (isa<BraceStmt>(S) && shouldPassBraceStructureNode(cast<BraceStmt>(S)))
    popStructureNode();
  else if (SwitchStmt *SW = dyn_cast<SwitchStmt>(S)) {
    if (SW->getLBraceLoc().isValid() && SW->getRBraceLoc().isValid())
      popStructureNode();
  }

  return S;
}

bool ModelASTWalker::walkToDeclPre(Decl *D) {
  if (D->isImplicit())
    return false;

  if (!handleAttrs(D->getAttrs()))
    return false;

  if (AbstractFunctionDecl *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
    FuncDecl *FD = dyn_cast<FuncDecl>(AFD);
    if (FD && FD->isAccessor()) {
      // Pass context sensitive keyword token.
      SourceLoc SL = FD->getFuncLoc();
      // Make sure the func loc is not the start of the function body, in which
      // case the context sensitive keyword was implied.
      if (FD->getBodySourceRange().Start != SL) {
        unsigned TokLen;
        switch (FD->getAccessorKind()) {
          case AccessorKind::NotAccessor: llvm_unreachable("expected accessor");
          case AccessorKind::IsGetter: TokLen = 3; break;
          case AccessorKind::IsSetter: TokLen = 3; break;
          case AccessorKind::IsWillSet: TokLen = 7; break;
          case AccessorKind::IsDidSet: TokLen = 6; break;
        }
        if (!passNonTokenNode({ SyntaxNodeKind::Keyword,
          CharSourceRange(SL, TokLen)}))
          return false;
      }
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
      SN.Range = charSourceRangeFromSourceRange(SM, AFD->getSourceRange());
      SN.BodyRange = innerCharSourceRangeFromSourceRange(SM,
                                                     AFD->getBodySourceRange());
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
    SN.Range = charSourceRangeFromSourceRange(SM, NTD->getSourceRange());
    SN.BodyRange = innerCharSourceRangeFromSourceRange(SM, NTD->getBraces());
    SourceLoc NRStart = NTD->getNameLoc();
    SourceLoc NREnd = NRStart.getAdvancedLoc(NTD->getName().getLength());
    SN.NameRange = CharSourceRange(SM, NRStart, NREnd);

    for (const TypeLoc &TL : NTD->getInherited()) {
      CharSourceRange TR = charSourceRangeFromSourceRange(SM,
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
      SN.Range = charSourceRangeFromSourceRange(SM, SR);
      if (VD->hasAccessorFunctions())
        SN.BodyRange = innerCharSourceRangeFromSourceRange(SM,
                                                           VD->getBracesRange());
      SourceLoc NRStart = VD->getNameLoc();
      SourceLoc NREnd = NRStart.getAdvancedLoc(VD->getName().getLength());
      SN.NameRange = CharSourceRange(SM, NRStart, NREnd);
      SN.TypeRange = charSourceRangeFromSourceRange(SM,
                                        VD->getTypeSourceRangeForDiagnostics());

      SN.Kind = SyntaxStructureKind::InstanceVariable;
      SN.Attrs = VD->getAttrs();
      pushStructureNode(SN);
    }

  } else if (auto ConfigD = dyn_cast<IfConfigDecl>(D)) {
    if (!passNonTokenNode({ SyntaxNodeKind::BuildConfigKeyword,
          CharSourceRange(ConfigD->getIfLoc(), 3/*'#if'*/) }))
      return false;
    if (!annotateIfConfigConditionIdentifiers(ConfigD->getCond()))
      return false;
    for (auto D : ConfigD->getThenMembers())
      if (D->walk(*this))
        return false;
    if (ConfigD->hasElse()) {
      if (!passNonTokenNode({ SyntaxNodeKind::BuildConfigKeyword,
            CharSourceRange(ConfigD->getElseLoc(), 5/*'#else'*/) }))
        return false;

      for (auto D : ConfigD->getElseMembers())
        if (D->walk(*this))
          return false;
    }
    if (ConfigD->getEndLoc().isValid())
      if (!passNonTokenNode({ SyntaxNodeKind::BuildConfigKeyword,
            CharSourceRange(ConfigD->getEndLoc(), 6/*'#endif'*/) }))
        return false;
  }
  return true;
}

bool ModelASTWalker::walkToDeclPost(swift::Decl *D) {
  if (isa<AbstractFunctionDecl>(D)) {
    FuncDecl *FD = dyn_cast<FuncDecl>(D);
    if (!(FD && FD->isAccessor())) {
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
  if (auto AttrT = dyn_cast<AttributedTypeRepr>(T)) {
    if (!handleAttrs(AttrT->getAttrs()))
      return false;

  } else if (auto IdT = dyn_cast<ComponentIdentTypeRepr>(T)) {
    if (!passNonTokenNode({ SyntaxNodeKind::TypeId,
                            CharSourceRange(IdT->getIdLoc(),
                                            IdT->getIdentifier().getLength())
                          }))
      return false;

  } else if (auto InOutT = dyn_cast<InOutTypeRepr>(T)) {
    if (!passNonTokenNode({ SyntaxNodeKind::Keyword,
                            CharSourceRange(InOutT->getInOutLoc(), /*'inout'*/5)
                          }))
      return false;
  }
  return true;
}

namespace {
template <typename FnTy>
class IdRefWalker : public ASTWalker {
  const FnTy &Fn;

public:
  IdRefWalker(const FnTy &Fn) : Fn(Fn) {}

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    if (auto DRE = dyn_cast<UnresolvedDeclRefExpr>(E)) {
      if (!DRE->hasName())
        return { true, E };
      if (DRE->getRefKind() != DeclRefKind::Ordinary)
        return { true, E };
      if (!Fn(CharSourceRange(DRE->getSourceRange().Start,
                              DRE->getName().getLength())))
        return { false, nullptr };
    }
    return { true, E };
  }
};
}

bool ModelASTWalker::annotateIfConfigConditionIdentifiers(Expr *Cond) {
  if (!Cond)
    return true;
  auto passNode = [&](CharSourceRange R) {
    return passNonTokenNode({ SyntaxNodeKind::BuildConfigId, R });
  };

  IdRefWalker<decltype(passNode)> Walker(passNode);
  if (!Cond->walk(Walker))
    return false;
  return true;
}

bool ModelASTWalker::handleAttrs(const DeclAttributes &Attrs) {
  SmallVector<SourceLoc, 4> Locs;
  Attrs.getAttrLocs(Locs);
  if (Locs.empty())
    return true;

  return handleAttrLocs(Attrs.AtLoc, Locs);
}

bool ModelASTWalker::handleAttrs(const TypeAttributes &Attrs) {
  SmallVector<SourceLoc, 4> Locs;
  Attrs.getAttrLocs(Locs);
  if (Locs.empty())
    return true;

  return handleAttrLocs(Attrs.AtLoc, Locs);
}
bool ModelASTWalker::handleAttrLocs(SourceLoc BeginLoc,
                                    ArrayRef<SourceLoc> Locs) {
  if (Locs.empty())
    return true;

  SmallVector<SourceLoc, 6> SortedLocs(Locs.begin(), Locs.end());
  std::sort(SortedLocs.begin(), SortedLocs.end(),
      [&](SourceLoc LHS, SourceLoc RHS) {
        return SM.isBeforeInBuffer(LHS, RHS);
      }
  );
  Locs = SortedLocs;

  // The BeginLoc is invalid if there's no leading '@', which happens if there
  // are only virtual attributes. In that case we'll use the location of the
  // first attribute.
  if (BeginLoc.isInvalid())
    BeginLoc = Locs.front();

  std::vector<Token> Toks =
      swift::tokenize(LangOpts, SM, BufferID,
                      SM.getLocOffsetInBuffer(BeginLoc, BufferID),
                      SM.getLocOffsetInBuffer(SortedLocs.back(), BufferID),
                      /*KeepComments=*/true,
                      /*TokenizeInterpolatedString=*/false);

  auto passAttrNode = [&](SourceLoc AtLoc, SourceLoc AttrLoc) -> bool {
    SourceRange Range;
    if (AtLoc.isValid())
      Range = SourceRange(AtLoc, AttrLoc);
    else
      Range = AttrLoc;
    if (!passNonTokenNode({ SyntaxNodeKind::AttributeBuiltin,
                            charSourceRangeFromSourceRange(SM, Range) }))
      return false;

    if (TokenNodes.front().Range.getStart() == AttrLoc)
      TokenNodes = TokenNodes.slice(1);
    return true;
  };

  SourceLoc AtLoc;
  for (auto Tok : Toks) {
    if (Tok.getLoc() == Locs.front()) {
      Locs = Locs.slice(1);
      if (!passAttrNode(AtLoc, Tok.getLoc()))
        return false;
    }

    if (Tok.is(tok::at_sign))
      AtLoc = Tok.getLoc();
    else if (Tok.isNot(tok::exclaim_postfix))
      AtLoc = SourceLoc();
  }

  if (!Locs.empty()) {
    if (!passAttrNode(AtLoc, Locs.front()))
      return false;
  }

  return true;
}

bool ModelASTWalker::shouldPassBraceStructureNode(BraceStmt *S) {
  return (!dyn_cast_or_null<AbstractFunctionDecl>(Parent.getAsDecl()) &&
          !dyn_cast_or_null<TopLevelCodeDecl>(Parent.getAsDecl()) &&
          !dyn_cast_or_null<CaseStmt>(Parent.getAsStmt()) &&
          S->getSourceRange().isValid());
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
