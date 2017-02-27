//===--- LegacyASTTransformer.cpp - Swift lib/AST -> lib/Syntax -----------===//
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

#include "swift/AST/AST.h"
#include "swift/Syntax/DeclSyntax.h"
#include "swift/Syntax/ExprSyntax.h"
#include "swift/Syntax/GenericSyntax.h"
#include "swift/Syntax/LegacyASTTransformer.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/StmtSyntax.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/TokenSyntax.h"

using namespace swift;
using namespace swift::syntax;

namespace {
  bool
  tokenContainsOffset(unsigned Offset,
                      const std::pair<RC<TokenSyntax>,
                      AbsolutePosition> &TokAndPos) {
    auto Start = TokAndPos.second.getOffset();
    auto End = Start + TokAndPos.first->getText().size();
    return Offset >= Start && Offset < End;
  }
  std::vector<RC<TokenSyntax>>
  getTokenSyntaxsInRange(SourceRange Range, SourceManager &SourceMgr,
                       unsigned BufferID,
                       const TokenPositionList &Tokens) {
    auto StartOffset = SourceMgr.getLocOffsetInBuffer(Range.Start, BufferID);
    auto EndOffset = SourceMgr.getLocOffsetInBuffer(Range.End, BufferID);

    auto Start = Tokens.begin();
    auto End = Tokens.rbegin();

    while (Start != Tokens.end()) {
      if (tokenContainsOffset(StartOffset, *Start)) {
        break;
      }
      ++Start;
    }

    while (End != Tokens.rend()) {
      if (tokenContainsOffset(EndOffset, *End)) {
        break;
      }
      ++End;
    }

    assert(Start != Tokens.end());
    assert(End.base() != Tokens.end());
    assert(Start <= End.base());

    std::vector<RC<TokenSyntax>> TokensInRange;

    while(Start < End.base()) {
      TokensInRange.push_back(Start->first);
      ++Start;
    }

    return TokensInRange;
  }
}

Optional<Syntax>
syntax::transformAST(ASTNode Node,
                     sema::Semantics &Sema,
                     SourceManager &SourceMgr,
                     const unsigned BufferID,
                     const TokenPositionList &Tokens) {
  LegacyASTTransformer Transformer { Sema, SourceMgr, BufferID, Tokens };

  if (Node.is<Expr *>()) {
    auto E = Node.get<Expr *>();
    if (E->isImplicit() || E->getSourceRange().isInvalid()) {
      return None;
    }
    auto Transformed = Transformer.visit(E);
    Sema.recordSyntaxMapping(Transformed, Node);
    return Syntax { Transformed, Transformed.get() };
  } else if (Node.is<Decl *>()) {
    auto D = Node.get<Decl *>();
    if (auto VD = dyn_cast<VarDecl>(D)) {
      if (VD->getParentPattern()) {
        return None;
      }
    } else if (auto FD = dyn_cast<FuncDecl>(D)) {
      if (FD->isGetterOrSetter()) {
        return None;
      }
    }
    if (D->isImplicit() || D->getSourceRange().isInvalid()) {
      return None;
    }
    auto Transformed = Transformer.visit(D);
    Sema.recordSyntaxMapping(Transformed, Node);
    return Syntax { Transformed, Transformed.get() };
  } else if (Node.is<Stmt *>()) {
    auto S = Node.get<Stmt *>();
    if (S->isImplicit() || S->getSourceRange().isInvalid()) {
      return None;
    }
    auto Transformed = Transformer.visit(S);
    Sema.recordSyntaxMapping(Transformed, Node);
    return Syntax { Transformed, Transformed.get() };
  }
  return None;
}

SourceLoc LegacyASTTransformer::getStartLocForDecl(const Decl *D) const {
  return D->getAttrs().isEmpty()
    ? D->getStartLoc()
    : D->getAttrs().getStartLoc();
}

SourceLoc LegacyASTTransformer::getEndLocForDecl(const Decl *D) const {
  return D->TrailingSemiLoc.isValid()
    ? D->TrailingSemiLoc
    : D->getEndLoc();
}

SourceLoc LegacyASTTransformer::getEndLocForStmt(const Stmt *S) const {
  return S->TrailingSemiLoc.isValid()
  ? S->TrailingSemiLoc
  : S->getEndLoc();
}

SourceLoc LegacyASTTransformer::getEndLocForExpr(const Expr *E) const {
  return E->TrailingSemiLoc.isValid()
  ? E->TrailingSemiLoc
  : E->getEndLoc();
}

RC<SyntaxData> LegacyASTTransformer::getUnknownSyntax(SourceRange SR) {
  auto ComprisingTokens = getTokenSyntaxsInRange(SR, SourceMgr,
                                                 BufferID, Tokens);
  RawSyntax::LayoutList Layout;
  std::copy(ComprisingTokens.begin(),
            ComprisingTokens.end(),
            std::back_inserter(Layout));
  auto Raw = RawSyntax::make(SyntaxKind::Unknown,
                             Layout,
                             SourcePresence::Present);
  return UnknownSyntaxData::make(Raw);
}

RC<SyntaxData> LegacyASTTransformer::getUnknownDecl(Decl *D) {
  SourceRange SR {getStartLocForDecl(D),getEndLocForDecl(D)};
  auto ComprisingTokens = getTokenSyntaxsInRange(SR, SourceMgr,
                                                 BufferID, Tokens);
  RawSyntax::LayoutList Layout;
  std::copy(ComprisingTokens.begin(),
            ComprisingTokens.end(),
            std::back_inserter(Layout));
  auto Raw = RawSyntax::make(SyntaxKind::UnknownExpr,
                             Layout,
                             SourcePresence::Present);
  return UnknownDeclSyntaxData::make(Raw);
}

RC<SyntaxData> LegacyASTTransformer::getUnknownStmt(Stmt *S) {
  SourceRange SR { S->getStartLoc(), getEndLocForStmt(S) };
  auto ComprisingTokens = getTokenSyntaxsInRange(SR, SourceMgr,
                                                 BufferID, Tokens);
  RawSyntax::LayoutList Layout;
  std::copy(ComprisingTokens.begin(),
            ComprisingTokens.end(),
            std::back_inserter(Layout));
  auto Raw = RawSyntax::make(SyntaxKind::UnknownExpr,
                             Layout,
                             SourcePresence::Present);
  return UnknownStmtSyntaxData::make(Raw);
}

RC<SyntaxData> LegacyASTTransformer::getUnknownExpr(Expr *E) {
  SourceRange SR { E->getStartLoc(), getEndLocForExpr(E) };
  auto ComprisingTokens = getTokenSyntaxsInRange(SR, SourceMgr,
                                                 BufferID, Tokens);
  RawSyntax::LayoutList Layout;
  std::copy(ComprisingTokens.begin(),
            ComprisingTokens.end(),
            std::back_inserter(Layout));
  auto Raw = RawSyntax::make(SyntaxKind::UnknownExpr,
                             Layout,
                             SourcePresence::Present);
  return UnknownExprSyntaxData::make(Raw);
}

#pragma mark - Declarations

RC<SyntaxData>
LegacyASTTransformer::visitImportDecl(ImportDecl *D,
                                      const SyntaxData *Parent,
                                      const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitExtensionDecl(ExtensionDecl *D,
                                         const SyntaxData *Parent,
                                         const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitPatternBindingDecl(PatternBindingDecl *PBD,
                                              const SyntaxData *Parent,
                                              const CursorIndex IndexInParent) {
  auto StartLoc = getStartLocForDecl(PBD);

  for (auto Entry : PBD->getPatternList()) {
    auto Loc = getStartLocForDecl(Entry.getAnchoringVarDecl());
    if (StartLoc.isInvalid() || SourceMgr.isBeforeInBuffer(Loc, StartLoc)) {
      StartLoc = Loc;
    }
  }
  auto EndLoc = getEndLocForDecl(PBD);
  return getUnknownSyntax({StartLoc, EndLoc});
}

RC<SyntaxData>
LegacyASTTransformer::visitEnumCaseDecl(EnumCaseDecl *D,
                                        const SyntaxData *Parent,
                                        const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitTopLevelCodeDecl(TopLevelCodeDecl *D,
                                            const SyntaxData *Parent,
                                            const CursorIndex IndexInParent) {
  return visitBraceStmt(D->getBody(), Parent, IndexInParent);
}

RC<SyntaxData>
LegacyASTTransformer::visitIfConfigDecl(IfConfigDecl *D,
                                        const SyntaxData *Parent,
                                        const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitPrecedenceGroupDecl(
    PrecedenceGroupDecl *D,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitInfixOperatorDecl(InfixOperatorDecl *D,
                                             const SyntaxData *Parent,
                                             const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitPrefixOperatorDecl(PrefixOperatorDecl *D,
                                              const SyntaxData *Parent,
                                              const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitPostfixOperatorDecl(
    PostfixOperatorDecl *D,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitGenericTypeParamDecl(
    GenericTypeParamDecl *D,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}
RC<SyntaxData>
LegacyASTTransformer::visitAssociatedTypeDecl(AssociatedTypeDecl *D,
                                              const SyntaxData *Parent,
                                              const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitTypeAliasDecl(TypeAliasDecl *D,
                                         const SyntaxData *Parent,
                                         const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitEnumDecl(EnumDecl *D,
                                    const SyntaxData *Parent,
                                    const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitStructDecl(StructDecl *D,
                                      const SyntaxData *Parent,
                                      const CursorIndex IndexInParent) {
  return getUnknownDecl(D);

// TODO
#if 0
  StructDeclSyntaxBuilder StructBuilder;
  if (D->getStartLoc().isValid()) {
    auto StructKeyword = findTokenSyntax(tok::kw_struct, "struct", SourceMgr,
                                       D->getStartLoc(), BufferID, Tokens);
    StructBuilder.useStructKeyword(StructKeyword);
  }
  
  if (D->getNameLoc().isValid()) {
    auto Identifier = findTokenSyntax(tok::identifier,
                                    OwnedString(D->getName().str()),
                                    SourceMgr, D->getNameLoc(), BufferID,
                                    Tokens);
    StructBuilder.useIdentifier(Identifier);
  }

  if (D->getBraces().isValid()) {
    auto LeftBraceToken = findTokenSyntax(tok::l_brace, "{", SourceMgr,
                                        D->getBraces().Start, BufferID, Tokens);
    StructBuilder.useLeftBrace(LeftBraceToken);
    auto RightBraceToken = findTokenSyntax(tok::r_brace, "}", SourceMgr,
                                         D->getBraces().End, BufferID, Tokens);
    StructBuilder.useRightBrace(RightBraceToken);
  }

  DeclMembersSyntaxBuilder MemberBuilder;
  for (auto Member : D->getMembers()) {
    auto TransformedMember = transformAST(Member, Sema,
                                          SourceMgr, BufferID, Tokens);
    if (TransformedMember.hasValue()) {
      if (auto MD = TransformedMember.getValue().getAs<DeclSyntax>()) {
        MemberBuilder.addMember(MD.getValue());
      } else {
        continue;
        return getUnknownDecl(D);
      }
    } else {
      continue;
      return getUnknownDecl(D);
    }
  }

  StructBuilder.useMembers(MemberBuilder.build());

  return StructBuilder.build().Root;
#endif
}

RC<SyntaxData>
LegacyASTTransformer::visitClassDecl(ClassDecl *D,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitProtocolDecl(ProtocolDecl *D,
                                        const SyntaxData *Parent,
                                        const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitModuleDecl(ModuleDecl *D,
                                      const SyntaxData *Parent,
                                      const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitVarDecl(VarDecl *VD,
                                   const SyntaxData *Parent,
                                   const CursorIndex IndexInParent) {
  return getUnknownDecl(VD);
}

RC<SyntaxData>
LegacyASTTransformer::visitParamDecl(ParamDecl *D,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitSubscriptDecl(SubscriptDecl *D,
                                         const SyntaxData *Parent,
                                         const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitConstructorDecl(ConstructorDecl *D,
                                           const SyntaxData *Parent,
                                           const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitDestructorDecl(DestructorDecl *D,
                                          const SyntaxData *Parent,
                                          const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

RC<SyntaxData>
LegacyASTTransformer::visitFuncDecl(FuncDecl *FD,
                                    const SyntaxData *Parent,
                                    const CursorIndex IndexInParent) {
  return getUnknownDecl(FD);
}

RC<SyntaxData>
LegacyASTTransformer::visitEnumElementDecl(EnumElementDecl *D,
                                           const SyntaxData *Parent,
                                           const CursorIndex IndexInParent) {
  return getUnknownDecl(D);
}

#pragma mark - Statements

RC<SyntaxData>
LegacyASTTransformer::visitBraceStmt(BraceStmt *S,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  auto LeftBrace = S->getLBraceLoc().isValid()
    ? findTokenSyntax(tok::l_brace, "{", SourceMgr, S->getLBraceLoc(),
                    BufferID, Tokens)
    : TokenSyntax::missingToken(tok::l_brace, "{");

  StmtListSyntaxBuilder StmtsBuilder;
  for (auto Node : S->getElements()) {
    auto Transformed = transformAST(Node, Sema, SourceMgr, BufferID, Tokens);
    if (Transformed.hasValue()) {
      StmtsBuilder.addStatement(Transformed.getValue());
    }
  }

  auto RightBrace = S->getLBraceLoc().isValid()
    ? findTokenSyntax(tok::r_brace, "}", SourceMgr, S->getLBraceLoc(),
                    BufferID, Tokens)
    : TokenSyntax::missingToken(tok::r_brace, "}");

  return SyntaxFactory::makeCodeBlock(LeftBrace, StmtsBuilder.build(),
                                      RightBrace).Root;
}

RC<SyntaxData>
LegacyASTTransformer::visitReturnStmt(ReturnStmt *S,
                                      const SyntaxData *Parent,
                                      const CursorIndex IndexInParent) {
  auto ReturnKW = findTokenSyntax(tok::kw_return, "return", SourceMgr,
                                  S->getReturnLoc(), BufferID, Tokens);
  auto Result = transformAST(S->getResult(), Sema, SourceMgr, BufferID,
                             Tokens);

  if (!Result.hasValue()) {
    return getUnknownStmt(S);
  }

  return SyntaxFactory::makeReturnStmt(ReturnKW,
    Result.getValue().castTo<ExprSyntax>()).Root;
}

RC<SyntaxData>
LegacyASTTransformer::visitDeferStmt(DeferStmt *S,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitIfStmt(IfStmt *S,
                                  const SyntaxData *Parent,
                                  const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitGuardStmt(GuardStmt *S,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitWhileStmt(WhileStmt *S,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitDoStmt(DoStmt *S,
                                  const SyntaxData *Parent,
                                  const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitDoCatchStmt(DoCatchStmt *S,
                                       const SyntaxData *Parent,
                                       const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitRepeatWhileStmt(RepeatWhileStmt *S,
                                           const SyntaxData *Parent,
                                           const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitForStmt(ForStmt *S,
                                   const SyntaxData *Parent,
                                   const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitForEachStmt(ForEachStmt *S,
                                       const SyntaxData *Parent,
                                       const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitSwitchStmt(SwitchStmt *S,
                                      const SyntaxData *Parent,
                                      const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitCaseStmt(CaseStmt *S,
                                    const SyntaxData *Parent,
                                    const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitCatchStmt(CatchStmt *S,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitBreakStmt(BreakStmt *S,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitContinueStmt(ContinueStmt *S,
                                        const SyntaxData *Parent,
                                        const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitFallthroughStmt(FallthroughStmt *S,
                                           const SyntaxData *Parent,
                                           const CursorIndex IndexInParent) {
  if (S->getLoc().isInvalid()) {
    return SyntaxFactory::makeBlankFallthroughStmt().Root;
  }

  auto FallthroughToken = findTokenSyntax(tok::kw_fallthrough, "fallthrough",
                                          SourceMgr, S->getLoc(),
                                          BufferID, Tokens);
  return SyntaxFactory::makeFallthroughStmt(FallthroughToken).Root;
}

RC<SyntaxData>
LegacyASTTransformer::visitIfConfigStmt(IfConfigStmt *S,
                                        const SyntaxData *Parent,
                                        const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitFailStmt(FailStmt *S,
                                    const SyntaxData *Parent,
                                    const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

RC<SyntaxData>
LegacyASTTransformer::visitThrowStmt(ThrowStmt *S,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownStmt(S);
}

#pragma mark - Expressions

RC<SyntaxData>
LegacyASTTransformer::visitErrorExpr(ErrorExpr *E,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitNilLiteralExpr(NilLiteralExpr *E,
                                          const SyntaxData *Parent,
                                          const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitIntegerLiteralExpr(IntegerLiteralExpr *E,
                                              const SyntaxData *Parent,
                                              const CursorIndex IndexInParent) {
  auto Sign = E->getMinusLoc().isValid()
    ? findTokenSyntax(tok::oper_prefix, OwnedString(),
                      SourceMgr, E->getMinusLoc(),
                      BufferID, Tokens)
    : TokenSyntax::missingToken(tok::oper_prefix, "");
  auto Digits = findTokenSyntax(tok::integer_literal, OwnedString(),
                                SourceMgr, E->getDigitsLoc(),
                                BufferID, Tokens);
  return SyntaxFactory::makeIntegerLiteralExpr(Sign, Digits).Root;
}

RC<SyntaxData>
LegacyASTTransformer::visitFloatLiteralExpr(FloatLiteralExpr *E,
                                            const SyntaxData *Parent,
                                            const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitBooleanLiteralExpr(BooleanLiteralExpr *E,
                                              const SyntaxData *Parent,
                                              const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitStringLiteralExpr(StringLiteralExpr *E,
                                             const SyntaxData *Parent,
                                             const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitInterpolatedStringLiteralExpr(
    InterpolatedStringLiteralExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitObjectLiteralExpr(ObjectLiteralExpr *E,
                                             const SyntaxData *Parent,
                                             const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitMagicIdentifierLiteralExpr(
    MagicIdentifierLiteralExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitDiscardAssignmentExpr(
    DiscardAssignmentExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitDeclRefExpr(DeclRefExpr *E,
                                       const SyntaxData *Parent,
                                       const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitSuperRefExpr(SuperRefExpr *E,
                                        const SyntaxData *Parent,
                                        const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitTypeExpr(TypeExpr *E,
                                    const SyntaxData *Parent,
                                    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitOtherConstructorDeclRefExpr(
    OtherConstructorDeclRefExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData> LegacyASTTransformer::visitDotSyntaxBaseIgnoredExpr(
    DotSyntaxBaseIgnoredExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitOverloadedDeclRefExpr(
    OverloadedDeclRefExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitUnresolvedDeclRefExpr(
    UnresolvedDeclRefExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitMemberRefExpr(MemberRefExpr *E,
                                         const SyntaxData *Parent,
                                         const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitDynamicMemberRefExpr(
    DynamicMemberRefExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitDynamicSubscriptExpr(
    DynamicSubscriptExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData> LegacyASTTransformer::visitUnresolvedSpecializeExpr(
    UnresolvedSpecializeExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitUnresolvedMemberExpr(
    UnresolvedMemberExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitUnresolvedDotExpr(UnresolvedDotExpr *E,
                                             const SyntaxData *Parent,
                                             const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitSequenceExpr(SequenceExpr *E,
                                        const SyntaxData *Parent,
                                        const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitParenExpr(ParenExpr *E,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitDotSelfExpr(DotSelfExpr *E,
                                       const SyntaxData *Parent,
                                       const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitTryExpr(TryExpr *E,
                                   const SyntaxData *Parent,
                                   const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitForceTryExpr(ForceTryExpr *E,
                                        const SyntaxData *Parent,
                                        const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitOptionalTryExpr(OptionalTryExpr *E,
                                           const SyntaxData *Parent,
                                           const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitTupleExpr(TupleExpr *E,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitArrayExpr(ArrayExpr *E,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitDictionaryExpr(DictionaryExpr *E,
                                          const SyntaxData *Parent,
                                          const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitSubscriptExpr(SubscriptExpr *E,
                                         const SyntaxData *Parent,
                                         const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitTupleElementExpr(TupleElementExpr *E,
                                            const SyntaxData *Parent,
                                            const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitCaptureListExpr(CaptureListExpr *E,
                                           const SyntaxData *Parent,
                                           const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitClosureExpr(ClosureExpr *E,
                                       const SyntaxData *Parent,
                                       const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitAutoClosureExpr(AutoClosureExpr *E,
                                           const SyntaxData *Parent,
                                           const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitInOutExpr(InOutExpr *E,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitDynamicTypeExpr(DynamicTypeExpr *E,
                                           const SyntaxData *Parent,
                                           const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitRebindSelfInConstructorExpr(
    RebindSelfInConstructorExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitOpaqueValueExpr(OpaqueValueExpr *E,
                                           const SyntaxData *Parent,
                                           const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitBindOptionalExpr(BindOptionalExpr *E,
                                            const SyntaxData *Parent,
                                            const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitOptionalEvaluationExpr(
    OptionalEvaluationExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitForceValueExpr(ForceValueExpr *E,
                                          const SyntaxData *Parent,
                                          const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitOpenExistentialExpr(
    OpenExistentialExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData> LegacyASTTransformer::visitMakeTemporarilyEscapableExpr(
    MakeTemporarilyEscapableExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitCallExpr(CallExpr *E,
                                    const SyntaxData *Parent,
                                    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitPrefixUnaryExpr(PrefixUnaryExpr *E,
                                           const SyntaxData *Parent,
                                           const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitPostfixUnaryExpr(PostfixUnaryExpr *E,
                                            const SyntaxData *Parent,
                                            const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitBinaryExpr(BinaryExpr *E,
                                      const SyntaxData *Parent,
                                      const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitDotSyntaxCallExpr(DotSyntaxCallExpr *E,
                                             const SyntaxData *Parent,
                                             const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitConstructorRefCallExpr(
    ConstructorRefCallExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitLoadExpr(LoadExpr *E,
                                    const SyntaxData *Parent,
                                    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitTupleShuffleExpr(TupleShuffleExpr *E,
                                            const SyntaxData *Parent,
                                            const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitUnresolvedTypeConversionExpr(
    UnresolvedTypeConversionExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitFunctionConversionExpr(
    FunctionConversionExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData> LegacyASTTransformer::visitCovariantFunctionConversionExpr(
    CovariantFunctionConversionExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData> LegacyASTTransformer::visitCovariantReturnConversionExpr(
    CovariantReturnConversionExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitMetatypeConversionExpr(
    MetatypeConversionExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitCollectionUpcastConversionExpr(
    CollectionUpcastConversionExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitErasureExpr(ErasureExpr *E,
                                       const SyntaxData *Parent,
                                       const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitAnyHashableErasureExpr(
    AnyHashableErasureExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitDerivedToBaseExpr(DerivedToBaseExpr *E,
                                             const SyntaxData *Parent,
                                             const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitArchetypeToSuperExpr(
    ArchetypeToSuperExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitInjectIntoOptionalExpr(
    InjectIntoOptionalExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitClassMetatypeToObjectExpr(
    ClassMetatypeToObjectExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData> LegacyASTTransformer::visitExistentialMetatypeToObjectExpr(
    ExistentialMetatypeToObjectExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData> LegacyASTTransformer::visitProtocolMetatypeToObjectExpr(
    ProtocolMetatypeToObjectExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitInOutToPointerExpr(InOutToPointerExpr *E,
                                              const SyntaxData *Parent,
                                              const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitArrayToPointerExpr(ArrayToPointerExpr *E,
                                              const SyntaxData *Parent,
                                              const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitStringToPointerExpr(
    StringToPointerExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitPointerToPointerExpr(
    PointerToPointerExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitLValueToPointerExpr(
    LValueToPointerExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData> LegacyASTTransformer::visitForeignObjectConversionExpr(
    ForeignObjectConversionExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitUnevaluatedInstanceExpr(
    UnevaluatedInstanceExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitForcedCheckedCastExpr(
    ForcedCheckedCastExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData> LegacyASTTransformer::visitConditionalCheckedCastExpr(
    ConditionalCheckedCastExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitIsExpr(IsExpr *E,
                                  const SyntaxData *Parent,
                                  const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitCoerceExpr(CoerceExpr *E,
                                      const SyntaxData *Parent,
                                      const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitArrowExpr(ArrowExpr *E,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitIfExpr(IfExpr *E,
                                  const SyntaxData *Parent,
                                  const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitEnumIsCaseExpr(EnumIsCaseExpr *E,
                                          const SyntaxData *Parent,
                                          const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitAssignExpr(AssignExpr *E,
                                      const SyntaxData *Parent,
                                      const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitCodeCompletionExpr(CodeCompletionExpr *E,
                                              const SyntaxData *Parent,
                                              const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitUnresolvedPatternExpr(
    UnresolvedPatternExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitEditorPlaceholderExpr(
    EditorPlaceholderExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitObjCSelectorExpr(ObjCSelectorExpr *E,
                                            const SyntaxData *Parent,
                                            const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitObjCKeyPathExpr(ObjCKeyPathExpr *E,
                                           const SyntaxData *Parent,
                                           const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<TokenSyntax>
syntax::findTokenSyntax(tok ExpectedKind,
                        OwnedString ExpectedText,
                        SourceManager &SourceMgr,
                        SourceLoc Loc,
                        unsigned BufferID,
                        const TokenPositionList &Tokens) {
  auto Offset = SourceMgr.getLocOffsetInBuffer(Loc, BufferID);

  size_t Start = 0;
  size_t End = Tokens.size() - 1;

  while (Start <= End) {
    auto Mid = (Start + End) / 2;
    auto TokAndPos = Tokens[Mid];
    auto Tok = TokAndPos.first;
    auto Pos = TokAndPos.second;

    auto TokStart = Pos.getOffset();
    auto TokEnd = TokStart + Tok->getText().size();

    if (Offset == TokStart) {
      if (Tok->getTokenKind() == ExpectedKind &&
          (ExpectedText.empty() || Tok->getText() == ExpectedText.str())) {
        return Tok;
      } else {
        return TokenSyntax::missingToken(ExpectedKind, ExpectedText);
      }
    }

    if (TokStart < Offset) {
      Start = Mid + 1;
    } else {
      End = Mid - 1;
    }
  }

  return TokenSyntax::missingToken(ExpectedKind, ExpectedText);
}
