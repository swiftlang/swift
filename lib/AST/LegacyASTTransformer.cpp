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

#include "swift/AST/LegacyASTTransformer.h"
#include "swift/AST/ParameterList.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/SyntaxBuilders.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/UnknownSyntax.h"

using namespace swift;
using namespace swift::syntax;

namespace {
  bool
  tokenContainsOffset(unsigned Offset,
                      const std::pair<RC<RawTokenSyntax>,
                                      AbsolutePosition> &TokAndPos) {
    auto Start = TokAndPos.second.getOffset();
    auto End = Start + TokAndPos.first->getText().size();
    return Offset >= Start && Offset < End;
  }

  std::vector<RC<RawTokenSyntax>>
  getRawTokenSyntaxesInRange(SourceRange Range, SourceManager &SourceMgr,
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

    std::vector<RC<RawTokenSyntax>> TokensInRange;

    while (Start < End.base()) {
      TokensInRange.push_back(Start->first);
      ++Start;
    }

    return TokensInRange;
  }
} // anonymous namespace

Optional<Syntax>
syntax::transformAST(ASTNode Node,
                     SyntaxASTMap &ASTMap,
                     SourceManager &SourceMgr,
                     const unsigned BufferID,
                     const TokenPositionList &Tokens) {
  LegacyASTTransformer transformer = { ASTMap, SourceMgr, BufferID, Tokens };
  return transformer.transform(Node);
};

Optional<Syntax> LegacyASTTransformer::transform(ASTNode Node) {
  if (Node.is<Expr *>()) {
    if (auto node = transform(Node.get<Expr *>())) {
      return node.getValue();
    }
  } else if (Node.is<Decl *>()) {
    if (auto node = transform(Node.get<Decl *>())) {
      return node.getValue();
    }
  } else if (Node.is<Stmt *>()) {
    if (auto node = transform(Node.get<Stmt *>())) {
      return node.getValue();
    }
  }
  return None;
}

StmtSyntax LegacyASTTransformer::getStmtSyntax(Syntax Node) {
  if (Node.isDecl())
    return SyntaxFactory::makeDeclarationStmt(Node.castTo<DeclSyntax>(),
                                              None);
  if (Node.isExpr())
    return SyntaxFactory::makeExpressionStmt(Node.castTo<ExprSyntax>(),
                                             None);
  return Node.castTo<StmtSyntax>();
}

Optional<TypeSyntax> LegacyASTTransformer::transform(TypeRepr *T) {
  if (T->getSourceRange().isInvalid()) {
    return None;
  }
  auto Transformed = visit(T);
  return TypeSyntax { Transformed, Transformed.get() };
}

Optional<ExprSyntax> LegacyASTTransformer::transform(Expr *E) {
  if (E->isImplicit() || E->getSourceRange().isInvalid()) {
    return None;
  }
  auto Transformed = visit(E);
  ASTMap.recordSyntaxMapping(Transformed, E);
  return ExprSyntax { Transformed, Transformed.get() };
}

Optional<DeclSyntax> LegacyASTTransformer::transform(Decl *D) {
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
  auto Transformed = visit(D);
  ASTMap.recordSyntaxMapping(Transformed, D);
  return DeclSyntax { Transformed, Transformed.get() };
}

Optional<StmtSyntax> LegacyASTTransformer::transform(Stmt *S) {
  if (S->isImplicit() || S->getSourceRange().isInvalid()) {
    return None;
  }
  auto Transformed = visit(S);
  ASTMap.recordSyntaxMapping(Transformed, S);
  return StmtSyntax { Transformed, Transformed.get() };
}

AttributeSyntax LegacyASTTransformer::getAttribute(DeclAttribute *attr) {
  AttributeSyntaxBuilder b;
  b.useAtSignToken(findToken(attr->AtLoc, tok::at_sign, "@"));

  auto name = findToken(attr->getLocation(), tok::identifier);
  b.useIdentifier(name);

  auto tokens = getRawTokenSyntaxesInRange(attr->getRange(), SourceMgr,
                                           BufferID, Tokens);

  // Search through the tokens for the leading left paren and right paren.
  // Any other tokens are added to the attribute directly.
  bool hasLeftParen = false;
  for (size_t i = 0; i < tokens.size(); ++i) {
    auto token = tokens[i];
    if (token == name.getRawToken()) continue;
    auto tokenSyntax = make<TokenSyntax>(token);

    // If the 2nd token in the list is a left paren, it's considered
    // "the left paren"
    if (i == 1 && token->getTokenKind() == tok::l_paren) {
      hasLeftParen = true;
      b.useLeftParen(tokenSyntax);
      continue;
    }

    // If the last token in the list is a right paren, it's considered
    // "the right paren"
    if (i == tokens.size() - 1 && hasLeftParen &&
        token->getTokenKind() == tok::r_paren) {
      b.useRightParen(tokenSyntax);
      continue;
    }

    // Otherwise, it's just a token
    b.addToken(tokenSyntax);
  }

  return b.build();
}

DeclModifierSyntax LegacyASTTransformer::getModifier(DeclAttribute *attr) {
  DeclModifierSyntaxBuilder b;

  auto tokens = getRawTokenSyntaxesInRange(attr->getRange(), SourceMgr,
                                           BufferID, Tokens);

  // FIXME: This relies on modifiers either being one of two forms:
  //        - public, private, internal, weak, lazy (1 token)
  //        - private(set), unsafe(unowned) (4 tokens)

  if (tokens.size() >= 1) {
    // public, private, weak, lazy, etc.
    b.useName(make<TokenSyntax>(tokens[0]));
  }
  if (tokens.size() == 4) {
    // private(set), unsafe(unowned), etc.
    b.useLeftParen(make<TokenSyntax>(tokens[1]));
    b.useArgument(make<TokenSyntax>(tokens[2]));
    b.useRightParen(make<TokenSyntax>(tokens[3]));
  }

  return b.build();
}

std::pair<AttributeListSyntax, ModifierListSyntax>
LegacyASTTransformer::getAttributesFromDecl(Decl *D) {
  std::vector<AttributeSyntax> attrs;
  std::vector<Syntax> modifiers;
  for (auto *attr : D->getAttrs()) {
    if (attr->AtLoc.isValid()) {
      attrs.push_back(getAttribute(attr));
    } else {
      modifiers.push_back(getModifier(attr));
    }
  }

  return {
    SyntaxFactory::makeAttributeList(attrs),
    SyntaxFactory::makeModifierList(modifiers)
  };
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

RC<SyntaxData>
LegacyASTTransformer::getUnknownSyntax(SourceRange SR, SyntaxKind Kind) {
  auto ComprisingTokens = getRawTokenSyntaxesInRange(SR, SourceMgr,
                                                 BufferID, Tokens);
  RawSyntax::LayoutList Layout;
  std::copy(ComprisingTokens.begin(),
            ComprisingTokens.end(),
            std::back_inserter(Layout));
  auto Raw = RawSyntax::make(Kind, Layout, SourcePresence::Present);
  return SyntaxData::make(Raw);
}

RC<SyntaxData> LegacyASTTransformer::getUnknownType(TypeRepr *T) {
  auto ComprisingTokens = getRawTokenSyntaxesInRange(T->getSourceRange(),
                             SourceMgr, BufferID, Tokens);
  RawSyntax::LayoutList Layout;
  std::copy(ComprisingTokens.begin(),
            ComprisingTokens.end(),
            std::back_inserter(Layout));
  auto Raw = RawSyntax::make(SyntaxKind::UnknownType,
                             Layout,
                             SourcePresence::Present);
  return SyntaxData::make(Raw);
}

RC<SyntaxData> LegacyASTTransformer::getUnknownDecl(Decl *D) {
  SourceRange SR {getStartLocForDecl(D),getEndLocForDecl(D)};
  auto ComprisingTokens = getRawTokenSyntaxesInRange(SR, SourceMgr,
                                                     BufferID, Tokens);
  RawSyntax::LayoutList Layout;
  std::copy(ComprisingTokens.begin(),
            ComprisingTokens.end(),
            std::back_inserter(Layout));
  auto Raw = RawSyntax::make(SyntaxKind::UnknownDecl,
                             Layout,
                             SourcePresence::Present);
  return SyntaxData::make(Raw);
}

RC<SyntaxData> LegacyASTTransformer::getUnknownStmt(Stmt *S) {
  SourceRange SR { S->getStartLoc(), getEndLocForStmt(S) };
  auto ComprisingTokens = getRawTokenSyntaxesInRange(SR, SourceMgr,
                                                 BufferID, Tokens);
  RawSyntax::LayoutList Layout;
  std::copy(ComprisingTokens.begin(),
            ComprisingTokens.end(),
            std::back_inserter(Layout));
  auto Raw = RawSyntax::make(SyntaxKind::UnknownStmt,
                             Layout,
                             SourcePresence::Present);
  return SyntaxData::make(Raw);
}

RC<SyntaxData> LegacyASTTransformer::getUnknownExpr(Expr *E) {
  SourceRange SR { E->getStartLoc(), getEndLocForExpr(E) };
  auto ComprisingTokens = getRawTokenSyntaxesInRange(SR, SourceMgr,
                                                 BufferID, Tokens);
  RawSyntax::LayoutList Layout;
  std::copy(ComprisingTokens.begin(),
            ComprisingTokens.end(),
            std::back_inserter(Layout));
  auto Raw = RawSyntax::make(SyntaxKind::UnknownExpr,
                             Layout,
                             SourcePresence::Present);
  return SyntaxData::make(Raw);
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
  return getUnknownDecl(PBD);
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
LegacyASTTransformer::visitMissingMemberDecl(
    MissingMemberDecl *D,
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

#if 0
  StructDeclSyntaxBuilder StructBuilder;
  if (D->getStartLoc().isValid()) {
    auto StructKeyword = findToken(D->getStartLoc(), tok::kw_struct, "struct");
    StructBuilder.useStructKeyword(StructKeyword);
  }
  
  if (D->getNameLoc().isValid()) {
    auto Identifier = findToken(D->getNameLoc(), tok::identifier,
                                OwnedString(D->getName().str()));
    StructBuilder.useName(Identifier);
  }

  if (D->getBraces().isValid()) {
    auto LeftBraceToken = findToken(D->getBraces().Start, tok::l_brace, "{");
    StructBuilder.useLeftBrace(LeftBraceToken);
    auto RightBraceToken = findToken(D->getBraces().End, tok::r_brace, "}");
    StructBuilder.useRightBrace(RightBraceToken);
  }

  for (auto Member : D->getMembers()) {
    auto TransformedMember = transform(Member);
    if (TransformedMember.hasValue()) {
      if (auto MD = TransformedMember.getValue().getAs<DeclSyntax>()) {
        StructBuilder.addDecl(MD.getValue());
      } else {
        continue;
        return getUnknownDecl(D);
      }
    } else {
      continue;
      return getUnknownDecl(D);
    }
  }

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

  /// Builds type attributes into the provided attribute builder.
  /// Returns `false` if there was no valid attribute.
  auto buildTypeAttribute = [&](SourceRange range,
                                AttributeSyntaxBuilder &attrBuilder) -> bool {
    auto toks = getRawTokenSyntaxesInRange(range, SourceMgr,
                                           BufferID, Tokens);
    if (toks.size() < 2) return false;

    // Handle attributes like `@autoclosure`.
    if (toks[0]->getTokenKind() == tok::at_sign) {
      attrBuilder.useAtSignToken(make<TokenSyntax>(toks[0]));
    }

    if (toks[1]->getTokenKind() == tok::identifier) {
      attrBuilder.useIdentifier(make<TokenSyntax>(toks[1]));
    }

    // Handle attributes like @convention(c)
    if (toks.size() > 2 && toks[2]->getTokenKind() == tok::l_paren) {
      // Get the left paren
      attrBuilder.useLeftParen(make<TokenSyntax>(toks[2]));

      // Walk the rest of the tokens in the range. If the last
      // token is a right paren, use it as "the" right paren.
      // Otherwise, add it to the builder.
      for (size_t i = 3; i < toks.size(); ++i) {
        auto tok = toks[i];
        auto tokSyntax = make<TokenSyntax>(tok);
        if (i == toks.size() - 1 &&
            tok->getTokenKind() == tok::r_paren) {
          attrBuilder.useRightParen(tokSyntax);
        } else {
          attrBuilder.addToken(tokSyntax);
        }
      }
    }

    return true;
  };

  FunctionDeclSyntaxBuilder b;

  auto pair = getAttributesFromDecl(FD);

  for (auto attr : pair.first) {
    b.addAttribute(attr);
  }

  for (auto mod : pair.second) {
    b.addModifier(mod);
  }

  auto funcKW = findToken(FD->getFuncLoc(), tok::kw_func, "func");
  b.useFuncKeyword(funcKW);

  auto name = findToken(FD->getNameLoc(), tok::identifier);
  b.useIdentifier(name);

  if (auto body = FD->getBody()) {
    auto codeBlock = transform(body)->castTo<CodeBlockSyntax>();
    b.useBody(codeBlock);
  }

  FunctionSignatureSyntaxBuilder sigBuilder;
  auto paramList = FD->getParameterList(0);
  sigBuilder.useLeftParen(findToken(paramList->getLParenLoc(),
                                    tok::l_paren));
  sigBuilder.useRightParen(findToken(paramList->getRParenLoc(),
                                     tok::r_paren));
  for (auto param : *paramList) {
    FunctionParameterSyntaxBuilder paramBuilder;

    if (param->getArgumentNameLoc().isValid()) {
      auto externalName = findToken(param->getArgumentNameLoc(),
                                    tok::identifier);
      paramBuilder.useExternalName(externalName);
    }

    if (param->getNameLoc().isValid()) {
      auto localName = findToken(param->getNameLoc(),
                                    tok::identifier);
      paramBuilder.useLocalName(localName);
    }

    if (param->getColonLoc().isValid()) {
      paramBuilder.useColon(findToken(param->getColonLoc(),
                                      tok::colon));
    }

    if (param->getDefaultEqualsLoc().isValid()) {
      paramBuilder.useDefaultEquals(findToken(param->getDefaultEqualsLoc(),
                                              tok::equal));
    }

    if (param->getEllipsisLoc().isValid()) {
      paramBuilder.useEllipsis(findToken(param->getEllipsisLoc(),
                                         None/*any token*/, "..."));
    }

    if (param->getTrailingCommaLoc().isValid()) {
      paramBuilder.useTrailingComma(findToken(param->getTrailingCommaLoc(),
                                              tok::comma));
    }

    TypeAnnotationSyntaxBuilder typeBuilder;
    if (auto typeRepr = param->getTypeLoc().getTypeRepr()) {
      if (auto attrTR = dyn_cast<AttributedTypeRepr>(typeRepr)) {
        if (auto underlyingType = attrTR->getTypeRepr()) {
          if (auto transformed = transform(underlyingType)) {
            typeBuilder.useType(transformed.getValue());
          }
        }

        SmallVector<SourceRange, 4> attrRanges;
        attrTR->getAttrs().getAttrRanges(attrRanges);
        for (auto range : attrRanges) {
          AttributeSyntaxBuilder attrBuilder;
          buildTypeAttribute(range, attrBuilder);
          typeBuilder.addAttribute(attrBuilder.build());
        }
      } else if (auto type = transform(typeRepr)) {
        typeBuilder.useType(type.getValue());
      }
    }

    if (param->getSpecifierLoc().isValid()) {
      typeBuilder.useSpecifier(findToken(param->getSpecifierLoc()));
    }

    paramBuilder.useTypeAnnotation(typeBuilder.build());

    if (param->isDefaultArgument()) {
      auto defaultExpr = param->getDefaultValue();
      if (auto defaultValue = transform(defaultExpr)) {
        paramBuilder.useDefaultValue(defaultValue.getValue());
      } else {
        auto data = getUnknownExpr(defaultExpr);
        paramBuilder.useDefaultValue(ExprSyntax { data, data.get() });
      }
    }

    sigBuilder.addFunctionParameter(paramBuilder.build());
  }

  if (auto typeRepr = FD->getBodyResultTypeLoc().getTypeRepr()) {
    if (auto attrTR = dyn_cast<AttributedTypeRepr>(typeRepr)) {
      typeRepr = attrTR->getTypeRepr();

      SmallVector<SourceRange, 4> attrRanges;
      attrTR->getAttrs().getAttrRanges(attrRanges);
      for (auto range : attrRanges) {
        AttributeSyntaxBuilder attrBuilder;
        buildTypeAttribute(range, attrBuilder);
        sigBuilder.addAttribute(attrBuilder.build());
      }
    }
    if (auto type = transform(typeRepr)) {
      sigBuilder.useReturnType(type.getValue());
    }
  }

  if (FD->getArrowLoc().isValid()) {
    sigBuilder.useArrow(findToken(FD->getArrowLoc(), tok::arrow));
  }
  
  b.useSignature(sigBuilder.build());

  return b.build().Root;
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

  CodeBlockSyntaxBuilder b;
  auto leftBrace = S->getLBraceLoc().isValid()
    ? findToken(S->getLBraceLoc(), tok::l_brace, "{")
    : TokenSyntax::missingToken(tok::l_brace, "{");
  b.useOpenBrace(leftBrace);

  for (auto node : S->getElements()) {
    auto transformed = transform(node);
    if (transformed.hasValue()) {
      /// Because this syntax comes from an ASTNode, we need to handle Decl/Expr
      /// and convert them into implicit DeclStmtSyntax and ExprStmtSyntaxes.
      b.addStmt(getStmtSyntax(*transformed));
    }
  }

  auto rightBrace = S->getLBraceLoc().isValid()
    ? findToken(S->getLBraceLoc(), tok::r_brace, "}")
    : TokenSyntax::missingToken(tok::r_brace, "}");
  b.useCloseBrace(rightBrace);

  return b.build().Root;
}

RC<SyntaxData>
LegacyASTTransformer::visitReturnStmt(ReturnStmt *S,
                                      const SyntaxData *Parent,
                                      const CursorIndex IndexInParent) {
  auto ReturnKW = findToken(S->getReturnLoc(), tok::kw_return, "return");
  auto Semicolon = findToken(S->getEndLoc(), tok::semi, ";");
  auto Result = transform(S->getResult());

  if (!Result.hasValue()) {
    return getUnknownStmt(S);
  }

  return SyntaxFactory::makeReturnStmt(ReturnKW, Result.getValue(),
                                       Semicolon).Root;
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

  auto FallthroughToken = findToken(S->getLoc(), tok::kw_fallthrough,
                                    "fallthrough");
  auto Semicolon = SyntaxFactory::makeToken(tok::semi, ";",
                                            SourcePresence::Missing,
                                            {}, {});
  return SyntaxFactory::makeFallthroughStmt(FallthroughToken, Semicolon).Root;
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
    ? findToken(E->getMinusLoc(), tok::oper_prefix)
    : TokenSyntax::missingToken(tok::oper_prefix, "");
  auto Digits = findToken(E->getDigitsLoc(), tok::integer_literal);
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
LegacyASTTransformer::visitConditionalBridgeFromObjCExpr(
    ConditionalBridgeFromObjCExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitBridgeFromObjCExpr(
    BridgeFromObjCExpr *E,
    const SyntaxData *Parent,
    const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitBridgeToObjCExpr(
    BridgeToObjCExpr *E,
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

RC<SyntaxData> LegacyASTTransformer::visitKeyPathApplicationExpr(
    KeyPathApplicationExpr *E,
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
LegacyASTTransformer::visitKeyPathExpr(KeyPathExpr *E,
                                       const SyntaxData *Parent,
                                       const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

RC<SyntaxData>
LegacyASTTransformer::visitKeyPathDotExpr(KeyPathDotExpr *E,
                                          const SyntaxData *Parent,
                                          const CursorIndex IndexInParent) {
  return getUnknownExpr(E);
}

#pragma mark - TypeReprs

RC<SyntaxData>
LegacyASTTransformer::visitErrorTypeRepr(ErrorTypeRepr *T,
                                  const SyntaxData *Parent,
                                  const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitAttributedTypeRepr(AttributedTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitSimpleIdentTypeRepr(SimpleIdentTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  auto name = findToken(T->getIdLoc(), tok::identifier);
  return SyntaxFactory::makeTypeIdentifier(name, None, None, None).Root;
}

RC<SyntaxData>
LegacyASTTransformer::visitGenericIdentTypeRepr(GenericIdentTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitCompoundIdentTypeRepr(CompoundIdentTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitFunctionTypeRepr(FunctionTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitArrayTypeRepr(ArrayTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitDictionaryTypeRepr(DictionaryTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitOptionalTypeRepr(OptionalTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitImplicitlyUnwrappedOptionalTypeRepr(
  ImplicitlyUnwrappedOptionalTypeRepr *T, const SyntaxData *Parent,
  const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitTupleTypeRepr(TupleTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitCompositionTypeRepr(CompositionTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitMetatypeTypeRepr(MetatypeTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitProtocolTypeRepr(ProtocolTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitInOutTypeRepr(InOutTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitSharedTypeRepr(SharedTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitFixedTypeRepr(FixedTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

RC<SyntaxData>
LegacyASTTransformer::visitSILBoxTypeRepr(SILBoxTypeRepr *T,
  const SyntaxData *Parent, const CursorIndex IndexInParent) {
  return getUnknownType(T);
}

TokenSyntax
LegacyASTTransformer::findToken(SourceLoc Loc,
                                Optional<tok> ExpectedKind,
                                OwnedString ExpectedText) {
  auto Offset = SourceMgr.getLocOffsetInBuffer(Loc, BufferID);

  size_t Start = 0;
  size_t End = Tokens.size() - 1;

  while (Start <= End) {
    auto Mid = (Start + End) / 2;
    auto TokAndPos = Tokens[Mid];
    auto Tok = TokAndPos.first;
    auto Pos = TokAndPos.second;

    auto TokStart = Pos.getOffset();

    // If we're expecting a specific kind of token, then check here. These
    // flags will be true if there is no expectation passed-in.
    auto IsExpectedKind = 
      !ExpectedKind.hasValue() || Tok->getTokenKind() == *ExpectedKind;
    auto IsExpectedText = ExpectedText.empty() ||
      Tok->getText() == ExpectedText.str();

    if (Offset == TokStart) {
      if (IsExpectedKind && IsExpectedText) {
        return make<TokenSyntax>(Tok);
      } else {
        return TokenSyntax::missingToken(ExpectedKind.getValueOr(tok::unknown),
                                         ExpectedText);
      }
    }

    if (TokStart < Offset) {
      Start = Mid + 1;
    } else {
      End = Mid - 1;
    }
  }

  return TokenSyntax::missingToken(ExpectedKind.getValueOr(tok::unknown),
                                   ExpectedText);
}
