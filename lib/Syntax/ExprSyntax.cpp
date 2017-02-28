//===--- ExprSyntax.cpp - Swift Expression Syntax Impl. ---------*- C++ -*-===//
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

#include "swift/Syntax/ExprSyntax.h"
#include "swift/Syntax/GenericSyntax.h"

using namespace swift;
using namespace swift::syntax;

#pragma mark - expression Data

RC<ExprSyntaxData> ExprSyntaxData::make(RC<RawSyntax> Raw,
                                        const SyntaxData *Parent,
                                        CursorIndex IndexInParent) {
  return RC<ExprSyntaxData> {
    new ExprSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<ExprSyntaxData> ExprSyntaxData::makeBlank() {
  return make(RawSyntax::missing(SyntaxKind::MissingExpr));
}

#pragma mark - expression API

ExprSyntax::ExprSyntax(const RC<SyntaxData> Root, const ExprSyntaxData *Data)
  : Syntax(Root, Data) {}

#pragma mark - unknown-expression Data

UnknownExprSyntaxData::UnknownExprSyntaxData(RC<RawSyntax> Raw,
                                             const SyntaxData *Parent,
                                             CursorIndex IndexInParent)
  : ExprSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::UnknownExpr);
}

RC<UnknownExprSyntaxData>
UnknownExprSyntaxData::make(RC<RawSyntax> Raw,
                            const SyntaxData *Parent,
                            CursorIndex IndexInParent) {
  return RC<UnknownExprSyntaxData> {
    new UnknownExprSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

#pragma mark - unknown-expression API

UnknownExprSyntax::UnknownExprSyntax(const RC<SyntaxData> Root,
                                     const UnknownExprSyntaxData *Data)
  : ExprSyntax(Root, Data) {}

#pragma mark - integer-literal-expression Data

IntegerLiteralExprSyntaxData::
IntegerLiteralExprSyntaxData(RC<RawSyntax> Raw,
                             const SyntaxData *Parent,
                             CursorIndex IndexInParent)
  : ExprSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::IntegerLiteralExpr);
  assert(Raw->Layout.size() == 2);
  syntax_assert_child_token(Raw, IntegerLiteralExprSyntax::Cursor::Sign,
                            tok::oper_prefix);
  syntax_assert_child_token(Raw, IntegerLiteralExprSyntax::Cursor::Digits,
                            tok::integer_literal);
}

RC<IntegerLiteralExprSyntaxData>
IntegerLiteralExprSyntaxData::make(RC<RawSyntax> Raw,
                                   const SyntaxData *Parent,
                                   CursorIndex IndexInParent) {
  return RC<IntegerLiteralExprSyntaxData> {
    new IntegerLiteralExprSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}
RC<IntegerLiteralExprSyntaxData> IntegerLiteralExprSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::IntegerLiteralExpr,
    {
      TokenSyntax::missingToken(tok::oper_prefix, ""),
      TokenSyntax::missingToken(tok::integer_literal, "")
    },
    SourcePresence::Present);
  return make(Raw);
}

#pragma mark - integer-literal-expression API

IntegerLiteralExprSyntax::
IntegerLiteralExprSyntax(const RC<SyntaxData> Root,
                         const IntegerLiteralExprSyntaxData *Data)
  : ExprSyntax(Root, Data) {}

IntegerLiteralExprSyntax
IntegerLiteralExprSyntax::withDigits(RC<TokenSyntax> NewDigits) const {
  assert(NewDigits->getTokenKind() == tok::integer_literal);
  return Data->replaceChild<IntegerLiteralExprSyntax>(NewDigits,
                                                      Cursor::Digits);
}

#pragma mark - symbolic-reference Data

SymbolicReferenceExprSyntaxData::
SymbolicReferenceExprSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent,
                                CursorIndex IndexInParent)
  : ExprSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Layout.size() == 2);
  syntax_assert_child_token(Raw,
    SymbolicReferenceExprSyntax::Cursor::Identifier, tok::identifier);
  syntax_assert_child_kind(Raw,
    SymbolicReferenceExprSyntax::Cursor::GenericArgumentClause,
    SyntaxKind::GenericArgumentClause);
}

RC<SymbolicReferenceExprSyntaxData>
SymbolicReferenceExprSyntaxData::make(RC<RawSyntax> Raw,
                                      const SyntaxData *Parent,
     CursorIndex IndexInParent) {
  return RC<SymbolicReferenceExprSyntaxData> {
    new SymbolicReferenceExprSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<SymbolicReferenceExprSyntaxData>
SymbolicReferenceExprSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::SymbolicReferenceExpr,
    {
      TokenSyntax::missingToken(tok::identifier, ""),
      RawSyntax::missing(SyntaxKind::GenericArgumentClause),
    },
    SourcePresence::Present);
  return make(Raw);
}

#pragma mark - symbolic-reference API

SymbolicReferenceExprSyntax::
SymbolicReferenceExprSyntax(const RC<SyntaxData> Root, const DataType *Data)
  : ExprSyntax(Root, Data) {}

RC<TokenSyntax> SymbolicReferenceExprSyntax::getIdentifier() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::Identifier));
}

SymbolicReferenceExprSyntax SymbolicReferenceExprSyntax::
withIdentifier(RC<TokenSyntax> NewIdentifier) const {
  assert(NewIdentifier->getTokenKind() == tok::identifier);
  return Data->replaceChild<SymbolicReferenceExprSyntax>(NewIdentifier,
                                                         Cursor::Identifier);
}

llvm::Optional<GenericArgumentClauseSyntax>
SymbolicReferenceExprSyntax::getGenericArgumentClause() const {
  auto RawClause = getRaw()->getChild(Cursor::GenericArgumentClause);
  if (RawClause->isMissing()) {
    return llvm::None;
  }

  auto *MyData = getUnsafeData<SymbolicReferenceExprSyntax>();
  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedGenericArgClause);
  SyntaxData::realizeSyntaxNode<GenericArgumentClauseSyntax>(ChildPtr,
                                                             RawClause, MyData,
    cursorIndex(Cursor::GenericArgumentClause));

  return llvm::Optional<GenericArgumentClauseSyntax> {
    GenericArgumentClauseSyntax {
      Root,
      MyData->CachedGenericArgClause.get()
    }
  };
}

SymbolicReferenceExprSyntax SymbolicReferenceExprSyntax::
withGenericArgumentClause(GenericArgumentClauseSyntax NewGenericArgs) const {
  return Data->replaceChild<SymbolicReferenceExprSyntax>(
    NewGenericArgs.getRaw(), Cursor::GenericArgumentClause);
}

#pragma mark - function-call-argument Data

FunctionCallArgumentSyntaxData::
FunctionCallArgumentSyntaxData(RC<RawSyntax> Raw,
                               const SyntaxData *Parent,
                               CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
    syntax_assert_child_token(Raw, FunctionCallArgumentSyntax::Cursor::Label,
                              tok::identifier);
    syntax_assert_child_token_text(Raw,
                                   FunctionCallArgumentSyntax::Cursor::Colon,
                                   tok::colon, ":");
    assert(
      Raw->getChild(FunctionCallArgumentSyntax::Cursor::Expression)->isExpr());

    syntax_assert_child_token_text(Raw,
                                   FunctionCallArgumentSyntax::Cursor::Comma,
                                   tok::comma, ",");
}

RC<FunctionCallArgumentSyntaxData>
FunctionCallArgumentSyntaxData::make(RC<RawSyntax> Raw,
                                     const SyntaxData *Parent,
                                     CursorIndex IndexInParent) {
  return RC<FunctionCallArgumentSyntaxData> {
    new FunctionCallArgumentSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<FunctionCallArgumentSyntaxData> FunctionCallArgumentSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionCallArgument,
                             {
                               TokenSyntax::missingToken(tok::identifier, ""),
                               TokenSyntax::missingToken(tok::colon, ":"),
                               RawSyntax::missing(SyntaxKind::MissingExpr),
                               TokenSyntax::missingToken(tok::comma, ",")
                             },
                             SourcePresence::Present);
  return make(Raw);
}

#pragma mark - function-call-argument API

FunctionCallArgumentSyntax::
FunctionCallArgumentSyntax(const RC<SyntaxData> Root, const DataType *Data)
  : Syntax(Root, Data) {}

RC<TokenSyntax> FunctionCallArgumentSyntax::getLabel() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::Label));
}

FunctionCallArgumentSyntax
FunctionCallArgumentSyntax::withLabel(RC<TokenSyntax> NewLabel) const {
  assert(NewLabel->getTokenKind() == tok::identifier);
  return Data->replaceChild<FunctionCallArgumentSyntax>(NewLabel,
                                                        Cursor::Label);
}

RC<TokenSyntax> FunctionCallArgumentSyntax::getColonToken() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::Colon));
}

FunctionCallArgumentSyntax
FunctionCallArgumentSyntax::withColonToken(RC<TokenSyntax> NewColon) const {
  syntax_assert_token_is(NewColon, tok::colon, ":");
  return Data->replaceChild<FunctionCallArgumentSyntax>(NewColon,
                                                        Cursor::Colon);
}

llvm::Optional<ExprSyntax> FunctionCallArgumentSyntax::getExpression() const {

  auto RawExpression = getRaw()->getChild(Cursor::Expression);
  if (RawExpression->isMissing()) {
    return llvm::None;
  }

  auto *MyData = getUnsafeData<FunctionCallArgumentSyntax>();

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedExpression);

  SyntaxData::realizeSyntaxNode<ExprSyntax>(ChildPtr, RawExpression, MyData,
                                            cursorIndex(Cursor::Expression));
  
  return ExprSyntax { Root, MyData->CachedExpression.get() };
}

FunctionCallArgumentSyntax
FunctionCallArgumentSyntax::withExpression(ExprSyntax NewExpression) const {
  return Data->replaceChild<FunctionCallArgumentSyntax>(NewExpression.getRaw(),
                                                        Cursor::Expression);
}

RC<TokenSyntax> FunctionCallArgumentSyntax::getTrailingComma() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::Comma));
}

FunctionCallArgumentSyntax FunctionCallArgumentSyntax::
withTrailingComma(RC<TokenSyntax> NewTrailingComma) const {
  syntax_assert_token_is(NewTrailingComma, tok::comma, ",");
  return Data->replaceChild<FunctionCallArgumentSyntax>(NewTrailingComma,
    FunctionCallArgumentSyntax::Cursor::Comma);
}

#pragma mark - function-call-argument-list Data

FunctionCallArgumentListSyntaxData::
FunctionCallArgumentListSyntaxData(const RC<RawSyntax> Raw,
                                   const SyntaxData *Parent,
                                   CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
#ifndef NDEBUG
  for (auto Child : Raw->Layout) {
    assert(Child->Kind == SyntaxKind::FunctionCallArgument);
  }
#endif
  for (size_t i = 0; i < Raw->Layout.size(); ++i) {
    CachedArguments.emplace_back(nullptr);
  }
}

RC<FunctionCallArgumentListSyntaxData>
FunctionCallArgumentListSyntaxData::make(RC<RawSyntax> Raw,
                                         const SyntaxData *Parent,
                                         CursorIndex IndexInParent) {
  return RC<FunctionCallArgumentListSyntaxData> {
    new FunctionCallArgumentListSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<FunctionCallArgumentListSyntaxData>
FunctionCallArgumentListSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionCallArgumentList, {},
                             SourcePresence::Present);
  return make(Raw);
}

#pragma mark - function-call-argument-list API

FunctionCallArgumentListSyntax::
FunctionCallArgumentListSyntax(const RC<SyntaxData> Root,
                               const DataType *Data)
  : Syntax(Root, Data) {}

size_t
FunctionCallArgumentListSyntax::getNumArguments() const {
  return getRaw()->Layout.size();
}

FunctionCallArgumentSyntax
FunctionCallArgumentListSyntax::getArgument(size_t Index) const {
  assert(Index <= getRaw()->Layout.size());

  auto RawArg = getRaw()->Layout[Index];

  auto *MyData = getUnsafeData<FunctionCallArgumentListSyntax>();

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    MyData->CachedArguments.data() + Index);

  SyntaxData::realizeSyntaxNode<FunctionCallArgumentSyntax>(ChildPtr, RawArg,
                                                            MyData,
                                                            Index);

  return FunctionCallArgumentSyntax {
    Root,
    MyData->CachedArguments[Index].get()
  };
}

FunctionCallArgumentListSyntax
FunctionCallArgumentListSyntax::withAdditionalArgument(
  FunctionCallArgumentSyntax AdditionalArgument) const {
  auto NewRaw = getRaw()->append(AdditionalArgument.getRaw());
  return Data->replaceSelf<FunctionCallArgumentListSyntax>(NewRaw);
}

#pragma mark - function-call-expression Data

RC<FunctionCallArgumentListSyntaxData> CachedArgumentList;

FunctionCallExprSyntaxData::FunctionCallExprSyntaxData(RC<RawSyntax> Raw,
                           const SyntaxData *Parent,
                           CursorIndex IndexInParent)
    : ExprSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Layout.size() == 4);
  assert(Raw->getChild(FunctionCallExprSyntax::Cursor::CalledExpression)
           ->isExpr());
  syntax_assert_child_token_text(Raw, FunctionCallExprSyntax::Cursor::LeftParen,
                                 tok::l_paren, "(");
  syntax_assert_child_kind(Raw, FunctionCallExprSyntax::Cursor::ArgumentList,
                           SyntaxKind::FunctionCallArgumentList);
  syntax_assert_child_token_text(Raw,
                                 FunctionCallExprSyntax::Cursor::RightParen,
                                 tok::r_paren, ")");
}

RC<FunctionCallExprSyntaxData>
FunctionCallExprSyntaxData::make(RC<RawSyntax> Raw, const SyntaxData *Parent,
                                 CursorIndex IndexInParent) {
  return RC<FunctionCallExprSyntaxData> {
    new FunctionCallExprSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<FunctionCallExprSyntaxData> FunctionCallExprSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionCallExpr,
  {
    RawSyntax::missing(SyntaxKind::MissingExpr),
    TokenSyntax::missingToken(tok::l_paren, "("),
    RawSyntax::missing(SyntaxKind::FunctionCallArgumentList),
    TokenSyntax::missingToken(tok::r_paren, ")"),
  },
  SourcePresence::Present);
  return make(Raw);
}


#pragma mark - function-call-expression API

FunctionCallExprSyntax::FunctionCallExprSyntax(const RC<SyntaxData> Root,
                                               const DataType *Data)
  : ExprSyntax(Root, Data) {}

ExprSyntax FunctionCallExprSyntax::getCalledExpression() const {
  auto RawArg = getRaw()->getChild(Cursor::CalledExpression);

  auto *MyData = getUnsafeData<FunctionCallExprSyntax>();

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedCalledExpression);

  SyntaxData::realizeSyntaxNode<ExprSyntax>(
    ChildPtr, RawArg, MyData, cursorIndex(Cursor::CalledExpression));

  return ExprSyntax {
    Root,
    MyData->CachedCalledExpression.get(),
  };
}

FunctionCallExprSyntax FunctionCallExprSyntax::
withCalledExpression(ExprSyntax NewBaseExpression) const {
  return Data->replaceChild<FunctionCallExprSyntax>(NewBaseExpression.getRaw(),
                                                    Cursor::CalledExpression);
}

RC<TokenSyntax> FunctionCallExprSyntax::getLeftParen() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::LeftParen));
}

FunctionCallExprSyntax
FunctionCallExprSyntax::withLeftParen(RC<TokenSyntax> NewLeftParen) const {
  syntax_assert_token_is(NewLeftParen, tok::l_paren, "(");
  return Data->replaceChild<FunctionCallExprSyntax>(NewLeftParen,
                                                    Cursor::LeftParen);
}

FunctionCallArgumentListSyntax FunctionCallExprSyntax::getArgumentList() const {
  auto RawArg = getRaw()->getChild(Cursor::ArgumentList);

  auto *MyData = getUnsafeData<FunctionCallExprSyntax>();

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedArgumentList);

  SyntaxData::realizeSyntaxNode<FunctionCallArgumentListSyntax>(
    ChildPtr, RawArg, MyData, cursorIndex(Cursor::ArgumentList));
  
  return FunctionCallArgumentListSyntax {
    Root,
    MyData->CachedArgumentList.get(),
  };
}

FunctionCallExprSyntax FunctionCallExprSyntax::
withArgumentList(FunctionCallArgumentListSyntax NewArgumentList) const {
  return Data->replaceChild<FunctionCallExprSyntax>(NewArgumentList.getRaw(),
                                                    Cursor::ArgumentList);
}

RC<TokenSyntax> FunctionCallExprSyntax::getRightParen() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::RightParen));
}

FunctionCallExprSyntax
FunctionCallExprSyntax::withRightParen(RC<TokenSyntax> NewRightParen) const {
  syntax_assert_token_is(NewRightParen, tok::r_paren, ")");
  return Data->replaceChild<FunctionCallExprSyntax>(NewRightParen,
                                                    Cursor::RightParen);
}

#pragma mark - function-call-expression Builder

FunctionCallExprSyntaxBuilder::FunctionCallExprSyntaxBuilder()
  : CallLayout(FunctionCallExprSyntaxData::makeBlank()->getRaw()->Layout),
    ListLayout(
      FunctionCallArgumentListSyntaxData::makeBlank()->getRaw()->Layout) {}

FunctionCallExprSyntaxBuilder &
FunctionCallExprSyntaxBuilder::useLeftParen(RC<TokenSyntax> LeftParen) {
  syntax_assert_token_is(LeftParen, tok::l_paren, "(");
  CallLayout[cursorIndex(FunctionCallExprSyntax::Cursor::LeftParen)]
    = LeftParen;
  return *this;
}

FunctionCallExprSyntaxBuilder &FunctionCallExprSyntaxBuilder::
appendArgument(FunctionCallArgumentSyntax AdditionalArgument) {
  ListLayout.push_back(AdditionalArgument.getRaw());
  return *this;
}

FunctionCallExprSyntaxBuilder &FunctionCallExprSyntaxBuilder::
useCalledExpression(ExprSyntax CalledExpression) {
  CallLayout[cursorIndex(FunctionCallExprSyntax::Cursor::CalledExpression)]
    = CalledExpression.getRaw();
  return *this;
}

FunctionCallExprSyntaxBuilder &
FunctionCallExprSyntaxBuilder::useRightParen(RC<TokenSyntax> RightParen) {
  syntax_assert_token_is(RightParen, tok::r_paren, ")");
  CallLayout[cursorIndex(FunctionCallExprSyntax::Cursor::RightParen)]
    = RightParen;
  return *this;
}

FunctionCallExprSyntax FunctionCallExprSyntaxBuilder::build() const {
  auto RawArgs = RawSyntax::make(SyntaxKind::FunctionCallArgumentList,
                                 ListLayout, SourcePresence::Present);
  auto RawCall = RawSyntax::make(SyntaxKind::FunctionCallExpr, CallLayout,
                                 SourcePresence::Present)
    ->replaceChild(FunctionCallExprSyntax::Cursor::ArgumentList, RawArgs);
  auto Data = FunctionCallExprSyntaxData::make(RawCall);
  return { Data, Data.get() };
}
