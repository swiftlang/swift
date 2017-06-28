//===--- DeclSyntax.cpp - Declaration Syntax Implementation ---------------===//
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

#include "swift/Syntax/DeclSyntax.h"
#include "swift/Syntax/ExprSyntax.h"
#include "swift/Syntax/GenericSyntax.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/StmtSyntax.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/TypeSyntax.h"

using namespace swift;
using namespace swift::syntax;

#pragma mark - declaration API

void DeclModifierSyntax::validate() const {
  assert(Data->Raw->isDecl());
}

DeclModifierSyntax DeclModifierSyntax::makeBlank() {
  return make<DeclModifierSyntax>(
            RawSyntax::make(SyntaxKind::DeclModifier,
                            {
                              RawTokenSyntax::missingToken(tok::identifier, ""),
                              RawTokenSyntax::missingToken(tok::l_paren, "("),
                              RawTokenSyntax::missingToken(tok::identifier, ""),
                              RawTokenSyntax::missingToken(tok::r_paren, ")"),
                            },
                            SourcePresence::Present));
}

#pragma mark - declaration-modifier API

TokenSyntax DeclModifierSyntax::getName() const {
  return { Root, Data->getChild(Cursor::Name).get() };
}

DeclModifierSyntax DeclModifierSyntax::withName(TokenSyntax NewName) const {
  assert(NewName.getTokenKind() == tok::identifier);
  return Data->replaceChild<DeclModifierSyntax>(NewName.getRaw(), Cursor::Name);
}

TokenSyntax DeclModifierSyntax::getLeftParenToken() const {
  return { Root, Data->getChild(Cursor::LeftParen).get() };
}

DeclModifierSyntax
DeclModifierSyntax::withLeftParenToken(TokenSyntax NewLeftParen) const {
  syntax_assert_token_is(NewLeftParen, tok::l_paren, "(");
  return Data->replaceChild<DeclModifierSyntax>(NewLeftParen.getRaw(),
                                                Cursor::LeftParen);
}

TokenSyntax DeclModifierSyntax::getArgument() const {
  return { Root, Data->getChild(Cursor::Argument).get() };
}

DeclModifierSyntax
DeclModifierSyntax::withArgument(TokenSyntax NewArgument) const {
  assert(NewArgument.getTokenKind() == tok::identifier);
  return Data->replaceChild<DeclModifierSyntax>(NewArgument.getRaw(),
                                                Cursor::Argument);
}

TokenSyntax DeclModifierSyntax::getRightParenToken() const {
  return { Root, Data->getChild(Cursor::RightParen).get() };
}

DeclModifierSyntax
DeclModifierSyntax::withRightParenToken(TokenSyntax NewRightParen) const {
  syntax_assert_token_is(NewRightParen, tok::r_paren, ")");
  return Data->replaceChild<DeclModifierSyntax>(NewRightParen.getRaw(),
                                                Cursor::RightParen);
}

#pragma mark - unknown-statement API

void UnknownDeclSyntax::validate() const {
    assert(Data->Raw->Kind == SyntaxKind::UnknownDecl);
}

#pragma mark - declaration-members API

DeclMembersSyntax DeclMembersSyntax::makeBlank() {
  return make<DeclMembersSyntax>(RawSyntax::make(SyntaxKind::DeclMembers, {},
                                                 SourcePresence::Present));
}

#pragma mark - declaration-members Builder

DeclMembersSyntaxBuilder &
DeclMembersSyntaxBuilder::addMember(DeclSyntax Member) {
  MembersLayout.push_back(Member.getRaw());
  return *this;
}

DeclMembersSyntax DeclMembersSyntaxBuilder::build() const {
  auto Raw = RawSyntax::make(SyntaxKind::DeclMembers, MembersLayout,
                             SourcePresence::Present);
  auto Data = SyntaxData::make(Raw);
  return DeclMembersSyntax { Data, Data.get() };
}

#pragma mark - struct-declaration Data

void StructDeclSyntax::validate() const {
  assert(Data->Raw->Kind == SyntaxKind::StructDecl);
  syntax_assert_child_token_text(Data->Raw,
                                 StructDeclSyntax::Cursor::StructKeyword,
                                 tok::kw_struct, "struct");
  syntax_assert_child_token(Data->Raw, StructDeclSyntax::Cursor::Identifier,
                            tok::identifier);
  syntax_assert_child_kind(Data->Raw,
                           StructDeclSyntax::Cursor::GenericParameterClause,
                           SyntaxKind::GenericParameterClause);
  syntax_assert_child_kind(Data->Raw,
                           StructDeclSyntax::Cursor::GenericWhereClause,
                           SyntaxKind::GenericWhereClause);
  syntax_assert_child_token_text(Data->Raw, StructDeclSyntax::Cursor::LeftBrace,
                                 tok::l_brace, "{");
  syntax_assert_child_kind(Data->Raw, StructDeclSyntax::Cursor::Members,
                           SyntaxKind::DeclMembers);
  syntax_assert_child_token_text(Data->Raw,
                                 StructDeclSyntax::Cursor::RightBrace,
                                 tok::r_brace, "}");
}

StructDeclSyntax StructDeclSyntax::makeBlank() {
  return make<StructDeclSyntax>(RawSyntax::make(SyntaxKind::StructDecl,
    {
      RawTokenSyntax::missingToken(tok::kw_struct, "struct"),
      RawTokenSyntax::missingToken(tok::identifier, ""),
      RawSyntax::missing(SyntaxKind::GenericParameterClause),
      RawSyntax::missing(SyntaxKind::GenericWhereClause),
      RawTokenSyntax::missingToken(tok::l_brace, "{"),
      RawSyntax::missing(SyntaxKind::DeclMembers),
      RawTokenSyntax::missingToken(tok::r_brace, "}"),
    },
    SourcePresence::Present));
}

#pragma mark - struct-declaration API

TokenSyntax StructDeclSyntax::getStructKeyword() const {
  return { Root, Data->getChild(Cursor::StructKeyword).get() };
}

StructDeclSyntax
StructDeclSyntax::withStructKeyword(TokenSyntax NewStructKeyword)
const {
  syntax_assert_token_is(NewStructKeyword, tok::kw_struct, "struct");
  return Data->replaceChild<StructDeclSyntax>(NewStructKeyword.getRaw(),
                                              Cursor::StructKeyword);
}

StructDeclSyntax
StructDeclSyntax::withLeftBrace(TokenSyntax NewLeftBrace) const {
  syntax_assert_token_is(NewLeftBrace, tok::l_brace, "{");
  return Data->replaceChild<StructDeclSyntax>(NewLeftBrace.getRaw(),
                                              Cursor::LeftBrace);
}

TokenSyntax StructDeclSyntax::getLeftBraceToken() const {
  return { Root, Data->getChild(Cursor::LeftBrace).get() };
}

StructDeclSyntax
StructDeclSyntax::withMembers(DeclMembersSyntax NewMembers) const {
  return Data->replaceChild<StructDeclSyntax>(NewMembers.getRaw(),
                                              Cursor::Members);
}

DeclMembersSyntax StructDeclSyntax::getMembers() const {
  return DeclMembersSyntax { Root, Data->getChild(Cursor::Members).get() };
}

#pragma mark - struct-declaration Builder

StructDeclSyntaxBuilder::StructDeclSyntaxBuilder()
  : StructLayout(SyntaxFactory::makeBlankStructDecl().getRaw()->Layout) {}

StructDeclSyntaxBuilder &
StructDeclSyntaxBuilder::useStructKeyword(TokenSyntax StructKeyword) {
  syntax_assert_token_is(StructKeyword, tok::kw_struct, "struct");
  auto Index = cursorIndex(StructDeclSyntax::Cursor::StructKeyword);
  StructLayout[Index] = StructKeyword.getRaw();
  return *this;
}

StructDeclSyntaxBuilder &
StructDeclSyntaxBuilder::useIdentifier(TokenSyntax Identifier) {
  assert(Identifier.getTokenKind() == tok::identifier);
  auto Index = cursorIndex(StructDeclSyntax::Cursor::Identifier);
  StructLayout[Index] = Identifier.getRaw();
  return *this;
}

StructDeclSyntaxBuilder &
StructDeclSyntaxBuilder::useLeftBrace(TokenSyntax LeftBrace) {
  syntax_assert_token_is(LeftBrace, tok::l_brace, "{");
  auto Index = cursorIndex(StructDeclSyntax::Cursor::LeftBrace);
  StructLayout[Index] = LeftBrace.getRaw();
  return *this;
}

StructDeclSyntaxBuilder &
StructDeclSyntaxBuilder::useMembers(DeclMembersSyntax Members) {
  auto Index = cursorIndex(StructDeclSyntax::Cursor::Members);
  StructLayout[Index] = Members.getRaw();
  return *this;
}

StructDeclSyntaxBuilder &
StructDeclSyntaxBuilder::useRightBrace(TokenSyntax RightBrace) {
  syntax_assert_token_is(RightBrace, tok::r_brace, "}");
  auto Index = cursorIndex(StructDeclSyntax::Cursor::RightBrace);
  StructLayout[Index] = RightBrace.getRaw();
  return *this;
}

StructDeclSyntax StructDeclSyntaxBuilder::build() const {
  auto Raw = RawSyntax::make(SyntaxKind::StructDecl, StructLayout,
                             SourcePresence::Present);
  auto Data = SyntaxData::make(Raw);
  return StructDeclSyntax { Data, Data.get() };
}

TypeAliasDeclSyntax
TypeAliasDeclSyntax::makeBlank() {
  return make<TypeAliasDeclSyntax>(RawSyntax::make(SyntaxKind::TypeAliasDecl,
    {
      RawTokenSyntax::missingToken(tok::kw_typealias, "typealias"),
      RawTokenSyntax::missingToken(tok::identifier, ""),
      RawSyntax::missing(SyntaxKind::GenericParameterClause),
      RawTokenSyntax::missingToken(tok::equal, "="),
      RawSyntax::missing(SyntaxKind::MissingType),
    },
    SourcePresence::Present));
}

#pragma mark - type-alias API

TypeAliasDeclSyntax TypeAliasDeclSyntax::
withTypeAliasKeyword(TokenSyntax NewTypeAliasKeyword) const {
  syntax_assert_token_is(NewTypeAliasKeyword, tok::kw_typealias, "typealias");
  return Data->replaceChild<TypeAliasDeclSyntax>(NewTypeAliasKeyword.getRaw(),
                                                 Cursor::TypeAliasKeyword);
}

TypeAliasDeclSyntax
TypeAliasDeclSyntax::withIdentifier(TokenSyntax NewIdentifier) const {
  assert(NewIdentifier.getTokenKind() == tok::identifier);
  return Data->replaceChild<TypeAliasDeclSyntax>(NewIdentifier.getRaw(),
                                                 Cursor::Identifier);
}

TypeAliasDeclSyntax TypeAliasDeclSyntax::
withGenericParameterClause(GenericParameterClauseSyntax NewGenericParams)
const {
  return Data->replaceChild<TypeAliasDeclSyntax>(NewGenericParams.getRaw(),
    Cursor::GenericParameterClause);
}

TypeAliasDeclSyntax
TypeAliasDeclSyntax::withEqualToken(TokenSyntax NewEqualToken) const {
  syntax_assert_token_is(NewEqualToken, tok::equal, "=");
  return Data->replaceChild<TypeAliasDeclSyntax>(NewEqualToken.getRaw(),
                                                 Cursor::EqualToken);
}

TypeAliasDeclSyntax
TypeAliasDeclSyntax::withTypeSyntax(TypeSyntax NewType) const {
  return Data->replaceChild<TypeAliasDeclSyntax>(NewType.getRaw(),
                                                 Cursor::Type);
}

#pragma mark - type-alias Builder

TypeAliasDeclSyntaxBuilder::TypeAliasDeclSyntaxBuilder()
  : TypeAliasLayout(SyntaxFactory::makeBlankTypealiasDecl().getRaw()->Layout)
  {}

TypeAliasDeclSyntaxBuilder &TypeAliasDeclSyntaxBuilder::
useTypeAliasKeyword(TokenSyntax TypeAliasKeyword) {
  syntax_assert_token_is(TypeAliasKeyword, tok::kw_typealias, "typealias");
  auto Index = cursorIndex(TypeAliasDeclSyntax::Cursor::TypeAliasKeyword);
  TypeAliasLayout[Index] = TypeAliasKeyword.getRaw();
  return *this;
}

TypeAliasDeclSyntaxBuilder &
TypeAliasDeclSyntaxBuilder::useIdentifier(TokenSyntax Identifier) {
  assert(Identifier.getTokenKind() == tok::identifier);
  auto Index = cursorIndex(TypeAliasDeclSyntax::Cursor::Identifier);
  TypeAliasLayout[Index] = Identifier.getRaw();
  return *this;
}

TypeAliasDeclSyntaxBuilder &TypeAliasDeclSyntaxBuilder::
useGenericParameterClause(GenericParameterClauseSyntax GenericParams) {
  auto Index = cursorIndex(TypeAliasDeclSyntax::Cursor::GenericParameterClause);
  TypeAliasLayout[Index] = GenericParams.getRaw();
  return *this;
}

TypeAliasDeclSyntaxBuilder &
TypeAliasDeclSyntaxBuilder::useEqualToken(TokenSyntax EqualToken) {
  auto Index = cursorIndex(TypeAliasDeclSyntax::Cursor::EqualToken);
  TypeAliasLayout[Index] = EqualToken.getRaw();
  return *this;
}

TypeAliasDeclSyntaxBuilder &
TypeAliasDeclSyntaxBuilder::useType(TypeSyntax ReferentType) {
  auto Index = cursorIndex(TypeAliasDeclSyntax::Cursor::Type);
  TypeAliasLayout[Index] = ReferentType.getRaw();
  return *this;
}

TypeAliasDeclSyntax TypeAliasDeclSyntaxBuilder::build() const {
  auto Raw = RawSyntax::make(SyntaxKind::TypeAliasDecl, TypeAliasLayout,
                             SourcePresence::Present);
  auto Data = SyntaxData::make(Raw);
  return { Data, Data.get() };
}

#pragma mark - function-parameter Data

void FunctionParameterSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Layout.size() == 8);
  syntax_assert_child_token(Raw, Cursor::ExternalName,
                            tok::identifier);
  syntax_assert_child_token(Raw, Cursor::LocalName,
                            tok::identifier);
  syntax_assert_child_token_text(Raw, Cursor::Colon, tok::colon, ":");
  assert(Raw->getChild(Cursor::Type)->isType());
  syntax_assert_child_token_text(Raw, Cursor::Ellipsis, tok::identifier, "...");
  syntax_assert_child_token_text(Raw, Cursor::DefaultEqual, tok::equal, "=");
  assert(Raw->getChild(Cursor::DefaultExpression)->isExpr());
  syntax_assert_child_token_text(Raw, Cursor::TrailingComma, tok::comma, ",");
}

FunctionParameterSyntax FunctionParameterSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionParameter,
  {
    RawTokenSyntax::missingToken(tok::identifier, ""),
    RawTokenSyntax::missingToken(tok::identifier, ""),
    RawTokenSyntax::missingToken(tok::colon, ":"),
    RawSyntax::missing(SyntaxKind::MissingType),
    RawTokenSyntax::missingToken(tok::identifier, "..."),
    RawTokenSyntax::missingToken(tok::equal, "="),
    RawSyntax::missing(SyntaxKind::MissingExpr),
    RawTokenSyntax::missingToken(tok::comma, ","),
  },
  SourcePresence::Present);
  return make<FunctionParameterSyntax>(Raw);
}


#pragma mark - function-parameter API

TokenSyntax FunctionParameterSyntax::getExternalName() const {
  return { Root, Data->getChild(Cursor::ExternalName).get() };
}

FunctionParameterSyntax FunctionParameterSyntax::
withExternalName(TokenSyntax NewExternalName) const {
  assert(NewExternalName.getTokenKind() == tok::identifier);
  return Data->replaceChild<FunctionParameterSyntax>(NewExternalName.getRaw(),
                                                     Cursor::ExternalName);
}

TokenSyntax FunctionParameterSyntax::getLocalName() const {
  return { Root, Data->getChild(Cursor::LocalName).get() };
}

FunctionParameterSyntax FunctionParameterSyntax::
withLocalName(TokenSyntax NewLocalName) const {
  assert(NewLocalName.getTokenKind() == tok::identifier);
  return Data->replaceChild<FunctionParameterSyntax>(NewLocalName.getRaw(),
                                                     Cursor::LocalName);
}

TokenSyntax FunctionParameterSyntax::getColonToken() const {
  return { Root, Data->getChild(Cursor::Colon).get() };
}

FunctionParameterSyntax FunctionParameterSyntax::
withColonToken(TokenSyntax NewColonToken) const {
  syntax_assert_token_is(NewColonToken, tok::colon, ":");
  return Data->replaceChild<FunctionParameterSyntax>(NewColonToken.getRaw(),
                                                     Cursor::Colon);
}

llvm::Optional<TypeSyntax> FunctionParameterSyntax::getTypeSyntax() const {
  auto RawType = getRaw()->getChild(Cursor::Type);
  if (RawType->isMissing()) {
    return llvm::None;
  }

  return TypeSyntax { Root, Data->getChild(Cursor::Type).get() };
}

FunctionParameterSyntax FunctionParameterSyntax::
withTypeSyntax(llvm::Optional<TypeSyntax> NewType) const {
  if (!NewType.hasValue()) {
    auto RawType = RawSyntax::missing(SyntaxKind::MissingType);
    return Data->replaceChild<FunctionParameterSyntax>(RawType, Cursor::Type);
  }

  return Data->replaceChild<FunctionParameterSyntax>(
    NewType.getValue().getRaw(), Cursor::Type);
}

TokenSyntax FunctionParameterSyntax::getEqualToken() const {
  return { Root, Data->getChild(Cursor::DefaultEqual).get() };
}

FunctionParameterSyntax FunctionParameterSyntax::
withEqualToken(TokenSyntax NewEqualToken) const {
  assert(NewEqualToken.getTokenKind() == tok::equal);
  return Data->replaceChild<FunctionParameterSyntax>(NewEqualToken.getRaw(),
                                                     Cursor::DefaultEqual);
}

llvm::Optional<ExprSyntax> FunctionParameterSyntax::getDefaultValue() const {
  auto RawExpr = getRaw()->getChild(Cursor::DefaultExpression);
  if (RawExpr->isMissing()) {
    return llvm::None;
  }

  return ExprSyntax { Root, Data->getChild(Cursor::DefaultExpression).get() };
}

FunctionParameterSyntax FunctionParameterSyntax::
withDefaultValue(llvm::Optional<ExprSyntax> NewDefaultValue) const {
  if (!NewDefaultValue.hasValue()) {
    auto RawType = RawSyntax::missing(SyntaxKind::MissingExpr);
    return Data->replaceChild<FunctionParameterSyntax>(RawType,
      Cursor::DefaultExpression);
  }

  return Data->replaceChild<FunctionParameterSyntax>(
    NewDefaultValue.getValue().getRaw(), Cursor::DefaultExpression);
}

TokenSyntax FunctionParameterSyntax::getTrailingComma() const {
  return { Root, Data->getChild(Cursor::TrailingComma).get() };
}

FunctionParameterSyntax FunctionParameterSyntax::
withTrailingComma(TokenSyntax NewTrailingComma) const {
  syntax_assert_token_is(NewTrailingComma, tok::comma, ",");
  return Data->replaceChild<FunctionParameterSyntax>(NewTrailingComma.getRaw(),
                                                     Cursor::TrailingComma);
}

#pragma mark - function-signature Data

void FunctionSignatureSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Layout.size() == 7);
  syntax_assert_child_token_text(Raw,
                                 Cursor::LeftParen,
                                 tok::l_paren, "(");

  assert(Raw->getChild(Cursor::ParameterList)->Kind ==
         SyntaxKind::FunctionParameterList);

  syntax_assert_child_token_text(Raw,
                                 Cursor::RightParen,
                                 tok::r_paren, ")");
#ifndef NDEBUG
  auto ThrowsRethrows = cast<RawTokenSyntax>(
    Raw->getChild(Cursor::ThrowsOrRethrows));
  assert(ThrowsRethrows->getTokenKind() == tok::kw_throws ||
         ThrowsRethrows->getTokenKind() == tok::kw_rethrows);
#endif
  syntax_assert_child_token_text(Raw, Cursor::Arrow,
                                 tok::arrow, "->");
  syntax_assert_child_kind(Raw,
                           Cursor::ReturnTypeAttributes,
                           SyntaxKind::TypeAttributes);
  assert(Raw->getChild(Cursor::ReturnType)->isType());
}

FunctionSignatureSyntax FunctionSignatureSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionSignature,
  {
    RawTokenSyntax::missingToken(tok::l_paren, "("),
    RawSyntax::missing(SyntaxKind::FunctionParameterList),
    RawTokenSyntax::missingToken(tok::r_paren, ")"),
    RawTokenSyntax::missingToken(tok::kw_throws, "throws"),
    RawTokenSyntax::missingToken(tok::arrow, "->"),
    RawSyntax::missing(SyntaxKind::TypeAttributes),
    RawSyntax::missing(SyntaxKind::MissingType),
  },
  SourcePresence::Present);
  return make<FunctionSignatureSyntax>(Raw);
}

#pragma mark - function-signature API

TokenSyntax FunctionSignatureSyntax::getLeftParenToken() const {
  return { Root, Data->getChild(Cursor::LeftParen).get() };
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withLeftParenToken(TokenSyntax NewLeftParen) const {
  syntax_assert_token_is(NewLeftParen, tok::l_paren, "(");
  return Data->replaceChild<FunctionSignatureSyntax>(NewLeftParen.getRaw(),
                                                     Cursor::LeftParen);
}

FunctionParameterListSyntax FunctionSignatureSyntax::getParameterList() const {
  return FunctionParameterListSyntax {
    Root,
    Data->getChild(Cursor::ParameterList).get()
  };
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withParameterList(FunctionParameterListSyntax NewParameterList) const {
  return Data->replaceChild<FunctionSignatureSyntax>(NewParameterList.getRaw(),
                                                     Cursor::ParameterList);
}

TokenSyntax FunctionSignatureSyntax::getRightParenToken() const {
  return { Root, Data->getChild(Cursor::RightParen).get() };
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withRightParenToken(TokenSyntax NewRightParen) const {
  syntax_assert_token_is(NewRightParen, tok::r_paren, ")");
  return Data->replaceChild<FunctionSignatureSyntax>(NewRightParen.getRaw(),
                                                     Cursor::RightParen);
}

TokenSyntax FunctionSignatureSyntax::getThrowsToken() const {
  auto Throw = cast<RawTokenSyntax>(
    getRaw()->getChild(Cursor::ThrowsOrRethrows));
  if (Throw->isNot(tok::kw_throws)) {
    return TokenSyntax::missingToken(tok::kw_throws, "throws");
  }
  return { Root, Data->getChild(Cursor::ThrowsOrRethrows).get() };
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withThrowsToken(TokenSyntax NewThrowsToken) const {
  syntax_assert_token_is(NewThrowsToken, tok::kw_throws, "throws");
  return Data->replaceChild<FunctionSignatureSyntax>(NewThrowsToken.getRaw(),
                                                     Cursor::ThrowsOrRethrows);
}

TokenSyntax FunctionSignatureSyntax::getRethrowsToken() const {
  auto Rethrow = cast<RawTokenSyntax>(
    getRaw()->getChild(Cursor::ThrowsOrRethrows));
  if (Rethrow->isNot(tok::kw_rethrows)) {
    return TokenSyntax::missingToken(tok::kw_rethrows, "rethrows");
  }
  return { Root, Data->getChild(Cursor::ThrowsOrRethrows).get() };
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withRethrowsToken(TokenSyntax NewRethrowsToken) const {
  syntax_assert_token_is(NewRethrowsToken, tok::kw_rethrows, "rethrows");
  return Data->replaceChild<FunctionSignatureSyntax>(NewRethrowsToken.getRaw(),
                                                     Cursor::ThrowsOrRethrows);
}

TokenSyntax FunctionSignatureSyntax::getArrowToken() const {
  return { Root, Data->getChild(Cursor::Arrow).get() };
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withArrowToken(TokenSyntax NewArrowToken) const {
  syntax_assert_token_is(NewArrowToken, tok::arrow, "->");
  return Data->replaceChild<FunctionSignatureSyntax>(NewArrowToken.getRaw(),
                                                     Cursor::Arrow);
}

TypeAttributesSyntax FunctionSignatureSyntax::getReturnTypeAttributes() const {
  return TypeAttributesSyntax {
    Root,
    Data->getChild(Cursor::ReturnTypeAttributes).get()
  };
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withReturnTypeAttributes(TypeAttributesSyntax NewAttributes) const {
  return Data->replaceChild<FunctionSignatureSyntax>(NewAttributes.getRaw(),
    Cursor::ReturnTypeAttributes);
}

TypeSyntax FunctionSignatureSyntax::getReturnTypeSyntax() const {
  return TypeSyntax {
    Root,
    Data->getChild(Cursor::ReturnType).get()
  };
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withReturnTypeSyntax(TypeSyntax NewReturnTypeSyntax) const {
  return Data->replaceChild<FunctionSignatureSyntax>(
    NewReturnTypeSyntax.getRaw(), Cursor::ReturnType);
}

#pragma mark - function-declaration-data

void FunctionDeclSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::FunctionDecl);
  assert(Raw->Layout.size() == 8);
  syntax_assert_child_kind(Raw, FunctionDeclSyntax::Cursor::Attributes,
                           SyntaxKind::TypeAttributes);
  syntax_assert_child_kind(Raw, FunctionDeclSyntax::Cursor::Modifiers,
                           SyntaxKind::DeclModifierList);
  syntax_assert_child_token_text(Raw, FunctionDeclSyntax::Cursor::FuncKeyword,
                                 tok::kw_func, "func");
  syntax_assert_child_token(Raw, FunctionDeclSyntax::Cursor::Identifier,
                            tok::identifier);
  syntax_assert_child_kind(Raw,
                           FunctionDeclSyntax::Cursor::GenericParameterClause,
                           SyntaxKind::GenericParameterClause);
  syntax_assert_child_kind(Raw, FunctionDeclSyntax::Cursor::Signature,
                           SyntaxKind::FunctionSignature);
  syntax_assert_child_kind(Raw, FunctionDeclSyntax::Cursor::GenericWhereClause,
                           SyntaxKind::GenericWhereClause);
  syntax_assert_child_kind(Raw, FunctionDeclSyntax::Cursor::Body,
                           SyntaxKind::CodeBlockStmt);
}

FunctionDeclSyntax FunctionDeclSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionDecl,
    {
      RawSyntax::missing(SyntaxKind::TypeAttributes),
      RawSyntax::missing(SyntaxKind::DeclModifierList),
      RawTokenSyntax::missingToken(tok::kw_func, "func"),
      RawTokenSyntax::missingToken(tok::identifier, ""),
      RawSyntax::missing(SyntaxKind::GenericParameterClause),
      RawSyntax::missing(SyntaxKind::FunctionSignature),
      RawSyntax::missing(SyntaxKind::GenericWhereClause),
      RawSyntax::missing(SyntaxKind::CodeBlockStmt),
    },
    SourcePresence::Present);
  return make<FunctionDeclSyntax>(Raw);
}

#pragma mark - function-declaration-API

TypeAttributesSyntax FunctionDeclSyntax::getAttributes() const {
  return { Root, Data->getChild(Cursor::Attributes).get() };
}

FunctionDeclSyntax
FunctionDeclSyntax::withAttributes(TypeAttributesSyntax NewAttributes) const {
  return Data->replaceChild<FunctionDeclSyntax>(NewAttributes.getRaw(),
                                                Cursor::Attributes);
}

DeclModifierListSyntax FunctionDeclSyntax::getModifiers() const {
  return { Root, Data->getChild(Cursor::Modifiers).get() };
}

FunctionDeclSyntax
FunctionDeclSyntax::withModifiers(DeclModifierListSyntax NewModifiers) const {
  return Data->replaceChild<FunctionDeclSyntax>(NewModifiers.getRaw(),
                                                Cursor::Modifiers);
}

TokenSyntax FunctionDeclSyntax::getFuncKeyword() const {
  return { Root, Data->getChild(Cursor::FuncKeyword).get() };
}

FunctionDeclSyntax
FunctionDeclSyntax::withFuncKeyword(TokenSyntax NewFuncKeyword) const {
  syntax_assert_token_is(NewFuncKeyword, tok::kw_func, "func");
  return Data->replaceChild<FunctionDeclSyntax>(NewFuncKeyword.getRaw(),
                                                Cursor::FuncKeyword);
}

TokenSyntax FunctionDeclSyntax::getIdentifier() const {
  return { Root, Data->getChild(Cursor::Identifier).get() };
}

FunctionDeclSyntax
FunctionDeclSyntax::withIdentifier(TokenSyntax NewIdentifier) const {
  assert(NewIdentifier.getTokenKind() == tok::identifier);
  return Data->replaceChild<FunctionDeclSyntax>(NewIdentifier.getRaw(),
                                                Cursor::Identifier);
}

llvm::Optional<GenericParameterClauseSyntax>
FunctionDeclSyntax::getGenericParameterClause() const {
  auto RawGenericParams = getRaw()->getChild(Cursor::GenericParameterClause);
  if (RawGenericParams->isMissing()) {
    return llvm::None;
  }
  return llvm::Optional<GenericParameterClauseSyntax> {
    GenericParameterClauseSyntax {
      Root,
      Data->getChild(Cursor::GenericParameterClause).get()
    }
  };
}

FunctionDeclSyntax FunctionDeclSyntax::withGenericParameterClause(
    llvm::Optional<GenericParameterClauseSyntax> NewGenericParams) const {
  auto RawParams = NewGenericParams.hasValue()
    ? NewGenericParams->getRaw()
    : SyntaxFactory::makeBlankGenericParameterClause().getRaw();
  return Data->replaceChild<FunctionDeclSyntax>(RawParams,
                                                Cursor::GenericParameterClause);

}

FunctionSignatureSyntax FunctionDeclSyntax::getSignature() const {
  return { Root, Data->getChild(Cursor::Signature).get() };
}

FunctionDeclSyntax
FunctionDeclSyntax::withSignature(FunctionSignatureSyntax NewSignature) const {
  return Data->replaceChild<FunctionDeclSyntax>(NewSignature.getRaw(),
                                                Cursor::Signature);
}

llvm::Optional<CodeBlockStmtSyntax> FunctionDeclSyntax::getBody() const {
  auto RawBody = getRaw()->getChild(Cursor::Body);
  if (RawBody->isMissing()) {
    return llvm::None;
  }
  
  return llvm::Optional<CodeBlockStmtSyntax> {
    CodeBlockStmtSyntax { Root, Data->getChild(Cursor::Body).get() }
  };
}

FunctionDeclSyntax FunctionDeclSyntax::
withBody(llvm::Optional<CodeBlockStmtSyntax> NewBody) const {
  auto RawBody = NewBody.hasValue()
    ? NewBody->getRaw()
    : SyntaxFactory::makeBlankCodeBlock().getRaw();
  return Data->replaceChild<FunctionDeclSyntax>(RawBody,
                                                Cursor::Body);
}
