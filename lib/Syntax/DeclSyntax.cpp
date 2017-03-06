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

#pragma mark - declaration Data

DeclSyntaxData::DeclSyntaxData(RC<RawSyntax> Raw,
                               const SyntaxData *Parent,
                               CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->isDecl());
}

#pragma mark - declaration API

DeclSyntax::DeclSyntax(const RC<SyntaxData> Root, const DeclSyntaxData *Data)
  : Syntax(Root, Data) {}

#pragma mark - declaration-modifier Data

DeclModifierSyntaxData::
DeclModifierSyntaxData(const RC<RawSyntax> Raw,
                       const SyntaxData *Parent,
                       const CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::DeclModifier);
  assert(Raw->Layout.size() == 4);
#ifndef NDEBUG
  auto Name =
    cast<TokenSyntax>(Raw->getChild(DeclModifierSyntax::Cursor::Name));
  auto Kind = Name->getTokenKind();
  assert(Kind == tok::kw_class ||
         Kind == tok::kw_static ||
         Kind == tok::identifier ||
         Kind == tok::kw_public ||
         Kind == tok::kw_private ||
         Kind == tok::kw_fileprivate ||
         Kind == tok::kw_internal);
#endif
  syntax_assert_child_token_text(Raw, DeclModifierSyntax::Cursor::LeftParen,
                                 tok::l_paren, "(");
  syntax_assert_child_token(Raw, DeclModifierSyntax::Cursor::Argument,
                           tok::identifier);
  syntax_assert_child_token_text(Raw, DeclModifierSyntax::Cursor::RightParen,
                                 tok::r_paren, ")");
}

RC<DeclModifierSyntaxData>
DeclModifierSyntaxData::make(const RC<RawSyntax> Raw,
                             const SyntaxData *Parent,
                             const CursorIndex IndexInParent) {
  return RC<DeclModifierSyntaxData>{
    new DeclModifierSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<DeclModifierSyntaxData> DeclModifierSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::DeclModifier,
                             {
                               TokenSyntax::missingToken(tok::identifier, ""),
                               TokenSyntax::missingToken(tok::l_paren, "("),
                               TokenSyntax::missingToken(tok::identifier, ""),
                               TokenSyntax::missingToken(tok::r_paren, ")"),
                             },
                             SourcePresence::Present);
  return make(Raw);
}

#pragma mark - declaration-modifier API

RC<TokenSyntax> DeclModifierSyntax::getName() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::Name));
}

DeclModifierSyntax DeclModifierSyntax::withName(RC<TokenSyntax> NewName) const {
  assert(NewName->getTokenKind() == tok::identifier);
  return Data->replaceChild<DeclModifierSyntax>(NewName, Cursor::Name);
}

RC<TokenSyntax> DeclModifierSyntax::getLeftParenToken() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::LeftParen));
}

DeclModifierSyntax
DeclModifierSyntax::withLeftParenToken(RC<TokenSyntax> NewLeftParen) const {
  syntax_assert_token_is(NewLeftParen, tok::l_paren, "(");
  return Data->replaceChild<DeclModifierSyntax>(NewLeftParen,
                                                Cursor::LeftParen);
}

RC<TokenSyntax> DeclModifierSyntax::getArgument() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::Argument));
}

DeclModifierSyntax
DeclModifierSyntax::withArgument(RC<TokenSyntax> NewArgument) const {
  assert(NewArgument->getTokenKind() == tok::identifier);
  return Data->replaceChild<DeclModifierSyntax>(NewArgument, Cursor::Argument);
}

RC<TokenSyntax> DeclModifierSyntax::getRightParenToken() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::RightParen));
}

DeclModifierSyntax
DeclModifierSyntax::withRightParenToken(RC<TokenSyntax> NewRightParen) const {
  syntax_assert_token_is(NewRightParen, tok::r_paren, ")");
  return Data->replaceChild<DeclModifierSyntax>(NewRightParen,
                                                Cursor::RightParen);
}

#pragma mark - unknown-statement Data

UnknownDeclSyntaxData::UnknownDeclSyntaxData(RC<RawSyntax> Raw,
                                             const SyntaxData *Parent,
                                             CursorIndex IndexInParent)
  : UnknownSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::UnknownDecl);
}

RC<UnknownDeclSyntaxData>
UnknownDeclSyntaxData::make(RC<RawSyntax> Raw,
                            const SyntaxData *Parent,
                            CursorIndex IndexInParent) {
  auto UnknownRaw = RawSyntax::make(SyntaxKind::UnknownDecl, Raw->Layout,
                                    Raw->Presence);
  return RC<UnknownDeclSyntaxData> {
    new UnknownDeclSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

#pragma mark - unknown-statement API

UnknownDeclSyntax::UnknownDeclSyntax(const RC<SyntaxData> Root,
                                     const UnknownDeclSyntaxData *Data)
  : UnknownSyntax(Root, Data) {}

#pragma mark - declaration-members Data

DeclMembersSyntaxData::DeclMembersSyntaxData(RC<RawSyntax> Raw,
                                             const SyntaxData *Parent,
                                             CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::DeclMembers);
}

RC<DeclMembersSyntaxData>
DeclMembersSyntaxData::make(RC<RawSyntax> Raw,
                            const SyntaxData *Parent,
                            CursorIndex IndexInParent) {
  return RC<DeclMembersSyntaxData> {
    new DeclMembersSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<DeclMembersSyntaxData> DeclMembersSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::DeclMembers, {},
                              SourcePresence::Present));
}

#pragma mark - declaration-members API

DeclMembersSyntax::DeclMembersSyntax(const RC<SyntaxData> Root,
                                     const DeclMembersSyntaxData *Data)
  : Syntax(Root, Data) {}

#pragma mark - declaration-members Builder

DeclMembersSyntaxBuilder &
DeclMembersSyntaxBuilder::addMember(DeclSyntax Member) {
  MembersLayout.push_back(Member.getRaw());
  return *this;
}

DeclMembersSyntax DeclMembersSyntaxBuilder::build() const {
  auto Raw = RawSyntax::make(SyntaxKind::DeclMembers, MembersLayout,
                             SourcePresence::Present);
  auto Data = DeclMembersSyntaxData::make(Raw);
  return DeclMembersSyntax { Data, Data.get() };
}

#pragma mark - struct-declaration Data

StructDeclSyntaxData::StructDeclSyntaxData(RC<RawSyntax> Raw,
                                           const SyntaxData *Parent,
                                           CursorIndex IndexInParent)
  : DeclSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::StructDecl);
  syntax_assert_child_token_text(Raw, StructDeclSyntax::Cursor::StructKeyword,
                                 tok::kw_struct, "struct");
  syntax_assert_child_token(Raw, StructDeclSyntax::Cursor::Identifier,
                            tok::identifier);
  syntax_assert_child_kind(Raw,
                           StructDeclSyntax::Cursor::GenericParameterClause,
                           SyntaxKind::GenericParameterClause);
  syntax_assert_child_kind(Raw, StructDeclSyntax::Cursor::GenericWhereClause,
                           SyntaxKind::GenericWhereClause);
  syntax_assert_child_token_text(Raw, StructDeclSyntax::Cursor::LeftBrace,
                                 tok::l_brace, "{");
  syntax_assert_child_kind(Raw, StructDeclSyntax::Cursor::Members,
                           SyntaxKind::DeclMembers);
  syntax_assert_child_token_text(Raw, StructDeclSyntax::Cursor::RightBrace,
                                 tok::r_brace, "}");
}

RC<StructDeclSyntaxData> StructDeclSyntaxData::make(RC<RawSyntax> Raw,
                                                    const SyntaxData *Parent,
                                                    CursorIndex IndexInParent) {
  return RC<StructDeclSyntaxData> {
    new StructDeclSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<StructDeclSyntaxData> StructDeclSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::StructDecl,
    {
      TokenSyntax::missingToken(tok::kw_struct, "struct"),
      TokenSyntax::missingToken(tok::identifier, ""),
      RawSyntax::missing(SyntaxKind::GenericParameterClause),
      RawSyntax::missing(SyntaxKind::GenericWhereClause),
      TokenSyntax::missingToken(tok::l_brace, "{"),
      RawSyntax::missing(SyntaxKind::DeclMembers),
      TokenSyntax::missingToken(tok::r_brace, "}"),
    },
    SourcePresence::Present));
}

#pragma mark - struct-declaration API

StructDeclSyntax::StructDeclSyntax(const RC<SyntaxData> Root,
                                   const StructDeclSyntaxData *Data)
  : DeclSyntax(Root, Data) {}

RC<TokenSyntax> StructDeclSyntax::getStructKeyword() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::StructKeyword));
}

StructDeclSyntax
StructDeclSyntax::withStructKeyword(RC<TokenSyntax> NewStructKeyword)
const {
  syntax_assert_token_is(NewStructKeyword, tok::kw_struct, "struct");
  return Data->replaceChild<StructDeclSyntax>(NewStructKeyword,
                                              Cursor::StructKeyword);
}

StructDeclSyntax
StructDeclSyntax::withLeftBrace(RC<TokenSyntax> NewLeftBrace) const {
  syntax_assert_token_is(NewLeftBrace, tok::l_brace, "{");
  return Data->replaceChild<StructDeclSyntax>(NewLeftBrace,
                                              Cursor::LeftBrace);
}

RC<TokenSyntax> StructDeclSyntax::getLeftBraceToken() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::LeftBrace));
}

StructDeclSyntax
StructDeclSyntax::withMembers(DeclMembersSyntax NewMembers) const {
  return Data->replaceChild<StructDeclSyntax>(NewMembers.getRaw(),
                                              Cursor::Members);
}

DeclMembersSyntax StructDeclSyntax::getMembers() const {
  auto Raw = getRaw()->getChild(Cursor::Members);
  auto MembersData = DeclMembersSyntaxData::make(Raw,
                                                 Data,
                                                 cursorIndex(Cursor::Members));
  const_cast<StructDeclSyntaxData *>(getData())->CachedMembers = MembersData;
  return DeclMembersSyntax { Root, MembersData.get() };
}

#pragma mark - struct-declaration Builder

StructDeclSyntaxBuilder::StructDeclSyntaxBuilder()
  : StructLayout(SyntaxFactory::makeBlankStructDecl().getRaw()->Layout) {}

StructDeclSyntaxBuilder &
StructDeclSyntaxBuilder::useStructKeyword(RC<TokenSyntax> StructKeyword) {
  syntax_assert_token_is(StructKeyword, tok::kw_struct, "struct");
  auto Index = cursorIndex(StructDeclSyntax::Cursor::StructKeyword);
  StructLayout[Index] = StructKeyword;
  return *this;
}

StructDeclSyntaxBuilder &
StructDeclSyntaxBuilder::useIdentifier(RC<TokenSyntax> Identifier) {
  assert(Identifier->getTokenKind() == tok::identifier);
  auto Index = cursorIndex(StructDeclSyntax::Cursor::Identifier);
  StructLayout[Index] = Identifier;
  return *this;
}

StructDeclSyntaxBuilder &
StructDeclSyntaxBuilder::useLeftBrace(RC<TokenSyntax> LeftBrace) {
  syntax_assert_token_is(LeftBrace, tok::l_brace, "{");
  auto Index = cursorIndex(StructDeclSyntax::Cursor::LeftBrace);
  StructLayout[Index] = LeftBrace;
  return *this;
}

StructDeclSyntaxBuilder &
StructDeclSyntaxBuilder::useMembers(DeclMembersSyntax Members) {
  auto Index = cursorIndex(StructDeclSyntax::Cursor::Members);
  StructLayout[Index] = Members.getRaw();
  return *this;
}

StructDeclSyntaxBuilder &
StructDeclSyntaxBuilder::useRightBrace(RC<TokenSyntax> RightBrace) {
  syntax_assert_token_is(RightBrace, tok::r_brace, "}");
  auto Index = cursorIndex(StructDeclSyntax::Cursor::RightBrace);
  StructLayout[Index] = RightBrace;
  return *this;
}

StructDeclSyntax StructDeclSyntaxBuilder::build() const {
  auto Raw = RawSyntax::make(SyntaxKind::StructDecl, StructLayout,
                             SourcePresence::Present);
  auto Data = StructDeclSyntaxData::make(Raw);
  return StructDeclSyntax { Data, Data.get() };
}

#pragma mark - type-alias Data

TypeAliasDeclSyntaxData::TypeAliasDeclSyntaxData(RC<RawSyntax> Raw,
                                                 const SyntaxData *Parent,
                                                 CursorIndex IndexInParent)
  : DeclSyntaxData(Raw, Parent, IndexInParent) {}

RC<TypeAliasDeclSyntaxData>
TypeAliasDeclSyntaxData::make(RC<RawSyntax> Raw,
                              const SyntaxData *Parent,
                              CursorIndex IndexInParent) {
  return RC<TypeAliasDeclSyntaxData> {
    new TypeAliasDeclSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<TypeAliasDeclSyntaxData>
TypeAliasDeclSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::TypeAliasDecl,
    {
      TokenSyntax::missingToken(tok::kw_typealias, "typealias"),
      TokenSyntax::missingToken(tok::identifier, ""),
      RawSyntax::missing(SyntaxKind::GenericParameterClause),
      TokenSyntax::missingToken(tok::equal, "="),
      RawSyntax::missing(SyntaxKind::MissingType),
    },
    SourcePresence::Present));
}

#pragma mark - type-alias API

TypeAliasDeclSyntax::TypeAliasDeclSyntax(RC<SyntaxData> Root,
                                         const TypeAliasDeclSyntaxData *Data)
  : DeclSyntax(Root, Data) {}

TypeAliasDeclSyntax TypeAliasDeclSyntax::
withTypeAliasKeyword(RC<TokenSyntax> NewTypeAliasKeyword) const {
  syntax_assert_token_is(NewTypeAliasKeyword, tok::kw_typealias, "typealias");
  return Data->replaceChild<TypeAliasDeclSyntax>(NewTypeAliasKeyword,
                                                 Cursor::TypeAliasKeyword);
}

TypeAliasDeclSyntax
TypeAliasDeclSyntax::withIdentifier(RC<TokenSyntax> NewIdentifier) const {
  assert(NewIdentifier->getTokenKind() == tok::identifier);
  return Data->replaceChild<TypeAliasDeclSyntax>(NewIdentifier,
                                                 Cursor::Identifier);
}

TypeAliasDeclSyntax TypeAliasDeclSyntax::
withGenericParameterClause(GenericParameterClauseSyntax NewGenericParams)
const {
  return Data->replaceChild<TypeAliasDeclSyntax>(NewGenericParams.getRaw(),
    Cursor::GenericParameterClause);
}

TypeAliasDeclSyntax
TypeAliasDeclSyntax::withEqualToken(RC<TokenSyntax> NewEqualToken) const {
  syntax_assert_token_is(NewEqualToken, tok::equal, "=");
  return Data->replaceChild<TypeAliasDeclSyntax>(NewEqualToken,
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
useTypeAliasKeyword(RC<TokenSyntax> TypeAliasKeyword) {
  syntax_assert_token_is(TypeAliasKeyword, tok::kw_typealias, "typealias");
  auto Index = cursorIndex(TypeAliasDeclSyntax::Cursor::TypeAliasKeyword);
  TypeAliasLayout[Index] = TypeAliasKeyword;
  return *this;
}

TypeAliasDeclSyntaxBuilder &
TypeAliasDeclSyntaxBuilder::useIdentifier(RC<TokenSyntax> Identifier) {
  assert(Identifier->getTokenKind() == tok::identifier);
  auto Index = cursorIndex(TypeAliasDeclSyntax::Cursor::Identifier);
  TypeAliasLayout[Index] = Identifier;
  return *this;
}

TypeAliasDeclSyntaxBuilder &TypeAliasDeclSyntaxBuilder::
useGenericParameterClause(GenericParameterClauseSyntax GenericParams) {
  auto Index = cursorIndex(TypeAliasDeclSyntax::Cursor::GenericParameterClause);
  TypeAliasLayout[Index] = GenericParams.getRaw();
  return *this;
}

TypeAliasDeclSyntaxBuilder &
TypeAliasDeclSyntaxBuilder::useEqualToken(RC<TokenSyntax> EqualToken) {
  auto Index = cursorIndex(TypeAliasDeclSyntax::Cursor::EqualToken);
  TypeAliasLayout[Index] = EqualToken;
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
  auto Data = TypeAliasDeclSyntaxData::make(Raw);
  return { Data, Data.get() };
}

#pragma mark - function-parameter Data

FunctionParameterSyntaxData::FunctionParameterSyntaxData(RC<RawSyntax> Raw,
                            const SyntaxData *Parent,
                            CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Layout.size() == 8);
  syntax_assert_child_token(Raw, FunctionParameterSyntax::Cursor::ExternalName,
                            tok::identifier);
  syntax_assert_child_token(Raw, FunctionParameterSyntax::Cursor::LocalName,
                            tok::identifier);
  syntax_assert_child_token_text(Raw, FunctionParameterSyntax::Cursor::Colon,
                                 tok::colon, ":");
  assert(Raw->getChild(FunctionParameterSyntax::Cursor::Type)->isType());
  syntax_assert_child_token_text(Raw, FunctionParameterSyntax::Cursor::Ellipsis,
                                 tok::identifier, "...");
  syntax_assert_child_token_text(Raw,
                                 FunctionParameterSyntax::Cursor::DefaultEqual,
                                 tok::equal, "=");
  assert(Raw->getChild(
    FunctionParameterSyntax::Cursor::DefaultExpression)->isExpr());
  syntax_assert_child_token_text(Raw,
                                 FunctionParameterSyntax::Cursor::TrailingComma,
                                 tok::comma, ",");
}

RC<FunctionParameterSyntaxData>
FunctionParameterSyntaxData::make(RC<RawSyntax> Raw, const SyntaxData *Parent,
     CursorIndex IndexInParent) {
  return RC<FunctionParameterSyntaxData> {
    new FunctionParameterSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<FunctionParameterSyntaxData> FunctionParameterSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionParameter,
  {
    TokenSyntax::missingToken(tok::identifier, ""),
    TokenSyntax::missingToken(tok::identifier, ""),
    TokenSyntax::missingToken(tok::colon, ":"),
    RawSyntax::missing(SyntaxKind::MissingType),
    TokenSyntax::missingToken(tok::identifier, "..."),
    TokenSyntax::missingToken(tok::equal, "="),
    RawSyntax::missing(SyntaxKind::MissingExpr),
    TokenSyntax::missingToken(tok::comma, ","),
  },
  SourcePresence::Present);
  return make(Raw);
}


#pragma mark - function-parameter API

RC<TokenSyntax> FunctionParameterSyntax::getExternalName() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::ExternalName));
}

FunctionParameterSyntax FunctionParameterSyntax::
withExternalName(RC<TokenSyntax> NewExternalName) const {
  assert(NewExternalName->getTokenKind() == tok::identifier);
  return Data->replaceChild<FunctionParameterSyntax>(NewExternalName,
                                                     Cursor::ExternalName);
}

RC<TokenSyntax> FunctionParameterSyntax::getLocalName() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::LocalName));
}

FunctionParameterSyntax FunctionParameterSyntax::
withLocalName(RC<TokenSyntax> NewLocalName) const {
  assert(NewLocalName->getTokenKind() == tok::identifier);
  return Data->replaceChild<FunctionParameterSyntax>(NewLocalName,
                                                     Cursor::LocalName);
}

RC<TokenSyntax> FunctionParameterSyntax::getColonToken() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::Colon));
}

FunctionParameterSyntax FunctionParameterSyntax::
withColonToken(RC<TokenSyntax> NewColonToken) const {
  syntax_assert_token_is(NewColonToken, tok::colon, ":");
  return Data->replaceChild<FunctionParameterSyntax>(NewColonToken,
                                                     Cursor::Colon);
}

llvm::Optional<TypeSyntax> FunctionParameterSyntax::getTypeSyntax() const {
  auto RawType = getRaw()->getChild(Cursor::Type);
  if (RawType->isMissing()) {
    return llvm::None;
  }

  auto *MyData = getUnsafeData<FunctionParameterSyntax>();

  if (MyData->CachedTypeSyntax) {
    return TypeSyntax { Root, MyData->CachedTypeSyntax.get() };
  }

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedTypeSyntax);

  SyntaxData::realizeSyntaxNode<TypeSyntax>(ChildPtr, RawType, MyData,
                                            cursorIndex(Cursor::Type));

  return TypeSyntax { Root, MyData->CachedTypeSyntax.get() };
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

RC<TokenSyntax> FunctionParameterSyntax::getEqualToken() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::DefaultEqual));
}

FunctionParameterSyntax FunctionParameterSyntax::
withEqualToken(RC<TokenSyntax> NewEqualToken) const {
  assert(NewEqualToken->getTokenKind() == tok::equal);
  return Data->replaceChild<FunctionParameterSyntax>(NewEqualToken,
                                                     Cursor::DefaultEqual);
}

llvm::Optional<ExprSyntax> FunctionParameterSyntax::getDefaultValue() const {
    auto RawExpr = getRaw()->getChild(Cursor::DefaultExpression);
    if (RawExpr->isMissing()) {
      return llvm::None;
    }

    auto *MyData = getUnsafeData<FunctionParameterSyntax>();

    if (MyData->CachedTypeSyntax) {
      return ExprSyntax { Root, MyData->CachedDefaultValue.get() };
    }

    auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
      &MyData->CachedDefaultValue);

    SyntaxData::realizeSyntaxNode<TypeSyntax>(ChildPtr, RawExpr, MyData,
      cursorIndex(Cursor::DefaultExpression));
    
    return ExprSyntax { Root, MyData->CachedDefaultValue.get() };
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

RC<TokenSyntax> FunctionParameterSyntax::getTrailingComma() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::TrailingComma));
}

FunctionParameterSyntax FunctionParameterSyntax::
withTrailingComma(RC<TokenSyntax> NewTrailingComma) const {
  syntax_assert_token_is(NewTrailingComma, tok::comma, ",");
  return Data->replaceChild<FunctionParameterSyntax>(NewTrailingComma,
                                                     Cursor::TrailingComma);
}

#pragma mark - function-signature Data

FunctionSignatureSyntaxData::
FunctionSignatureSyntaxData(const RC<RawSyntax> Raw,
                            const SyntaxData *Parent,
                            const CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Layout.size() == 7);
  syntax_assert_child_token_text(Raw,
                                 FunctionSignatureSyntax::Cursor::LeftParen,
                                 tok::l_paren, "(");

  assert(Raw->getChild(FunctionSignatureSyntax::Cursor::ParameterList)->Kind ==
         SyntaxKind::FunctionParameterList);

  syntax_assert_child_token_text(Raw,
                                 FunctionSignatureSyntax::Cursor::RightParen,
                                 tok::r_paren, ")");
#ifndef NDEBUG
  auto ThrowsRethrows = cast<TokenSyntax>(
    Raw->getChild(FunctionSignatureSyntax::Cursor::ThrowsOrRethrows));
  assert(cast<TokenSyntax>(ThrowsRethrows)->getTokenKind() == tok::kw_throws ||
         cast<TokenSyntax>(ThrowsRethrows)->getTokenKind() == tok::kw_rethrows);
#endif
  syntax_assert_child_token_text(Raw, FunctionSignatureSyntax::Cursor::Arrow,
                                 tok::arrow, "->");
  syntax_assert_child_kind(Raw,
    FunctionSignatureSyntax::Cursor::ReturnTypeAttributes,
    SyntaxKind::TypeAttributes);
  assert(Raw->getChild(FunctionSignatureSyntax::Cursor::ReturnType)->isType());
}

RC<FunctionSignatureSyntaxData>
FunctionSignatureSyntaxData::make(RC<RawSyntax> Raw, const SyntaxData *Parent,
                                  CursorIndex IndexInParent) {
  return RC<FunctionSignatureSyntaxData> {
    new FunctionSignatureSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<FunctionSignatureSyntaxData> FunctionSignatureSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionSignature,
  {
    TokenSyntax::missingToken(tok::l_paren, "("),
    RawSyntax::missing(SyntaxKind::FunctionParameterList),
    TokenSyntax::missingToken(tok::r_paren, ")"),
    TokenSyntax::missingToken(tok::kw_throws, "throws"),
    TokenSyntax::missingToken(tok::arrow, "->"),
    RawSyntax::missing(SyntaxKind::TypeAttributes),
    RawSyntax::missing(SyntaxKind::MissingType),
  },
  SourcePresence::Present);
  return make(Raw);
}

#pragma mark - function-signature API

RC<TokenSyntax> FunctionSignatureSyntax::getLeftParenToken() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::LeftParen));
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withLeftParenToken(RC<TokenSyntax> NewLeftParen) const {
  syntax_assert_token_is(NewLeftParen, tok::l_paren, "(");
  return Data->replaceChild<FunctionSignatureSyntax>(NewLeftParen,
                                                     Cursor::LeftParen);
}

FunctionParameterListSyntax FunctionSignatureSyntax::getParameterList() const {
  auto RawList = getRaw()->getChild(Cursor::ParameterList);

  auto *MyData = getUnsafeData<FunctionSignatureSyntax>();

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedParameterList);

  SyntaxData::realizeSyntaxNode<FunctionParameterListSyntax>(ChildPtr, RawList,
    MyData, cursorIndex(Cursor::ParameterList));
  
  return FunctionParameterListSyntax {
    Root,
    MyData->CachedParameterList.get()
  };
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withParameterList(FunctionParameterListSyntax NewParameterList) const {
  return Data->replaceChild<FunctionSignatureSyntax>(NewParameterList.getRaw(),
                                                     Cursor::ParameterList);
}

RC<TokenSyntax> FunctionSignatureSyntax::getRightParenToken() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::RightParen));
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withRightParenToken(RC<TokenSyntax> NewRightParen) const {
  syntax_assert_token_is(NewRightParen, tok::r_paren, ")");
  return Data->replaceChild<FunctionSignatureSyntax>(NewRightParen,
                                                     Cursor::RightParen);
}

RC<TokenSyntax> FunctionSignatureSyntax::getThrowsToken() const {
  auto Throw = cast<TokenSyntax>(getRaw()->getChild(Cursor::ThrowsOrRethrows));
  if (Throw->getTokenKind() != tok::kw_throws) {
    return TokenSyntax::missingToken(tok::kw_throws, "throws");
  }
  return Throw;
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withThrowsToken(RC<TokenSyntax> NewThrowsToken) const {
  syntax_assert_token_is(NewThrowsToken, tok::kw_throws, "throws");
  return Data->replaceChild<FunctionSignatureSyntax>(NewThrowsToken,
                                                     Cursor::ThrowsOrRethrows);
}

RC<TokenSyntax> FunctionSignatureSyntax::getRethrowsToken() const {
  auto Rethrow = cast<TokenSyntax>(
    getRaw()->getChild(Cursor::ThrowsOrRethrows));
  if (Rethrow->getTokenKind() != tok::kw_rethrows) {
    return TokenSyntax::missingToken(tok::kw_rethrows, "rethrows");
  }
  return Rethrow;
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withRethrowsToken(RC<TokenSyntax> NewRethrowsToken) const {
  syntax_assert_token_is(NewRethrowsToken, tok::kw_rethrows, "rethrows");
  return Data->replaceChild<FunctionSignatureSyntax>(NewRethrowsToken,
                                                     Cursor::ThrowsOrRethrows);
}

RC<TokenSyntax> FunctionSignatureSyntax::getArrowToken() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::Arrow));
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withArrowToken(RC<TokenSyntax> NewArrowToken) const {
  syntax_assert_token_is(NewArrowToken, tok::arrow, "->");
  return Data->replaceChild<FunctionSignatureSyntax>(NewArrowToken,
                                                     Cursor::Arrow);
}

TypeAttributesSyntax FunctionSignatureSyntax::getReturnTypeAttributes() const {
  auto RawAttrs = getRaw()->getChild(Cursor::ReturnTypeAttributes);

  auto *MyData = getUnsafeData<FunctionSignatureSyntax>();

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedReturnTypeAttributes);

  SyntaxData::realizeSyntaxNode<TypeAttributesSyntax>(ChildPtr, RawAttrs,
    MyData, cursorIndex(Cursor::ReturnTypeAttributes));

  return TypeAttributesSyntax {
    Root,
    MyData->CachedReturnTypeAttributes.get()
  };
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withReturnTypeAttributes(TypeAttributesSyntax NewAttributes) const {
  return Data->replaceChild<FunctionSignatureSyntax>(NewAttributes.getRaw(),
    Cursor::ReturnTypeAttributes);
}

TypeSyntax FunctionSignatureSyntax::getReturnTypeSyntax() const {
  auto RawType = getRaw()->getChild(Cursor::ReturnType);

  auto *MyData = getUnsafeData<FunctionSignatureSyntax>();

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedReturnTypeSyntax);

  SyntaxData::realizeSyntaxNode<TypeSyntax>(ChildPtr, RawType,
    MyData, cursorIndex(Cursor::ReturnType));

  return TypeSyntax {
    Root,
    MyData->CachedReturnTypeSyntax.get()
  };
}

FunctionSignatureSyntax FunctionSignatureSyntax::
withReturnTypeSyntax(TypeSyntax NewReturnTypeSyntax) const {
  return Data->replaceChild<FunctionSignatureSyntax>(
    NewReturnTypeSyntax.getRaw(), Cursor::ReturnType);
}

#pragma mark - function-declaration-data

FunctionDeclSyntaxData::
FunctionDeclSyntaxData(const RC<RawSyntax> Raw,
                       const SyntaxData *Parent,
                       const CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
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

RC<FunctionDeclSyntaxData> FunctionDeclSyntaxData::make(const RC<RawSyntax> Raw,
                                       const SyntaxData *Parent,
                                       const CursorIndex IndexInParent) {
  return RC<FunctionDeclSyntaxData> {
    new FunctionDeclSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<FunctionDeclSyntaxData> FunctionDeclSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionDecl,
    {
      RawSyntax::missing(SyntaxKind::TypeAttributes),
      RawSyntax::missing(SyntaxKind::DeclModifierList),
      TokenSyntax::missingToken(tok::kw_func, "func"),
      TokenSyntax::missingToken(tok::identifier, ""),
      RawSyntax::missing(SyntaxKind::GenericParameterClause),
      RawSyntax::missing(SyntaxKind::FunctionSignature),
      RawSyntax::missing(SyntaxKind::GenericWhereClause),
      RawSyntax::missing(SyntaxKind::CodeBlockStmt),
    },
    SourcePresence::Present);
  return make(Raw);
}

#pragma mark - function-declaration-API

TypeAttributesSyntax FunctionDeclSyntax::getAttributes() const {
  auto RawAttrs = getRaw()->getChild(Cursor::Attributes);

  auto *MyData = getUnsafeData<FunctionDeclSyntax>();

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedAttributes);

  SyntaxData::realizeSyntaxNode<ExprSyntax>(ChildPtr, RawAttrs, MyData,
                                            cursorIndex(Cursor::Attributes));
  
  return { Root, MyData->CachedAttributes.get() };
}

FunctionDeclSyntax
FunctionDeclSyntax::withAttributes(TypeAttributesSyntax NewAttributes) const {
  return Data->replaceChild<FunctionDeclSyntax>(NewAttributes.getRaw(),
                                                Cursor::Attributes);
}

DeclModifierListSyntax FunctionDeclSyntax::getModifiers() const {
  auto RawModifiers = getRaw()->getChild(Cursor::Attributes);

  auto *MyData = getUnsafeData<FunctionDeclSyntax>();

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedModifiers);

  SyntaxData::realizeSyntaxNode<DeclModifierListSyntax>(ChildPtr, RawModifiers,
    MyData, cursorIndex(Cursor::Modifiers));
  
  return { Root, MyData->CachedModifiers.get() };
}

FunctionDeclSyntax
FunctionDeclSyntax::withModifiers(DeclModifierListSyntax NewModifiers) const {
  return Data->replaceChild<FunctionDeclSyntax>(NewModifiers.getRaw(),
                                                Cursor::Modifiers);
}

RC<TokenSyntax> FunctionDeclSyntax::getFuncKeyword() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::FuncKeyword));
}

FunctionDeclSyntax
FunctionDeclSyntax::withFuncKeyword(RC<TokenSyntax> NewFuncKeyword) const {
  syntax_assert_token_is(NewFuncKeyword, tok::kw_func, "func");
  return Data->replaceChild<FunctionDeclSyntax>(NewFuncKeyword,
                                                Cursor::FuncKeyword);

}

RC<TokenSyntax> FunctionDeclSyntax::getIdentifier() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::Identifier));
}

FunctionDeclSyntax
FunctionDeclSyntax::withIdentifier(RC<TokenSyntax> NewIdentifier) const {
  assert(NewIdentifier->getTokenKind() == tok::identifier);
  return Data->replaceChild<FunctionDeclSyntax>(NewIdentifier,
                                                Cursor::Identifier);
}

llvm::Optional<GenericParameterClauseSyntax>
FunctionDeclSyntax::getGenericParameterClause() const {
  auto RawGenericParams = getRaw()->getChild(Cursor::Attributes);
  if (RawGenericParams->isMissing()) {
    return llvm::None;
  }

  auto *MyData = getUnsafeData<FunctionDeclSyntax>();

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedGenericParams);

  SyntaxData::realizeSyntaxNode<DeclModifierListSyntax>(ChildPtr,
    RawGenericParams, MyData, cursorIndex(Cursor::GenericParameterClause));

  GenericParameterClauseSyntax Params {
    Root,
    MyData->CachedGenericParams.get()
  };
  
  return Params;
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
  auto RawSig = getRaw()->getChild(Cursor::Attributes);

  auto *MyData = getUnsafeData<FunctionDeclSyntax>();

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedSignature);

  SyntaxData::realizeSyntaxNode<FunctionSignatureSyntax>(ChildPtr, RawSig,
    MyData, cursorIndex(Cursor::Signature));

  return { Root, MyData->CachedSignature.get() };
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

  auto *MyData = getUnsafeData<FunctionDeclSyntax>();

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedBody);

  SyntaxData::realizeSyntaxNode<CodeBlockStmtSyntax>(ChildPtr,
    RawBody, MyData, cursorIndex(Cursor::Body));
  
  CodeBlockStmtSyntax Body {
    Root,
    MyData->CachedBody.get()
  };
  
  return Body;
}

FunctionDeclSyntax FunctionDeclSyntax::
withBody(llvm::Optional<CodeBlockStmtSyntax> NewBody) const {
  auto RawBody = NewBody.hasValue()
    ? NewBody->getRaw()
    : SyntaxFactory::makeBlankCodeBlock().getRaw();
  return Data->replaceChild<FunctionDeclSyntax>(RawBody,
                                                Cursor::Body);
}
