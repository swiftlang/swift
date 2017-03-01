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
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/SyntaxFactory.h"

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

FunctionParameterSyntax::FunctionParameterSyntax(const RC<SyntaxData> Root,
                                                 const DataType *Data)
  : Syntax(Root, Data) {}

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
