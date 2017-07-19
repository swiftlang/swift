//===--- TypeSyntax.cpp - Swift Type Syntax Implementation ----------------===//
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

#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/TypeSyntax.h"

using namespace swift;
using namespace swift::syntax;

using llvm::None;
using llvm::Optional;

#pragma mark - balanced-tokens Data

void BalancedTokensSyntax::validate() const {
  assert(Data->Raw->Kind == SyntaxKind::BalancedTokens);
  // TODO: Add some checks here that each element of raw syntax
  // matches the grammar rules in the doc comment of
  // this class.
}

BalancedTokensSyntax BalancedTokensSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::BalancedTokens, {},
                             SourcePresence::Present);
  return make<BalancedTokensSyntax>(Raw);
}

#pragma mark - balanced-tokens API

BalancedTokensSyntax
BalancedTokensSyntax::addBalancedToken(TokenSyntax NewBalancedToken) const {
#ifndef NDEBUG
  assert(NewBalancedToken.getTokenKind() != tok::l_paren);
  assert(NewBalancedToken.getTokenKind() != tok::r_paren);
  assert(NewBalancedToken.getTokenKind() != tok::l_square);
  assert(NewBalancedToken.getTokenKind() != tok::r_square);
  assert(NewBalancedToken.getTokenKind() != tok::l_brace);
  assert(NewBalancedToken.getTokenKind() != tok::r_brace);
  auto IsIdentifier = NewBalancedToken.getTokenKind() == tok::identifier;
  auto IsKeyword = NewBalancedToken.isKeyword();
  auto IsLiteral = NewBalancedToken.isLiteral();
  auto IsOperator = NewBalancedToken.isOperator();
  auto IsPunctuation = NewBalancedToken.isPunctuation();
  assert(IsIdentifier || IsKeyword || IsLiteral || IsOperator ||
         IsPunctuation);
#endif
  auto Layout = getRaw()->Layout;
  Layout.push_back(NewBalancedToken.getRaw());

  auto NewRaw = RawSyntax::make(SyntaxKind::BalancedTokens, Layout,
                                SourcePresence::Present);
  return Data->replaceSelf<BalancedTokensSyntax>(NewRaw);
}

#pragma mark - type-attribute Data

void TypeAttributeSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::TypeAttribute);
  assert(Raw->Layout.size() == 5);
  syntax_assert_child_token_text(Raw,
                                 TypeAttributeSyntax::Cursor::AtSignToken,
                                 tok::at_sign, "@");
  syntax_assert_child_token(Raw, TypeAttributeSyntax::Cursor::Identifier,
                            tok::identifier);
  syntax_assert_child_token_text(Raw,
                                 TypeAttributeSyntax::Cursor::LeftParenToken,
                                 tok::l_paren,
                                 "(");
  syntax_assert_child_kind(Raw, TypeAttributeSyntax::Cursor::BalancedTokens,
                           SyntaxKind::BalancedTokens);
  syntax_assert_child_token_text(Raw,
                                 TypeAttributeSyntax::Cursor::RightParenToken,
                                 tok::r_paren,
                                 ")");
}

#pragma mark - type-attribute API

TypeAttributeSyntax TypeAttributeSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::TypeAttribute,
                             {
                               RawTokenSyntax::missingToken(tok::at_sign, "@"),
                               RawTokenSyntax::missingToken(tok::identifier, ""),
                               RawTokenSyntax::missingToken(tok::l_paren, "("),
                               RawSyntax::missing(SyntaxKind::BalancedTokens),
                               RawTokenSyntax::missingToken(tok::r_paren, ")"),
                             },
                             SourcePresence::Present);
  return make<TypeAttributeSyntax>(Raw);
}

TypeAttributeSyntax
TypeAttributeSyntax::withAtSignToken(TokenSyntax NewAtSignToken) const {
  syntax_assert_token_is(NewAtSignToken, tok::at_sign, "@");
  return Data->replaceChild<TypeAttributeSyntax>(NewAtSignToken.getRaw(),
                                                 Cursor::AtSignToken);
}

TypeAttributeSyntax
TypeAttributeSyntax::withIdentifier(TokenSyntax NewIdentifier) const {
  assert(NewIdentifier.getTokenKind() == tok::identifier);
  return Data->replaceChild<TypeAttributeSyntax>(NewIdentifier.getRaw(),
                                                 Cursor::Identifier);
};

TypeAttributeSyntax TypeAttributeSyntax::
withLeftParenToken(TokenSyntax NewLeftParenToken) const {
  assert(NewLeftParenToken.getTokenKind() == tok::l_paren);
  return Data->replaceChild<TypeAttributeSyntax>(NewLeftParenToken.getRaw(),
                                                 Cursor::LeftParenToken);
};

TypeAttributeSyntax TypeAttributeSyntax::
withBalancedTokens(BalancedTokensSyntax NewBalancedTokens) const {
  return Data->replaceChild<TypeAttributeSyntax>(NewBalancedTokens.getRaw(),
                                                 Cursor::BalancedTokens);
}

TypeAttributeSyntax TypeAttributeSyntax::
withRightParenToken(TokenSyntax NewRightParenToken) const {
  assert(NewRightParenToken.getTokenKind() == tok::r_paren);
  return Data->replaceChild<TypeAttributeSyntax>(NewRightParenToken.getRaw(),
                                                 Cursor::RightParenToken);
};

#pragma mark - type-identifier API

void TypeIdentifierSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::TypeIdentifier);
  assert(Raw->Layout.size() == 4);
  syntax_assert_child_token(Raw, TypeIdentifierSyntax::Cursor::Identifier,
                            tok::identifier);
  syntax_assert_child_kind(Raw,
    TypeIdentifierSyntax::Cursor::GenericArgumentClause,
    SyntaxKind::GenericArgumentClause);
  syntax_assert_child_token_text(Raw, TypeIdentifierSyntax::Cursor::DotToken,
                                 tok::period, ".");
  syntax_assert_child_kind(Raw,
                           TypeIdentifierSyntax::Cursor::ChildTypeIdentifier,
                           SyntaxKind::TypeIdentifier);
}

TypeIdentifierSyntax TypeIdentifierSyntax::makeBlank() {
  return make<TypeIdentifierSyntax>(RawSyntax::make(
    SyntaxKind::TypeIdentifier,
    {
      RawTokenSyntax::missingToken(tok::identifier, ""),
      RawSyntax::missing(SyntaxKind::GenericArgumentClause),
      RawTokenSyntax::missingToken(tok::period, "."),
      RawSyntax::missing(SyntaxKind::TypeIdentifier),
    },
    SourcePresence::Present));
}

TypeIdentifierSyntax
TypeIdentifierSyntax::addChildType(TypeIdentifierSyntax ChildType) const {
  auto MaybeChild = getRaw()->getChild(Cursor::ChildTypeIdentifier);

  if (MaybeChild->isMissing()) {
    auto NewRaw =
        getRaw()->replaceChild(Cursor::DotToken,
                               SyntaxFactory::makeDotToken({}, {}).getRaw())
            ->replaceChild(Cursor::ChildTypeIdentifier, ChildType.getRaw());

    return Data->replaceSelf<TypeIdentifierSyntax>(NewRaw);
  } else {
    auto NewRawChild = MaybeChild->replaceChild(Cursor::ChildTypeIdentifier,
                                                ChildType.getRaw());
    auto NewRaw = getRaw()->replaceChild(Cursor::ChildTypeIdentifier,
                                         NewRawChild);
    return Data->replaceSelf<TypeIdentifierSyntax>(NewRaw);
  }
}

TypeIdentifierSyntax
TypeIdentifierSyntax::withIdentifier(TokenSyntax NewIdentifier) const {
  assert(NewIdentifier.getTokenKind() == tok::identifier);
  return Data->replaceChild<TypeIdentifierSyntax>(NewIdentifier.getRaw(),
                                                  Cursor::Identifier);
}

TypeIdentifierSyntax
TypeIdentifierSyntax::withDotToken(TokenSyntax NewDotToken) const {
  syntax_assert_token_is(NewDotToken, tok::period, ".");
  return Data->replaceChild<TypeIdentifierSyntax>(NewDotToken.getRaw(),
                                                  Cursor::DotToken);
}

#pragma mark - tuple-type API

void TupleTypeSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::TupleType);
  assert(Raw->Layout.size() == 3);
  syntax_assert_child_token_text(Raw, TupleTypeSyntax::Cursor::LeftParenToken,
                                 tok::l_paren,
                                 "(");
  syntax_assert_child_kind(Raw, TupleTypeSyntax::Cursor::TypeElementList,
                           SyntaxKind::TupleTypeElementList);
  syntax_assert_child_token_text(Raw, TupleTypeSyntax::Cursor::RightParenToken,
                                 tok::r_paren,
                                 ")");
}

TupleTypeSyntax
TupleTypeSyntax::makeBlank() {
  return make<TupleTypeSyntax>(
      RawSyntax::make(SyntaxKind::TupleType,
                      {
                        RawTokenSyntax::missingToken(tok::l_paren, "("),
                        RawSyntax::missing(SyntaxKind::TupleTypeElementList),
                        RawTokenSyntax::missingToken(tok::r_paren, ")"),
                      },
                      SourcePresence::Present));
}

TupleTypeSyntax
TupleTypeSyntax::withLeftParen(TokenSyntax NewLeftParen) const {
  syntax_assert_token_is(NewLeftParen, tok::l_paren, "(");
  auto NewRaw = getRaw()->replaceChild(Cursor::LeftParenToken,
                                       NewLeftParen.getRaw());
  return Data->replaceSelf<TupleTypeSyntax>(NewRaw);
}

TupleTypeSyntax TupleTypeSyntax::
withTypeElementList(TupleTypeElementListSyntax NewTypeElementList) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::TypeElementList,
                                       NewTypeElementList.getRaw());
  return Data->replaceSelf<TupleTypeSyntax>(NewRaw);
}

TupleTypeSyntax TupleTypeSyntax::
withRightParen(TokenSyntax NewRightParen) const {
  syntax_assert_token_is(NewRightParen, tok::r_paren, ")");
  return Data->replaceChild<TupleTypeSyntax>(NewRightParen.getRaw(),
                                             Cursor::RightParenToken);
}

#pragma mark - tuple-type Builder

TupleTypeSyntaxBuilder::TupleTypeSyntaxBuilder()
  : ElementTypeLayout(
      SyntaxFactory::makeBlankTupleTypeElementList().getRaw()->Layout) {}

TupleTypeSyntaxBuilder &TupleTypeSyntaxBuilder::
addElementTypeSyntax(TupleTypeElementSyntax ElementTypeSyntax) {
  ElementTypeLayout.push_back(ElementTypeSyntax.getRaw());
  return *this;
}

TupleTypeSyntaxBuilder &
TupleTypeSyntaxBuilder::useLeftParen(TokenSyntax LeftParen) {
  LeftParenToken = LeftParen.getRaw();
  return *this;
}

TupleTypeSyntaxBuilder &
TupleTypeSyntaxBuilder::useRightParen(TokenSyntax RightParen) {
  RightParenToken = RightParen.getRaw();
  return *this;
}

TupleTypeSyntax TupleTypeSyntaxBuilder::build() const {
  auto ElementsRaw = RawSyntax::make(SyntaxKind::TupleTypeElementList,
                                     { ElementTypeLayout },
                                     SourcePresence::Present);
  auto Raw = RawSyntax::make(SyntaxKind::TupleType,
                             {
                               LeftParenToken,
                               ElementsRaw,
                               RightParenToken,
                             },
                             SourcePresence::Present);
  return make<TupleTypeSyntax>(Raw);
}

#pragma mark - tuple-type-element API

void TupleTypeElementSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::TupleTypeElement);
  assert(Raw->Layout.size() == 6);
  syntax_assert_child_token(Raw, TupleTypeElementSyntax::Cursor::Label,
                            tok::identifier);
  syntax_assert_child_token_text(Raw,
                                 TupleTypeElementSyntax::Cursor::ColonToken,
                                 tok::colon, ":");
  syntax_assert_child_kind(Raw, TupleTypeElementSyntax::Cursor::Attributes,
                           SyntaxKind::TypeAttributes);
  syntax_assert_child_token_text(Raw,
                                 TupleTypeElementSyntax::Cursor::InoutToken,
                                 tok::kw_inout,
                                 "inout");
  syntax_assert_child_token_text(Raw,
                                 TupleTypeElementSyntax::Cursor::CommaToken,
                                 tok::comma, ",");
  assert(Raw->getChild(TupleTypeElementSyntax::Cursor::Type)->isType());
}

TupleTypeElementSyntax TupleTypeElementSyntax::makeBlank() {
  return make<TupleTypeElementSyntax>(
      RawSyntax::make(SyntaxKind::TupleTypeElement,
                      {
                        RawTokenSyntax::missingToken(tok::identifier, ""),
                        RawTokenSyntax::missingToken(tok::colon, ":"),
                        RawSyntax::missing(SyntaxKind::TypeAttributes),
                        RawTokenSyntax::missingToken(tok::kw_inout, "inout"),
                        RawSyntax::missing(SyntaxKind::MissingType),
                        RawTokenSyntax::missingToken(tok::comma, ","),
                      },
                      SourcePresence::Present));
}


TokenSyntax
TupleTypeElementSyntax::getLabel() const {
  TokenSyntax Child = { Root, Data->getChild(Cursor::Label).get() };
  assert(Child.getTokenKind() == tok::identifier);
  return Child;
}

TupleTypeElementSyntax
TupleTypeElementSyntax::withLabel(TokenSyntax NewLabel) const {
  assert(NewLabel.getTokenKind() == tok::identifier);
  return Data->replaceChild<TupleTypeElementSyntax>(NewLabel.getRaw(),
                                                    Cursor::Label);
}

TokenSyntax
TupleTypeElementSyntax::getColonToken() const {
  TokenSyntax ColonToken = { Root, Data->getChild(Cursor::ColonToken).get() };
  syntax_assert_token_is(ColonToken, tok::colon, ":");
  return ColonToken;
}

TupleTypeElementSyntax
TupleTypeElementSyntax::withColonToken(TokenSyntax NewColonToken) const {
  syntax_assert_token_is(NewColonToken, tok::colon, ":");
  return Data->replaceChild<TupleTypeElementSyntax>(NewColonToken.getRaw(),
                                                    Cursor::ColonToken);
}

TokenSyntax
TupleTypeElementSyntax::getCommaToken() const {
  TokenSyntax CommaToken = { Root, Data->getChild(Cursor::CommaToken).get() };
  syntax_assert_token_is(CommaToken, tok::comma, ",");
  return CommaToken;
}

TupleTypeElementSyntax
TupleTypeElementSyntax::withCommaToken(TokenSyntax NewCommaToken) const {
  syntax_assert_token_is(NewCommaToken, tok::comma, ",");
  return Data->replaceChild<TupleTypeElementSyntax>(NewCommaToken.getRaw(),
                                                    Cursor::CommaToken);
}

TupleTypeElementSyntax TupleTypeElementSyntax::
withTypeAttributes(TypeAttributesSyntax NewTypeAttributes) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::Attributes,
                                       NewTypeAttributes.getRaw());
  return Data->replaceSelf<TupleTypeElementSyntax>(NewRaw);
}

TupleTypeElementSyntax TupleTypeElementSyntax::
withInoutToken(TokenSyntax NewInoutToken) const {
  syntax_assert_token_is(NewInoutToken, tok::kw_inout, "inout");
  return Data->replaceChild<TupleTypeElementSyntax>(NewInoutToken.getRaw(),
                                                    Cursor::InoutToken);
}

TupleTypeElementSyntax
TupleTypeElementSyntax::withTypeSyntax(TypeSyntax NewTypeSyntax) const {
  return Data->replaceChild<TupleTypeElementSyntax>(NewTypeSyntax.getRaw(),
                                                    Cursor::Type);
}

#pragma mark - metatype-type API

void MetatypeTypeSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::MetatypeType);
  assert(Raw->Layout.size() == 3);
  assert(Raw->getChild(MetatypeTypeSyntax::Cursor::BaseType)->isType());
  syntax_assert_child_token_text(Raw, MetatypeTypeSyntax::Cursor::DotToken,
                                 tok::period, ".");
  syntax_assert_child_token(Raw, MetatypeTypeSyntax::Cursor::TypeToken,
                            tok::identifier);
}

MetatypeTypeSyntax MetatypeTypeSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::MetatypeType,
                             {
                               RawSyntax::missing(SyntaxKind::MissingType),
                               RawTokenSyntax::missingToken(tok::period, "."),
                               RawTokenSyntax::missingToken(tok::identifier,
                                                            ""),
                             },
                             SourcePresence::Present);
  return make<MetatypeTypeSyntax>(Raw);
}

MetatypeTypeSyntax
MetatypeTypeSyntax::withBaseTypeSyntax(TypeSyntax NewBaseTypeSyntax) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::BaseType,
                                       NewBaseTypeSyntax.getRaw());
  return Data->replaceSelf<MetatypeTypeSyntax>(NewRaw);
}

MetatypeTypeSyntax
MetatypeTypeSyntax::withDotToken(TokenSyntax NewDotToken) const {
  syntax_assert_token_is(NewDotToken, tok::period, ".");
  return Data->replaceChild<MetatypeTypeSyntax>(NewDotToken.getRaw(),
                                                Cursor::DotToken);
}

MetatypeTypeSyntax
MetatypeTypeSyntax::withTypeToken(TokenSyntax NewTypeToken) const {
  assert(NewTypeToken.getTokenKind() == tok::identifier);
  assert(NewTypeToken.getText() == "Type" ||
         NewTypeToken.getText() == "Protocol");
  return Data->replaceChild<MetatypeTypeSyntax>(NewTypeToken.getRaw(),
                                                Cursor::TypeToken);
}

#pragma mark - optional-type API

void OptionalTypeSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::OptionalType);
  assert(Raw->Layout.size() == 2);
  assert(Raw->getChild(OptionalTypeSyntax::Cursor::BaseType)->isType());
  syntax_assert_child_token_text(Raw, OptionalTypeSyntax::Cursor::QuestionToken,
                                 tok::question_postfix, "?");
}

OptionalTypeSyntax OptionalTypeSyntax::makeBlank() {
  return make<OptionalTypeSyntax>(RawSyntax::make(SyntaxKind::OptionalType,
    {
      RawSyntax::missing(SyntaxKind::MissingType),
      RawTokenSyntax::missingToken(tok::question_postfix, "?"),
    },
    SourcePresence::Present));
}

OptionalTypeSyntax
OptionalTypeSyntax::withBaseTypeSyntax(TypeSyntax NewTypeSyntax) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::BaseType, NewTypeSyntax.getRaw());
  return Data->replaceSelf<OptionalTypeSyntax>(NewRaw);
}

OptionalTypeSyntax
OptionalTypeSyntax::withQuestionToken(TokenSyntax NewQuestionToken) const {
  syntax_assert_token_is(NewQuestionToken, tok::question_postfix, "?");
  return Data->replaceChild<OptionalTypeSyntax>(NewQuestionToken.getRaw(),
                                                Cursor::QuestionToken);
}

#pragma mark - implicitly-unwrapped-optional-type API

void ImplicitlyUnwrappedOptionalTypeSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::ImplicitlyUnwrappedOptionalType);
  assert(Raw->Layout.size() == 2);
  assert(Raw->getChild(ImplicitlyUnwrappedOptionalTypeSyntax::Cursor::Type)
           ->isType());
  syntax_assert_child_token_text(Raw,
    ImplicitlyUnwrappedOptionalTypeSyntax::Cursor::ExclaimToken,
    tok::exclaim_postfix, "!");
}

ImplicitlyUnwrappedOptionalTypeSyntax
ImplicitlyUnwrappedOptionalTypeSyntax::makeBlank() {
  auto Raw = RawSyntax::make(
               SyntaxKind::ImplicitlyUnwrappedOptionalType,
               {
                 RawSyntax::missing(SyntaxKind::MissingType),
                 RawTokenSyntax::missingToken(tok::exclaim_postfix, "!"),
               },
               SourcePresence::Present);
  return make<ImplicitlyUnwrappedOptionalTypeSyntax>(Raw);
}

ImplicitlyUnwrappedOptionalTypeSyntax ImplicitlyUnwrappedOptionalTypeSyntax::
withBaseTypeSyntax(TypeSyntax NewTypeSyntax) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::Type, NewTypeSyntax.getRaw());
  return Data->replaceSelf<ImplicitlyUnwrappedOptionalTypeSyntax>(NewRaw);
}

ImplicitlyUnwrappedOptionalTypeSyntax ImplicitlyUnwrappedOptionalTypeSyntax::
withExclaimToken(TokenSyntax NewExclaimToken) const {
  syntax_assert_token_is(NewExclaimToken, tok::exclaim_postfix, "!");
  return Data->replaceChild<ImplicitlyUnwrappedOptionalTypeSyntax>(
           NewExclaimToken.getRaw(), Cursor::ExclaimToken);
}

#pragma mark - array-type API

void ArrayTypeSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::ArrayType);
  assert(Raw->Layout.size() == 3);
  syntax_assert_child_token_text(Raw,
    ArrayTypeSyntax::Cursor::LeftSquareBracketToken,
    tok::l_square, "[");
  assert(Raw->getChild(ArrayTypeSyntax::Cursor::Type)->isType());
  syntax_assert_child_token_text(Raw,
    ArrayTypeSyntax::Cursor::RightSquareBracketToken,
    tok::r_square, "]");
}

ArrayTypeSyntax ArrayTypeSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::ArrayType,
                             {
                               RawTokenSyntax::missingToken(tok::l_square, "["),
                               RawSyntax::missing(SyntaxKind::MissingType),
                               RawTokenSyntax::missingToken(tok::r_square, "]"),
                             },
                             SourcePresence::Present);
  return make<ArrayTypeSyntax>(Raw);
}

ArrayTypeSyntax ArrayTypeSyntax::
withLeftSquareBracketToken(TokenSyntax NewLeftSquareBracketToken) const {
  syntax_assert_token_is(NewLeftSquareBracketToken, tok::l_square, "[");
  return Data->replaceChild<ArrayTypeSyntax>(NewLeftSquareBracketToken.getRaw(),
                                             Cursor::LeftSquareBracketToken);
}

ArrayTypeSyntax ArrayTypeSyntax::withType(TypeSyntax NewType) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::Type, NewType.getRaw());
  return Data->replaceSelf<ArrayTypeSyntax>(NewRaw);
}

ArrayTypeSyntax ArrayTypeSyntax::
withRightSquareBracketToken(TokenSyntax NewRightSquareBracketToken) const {
  syntax_assert_token_is(NewRightSquareBracketToken, tok::r_square, "]");
  return Data->replaceChild<ArrayTypeSyntax>(
           NewRightSquareBracketToken.getRaw(),
           Cursor::RightSquareBracketToken);
}

#pragma mark - dictionary-type API

void DictionaryTypeSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::DictionaryType);
  assert(Raw->Layout.size() == 5);

  syntax_assert_child_token_text(Raw,
    DictionaryTypeSyntax::Cursor::LeftSquareBracketToken,
    tok::l_square, "[");
  assert(Raw->getChild(DictionaryTypeSyntax::Cursor::KeyType)->isType());
  syntax_assert_child_token_text(Raw, DictionaryTypeSyntax::Cursor::ColonToken,
                                 tok::colon, ":");
  assert(Raw->getChild(DictionaryTypeSyntax::Cursor::ValueType)->isType());
  syntax_assert_child_token_text(Raw,
    DictionaryTypeSyntax::Cursor::RightSquareBracketToken,
    tok::r_square, "]");
}

DictionaryTypeSyntax DictionaryTypeSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::DictionaryType,
                             {
                               RawTokenSyntax::missingToken(tok::l_square, "["),
                               RawSyntax::missing(SyntaxKind::MissingType),
                               RawTokenSyntax::missingToken(tok::colon, ":"),
                               RawSyntax::missing(SyntaxKind::MissingType),
                               RawTokenSyntax::missingToken(tok::r_square, "]"),
                             },
                             SourcePresence::Present);
  return make<DictionaryTypeSyntax>(Raw);
}

DictionaryTypeSyntax DictionaryTypeSyntax::
withLeftSquareBracketToken(TokenSyntax NewLeftSquareBracketToken) const {
  syntax_assert_token_is(NewLeftSquareBracketToken, tok::l_square, "[");
  return Data->replaceChild<DictionaryTypeSyntax>(
           NewLeftSquareBracketToken.getRaw(), Cursor::LeftSquareBracketToken);
}

DictionaryTypeSyntax
DictionaryTypeSyntax::withKeyTypeSyntax(TypeSyntax NewTypeSyntax) const {
  return Data->replaceChild<DictionaryTypeSyntax>(NewTypeSyntax.getRaw(),
                                                  Cursor::KeyType);
}

DictionaryTypeSyntax
DictionaryTypeSyntax::withColon(TokenSyntax NewColonToken) const {
  syntax_assert_token_is(NewColonToken, tok::colon, ":");
  return Data->replaceChild<DictionaryTypeSyntax>(NewColonToken.getRaw(),
                                                  Cursor::ColonToken);
}

DictionaryTypeSyntax
DictionaryTypeSyntax::withValueTypeSyntax(TypeSyntax NewTypeSyntax) const {
  return Data->replaceChild<DictionaryTypeSyntax>(NewTypeSyntax.getRaw(),
                                                  Cursor::ValueType);
}

DictionaryTypeSyntax DictionaryTypeSyntax::
withRightSquareBracketToken(TokenSyntax NewRightSquareBracketToken) const {
  syntax_assert_token_is(NewRightSquareBracketToken, tok::r_square, "]");
  return Data->replaceChild<DictionaryTypeSyntax>(
           NewRightSquareBracketToken.getRaw(),
           Cursor::RightSquareBracketToken);
}

#pragma mark - function-type-argument API

void FunctionTypeArgumentSyntax::validate() const {}

FunctionTypeArgumentSyntax FunctionTypeArgumentSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionTypeArgument,
                             {
                               RawTokenSyntax::missingToken(tok::identifier, ""),
                               RawTokenSyntax::missingToken(tok::identifier, ""),
                               RawTokenSyntax::missingToken(tok::colon, ","),
                             },
                             SourcePresence::Present);
  return make<FunctionTypeArgumentSyntax>(Raw);
}

#pragma mark - function-type API

void FunctionTypeSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::FunctionType);
  syntax_assert_child_kind(Raw, FunctionTypeSyntax::Cursor::TypeAttributes,
                           SyntaxKind::TypeAttributes);
  syntax_assert_child_token_text(Raw, FunctionTypeSyntax::Cursor::LeftParen,
                                 tok::l_paren, "(");

  syntax_assert_child_kind(Raw, FunctionTypeSyntax::Cursor::ArgumentList,
                           SyntaxKind::FunctionParameterList);

  syntax_assert_child_token_text(Raw, FunctionTypeSyntax::Cursor::RightParen,
                                 tok::r_paren, ")");
#ifndef NDEBUG
  auto ThrowsOrRethrows =
    cast<RawTokenSyntax>(
      Raw->getChild(FunctionTypeSyntax::Cursor::ThrowsOrRethrows));
  assert(ThrowsOrRethrows->is(tok::kw_throws, "throws") ||
         ThrowsOrRethrows->is(tok::kw_rethrows, "rethrows"));
#endif
  syntax_assert_child_token_text(Raw, FunctionTypeSyntax::Cursor::Arrow,
                                 tok::arrow, "->");
  assert(Raw->getChild(FunctionTypeSyntax::Cursor::ReturnType)->isType());
}

FunctionTypeSyntax FunctionTypeSyntax::makeBlank() {
  return make<FunctionTypeSyntax>(RawSyntax::make(SyntaxKind::FunctionType,
    {
      RawSyntax::missing(SyntaxKind::TypeAttributes),
      RawTokenSyntax::missingToken(tok::l_paren, "("),
      RawSyntax::missing(SyntaxKind::FunctionParameterList),
      RawTokenSyntax::missingToken(tok::r_paren, ")"),
      RawTokenSyntax::missingToken(tok::kw_throws, "throws"),
      RawTokenSyntax::missingToken(tok::arrow, "->"),
      RawSyntax::missing(SyntaxKind::MissingType),
    },
    SourcePresence::Present));
}

FunctionTypeSyntax FunctionTypeSyntax::
withTypeAttributes(TypeAttributesSyntax NewAttributes) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::TypeAttributes,
                                     NewAttributes.getRaw());
  return Data->replaceSelf<FunctionTypeSyntax>(NewRaw);
}

FunctionTypeSyntax
FunctionTypeSyntax::withLeftArgumentsParen(TokenSyntax NewLeftParen) const {
  syntax_assert_token_is(NewLeftParen, tok::l_paren, "(");
  return Data->replaceChild<FunctionTypeSyntax>(NewLeftParen.getRaw(),
                                                Cursor::LeftParen);
}

FunctionTypeSyntax FunctionTypeSyntax::
addTypeArgument(llvm::Optional<TokenSyntax> MaybeComma,
                FunctionTypeArgumentSyntax NewArgument) const {
  auto ArgList = getRaw()->getChild(Cursor::ArgumentList);

  if (MaybeComma.hasValue()) {
    syntax_assert_token_is(MaybeComma.getValue(), tok::comma, ",");
    ArgList = ArgList->append(MaybeComma->getRaw());
  } else {
    if (!ArgList->Layout.empty()) {
      ArgList = ArgList->append(RawTokenSyntax::missingToken(tok::comma, ","));
    }
  }
  ArgList = ArgList->append(NewArgument.getRaw());
  return Data->replaceChild<FunctionTypeSyntax>(ArgList, Cursor::ArgumentList);
}

FunctionTypeSyntax FunctionTypeSyntax::
withRightArgumentsParen(TokenSyntax NewLeftParen) const {
  syntax_assert_token_is(NewLeftParen, tok::r_paren, ")");
  return Data->replaceChild<FunctionTypeSyntax>(NewLeftParen.getRaw(),
                                                Cursor::RightParen);
}

FunctionTypeSyntax
FunctionTypeSyntax::withThrowsKeyword(TokenSyntax NewThrowsKeyword) const {
  syntax_assert_token_is(NewThrowsKeyword, tok::kw_throws, "throws");
  return Data->replaceChild<FunctionTypeSyntax>(NewThrowsKeyword.getRaw(),
                                                Cursor::ThrowsOrRethrows);
}

FunctionTypeSyntax FunctionTypeSyntax::
withRethrowsKeyword(TokenSyntax NewThrowsKeyword) const {
  syntax_assert_token_is(NewThrowsKeyword, tok::kw_rethrows, "rethrows");
  return Data->replaceChild<FunctionTypeSyntax>(NewThrowsKeyword.getRaw(),
                                                Cursor::ThrowsOrRethrows);
}

FunctionTypeSyntax
FunctionTypeSyntax::withArrow(TokenSyntax NewArrow) const {
  syntax_assert_token_is(NewArrow, tok::arrow, "->");
  return Data->replaceChild<FunctionTypeSyntax>(NewArrow.getRaw(),
                                                Cursor::Arrow);
}

FunctionTypeSyntax
FunctionTypeSyntax::withReturnTypeSyntax(TypeSyntax NewReturnTypeSyntax) const {
  return Data->replaceChild<FunctionTypeSyntax>(NewReturnTypeSyntax.getRaw(),
                                                Cursor::ReturnType);
}

#pragma mark - function-type Builder

FunctionTypeSyntaxBuilder::FunctionTypeSyntaxBuilder()
    : FunctionTypeLayout(
          SyntaxFactory::makeBlankFunctionType().getRaw()->Layout) {}

FunctionTypeSyntaxBuilder &FunctionTypeSyntaxBuilder::useTypeAttributes(
    TypeAttributeSyntax NewAttributes) {
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::TypeAttributes);
  FunctionTypeLayout[Index] = NewAttributes.getRaw();
  return *this;
}

FunctionTypeSyntaxBuilder &
FunctionTypeSyntaxBuilder::useLeftArgumentsParen(TokenSyntax NewLeftParen) {
  syntax_assert_token_is(NewLeftParen, tok::l_paren, "(");
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::LeftParen);
  FunctionTypeLayout[Index] = NewLeftParen.getRaw();
  return *this;
}

FunctionTypeSyntaxBuilder &FunctionTypeSyntaxBuilder::
useRightArgumentsParen(TokenSyntax NewRightParen) {
  syntax_assert_token_is(NewRightParen, tok::r_paren, ")");
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::RightParen);
  FunctionTypeLayout[Index] = NewRightParen.getRaw();
  return *this;
}

FunctionTypeSyntaxBuilder &FunctionTypeSyntaxBuilder::
addArgumentTypeSyntax(Optional<TokenSyntax> MaybeComma,
                      FunctionTypeArgumentSyntax NewTypeArgument) {
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::ArgumentList);
  auto TypeArgumentsLayout = FunctionTypeLayout[Index]->Layout;

  if (MaybeComma.hasValue()) {
    syntax_assert_token_is(MaybeComma.getValue(), tok::comma, ",");
    TypeArgumentsLayout.push_back(MaybeComma->getRaw());
  } else {
    if (TypeArgumentsLayout.empty()) {
      TypeArgumentsLayout.push_back(RawTokenSyntax::missingToken(tok::comma,
                                                                 ","));
    }
  }

  TypeArgumentsLayout.push_back(NewTypeArgument.getRaw());

  FunctionTypeLayout[Index] = RawSyntax::make(SyntaxKind::FunctionParameterList,
                                              TypeArgumentsLayout,
                                              SourcePresence::Present);
  return *this;
}

FunctionTypeSyntaxBuilder &
FunctionTypeSyntaxBuilder::useThrowsKeyword(TokenSyntax NewThrowsKeyword) {
  syntax_assert_token_is(NewThrowsKeyword, tok::kw_throws, "throws");
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::ThrowsOrRethrows);
  FunctionTypeLayout[Index] = NewThrowsKeyword.getRaw();
  return *this;
}

FunctionTypeSyntaxBuilder &
FunctionTypeSyntaxBuilder::useRethrowsKeyword(TokenSyntax NewRethrowsKeyword) {
  syntax_assert_token_is(NewRethrowsKeyword, tok::kw_rethrows, "rethrows");
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::ThrowsOrRethrows);
  FunctionTypeLayout[Index] = NewRethrowsKeyword.getRaw();
  return *this;
}

FunctionTypeSyntaxBuilder &
FunctionTypeSyntaxBuilder::useArrow(TokenSyntax NewArrow) {
  syntax_assert_token_is(NewArrow, tok::arrow, "->");
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::Arrow);
  FunctionTypeLayout[Index] = NewArrow.getRaw();
  return *this;
}

FunctionTypeSyntaxBuilder &
FunctionTypeSyntaxBuilder::useReturnTypeSyntax(TypeSyntax NewReturnType) {
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::ReturnType);
  FunctionTypeLayout[Index] = NewReturnType.getRaw();
  return *this;
}

FunctionTypeSyntax FunctionTypeSyntaxBuilder::build() const {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionType, FunctionTypeLayout,
                             SourcePresence::Present);
  return make<FunctionTypeSyntax>(Raw);
}
