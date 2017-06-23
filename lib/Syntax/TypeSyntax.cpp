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
BalancedTokensSyntax::addBalancedToken(RC<TokenSyntax> NewBalancedToken) const {
#ifndef NDEBUG
  assert(NewBalancedToken->getTokenKind() != tok::l_paren);
  assert(NewBalancedToken->getTokenKind() != tok::r_paren);
  assert(NewBalancedToken->getTokenKind() != tok::l_square);
  assert(NewBalancedToken->getTokenKind() != tok::r_square);
  assert(NewBalancedToken->getTokenKind() != tok::l_brace);
  assert(NewBalancedToken->getTokenKind() != tok::r_brace);
  auto IsIdentifier = NewBalancedToken->getTokenKind() == tok::identifier;
  auto IsKeyword = NewBalancedToken->isKeyword();
  auto IsLiteral = NewBalancedToken->isLiteral();
  auto IsOperator = NewBalancedToken->isOperator();
  auto IsPunctuation = NewBalancedToken->isPunctuation();
  assert(IsIdentifier || IsKeyword || IsLiteral || IsOperator ||
         IsPunctuation);
#endif
  auto Layout = getRaw()->Layout;
  Layout.push_back(NewBalancedToken);

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
                               TokenSyntax::missingToken(tok::at_sign, "@"),
                               TokenSyntax::missingToken(tok::identifier, ""),
                               TokenSyntax::missingToken(tok::l_paren, "("),
                               RawSyntax::missing(SyntaxKind::BalancedTokens),
                               TokenSyntax::missingToken(tok::r_paren, ")"),
                             },
                             SourcePresence::Present);
  return make<TypeAttributeSyntax>(Raw);
}

TypeAttributeSyntax
TypeAttributeSyntax::withAtSignToken(RC<TokenSyntax> NewAtSignToken) const {
  syntax_assert_token_is(NewAtSignToken, tok::at_sign, "@");
  return Data->replaceChild<TypeAttributeSyntax>(NewAtSignToken,
                                                 Cursor::AtSignToken);
}

TypeAttributeSyntax
TypeAttributeSyntax::withIdentifier(RC<TokenSyntax> NewIdentifier) const {
  assert(NewIdentifier->getTokenKind() == tok::identifier);
  return Data->replaceChild<TypeAttributeSyntax>(NewIdentifier,
                                                 Cursor::Identifier);
};

TypeAttributeSyntax TypeAttributeSyntax::
withLeftParenToken(RC<TokenSyntax> NewLeftParenToken) const {
  assert(NewLeftParenToken->getTokenKind() == tok::l_paren);
  return Data->replaceChild<TypeAttributeSyntax>(NewLeftParenToken,
                                                 Cursor::LeftParenToken);
};

TypeAttributeSyntax TypeAttributeSyntax::
withBalancedTokens(BalancedTokensSyntax NewBalancedTokens) const {
  return Data->replaceChild<TypeAttributeSyntax>(NewBalancedTokens.getRaw(),
                                                 Cursor::BalancedTokens);
}

TypeAttributeSyntax TypeAttributeSyntax::
withRightParenToken(RC<TokenSyntax> NewRightParenToken) const {
  assert(NewRightParenToken->getTokenKind() == tok::r_paren);
  return Data->replaceChild<TypeAttributeSyntax>(NewRightParenToken,
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
      TokenSyntax::missingToken(tok::identifier, ""),
      RawSyntax::missing(SyntaxKind::GenericArgumentClause),
      TokenSyntax::missingToken(tok::period, "."),
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
                               SyntaxFactory::makeDotToken({}, {}))
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
TypeIdentifierSyntax::withIdentifier(RC<TokenSyntax> NewIdentifier) const {
  assert(NewIdentifier->getTokenKind() == tok::identifier);
  auto NewRaw = getRaw()->replaceChild(Cursor::Identifier,
                                               NewIdentifier);
  return Data->replaceSelf<TypeIdentifierSyntax>(NewRaw);
}

TypeIdentifierSyntax
TypeIdentifierSyntax::withDotToken(RC<TokenSyntax> NewDotToken) const {
  syntax_assert_token_is(NewDotToken, tok::period, ".");
  auto NewRaw = getRaw()->replaceChild(Cursor::DotToken, NewDotToken);
  return Data->replaceSelf<TypeIdentifierSyntax>(NewRaw);
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
                        TokenSyntax::missingToken(tok::l_paren, "("),
                        RawSyntax::missing(SyntaxKind::TupleTypeElementList),
                        TokenSyntax::missingToken(tok::r_paren, ")"),
                      },
                      SourcePresence::Present));
}

TupleTypeSyntax
TupleTypeSyntax::withLeftParen(RC<TokenSyntax> NewLeftParen) const {
  syntax_assert_token_is(NewLeftParen, tok::l_paren, "(");
  auto NewRaw = getRaw()->replaceChild(Cursor::LeftParenToken, NewLeftParen);
  return Data->replaceSelf<TupleTypeSyntax>(NewRaw);
}

TupleTypeSyntax TupleTypeSyntax::
withTypeElementList(TupleTypeElementListSyntax NewTypeElementList) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::TypeElementList,
                                       NewTypeElementList.getRaw());
  return Data->replaceSelf<TupleTypeSyntax>(NewRaw);
}

TupleTypeSyntax TupleTypeSyntax::
withRightParen(RC<TokenSyntax> NewRightParen) const {
  syntax_assert_token_is(NewRightParen, tok::r_paren, ")");
  auto NewRaw = getRaw()->replaceChild(Cursor::RightParenToken, NewRightParen);
  return Data->replaceSelf<TupleTypeSyntax>(NewRaw);
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
TupleTypeSyntaxBuilder::useLeftParen(RC<TokenSyntax> LeftParen) {
  LeftParenToken = LeftParen;
  return *this;
}

TupleTypeSyntaxBuilder &
TupleTypeSyntaxBuilder::useRightParen(RC<TokenSyntax> RightParen) {
  RightParenToken = RightParen;
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
                        TokenSyntax::missingToken(tok::identifier, ""),
                        TokenSyntax::missingToken(tok::colon, ":"),
                        RawSyntax::missing(SyntaxKind::TypeAttributes),
                        TokenSyntax::missingToken(tok::kw_inout, "inout"),
                        RawSyntax::missing(SyntaxKind::MissingType),
                        TokenSyntax::missingToken(tok::comma, ","),
                      },
                      SourcePresence::Present));
}


RC<TokenSyntax>
TupleTypeElementSyntax::getLabel() const {
  auto Label = cast<TokenSyntax>(getRaw()->getChild(Cursor::Label));
  assert(Label->getTokenKind() == tok::identifier);
  return Label;
}

TupleTypeElementSyntax
TupleTypeElementSyntax::withLabel(RC<TokenSyntax> NewLabel) const {
  assert(NewLabel->getTokenKind() == tok::identifier);
  auto NewRaw = getRaw()->replaceChild(Cursor::Label, NewLabel);
  return Data->replaceSelf<TupleTypeElementSyntax>(NewRaw);
}

RC<TokenSyntax>
TupleTypeElementSyntax::getColonToken() const {
  auto ColonToken = cast<TokenSyntax>(getRaw()->getChild(Cursor::ColonToken));
  syntax_assert_token_is(ColonToken, tok::colon, ":");
  return ColonToken;
}

TupleTypeElementSyntax
TupleTypeElementSyntax::withColonToken(RC<TokenSyntax> NewColonToken) const {
  syntax_assert_token_is(NewColonToken, tok::colon, ":")
  auto NewRaw = getRaw()->replaceChild(Cursor::ColonToken, NewColonToken);
  return Data->replaceSelf<TupleTypeElementSyntax>(NewRaw);
}

RC<TokenSyntax>
TupleTypeElementSyntax::getCommaToken() const {
  auto CommaToken = cast<TokenSyntax>(getRaw()->getChild(Cursor::CommaToken));
  syntax_assert_token_is(CommaToken, tok::comma, ",");
  return CommaToken;
}

TupleTypeElementSyntax
TupleTypeElementSyntax::withCommaToken(RC<TokenSyntax> NewCommaToken) const {
  syntax_assert_token_is(NewCommaToken, tok::comma, ",")
  auto NewRaw = getRaw()->replaceChild(Cursor::CommaToken, NewCommaToken);
  return Data->replaceSelf<TupleTypeElementSyntax>(NewRaw);
}

TupleTypeElementSyntax TupleTypeElementSyntax::
withTypeAttributes(TypeAttributesSyntax NewTypeAttributes) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::Attributes,
                                       NewTypeAttributes.getRaw());
  return Data->replaceSelf<TupleTypeElementSyntax>(NewRaw);
}

TupleTypeElementSyntax TupleTypeElementSyntax::
withInoutToken(RC<TokenSyntax> NewInoutToken) const {
  syntax_assert_token_is(NewInoutToken, tok::kw_inout, "inout");
  auto NewRaw = getRaw()->replaceChild(Cursor::InoutToken, NewInoutToken);
  return Data->replaceSelf<TupleTypeElementSyntax>(NewRaw);
}

TupleTypeElementSyntax
TupleTypeElementSyntax::withTypeSyntax(TypeSyntax NewTypeSyntax) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::Type, NewTypeSyntax.getRaw());
  return Data->replaceSelf<TupleTypeElementSyntax>(NewRaw);
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
                               TokenSyntax::missingToken(tok::period, "."),
                               TokenSyntax::missingToken(tok::identifier, ""),
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
MetatypeTypeSyntax::withDotToken(RC<TokenSyntax> NewDotToken) const {
  syntax_assert_token_is(NewDotToken, tok::period, ".");
  auto NewRaw = getRaw()->replaceChild(Cursor::DotToken, NewDotToken);
  return Data->replaceSelf<MetatypeTypeSyntax>(NewRaw);
}

MetatypeTypeSyntax
MetatypeTypeSyntax::withTypeToken(RC<TokenSyntax> NewTypeToken) const {
  assert(NewTypeToken->getTokenKind() == tok::identifier);
  assert(NewTypeToken->getText() == "Type" ||
         NewTypeToken->getText() == "Protocol");
  auto NewRaw = getRaw()->replaceChild(Cursor::TypeToken, NewTypeToken);
  return Data->replaceSelf<MetatypeTypeSyntax>(NewRaw);
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
      TokenSyntax::missingToken(tok::question_postfix, "?"),
    },
    SourcePresence::Present));
}

OptionalTypeSyntax
OptionalTypeSyntax::withBaseTypeSyntax(TypeSyntax NewTypeSyntax) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::BaseType, NewTypeSyntax.getRaw());
  return Data->replaceSelf<OptionalTypeSyntax>(NewRaw);
}

OptionalTypeSyntax
OptionalTypeSyntax::withQuestionToken(RC<TokenSyntax> NewQuestionToken) const {
  syntax_assert_token_is(NewQuestionToken, tok::question_postfix, "?");
  auto NewRaw = getRaw()->replaceChild(Cursor::QuestionToken, NewQuestionToken);
  return Data->replaceSelf<OptionalTypeSyntax>(NewRaw);
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
                 TokenSyntax::missingToken(tok::exclaim_postfix, "!"),
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
withExclaimToken(RC<TokenSyntax> NewExclaimToken) const {
  syntax_assert_token_is(NewExclaimToken, tok::exclaim_postfix, "!");
  auto NewRaw = getRaw()->replaceChild(Cursor::ExclaimToken, NewExclaimToken);
  return Data->replaceSelf<ImplicitlyUnwrappedOptionalTypeSyntax>(NewRaw);
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
                               TokenSyntax::missingToken(tok::l_square, "["),
                               RawSyntax::missing(SyntaxKind::MissingType),
                               TokenSyntax::missingToken(tok::r_square, "]"),
                             },
                             SourcePresence::Present);
  return make<ArrayTypeSyntax>(Raw);
}

ArrayTypeSyntax ArrayTypeSyntax::
withLeftSquareBracketToken(RC<TokenSyntax> NewLeftSquareBracketToken) const {
  syntax_assert_token_is(NewLeftSquareBracketToken, tok::l_square, "[");
  auto NewRaw = getRaw()->replaceChild(Cursor::LeftSquareBracketToken,
                                     NewLeftSquareBracketToken);
  return Data->replaceSelf<ArrayTypeSyntax>(NewRaw);
}

ArrayTypeSyntax ArrayTypeSyntax::withType(TypeSyntax NewType) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::Type, NewType.getRaw());
  return Data->replaceSelf<ArrayTypeSyntax>(NewRaw);
}

ArrayTypeSyntax ArrayTypeSyntax::
withRightSquareBracketToken(RC<TokenSyntax> NewRightSquareBracketToken) const {
  syntax_assert_token_is(NewRightSquareBracketToken, tok::r_square, "]");
  auto NewRaw = getRaw()->replaceChild(Cursor::RightSquareBracketToken,
                                       NewRightSquareBracketToken);
  return Data->replaceSelf<ArrayTypeSyntax>(NewRaw);
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
                               TokenSyntax::missingToken(tok::l_square, "["),
                               RawSyntax::missing(SyntaxKind::MissingType),
                               TokenSyntax::missingToken(tok::colon, ":"),
                               RawSyntax::missing(SyntaxKind::MissingType),
                               TokenSyntax::missingToken(tok::r_square, "]"),
                             },
                             SourcePresence::Present);
  return make<DictionaryTypeSyntax>(Raw);
}

DictionaryTypeSyntax DictionaryTypeSyntax::
withLeftSquareBracketToken(RC<TokenSyntax> NewLeftSquareBracketToken) const {
  syntax_assert_token_is(NewLeftSquareBracketToken, tok::l_square, "[");
  auto NewRaw = getRaw()->replaceChild(Cursor::LeftSquareBracketToken,
                                       NewLeftSquareBracketToken);
  return Data->replaceSelf<DictionaryTypeSyntax>(NewRaw);
}

DictionaryTypeSyntax
DictionaryTypeSyntax::withKeyTypeSyntax(TypeSyntax NewTypeSyntax) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::KeyType, NewTypeSyntax.getRaw());
  return Data->replaceSelf<DictionaryTypeSyntax>(NewRaw);
}

DictionaryTypeSyntax
DictionaryTypeSyntax::withColon(RC<TokenSyntax> NewColonToken) const {
  syntax_assert_token_is(NewColonToken, tok::colon, ":");
  auto NewRaw = getRaw()->replaceChild(Cursor::ColonToken, NewColonToken);
  return Data->replaceSelf<DictionaryTypeSyntax>(NewRaw);
}

DictionaryTypeSyntax
DictionaryTypeSyntax::withValueTypeSyntax(TypeSyntax NewTypeSyntax) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::ValueType,
                                       NewTypeSyntax.getRaw());
  return Data->replaceSelf<DictionaryTypeSyntax>(NewRaw);
}

DictionaryTypeSyntax DictionaryTypeSyntax::
withRightSquareBracketToken(RC<TokenSyntax> NewRightSquareBracketToken) const {
  syntax_assert_token_is(NewRightSquareBracketToken, tok::r_square, "]");
  auto NewRaw = getRaw()->replaceChild(Cursor::RightSquareBracketToken,
                                       NewRightSquareBracketToken);
  return Data->replaceSelf<DictionaryTypeSyntax>(NewRaw);
}

#pragma mark - function-type-argument API

void FunctionTypeArgumentSyntax::validate() const {}

FunctionTypeArgumentSyntax FunctionTypeArgumentSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionTypeArgument,
                             {
                               TokenSyntax::missingToken(tok::identifier, ""),
                               TokenSyntax::missingToken(tok::identifier, ""),
                               TokenSyntax::missingToken(tok::colon, ","),
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
    cast<TokenSyntax>(
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
      TokenSyntax::missingToken(tok::l_paren, "("),
      RawSyntax::missing(SyntaxKind::FunctionParameterList),
      TokenSyntax::missingToken(tok::r_paren, ")"),
      TokenSyntax::missingToken(tok::kw_throws, "throws"),
      TokenSyntax::missingToken(tok::arrow, "->"),
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
FunctionTypeSyntax::withLeftArgumentsParen(RC<TokenSyntax> NewLeftParen) const {
  syntax_assert_token_is(NewLeftParen, tok::l_paren, "(");
  auto NewRaw = getRaw()->replaceChild(Cursor::LeftParen, NewLeftParen);
  return Data->replaceSelf<FunctionTypeSyntax>(NewRaw);
}

FunctionTypeSyntax FunctionTypeSyntax::
addTypeArgument(llvm::Optional<RC<TokenSyntax>> MaybeComma,
                FunctionTypeArgumentSyntax NewArgument) const {
  auto ArgList = getRaw()->getChild(Cursor::ArgumentList);

  if (MaybeComma.hasValue()) {
    syntax_assert_token_is(MaybeComma.getValue(), tok::comma, ",");
    ArgList = ArgList->append(MaybeComma.getValue());
  } else {
    if (!ArgList->Layout.empty()) {
      ArgList = ArgList->append(TokenSyntax::missingToken(tok::comma, ","));
    }
  }
  ArgList = ArgList->append(NewArgument.getRaw());
  auto NewRaw = getRaw()->replaceChild(Cursor::ArgumentList, ArgList);
  return Data->replaceSelf<FunctionTypeSyntax>(NewRaw);
}

FunctionTypeSyntax FunctionTypeSyntax::
withRightArgumentsParen(RC<TokenSyntax> NewLeftParen) const {
  syntax_assert_token_is(NewLeftParen, tok::r_paren, ")");
  auto NewRaw = getRaw()->replaceChild(Cursor::RightParen, NewLeftParen);
  return Data->replaceSelf<FunctionTypeSyntax>(NewRaw);
}

FunctionTypeSyntax
FunctionTypeSyntax::withThrowsKeyword(RC<TokenSyntax> NewThrowsKeyword) const {
  syntax_assert_token_is(NewThrowsKeyword, tok::kw_throws, "throws");
  auto NewRaw = getRaw()->replaceChild(Cursor::ThrowsOrRethrows,
                                       NewThrowsKeyword);
  return Data->replaceSelf<FunctionTypeSyntax>(NewRaw);
}

FunctionTypeSyntax FunctionTypeSyntax::
withRethrowsKeyword(RC<TokenSyntax> NewThrowsKeyword) const {
  syntax_assert_token_is(NewThrowsKeyword, tok::kw_rethrows, "rethrows");
  auto NewRaw = getRaw()->replaceChild(Cursor::ThrowsOrRethrows,
                                       NewThrowsKeyword);
  return Data->replaceSelf<FunctionTypeSyntax>(NewRaw);
}

FunctionTypeSyntax
FunctionTypeSyntax::withArrow(RC<TokenSyntax> NewArrow) const {
  syntax_assert_token_is(NewArrow, tok::arrow, "->");
  auto NewRaw = getRaw()->replaceChild(Cursor::Arrow, NewArrow);
  return Data->replaceSelf<FunctionTypeSyntax>(NewRaw);
}

FunctionTypeSyntax
FunctionTypeSyntax::withReturnTypeSyntax(TypeSyntax NewReturnTypeSyntax) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::ReturnType,
                                       NewReturnTypeSyntax.getRaw());
  return Data->replaceSelf<FunctionTypeSyntax>(NewRaw);
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
FunctionTypeSyntaxBuilder::useLeftArgumentsParen(RC<TokenSyntax> NewLeftParen) {
  syntax_assert_token_is(NewLeftParen, tok::l_paren, "(");
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::LeftParen);
  FunctionTypeLayout[Index] = NewLeftParen;
  return *this;
}

FunctionTypeSyntaxBuilder &FunctionTypeSyntaxBuilder::
useRightArgumentsParen(RC<TokenSyntax> NewRightParen) {
  syntax_assert_token_is(NewRightParen, tok::r_paren, ")");
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::RightParen);
  FunctionTypeLayout[Index] = NewRightParen;
  return *this;
}

FunctionTypeSyntaxBuilder &FunctionTypeSyntaxBuilder::
addArgumentTypeSyntax(Optional<RC<TokenSyntax>> MaybeComma,
                      FunctionTypeArgumentSyntax NewTypeArgument) {
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::ArgumentList);
  auto TypeArgumentsLayout = FunctionTypeLayout[Index]->Layout;

  if (MaybeComma.hasValue()) {
    syntax_assert_token_is(MaybeComma.getValue(), tok::comma, ",");
    TypeArgumentsLayout.push_back(MaybeComma.getValue());
  } else {
    if (TypeArgumentsLayout.empty()) {
      TypeArgumentsLayout.push_back(TokenSyntax::missingToken(tok::comma, ","));
    }
  }

  TypeArgumentsLayout.push_back(NewTypeArgument.getRaw());

  FunctionTypeLayout[Index] = RawSyntax::make(SyntaxKind::FunctionParameterList,
                                              TypeArgumentsLayout,
                                              SourcePresence::Present);
  return *this;
}

FunctionTypeSyntaxBuilder &
FunctionTypeSyntaxBuilder::useThrowsKeyword(RC<TokenSyntax> NewThrowsKeyword) {
  syntax_assert_token_is(NewThrowsKeyword, tok::kw_throws, "throws");
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::ThrowsOrRethrows);
  FunctionTypeLayout[Index] = NewThrowsKeyword;
  return *this;
}

FunctionTypeSyntaxBuilder &
FunctionTypeSyntaxBuilder::useRethrowsKeyword(RC<TokenSyntax> NewRethrowsKeyword) {
  syntax_assert_token_is(NewRethrowsKeyword, tok::kw_rethrows, "rethrows");
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::ThrowsOrRethrows);
  FunctionTypeLayout[Index] = NewRethrowsKeyword;
  return *this;
}

FunctionTypeSyntaxBuilder &
FunctionTypeSyntaxBuilder::useArrow(RC<TokenSyntax> NewArrow) {
  syntax_assert_token_is(NewArrow, tok::arrow, "->");
  auto Index = cursorIndex(FunctionTypeSyntax::Cursor::Arrow);
  FunctionTypeLayout[Index] = NewArrow;
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
