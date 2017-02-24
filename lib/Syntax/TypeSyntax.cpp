//===--- TypeSyntax.cpp - Swift Type Syntax Implementation ------*- C++ -*-===//
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

TypeSyntaxData::TypeSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent,
                               CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {}

TypeSyntax::TypeSyntax(RC<SyntaxData> Root, const TypeSyntaxData *Data)
  : Syntax(Root, Data) {}

#pragma mark - balanced-tokens Data

BalancedTokensSyntaxData::BalancedTokensSyntaxData(RC<RawSyntax> Raw,
                                                   const SyntaxData *Parent,
                                                   CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::BalancedTokens);
  // TODO: Add some checks here that each element of raw syntax
  // matches the grammar rules in the doc comment of
  // this class.
}

RC<BalancedTokensSyntaxData>
BalancedTokensSyntaxData::make(RC<RawSyntax> Raw,
                               const SyntaxData *Parent,
                               CursorIndex IndexInParent) {
  return RC<BalancedTokensSyntaxData> {
    new BalancedTokensSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<BalancedTokensSyntaxData> BalancedTokensSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::BalancedTokens, {},
                              SourcePresence::Present));
}

#pragma mark - balanced-tokens API

BalancedTokensSyntax::BalancedTokensSyntax(RC<SyntaxData> Root,
                                           const BalancedTokensSyntaxData *Data)
  : Syntax(Root, Data) {}

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

TypeAttributeSyntaxData::TypeAttributeSyntaxData(RC<RawSyntax> Raw,
                                                 const SyntaxData *Parent,
                                                 CursorIndex IndexInParent)
    : SyntaxData(Raw, Parent, IndexInParent) {
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

RC<TypeAttributeSyntaxData>
TypeAttributeSyntaxData::make(RC<RawSyntax> Raw,
                              const SyntaxData *Parent,
                              CursorIndex IndexInParent) {
  return RC<TypeAttributeSyntaxData> {
    new TypeAttributeSyntaxData { Raw, Parent, IndexInParent }
  };
}

#pragma mark - type-attribute API

TypeAttributeSyntax::TypeAttributeSyntax(RC<SyntaxData> Root,
                                         const TypeAttributeSyntaxData *Data)
  : Syntax(Root, Data) {}

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

#pragma mark - type-attributes Data

TypeAttributesSyntaxData::TypeAttributesSyntaxData(RC<RawSyntax> Raw,
                                                   const SyntaxData *Parent,
                                                   CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  // TODO: kind assertions
}

RC<TypeAttributesSyntaxData>
TypeAttributesSyntaxData::make(RC<RawSyntax> Raw,
                               const SyntaxData *Parent,
                               CursorIndex IndexInParent) {
  return RC<TypeAttributesSyntaxData> {
    new TypeAttributesSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<TypeAttributesSyntaxData> TypeAttributesSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::TypeAttributes, {},
                              SourcePresence::Present));
}

#pragma mark - type-attributes API

TypeAttributesSyntax::
TypeAttributesSyntax(RC<SyntaxData> Root, const TypeAttributesSyntaxData *Data)
  : Syntax(Root, Data) {}

TypeAttributesSyntax TypeAttributesSyntax::
addTypeAttribute(TypeAttributeSyntax NewTypeAttribute) const {
  auto Layout = getRaw()->Layout;
  Layout.push_back(NewTypeAttribute.getRaw());
  auto NewRaw = RawSyntax::make(SyntaxKind::TypeAttributes, Layout,
                                SourcePresence::Present);
  return Data->replaceSelf<TypeAttributesSyntax>(NewRaw);
}

#pragma mark - type-identifier Data

TypeIdentifierSyntaxData::TypeIdentifierSyntaxData(RC<RawSyntax> Raw,
                                                   const SyntaxData *Parent,
                                                   CursorIndex IndexInParent)
    : TypeSyntaxData(Raw, Parent, IndexInParent) {
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

RC<TypeIdentifierSyntaxData>
TypeIdentifierSyntaxData::make(RC<RawSyntax> Raw,
                               const SyntaxData *Parent,
                               CursorIndex IndexInParent) {
  return RC<TypeIdentifierSyntaxData> {
    new TypeIdentifierSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<TypeIdentifierSyntaxData>
TypeIdentifierSyntaxData::makeBlank() {
  return make(RawSyntax::make(
    SyntaxKind::TypeIdentifier,
    {
      TokenSyntax::missingToken(tok::identifier, ""),
      RawSyntax::missing(SyntaxKind::GenericArgumentClause),
      TokenSyntax::missingToken(tok::period, "."),
      RawSyntax::missing(SyntaxKind::TypeIdentifier),
    },
    SourcePresence::Present));
}

#pragma mark - type-identifier API

TypeIdentifierSyntax::TypeIdentifierSyntax(RC<SyntaxData> Root,
                                           const TypeIdentifierSyntaxData *Data)
  : TypeSyntax(Root, Data) {}

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

#pragma mark - type-argument-list Data

TypeArgumentListSyntaxData::
TypeArgumentListSyntaxData(RC<RawSyntax> Raw,
                           const SyntaxData *Parent,
                           CursorIndex IndexInParent)
    : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::TypeArgumentList);
#ifndef NDEBUG
  for (size_t i = 0; i < Raw->Layout.size(); ++i) {
    if (i % 2 == 0) {
      assert(Raw->getChild(i)->Kind == SyntaxKind::TupleTypeElement);
    } else {
      syntax_assert_token_is(cast<TokenSyntax>(Raw->getChild(i)), tok::comma,
                             ",");
    }
  }
#endif
}

RC<TypeArgumentListSyntaxData>
TypeArgumentListSyntaxData::make(RC<RawSyntax> Raw,
                                 const SyntaxData *Parent,
                                 CursorIndex IndexInParent) {
  return RC<TypeArgumentListSyntaxData> {
    new TypeArgumentListSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<TypeArgumentListSyntaxData>
TypeArgumentListSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::TypeArgumentList, {},
                              SourcePresence::Present));
}

#pragma mark - type-argument-list API

TypeArgumentListSyntax::
TypeArgumentListSyntax(RC<SyntaxData> Root,
                       const TypeArgumentListSyntaxData *Data)
  : Syntax(Root, Data) {}

TypeArgumentListSyntax TypeArgumentListSyntax::addType(
    Optional<RC<TokenSyntax>> MaybeComma,
    TupleTypeElementSyntax NewTypeArgument) const {
  auto Layout = getRaw()->Layout;

  if (MaybeComma.hasValue()) {
    syntax_assert_token_is(MaybeComma.getValue(), tok::comma, ",");
    Layout.push_back(MaybeComma.getValue());
  }

  Layout.push_back(NewTypeArgument.getRaw());

  auto NewRaw = RawSyntax::make(SyntaxKind::TypeArgumentList, Layout,
                                SourcePresence::Present);
  return Data->replaceSelf<TypeArgumentListSyntax>(NewRaw);
}

#pragma mark - tuple-type Data

TupleTypeSyntaxData::TupleTypeSyntaxData(RC<RawSyntax> Raw,
                                         const SyntaxData *Parent,
                                         CursorIndex IndexInParent)
    : TypeSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::TupleType);
  assert(Raw->Layout.size() == 3);
  syntax_assert_child_token_text(Raw, TupleTypeSyntax::Cursor::LeftParenToken,
                                 tok::l_paren,
                                 "(");
  syntax_assert_child_kind(Raw, TupleTypeSyntax::Cursor::TypeArgumentList,
                           SyntaxKind::TypeArgumentList);
  syntax_assert_child_token_text(Raw, TupleTypeSyntax::Cursor::RightParenToken,
                                 tok::r_paren,
                                 ")");
}

RC<TupleTypeSyntaxData>
TupleTypeSyntaxData::make(RC<RawSyntax> Raw,
                          const SyntaxData *Parent,
                          CursorIndex IndexInParent) {
  return RC<TupleTypeSyntaxData> {
    new TupleTypeSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<TupleTypeSyntaxData>
TupleTypeSyntaxData::makeBlank() {
  return make(
      RawSyntax::make(SyntaxKind::TupleType,
                      {
                        TokenSyntax::missingToken(tok::l_paren, "("),
                        RawSyntax::missing(SyntaxKind::TypeArgumentList),
                        TokenSyntax::missingToken(tok::r_paren, ")"),
                      },
                      SourcePresence::Present));
}

#pragma mark - tuple-type API

TupleTypeSyntax::TupleTypeSyntax(RC<SyntaxData> Root,
                                 const TupleTypeSyntaxData *Data)
  : TypeSyntax(Root, Data) {}

TupleTypeSyntax
TupleTypeSyntax::withLeftParen(RC<TokenSyntax> NewLeftParen) const {
  syntax_assert_token_is(NewLeftParen, tok::l_paren, "(");
  auto NewRaw = getRaw()->replaceChild(Cursor::LeftParenToken, NewLeftParen);
  return Data->replaceSelf<TupleTypeSyntax>(NewRaw);
}

TupleTypeSyntax TupleTypeSyntax::
withTypeArgumentList(TypeArgumentListSyntax NewTypeArgumentList) const {
  auto NewRaw = getRaw()->replaceChild(Cursor::TypeArgumentList,
                                       NewTypeArgumentList.getRaw());
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
      SyntaxFactory::makeBlankTypeArgumentList().getRaw()->Layout) {}

TupleTypeSyntaxBuilder &TupleTypeSyntaxBuilder::
addElementTypeSyntax(llvm::Optional<RC<TokenSyntax>> MaybeComma,
                     TupleTypeElementSyntax ElementTypeSyntax) {
  if (MaybeComma.hasValue()) {
    syntax_assert_token_is(MaybeComma.getValue(), tok::comma, ",");
    ElementTypeLayout.push_back(MaybeComma.getValue());
  } else {
    ElementTypeLayout.push_back(TokenSyntax::missingToken(tok::comma, ","));
  }
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
  auto ElementsRaw = RawSyntax::make(SyntaxKind::TypeArgumentList,
                                     { ElementTypeLayout },
                                     SourcePresence::Present);
  auto Raw = RawSyntax::make(SyntaxKind::TupleType,
                             {
                               LeftParenToken,
                               ElementsRaw,
                               RightParenToken,
                             },
                             SourcePresence::Present);
  auto Data = TupleTypeSyntaxData::make(Raw);
  return { Data, Data.get() };
}

#pragma mark - tuple-type-element Data

TupleTypeElementSyntaxData::
TupleTypeElementSyntaxData(RC<RawSyntax> Raw,
                           const SyntaxData *Parent,
                           CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::TupleTypeElement);
  assert(Raw->Layout.size() == 5);
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
  assert(Raw->getChild(TupleTypeElementSyntax::Cursor::Type)->isType());
}

RC<TupleTypeElementSyntaxData>
TupleTypeElementSyntaxData::make(RC<RawSyntax> Raw,
                                 const SyntaxData *Parent,
                                 CursorIndex IndexInParent) {
  return RC<TupleTypeElementSyntaxData> {
    new TupleTypeElementSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<TupleTypeElementSyntaxData>
TupleTypeElementSyntaxData::makeBlank() {
  return make(
      RawSyntax::make(SyntaxKind::TupleTypeElement,
                      {
                        TokenSyntax::missingToken(tok::identifier, ""),
                        TokenSyntax::missingToken(tok::colon, ":"),
                        RawSyntax::missing(SyntaxKind::TypeAttributes),
                        TokenSyntax::missingToken(tok::kw_inout, "inout"),
                        RawSyntax::missing(SyntaxKind::MissingType),
                      },
                      SourcePresence::Present));
}

#pragma mark - tuple-type-element API

TupleTypeElementSyntax::
TupleTypeElementSyntax(RC<SyntaxData> Root,
                       const TupleTypeElementSyntaxData *Data)
  : Syntax(Root, Data) {}

TupleTypeElementSyntax
TupleTypeElementSyntax::withLabel(RC<TokenSyntax> NewLabel) const {
  assert(NewLabel->getTokenKind() == tok::identifier);
  auto NewRaw = getRaw()->replaceChild(Cursor::Label, NewLabel);
  return Data->replaceSelf<TupleTypeElementSyntax>(NewRaw);
}

TupleTypeElementSyntax
TupleTypeElementSyntax::withColonToken(RC<TokenSyntax> NewColonToken) const {
  syntax_assert_token_is(NewColonToken, tok::colon, ":")
  auto NewRaw = getRaw()->replaceChild(Cursor::ColonToken, NewColonToken);
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

#pragma mark - metatype-type Data

MetatypeTypeSyntaxData::MetatypeTypeSyntaxData(RC<RawSyntax> Raw,
                                               const SyntaxData *Parent,
                                               CursorIndex IndexInParent)
    : TypeSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::MetatypeType);
  assert(Raw->Layout.size() == 3);
  assert(Raw->getChild(MetatypeTypeSyntax::Cursor::BaseType)->isType());
  syntax_assert_child_token_text(Raw, MetatypeTypeSyntax::Cursor::DotToken,
                                 tok::period, ".");
  syntax_assert_child_token(Raw, MetatypeTypeSyntax::Cursor::TypeToken,
                            tok::identifier);
}

RC<MetatypeTypeSyntaxData>
MetatypeTypeSyntaxData::make(RC<RawSyntax> Raw,
                             const SyntaxData *Parent,
                             CursorIndex IndexInParent) {
  return RC<MetatypeTypeSyntaxData> {
    new MetatypeTypeSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<MetatypeTypeSyntaxData> MetatypeTypeSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::MetatypeType,
                              {
                                RawSyntax::missing(SyntaxKind::MissingType),
                                TokenSyntax::missingToken(tok::period, "."),
                                TokenSyntax::missingToken(tok::identifier, ""),
                              },
                              SourcePresence::Present));
}

#pragma mark - metatype-type API

MetatypeTypeSyntax::MetatypeTypeSyntax(RC<SyntaxData> Root,
                                       const MetatypeTypeSyntaxData *Data)
  : TypeSyntax(Root, Data) {}

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

#pragma mark - optional-type Data

OptionalTypeSyntaxData::OptionalTypeSyntaxData(RC<RawSyntax> Raw,
                                               const SyntaxData *Parent,
                                               CursorIndex IndexInParent)
    : TypeSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::OptionalType);
  assert(Raw->Layout.size() == 2);
  assert(Raw->getChild(OptionalTypeSyntax::Cursor::BaseType)->isType());
  syntax_assert_child_token_text(Raw, OptionalTypeSyntax::Cursor::QuestionToken,
                                 tok::question_postfix, "?");
}

RC<OptionalTypeSyntaxData>
OptionalTypeSyntaxData::make(RC<RawSyntax> Raw,
                             const SyntaxData *Parent,
                             CursorIndex IndexInParent) {
  return RC<OptionalTypeSyntaxData> {
    new OptionalTypeSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<OptionalTypeSyntaxData> OptionalTypeSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::OptionalType,
    {
      RawSyntax::missing(SyntaxKind::MissingType),
      TokenSyntax::missingToken(tok::question_postfix, "?"),
    },
    SourcePresence::Present));
}

#pragma mark - optional-type API

OptionalTypeSyntax::OptionalTypeSyntax(RC<SyntaxData> Root,
                                       const OptionalTypeSyntaxData *Data)
  : TypeSyntax(Root, Data) {}

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

#pragma mark - implicitly-unwrapped-optional-type Data

ImplicitlyUnwrappedOptionalTypeSyntaxData::
ImplicitlyUnwrappedOptionalTypeSyntaxData(RC<RawSyntax> Raw,
                                          const SyntaxData *Parent,
                                          CursorIndex IndexInParent)
    : TypeSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::ImplicitlyUnwrappedOptionalType);
  assert(Raw->Layout.size() == 2);
  assert(Raw->getChild(ImplicitlyUnwrappedOptionalTypeSyntax::Cursor::Type)
           ->isType());
  syntax_assert_child_token_text(Raw,
    ImplicitlyUnwrappedOptionalTypeSyntax::Cursor::ExclaimToken,
    tok::exclaim_postfix, "!");
}

RC<ImplicitlyUnwrappedOptionalTypeSyntaxData>
ImplicitlyUnwrappedOptionalTypeSyntaxData::make(RC<RawSyntax> Raw,
                                                const SyntaxData *Parent,
                                                CursorIndex IndexInParent) {
  return RC<ImplicitlyUnwrappedOptionalTypeSyntaxData> {
    new ImplicitlyUnwrappedOptionalTypeSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<ImplicitlyUnwrappedOptionalTypeSyntaxData>
ImplicitlyUnwrappedOptionalTypeSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::ImplicitlyUnwrappedOptionalType,
    {
      RawSyntax::missing(SyntaxKind::MissingType),
      TokenSyntax::missingToken(tok::exclaim_postfix, "!"),
    },
    SourcePresence::Present));
}

#pragma mark - implicitly-unwrapped-optional-type API

ImplicitlyUnwrappedOptionalTypeSyntax::
ImplicitlyUnwrappedOptionalTypeSyntax(RC<SyntaxData> Root,
    const ImplicitlyUnwrappedOptionalTypeSyntaxData *Data)
  : TypeSyntax(Root, Data) {}

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

#pragma mark - array-type Data

ArrayTypeSyntaxData::ArrayTypeSyntaxData(RC<RawSyntax> Raw,
                                         const SyntaxData *Parent,
                                         CursorIndex IndexInParent)
    : TypeSyntaxData(Raw, Parent, IndexInParent) {
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

RC<ArrayTypeSyntaxData> ArrayTypeSyntaxData::make(RC<RawSyntax> Raw,
                                                  const SyntaxData *Parent,
                                                  CursorIndex IndexInParent) {
  return RC<ArrayTypeSyntaxData> {
    new ArrayTypeSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<ArrayTypeSyntaxData> ArrayTypeSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::ArrayType,
                              {
                                TokenSyntax::missingToken(tok::l_square, "["),
                                RawSyntax::missing(SyntaxKind::MissingType),
                                TokenSyntax::missingToken(tok::r_square, "]"),
                              },
                              SourcePresence::Present));
}

#pragma mark - array-type API

ArrayTypeSyntax::ArrayTypeSyntax(RC<SyntaxData> Root,
                                 const ArrayTypeSyntaxData *Data)
  : TypeSyntax(Root, Data) {}

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

#pragma mark - dictionary-type Data

DictionaryTypeSyntaxData::DictionaryTypeSyntaxData(RC<RawSyntax> Raw,
                                                   const SyntaxData *Parent,
                                                   CursorIndex IndexInParent)
    : TypeSyntaxData(Raw, Parent, IndexInParent) {
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

RC<DictionaryTypeSyntaxData>
DictionaryTypeSyntaxData::make(RC<RawSyntax> Raw,
                               const SyntaxData *Parent,
                               CursorIndex IndexInParent) {
  return RC<DictionaryTypeSyntaxData> {
    new DictionaryTypeSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<DictionaryTypeSyntaxData>
DictionaryTypeSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::DictionaryType,
                              {
                                TokenSyntax::missingToken(tok::l_square, "["),
                                RawSyntax::missing(SyntaxKind::MissingType),
                                TokenSyntax::missingToken(tok::colon, ":"),
                                RawSyntax::missing(SyntaxKind::MissingType),
                                TokenSyntax::missingToken(tok::r_square, "]"),
                              },
                              SourcePresence::Present));
}

#pragma mark - dictionary-type API

DictionaryTypeSyntax::DictionaryTypeSyntax(RC<SyntaxData> Root,
                                           const DictionaryTypeSyntaxData *Data)
  : TypeSyntax(Root, Data) {}

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

#pragma mark - function-type-argument Data

FunctionTypeArgumentSyntaxData::
FunctionTypeArgumentSyntaxData(RC<RawSyntax> Raw,
                               const SyntaxData *Parent,
                               CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {}

RC<FunctionTypeArgumentSyntaxData>
FunctionTypeArgumentSyntaxData::make(RC<RawSyntax> Raw,
                                     const SyntaxData *Parent,
                                     CursorIndex IndexInParent) {
  return RC<FunctionTypeArgumentSyntaxData> {
    new FunctionTypeArgumentSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<FunctionTypeArgumentSyntaxData>
FunctionTypeArgumentSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionTypeArgument,
                             {
                               TokenSyntax::missingToken(tok::identifier, ""),
                               TokenSyntax::missingToken(tok::identifier, ""),
                               TokenSyntax::missingToken(tok::colon, ","),
                             },
                             SourcePresence::Present);
  return make(Raw);
}

#pragma mark - function-type-argument API

FunctionTypeArgumentSyntax::
FunctionTypeArgumentSyntax(RC<SyntaxData> Root,
                           const FunctionTypeArgumentSyntaxData *Data)
  : Syntax(Root, Data) {}

#pragma mark - function-type Data

FunctionTypeSyntaxData::FunctionTypeSyntaxData(RC<RawSyntax> Raw,
                                               const SyntaxData *Parent,
                                               CursorIndex IndexInParent)
  : TypeSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::FunctionType);
  syntax_assert_child_kind(Raw, FunctionTypeSyntax::Cursor::TypeAttributes,
                           SyntaxKind::TypeAttributes);
  syntax_assert_child_token_text(Raw, FunctionTypeSyntax::Cursor::LeftParen,
                                 tok::l_paren, "(");
  syntax_assert_child_kind(Raw, FunctionTypeSyntax::Cursor::ArgumentList,
                           SyntaxKind::TypeArgumentList);
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

RC<FunctionTypeSyntaxData>
FunctionTypeSyntaxData::make(RC<RawSyntax> Raw,
                             const SyntaxData *Parent,
                             CursorIndex IndexInParent) {
  return RC<FunctionTypeSyntaxData> {
    new FunctionTypeSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<FunctionTypeSyntaxData> FunctionTypeSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::FunctionType,
    {
      RawSyntax::missing(SyntaxKind::TypeAttributes),
      TokenSyntax::missingToken(tok::l_paren, "("),
      RawSyntax::missing(SyntaxKind::TypeArgumentList),
      TokenSyntax::missingToken(tok::r_paren, ")"),
      TokenSyntax::missingToken(tok::kw_throws, "throws"),
      TokenSyntax::missingToken(tok::arrow, "->"),
      RawSyntax::missing(SyntaxKind::MissingType),
    },
    SourcePresence::Present));
}

#pragma mark - function-type API

FunctionTypeSyntax::FunctionTypeSyntax(RC<SyntaxData> Root,
                                       const FunctionTypeSyntaxData *Data)
  : TypeSyntax(Root, Data) {}

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

  FunctionTypeLayout[Index] = RawSyntax::make(SyntaxKind::TypeArgumentList,
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
  auto Data = FunctionTypeSyntaxData::make(Raw);
  return { Data, Data.get() };
}
