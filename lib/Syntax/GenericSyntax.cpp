//===--- GenericSyntax.cpp - Swift Generic Syntax Implementation ----------===//
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

#include "swift/Syntax/GenericSyntax.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/TypeSyntax.h"

using llvm::Optional;
using llvm::None;

using namespace swift;
using namespace swift::syntax;

#pragma mark - conformance-requirement Data

void ConformanceRequirementSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::ConformanceRequirement);
  syntax_assert_child_kind(Raw, Cursor::LeftTypeIdentifier,
    SyntaxKind::TypeIdentifier);
  syntax_assert_child_token_text(Raw, Cursor::Colon,
                                 tok::colon, ":");
  syntax_assert_child_kind(Raw, Cursor::RightTypeIdentifier,
    SyntaxKind::TypeIdentifier);
}

ConformanceRequirementSyntax
ConformanceRequirementSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::ConformanceRequirement,
                             {
                               RawSyntax::missing(SyntaxKind::TypeIdentifier),
                               RawTokenSyntax::missingToken(tok::colon, ":"),
                               RawSyntax::missing(SyntaxKind::TypeIdentifier),
                             },
                             SourcePresence::Present);
  return make<ConformanceRequirementSyntax>(Raw);
}

#pragma mark - generic-parameter API

void GenericParameterSyntax::validate() const {
  assert(Data->Raw->Kind == SyntaxKind::GenericParameter);
}

GenericParameterSyntax GenericParameterSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::GenericParameter,
                             {
                               RawTokenSyntax::missingToken(tok::identifier, ""),
                               RawTokenSyntax::missingToken(tok::colon, ":"),
                               RawSyntax::missing(SyntaxKind::MissingType),
                             },
                             SourcePresence::Present);
  return make<GenericParameterSyntax>(Raw);
}

#pragma mark -
#pragma mark - generic-parameter-clause API

void GenericParameterClauseSyntax::validate() const {
  auto Raw = Data->Raw;
  syntax_assert_child_token_text(Raw, Cursor::LeftAngleBracketToken,
    tok::l_angle, "<");

  syntax_assert_child_kind(Raw, Cursor::GenericParameterList,
    SyntaxKind::GenericParameterList);

  syntax_assert_child_token_text(Raw, Cursor::RightAngleBracketToken,
    tok::r_angle, ">");
}

GenericParameterClauseSyntax
GenericParameterClauseSyntax::makeBlank() {
  auto Raw = RawSyntax::make(
               SyntaxKind::GenericParameterClause,
               {
                 RawTokenSyntax::missingToken(tok::l_angle, "<"),
                 RawSyntax::missing(SyntaxKind::GenericParameterList),
                 RawTokenSyntax::missingToken(tok::r_angle, ">"),
               },
               SourcePresence::Present);
  return make<GenericParameterClauseSyntax>(Raw);
}

#pragma mark - generic-parameter-clause Builder

GenericParameterClauseBuilder::GenericParameterClauseBuilder()
  : LeftAngleToken(RawTokenSyntax::missingToken(tok::l_angle, "<")),
    ParameterListLayout(RawSyntax::missing(SyntaxKind::GenericParameterList)
                          ->Layout),
    RightAngleToken(RawTokenSyntax::missingToken(tok::r_angle, ">")) {}

GenericParameterClauseBuilder &GenericParameterClauseBuilder::
useLeftAngleBracket(TokenSyntax LeftAngle) {
  syntax_assert_token_is(LeftAngle, tok::l_angle, "<");
  LeftAngleToken = LeftAngle.getRawToken();
  return *this;
}

GenericParameterClauseBuilder &GenericParameterClauseBuilder::
addParameter(llvm::Optional<TokenSyntax> MaybeComma,
             GenericParameterSyntax Parameter) {
  if (MaybeComma.hasValue()) {
    syntax_assert_token_is(MaybeComma.getValue(), tok::comma, ",");
    ParameterListLayout.push_back(MaybeComma->getRaw());
  } else {
    ParameterListLayout.push_back(
                            RawTokenSyntax::missingToken(tok::comma, ","));
  }
  ParameterListLayout.push_back(Parameter.getRaw());
  return *this;
}

GenericParameterClauseBuilder &GenericParameterClauseBuilder::
useRightAngleBracket(TokenSyntax RightAngle) {
  syntax_assert_token_is(RightAngle, tok::r_angle, ">");
  RightAngleToken = RightAngle.getRawToken();
  return *this;
}

GenericParameterClauseSyntax GenericParameterClauseBuilder::build() const {
  auto ListRaw = RawSyntax::make(SyntaxKind::GenericParameterList,
                                 ParameterListLayout, SourcePresence::Present);
  auto Raw = RawSyntax::make(SyntaxKind::GenericParameterClause,
                             {
                               LeftAngleToken,
                               ListRaw,
                               RightAngleToken,
                             },
                             SourcePresence::Present);
  auto Data = SyntaxData::make(Raw);
  return { Data, Data.get() };
}

#pragma mark - generic-where-clause API

void GenericWhereClauseSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::GenericWhereClause);
  syntax_assert_child_token_text(Raw,
    GenericWhereClauseSyntax::Cursor::WhereKeyword, tok::kw_where, "where");
  syntax_assert_child_kind(Raw,
    GenericWhereClauseSyntax::Cursor::RequirementList,
    SyntaxKind::GenericRequirementList);
}

GenericWhereClauseSyntax GenericWhereClauseSyntax::makeBlank() {
  auto Raw = RawSyntax::make(
               SyntaxKind::GenericWhereClause,
               {
                 RawTokenSyntax::missingToken(tok::kw_where, "where"),
                 RawSyntax::missing(SyntaxKind::GenericRequirementList),
               },
               SourcePresence::Present);
  return make<GenericWhereClauseSyntax>(Raw);
}

GenericWhereClauseSyntax GenericWhereClauseSyntax::
withWhereKeyword(TokenSyntax NewWhereKeyword) const {
  syntax_assert_token_is(NewWhereKeyword, tok::kw_where, "where");
  return Data->replaceChild<GenericWhereClauseSyntax>(NewWhereKeyword.getRaw(),
                                                      Cursor::WhereKeyword);
}

GenericWhereClauseSyntax GenericWhereClauseSyntax::
withRequirementList(GenericRequirementListSyntax NewRequirements) const {
  return Data->replaceChild<GenericWhereClauseSyntax>(NewRequirements.getRaw(),
                                                      Cursor::RequirementList);
}

#pragma mark - same-type-requirement API

void SameTypeRequirementSyntax::validate() const {
  auto Raw = Data->Raw;
  assert(Raw->Kind == SyntaxKind::SameTypeRequirement);
  assert(Raw->Layout.size() == 3);
  syntax_assert_child_kind(Raw, Cursor::LeftTypeIdentifier,
    SyntaxKind::TypeIdentifier);
  syntax_assert_child_token_text(Raw, Cursor::EqualityToken,
    tok::oper_binary_spaced, "==");
  assert(Raw->getChild(Cursor::RightType)->isType());
}

SameTypeRequirementSyntax SameTypeRequirementSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::SameTypeRequirement,
                             {
                               RawSyntax::missing(SyntaxKind::TypeIdentifier),
                               RawTokenSyntax::missingToken(tok::equal, "="),
                               RawSyntax::missing(SyntaxKind::MissingType),
                             },
                             SourcePresence::Present);
  return make<SameTypeRequirementSyntax>(Raw);
}

#pragma mark - generic-argument-clause API (TODO)

GenericArgumentClauseSyntax
GenericArgumentClauseSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::GenericArgumentClause,
                             {
                               RawTokenSyntax::missingToken(tok::l_angle, "<"),
                               RawSyntax::missing(SyntaxKind::GenericArgumentList),
                               RawTokenSyntax::missingToken(tok::r_angle, ">"),
                             },
                             SourcePresence::Present);
  return make<GenericArgumentClauseSyntax>(Raw);
}


GenericArgumentClauseSyntax GenericArgumentClauseSyntax::
withLeftAngleBracket(TokenSyntax NewLeftAngleBracket) const {
  syntax_assert_token_is(NewLeftAngleBracket, tok::l_angle, "<");
  return Data->replaceChild<GenericArgumentClauseSyntax>(
    NewLeftAngleBracket.getRaw(), Cursor::LeftAngleBracketToken);
}



GenericArgumentClauseSyntax GenericArgumentClauseSyntax::
withRightAngleBracket(TokenSyntax NewRightAngleBracket) const {
  syntax_assert_token_is(NewRightAngleBracket, tok::r_angle, ">");
  return Data->replaceChild<GenericArgumentClauseSyntax>(
    NewRightAngleBracket.getRaw(), Cursor::RightAngleBracketToken);
}

#pragma mark - generic-argument-clause Builder

GenericArgumentClauseBuilder::GenericArgumentClauseBuilder()
  : ArgumentListLayout(
      SyntaxFactory::makeBlankGenericArgumentClause().getRaw()->Layout) {}

GenericArgumentClauseBuilder &GenericArgumentClauseBuilder::
useLeftAngleBracket(TokenSyntax LeftAngle) {
  syntax_assert_token_is(LeftAngle, tok::l_angle, "<");
  LeftAngleToken = LeftAngle.getRawToken();
  return *this;
}

GenericArgumentClauseBuilder &GenericArgumentClauseBuilder::
addGenericArgument(llvm::Optional<TokenSyntax> MaybeComma,
                   TypeSyntax ArgumentTypeSyntax) {
  if (MaybeComma.hasValue()) {
    syntax_assert_token_is(MaybeComma.getValue(), tok::comma, ",");
    ArgumentListLayout.push_back(MaybeComma->getRaw());
  } else {
    ArgumentListLayout.push_back(
       RawTokenSyntax::missingToken(tok::comma, ","));
  }
  ArgumentListLayout.push_back(ArgumentTypeSyntax.getRaw());
  return *this;
}

GenericArgumentClauseBuilder &GenericArgumentClauseBuilder::
useRightAngleBracket(TokenSyntax RightAngle) {
  syntax_assert_token_is(RightAngle, tok::r_angle, ">");
  RightAngleToken = RightAngle.getRawToken();
  return *this;
}


GenericArgumentClauseSyntax GenericArgumentClauseBuilder::build() const {
  auto ArgListRaw = RawSyntax::make(SyntaxKind::GenericParameterList,
                                    ArgumentListLayout,
                                    SourcePresence::Present);
  auto Raw = RawSyntax::make(SyntaxKind::GenericArgumentClause,
                             {
                               LeftAngleToken,
                               ArgListRaw,
                               RightAngleToken
                             },
                             SourcePresence::Present);
  auto Data = SyntaxData::make(Raw);
  return { Data, Data.get() };
}
