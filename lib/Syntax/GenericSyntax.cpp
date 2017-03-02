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

ConformanceRequirementSyntaxData::
ConformanceRequirementSyntaxData(RC<RawSyntax> Raw,
                                 const SyntaxData *Parent,
                                 CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::ConformanceRequirement);
  syntax_assert_child_kind(Raw,
    ConformanceRequirementSyntax::Cursor::LeftTypeIdentifier,
    SyntaxKind::TypeIdentifier);
  syntax_assert_child_token_text(Raw,
                                 ConformanceRequirementSyntax::Cursor::Colon,
                                 tok::colon, ":");
  syntax_assert_child_kind(Raw,
    ConformanceRequirementSyntax::Cursor::RightTypeIdentifier,
    SyntaxKind::TypeIdentifier);
}

RC<ConformanceRequirementSyntaxData>
ConformanceRequirementSyntaxData::make(RC<RawSyntax> Raw,
                                      const SyntaxData *Parent,
                                      CursorIndex IndexInParent) {
  return RC<ConformanceRequirementSyntaxData> {
    new ConformanceRequirementSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<ConformanceRequirementSyntaxData>
ConformanceRequirementSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::ConformanceRequirement,
                             {
                               RawSyntax::missing(SyntaxKind::TypeIdentifier),
                               TokenSyntax::missingToken(tok::colon, ":"),
                               RawSyntax::missing(SyntaxKind::TypeIdentifier),
                             },
                             SourcePresence::Present);
  return make(Raw);
}


#pragma mark - generic-parameter Data

GenericParameterSyntaxData::
GenericParameterSyntaxData(RC<RawSyntax> Raw,
                           const SyntaxData *Parent,
                           CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::GenericParameter);
}

RC<GenericParameterSyntaxData>
GenericParameterSyntaxData::make(RC<RawSyntax> Raw,
                                 const SyntaxData *Parent,
                                 CursorIndex IndexInParent) {
  return RC<GenericParameterSyntaxData> {
    new GenericParameterSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<GenericParameterSyntaxData> GenericParameterSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::GenericParameter,
                              {
                                TokenSyntax::missingToken(tok::identifier, ""),
                                TokenSyntax::missingToken(tok::colon, ":"),
                                RawSyntax::missing(SyntaxKind::MissingType),
                              },
                              SourcePresence::Present));
}

#pragma mark - generic-parameter API

GenericParameterSyntax::
GenericParameterSyntax(RC<SyntaxData> Root,
                       const GenericParameterSyntaxData *Data)
  : Syntax(Root, Data) {}

#pragma mark - generic-parameter-list Data

GenericParameterListSyntaxData::
GenericParameterListSyntaxData(RC<RawSyntax> Raw,
                               const SyntaxData *Parent,
                               CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::GenericParameterList);
}

RC<GenericParameterListSyntaxData>
GenericParameterListSyntaxData::make(RC<RawSyntax> Raw,
                                     const SyntaxData *Parent,
                                     CursorIndex IndexInParent) {
  return RC<GenericParameterListSyntaxData> {
    new GenericParameterListSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<GenericParameterListSyntaxData> GenericParameterListSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::GenericParameterList,
                             {}, SourcePresence::Present);
  return make(Raw);
}

#pragma mark -
#pragma mark generic-parameter-clause Data

GenericParameterClauseSyntaxData::
GenericParameterClauseSyntaxData(RC<RawSyntax> Raw,
                                 const SyntaxData *Parent,
                                 CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  syntax_assert_child_token_text(Raw,
    GenericParameterClauseSyntax::Cursor::LeftAngleBracketToken,
    tok::l_angle, "<");

  syntax_assert_child_kind(Raw,
    GenericParameterClauseSyntax::Cursor::GenericParameterList,
    SyntaxKind::GenericParameterList);

  syntax_assert_child_token_text(Raw,
    GenericParameterClauseSyntax::Cursor::RightAngleBracketToken,
    tok::r_angle, ">");
}

RC<GenericParameterClauseSyntaxData>
GenericParameterClauseSyntaxData::make(const RC<RawSyntax> Raw,
                                       const SyntaxData *Parent,
                                       const CursorIndex IndexInParent) {
  return RC<GenericParameterClauseSyntaxData> {
    new GenericParameterClauseSyntaxData {
      Raw, Parent, IndexInParent,
    }
  };
}

RC<GenericParameterClauseSyntaxData>
GenericParameterClauseSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::GenericParameterClause,
    {
      TokenSyntax::missingToken(tok::l_angle, "<"),
      RawSyntax::missing(SyntaxKind::GenericParameterList),
      TokenSyntax::missingToken(tok::r_angle, ">"),
    },
    SourcePresence::Present));
}

#pragma mark - generic-parameter-clause API

GenericParameterClauseSyntax::
GenericParameterClauseSyntax(const RC<SyntaxData> Root,
                             const GenericParameterClauseSyntaxData *Data)
  : Syntax(Root, Data) {}

#pragma mark - generic-parameter-clause Builder

GenericParameterClauseBuilder::GenericParameterClauseBuilder()
  : LeftAngleToken(TokenSyntax::missingToken(tok::l_angle, "<")),
    ParameterListLayout(RawSyntax::missing(SyntaxKind::GenericParameterList)
                          ->Layout),
    RightAngleToken(TokenSyntax::missingToken(tok::r_angle, ">")) {}

GenericParameterClauseBuilder &GenericParameterClauseBuilder::
useLeftAngleBracket(RC<TokenSyntax> LeftAngle) {
  syntax_assert_token_is(LeftAngle, tok::l_angle, "<");
  LeftAngleToken = LeftAngle;
  return *this;
}

GenericParameterClauseBuilder &GenericParameterClauseBuilder::
addParameter(llvm::Optional<RC<TokenSyntax>> MaybeComma,
             GenericParameterSyntax Parameter) {
  if (MaybeComma.hasValue()) {
    syntax_assert_token_is(MaybeComma.getValue(), tok::comma, ",");
    ParameterListLayout.push_back(MaybeComma.getValue());
  } else {
    ParameterListLayout.push_back(TokenSyntax::missingToken(tok::comma, ","));
  }
  ParameterListLayout.push_back(Parameter.getRaw());
  return *this;
}

GenericParameterClauseBuilder &GenericParameterClauseBuilder::
useRightAngleBracket(RC<TokenSyntax> RightAngle) {
  syntax_assert_token_is(RightAngle, tok::r_angle, ">");
  RightAngleToken = RightAngle;
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
  auto Data = GenericParameterClauseSyntaxData::make(Raw);
  return { Data, Data.get() };
}

#pragma mark - generic-requirement-list Data

GenericRequirementListSyntaxData::
GenericRequirementListSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent,
                                 CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::GenericRequirementList);
}

RC<GenericRequirementListSyntaxData>
GenericRequirementListSyntaxData::make(RC<RawSyntax> Raw,
                                       const SyntaxData *Parent,
                                       CursorIndex IndexInParent) {
  return RC<GenericRequirementListSyntaxData> {
    new GenericRequirementListSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<GenericRequirementListSyntaxData>
GenericRequirementListSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::GenericRequirementList,
                             {},
                             SourcePresence::Present);
  return make(Raw);
}

#pragma mark - generic-where-clause Data

GenericWhereClauseSyntaxData::
GenericWhereClauseSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent,
                             CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::GenericWhereClause);
  syntax_assert_child_token_text(Raw,
    GenericWhereClauseSyntax::Cursor::WhereToken, tok::kw_where, "where");
  syntax_assert_child_kind(Raw,
    GenericWhereClauseSyntax::Cursor::RequirementList,
    SyntaxKind::GenericRequirementList);
}

RC<GenericWhereClauseSyntaxData>
GenericWhereClauseSyntaxData::make(RC<RawSyntax> Raw,
                                   const SyntaxData *Parent,
                                   CursorIndex IndexInParent) {
  return RC<GenericWhereClauseSyntaxData> {
    new GenericWhereClauseSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<GenericWhereClauseSyntaxData> GenericWhereClauseSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::GenericWhereClause,
    {
      TokenSyntax::missingToken(tok::kw_where, "where"),
      RawSyntax::missing(SyntaxKind::GenericRequirementList),
    },
    SourcePresence::Present));
}

#pragma mark - generic-where-clause API

GenericWhereClauseSyntax::
GenericWhereClauseSyntax(RC<SyntaxData> Root,
                         const GenericWhereClauseSyntaxData *Data)
  : Syntax(Root, Data) {}

#pragma mark - same-type-requirement Data

SameTypeRequirementSyntaxData::
SameTypeRequirementSyntaxData(RC<RawSyntax> Raw,
                              const SyntaxData *Parent,
                              CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::SameTypeRequirement);
  assert(Raw->Layout.size() == 3);
  syntax_assert_child_kind(Raw,
    SameTypeRequirementSyntax::Cursor::LeftTypeIdentifier,
    SyntaxKind::TypeIdentifier);
  syntax_assert_child_token_text(Raw,
    SameTypeRequirementSyntax::Cursor::EqualityToken, tok::identifier, "==");
  assert(Raw->getChild(SameTypeRequirementSyntax::Cursor::RightType)->isType());
}

RC<SameTypeRequirementSyntaxData>
SameTypeRequirementSyntaxData::make(RC<RawSyntax> Raw,
                                    const SyntaxData *Parent,
                                    CursorIndex IndexInParent) {
  return RC<SameTypeRequirementSyntaxData> {
    new SameTypeRequirementSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<SameTypeRequirementSyntaxData> SameTypeRequirementSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::SameTypeRequirement,
                              {
                                RawSyntax::missing(SyntaxKind::TypeIdentifier),
                                TokenSyntax::missingToken(tok::equal, "="),
                                RawSyntax::missing(SyntaxKind::MissingType),
                              },
                              SourcePresence::Present));
}

#pragma mark - same-type-requirement API

SameTypeRequirementSyntax::
SameTypeRequirementSyntax(RC<SyntaxData> Root,
                          const SameTypeRequirementSyntaxData *Data)
  : Syntax(Root, Data) {}

#pragma mark - generic-argument-list Data

GenericArgumentListSyntaxData::
GenericArgumentListSyntaxData(RC<RawSyntax> Raw,
                              const SyntaxData *Parent,
                              CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {
    assert(Raw->Kind == SyntaxKind::GenericArgumentList);
}

RC<GenericArgumentListSyntaxData>
GenericArgumentListSyntaxData::make(RC<RawSyntax> Raw, const SyntaxData *Parent,
                                    CursorIndex IndexInParent) {
  return RC<GenericArgumentListSyntaxData> {
    new GenericArgumentListSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<GenericArgumentListSyntaxData> GenericArgumentListSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::GenericArgumentList, {},
                             SourcePresence::Present);
  return make(Raw);
}


#pragma mark - generic-argument-list API (TODO)

#pragma mark - generic-argument-clause Data

GenericArgumentClauseSyntaxData::
GenericArgumentClauseSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent,
                                CursorIndex IndexInParent)
  : SyntaxData(Raw, Parent, IndexInParent) {}

RC<GenericArgumentClauseSyntaxData>
GenericArgumentClauseSyntaxData::make(RC<RawSyntax> Raw,
                                      const SyntaxData *Parent,
                                      CursorIndex IndexInParent) {
  return RC<GenericArgumentClauseSyntaxData> {
    new GenericArgumentClauseSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<GenericArgumentClauseSyntaxData>
GenericArgumentClauseSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::GenericArgumentClause,
                             {
                               TokenSyntax::missingToken(tok::l_angle, "<"),
                               RawSyntax::missing(SyntaxKind::TypeArgumentList),
                               TokenSyntax::missingToken(tok::r_angle, ">"),
                             },
                             SourcePresence::Present);
  return make(Raw);
}

#pragma mark - generic-argument-clause API

GenericArgumentClauseSyntax::
GenericArgumentClauseSyntax(RC<SyntaxData> Root,
                            const GenericArgumentClauseSyntaxData *Data)
  : Syntax(Root, Data) {}

GenericArgumentClauseSyntax GenericArgumentClauseSyntax::
withLeftAngleBracket(RC<TokenSyntax> NewLeftAngleBracket) const {
  syntax_assert_token_is(NewLeftAngleBracket, tok::l_angle, "<");
  return Data->replaceChild<GenericArgumentClauseSyntax>(
    NewLeftAngleBracket, Cursor::LeftAngleBracketToken);
}



GenericArgumentClauseSyntax GenericArgumentClauseSyntax::
withRightAngleBracket(RC<TokenSyntax> NewRightAngleBracket) const {
  syntax_assert_token_is(NewRightAngleBracket, tok::r_angle, ">");
  return Data->replaceChild<GenericArgumentClauseSyntax>(
    NewRightAngleBracket, Cursor::RightAngleBracketToken);
}

#pragma mark - generic-argument-clause Builder

GenericArgumentClauseBuilder::GenericArgumentClauseBuilder()
  : ArgumentListLayout(
      SyntaxFactory::makeBlankGenericArgumentClause().getRaw()->Layout) {}

GenericArgumentClauseBuilder &GenericArgumentClauseBuilder::
useLeftAngleBracket(RC<TokenSyntax> LeftAngle) {
  syntax_assert_token_is(LeftAngle, tok::l_angle, "<");
  LeftAngleToken = LeftAngle;
  return *this;
}

GenericArgumentClauseBuilder &GenericArgumentClauseBuilder::
addGenericArgument(llvm::Optional<RC<TokenSyntax>> MaybeComma,
                   TypeSyntax ArgumentTypeSyntax) {
  if (MaybeComma.hasValue()) {
    syntax_assert_token_is(MaybeComma.getValue(), tok::comma, ",");
    ArgumentListLayout.push_back(MaybeComma.getValue());
  } else {
    ArgumentListLayout.push_back(TokenSyntax::missingToken(tok::comma, ","));
  }
  ArgumentListLayout.push_back(ArgumentTypeSyntax.getRaw());
  return *this;
}

GenericArgumentClauseBuilder &GenericArgumentClauseBuilder::
useRightAngleBracket(RC<TokenSyntax> RightAngle) {
  syntax_assert_token_is(RightAngle, tok::r_angle, ">");
  RightAngleToken = RightAngle;
  return *this;
}


GenericArgumentClauseSyntax GenericArgumentClauseBuilder::build() const {
  auto ArgListRaw = RawSyntax::make(SyntaxKind::TypeArgumentList,
                                    ArgumentListLayout,
                                    SourcePresence::Present);
  auto Raw = RawSyntax::make(SyntaxKind::GenericArgumentClause,
                             {
                               LeftAngleToken,
                               ArgListRaw,
                               RightAngleToken
                             },
                             SourcePresence::Present);
  auto Data = GenericArgumentClauseSyntaxData::make(Raw);
  return { Data, Data.get() };
}
