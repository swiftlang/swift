//===--- SyntaxBulider.cpp - Swift Syntax Builder Implementation *- C++ -*-===//
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
#include "swift/Syntax/StmtSyntax.h"
#include "swift/Syntax/TypeSyntax.h"
#include "swift/Syntax/GenericSyntax.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/TokenSyntax.h"

using namespace swift;
using namespace swift::syntax;

UnknownSyntax
SyntaxFactory::makeUnknownSyntax(llvm::ArrayRef<RC<TokenSyntax>> Tokens) {
  RawSyntax::LayoutList Layout;
  std::copy(Tokens.begin(), Tokens.end(), std::back_inserter(Layout));
  auto Raw = RawSyntax::make(SyntaxKind::Unknown, Layout,
                             SourcePresence::Present);
  auto Data = UnknownSyntaxData::make(Raw);
  return { Data, Data.get() };
}

#pragma mark - Declarations

StructDeclSyntax
SyntaxFactory::makeStructDecl(RC<TokenSyntax> StructToken,
                              RC<TokenSyntax> Identifier,
                              Syntax GenericParameters,
                              Syntax WhereClause, RC<TokenSyntax> LeftBrace,
                              Syntax DeclMembers, RC<TokenSyntax> RightBrace) {
  auto Raw = RawSyntax::make(SyntaxKind::StructDecl,
                             {
                               StructToken,
                               Identifier,
                               GenericParameters.getRaw(),
                               WhereClause.getRaw(),
                               LeftBrace,
                               DeclMembers.getRaw(),
                               RightBrace
                             },
                             SourcePresence::Present);
  auto Data = StructDeclSyntaxData::make(Raw);
  return StructDeclSyntax {
    Data,
    Data.get()
  };
}

StructDeclSyntax SyntaxFactory::makeBlankStructDecl() {
  auto Data = StructDeclSyntaxData::makeBlank();
  return StructDeclSyntax {
    Data, Data.get()
  };
}

TypeAliasDeclSyntax SyntaxFactory::makeTypealiasDecl(
    RC<TokenSyntax> TypealiasToken, RC<TokenSyntax> Identifier,
    GenericParameterClauseSyntax GenericParams, RC<TokenSyntax> AssignmentToken,
    TypeSyntax Type) {
  auto Raw = RawSyntax::make(SyntaxKind::TypeAliasDecl,
                             {TypealiasToken, Identifier, GenericParams.getRaw(),
                              AssignmentToken, Type.getRaw()},
                             SourcePresence::Present);
  auto Data = TypeAliasDeclSyntaxData::make(Raw);
  return TypeAliasDeclSyntax { Data, Data.get() };
}

TypeAliasDeclSyntax SyntaxFactory::makeBlankTypealiasDecl() {
  auto Data = TypeAliasDeclSyntaxData::makeBlank();
  return TypeAliasDeclSyntax { Data, Data.get() };
}

DeclMembersSyntax SyntaxFactory::makeBlankDeclMembers() {
  auto Data = DeclMembersSyntaxData::makeBlank();
  return DeclMembersSyntax { Data, Data.get() };
}

TypeIdentifierSyntax
SyntaxFactory::makeTypeIdentifier(OwnedString Name,
                                  const Trivia &LeadingTrivia,
                                  const Trivia &TrailingTrivia) {
  auto Raw = RawSyntax::make(
      SyntaxKind::TypeIdentifier,
      {
          SyntaxFactory::makeIdentifier(Name, LeadingTrivia, TrailingTrivia),
          RawSyntax::missing(SyntaxKind::GenericArgumentClause),
          TokenSyntax::missingToken(tok::period, "."),
          RawSyntax::missing(SyntaxKind::TypeIdentifier),
      },
      SourcePresence::Present);
  auto Data = TypeIdentifierSyntaxData::make(Raw);
  return TypeIdentifierSyntax { Data, Data.get() };
}

TupleTypeElementSyntax
SyntaxFactory::makeTupleTypeElement(TypeSyntax ElementType) {
  auto Data = TupleTypeElementSyntaxData::makeBlank();
  return TupleTypeElementSyntax { Data, Data.get() }
      .withTypeSyntax(ElementType);
}

TypeIdentifierSyntax
SyntaxFactory::makeTypeIdentifier(RC<TokenSyntax> Identifier,
                                  GenericArgumentClauseSyntax GenericArgs) {
  auto Raw = RawSyntax::make(SyntaxKind::TypeIdentifier,
                             {
                                 Identifier, GenericArgs.getRaw(),
                                 TokenSyntax::missingToken(tok::period, "."),
                                 RawSyntax::missing(SyntaxKind::TypeIdentifier),
                             },
                             SourcePresence::Present);
  auto Data = TypeIdentifierSyntaxData::make(Raw);
  return TypeIdentifierSyntax { Data, Data.get() };
}

OptionalTypeSyntax
SyntaxFactory::makeOptionalType(TypeSyntax BaseType,
                                const Trivia &TrailingTrivia) {
  auto Raw = RawSyntax::make(SyntaxKind::OptionalType,
    {
     BaseType.getRaw(),
     SyntaxFactory::makeQuestionPostfixToken(TrailingTrivia),
    },
    SourcePresence::Present);

  auto Data = OptionalTypeSyntaxData::make(Raw);
  return OptionalTypeSyntax { Data, Data.get() };
}

OptionalTypeSyntax SyntaxFactory::makeBlankOptionalType() {
  auto Data = OptionalTypeSyntaxData::makeBlank();
  return OptionalTypeSyntax { Data, Data.get() };
}

ImplicitlyUnwrappedOptionalTypeSyntax
SyntaxFactory::makeImplicitlyUnwrappedOptionalType(
    TypeSyntax BaseType, const Trivia &TrailingTrivia) {
  auto Raw = RawSyntax::make(SyntaxKind::ImplicitlyUnwrappedOptionalType,
    {
        BaseType.getRaw(), SyntaxFactory::makeExclaimPostfixToken(
                           TrailingTrivia),
    },
    SourcePresence::Present);
  auto Data = ImplicitlyUnwrappedOptionalTypeSyntaxData::make(Raw);
  return ImplicitlyUnwrappedOptionalTypeSyntax { Data, Data.get() };
}

ImplicitlyUnwrappedOptionalTypeSyntax
SyntaxFactory::makeBlankImplicitlyUnwrappedOptionalType() {
  auto Data = ImplicitlyUnwrappedOptionalTypeSyntaxData::makeBlank();
  return ImplicitlyUnwrappedOptionalTypeSyntax { Data, Data.get() };
}

MetatypeTypeSyntax SyntaxFactory::makeMetatypeType(TypeSyntax BaseType,
                                                   RC<TokenSyntax> DotToken,
                                                   RC<TokenSyntax> TypeToken) {
  auto Raw = RawSyntax::make(SyntaxKind::MetatypeType,
                      {
                        BaseType.getRaw(),
                        DotToken,
                        TypeToken
                      },
                      SourcePresence::Present);
  auto Data = MetatypeTypeSyntaxData::make(Raw);
  return MetatypeTypeSyntax { Data, Data.get() };
}

MetatypeTypeSyntax SyntaxFactory::makeBlankMetatypeType() {
  auto Data = MetatypeTypeSyntaxData::makeBlank();
  return MetatypeTypeSyntax { Data, Data.get() };
}

TypeIdentifierSyntax SyntaxFactory::makeAnyTypeIdentifier() {
  auto Data = TypeIdentifierSyntaxData::makeBlank();
  return TypeIdentifierSyntax { Data, Data.get() }
    .withIdentifier(makeIdentifier("Any", {}, {}));
}

TypeIdentifierSyntax SyntaxFactory::makeSelfTypeIdentifier() {
  auto Data = TypeIdentifierSyntaxData::makeBlank();
  return TypeIdentifierSyntax { Data, Data.get() }
    .withIdentifier(makeIdentifier("Self", {}, {}));
}

SameTypeRequirementSyntax SyntaxFactory::makeSameTypeRequirement(
    TypeIdentifierSyntax LeftTypeIdentifier, RC<TokenSyntax> EqualityToken,
    TypeSyntax RightType) {
  auto Raw = RawSyntax::make(SyntaxKind::SameTypeRequirement,
                             {
                               LeftTypeIdentifier.getRaw(),
                               EqualityToken,
                               RightType.getRaw()
                             },
                             SourcePresence::Present);
  auto Data = SameTypeRequirementSyntaxData::make(Raw);
  return SameTypeRequirementSyntax { Data, Data.get() };
}

SameTypeRequirementSyntax SyntaxFactory::makeBlankSameTypeRequirement() {
  auto Data = SameTypeRequirementSyntaxData::makeBlank();
  return SameTypeRequirementSyntax { Data, Data.get() };
}

#pragma mark - Statements

CodeBlockStmtSyntax
SyntaxFactory::makeCodeBlock(RC<TokenSyntax> LeftBraceToken,
                             StmtListSyntax Elements,
                             RC<TokenSyntax> RightBraceToken) {
  auto Raw = RawSyntax::make(SyntaxKind::CodeBlockStmt,
                             {
                               LeftBraceToken,
                               Elements.getRaw(),
                               RightBraceToken
                             }, SourcePresence::Present);
  auto Data = CodeBlockStmtSyntaxData::make(Raw);
  return CodeBlockStmtSyntax { Data, Data.get() };
}

CodeBlockStmtSyntax SyntaxFactory::makeBlankCodeBlock() {
  auto Data = CodeBlockStmtSyntaxData::makeBlank();
  return CodeBlockStmtSyntax { Data, Data.get() };
}

FallthroughStmtSyntax
SyntaxFactory::makeFallthroughStmt(RC<TokenSyntax> FallthroughKeyword) {
  auto Raw = RawSyntax::make(SyntaxKind::FallthroughStmt, {
    FallthroughKeyword
  }, SourcePresence::Present);
  auto Data = FallthroughStmtSyntaxData::make(Raw);
  return FallthroughStmtSyntax { Data, Data.get() };
}

FallthroughStmtSyntax SyntaxFactory::makeBlankFallthroughStmt() {
  auto Data = FallthroughStmtSyntaxData::makeBlank();
  return FallthroughStmtSyntax { Data, Data.get() };
}

#pragma mark - break-statement

/// Make a break statement with the give `break` keyword and
/// destination label.
BreakStmtSyntax
SyntaxFactory::makeBreakStmt(RC<TokenSyntax> BreakKeyword,
                               RC<TokenSyntax> Label) {
  auto Raw = RawSyntax::make(SyntaxKind::BreakStmt,
                             {
                               BreakKeyword,
                               Label,
                             },
                             SourcePresence::Present);
  auto Data = BreakStmtSyntaxData::make(Raw);
  return { Data, Data.get() };
}

/// Make a break statement with the `break` keyword
/// and destination label marked as missing.
BreakStmtSyntax SyntaxFactory::makeBlankBreakStmtSyntax() {
  auto Data = BreakStmtSyntaxData::makeBlank();
  return { Data, Data.get() };
}

#pragma mark - continue-statement

ContinueStmtSyntax
SyntaxFactory::makeContinueStmt(RC<TokenSyntax> ContinueKeyword,
                                RC<TokenSyntax> Label) {
  auto Raw = RawSyntax::make(SyntaxKind::BreakStmt,
                             {
                               ContinueKeyword,
                               Label,
                             },
                             SourcePresence::Present);
  auto Data = ContinueStmtSyntaxData::make(Raw);
  return { Data, Data.get() };
}

/// Make a break statement with the `break` keyword
/// and destination label marked as missing.
ContinueStmtSyntax SyntaxFactory::makeBlankContinueStmtSyntax() {
  auto Data = ContinueStmtSyntaxData::makeBlank();
  return { Data, Data.get() };
}

#pragma mark - return-statement

/// Make a return statement with the given `return` keyword and returned
/// expression.
ReturnStmtSyntax
SyntaxFactory::makeReturnStmt(RC<TokenSyntax> ReturnKeyword,
                              ExprSyntax ReturnedExpression) {
  auto Raw = RawSyntax::make(SyntaxKind::ReturnStmt,
                             {
                               ReturnKeyword,
                               ReturnedExpression.getRaw(),
                             },
                             SourcePresence::Present);
  auto Data = ReturnStmtSyntaxData::make(Raw);
  return { Data, Data.get() };
}

ReturnStmtSyntax SyntaxFactory::makeBlankReturnStmt() {
  auto Raw = RawSyntax::make(SyntaxKind::ReturnStmt,
    {
     TokenSyntax::missingToken(tok::kw_return, "return"),
     RawSyntax::missing(SyntaxKind::MissingExpr),
    },
    SourcePresence::Present);
  auto Data = ReturnStmtSyntaxData::make(Raw);
  return { Data, Data.get() };
}

#pragma mark - Expressions

IntegerLiteralExprSyntax
SyntaxFactory::makeIntegerLiteralExpr(RC<TokenSyntax> Sign,
                                      RC<TokenSyntax> Digits) {
  auto Raw = RawSyntax::make(SyntaxKind::IntegerLiteralExpr,
                             {
                               Sign,
                               Digits,
                             },
                             SourcePresence::Present);
  auto Data = IntegerLiteralExprSyntaxData::make(Raw);
  return { Data, Data.get() };
}

IntegerLiteralExprSyntax SyntaxFactory::makeBlankIntegerLiteralExpr() {
  auto Raw = RawSyntax::make(SyntaxKind::IntegerLiteralExpr,
    {
     TokenSyntax::missingToken(tok::oper_prefix, "-"),
     TokenSyntax::missingToken(tok::integer_literal, ""),
    },
    SourcePresence::Present);
  auto Data = IntegerLiteralExprSyntaxData::make(Raw);
  return { Data, Data.get() };
}


#pragma mark - Tokens

RC<TokenSyntax>
SyntaxFactory::makeFallthroughKeyword(const Trivia &LeadingTrivia,
                                      const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::kw_fallthrough, "fallthrough",
                           SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax>
SyntaxFactory::makeAtSignToken(const Trivia &LeadingTrivia,
                               const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::at_sign, "@", SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax>
SyntaxFactory::makeBreakKeyword(const swift::syntax::Trivia &LeadingTrivia,
                                const swift::syntax::Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::kw_break, "break", SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax> SyntaxFactory::
makeContinueKeyword(const swift::syntax::Trivia &LeadingTrivia,
                    const swift::syntax::Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::kw_continue, "continue",
                           SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax> SyntaxFactory::
makeReturnKeyword(const swift::syntax::Trivia &LeadingTrivia,
                  const swift::syntax::Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::kw_return, "return",
                           SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax>
SyntaxFactory::makeLeftAngleToken(const Trivia &LeadingTrivia,
                                  const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::l_angle, "<", SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax>
SyntaxFactory::makeRightAngleToken(const Trivia &LeadingTrivia,
                                   const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::r_angle, ">", SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax>
SyntaxFactory::makeLeftParenToken(const Trivia &LeadingTrivia,
                                  const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::l_paren, "(", SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax>
SyntaxFactory::makeRightParenToken(const Trivia &LeadingTrivia,
                                   const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::r_paren, ")", SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax>
SyntaxFactory::makeLeftSquareBracketToken(const Trivia &LeadingTrivia,
                                          const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::l_square, "[", SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax>
SyntaxFactory::makeRightSquareBracketToken(const Trivia &LeadingTrivia,
                                           const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::r_square, "]", SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax>
SyntaxFactory::makeQuestionPostfixToken(const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::question_postfix, "?", SourcePresence::Present,
                           {}, TrailingTrivia);
}

RC<TokenSyntax>
SyntaxFactory::makeExclaimPostfixToken(const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::exclaim_postfix, "!", SourcePresence::Present,
                           {}, TrailingTrivia);
}

RC<TokenSyntax> SyntaxFactory::makeIdentifier(OwnedString Name,
                                           const Trivia &LeadingTrivia,
                                           const Trivia &TrailingTrivia) {
  return RC<TokenSyntax>{
    new TokenSyntax {
      tok::identifier, Name,
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax> SyntaxFactory::makeCommaToken(const Trivia &LeadingTrivia,
                                             const Trivia &TrailingTrivia) {
  return RC<TokenSyntax>{
    new TokenSyntax{tok::comma, ",",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax> SyntaxFactory::makeColonToken(const Trivia &LeadingTrivia,
                                             const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax{tok::colon, ":",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax> SyntaxFactory::makeDotToken(const Trivia &LeadingTrivia,
                                           const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax {
      tok::period, ".",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia,
    }
  };
}

RC<TokenSyntax> SyntaxFactory::makeStructKeyword(const Trivia &LeadingTrivia,
                                                 const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax{tok::kw_struct, "struct",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax> SyntaxFactory::makeWhereKeyword(const Trivia &LeadingTrivia,
                                                const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax{tok::kw_where, "where",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax> SyntaxFactory::makeInoutKeyword(const Trivia &LeadingTrivia,
                                                const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax{tok::kw_inout, "inout",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax>
SyntaxFactory::makeThrowsKeyword(const Trivia &LeadingTrivia,
                                 const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax {
      tok::kw_throws, "throws",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax>
SyntaxFactory::makeRethrowsKeyword(const Trivia &LeadingTrivia,
                                   const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax {
      tok::kw_rethrows, "rethrows",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax>
SyntaxFactory::makeTypealiasKeyword(const Trivia &LeadingTrivia,
                                    const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax {
      tok::kw_typealias, "typealias",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax>
SyntaxFactory::makeEqualToken(const Trivia &LeadingTrivia,
                              const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax{tok::equal, "=",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax> SyntaxFactory::makeArrow(const Trivia &LeadingTrivia,
                                      const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax{tok::arrow, "->",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax>
SyntaxFactory::makeEqualityOperator(const Trivia &LeadingTrivia,
                                    const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax {
      tok::oper_binary_spaced, "==",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax> SyntaxFactory::makeTypeToken(const Trivia &LeadingTrivia,
                                          const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax {
      tok::identifier, "Type",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax>
SyntaxFactory::makeProtocolToken(const Trivia &LeadingTrivia,
                                 const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax{tok::identifier, "Protocol",
      SourcePresence::Present,
      LeadingTrivia,
      TrailingTrivia
    }
  };
}

RC<TokenSyntax>
SyntaxFactory::makeIntegerLiteralToken(OwnedString Digits,
                                       const Trivia &LeadingTrivia,
                                       const Trivia &TrailingTrivia) {
  return RC<TokenSyntax> {
    new TokenSyntax(tok::integer_literal, Digits, SourcePresence::Present,
                    LeadingTrivia,
                    TrailingTrivia)
  };
}

TupleTypeSyntax SyntaxFactory::makeVoidTupleType() {
  auto Raw = RawSyntax::make(SyntaxKind::TupleType,
    {
        SyntaxFactory::makeLeftParenToken({}, {}),
        RawSyntax::missing(SyntaxKind::TypeArgumentList),
        SyntaxFactory::makeRightParenToken({}, {}),
    },
    SourcePresence::Present);
  auto Data = TupleTypeSyntaxData::make(std::move(Raw));
  return TupleTypeSyntax {
    Data, Data.get()
  };
}

TupleTypeElementSyntax
SyntaxFactory::makeTupleTypeElement(RC<TokenSyntax> Name,
                                    TypeSyntax ElementTypeSyntax) {
  auto Data = TupleTypeElementSyntaxData::makeBlank();
  return TupleTypeElementSyntax { Data, Data.get() }
    .withLabel(Name)
    .withColonToken(SyntaxFactory::makeColonToken({}, Trivia::spaces(1)))
    .withTypeSyntax(ElementTypeSyntax);
}

#pragma mark - Generics

GenericParameterClauseSyntax
SyntaxFactory::makeBlankGenericParameterClause() {
  auto Data = GenericParameterClauseSyntaxData::makeBlank();
  return GenericParameterClauseSyntax { Data, Data.get() };
}

GenericArgumentClauseSyntax SyntaxFactory::makeBlankGenericArgumentClause() {
  auto Data = GenericArgumentClauseSyntaxData::makeBlank();
  return { Data, Data.get() };
}

GenericWhereClauseSyntax
SyntaxFactory::makeBlankGenericWhereClause() {
  auto Data = GenericWhereClauseSyntaxData::makeBlank();
  return GenericWhereClauseSyntax { Data, Data.get() };
}

GenericParameterSyntax
SyntaxFactory::makeGenericParameter(OwnedString TypeName,
                                    const Trivia &LeadingTrivia,
                                    const Trivia &TrailingTrivia) {
  auto Raw = RawSyntax::make(SyntaxKind::GenericParameter,
                             {
                               SyntaxFactory::makeIdentifier(TypeName,
                                                             LeadingTrivia,
                                                             TrailingTrivia),
                               TokenSyntax::missingToken(tok::colon, ":"),
                               RawSyntax::missing(SyntaxKind::TypeIdentifier),
                             },
                             SourcePresence::Present);
  auto Data = GenericParameterSyntaxData::make(Raw);
  return GenericParameterSyntax { Data, Data.get() };
}

ArrayTypeSyntax
SyntaxFactory::makeArrayType(RC<TokenSyntax> LeftSquareBracket,
                             TypeSyntax ElementType,
                             RC<TokenSyntax> RightSquareBracket) {
  auto Raw = RawSyntax::make(SyntaxKind::ArrayType,
                             {
                               LeftSquareBracket,
                               ElementType.getRaw(),
                               RightSquareBracket
                             },
                             SourcePresence::Present);
  auto Data = ArrayTypeSyntaxData::make(Raw);
  return ArrayTypeSyntax { Data, Data.get() };
}

ArrayTypeSyntax SyntaxFactory::makeBlankArrayType() {
  auto Data = ArrayTypeSyntaxData::makeBlank();
  return ArrayTypeSyntax { Data, Data.get() };
}

DictionaryTypeSyntax
SyntaxFactory::makeDictionaryType(RC<TokenSyntax> LeftSquareBracket,
                                  TypeSyntax KeyType,
                                  RC<TokenSyntax> Colon,
                                  TypeSyntax ValueType,
                                  RC<TokenSyntax> RightSquareBracket) {
  auto Raw = RawSyntax::make(SyntaxKind::DictionaryType,
                             {
                               LeftSquareBracket,
                               KeyType.getRaw(),
                               Colon,
                               ValueType.getRaw(),
                               RightSquareBracket
                             },
                             SourcePresence::Present);
  auto Data = DictionaryTypeSyntaxData::make(Raw);
  return DictionaryTypeSyntax { Data, Data.get() };
}

DictionaryTypeSyntax SyntaxFactory::makeBlankDictionaryType() {
  auto Data = DictionaryTypeSyntaxData::makeBlank();
  return DictionaryTypeSyntax { Data, Data.get() };
}

#pragma mark - Operators

/// Make a prefix operator with the given text.
RC<TokenSyntax>
SyntaxFactory::makePrefixOpereator(OwnedString Name,
                                   const Trivia &LeadingTrivia) {
  return TokenSyntax::make(tok::oper_prefix, Name,
                           SourcePresence::Present, LeadingTrivia, {});
}

#pragma mark - Types

#pragma mark - type-attribute

TypeAttributeSyntax
SyntaxFactory::makeTypeAttribute(RC<TokenSyntax> AtSignToken,
                                RC<TokenSyntax> Identifier,
                                RC<TokenSyntax> LeftParen,
                                BalancedTokensSyntax BalancedTokens,
                                RC<TokenSyntax> RightParen) {
  auto Raw = RawSyntax::make(SyntaxKind::TypeAttribute,
                             {
                               AtSignToken,
                               Identifier,
                               LeftParen,
                               BalancedTokens.getRaw(),
                               RightParen,
                             },
                             SourcePresence::Present);
  auto Data = TypeAttributeSyntaxData::make(Raw);
  return TypeAttributeSyntax { Data, Data.get() };
}

TypeAttributeSyntax SyntaxFactory::makeBlankTypeAttribute() {
  auto Raw = RawSyntax::make(SyntaxKind::TypeAttribute,
                             {
                               TokenSyntax::missingToken(tok::at_sign, "@"),
                               TokenSyntax::missingToken(tok::identifier, ""),
                               TokenSyntax::missingToken(tok::l_paren, "("),
                               RawSyntax::missing(SyntaxKind::BalancedTokens),
                               TokenSyntax::missingToken(tok::r_paren, ")"),
                             },
                             SourcePresence::Present);
  auto Data = TypeAttributeSyntaxData::make(Raw);
  return TypeAttributeSyntax { Data, Data.get() };
}

#pragma mark - type-attributes

#pragma mark - balanced-tokens

BalancedTokensSyntax
SyntaxFactory::makeBalancedTokens(RawSyntax::LayoutList Tokens) {
  auto Raw = RawSyntax::make(SyntaxKind::BalancedTokens,
                             Tokens,
                             SourcePresence::Present);
  auto Data = BalancedTokensSyntaxData::make(Raw);
  return BalancedTokensSyntax { Data, Data.get() };
}

BalancedTokensSyntax SyntaxFactory::makeBlankBalancedTokens() {
  auto Raw = RawSyntax::make(SyntaxKind::BalancedTokens,
                             {},
                             SourcePresence::Present);
  auto Data = BalancedTokensSyntaxData::make(Raw);
  return BalancedTokensSyntax { Data, Data.get() };
}

#pragma mark - function-type

FunctionTypeSyntax SyntaxFactory::makeFunctionType(
    TypeAttributesSyntax TypeAttributes, RC<TokenSyntax> LeftParen,
    TypeArgumentListSyntax ArgumentList, RC<TokenSyntax> RightParen,
    RC<TokenSyntax> ThrowsOrRethrows, RC<TokenSyntax> Arrow,
    TypeSyntax ReturnType) {
  auto Raw =
      RawSyntax::make(SyntaxKind::FunctionType,
                      {
                        TypeAttributes.getRaw(),
                        LeftParen,
                        ArgumentList.getRaw(),
                        RightParen,
                        ThrowsOrRethrows,
                        Arrow,
                        ReturnType.getRaw()
                      },
                      SourcePresence::Present);
  auto Data = FunctionTypeSyntaxData::make(Raw);
  return FunctionTypeSyntax { Data, Data.get() };
}

FunctionTypeSyntax SyntaxFactory::makeBlankFunctionType() {
  auto Data = FunctionTypeSyntaxData::makeBlank();
  return FunctionTypeSyntax { Data, Data.get() };
}

#pragma mark -
#pragma mark function-type-argument

FunctionTypeArgumentSyntax SyntaxFactory::
makeFunctionTypeArgument(RC<TokenSyntax> ExternalParameterName,
                         RC<TokenSyntax> LocalParameterName,
                         TypeAttributesSyntax TypeAttributes,
                         RC<TokenSyntax> InoutKeyword,
                         RC<TokenSyntax> ColonToken,
                         TypeSyntax ParameterTypeSyntax) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionTypeArgument,
                             {
                               ExternalParameterName,
                               LocalParameterName,
                               TypeAttributes.getRaw(),
                               InoutKeyword,
                               ColonToken,
                               ParameterTypeSyntax.getRaw()
                             }, SourcePresence::Present);
  auto Data = FunctionTypeArgumentSyntaxData::make(Raw);
  return { Data, Data.get() };
}

FunctionTypeArgumentSyntax SyntaxFactory::
makeFunctionTypeArgument(RC<TokenSyntax> LocalParameterName,
                         RC<TokenSyntax> ColonToken,
                         swift::syntax::TypeSyntax TypeArgument) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionTypeArgument,
                             {
                               TokenSyntax::missingToken(tok::identifier, ""),
                               LocalParameterName,
                               RawSyntax::missing(SyntaxKind::TypeAttributes),
                               TokenSyntax::missingToken(tok::kw_inout, "inout"),
                               ColonToken,
                               TypeArgument.getRaw()
                             }, SourcePresence::Present);
  auto Data = FunctionTypeArgumentSyntaxData::make(Raw);
  return { Data, Data.get() };
}

FunctionTypeArgumentSyntax
SyntaxFactory::makeFunctionTypeArgument(TypeSyntax TypeArgument) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionTypeArgument,
    {
      TokenSyntax::missingToken(tok::identifier, ""),
      TokenSyntax::missingToken(tok::identifier, ""),
      RawSyntax::missing(SyntaxKind::TypeAttributes),
      TokenSyntax::missingToken(tok::kw_inout, "inout"),
      TokenSyntax::missingToken(tok::colon, ":"),
      TypeArgument.getRaw()
    }, SourcePresence::Present);
  auto Data = FunctionTypeArgumentSyntaxData::make(Raw);
  return { Data, Data.get() };
}

#pragma mark -
#pragma mark type-attributes

TypeAttributesSyntax SyntaxFactory::makeBlankTypeAttributes() {
  auto Data = TypeAttributesSyntaxData::makeBlank();
  return TypeAttributesSyntax { Data, Data.get() };
}

TypeArgumentListSyntax SyntaxFactory::makeBlankTypeArgumentList() {
  auto Data =  TypeArgumentListSyntaxData::makeBlank();
  return TypeArgumentListSyntax { Data, Data.get() };
}
