//===--- SyntaxFactory.cpp - Swift Syntax Builder Implementation ----------===//
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
#include "swift/Syntax/UnknownSyntax.h"

using namespace swift;
using namespace swift::syntax;

UnknownSyntax
SyntaxFactory::makeUnknownSyntax(llvm::ArrayRef<RC<TokenSyntax>> Tokens) {
  RawSyntax::LayoutList Layout;
  std::copy(Tokens.begin(), Tokens.end(), std::back_inserter(Layout));
  auto Raw = RawSyntax::make(SyntaxKind::Unknown, Layout,
                             SourcePresence::Present);
  return make<UnknownSyntax>(Raw);
}

#pragma mark - Declarations

#pragma mark - declaration-modifier

DeclModifierSyntax SyntaxFactory::makeDeclModifier(RC<TokenSyntax> Name,
                                                   RC<TokenSyntax> LeftParen,
                                                   RC<TokenSyntax> Argument,
                                                   RC<TokenSyntax> RightParen) {
  auto Raw = RawSyntax::make(SyntaxKind::DeclModifier,
                             {
                               Name,
                               LeftParen,
                               Argument,
                               RightParen,
                             },
                             SourcePresence::Present);
  return make<DeclModifierSyntax>(Raw);
}

DeclModifierSyntax SyntaxFactory::makeBlankDeclModifier() {
  return DeclModifierSyntax::makeBlank();
}

#pragma mark - declaration-modifier-list

DeclModifierListSyntax SyntaxFactory::
makeDeclModifierList(const std::vector<DeclModifierSyntax> &Modifiers) {
  RawSyntax::LayoutList Layout;
  for (auto Modifier : Modifiers) {
    Layout.push_back(Modifier.getRaw());
  }

  auto Raw = RawSyntax::make(SyntaxKind::DeclModifierList, Layout,
                             SourcePresence::Present);
  return make<DeclModifierListSyntax>(Raw);
}

DeclModifierListSyntax SyntaxFactory::makeBlankDeclModifierList() {
  return DeclModifierListSyntax::makeBlank();
}

#pragma mark - struct-declaration

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
  return make<StructDeclSyntax>(Raw);
}

StructDeclSyntax SyntaxFactory::makeBlankStructDecl() {
  return StructDeclSyntax::makeBlank();
}

#pragma mark - type-alias-declaration

TypeAliasDeclSyntax SyntaxFactory::makeTypealiasDecl(
    RC<TokenSyntax> TypealiasToken, RC<TokenSyntax> Identifier,
    GenericParameterClauseSyntax GenericParams, RC<TokenSyntax> AssignmentToken,
    TypeSyntax Type) {
  auto Raw = RawSyntax::make(SyntaxKind::TypeAliasDecl,
                             {TypealiasToken, Identifier, GenericParams.getRaw(),
                              AssignmentToken, Type.getRaw()},
                             SourcePresence::Present);
  return make<TypeAliasDeclSyntax>(Raw);
}

TypeAliasDeclSyntax SyntaxFactory::makeBlankTypealiasDecl() {
  return TypeAliasDeclSyntax::makeBlank();
}

DeclMembersSyntax SyntaxFactory::makeBlankDeclMembers() {
  return DeclMembersSyntax::makeBlank();
}

#pragma mark - function-parameter

FunctionParameterSyntax SyntaxFactory::
makeFunctionParameter(RC<TokenSyntax> ExternalName,
                      RC<TokenSyntax> LocalName,
                      RC<TokenSyntax> Colon,
                      llvm::Optional<TypeSyntax> ParameterTypeSyntax,
                      RC<TokenSyntax> Ellipsis,
                      RC<TokenSyntax> Equal,
                      llvm::Optional<ExprSyntax> DefaultValue,
                      RC<TokenSyntax> TrailingComma) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionParameter,
                             {
                               ExternalName,
                               LocalName,
                               Colon,
                               ParameterTypeSyntax.hasValue()
                                 ? ParameterTypeSyntax.getValue().getRaw()
                                 : RawSyntax::missing(SyntaxKind::MissingType),
                               Ellipsis,
                               Equal,
                               DefaultValue.hasValue()
                                 ? DefaultValue.getValue().getRaw()
                                 : RawSyntax::missing(SyntaxKind::MissingExpr),
                               TrailingComma,
                             },
                             SourcePresence::Present);
  return make<FunctionParameterSyntax>(Raw);
}

FunctionParameterSyntax SyntaxFactory::makeBlankFunctionParameter() {
  return FunctionParameterSyntax::makeBlank();
}

#pragma mark - function-parameter-list

FunctionParameterListSyntax SyntaxFactory::makeFunctionParameterList(
    std::vector<FunctionParameterSyntax> Parameters) {
  auto Data = FunctionParameterListSyntax::makeData(Parameters);
  return FunctionParameterListSyntax { Data, Data.get() };
}

FunctionParameterListSyntax SyntaxFactory::makeBlankFunctionParameterList() {
  return FunctionParameterListSyntax::makeBlank();
}

#pragma mark - function-signature

FunctionSignatureSyntax
SyntaxFactory::makeFunctionSignature(RC<TokenSyntax> LeftParen,
                                     FunctionParameterListSyntax ParameterList,
                                     RC<TokenSyntax> RightParen,
                                     RC<TokenSyntax> ThrowsOrRethrows,
                                     RC<TokenSyntax> Arrow,
                                     TypeAttributesSyntax ReturnTypeAttributes,
                                     TypeSyntax ReturnTypeSyntax) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionSignature,
                             {
                               LeftParen,
                               ParameterList.getRaw(),
                               RightParen,
                               ThrowsOrRethrows,
                               Arrow,
                               ReturnTypeAttributes.getRaw(),
                               ReturnTypeSyntax.getRaw()
                             },
                             SourcePresence::Present);
  return make<FunctionSignatureSyntax>(Raw);
}

FunctionSignatureSyntax SyntaxFactory::makeBlankFunctionSignature() {
  return FunctionSignatureSyntax::makeBlank();
}

#pragma mark - function-declaration

FunctionDeclSyntax SyntaxFactory::
makeFunctionDecl(TypeAttributesSyntax Attributes,
                 DeclModifierListSyntax Modifiers,
                 RC<TokenSyntax> FuncKeyword,
                 RC<TokenSyntax> Identifier,
                 llvm::Optional<GenericParameterClauseSyntax> GenericParams,
                 FunctionSignatureSyntax Signature,
                 llvm::Optional<GenericWhereClauseSyntax> GenericWhereClause,
                 llvm::Optional<CodeBlockStmtSyntax> Body) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionDecl,
    {
      Attributes.getRaw(),
      Modifiers.getRaw(),
      FuncKeyword,
      Identifier,
      GenericParams.hasValue()
       ? GenericParams.getValue().getRaw()
       : SyntaxFactory::makeBlankGenericParameterClause().getRaw(),
      Signature.getRaw(),
      GenericWhereClause.hasValue()
        ? GenericWhereClause.getValue().getRaw()
        : SyntaxFactory::makeBlankGenericWhereClause().getRaw(),
      Body.hasValue()
       ? Body.getValue().getRaw()
       : SyntaxFactory::makeBlankCodeBlock().getRaw()
    },
    SourcePresence::Present);
  return make<FunctionDeclSyntax>(Raw);
}

FunctionDeclSyntax SyntaxFactory::makeBlankFunctionDecl() {
  return FunctionDeclSyntax::makeBlank();
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
  return make<CodeBlockStmtSyntax>(Raw);
}

CodeBlockStmtSyntax SyntaxFactory::makeBlankCodeBlock() {
  return CodeBlockStmtSyntax::makeBlank();
}

FallthroughStmtSyntax
SyntaxFactory::makeFallthroughStmt(RC<TokenSyntax> FallthroughKeyword) {
  auto Raw = RawSyntax::make(SyntaxKind::FallthroughStmt, {
    FallthroughKeyword
  }, SourcePresence::Present);
  return make<FallthroughStmtSyntax>(Raw);
}

FallthroughStmtSyntax SyntaxFactory::makeBlankFallthroughStmt() {
  return FallthroughStmtSyntax::makeBlank();
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
  return make<BreakStmtSyntax>(Raw);
}

/// Make a break statement with the `break` keyword
/// and destination label marked as missing.
BreakStmtSyntax SyntaxFactory::makeBlankBreakStmtSyntax() {
  return BreakStmtSyntax::makeBlank();
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
  return make<ContinueStmtSyntax>(Raw);
}

/// Make a break statement with the `break` keyword
/// and destination label marked as missing.
ContinueStmtSyntax SyntaxFactory::makeBlankContinueStmtSyntax() {
  return ContinueStmtSyntax::makeBlank();
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
  return make<ReturnStmtSyntax>(Raw);
}

ReturnStmtSyntax SyntaxFactory::makeBlankReturnStmt() {
  auto Raw = RawSyntax::make(SyntaxKind::ReturnStmt,
    {
     TokenSyntax::missingToken(tok::kw_return, "return"),
     RawSyntax::missing(SyntaxKind::MissingExpr),
    },
    SourcePresence::Present);
  return make<ReturnStmtSyntax>(Raw);
}

/// Make a statement list from a loosely connected list of statements.
StmtListSyntax
SyntaxFactory::makeStmtList(const std::vector<StmtSyntax> &Statements) {
  RawSyntax::LayoutList Layout;
  for (auto Stmt : Statements) {
    Layout.push_back(Stmt.getRaw());
  }

  auto Raw = RawSyntax::make(SyntaxKind::StmtList, Layout,
                             SourcePresence::Present);
  return make<StmtListSyntax>(Raw);
}

/// Make an empty statement list.
StmtListSyntax SyntaxFactory::makeBlankStmtList() {
  auto Raw = RawSyntax::make(SyntaxKind::StmtList, {}, SourcePresence::Present);
  return make<StmtListSyntax>(Raw);
}

#pragma mark - Expressions

#pragma mark - integer-literal-expression

IntegerLiteralExprSyntax
SyntaxFactory::makeIntegerLiteralExpr(RC<TokenSyntax> Sign,
                                      RC<TokenSyntax> Digits) {
  auto Raw = RawSyntax::make(SyntaxKind::IntegerLiteralExpr,
                             {
                               Sign,
                               Digits,
                             },
                             SourcePresence::Present);
  return make<IntegerLiteralExprSyntax>(Raw);
}

IntegerLiteralExprSyntax SyntaxFactory::makeBlankIntegerLiteralExpr() {
  auto Raw = RawSyntax::make(SyntaxKind::IntegerLiteralExpr,
    {
     TokenSyntax::missingToken(tok::oper_prefix, "-"),
     TokenSyntax::missingToken(tok::integer_literal, ""),
    },
    SourcePresence::Present);
  return make<IntegerLiteralExprSyntax>(Raw);
}

#pragma mark - symbolic-reference

SymbolicReferenceExprSyntax
SyntaxFactory::makeSymbolicReferenceExpr(RC<TokenSyntax> Identifier,
  llvm::Optional<GenericArgumentClauseSyntax> GenericArgs) {
  auto Raw = RawSyntax::make(SyntaxKind::SymbolicReferenceExpr,
    {
      Identifier,
      GenericArgs.hasValue()
        ? GenericArgs.getValue().getRaw()
        : RawSyntax::missing(SyntaxKind::GenericArgumentClause)
    },
    SourcePresence::Present);
  return make<SymbolicReferenceExprSyntax>(Raw);
}

SymbolicReferenceExprSyntax SyntaxFactory::makeBlankSymbolicReferenceExpr() {
  return SymbolicReferenceExprSyntax::makeBlank();
}

#pragma mark - function-call-argument

FunctionCallArgumentSyntax SyntaxFactory::makeBlankFunctionCallArgument() {
  return FunctionCallArgumentSyntax::makeBlank();
}

FunctionCallArgumentSyntax
SyntaxFactory::makeFunctionCallArgument(RC<TokenSyntax> Label,
                                        RC<TokenSyntax> Colon,
                                        ExprSyntax ExpressionArgument,
                                        RC<TokenSyntax> TrailingComma) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionCallArgument,
                             {
                               Label,
                               Colon,
                               ExpressionArgument.getRaw(),
                               TrailingComma
                             },
                             SourcePresence::Present);
  return make<FunctionCallArgumentSyntax>(Raw);
}

#pragma mark - function-call-argument-list

/// Make a function call argument list with the given arguments.
FunctionCallArgumentListSyntax
SyntaxFactory::makeFunctionCallArgumentList(
  std::vector<FunctionCallArgumentSyntax> Arguments) {
  RawSyntax::LayoutList Layout;
  for (const auto &Arg : Arguments) {
    Layout.push_back(Arg.getRaw());
  }

  auto Raw = RawSyntax::make(SyntaxKind::FunctionCallArgumentList, Layout,
                             SourcePresence::Present);
  return make<FunctionCallArgumentListSyntax>(Raw);
}


FunctionCallArgumentListSyntax
SyntaxFactory::makeBlankFunctionCallArgumentList() {
  return FunctionCallArgumentListSyntax::makeBlank();
}

#pragma mark - function-call-expression

FunctionCallExprSyntax
SyntaxFactory::makeFunctionCallExpr(ExprSyntax CalledExpr,
                                    RC<TokenSyntax> LeftParen,
                                    FunctionCallArgumentListSyntax Arguments,
                                    RC<TokenSyntax> RightParen) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionCallExpr,
                             {
                               CalledExpr.getRaw(),
                               LeftParen,
                               Arguments.getRaw(),
                               RightParen
                             },
                             SourcePresence::Present);
  return make<FunctionCallExprSyntax>(Raw);
}


FunctionCallExprSyntax SyntaxFactory::makeBlankFunctionCallExpr() {
  return FunctionCallExprSyntax::makeBlank();
}

#pragma mark - Tokens

RC<TokenSyntax>
SyntaxFactory::makeStaticKeyword(const Trivia &LeadingTrivia,
                                 const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::kw_static, "static",
                           SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax>
SyntaxFactory::makePublicKeyword(const Trivia &LeadingTrivia,
                                 const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::kw_public, "public",
                           SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax>
SyntaxFactory::makeFuncKeyword(const Trivia &LeadingTrivia,
                                      const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::kw_func, "func",
                           SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

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
SyntaxFactory::makeLeftBraceToken(const Trivia &LeadingTrivia,
                                  const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::l_brace, "{", SourcePresence::Present,
                           LeadingTrivia, TrailingTrivia);
}

RC<TokenSyntax>
SyntaxFactory::makeRightBraceToken(const Trivia &LeadingTrivia,
                                   const Trivia &TrailingTrivia) {
  return TokenSyntax::make(tok::r_brace, "}", SourcePresence::Present,
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

#pragma mark - Generics

GenericParameterClauseSyntax
SyntaxFactory::makeBlankGenericParameterClause() {
  return GenericParameterClauseSyntax::makeBlank();
}

GenericArgumentClauseSyntax SyntaxFactory::makeBlankGenericArgumentClause() {
  return GenericArgumentClauseSyntax::makeBlank();
}

GenericWhereClauseSyntax
SyntaxFactory::makeBlankGenericWhereClause() {
  return GenericWhereClauseSyntax::makeBlank();
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
  return make<GenericParameterSyntax>(Raw);
}

SameTypeRequirementSyntax SyntaxFactory::
makeSameTypeRequirement( TypeIdentifierSyntax LeftTypeIdentifier,
                        RC<TokenSyntax> EqualityToken,
                        TypeSyntax RightType) {
  auto Raw = RawSyntax::make(SyntaxKind::SameTypeRequirement,
                             {
                               LeftTypeIdentifier.getRaw(),
                               EqualityToken,
                               RightType.getRaw()
                             },
                             SourcePresence::Present);
  return make<SameTypeRequirementSyntax>(Raw);
}

SameTypeRequirementSyntax SyntaxFactory::makeBlankSameTypeRequirement() {
  return SameTypeRequirementSyntax::makeBlank();
}

GenericRequirementListSyntax SyntaxFactory::
makeGenericRequirementList(std::vector<GenericRequirementSyntax> &Requirements){
  RawSyntax::LayoutList Layout;
  for (auto Req : Requirements) {
    Layout.push_back(Req.getRaw());
  }
  auto Raw = RawSyntax::make(SyntaxKind::GenericRequirementList, Layout,
                             SourcePresence::Present);
  return make<GenericRequirementListSyntax>(Raw);
}

GenericRequirementListSyntax SyntaxFactory::makeBlankGenericRequirementList() {
  return GenericRequirementListSyntax::makeBlank();
}

#pragma mark - Operators

/// Make a prefix operator with the given text.
RC<TokenSyntax>
SyntaxFactory::makePrefixOperator(OwnedString Name,
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
  return make<TypeAttributeSyntax>(Raw);
}

TypeAttributeSyntax SyntaxFactory::makeBlankTypeAttribute() {
  return TypeAttributeSyntax::makeBlank();
}

#pragma mark - type-attributes

#pragma mark - balanced-tokens

BalancedTokensSyntax
SyntaxFactory::makeBalancedTokens(RawSyntax::LayoutList Tokens) {
  auto Raw = RawSyntax::make(SyntaxKind::BalancedTokens,
                             Tokens,
                             SourcePresence::Present);
  return make<BalancedTokensSyntax>(Raw);
}

BalancedTokensSyntax SyntaxFactory::makeBlankBalancedTokens() {
  return BalancedTokensSyntax::makeBlank();
}

#pragma mark - type-identifier

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
  return make<TypeIdentifierSyntax>(Raw);
}

TypeIdentifierSyntax SyntaxFactory::makeAnyTypeIdentifier() {
  return TypeIdentifierSyntax::makeBlank()
    .withIdentifier(makeIdentifier("Any", {}, {}));
}

TypeIdentifierSyntax SyntaxFactory::makeSelfTypeIdentifier() {
  return TypeIdentifierSyntax::makeBlank()
    .withIdentifier(makeIdentifier("Self", {}, {}));
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
  return make<TypeIdentifierSyntax>(Raw);
}

#pragma mark - tuple-type

TupleTypeSyntax SyntaxFactory::makeVoidTupleType() {
  auto Raw = RawSyntax::make(SyntaxKind::TupleType,
    {
      SyntaxFactory::makeLeftParenToken({}, {}),
      RawSyntax::missing(SyntaxKind::TupleTypeElementList),
      SyntaxFactory::makeRightParenToken({}, {}),
    },
    SourcePresence::Present);
  return make<TupleTypeSyntax>(Raw);
}

TupleTypeSyntax
SyntaxFactory::makeTupleType(RC<TokenSyntax> LParen,
                             TupleTypeElementListSyntax Elements,
                             RC<TokenSyntax> RParen) {
  auto Raw = RawSyntax::make(SyntaxKind::TupleType,
                             { LParen, Elements.getRaw(), RParen },
                             SourcePresence::Present);
  return make<TupleTypeSyntax>(Raw);
}

TupleTypeElementSyntax
SyntaxFactory::makeTupleTypeElement(RC<TokenSyntax> Name,
                                    RC<TokenSyntax> Colon,
                                    TypeSyntax ElementTypeSyntax,
                                    Optional<RC<TokenSyntax>> MaybeComma) {
  RC<TokenSyntax> Comma;
  if (MaybeComma.hasValue()) {
    Comma = MaybeComma.getValue();
  } else {
    Comma = TokenSyntax::missingToken(tok::comma, ",");
  }
  return TupleTypeElementSyntax::makeBlank()
    .withLabel(Name)
    .withColonToken(Colon)
    .withTypeSyntax(ElementTypeSyntax)
    .withCommaToken(Comma);
}


TupleTypeElementSyntax
SyntaxFactory::makeTupleTypeElement(TypeSyntax ElementType,
                                    Optional<RC<TokenSyntax>> MaybeComma) {
  RC<TokenSyntax> Comma;
  if (MaybeComma.hasValue()) {
    Comma = MaybeComma.getValue();
  } else {
    Comma = TokenSyntax::missingToken(tok::comma, ",");
  }
  return TupleTypeElementSyntax::makeBlank()
    .withTypeSyntax(ElementType)
    .withCommaToken(Comma);
}

TupleTypeElementListSyntax
SyntaxFactory::makeTupleTypeElementList(
  std::vector<TupleTypeElementSyntax> ElementTypes) {
  auto Data = TupleTypeElementListSyntax::makeData(ElementTypes);
  return TupleTypeElementListSyntax { Data, Data.get() };
}

#pragma mark - optional-type

OptionalTypeSyntax
SyntaxFactory::makeOptionalType(TypeSyntax BaseType,
                                const Trivia &TrailingTrivia) {
  auto Raw = RawSyntax::make(SyntaxKind::OptionalType,
  {
    BaseType.getRaw(),
    SyntaxFactory::makeQuestionPostfixToken(TrailingTrivia),
  },
  SourcePresence::Present);

  return make<OptionalTypeSyntax>(Raw);
}

OptionalTypeSyntax SyntaxFactory::makeBlankOptionalType() {
  return OptionalTypeSyntax::makeBlank();
}

#pragma mark - implicitly-unwrapped-optional-type

ImplicitlyUnwrappedOptionalTypeSyntax SyntaxFactory::
makeImplicitlyUnwrappedOptionalType(TypeSyntax BaseType,
                                    const Trivia &TrailingTrivia) {
  auto Raw = RawSyntax::make(SyntaxKind::ImplicitlyUnwrappedOptionalType,
    {
      BaseType.getRaw(),
      SyntaxFactory::makeExclaimPostfixToken(TrailingTrivia),
    },
    SourcePresence::Present);
  return make<ImplicitlyUnwrappedOptionalTypeSyntax>(Raw);
}

ImplicitlyUnwrappedOptionalTypeSyntax
SyntaxFactory::makeBlankImplicitlyUnwrappedOptionalType() {
  return ImplicitlyUnwrappedOptionalTypeSyntax::makeBlank();
}

#pragma mark - metatype-type

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
  return make<MetatypeTypeSyntax>(Raw);
}

MetatypeTypeSyntax SyntaxFactory::makeBlankMetatypeType() {
  return MetatypeTypeSyntax::makeBlank();
}

#pragma mark - function-type

FunctionTypeSyntax SyntaxFactory::makeFunctionType(
    TypeAttributesSyntax TypeAttributes, RC<TokenSyntax> LeftParen,
    FunctionParameterListSyntax ArgumentList, RC<TokenSyntax> RightParen,
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
  return make<FunctionTypeSyntax>(Raw);
}

FunctionTypeSyntax SyntaxFactory::makeBlankFunctionType() {
  return FunctionTypeSyntax::makeBlank();
}

#pragma mark - function-type-argument

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
  return make<FunctionTypeArgumentSyntax>(Raw);
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
  return make<FunctionTypeArgumentSyntax>(Raw);
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
  return make<FunctionTypeArgumentSyntax>(Raw);
}

#pragma mark -
#pragma mark type-attributes

TypeAttributesSyntax SyntaxFactory::makeBlankTypeAttributes() {
  return TypeAttributesSyntax::makeBlank();
}

TupleTypeElementListSyntax SyntaxFactory::makeBlankTupleTypeElementList() {
  return TupleTypeElementListSyntax::makeBlank();
}

#pragma mark - array-type

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
  return make<ArrayTypeSyntax>(Raw);
}

ArrayTypeSyntax SyntaxFactory::makeBlankArrayType() {
  return ArrayTypeSyntax::makeBlank();
}

#pragma mark - dictionary-type

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
  return make<DictionaryTypeSyntax>(Raw);
}

DictionaryTypeSyntax SyntaxFactory::makeBlankDictionaryType() {
  return DictionaryTypeSyntax::makeBlank();
}
