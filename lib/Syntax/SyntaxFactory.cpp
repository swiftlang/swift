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
  auto Data = UnknownSyntaxData::make(Raw);
  return { Data, Data.get() };
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
  auto Data = DeclModifierSyntaxData::make(Raw);
  return { Data, Data.get() };
}

DeclModifierSyntax SyntaxFactory::makeBlankDeclModifier() {
  auto Data = DeclModifierSyntaxData::makeBlank();
  return { Data, Data.get() };
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
  auto Data = DeclModifierListSyntaxData::make(Raw);
  return { Data, Data.get() };
}

DeclModifierListSyntax SyntaxFactory::makeBlankDeclModifierList() {
  auto Data = DeclModifierListSyntaxData::makeBlank();
  return { Data, Data.get() };
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

#pragma mark - type-alias-declaration

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
  auto Data = FunctionParameterSyntaxData::make(Raw);
  return { Data, Data.get() };
}

FunctionParameterSyntax SyntaxFactory::makeBlankFunctionParameter() {
  auto Data = FunctionParameterSyntaxData::makeBlank();
  return { Data, Data.get() };
}

#pragma mark - function-parameter-list

FunctionParameterListSyntax SyntaxFactory::makeFunctionParameterList(
    const std::vector<FunctionParameterSyntax> &Parameters) {
  RawSyntax::LayoutList Layout;
  for (auto Param : Parameters) {
    Layout.push_back(Param.getRaw());
  }

  auto Raw = RawSyntax::make(SyntaxKind::FunctionParameterList, Layout,
                             SourcePresence::Present);
  auto Data = FunctionParameterListSyntaxData::make(Raw);
  return { Data, Data.get() };
}

FunctionParameterListSyntax SyntaxFactory::makeBlankFunctionParameterList() {
  auto Data = FunctionParameterListSyntaxData::makeBlank();
  return { Data, Data.get() };
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
  auto Data = FunctionSignatureSyntaxData::make(Raw);
  return { Data, Data.get() };
}

FunctionSignatureSyntax SyntaxFactory::makeBlankFunctionSignature() {
  auto Data = FunctionSignatureSyntaxData::makeBlank();
  return { Data, Data.get() };
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
  auto Data = FunctionDeclSyntaxData::make(Raw);
  return { Data, Data.get() };
}

FunctionDeclSyntax SyntaxFactory::makeBlankFunctionDecl() {
  auto Data = FunctionDeclSyntaxData::makeBlank();
  return { Data, Data.get() };
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

/// Make a statement list from a loosely connected list of statements.
StmtListSyntax
SyntaxFactory::makeStmtList(const std::vector<StmtSyntax> &Statements) {
  RawSyntax::LayoutList Layout;
  for (auto Stmt : Statements) {
    Layout.push_back(Stmt.getRaw());
  }

  auto Raw = RawSyntax::make(SyntaxKind::StmtList, Layout,
                             SourcePresence::Present);
  auto Data = StmtListSyntaxData::make(Raw);
  return { Data, Data.get() };
}

/// Make an empty statement list.
StmtListSyntax SyntaxFactory::makeBlankStmtList() {
  auto Raw = RawSyntax::make(SyntaxKind::StmtList, {}, SourcePresence::Present);
  auto Data = StmtListSyntaxData::make(Raw);
  return { Data, Data.get() };
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
  auto Data = SymbolicReferenceExprSyntaxData::make(Raw);
  return { Data, Data.get() };
}

SymbolicReferenceExprSyntax SyntaxFactory::makeBlankSymbolicReferenceExpr() {
  auto Data = SymbolicReferenceExprSyntaxData::makeBlank();
  return { Data, Data.get() };
}

#pragma mark - function-call-argument

FunctionCallArgumentSyntax SyntaxFactory::makeBlankFunctionCallArgument() {
  auto Data = FunctionCallArgumentSyntaxData::makeBlank();
  return { Data, Data.get() };
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
  auto Data = FunctionCallArgumentSyntaxData::make(Raw);
  return { Data, Data.get() };
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
  auto Data = FunctionCallArgumentListSyntaxData::make(Raw);
  return { Data, Data.get() };
}


FunctionCallArgumentListSyntax
SyntaxFactory::makeBlankFunctionCallArgumentList() {
  auto Data = FunctionCallArgumentListSyntaxData::makeBlank();
  return { Data, Data.get() };
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
  auto Data = FunctionCallExprSyntaxData::make(Raw);
  return { Data, Data.get() };
}


FunctionCallExprSyntax SyntaxFactory::makeBlankFunctionCallExpr() {
  auto Data = FunctionCallExprSyntaxData::makeBlank();
  return { Data, Data.get() };
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
  auto Data = SameTypeRequirementSyntaxData::make(Raw);
  return SameTypeRequirementSyntax { Data, Data.get() };
}

SameTypeRequirementSyntax SyntaxFactory::makeBlankSameTypeRequirement() {
  auto Data = SameTypeRequirementSyntaxData::makeBlank();
  return SameTypeRequirementSyntax { Data, Data.get() };
}

GenericRequirementListSyntax SyntaxFactory::
makeGenericRequirementList(std::vector<GenericRequirementSyntax> &Requirements){
  RawSyntax::LayoutList Layout;
  for (auto Req : Requirements) {
    Layout.push_back(Req.getRaw());
  }
  auto Raw = RawSyntax::make(SyntaxKind::GenericRequirementList, Layout,
                             SourcePresence::Present);
  auto Data = GenericRequirementListSyntaxData::make(Raw);
  return { Data, Data.get() };
}

GenericRequirementListSyntax SyntaxFactory::makeBlankGenericRequirementList() {
  auto Data = GenericRequirementListSyntaxData::makeBlank();
  return { Data, Data.get() };
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
  auto Data = TypeAttributeSyntaxData::make(Raw);
  return TypeAttributeSyntax { Data, Data.get() };
}

TypeAttributeSyntax SyntaxFactory::makeBlankTypeAttribute() {
  auto Data = TypeAttributeSyntaxData::makeBlank();
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
  auto Data = BalancedTokensSyntaxData::makeBlank();
  return BalancedTokensSyntax { Data, Data.get() };
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
  auto Data = TypeIdentifierSyntaxData::make(Raw);
  return TypeIdentifierSyntax { Data, Data.get() };
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

#pragma mark - tuple-type

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

TupleTypeSyntax
SyntaxFactory::makeTupleType(llvm::ArrayRef<TupleTypeElementSyntax> Types) {
  RawSyntax::LayoutList RawTypes;
  for (auto &Type : Types) {
    RawTypes.push_back(Type.getRaw());
  }
  auto ArgListRaw = RawSyntax::make(SyntaxKind::TypeArgumentList,
                                    RawTypes,
                                    SourcePresence::Present);
  auto Raw = RawSyntax::make(SyntaxKind::TupleType,
    {
      SyntaxFactory::makeLeftParenToken({}, {}),
      ArgListRaw,
      SyntaxFactory::makeRightParenToken({}, {})
    },
    SourcePresence::Present);
  auto Data = TupleTypeSyntaxData::make(std::move(Raw));
  return TupleTypeSyntax {
    Data, Data.get()
  };
}

TupleTypeElementSyntax
SyntaxFactory::makeTupleTypeElement(RC<TokenSyntax> Name,
                                    TypeSyntax ElementTypeSyntax,
                                    Optional<RC<TokenSyntax>> MaybeComma) {
  auto Data = TupleTypeElementSyntaxData::makeBlank();
  RC<TokenSyntax> Comma;
  if (MaybeComma.hasValue()) {
    Comma = MaybeComma.getValue();
  } else {
    Comma = TokenSyntax::missingToken(tok::comma, ",");
  }
  return TupleTypeElementSyntax { Data, Data.get() }
    .withLabel(Name)
    .withColonToken(SyntaxFactory::makeColonToken({}, Trivia::spaces(1)))
    .withTypeSyntax(ElementTypeSyntax)
    .withCommaToken(Comma);
}


TupleTypeElementSyntax
SyntaxFactory::makeTupleTypeElement(TypeSyntax ElementType,
                                    Optional<RC<TokenSyntax>> MaybeComma) {
  auto Data = TupleTypeElementSyntaxData::makeBlank();
  RC<TokenSyntax> Comma;
  if (MaybeComma.hasValue()) {
    Comma = MaybeComma.getValue();
  } else {
    Comma = TokenSyntax::missingToken(tok::comma, ",");
  }
  return TupleTypeElementSyntax { Data, Data.get() }
    .withTypeSyntax(ElementType)
    .withCommaToken(Comma);
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

  auto Data = OptionalTypeSyntaxData::make(Raw);
  return OptionalTypeSyntax { Data, Data.get() };
}

OptionalTypeSyntax SyntaxFactory::makeBlankOptionalType() {
  auto Data = OptionalTypeSyntaxData::makeBlank();
  return OptionalTypeSyntax { Data, Data.get() };
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
  auto Data = ImplicitlyUnwrappedOptionalTypeSyntaxData::make(Raw);
  return ImplicitlyUnwrappedOptionalTypeSyntax { Data, Data.get() };
}

ImplicitlyUnwrappedOptionalTypeSyntax
SyntaxFactory::makeBlankImplicitlyUnwrappedOptionalType() {
  auto Data = ImplicitlyUnwrappedOptionalTypeSyntaxData::makeBlank();
  return ImplicitlyUnwrappedOptionalTypeSyntax { Data, Data.get() };
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
  auto Data = MetatypeTypeSyntaxData::make(Raw);
  return MetatypeTypeSyntax { Data, Data.get() };
}

MetatypeTypeSyntax SyntaxFactory::makeBlankMetatypeType() {
  auto Data = MetatypeTypeSyntaxData::makeBlank();
  return MetatypeTypeSyntax { Data, Data.get() };
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
  auto Data = TypeArgumentListSyntaxData::makeBlank();
  return TypeArgumentListSyntax { Data, Data.get() };
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
  auto Data = ArrayTypeSyntaxData::make(Raw);
  return ArrayTypeSyntax { Data, Data.get() };
}

ArrayTypeSyntax SyntaxFactory::makeBlankArrayType() {
  auto Data = ArrayTypeSyntaxData::makeBlank();
  return ArrayTypeSyntax { Data, Data.get() };
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
  auto Data = DictionaryTypeSyntaxData::make(Raw);
  return DictionaryTypeSyntax { Data, Data.get() };
}

DictionaryTypeSyntax SyntaxFactory::makeBlankDictionaryType() {
  auto Data = DictionaryTypeSyntaxData::makeBlank();
  return DictionaryTypeSyntax { Data, Data.get() };
}


