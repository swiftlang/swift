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
SyntaxFactory::makeUnknownSyntax(llvm::ArrayRef<TokenSyntax> Tokens) {
  RawSyntax::LayoutList Layout;
  for (auto &Token : Tokens) {
    Layout.push_back(Token.getRaw());
  }
  auto Raw = RawSyntax::make(SyntaxKind::Unknown, Layout,
                             SourcePresence::Present);
  return make<UnknownSyntax>(Raw);
}

#pragma mark - Declarations

#pragma mark - declaration-modifier

DeclModifierSyntax SyntaxFactory::makeDeclModifier(TokenSyntax Name,
                                                   TokenSyntax LeftParen,
                                                   TokenSyntax Argument,
                                                   TokenSyntax RightParen) {
  auto Raw = RawSyntax::make(SyntaxKind::DeclModifier,
                             {
                               Name.getRaw(),
                               LeftParen.getRaw(),
                               Argument.getRaw(),
                               RightParen.getRaw(),
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
SyntaxFactory::makeStructDecl(TokenSyntax StructToken,
                              TokenSyntax Identifier,
                              Syntax GenericParameters,
                              Syntax WhereClause, TokenSyntax LeftBrace,
                              Syntax DeclMembers, TokenSyntax RightBrace) {
  auto Raw = RawSyntax::make(SyntaxKind::StructDecl,
                             {
                               StructToken.getRaw(),
                               Identifier.getRaw(),
                               GenericParameters.getRaw(),
                               WhereClause.getRaw(),
                               LeftBrace.getRaw(),
                               DeclMembers.getRaw(),
                               RightBrace.getRaw()
                             },
                             SourcePresence::Present);
  return make<StructDeclSyntax>(Raw);
}

StructDeclSyntax SyntaxFactory::makeBlankStructDecl() {
  return StructDeclSyntax::makeBlank();
}

#pragma mark - type-alias-declaration

TypeAliasDeclSyntax SyntaxFactory::makeTypealiasDecl(
    TokenSyntax TypealiasToken, TokenSyntax Identifier,
    GenericParameterClauseSyntax GenericParams, TokenSyntax AssignmentToken,
    TypeSyntax Type) {
  auto Raw = RawSyntax::make(SyntaxKind::TypeAliasDecl,
                             {
                               TypealiasToken.getRaw(),
                               Identifier.getRaw(),
                               GenericParams.getRaw(),
                               AssignmentToken.getRaw(),
                               Type.getRaw()
                             },
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
makeFunctionParameter(TokenSyntax ExternalName,
                      TokenSyntax LocalName,
                      TokenSyntax Colon,
                      llvm::Optional<TypeSyntax> ParameterTypeSyntax,
                      TokenSyntax Ellipsis,
                      TokenSyntax Equal,
                      llvm::Optional<ExprSyntax> DefaultValue,
                      TokenSyntax TrailingComma) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionParameter,
                             {
                               ExternalName.getRaw(),
                               LocalName.getRaw(),
                               Colon.getRaw(),
                               ParameterTypeSyntax.hasValue()
                                 ? ParameterTypeSyntax.getValue().getRaw()
                                 : RawSyntax::missing(SyntaxKind::MissingType),
                               Ellipsis.getRaw(),
                               Equal.getRaw(),
                               DefaultValue.hasValue()
                                 ? DefaultValue.getValue().getRaw()
                                 : RawSyntax::missing(SyntaxKind::MissingExpr),
                               TrailingComma.getRaw(),
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
SyntaxFactory::makeFunctionSignature(TokenSyntax LeftParen,
                                     FunctionParameterListSyntax ParameterList,
                                     TokenSyntax RightParen,
                                     TokenSyntax ThrowsOrRethrows,
                                     TokenSyntax Arrow,
                                     TypeAttributesSyntax ReturnTypeAttributes,
                                     TypeSyntax ReturnTypeSyntax) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionSignature,
                             {
                               LeftParen.getRaw(),
                               ParameterList.getRaw(),
                               RightParen.getRaw(),
                               ThrowsOrRethrows.getRaw(),
                               Arrow.getRaw(),
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
                 TokenSyntax FuncKeyword,
                 TokenSyntax Identifier,
                 llvm::Optional<GenericParameterClauseSyntax> GenericParams,
                 FunctionSignatureSyntax Signature,
                 llvm::Optional<GenericWhereClauseSyntax> GenericWhereClause,
                 llvm::Optional<CodeBlockStmtSyntax> Body) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionDecl,
    {
      Attributes.getRaw(),
      Modifiers.getRaw(),
      FuncKeyword.getRaw(),
      Identifier.getRaw(),
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
SyntaxFactory::makeCodeBlock(TokenSyntax LeftBraceToken,
                             StmtListSyntax Elements,
                             TokenSyntax RightBraceToken) {
  auto Raw = RawSyntax::make(SyntaxKind::CodeBlockStmt,
                             {
                               LeftBraceToken.getRaw(),
                               Elements.getRaw(),
                               RightBraceToken.getRaw()
                             }, SourcePresence::Present);
  return make<CodeBlockStmtSyntax>(Raw);
}

CodeBlockStmtSyntax SyntaxFactory::makeBlankCodeBlock() {
  return CodeBlockStmtSyntax::makeBlank();
}

FallthroughStmtSyntax
SyntaxFactory::makeFallthroughStmt(TokenSyntax FallthroughKeyword) {
  auto Raw = RawSyntax::make(SyntaxKind::FallthroughStmt, {
    FallthroughKeyword.getRaw()
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
SyntaxFactory::makeBreakStmt(TokenSyntax BreakKeyword,
                               TokenSyntax Label) {
  auto Raw = RawSyntax::make(SyntaxKind::BreakStmt,
                             {
                               BreakKeyword.getRaw(),
                               Label.getRaw(),
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
SyntaxFactory::makeContinueStmt(TokenSyntax ContinueKeyword,
                                TokenSyntax Label) {
  auto Raw = RawSyntax::make(SyntaxKind::BreakStmt,
                             {
                               ContinueKeyword.getRaw(),
                               Label.getRaw(),
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
SyntaxFactory::makeReturnStmt(TokenSyntax ReturnKeyword,
                              ExprSyntax ReturnedExpression) {
  auto Raw = RawSyntax::make(SyntaxKind::ReturnStmt,
                             {
                               ReturnKeyword.getRaw(),
                               ReturnedExpression.getRaw(),
                             },
                             SourcePresence::Present);
  return make<ReturnStmtSyntax>(Raw);
}

ReturnStmtSyntax SyntaxFactory::makeBlankReturnStmt() {
  auto Raw = RawSyntax::make(SyntaxKind::ReturnStmt,
    {
     RawTokenSyntax::missingToken(tok::kw_return, "return"),
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
SyntaxFactory::makeIntegerLiteralExpr(TokenSyntax Sign,
                                      TokenSyntax Digits) {
  auto Raw = RawSyntax::make(SyntaxKind::IntegerLiteralExpr,
                             {
                               Sign.getRaw(),
                               Digits.getRaw(),
                             },
                             SourcePresence::Present);
  return make<IntegerLiteralExprSyntax>(Raw);
}

IntegerLiteralExprSyntax SyntaxFactory::makeBlankIntegerLiteralExpr() {
  auto Raw = RawSyntax::make(SyntaxKind::IntegerLiteralExpr,
    {
     RawTokenSyntax::missingToken(tok::oper_prefix, "-"),
     RawTokenSyntax::missingToken(tok::integer_literal, ""),
    },
    SourcePresence::Present);
  return make<IntegerLiteralExprSyntax>(Raw);
}

#pragma mark - symbolic-reference

SymbolicReferenceExprSyntax
SyntaxFactory::makeSymbolicReferenceExpr(TokenSyntax Identifier,
  llvm::Optional<GenericArgumentClauseSyntax> GenericArgs) {
  auto Raw = RawSyntax::make(SyntaxKind::SymbolicReferenceExpr,
    {
      Identifier.getRaw(),
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
SyntaxFactory::makeFunctionCallArgument(TokenSyntax Label,
                                        TokenSyntax Colon,
                                        ExprSyntax ExpressionArgument,
                                        TokenSyntax TrailingComma) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionCallArgument,
                             {
                               Label.getRaw(),
                               Colon.getRaw(),
                               ExpressionArgument.getRaw(),
                               TrailingComma.getRaw()
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
                                    TokenSyntax LeftParen,
                                    FunctionCallArgumentListSyntax Arguments,
                                    TokenSyntax RightParen) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionCallExpr,
                             {
                               CalledExpr.getRaw(),
                               LeftParen.getRaw(),
                               Arguments.getRaw(),
                               RightParen.getRaw()
                             },
                             SourcePresence::Present);
  return make<FunctionCallExprSyntax>(Raw);
}


FunctionCallExprSyntax SyntaxFactory::makeBlankFunctionCallExpr() {
  return FunctionCallExprSyntax::makeBlank();
}

#pragma mark - Tokens

TokenSyntax
SyntaxFactory::makeToken(tok Kind, OwnedString Text, SourcePresence Presence,
                         const Trivia &LeadingTrivia,
                         const Trivia &TrailingTrivia) {
  return make<TokenSyntax>(RawTokenSyntax::make(Kind, Text, Presence,
                                                LeadingTrivia, TrailingTrivia));
}

TokenSyntax
SyntaxFactory::makeStaticKeyword(const Trivia &LeadingTrivia,
                                 const Trivia &TrailingTrivia) {

  return makeToken(tok::kw_static, "static", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makePublicKeyword(const Trivia &LeadingTrivia,
                                 const Trivia &TrailingTrivia) {
  return makeToken(tok::kw_public, "public", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeFuncKeyword(const Trivia &LeadingTrivia,
                                      const Trivia &TrailingTrivia) {
  return makeToken(tok::kw_func, "func", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeFallthroughKeyword(const Trivia &LeadingTrivia,
                                      const Trivia &TrailingTrivia) {
  return makeToken(tok::kw_fallthrough, "fallthrough", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeAtSignToken(const Trivia &LeadingTrivia,
                               const Trivia &TrailingTrivia) {
  return makeToken(tok::at_sign, "@", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeBreakKeyword(const swift::syntax::Trivia &LeadingTrivia,
                                const swift::syntax::Trivia &TrailingTrivia) {
  return makeToken(tok::kw_break, "break", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax SyntaxFactory::
makeContinueKeyword(const swift::syntax::Trivia &LeadingTrivia,
                    const swift::syntax::Trivia &TrailingTrivia) {
  return makeToken(tok::kw_continue, "continue", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax SyntaxFactory::
makeReturnKeyword(const swift::syntax::Trivia &LeadingTrivia,
                  const swift::syntax::Trivia &TrailingTrivia) {
  return makeToken(tok::kw_return, "return", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeLeftAngleToken(const Trivia &LeadingTrivia,
                                  const Trivia &TrailingTrivia) {
  return makeToken(tok::l_angle, "<", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeRightAngleToken(const Trivia &LeadingTrivia,
                                   const Trivia &TrailingTrivia) {
  return makeToken(tok::r_angle, ">", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeLeftParenToken(const Trivia &LeadingTrivia,
                                  const Trivia &TrailingTrivia) {
  return makeToken(tok::l_paren, "(", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeRightParenToken(const Trivia &LeadingTrivia,
                                   const Trivia &TrailingTrivia) {
  return makeToken(tok::r_paren, ")", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}
TokenSyntax
SyntaxFactory::makeLeftBraceToken(const Trivia &LeadingTrivia,
                                  const Trivia &TrailingTrivia) {
  return makeToken(tok::l_brace, "{", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeRightBraceToken(const Trivia &LeadingTrivia,
                                   const Trivia &TrailingTrivia) {
  return makeToken(tok::r_brace, "}", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeLeftSquareBracketToken(const Trivia &LeadingTrivia,
                                          const Trivia &TrailingTrivia) {
  return makeToken(tok::l_square, "[", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeRightSquareBracketToken(const Trivia &LeadingTrivia,
                                           const Trivia &TrailingTrivia) {
  return makeToken(tok::r_square, "]", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeQuestionPostfixToken(const Trivia &TrailingTrivia) {
  return makeToken(tok::question_postfix, "?", SourcePresence::Present,
                   {}, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeExclaimPostfixToken(const Trivia &TrailingTrivia) {
  return makeToken(tok::exclaim_postfix, "!", SourcePresence::Present,
                   {}, TrailingTrivia);
}

TokenSyntax SyntaxFactory::makeIdentifier(OwnedString Name,
                                           const Trivia &LeadingTrivia,
                                           const Trivia &TrailingTrivia) {
  return makeToken(tok::identifier, Name, SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax SyntaxFactory::makeCommaToken(const Trivia &LeadingTrivia,
                                             const Trivia &TrailingTrivia) {
  return makeToken(tok::comma, ",", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax SyntaxFactory::makeColonToken(const Trivia &LeadingTrivia,
                                             const Trivia &TrailingTrivia) {
  return makeToken(tok::colon, ":", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax SyntaxFactory::makeDotToken(const Trivia &LeadingTrivia,
                                           const Trivia &TrailingTrivia) {
  return makeToken(tok::period, ".", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax SyntaxFactory::makeStructKeyword(const Trivia &LeadingTrivia,
                                                 const Trivia &TrailingTrivia) {
  return makeToken(tok::kw_struct, "struct", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax SyntaxFactory::makeWhereKeyword(const Trivia &LeadingTrivia,
                                                const Trivia &TrailingTrivia) {
  return makeToken(tok::kw_where, "where", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax SyntaxFactory::makeInoutKeyword(const Trivia &LeadingTrivia,
                                                const Trivia &TrailingTrivia) {
  return makeToken(tok::kw_inout, "inout", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeThrowsKeyword(const Trivia &LeadingTrivia,
                                 const Trivia &TrailingTrivia) {
  return makeToken(tok::kw_throws, "throws", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeRethrowsKeyword(const Trivia &LeadingTrivia,
                                   const Trivia &TrailingTrivia) {
  return makeToken(tok::kw_rethrows, "rethrows", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeTypealiasKeyword(const Trivia &LeadingTrivia,
                                    const Trivia &TrailingTrivia) {
  return makeToken(tok::kw_typealias, "typealias", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeEqualToken(const Trivia &LeadingTrivia,
                              const Trivia &TrailingTrivia) {
  return makeToken(tok::equal, "=", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax SyntaxFactory::makeArrow(const Trivia &LeadingTrivia,
                                      const Trivia &TrailingTrivia) {
  return makeToken(tok::arrow, "->", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeEqualityOperator(const Trivia &LeadingTrivia,
                                    const Trivia &TrailingTrivia) {
  return makeToken(tok::oper_binary_spaced, "==", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax SyntaxFactory::makeTypeToken(const Trivia &LeadingTrivia,
                                          const Trivia &TrailingTrivia) {
  return makeToken(tok::identifier, "Type", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeProtocolToken(const Trivia &LeadingTrivia,
                                 const Trivia &TrailingTrivia) {
  return makeToken(tok::identifier, "Protocol", SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
}

TokenSyntax
SyntaxFactory::makeIntegerLiteralToken(OwnedString Digits,
                                       const Trivia &LeadingTrivia,
                                       const Trivia &TrailingTrivia) {
  return makeToken(tok::integer_literal, Digits, SourcePresence::Present,
                   LeadingTrivia, TrailingTrivia);
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
                               RawTokenSyntax::make(tok::identifier,
                                                    TypeName,
                                                    SourcePresence::Present,
                                                    LeadingTrivia,
                                                    TrailingTrivia),
                               RawTokenSyntax::missingToken(tok::colon, ":"),
                               RawSyntax::missing(SyntaxKind::TypeIdentifier),
                             },
                             SourcePresence::Present);
  return make<GenericParameterSyntax>(Raw);
}

SameTypeRequirementSyntax SyntaxFactory::
makeSameTypeRequirement( TypeIdentifierSyntax LeftTypeIdentifier,
                        TokenSyntax EqualityToken,
                        TypeSyntax RightType) {
  auto Raw = RawSyntax::make(SyntaxKind::SameTypeRequirement,
                             {
                               LeftTypeIdentifier.getRaw(),
                               EqualityToken.getRaw(),
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
TokenSyntax
SyntaxFactory::makePrefixOperator(OwnedString Name,
                                   const Trivia &LeadingTrivia) {
  return makeToken(tok::oper_prefix, Name,
                   SourcePresence::Present, LeadingTrivia, {});
}

#pragma mark - Types

#pragma mark - type-attribute

TypeAttributeSyntax
SyntaxFactory::makeTypeAttribute(TokenSyntax AtSignToken,
                                TokenSyntax Identifier,
                                TokenSyntax LeftParen,
                                BalancedTokensSyntax BalancedTokens,
                                TokenSyntax RightParen) {
  auto Raw = RawSyntax::make(SyntaxKind::TypeAttribute,
                             {
                               AtSignToken.getRaw(),
                               Identifier.getRaw(),
                               LeftParen.getRaw(),
                               BalancedTokens.getRaw(),
                               RightParen.getRaw(),
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
SyntaxFactory::makeBalancedTokens(llvm::ArrayRef<TokenSyntax> Tokens) {
  RawSyntax::LayoutList RawTokens;
  for (auto &Tok : Tokens) {
    RawTokens.push_back(Tok.getRaw());
  }
  auto Raw = RawSyntax::make(SyntaxKind::BalancedTokens,
                             RawTokens,
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
                               RawTokenSyntax::make(tok::identifier,
                                                    Name,
                                                    SourcePresence::Present,
                                                    LeadingTrivia,
                                                    TrailingTrivia),
                               RawSyntax::missing(SyntaxKind::GenericArgumentClause),
                               RawTokenSyntax::missingToken(tok::period, "."),
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
SyntaxFactory::makeTypeIdentifier(TokenSyntax Identifier,
                                  GenericArgumentClauseSyntax GenericArgs) {
  auto Raw = RawSyntax::make(SyntaxKind::TypeIdentifier,
                             {
                               Identifier.getRaw(), GenericArgs.getRaw(),
                               RawTokenSyntax::missingToken(tok::period, "."),
                               RawSyntax::missing(SyntaxKind::TypeIdentifier),
                             },
                             SourcePresence::Present);
  return make<TypeIdentifierSyntax>(Raw);
}

#pragma mark - tuple-type

TupleTypeSyntax SyntaxFactory::makeVoidTupleType() {
  auto Raw = RawSyntax::make(SyntaxKind::TupleType,
    {
      SyntaxFactory::makeLeftParenToken({}, {}).getRaw(),
      RawSyntax::missing(SyntaxKind::TupleTypeElementList),
      SyntaxFactory::makeRightParenToken({}, {}).getRaw(),
    },
    SourcePresence::Present);
  return make<TupleTypeSyntax>(Raw);
}

TupleTypeSyntax
SyntaxFactory::makeTupleType(TokenSyntax LParen,
                             TupleTypeElementListSyntax Elements,
                             TokenSyntax RParen) {
  auto Raw = RawSyntax::make(SyntaxKind::TupleType,
                             { LParen.getRaw(),
                               Elements.getRaw(),
                               RParen.getRaw() },
                             SourcePresence::Present);
  return make<TupleTypeSyntax>(Raw);
}

TupleTypeElementSyntax
SyntaxFactory::makeTupleTypeElement(TokenSyntax Name,
                                    TokenSyntax Colon,
                                    TypeSyntax ElementTypeSyntax,
                                    Optional<TokenSyntax> MaybeComma) {
  TokenSyntax Comma = MaybeComma.getValueOr(
                        TokenSyntax::missingToken(tok::comma, ","));
  return TupleTypeElementSyntax::makeBlank()
    .withLabel(Name)
    .withColonToken(Colon)
    .withTypeSyntax(ElementTypeSyntax)
    .withCommaToken(Comma);
}


TupleTypeElementSyntax
SyntaxFactory::makeTupleTypeElement(TypeSyntax ElementType,
                                    Optional<TokenSyntax> MaybeComma) {
  TokenSyntax Comma = MaybeComma.getValueOr(
                        TokenSyntax::missingToken(tok::comma, ","));
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
    SyntaxFactory::makeQuestionPostfixToken(TrailingTrivia).getRaw(),
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
      SyntaxFactory::makeExclaimPostfixToken(TrailingTrivia).getRaw(),
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
                                                   TokenSyntax DotToken,
                                                   TokenSyntax TypeToken) {
  auto Raw = RawSyntax::make(SyntaxKind::MetatypeType,
                             {
                               BaseType.getRaw(),
                               DotToken.getRaw(),
                               TypeToken.getRaw()
                             },
                             SourcePresence::Present);
  return make<MetatypeTypeSyntax>(Raw);
}

MetatypeTypeSyntax SyntaxFactory::makeBlankMetatypeType() {
  return MetatypeTypeSyntax::makeBlank();
}

#pragma mark - function-type

FunctionTypeSyntax SyntaxFactory::makeFunctionType(
    TypeAttributesSyntax TypeAttributes, TokenSyntax LeftParen,
    FunctionParameterListSyntax ArgumentList, TokenSyntax RightParen,
    TokenSyntax ThrowsOrRethrows, TokenSyntax Arrow,
    TypeSyntax ReturnType) {
  auto Raw =
      RawSyntax::make(SyntaxKind::FunctionType,
                      {
                        TypeAttributes.getRaw(),
                        LeftParen.getRaw(),
                        ArgumentList.getRaw(),
                        RightParen.getRaw(),
                        ThrowsOrRethrows.getRaw(),
                        Arrow.getRaw(),
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
makeFunctionTypeArgument(TokenSyntax ExternalParameterName,
                         TokenSyntax LocalParameterName,
                         TypeAttributesSyntax TypeAttributes,
                         TokenSyntax InoutKeyword,
                         TokenSyntax ColonToken,
                         TypeSyntax ParameterTypeSyntax) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionTypeArgument,
                             {
                               ExternalParameterName.getRaw(),
                               LocalParameterName.getRaw(),
                               TypeAttributes.getRaw(),
                               InoutKeyword.getRaw(),
                               ColonToken.getRaw(),
                               ParameterTypeSyntax.getRaw()
                             }, SourcePresence::Present);
  return make<FunctionTypeArgumentSyntax>(Raw);
}

FunctionTypeArgumentSyntax SyntaxFactory::
makeFunctionTypeArgument(TokenSyntax LocalParameterName,
                         TokenSyntax ColonToken,
                         swift::syntax::TypeSyntax TypeArgument) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionTypeArgument,
                             {
                               RawTokenSyntax::missingToken(tok::identifier,
                                                            ""),
                               LocalParameterName.getRaw(),
                               RawSyntax::missing(SyntaxKind::TypeAttributes),
                               RawTokenSyntax::missingToken(tok::kw_inout,
                                                            "inout"),
                               ColonToken.getRaw(),
                               TypeArgument.getRaw()
                             }, SourcePresence::Present);
  return make<FunctionTypeArgumentSyntax>(Raw);
}

FunctionTypeArgumentSyntax
SyntaxFactory::makeFunctionTypeArgument(TypeSyntax TypeArgument) {
  auto Raw = RawSyntax::make(SyntaxKind::FunctionTypeArgument,
    {
      RawTokenSyntax::missingToken(tok::identifier, ""),
      RawTokenSyntax::missingToken(tok::identifier, ""),
      RawSyntax::missing(SyntaxKind::TypeAttributes),
      RawTokenSyntax::missingToken(tok::kw_inout, "inout"),
      RawTokenSyntax::missingToken(tok::colon, ":"),
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
SyntaxFactory::makeArrayType(TokenSyntax LeftSquareBracket,
                             TypeSyntax ElementType,
                             TokenSyntax RightSquareBracket) {
  auto Raw = RawSyntax::make(SyntaxKind::ArrayType,
                             {
                               LeftSquareBracket.getRaw(),
                               ElementType.getRaw(),
                               RightSquareBracket.getRaw()
                             },
                             SourcePresence::Present);
  return make<ArrayTypeSyntax>(Raw);
}

ArrayTypeSyntax SyntaxFactory::makeBlankArrayType() {
  return ArrayTypeSyntax::makeBlank();
}

#pragma mark - dictionary-type

DictionaryTypeSyntax
SyntaxFactory::makeDictionaryType(TokenSyntax LeftSquareBracket,
                                  TypeSyntax KeyType,
                                  TokenSyntax Colon,
                                  TypeSyntax ValueType,
                                  TokenSyntax RightSquareBracket) {
  auto Raw = RawSyntax::make(SyntaxKind::DictionaryType,
                             {
                               LeftSquareBracket.getRaw(),
                               KeyType.getRaw(),
                               Colon.getRaw(),
                               ValueType.getRaw(),
                               RightSquareBracket.getRaw()
                             },
                             SourcePresence::Present);
  return make<DictionaryTypeSyntax>(Raw);
}

DictionaryTypeSyntax SyntaxFactory::makeBlankDictionaryType() {
  return DictionaryTypeSyntax::makeBlank();
}
