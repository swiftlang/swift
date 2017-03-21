//===--- SyntaxFactory.h - Swift Syntax Builder Interface -------*- C++ -*-===//
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
//
// This file defines the SyntaxFactory, one of the most important client-facing
// types in lib/Syntax and likely to be very commonly used.
//
// Effectively a namespace, SyntaxFactory is never instantiated, but is *the*
// one-stop shop for making new Syntax nodes. Putting all of these into a
// collection of static methods provides a single point of API lookup for
// clients' convenience and also allows the library to hide all of the
// constructors for all Syntax nodes, as the SyntaxFactory is friend to all.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_SyntaxFactory_H
#define SWIFT_SYNTAX_SyntaxFactory_H

#include "swift/Syntax/DeclSyntax.h"
#include "swift/Syntax/GenericSyntax.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/TypeSyntax.h"
#include "swift/Syntax/Trivia.h"
#include "llvm/ADT/ArrayRef.h"

#include <vector>

namespace swift {
namespace syntax {

#define SYNTAX(Id, Parent) class Id##Syntax;
#include "swift/Syntax/SyntaxKinds.def"
class DeclSyntax;
class ExprSyntax;
class StmtSyntax;
class UnknownSyntax;
struct TokenSyntax;

/// The Syntax builder - the one-stop shop for making new Syntax nodes.
struct SyntaxFactory {
  /// Collect a list of tokens into a piece of "unknown" syntax.
  static UnknownSyntax
  makeUnknownSyntax(llvm::ArrayRef<RC<TokenSyntax>> Tokens);

#pragma mark - Declarations

#pragma mark - declaration-modifier

  /// Make a declaration modifier with the specified elements.
  static DeclModifierSyntax
  makeDeclModifier(RC<TokenSyntax> Name, RC<TokenSyntax> LeftParen,
                   RC<TokenSyntax> Argument, RC<TokenSyntax> RightParen);

  /// Make a declaration modifier with all missing elements.
  static DeclModifierSyntax makeBlankDeclModifier();

  /// Make a declaration modifier list with the specified modifiers.
  static DeclModifierListSyntax
  makeDeclModifierList(const std::vector<DeclModifierSyntax> &Modifiers);

  /// Make an empty declaration modifier list.
  static DeclModifierListSyntax makeBlankDeclModifierList();

#pragma mark - struct-declaration

  /// Make a struct declaration with the specified elements.
  static StructDeclSyntax
  makeStructDecl(RC<TokenSyntax> StructToken, RC<TokenSyntax> Identifier,
                 Syntax GenericParameters, Syntax WhereClause,
                 RC<TokenSyntax> LeftBrace, Syntax DeclMembers,
                 RC<TokenSyntax> RightBrace);

  /// Make a struct declaration with all missing elements.
  static StructDeclSyntax makeBlankStructDecl();

  /// Make a typealias declaration with the specified elements.
  static TypeAliasDeclSyntax
  makeTypealiasDecl(RC<TokenSyntax> TypealiasToken, RC<TokenSyntax> Identifier,
                    GenericParameterClauseSyntax GenericParams,
                    RC<TokenSyntax> AssignmentToken, TypeSyntax Type);

  /// Make a typealias declaration with all missing elements.
  static TypeAliasDeclSyntax makeBlankTypealiasDecl();

  /// Make an empty list of declaration members.
  static DeclMembersSyntax makeBlankDeclMembers();

  /// Make a function declaration with the specified elements.
  static FunctionDeclSyntax
  makeFunctionDecl(TypeAttributesSyntax Attributes,
                   DeclModifierListSyntax Modifiers,
                   RC<TokenSyntax> FuncKeyword,
                   RC<TokenSyntax> Identifier,
                   llvm::Optional<GenericParameterClauseSyntax> GenericParams,
                   FunctionSignatureSyntax Signature,
                   llvm::Optional<GenericWhereClauseSyntax> GenericWhereClause,
                   llvm::Optional<CodeBlockStmtSyntax> Body);

  /// Make a function declaration with all missing elements.
  static FunctionDeclSyntax makeBlankFunctionDecl();

#pragma mark - function-parameter

  /// Make a function parameter with the given elements.
  static FunctionParameterSyntax
  makeFunctionParameter(RC<TokenSyntax> ExternalName, RC<TokenSyntax> LocalName,
                        RC<TokenSyntax> Colon,
                        llvm::Optional<TypeSyntax> ParameterTypeSyntax,
                        RC<TokenSyntax> Ellipsis,
                        RC<TokenSyntax> Equal,
                        llvm::Optional<ExprSyntax> DefaultValue,
                        RC<TokenSyntax> TrailingComma);

  /// Make a function parameter with all elements marked as missing.
  static FunctionParameterSyntax makeBlankFunctionParameter();

#pragma mark - function-parameter-list

  /// Make a function parameter list with the given parameters.
  static FunctionParameterListSyntax makeFunctionParameterList(
    const std::vector<FunctionParameterSyntax> &Parameters);

  /// Make an empty function parameter list.
  static FunctionParameterListSyntax makeBlankFunctionParameterList();

#pragma mark - function-signature

  /// Make a function signature with the given elements.
  static FunctionSignatureSyntax
  makeFunctionSignature(RC<TokenSyntax> LeftParen,
                        FunctionParameterListSyntax ParameterList,
                        RC<TokenSyntax> RightParen,
                        RC<TokenSyntax> ThrowsOrRethrows,
                        RC<TokenSyntax> Arrow,
                        TypeAttributesSyntax ReturnTypeAttributes,
                        TypeSyntax ReturnTypeSyntax);

  /// Make a blank function signature.
  static FunctionSignatureSyntax makeBlankFunctionSignature();

  //-/ Make a function declaration with the given elements.
  // TODO

  //-/ Make a blank function declaration.
  // TODO

#pragma mark - Statements

  /// Make a code block with the specified elements.
  static CodeBlockStmtSyntax
  makeCodeBlock(RC<TokenSyntax> LeftBraceToken,
                StmtListSyntax Elements,
                RC<TokenSyntax> RightBraceToken);

  /// Make a code block with all missing elements.
  static CodeBlockStmtSyntax makeBlankCodeBlock();

  /// Make a fallthrough statement with the give `fallthrough` keyword.
  static FallthroughStmtSyntax
  makeFallthroughStmt(RC<TokenSyntax> FallthroughKeyword);

  /// Make a fallthrough statement with the `fallthrough` keyword
  /// marked as missing.
  static FallthroughStmtSyntax makeBlankFallthroughStmt();

  /// Make a break statement with the give `break` keyword and
  /// destination label.
  static BreakStmtSyntax
  makeBreakStmt(RC<TokenSyntax> BreakKeyword, RC<TokenSyntax> Label);

  /// Make a break statement with the `break` keyword
  /// and destination label marked as missing.
  static BreakStmtSyntax makeBlankBreakStmtSyntax();

  /// Make a continue statement with the given `continue` keyword and
  /// destination label.
  static ContinueStmtSyntax
  makeContinueStmt(RC<TokenSyntax> ContinueKeyword, RC<TokenSyntax> Label);

  /// Make a continue statement with the `continue` keyword
  /// and destination label marked as missing.
  static ContinueStmtSyntax makeBlankContinueStmtSyntax();

  /// Make a return statement with the given `return` keyword and returned
  /// expression.
  static ReturnStmtSyntax
  makeReturnStmt(RC<TokenSyntax> ReturnKeyword, ExprSyntax ReturnedExpression);

  /// Make a return statement with the `return` keyword and return expression
  /// marked as missing.
  static ReturnStmtSyntax makeBlankReturnStmt();

  /// Make a statement list from a loosely connected list of statements.
  static StmtListSyntax makeStmtList(const std::vector<StmtSyntax> &Statements);

  /// Make an empty statement list.
  static StmtListSyntax makeBlankStmtList();

#pragma mark - Expressions

  /// Make an integer literal with the given '+'/'-' sign and string of digits.
  static IntegerLiteralExprSyntax
  makeIntegerLiteralExpr(RC<TokenSyntax> Sign, RC<TokenSyntax> Digits);

  /// Make an integer literal with the sign and string of digits marked
  /// as missing.
  static IntegerLiteralExprSyntax makeBlankIntegerLiteralExpr();

  /// Make a symbolic reference with the given identifier and optionally a
  /// generic argument clause.
  static SymbolicReferenceExprSyntax
  makeSymbolicReferenceExpr(RC<TokenSyntax> Identifier,
    llvm::Optional<GenericArgumentClauseSyntax> GenericArgs);

  /// Make a symbolic reference expression with the identifier and
  /// generic argument clause marked as missing.
  static SymbolicReferenceExprSyntax makeBlankSymbolicReferenceExpr();

  /// Make a function call argument with all elements marked as missing.
  static FunctionCallArgumentSyntax makeBlankFunctionCallArgument();

  /// Make a function call argument based on an expression with the
  /// given elements.
  static FunctionCallArgumentSyntax
  makeFunctionCallArgument(RC<TokenSyntax> Label, RC<TokenSyntax> Colon,
                           ExprSyntax ExpressionArgument,
                           RC<TokenSyntax> TrailingComma);

  /// Make a function call argument list with the given arguments.
  static FunctionCallArgumentListSyntax
  makeFunctionCallArgumentList(
    std::vector<FunctionCallArgumentSyntax> Arguments);

  /// Make a function call argument list with no arguments.
  static FunctionCallArgumentListSyntax makeBlankFunctionCallArgumentList();

  /// Make a function call expression with the given elements.
  static FunctionCallExprSyntax
  makeFunctionCallExpr(ExprSyntax CalledExpr, RC<TokenSyntax> LeftParen,
                       FunctionCallArgumentListSyntax Arguments,
                       RC<TokenSyntax> RightParen);

  /// Make a function call expression with all elements marked as missing.
  static FunctionCallExprSyntax makeBlankFunctionCallExpr();

#pragma mark - Tokens

  /// Make a 'static' keyword with the specified leading and trailing trivia.
  static RC<TokenSyntax> makeStaticKeyword(const Trivia &LeadingTrivia,
                                           const Trivia &TrailingTrivia);


  /// Make a 'public' keyword with the specified leading and trailing trivia.
  static RC<TokenSyntax> makePublicKeyword(const Trivia &LeadingTrivia,
                                           const Trivia &TrailingTrivia);

  /// Make a 'func' keyword with the specified leading and trailing trivia.
  static RC<TokenSyntax> makeFuncKeyword(const Trivia &LeadingTrivia,
                                         const Trivia &TrailingTrivia);

  /// Make a 'fallthrough' keyword with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeFallthroughKeyword(const Trivia &LeadingTrivia,
                                                const Trivia &TrailingTrivia);

  /// Make an at-sign '@' token with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeAtSignToken(const Trivia &LeadingTrivia,
                                         const Trivia &TrailingTrivia);

  /// Make a 'break' keyword with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeBreakKeyword(const Trivia &LeadingTrivia,
                                          const Trivia &TrailingTrivia);

  /// Make a 'continue' keyword with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeContinueKeyword(const Trivia &LeadingTrivia,
                                             const Trivia &TrailingTrivia);

  /// Make a 'return' keyword with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeReturnKeyword(const Trivia &LeadingTrivia,
                                           const Trivia &TrailingTrivia);

  /// Make a left angle '<' token with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeLeftAngleToken(const Trivia &LeadingTrivia,
                                            const Trivia &TrailingTrivia);

  /// Make a right angle '>' token with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeRightAngleToken(const Trivia &LeadingTrivia,
                                             const Trivia &TrailingTrivia);

  /// Make a left parenthesis '(' token with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeLeftParenToken(const Trivia &LeadingTrivia,
                                            const Trivia &TrailingTrivia);

  /// Make a right parenthesis ')' token with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeRightParenToken(const Trivia &LeadingTrivia,
                                            const Trivia &TrailingTrivia);

  /// Make a left brace '{' token with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeLeftBraceToken(const Trivia &LeadingTrivia,
                                            const Trivia &TrailingTrivia);

  /// Make a right brace '}' token with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeRightBraceToken(const Trivia &LeadingTrivia,
                                             const Trivia &TrailingTrivia);


  /// Make a left square bracket '[' token with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax>
  makeLeftSquareBracketToken(const Trivia &LeadingTrivia,
                             const Trivia &TrailingTrivia);

  /// Make a right square bracket ']' token with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax>
  makeRightSquareBracketToken(const Trivia &LeadingTrivia,
                              const Trivia &TrailingTrivia);

  /// Make a postfix question '?' token with the specified trailing trivia.
  /// The leading trivia is assumed to be of zero width.
  static RC<TokenSyntax>
  makeQuestionPostfixToken(const Trivia &TrailingTrivia);

  /// Make an exclamation '!' token with the specified trailing trivia.
  /// The leading trivia is assumed to be of zero width.
  static RC<TokenSyntax> makeExclaimPostfixToken(const Trivia &TrailingTrivia);

  /// Make an identifier token with the specified leading and trailing trivia.
  static RC<TokenSyntax> makeIdentifier(OwnedString Name,
                                        const Trivia &LeadingTrivia,
                                        const Trivia &TrailingTrivia);

  /// Make a comma ',' token with the specified leading and trailing trivia.
  static RC<TokenSyntax> makeCommaToken(const Trivia &LeadingTrivia,
                                        const Trivia &TrailingTrivia);

  /// Make a colon ':' token with the specified leading and trailing trivia.
  static RC<TokenSyntax> makeColonToken(const Trivia &LeadingTrivia,
                                        const Trivia &TrailingTrivia);

  /// Make a dot '.' token with the specified leading and trailing trivia.
  static RC<TokenSyntax> makeDotToken(const Trivia &LeadingTrivia,
                                      const Trivia &TrailingTrivia);

  /// Make a 'struct' keyword with the specified leading and trailing trivia.
  static RC<TokenSyntax> makeStructKeyword(const Trivia &LeadingTrivia,
                                           const Trivia &TrailingTrivia);

  /// Make a 'where' keyword with the specified leading and trailing trivia.
  static RC<TokenSyntax> makeWhereKeyword(const Trivia &LeadingTrivia,
                                          const Trivia &TrailingTrivia);

  /// Make a 'inout' keyword with the specified leading and trailing trivia.
  static RC<TokenSyntax> makeInoutKeyword(const Trivia &LeadingTrivia,
                                          const Trivia &TrailingTrivia);

  /// Make a 'throws' keyword with the specified leading and trailing trivia.
  static RC<TokenSyntax> makeThrowsKeyword(const Trivia &LeadingTrivia,
                                           const Trivia &TrailingTrivia);

  /// Make a 'rethrows' keyword with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeRethrowsKeyword(const Trivia &LeadingTrivia,
                                             const Trivia &TrailingTrivia);

  /// Make a 'typealias' keyword with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeTypealiasKeyword(const Trivia &LeadingTrivia,
                                              const Trivia &TrailingTrivia);

  /// Make an equal '=' token with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeEqualToken(const Trivia &LeadingTrivia,
                                        const Trivia &TrailingTrivia);

  /// Make an arrow '->' token with the specified leading and trailing trivia.
  static RC<TokenSyntax> makeArrow(const Trivia &LeadingTrivia,
                                   const Trivia &TrailingTrivia);

  /// Make an equality '==' binary operator with the specified leading and
  /// trailing trivia.
  static RC<TokenSyntax> makeEqualityOperator(const Trivia &LeadingTrivia,
                                           const Trivia &TrailingTrivia);

  /// Make the terminal identifier token `Type`
  static RC<TokenSyntax> makeTypeToken(const Trivia &LeadingTrivia,
                                       const Trivia &TrailingTrivia);

  /// Make the terminal identifier token `Protocol`
  static RC<TokenSyntax> makeProtocolToken(const Trivia &LeadingTrivia,
                                           const Trivia &TrailingTrivia);

  /// Make a token representing the digits of an integer literal.
  ///
  /// Note: This is not a stand-in for the expression, which can contain
  /// a minus sign.
  static RC<TokenSyntax> makeIntegerLiteralToken(OwnedString Digits,
                                                 const Trivia &LeadingTrivia,
                                                 const Trivia &TrailingTrivia);

#pragma mark - Operators

  /// Make a prefix operator with the given text.
  static RC<TokenSyntax> makePrefixOperator(OwnedString Name,
                                             const Trivia &LeadingTrivia);

#pragma mark - Types

#pragma mark - type-attribute

  /// Make a type attribute with the specified elements.
  static TypeAttributeSyntax
  makeTypeAttribute(RC<TokenSyntax> AtSignToken,
                    RC<TokenSyntax> Identifier,
                    RC<TokenSyntax> LeftParen,
                    BalancedTokensSyntax BalancedTokens,
                    RC<TokenSyntax> RightParen);

  /// Make a type attribute with all elements marked as missing.
  static TypeAttributeSyntax makeBlankTypeAttribute();

#pragma mark - type-attributes

  /// Make a set of type attributes with all elements marked as missing.
  static TypeAttributesSyntax makeBlankTypeAttributes();

#pragma mark - balanced-tokens

  /// Make a list of balanced tokens.
  static BalancedTokensSyntax
  makeBalancedTokens(RawSyntax::LayoutList Tokens);

  /// Make an empty list of balanced tokens.
  static BalancedTokensSyntax makeBlankBalancedTokens();

#pragma mark - type-identifier

  /// Make a non-generic type identifier with some name.
  static TypeIdentifierSyntax
  makeTypeIdentifier(OwnedString Name, const Trivia &LeadingTrivia,
                     const Trivia &TrailingTrivia);

  /// Make a generic type identifier.
  static TypeIdentifierSyntax
  makeTypeIdentifier(RC<TokenSyntax> Identifier,
                     GenericArgumentClauseSyntax GenericArgs);

  /// Make a bare "Any" type.
  static TypeIdentifierSyntax makeAnyTypeIdentifier();

  /// Make a bare "Self" type.
  static TypeIdentifierSyntax makeSelfTypeIdentifier();

#pragma mark - tuple-type

  /// Make a bare "()" void tuple type
  static TupleTypeSyntax makeVoidTupleType();

  /// Make a tuple type from an array of types and the provided left/right
  /// paren tokens.
  static TupleTypeSyntax
  makeTupleType(RC<TokenSyntax> LParen,
                TupleTypeElementListSyntax Elements,
                RC<TokenSyntax> RParen);

  /// Make a tuple type element of the form 'Name: ElementType'
  static TupleTypeElementSyntax
  makeTupleTypeElement(RC<TokenSyntax> Name, RC<TokenSyntax> Colon,
                       TypeSyntax ElementType,
                       llvm::Optional<RC<TokenSyntax>> MaybeComma = llvm::None);

  /// Make a tuple type element without a label.
  static TupleTypeElementSyntax
  makeTupleTypeElement(TypeSyntax ElementType,
                       llvm::Optional<RC<TokenSyntax>> MaybeComma = llvm::None);

#pragma mark - optional-type

  /// Make an optional type, such as `Int?`
  static OptionalTypeSyntax
  makeOptionalType(TypeSyntax BaseType, const Trivia &TrailingTrivia);

  /// Make an optional type with all elements marked as missing.
  static OptionalTypeSyntax makeBlankOptionalType();

#pragma mark - implicitly-unwrapped-optional-type

  /// Make an implicitly unwrapped optional type, such as `Int!`
  static ImplicitlyUnwrappedOptionalTypeSyntax
  makeImplicitlyUnwrappedOptionalType(TypeSyntax BaseType,
                                      const Trivia &TrailingTrivia);

  static ImplicitlyUnwrappedOptionalTypeSyntax
  makeBlankImplicitlyUnwrappedOptionalType();

#pragma mark - metatype-type

  /// Make a metatype type, as in `T.Type`
  /// `Type` is a terminal token here, not a placeholder for something else.
  static MetatypeTypeSyntax makeMetatypeType(TypeSyntax BaseType,
                                             RC<TokenSyntax> DotToken,
                                             RC<TokenSyntax> TypeToken);

  /// Make a metatype type with all elements marked as missing.
  static MetatypeTypeSyntax makeBlankMetatypeType();

#pragma mark - array-type

  /// Make a sugared Array type, as in `[MyType]`.
  static ArrayTypeSyntax makeArrayType(RC<TokenSyntax> LeftSquareBracket,
                                       TypeSyntax ElementType,
                                       RC<TokenSyntax> RightSquareBracket);

  /// Make an array type with all elements marked as missing.
  static ArrayTypeSyntax makeBlankArrayType();

#pragma mark - dictionary-type

  /// Make a Dictionary type, as in `[Key : Value]`.
  static DictionaryTypeSyntax
  makeDictionaryType(RC<TokenSyntax> LeftSquareBracket, TypeSyntax KeyType,
                     RC<TokenSyntax> Colon, TypeSyntax ValueType,
                     RC<TokenSyntax> RightSquareBracket);

  /// Make an a dictionary type with all elements marked as missing.
  static DictionaryTypeSyntax makeBlankDictionaryType();

#pragma mark - function-type-argument

  /// Make a function argument type syntax with the specified elements.
  static FunctionTypeArgumentSyntax
  makeFunctionTypeArgument(RC<TokenSyntax> ExternalParameterName,
                           RC<TokenSyntax> LocalParameterName,
                           TypeAttributesSyntax TypeAttributes,
                           RC<TokenSyntax> InoutKeyword,
                           RC<TokenSyntax> ColonToken,
                           TypeSyntax ParameterTypeSyntax);

  /// Make a simple function type argument syntax with the given label and
  /// simple type name.
  static FunctionTypeArgumentSyntax
  makeFunctionTypeArgument(RC<TokenSyntax> LocalParameterName,
                           RC<TokenSyntax> ColonToken,
                           TypeSyntax ParameterType);

  /// Make a simple function type argument syntax with the given simple
  /// type name.
  static FunctionTypeArgumentSyntax
  makeFunctionTypeArgument(TypeSyntax TypeArgument);

  /// Make a function argument type syntax with all elements marked as missing.
  static FunctionTypeArgumentSyntax makeBlankFunctionArgumentType();

#pragma mark - function-type

  /// Make a function type, for example, `(Int, Int) throws -> Int`
  static FunctionTypeSyntax
  makeFunctionType(TypeAttributesSyntax TypeAttributes,
                   RC<TokenSyntax> LeftParen,
                   TupleTypeElementListSyntax ArgumentList,
                   RC<TokenSyntax> RightParen, RC<TokenSyntax> ThrowsOrRethrows,
                   RC<TokenSyntax> Arrow, TypeSyntax ReturnType);

  /// Make a function type with all elements marked as missing.
  static FunctionTypeSyntax makeBlankFunctionType();

#pragma mark - tuple-type-element-list

  /// Make a list of type arguments with all elements marked as missing.
  static TupleTypeElementListSyntax makeBlankTupleTypeElementList();

#pragma mark - Generics

  /// Make an empty generic parameter clause.
  static GenericParameterClauseSyntax
  makeBlankGenericParameterClause();

  /// Make an empty generic argument clause.
  static GenericArgumentClauseSyntax makeBlankGenericArgumentClause();

  /// Make an empty generic where clause.
  static GenericWhereClauseSyntax makeBlankGenericWhereClause();

  /// Make a same-type requirement with the specified elements.
  ///
  /// Any elements are allowed to be marked as missing.
  static SameTypeRequirementSyntax
  makeSameTypeRequirement(TypeIdentifierSyntax LeftTypeIdentifier,
                          RC<TokenSyntax> EqualityToken, TypeSyntax RightType);

  /// Make a list of generic requirements with the given loosely collected
  /// requirements/
  static GenericRequirementListSyntax makeGenericRequirementList(
    std::vector<GenericRequirementSyntax> &Requirements);

  /// Make an empty list of generic requirements.
  static GenericRequirementListSyntax makeBlankGenericRequirementList();

  /// Make a same-type requirement with all elements marked as missing.
  static SameTypeRequirementSyntax makeBlankSameTypeRequirement();

  /// Make an empty same-type-requirement with all missing elements.
  static SameTypeRequirementSyntax makeSameTypeRequirement();

  /// Make a generic parameter with the specified name and trivia.
  static GenericParameterSyntax
  makeGenericParameter(OwnedString TypeName, const Trivia &LeadingTrivia,
                       const Trivia &TrailingTrivia);

  /// Make a generic parameter with all elements marked as missing.
  static GenericParameterSyntax makeBlankGenericParameter();
};
} // end namespace syntax
} // end namespace swift
#endif // SWIFT_SYNTAX_SyntaxFactory_H
