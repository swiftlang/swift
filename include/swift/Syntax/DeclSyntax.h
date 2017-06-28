//===--- DeclSyntax.h - Declaration Syntax Interface ------------*- C++ -*-===//
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
// This file defines the interface for declaration-specific syntax nodes,
// such as for structures, enumerations, type aliases, and constants.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_DECLSYNTAX_H
#define SWIFT_SYNTAX_DECLSYNTAX_H

#include "swift/Syntax/References.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/SyntaxCollection.h"
#include "swift/Syntax/SyntaxData.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/TypeSyntax.h"
#include "swift/Syntax/UnknownSyntax.h"

#include "llvm/ADT/BitVector.h"

namespace swift {
namespace syntax {

class ExprSyntax;
class CodeBlockStmtSyntax;
class GenericWhereClauseSyntax;

#pragma mark declaration-modifier API

/// declaration-modifier -> access-level-modifier
///                       | mutation-modifier
///                       | 'class'
///                       | 'convenience'
///                       | 'dynamic'
///                       | 'final'
///                       | 'infix'
///                       | 'lazy'
///                       | 'optional'
///                       | 'override'
///                       | 'postfix'
///                       | 'prefix'
///                       | 'required'
///                       | 'static'
///                       | 'unowned'
///                       | 'unowned(safe)'
///                       | 'unowned(unsafe)'
///                       | 'weak'
/// access-level-modifier -> 'private' | 'private' '(' 'set' ')'
///                        | 'fileprivate' | 'fileprivate' '(' 'set' ')'
///                        | 'internal' | 'internal' '(' 'set' ')'
///                        | 'public' | 'public' '(' 'set' ')'
///                        | 'open' | 'open' '(' 'set' ')'
/// mutation-modifier -> 'mutating' | 'nonmutating'
class DeclModifierSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class Syntax;
  friend class SyntaxData;

  enum class Cursor : CursorIndex {
    Name,
    LeftParen,
    Argument,
    RightParen
  };

  virtual void validate() const override;

public:
  static DeclModifierSyntax makeBlank();
  DeclModifierSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : Syntax(Root, Data) {}

  /// Return the name of the modifier.
  TokenSyntax getName() const;

  /// Return a DeclModifierSyntax with the given name.
  DeclModifierSyntax withName(TokenSyntax NewName) const;

  /// Return the left parenthesis '(' token as a part of the argument clause,
  /// if there is one.
  TokenSyntax getLeftParenToken() const;

  /// Return a DeclModifierSyntax with the given left parenthesis '(' token.
  DeclModifierSyntax withLeftParenToken(TokenSyntax NewLeftParen) const;

  /// Get the argument to the declaration modifier.
  ///
  /// This is either:
  /// - 'set' for the access modifiers such as 'private' or 'public', or
  /// - 'safe' / 'unsafe' for the 'unowned' modifier.
  TokenSyntax getArgument() const;

  /// Return a DeclModifierSyntax with the given argument.
  DeclModifierSyntax withArgument(TokenSyntax NewArgument) const;

  /// Return the right parenthesis ')' token as a part of the argument clause,
  /// if there is one.
  TokenSyntax getRightParenToken() const;

  /// Return a DeclModifierSyntax with the given right parenthesis ')' token.
  DeclModifierSyntax withRightParenToken(TokenSyntax NewRightParen) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::DeclModifier;
  }
};

#pragma mark declaration API

class DeclSyntax : public Syntax {
  friend class Syntax;
protected:
  virtual void validate() const override {}

public:
  static DeclSyntax makeBlank();
  DeclSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : Syntax(Root, Data) {}

  static bool classof(const SyntaxData *S) { return S->isDecl(); }
};

#pragma mark - unknown-declaration API

class UnknownDeclSyntax : public UnknownSyntax {
  friend class SyntaxData;
  friend class LegacyASTTransformer;

  virtual void validate() const override;

public:
  static UnknownDeclSyntax makeBlank();
  UnknownDeclSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : UnknownSyntax(Root, Data)  {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::UnknownDecl;
  }
};

#pragma mark -
#pragma mark declaration-members API

class DeclMembersSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class SyntaxData;
  friend class Syntax;
  friend class DeclMembersSyntaxBuilder;
  friend class StructDeclSyntax;

  virtual void validate() const override {}

public:
  static DeclMembersSyntax makeBlank();
  DeclMembersSyntax(RC<SyntaxData> Root, const SyntaxData *Data)
    : Syntax(Root, Data) {}
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::DeclMembers;
  }
};

#pragma mark -
#pragma mark declaration-members Builder (TODO)

class DeclMembersSyntaxBuilder final {
  RawSyntax::LayoutList MembersLayout;
public:
  DeclMembersSyntaxBuilder &addMember(DeclSyntax Member);
  DeclMembersSyntax build() const;
};

#pragma mark -

#pragma mark - struct-declaration API

/// struct-declaration -> attributes? access-level-modifier?
///                       'struct' struct-name
///                       generic-parameter-clause? type-inheritance-clause?
///                       generic-where-clause?
///                       '{' struct-members '}'
/// struct-name -> identifier
/// struct-members -> struct-member struct-members?
/// struct-member -> declaration | compiler-control-statement
class StructDeclSyntax final : public DeclSyntax {
  friend struct SyntaxFactory;
  friend class Syntax;
  friend class SyntaxData;
  friend class StructDeclSyntaxBuilder;

  enum class Cursor : CursorIndex {
    StructKeyword,
    Identifier,
    GenericParameterClause,
    GenericWhereClause,
    LeftBrace,
    Members,
    RightBrace,
    First = StructKeyword,
    Last = RightBrace,
  };

  virtual void validate() const override;

public:
  static StructDeclSyntax makeBlank();
  StructDeclSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : DeclSyntax(Root, Data) {}

  /// Return the 'struct' keyword attached to the declaration.
  TokenSyntax getStructKeyword() const;

  /// Return a StructDeclSyntax with the given 'struct' keyword.
  StructDeclSyntax withStructKeyword(TokenSyntax NewStructKeyword) const;

  /// Return the identifier of the struct.
  TokenSyntax getIdentifier() const;

  /// Return a StructDeclSyntax with the given identifier.
  StructDeclSyntax withIdentifier(TokenSyntax NewIdentifier) const;

  /// Return the generic parameter clause of the struct declaration.
  GenericParameterClauseSyntax getGenericParameterClause() const;

  /// Return a StructDeclSyntax with the given generic parameter clause.
  StructDeclSyntax
  withGenericParameterClause(GenericParameterClauseSyntax NewGenericParams)
  const;

  /// Return the where clause of the struct declaration.
  GenericWhereClauseSyntax getGenericWhereClause() const;

  /// Return a StructDeclSyntax with the given generic where clause.
  StructDeclSyntax
  withWhereClause(GenericWhereClauseSyntax NewWhereClause) const;

  /// Return the left brace '{' token of the struct declaration.
  TokenSyntax getLeftBraceToken() const;

  /// Return a StructDeclSyntax with the given left brace '{' token.
  StructDeclSyntax withLeftBrace(TokenSyntax NewLeftBrace) const;

  /// Return the members' syntax of the struct.
  DeclMembersSyntax getMembers() const;

  /// Return a StructDeclSyntax with the given new members.
  StructDeclSyntax withMembers(DeclMembersSyntax NewMembers) const;

  /// Return the right brace '}' token of the struct declaration.
  TokenSyntax getRightBraceToken() const;

  /// Return a StructDeclSyntax with the given right brace '}' token.
  StructDeclSyntax withRightBrace(TokenSyntax NewRightBrace) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::StructDecl;
  }
};

#pragma mark - struct-declaration Builder

class StructDeclSyntaxBuilder final {
  RawSyntax::LayoutList StructLayout;
public:
  StructDeclSyntaxBuilder();
  StructDeclSyntaxBuilder &useStructKeyword(TokenSyntax StructKeyword);
  StructDeclSyntaxBuilder &useIdentifier(TokenSyntax Identifier);
  StructDeclSyntaxBuilder &useLeftBrace(TokenSyntax LeftBrace);

  StructDeclSyntaxBuilder &
  useGenericParameterClause(GenericParameterClauseSyntax GenericParams);

  StructDeclSyntaxBuilder &
  useGenericWhereClause(GenericWhereClauseSyntax GenericWhereClause);

  StructDeclSyntaxBuilder &useMembers(DeclMembersSyntax Members);
  StructDeclSyntaxBuilder &useRightBrace(TokenSyntax RightBrace);
  StructDeclSyntax build() const;
};

#pragma mark -

#pragma mark -
#pragma mark - type-alias API

class TypeAliasDeclSyntax final : public DeclSyntax {
  friend struct SyntaxFactory;
  friend class SyntaxData;
  friend class TypeAliasDeclSyntaxBuilder;

  enum Cursor : CursorIndex {
    TypeAliasKeyword,
    Identifier,
    GenericParameterClause,
    EqualToken,
    Type
  };

  virtual void validate() const override {}

public:
  static TypeAliasDeclSyntax makeBlank();
  TypeAliasDeclSyntax(RC<SyntaxData> Root, const SyntaxData *Data)
    : DeclSyntax(Root, Data) {}

  /// Return the 'typealias' keyword for the declaration.
  TokenSyntax getTypealiasKeyword() const {
    return { Root, Data->getChild(Cursor::TypeAliasKeyword).get() };
  }

  /// Return a TypeAliasDeclSyntax with the given 'typealias' keyword.
  TypeAliasDeclSyntax
  withTypeAliasKeyword(TokenSyntax NewTypeAliasKeyword) const;

  /// Return the identifier for the declaration.
  TokenSyntax getIdentifier() const;

  /// Return a TypeAliasDeclSyntax with the given identifier.
  TypeAliasDeclSyntax withIdentifier(TokenSyntax NewIdentifier) const;

  /// Return the generic parameter clause of the declaration.
  GenericParameterClauseSyntax getGenericParameterClause() const;

  /// Return a TypeAliasDeclSyntax with the given generic parameter clause.
  TypeAliasDeclSyntax
  withGenericParameterClause( GenericParameterClauseSyntax NewGenericParams)
  const;

  /// Return the equal '=' token from the declaration.
  TokenSyntax getEqualToken() const;

  /// Return a TypeAliasDeclSyntax with the given equal '=' token.
  TypeAliasDeclSyntax
  withEqualToken(TokenSyntax NewEqualToken) const;

  /// Return the type syntax to which the type alias is assigned.
  TypeSyntax getTypeSyntax() const;

  /// Return a TypeAliasDeclSyntax with the given type assignment.
  TypeAliasDeclSyntax withTypeSyntax(TypeSyntax NewType) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::TypeAliasDecl;
  }
};

#pragma mark - type-alias Builder

class TypeAliasDeclSyntaxBuilder final {
  RawSyntax::LayoutList TypeAliasLayout;
public:
  TypeAliasDeclSyntaxBuilder();

  TypeAliasDeclSyntaxBuilder &
  useTypeAliasKeyword(TokenSyntax TypeAliasKeyword);

  TypeAliasDeclSyntaxBuilder &useIdentifier(TokenSyntax Identifier);

  TypeAliasDeclSyntaxBuilder &
  useGenericParameterClause(GenericParameterClauseSyntax GenericParams);

  TypeAliasDeclSyntaxBuilder &useEqualToken(TokenSyntax EqualToken);

  TypeAliasDeclSyntaxBuilder &useType(TypeSyntax ReferentType);

  TypeAliasDeclSyntax build() const;
};

#pragma mark - function-parameter API

/// parameter ->
/// external-parameter-name? local-parameter-name ':'
///   type '...'? '='? expression? ','?
class FunctionParameterSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class Syntax;
  friend class SyntaxData;

  enum class Cursor : CursorIndex {
    ExternalName,
    LocalName,
    Colon,
    Type,
    Ellipsis,
    DefaultEqual,
    DefaultExpression,
    TrailingComma,
  };

  virtual void validate() const override;

public:
  static FunctionParameterSyntax makeBlank();

  FunctionParameterSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : Syntax(Root, Data) {}

  /// Get the external name of the parameter, if there is one.
  TokenSyntax getExternalName() const;

  /// Return a FunctionParameterSyntax with the given external name.
  FunctionParameterSyntax
  withExternalName(TokenSyntax NewExternalName) const;

  /// Return the local name of the parameter.
  TokenSyntax getLocalName() const;

  /// Return a FunctionParameterSyntax with the given local name.
  FunctionParameterSyntax
  withLocalName(TokenSyntax NewLocalName) const;

  /// Return the colon ':' token between the local name and type of the
  /// parameter.
  TokenSyntax getColonToken() const;

  /// Return a FunctionParameterSyntax with the given colon token between
  /// the local name and type.
  FunctionParameterSyntax
  withColonToken(TokenSyntax NewColonToken) const;

  /// Return the syntax for the type of this parameter.
  llvm::Optional<TypeSyntax> getTypeSyntax() const;

  /// Return a FunctionParameterSyntax with the given parameter type syntax.
  FunctionParameterSyntax
  withTypeSyntax(llvm::Optional<TypeSyntax> NewType) const;

  /// Return the equal '=' token in between the parameter type and the default
  /// value, if there is one.
  TokenSyntax getEqualToken() const;

  /// Return a FunctionParameterSyntax with the given equal '=' token in
  /// between the parameter type and the default value.
  FunctionParameterSyntax withEqualToken(TokenSyntax NewEqualToken) const;

  /// Return the expression for the default value of the parameter, if there
  /// is one.
  llvm::Optional<ExprSyntax> getDefaultValue() const;

  /// Return a FunctionParameterSyntax with the given default value. To remove
  /// the default value, pass llvm::None.
  FunctionParameterSyntax
  withDefaultValue(llvm::Optional<ExprSyntax> NewDefaultValue) const;

  /// Return the trailing comma on the parameter, if there is one.
  TokenSyntax getTrailingComma() const;

  /// Return a FunctionParameterSyntax with the given trailing comma.
  FunctionParameterSyntax
  withTrailingComma(TokenSyntax NewTrailingComma) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::FunctionParameter;
  }
};

#pragma mark - function-parameter-list API

/// parameter-list -> parameter | parameter ',' parameter-list
using FunctionParameterListSyntax =
  SyntaxCollection<SyntaxKind::FunctionParameterList, FunctionParameterSyntax>;

#pragma mark - function-signature API

/// function-signature ->
///   '(' parameter-list? ')' (throws | rethrows)? '->' attributes? type
class FunctionSignatureSyntax final : public Syntax {
  friend struct SyntaxBuilder;
  friend class Syntax;
  friend class SyntaxData;

  enum class Cursor : CursorIndex {
    LeftParen,
    ParameterList,
    RightParen,
    ThrowsOrRethrows,
    Arrow,
    ReturnTypeAttributes,
    ReturnType,
  };

  virtual void validate() const override;

public:
  static FunctionSignatureSyntax makeBlank();

  FunctionSignatureSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : Syntax(Root, Data) {}

  /// Return the left parenthesis '(' token enclosing the parameter list.
  TokenSyntax getLeftParenToken() const;

  /// Return a FunctionSignatureSyntax with the given left parenthesis '(' token
  /// enclosing the parameter list.
  FunctionSignatureSyntax
  withLeftParenToken(TokenSyntax NewLeftParen) const;

  /// Return the parameter list for this signature.
  FunctionParameterListSyntax getParameterList() const;

  /// Return the parameter list for this signature.
  FunctionSignatureSyntax
  withParameterList(FunctionParameterListSyntax NewParameterList) const;

  /// Return the right parenthesis ')' token enclosing the parameter list.
  TokenSyntax getRightParenToken() const;

  /// Return a FunctionSignatureSyntax with the given right parenthesis ')' token
  /// enclosing the parameter list.
  FunctionSignatureSyntax
  withRightParenToken(TokenSyntax NewRightParen) const;

  /// Return the 'throws' token in this signature if it exists.
  TokenSyntax getThrowsToken() const;

  /// Return a FunctionSignatureSyntax with the given 'throws' token.
  FunctionSignatureSyntax withThrowsToken(TokenSyntax NewThrowsToken) const;

  /// Return the 'rethrows' token in this signature if it exists;
  TokenSyntax getRethrowsToken() const;

  /// Return a FunctionSignatureSyntax with the given 'rethrows' token.
  FunctionSignatureSyntax
  withRethrowsToken(TokenSyntax NewRethrowsToken) const;

  /// Return the arrow '->' token for the signature.
  TokenSyntax getArrowToken() const;

  /// Return a FunctionSignatureSyntax with the given arrow token
  FunctionSignatureSyntax withArrowToken(TokenSyntax NewArrowToken) const;

  /// Return the return type attributes for the signature.
  TypeAttributesSyntax getReturnTypeAttributes() const;

  /// Return a FunctionSignatureSyntax with the given return type attributes.
  FunctionSignatureSyntax
  withReturnTypeAttributes(TypeAttributesSyntax NewReturnTypeAttributes) const;

  /// Return the syntax for the return type of the signature.
  TypeSyntax getReturnTypeSyntax() const;

  /// Return a FunctionSignatureSyntax with the given return type.
  FunctionSignatureSyntax withReturnTypeSyntax(TypeSyntax NewReturnType) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::FunctionSignature;
  }
};

#pragma mark - function-declaration API

class FunctionDeclSyntax final : public DeclSyntax {
  friend struct SyntaxFactory;
  friend class Syntax;
  friend class SyntaxData;

  enum class Cursor : CursorIndex {
    Attributes,
    Modifiers,
    FuncKeyword,
    Identifier,
    GenericParameterClause,
    Signature,
    GenericWhereClause,
    Body
  };

  virtual void validate() const override;

public:
  static FunctionDeclSyntax makeBlank();
  FunctionDeclSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : DeclSyntax(Root, Data) {}

  /// Get the attributes of this function declaration.
  TypeAttributesSyntax getAttributes() const;

  /// Return a FunctionDeclSyntax with the given attributes.
  FunctionDeclSyntax withAttributes(TypeAttributesSyntax NewAttributes) const;

  /// Get the modifiers of this function declaration.
  DeclModifierListSyntax getModifiers() const;

  /// Return a FunctionDeclSyntax with the given modifiers.
  FunctionDeclSyntax withModifiers(DeclModifierListSyntax NewModifiers) const;

  /// Return the 'func' keyword of this function declaration.
  TokenSyntax getFuncKeyword() const;

  /// Return a FunctionDeclSyntax with the given 'func' keyword.
  FunctionDeclSyntax withFuncKeyword(TokenSyntax NewFuncKeyword) const;

  /// Return the identifier of the function declaration.
  TokenSyntax getIdentifier() const;

  /// Return a FunctionDeclSyntax with the given identifier.
  FunctionDeclSyntax withIdentifier(TokenSyntax NewIdentifier) const;

  /// Return the generic parameter clause of the function declaration, if
  /// there is one. Otherwise, return llvm::None.
  llvm::Optional<GenericParameterClauseSyntax>
  getGenericParameterClause() const;

  /// Return a FunctionDeclSyntax with the given generic parameter clause.
  /// To remove the generic parameters, pass in llvm::None.
  FunctionDeclSyntax withGenericParameterClause(
    llvm::Optional<GenericParameterClauseSyntax> NewGenericParams) const;

  /// Return the signature of the function declaration.
  FunctionSignatureSyntax getSignature() const;

  /// Return a FunctionDeclSyntax with the given function signature.
  FunctionDeclSyntax withSignature(FunctionSignatureSyntax NewSignature) const;

  /// Return the body of the function declaration, if there is one.
  ///
  /// As an example, function declarations in protocols have no body.
  llvm::Optional<CodeBlockStmtSyntax> getBody() const;

  /// Return a FunctionDeclSyntax with the given body. To remove the body,
  /// pass in llvm::None.
  FunctionDeclSyntax
  withBody(llvm::Optional<CodeBlockStmtSyntax> NewBody) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::FunctionDecl;
  }
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_DECLSYNTAX_H
