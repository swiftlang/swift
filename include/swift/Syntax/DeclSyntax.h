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
class ExprSyntaxData;
class CodeBlockStmtSyntax;
class CodeBlockStmtSyntaxData;
class TypeAttributesSyntax;
class TypeAttributesSyntaxData;
class DeclModifierListSyntax;
class GenericWhereClauseSyntax;
class GenericWhereClauseSyntaxData;
class GenericParameterListSyntax;
class GenericParameterListSyntaxData;

#pragma mark declaration-modifier Data

class DeclModifierSyntaxData final : public SyntaxData {
  friend struct SyntaxFactory;
  friend class SyntaxData;
  friend class Syntax;
  friend class DeclModifierSyntax;

  DeclModifierSyntaxData(const RC<RawSyntax> Raw,
                         const SyntaxData *Parent = nullptr,
                         const CursorIndex IndexInParent = 0);

  static RC<DeclModifierSyntaxData> make(const RC<RawSyntax> Raw,
                                         const SyntaxData *Parent = nullptr,
                                         const CursorIndex IndexInParent = 0);

  static RC<DeclModifierSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *SD) {
    return SD->getKind() == SyntaxKind::DeclModifier;
  }
};

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
  friend class DeclModifierSyntaxData;

  enum class Cursor : CursorIndex {
    Name,
    LeftParen,
    Argument,
    RightParen
  };

  DeclModifierSyntax(const RC<SyntaxData> Root, const DataType *Data)
    : Syntax(Root, Data) {}

public:
  using DataType = DeclModifierSyntaxData;

  /// Return the name of the modifier.
  RC<TokenSyntax> getName() const;

  /// Return a DeclModifierSyntax with the given name.
  DeclModifierSyntax withName(RC<TokenSyntax> NewName) const;

  /// Return the left parenthesis '(' token as a part of the argument clause,
  /// if there is one.
  RC<TokenSyntax> getLeftParenToken() const;

  /// Return a DeclModifierSyntax with the given left parenthesis '(' token.
  DeclModifierSyntax withLeftParenToken(RC<TokenSyntax> NewLeftParen) const;

  /// Get the argument to the declaration modifier.
  ///
  /// This is either:
  /// - 'set' for the access modifiers such as 'private' or 'public', or
  /// - 'safe' / 'unsafe' for the 'unowned' modifier.
  RC<TokenSyntax> getArgument() const;

  /// Return a DeclModifierSyntax with the given argument.
  DeclModifierSyntax withArgument(RC<TokenSyntax> NewArgument) const;

  /// Return the right parenthesis ')' token as a part of the argument clause,
  /// if there is one.
  RC<TokenSyntax> getRightParenToken() const;

  /// Return a DeclModifierSyntax with the given right parenthesis ')' token.
  DeclModifierSyntax withRightParenToken(RC<TokenSyntax> NewRightParen) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::DeclModifier;
  }
};

#pragma mark declaration-modifiers Data

using DeclModifierListSyntaxData =
  SyntaxCollectionData<SyntaxKind::DeclModifierList, DeclModifierSyntax>;

#pragma mark declaration-modifiers API

class DeclModifierListSyntax final :
  public SyntaxCollection<SyntaxKind::DeclModifierList, DeclModifierSyntax> {

  friend struct SyntaxFactory;
  friend class Syntax;
  friend class SyntaxData;
  friend class FunctionDeclSyntax;

  using DataType = DeclModifierListSyntaxData;

  DeclModifierListSyntax(const RC<SyntaxData> Root, const DataType *Data)
    : SyntaxCollection(Root, Data) {}

public:
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::DeclModifierList;
  }
};

#pragma mark declaration Data

class DeclSyntaxData : public SyntaxData {
protected:
  DeclSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
                 CursorIndex IndexInParent = 0);

public:
  static bool classof(const SyntaxData *S) { return S->isDecl(); }
};

#pragma mark declaration API

class DeclSyntax : public Syntax {
  friend class Syntax;
  using DataType = DeclSyntaxData;
protected:
  DeclSyntax(const RC<SyntaxData> Root, const DeclSyntaxData *Data);

public:
  static bool classof(const SyntaxData *S) { return S->isDecl(); }
};

#pragma mark - unknown-declaration Data

class UnknownDeclSyntaxData : public UnknownSyntaxData {
  UnknownDeclSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
                        CursorIndex IndexInParent = 0);
public:
  static RC<UnknownDeclSyntaxData> make(RC<RawSyntax> Raw,
                                        const SyntaxData *Parent = nullptr,
                                        CursorIndex IndexInParent = 0);

  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::UnknownDecl;
  }
};

#pragma mark - unknown-declaration API

class UnknownDeclSyntax : public UnknownSyntax {
  friend class SyntaxData;
  friend class UnknownStmtSyntaxData;
  friend class LegacyASTTransformer;

  using DataType = UnknownDeclSyntaxData;

  UnknownDeclSyntax(const RC<SyntaxData> Root,
                    const UnknownDeclSyntaxData *Data);

public:
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::UnknownDecl;
  }
};

#pragma mark declaration-members Data

class DeclMembersSyntaxData final : public SyntaxData {
  friend class SyntaxData;
  friend class DeclMembersSyntaxBuilder;
  friend struct SyntaxFactory;

  DeclMembersSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
                        CursorIndex IndexInParent = 0);

public:
  static RC<DeclMembersSyntaxData> make(RC<RawSyntax> Raw,
                                        const SyntaxData *Parent = nullptr,
                                        CursorIndex IndexInParent = 0);
  static RC<DeclMembersSyntaxData> makeBlank();

  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::DeclMembers;
  }
};

#pragma mark -
#pragma mark declaration-members API

class DeclMembersSyntax final : public Syntax {
  using DataType = DeclMembersSyntaxData;
  friend struct SyntaxFactory;
  friend class SyntaxData;
  friend class Syntax;
  friend class DeclMembersSyntaxBuilder;
  friend class StructDeclSyntax;

  DeclMembersSyntax(RC<SyntaxData> Root,
                    const DeclMembersSyntaxData *Data);
public:
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
#pragma mark struct-declaration Data

class StructDeclSyntaxData final : public DeclSyntaxData {
  friend class SyntaxData;
  friend class StructDeclSyntax;
  friend class StructDeclSyntaxBuilder;
  friend struct SyntaxFactory;

  RC<GenericWhereClauseSyntaxData> CachedWhereClause;
  RC<GenericParameterClauseSyntaxData> CachedGenericParams;
  RC<DeclMembersSyntaxData> CachedMembers;

  StructDeclSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
                       CursorIndex IndexInParent = 0);

  static RC<StructDeclSyntaxData> make(RC<RawSyntax> Raw,
                                       const SyntaxData *Parent = nullptr,
                                       CursorIndex IndexInParent = 0);
  static RC<StructDeclSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::StructDecl;
  }
};

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
  using DataType = StructDeclSyntaxData;
  friend struct SyntaxFactory;
  friend class Syntax;
  friend class SyntaxData;
  friend class StructDeclSyntaxData;
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

  StructDeclSyntax(const RC<SyntaxData> Root, const StructDeclSyntaxData *Data);

  const StructDeclSyntaxData *getData() const {
    return cast<StructDeclSyntaxData>(Data);
  }

public:
  /// Return the 'struct' keyword attached to the declaration.
  RC<TokenSyntax> getStructKeyword() const;

  /// Return a StructDeclSyntax with the given 'struct' keyword.
  StructDeclSyntax withStructKeyword(RC<TokenSyntax> NewStructKeyword) const;

  /// Return the identifier of the struct.
  RC<TokenSyntax> getIdentifier() const;

  /// Return a StructDeclSyntax with the given identifier.
  StructDeclSyntax withIdentifier(RC<TokenSyntax> NewIdentifier) const;

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
  RC<TokenSyntax> getLeftBraceToken() const;

  /// Return a StructDeclSyntax with the given left brace '{' token.
  StructDeclSyntax withLeftBrace(RC<TokenSyntax> NewLeftBrace) const;

  /// Return the members' syntax of the struct.
  DeclMembersSyntax getMembers() const;

  /// Return a StructDeclSyntax with the given new members.
  StructDeclSyntax withMembers(DeclMembersSyntax NewMembers) const;

  /// Return the right brace '}' token of the struct declaration.
  RC<TokenSyntax> getRightBraceToken() const;

  /// Return a StructDeclSyntax with the given right brace '}' token.
  StructDeclSyntax withRightBrace(RC<TokenSyntax> NewRightBrace) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::StructDecl;
  }
};

#pragma mark - struct-declaration Builder

class StructDeclSyntaxBuilder final {
  RawSyntax::LayoutList StructLayout;
public:
  StructDeclSyntaxBuilder();
  StructDeclSyntaxBuilder &useStructKeyword(RC<TokenSyntax> StructKeyword);
  StructDeclSyntaxBuilder &useIdentifier(RC<TokenSyntax> Identifier);
  StructDeclSyntaxBuilder &useLeftBrace(RC<TokenSyntax> LeftBrace);

  StructDeclSyntaxBuilder &
  useGenericParameterClause(GenericParameterClauseSyntax GenericParams);

  StructDeclSyntaxBuilder &
  useGenericWhereClause(GenericWhereClauseSyntax GenericWhereClause);

  StructDeclSyntaxBuilder &useMembers(DeclMembersSyntax Members);
  StructDeclSyntaxBuilder &useRightBrace(RC<TokenSyntax> RightBrace);
  StructDeclSyntax build() const;
};

#pragma mark -
#pragma mark - type-alias Data

class TypeAliasDeclSyntaxData final : public DeclSyntaxData {
  friend class SyntaxData;
  friend struct SyntaxFactory;
  friend class TypeAliasDeclSyntaxBuilder;

  TypeAliasDeclSyntaxData(RC<RawSyntax> Raw,
                          const SyntaxData *Parent = nullptr,
                          CursorIndex IndexInParent = 0);
  static RC<TypeAliasDeclSyntaxData> make(RC<RawSyntax> Raw,
                                          const SyntaxData *Parent = nullptr,
                                          CursorIndex IndexInParent = 0);
  static RC<TypeAliasDeclSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::TypeAliasDecl;
  }
};

#pragma mark -
#pragma mark - type-alias API

class TypeAliasDeclSyntax final : public DeclSyntax {
  friend struct SyntaxFactory;
  friend class SyntaxData;
  friend class TypeAliasDeclSyntaxData;
  friend class TypeAliasDeclSyntaxBuilder;

  using DataType = TypeAliasDeclSyntaxData;

  enum Cursor : CursorIndex {
    TypeAliasKeyword,
    Identifier,
    GenericParameterClause,
    EqualToken,
    Type
  };

  TypeAliasDeclSyntax(RC<SyntaxData> Root, const TypeAliasDeclSyntaxData *Data);

public:
  /// Return the 'typealias' keyword for the declaration.
  RC<TokenSyntax> getTypealiasKeyword() const {
    return cast<TokenSyntax>(getRaw()->getChild(Cursor::TypeAliasKeyword));
  }

  /// Return a TypeAliasDeclSyntax with the given 'typealias' keyword.
  TypeAliasDeclSyntax
  withTypeAliasKeyword(RC<TokenSyntax> NewTypeAliasKeyword) const;

  /// Return the identifier for the declaration.
  RC<TokenSyntax> getIdentifier() const;

  /// Return a TypeAliasDeclSyntax with the given identifier.
  TypeAliasDeclSyntax withIdentifier(RC<TokenSyntax> NewIdentifier) const;

  /// Return the generic parameter clause of the declaration.
  GenericParameterClauseSyntax getGenericParameterClause() const;

  /// Return a TypeAliasDeclSyntax with the given generic parameter clause.
  TypeAliasDeclSyntax
  withGenericParameterClause( GenericParameterClauseSyntax NewGenericParams)
  const;

  /// Return the equal '=' token from the declaration.
  RC<TokenSyntax> getEqualToken() const;

  /// Return a TypeAliasDeclSyntax with the given equal '=' token.
  TypeAliasDeclSyntax
  withEqualToken(RC<TokenSyntax> NewEqualToken) const;

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
  useTypeAliasKeyword(RC<TokenSyntax> TypeAliasKeyword);

  TypeAliasDeclSyntaxBuilder &useIdentifier(RC<TokenSyntax> Identifier);

  TypeAliasDeclSyntaxBuilder &
  useGenericParameterClause(GenericParameterClauseSyntax GenericParams);

  TypeAliasDeclSyntaxBuilder &useEqualToken(RC<TokenSyntax> EqualToken);

  TypeAliasDeclSyntaxBuilder &useType(TypeSyntax ReferentType);

  TypeAliasDeclSyntax build() const;
};

#pragma mark - function-parameter Data

class FunctionParameterSyntaxData final : public SyntaxData {

  friend struct SyntaxFactory;
  friend class Syntax;
  friend class SyntaxData;
  friend class FunctionParameterSyntax;

  RC<TypeSyntaxData> CachedTypeSyntax;
  RC<ExprSyntaxData> CachedDefaultValue;

  FunctionParameterSyntaxData(RC<RawSyntax> Raw,
                              const SyntaxData *Parent = nullptr,
                              CursorIndex IndexInParent = 0);
  static RC<FunctionParameterSyntaxData>
  make(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);
  static RC<FunctionParameterSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *SD) {
    return SD->getKind() == SyntaxKind::FunctionParameter;
  }
};

#pragma mark - function-parameter API

/// parameter ->
/// external-parameter-name? local-parameter-name ':'
///   type '...'? '='? expression? ','?
class FunctionParameterSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class Syntax;
  friend class SyntaxData;
  friend class FunctionParameterSyntaxData;

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

public:
  using DataType = FunctionParameterSyntaxData;

  FunctionParameterSyntax(const RC<SyntaxData> Root, const DataType *Data)
    : Syntax(Root, Data) {}

  /// Get the external name of the parameter, if there is one.
  RC<TokenSyntax> getExternalName() const;

  /// Return a FunctionParameterSyntax with the given external name.
  FunctionParameterSyntax
  withExternalName(RC<TokenSyntax> NewExternalName) const;

  /// Return the local name of the parameter.
  RC<TokenSyntax> getLocalName() const;

  /// Return a FunctionParameterSyntax with the given local name.
  FunctionParameterSyntax
  withLocalName(RC<TokenSyntax> NewLocalName) const;

  /// Return the colon ':' token between the local name and type of the
  /// parameter.
  RC<TokenSyntax> getColonToken() const;

  /// Return a FunctionParameterSyntax with the given colon token between
  /// the local name and type.
  FunctionParameterSyntax
  withColonToken(RC<TokenSyntax> NewColonToken) const;

  /// Return the syntax for the type of this parameter.
  llvm::Optional<TypeSyntax> getTypeSyntax() const;

  /// Return a FunctionParameterSyntax with the given parameter type syntax.
  FunctionParameterSyntax
  withTypeSyntax(llvm::Optional<TypeSyntax> NewType) const;

  /// Return the equal '=' token in between the parameter type and the default
  /// value, if there is one.
  RC<TokenSyntax> getEqualToken() const;

  /// Return a FunctionParameterSyntax with the given equal '=' token in
  /// between the parameter type and the default value.
  FunctionParameterSyntax withEqualToken(RC<TokenSyntax> NewEqualToken) const;

  /// Return the expresion for the default value of the parameter, if there
  /// is one.
  llvm::Optional<ExprSyntax> getDefaultValue() const;

  /// Return a FunctionParameterSyntax with the given default value. To remove
  /// the default value, pass llvm::None.
  FunctionParameterSyntax
  withDefaultValue(llvm::Optional<ExprSyntax> NewDefaultValue) const;

  /// Return the trailing comma on the parameter, if there is one.
  RC<TokenSyntax> getTrailingComma() const;

  /// Return a FunctionParameterSyntax with the given trailing comma.
  FunctionParameterSyntax
  withTrailingComma(RC<TokenSyntax> NewTrailingComma) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::FunctionParameter;
  }
};

#pragma mark - function-parameter-list Data

using FunctionParameterListSyntaxData =
  SyntaxCollectionData<SyntaxKind::FunctionParameterList,
  FunctionParameterSyntax>;

#pragma mark - function-parameter-list API

/// parameter-list -> parameteter | parameter ',' parameter-list
class FunctionParameterListSyntax final : public
  SyntaxCollection<SyntaxKind::FunctionParameterList, FunctionParameterSyntax> {
  friend struct SyntaxFactory;
  friend class Syntax;
  friend class SyntaxData;
  friend class FunctionSignatureSyntax;

  using DataType = FunctionParameterListSyntaxData;

  FunctionParameterListSyntax(const RC<SyntaxData> Root,
                              const DataType *Data)
    : SyntaxCollection(Root, Data) {}

public:
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::FunctionParameterList;
  }
};

#pragma mark - function-signature Data

class FunctionSignatureSyntaxData final : public SyntaxData {
  friend struct SyntaxFactory;
  friend class SyntaxData;
  friend class FunctionSignatureSyntax;

  RC<FunctionParameterListSyntaxData> CachedParameterList;
  RC<TypeAttributesSyntaxData> CachedReturnTypeAttributes;
  RC<TypeSyntaxData> CachedReturnTypeSyntax;

  FunctionSignatureSyntaxData(const RC<RawSyntax> Raw,
                              const SyntaxData *Parent = nullptr,
                              const CursorIndex IndexInParent = 0);

  static RC<FunctionSignatureSyntaxData>
  make(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);
  static RC<FunctionSignatureSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *SD) {
    return SD->getKind() == SyntaxKind::FunctionSignature;
  }
};

#pragma mark - function-signature API

/// function-signature ->
///   '(' parameter-list? ')' (throws | rethrows)? '->' attributes? type
class FunctionSignatureSyntax final : public Syntax {
  friend struct SyntaxBuilder;
  friend class Syntax;
  friend class SyntaxData;
  friend class FunctionSignatureSyntaxData;

  enum class Cursor : CursorIndex {
    LeftParen,
    ParameterList,
    RightParen,
    ThrowsOrRethrows,
    Arrow,
    ReturnTypeAttributes,
    ReturnType,
  };

public:
  using DataType = FunctionSignatureSyntaxData;

  FunctionSignatureSyntax(const RC<SyntaxData> Root, const DataType *Data)
    : Syntax(Root, Data) {}

  /// Return the left parenthesis '(' token enclosing the parameter list.
  RC<TokenSyntax> getLeftParenToken() const;

  /// Return a FunctionSignatureSyntax with the given left parentesis '(' token
  /// enclosing the parameter list.
  FunctionSignatureSyntax
  withLeftParenToken(RC<TokenSyntax> NewLeftParen) const;

  /// Return the parameter list for this signature.
  FunctionParameterListSyntax getParameterList() const;

  /// Return the parameter list for this signature.
  FunctionSignatureSyntax
  withParameterList(FunctionParameterListSyntax NewParameterList) const;

  /// Return the right parenthesis ')' token enclosing the parameter list.
  RC<TokenSyntax> getRightParenToken() const;

  /// Return a FunctionSignatureSyntax with the given right parentesis ')' token
  /// enclosing the parameter list.
  FunctionSignatureSyntax
  withRightParenToken(RC<TokenSyntax> NewRightParen) const;

  /// Return the 'throws' token in this signature if it exists.
  RC<TokenSyntax> getThrowsToken() const;

  /// Return a FunctionSignatureSyntax with the given 'throws' token.
  FunctionSignatureSyntax withThrowsToken(RC<TokenSyntax> NewThrowsToken) const;

  /// Return the 'rethrows' token in this signature if it exists;
  RC<TokenSyntax> getRethrowsToken() const;

  /// Return a FunctionSignatureSyntax with the given 'rethrows' token.
  FunctionSignatureSyntax
  withRethrowsToken(RC<TokenSyntax> NewRethrowsToken) const;

  /// Return the arrow '->' token for the signature.
  RC<TokenSyntax> getArrowToken() const;

  /// Return a FunctionSignatureSyntax with the given arrow token
  FunctionSignatureSyntax withArrowToken(RC<TokenSyntax> NewArrowToken) const;

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

#pragma mark - function-declaration Data

class FunctionDeclSyntaxData final : public SyntaxData {
  friend struct SyntaxFactory;
  friend class SyntaxData;
  friend class FunctionDeclSyntax;

  RC<TypeAttributesSyntaxData> CachedAttributes;
  RC<DeclModifierListSyntaxData> CachedModifiers;
  RC<GenericParameterClauseSyntaxData> CachedGenericParams;
  RC<FunctionSignatureSyntaxData> CachedSignature;
  RC<GenericWhereClauseSyntaxData> CachedGenericWhereClause;
  RC<CodeBlockStmtSyntaxData> CachedBody;

  FunctionDeclSyntaxData(const RC<RawSyntax> Raw,
                         const SyntaxData *Parent = nullptr,
                         const CursorIndex IndexInParent = 0);

  static RC<FunctionDeclSyntaxData> make(const RC<RawSyntax> Raw,
                                         const SyntaxData *Parent = nullptr,
                                         const CursorIndex IndexInParent = 0);
  static RC<FunctionDeclSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *SD) {
    return SD->getKind() == SyntaxKind::FunctionDecl;
  }
};

#pragma mark - function-declaration API

class FunctionDeclSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class Syntax;
  friend class SyntaxData;
  friend class FunctionDeclSyntaxData;

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

  using DataType = FunctionDeclSyntaxData;

  FunctionDeclSyntax(const RC<SyntaxData> Root, const DataType *Data)
    : Syntax(Root, Data) {}

public:
  /// Get the attributes of this function declaration.
  TypeAttributesSyntax getAttributes() const;

  /// Return a FunctionDeclSyntax with the given attributes.
  FunctionDeclSyntax withAttributes(TypeAttributesSyntax NewAttributes) const;

  /// Get the modifiers of this function declaration.
  DeclModifierListSyntax getModifiers() const;

  /// Return a FunctionDeclSyntax with the given modifiers.
  FunctionDeclSyntax withModifiers(DeclModifierListSyntax NewModifiers) const;

  /// Return the 'func' keyword of tis function declaration.
  RC<TokenSyntax> getFuncKeyword() const;

  /// Return a FunctionDeclSyntax with the given 'func' keyword.
  FunctionDeclSyntax withFuncKeyword(RC<TokenSyntax> NewFuncKeyword) const;

  /// Return the identifier of the function declaration.
  RC<TokenSyntax> getIdentifier() const;

  /// Return a FunctionDeclSyntax with the given identifier.
  FunctionDeclSyntax withIdentifier(RC<TokenSyntax> NewIdentifier) const;

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
