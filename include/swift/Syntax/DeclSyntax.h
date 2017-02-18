//===--- DeclSyntax.cpp - Declaration Syntax Interface ----------*- C++ -*-===//
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

#include "swift/Syntax/GenericSyntax.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/SyntaxData.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/TypeSyntax.h"

#include "llvm/ADT/BitVector.h"

namespace swift {
namespace syntax {

class DeclSyntaxData : public SyntaxData {
protected:
  DeclSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
                 CursorIndex IndexInParent = 0);

public:
  static bool classof(const SyntaxData *S) { return S->isDecl(); }
};

class DeclSyntax : public Syntax {
  friend class Syntax;
  using DataType = DeclSyntaxData;
protected:
  DeclSyntax(const RC<SyntaxData> Root, const DeclSyntaxData *Data);

public:
  static bool classof(const SyntaxData *S) { return S->isDecl(); }
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
  static DeclMembersSyntax make(const RC<SyntaxData> Root,
                                const DeclMembersSyntaxData *Data);
  static DeclMembersSyntax makeBlank();

  static bool classof(const SyntaxData *S) {
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

  /// Return the where clayse of the struct declaration.
  GenericWhereClauseSyntax getGenericWhereClause() const;

  /// Return a StructDeclSyntax with the given generic where clause.
  StructDeclSyntax
  withWhereClause(GenericWhereClauseSyntax NewWhereClause) const;

  /// Return the left brace '{' token of the struct declartion.
  RC<TokenSyntax> getLeftBraceToken() const;

  /// Return a StructDeclSyntax with the given left brace '{' token.
  StructDeclSyntax withLeftBrace(RC<TokenSyntax> NewLeftBrace) const;

  /// Return the members' syntax of the struct.
  DeclMembersSyntax getMembers() const;

  /// Return a StructDeclSyntax with the given new members.
  StructDeclSyntax withMembers(DeclMembersSyntax NewMembers) const;

  /// Return the right brace '}' token of the struct declartion.
  RC<TokenSyntax> getRightBraceToken() const;

  /// Return a StructDeclSyntax with the given right brace '}' token.
  StructDeclSyntax withRightBrace(RC<TokenSyntax> NewRightBrace) const;

  static bool classof(const SyntaxData *Data) {
    return Data->getKind() == SyntaxKind::StructDecl;
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

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_DECLSYNTAX_H
