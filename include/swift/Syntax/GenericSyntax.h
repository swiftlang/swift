//===--- GenericSyntax.h - Swift Generic Syntax Interface -------*- C++ -*-===//
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
// This file defines the interface for generic syntax nodes,
// such as for generic parameter/argument-clauses and where-clauses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_GENERICSYNTAX_H
#define SWIFT_SYNTAX_GENERICSYNTAX_H

#include "swift/Syntax/References.h"
#include "swift/Syntax/SyntaxCollection.h"
#include "swift/Syntax/SyntaxData.h"
#include "swift/Syntax/TokenSyntax.h"

namespace swift {
namespace syntax {

class TypeSyntax;
class TypeIdentifierSyntax;

#pragma mark - generic-requirement API

class GenericRequirementSyntax : public Syntax {
  friend class Syntax;
  
protected:
  virtual void validate() const override {}
public:
  static GenericRequirementSyntax makeBlank();
  GenericRequirementSyntax(const RC<SyntaxData> Root,
                               const SyntaxData *Data)
    : Syntax(Root, Data) {}
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::ConformanceRequirement ||
           S->getKind() == SyntaxKind::SameTypeRequirement;
  }
};

#pragma mark - conformance-requirement API

/// conformance-requirement -> type-identifier : type-identifier
class ConformanceRequirementSyntax final : public GenericRequirementSyntax {

  friend class Syntax;

  enum Cursor : CursorIndex {
    LeftTypeIdentifier,
    Colon,
    RightTypeIdentifier,
  };

protected:
  virtual void validate() const override;
public:
  static ConformanceRequirementSyntax makeBlank();
  ConformanceRequirementSyntax(const RC<SyntaxData> Root,
                            const SyntaxData *Data)
    : GenericRequirementSyntax(Root, Data) {}

    /// Return the conforming "left-hand" type identifier in the
  /// conformance requirement.
  TypeIdentifierSyntax getConformingTypeIdentifier() const;

  /// Return a new ConformanceRequirementSyntax with the given conforming
  /// "left-hand" type identifier.
  ConformanceRequirementSyntax
  withConformingTypeIdentifier(TokenSyntax NewTypeIdentifier) const;

  /// Return the colon token in the conformance requirement.
  TokenSyntax getColonToken() const;

  /// Return a new ConformanceRequirementSyntax with the given colon token.
  ConformanceRequirementSyntax
  withColonToken(TokenSyntax NewColonToken);

  /// Return the "right-hand" inherited type from the conformance requirement.
  TypeIdentifierSyntax getInheritedType() const;

  /// Return a ConformanceRequirementSyntax with the given inherited type.
  ConformanceRequirementSyntax
  withInheritedType(TypeSyntax NewInheritedType);

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::ConformanceRequirement;
  }
  static bool classof(const GenericRequirementSyntax *S) {
    return S->getKind() == SyntaxKind::ConformanceRequirement;
  }
};

#pragma mark -same-type-requirement API

/// same-type-requirement -> type-identifier == type
class SameTypeRequirementSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class Syntax;
  enum Cursor : CursorIndex {
    LeftTypeIdentifier,
    EqualityToken,
    RightType,
  };

  virtual void validate() const override;

public:
  static SameTypeRequirementSyntax makeBlank();
  SameTypeRequirementSyntax(const RC<SyntaxData> Root,
                            const SyntaxData *Data)
    : Syntax(Root, Data) {}

    /// Return the type identifier on the left side of the same-type requirement.
  TypeIdentifierSyntax getLeftTypeIdentifier() const;

  /// Return a SameTypeRequirementSyntax with the given type identifier on
  /// the side.
  SameTypeRequirementSyntax
  withLeftTypeIdentifier(TypeIdentifierSyntax NewLeftTypeIdentifier) const;

  /// Return the equality '==' operator token from the same-type requirement.
  TokenSyntax getEqualityToken() const;

  SameTypeRequirementSyntax
  withEqualityToken(TokenSyntax NewEqualityToken) const;

  /// Return the type syntax from the right side of the same-type requirement.
  TypeSyntax getRightType() const;

  /// Return a SameTypeRequirementSyntax with the given type syntax on the
  /// right side,
  SameTypeRequirementSyntax withRightType(TypeSyntax NewRightType) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::SameTypeRequirement;
  }
};

#pragma mark - generic-parameter API

/// generic-parameter -> type-name
///                    | type-name : type-identifier
///                    | type-name : protocol-composition-type
class GenericParameterSyntax final : public Syntax {
  friend struct SyntaxFactory;

  enum class Cursor : CursorIndex {
    Identifier,
    ColonToken,
    InheritedType,
  };

protected:
  virtual void validate() const override;

public:
  static GenericParameterSyntax makeBlank();
  GenericParameterSyntax(const RC<SyntaxData> Root,
                         const SyntaxData *Data)
    : Syntax(Root, Data) {}

  /// Return the name of the generic parameter.
  TokenSyntax getIdentifier() const;

  /// Returns a GenericParameterSyntax with the given parameter identifier
  GenericParameterSyntax
  withIdentifier(TokenSyntax NewIdentifier) const;

  /// Return the colon token before the inherited type, if applicable.
  TokenSyntax getColonToken() const;

  /// Return a GenericParameterSyntax with the given colon token before the
  /// inherited type.
  GenericParameterSyntax
  withColonToken(TokenSyntax NewColonToken) const;

  /// Return the inherited type or protocol composition to which the
  /// parameter conforms, if applicable.
  TypeSyntax getInheritedTypeSyntax() const;

  /// Return a GenericParameterSyntax with the given inherited type syntax.
  ///
  /// This must either be a TypeIdentifierSyntax or
  /// a ProtocolCompositionTypeSyntax.
  GenericParameterSyntax
  withInheritedTypeSyntax(TypeSyntax NewInheritedType) const;
  
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::GenericParameter;
  }
};

#pragma mark - generic-parameter-clause API

/// generic-parameter-clause -> '<' generic-argument-list '>'
class GenericParameterClauseSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class GenericParameterClauseBuilder;
  friend class FunctionDeclSyntax;

  enum class Cursor : CursorIndex {
    LeftAngleBracketToken,
    GenericParameterList,
    RightAngleBracketToken,
  };

protected:
  virtual void validate() const override;

public:
  static GenericParameterClauseSyntax makeBlank();
  GenericParameterClauseSyntax(const RC<SyntaxData> Root,
                               const SyntaxData *Data)
    : Syntax(Root, Data) {}
  /// Return the left angle bracket '<' token on the generic parameter clause.
  TokenSyntax getLeftAngleBracket() const;

  /// Return a new GenericParameterClauseSyntax with the given left angle
  /// bracket '<' token.
  GenericParameterClauseSyntax
  withLeftAngleBracket(TokenSyntax NewLeftAngleBracketToken) const;

  /// Return the GenericParameterListSyntax inside the angle bracket tokens.
  GenericParameterListSyntax getGenericParameterList() const;

  /// Return a GenericParameterClauseSyntax with the given generic
  /// parameter list.
  GenericParameterClauseSyntax
  withGenericParams(GenericParameterListSyntax NewGenericParams) const;

  /// Return the right angle bracket '>' token on the generic parameter clause.
  TokenSyntax getRightAngleBracket() const;

  /// Return a GenericParameterClauseSyntax with the given right angle
  /// bracket '>' token.
  GenericParameterClauseSyntax
  withRightAngleBracket(TokenSyntax NewRightAngleBracketToken) const;
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::GenericParameterClause;
  }
};

#pragma mark - generic-parameter-clause Builder

class GenericParameterClauseBuilder {
  RC<RawTokenSyntax> LeftAngleToken;
  RawSyntax::LayoutList ParameterListLayout;
  RC<RawTokenSyntax> RightAngleToken;

public:
  GenericParameterClauseBuilder();

  GenericParameterClauseBuilder &
  useLeftAngleBracket(TokenSyntax LeftAngleBracket);

  GenericParameterClauseBuilder &
  addParameter(llvm::Optional<TokenSyntax> MaybeComma,
               GenericParameterSyntax Parameter);

  GenericParameterClauseBuilder &
  useRightAngleBracket(TokenSyntax RightAngleBracket);

  GenericParameterClauseSyntax build() const;
};

#pragma mark - generic-argument-clause API

/// generic-argument-clause -> '<' generic-argument-list '>'
class GenericArgumentClauseSyntax : public Syntax {
  friend struct SyntaxFactory;
  friend class GenericArgumentClauseBuilder;
  friend class SymbolicReferenceExprSyntax;
  friend class Syntax;

    enum class Cursor : CursorIndex {
    LeftAngleBracketToken,
    GenericArgumentList,
    RightAngleBracketToken,
  };

protected:
  virtual void validate() const override {}

public:
  static GenericArgumentClauseSyntax makeBlank();
  GenericArgumentClauseSyntax(const RC<SyntaxData> Root,
                              const SyntaxData *Data)
    : Syntax(Root, Data) {}

  /// Return the left angle bracket '<' token on the generic argument clause.
  TokenSyntax getLeftAngleBracket() const;

  /// Return a new GenericArgumentClauseSyntax with the given left angle
  /// bracket '<' token.
  GenericArgumentClauseSyntax
  withLeftAngleBracket(TokenSyntax NewLeftAngleBracket) const;

  /// Return the GenericArgumentClauseSyntax inside the angle bracket tokens.
  GenericArgumentListSyntax getGenericParameterList() const;

  /// Return a GenericArgumentClauseSyntax with the given generic
  /// parameter list.
  GenericArgumentClauseSyntax
  withGenericParams(GenericParameterListSyntax NewGenericParams) const;

  /// Return the right angle bracket '>' token on the generic argument clause.
  TokenSyntax getRightAngleBracket() const;

  /// Return a new GenericArgumentClauseSyntax with the given right angle
  /// bracket '>' token.
  GenericArgumentClauseSyntax
  withRightAngleBracket(TokenSyntax NewRightAngleBracket) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::GenericArgumentClause;
  }
};

#pragma mark - generic-argument-clause Builder

class GenericArgumentClauseBuilder {
  RC<RawTokenSyntax> LeftAngleToken;
  RawSyntax::LayoutList ArgumentListLayout;
  RC<RawTokenSyntax> RightAngleToken;
public:
  GenericArgumentClauseBuilder();

  GenericArgumentClauseBuilder &
  useLeftAngleBracket(TokenSyntax LeftAngleBracket);

  GenericArgumentClauseBuilder &
  addGenericArgument(llvm::Optional<TokenSyntax> MaybeComma,
                     TypeSyntax ArgumentTypeSyntax);

  GenericArgumentClauseBuilder &
  useRightAngleBracket(TokenSyntax RightAngleBracket);

  GenericArgumentClauseSyntax build() const;
};

#pragma mark - generic-where-clause API

/// generic-where-clause -> 'where' requirement-list
class GenericWhereClauseSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class Syntax;
  enum class Cursor : CursorIndex {
    WhereKeyword,
    RequirementList,
  };

protected:
  virtual void validate() const override;

public:
  static GenericWhereClauseSyntax makeBlank();
  GenericWhereClauseSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : Syntax(Root, Data) {}

  /// Return the 'where' keyword in the generic where clause.
  TokenSyntax getWhereKeyword() const;

  /// Return a GenericWhereClauseSyntax with the given 'where' keyword.
  GenericWhereClauseSyntax
  withWhereKeyword(TokenSyntax NewWhereKeyword) const;

  /// Return the requirement list from the where clause.
  GenericRequirementListSyntax getRequirementList() const;

  /// Return a GenericWhereClauseSyntax with the given requirement list.
  GenericWhereClauseSyntax
  withRequirementList(GenericRequirementListSyntax NewRequirements) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::GenericWhereClause;
  }
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_GENERICSYNTAX_H
