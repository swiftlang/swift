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
#include "swift/Syntax/SyntaxData.h"
#include "swift/Syntax/TokenSyntax.h"

namespace swift {
namespace syntax {

class TypeSyntax;
class TypeSyntaxData;
class TypeIdentifierSyntax;
class TypeIdentifierSyntaxData;

#pragma mark - conformance-requirement Data

class ConformanceRequirementSyntaxData final : public SyntaxData {
  friend class SyntaxData;
  RC<TypeIdentifierSyntaxData> CachedConformingTypeIdentifier;
  RC<TypeSyntaxData> InheritedType;

  ConformanceRequirementSyntaxData(RC<RawSyntax> Raw,
                                   const SyntaxData *Parent = nullptr,
                                   CursorIndex IndexInParent = 0);
  static RC<ConformanceRequirementSyntaxData>
  make(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);
  static RC<ConformanceRequirementSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::ConformanceRequirement;
  }
};

#pragma mark - conformance-requirement API

/// conformance-requirement -> type-identifier : type-identifier
class ConformanceRequirementSyntax final : public Syntax {

  friend class ConformanceRequirementSyntaxData;

  enum Cursor : CursorIndex {
    LeftTypeIdentifier,
    Colon,
    RightTypeIdentifier,
  };

  ConformanceRequirementSyntax(RC<SyntaxData> Root,
                               ConformanceRequirementSyntaxData *Data);

  static ConformanceRequirementSyntax make(RC<RawSyntax> Raw,
                                           const SyntaxData *Parent = nullptr,
                                           CursorIndex IndexInParent = 0);

  static ConformanceRequirementSyntax makeBlank();

public:
  /// Return the conforming "left-hand" type identifier in the
  /// conformance requirement.
  TypeIdentifierSyntax getConformingTypeIdentifier() const;

  /// Return a new ConformanceRequirementSyntax with the given conforming
  /// "left-hand" type identifier.
  ConformanceRequirementSyntax
  withConformingTypeIdentifier(RC<TokenSyntax> NewTypeIdentifier) const;

  /// Return the colon token in the conformance requirement.
  RC<TokenSyntax> getColonToken() const;

  /// Return a new ConformanceRequirementSyntax with the given colon token.
  ConformanceRequirementSyntax
  withColonToken(RC<TokenSyntax> NewColonToken);

  /// Return the "right-hand" inherited type from the conformance requirement.
  TypeIdentifierSyntax getInheritedType() const;

  /// Return a ConformanceRequirementSyntax with the given inherited type.
  ConformanceRequirementSyntax
  withInheritedType(TypeSyntax NewInheritedType);

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::ConformanceRequirement;
  }
};

#pragma mark - same-type-requirement Data

class SameTypeRequirementSyntaxData final : public SyntaxData {
  friend struct SyntaxFactory;
  friend class SyntaxData;
  RC<TypeIdentifierSyntaxData> CachedLeftTypeIdentifier;
  RC<TypeSyntaxData> CachedRightType;

  SameTypeRequirementSyntaxData(RC<RawSyntax> Raw,
                                const SyntaxData *Parent = nullptr,
                                CursorIndex IndexInParent = 0);
  static RC<SameTypeRequirementSyntaxData>
  make(RC<RawSyntax> Raw,
       const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);
  static RC<SameTypeRequirementSyntaxData> makeBlank();

public:
static bool classof(const SyntaxData *S) {
  return S->getKind() == SyntaxKind::SameTypeRequirement;
}
};

#pragma mark -same-type-requirement API

/// same-type-requirement -> type-identifier == type
class SameTypeRequirementSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class SameTypeRequirementSyntaxData;

  enum Cursor : CursorIndex {
    LeftTypeIdentifier,
    EqualityToken,
    RightType,
  };

  SameTypeRequirementSyntax(RC<SyntaxData> Root,
                            const SameTypeRequirementSyntaxData *Data);

public:

  /// Return the type identifier on the left side of the same-type requirement.
  TypeIdentifierSyntax getLeftTypeIdentifier() const;

  /// Return a SameTypeRequirementSyntax with the given type identifier on
  /// the side.
  SameTypeRequirementSyntax
  withLeftTypeIdentifier(TypeIdentifierSyntax NewLeftTypeIdentifier) const;

  /// Return the equality '==' operator token from the same-type requirement.
  RC<TokenSyntax> getEqualityToken() const;

  SameTypeRequirementSyntax
  withEqualityToken(RC<TokenSyntax> NewEqualityToken) const;

  /// Return the type syntax from the right side of the same-type requirement.
  TypeSyntax getRightType() const;

  /// Return a SameTypeRequirementSyntax with the given type syntax on the
  /// right side,
  SameTypeRequirementSyntax withRightType(TypeSyntax NewRightType) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::SameTypeRequirement;
  }
};

#pragma mark generic-parameter Data

  class GenericParameterSyntaxData final : public SyntaxData {
    friend class SyntaxData;
    friend struct SyntaxFactory;
    RC<TypeSyntaxData> CachedInheritedType;

    GenericParameterSyntaxData(RC<RawSyntax> Raw,
                               const SyntaxData *Parent = nullptr,
                               CursorIndex IndexInParent = 0);

    static RC<GenericParameterSyntaxData>
    make(RC<RawSyntax> Raw,
         const SyntaxData *Parent = nullptr,
         CursorIndex IndexInParent = 0);
    static RC<GenericParameterSyntaxData> makeBlank();

    static bool classof(const SyntaxData *S) {
      return S->getKind() == SyntaxKind::GenericParameterClause;
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

  GenericParameterSyntax(RC<SyntaxData> Root,
                         const GenericParameterSyntaxData *Data);

public:

  /// Return the name of the generic parameter.
  RC<TokenSyntax> getIdentifier() const;

  /// Returns a GenericParameterSyntax with the given parameter identifier
  GenericParameterSyntax
  withIdentifier(RC<TokenSyntax> NewIdentifier) const;

  /// Return the colon token before the inherited type, if applicable.
  RC<TokenSyntax> getColonToken() const;

  /// Return a GenericParameterSyntax with the given colon token before the
  /// inherited type.
  GenericParameterSyntax
  withColonToken(RC<TokenSyntax> NewColonToken) const;

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


#pragma mark - generic-parameter-list Data

class GenericParameterListSyntaxData final : public SyntaxData {
  friend class SyntaxData;

  GenericParameterListSyntaxData(RC<RawSyntax> Raw,
                                 const SyntaxData *Parent = nullptr,
                                 CursorIndex IndexInParent = 0);
  static RC<GenericParameterListSyntaxData>
  make(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);
  static RC<GenericParameterListSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::GenericParameterList;
  }
};

#pragma mark - generic-parameter-list API

/// generic-parameter-list -> generic-parameter
///                         | generic-parameter ',' generic-parameter-list
class GenericParameterListSyntax final : public Syntax {
  friend struct SyntaxFactory;
};

#pragma mark generic-parameter-clause Data

class GenericParameterClauseSyntaxData final : public SyntaxData {
  friend class SyntaxData;
  friend struct SyntaxFactory;
  friend class GenericParameterClauseBuilder;

  RC<GenericParameterListSyntaxData> CachedGenericParameterList;

  GenericParameterClauseSyntaxData(RC<RawSyntax> Raw,
                                   const SyntaxData *Parent = nullptr,
                                   CursorIndex IndexInParent = 0);
  static RC<GenericParameterClauseSyntaxData>
  make(RC<RawSyntax> Raw,
       const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);
  static RC<GenericParameterClauseSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::GenericParameterClause;
  }
};

#pragma mark - generic-parameter-clause API

/// generic-parameter-clause -> '<' generic-argument-list '>'
class GenericParameterClauseSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class GenericParameterClauseSyntaxData;
  friend class GenericParameterClauseBuilder;
  enum class Cursor : CursorIndex {
    LeftAngleBracketToken,
    GenericParameterList,
    RightAngleBracketToken,
  };

  GenericParameterClauseSyntax(RC<SyntaxData> Root,
                               const GenericParameterClauseSyntaxData *Data);

public:
  /// Return the left angle bracket '<' token on the generic parameter clause.
  RC<TokenSyntax> getLeftAngleBracket() const;

  /// Return a new GenericParameterClauseSyntax with the given left angle
  /// bracket '<' token.
  GenericParameterClauseSyntax
  withLeftAngleBracket(RC<TokenSyntax> NewLeftAngleBracketToken) const;

  /// Return the GenericParameterListSyntax inside the angle bracket tokens.
  GenericParameterListSyntax getGenericParameterList() const;

  /// Return a GenericParameterClauseSyntax with the given generic
  /// parameter list.
  GenericParameterClauseSyntax
  withGenericParams(GenericParameterListSyntax NewGenericParams) const;

  /// Return the right angle bracket '>' token on the generic parameter clause.
  RC<TokenSyntax> getRightAngleBracket() const;

  /// Return a GenericParameterClauseSyntax with the given right angle
  /// bracket '>' token.
  GenericParameterClauseSyntax
  withRightAngleBracket(RC<TokenSyntax> NewRightAngleBracketToken) const;
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::GenericParameterClause;
  }
};

#pragma mark - generic-parameter-clause Builder

class GenericParameterClauseBuilder {
  RC<TokenSyntax> LeftAngleToken;
  RawSyntax::LayoutList ParameterListLayout;
  RC<TokenSyntax> RightAngleToken;

public:
  GenericParameterClauseBuilder();

  GenericParameterClauseBuilder &
  useLeftAngleBracket(RC<TokenSyntax> LeftAngleBracket);

  GenericParameterClauseBuilder &
  addParameter(llvm::Optional<RC<TokenSyntax>> MaybeComma,
               GenericParameterSyntax Parameter);

  GenericParameterClauseBuilder &
  useRightAngleBracket(RC<TokenSyntax> RightAngleBracket);

  GenericParameterClauseSyntax build() const;
};

#pragma mark - generic-argument-list Data

class GenericArgumentListSyntaxData final : public SyntaxData {
  friend class SyntaxData;
  GenericArgumentListSyntaxData(RC<RawSyntax> Raw,
                                const SyntaxData *Parent = nullptr,
                                CursorIndex IndexInParent = 0);
  static RC<GenericArgumentListSyntaxData>
  make(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);
  static RC<GenericArgumentListSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::GenericArgumentList;
  }
};

#pragma mark - generic-argument-list API

/// generic-argument-list -> generic-argument
///                        | generic-argument ',' generic-argument-list
class GenericArgumentListSyntax final : public Syntax {
  friend struct SyntaxFactory;

  GenericArgumentListSyntax(RC<SyntaxData> Root,
                            GenericArgumentListSyntaxData *Data);

  static GenericArgumentListSyntax make(RC<RawSyntax> Raw);

  static GenericArgumentListSyntax makeBlank();

public:
  /// Get the n-th generic argument from this list.
  TypeSyntax getArgument(unsigned n) const;

  /// Return a GenericArgumentListSyntax with the given generic argument added.
  GenericArgumentListSyntax
  addGenericArgument(llvm::Optional<RC<TokenSyntax>> Comma,
                     TypeSyntax NewArgument) const;

  static bool classof (const Syntax *S) {
    return S->getKind() == SyntaxKind::GenericArgumentList;
  }
};

#pragma mark - generic-argument-clause Data

class GenericArgumentClauseSyntaxData final : public SyntaxData {
  friend class SyntaxData;
  friend struct SyntaxFactory;
  friend class GenericArgumentClauseBuilder;
  RC<GenericArgumentListSyntaxData> CachedGenericArgumentList;

  GenericArgumentClauseSyntaxData(RC<RawSyntax> Raw,
                                  const SyntaxData *Parent = nullptr,
                                  CursorIndex IndexInParent = 0);
  static RC<GenericArgumentClauseSyntaxData>
  make(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);
  static RC<GenericArgumentClauseSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::GenericArgumentClause;
  }
};

#pragma mark - generic-argument-clause API

/// generic-argument-clause -> '<' generic-argument-list '>'
class GenericArgumentClauseSyntax : public Syntax {
  friend struct SyntaxFactory;
  friend class GenericArgumentClauseBuilder;
  friend class SymbolicReferenceExprSyntax;
  friend class SyntaxData;

  using DataType = GenericArgumentClauseSyntaxData;

  enum class Cursor : CursorIndex {
    LeftAngleBracketToken,
    GenericArgumentList,
    RightAngleBracketToken,
  };

  GenericArgumentClauseSyntax(RC<SyntaxData> Root,
                              const GenericArgumentClauseSyntaxData *Data);

public:
  /// Return the left angle bracket '<' token on the generic argument clause.
  RC<TokenSyntax> getLeftAngleBracket() const;

  /// Return a new GenericArgumentClauseSyntax with the given left angle
  /// bracket '<' token.
  GenericArgumentClauseSyntax
  withLeftAngleBracket(RC<TokenSyntax> NewLeftAngleBracket) const;

  /// Return the GenericArgumentClauseSyntax inside the angle bracket tokens.
  GenericArgumentListSyntax getGenericParameterList() const;

  /// Return a GenericArgumentClauseSyntax with the given generic
  /// parameter list.
  GenericArgumentClauseSyntax
  withGenericParams(GenericParameterListSyntax NewGenericParams) const;

  /// Return the right angle bracket '>' token on the generic argument clause.
  RC<TokenSyntax> getRightAngleBracket() const;

  /// Return a new GenericArgumentClauseSyntax with the given right angle
  /// bracket '>' token.
  GenericArgumentClauseSyntax
  withRightAngleBracket(RC<TokenSyntax> NewRightAngleBracket) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::GenericArgumentClause;
  }
};

#pragma mark - generic-argument-clause Builder

class GenericArgumentClauseBuilder {
  RC<TokenSyntax> LeftAngleToken;
  RawSyntax::LayoutList ArgumentListLayout;
  RC<TokenSyntax> RightAngleToken;
public:
  GenericArgumentClauseBuilder();

  GenericArgumentClauseBuilder &
  useLeftAngleBracket(RC<TokenSyntax> LeftAngleBracket);

  GenericArgumentClauseBuilder &
  addGenericArgument(llvm::Optional<RC<TokenSyntax>> MaybeComma,
                     TypeSyntax ArgumentTypeSyntax);

  GenericArgumentClauseBuilder &
  useRightAngleBracket(RC<TokenSyntax> RightAngleBracket);

  GenericArgumentClauseSyntax build() const;
};

#pragma mark - generic-requirement-list Data

class GenericRequirementListSyntaxData final : public SyntaxData {
  friend class SyntaxData;
  GenericRequirementListSyntaxData(RC<RawSyntax> Raw,
                                   const SyntaxData *Parent = nullptr,
                                   CursorIndex IndexInParent = 0);
  static RC<GenericRequirementListSyntaxData>
  make(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);
  static RC<GenericRequirementListSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::GenericRequirementList;
  }
};

#pragma mark - generic-requirement-list API

/// requirement-list -> requirement | requirement ',' requirement-list
///
/// requirement -> conformance-requirement | same-type-requirement
class GenericRequirementListSyntax final : public Syntax {
  friend struct SyntaxFactory;

  GenericRequirementListSyntax(RC<SyntaxData> Root,
                               GenericRequirementListSyntaxData *Data);

  static GenericRequirementListSyntax make(RC<RawSyntax> Raw);
  static GenericRequirementListSyntax makeBlank();

public:
  // TODO: getRequirement(unsigned n) const;
  // TODO: withAddedRequirement(llvm::Optional<RC<TokenSyntax>> MaybeComma,
  //                            GenericRequirementSyntax NewRequirement) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::GenericRequirementList;
  }
};

#pragma mark - generic-where-clause Data

class GenericWhereClauseSyntaxData final : public SyntaxData {
  friend class SyntaxData;
  friend struct SyntaxFactory;

  RC<GenericRequirementListSyntaxData> CachedRequirementList;

  GenericWhereClauseSyntaxData(RC<RawSyntax> Raw,
                               const SyntaxData *Parent = nullptr,
                               CursorIndex IndexInParent = 0);
  static RC<GenericWhereClauseSyntaxData> make(RC<RawSyntax> Raw,
                                               const SyntaxData *Parent = nullptr,
                                               CursorIndex IndexInParent = 0);
  static RC<GenericWhereClauseSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::GenericWhereClause;
  }
};

#pragma mark - generic-where-clause API

/// generic-where-clause -> 'where' requirement-list
class GenericWhereClauseSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class GenericWhereClauseSyntaxData;
  enum class Cursor : CursorIndex {
    WhereToken,
    RequirementList,
  };

  GenericWhereClauseSyntax(RC<SyntaxData> Root,
                           const GenericWhereClauseSyntaxData *Data);

public:
  /// Return the 'where' keyword in the generic where clause.
  RC<TokenSyntax> getWhereKeyword() const;

  /// Return a GenericWhereClauseSyntax with the given 'where' keyword.
  GenericWhereClauseSyntax
  withWhereKeyword(RC<TokenSyntax> NewWhereKeyword) const;

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
