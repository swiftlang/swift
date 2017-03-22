//===--- TypeSyntax.h - Swift Type Syntax Interface -------------*- C++ -*-===//
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
// This file defines the interface for type syntax nodes,
// such as for type representations for tuple types (Int, Int), or
// function types () -> (), for example.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_TYPESYNTAX_H
#define SWIFT_SYNTAX_TYPESYNTAX_H

#include "swift/Syntax/References.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/SyntaxData.h"
#include "swift/Syntax/TokenSyntax.h"

namespace swift {
namespace syntax {

class GenericArgumentClauseSyntax;
class GenericArgumentClauseSyntaxData;
class GenericParameterClauseSyntax;
class GenericParameterClauseSyntaxData;

#pragma mark - balanced-tokens Data

class BalancedTokensSyntaxData final : public SyntaxData {
  friend class SyntaxData;
  friend struct SyntaxFactory;

  BalancedTokensSyntaxData(RC<RawSyntax> Raw,
                           const SyntaxData *Parent = nullptr,
                           CursorIndex IndexInParent = 0);
  static RC<BalancedTokensSyntaxData> make(RC<RawSyntax> Raw,
                                           const SyntaxData *Parent = nullptr,
                                           CursorIndex IndexInParent = 0);
  static RC<BalancedTokensSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::BalancedTokens;
  }
};

#pragma mark - balanced-tokens API

/// balanced-tokens -> Any identifier, keyword, literal, or operator
///                  | Any punctuation except (, ), [, ], {, or }
class BalancedTokensSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class SyntaxData;

  using DataType = BalancedTokensSyntaxData;

  BalancedTokensSyntax(RC<SyntaxData> Root,
                       const BalancedTokensSyntaxData *Data);

public:
  // TODO: TODO: BalancedTokensSyntax::getBalancedToken

  BalancedTokensSyntax
  addBalancedToken(RC<TokenSyntax> NewBalancedToken) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::BalancedTokens;
  }
};

#pragma mark - type-attribute Data

class TypeAttributeSyntaxData final : public SyntaxData {
  friend class SyntaxData;
  RC<BalancedTokensSyntaxData> CachedBalancedTokens;
  friend struct SyntaxFactory;

  TypeAttributeSyntaxData(RC<RawSyntax> Raw,
                          const SyntaxData *Parent = nullptr,
                          CursorIndex IndexInParent = 0);
  static RC<TypeAttributeSyntaxData> make(RC<RawSyntax> Raw,
                                          const SyntaxData *Parent = nullptr,
                                          CursorIndex IndexInParent = 0);
  static RC<TypeAttributeSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::TypeAttribute;
  }
};

#pragma mark - type-attribute API

/// type-attribute -> '@' identifier attribute-argument-clause?
/// attribute-argument-clause -> '(' balanced-tokens ')'
class TypeAttributeSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class TypeAttributeSyntaxData;
  friend class SyntaxData;

  using DataType = TypeAttributeSyntaxData;

  enum class Cursor : CursorIndex {
    AtSignToken,
    Identifier,
    LeftParenToken,
    BalancedTokens,
    RightParenToken,
  };

  TypeAttributeSyntax(RC<SyntaxData> Root, const TypeAttributeSyntaxData *Data);

public:

  /// Return the '@' token associated with the type attribute.
  RC<TokenSyntax> getAtSignToken() const;

  /// Return a new TypeAttributeSyntax with the given '@' token.
  TypeAttributeSyntax withAtSignToken(RC<TokenSyntax> NewAtSignToken) const;

  /// Return the name of the type attribute.
  RC<TokenSyntax> getIdentifier() const;

  /// Return a new TypeAttributeSyntax with the given name.
  TypeAttributeSyntax withIdentifier(RC<TokenSyntax> NewIdentifier) const;

  /// Return the left parenthesis '(' token attached to the type attribute.
  RC<TokenSyntax> getLeftParenToken() const;

  /// Return a TypeAttributeSyntax with the given left parenthesis '(' token.
  TypeAttributeSyntax
  withLeftParenToken(RC<TokenSyntax> NewLeftParenToken) const;

  /// Return the "balanced tokens" of the type attributes; the arguments.
  BalancedTokensSyntax getBalancedTokens() const;

  /// Return a TypeAttributeSyntax with the given balanced tokens as arguments
  /// to the type attribute.
  TypeAttributeSyntax
  withBalancedTokens(BalancedTokensSyntax NewBalancedTokens) const;

  /// Return the right parenthesis ')' token attached to the type attribute.
  RC<TokenSyntax> getRightParenToken() const;

  /// Return a TypeAttributeSyntax with the given right parenthesis ')' token.
  TypeAttributeSyntax
  withRightParenToken(RC<TokenSyntax> NewRightParenToken) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::TypeAttribute;
  }
};

#pragma mark - type-attributes Data

class TypeAttributesSyntaxData final : public SyntaxData {
  friend class SyntaxData;
  friend class TypeAttributesSyntax;
  friend struct SyntaxFactory;

  TypeAttributesSyntaxData(RC<RawSyntax> Raw,
                           const SyntaxData *Parent = nullptr,
                           CursorIndex IndexInParent = 0);
  static RC<TypeAttributesSyntaxData> make(RC<RawSyntax> Raw,
                                           const SyntaxData *Parent = nullptr,
                                           CursorIndex IndexInParent = 0);
  static RC<TypeAttributesSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::TypeAttributes;
  }
};

#pragma mark - type-attributes API

/// type-attributes -> type-attribute
///                  | type-attribute type-attributes
class TypeAttributesSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class TypeAttributesSyntaxData;
  friend class SyntaxData;
  friend class FunctionSignatureSyntax;
  friend class FunctionDeclSyntax;

  using DataType = TypeAttributesSyntaxData;

  TypeAttributesSyntax(RC<SyntaxData> Root,
                       const TypeAttributesSyntaxData *Data);
public:
  // TODO: Convert to SyntaxCollection
  // 

  TypeAttributesSyntax
  addTypeAttribute(TypeAttributeSyntax NewTypeAttribute) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::TypeAttributes;
  }
};

#pragma mark - type-syntax Data

class TypeSyntaxData : public SyntaxData {
  friend class SyntaxData;
  friend class TypeSyntax;

protected:
  TypeSyntaxData(RC<RawSyntax> Raw,
                 const SyntaxData *Parent = nullptr,
                 CursorIndex IndexInParent = 0);

public:
  static bool classof(const SyntaxData *S) {
    return S->isType();
  }
};

#pragma mark - type-syntax API

/// type -> array-type
///       | dictionary-type
///       | function-type
///       | type-identifier
///       | tuple-type
///       | optional-type
///       | implicitly-unwrapped-optional-type
///       | protocol-composition-type
///       | metatype-type
///       | 'Any'
///       | 'Self'
class TypeSyntax : public Syntax {
  using DataType = TypeSyntaxData;
  friend class SyntaxData;
  friend class FunctionParameterSyntax;
  friend class FunctionSignatureSyntax;
protected:
  TypeSyntax(const RC<SyntaxData> Root, const TypeSyntaxData *Data);
public:
  static bool classof(const Syntax *S) {
    return S->isType();
  }
};

#pragma mark - type-identifier Data

class TypeIdentifierSyntaxData final : public TypeSyntaxData {
  friend struct SyntaxFactory;
  friend class TypeIdentifierSyntax;
  friend class SyntaxData;
  
  RC<GenericArgumentClauseSyntaxData> CachedGenericArgumentClause;
  RC<TypeIdentifierSyntaxData> CachedChildTypeIdentifier;

  TypeIdentifierSyntaxData(RC<RawSyntax> Raw,
                           const SyntaxData *Parent = nullptr,
                           CursorIndex IndexInParent = 0);

  static RC<TypeIdentifierSyntaxData> make (RC<RawSyntax> Raw,
                                            const SyntaxData *Parent = nullptr,
                                            CursorIndex IndexInParent = 0);
  static RC<TypeIdentifierSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::TypeIdentifier;
  }
};

#pragma mark - type-identifier API

/// type-identifier -> type-name generic-argument-clause?
///                  | type-name generic-argument-clause '.' type-identifier
class TypeIdentifierSyntax final : public TypeSyntax {
  friend struct SyntaxFactory;
  friend class TypeIdentifierSyntaxData;
  friend class SyntaxData;

  using DataType = TypeIdentifierSyntaxData;

private:
  enum class Cursor {
    Identifier,
    GenericArgumentClause,
    DotToken,
    ChildTypeIdentifier,
  };

  TypeIdentifierSyntax(RC<SyntaxData> Root,
                       const TypeIdentifierSyntaxData *Data);

public:
  RC<TokenSyntax> getIdentifier() const;

  TypeIdentifierSyntax
  withIdentifier(RC<TokenSyntax> NewIdentifier) const;

  GenericArgumentClauseSyntax getGenericArgumentClause() const;
  TypeIdentifierSyntax
  withGenericArgumentClause(GenericArgumentClauseSyntax NewGenericArgs) const;

  RC<TokenSyntax> getDotToken() const;
  TypeIdentifierSyntax withDotToken(RC<TokenSyntax> NewIdentifier) const;

  TypeIdentifierSyntax getChildType() const;
  TypeIdentifierSyntax addChildType(TypeIdentifierSyntax ChildType) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::TypeIdentifier;
  }
};

#pragma mark - tuple-type-element Data

class TupleTypeElementSyntaxData final : public SyntaxData {
  friend class SyntaxData;
  friend struct SyntaxFactory;

  TupleTypeElementSyntaxData(RC<RawSyntax> Raw,
                             const SyntaxData *Parent = nullptr,
                             CursorIndex IndexInParent = 0);
  static RC<TupleTypeElementSyntaxData> make(RC<RawSyntax> Raw,
                                             const SyntaxData *Parent = nullptr,
                                             CursorIndex IndexInParent = 0);
  static RC<TupleTypeElementSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::TupleTypeElement;
  }
};

#pragma mark - tuple-type-element API

/// tuple-type-element -> (identifier ':')? type-attributes? 'inout'? type
///
/// Used for tuple elements and function argument types. This can simply be
/// a type without a label.
class TupleTypeElementSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class TupleTypeElementSyntaxData;
  friend class SyntaxData;

  enum class Cursor : CursorIndex {
    Label,
    ColonToken,
    Attributes,
    InoutToken,
    Type,
    CommaToken,
  };

  TupleTypeElementSyntax(RC<SyntaxData> Root,
                         const TupleTypeElementSyntaxData *Data);
public:
  using DataType = TupleTypeElementSyntaxData;
  
  /// Return the label of the tuple type element.
  RC<TokenSyntax> getLabel() const;

  /// Return a new named tuple type element with the specified identifier.
  TupleTypeElementSyntax withLabel(RC<TokenSyntax> NewIdentifier) const;

  /// Return the colon token of the tuple type element.
  RC<TokenSyntax> getColonToken() const;

  /// Return a new named tuple type element with a colon token replacement
  /// using the specified leading and trailing trivia.
  TupleTypeElementSyntax
  withColonToken(RC<TokenSyntax> NewColonToken) const;

  /// Return the comma token of the tuple type element.
  RC<TokenSyntax> getCommaToken() const;

  /// Return a new named tuple type element with a comma token replacement
  /// using the specified leading and trailing trivia.
  TupleTypeElementSyntax
  withCommaToken(RC<TokenSyntax> NewCommaToken) const;

  /// Return the type attributes for the tuple type element.
  TypeAttributesSyntax getTypeAttributes() const;

  /// Return a new named tuple type element with the specified attributes.
  TupleTypeElementSyntax
  withTypeAttributes(TypeAttributesSyntax NewTypeAttributes) const;

  /// Return the 'inout' token of the tuple type element.
  RC<TokenSyntax> getInoutToken() const;

  /// Return a new named tuple type element with the 'inout' keyword added.
  TupleTypeElementSyntax withInoutToken(RC<TokenSyntax> NewInoutToken) const;

  TypeSyntax getTypeSyntax() const;

  /// Return a new named tuple type element with the specified type.
  TupleTypeElementSyntax withTypeSyntax(TypeSyntax NewType) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::TupleTypeElement;
  }
};
  
#pragma mark - tuple-type Data

class TupleTypeSyntaxData final : public TypeSyntaxData {
  friend class SyntaxData;
  friend struct SyntaxFactory;
  friend class TupleTypeSyntaxBuilder;
  TupleTypeSyntaxData(RC<RawSyntax> Raw,
                      const SyntaxData *Parent = nullptr,
                      CursorIndex IndexInParent = 0);

  static RC<TupleTypeSyntaxData> make(RC<RawSyntax> Raw,
                                      const SyntaxData *Parent = nullptr,
                                      CursorIndex IndexInParent = 0);
  static RC<TupleTypeSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::TupleType;
  }
};

#pragma mark - tuple-type API

/// tuple-type -> '(' tuple-type-element-list ')'
class TupleTypeSyntax final : public TypeSyntax {
  friend struct SyntaxFactory;
  friend class TupleTypeSyntaxData;
  friend class SyntaxData;
  friend class TupleTypeSyntaxBuilder;

  using DataType = TupleTypeSyntaxData;

  enum class Cursor : CursorIndex {
    LeftParenToken,
    TypeElementList,
    RightParenToken,
  };

  TupleTypeSyntax(RC<SyntaxData> Root, const TupleTypeSyntaxData *Data);

public:
  /// Return the left paren '(' token surrounding the tuple type syntax.
  RC<TokenSyntax> getLeftParen() const;
  TupleTypeSyntax withLeftParen(RC<TokenSyntax> NewLeftParen) const;

  /// Get the type argument list inside the tuple type syntax.
  TupleTypeElementListSyntax getTypeElementList() const;

  /// Return a new tuple type syntax with the given type argument list.
  TupleTypeSyntax
  withTypeElementList(TupleTypeElementListSyntax NewTypeElementList) const;

  /// Return the right paren ')' token surrounding the tuple type syntax.
  RC<TokenSyntax> getRightParen() const;
  TupleTypeSyntax withRightParen(RC<TokenSyntax> NewRightParen) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::TupleType;
  }
};

#pragma mark - tuple-type Builder

/// Incrementally builds tuple type syntax.
class TupleTypeSyntaxBuilder final {
  RC<RawSyntax> LeftParenToken;
  RawSyntax::LayoutList ElementTypeLayout;
  RC<RawSyntax> RightParenToken;

public:
  TupleTypeSyntaxBuilder();

  /// Use the given left paren '(' token when building the tuple type syntax.
  TupleTypeSyntaxBuilder &useLeftParen(RC<TokenSyntax> LeftParen);

  /// Add an element type to the eventual tuple type syntax.
  TupleTypeSyntaxBuilder &
  addElementTypeSyntax(TupleTypeElementSyntax ElementTypeSyntax);

  /// Use the given left paren '(' token when building the tuple type syntax.
  TupleTypeSyntaxBuilder &useRightParen(RC<TokenSyntax> RightParen);

  /// Build a TupleTypeSyntax from the elements seen so far.
  ///
  /// This method is stateless and can be called multiple times to get
  /// new tuple type syntax nodes.
  TupleTypeSyntax build() const;
};

#pragma mark - metatype-type Data

class MetatypeTypeSyntaxData final : public TypeSyntaxData {
  friend struct SyntaxFactory;
  friend class SyntaxData;
  MetatypeTypeSyntaxData(RC<RawSyntax> Raw,
                         const SyntaxData *Parent = nullptr,
                         CursorIndex IndexInParent = 0);
  static RC<MetatypeTypeSyntaxData> make(RC<RawSyntax> Raw,
                                         const SyntaxData *Parent = nullptr,
                                         CursorIndex IndexInParent = 0);
  static RC<MetatypeTypeSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::MetatypeType;
  }
};

#pragma mark - metatype-type API

/// metatype-type -> type '.' 'Type'
///                | type '.' 'Protocol'
class MetatypeTypeSyntax final : public TypeSyntax {
  friend struct SyntaxFactory;
  friend class MetatypeTypeSyntaxData;
  friend class SyntaxData;

  using DataType = MetatypeTypeSyntaxData;

  enum class Cursor : CursorIndex {
    BaseType,
    DotToken,
    TypeToken,
  };

  MetatypeTypeSyntax(RC<SyntaxData> Root, const MetatypeTypeSyntaxData *Data);

public:
  TypeSyntax getBaseTypeSyntax() const;
  /// Return a new metatype type with the given base type - the `A` in `A.Type`.
  MetatypeTypeSyntax withBaseTypeSyntax(TypeSyntax NewBaseType) const;

  /// Return the dot token.
  RC<TokenSyntax> getDotToken() const;

  /// Return a new metatype type with the given dot token.
  MetatypeTypeSyntax withDotToken(RC<TokenSyntax> NewDotToken) const;

  /// Return the child type - either the identifiers `Type` or `Protocol`.
  RC<TokenSyntax> getTypeToken() const;

  /// Return a new metatype type with the given child type - either the
  /// identifiers: `Type` or `Protocol`.
  MetatypeTypeSyntax withTypeToken(RC<TokenSyntax> NewTypeToken) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::MetatypeType;
  }
};

#pragma mark - optional-type Data

class OptionalTypeSyntaxData final : public TypeSyntaxData {
  friend class SyntaxData;
  friend struct SyntaxFactory;
  OptionalTypeSyntaxData(RC<RawSyntax> Raw,
                         const SyntaxData *Parent = nullptr,
                         CursorIndex IndexInParent = 0);
  static RC<OptionalTypeSyntaxData> make(RC<RawSyntax> Raw,
                                         const SyntaxData *Parent = nullptr,
                                         CursorIndex IndexInParent = 0);
  static RC<OptionalTypeSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::OptionalType;
  }
};

#pragma mark - optional-type API

/// optional-type -> type '?'
class OptionalTypeSyntax final : public TypeSyntax {
  friend struct SyntaxFactory;
  friend class OptionalTypeSyntaxData;
  friend class SyntaxData;

  using DataType = OptionalTypeSyntaxData;

  enum class Cursor : CursorIndex {
    BaseType,
    QuestionToken
  };

  OptionalTypeSyntax(RC<SyntaxData> Root, const OptionalTypeSyntaxData *Data);

public:
  /// Return the syntax of the type to which this optional type refers.
  TypeSyntax getBaseTypeSyntax() const;

  /// Return a new optional type with the given base type.
  OptionalTypeSyntax withBaseTypeSyntax(TypeSyntax NewBaseType) const;

  /// Return the question-mark '?' token attached to this optional type syntax.
  RC<TokenSyntax> getQuestionToken() const;

  /// Return a new optional type with the given question-mark token.
  OptionalTypeSyntax
  withQuestionToken(RC<TokenSyntax> NewQuestionToken) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::OptionalType;
  }
};

#pragma mark - implicitly-unwrapped-optional-type Data

class ImplicitlyUnwrappedOptionalTypeSyntaxData final : public TypeSyntaxData {
  friend struct SyntaxFactory;
  friend class SyntaxData;
  ImplicitlyUnwrappedOptionalTypeSyntaxData(RC<RawSyntax> Raw,
                                            const SyntaxData *Parent = nullptr,
                                            CursorIndex IndexInParent = 0);

  static RC<ImplicitlyUnwrappedOptionalTypeSyntaxData>
  make(RC<RawSyntax> Raw,
       const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);
  static RC<ImplicitlyUnwrappedOptionalTypeSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::ImplicitlyUnwrappedOptionalType;
  }
};

#pragma mark - implicitly-unwrapped-optional-type API

/// implicitly-unwrapped-optional-type -> type '!'
class ImplicitlyUnwrappedOptionalTypeSyntax final : public TypeSyntax {
  friend struct SyntaxFactory;
  friend class ImplicitlyUnwrappedOptionalTypeSyntaxData;
  friend class SyntaxData;

  using DataType = ImplicitlyUnwrappedOptionalTypeSyntaxData;

  enum class Cursor : CursorIndex { Type, ExclaimToken };

  ImplicitlyUnwrappedOptionalTypeSyntax(RC<SyntaxData> Root,
      const ImplicitlyUnwrappedOptionalTypeSyntaxData *Data);

public:
  /// Return the syntax for the base type to which this implicitly unwrapped
  /// optional type refers.
  TypeSyntax getBaseTypeSyntax() const;

  /// Return a new implicitly unwrapped optional type syntax with the given base
  /// type syntax
  ImplicitlyUnwrappedOptionalTypeSyntax
  withBaseTypeSyntax(TypeSyntax NewBaseTypeSyntax) const;

  /// Return the exclamation-mark '!' token attached to the end of this
  /// implicitly unwrapped optional type syntax.
  RC<TokenSyntax> getExclaimToken() const;

  /// Return a new implicitly unwrapped optional type with the given
  /// exclamation-mark '!' token.
  ImplicitlyUnwrappedOptionalTypeSyntax
  withExclaimToken(RC<TokenSyntax> NewExclaimToken) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::OptionalType;
  }
};

#pragma mark - array-type Data

class ArrayTypeSyntaxData final : public TypeSyntaxData {
  friend class SyntaxData;
  friend struct SyntaxFactory;

  ArrayTypeSyntaxData(RC<RawSyntax> Raw,
                      const SyntaxData *Parent = nullptr,
                      CursorIndex IndexInParent = 0);

  static RC<ArrayTypeSyntaxData> make(RC<RawSyntax> Raw,
                                      const SyntaxData *Parent = nullptr,
                                      CursorIndex IndexInParent = 0);
  static RC<ArrayTypeSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::OptionalType;
  }
};

#pragma mark - array-type API

// array-type -> '[' type ']'
class ArrayTypeSyntax final : public TypeSyntax {
  friend struct SyntaxFactory;
  friend class ArrayTypeSyntaxData;
  friend class SyntaxData;

  using DataType = ArrayTypeSyntaxData;

  enum class Cursor : CursorIndex {
    LeftSquareBracketToken,
    Type,
    RightSquareBracketToken,
  };

  ArrayTypeSyntax(RC<SyntaxData> Root, const ArrayTypeSyntaxData *Data);

public:
  /// Return the left square bracket '[' token surrounding the array
  /// type syntax.
  RC<TokenSyntax> getLeftSquareBracketToken() const;

  /// Return a new array type with the given left square bracket token.
  ArrayTypeSyntax
  withLeftSquareBracketToken(RC<TokenSyntax> NewLeftSquareBracketToken) const;

  /// Return a new array type with the given element type.
  ArrayTypeSyntax withType(TypeSyntax NewType) const;

  /// Return the right square bracket ']' token surrounding the array
  /// type syntax.
  RC<TokenSyntax> getRightSquareBracketToken() const;

  /// Return a new array type with the given right square bracket token.
  ArrayTypeSyntax
  withRightSquareBracketToken(RC<TokenSyntax> NewRightSquareBracketToken) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::ArrayType;
  }
};

#pragma mark - dictionary-type Data

class DictionaryTypeSyntaxData final : public TypeSyntaxData {
  friend class SyntaxData;
  friend class DictionaryTypeSyntax;
  friend struct SyntaxFactory;

  RC<TypeSyntaxData> CachedKeyTypeSyntax;
  RC<TypeSyntaxData> CachedValueTypeSyntax;

  DictionaryTypeSyntaxData(RC<RawSyntax> Raw,
                           const SyntaxData *Parent = nullptr,
                           CursorIndex IndexInParent = 0);

  static RC<DictionaryTypeSyntaxData> make(RC<RawSyntax> Raw,
                                           const SyntaxData *Parent = nullptr,
                                           CursorIndex IndexInParent = 0);
  static RC<DictionaryTypeSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::ArrayType;
  }
};

#pragma mark - dictionary-type API

// dictionary-type -> '[' type ':' type ']'
class DictionaryTypeSyntax final : public TypeSyntax {
  friend struct SyntaxFactory;
  friend class DictionaryTypeSyntaxData;
  friend class SyntaxData;

  using DataType = DictionaryTypeSyntaxData;

  enum class Cursor : CursorIndex {
    LeftSquareBracketToken,
    KeyType,
    ColonToken,
    ValueType,
    RightSquareBracketToken,
  };

  DictionaryTypeSyntax(RC<SyntaxData> Root,
                       const DictionaryTypeSyntaxData *Data);

public:
  /// Return the left square bracket '[' token surrounding the dictionary
  /// type syntax.
  RC<TokenSyntax> getLeftSquareBracketToken() const;

  /// Return a new dictionary type with the given left square bracket token.
  DictionaryTypeSyntax
  withLeftSquareBracketToken(RC<TokenSyntax> NewLeftSquareBracketToken) const;

  /// Return the key type syntax for this dictionary type.
  TypeSyntax getKeyTypeSyntax() const;

  /// Return a new dictionary type with the given key type.
  DictionaryTypeSyntax withKeyTypeSyntax(TypeSyntax NewKeyType) const;

  /// Get the colon token in the dictionary type syntax.
  RC<TokenSyntax> getColonToken() const;

  /// Return a new dictionary type with the given colon token.
  DictionaryTypeSyntax withColon(RC<TokenSyntax> NewColonToken) const;

  /// Return the value type syntax for this dictionary type.
  TypeSyntax getValueTypeSyntax() const;

  /// Return a new dictionary type with the given value type.
  DictionaryTypeSyntax withValueTypeSyntax(TypeSyntax NewTypeSyntax) const;

  /// Return the right square bracket ']' token surrounding the dictionary
  /// type syntax.
  RC<TokenSyntax> getRightSquareBracketToken() const;

  /// Return a new dictionary type with the given right square bracket token.
  DictionaryTypeSyntax
  withRightSquareBracketToken(RC<TokenSyntax> NewRightSquareBracketToken) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::DictionaryType;
  }
};

#pragma mark - function-type-argument Data

class FunctionTypeArgumentSyntaxData final : public SyntaxData {
  friend class SyntaxData;
  friend struct SyntaxFactory;

  FunctionTypeArgumentSyntaxData(RC<RawSyntax> Raw,
                                 const SyntaxData *Parent = nullptr,
                                 CursorIndex IndexInParent = 0);

  static RC<FunctionTypeArgumentSyntaxData>
  make(RC<RawSyntax> Raw,
       const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);
  static RC<FunctionTypeArgumentSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *SD) {
    return SD->getKind() == SyntaxKind::FunctionTypeArgument;
  }
};

#pragma mark - function-type-argument API

class FunctionTypeArgumentSyntax final : public Syntax {
  friend struct SyntaxFactory;
  friend class SyntaxData;

  using DataType = FunctionTypeArgumentSyntaxData;

  enum class Cursor : CursorIndex {
    ExternalParameterName,
    LocalParameterName,
    ColonToken,
    TypeAttributes,
    InoutKeyword,
    Type,
  };

  FunctionTypeArgumentSyntax(RC<SyntaxData> Root,
                             const FunctionTypeArgumentSyntaxData *Data);

public:
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::FunctionTypeArgument;
  }
};


#pragma mark - function-type Data

class FunctionTypeSyntaxData final : public TypeSyntaxData {
  friend class SyntaxData;
  friend class FunctionTypeSyntax;
  friend class FunctionTypeSyntaxBuilder;
  friend struct SyntaxFactory;

  FunctionTypeSyntaxData(RC<RawSyntax> Raw,
                         const SyntaxData *Parent = nullptr,
                         CursorIndex IndexInParent = 0);
  static RC<FunctionTypeSyntaxData> make(RC<RawSyntax> Raw,
                                         const SyntaxData *Parent = nullptr,
                                         CursorIndex IndexInParent = 0);
  static RC<FunctionTypeSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::DictionaryType;
  }
};

#pragma mark - function-type API

/// function-type ->
///   type-attributes? function-type-argument-clause 'throws'? '->' type
/// | type-attributes? function-type-argument-clause 'rethrows' '->' type
class FunctionTypeSyntax final : public TypeSyntax {
  friend struct SyntaxFactory;
  friend class FunctionTypeSyntaxBuilder;
  friend class FunctionTypeSyntaxData;
  friend class SyntaxData;

  using DataType = FunctionTypeSyntaxData;

  enum class Cursor : CursorIndex {
    TypeAttributes,
    LeftParen,
    ArgumentList,
    RightParen,
    ThrowsOrRethrows,
    Arrow,
    ReturnType
  };

  FunctionTypeSyntax(RC<SyntaxData> Root,
                     const FunctionTypeSyntaxData *Data);

public:

  /// Return the type attributes for the function type.
  TypeAttributesSyntax getAttributes() const;

  /// Return a new function type with the given type attributes.
  FunctionTypeSyntax
  withTypeAttributes(TypeAttributesSyntax NewAttributes) const;

  /// Return the left parenthesis '(' token surrounding the argument type.
  RC<TokenSyntax> getLeftArgumentsParen() const;

  /// Return a new function type with the given left parenthesis on the type
  /// argument list.
  FunctionTypeSyntax
  withLeftArgumentsParen(RC<TokenSyntax> NewLeftParen) const;

  /// Return a new function type with the additional argument type and
  /// optionally a preceding comma token.
  FunctionTypeSyntax
  addTypeArgument(llvm::Optional<RC<TokenSyntax>> MaybeComma,
                  FunctionTypeArgumentSyntax NewArgument) const;

  /// Return the type arguments list for this function type syntax.
  TupleTypeElementListSyntax getTypeElementList() const;

  /// Return a new function type with the given type argument list.
  ///
  /// This replaces all of the argument types.
  FunctionTypeSyntax
  withTypeElementList(TupleTypeElementListSyntax NewArgumentList) const;

  /// Return the right parenthesis ')' token surrounding the argument type.
  RC<TokenSyntax> getRightArgumentsParen() const;

  /// Return a new function type with the given right parenthesis ')'
  /// on the type argument list.
  FunctionTypeSyntax
  withRightArgumentsParen(RC<TokenSyntax> NewRightParen) const;

  /// Return the 'throws' or 'rethrows' keyword on the function type syntax.
  RC<TokenSyntax> getThrowsOrRethrowsKeyword() const;

  /// Return a new function type with the given `throws` keyword.
  ///
  /// This fills the same slot held by the `rethrows` keyword.
  FunctionTypeSyntax withThrowsKeyword(RC<TokenSyntax> NewThrowsKeyword) const;

  /// Return a new function type with the given `rethrows` keyword.
  ///
  /// This fills the same slot held by the `throws` keyword.
  FunctionTypeSyntax
  withRethrowsKeyword(RC<TokenSyntax> NewThrowsKeyword) const;

  /// Return the arrow token in the function type syntax.
  RC<TokenSyntax> getArrow() const;

  /// Return a new function type with the given arrow token.
  FunctionTypeSyntax withArrow(RC<TokenSyntax> NewArrow) const;

  // Return the return type syntax for the function type.
  TypeSyntax getReturnTypeSyntax() const;

  /// Return a new function type with the given return type.
  FunctionTypeSyntax withReturnTypeSyntax(TypeSyntax NewReturnType) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::FunctionType;
  }
};

#pragma mark - function-type Builder

/// Incrementally builds function type syntax.
class FunctionTypeSyntaxBuilder final {
  RawSyntax::LayoutList FunctionTypeLayout;
public:
  FunctionTypeSyntaxBuilder();

  /// Use the given type attributes when building the eventual function
  /// syntax.
  FunctionTypeSyntaxBuilder &
  useTypeAttributes(TypeAttributeSyntax NewAttributes);

  /// Use the given left paren '(' token on the argument type syntax.
  FunctionTypeSyntaxBuilder &useLeftArgumentsParen(RC<TokenSyntax> LeftParen);

  FunctionTypeSyntaxBuilder &
  addArgumentTypeSyntax(llvm::Optional<RC<TokenSyntax>> MaybeComma,
                        FunctionTypeArgumentSyntax Argument);

  /// Use the given right paren ')' token on the argument type syntax.
  FunctionTypeSyntaxBuilder &useRightArgumentsParen(RC<TokenSyntax> RightParen);

  /// Use the given 'throws' keyword in the function type syntax.
  FunctionTypeSyntaxBuilder &useThrowsKeyword(RC<TokenSyntax> ThrowsKeyword);

  FunctionTypeSyntaxBuilder &
  useRethrowsKeyword(RC<TokenSyntax> RethrowsKeyword);

  FunctionTypeSyntaxBuilder &useArrow(RC<TokenSyntax> Arrow);
  FunctionTypeSyntaxBuilder &useReturnTypeSyntax(TypeSyntax ReturnType);

  FunctionTypeSyntax build() const;
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_TYPESYNTAX_H
