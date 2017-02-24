//===--- ExprSyntax.h - Swift Expression Syntax Interface --------*- C++ -*-===//
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
// This file defines the interface for expresion-specific syntax nodes,
// such as 
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_EXPRSYNTAX_H
#define SWIFT_SYNTAX_EXPRSYNTAX_H

#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/SyntaxData.h"
#include "swift/Syntax/TokenSyntax.h"

using llvm::Optional;

namespace swift {
namespace syntax {

class GenericArgumentClauseSyntax;
class GenericArgumentClauseSyntaxData;

class ExprSyntaxData : public SyntaxData {
protected:
  ExprSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
                 CursorIndex IndexInParent = 0)
    : SyntaxData(Raw, Parent, IndexInParent) {
    assert(Raw->isExpr());
  }
public:
  static RC<ExprSyntaxData> make(RC<RawSyntax> Raw,
                                 const SyntaxData *Parent = nullptr,
                                 CursorIndex IndexInParent = 0);
  static RC<ExprSyntaxData> makeBlank();
  static bool classof(const SyntaxData *S) {
    return S->isExpr();
  }
};

class ExprSyntax : public Syntax {
public:
  using DataType = ExprSyntaxData;

  ExprSyntax(const RC<SyntaxData> Root, const ExprSyntaxData *Data);
  static bool classof(const Syntax *S) {
    return S->isExpr();
  }
};

#pragma mark - unknown-expression Data

class UnknownExprSyntaxData : public ExprSyntaxData {
  UnknownExprSyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
                        CursorIndex IndexInParent = 0);
public:
  static RC<UnknownExprSyntaxData> make(RC<RawSyntax> Raw,
                                 const SyntaxData *Parent = nullptr,
                                 CursorIndex IndexInParent = 0);

  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::UnknownExpr;
  }
};

#pragma mark - unknown-expression API

class UnknownExprSyntax : public ExprSyntax {
  friend class SyntaxData;
  friend class UnknownExprSyntaxData;
  friend class LegacyASTTransformer;

  using DataType = UnknownExprSyntaxData;

  UnknownExprSyntax(const RC<SyntaxData> Root,
                    const UnknownExprSyntaxData *Data);

public:
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::UnknownExpr;
  }
};

#pragma mark - integer-literal-expression Data

class IntegerLiteralExprSyntaxData : public ExprSyntaxData {
  friend struct SyntaxFactory;
  friend class SyntaxData;

  IntegerLiteralExprSyntaxData(RC<RawSyntax> Raw,
                               const SyntaxData *Parent = nullptr,
                               CursorIndex IndexInParent = 0);
  static RC<IntegerLiteralExprSyntaxData> make(RC<RawSyntax> Raw,
                                       const SyntaxData *Parent = nullptr,
                                       CursorIndex IndexInParent = 0);
  static RC<IntegerLiteralExprSyntaxData> makeBlank();
public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::IntegerLiteralExpr;
  }
};

#pragma mark - integer-literal-expression API

class IntegerLiteralExprSyntax : public ExprSyntax {
  using DataType = IntegerLiteralExprSyntaxData;
  friend struct SyntaxFactory;
  friend class SyntaxData;
  friend class IntegerLiteralExprSyntaxData;

  IntegerLiteralExprSyntax(const RC<SyntaxData> Root,
                           const IntegerLiteralExprSyntaxData *Data);

  enum class Cursor : CursorIndex {
    Sign,
    Digits
  };

public:

  /// Get the '+' or '-' associated with this integer literal expression.
  RC<TokenSyntax> getSign() const;

  /// Return a new IntegerLiteralExprSyntax with the given '+' or '-' sign.
  IntegerLiteralExprSyntax withSign(RC<TokenSyntax> NewSign) const;

  /// Return the string of digits comprising the number part of the integer
  /// literal expression.
  RC<TokenSyntax> getDigits() const;

  /// Return a new IntegerLiteralExprSyntax with the given string of digits.
  IntegerLiteralExprSyntax withDigits(RC<TokenSyntax> NewDigits) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::IntegerLiteralExpr;
  }
};

#pragma mark - symbolic-reference Data

class SymbolicReferenceExprSyntaxData : public ExprSyntaxData {
  friend class SymbolicReferenceExprSyntax;
  friend class SyntaxData;
  friend struct SyntaxFactory;

  RC<GenericArgumentClauseSyntaxData> CachedGenericArgClause;

  SymbolicReferenceExprSyntaxData(RC<RawSyntax> Raw,
                                  const SyntaxData *Parent = nullptr,
                                  CursorIndex IndexInParent = 0);

  static RC<SymbolicReferenceExprSyntaxData>
  make(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);

  static RC<SymbolicReferenceExprSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::SymbolicReferenceExpr;
  }
};

#pragma mark - symbolic-reference API

/// symbolic-reference-expression -> identifier generic-argument-clause?
///
/// This is shown as primary-expression -> identifier generic-argument-clause?
/// in the grammar. It can be just an identifier referring to some
/// declaration, or it could perhaps be a constructor call to `Array<Int>`.
class SymbolicReferenceExprSyntax : public ExprSyntax {

  using DataType = SymbolicReferenceExprSyntaxData;

  friend struct SyntaxFactory;
  friend class SyntaxData;
  friend class Syntax;
  friend class SymbolicReferenceExprSyntaxData;

  enum class Cursor : CursorIndex {
    Identifier,
    GenericArgumentClause
  };

  SymbolicReferenceExprSyntax(const RC<SyntaxData> Root,
                              const DataType *Data);

public:

  /// Get the identifier for the symbol to which this expression refers.
  RC<TokenSyntax> getIdentifier() const;

  /// Return a new `SymbolicReferenceExprSyntax` with the given identifier.
  SymbolicReferenceExprSyntax
  withIdentifier(RC<TokenSyntax> NewIdentifier) const;

  /// Return the generic arguments this symbolic reference has, if it has one.
  llvm::Optional<GenericArgumentClauseSyntax> getGenericArgumentClause() const;

  /// Return a new `SymbolicReferenceExprSyntax` with the given generic
  /// arguments.
  SymbolicReferenceExprSyntax
  withGenericArgumentClause(GenericArgumentClauseSyntax NewGenericArgs) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::SymbolicReferenceExpr;
  }
};

#pragma mark - function-call-argument Data

class FunctionCallArgumentSyntaxData : public SyntaxData {
  friend struct SyntaxFactory;
  friend class FunctionCallArgumentSyntax;
  friend class SyntaxData;

  RC<ExprSyntaxData> CachedExpression;

  FunctionCallArgumentSyntaxData(RC<RawSyntax> Raw,
                           const SyntaxData *Parent = nullptr,
                           CursorIndex IndexInParent = 0);

  static RC<FunctionCallArgumentSyntaxData>
  make(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);

  static RC<FunctionCallArgumentSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::FunctionCallArgument;
  }
};

#pragma mark - function-call-argument API

/// function-call-argument -> label? ':'? (expression | operator) ','?
class FunctionCallArgumentSyntax : public Syntax {

  using DataType = FunctionCallArgumentSyntaxData;

  friend struct SyntaxFactory;
  friend class SyntaxData;
  friend class Syntax;
  friend class FunctionCallArgumentSyntaxData;
  friend class FunctionCallArgumentListSyntax;

  enum class Cursor {
    Label,
    Colon,
    Expression,
    Comma,
  };

  FunctionCallArgumentSyntax(const RC<SyntaxData> Root,
                             const DataType *Data);

public:

  /// Return the label identifier for this argument, if it has one.
  RC<TokenSyntax> getLabel() const;

  /// Return a new `FunctionCallArgumentSyntax` with the given label.
  FunctionCallArgumentSyntax withLabel(RC<TokenSyntax> NewLabel) const;

  /// Get the colon ':' token in between the label and argument,
  /// if there is one.
  RC<TokenSyntax> getColonToken() const;

  /// Return a new `FunctionCallArgumentSyntax` with the given colon ':' token.
  FunctionCallArgumentSyntax withColonToken(RC<TokenSyntax> NewColon) const;

  /// Returns the expression of the argument.
  llvm::Optional<ExprSyntax> getExpression() const;

  /// Return a new `FunctionCallArgumentSyntax` with the given expression
  /// argument.
  FunctionCallArgumentSyntax withExpression(ExprSyntax NewExpression) const;

  /// Get the comma ',' token immediately following this argument, if there
  /// is one.
  RC<TokenSyntax> getTrailingComma() const;

  /// Return a new `FunctionCallArgumentSyntax` with the given comma attached
  /// to the end of the argument.
  FunctionCallArgumentSyntax
  withTrailingComma(RC<TokenSyntax> NewTrailingComma) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::FunctionCallArgument;
  }
};

#pragma mark - function-call-argument-list Data

class FunctionCallArgumentListSyntaxData : public SyntaxData {
  friend struct SyntaxFactory;
  friend class FunctionCallArgumentListSyntax;
  friend class FunctionCallExprSyntaxBuilder;
  friend class SyntaxData;

  std::vector<RC<FunctionCallArgumentSyntaxData>> CachedArguments;

  FunctionCallArgumentListSyntaxData(const RC<RawSyntax> Raw,
                                     const SyntaxData *Parent = nullptr,
                                     CursorIndex IndexInParent = 0);

  static RC<FunctionCallArgumentListSyntaxData>
  make(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);

  static RC<FunctionCallArgumentListSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::FunctionCallArgumentList;
  }
};

#pragma mark - function-call-argument-list API

/// function-call-argument-list -> function-call-argument
///                                function-call-argument-list?
class FunctionCallArgumentListSyntax : public Syntax {
  using DataType = FunctionCallArgumentListSyntaxData;
  friend struct SyntaxFactory;
  friend class FunctionCallArgumentListSyntaxData;
  friend class FunctionCallExprSyntax;
  friend class Syntax;
  friend class SyntaxData;

  FunctionCallArgumentListSyntax(const RC<SyntaxData> Root,
                                 const DataType *Data);

public:
  /// Return the number of arguments in this list.
  size_t getNumArguments() const;

  /// Get the argument at the given Index.
  FunctionCallArgumentSyntax getArgument(size_t n) const;

  /// Returns a new `FunctionCallArgumentListSyntax` with the given
  /// argument added to the end.
  FunctionCallArgumentListSyntax
  withAdditionalArgument(FunctionCallArgumentSyntax AdditionalArgument) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::FunctionCallExpr;
  }
};

#pragma mark - function-call-expression Data

class FunctionCallExprSyntaxData : public ExprSyntaxData {
  friend struct SyntaxFactory;
  friend class FunctionCallExprSyntax;
  friend class FunctionCallExprSyntaxBuilder;
  friend class SyntaxData;

  RC<ExprSyntaxData> CachedCalledExpression;
  RC<FunctionCallArgumentListSyntaxData> CachedArgumentList;

  FunctionCallExprSyntaxData(RC<RawSyntax> Raw,
                                 const SyntaxData *Parent = nullptr,
                                 CursorIndex IndexInParent = 0);

  static RC<FunctionCallExprSyntaxData>
  make(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
       CursorIndex IndexInParent = 0);

  static RC<FunctionCallExprSyntaxData> makeBlank();

public:
  static bool classof(const SyntaxData *S) {
    return S->getKind() == SyntaxKind::FunctionCallExpr;
  }
};

#pragma mark - function-call-expression API

class FunctionCallExprSyntax : public ExprSyntax {
  using DataType = FunctionCallExprSyntaxData;
  friend struct SyntaxFactory;
  friend class FunctionCallExprSyntaxData;
  friend class FunctionCallExprSyntaxBuilder;
  friend class Syntax;
  friend class SyntaxData;

  enum class Cursor: CursorIndex {
    CalledExpression,
    LeftParen,
    ArgumentList,
    RightParen,
  };

  FunctionCallExprSyntax(const RC<SyntaxData> Root, const DataType *Data);

public:

  /// Get the base expression getting called.
  ExprSyntax getCalledExpression() const;

  /// Return a new `FunctionCallExprSyntax` with the given base expression
  /// to be called.
  FunctionCallExprSyntax
  withCalledExpression(ExprSyntax NewBaseExpression) const;

  /// Return the left parenthesis '(' token in this call.
  RC<TokenSyntax> getLeftParen() const;

  /// Return a new `FunctionCallExprSyntax` with the given left parenthesis '('
  /// token.
  FunctionCallExprSyntax withLeftParen(RC<TokenSyntax> NewLeftParen) const;

  /// Get the list of arguments in this call expression.
  FunctionCallArgumentListSyntax getArgumentList() const;

  /// Return a new `FunctionCallExprSyntax` with the given argument list.
  FunctionCallExprSyntax
  withArgumentList(FunctionCallArgumentListSyntax NewArgumentList) const;

  /// Return the right parenthesis ')' token in this call.
  RC<TokenSyntax> getRightParen() const;

  /// Return a new `FunctionCallExprSyntax` with the given right parenthesis ')'
  /// token.
  FunctionCallExprSyntax withRightParen(RC<TokenSyntax> NewLeftParen) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::FunctionCallExpr;
  }
};

#pragma mark - function-call-argument-list-builder
class FunctionCallExprSyntaxBuilder {
  RawSyntax::LayoutList CallLayout;
  RawSyntax::LayoutList ListLayout;
public:

  /// Start the builder with all elements marked as missing or empty.
  FunctionCallExprSyntaxBuilder();

  /// Use the given expression as the call target.
  FunctionCallExprSyntaxBuilder &
  useCalledExpression(ExprSyntax CalledExpression);

  /// Use the given left parenthesis '(' token in the function call.
  FunctionCallExprSyntaxBuilder &useLeftParen(RC<TokenSyntax> LeftParen);

  /// Add an additional argument to the layout.
  FunctionCallExprSyntaxBuilder &
  appendArgument(FunctionCallArgumentSyntax AdditionalArgument);

  /// Use the given right parenthesis ')' token in the function call.
  FunctionCallExprSyntaxBuilder &useRightParen(RC<TokenSyntax> RightParen);

  /// Return a `FunctionCallExprSyntax` with the arguments added so far.
  FunctionCallExprSyntax build() const;
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_EXPRSYNTAX_H
