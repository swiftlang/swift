//===--- ExprSyntax.h - Swift Expression Syntax Interface -------*- C++ -*-===//
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
// This file defines the interface for expression-specific syntax nodes,
// such as 
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_EXPRSYNTAX_H
#define SWIFT_SYNTAX_EXPRSYNTAX_H

#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/SyntaxData.h"
#include "swift/Syntax/SyntaxCollection.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/UnknownSyntax.h"

using llvm::Optional;

namespace swift {
namespace syntax {


#pragma mark - function-call-argument-list API

class FunctionCallArgumentSyntax;

class GenericArgumentClauseSyntax;

class ExprSyntax : public Syntax {
  friend class FunctionParameterSyntax;

protected:
  virtual void validate() const override {}

public:
  static ExprSyntax makeBlank();
  ExprSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : Syntax(Root, Data) {}
  static bool classof(const Syntax *S) {
    return S->isExpr();
  }
};

#pragma mark - unknown-expression API

class UnknownExprSyntax : public UnknownSyntax {
  friend class LegacyASTTransformer;

  virtual void validate() const override;
public:
  static UnknownExprSyntax makeBlank();
  UnknownExprSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : UnknownSyntax(Root, Data) {}
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::UnknownExpr;
  }
};

#pragma mark - integer-literal-expression API

class IntegerLiteralExprSyntax : public ExprSyntax {
    friend struct SyntaxFactory;
  
  virtual void validate() const override;

  enum class Cursor : CursorIndex {
    Sign,
    Digits
  };

public:

  static IntegerLiteralExprSyntax makeBlank();
  IntegerLiteralExprSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : ExprSyntax(Root, Data) {}

  /// Get the '+' or '-' associated with this integer literal expression.
  TokenSyntax getSign() const;

  /// Return a new IntegerLiteralExprSyntax with the given '+' or '-' sign.
  IntegerLiteralExprSyntax withSign(TokenSyntax NewSign) const;

  /// Return the string of digits comprising the number part of the integer
  /// literal expression.
  TokenSyntax getDigits() const;

  /// Return a new IntegerLiteralExprSyntax with the given string of digits.
  IntegerLiteralExprSyntax withDigits(TokenSyntax NewDigits) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::IntegerLiteralExpr;
  }

  virtual ~IntegerLiteralExprSyntax() {}
};

#pragma mark - symbolic-reference API

/// symbolic-reference-expression -> identifier generic-argument-clause?
///
/// This is shown as primary-expression -> identifier generic-argument-clause?
/// in the grammar. It can be just an identifier referring to some
/// declaration, or it could perhaps be a constructor call to `Array<Int>`.
class SymbolicReferenceExprSyntax : public ExprSyntax {
  friend struct SyntaxFactory;
  friend class Syntax;

  enum class Cursor : CursorIndex {
    Identifier,
    GenericArgumentClause
  };

  virtual void validate() const override;

public:
  static SymbolicReferenceExprSyntax makeBlank();
  SymbolicReferenceExprSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : ExprSyntax(Root, Data) {}


  /// Get the identifier for the symbol to which this expression refers.
  TokenSyntax getIdentifier() const;

  /// Return a new `SymbolicReferenceExprSyntax` with the given identifier.
  SymbolicReferenceExprSyntax
  withIdentifier(TokenSyntax NewIdentifier) const;

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

#pragma mark - function-call-argument API

/// function-call-argument -> label? ':'? (expression | operator) ','?
class FunctionCallArgumentSyntax : public Syntax {

  friend struct SyntaxFactory;
  friend class Syntax;
  friend class SyntaxCollection<SyntaxKind::FunctionCallArgumentList,
                                FunctionCallArgumentSyntax>;

  enum class Cursor {
    Label,
    Colon,
    Expression,
    Comma,
  };

  virtual void validate() const override;

public:

  static FunctionCallArgumentSyntax makeBlank();
  FunctionCallArgumentSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : Syntax(Root, Data) {}

  /// Return the label identifier for this argument, if it has one.
  TokenSyntax getLabel() const;

  /// Return a new `FunctionCallArgumentSyntax` with the given label.
  FunctionCallArgumentSyntax withLabel(TokenSyntax NewLabel) const;

  /// Get the colon ':' token in between the label and argument,
  /// if there is one.
  TokenSyntax getColonToken() const;

  /// Return a new `FunctionCallArgumentSyntax` with the given colon ':' token.
  FunctionCallArgumentSyntax withColonToken(TokenSyntax NewColon) const;

  /// Returns the expression of the argument.
  llvm::Optional<ExprSyntax> getExpression() const;

  /// Return a new `FunctionCallArgumentSyntax` with the given expression
  /// argument.
  FunctionCallArgumentSyntax withExpression(ExprSyntax NewExpression) const;

  /// Get the comma ',' token immediately following this argument, if there
  /// is one.
  TokenSyntax getTrailingComma() const;

  /// Return a new `FunctionCallArgumentSyntax` with the given comma attached
  /// to the end of the argument.
  FunctionCallArgumentSyntax
  withTrailingComma(TokenSyntax NewTrailingComma) const;

  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::FunctionCallArgument;
  }
};

#pragma mark - function-call-expression API

class FunctionCallExprSyntax : public ExprSyntax {
  friend struct SyntaxFactory;
  friend class FunctionCallExprSyntaxBuilder;
  friend class Syntax;
  
  enum class Cursor: CursorIndex {
    CalledExpression,
    LeftParen,
    ArgumentList,
    RightParen,
  };

  virtual void validate() const override;

public:
  static FunctionCallExprSyntax makeBlank();
  FunctionCallExprSyntax(const RC<SyntaxData> Root, const SyntaxData *Data)
    : ExprSyntax(Root, Data) {}

  /// Get the base expression getting called.
  ExprSyntax getCalledExpression() const;

  /// Return a new `FunctionCallExprSyntax` with the given base expression
  /// to be called.
  FunctionCallExprSyntax
  withCalledExpression(ExprSyntax NewBaseExpression) const;

  /// Return the left parenthesis '(' token in this call.
  TokenSyntax getLeftParen() const;

  /// Return a new `FunctionCallExprSyntax` with the given left parenthesis '('
  /// token.
  FunctionCallExprSyntax withLeftParen(TokenSyntax NewLeftParen) const;

  /// Get the list of arguments in this call expression.
  FunctionCallArgumentListSyntax getArgumentList() const;

  /// Return a new `FunctionCallExprSyntax` with the given argument list.
  FunctionCallExprSyntax
  withArgumentList(FunctionCallArgumentListSyntax NewArgumentList) const;

  /// Return the right parenthesis ')' token in this call.
  TokenSyntax getRightParen() const;

  /// Return a new `FunctionCallExprSyntax` with the given right parenthesis ')'
  /// token.
  FunctionCallExprSyntax withRightParen(TokenSyntax NewLeftParen) const;

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
  FunctionCallExprSyntaxBuilder &useLeftParen(TokenSyntax LeftParen);

  /// Add an additional argument to the layout.
  FunctionCallExprSyntaxBuilder &
  appendArgument(FunctionCallArgumentSyntax AdditionalArgument);

  /// Use the given right parenthesis ')' token in the function call.
  FunctionCallExprSyntaxBuilder &useRightParen(TokenSyntax RightParen);

  /// Return a `FunctionCallExprSyntax` with the arguments added so far.
  FunctionCallExprSyntax build() const;
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_EXPRSYNTAX_H
