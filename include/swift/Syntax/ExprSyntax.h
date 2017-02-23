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

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_EXPRSYNTAX_H
