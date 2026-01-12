//===--- LiteralExpressionFolding.h - ---------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Simple AST-based evaluator of supported literal expressions
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_LITERAL_EXPR_FOLD_H
#define SWIFT_SEMA_LITERAL_EXPR_FOLD_H

#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"

namespace swift {
namespace LiteralExprFolding {

/// Kinds of failures which can be encountered when
/// constant-folding or literal/const expression
enum class IllegalConstError : int8_t {
  TypeNotSupported,
  AssociatedValue,
  UnsupportedBinaryOperator,
  UnsupportedUnaryOperator,
  TypeExpression,
  KeyPath,
  Closure,
  ClosureWithCaptures,
  OpaqueDeclRef,
  NonConstDeclRef,
  OpaqueFuncDeclRef,
  NonConventionCFunc,
  OpaqueCalleeRef,
  NonConstParameter,
  DivideByZero,
  IntegerOverflow,
  UpstreamError,
  Default
};

bool supportedOperator(const ApplyExpr *operatorApplyExpr);
void diagnoseError(SourceLoc errorLoc, IllegalConstError reason,
                   DiagnosticEngine &diags);

} // namespace LiteralExprFolding

/// Attempt to constant-fold an expression down to a
/// constant of integer or floating-point type.
/// Returns a `IntegerLiteralExpr` if successful,
/// `nullptr` otherwise.
Expr *foldLiteralExpression(const Expr *expr, ASTContext *ctx);

} // namespace swift

#endif // SWIFT_SEMA_LITERAL_EXPR_FOLD_H
