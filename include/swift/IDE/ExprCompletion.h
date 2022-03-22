//===--- ExprCompletion.h -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_EXPRCOMPLETION_H
#define SWIFT_IDE_EXPRCOMPLETION_H

#include "swift/IDE/CodeCompletionConsumer.h"
#include "swift/IDE/CodeCompletionContext.h"
#include "swift/IDE/TypeCheckCompletionCallback.h"

namespace swift {
namespace ide {

class ExprTypeCheckCompletionCallback : public TypeCheckCompletionCallback {
public:
  struct Result {
    /// The contextual type that the code completion expression should produce.
    Type ExpectedType;

    /// If the code completion expression is an implicit return in a
    /// single-expression closure.
    bool IsImplicitSingleExpressionReturn;

    /// Whether the surrounding context is async and thus calling async
    /// functions is supported.
    bool IsInAsyncContext;

    /// Types of variables that were determined in the solution that produced
    /// this result. This in particular includes parameters of closures that
    /// were type-checked with the code completion expression.
    llvm::SmallDenseMap<const VarDecl *, Type> SolutionSpecificVarTypes;
  };

private:
  CodeCompletionExpr *CompletionExpr;
  DeclContext *DC;

  SmallVector<Result, 4> Results;

  void sawSolutionImpl(const constraints::Solution &solution) override;

public:
  /// \param DC The decl context in which the \p CompletionExpr occurs.
  ExprTypeCheckCompletionCallback(CodeCompletionExpr *CompletionExpr,
                                  DeclContext *DC)
      : CompletionExpr(CompletionExpr), DC(DC) {}

  /// \param CCLoc The location of the code completion token.
  void deliverResults(SourceLoc CCLoc,
                      ide::CodeCompletionContext &CompletionCtx,
                      CodeCompletionConsumer &Consumer);
};

} // end namespace ide
} // end namespace swift

#endif // SWIFT_IDE_EXPRCOMPLETION_H
