//===--- UnresolvedMemberCompletion.h -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_UNRESOLVEDMEMBERCOMPLETION_H
#define SWIFT_IDE_UNRESOLVEDMEMBERCOMPLETION_H

#include "swift/IDE/CodeCompletionConsumer.h"
#include "swift/IDE/CodeCompletionContext.h"
#include "swift/Sema/CodeCompletionTypeChecking.h"

namespace swift {
namespace ide {

/// Used to collect and store information needed to perform unresolved member
/// completion (\c CompletionKind::UnresolvedMember ) from the solutions
/// formed during expression type-checking.
class UnresolvedMemberTypeCheckCompletionCallback
    : public TypeCheckCompletionCallback {
public:
  struct ExprResult {
    Type ExpectedTy;
    bool IsImplicitSingleExpressionReturn;
  };

private:
  CodeCompletionExpr *CompletionExpr;
  SmallVector<ExprResult, 4> ExprResults;
  SmallVector<Type, 1> EnumPatternTypes;
  bool GotCallback = false;

public:
  UnresolvedMemberTypeCheckCompletionCallback(
      CodeCompletionExpr *CompletionExpr)
      : CompletionExpr(CompletionExpr) {}

  ArrayRef<ExprResult> getExprResults() const { return ExprResults; }

  /// If we are completing in a pattern matching position, the types of all
  /// enums for whose cases are valid as an \c EnumElementPattern.
  ArrayRef<Type> getEnumPatternTypes() const { return EnumPatternTypes; }

  /// True if at least one solution was passed via the \c sawSolution
  /// callback.
  bool gotCallback() const { return GotCallback; }

  /// Typecheck the code completion expression in its outermost expression
  /// context, calling \c sawSolution for each solution formed.
  void fallbackTypeCheck(DeclContext *DC);

  void sawSolution(const constraints::Solution &solution) override;
};

void deliverUnresolvedMemberResults(
    ArrayRef<UnresolvedMemberTypeCheckCompletionCallback::ExprResult> Results,
    ArrayRef<Type> EnumPatternTypes, DeclContext *DC, SourceLoc DotLoc,
    ide::CodeCompletionContext &CompletionCtx,
    CodeCompletionConsumer &Consumer);

} // end namespace ide
} // end namespace swift

#endif // SWIFT_IDE_UNRESOLVEDMEMBERCOMPLETION_H
