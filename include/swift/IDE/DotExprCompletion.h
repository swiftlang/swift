//===--- DotExprCodeCompletion.h ------------------------------------------===//
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

#ifndef SWIFT_IDE_DOTEXPRCODECOMPLETION_H
#define SWIFT_IDE_DOTEXPRCODECOMPLETION_H

#include "swift/IDE/CodeCompletionConsumer.h"
#include "swift/IDE/CodeCompletionContext.h"
#include "swift/Sema/CodeCompletionTypeChecking.h"

namespace swift {
namespace ide {

/// Used to collect and store information needed to perform member completion
/// (\c CompletionKind::DotExpr ) from the solutions formed during expression
/// type-checking.
class DotExprTypeCheckCompletionCallback : public TypeCheckCompletionCallback {
  struct Result {
    Type BaseTy;
    ValueDecl *BaseDecl;
    SmallVector<Type, 4> ExpectedTypes;
    bool ExpectsNonVoid;
    bool BaseIsStaticMetaType;
    bool IsImplicitSingleExpressionReturn;
  };

  CodeCompletionExpr *CompletionExpr;
  SmallVector<Result, 4> Results;
  llvm::DenseMap<std::pair<Type, Decl *>, size_t> BaseToSolutionIdx;

public:
  DotExprTypeCheckCompletionCallback(CodeCompletionExpr *CompletionExpr)
      : CompletionExpr(CompletionExpr) {}

  /// Typecheck the code completion expression in isolation, calling
  /// \c sawSolution for each solution formed.
  void fallbackTypeCheck(DeclContext *DC) override;

  void sawSolution(const constraints::Solution &solution) override;

  void deliverResults(Expr *BaseExpr, DeclContext *DC, SourceLoc DotLoc,
                      bool IsInSelector, CodeCompletionContext &CompletionCtx,
                      CodeCompletionConsumer &Consumer);
};

} // end namespace ide
} // end namespace swift

#endif // SWIFT_IDE_DOTEXPRCODECOMPLETION_H
