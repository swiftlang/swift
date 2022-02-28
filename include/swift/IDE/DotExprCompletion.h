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

  DeclContext *DC;
  CodeCompletionExpr *CompletionExpr;
  SmallVector<Result, 4> Results;
  llvm::DenseMap<std::pair<Type, Decl *>, size_t> BaseToSolutionIdx;
  bool GotCallback = false;

public:
  DotExprTypeCheckCompletionCallback(DeclContext *DC,
                                     CodeCompletionExpr *CompletionExpr)
      : DC(DC), CompletionExpr(CompletionExpr) {}

  /// True if at least one solution was passed via the \c sawSolution
  /// callback.
  bool gotCallback() const { return GotCallback; }

  /// Typecheck the code completion expression in isolation, calling
  /// \c sawSolution for each solution formed.
  void fallbackTypeCheck();

  void sawSolution(const constraints::Solution &solution) override;

  void deliverResults(Expr *BaseExpr, DeclContext *DC, SourceLoc DotLoc,
                      bool IsInSelector, CodeCompletionContext &CompletionCtx,
                      CodeCompletionConsumer &Consumer);
};

} // end namespace ide
} // end namespace swift

#endif // SWIFT_IDE_DOTEXPRCODECOMPLETION_H
