//===--- ExprCompletion.cpp -----------------------------------------------===//
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

#include "swift/IDE/ExprCompletion.h"
#include "swift/IDE/CodeCompletion.h"
#include "swift/IDE/CompletionLookup.h"
#include "swift/Sema/ConstraintSystem.h"

using namespace swift;
using namespace swift::ide;
using namespace swift::constraints;

void ExprTypeCheckCompletionCallback::sawSolution(
    const constraints::Solution &S) {
  TypeCheckCompletionCallback::sawSolution(S);

  auto &CS = S.getConstraintSystem();

  // Prefer to get the expected type as the completion expression's contextual
  // type. If that fails (because there is no explicit contextual type spelled
  // out in the source), the code completion expression will have been
  // type-checked to its expected contextual type.
  Type ExpectedTy =
      CS.getContextualType(CompletionExpr, /*forConstraint=*/false);
  if (!ExpectedTy) {
    ExpectedTy = S.getResolvedType(CompletionExpr);
  }
  if (ExpectedTy->hasUnresolvedType()) {
    ExpectedTy = Type();
  }

  bool ImplicitReturn = isImplicitSingleExpressionReturn(CS, CompletionExpr);

  // We are in an async context if
  //  - the decl context is async or
  //  - the decl context is sync but it's used in a context that expectes an
  //    async function. This happens if the code completion token is in a
  //    closure that doesn't contain any async calles. Thus the closure is
  //    type-checked as non-async, but it might get converted to an async
  //    closure based on its contextual type.
  bool isAsync = CS.isAsynchronousContext(DC);
  if (!isAsync) {
    auto target = S.solutionApplicationTargets.find(dyn_cast<ClosureExpr>(DC));
    if (target != S.solutionApplicationTargets.end()) {
      if (auto ContextTy = target->second.getClosureContextualType()) {
        if (auto ContextFuncTy =
                S.simplifyType(ContextTy)->getAs<AnyFunctionType>()) {
          isAsync = ContextFuncTy->isAsync();
        }
      }
    }
  }

  llvm::SmallDenseMap<const VarDecl *, Type> SolutionSpecificVarTypes;
  for (auto NT : S.nodeTypes) {
    if (auto VD = dyn_cast_or_null<VarDecl>(NT.first.dyn_cast<Decl *>())) {
      SolutionSpecificVarTypes[VD] = S.simplifyType(NT.second);
    }
  }

  Results.push_back(
      {ExpectedTy, ImplicitReturn, isAsync, SolutionSpecificVarTypes});
}

void ExprTypeCheckCompletionCallback::deliverResults(
    SourceLoc CCLoc, ide::CodeCompletionContext &CompletionCtx,
    CodeCompletionConsumer &Consumer) {
  ASTContext &Ctx = DC->getASTContext();
  CompletionLookup Lookup(CompletionCtx.getResultSink(), Ctx, DC,
                          &CompletionCtx);

  for (auto &Result : Results) {
    Lookup.setExpectedTypes(Result.ExpectedType,
                            Result.IsImplicitSingleExpressionReturn);
    Lookup.setCanCurrDeclContextHandleAsync(Result.IsInAsyncContext);
    Lookup.setSolutionSpecificVarTypes(Result.SolutionSpecificVarTypes);

    Lookup.getValueCompletionsInDeclContext(CCLoc);
    Lookup.getSelfTypeCompletionInDeclContext(CCLoc, /*isForDeclResult=*/false);
  }

  deliverCompletionResults(CompletionCtx, Lookup, DC, Consumer);
}
