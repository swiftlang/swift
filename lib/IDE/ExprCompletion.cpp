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

void ExprTypeCheckCompletionCallback::sawSolutionImpl(
    const constraints::Solution &S) {
  auto &CS = S.getConstraintSystem();

  Type ExpectedTy = getTypeForCompletion(S, CompletionExpr);

  bool ImplicitReturn = isImplicitSingleExpressionReturn(CS, CompletionExpr);

  bool IsAsync = isContextAsync(S, DC);

  llvm::SmallDenseMap<const VarDecl *, Type> SolutionSpecificVarTypes;
  for (auto NT : S.nodeTypes) {
    if (auto VD = dyn_cast_or_null<VarDecl>(NT.first.dyn_cast<Decl *>())) {
      SolutionSpecificVarTypes[VD] = S.simplifyType(NT.second);
    }
  }

  Results.push_back(
      {ExpectedTy, ImplicitReturn, IsAsync, SolutionSpecificVarTypes});
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
