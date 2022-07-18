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

static bool solutionSpecificVarTypesEqual(
    const llvm::SmallDenseMap<const VarDecl *, Type> &LHS,
    const llvm::SmallDenseMap<const VarDecl *, Type> &RHS) {
  if (LHS.size() != RHS.size()) {
    return false;
  }
  for (auto LHSEntry : LHS) {
    auto RHSEntry = RHS.find(LHSEntry.first);
    if (RHSEntry == RHS.end()) {
      // Entry of the LHS doesn't exist in RHS
      return false;
    } else if (!nullableTypesEqual(LHSEntry.second, RHSEntry->second)) {
      return false;
    }
  }
  return true;
}

bool ExprTypeCheckCompletionCallback::Result::operator==(
    const Result &Other) const {
  return IsImplicitSingleExpressionReturn ==
             Other.IsImplicitSingleExpressionReturn &&
         IsInAsyncContext == Other.IsInAsyncContext &&
         nullableTypesEqual(UnresolvedMemberBaseType,
                            Other.UnresolvedMemberBaseType) &&
         solutionSpecificVarTypesEqual(SolutionSpecificVarTypes,
                                       Other.SolutionSpecificVarTypes);
}

void ExprTypeCheckCompletionCallback::addExpectedType(Type ExpectedType) {
  auto IsEqual = [&ExpectedType](Type Other) {
    return nullableTypesEqual(ExpectedType, Other);
  };
  if (llvm::any_of(ExpectedTypes, IsEqual)) {
    return;
  }
  ExpectedTypes.push_back(ExpectedType);
}

void ExprTypeCheckCompletionCallback::addResult(
    bool IsImplicitSingleExpressionReturn, bool IsInAsyncContext,
    Type UnresolvedMemberBaseType,
    llvm::SmallDenseMap<const VarDecl *, Type> SolutionSpecificVarTypes) {
  if (!AddUnresolvedMemberCompletions) {
    UnresolvedMemberBaseType = Type();
  }
  Result NewResult = {IsImplicitSingleExpressionReturn, IsInAsyncContext,
                      UnresolvedMemberBaseType, SolutionSpecificVarTypes};
  if (llvm::is_contained(Results, NewResult)) {
    return;
  }
  Results.push_back(NewResult);
}

void ExprTypeCheckCompletionCallback::sawSolutionImpl(
    const constraints::Solution &S) {
  auto &CS = S.getConstraintSystem();

  Type ExpectedTy = getTypeForCompletion(S, CompletionExpr);

  bool ImplicitReturn = isImplicitSingleExpressionReturn(CS, CompletionExpr);

  bool IsAsync = isContextAsync(S, DC);

  llvm::SmallDenseMap<const VarDecl *, Type> SolutionSpecificVarTypes;
  getSolutionSpecificVarTypes(S, SolutionSpecificVarTypes);

  addResult(ImplicitReturn, IsAsync, ExpectedTy, SolutionSpecificVarTypes);
  addExpectedType(ExpectedTy);

  if (auto PatternMatchType = getPatternMatchType(S, CompletionExpr)) {
    addResult(ImplicitReturn, IsAsync, PatternMatchType,
              SolutionSpecificVarTypes);
    addExpectedType(PatternMatchType);
  }
}

void ExprTypeCheckCompletionCallback::deliverResults(
    SourceLoc CCLoc, ide::CodeCompletionContext &CompletionCtx,
    CodeCompletionConsumer &Consumer) {
  ASTContext &Ctx = DC->getASTContext();
  CompletionLookup Lookup(CompletionCtx.getResultSink(), Ctx, DC,
                          &CompletionCtx);
  Lookup.shouldCheckForDuplicates(Results.size() > 1);

  for (auto &Result : Results) {
    Lookup.setExpectedTypes(ExpectedTypes,
                            Result.IsImplicitSingleExpressionReturn);
    Lookup.setCanCurrDeclContextHandleAsync(Result.IsInAsyncContext);
    Lookup.setSolutionSpecificVarTypes(Result.SolutionSpecificVarTypes);

    Lookup.getValueCompletionsInDeclContext(CCLoc);
    Lookup.getSelfTypeCompletionInDeclContext(CCLoc, /*isForDeclResult=*/false);
    if (Result.UnresolvedMemberBaseType) {
      Lookup.getUnresolvedMemberCompletions(Result.UnresolvedMemberBaseType);
    }
  }

  deliverCompletionResults(CompletionCtx, Lookup, DC, Consumer);
}
