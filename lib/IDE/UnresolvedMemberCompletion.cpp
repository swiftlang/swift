//===--- UnresolvedMemberCodeCompletion.cpp -------------------------------===//
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

#include "swift/IDE/UnresolvedMemberCompletion.h"
#include "swift/IDE/CodeCompletion.h"
#include "swift/IDE/CompletionLookup.h"
#include "swift/Sema/CompletionContextFinder.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/IDETypeChecking.h"

using namespace swift;
using namespace swift::constraints;
using namespace swift::ide;

/// If the code completion variable occurs in a pattern matching position, we
/// have an AST that looks like this.
/// \code
/// (binary_expr implicit type='$T3'
///   (overloaded_decl_ref_expr function_ref=compound decls=[
///     Swift.(file).~=,
///     Swift.(file).Optional extension.~=])
///   (argument_list implicit
///     (argument
///       (code_completion_expr implicit type='$T1'))
///     (argument
///       (declref_expr implicit decl=swift_ide_test.(file).foo(x:).$match))))
/// \endcode
/// If the code completion expression occurs in such an AST, return the
/// declaration of the \c $match variable, otherwise return \c nullptr.
static VarDecl *getMatchVarIfInPatternMatch(CodeCompletionExpr *CompletionExpr,
                                            ConstraintSystem &CS) {
  auto &Context = CS.getASTContext();

  auto *Binary = dyn_cast_or_null<BinaryExpr>(CS.getParentExpr(CompletionExpr));
  if (!Binary || !Binary->isImplicit() || Binary->getLHS() != CompletionExpr) {
    return nullptr;
  }

  auto CalledOperator = Binary->getFn();
  if (!CalledOperator || !CalledOperator->isImplicit()) {
    return nullptr;
  }
  // The reference to the ~= operator might be an OverloadedDeclRefExpr or a
  // DeclRefExpr, depending on how many ~= operators are viable.
  if (auto Overloaded =
          dyn_cast_or_null<OverloadedDeclRefExpr>(CalledOperator)) {
    if (!llvm::all_of(Overloaded->getDecls(), [&Context](ValueDecl *D) {
          return D->getBaseName() == Context.Id_MatchOperator;
        })) {
      return nullptr;
    }
  } else if (auto Ref = dyn_cast_or_null<DeclRefExpr>(CalledOperator)) {
    if (Ref->getDecl()->getBaseName() != Context.Id_MatchOperator) {
      return nullptr;
    }
  } else {
    return nullptr;
  }

  auto MatchArg = dyn_cast_or_null<DeclRefExpr>(Binary->getRHS());
  if (!MatchArg || !MatchArg->isImplicit()) {
    return nullptr;
  }

  auto MatchVar = MatchArg->getDecl();
  if (MatchVar && MatchVar->isImplicit() &&
      MatchVar->getBaseName() == Context.Id_PatternMatchVar) {
    return dyn_cast<VarDecl>(MatchVar);
  } else {
    return nullptr;
  }
}

void UnresolvedMemberTypeCheckCompletionCallback::sawSolution(
    const constraints::Solution &S) {
  TypeCheckCompletionCallback::sawSolution(S);

  auto &CS = S.getConstraintSystem();
  Type ExpectedTy = getTypeForCompletion(S, CompletionExpr);
  // If the type couldn't be determined (e.g. because there isn't any context
  // to derive it from), let's not attempt to do a lookup since it wouldn't
  // produce any useful results anyway.
  if (ExpectedTy && !ExpectedTy->is<UnresolvedType>()) {
    // If ExpectedTy is a duplicate of any other result, ignore this solution.
    if (!llvm::any_of(ExprResults, [&](const ExprResult &R) {
          return R.ExpectedTy->isEqual(ExpectedTy);
        })) {
      bool SingleExprBody =
          isImplicitSingleExpressionReturn(CS, CompletionExpr);
      ExprResults.push_back({ExpectedTy, SingleExprBody});
    }
  }

  if (auto MatchVar = getMatchVarIfInPatternMatch(CompletionExpr, CS)) {
    Type MatchVarType;
    // If the MatchVar has an explicit type, it's not part of the solution. But
    // we can look it up in the constraint system directly.
    if (auto T = S.getConstraintSystem().getVarType(MatchVar)) {
      MatchVarType = T;
    } else {
      MatchVarType = S.getResolvedType(MatchVar);
    }
    if (MatchVarType && !MatchVarType->is<UnresolvedType>()) {
      if (!llvm::any_of(EnumPatternTypes, [&](const Type &R) {
            return R->isEqual(MatchVarType);
          })) {
        EnumPatternTypes.push_back(MatchVarType);
      }
    }
  }
}

void UnresolvedMemberTypeCheckCompletionCallback::deliverResults(
    DeclContext *DC, SourceLoc DotLoc,
    ide::CodeCompletionContext &CompletionCtx,
    CodeCompletionConsumer &Consumer) {
  ASTContext &Ctx = DC->getASTContext();
  CompletionLookup Lookup(CompletionCtx.getResultSink(), Ctx, DC,
                          &CompletionCtx);

  assert(DotLoc.isValid());
  Lookup.setHaveDot(DotLoc);
  Lookup.shouldCheckForDuplicates(ExprResults.size() + EnumPatternTypes.size() >
                                  1);

  // Get the canonical versions of the top-level types
  SmallPtrSet<CanType, 4> originalTypes;
  for (auto &Result : ExprResults)
    originalTypes.insert(Result.ExpectedTy->getCanonicalType());

  for (auto &Result : ExprResults) {
    Lookup.setExpectedTypes({Result.ExpectedTy},
                            Result.IsImplicitSingleExpressionReturn,
                            /*expectsNonVoid*/ true);
    Lookup.setIdealExpectedType(Result.ExpectedTy);

    // For optional types, also get members of the unwrapped type if it's not
    // already equivalent to one of the top-level types. Handling it via the top
    // level type and not here ensures we give the correct type relation
    // (identical, rather than convertible).
    if (Result.ExpectedTy->getOptionalObjectType()) {
      Type Unwrapped = Result.ExpectedTy->lookThroughAllOptionalTypes();
      if (originalTypes.insert(Unwrapped->getCanonicalType()).second)
        Lookup.getUnresolvedMemberCompletions(Unwrapped);
    }
    Lookup.getUnresolvedMemberCompletions(Result.ExpectedTy);
  }

  // Offer completions when interpreting the pattern match as an
  // EnumElementPattern.
  for (auto &Ty : EnumPatternTypes) {
    Lookup.setExpectedTypes({Ty}, /*IsImplicitSingleExpressionReturn=*/false,
                            /*expectsNonVoid=*/true);
    Lookup.setIdealExpectedType(Ty);

    // We can pattern match MyEnum against Optional<MyEnum>
    if (Ty->getOptionalObjectType()) {
      Type Unwrapped = Ty->lookThroughAllOptionalTypes();
      Lookup.getEnumElementPatternCompletions(Unwrapped);
    }

    Lookup.getEnumElementPatternCompletions(Ty);
  }

  deliverCompletionResults(CompletionCtx, Lookup, DC, Consumer);
}
