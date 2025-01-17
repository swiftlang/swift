//===--- TypeCheckCompletionCallback.cpp ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Assertions.h"
#include "swift/IDE/TypeCheckCompletionCallback.h"
#include "swift/IDE/CompletionLookup.h"
#include "swift/Sema/CompletionContextFinder.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/IDETypeChecking.h"

using namespace swift;
using namespace swift::ide;
using namespace swift::constraints;

void TypeCheckCompletionCallback::fallbackTypeCheck(DeclContext *DC) {
  assert(!GotCallback);

  auto finder = CompletionContextFinder::forFallback(DC);
  if (!finder.hasCompletionExpr())
    return;

  auto fallback = finder.getFallbackCompletionExpr();
  if (!fallback || isa<AbstractClosureExpr>(fallback->DC)) {
    // If the expression is embedded in a closure, the constraint system tries
    // to retrieve that closure's type, which will fail since we won't have
    // generated any type variables for it. Thus, fallback type checking isn't
    // available in this case.
    return;
  }

  SyntacticElementTarget completionTarget(fallback->E, fallback->DC, CTP_Unused,
                                          Type(),
                                          /*isDiscared=*/true);
  typeCheckForCodeCompletion(completionTarget, /*needsPrecheck=*/true,
                             [&](const Solution &S) { sawSolution(S); });
}

// MARK: - Utility functions for subclasses of TypeCheckCompletionCallback

Type swift::ide::getTypeForCompletion(const constraints::Solution &S,
                                      ASTNode Node) {
  // Use the contextual type, unless it is still unresolved, in which case fall
  // back to getting the type from the expression.
  if (auto ContextualType = S.getContextualType(Node)) {
    if (!ContextualType->hasUnresolvedType())
      return ContextualType;
  }

  if (!S.hasType(Node)) {
    assert(false && "Expression wasn't type checked?");
    return nullptr;
  }

  Type Result;

  if (isExpr<CodeCompletionExpr>(Node)) {
    Result = S.simplifyTypeForCodeCompletion(S.getType(Node));
  } else {
    Result = S.getResolvedType(Node);
  }

  if (Result && Result->is<UnresolvedType>()) {
    Result = Type();
  }
  return Result;
}

/// If the code completion expression \p E occurs in a pattern matching
/// position, we have an AST that looks like this.
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
static VarDecl *getMatchVarIfInPatternMatch(Expr *E, const Solution &S) {
  if (auto EP = S.getExprPatternFor(E))
    return EP.get()->getMatchVar();

  // TODO: Once ExprPattern type-checking is fully moved into the solver,
  // the below can be deleted.
  auto &CS = S.getConstraintSystem();
  auto &Context = CS.getASTContext();

  auto *Binary = dyn_cast_or_null<BinaryExpr>(CS.getParentExpr(E));
  if (!Binary || !Binary->isImplicit() || Binary->getLHS() != E) {
    return nullptr;
  }

  auto CalledOperator = Binary->getFn();
  if (!isPatternMatchingOperator(CalledOperator)) {
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

Type swift::ide::getPatternMatchType(const constraints::Solution &S, Expr *E) {
  auto MatchVar = getMatchVarIfInPatternMatch(E, S);
  if (!MatchVar)
    return nullptr;

  if (S.hasType(MatchVar))
    return S.getResolvedType(MatchVar);

  // If the ExprPattern wasn't solved as part of the constraint system, it's
  // not part of the solution.
  // TODO: This can be removed once ExprPattern type-checking is fully part
  // of the constraint system.
  auto Ty = MatchVar->getTypeInContext();
  if (Ty->hasError())
    return Type();
  return Ty;
}

void swift::ide::getSolutionSpecificVarTypes(
    const constraints::Solution &S,
    llvm::SmallDenseMap<const VarDecl *, Type> &Result) {
  assert(Result.empty());
  for (auto NT : S.nodeTypes) {
    if (auto VD = dyn_cast_or_null<VarDecl>(NT.first.dyn_cast<Decl *>())) {
      Result[VD] = S.simplifyType(NT.second);
    }
  }
}

void WithSolutionSpecificVarTypesRAII::setInterfaceType(VarDecl *VD, Type Ty) {
  VD->getASTContext().evaluator.cacheOutput(InterfaceTypeRequest{VD},
                                            std::move(Ty));
}

bool swift::ide::isImpliedResult(const Solution &S, Expr *CompletionExpr) {
  return S.isImpliedResult(CompletionExpr).has_value();
}

bool swift::ide::isContextAsync(const constraints::Solution &S,
                                DeclContext *DC) {
  // We are in an async context if
  //  - the decl context is async
  if (S.getConstraintSystem().isAsynchronousContext(DC)) {
    return true;
  }

  //  - the decl context is sync but it's used in a context that expectes an
  //    async function. This happens if the code completion token is in a
  //    closure that doesn't contain any async calles. Thus the closure is
  //    type-checked as non-async, but it might get converted to an async
  //    closure based on its contextual type
  if (auto target = S.getTargetFor(dyn_cast<ClosureExpr>(DC))) {
    if (auto ContextTy = target->getClosureContextualType()) {
      if (auto ContextFuncTy =
              S.simplifyType(ContextTy)->getAs<AnyFunctionType>()) {
        return ContextFuncTy->isAsync();
      }
    }
  }

  //  - we did not record any information about async-ness of the context in the
  //    solution, but the type information recorded AST declares the context as
  //    async.
  return canDeclContextHandleAsync(DC);
}

bool swift::ide::nullableTypesEqual(Type LHS, Type RHS) {
  if (LHS.isNull() && RHS.isNull()) {
    return true;
  } else if (LHS.isNull() || RHS.isNull()) {
    // One type is null but the other is not.
    return false;
  } else {
    return LHS->isEqual(RHS);
  }
}
