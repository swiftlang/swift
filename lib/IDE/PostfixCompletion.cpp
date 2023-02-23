//===--- DotExprCodeCompletion.cpp ----------------------------------------===//
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

#include "swift/IDE/PostfixCompletion.h"
#include "swift/IDE/CodeCompletion.h"
#include "swift/IDE/CompletionLookup.h"
#include "swift/Sema/CompletionContextFinder.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/IDETypeChecking.h"

using namespace swift;
using namespace swift::constraints;
using namespace swift::ide;

bool PostfixCompletionCallback::Result::canBeMergedWith(const Result &Other,
                                                        DeclContext &DC) const {
  if (BaseDecl != Other.BaseDecl) {
    return false;
  }
  if (!BaseTy->isEqual(Other.BaseTy) &&
      !isConvertibleTo(BaseTy, Other.BaseTy, /*openArchetypes=*/true, DC) &&
      !isConvertibleTo(Other.BaseTy, BaseTy, /*openArchetypes=*/true, DC)) {
    return false;
  }
  return true;
}

void PostfixCompletionCallback::Result::merge(const Result &Other,
                                              DeclContext &DC) {
  assert(canBeMergedWith(Other, DC));
  // These properties should match if we are talking about the same BaseDecl.
  assert(BaseIsStaticMetaType == Other.BaseIsStaticMetaType);

  if (!BaseTy->isEqual(Other.BaseTy) &&
      isConvertibleTo(Other.BaseTy, BaseTy, /*openArchetypes=*/true, DC)) {
    // Pick the more specific base type as it will produce more solutions.
    BaseTy = Other.BaseTy;
  }

  // There could be multiple results that have different actor isolations if the
  // closure is an argument to a function that has multiple overloads with
  // different isolations for the closure. Producing multiple results for these
  // is usually not very enlightning. For now, we just pick the first actor
  // isolation that we find. This is good enough in practice.
  // What we should really do is probably merge these two actor isolations and
  // pick the weakest isolation for each closure.

  for (auto &OtherExpectedTy : Other.ExpectedTypes) {
    auto IsEqual = [&](Type Ty) { return Ty->isEqual(OtherExpectedTy); };
    if (llvm::any_of(ExpectedTypes, IsEqual)) {
      // We already know if this expected type
      continue;
    }
    ExpectedTypes.push_back(OtherExpectedTy);
  }
  ExpectsNonVoid &= Other.ExpectsNonVoid;
  IsImplicitSingleExpressionReturn |= Other.IsImplicitSingleExpressionReturn;
  IsInAsyncContext |= Other.IsInAsyncContext;
}

void PostfixCompletionCallback::addResult(const Result &Res) {
  auto ExistingRes =
      llvm::find_if(Results, [&Res, DC = DC](const Result &ExistingResult) {
        return ExistingResult.canBeMergedWith(Res, *DC);
      });
  if (ExistingRes != Results.end()) {
    ExistingRes->merge(Res, *DC);
  } else {
    Results.push_back(Res);
  }
}

void PostfixCompletionCallback::fallbackTypeCheck(DeclContext *DC) {
  assert(!gotCallback());

  // Default to checking the completion expression in isolation.
  Expr *fallbackExpr = CompletionExpr;
  DeclContext *fallbackDC = DC;

  CompletionContextFinder finder(DC);
  if (finder.hasCompletionExpr()) {
    if (auto fallback = finder.getFallbackCompletionExpr()) {
      fallbackExpr = fallback->E;
      fallbackDC = fallback->DC;
    }
  }

  SyntacticElementTarget completionTarget(fallbackExpr, fallbackDC, CTP_Unused,
                                          Type(),
                                          /*isDiscared=*/true);

  typeCheckForCodeCompletion(completionTarget, /*needsPrecheck*/ true,
                             [&](const Solution &S) { sawSolution(S); });
}

static ClosureActorIsolation
getClosureActorIsolation(const Solution &S, AbstractClosureExpr *ACE) {
  auto getType = [&S](Expr *E) -> Type {
    // Prefer the contextual type of the closure because it might be 'weaker'
    // than the type determined for the closure by the constraints system. E.g.
    // the contextual type might have a global actor attribute but because no
    // methods from that global actor are called in the closure, the closure has
    // a non-actor type.
    if (auto target = S.getTargetFor(dyn_cast<ClosureExpr>(E))) {
      if (auto Ty = target->getClosureContextualType())
        return Ty;
    }
    if (!S.hasType(E)) {
      return Type();
    }
    return getTypeForCompletion(S, E);
  };
  auto getClosureActorIsolationThunk = [&S](AbstractClosureExpr *ACE) {
    return getClosureActorIsolation(S, ACE);
  };
  return determineClosureActorIsolation(ACE, getType,
                                        getClosureActorIsolationThunk);
}

void PostfixCompletionCallback::sawSolutionImpl(
    const constraints::Solution &S) {
  auto &CS = S.getConstraintSystem();
  auto *ParsedExpr = CompletionExpr->getBase();
  auto *SemanticExpr = ParsedExpr->getSemanticsProvidingExpr();

  auto BaseTy = getTypeForCompletion(S, ParsedExpr);
  // If base type couldn't be determined (e.g. because base expression
  // is an invalid reference), let's not attempt to do a lookup since
  // it wouldn't produce any useful results anyway.
  if (!BaseTy)
    return;

  auto *Locator = CS.getConstraintLocator(SemanticExpr);
  Type ExpectedTy = getTypeForCompletion(S, CompletionExpr);
  Expr *ParentExpr = CS.getParentExpr(CompletionExpr);
  if (!ParentExpr && !ExpectedTy)
    ExpectedTy = CS.getContextualType(CompletionExpr, /*forConstraint=*/false);

  auto *CalleeLocator = S.getCalleeLocator(Locator);
  ValueDecl *ReferencedDecl = nullptr;
  if (auto SelectedOverload = S.getOverloadChoiceIfAvailable(CalleeLocator))
    ReferencedDecl = SelectedOverload->choice.getDeclOrNull();

  llvm::DenseMap<AbstractClosureExpr *, ClosureActorIsolation>
      ClosureActorIsolations;
  bool IsAsync = isContextAsync(S, DC);
  for (auto SAT : S.targets) {
    if (auto ACE = getAsExpr<AbstractClosureExpr>(SAT.second.getAsASTNode())) {
      ClosureActorIsolations[ACE] = getClosureActorIsolation(S, ACE);
    }
  }

  bool BaseIsStaticMetaType = S.isStaticallyDerivedMetatype(ParsedExpr);

  SmallVector<Type, 4> ExpectedTypes;
  if (ExpectedTy) {
    ExpectedTypes.push_back(ExpectedTy);
  }

  bool ExpectsNonVoid = false;
  ExpectsNonVoid |= ExpectedTy && !ExpectedTy->isVoid();
  ExpectsNonVoid |=
      !ParentExpr && CS.getContextualTypePurpose(CompletionExpr) != CTP_Unused;

  for (auto SAT : S.targets) {
    if (ExpectsNonVoid) {
      // ExpectsNonVoid is already set. No need to iterate further.
      break;
    }
    if (SAT.second.getAsExpr() == CompletionExpr) {
      ExpectsNonVoid |= SAT.second.getExprContextualTypePurpose() != CTP_Unused;
    }
  }

  bool IsImplicitSingleExpressionReturn =
      isImplicitSingleExpressionReturn(CS, CompletionExpr);

  Result Res = {
      BaseTy,
      ReferencedDecl,
      BaseIsStaticMetaType,
      ExpectedTypes,
      ExpectsNonVoid,
      IsImplicitSingleExpressionReturn,
      IsAsync,
      ClosureActorIsolations
  };

  addResult(Res);
}

void PostfixCompletionCallback::deliverResults(
    Expr *BaseExpr, DeclContext *DC, SourceLoc DotLoc, bool IsInSelector,
    CodeCompletionContext &CompletionCtx, CodeCompletionConsumer &Consumer) {
  ASTContext &Ctx = DC->getASTContext();
  CompletionLookup Lookup(CompletionCtx.getResultSink(), Ctx, DC,
                          &CompletionCtx);

  if (DotLoc.isValid())
    Lookup.setHaveDot(DotLoc);

  Lookup.setIsSuperRefExpr(isa<SuperRefExpr>(BaseExpr));

  if (auto *DRE = dyn_cast<DeclRefExpr>(BaseExpr))
    Lookup.setIsSelfRefExpr(DRE->getDecl()->getName() == Ctx.Id_self);

  if (isa<BindOptionalExpr>(BaseExpr) || isa<ForceValueExpr>(BaseExpr))
    Lookup.setIsUnwrappedOptional(true);

  if (IsInSelector) {
    Lookup.includeInstanceMembers();
    Lookup.setPreferFunctionReferencesToCalls();
  }

  Lookup.shouldCheckForDuplicates(Results.size() > 1);
  for (auto &Result : Results) {
    Lookup.setCanCurrDeclContextHandleAsync(Result.IsInAsyncContext);
    Lookup.setClosureActorIsolations(Result.ClosureActorIsolations);
    Lookup.setIsStaticMetatype(Result.BaseIsStaticMetaType);
    Lookup.getPostfixKeywordCompletions(Result.BaseTy, BaseExpr);
    Lookup.setExpectedTypes(Result.ExpectedTypes,
                            Result.IsImplicitSingleExpressionReturn,
                            Result.ExpectsNonVoid);
    if (isDynamicLookup(Result.BaseTy))
      Lookup.setIsDynamicLookup();
    Lookup.getValueExprCompletions(Result.BaseTy, Result.BaseDecl);
  }

  deliverCompletionResults(CompletionCtx, Lookup, DC, Consumer);
}
