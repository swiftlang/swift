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

bool PostfixCompletionCallback::Result::canBeMergedWith(
    const Result &Other) const {
  return BaseTy->isEqual(Other.BaseTy) && BaseDecl == Other.BaseDecl;
}

void PostfixCompletionCallback::Result::merge(const Result &Other) {
  assert(canBeMergedWith(Other));
  // These properties should match if we are talking about the same BaseDecl.
  assert(IsBaseDeclUnapplied == Other.IsBaseDeclUnapplied);
  assert(BaseIsStaticMetaType == Other.BaseIsStaticMetaType);

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
      llvm::find_if(Results, [&Res](const Result &ExistingResult) {
        return ExistingResult.canBeMergedWith(Res);
      });
  if (ExistingRes != Results.end()) {
    ExistingRes->merge(Res);
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

  SolutionApplicationTarget completionTarget(fallbackExpr, fallbackDC,
                                             CTP_Unused, Type(),
                                             /*isDiscared=*/true);

  typeCheckForCodeCompletion(completionTarget, /*needsPrecheck*/ true,
                             [&](const Solution &S) { sawSolution(S); });
}

/// Returns \c true if \p Choice refers to a function that hasn't been called
/// yet.
static bool isUnappliedFunctionRef(const OverloadChoice &Choice) {
  if (!Choice.isDecl()) {
    return false;
  }
  switch (Choice.getFunctionRefKind()) {
  case FunctionRefKind::Unapplied:
    return true;
  case FunctionRefKind::SingleApply:
    if (auto BaseTy = Choice.getBaseType()) {
      // We consider curried member calls as unapplied. E.g.
      //   MyStruct.someInstanceFunc(theInstance)#^COMPLETE^#
      // is unapplied.
      return BaseTy->is<MetatypeType>() && !Choice.getDeclOrNull()->isStatic();
    } else {
      return false;
    }
  default:
    return false;
  }
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
  if (!ParentExpr)
    ExpectedTy = CS.getContextualType(CompletionExpr, /*forConstraint=*/false);

  auto *CalleeLocator = S.getCalleeLocator(Locator);
  ValueDecl *ReferencedDecl = nullptr;
  bool IsBaseDeclUnapplied = false;
  if (auto SelectedOverload = S.getOverloadChoiceIfAvailable(CalleeLocator)) {
    ReferencedDecl = SelectedOverload->choice.getDeclOrNull();
    IsBaseDeclUnapplied = isUnappliedFunctionRef(SelectedOverload->choice);
  }

  bool BaseIsStaticMetaType = S.isStaticallyDerivedMetatype(ParsedExpr);

  SmallVector<Type, 4> ExpectedTypes;
  if (ExpectedTy) {
    ExpectedTypes.push_back(ExpectedTy);
  }

  bool ExpectsNonVoid =
      ExpectedTy ? !ExpectedTy->isVoid()
                 : !ParentExpr && CS.getContextualTypePurpose(CompletionExpr) !=
                                      CTP_Unused;

  bool IsImplicitSingleExpressionReturn =
      isImplicitSingleExpressionReturn(CS, CompletionExpr);

  bool IsInAsyncContext = isContextAsync(S, DC);

  Result Res = {BaseTy,
                ReferencedDecl,
                IsBaseDeclUnapplied,
                BaseIsStaticMetaType,
                ExpectedTypes,
                ExpectsNonVoid,
                IsImplicitSingleExpressionReturn,
                IsInAsyncContext};

  addResult(Res);
}

/// Returns \c true if \p T is '_OptionalNilComparisonType'.
static bool isOptionalNilComparisonType(Type T) {
  if (!T) {
    return false;
  }
  auto *nominal = T->getAnyNominal();
  if (!nominal) {
    return false;
  }
  return (nominal->isStdlibDecl() &&
          nominal->getName() ==
              nominal->getASTContext().Id_OptionalNilComparisonType);
}

static DeclRefKind getDeclRefKindOfOperator(OperatorDecl *op) {
  switch (op->getKind()) {
  case DeclKind::PrefixOperator:
    return DeclRefKind::PrefixOperator;
  case DeclKind::PostfixOperator:
    return DeclRefKind::PostfixOperator;
  case DeclKind::InfixOperator:
    return DeclRefKind::BinaryOperator;
  default:
    llvm_unreachable("unexpected operator kind");
  }
}

/// Return type of \c getOperatorCompletionTypes.
struct OperatorResultTypes {
  /// If we are trying to complete a binary operator, the type the operator
  /// expects for the RHS. Null for postfix operators.
  Type RHSType;

  /// The type the operator returns when called.
  Type ResultType;

  bool operator==(const OperatorResultTypes &Other) const {
    return nullableTypesEqual(RHSType, Other.RHSType) &&
           nullableTypesEqual(ResultType, Other.ResultType);
  }
};

/// Builds a constriant system that tries applying the operator \p op on a LHS
/// of type \p LHSType. If that succeeds, returns the result type of the
/// operator call and (in case of binary operators) the expected type for the
/// RHS.
static SmallVector<OperatorResultTypes>
getOperatorCompletionTypes(DeclContext *DC, Type LHSType, OperatorDecl *Op) {
  ConstraintSystemOptions options;
  options |= ConstraintSystemFlags::SuppressDiagnostics;

  ConstraintSystem CS(DC, options);

  // The source loc of the generated expression doesn't matter.
  SourceLoc Loc;

  // We represent the LHS and RHS by CodeCompletionExprs because there's no
  // other better choice. rhs will have its type set in the constraint system
  // below and, in case of binary operators, rhs will be inspected for its type
  // when the constraint system has been solved.
  CodeCompletionExpr lhs(Loc);
  CodeCompletionExpr rhs(Loc);

  UnresolvedDeclRefExpr UDRE(DeclNameRef(Op->getName()),
                             getDeclRefKindOfOperator(Op), DeclNameLoc(Loc));
  DiagnosticTransaction IgnoreDiags(DC->getASTContext().Diags);
  Expr *OpExpr =
      resolveDeclRefExpr(&UDRE, DC, /*replaceInvalidRefsWithErrors=*/true);
  IgnoreDiags.abort();
  if (isa<ErrorExpr>(OpExpr)) {
    // If we couldn't resolve the operator (e.g. because there is only an
    // operator definition but no decls that implement it), we can't call the
    // operator.
    return {};
  }

  Expr *OpCallExpr;
  switch (Op->getKind()) {
  case DeclKind::PrefixOperator:
    // Don't insert prefix operators in postfix position.
    return {};
  case DeclKind::PostfixOperator:
    OpCallExpr = PostfixUnaryExpr::create(DC->getASTContext(), OpExpr, &lhs);
    break;
  case DeclKind::InfixOperator:
    OpCallExpr = BinaryExpr::create(DC->getASTContext(), &lhs, OpExpr, &rhs,
                                    /*implicit*/ true);
    break;
  default:
    llvm_unreachable("unexpected operator kind");
  }

  CS.preCheckExpression(OpCallExpr, DC, /*replaceInvalidRefsWithErrors=*/true,
                        /*leaveClosureBodyUnchecked=*/false);
  CS.generateConstraints(OpCallExpr, DC);

  CS.assignFixedType(CS.getType(&lhs)->getAs<TypeVariableType>(), LHSType);

  SmallVector<Solution, 4> Solutions;
  CS.solve(Solutions);

  SmallVector<OperatorResultTypes> Results;
  for (auto &S : Solutions) {
    Type RHSType;
    if (Op->getKind() == DeclKind::InfixOperator) {
      RHSType = getTypeForCompletion(S, &rhs);
    }
    Type ResultType = getTypeForCompletion(S, OpCallExpr);

    OperatorResultTypes ResultTypes = {RHSType, ResultType};
    if (llvm::is_contained(Results, ResultTypes)) {
      continue;
    }

    if (S.getFixedScore().Data[SK_ValueToOptional] > 0) {
      if (Op->getName().str() == "??" || isOptionalNilComparisonType(RHSType)) {
        // Don't suggest optional operators that need to demote the LHS to an
        // Optional to become applicable.
        continue;
      }
    }

    Results.push_back(ResultTypes);
  }

  return Results;
}

/// Adds applicable operator suggestions to \p Lookup.
static void addOperatorResults(Type LHSType, ArrayRef<OperatorDecl *> Operators,
                               DeclContext *DC, CompletionLookup &Lookup) {
  for (auto Op : Operators) {
    switch (Op->getKind()) {
    case DeclKind::PrefixOperator:
      break;
    case DeclKind::PostfixOperator:
      for (auto operatorType : getOperatorCompletionTypes(DC, LHSType, Op)) {
        Lookup.addPostfixOperatorCompletion(Op, operatorType.ResultType);
      }
      break;
    case DeclKind::InfixOperator:
      for (auto operatorType : getOperatorCompletionTypes(DC, LHSType, Op)) {
        Lookup.addInfixOperatorCompletion(Op, operatorType.ResultType,
                                          operatorType.RHSType);
      }
      break;
    default:
      llvm_unreachable("unexpected operator kind");
    }
  }
  if (LHSType->hasLValueType()) {
    Lookup.addAssignmentOperator(LHSType->getRValueType());
  }
  if (auto ValueT = LHSType->getRValueType()->getOptionalObjectType()) {
    Lookup.addPostfixBang(ValueT);
  }
}

void PostfixCompletionCallback::deliverResults(
    SourceLoc DotLoc, bool IsInSelector, bool IncludeOperators,
    bool HasLeadingSpace, CodeCompletionContext &CompletionCtx,
    CodeCompletionConsumer &Consumer) {
  ASTContext &Ctx = DC->getASTContext();
  CompletionLookup Lookup(CompletionCtx.getResultSink(), Ctx, DC,
                          &CompletionCtx);

  if (DotLoc.isValid()) {
    assert(!IncludeOperators && "We shouldn't be suggesting operators if we "
                                "are completing after a dot");
    Lookup.setHaveDot(DotLoc);
  }
  Lookup.setHaveLeadingSpace(HasLeadingSpace);

  Expr *BaseExpr = CompletionExpr->getBase();
  Lookup.setIsSuperRefExpr(isa<SuperRefExpr>(BaseExpr));

  if (auto *DRE = dyn_cast<DeclRefExpr>(BaseExpr))
    Lookup.setIsSelfRefExpr(DRE->getDecl()->getName() == Ctx.Id_self);

  if (isa<BindOptionalExpr>(BaseExpr) || isa<ForceValueExpr>(BaseExpr))
    Lookup.setIsUnwrappedOptional(true);

  if (IsInSelector) {
    Lookup.includeInstanceMembers();
    Lookup.setPreferFunctionReferencesToCalls();
  }
  SmallVector<OperatorDecl *, 0> Operators;
  if (IncludeOperators) {
    Lookup.collectOperators(Operators);
  }

  Lookup.shouldCheckForDuplicates(Results.size() > 1);
  for (auto &Result : Results) {
    Lookup.setCanCurrDeclContextHandleAsync(Result.IsInAsyncContext);
    Lookup.setIsStaticMetatype(Result.BaseIsStaticMetaType);
    Lookup.getPostfixKeywordCompletions(Result.BaseTy, BaseExpr);
    Lookup.setExpectedTypes(Result.ExpectedTypes,
                            Result.IsImplicitSingleExpressionReturn,
                            Result.ExpectsNonVoid);
    if (isDynamicLookup(Result.BaseTy))
      Lookup.setIsDynamicLookup();
    Lookup.getValueExprCompletions(Result.BaseTy, Result.BaseDecl,
                                   Result.IsBaseDeclUnapplied);

    if (IncludeOperators && !Result.BaseIsStaticMetaType) {
      addOperatorResults(Result.BaseTy, Operators, DC, Lookup);
    }
  }

  deliverCompletionResults(CompletionCtx, Lookup, DC, Consumer);
}
