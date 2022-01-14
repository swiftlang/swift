//===--- CodeCompletionResultType.cpp -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/CodeCompletionResultType.h"
#include "swift/Sema/IDETypeChecking.h"

using namespace swift;
using namespace ide;

static CodeCompletionResultTypeRelation
calculateTypeRelation(Type Ty, Type ExpectedTy, const DeclContext &DC) {
  if (Ty.isNull() || ExpectedTy.isNull() || Ty->is<ErrorType>() ||
      ExpectedTy->is<ErrorType>())
    return CodeCompletionResultTypeRelation::Unrelated;

  // Equality/Conversion of GenericTypeParameterType won't account for
  // requirements – ignore them
  if (!Ty->hasTypeParameter() && !ExpectedTy->hasTypeParameter()) {
    if (Ty->isEqual(ExpectedTy))
      return CodeCompletionResultTypeRelation::Identical;
    bool isAny = false;
    isAny |= ExpectedTy->isAny();
    isAny |= ExpectedTy->is<ArchetypeType>() &&
             !ExpectedTy->castTo<ArchetypeType>()->hasRequirements();

    if (!isAny && isConvertibleTo(Ty, ExpectedTy, /*openArchetypes=*/true,
                                  const_cast<DeclContext &>(DC)))
      return CodeCompletionResultTypeRelation::Convertible;
  }
  if (auto FT = Ty->getAs<AnyFunctionType>()) {
    if (FT->getResult()->isVoid())
      return CodeCompletionResultTypeRelation::Invalid;
  }
  return CodeCompletionResultTypeRelation::Unrelated;
}

static CodeCompletionResultTypeRelation
calculateMaxTypeRelation(Type Ty, const ExpectedTypeContext &typeContext,
                         const DeclContext &DC) {
  if (Ty->isVoid() && typeContext.requiresNonVoid())
    return CodeCompletionResultTypeRelation::Invalid;
  if (typeContext.empty())
    return CodeCompletionResultTypeRelation::Unknown;

  if (auto funcTy = Ty->getAs<AnyFunctionType>())
    Ty = funcTy->removeArgumentLabels(1);

  auto Result = CodeCompletionResultTypeRelation::Unrelated;
  for (auto expectedTy : typeContext.getPossibleTypes()) {
    // Do not use Void type context for a single-expression body, since the
    // implicit return does not constrain the expression.
    //
    //     { ... -> ()  in x } // x can be anything
    //
    // This behaves differently from explicit return, and from non-Void:
    //
    //     { ... -> Int in x }        // x must be Int
    //     { ... -> ()  in return x } // x must be Void
    if (typeContext.isImplicitSingleExpressionReturn() && expectedTy->isVoid())
      continue;

    Result = std::max(Result, calculateTypeRelation(Ty, expectedTy, DC));
  }

  // Map invalid -> unrelated when in a single-expression body, since the
  // input may be incomplete.
  if (typeContext.isImplicitSingleExpressionReturn() &&
      Result == CodeCompletionResultTypeRelation::Invalid)
    Result = CodeCompletionResultTypeRelation::Unrelated;
  
  return Result;
}

CodeCompletionResultTypeRelation
CodeCompletionResultType::calculateTypeRelation(
    const ExpectedTypeContext *TypeContext, const DeclContext *DC) const {
  if (isNotApplicable()) {
    return CodeCompletionResultTypeRelation::NotApplicable;
  }

  Type ResultTy = TypeAndFlags.getPointer();
  if (!TypeContext || !DC || !ResultTy) {
    return CodeCompletionResultTypeRelation::Unknown;
  }
  
  CodeCompletionResultTypeRelation Result =
      calculateMaxTypeRelation(ResultTy, *TypeContext, *DC);
  if (TypeAndFlags.getInt().contains(Flags::AlsoConsiderMetatype)) {
    Result =
        std::max(Result, calculateMaxTypeRelation(
                             MetatypeType::get(ResultTy, DC->getASTContext()),
                             *TypeContext, *DC));
  }
  return Result;
}
