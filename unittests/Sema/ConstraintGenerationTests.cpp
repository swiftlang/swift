//===--- ConstraintGenerationTests.cpp ------------------------------------===//
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

#include "SemaFixture.h"
#include "swift/AST/Expr.h"
#include "llvm/Support/Casting.h"

using namespace swift;
using namespace swift::unittest;
using namespace swift::constraints;

static Expr *applySolution(ConstraintSystem &cs, Expr *expr,
                           Solution &solution) {
  SolutionApplicationTarget target(expr, cs.DC, CTP_Unused, Type(),
                                   /*isDiscarded=*/false);
  auto result = cs.applySolution(solution, target);
  return result ? result->getAsExpr() : nullptr;
}

static Type getTypeOfCoercedExpr(ExplicitCastExpr *castExpr) {
  return castExpr->getSubExpr()->getType();
}

TEST_F(SemaTest, TestImplicitForceCastConstraintGeneration) {
  ConstraintSystem cs(DC, ConstraintSystemOptions());

  auto *literal = IntegerLiteralExpr::createFromUnsigned(Context, 42);

  auto *castExpr = ForcedCheckedCastExpr::createImplicit(Context, literal,
                                                         Context.TheAnyType);

  auto *expr = cs.generateConstraints(castExpr, DC, /*isInputExpression=*/true);

  ASSERT_NE(expr, nullptr);

  SmallVector<Solution, 2> solutions;
  cs.solve(solutions);

  ASSERT_EQ(solutions.size(), (unsigned)1);

  auto &solution = solutions.front();

  ASSERT_TRUE(solution.getResolvedType(literal)->isEqual(getStdlibType("Int")));
  ASSERT_TRUE(solution.getResolvedType(castExpr)->isEqual(Context.TheAnyType));

  auto *resultExpr = applySolution(cs, expr, solution);
  ASSERT_NE(resultExpr, nullptr);
  ASSERT_TRUE(getTypeOfCoercedExpr(cast<ForcedCheckedCastExpr>(resultExpr))
                  ->isEqual(getStdlibType("Int")));
}

TEST_F(SemaTest, TestImplicitCoercionConstraintGeneration) {
  ConstraintSystem cs(DC, ConstraintSystemOptions());

  auto *literal = IntegerLiteralExpr::createFromUnsigned(Context, 42);

  auto *castExpr = CoerceExpr::createImplicit(Context, literal,
                                              getStdlibType("Double"));

  auto *expr = cs.generateConstraints(castExpr, DC, /*isInputExpression=*/true);

  ASSERT_NE(expr, nullptr);

  SmallVector<Solution, 2> solutions;
  cs.solve(solutions);

  ASSERT_EQ(solutions.size(), (unsigned)1);

  auto &solution = solutions.front();

  ASSERT_TRUE(solution.getResolvedType(literal)->isEqual(getStdlibType("Double")));
  ASSERT_TRUE(
      solution.getResolvedType(castExpr)->isEqual(getStdlibType("Double")));

  auto *resultExpr = applySolution(cs, expr, solution);
  ASSERT_NE(resultExpr, nullptr);
  ASSERT_TRUE(getTypeOfCoercedExpr(cast<CoerceExpr>(resultExpr))
                  ->isEqual(getStdlibType("Double")));
}

TEST_F(SemaTest, TestImplicitConditionalCastConstraintGeneration) {
  ConstraintSystem cs(DC, ConstraintSystemOptions());

  auto *literal = IntegerLiteralExpr::createFromUnsigned(Context, 42);

  auto *castExpr = ConditionalCheckedCastExpr::createImplicit(
      Context, literal, getStdlibType("Double"));

  auto *expr = cs.generateConstraints(castExpr, DC, /*isInputExpression=*/true);

  ASSERT_NE(expr, nullptr);

  SmallVector<Solution, 2> solutions;
  cs.solve(solutions);

  ASSERT_EQ(solutions.size(), (unsigned)1);

  auto &solution = solutions.front();

  ASSERT_TRUE(solution.getResolvedType(literal)->isEqual(getStdlibType("Int")));
  ASSERT_TRUE(solution.getResolvedType(castExpr)->isEqual(
      OptionalType::get(getStdlibType("Double"))));

  auto *resultExpr = applySolution(cs, expr, solution);
  ASSERT_NE(resultExpr, nullptr);
  ASSERT_TRUE(getTypeOfCoercedExpr(cast<ConditionalCheckedCastExpr>(resultExpr))
              ->isEqual(getStdlibType("Int")));
}
