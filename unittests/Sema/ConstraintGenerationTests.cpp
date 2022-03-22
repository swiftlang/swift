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

using namespace swift;
using namespace swift::unittest;
using namespace swift::constraints;

TEST_F(SemaTest, TestImplicitForceCastConstraintGeneration) {
  ConstraintSystem cs(DC, ConstraintSystemOptions());

  auto *literal = IntegerLiteralExpr::createFromUnsigned(Context, 42);

  auto *cast = ForcedCheckedCastExpr::createImplicit(Context, literal,
                                                     getStdlibType("Double"));

  auto *expr = cs.generateConstraints(cast, DC, /*isInputExpression=*/true);

  ASSERT_NE(expr, nullptr);

  SmallVector<Solution, 2> solutions;
  cs.solve(solutions);

  ASSERT_EQ(solutions.size(), (unsigned)1);

  auto &solution = solutions.front();

  ASSERT_TRUE(solution.getResolvedType(literal)->isEqual(getStdlibType("Int")));
  ASSERT_TRUE(solution.getResolvedType(cast)->isEqual(getStdlibType("Double")));
}

TEST_F(SemaTest, TestImplicitCoercionConstraintGeneration) {
  ConstraintSystem cs(DC, ConstraintSystemOptions());

  auto *literal = IntegerLiteralExpr::createFromUnsigned(Context, 42);

  auto *cast = CoerceExpr::createImplicit(Context, literal,
                                          getStdlibType("Double"));

  auto *expr = cs.generateConstraints(cast, DC, /*isInputExpression=*/true);

  ASSERT_NE(expr, nullptr);

  SmallVector<Solution, 2> solutions;
  cs.solve(solutions);

  ASSERT_EQ(solutions.size(), (unsigned)1);

  auto &solution = solutions.front();

  ASSERT_TRUE(solution.getResolvedType(literal)->isEqual(getStdlibType("Double")));
  ASSERT_TRUE(solution.getResolvedType(cast)->isEqual(getStdlibType("Double")));
}

TEST_F(SemaTest, TestImplicitConditionalCastConstraintGeneration) {
  ConstraintSystem cs(DC, ConstraintSystemOptions());

  auto *literal = IntegerLiteralExpr::createFromUnsigned(Context, 42);

  auto *cast = ConditionalCheckedCastExpr::createImplicit(
      Context, literal, getStdlibType("Double"));

  auto *expr = cs.generateConstraints(cast, DC, /*isInputExpression=*/true);

  ASSERT_NE(expr, nullptr);

  SmallVector<Solution, 2> solutions;
  cs.solve(solutions);

  ASSERT_EQ(solutions.size(), (unsigned)1);

  auto &solution = solutions.front();

  ASSERT_TRUE(solution.getResolvedType(literal)->isEqual(getStdlibType("Int")));
  ASSERT_TRUE(solution.getResolvedType(cast)->isEqual(
      OptionalType::get(getStdlibType("Double"))));
}
