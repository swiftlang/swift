//===--- ConstraintSimplificationTests.cpp --------------------------------===//
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

#include "SemaFixture.h"
#include "swift/Sema/ConstraintSystem.h"

using namespace swift;
using namespace swift::unittest;
using namespace swift::constraints;

TEST_F(SemaTest, TestTrailingClosureMatchRecordingForIdenticalFunctions) {
  ConstraintSystem cs(DC, ConstraintSystemOptions());

  auto intType = getStdlibType("Int");
  auto floatType = getStdlibType("Float");

  auto func = FunctionType::get(
      {FunctionType::Param(intType), FunctionType::Param(intType)}, floatType);

  cs.addConstraint(
      ConstraintKind::ApplicableFunction, func, func,
      cs.getConstraintLocator({}, ConstraintLocator::ApplyFunction));

  SmallVector<Solution, 2> solutions;
  cs.solve(solutions);

  ASSERT_EQ(solutions.size(), (unsigned)1);

  const auto &solution = solutions.front();

  auto *locator = cs.getConstraintLocator({}, ConstraintLocator::ApplyArgument);
  auto choice = solution.argumentMatchingChoices.find(locator);
  ASSERT_TRUE(choice != solution.argumentMatchingChoices.end());
  MatchCallArgumentResult expected{
      TrailingClosureMatching::Forward, {{0}, {1}}, None};
  ASSERT_EQ(choice->second, expected);
}
