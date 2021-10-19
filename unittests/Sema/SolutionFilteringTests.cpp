//===--- SolutionFilteringTests.cpp ---------------------------------------===//
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

/// Make sure that minimization works as expected
/// and would remove solutions that score worse
/// than the best set even in ambiguity cases
/// (where there is more than one best solution).
TEST_F(SemaTest, TestFilteringBasedOnSolutionScore) {
  Score bestScore{};

  Score worseScore1{};
  Score worseScore2{};

  worseScore1.Data[SK_Unavailable] += 1;
  worseScore2.Data[SK_Fix] += 1;

  ConstraintSystem CS(DC, ConstraintSystemOptions());

  // Let's test worse solutions in different positions
  // in the list - beginning, middle, end.

  // To make "best" solutions ambiguous, let's use a single
  // type variable resolved into multiple distinct types.
  auto *ambiguousVar = CS.createTypeVariable(
      CS.getConstraintLocator({}), /*options=*/TVO_PrefersSubtypeBinding);

  auto formSolution = [&](Score score, Type type) -> Solution {
    Solution solution{CS, score};
    solution.typeBindings[ambiguousVar] = type;
    return solution;
  };

  // Nothing to remove
  {
    SmallVector<Solution, 4> solutions;

    solutions.push_back(formSolution(bestScore, getStdlibType("String")));
    solutions.push_back(formSolution(bestScore, getStdlibType("Int")));

    auto best = CS.findBestSolution(solutions, /*minimize=*/true);

    ASSERT_FALSE(best.hasValue());
    ASSERT_EQ(solutions.size(), 2u);
    ASSERT_EQ(solutions[0].getFixedScore(), bestScore);
    ASSERT_EQ(solutions[1].getFixedScore(), bestScore);
  }

  // Worst solutions are at the front
  {
    SmallVector<Solution, 4> solutions;

    solutions.push_back(formSolution(worseScore1, getStdlibType("Int")));
    solutions.push_back(formSolution(worseScore2, getStdlibType("Int")));

    solutions.push_back(formSolution(bestScore, getStdlibType("String")));
    solutions.push_back(formSolution(bestScore, getStdlibType("Int")));

    auto best = CS.findBestSolution(solutions, /*minimize=*/true);

    ASSERT_FALSE(best.hasValue());
    ASSERT_EQ(solutions.size(), 2u);
    ASSERT_EQ(solutions[0].getFixedScore(), bestScore);
    ASSERT_EQ(solutions[1].getFixedScore(), bestScore);
  }

  // Worst solutions are in the middle
  {
    SmallVector<Solution, 4> solutions;

    solutions.push_back(formSolution(bestScore, getStdlibType("String")));

    solutions.push_back(formSolution(worseScore1, getStdlibType("Int")));
    solutions.push_back(formSolution(worseScore2, getStdlibType("Int")));

    solutions.push_back(formSolution(bestScore, getStdlibType("Int")));

    auto best = CS.findBestSolution(solutions, /*minimize=*/true);

    ASSERT_FALSE(best.hasValue());
    ASSERT_EQ(solutions.size(), 2u);
    ASSERT_EQ(solutions[0].getFixedScore(), bestScore);
    ASSERT_EQ(solutions[1].getFixedScore(), bestScore);
  }

  // Worst solutions are at the end
  {
    SmallVector<Solution, 4> solutions;

    solutions.push_back(formSolution(bestScore, getStdlibType("String")));
    solutions.push_back(formSolution(bestScore, getStdlibType("Int")));

    solutions.push_back(formSolution(worseScore1, getStdlibType("Int")));
    solutions.push_back(formSolution(worseScore2, getStdlibType("Int")));

    auto best = CS.findBestSolution(solutions, /*minimize=*/true);

    ASSERT_FALSE(best.hasValue());
    ASSERT_EQ(solutions.size(), 2u);
    ASSERT_EQ(solutions[0].getFixedScore(), bestScore);
    ASSERT_EQ(solutions[1].getFixedScore(), bestScore);
  }

  // Worst solutions are spread out
  {
    SmallVector<Solution, 4> solutions;

    solutions.push_back(formSolution(worseScore1, getStdlibType("Int")));
    solutions.push_back(formSolution(bestScore, getStdlibType("String")));
    solutions.push_back(formSolution(worseScore1, getStdlibType("Int")));
    solutions.push_back(formSolution(worseScore2, getStdlibType("Int")));
    solutions.push_back(formSolution(bestScore, getStdlibType("Int")));
    solutions.push_back(formSolution(worseScore1, getStdlibType("Int")));
    solutions.push_back(formSolution(worseScore2, getStdlibType("Int")));

    auto best = CS.findBestSolution(solutions, /*minimize=*/true);

    ASSERT_FALSE(best.hasValue());
    ASSERT_EQ(solutions.size(), 2u);
    ASSERT_EQ(solutions[0].getFixedScore(), bestScore);
    ASSERT_EQ(solutions[1].getFixedScore(), bestScore);
  }
}
