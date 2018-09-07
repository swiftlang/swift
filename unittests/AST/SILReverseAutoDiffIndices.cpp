//===--- SILReverseAutoDiffIndices.cpp - Tests SILReverseAutoDiffIndices --===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AutoDiff.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(SILReverseAutoDiffIndices, Equality) {
  std::array<unsigned, 0> empty;
  // Each example is distinct.
  SILReverseAutoDiffIndices examples[] = {
    {0, empty},
    {1, empty},
    {0, {0}},
    {0, {0, 1}},
    {0, {1}},
    {0, {1, 2}},
    {0, {100}},
    {0, {0, 100}},
    {0, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}}
  };
  size_t exampleCount = std::extent<decltype(examples)>::value;
  for (size_t i = 0; i < exampleCount; ++i) {
    auto example1 = examples[i];
    auto grownExample1 = example1;
    grownExample1.parameters.resize(grownExample1.parameters.size() + 1);

    // Make sure that the grown example is actually grown.
    EXPECT_TRUE(example1.parameters.size() < grownExample1.parameters.size());

    // Test that the example is equal to itself and to the grown version of
    // itself.
    EXPECT_TRUE(example1 == example1);
    EXPECT_TRUE(example1 == grownExample1);
    EXPECT_TRUE(grownExample1 == example1);

    // Test that the example is not equal to any of the others.
    for (size_t j = i + 1; j < exampleCount; ++j) {
      auto example2 = examples[j];
      auto grownExample2 = example2;
      grownExample2.parameters.resize(grownExample2.parameters.size() + 1);

      // Make sure that the grown example is actually grown.
      EXPECT_TRUE(example2.parameters.size() < grownExample2.parameters.size());

      EXPECT_FALSE(example1 == example2);
      EXPECT_FALSE(example2 == example1);

      EXPECT_FALSE(example1 == grownExample2);
      EXPECT_FALSE(grownExample2 == example1);

      EXPECT_FALSE(example2 == grownExample1);
      EXPECT_FALSE(grownExample1 == example2);
    }
  }
}
