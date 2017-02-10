//===--- TransformArrayRef.cpp --------------------------------------------===//
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

#include "swift/Basic/TransformArrayRef.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(TransformArrayRefTest, Empty) {
  auto transform = [](int i) -> float { return float(i); };
  std::function<float (int)> f(transform);
  std::vector<int> v1;
  auto EmptyArray = makeTransformArrayRef(llvm::ArrayRef<int>(v1), f);
  EXPECT_EQ(EmptyArray.empty(), v1.empty());
}

TEST(TransformArrayRefTest, Subscript) {
  auto transform = [](int i) -> float { return float(i); };
  std::function<float (int)> f(transform);
  std::vector<int> v1;

  v1.push_back(0);
  v1.push_back(2);
  v1.push_back(3);
  v1.push_back(100);
  v1.push_back(-5);
  v1.push_back(-30);

  auto Array = makeTransformArrayRef(llvm::ArrayRef<int>(v1), f);

  EXPECT_EQ(Array.size(), v1.size());
  for (unsigned i = 0, e = Array.size(); i != e; ++i) {
    EXPECT_EQ(Array[i], transform(v1[i]));
  }
}

TEST(TransformArrayRefTest, Iteration) {
  auto transform = [](int i) -> float { return float(i); };
  std::function<float (int)> f(transform);
  std::vector<int> v1;

  v1.push_back(0);
  v1.push_back(2);
  v1.push_back(3);
  v1.push_back(100);
  v1.push_back(-5);
  v1.push_back(-30);

  auto Array = makeTransformArrayRef(llvm::ArrayRef<int>(v1), f);

  auto VBegin = v1.begin();
  auto VIter = v1.begin();
  auto VEnd = v1.end();
  auto TBegin = Array.begin();
  auto TIter = Array.begin();
  auto TEnd = Array.end();

  // Forwards.
  while (VIter != VEnd) {
    EXPECT_NE(TIter, TEnd);
    EXPECT_EQ(transform(*VIter), *TIter);
    ++VIter;
    ++TIter;
  }

  // Backwards.
  while (VIter != VBegin) {
    EXPECT_NE(TIter, TBegin);

    --VIter;
    --TIter;

    EXPECT_EQ(transform(*VIter), *TIter);
  }
}

TEST(TransformArrayRefTest, Slicing) {
  auto transform = [](int i) -> float { return float(i); };
  std::function<float (int)> f(transform);
  std::vector<int> v1;

  v1.push_back(0);
  v1.push_back(2);
  v1.push_back(3);
  v1.push_back(100);
  v1.push_back(-5);
  v1.push_back(-30);

  auto Array = llvm::ArrayRef<int>(v1);
  auto TArray = makeTransformArrayRef(Array, f);

  EXPECT_EQ(Array.size(), TArray.size());
  while (!Array.empty()) {
    EXPECT_EQ(transform(*Array.begin()), *TArray.begin());
    Array = Array.slice(1);
    TArray = TArray.slice(1);
  }
}
