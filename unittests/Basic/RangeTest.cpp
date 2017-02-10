//===--- RangeTest.cpp ----------------------------------------------------===//
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

#include "swift/Basic/Range.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Basic/ValueEnumerator.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(Range, basic) {
  unsigned start = 0;
  unsigned end = 50;
  unsigned expected_i = start;
  bool sawEndMinusOne = false;
  for (unsigned i : range(start, end)) {
    EXPECT_GE(i, start);
    EXPECT_LT(i, end);
    EXPECT_EQ(expected_i, i);
    ++expected_i;

    sawEndMinusOne |= (i == (end - 1));
  }
  EXPECT_TRUE(sawEndMinusOne);
}

TEST(ReverseRange, basic) {
  unsigned start = 0;
  unsigned end = 50;
  unsigned expected_i = end;
  bool sawStartPlusOne = false;
  for (unsigned i : reverse_range(start, end)) {
    EXPECT_GT(i, start);
    EXPECT_LE(i, end);
    EXPECT_EQ(expected_i, i);
    --expected_i;

    sawStartPlusOne |= (i == start + 1);
  }
  EXPECT_TRUE(sawStartPlusOne);
}
