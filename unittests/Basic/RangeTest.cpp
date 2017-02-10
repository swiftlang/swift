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
