//===--- RangeTest.cpp - Tests for Range utilities -----------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Basic/SourceLoc.h"
#include <gtest/gtest.h>

using namespace swiftc;

TEST(RangeTest, SourceLocBasics) {
  SourceLoc loc1(100);
  SourceLoc loc2(200);
  SourceLoc invalid;
  
  EXPECT_TRUE(loc1.isValid());
  EXPECT_TRUE(loc2.isValid());
  EXPECT_FALSE(invalid.isValid());
  
  EXPECT_EQ(loc1.getRawValue(), 100u);
  EXPECT_EQ(loc2.getRawValue(), 200u);
  EXPECT_EQ(invalid.getRawValue(), 0u);
}

TEST(RangeTest, SourceRangeBasics) {
  SourceLoc start(50);
  SourceLoc end(100);
  SourceRange range(start, end);
  
  EXPECT_EQ(range.getStart().getRawValue(), 50u);
  EXPECT_EQ(range.getEnd().getRawValue(), 100u);
  EXPECT_TRUE(range.isValid());
}

TEST(RangeTest, SourceRangeInvalid) {
  SourceLoc invalid;
  SourceRange invalidRange(invalid, invalid);
  
  EXPECT_FALSE(invalidRange.isValid());
}

TEST(RangeTest, SourceRangeEquality) {
  SourceLoc start(100);
  SourceLoc end(200);
  SourceRange range1(start, end);
  SourceRange range2(start, end);
  SourceRange range3(SourceLoc(50), SourceLoc(150));
  
  EXPECT_EQ(range1, range2);
  EXPECT_NE(range1, range3);
}

TEST(RangeTest, SourceRangeValidation) {
  SourceRange invalidRange;
  EXPECT_FALSE(invalidRange.isValid());
  EXPECT_TRUE(invalidRange.isInvalid());
  
  SourceRange validRange(SourceLoc(100), SourceLoc(200));
  EXPECT_TRUE(validRange.isValid());
  EXPECT_FALSE(validRange.isInvalid());
}

TEST(RangeTest, SourceRangeFromSingleLoc) {
  SourceLoc loc(150);
  SourceRange range(loc);
  
  EXPECT_EQ(range.getStart(), loc);
  EXPECT_EQ(range.getEnd(), loc);
  EXPECT_TRUE(range.isValid());
}