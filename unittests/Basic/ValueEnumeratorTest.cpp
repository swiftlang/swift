//===--- ValueEnumeratorTest.cpp ------------------------------------------===//
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

#include "swift/Basic/ValueEnumerator.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Basic/Range.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(ValueEnumerator, basic) {

  {
    ValueEnumerator<int> Trans;
    // Check that indexing is persistent.
    EXPECT_EQ(Trans.getIndex(99), Trans.getIndex(99));
    EXPECT_EQ(Trans.getIndex(100), Trans.getIndex(100));

    // Check that we don't have collisions.
    bool SameIndex = Trans.getIndex(82) == Trans.getIndex(73);
    EXPECT_FALSE(SameIndex);

    // Check that invalidation works.
    // After invalidation the old index must not be equal to the new index.
    size_t oldIndex = Trans.getIndex(99);
    Trans.invalidateValue(99);
    size_t newIndex = Trans.getIndex(99);
    EXPECT_FALSE(newIndex == oldIndex);
  }

  {
    const char *string_1 = "hello";
    const char *string_2 = "goodbye";
    const char *string_3 = ":-)";
    ValueEnumerator<const char *> Trans;
    EXPECT_EQ(Trans.getIndex(nullptr), Trans.getIndex(nullptr));
    EXPECT_EQ(Trans.getIndex(string_1), Trans.getIndex(string_1));
    EXPECT_EQ(Trans.getIndex(string_2), Trans.getIndex(string_2));

    // Check that invalidation works.
    size_t oldIndex = Trans.getIndex(string_3);
    Trans.invalidateValue(string_3);
    size_t newIndex = Trans.getIndex(string_3);
    EXPECT_FALSE(newIndex == oldIndex);

    // Check that different values don't give the same index.
    EXPECT_FALSE(Trans.getIndex(string_2) == Trans.getIndex(string_3));
  }

  {
    ValueEnumerator<int> Trans;
    // Check a bunch of integers.
    for (int i = 1; i < 10000; i++) {
      EXPECT_TRUE(Trans.getIndex(0) != Trans.getIndex(i));
    }

    // Check that there are no accidental collisions.
    for (int i = 0; i < 10000; i++) {
      for (int j = 1; j < 10; j++) {
        EXPECT_TRUE(Trans.getIndex(i) != Trans.getIndex(i + j));
      }
    }

    // Check that indexing is still persistent.
    EXPECT_EQ(Trans.getIndex(100), Trans.getIndex(100));
  }
}
