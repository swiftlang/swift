//===--- StringComparison.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/StringComparison.h"
#include <gtest/gtest.h>
#include <random>
#include <vector>
#include <chrono>

//===----------------------------------------------------------------------===//
//                                Small Tests
//===----------------------------------------------------------------------===//

TEST(TestStringComparison, test_swift_smallStringComparison) {
  int count = 10;
  uint8_t *lhs = new uint8_t[count];
  uint16_t *rhs = new uint16_t[count];

  using clock = std::chrono::high_resolution_clock;
  auto seed = clock::now().time_since_epoch().count();
  std::mt19937 mt_rand(seed);

  for (int i = 0; i < count; ++i) {
    lhs[i] = static_cast<uint8_t>(mt_rand());
    rhs[i] = lhs[i];
  }

  // We expect to return count since they are the same.
  EXPECT_EQ(swift_stdlib_findDiffIdx_UInt8UInt16(lhs, rhs, count), count);

  // Introduce a delta in element 5 in the 8 bit vector and then go look for it!
  ++lhs[5];
  EXPECT_EQ(swift_stdlib_findDiffIdx_UInt8UInt16(lhs, rhs, count), 5);

  // Then introduce one in element 4 of the 16 bit. We should ignore the 5th
  // element here since we should hit element 4 first.
  ++rhs[4];
  EXPECT_EQ(swift_stdlib_findDiffIdx_UInt8UInt16(lhs, rhs, count), 4);

  delete [] lhs;
  delete [] rhs;
}

//===----------------------------------------------------------------------===//
//                                Large Tests
//===----------------------------------------------------------------------===//

TEST(TestStringComparison, test_swift_largeStringComparisonBackwards) {
  // 4 pages
  int count = 4096*4;
  uint8_t *lhsOrig = new uint8_t[count];
  uint16_t *rhsOrig = new uint16_t[count];

  using clock = std::chrono::high_resolution_clock;
  auto seed = clock::now().time_since_epoch().count();
  std::mt19937 mt_rand(seed);

  for (int i = 0; i < count; ++i) {
    lhsOrig[i] = static_cast<uint8_t>(mt_rand());
    rhsOrig[i] = lhsOrig[i];
  }

  // We expect to return count since they are the same.
  EXPECT_EQ(swift_stdlib_findDiffIdx_UInt8UInt16(lhsOrig, rhsOrig, count), count);

  uint8_t *lhsCopy = new uint8_t[count];
  memcpy(lhsCopy, lhsOrig, count*sizeof(uint8_t));
  uint16_t *rhsCopy = new uint16_t[count];
  memcpy(rhsCopy, rhsOrig, count*sizeof(uint16_t));

  // Our plan is to iterate back through the array and create errors as we
  // go. We should only return the first one.
  for (int i = count-1; i; --i) {
    ++lhsCopy[i];
    EXPECT_EQ(swift_stdlib_findDiffIdx_UInt8UInt16(lhsCopy, rhsCopy, count), i);
  }

  // Now try the rhs. First undo our change to lhsCopy.
  memcpy(lhsCopy, lhsOrig, count*sizeof(uint8_t));
  for (int i = count-1; i; --i) {
    ++rhsCopy[i];
    EXPECT_EQ(swift_stdlib_findDiffIdx_UInt8UInt16(lhsCopy, rhsCopy, count), i);
  }

  delete [] lhsOrig;
  delete [] rhsOrig;
  delete [] lhsCopy;
  delete [] rhsCopy;
}

TEST(TestStringComparison, test_swift_largeStringComparisonBoundaryConditionTest) {
#if false
  // 4 pages
  unsigned count = 4096*4;
  uint8_t *lhsOrig = new uint8_t[count];
  uint16_t *rhsOrig = new uint16_t[count];

  using clock = std::chrono::high_resolution_clock;
  auto seed = clock::now().time_since_epoch().count();
  std::mt19937 mt_rand(seed);

  for (unsigned i = 0; i < count; ++i) {
    lhsOrig[i] = static_cast<uint8_t>(mt_rand());
    rhsOrig[i] = lhsOrig[i];
  }

  // We expect to return count since they are the same.
  EXPECT_EQ(swift_stdlib_findDiffIdx_UInt8UInt16(lhsOrig, rhsOrig, count), count);

  uint8_t *lhsCopy = new uint8_t[count];
  memcpy(lhsCopy, lhsOrig, count*sizeof(uint8_t));
  uint8_t *rhsCopy = new uint16_t[count];
  memcpy(rhsCopy, rhsOrig, count*sizeof(uint16_t));

  // Our plan is to iterate back through the array and create errors as we
  // go. We should only return the first one.
  for (int i = count-1; i; --i) {
    ++lhsCopy[i];
    EXPECT_EQ(swift_stdlib_findDiffIdx_UInt8UInt16(lhsCopy, rhsCopy, count), i);
  }

  // Now try the rhs. First undo our change to lhsCopy.
  memcpy(lhsCopy, lhsOrig, count*sizeof(uint8_t));
  for (int i = count-1; i; --i) {
    ++rhsCopy[i];
    EXPECT_EQ(swift_stdlib_findDiffIdx_UInt8UInt16(lhsCopy, rhsCopy, count), i);
  }

  delete [] lhsOrig;
  delete [] rhsOrig;
  delete [] lhsCopy;
  delete [] rhsCopy;
#endif
}
