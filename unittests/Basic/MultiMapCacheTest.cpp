//===--- MultiMapCacheTest.cpp --------------------------------------------===//
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

#include "swift/Basic/MultiMapCache.h"
#include "swift/Basic/Range.h"
#include "gtest/gtest.h"
#include <random>

using namespace swift;

namespace {

/// A multimap cache that caches the initial 4 powers of each key.
struct PowerMultiMapCache
    : public MultiMapCache<PowerMultiMapCache, unsigned, unsigned> {
  bool constructValuesForKey(unsigned key, std::vector<unsigned> &data) {
    // Construct the first 3 powers of key.
    data.push_back(key);
    data.push_back(key * key);
    data.push_back(key * key * key);
    return true;
  }
};

} // end anonymous namespace

TEST(MultiMapCache, powersTest) {
  PowerMultiMapCache cache;

  EXPECT_TRUE(cache.empty());
  EXPECT_EQ(cache.size(), 0u);
  for (unsigned index : range(1, 256)) {
    auto array = *cache.get(index);
    for (unsigned power : array) {
      EXPECT_EQ(power % index, 0);
    }
  }
  EXPECT_FALSE(cache.empty());
  EXPECT_EQ(cache.size(), 255);
  for (unsigned index : range(1, 256)) {
    auto array = *cache.get(index);
    for (unsigned power : array) {
      EXPECT_EQ(power % index, 0);
    }
  }
  EXPECT_FALSE(cache.empty());
  EXPECT_EQ(cache.size(), 255);

  cache.clear();
  EXPECT_TRUE(cache.empty());
  EXPECT_EQ(cache.size(), 0);
}
