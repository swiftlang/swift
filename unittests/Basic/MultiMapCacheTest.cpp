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

TEST(MultiMapCache, powersTest) {
  std::function<bool(unsigned, std::vector<unsigned> &)> cacheCompute =
      [&](unsigned key, std::vector<unsigned> &outArray) {
        outArray.push_back(key);
        outArray.push_back(key * key);
        outArray.push_back(key * key * key);
        return true;
      };
  MultiMapCache<unsigned, unsigned> cache(cacheCompute);

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

TEST(MultiMapCache, smallTest) {
  std::function<bool(unsigned, SmallVectorImpl<unsigned> &)> cacheCompute =
      [&](unsigned key, SmallVectorImpl<unsigned> &outArray) {
        outArray.push_back(key);
        outArray.push_back(key * key);
        outArray.push_back(key * key * key);
        return true;
      };
  SmallMultiMapCache<unsigned, unsigned> cache(cacheCompute);

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
