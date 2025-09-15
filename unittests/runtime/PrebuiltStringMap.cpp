//===--- PrebuiltStringMap.cpp - Unit tests for PrebuiltStringMap ---------===//
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

#include "swift/Runtime/PrebuiltStringMap.h"
#include "gtest/gtest.h"

static bool stringIsNull(const char *str) { return str == nullptr; }

TEST(PrebuiltStringMapTest, PrebuiltStringMap) {
  auto testOnce = [&](unsigned testEntryCount) {
    std::vector<std::pair<std::string, unsigned>> testVector;
    testVector.reserve(testEntryCount);
    for (unsigned i = 0; i < testEntryCount; i++) {
      std::string key;
      for (unsigned j = 0; j < i; j++)
        key += std::to_string(j);
      testVector.push_back({key, i});
    }

    using Map = swift::PrebuiltStringMap<const char *, unsigned, stringIsNull>;

    unsigned mapSize = testEntryCount * 4 / 3;
    void *mapAllocation = calloc(1, Map::byteSize(mapSize));
    Map *map = new (mapAllocation) Map(mapSize);

    // Populate the map.
    for (auto &[key, value] : testVector) {
      const char *keyCStr = key.c_str();
      auto *element = map->insert(keyCStr);
      EXPECT_NE(element, nullptr);

      element->key = keyCStr;
      element->value = value;
    }

    // Verify that we can find all the test values.
    for (auto &[key, value] : testVector) {
      const char *keyCStr = key.c_str();
      auto *element = map->find(keyCStr);
      EXPECT_NE(element, nullptr);

      EXPECT_EQ(element->key, key);
      EXPECT_EQ(element->value, value);
    }

    // Test the interface that takes a string and length without NUL
    // termination.
    for (auto &[key, value] : testVector) {
      auto keyCopy = key;
      keyCopy += "xyz";
      const char *keyCStr = keyCopy.c_str();
      auto *element = map->find(keyCStr, key.size());
      EXPECT_NE(element, nullptr);

      EXPECT_EQ(element->key, key);
      EXPECT_EQ(element->value, value);
    }

    // Verify that nonexistent keys are not found.
    const char *nonexistentKey = "ceci n'est pas une clef";
    auto *element = map->find(nonexistentKey);
    EXPECT_EQ(element, nullptr);

    free(mapAllocation);
  };

  testOnce(10);
  testOnce(100);
  testOnce(1000);
}

TEST(PrebuiltStringMapTest, PrebuiltAuxDataImplicitStringMap) {
  auto testOnce = [&](unsigned testEntryCount) {
    // Test a map containing positive integers, where the key is a string
    // derived from the integer. The aux data is the negative of the integer.
    using Map = swift::PrebuiltAuxDataImplicitStringMap<uint64_t, int32_t>;

    auto getKey = [](unsigned n) {
      std::string key;
      for (unsigned i = 0; i < n; i++) {
        key += 'A' + (i % 26);
      }
      return key;
    };

    unsigned mapSize = testEntryCount * 4 / 3;
    void *mapAllocation = calloc(1, Map::byteSize(mapSize));
    Map *map = new (mapAllocation) Map(mapSize);

    // Populate the map.
    for (unsigned n = 0; n < testEntryCount; n++) {
      auto key = getKey(n);

      auto isNull = [](auto pointers) { return *pointers.first == 0; };
      auto pointers = map->insert(key.c_str(), isNull);
      EXPECT_NE(pointers.first, nullptr);
      EXPECT_NE(pointers.second, nullptr);

      *pointers.first = n;
      *pointers.second = -(int64_t)n;
    }

    // Verify that we can find all the test values.
    for (unsigned n = 0; n < testEntryCount; n++) {
      auto key = getKey(n);
      auto keyLength = key.size();

      // Add some trash to the end to make sure the lookup doesn't look beyond
      // the specified length.
      key += "xyz";

      auto isMatch = [n](auto pointers) { return *pointers.first == n; };
      auto isNull = [](auto pointers) { return *pointers.first == 0; };
      auto pointers = map->find(key.c_str(), keyLength, isMatch, isNull);
      EXPECT_NE(pointers.first, nullptr);
      EXPECT_NE(pointers.second, nullptr);
      EXPECT_EQ(*pointers.first, n);
      EXPECT_EQ(*pointers.second, -(int64_t)n);
    }

    // Verfy a nonexistent value is not found.
    const char *nonexistentKey = "ceci n'est pas une clef";
    auto isMatch = [](auto pointers) { return false; };
    auto isNull = [](auto pointers) { return *pointers.first == 0; };
    auto pointers =
        map->find(nonexistentKey, strlen(nonexistentKey), isMatch, isNull);
    EXPECT_EQ(*pointers.first, 0);
    EXPECT_EQ(*pointers.second, 0);
  };

  testOnce(10);
  testOnce(100);
  testOnce(1000);
}
