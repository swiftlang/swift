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

TEST(PrebuiltStringMapTest, basic) {
  unsigned testEntryCount = 1000;

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

  for (auto &[key, value] : testVector) {
    const char *keyCStr = key.c_str();
    auto *element = map->insert(keyCStr);
    EXPECT_NE(element, nullptr);

    element->key = keyCStr;
    element->value = value;
  }

  for (auto &[key, value] : testVector) {
    const char *keyCStr = key.c_str();
    auto *element = map->find(keyCStr);
    EXPECT_NE(element, nullptr);

    EXPECT_EQ(element->key, key);
    EXPECT_EQ(element->value, value);
  }

  free(mapAllocation);
}
