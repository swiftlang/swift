//===--- FrozenMultiMapTest.cpp -------------------------------------------===//
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

#define DEBUG_TYPE "swift-frozen-multi-map-test"

#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Lazy.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"
#include <chrono>
#include <map>
#include <optional>
#include <random>
#include <set>

using namespace swift;

namespace {

class Canary {
  static unsigned currentID;
  unsigned id;

public:
  static void resetIDs() { currentID = 0; }
  Canary(unsigned id) : id(id) {}
  Canary() {
    id = currentID;
    ++currentID;
  }

  unsigned getID() const { return id; }
  bool operator<(const Canary &other) const { return id < other.id; }

  bool operator==(const Canary &other) const { return id == other.id; }

  bool operator!=(const Canary &other) const { return !(*this == other); }
};

unsigned Canary::currentID = 0;

} // namespace

TEST(FrozenMultiMapCustomTest, SimpleFind) {
  Canary::resetIDs();
  FrozenMultiMap<Canary, Canary> map;

  auto key1 = Canary();
  auto key2 = Canary();
  map.insert(key1, Canary());
  map.insert(key1, Canary());
  map.insert(key1, Canary());
  map.insert(key2, Canary());
  map.insert(key2, Canary());

  map.setFrozen();

  EXPECT_EQ(map.size(), 5u);
  {
    auto r = map.find(key1);
    EXPECT_TRUE(r.has_value());
    EXPECT_EQ(r->size(), 3u);
    EXPECT_EQ((*r)[0].getID(), 2u);
    EXPECT_EQ((*r)[1].getID(), 3u);
    EXPECT_EQ((*r)[2].getID(), 4u);
  }

  {
    auto r = map.find(key2);
    EXPECT_TRUE(r.has_value());
    EXPECT_EQ(r->size(), 2u);
    EXPECT_EQ((*r)[0].getID(), 5u);
    EXPECT_EQ((*r)[1].getID(), 6u);
  }
}

TEST(FrozenMultiMapCustomTest, TestResetWorks) {
  Canary::resetIDs();
  FrozenMultiMap<Canary, Canary> map;

  auto key1 = Canary();
  auto key2 = Canary();
  map.insert(key1, Canary());
  map.insert(key1, Canary());
  map.insert(key1, Canary());
  map.insert(key2, Canary());
  map.insert(key2, Canary());

  map.setFrozen();
  map.reset();
  map.insert(key1, Canary());
  map.insert(key1, Canary());
  map.insert(key1, Canary());
  map.insert(key2, Canary());
  map.insert(key2, Canary());

  map.setFrozen();

  // Just do a quick soundness test.
  auto range = map.getRange();
  auto begin = range.begin();
  auto end = range.end();
  ++begin;
  ++begin;
  EXPECT_EQ(std::distance(begin, end), 0);
}

TEST(FrozenMultiMapCustomTest, SimpleIter) {
  Canary::resetIDs();
  FrozenMultiMap<Canary, Canary> map;

  auto key1 = Canary();
  auto key2 = Canary();
  map.insert(key1, Canary());
  map.insert(key1, Canary());
  map.insert(key1, Canary());
  map.insert(key2, Canary());
  map.insert(key2, Canary());

  map.setFrozen();

  EXPECT_EQ(map.size(), 5u);

  auto range = map.getRange();
  EXPECT_EQ(std::distance(range.begin(), range.end()), 2);

  {
    auto begin = range.begin();
    auto end = range.end();
    ++begin;
    ++begin;
    EXPECT_EQ(std::distance(begin, end), 0);
  }

  auto iter = range.begin();
  {
    auto p = *iter;
    EXPECT_EQ(p.first.getID(), key1.getID());
    EXPECT_EQ(p.second.size(), 3u);
    EXPECT_EQ(p.second[0].getID(), 2u);
    EXPECT_EQ(p.second[1].getID(), 3u);
    EXPECT_EQ(p.second[2].getID(), 4u);
  }

  ++iter;
  {
    auto p = *iter;
    EXPECT_EQ(p.first.getID(), key2.getID());
    EXPECT_EQ(p.second.size(), 2u);
    EXPECT_EQ(p.second[0].getID(), 5u);
    EXPECT_EQ(p.second[1].getID(), 6u);
  }
}

TEST(FrozenMultiMapCustomTest, RandomAgainstStdMultiMap) {
  Canary::resetIDs();
  FrozenMultiMap<unsigned, unsigned> map;
  std::multimap<unsigned, unsigned> stdMultiMap;

  auto seed =
      std::chrono::high_resolution_clock::now().time_since_epoch().count();
  std::mt19937 mt_rand(seed);

  std::vector<unsigned> keyIdList;
  for (unsigned i = 0; i < 1024; ++i) {
    unsigned keyID = mt_rand() % 20;
    keyIdList.push_back(keyID);
    for (unsigned valueID = (mt_rand()) % 15; valueID < 15; ++valueID) {
      map.insert(keyID, valueID);
      stdMultiMap.emplace(keyID, valueID);
    }
  }

  map.setFrozen();

  // Then for each key.
  for (unsigned i : keyIdList) {
    // Make sure that we have the same elements in the same order for each key.
    auto range = *map.find(i);
    auto stdRange = stdMultiMap.equal_range(i);
    EXPECT_EQ(std::distance(range.begin(), range.end()),
              std::distance(stdRange.first, stdRange.second));
    auto modernStdRange = llvm::make_range(stdRange.first, stdRange.second);
    for (auto p : llvm::zip(range, modernStdRange)) {
      unsigned lhs = std::get<0>(p);
      unsigned rhs = std::get<1>(p).second;
      EXPECT_EQ(lhs, rhs);
    }
  }

  // Then check that when we iterate over both ranges, we get the same order.
  {
    auto range = map.getRange();
    auto rangeIter = range.begin();
    auto stdRangeIter = stdMultiMap.begin();

    while (rangeIter != range.end()) {
      auto rangeElt = *rangeIter;

      for (unsigned i : indices(rangeElt.second)) {
        EXPECT_EQ(rangeElt.first, stdRangeIter->first);
        EXPECT_EQ(rangeElt.second[i], stdRangeIter->second);
        ++stdRangeIter;
      }

      ++rangeIter;
    }
  }
}

TEST(FrozenMultiMapCustomTest, SimpleErase1) {
  Canary::resetIDs();
  FrozenMultiMap<Canary, Canary> map;

  auto key1 = Canary();
  auto key2 = Canary();
  map.insert(key1, Canary());
  map.insert(key1, Canary());
  map.insert(key1, Canary());
  map.insert(key2, Canary());
  map.insert(key2, Canary());

  map.setFrozen();

  EXPECT_EQ(map.size(), 5u);

  EXPECT_TRUE(map.erase(key2));
  EXPECT_FALSE(map.erase(key2));

  {
    auto range = map.getRange();
    EXPECT_EQ(std::distance(range.begin(), range.end()), 1);

    {
      auto begin = range.begin();
      auto end = range.end();
      ++begin;
      EXPECT_EQ(std::distance(begin, end), 0);
    }

    auto iter = range.begin();
    {
      auto p = *iter;
      EXPECT_EQ(p.first.getID(), key1.getID());
      EXPECT_EQ(p.second.size(), 3u);
      EXPECT_EQ(p.second[0].getID(), 2u);
      EXPECT_EQ(p.second[1].getID(), 3u);
      EXPECT_EQ(p.second[2].getID(), 4u);
    }
  }

  EXPECT_TRUE(map.erase(key1));
  EXPECT_FALSE(map.erase(key1));

  {
    auto range = map.getRange();
    EXPECT_EQ(std::distance(range.begin(), range.end()), 0);
  }
}

TEST(FrozenMultiMapCustomTest, SimpleErase2) {
  Canary::resetIDs();
  FrozenMultiMap<Canary, Canary> map;

  auto key1 = Canary();
  auto key2 = Canary();
  map.insert(key1, Canary());
  map.insert(key1, Canary());
  map.insert(key1, Canary());
  map.insert(key2, Canary());
  map.insert(key2, Canary());

  map.setFrozen();

  EXPECT_EQ(map.size(), 5u);

  EXPECT_TRUE(map.erase(key1));

  {
    auto range = map.getRange();
    EXPECT_EQ(std::distance(range.begin(), range.end()), 1);

    {
      auto begin = range.begin();
      auto end = range.end();
      ++begin;
      EXPECT_EQ(std::distance(begin, end), 0);
    }

    auto iter = range.begin();
    {
      auto p = *iter;
      EXPECT_EQ(p.first.getID(), key2.getID());
      EXPECT_EQ(p.second.size(), 2u);
      EXPECT_EQ(p.second[0].getID(), 5u);
      EXPECT_EQ(p.second[1].getID(), 6u);
    }
  }

  EXPECT_TRUE(map.erase(key2));
  EXPECT_FALSE(map.erase(key2));

  {
    auto range = map.getRange();
    EXPECT_EQ(std::distance(range.begin(), range.end()), 0);
  }
}
