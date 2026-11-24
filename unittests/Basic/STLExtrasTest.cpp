//===--- STLExtrasTest.cpp ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/STLExtras.h"
#include "gtest/gtest.h"
#include <random>
#include <utility>
#include <vector>

using namespace swift;

TEST(RemoveAdjacentIf, NoRemovals) {
  {
    int items[] = { 1, 2, 3 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, std::end(items));
  }

  {
    int items[] = { 1 };
    // Test an empty range.
    auto result = removeAdjacentIf(std::begin(items), std::begin(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, std::begin(items));
  }

  {
    int *null = nullptr;
    auto result = removeAdjacentIf(null, null, std::equal_to<int>());
    EXPECT_EQ(result, null);
  }
}

TEST(RemoveAdjacentIf, OnlyOneRun) {
  {
    int items[] = { 1, 2, 3, 3, 4, 5, 6 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, &items[5]);
    EXPECT_EQ(items[0], 1);
    EXPECT_EQ(items[1], 2);
    EXPECT_EQ(items[2], 4);
    EXPECT_EQ(items[3], 5);
    EXPECT_EQ(items[4], 6);
  }

  {
    int items[] = { 1, 2, 3, 3, 3, 4, 5, 6 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, &items[5]);
    EXPECT_EQ(items[0], 1);
    EXPECT_EQ(items[1], 2);
    EXPECT_EQ(items[2], 4);
    EXPECT_EQ(items[3], 5);
    EXPECT_EQ(items[4], 6);
  }

  {
    int items[] = { 1, 2, 3, 3, 3, 3, 4, 5, 6 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, &items[5]);
    EXPECT_EQ(items[0], 1);
    EXPECT_EQ(items[1], 2);
    EXPECT_EQ(items[2], 4);
    EXPECT_EQ(items[3], 5);
    EXPECT_EQ(items[4], 6);
  }

  {
    int items[] = { 1, 2, 3, 3, 3, 3 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, &items[2]);
    EXPECT_EQ(items[0], 1);
    EXPECT_EQ(items[1], 2);
  }

  {
    int items[] = { 3, 3, 3, 3, 4, 5, 6 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, &items[3]);
    EXPECT_EQ(items[0], 4);
    EXPECT_EQ(items[1], 5);
    EXPECT_EQ(items[2], 6);
  }

  {
    int items[] = { 1, 1, 1, 1 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, &items[0]);
  }
}


TEST(RemoveAdjacentIf, MultipleRuns) {
  {
    int items[] = { 1, 2, 3, 3, 4, 5, 6, 7, 7, 8, 9 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, &items[7]);
    EXPECT_EQ(items[0], 1);
    EXPECT_EQ(items[1], 2);
    EXPECT_EQ(items[2], 4);
    EXPECT_EQ(items[3], 5);
    EXPECT_EQ(items[4], 6);
    EXPECT_EQ(items[5], 8);
    EXPECT_EQ(items[6], 9);
  }

  {
    int items[] = { 1, 2, 3, 3, 3, 4, 5, 6, 7, 7, 7, 8, 9 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, &items[7]);
    EXPECT_EQ(items[0], 1);
    EXPECT_EQ(items[1], 2);
    EXPECT_EQ(items[2], 4);
    EXPECT_EQ(items[3], 5);
    EXPECT_EQ(items[4], 6);
    EXPECT_EQ(items[5], 8);
    EXPECT_EQ(items[6], 9);
  }

  {
    int items[] = { 1, 2, 3, 3, 3, 3, 4, 5, 6, 7, 7, 7, 7, 8, 9 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, &items[7]);
    EXPECT_EQ(items[0], 1);
    EXPECT_EQ(items[1], 2);
    EXPECT_EQ(items[2], 4);
    EXPECT_EQ(items[3], 5);
    EXPECT_EQ(items[4], 6);
    EXPECT_EQ(items[5], 8);
    EXPECT_EQ(items[6], 9);
  }

  {
    int items[] = { 1, 2, 3, 3, 3, 3, 7, 7 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, &items[2]);
    EXPECT_EQ(items[0], 1);
    EXPECT_EQ(items[1], 2);
  }

  {
    int items[] = { 3, 3, 3, 3, 4, 5, 6, 7, 7 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, &items[3]);
    EXPECT_EQ(items[0], 4);
    EXPECT_EQ(items[1], 5);
    EXPECT_EQ(items[2], 6);
  }

  {
    int items[] = { 3, 3, 3, 3, 7, 7, 8, 9 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, &items[2]);
    EXPECT_EQ(items[0], 8);
    EXPECT_EQ(items[1], 9);
  }

  {
    int items[] = { 1, 1, 1, 1, 2, 2 };
    auto result = removeAdjacentIf(std::begin(items), std::end(items),
                                   std::equal_to<int>());
    EXPECT_EQ(result, &items[0]);
  }
}

namespace {
/// An element with a key and an ID. Elements with the same key are the same;
/// among them, higher IDs sort first.
using KeyAndID = std::pair<int, int>;

bool keyThenDescendingID(const KeyAndID &lhs, const KeyAndID &rhs) {
  if (lhs.first != rhs.first)
    return lhs.first < rhs.first;
  return lhs.second > rhs.second;
}

bool sameKey(const KeyAndID &lhs, const KeyAndID &rhs) {
  return lhs.first == rhs.first;
}

/// Orders only by key / 2, so that elements with keys 2k and 2k + 1 are
/// equivalent but not the same.
bool keyPair(const KeyAndID &lhs, const KeyAndID &rhs) {
  return lhs.first / 2 < rhs.first / 2;
}

/// Merge with std::inplace_merge and std::unique, which mergeUnique must be
/// equivalent to.
template <typename Compare>
std::vector<KeyAndID> referenceMergeUnique(std::vector<KeyAndID> elements,
                                           const std::vector<KeyAndID> &added,
                                           Compare less) {
  auto middle = elements.insert(elements.end(), added.begin(), added.end());
  std::inplace_merge(elements.begin(), middle, elements.end(), less);
  elements.erase(std::unique(elements.begin(), elements.end(), sameKey),
                 elements.end());
  return elements;
}

/// Check mergeUnique against std::inplace_merge and std::unique on random
/// inputs ordered by \p less.
template <typename Compare>
void checkMatchesInplaceMergeAndUnique(Compare less) {
  std::mt19937 generator(42);
  std::uniform_int_distribution<int> keys(0, 15);
  std::uniform_int_distribution<int> ids(0, 3);
  std::uniform_int_distribution<size_t> elementCounts(0, 20);
  std::uniform_int_distribution<size_t> addedCounts(0, 10);

  for (unsigned iteration = 0; iteration != 2000; ++iteration) {
    // Sorted elements without consecutive same elements, and sorted elements
    // to add.
    std::vector<KeyAndID> elements;
    for (size_t i = 0, e = elementCounts(generator); i != e; ++i)
      elements.push_back({keys(generator), ids(generator)});
    std::stable_sort(elements.begin(), elements.end(), less);
    elements.erase(std::unique(elements.begin(), elements.end(), sameKey),
                   elements.end());

    std::vector<KeyAndID> added;
    for (size_t i = 0, e = addedCounts(generator); i != e; ++i)
      added.push_back({keys(generator), ids(generator)});
    std::stable_sort(added.begin(), added.end(), less);

    auto expected = referenceMergeUnique(elements, added, less);
    mergeUnique(elements, added, less, sameKey);
    EXPECT_EQ(elements, expected) << "iteration " << iteration;
  }
}
} // end anonymous namespace

TEST(MergeUnique, EmptyRanges) {
  std::vector<KeyAndID> elements;
  mergeUnique(elements, std::vector<KeyAndID>{}, keyThenDescendingID, sameKey);
  EXPECT_TRUE(elements.empty());

  elements = {{1, 0}, {3, 0}};
  mergeUnique(elements, std::vector<KeyAndID>{}, keyThenDescendingID, sameKey);
  EXPECT_EQ(elements, (std::vector<KeyAndID>{{1, 0}, {3, 0}}));

  elements = {};
  mergeUnique(elements, std::vector<KeyAndID>{{1, 0}, {3, 0}},
              keyThenDescendingID, sameKey);
  EXPECT_EQ(elements, (std::vector<KeyAndID>{{1, 0}, {3, 0}}));
}

TEST(MergeUnique, InsertsInOrder) {
  std::vector<KeyAndID> elements = {{2, 0}, {4, 0}, {6, 0}};
  mergeUnique(elements, std::vector<KeyAndID>{{1, 0}, {5, 0}, {7, 0}},
              keyThenDescendingID, sameKey);
  EXPECT_EQ(elements, (std::vector<KeyAndID>{
                          {1, 0}, {2, 0}, {4, 0}, {5, 0}, {6, 0}, {7, 0}}));
}

TEST(MergeUnique, KeepsFirstOfSameElements) {
  // A same element that sorts first replaces the existing one.
  std::vector<KeyAndID> elements = {{1, 1}, {2, 1}, {3, 1}};
  mergeUnique(elements, std::vector<KeyAndID>{{2, 5}}, keyThenDescendingID,
              sameKey);
  EXPECT_EQ(elements, (std::vector<KeyAndID>{{1, 1}, {2, 5}, {3, 1}}));

  // A same element that sorts after the existing one is dropped.
  elements = {{1, 1}, {2, 5}, {3, 1}};
  mergeUnique(elements, std::vector<KeyAndID>{{2, 1}}, keyThenDescendingID,
              sameKey);
  EXPECT_EQ(elements, (std::vector<KeyAndID>{{1, 1}, {2, 5}, {3, 1}}));

  // Of several same elements added, the first one is kept.
  elements = {{1, 1}, {2, 1}, {3, 1}};
  mergeUnique(elements, std::vector<KeyAndID>{{2, 9}, {2, 7}, {3, 4}, {3, 2}},
              keyThenDescendingID, sameKey);
  EXPECT_EQ(elements, (std::vector<KeyAndID>{{1, 1}, {2, 9}, {3, 4}}));
}

TEST(MergeUnique, MatchesInplaceMergeAndUnique) {
  checkMatchesInplaceMergeAndUnique(keyThenDescendingID);
}

TEST(MergeUnique, MatchesInplaceMergeAndUniqueWithEquivalentElements) {
  checkMatchesInplaceMergeAndUnique(keyPair);
}
