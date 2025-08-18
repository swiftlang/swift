//===--- STLExtrasTest.cpp - Tests for STL utilities ---------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include <gtest/gtest.h>
#include <vector>
#include <algorithm>
#include <memory>

using namespace std;

// STL utility functions for testing
namespace {
  template<typename Container, typename Predicate>
  auto find_if_not(Container& container, Predicate pred) {
    return std::find_if_not(container.begin(), container.end(), pred);
  }
  
  template<typename Container, typename Value>
  bool contains(const Container& container, const Value& value) {
    return std::find(container.begin(), container.end(), value) != container.end();
  }
  
  template<typename Container>
  void reverse(Container& container) {
    std::reverse(container.begin(), container.end());
  }
}

TEST(STLExtrasTest, FindIfNot) {
  vector<int> numbers = {1, 2, 3, 4, 5, 6};
  
  auto it = find_if_not(numbers, [](int x) { return x % 2 == 1; }); // Find first even
  EXPECT_NE(it, numbers.end());
  EXPECT_EQ(*it, 2);
  
  auto it2 = find_if_not(numbers, [](int x) { return x < 10; }); // Find first >= 10
  EXPECT_EQ(it2, numbers.end());
}

TEST(STLExtrasTest, Contains) {
  vector<string> words = {"hello", "world", "test"};
  
  EXPECT_TRUE(contains(words, string("hello")));
  EXPECT_TRUE(contains(words, string("world")));
  EXPECT_FALSE(contains(words, string("missing")));
  EXPECT_FALSE(contains(words, string("")));
}

TEST(STLExtrasTest, ReverseContainer) {
  vector<int> numbers = {1, 2, 3, 4, 5};
  vector<int> expected = {5, 4, 3, 2, 1};
  
  reverse(numbers);
  
  EXPECT_EQ(numbers, expected);
}

TEST(STLExtrasTest, SmartPointerOperations) {
  vector<unique_ptr<int>> ptrs;
  ptrs.push_back(make_unique<int>(1));
  ptrs.push_back(make_unique<int>(2));
  ptrs.push_back(make_unique<int>(3));
  
  EXPECT_EQ(ptrs.size(), 3u);
  EXPECT_EQ(*ptrs[0], 1);
  EXPECT_EQ(*ptrs[1], 2);
  EXPECT_EQ(*ptrs[2], 3);
  
  // Test moving unique_ptr
  auto moved = std::move(ptrs[0]);
  EXPECT_EQ(ptrs[0], nullptr);
  EXPECT_NE(moved, nullptr);
  EXPECT_EQ(*moved, 1);
}

TEST(STLExtrasTest, AlgorithmChaining) {
  vector<int> numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  vector<int> result;
  
  // Find even numbers, square them, and collect
  copy_if(numbers.begin(), numbers.end(), back_inserter(result),
          [](int x) { return x % 2 == 0; });
  
  transform(result.begin(), result.end(), result.begin(),
            [](int x) { return x * x; });
  
  vector<int> expected = {4, 16, 36, 64, 100}; // 2², 4², 6², 8², 10²
  EXPECT_EQ(result, expected);
}

TEST(STLExtrasTest, EmptyContainerOperations) {
  vector<int> empty;
  
  EXPECT_FALSE(contains(empty, 42));
  EXPECT_EQ(find_if_not(empty, [](int) { return true; }), empty.end());
  
  reverse(empty);
  EXPECT_TRUE(empty.empty());
}