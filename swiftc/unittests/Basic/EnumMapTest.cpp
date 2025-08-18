//===--- EnumMapTest.cpp - Tests for enum-based maps ---------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include <gtest/gtest.h>
#include <array>
#include <string>

using namespace std;

// Simple enum map implementation for testing
enum class TokenType {
  Identifier,
  Number,
  String,
  Operator,
  Keyword,
  COUNT
};

template<typename Enum, typename Value, size_t N = static_cast<size_t>(Enum::COUNT)>
class EnumMap {
  array<Value, N> data;
  
public:
  EnumMap() = default;
  
  Value& operator[](Enum key) {
    return data[static_cast<size_t>(key)];
  }
  
  const Value& operator[](Enum key) const {
    return data[static_cast<size_t>(key)];
  }
  
  size_t size() const { return N; }
  
  auto begin() { return data.begin(); }
  auto end() { return data.end(); }
  auto begin() const { return data.begin(); }
  auto end() const { return data.end(); }
};

TEST(EnumMapTest, BasicOperations) {
  EnumMap<TokenType, string> tokenNames;
  
  tokenNames[TokenType::Identifier] = "identifier";
  tokenNames[TokenType::Number] = "number";
  tokenNames[TokenType::String] = "string";
  tokenNames[TokenType::Operator] = "operator";
  tokenNames[TokenType::Keyword] = "keyword";
  
  EXPECT_EQ(tokenNames[TokenType::Identifier], "identifier");
  EXPECT_EQ(tokenNames[TokenType::Number], "number");
  EXPECT_EQ(tokenNames[TokenType::String], "string");
  EXPECT_EQ(tokenNames[TokenType::Operator], "operator");
  EXPECT_EQ(tokenNames[TokenType::Keyword], "keyword");
}

TEST(EnumMapTest, SizeAndIteration) {
  EnumMap<TokenType, int> tokenCounts;
  
  EXPECT_EQ(tokenCounts.size(), 5u);
  
  // Initialize all values
  for (size_t i = 0; i < tokenCounts.size(); ++i) {
    tokenCounts[static_cast<TokenType>(i)] = i * 10;
  }
  
  // Test iteration
  int sum = 0;
  for (const auto& count : tokenCounts) {
    sum += count;
  }
  
  EXPECT_EQ(sum, 0 + 10 + 20 + 30 + 40); // 100
}

TEST(EnumMapTest, ConstAccess) {
  EnumMap<TokenType, string> tokenNames;
  tokenNames[TokenType::Identifier] = "test";
  
  const auto& constMap = tokenNames;
  EXPECT_EQ(constMap[TokenType::Identifier], "test");
}

TEST(EnumMapTest, DefaultValues) {
  EnumMap<TokenType, int> tokenCounts; // Default initialized
  
  for (size_t i = 0; i < tokenCounts.size(); ++i) {
    EXPECT_EQ(tokenCounts[static_cast<TokenType>(i)], 0);
  }
}

TEST(EnumMapTest, BooleanMap) {
  EnumMap<TokenType, bool> tokenFlags;
  
  EXPECT_FALSE(tokenFlags[TokenType::Identifier]);
  
  tokenFlags[TokenType::Identifier] = true;
  tokenFlags[TokenType::Keyword] = true;
  
  EXPECT_TRUE(tokenFlags[TokenType::Identifier]);
  EXPECT_FALSE(tokenFlags[TokenType::Number]);
  EXPECT_FALSE(tokenFlags[TokenType::String]);
  EXPECT_FALSE(tokenFlags[TokenType::Operator]);
  EXPECT_TRUE(tokenFlags[TokenType::Keyword]);
}