//===--- StringExtrasTest.cpp - Tests for string utilities ---------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include <gtest/gtest.h>
#include <string>
#include <algorithm>
#include <cctype>

using namespace std;

// Basic string utility functions for testing
namespace {
  string toLower(const string& str) {
    string result = str;
    transform(result.begin(), result.end(), result.begin(), ::tolower);
    return result;
  }
  
  string toUpper(const string& str) {
    string result = str;
    transform(result.begin(), result.end(), result.begin(), ::toupper);
    return result;
  }
  
  bool startsWith(const string& str, const string& prefix) {
    return str.size() >= prefix.size() && 
           str.substr(0, prefix.size()) == prefix;
  }
  
  bool endsWith(const string& str, const string& suffix) {
    return str.size() >= suffix.size() && 
           str.substr(str.size() - suffix.size()) == suffix;
  }
  
  string trim(const string& str) {
    size_t start = str.find_first_not_of(" \t\n\r");
    if (start == string::npos) return "";
    size_t end = str.find_last_not_of(" \t\n\r");
    return str.substr(start, end - start + 1);
  }
}

TEST(StringExtrasTest, ToLowerCase) {
  EXPECT_EQ(toLower("HELLO"), "hello");
  EXPECT_EQ(toLower("Hello"), "hello");
  EXPECT_EQ(toLower("hello"), "hello");
  EXPECT_EQ(toLower(""), "");
  EXPECT_EQ(toLower("123ABC"), "123abc");
}

TEST(StringExtrasTest, ToUpperCase) {
  EXPECT_EQ(toUpper("hello"), "HELLO");
  EXPECT_EQ(toUpper("Hello"), "HELLO");
  EXPECT_EQ(toUpper("HELLO"), "HELLO");
  EXPECT_EQ(toUpper(""), "");
  EXPECT_EQ(toUpper("123abc"), "123ABC");
}

TEST(StringExtrasTest, StartsWith) {
  EXPECT_TRUE(startsWith("hello world", "hello"));
  EXPECT_TRUE(startsWith("hello world", ""));
  EXPECT_TRUE(startsWith("hello", "hello"));
  EXPECT_FALSE(startsWith("hello", "world"));
  EXPECT_FALSE(startsWith("hi", "hello"));
  EXPECT_FALSE(startsWith("", "hello"));
}

TEST(StringExtrasTest, EndsWith) {
  EXPECT_TRUE(endsWith("hello world", "world"));
  EXPECT_TRUE(endsWith("hello world", ""));
  EXPECT_TRUE(endsWith("world", "world"));
  EXPECT_FALSE(endsWith("hello", "world"));
  EXPECT_FALSE(endsWith("hi", "hello"));
  EXPECT_FALSE(endsWith("", "hello"));
}

TEST(StringExtrasTest, Trim) {
  EXPECT_EQ(trim("  hello  "), "hello");
  EXPECT_EQ(trim("\t\nhello\r\n"), "hello");
  EXPECT_EQ(trim("hello"), "hello");
  EXPECT_EQ(trim(""), "");
  EXPECT_EQ(trim("   "), "");
  EXPECT_EQ(trim("  hello world  "), "hello world");
}

TEST(StringExtrasTest, CombinedOperations) {
  string test = "  HELLO WORLD  ";
  string result = trim(toLower(test));
  EXPECT_EQ(result, "hello world");
  
  EXPECT_TRUE(startsWith(toUpper("hello"), "HE"));
  EXPECT_TRUE(endsWith(toLower("WORLD"), "ld"));
}