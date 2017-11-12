//===--- OwnedStringTest.cpp ----------------------------------------------===//
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

#include "swift/Basic/OwnedString.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(OwnedStringTest, char_pointer_empty) {
  const char *data = "";
  const size_t length = strlen(data);
  OwnedString ownedString(data);

  EXPECT_EQ(length, ownedString.size());
  EXPECT_TRUE(ownedString.empty());

  OwnedString copy = ownedString.copy();
  EXPECT_EQ(length, copy.size());
  EXPECT_TRUE(copy.empty());

  StringRef str = copy.str();
  EXPECT_EQ("", str);
  EXPECT_EQ(length, str.size());
}

TEST(OwnedStringTest, char_pointer_non_empty) {
  const char *data = "string";
  const size_t length = strlen(data);
  OwnedString ownedString(data);

  EXPECT_EQ(length, ownedString.size());
  EXPECT_FALSE(ownedString.empty());

  OwnedString copy = ownedString.copy();
  EXPECT_EQ(length, copy.size());
  EXPECT_FALSE(copy.empty());

  StringRef str = copy.str();
  EXPECT_EQ("string", str);
  EXPECT_EQ(length, strlen(str.data()));
}

TEST(OwnedStringTest, char_pointer_length_equal) {
  const char *data = "string";
  size_t length = strlen(data);
  OwnedString ownedString(data, length);

  EXPECT_EQ(length, ownedString.size());
  EXPECT_FALSE(ownedString.empty());

  OwnedString copy = ownedString.copy();
  EXPECT_EQ(length, copy.size());
  EXPECT_FALSE(copy.empty());

  // Make sure we correctly copied the data and that it is null
  // terminated.
  StringRef str = copy.str();
  EXPECT_EQ("string", str);
  EXPECT_EQ(length, strlen(str.data()));
}

TEST(OwnedStringTest, char_pointer_length_nonzero) {
  const char *data = "string";
  const size_t length = 1;
  OwnedString ownedString(data, length);

  EXPECT_EQ(length, ownedString.size());
  EXPECT_FALSE(ownedString.empty());

  OwnedString copy = ownedString.copy();
  EXPECT_EQ(length, copy.size());
  EXPECT_FALSE(copy.empty());

  // Make sure we correctly copied the data and that it is null
  // terminated.
  StringRef str = copy.str();
  EXPECT_EQ("s", str);
  EXPECT_EQ(1UL, strlen(str.data()));
}

TEST(OwnedStringTest, char_pointer_length_zero) {
  const char *data = "string";
  const size_t length = 0;
  OwnedString ownedString(data, length);

  EXPECT_EQ(length, ownedString.size());
  EXPECT_TRUE(ownedString.empty());

  OwnedString copy = ownedString.copy();
  EXPECT_EQ(length, copy.size());
  EXPECT_TRUE(copy.empty());
}

TEST(OwnedStringTest, copy_original_new_different) {
  // Initialize a mutable string.
  const char *original = "string";
  const size_t length = strlen(original);
  char *data = static_cast<char *>(malloc(length + 1));
  memcpy(data, original, length);
  data[length] = '\0';

  // Create an OwnedString.
  OwnedString ownedString(data, length);

  EXPECT_EQ(length, ownedString.size());
  EXPECT_FALSE(ownedString.empty());

  // Copy the string
  OwnedString copy = ownedString.copy();
  EXPECT_EQ(length, copy.size());
  EXPECT_FALSE(copy.empty());

  // Make sure we correctly copied the data and that it is null
  // terminated.
  StringRef str = copy.str();
  EXPECT_EQ("string", str);
  EXPECT_EQ(length, strlen(str.data()));

  // Make sure updating the original pointer doesn't affect the copy.
  data[0] = 'a';

  EXPECT_EQ("string", str);
}

TEST(OwnedStringTest, copy_constructor_original_not_copy) {
  // Initialize a mutable string.
  const char *original = "string";
  const size_t length = strlen(original);
  char *data = static_cast<char *>(malloc(length + 1));
  memcpy(data, original, length);
  data[length] = '\0';

  // Create an OwnedString.
  OwnedString ownedString(data, length);

  EXPECT_EQ(length, ownedString.size());
  EXPECT_FALSE(ownedString.empty());

  // Copy the string
  OwnedString copy = OwnedString(ownedString);
  EXPECT_EQ(length, copy.size());
  EXPECT_FALSE(copy.empty());

  // Make sure we correctly copied the data and that it is null
  // terminated.
  StringRef str = copy.str();
  EXPECT_EQ("string", str);
  EXPECT_EQ(length, strlen(str.data()));

  // Make sure updating the original pointer doesn't affect the copy.
  data[0] = 'a';

  EXPECT_EQ("atring", str);
}

TEST(OwnedStringTest, copy_constructor_original_copy) {
  // Initialize a mutable string.
  const char *original = "string";
  const size_t length = strlen(original);
  char *data = static_cast<char *>(malloc(length + 1));
  memcpy(data, original, length);
  data[length] = '\0';

  // Create an OwnedString.
  OwnedString ownedString(data, length);

  EXPECT_EQ(length, ownedString.size());
  EXPECT_FALSE(ownedString.empty());

  // Copy the string
  OwnedString copy = OwnedString(ownedString.copy());
  EXPECT_EQ(length, copy.size());
  EXPECT_FALSE(copy.empty());

  // Make sure we correctly copied the data and that it is null
  // terminated.
  StringRef str = copy.str();
  EXPECT_EQ("string", str);
  EXPECT_EQ(length, strlen(str.data()));

  // Make sure updating the original pointer doesn't affect the copy.
  data[0] = 'a';

  EXPECT_EQ("string", str);
}
