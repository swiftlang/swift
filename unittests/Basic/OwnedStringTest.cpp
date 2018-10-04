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
  OwnedString ownedString = OwnedString::makeUnowned(data);

  EXPECT_EQ(length, ownedString.size());
  EXPECT_TRUE(ownedString.empty());
  EXPECT_EQ(data, ownedString.str().data());
}

TEST(OwnedStringTest, char_pointer_non_empty) {
  const char *data = "string";
  const size_t length = strlen(data);
  OwnedString ownedString = OwnedString::makeUnowned(data);

  EXPECT_EQ(length, ownedString.size());
  EXPECT_FALSE(ownedString.empty());
  EXPECT_EQ(data, ownedString.str().data());
}

TEST(OwnedStringTest, ref_counted_copies_buffer) {
  char *data = static_cast<char *>(malloc(6));
  memcpy(data, "hello", 6);
  size_t length = strlen(data);

  OwnedString ownedString =
      OwnedString::makeRefCounted(StringRef(data, length));

  EXPECT_EQ(ownedString.str(), "hello");
  EXPECT_NE(ownedString.str().data(), data);

  memcpy(data, "world", 6);

  // Even if the original buffer changes, the string should stay the same
  EXPECT_EQ(ownedString.str(), "hello");
}

TEST(OwnedStringTest, ref_counted_assignment) {
  OwnedString str = OwnedString::makeRefCounted("hello");
  OwnedString copy = str;

  EXPECT_EQ(str.str().data(), copy.str().data());
}
