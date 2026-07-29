//===--- SwiftNameTranslationTests.cpp ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/SwiftNameTranslation.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(SwiftNameTranslation, IsValidCxxIdentifier) {
  EXPECT_TRUE(cxx_translation::isValidCxxIdentifier("x"));
  EXPECT_TRUE(cxx_translation::isValidCxxIdentifier("_x"));
  EXPECT_TRUE(cxx_translation::isValidCxxIdentifier("x1"));
  EXPECT_TRUE(cxx_translation::isValidCxxIdentifier("helloWorld"));
  // '$' is accepted as an extension by the major C++ compilers.
  EXPECT_TRUE(cxx_translation::isValidCxxIdentifier("$x"));

  EXPECT_FALSE(cxx_translation::isValidCxxIdentifier(""));
  EXPECT_FALSE(cxx_translation::isValidCxxIdentifier("1"));
  EXPECT_FALSE(cxx_translation::isValidCxxIdentifier("1x"));
  EXPECT_FALSE(cxx_translation::isValidCxxIdentifier("hello world"));
  EXPECT_FALSE(cxx_translation::isValidCxxIdentifier("~"));
  EXPECT_FALSE(cxx_translation::isValidCxxIdentifier("Ü"));
}

TEST(SwiftNameTranslation, SanitizeNameForCxx) {
  // Valid identifiers are unchanged.
  EXPECT_EQ(cxx_translation::sanitizeNameForCxx("helloWorld"), "helloWorld");
  EXPECT_EQ(cxx_translation::sanitizeNameForCxx("_1"), "_1");
  EXPECT_EQ(cxx_translation::sanitizeNameForCxx("$x"), "$x");

  // A leading digit is preceded by an underscore.
  EXPECT_EQ(cxx_translation::sanitizeNameForCxx("1"), "_1");
  EXPECT_EQ(cxx_translation::sanitizeNameForCxx("2nd"), "_2nd");

  // Characters that are not valid in a C++ identifier are replaced with
  // their Unicode scalar values.
  EXPECT_EQ(cxx_translation::sanitizeNameForCxx("hello world"),
            "hello_u0020world");
  EXPECT_EQ(cxx_translation::sanitizeNameForCxx("~"), "_u007E");
  EXPECT_EQ(cxx_translation::sanitizeNameForCxx("+ - *"),
            "_u002B_u0020_u002D_u0020_u002A");

  // Non-ASCII characters are replaced with their Unicode scalar values, which
  // are always four hexadecimal digits for scalars in the basic multilingual
  // plane and eight hexadecimal digits for larger scalars.
  EXPECT_EQ(cxx_translation::sanitizeNameForCxx("Ü"), "_u00DC");
  EXPECT_EQ(cxx_translation::sanitizeNameForCxx("über"), "_u00FCber");
  EXPECT_EQ(cxx_translation::sanitizeNameForCxx("日本語"),
            "_u65E5_u672C_u8A9E");
  EXPECT_EQ(cxx_translation::sanitizeNameForCxx("🚀speed"),
            "_U0001F680speed");

  // A digit after a replaced character is not a leading digit.
  EXPECT_EQ(cxx_translation::sanitizeNameForCxx(" 1"), "_u00201");
}
