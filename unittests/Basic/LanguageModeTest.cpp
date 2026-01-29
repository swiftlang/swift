//===--- Basic/LanguageModeTest.cpp -----------------------------*- C++ -*-===//
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

#include "swift/Basic/LanguageMode.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(LanguageMode, isFuture) {
  EXPECT_FALSE(LanguageMode::v4.isFuture());
  EXPECT_FALSE(LanguageMode::v4_2.isFuture());
  EXPECT_FALSE(LanguageMode::v5.isFuture());
  EXPECT_FALSE(LanguageMode::v6.isFuture());
  EXPECT_TRUE(LanguageMode::future.isFuture());
}

TEST(LanguageMode, versionString) {
  EXPECT_EQ(LanguageMode::v4.versionString(), "4");
  EXPECT_EQ(LanguageMode::v4_2.versionString(), "4.2");
  EXPECT_EQ(LanguageMode::v5.versionString(), "5");
  EXPECT_EQ(LanguageMode::v6.versionString(), "6");
  EXPECT_EQ(LanguageMode::future.versionString(), "7");
}

TEST(LanguageMode, allSupportedModes) {
  const auto modes = LanguageMode::allSupportedModes();
  EXPECT_EQ(modes[0], LanguageMode::v4);
  EXPECT_EQ(modes[1], LanguageMode::v4_2);
  EXPECT_EQ(modes[2], LanguageMode::v5);
  EXPECT_EQ(modes[3], LanguageMode::v6);
}

TEST(LanguageMode, version) {
  EXPECT_EQ(LanguageMode::v4.version(), (std::pair<unsigned, unsigned>(4, 0)));
  EXPECT_EQ(LanguageMode::v4_2.version(),
            (std::pair<unsigned, unsigned>(4, 2)));
  EXPECT_EQ(LanguageMode::v5.version(), (std::pair<unsigned, unsigned>(5, 0)));
  EXPECT_EQ(LanguageMode::v6.version(), (std::pair<unsigned, unsigned>(6, 0)));
}

TEST(LanguageMode, isEffectiveIn) {
  EXPECT_FALSE(LanguageMode::v4.isEffectiveIn({}));
  EXPECT_FALSE(LanguageMode::v4.isEffectiveIn({1}));
  EXPECT_FALSE(LanguageMode::v4.isEffectiveIn({3, 0}));
  EXPECT_TRUE(LanguageMode::v4.isEffectiveIn({4, 0}));
  EXPECT_TRUE(LanguageMode::v4.isEffectiveIn({4, 50, 1}));
  EXPECT_TRUE(LanguageMode::v4.isEffectiveIn({9, 4}));

  EXPECT_FALSE(LanguageMode::v4_2.isEffectiveIn({}));
  EXPECT_FALSE(LanguageMode::v4_2.isEffectiveIn({3, 999}));
  EXPECT_FALSE(LanguageMode::v4_2.isEffectiveIn({4, 0}));
  EXPECT_TRUE(LanguageMode::v4_2.isEffectiveIn({4, 2, 0}));
  EXPECT_TRUE(LanguageMode::v4_2.isEffectiveIn({4, 2, 3}));
  EXPECT_TRUE(LanguageMode::v4_2.isEffectiveIn({9, 4}));

  EXPECT_FALSE(LanguageMode::v5.isEffectiveIn({}));
  EXPECT_FALSE(LanguageMode::v5.isEffectiveIn({2}));
  EXPECT_FALSE(LanguageMode::v5.isEffectiveIn({4, 0}));
  EXPECT_TRUE(LanguageMode::v5.isEffectiveIn({5}));
  EXPECT_TRUE(LanguageMode::v5.isEffectiveIn({5, 10}));
  EXPECT_TRUE(LanguageMode::v5.isEffectiveIn({6, 1}));

  EXPECT_FALSE(LanguageMode::v6.isEffectiveIn({4}));
  EXPECT_FALSE(LanguageMode::v6.isEffectiveIn({5, 999}));
  EXPECT_TRUE(LanguageMode::v6.isEffectiveIn({6, 0, 0, 0}));
  EXPECT_TRUE(LanguageMode::v6.isEffectiveIn({9, 9, 9, 9}));

  EXPECT_FALSE(LanguageMode::future.isEffectiveIn({}));
  EXPECT_FALSE(LanguageMode::future.isEffectiveIn({4, 0}));

  // These two must be false once 'future' is untied from Swift 7.
  EXPECT_TRUE(LanguageMode::future.isEffectiveIn({8, 0}));
  EXPECT_TRUE(LanguageMode::future.isEffectiveIn({999}));
}
