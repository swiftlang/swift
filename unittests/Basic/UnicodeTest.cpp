//===--- UnicodeTest.cpp --------------------------------------------------===//
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

#include "swift/Basic/Unicode.h"
#include "gtest/gtest.h"

using namespace swift::unicode;

TEST(ExtractExtendedGraphemeCluster, Test1) {
  EXPECT_EQ("", extractFirstExtendedGraphemeCluster(""));
  EXPECT_EQ("a", extractFirstExtendedGraphemeCluster("a"));
  EXPECT_EQ("a", extractFirstExtendedGraphemeCluster("abc"));
}

TEST(IsSingleExtendedGraphemeCluster, Test1) {
  EXPECT_EQ(false, isSingleExtendedGraphemeCluster(""));
  EXPECT_EQ(true, isSingleExtendedGraphemeCluster("a"));
}

TEST(IsSingleExtendedGraphemeCluster, IndicConjuncts) {
  // GB9c: Consonant [Extend Linker]* Linker [Extend Linker]* x Consonant.
  // Devanagari KA + VIRAMA + SSA.
  EXPECT_EQ(true, isSingleExtendedGraphemeCluster("क्ष"));
  // Devanagari SA + VIRAMA + TA + VIRAMA + RA.
  EXPECT_EQ(true,
            isSingleExtendedGraphemeCluster("स्त्र"));
  // Bengali and Malayalam conjuncts.
  EXPECT_EQ(true, isSingleExtendedGraphemeCluster("ক্ষ"));
  EXPECT_EQ(true, isSingleExtendedGraphemeCluster("ക്ക"));
  // An Extend scalar (NUKTA) or a ZWJ may appear before and after the linker.
  EXPECT_EQ(true, isSingleExtendedGraphemeCluster("क़्ष"));
  EXPECT_EQ(true, isSingleExtendedGraphemeCluster("क्‍ष"));

  // A linker without a following consonant is still a single cluster.
  EXPECT_EQ(true, isSingleExtendedGraphemeCluster("क्"));

  // Negative cases: the sequence is not a conjunct, so there is a break.
  // Consonant, linker, then a non-consonant.
  EXPECT_EQ(false, isSingleExtendedGraphemeCluster("क्a"));
  // Two consonants with no linker between them.
  EXPECT_EQ(false, isSingleExtendedGraphemeCluster("कष"));
  // ZWNJ is not InCB=Extend, so it ends the sequence.
  EXPECT_EQ(false, isSingleExtendedGraphemeCluster("क्‌ष"));
  // A linker that does not follow a consonant.
  EXPECT_EQ(false, isSingleExtendedGraphemeCluster("्ष"));
  EXPECT_EQ(false, isSingleExtendedGraphemeCluster("a्ष"));
}

TEST(ExtractExtendedGraphemeCluster, IndicConjuncts) {
  // Only the first conjunct is extracted; a second one starts a new cluster.
  EXPECT_EQ("क्ष",
            extractFirstExtendedGraphemeCluster("क्षक"));
  EXPECT_EQ("क्ष",
            extractFirstExtendedGraphemeCluster("क्ष क"));
}
