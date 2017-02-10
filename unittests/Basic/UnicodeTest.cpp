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
