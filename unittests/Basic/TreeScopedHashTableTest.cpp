//===--- TreeScopedHashTableTest.cpp --------------------------------------===//
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

#include "swift/Basic/TreeScopedHashTable.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(TreeScopedHashTableTest, T1) {
  using HashtableTy = TreeScopedHashTable<unsigned, unsigned>;
  using ScopeTy = HashtableTy::ScopeTy;
  HashtableTy HT;

  auto S1 = new ScopeTy(HT, 0);
  HT.insertIntoScope(*S1, 1, 1001);
  HT.insertIntoScope(*S1, 2, 1002);

  auto S2 = new ScopeTy(HT, S1);
  HT.insertIntoScope(*S2, 3, 2003);
  HT.insertIntoScope(*S2, 4, 2004);

  EXPECT_EQ(1001U, HT.lookup(*S1, 1));
  EXPECT_EQ(1002U, HT.lookup(*S1, 2));
  EXPECT_EQ(0U,    HT.lookup(*S1, 3));
  EXPECT_EQ(0U,    HT.lookup(*S1, 4));

  EXPECT_EQ(1001U, HT.lookup(*S2, 1));
  EXPECT_EQ(1002U, HT.lookup(*S2, 2));
  EXPECT_EQ(2003U, HT.lookup(*S2, 3));
  EXPECT_EQ(2004U, HT.lookup(*S2, 4));

  delete S2;
  delete S1;
}

TEST(TreeScopedHashTableTest, T2) {
  using HashtableTy = TreeScopedHashTable<unsigned, unsigned>;
  using ScopeTy = HashtableTy::ScopeTy;
  HashtableTy HT;

  auto S1 = new ScopeTy(HT, 0);
  HT.insertIntoScope(*S1, 1, 1001);
  HT.insertIntoScope(*S1, 2, 1002);

  auto S2 = new ScopeTy(HT, S1);
  HT.insertIntoScope(*S2, 3, 2003);
  HT.insertIntoScope(*S2, 4, 2004);

  auto S3 = new ScopeTy(HT, S1);
  HT.insertIntoScope(*S3, 1, 3001);
  HT.insertIntoScope(*S3, 4, 3004);

  EXPECT_EQ(1001U, HT.lookup(*S1, 1));
  EXPECT_EQ(1002U, HT.lookup(*S1, 2));
  EXPECT_EQ(0U,    HT.lookup(*S1, 3));
  EXPECT_EQ(0U,    HT.lookup(*S1, 4));

  EXPECT_EQ(1001U, HT.lookup(*S2, 1));
  EXPECT_EQ(1002U, HT.lookup(*S2, 2));
  EXPECT_EQ(2003U, HT.lookup(*S2, 3));
  EXPECT_EQ(2004U, HT.lookup(*S2, 4));

  EXPECT_EQ(3001U, HT.lookup(*S3, 1));
  EXPECT_EQ(1002U, HT.lookup(*S3, 2));
  EXPECT_EQ(0U,    HT.lookup(*S3, 3));
  EXPECT_EQ(3004U, HT.lookup(*S3, 4));

  EXPECT_EQ(true,  HT.count(*S3, 1));
  EXPECT_EQ(true,  HT.count(*S3, 2));
  EXPECT_EQ(false, HT.count(*S3, 3));
  EXPECT_EQ(true,  HT.count(*S3, 4));

  delete S2;

  EXPECT_EQ(1001U, HT.lookup(*S1, 1));
  EXPECT_EQ(1002U, HT.lookup(*S1, 2));
  EXPECT_EQ(0U,    HT.lookup(*S1, 3));
  EXPECT_EQ(0U,    HT.lookup(*S1, 4));

  EXPECT_EQ(3001U, HT.lookup(*S3, 1));
  EXPECT_EQ(1002U, HT.lookup(*S3, 2));
  EXPECT_EQ(0U,    HT.lookup(*S3, 3));
  EXPECT_EQ(3004U, HT.lookup(*S3, 4));

  delete S3;

  EXPECT_EQ(1001U, HT.lookup(*S1, 1));
  EXPECT_EQ(1002U, HT.lookup(*S1, 2));
  EXPECT_EQ(0U,    HT.lookup(*S1, 3));
  EXPECT_EQ(0U,    HT.lookup(*S1, 4));

  delete S1;
}

TEST(TreeScopedHashTableTest, IteratorTest) {
  using HashtableTy = TreeScopedHashTable<unsigned, int>;
  using ScopeTy = HashtableTy::ScopeTy;
  HashtableTy HT;

  auto S1 = new ScopeTy(HT, 0);
  HT.insertIntoScope(*S1, 1, 1001);
  HT.insertIntoScope(*S1, 2, 1002);

  auto S2 = new ScopeTy(HT, S1);
  HT.insertIntoScope(*S2, 1, 2001);
  HT.insertIntoScope(*S2, 4, 2004);

  auto S3 = new ScopeTy(HT, S1);
  HT.insertIntoScope(*S3, 1, 3001);
  HT.insertIntoScope(*S3, 4, 3004);

  {
    auto I = HT.begin(*S1, 1);
    EXPECT_EQ(1001, *I);
    I++;
    EXPECT_EQ(HT.end(), I);
  }
  {
    auto I = HT.begin(*S3, 1);
    EXPECT_EQ(3001, *I);
    I++;
    EXPECT_EQ(1001, *I);
    I++;
    EXPECT_EQ(HT.end(), I);
  }

  delete S3;
  delete S2;
  delete S1;
}

