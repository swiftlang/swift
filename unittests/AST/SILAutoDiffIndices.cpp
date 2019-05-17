//===--- SILAutoDiffIndices.cpp - Tests SILAutoDiffIndices ----------------===//
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
// SWIFT_ENABLE_TENSORFLOW

#include "swift/AST/AutoDiff.h"
#include "TestContext.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

TEST(AutoDiffIndexSubset, NumBitWordsNeeded) {
  EXPECT_EQ(AutoDiffIndexSubset::getNumBitWordsNeededForCapacity(0), 0u);
  EXPECT_EQ(AutoDiffIndexSubset::getNumBitWordsNeededForCapacity(1), 1u);
  EXPECT_EQ(AutoDiffIndexSubset::getNumBitWordsNeededForCapacity(5), 1u);
  EXPECT_EQ(AutoDiffIndexSubset::getNumBitWordsNeededForCapacity(
                AutoDiffIndexSubset::numBitsPerBitWord - 1), 1u);
  EXPECT_EQ(AutoDiffIndexSubset::getNumBitWordsNeededForCapacity(
                AutoDiffIndexSubset::numBitsPerBitWord), 2u);
  EXPECT_EQ(AutoDiffIndexSubset::getNumBitWordsNeededForCapacity(
                AutoDiffIndexSubset::numBitsPerBitWord * 2 - 1), 2u);
  EXPECT_EQ(AutoDiffIndexSubset::getNumBitWordsNeededForCapacity(
                AutoDiffIndexSubset::numBitsPerBitWord * 2), 3u);
}

TEST(AutoDiffIndexSubset, BitWordIndexAndOffset) {
  EXPECT_EQ(AutoDiffIndexSubset::getBitWordIndexAndOffset(0),
            std::make_pair(0u, 0u));
  EXPECT_EQ(AutoDiffIndexSubset::getBitWordIndexAndOffset(5),
            std::make_pair(0u, 5u));
  EXPECT_EQ(AutoDiffIndexSubset::getBitWordIndexAndOffset(8),
            std::make_pair(0u, 8u));
  EXPECT_EQ(AutoDiffIndexSubset::getBitWordIndexAndOffset(
            AutoDiffIndexSubset::numBitsPerBitWord - 1),
            std::make_pair(0u, AutoDiffIndexSubset::numBitsPerBitWord - 1));
  EXPECT_EQ(AutoDiffIndexSubset::getBitWordIndexAndOffset(
                AutoDiffIndexSubset::numBitsPerBitWord),
            std::make_pair(1u, 0u));
}

TEST(AutoDiffIndexSubset, Equality) {
  TestContext ctx;
  EXPECT_EQ(AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                     /*indices*/ {0}),
            AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                     /*indices*/ {0}));
  EXPECT_EQ(AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                     /*indices*/ {0, 2, 4}),
            AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                     /*indices*/ {0, 2, 4}));
  EXPECT_EQ(AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                     /*indices*/ {}),
            AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                     /*indices*/ {}));
  EXPECT_NE(AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 1,
                                     /*indices*/ {}),
            AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 0,
                                     /*indices*/ {}));
  EXPECT_NE(AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                     /*indices*/ {0}),
            AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                     /*indices*/ {}));
}

TEST(AutoDiffIndexSubset, Bits) {
  TestContext ctx;
  auto *indices1 = AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                            /*indices*/ {0, 2, 4});
  EXPECT_EQ(indices1->getNumBitWords(), 1u);
  EXPECT_EQ(indices1->getCapacity(), 5u);
  EXPECT_TRUE(indices1->contains(0));
  EXPECT_FALSE(indices1->contains(1));
  EXPECT_TRUE(indices1->contains(2));
  EXPECT_FALSE(indices1->contains(3));
  EXPECT_TRUE(indices1->contains(4));

  auto *indices2 = AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                            /*indices*/ {1, 3});
  EXPECT_EQ(indices2->getNumBitWords(), 1u);
  EXPECT_EQ(indices2->getCapacity(), 5u);
  EXPECT_FALSE(indices2->contains(0));
  EXPECT_TRUE(indices2->contains(1));
  EXPECT_FALSE(indices2->contains(2));
  EXPECT_TRUE(indices2->contains(3));
  EXPECT_FALSE(indices2->contains(4));
}

TEST(AutoDiffIndexSubset, Iteration) {
  TestContext ctx;
  // Test 1
  {
    auto *indices1 = AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                              /*indices*/ {0, 2, 4});
    // Check forward iteration.
    EXPECT_EQ(indices1->findFirst(), 0);
    EXPECT_EQ(indices1->findNext(0), 2);
    EXPECT_EQ(indices1->findNext(2), 4);
    EXPECT_EQ(indices1->findNext(4), (int)indices1->getCapacity());
    // Check reverse iteration.
    EXPECT_EQ(indices1->findLast(), 4);
    EXPECT_EQ(indices1->findPrevious(4), 2);
    EXPECT_EQ(indices1->findPrevious(2), 0);
    EXPECT_EQ(indices1->findPrevious(0), -1);
    // Check range.
    unsigned indices1Elements[3] = {0, 2, 4};
    EXPECT_TRUE(std::equal(indices1->begin(), indices1->end(),
                           indices1Elements));
  }
  // Test 2
  {
    auto *indices2 = AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                              /*indices*/ {1, 3});
    // Check forward iteration.
    EXPECT_EQ(indices2->findFirst(), 1);
    EXPECT_EQ(indices2->findNext(1), 3);
    EXPECT_EQ(indices2->findNext(3), (int)indices2->getCapacity());
    // Check reverse iteration.
    EXPECT_EQ(indices2->findLast(), 3);
    EXPECT_EQ(indices2->findPrevious(3), 1);
    EXPECT_EQ(indices2->findPrevious(1), -1);
    // Check range.
    unsigned indices2Elements[2] = {1, 3};
    EXPECT_TRUE(std::equal(indices2->begin(), indices2->end(),
                           indices2Elements));
  }
}

TEST(AutoDiffIndexSubset, SupersetAndSubset) {
  TestContext ctx;
  auto *indices1 = AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                            /*indices*/ {0, 2, 4});
  EXPECT_TRUE(indices1->isSupersetOf(indices1));
  EXPECT_TRUE(indices1->isSubsetOf(indices1));
  auto *indices2 = AutoDiffIndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                            /*indices*/ {2});
  EXPECT_TRUE(indices2->isSupersetOf(indices2));
  EXPECT_TRUE(indices2->isSubsetOf(indices2));

  EXPECT_TRUE(indices1->isSupersetOf(indices2));
  EXPECT_TRUE(indices2->isSubsetOf(indices1));
}

TEST(AutoDiffIndexSubset, Insertion) {
  TestContext ctx;
  auto *indices1 = AutoDiffIndexSubset::get(ctx.Ctx, 5, {0, 2, 4});
  EXPECT_EQ(indices1->adding(0, ctx.Ctx), indices1);
  EXPECT_EQ(indices1->adding(1, ctx.Ctx),
            AutoDiffIndexSubset::get(ctx.Ctx, 5, {0, 1, 2, 4}));
  EXPECT_EQ(indices1->adding(3, ctx.Ctx),
            AutoDiffIndexSubset::get(ctx.Ctx, 5, {0, 2, 3, 4}));
}
