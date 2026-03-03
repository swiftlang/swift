//===--------------------- IndexSubsetTests.cpp ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/IndexSubset.h"
#include "TestContext.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

TEST(IndexSubset, NumBitWordsNeeded) {
  EXPECT_EQ(IndexSubset::getNumBitWordsNeededForCapacity(0), 0u);
  EXPECT_EQ(IndexSubset::getNumBitWordsNeededForCapacity(1), 1u);
  EXPECT_EQ(IndexSubset::getNumBitWordsNeededForCapacity(5), 1u);
  EXPECT_EQ(IndexSubset::getNumBitWordsNeededForCapacity(
                IndexSubset::numBitsPerBitWord - 1), 1u);
  EXPECT_EQ(IndexSubset::getNumBitWordsNeededForCapacity(
                IndexSubset::numBitsPerBitWord), 2u);
  EXPECT_EQ(IndexSubset::getNumBitWordsNeededForCapacity(
                IndexSubset::numBitsPerBitWord * 2 - 1), 2u);
  EXPECT_EQ(IndexSubset::getNumBitWordsNeededForCapacity(
                IndexSubset::numBitsPerBitWord * 2), 3u);
}

TEST(IndexSubset, BitWordIndexAndOffset) {
  EXPECT_EQ(IndexSubset::getBitWordIndexAndOffset(0),
            std::make_pair(0u, 0u));
  EXPECT_EQ(IndexSubset::getBitWordIndexAndOffset(5),
            std::make_pair(0u, 5u));
  EXPECT_EQ(IndexSubset::getBitWordIndexAndOffset(8),
            std::make_pair(0u, 8u));
  EXPECT_EQ(IndexSubset::getBitWordIndexAndOffset(
            IndexSubset::numBitsPerBitWord - 1),
            std::make_pair(0u, IndexSubset::numBitsPerBitWord - 1));
  EXPECT_EQ(IndexSubset::getBitWordIndexAndOffset(
                IndexSubset::numBitsPerBitWord),
            std::make_pair(1u, 0u));
}

TEST(IndexSubset, Equality) {
  TestContext ctx;
  EXPECT_EQ(IndexSubset::get(ctx.Ctx, /*capacity*/ 5, /*indices*/ {0}),
            IndexSubset::get(ctx.Ctx, /*capacity*/ 5, /*indices*/ {0}));
  EXPECT_EQ(IndexSubset::get(ctx.Ctx, /*capacity*/ 5, /*indices*/ {0, 2, 4}),
            IndexSubset::get(ctx.Ctx, /*capacity*/ 5, /*indices*/ {0, 2, 4}));
  EXPECT_EQ(IndexSubset::get(ctx.Ctx, /*capacity*/ 5, /*indices*/ {}),
            IndexSubset::get(ctx.Ctx, /*capacity*/ 5, /*indices*/ {}));
  EXPECT_NE(IndexSubset::get(ctx.Ctx, /*capacity*/ 1, /*indices*/ {}),
            IndexSubset::get(ctx.Ctx, /*capacity*/ 0, /*indices*/ {}));
  EXPECT_NE(IndexSubset::get(ctx.Ctx, /*capacity*/ 5, /*indices*/ {0}),
            IndexSubset::get(ctx.Ctx, /*capacity*/ 5, /*indices*/ {}));
}

TEST(IndexSubset, Initializers) {
  TestContext ctx;
  // Default init.
  EXPECT_EQ(IndexSubset::getDefault(ctx.Ctx, /*capacity*/ 5,
                                    /*includeAll*/ true),
            IndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                             /*indices*/ {0, 1, 2, 3, 4}));
  EXPECT_EQ(IndexSubset::getDefault(ctx.Ctx, /*capacity*/ 5,
                                    /*includeAll*/ false),
            IndexSubset::get(ctx.Ctx, /*capacity*/ 5, /*indices*/ {}));
  EXPECT_EQ(IndexSubset::getDefault(ctx.Ctx, /*capacity*/ 0,
                                    /*includeAll*/ true),
            IndexSubset::get(ctx.Ctx, /*capacity*/ 0, /*indices*/ {}));
  EXPECT_EQ(IndexSubset::getDefault(ctx.Ctx, /*capacity*/ 0,
                                    /*includeAll*/ false),
            IndexSubset::get(ctx.Ctx, /*capacity*/ 0, /*indices*/ {}));
  // Bit vector init.
  {
    llvm::SmallBitVector bitVec(6);
    bitVec.set(1, 4);
    EXPECT_EQ(IndexSubset::get(ctx.Ctx, bitVec),
              IndexSubset::get(ctx.Ctx, /*capacity*/ 6, /*indices*/ {1, 2, 3}));
  }
  {
    llvm::SmallBitVector bitVec(0);
    EXPECT_EQ(IndexSubset::get(ctx.Ctx, bitVec),
              IndexSubset::get(ctx.Ctx, /*capacity*/ 0, /*indices*/ {}));
  }
  // String init.
  EXPECT_EQ(IndexSubset::getFromString(ctx.Ctx, "SSSSS"),
            IndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                     /*indices*/ {0, 1, 2, 3, 4}));
  EXPECT_EQ(IndexSubset::getFromString(ctx.Ctx, "UUUUU"),
            IndexSubset::get(ctx.Ctx, /*capacity*/ 5, /*indices*/ {}));
  EXPECT_EQ(IndexSubset::getFromString(ctx.Ctx, "SUSUS"),
            IndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                     /*indices*/ {0, 2, 4}));
  EXPECT_EQ(IndexSubset::getFromString(ctx.Ctx, ""),
            IndexSubset::get(ctx.Ctx, /*capacity*/ 0, /*indices*/ {}));
}

TEST(IndexSubset, Bits) {
  TestContext ctx;
  auto *indices1 = IndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                    /*indices*/ {0, 2, 4});
  EXPECT_EQ(indices1->getNumBitWords(), 1u);
  EXPECT_EQ(indices1->getCapacity(), 5u);
  EXPECT_TRUE(indices1->contains(0));
  EXPECT_FALSE(indices1->contains(1));
  EXPECT_TRUE(indices1->contains(2));
  EXPECT_FALSE(indices1->contains(3));
  EXPECT_TRUE(indices1->contains(4));

  auto *indices2 = IndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                    /*indices*/ {1, 3});
  EXPECT_EQ(indices2->getNumBitWords(), 1u);
  EXPECT_EQ(indices2->getCapacity(), 5u);
  EXPECT_FALSE(indices2->contains(0));
  EXPECT_TRUE(indices2->contains(1));
  EXPECT_FALSE(indices2->contains(2));
  EXPECT_TRUE(indices2->contains(3));
  EXPECT_FALSE(indices2->contains(4));
}

TEST(IndexSubset, Iteration) {
  TestContext ctx;
  // Test 1
  {
    auto *indices1 = IndexSubset::get(ctx.Ctx, /*capacity*/ 5,
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
    auto *indices2 = IndexSubset::get(ctx.Ctx, /*capacity*/ 5,
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

TEST(IndexSubset, SupersetAndSubset) {
  TestContext ctx;
  auto *indices1 = IndexSubset::get(ctx.Ctx, /*capacity*/ 5,
                                    /*indices*/ {0, 2, 4});
  EXPECT_TRUE(indices1->isSupersetOf(indices1));
  EXPECT_TRUE(indices1->isSubsetOf(indices1));
  auto *indices2 = IndexSubset::get(ctx.Ctx, /*capacity*/ 5, /*indices*/ {2});
  EXPECT_TRUE(indices2->isSupersetOf(indices2));
  EXPECT_TRUE(indices2->isSubsetOf(indices2));

  EXPECT_TRUE(indices1->isSupersetOf(indices2));
  EXPECT_TRUE(indices2->isSubsetOf(indices1));
}

TEST(IndexSubset, Insertion) {
  TestContext ctx;
  auto *indices1 = IndexSubset::get(ctx.Ctx, 5, {0, 2, 4});
  EXPECT_EQ(indices1->adding(0, ctx.Ctx), indices1);
  EXPECT_EQ(indices1->adding(1, ctx.Ctx),
            IndexSubset::get(ctx.Ctx, 5, {0, 1, 2, 4}));
  EXPECT_EQ(indices1->adding(3, ctx.Ctx),
            IndexSubset::get(ctx.Ctx, 5, {0, 2, 3, 4}));
}

TEST(IndexSubset, FindNext) {
  TestContext ctx;
  auto *indices1 = IndexSubset::get(ctx.Ctx, 5, {1, 2, 4});
  EXPECT_EQ(indices1->findFirst(), 1);
  EXPECT_EQ(indices1->findNext(/*startIndex*/ -1), 1);
  EXPECT_EQ(indices1->findNext(/*startIndex*/ 0), 1);
  EXPECT_EQ(indices1->findNext(/*startIndex*/ 1), 2);
  EXPECT_EQ(indices1->findNext(/*startIndex*/ 2), 4);
  EXPECT_EQ(indices1->findNext(/*startIndex*/ 3), 4);
}

TEST(IndexSubset, FindPrevious) {
  TestContext ctx;
  auto *indices1 = IndexSubset::get(ctx.Ctx, 5, {0, 2, 4});
  EXPECT_EQ(indices1->findLast(), 4);
  EXPECT_EQ(indices1->findPrevious(/*endIndex*/ 5), 4);
  EXPECT_EQ(indices1->findPrevious(/*endIndex*/ 4), 2);
  EXPECT_EQ(indices1->findPrevious(/*endIndex*/ 3), 2);
  EXPECT_EQ(indices1->findPrevious(/*endIndex*/ 2), 0);
  EXPECT_EQ(indices1->findPrevious(/*endIndex*/ 1), 0);
  EXPECT_EQ(indices1->findPrevious(/*endIndex*/ 0), -1);
}

TEST(IndexSubset, Lowering) {
  TestContext testCtx;
  auto &C = testCtx.Ctx;
  // ((T, T)) -> ()
  EXPECT_EQ(
      autodiff::getLoweredParameterIndices(
          IndexSubset::get(C, 1, {0}),
          FunctionType::get({
              FunctionType::Param(
                  TupleType::get({C.TheAnyType, C.TheAnyType}, C))},
              C.TheEmptyTupleType)),
      IndexSubset::get(C, 2, {0, 1}));
  // ((), (T, T)) -> ()
  EXPECT_EQ(
      autodiff::getLoweredParameterIndices(
          IndexSubset::get(C, 2, {1}),
          FunctionType::get({
              FunctionType::Param(C.TheEmptyTupleType),
              FunctionType::Param(
                  TupleType::get({C.TheAnyType, C.TheAnyType}, C))},
                                 C.TheEmptyTupleType)),
      IndexSubset::get(C, 2, {0, 1}));
  // (T, (T, T)) -> ()
  EXPECT_EQ(
    autodiff::getLoweredParameterIndices(
      IndexSubset::get(C, 2, {1}),
      FunctionType::get({
          FunctionType::Param(C.TheAnyType),
          FunctionType::Param(
            TupleType::get({C.TheAnyType, C.TheAnyType}, C))},
        C.TheEmptyTupleType)),
    IndexSubset::get(C, 3, {1, 2}));
  // (T, (T, T)) -> ()
  EXPECT_EQ(
    autodiff::getLoweredParameterIndices(
      IndexSubset::get(C, 2, {0, 1}),
      FunctionType::get({
          FunctionType::Param(C.TheAnyType),
          FunctionType::Param(
            TupleType::get({C.TheAnyType, C.TheAnyType}, C))},
        C.TheEmptyTupleType)),
    IndexSubset::get(C, 3, {0, 1, 2}));
  // (T, (T, T), (T, T), T) -> ()
  EXPECT_EQ(
    autodiff::getLoweredParameterIndices(
      IndexSubset::get(C, 4, {0, 1, 3}),
      FunctionType::get({
          FunctionType::Param(C.TheAnyType),
          FunctionType::Param(
            TupleType::get({C.TheAnyType, C.TheAnyType}, C)),
          FunctionType::Param(
            TupleType::get({C.TheAnyType, C.TheAnyType}, C)),
          FunctionType::Param(C.TheAnyType)},
        C.TheEmptyTupleType)),
    IndexSubset::get(C, 6, {0, 1, 2, 5}));
  // Method (T) -> ((T, T), (T, T), T) -> ()
  // TODO(TF-874): Fix this unit test.
  // The current actual result is:
  // `(autodiff_index_subset capacity=6 indices=(0, 1, 4))`.
#if 0
  EXPECT_EQ(
    autodiff::getLoweredParameterIndices(
      IndexSubset::get(C, 4, {0, 1, 3}),
      FunctionType::get(
          {FunctionType::Param(C.TheAnyType)},
          FunctionType::get({
              FunctionType::Param(
                  TupleType::get({C.TheAnyType, C.TheAnyType}, C)),
              FunctionType::Param(
                  TupleType::get({C.TheAnyType, C.TheAnyType}, C)),
              FunctionType::Param(C.TheAnyType)},
              C.TheEmptyTupleType)->withExtInfo(
                  FunctionType::ExtInfo().withSILRepresentation(
                  SILFunctionTypeRepresentation::Method)))),
    IndexSubset::get(C, 6, {0, 1, 4, 5}));
#endif
}

TEST(IndexSubset, GetSubsetParameterTypes) {
  TestContext testCtx;
  auto &C = testCtx.Ctx;
  // (T, T) -> ()
  {
    SmallVector<AnyFunctionType::Param, 8> params;
    auto *functionType = FunctionType::get({FunctionType::Param(C.TheAnyType),
                                            FunctionType::Param(C.TheAnyType)},
                                           C.TheEmptyTupleType);
    functionType->getSubsetParameters(IndexSubset::get(C, 1, {0}), params);
    AnyFunctionType::Param expected[] = {AnyFunctionType::Param(C.TheAnyType)};
    EXPECT_TRUE(std::equal(params.begin(), params.end(), expected,
                           [](auto param1, auto param2) {
                             return param1.getPlainType()->isEqual(param2.getPlainType());
                           }));
  }
  // (T) -> (T, T) -> ()
  {
    SmallVector<AnyFunctionType::Param, 8> params;
    auto *functionType =
        FunctionType::get({FunctionType::Param(C.TheIEEE16Type)},
                          FunctionType::get({FunctionType::Param(C.TheAnyType),
                                             FunctionType::Param(C.TheAnyType)},
                                            C.TheEmptyTupleType));
    functionType->getSubsetParameters(IndexSubset::get(C, 3, {0, 1, 2}),
                                      params);
    AnyFunctionType::Param expected[] = {
        AnyFunctionType::Param(C.TheIEEE16Type),
        AnyFunctionType::Param(C.TheAnyType),
        AnyFunctionType::Param(C.TheAnyType)};
    EXPECT_TRUE(std::equal(
        params.begin(), params.end(), expected, [](auto param1, auto param2) {
          return param1.getPlainType()->isEqual(param2.getPlainType());
        }));
  }
}
