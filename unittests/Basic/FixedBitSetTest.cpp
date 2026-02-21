//===--- FixedBitSetTest.cpp ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/FixedBitSet.h"
#include "gtest/gtest.h"

using namespace swift;

enum class Enum {
  A, B, C, D
};

template <size_t numElements, class ElementType>
static void expectIteration(const FixedBitSet<numElements, ElementType> &bitSet,
                            std::initializer_list<ElementType> expected) {
  auto ei = expected.begin(), ee = expected.end();
  for (ElementType value : bitSet) {
    EXPECT_FALSE(ei == ee);
    EXPECT_EQ(*ei, value);
    ++ei;
  }
  EXPECT_TRUE(ei == ee);
}

template <size_t numElements, class ElementType>
static void expectIterationAll(const FixedBitSet<numElements, ElementType> &bitSet) {
  ElementType expected = ElementType(0);
  for (ElementType value : bitSet) {
    EXPECT_EQ(expected, value);
    expected = ElementType(size_t(expected) + 1);
  }
  EXPECT_EQ(ElementType(numElements), expected);
}

template <class T>
static void simpleScript(T &bitSet) {
  EXPECT_FALSE(bitSet.contains(Enum::C));
  EXPECT_FALSE(bitSet.contains(Enum::D));
  EXPECT_TRUE(bitSet.empty());
  expectIteration(bitSet, {});

  bitSet.insert(Enum::D);

  EXPECT_FALSE(bitSet.contains(Enum::C));
  EXPECT_TRUE(bitSet.contains(Enum::D));
  EXPECT_FALSE(bitSet.empty());
  expectIteration(bitSet, {Enum::D});

  bitSet.insert(Enum::C);

  EXPECT_TRUE(bitSet.contains(Enum::C));
  EXPECT_TRUE(bitSet.contains(Enum::D));
  EXPECT_FALSE(bitSet.empty());
  expectIteration(bitSet, {Enum::C, Enum::D});

  bitSet.insert(Enum::C);

  EXPECT_TRUE(bitSet.contains(Enum::C));
  EXPECT_TRUE(bitSet.contains(Enum::D));
  EXPECT_FALSE(bitSet.empty());
  expectIteration(bitSet, {Enum::C, Enum::D});

  bitSet.remove(Enum::D);

  EXPECT_TRUE(bitSet.contains(Enum::C));
  EXPECT_FALSE(bitSet.contains(Enum::D));
  EXPECT_FALSE(bitSet.empty());
  expectIteration(bitSet, {Enum::C});

  bitSet.remove(Enum::D);

  EXPECT_TRUE(bitSet.contains(Enum::C));
  EXPECT_FALSE(bitSet.contains(Enum::D));
  EXPECT_FALSE(bitSet.empty());
  expectIteration(bitSet, {Enum::C});

  bitSet.remove(Enum::C);

  EXPECT_FALSE(bitSet.contains(Enum::C));
  EXPECT_FALSE(bitSet.contains(Enum::D));
  EXPECT_TRUE(bitSet.empty());
  expectIteration(bitSet, {});
}

TEST(FixedBitSet, simple_5bits) {
  FixedBitSet<5, Enum> bitSet;
  simpleScript(bitSet);
}

TEST(FixedBitSet, simple_8bits) {
  FixedBitSet<8, Enum> bitSet;
  simpleScript(bitSet);
}

TEST(FixedBitSet, simple_14bits) {
  FixedBitSet<14, Enum> bitSet;
  simpleScript(bitSet);
}

TEST(FixedBitSet, simple_16bits) {
  FixedBitSet<16, Enum> bitSet;
  simpleScript(bitSet);
}

TEST(FixedBitSet, simple_21bits) {
  FixedBitSet<21, Enum> bitSet;
  simpleScript(bitSet);
}

TEST(FixedBitSet, simple_32bits) {
  FixedBitSet<32, Enum> bitSet;
  simpleScript(bitSet);
}

TEST(FixedBitSet, simple_57bits) {
  FixedBitSet<57, Enum> bitSet;
  simpleScript(bitSet);
}

TEST(FixedBitSet, simple_141bits) {
  FixedBitSet<141, Enum> bitSet;
  simpleScript(bitSet);
}

TEST(FixedBitSet, initListConstructor) {
  FixedBitSet<211, int> bitSet = { 51, 51, 89, 14, 175, 89 };
  EXPECT_FALSE(bitSet.empty());
  EXPECT_TRUE(bitSet.contains(14));
  EXPECT_TRUE(bitSet.contains(51));
  EXPECT_TRUE(bitSet.contains(89));
  EXPECT_TRUE(bitSet.contains(175));
  EXPECT_FALSE(bitSet.contains(200));
  EXPECT_FALSE(bitSet.contains(0));
  expectIteration(bitSet, {14, 51, 89, 175});
}

TEST(FixedBitSet, insertOrRemove) {
  FixedBitSet<211, int> bitSet;
  EXPECT_TRUE(bitSet.empty());
  EXPECT_FALSE(bitSet.contains(143));
  EXPECT_FALSE(bitSet.contains(200));
  expectIteration(bitSet, {});

  bitSet.insertOrRemove(143, true);
  EXPECT_FALSE(bitSet.empty());
  EXPECT_TRUE(bitSet.contains(143));
  EXPECT_FALSE(bitSet.contains(200));
  expectIteration(bitSet, {143});

  bitSet.insertOrRemove(200, true);
  EXPECT_FALSE(bitSet.empty());
  EXPECT_TRUE(bitSet.contains(143));
  EXPECT_TRUE(bitSet.contains(200));
  expectIteration(bitSet, {143, 200});

  bitSet.insertOrRemove(143, false);
  EXPECT_FALSE(bitSet.empty());
  EXPECT_FALSE(bitSet.contains(143));
  EXPECT_TRUE(bitSet.contains(200));
  expectIteration(bitSet, {200});
}

// nullary insertAll needs special attention in testing because of its
// logic to avoid setting padding bits.

TEST(FixedBitSet, insertAll_nullary_4_enum) {
  FixedBitSet<4, Enum> bitSet;
  bitSet.insertAll();
  EXPECT_FALSE(bitSet.empty());
  expectIterationAll(bitSet);
  expectIteration(bitSet, {Enum::A, Enum::B, Enum::C, Enum::D});
}

TEST(FixedBitSet, insertAll_nullary_4) {
  FixedBitSet<4, size_t> bitSet;
  bitSet.insertAll();
  EXPECT_FALSE(bitSet.empty());
  expectIterationAll(bitSet);
}

TEST(FixedBitSet, insertAll_nullary_8) {
  FixedBitSet<8, size_t> bitSet;
  bitSet.insertAll();
  EXPECT_FALSE(bitSet.empty());
  expectIterationAll(bitSet);
}

TEST(FixedBitSet, insertAll_nullary_14) {
  FixedBitSet<14, size_t> bitSet;
  bitSet.insertAll();
  EXPECT_FALSE(bitSet.empty());
  expectIterationAll(bitSet);
}

TEST(FixedBitSet, insertAll_nullary_17) {
  FixedBitSet<17, size_t> bitSet;
  bitSet.insertAll();
  EXPECT_FALSE(bitSet.empty());
  expectIterationAll(bitSet);
}

TEST(FixedBitSet, insertAll_nullary_51) {
  FixedBitSet<51, size_t> bitSet;
  bitSet.insertAll();
  EXPECT_FALSE(bitSet.empty());
  expectIterationAll(bitSet);
}

TEST(FixedBitSet, insertAll_nullary_211) {
  FixedBitSet<211, size_t> bitSet;
  bitSet.insertAll();
  EXPECT_FALSE(bitSet.empty());
  expectIterationAll(bitSet);
}

TEST(FixedBitSet, insertAll_set) {
  FixedBitSet<51, int> bitSet = { 34, 12, 0, 8 };
  expectIteration(bitSet, {0, 8, 12, 34});

  FixedBitSet<51, int> other = { 23, 12, 1, 2 };
  bitSet.insertAll(other);
  expectIteration(bitSet, {0, 1, 2, 8, 12, 23, 34});
}

TEST(FixedBitSet, removeAll_nullary) {
  FixedBitSet<7, int> bitSet = { 6, 1, 4 };
  EXPECT_FALSE(bitSet.empty());
  expectIteration(bitSet, {1, 4, 6});

  bitSet.removeAll();
  EXPECT_TRUE(bitSet.empty());
  expectIteration(bitSet, {});
}

TEST(FixedBitSet, removeAll_set) {
  FixedBitSet<7, int> bitSet = { 6, 1, 4 };
  EXPECT_FALSE(bitSet.empty());
  expectIteration(bitSet, {1, 4, 6});

  FixedBitSet<7, int> other = { 6, 3, 4 };
  bitSet.removeAll(other);
  EXPECT_FALSE(bitSet.empty());
  expectIteration(bitSet, {1});

  FixedBitSet<7, int> other2 = { 1, 0 };
  bitSet.removeAll(other2);
  EXPECT_TRUE(bitSet.empty());
  expectIteration(bitSet, {});
}

TEST(FixedBitSet, removeAllExcept_set) {
  FixedBitSet<7, int> bitSet = { 6, 1, 4 };
  EXPECT_FALSE(bitSet.empty());
  expectIteration(bitSet, {1, 4, 6});

  FixedBitSet<7, int> other = { 6, 3, 4 };
  bitSet.removeAllExcept(other);
  EXPECT_FALSE(bitSet.empty());
  expectIteration(bitSet, {4, 6});

  FixedBitSet<7, int> other2 = { 1, 0 };
  bitSet.removeAllExcept(other2);
  EXPECT_TRUE(bitSet.empty());
  expectIteration(bitSet, {});
}

TEST(FixedBitSet, equality) {
  FixedBitSet<141, int> lhs = {};
  FixedBitSet<141, int> rhs = {1};
  EXPECT_NE(lhs, rhs);

  lhs.insert(1);  
  EXPECT_EQ(lhs, rhs);

  rhs.insert(107);
  EXPECT_NE(lhs, rhs);

  lhs.insert(107);
  EXPECT_EQ(lhs, rhs);
}
