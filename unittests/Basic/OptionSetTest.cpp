//===--- OptionSetTest.cpp ------------------------------------------------===//
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

#include "swift/Basic/OptionSet.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/ValueEnumerator.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(OptionSet, contains) {
  enum class Flags { A = 1 << 0, B = 1 << 1, C = 1 << 2 };

  OptionSet<Flags> emptySet;
  OptionSet<Flags> aSet = Flags::A;
  OptionSet<Flags> abSet = aSet | Flags::B;
  OptionSet<Flags> abcSet = abSet | Flags::C;
  OptionSet<Flags> bcSet = abcSet - Flags::A;
  OptionSet<Flags> cSet = bcSet - Flags::B;

  EXPECT_TRUE(emptySet.contains(emptySet));
  EXPECT_FALSE(emptySet.contains(aSet));
  EXPECT_FALSE(emptySet.contains(abSet));
  EXPECT_FALSE(emptySet.contains(abcSet));
  EXPECT_FALSE(emptySet.contains(bcSet));
  EXPECT_FALSE(emptySet.contains(cSet));

  EXPECT_TRUE(aSet.contains(emptySet));
  EXPECT_TRUE(aSet.contains(aSet));
  EXPECT_FALSE(aSet.contains(abSet));
  EXPECT_FALSE(aSet.contains(abcSet));
  EXPECT_FALSE(aSet.contains(bcSet));
  EXPECT_FALSE(aSet.contains(cSet));

  EXPECT_TRUE(abSet.contains(emptySet));
  EXPECT_TRUE(abSet.contains(aSet));
  EXPECT_TRUE(abSet.contains(abSet));
  EXPECT_FALSE(abSet.contains(abcSet));
  EXPECT_FALSE(abSet.contains(bcSet));
  EXPECT_FALSE(abSet.contains(cSet));

  EXPECT_TRUE(abcSet.contains(emptySet));
  EXPECT_TRUE(abcSet.contains(aSet));
  EXPECT_TRUE(abcSet.contains(abSet));
  EXPECT_TRUE(abcSet.contains(abcSet));
  EXPECT_TRUE(abcSet.contains(bcSet));
  EXPECT_TRUE(abcSet.contains(cSet));
}

TEST(OptionSet, intptr_t) {
  enum class Small : int8_t { A = 1 << 0 };

  OptionSet<Small> small = Small::A;
  EXPECT_EQ(static_cast<intptr_t>(Small::A), static_cast<intptr_t>(small));

  enum class UPtr : uintptr_t { A = std::numeric_limits<uintptr_t>::max() };

  OptionSet<UPtr> uptr = UPtr::A;
  EXPECT_EQ(static_cast<intptr_t>(UPtr::A), static_cast<intptr_t>(uptr));

  enum class Ptr : intptr_t { A = std::numeric_limits<intptr_t>::min() };

  OptionSet<Ptr> ptr = Ptr::A;
  EXPECT_EQ(static_cast<intptr_t>(Ptr::A), static_cast<intptr_t>(ptr));
}

TEST(OptionSet, intptr_t_isConstructible) {
  // First check that std::is_constructible counts explicit conversion
  // operators.
  class AlwaysConvertible {
  public:
    explicit operator intptr_t() const { return 0; }
  };

  if (!std::is_constructible<intptr_t, AlwaysConvertible>::value) {
    // std::is_constructible doesn't test what we want it to. Just exit early.
    return;
  }

  enum class LongLong : unsigned long long { A = 1 };
  bool isConvertible =
      std::is_constructible<intptr_t, OptionSet<LongLong>>::value;

  if (sizeof(intptr_t) < sizeof(long long)) {
    EXPECT_FALSE(isConvertible);
  } else {
    EXPECT_TRUE(isConvertible);
  }
}
