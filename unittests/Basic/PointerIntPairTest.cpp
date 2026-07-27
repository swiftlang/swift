//===--- PointerIntPairTest.cpp -------------------------------------------===//
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

#include "swift/Basic/PointerIntPair.h"
#include "llvm/ADT/PointerIntPair.h"
#include "gtest/gtest.h"
#include <type_traits>

using namespace swift;

namespace {
enum class E : unsigned { A, B, C, D, E_, F, G, H };
}

static_assert(
    std::is_same<PointerIntPair<int *, 2, unsigned>,
                 llvm::PointerIntPair<int *, 2, unsigned>>::value,
    "sufficient-bit case must alias llvm::PointerIntPair");
static_assert(
    std::is_same<PointerIntPair<int *, 3, E>,
                 pointer_int_pair_detail::SeparateStorage<int *, E>>::value,
    "insufficient-bit case must use the separate-storage fallback");

TEST(PointerIntPair, PackedPathRoundTrips) {
  int x = 0, y = 0;
  PointerIntPair<int *, 2, unsigned> p(&x, 3);
  EXPECT_EQ(p.getPointer(), &x);
  EXPECT_EQ(p.getInt(), 3u);
  p.setInt(1);
  EXPECT_EQ(p.getInt(), 1u);
  p.setPointer(&y);
  EXPECT_EQ(p.getPointer(), &y);
  EXPECT_EQ(p.getInt(), 1u);
  p.setPointerAndInt(&x, 2);
  EXPECT_EQ(p.getPointer(), &x);
  EXPECT_EQ(p.getInt(), 2u);
}

TEST(PointerIntPair, FallbackPathRoundTrips) {
  int x = 0, y = 0;
  PointerIntPair<int *, 3, E> p;
  EXPECT_EQ(p.getPointer(), nullptr);
  EXPECT_EQ(p.getInt(), E::A);

  p = PointerIntPair<int *, 3, E>(&x, E::F);
  EXPECT_EQ(p.getPointer(), &x);
  EXPECT_EQ(p.getInt(), E::F);

  for (unsigned i = 0; i < 8; ++i) {
    p.setInt(static_cast<E>(i));
    EXPECT_EQ(p.getInt(), static_cast<E>(i));
    EXPECT_EQ(p.getPointer(), &x);
  }

  p.setPointerAndInt(&y, E::C);
  EXPECT_EQ(p.getPointer(), &y);
  EXPECT_EQ(p.getInt(), E::C);

  p.initWithPointer(&x);
  EXPECT_EQ(p.getPointer(), &x);
  EXPECT_EQ(p.getInt(), E::A);

  PointerIntPair<int *, 3, E> q(&x, E::A);
  EXPECT_TRUE(p == q);
  q.setInt(E::D);
  EXPECT_TRUE(p != q);
}
