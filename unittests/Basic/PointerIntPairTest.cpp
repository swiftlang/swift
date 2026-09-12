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

// A couple of arbitrary, distinct discriminators for the two arms below.
// Plain integers (rather than ptrauth_string_discriminator(...)) so this
// file compiles the same whether or not SWIFT_PTRAUTH is enabled.
using PackedAuth = swift::AddressDiversifiedPointerAuth<0xbeef>;
using SeparateAuth = swift::AddressDiversifiedPointerAuth<0xf00d>;

static_assert(
    std::is_same<PointerIntPair<int *, 2, unsigned>,
                 PointerIntPair<int *, 2, unsigned,
                                llvm::PointerLikeTypeTraits<int *>,
                                pointer_int_pair_detail::Unauthenticated>>::value,
    "omitting Auth must be identical to explicitly requesting Unauthenticated");
static_assert(
    std::is_same<PointerIntPair<int *, 2, unsigned,
                                llvm::PointerLikeTypeTraits<int *>, PackedAuth>,
                 pointer_int_pair_detail::AuthenticatedPacked<
                     int *, 2, unsigned, llvm::PointerLikeTypeTraits<int *>,
                     PackedAuth>>::value,
    "sufficient-bit case with a real Auth policy must use AuthenticatedPacked");
static_assert(
    std::is_same<
        PointerIntPair<int *, 3, E, llvm::PointerLikeTypeTraits<int *>,
                       SeparateAuth>,
        pointer_int_pair_detail::AuthenticatedSeparateStorage<int *, E,
                                                               SeparateAuth>>::value,
    "insufficient-bit case with a real Auth policy must use "
    "AuthenticatedSeparateStorage");

TEST(PointerIntPair, AuthenticatedPackedRoundTrips) {
  int x = 0, y = 0;
  PointerIntPair<int *, 2, unsigned, llvm::PointerLikeTypeTraits<int *>,
                 PackedAuth>
      p(&x, 3);
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

  // Two objects holding the same logical value at different addresses must
  // still compare equal: address-diversified signing makes their raw signed
  // bits differ, so operator== has to compare the authenticated value, not
  // the raw storage word. (The raw-bits difference itself only holds when
  // signing is actually diversified, i.e. under real SWIFT_PTRAUTH -- on a
  // no-op build the two would be bitwise identical too.)
  decltype(p) q(&x, 2);
#if SWIFT_PTRAUTH
  EXPECT_NE(p.getOpaqueValue(), q.getOpaqueValue());
#endif
  EXPECT_TRUE(p == q);
  q.setInt(0);
  EXPECT_TRUE(p != q);

#if SWIFT_PTRAUTH
  // Flipping a bit in the raw signed word must be detectable. Corrupt and
  // restore in place on `p` itself (not via getFromOpaqueValue into a new
  // object, since the signature is diversified on p's own address) and
  // confirm the recovered pointer no longer matches.
  void *Corrupted = reinterpret_cast<void *>(
      reinterpret_cast<uintptr_t>(p.getOpaqueValue()) ^ 0x10);
  p.setFromOpaqueValue(Corrupted);
  EXPECT_NE(p.getPointer(), &x);
#endif
}

TEST(PointerIntPair, AuthenticatedSeparateStorageRoundTrips) {
  int x = 0, y = 0;
  PointerIntPair<int *, 3, E, llvm::PointerLikeTypeTraits<int *>,
                 SeparateAuth>
      p;
  EXPECT_EQ(p.getPointer(), nullptr);
  EXPECT_EQ(p.getInt(), E::A);

  p = decltype(p)(&x, E::F);
  EXPECT_EQ(p.getPointer(), &x);
  EXPECT_EQ(p.getInt(), E::F);

  p.setPointerAndInt(&y, E::C);
  EXPECT_EQ(p.getPointer(), &y);
  EXPECT_EQ(p.getInt(), E::C);

  p.initWithPointer(&x);
  EXPECT_EQ(p.getPointer(), &x);
  EXPECT_EQ(p.getInt(), E::A);

  decltype(p) q(&x, E::A);
  EXPECT_TRUE(p == q);
  q.setInt(E::D);
  EXPECT_TRUE(p != q);
}
