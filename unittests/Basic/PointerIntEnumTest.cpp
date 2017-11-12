//===--- PointerIntEnumTest.cpp -------------------------------------------===//
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

#include "swift/Basic/PointerIntEnum.h"
#include "swift/Basic/type_traits.h"
#include "llvm/ADT/ArrayRef.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {

enum class EnumTy : unsigned {
  Ptr1 = 0,
  Ptr2 = 1,
  Ptr3 = 2,
  FirstPointerKind = Ptr1,
  LastPointerKind = Ptr3,

  // Index Projection Kinds
  FirstIndexKind = 7,
  Index1 = PointerIntEnumIndexKindValue<0, EnumTy>::value,
  Index2 = PointerIntEnumIndexKindValue<1, EnumTy>::value,
  Index3 = PointerIntEnumIndexKindValue<2, EnumTy>::value,
  Index4 = PointerIntEnumIndexKindValue<3, EnumTy>::value,
  Index5 = PointerIntEnumIndexKindValue<4, EnumTy>::value,
  LastIndexKind = Index5,
};

using PointerIntEnumTy =
    PointerIntEnum<EnumTy, void *, 3, 4, llvm::PointerLikeTypeTraits<void *>>;

static_assert(IsTriviallyCopyable<PointerIntEnumTy>::value,
              "PointerIntEnum type should be trivially copyable");

static constexpr uintptr_t InvalidStorage = uintptr_t(0) - 1;

} // end anonymous namespace

TEST(PointerIntEnumTest, DefaultConstructorYieldsInvalid) {
  PointerIntEnumTy Enum;
  EXPECT_FALSE(Enum.isValid());
  EXPECT_TRUE(Enum.getStorage() == InvalidStorage);
}

TEST(PointerIntEnumTest, PointerConstructor) {
  int *data = new int[1];
  PointerIntEnumTy Enum(EnumTy::Ptr1, data);
  EXPECT_TRUE(Enum.isValid());
  EXPECT_EQ(*Enum.getKind(), EnumTy::Ptr1);
  EXPECT_EQ(Enum.getPointer(), data);

  // Make sure that the value is laid out correctly in memory.
  uintptr_t Value = uintptr_t(data) | uintptr_t(EnumTy::Ptr1);
  EXPECT_EQ(Enum.getStorage(), Value);

  delete[] data;
}

TEST(PointerIntEnumTest, IndexConstructor) {
  // First test a case that we can represent.
  {
    PointerIntEnumTy Enum(EnumTy::Index3, 0xBEEF);
    EXPECT_TRUE(Enum.isValid());
    EXPECT_EQ(*Enum.getKind(), EnumTy::Index3);
    EXPECT_EQ(Enum.getIndex(), uintptr_t(0xBEEF));

    // Make sure that the value is laid out correctly in memory.
    uintptr_t Value = (uintptr_t(0xBEEF) << 7) | uintptr_t(EnumTy::Index3);
    EXPECT_EQ(Enum.getStorage(), Value);
  }

  // Then test the boundary from representable index to unrepresentable index.
  uintptr_t MaxIndex = (uintptr_t(1) << (sizeof(uintptr_t) * CHAR_BIT - 7)) - 2;
  {
    PointerIntEnumTy Enum(EnumTy::Index3, MaxIndex + 1);
    EXPECT_FALSE(Enum.isValid());
    EXPECT_FALSE(Enum.getKind());
    EXPECT_EQ(Enum.getStorage(), InvalidStorage);
  }

  {
    PointerIntEnumTy Enum(EnumTy::Index4, MaxIndex);
    EXPECT_TRUE(Enum.isValid());
    EXPECT_EQ(*Enum.getKind(), EnumTy::Index4);
    EXPECT_EQ(Enum.getIndex(), MaxIndex);

    // Make sure that the value is laid out correctly in memory.
    uintptr_t Value = (uintptr_t(MaxIndex) << 7) | uintptr_t(EnumTy::Index4);
    EXPECT_EQ(Enum.getStorage(), Value);
  }
}

TEST(PointerIntEnumTest, CopyConstructorAssignment) {
  PointerIntEnumTy IntEnum(EnumTy::Index3, 0xBEEF);
  uintptr_t IntEnumStorageValue =
      (uintptr_t(0xBEEF) << 7) | uintptr_t(EnumTy::Index3);
  int *data = new int[1];
  PointerIntEnumTy PtrEnum(EnumTy::Ptr2, data);
  uintptr_t PtrEnumStorageValue = uintptr_t(data) | uintptr_t(EnumTy::Ptr2);

  PointerIntEnumTy Enum2(IntEnum);
  PointerIntEnumTy Enum3 = IntEnum;

  EXPECT_TRUE(Enum2.isValid());
  EXPECT_EQ(*Enum2.getKind(), EnumTy::Index3);
  EXPECT_EQ(Enum2.getIndex(), uintptr_t(0xBEEF));
  EXPECT_EQ(Enum2.getStorage(), IntEnumStorageValue);
  EXPECT_EQ(IntEnum, Enum2);
  EXPECT_NE(PtrEnum, Enum2);

  EXPECT_TRUE(Enum3.isValid());
  EXPECT_EQ(*Enum3.getKind(), EnumTy::Index3);
  EXPECT_EQ(Enum3.getIndex(), uintptr_t(0xBEEF));
  EXPECT_EQ(Enum3.getStorage(), IntEnumStorageValue);
  EXPECT_EQ(IntEnum, Enum3);
  EXPECT_NE(PtrEnum, Enum3);

  Enum3 = PtrEnum;
  PointerIntEnumTy Enum4(PtrEnum);

  EXPECT_TRUE(Enum3.isValid());
  EXPECT_EQ(*Enum3.getKind(), EnumTy::Ptr2);
  EXPECT_EQ(Enum3.getPointer(), data);
  EXPECT_EQ(Enum3.getStorage(), PtrEnumStorageValue);
  EXPECT_EQ(Enum3, PtrEnum);
  EXPECT_NE(Enum3, IntEnum);

  EXPECT_TRUE(Enum4.isValid());
  EXPECT_EQ(*Enum4.getKind(), EnumTy::Ptr2);
  EXPECT_EQ(Enum4.getPointer(), data);
  EXPECT_EQ(Enum4.getStorage(), PtrEnumStorageValue);
  EXPECT_EQ(Enum4, PtrEnum);
  EXPECT_NE(Enum4, IntEnum);

  // Round trip Enum3
  Enum3 = IntEnum;
  EXPECT_TRUE(Enum3.isValid());
  EXPECT_EQ(*Enum3.getKind(), EnumTy::Index3);
  EXPECT_EQ(Enum3.getIndex(), uintptr_t(0xBEEF));
  EXPECT_EQ(Enum3.getStorage(), IntEnumStorageValue);
  EXPECT_EQ(IntEnum, Enum3);
  EXPECT_NE(PtrEnum, Enum3);

  delete[] data;
}

// We have a trivial move constructor, so we copy when we move.
TEST(PointerIntEnumTest, MoveConstructorAssignment) {
  PointerIntEnumTy IntEnum(EnumTy::Index3, 0xBEEF);
  uintptr_t IntEnumStorageValue =
      (uintptr_t(0xBEEF) << 7) | uintptr_t(EnumTy::Index3);
  int *data = new int[1];
  PointerIntEnumTy PtrEnum(EnumTy::Ptr2, data);
  uintptr_t PtrEnumStorageValue = uintptr_t(data) | uintptr_t(EnumTy::Ptr2);

  PointerIntEnumTy Enum2(std::move(IntEnum));
  PointerIntEnumTy Enum3 = std::move(IntEnum);

  EXPECT_TRUE(Enum2.isValid());
  EXPECT_EQ(*Enum2.getKind(), EnumTy::Index3);
  EXPECT_EQ(Enum2.getIndex(), uintptr_t(0xBEEF));
  EXPECT_EQ(Enum2.getStorage(), IntEnumStorageValue);
  EXPECT_EQ(IntEnum, Enum2);
  EXPECT_NE(PtrEnum, Enum2);

  EXPECT_TRUE(Enum3.isValid());
  EXPECT_EQ(*Enum3.getKind(), EnumTy::Index3);
  EXPECT_EQ(Enum3.getIndex(), uintptr_t(0xBEEF));
  EXPECT_EQ(Enum3.getStorage(), IntEnumStorageValue);
  EXPECT_EQ(IntEnum, Enum3);
  EXPECT_NE(PtrEnum, Enum3);

  Enum3 = std::move(PtrEnum);
  PointerIntEnumTy Enum4(std::move(PtrEnum));

  EXPECT_TRUE(Enum3.isValid());
  EXPECT_EQ(*Enum3.getKind(), EnumTy::Ptr2);
  EXPECT_EQ(Enum3.getPointer(), data);
  EXPECT_EQ(Enum3.getStorage(), PtrEnumStorageValue);
  EXPECT_EQ(Enum3, PtrEnum);
  EXPECT_NE(Enum3, IntEnum);

  EXPECT_TRUE(Enum4.isValid());
  EXPECT_EQ(*Enum4.getKind(), EnumTy::Ptr2);
  EXPECT_EQ(Enum4.getPointer(), data);
  EXPECT_EQ(Enum4.getStorage(), PtrEnumStorageValue);
  EXPECT_EQ(Enum4, PtrEnum);
  EXPECT_NE(Enum4, IntEnum);

  // Round trip Enum3
  Enum3 = std::move(IntEnum);
  EXPECT_TRUE(Enum3.isValid());
  EXPECT_EQ(*Enum3.getKind(), EnumTy::Index3);
  EXPECT_EQ(Enum3.getIndex(), uintptr_t(0xBEEF));
  EXPECT_EQ(Enum3.getStorage(), IntEnumStorageValue);
  EXPECT_EQ(IntEnum, Enum3);
  EXPECT_NE(PtrEnum, Enum3);

  delete[] data;
}

TEST(PointerIntEnumTest, Comparisons) {
  PointerIntEnumTy IndexCase1(EnumTy::Index1, 5);

  // Make sure that enums with different cases but the same value always compare
  // different.
  PointerIntEnumTy IndexCase2(EnumTy::Index2, 5);
  EXPECT_NE(IndexCase1, IndexCase2);

  // Make sure that enums with the same case and the same value compare equal.
  PointerIntEnumTy IndexCase3(EnumTy::Index1, 5);
  EXPECT_EQ(IndexCase1, IndexCase3);

  // Make sure that enums with the same case, but different values do not
  // compare equal.
  PointerIntEnumTy IndexCase4(EnumTy::Index1, 6);
  EXPECT_NE(IndexCase1, IndexCase4);

  int *data1 = new int[1];
  int *data2 = new int[1];
  PointerIntEnumTy PtrCase1(EnumTy::Ptr1, data1);

  // Test that pointer enums with different cases but the same value compare
  // different.
  PointerIntEnumTy PtrCase2(EnumTy::Ptr2, data1);
  EXPECT_NE(PtrCase1, PtrCase2);

  // Test that pointer enums with the same case and data are equal.
  PointerIntEnumTy PtrCase3(EnumTy::Ptr1, data1);
  EXPECT_EQ(PtrCase1, PtrCase3);

  // Test that pointer enums with the same case but different data are not
  // equal.
  PointerIntEnumTy PtrCase4(EnumTy::Ptr1, data2);
  EXPECT_NE(PtrCase1, PtrCase4);

  // Test that pointers and indices compare differently.
  EXPECT_NE(IndexCase1, PtrCase1);

  // Test comparison in between invalid and valid PointerIntEnums.
  PointerIntEnumTy Invalid1;
  PointerIntEnumTy Invalid2;
  EXPECT_EQ(Invalid1, Invalid2);
  EXPECT_NE(Invalid1, IndexCase1);
  EXPECT_NE(Invalid1, PtrCase1);
  EXPECT_NE(IndexCase1, Invalid1);
  EXPECT_NE(PtrCase1, Invalid1);


  delete[] data2;
  delete[] data1;
}
