#include "swift/Basic/PointerIntEnum.h"
#include "llvm/ADT/ArrayRef.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {

enum class EnumTy : uint64_t {
  Invalid = 0,

  // Pointer Kinds
  Ptr1 = 1,
  Ptr2 = 2,
  Ptr3 = 3,
  LastPointerKind = Ptr3,

  // Index Kinds.
  //
  // When we have an index >= 4096, we malloc memory to store it. It needs to be
  // able to be stored in at most 3 bits.
  LargeIndex = 7,
  Index1 = 8,
  Index2 = 9,
  Index3 = 10,
  Index4 = 11,
  Index5 = 12,
  LastIndexKind = Index5,
};

using PointerIntEnumTy =
  PointerIntEnum<EnumTy, void *, 3, 4, llvm::PointerLikeTypeTraits<void *>,
                 true>;

} // end anonymous namespace

TEST(PointerIntEnumTest, DefaultConstructIsInvalid) {
  PointerIntEnumTy Enum;
  EXPECT_FALSE(Enum.isValid());
  EXPECT_TRUE(Enum.getKind() == EnumTy::Invalid);
}

TEST(PointerIntEnumTest, ConstructDestructInt) {
  llvm::ArrayRef<EnumTy> Cases((EnumTy[5]){EnumTy::Index1, EnumTy::Index2,
                                           EnumTy::Index3, EnumTy::Index4,
                                           EnumTy::Index5},
                               5);

  for (auto &Case : Cases) {
    for (unsigned i = 0, e = 50; i < e; ++i) {
      PointerIntEnumTy Enum(Case, i);
      EXPECT_TRUE(Enum.isValid());
      EXPECT_EQ(Enum.getKind(), Case);
      EXPECT_EQ(Enum.getIndex(), i);
      EXPECT_EQ(Enum.getRawKind(), Case);
    }

    for (unsigned i = 0, e = 4096; i < e; ++i) {
      PointerIntEnumTy Enum(Case, i);
      EXPECT_TRUE(Enum.isValid());
      EXPECT_EQ(Enum.getKind(), Case);
      EXPECT_EQ(Enum.getIndex(), i);
      EXPECT_EQ(Enum.getRawKind(), Case);
    }

    for (unsigned i = 4096, e = 10000; i < e; ++i) {
      void *ptr;
      uint64_t x;
      {
        PointerIntEnumTy Enum(Case, i);
        EXPECT_TRUE(Enum.isValid());
        EXPECT_EQ(Enum.getKind(), Case);
        EXPECT_EQ(Enum.getIndex(), i);
        EXPECT_EQ(Enum.getRawKind(), EnumTy::LargeIndex);
        ptr = Enum.getPointer();
        memcpy(&x, ptr, sizeof(x));
      }
      uint64_t y;
      memcpy(&y, ptr, sizeof(y));
      EXPECT_NE(y, x);
      EXPECT_EQ(y, -1ULL);
    }
  }
}

TEST(PointerIntEnumTest, ConstructDestructPointer) {
  EnumTy *Enums = new EnumTy[3];

  Enums[0] = EnumTy::Ptr1;
  Enums[1] = EnumTy::Ptr2;
  Enums[2] = EnumTy::Ptr3;

  for (unsigned ii = 0, ie = 3; ii < ie; ++ii) {
    for (unsigned jj = 0, je = 3; jj < je; ++jj) {
      void *Ptr = reinterpret_cast<void *>(&Enums[jj]);
      PointerIntEnumTy Enum(Enums[ii], Ptr);
      EXPECT_TRUE(Enum.isValid());
      EXPECT_EQ(Enum.getKind(), Enums[ii]);
      EXPECT_EQ(Enum.getPointer(), Ptr);
      EXPECT_EQ(Enum.getRawKind(), Enums[ii]);
    }
  }

  delete[] Enums;
}

TEST(PointerIntEnumTest, CopyConstructorLargeInt) {
  PointerIntEnumTy E(EnumTy::Index2, 5000);
  PointerIntEnumTy E2(E);

  EXPECT_TRUE(E.isValid());
  EXPECT_TRUE(E2.isValid());
  EXPECT_EQ(E.getKind(), E2.getKind());
  EXPECT_NE(E.getPointer(), E2.getPointer());
  EXPECT_EQ(E.getRawKind(), E2.getRawKind());
  EXPECT_EQ(E.getIndex(), E2.getIndex());
}

TEST(PointerIntEnumTest, CopyConstructorSmallInt) {
  PointerIntEnumTy E(EnumTy::Index3, 5);
  PointerIntEnumTy E2(E);

  EXPECT_TRUE(E.isValid());
  EXPECT_TRUE(E2.isValid());
  EXPECT_EQ(E.getKind(), E2.getKind());
  EXPECT_EQ(E.getRawKind(), E2.getRawKind());
  EXPECT_EQ(E.getIndex(), E2.getIndex());
}

TEST(PointerIntEnumTest, CopyConstructorPointer) {
  int *ptr = new int[1];
  PointerIntEnumTy E(EnumTy::Ptr1, reinterpret_cast<void *>(ptr));
  PointerIntEnumTy E2(E);

  EXPECT_TRUE(E.isValid());
  EXPECT_TRUE(E2.isValid());
  EXPECT_EQ(E.getKind(), E2.getKind());
  EXPECT_EQ(E.getPointer(), E2.getPointer());
  EXPECT_EQ(E.getRawKind(), E2.getRawKind());
  delete [] ptr;
}

TEST(PointerIntEnumTest, MoveConstructorLargeInt) {
  PointerIntEnumTy E(EnumTy::Index2, 5000);
  void *Ptr = E.getPointer();

  {
    PointerIntEnumTy E2(std::move(E));

    EXPECT_FALSE(E.isValid());
    EXPECT_EQ(E.getKind(), EnumTy::Invalid);
    EXPECT_EQ(E.getRawKind(), EnumTy::Invalid);

    EXPECT_TRUE(E2.isValid());
    EXPECT_EQ(E2.getKind(), EnumTy::Index2);
    EXPECT_EQ(E2.getRawKind(), EnumTy::LargeIndex);
    EXPECT_EQ(E2.getIndex(), 5000U);
    EXPECT_EQ(E2.getPointer(), Ptr);
  }

  uint64_t y;
  memcpy(&y, Ptr, sizeof(y));
  EXPECT_EQ(y, -1ULL);
}

TEST(PointerIntEnumTest, MoveConstructorSmallInt) {
  PointerIntEnumTy E(EnumTy::Index2, 4095);
  PointerIntEnumTy E2(std::move(E));

  EXPECT_FALSE(E.isValid());
  EXPECT_EQ(E.getKind(), EnumTy::Invalid);
  EXPECT_EQ(E.getRawKind(), EnumTy::Invalid);

  EXPECT_TRUE(E2.isValid());
  EXPECT_EQ(E2.getKind(), EnumTy::Index2);
  EXPECT_EQ(E2.getRawKind(), EnumTy::Index2);
  EXPECT_EQ(E2.getIndex(), 4095U);
}

TEST(PointerIntEnumTest, MoveConstructorPointer) {
  int *InputPtr = new int[1];
  InputPtr[0] = INT_MAX;
  PointerIntEnumTy E(EnumTy::Ptr3, InputPtr);
  void *Ptr = E.getPointer();
  EXPECT_EQ(Ptr, InputPtr);

  {
    PointerIntEnumTy E2(std::move(E));

    EXPECT_FALSE(E.isValid());
    EXPECT_EQ(E.getKind(), EnumTy::Invalid);
    EXPECT_EQ(E.getRawKind(), EnumTy::Invalid);

    EXPECT_TRUE(E2.isValid());
    EXPECT_EQ(E2.getKind(), EnumTy::Ptr3);
    EXPECT_EQ(E2.getRawKind(), EnumTy::Ptr3);
    EXPECT_EQ(E2.getPointer(), Ptr);
  }

  EXPECT_EQ(InputPtr[0], INT_MAX);

  delete [] InputPtr;
}

TEST(PointerIntEnumTest, CopyAssignInvalidToLargeInt) {
  PointerIntEnumTy Invalid;
  PointerIntEnumTy Large(EnumTy::Index3, 5000);

  Invalid = Large;

  EXPECT_TRUE(Invalid.isValid());
  EXPECT_EQ(Invalid.getKind(), EnumTy::Index3);
  EXPECT_EQ(Invalid.getRawKind(), EnumTy::LargeIndex);
  EXPECT_EQ(Invalid.getIndex(), 5000U);

  EXPECT_TRUE(Large.isValid());
  EXPECT_EQ(Large.getKind(), EnumTy::Index3);
  EXPECT_EQ(Large.getRawKind(), EnumTy::LargeIndex);
  EXPECT_EQ(Large.getIndex(), 5000U);
}

TEST(PointerIntEnumTest, CopyAssignSmallIntToLargeInt) {
  PointerIntEnumTy Small(EnumTy::Index2, 4095);
  PointerIntEnumTy Large(EnumTy::Index3, 5000);

  Small = Large;

  EXPECT_TRUE(Small.isValid());
  EXPECT_EQ(Small.getKind(), EnumTy::Index3);
  EXPECT_EQ(Small.getRawKind(), EnumTy::LargeIndex);
  EXPECT_EQ(Small.getIndex(), 5000U);

  EXPECT_TRUE(Large.isValid());
  EXPECT_EQ(Large.getKind(), EnumTy::Index3);
  EXPECT_EQ(Large.getRawKind(), EnumTy::LargeIndex);
  EXPECT_EQ(Large.getIndex(), 5000U);
}

TEST(PointerIntEnumTest, CopyAssignLargeIntToSmallInt) {
  PointerIntEnumTy Small(EnumTy::Index2, 4095);
  PointerIntEnumTy Large(EnumTy::Index3, 5000);
  void *Ptr = Large.getPointer();

  Large = Small;

  uint64_t y;
  memcpy(&y, Ptr, sizeof(y));
  EXPECT_EQ(y, -1ULL);

  EXPECT_TRUE(Small.isValid());
  EXPECT_EQ(Small.getKind(), EnumTy::Index2);
  EXPECT_EQ(Small.getRawKind(), EnumTy::Index2);
  EXPECT_EQ(Small.getIndex(), 4095U);

  EXPECT_TRUE(Large.isValid());
  EXPECT_EQ(Large.getKind(), EnumTy::Index2);
  EXPECT_EQ(Large.getRawKind(), EnumTy::Index2);
  EXPECT_EQ(Large.getIndex(), 4095U);
}

TEST(PointerIntEnumTest, CopyAssignPointerToLargeInt) {
  int *InputPtr = new int[1];
  InputPtr[0] = INT_MAX;

  void *Ptr1, *Ptr2;
  uint64_t Ptr1Value, Ptr2Value;
  {
    PointerIntEnumTy Large(EnumTy::Index3, 5000);
    Ptr1 = Large.getPointer();
    memcpy(&Ptr1Value, Ptr1, sizeof(Ptr1Value));
    EXPECT_NE(Ptr1Value, -1ULL);

    {
      PointerIntEnumTy Pointer(EnumTy::Ptr3, InputPtr);
      void *Ptr = Pointer.getPointer();
      EXPECT_EQ(Ptr, InputPtr);

      Pointer = Large;
      Ptr2 = Pointer.getPointer();
      EXPECT_NE(Ptr1, Ptr2);

      memcpy(&Ptr2Value, Ptr2, sizeof(Ptr2Value));
      EXPECT_NE(Ptr2Value, -1ULL);

      EXPECT_TRUE(Large.isValid());
      EXPECT_EQ(Large.getKind(), EnumTy::Index3);
      EXPECT_EQ(Large.getRawKind(), EnumTy::LargeIndex);
      EXPECT_EQ(Large.getIndex(), 5000U);

      EXPECT_TRUE(Pointer.isValid());
      EXPECT_EQ(Pointer.getKind(), EnumTy::Index3);
      EXPECT_EQ(Pointer.getRawKind(), EnumTy::LargeIndex);
      EXPECT_EQ(Pointer.getIndex(), 5000U);
    }

    memcpy(&Ptr2Value, Ptr2, sizeof(Ptr2Value));
    EXPECT_EQ(Ptr2Value, -1ULL);
    memcpy(&Ptr1Value, Ptr1, sizeof(Ptr1Value));
    EXPECT_NE(Ptr1Value, -1ULL);
  }
  memcpy(&Ptr1Value, Ptr1, sizeof(Ptr1Value));
  EXPECT_EQ(Ptr1Value, -1ULL);

  delete [] InputPtr;
}

TEST(PointerIntEnumTest, CopyAssignLargeIntToPointer) {
  int *InputPtr = new int[1];
  InputPtr[0] = INT_MAX;

  PointerIntEnumTy Pointer(EnumTy::Ptr3, InputPtr);
  PointerIntEnumTy Large(EnumTy::Index3, 5000);

  void *Ptr = Large.getPointer();

  uint64_t Value;
  memcpy(&Value, Ptr, sizeof(Value));
  EXPECT_NE(Value, -1ULL);

  Large = Pointer;

  memcpy(&Value, Ptr, sizeof(Value));
  EXPECT_EQ(Value, -1ULL);

  EXPECT_TRUE(Pointer.isValid());
  EXPECT_EQ(Pointer.getKind(), EnumTy::Ptr3);
  EXPECT_EQ(Pointer.getRawKind(), EnumTy::Ptr3);
  EXPECT_EQ(Pointer.getPointer(), InputPtr);

  EXPECT_TRUE(Large.isValid());
  EXPECT_EQ(Large.getKind(), EnumTy::Ptr3);
  EXPECT_EQ(Large.getRawKind(), EnumTy::Ptr3);
  EXPECT_EQ(Large.getPointer(), InputPtr);

  delete [] InputPtr;
}

TEST(PointerIntEnumTest, CopyAssignSmallIntToPointer) {
  int *InputPtr = new int[1];
  InputPtr[0] = INT_MAX;

  PointerIntEnumTy Pointer(EnumTy::Ptr3, InputPtr);
  PointerIntEnumTy Small(EnumTy::Index3, 4095);

  Small = Pointer;

  EXPECT_TRUE(Pointer.isValid());
  EXPECT_EQ(Pointer.getKind(), EnumTy::Ptr3);
  EXPECT_EQ(Pointer.getRawKind(), EnumTy::Ptr3);
  EXPECT_EQ(Pointer.getPointer(), InputPtr);

  EXPECT_TRUE(Small.isValid());
  EXPECT_EQ(Small.getKind(), EnumTy::Ptr3);
  EXPECT_EQ(Small.getRawKind(), EnumTy::Ptr3);
  EXPECT_EQ(Small.getPointer(), InputPtr);

  delete [] InputPtr;
}

TEST(PointerIntEnumTest, CopyAssignPointerToSmallInt) {
  int *InputPtr = new int[1];
  InputPtr[0] = INT_MAX;

  PointerIntEnumTy Pointer(EnumTy::Ptr3, InputPtr);
  PointerIntEnumTy Small(EnumTy::Index3, 4095);

  EXPECT_TRUE(Pointer.isValid());
  EXPECT_EQ(Pointer.getKind(), EnumTy::Ptr3);
  EXPECT_EQ(Pointer.getRawKind(), EnumTy::Ptr3);
  EXPECT_EQ(Pointer.getPointer(), InputPtr);

  EXPECT_TRUE(Small.isValid());
  EXPECT_EQ(Small.getKind(), EnumTy::Index3);
  EXPECT_EQ(Small.getRawKind(), EnumTy::Index3);
  EXPECT_EQ(Small.getIndex(), 4095U);

  Pointer = Small;

  EXPECT_TRUE(Pointer.isValid());
  EXPECT_EQ(Pointer.getKind(), EnumTy::Index3);
  EXPECT_EQ(Pointer.getRawKind(), EnumTy::Index3);
  EXPECT_EQ(Pointer.getIndex(), 4095U);

  EXPECT_TRUE(Small.isValid());
  EXPECT_EQ(Small.getKind(), EnumTy::Index3);
  EXPECT_EQ(Small.getRawKind(), EnumTy::Index3);
  EXPECT_EQ(Small.getIndex(), 4095U);

  delete [] InputPtr;
}

TEST(PointerIntEnumTest, MoveAssignSmallIntToLargeInt) {
  PointerIntEnumTy Small(EnumTy::Index2, 4095);
  PointerIntEnumTy Large(EnumTy::Index3, 5000);

  Small = std::move(Large);

  EXPECT_TRUE(Small.isValid());
  EXPECT_EQ(Small.getKind(), EnumTy::Index3);
  EXPECT_EQ(Small.getRawKind(), EnumTy::LargeIndex);
  EXPECT_EQ(Small.getIndex(), 5000U);

  EXPECT_TRUE(Large.isValid());
  EXPECT_EQ(Large.getKind(), EnumTy::Index2);
  EXPECT_EQ(Large.getRawKind(), EnumTy::Index2);
  EXPECT_EQ(Large.getIndex(), 4095U);
}

TEST(PointerIntEnumTest, MoveAssignLargeIntToSmallInt) {
  PointerIntEnumTy Small(EnumTy::Index2, 4095);
  PointerIntEnumTy Large(EnumTy::Index3, 5000);

  Large = std::move(Small);

  EXPECT_TRUE(Small.isValid());
  EXPECT_EQ(Small.getKind(), EnumTy::Index3);
  EXPECT_EQ(Small.getRawKind(), EnumTy::LargeIndex);
  EXPECT_EQ(Small.getIndex(), 5000U);

  EXPECT_TRUE(Large.isValid());
  EXPECT_EQ(Large.getKind(), EnumTy::Index2);
  EXPECT_EQ(Large.getRawKind(), EnumTy::Index2);
  EXPECT_EQ(Large.getIndex(), 4095U);
}

TEST(PointerIntEnumTest, MoveAssignPointerToLargeInt) {
  int *IntPtr = new int[1];

  PointerIntEnumTy Pointer(EnumTy::Ptr1, IntPtr);
  PointerIntEnumTy Large(EnumTy::Index3, 5000);

  Pointer = std::move(Large);

  EXPECT_TRUE(Pointer.isValid());
  EXPECT_EQ(Pointer.getKind(), EnumTy::Index3);
  EXPECT_EQ(Pointer.getRawKind(), EnumTy::LargeIndex);
  EXPECT_EQ(Pointer.getIndex(), 5000U);

  EXPECT_TRUE(Large.isValid());
  EXPECT_EQ(Large.getKind(), EnumTy::Ptr1);
  EXPECT_EQ(Large.getRawKind(), EnumTy::Ptr1);
  EXPECT_EQ(Large.getPointer(), IntPtr);

  delete [] IntPtr;
}

TEST(PointerIntEnumTest, MoveAssignLargeIntToPointer) {
  int *IntPtr = new int[1];

  PointerIntEnumTy Pointer(EnumTy::Ptr1, IntPtr);
  PointerIntEnumTy Large(EnumTy::Index3, 5000);

  Large = std::move(Pointer);

  EXPECT_TRUE(Pointer.isValid());
  EXPECT_EQ(Pointer.getKind(), EnumTy::Index3);
  EXPECT_EQ(Pointer.getRawKind(), EnumTy::LargeIndex);
  EXPECT_EQ(Pointer.getIndex(), 5000U);

  EXPECT_TRUE(Large.isValid());
  EXPECT_EQ(Large.getKind(), EnumTy::Ptr1);
  EXPECT_EQ(Large.getRawKind(), EnumTy::Ptr1);
  EXPECT_EQ(Large.getPointer(), IntPtr);

  delete [] IntPtr;
}

TEST(PointerIntEnumTest, MoveAssignSmallIntToPointer) {
  int *IntPtr = new int[1];

  PointerIntEnumTy Pointer(EnumTy::Ptr1, IntPtr);
  PointerIntEnumTy Small(EnumTy::Index2, 4095U);

  Pointer = std::move(Small);

  EXPECT_TRUE(Pointer.isValid());
  EXPECT_EQ(Pointer.getKind(), EnumTy::Index2);
  EXPECT_EQ(Pointer.getRawKind(), EnumTy::Index2);
  EXPECT_EQ(Pointer.getIndex(), 4095U);

  EXPECT_TRUE(Small.isValid());
  EXPECT_EQ(Small.getKind(), EnumTy::Ptr1);
  EXPECT_EQ(Small.getRawKind(), EnumTy::Ptr1);
  EXPECT_EQ(Small.getPointer(), IntPtr);

  delete [] IntPtr;
}

TEST(PointerIntEnumTest, MoveAssignPointerToSmallInt) {
  int *IntPtr = new int[1];

  PointerIntEnumTy Pointer(EnumTy::Ptr1, IntPtr);
  PointerIntEnumTy Small(EnumTy::Index2, 4095U);

  Small = std::move(Pointer);

  EXPECT_TRUE(Pointer.isValid());
  EXPECT_EQ(Pointer.getKind(), EnumTy::Index2);
  EXPECT_EQ(Pointer.getRawKind(), EnumTy::Index2);
  EXPECT_EQ(Pointer.getIndex(), 4095U);

  EXPECT_TRUE(Small.isValid());
  EXPECT_EQ(Small.getKind(), EnumTy::Ptr1);
  EXPECT_EQ(Small.getRawKind(), EnumTy::Ptr1);
  EXPECT_EQ(Small.getPointer(), IntPtr);

  delete [] IntPtr;
}
