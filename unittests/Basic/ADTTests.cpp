#include "swift/Basic/Range.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Basic/ValueEnumerator.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(OptionSet, contains) {
  enum class Flags {
    A = 1 << 0,
    B = 1 << 1,
    C = 1 << 2
  };

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
  enum class Small : int8_t {
    A = 1 << 0
  };

  OptionSet<Small> small = Small::A;
  EXPECT_EQ(static_cast<intptr_t>(Small::A), static_cast<intptr_t>(small));


  enum class UPtr : uintptr_t {
    A = std::numeric_limits<uintptr_t>::max()
  };

  OptionSet<UPtr> uptr = UPtr::A;
  EXPECT_EQ(static_cast<intptr_t>(UPtr::A), static_cast<intptr_t>(uptr));


  enum class Ptr : intptr_t {
    A = std::numeric_limits<intptr_t>::min()
  };

  OptionSet<Ptr> ptr = Ptr::A;
  EXPECT_EQ(static_cast<intptr_t>(Ptr::A), static_cast<intptr_t>(ptr));
}

TEST(OptionSet, intptr_t_isConstructible) {
  // First check that std::is_constructible counts explicit conversion
  // operators.
  class AlwaysConvertible {
  public:
    explicit operator intptr_t () const { return 0; }
  };

  if (!std::is_constructible<intptr_t, AlwaysConvertible>::value) {
    // std::is_constructible doesn't test what we want it to. Just exit early.
    return;
  }

  enum class LongLong : unsigned long long {
    A = 1
  };
  bool isConvertible =
      std::is_constructible<intptr_t, OptionSet<LongLong>>::value;

  if (sizeof(intptr_t) < sizeof(long long)) {
    EXPECT_FALSE(isConvertible);
  } else {
    EXPECT_TRUE(isConvertible);
  }
}


TEST(ValueEnumerator, basic) {

  {
  ValueEnumerator<int> Trans;
  // Check that indexing is persistent.
  EXPECT_EQ(Trans.getIndex(99), Trans.getIndex(99));
  EXPECT_EQ(Trans.getIndex(100), Trans.getIndex(100));

  // Check that we don't have collisions.
  bool SameIndex = Trans.getIndex(82) == Trans.getIndex(73);
  EXPECT_FALSE(SameIndex);

  // Check that invalidation works.
  // After invalidation the old index must not be equal to the new index.
  size_t oldIndex = Trans.getIndex(99);
  Trans.invalidateValue(99);
  size_t newIndex = Trans.getIndex(99);
  EXPECT_FALSE(newIndex == oldIndex);
  }

  {
  const char *string_1 = "hello";
  const char *string_2 = "goodbye";
  const char *string_3 = ":-)";
  ValueEnumerator<const char*> Trans;
  EXPECT_EQ(Trans.getIndex(nullptr), Trans.getIndex(nullptr));
  EXPECT_EQ(Trans.getIndex(string_1), Trans.getIndex(string_1));
  EXPECT_EQ(Trans.getIndex(string_2), Trans.getIndex(string_2));

  // Check that invalidation works.
  size_t oldIndex = Trans.getIndex(string_3);
  Trans.invalidateValue(string_3);
  size_t newIndex = Trans.getIndex(string_3);
  EXPECT_FALSE(newIndex == oldIndex);

  // Check that different values don't give the same index.
  EXPECT_FALSE(Trans.getIndex(string_2) == Trans.getIndex(string_3));
  }


  {
  ValueEnumerator<int> Trans;
  // Check a bunch of integers.
  for (int i = 1; i < 10000; i++) {
    EXPECT_TRUE(Trans.getIndex(0) != Trans.getIndex(i));
  }

  // Check that there are no accidental collisions.
  for (int i = 0; i < 10000; i++) {
    for (int j = 1; j < 10; j++) {
      EXPECT_TRUE(Trans.getIndex(i) != Trans.getIndex(i + j));
    }
  }

  // Check that indexing is still persistent.
  EXPECT_EQ(Trans.getIndex(100), Trans.getIndex(100));
  }

}

TEST(Range, basic) {
  unsigned start = 0;
  unsigned end = 50;
  unsigned expected_i = start;
  bool sawEndMinusOne = false;
  for (unsigned i : range(start, end)) {
    EXPECT_GE(i, start);
    EXPECT_LT(i, end);
    EXPECT_EQ(expected_i, i);
    ++expected_i;

    sawEndMinusOne |= (i == (end-1));
  }
  EXPECT_TRUE(sawEndMinusOne);
}

TEST(ReverseRange, basic) {
  unsigned start = 0;
  unsigned end = 50;
  unsigned expected_i = end;
  bool sawStartPlusOne = false;
  for (unsigned i : reverse_range(start, end)) {
    EXPECT_GT(i, start);
    EXPECT_LE(i, end);
    EXPECT_EQ(expected_i, i);
    --expected_i;

    sawStartPlusOne |= (i == start+1);
  }
  EXPECT_TRUE(sawStartPlusOne);
}
