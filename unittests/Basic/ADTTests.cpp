#include "swift/Basic/OptionSet.h"
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
