#include "swift/Basic/ClusteredBitVector.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(ClusteredBitVector, Add) {
  ClusteredBitVector vec;
  vec.add(31, 2133572605);

  EXPECT_EQ(31u, vec.size());
  EXPECT_EQ(true, vec[0]);
  EXPECT_EQ(false, vec[1]);
  EXPECT_EQ(true, vec[30]);
}

TEST(ClusteredBitVector, AppendEmptyAfterAdd) {
  ClusteredBitVector vec;
  vec.add(31, 2133572605);

  ClusteredBitVector emptyVec;
  vec.append(emptyVec);

  EXPECT_EQ(31u, vec.size());
  EXPECT_EQ(true, vec[0]);
  EXPECT_EQ(false, vec[1]);
  EXPECT_EQ(true, vec[30]);
}

TEST(ClusteredBitVector, AppendSetBits) {
  ClusteredBitVector vec;
  vec.add(23, 7988315);
  vec.appendSetBits(77);
  EXPECT_EQ(100u, vec.size());
  EXPECT_EQ(true, vec[0]);
  EXPECT_EQ(true, vec[30]);
  EXPECT_EQ(true, vec[70]);
}

TEST(ClusteredBitVector, AddAdd) {
  ClusteredBitVector vec;
  vec.add(8, 206);
  vec.add(17, 56854);
  EXPECT_EQ(25u, vec.size());
  EXPECT_EQ(true, vec[9]);
}

TEST(ClusteredBitVector, Copies) {
  ClusteredBitVector orig;
  orig.appendClearBits(71);

  ClusteredBitVector copy = orig;
  ClusteredBitVector vec;
  vec = copy;

  EXPECT_EQ(71u, vec.size());
  EXPECT_EQ(71u, copy.size());
  EXPECT_EQ(71u, orig.size());
  EXPECT_EQ(false, vec[64]);
  EXPECT_EQ(false, copy[64]);
  EXPECT_EQ(false, orig[64]);
}

TEST(ClusteredBitVector, CopyClearIntoAllocated) {
  ClusteredBitVector temp;
  temp.appendSetBits(94);

  ClusteredBitVector orig;
  orig.appendClearBits(71);

  ClusteredBitVector copy = orig;
  ClusteredBitVector vec;
  vec = temp;
  vec = copy;

  EXPECT_EQ(71u, vec.size());
  EXPECT_EQ(71u, copy.size());
  EXPECT_EQ(71u, orig.size());
  EXPECT_EQ(false, vec[64]);
  EXPECT_EQ(false, copy[64]);
  EXPECT_EQ(false, orig[64]);
}

TEST(ClusteredBitVector, AddMoveAdd) {
  ClusteredBitVector orig;
  orig.add(16, 41010);

  ClusteredBitVector vec;
  ClusteredBitVector temp = std::move(orig);
  vec = std::move(temp);
  EXPECT_EQ(0u, temp.size());

  vec.add(28, 146898948);
  EXPECT_EQ(true, vec[32]);
}

TEST(ClusteredBitVector, MultiChunkAppend) {
  ClusteredBitVector temp;
  temp.appendSetBits(72);
  temp.appendClearBits(22);
  temp.appendSetBits(83);

  ClusteredBitVector vec;
  vec.add(16, 0x000000000000967b);
  vec.append(temp);
  EXPECT_EQ(true, vec[64]);
}

TEST(ClusteredBitVector, AssignAfterGrowth) {
  ClusteredBitVector temp;
  temp.appendClearBits(118);

  ClusteredBitVector vec;
  vec.appendSetBits(65);
  vec = std::move(temp);
  EXPECT_EQ(false, vec[64]);
}

/// define == on llvm::Optional, you jerks
template <class T> struct ComparableOptional {
  Optional<T> Value;
  ComparableOptional(const T &value) : Value(value) {}
  ComparableOptional() : Value() {}

  bool operator==(const Optional<T> &other) const {
    if (Value.hasValue()) {
      return other.hasValue()
          && Value.getValue() == other.getValue();
    } else {
      return !other.hasValue();
    }
  }
};

TEST(ClusteredBitVector, Enumeration) {
  ClusteredBitVector temp;
  temp.appendClearBits(256);
  temp.setBit(64);
  temp.setBit(40);
  temp.setBit(39);
  temp.setBit(63);
  temp.setBit(201);

  using Opt = ComparableOptional<size_t>;

  auto enumerator = temp.enumerateSetBits();
  EXPECT_EQ(Opt(39), enumerator.findNext());
  EXPECT_EQ(Opt(40), enumerator.findNext());
  EXPECT_EQ(Opt(63), enumerator.findNext());
  EXPECT_EQ(Opt(64), enumerator.findNext());
  EXPECT_EQ(Opt(201), enumerator.findNext());
  EXPECT_EQ(Opt(), enumerator.findNext());
  EXPECT_EQ(Opt(), enumerator.findNext());
  EXPECT_EQ(Opt(), enumerator.findNext());
}
