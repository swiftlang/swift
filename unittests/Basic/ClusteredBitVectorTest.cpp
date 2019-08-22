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

TEST(ClusteredBitVector, SetClearBit) {
  ClusteredBitVector vec;
  vec.appendClearBits(64);
  vec.setBit(3);
  vec.setBit(7);
  vec.setBit(7);
  vec.setBit(63);
  EXPECT_EQ(3u, vec.count());
  EXPECT_EQ(true, vec[3]);
  EXPECT_EQ(true, vec[7]);
  EXPECT_EQ(true, vec[63]);
  EXPECT_EQ(false, vec[0]);
  EXPECT_EQ(false, vec[14]);
  EXPECT_EQ(false, vec[62]);

  vec.clearBit(63);
  EXPECT_EQ(2u, vec.count());
  EXPECT_EQ(false, vec[63]);
}

TEST(ClusteredBitVector, FlipAllSmall) {
  ClusteredBitVector vec;
  vec.appendClearBits(48);
  EXPECT_EQ(false, vec[12]);
  EXPECT_EQ(0u, vec.count());
  vec.flipAll();
  EXPECT_EQ(true, vec[12]);
  EXPECT_EQ(48u, vec.count());
  vec.clearBit(7);
  EXPECT_EQ(true, vec[12]);
  EXPECT_EQ(false, vec[7]);
  EXPECT_EQ(47u, vec.count());
  vec.flipAll();
  EXPECT_EQ(false, vec[12]);
  EXPECT_EQ(true, vec[7]);
  EXPECT_EQ(1u, vec.count());
}

TEST(ClusteredBitVector, FlipAllBig) {
  ClusteredBitVector vec;
  vec.appendClearBits(163);
  EXPECT_EQ(false, vec[12]);
  EXPECT_EQ(0u, vec.count());
  vec.flipAll();
  EXPECT_EQ(true, vec[12]);
  EXPECT_EQ(163u, vec.count());
  vec.clearBit(7);
  EXPECT_EQ(true, vec[12]);
  EXPECT_EQ(false, vec[7]);
  EXPECT_EQ(162u, vec.count());
  vec.flipAll();
  EXPECT_EQ(false, vec[12]);
  EXPECT_EQ(true, vec[7]);
  EXPECT_EQ(1u, vec.count());
}
