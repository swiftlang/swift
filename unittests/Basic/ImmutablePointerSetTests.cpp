#include "swift/Basic/ImmutablePointerSet.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Allocator.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(ImmutableSortedSet, OneElementSets) {
  llvm::BumpPtrAllocator BPA;
  ImmutablePointerSetFactory<unsigned> F(BPA);

  unsigned *ptr1 = (unsigned *)3;
  auto *OneEltSet1 = F.get(ptr1);
  EXPECT_EQ(OneEltSet1, F.get(ptr1));
  EXPECT_EQ(OneEltSet1, F.concat(OneEltSet1, OneEltSet1));

  unsigned *ptr2 = (unsigned *)2;
  auto *OneEltSet2 = F.get(ptr2);
  EXPECT_EQ(OneEltSet2, F.get(ptr2));
  EXPECT_EQ(OneEltSet2, F.concat(OneEltSet2, OneEltSet2));
  EXPECT_NE(OneEltSet2, OneEltSet1);

  auto *Concat1 = F.concat(OneEltSet1, OneEltSet2);
  auto *Concat2 = F.concat(OneEltSet2, OneEltSet1);
  EXPECT_NE(OneEltSet1, Concat1);
  EXPECT_NE(OneEltSet2, Concat1);
  EXPECT_EQ(Concat1, Concat2);
  EXPECT_EQ(Concat1, F.concat(Concat1, Concat1));
  EXPECT_EQ(Concat2, F.concat(Concat2, Concat2));
  EXPECT_EQ(Concat1, F.concat(Concat1, OneEltSet1));
  EXPECT_EQ(Concat1, F.concat(Concat1, OneEltSet2));

  EXPECT_EQ(Concat1->size(), 2U);
  EXPECT_FALSE(Concat1->empty());
  EXPECT_EQ(*Concat1->begin(), (unsigned *)2);
  EXPECT_EQ(*std::next(Concat1->begin()), (unsigned *)3);

  EXPECT_EQ(F.getEmptySet(), F.getEmptySet());
  EXPECT_EQ(OneEltSet1, F.concat(F.getEmptySet(), OneEltSet1));
  EXPECT_EQ(OneEltSet1, F.concat(OneEltSet1, F.getEmptySet()));
}

TEST(ImmutablePointerSet, MultipleElementSets) {
  llvm::BumpPtrAllocator BPA;
  ImmutablePointerSetFactory<unsigned> F(BPA);

  unsigned *Ptr1 = (unsigned *)3;
  unsigned *Ptr2 = (unsigned *)4;
  unsigned *Ptr3 = (unsigned *)3;
  unsigned *Ptr4 = (unsigned *)5;
  unsigned *Ptr5 = (unsigned *)6;
  ArrayRef<unsigned *> Data1 = {Ptr1, Ptr2};

  auto *TwoEltSet = F.get(Data1);
  EXPECT_FALSE(TwoEltSet->empty());
  EXPECT_EQ(TwoEltSet->size(), 2u);
  EXPECT_TRUE(TwoEltSet->count(Ptr1));
  EXPECT_TRUE(TwoEltSet->count(Ptr2));
  EXPECT_TRUE(TwoEltSet->count(Ptr3));
  EXPECT_FALSE(TwoEltSet->count(Ptr4));
  EXPECT_FALSE(TwoEltSet->count(Ptr5));

  auto *ThreeEltSet = F.concat(F.get(Ptr4), TwoEltSet);
  EXPECT_FALSE(ThreeEltSet->empty());
  EXPECT_EQ(ThreeEltSet->size(), 3u);
  EXPECT_NE(*ThreeEltSet, *TwoEltSet);
  EXPECT_TRUE(ThreeEltSet->count(Ptr1));
  EXPECT_TRUE(ThreeEltSet->count(Ptr2));
  EXPECT_TRUE(ThreeEltSet->count(Ptr3));
  EXPECT_TRUE(ThreeEltSet->count(Ptr4));
  EXPECT_FALSE(ThreeEltSet->count(Ptr5));
  EXPECT_EQ(ThreeEltSet, F.concat(TwoEltSet, ThreeEltSet));

  ArrayRef<unsigned *> Data2 = {Ptr3, Ptr4, Ptr5};
  auto *PartialOverlapSet = F.get(Data2);
  EXPECT_FALSE(PartialOverlapSet->empty());
  EXPECT_EQ(PartialOverlapSet->size(), 3u);
  EXPECT_TRUE(PartialOverlapSet->count(Ptr1));
  EXPECT_FALSE(PartialOverlapSet->count(Ptr2));
  EXPECT_TRUE(PartialOverlapSet->count(Ptr3));
  EXPECT_TRUE(PartialOverlapSet->count(Ptr4));
  EXPECT_TRUE(PartialOverlapSet->count(Ptr5));
  EXPECT_NE(*PartialOverlapSet, *ThreeEltSet);
  EXPECT_NE(*PartialOverlapSet, *TwoEltSet);

  auto *MixOfThreeAndPartialOverlap = ThreeEltSet->concat(PartialOverlapSet);
  EXPECT_FALSE(MixOfThreeAndPartialOverlap->empty());
  EXPECT_EQ(MixOfThreeAndPartialOverlap->size(), 4u);
  EXPECT_TRUE(MixOfThreeAndPartialOverlap->count(Ptr1));
  EXPECT_TRUE(MixOfThreeAndPartialOverlap->count(Ptr2));
  EXPECT_TRUE(MixOfThreeAndPartialOverlap->count(Ptr3));
  EXPECT_TRUE(MixOfThreeAndPartialOverlap->count(Ptr4));
  EXPECT_TRUE(MixOfThreeAndPartialOverlap->count(Ptr5));
  EXPECT_NE(*MixOfThreeAndPartialOverlap, *PartialOverlapSet);
  EXPECT_NE(*MixOfThreeAndPartialOverlap, *ThreeEltSet);
  EXPECT_NE(*MixOfThreeAndPartialOverlap, *TwoEltSet);
}
