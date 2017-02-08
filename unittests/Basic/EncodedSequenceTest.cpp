#include "swift/Basic/EncodedSequence.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {

struct Element {
  unsigned X, Y, Z;

  static Element decode(const EncodedSequenceBase::Chunk *&ptr) {
    auto x = EncodedSequenceBase::decodeIndex(ptr);
    auto y = EncodedSequenceBase::decodeIndex(ptr);
    auto z = EncodedSequenceBase::decodeIndex(ptr);
    return { x, y, z };
  }

  void encode(EncodedSequenceBase::Chunk *&ptr) const {
    EncodedSequenceBase::encodeIndex(X, ptr);
    EncodedSequenceBase::encodeIndex(Y, ptr);
    EncodedSequenceBase::encodeIndex(Z, ptr);
  }

  unsigned getEncodedSize() const {
    return EncodedSequenceBase::getEncodedIndexSize(X)
         + EncodedSequenceBase::getEncodedIndexSize(Y)
         + EncodedSequenceBase::getEncodedIndexSize(Z);
  }
};

enum class DataSet {
  None, Small, Big
};

static const DataSet AllDataSets[] = {
  DataSet::None,
  DataSet::Small,
  DataSet::Big,
};

struct Tester {
  SmallVector<Element, 4> Vector;
  EncodedSequence<Element> Sequence;

  Tester() {
    EXPECT_TRUE(Sequence.empty());
  }

  void push_back(Element elt) {
    Vector.push_back(elt);
    Sequence.push_back(elt);
    EXPECT_FALSE(Sequence.empty());
  }

  // The small data set should use inline storage on any pointer width.
  void push_small() {
    push_back({ 5, 75, 13 });
    check();
  }

  // The big data set should use out-of-line storage on any pointer width.
  void push_big() {
    push_back({ 557, 33, 197 });
    check();
    push_back({ 234897, 3, 0 });
    check();
    push_back({ 10123, 34987345, 1387123 });
    check();
  }

  void push(DataSet set) {
    switch (set) {
    case DataSet::None: return;
    case DataSet::Small: return push_small();
    case DataSet::Big: return push_big();
    }
  }

  void check() {
    auto vi = Vector.begin(), ve = Vector.end();
    auto si = Sequence.begin(), se = Sequence.end();
    while (true) {
      if (vi == ve) {
        EXPECT_EQ(se, si);
        return;
      }

      auto velt = *vi;
      auto selt = *si;
      EXPECT_EQ(velt.X, selt.X);
      EXPECT_EQ(velt.Y, selt.Y);
      EXPECT_EQ(velt.Z, selt.Z);

      auto selt2 = *si;
      EXPECT_EQ(selt.X, selt2.X);
      EXPECT_EQ(selt.Y, selt2.Y);
      EXPECT_EQ(selt.Z, selt2.Z);

      ++vi;
      ++si;
    }
  }
};

} // end anonymous namespace

TEST(EncodedSequenceTest, PushIterate) {
  Tester tester;
  tester.check();
  tester.push_back({ 557, 33, 197 });
  tester.check();
  tester.push_back({ 234897, 3, 0 });
  tester.check();
  tester.push_back({ 10123, 34987345, 1387123 });
  tester.check();
}


static void testCopyCtor(DataSet srcSet) {
  Tester src;
  src.push(srcSet);

  Tester dest = src;

  src.check();
  dest.check();
}

TEST(EncodedSequenceTest, CopyCtor) {
  for (auto srcSet : AllDataSets)
    testCopyCtor(srcSet);
}

static void testMoveCtor(DataSet srcSet) {
  Tester src;
  src.push(srcSet);

  Tester dest = std::move(src);

  EXPECT_TRUE(src.Sequence.empty());
  src.Vector.clear();

  src.check();
  dest.check();
}

TEST(EncodedSequenceTest, MoveCtor) {
  for (auto srcSet : AllDataSets)
    testMoveCtor(srcSet);
}

static void testCopyAssign(DataSet destSet, DataSet srcSet) {
  Tester src;
  src.push(srcSet);

  Tester dest;
  dest.push(destSet);

  dest = src;

  src.check();
  dest.check();
}

TEST(EncodedSequenceTest, CopyAssign) {
  for (auto destSet : AllDataSets)
    for (auto srcSet : AllDataSets)
      testCopyAssign(destSet, srcSet);
}

static void testMoveAssign(DataSet destSet, DataSet srcSet) {
  Tester src;
  src.push(srcSet);

  Tester dest;
  dest.push(destSet);

  dest = std::move(src);

  EXPECT_TRUE(src.Sequence.empty());
  src.Vector.clear();

  src.check();
  dest.check();
}

TEST(EncodedSequenceTest, MoveAssign) {
  for (auto destSet : AllDataSets)
    for (auto srcSet : AllDataSets)
      testMoveAssign(destSet, srcSet);
}
