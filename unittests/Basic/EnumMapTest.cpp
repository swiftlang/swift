#include "swift/Basic/EnumMap.h"
#include "llvm/ADT/DenseSet.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {

constexpr size_t MissingValue = ~(size_t) 0;

enum class A : uint16_t {
  lowerBound = 0,
  numValues = 1000
};

} // end anonymous namespace

template <>
struct swift::EnumTraits<A> {
  static constexpr size_t NumValues = (size_t) A::numValues;
};

namespace {

static void insertSuccess(EnumMap<A, size_t> &map, size_t key, size_t value) {
  auto result = map.insert(A(key), value);
  EXPECT_TRUE(result.second);
  EXPECT_NE(map.end(), result.first);
  EXPECT_EQ(value, *result.first);
}

static void insertFailure(EnumMap<A, size_t> &map, size_t key, size_t value,
                          size_t actualValue) {
  auto result = map.insert(A(key), value);
  EXPECT_FALSE(result.second);
  EXPECT_NE(map.end(), result.first);
  EXPECT_EQ(actualValue, *result.first);
}

static void lookupSuccess(EnumMap<A, size_t> &map, size_t key, size_t value) {
  auto result = map.find(A(key));
  EXPECT_NE(map.end(), result);
  EXPECT_EQ(value, *result);
}

static void lookupFailure(EnumMap<A, size_t> &map, size_t key) {
  auto result = map.find(A(key));
  EXPECT_EQ(map.end(), result);
}

#define INSERT_SUCCESS(KEY, VALUE) \
  insertSuccess(map, KEY, VALUE)
#define INSERT_FAILURE(KEY, VALUE, ACTUAL) \
  insertFailure(map, KEY, VALUE, ACTUAL)
#define LOOKUP_SUCCESS(KEY, VALUE) \
  lookupSuccess(map, KEY, VALUE)
#define LOOKUP_FAILURE(KEY) \
  lookupFailure(map, KEY)

struct entry {
  size_t key;
  size_t value;
};

static const entry globalEntries[] = {
  { 218, 110145 },
  { 361, 927012 },
  { 427, 608227 },
  { 861, 158552 },
  { 101, 466452 },
  { 391, 920472 },
  { 960, 522979 },
  { 36, 433291 },
  { 432, 110883 },
  { 752, 903125 },
  { 549, 887829 },
  { 475, 748953 },
  { 295, 214526 },
  { 533, 896211 },
  { 961, 684099 },
  { 230, 387362 },
  { 988, 205038 },
  { 980, 838945 },
  { 43, 319398 },
  { 704, 960347 },
  { 270, 837198 },
  { 611, 310181 },
  { 638, 44564 },
  { 193, 210584 },
  { 281, 620103 },
  { 682, 462845 },
  { 419, 85019 },
  { 812, 541739 },
  { 580, 266684 },
  { 559, 101634 },
  { 506, 639451 },
  { 96, 782184 },
  { 996, 927190 },
  { 392, 586071 },
  { 928, 50086 },
  { 976, 681150 },
  { 953, 172478 },
  { 863, 512828 },
  { 569, 947708 },
  { 139, 131866 },
  { 628, 884682 },
  { 877, 636903 },
  { 49, 871169 },
  { 172, 524694 },
  { 768, 211821 },
  { 104, 126356 },
  { 552, 262470 },
  { 343, 857409 },
  { 426, 535485 },
  { 84, 954703 },
  { 239, 889527 },
};

} // end anonymous namespace


TEST(EnumMap, Basic) {
  EnumMap<A, size_t> map;

  auto entries = llvm::ArrayRef(globalEntries);

  for (size_t iteration : range(entries.size())) {
    EXPECT_EQ(iteration, map.size());
    EXPECT_EQ(iteration == 0, map.empty());

    // Check that previous entries are still there.
    for (size_t i : range(iteration)) {
      LOOKUP_SUCCESS(entries[i].key, entries[i].value);
      INSERT_FAILURE(entries[i].key, MissingValue, entries[i].value);
    }

    // Check that later entries are not there.
    for (size_t i : range(iteration, entries.size())) {
      LOOKUP_FAILURE(entries[i].key);
    }

    INSERT_SUCCESS(entries[iteration].key, entries[iteration].value);
    LOOKUP_SUCCESS(entries[iteration].key, entries[iteration].value);
  }

  EXPECT_EQ(entries.size(), map.size());

  size_t i = 0;
  for (auto &value : map) {
    EXPECT_EQ(entries[i].value, value);
    i++;
  }
  EXPECT_EQ(entries.size(), i);
}
