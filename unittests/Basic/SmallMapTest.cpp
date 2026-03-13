#include "swift/Basic/SmallMap.h"
#include "llvm/ADT/DenseSet.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {

constexpr size_t MissingValue = ~(size_t) 0;
constexpr size_t EmptyKey = ~(size_t) 1;
constexpr size_t TombstoneKey = ~(size_t) 2;

template <class T>
struct Tracker {
  llvm::DenseSet<T> set;

  Tracker() = default;
  Tracker(const Tracker &) = delete;
  Tracker &operator=(const Tracker &) = delete;

  bool empty() const {
    return set.empty();
  }

  void insert(T value) {
    EXPECT_TRUE(set.insert(value).second);
  }

  void check(T value) {
    EXPECT_TRUE(set.contains(value));
  }

  void remove(T value) {
    EXPECT_TRUE(set.erase(value));
  }
};

class A {
  Tracker<const A *> *tracker;
  size_t value;

  explicit A(size_t specialValue) : tracker(nullptr), value(specialValue) {}

  void assignTrackers(const A &other) {
    if (tracker)
      tracker->check(this);
    if (other.tracker)
      other.tracker->check(&other);
    if (tracker != other.tracker) {
      if (tracker)
        tracker->remove(this);
      if (other.tracker)
        other.tracker->insert(this);
      tracker = other.tracker;
    }
  }

public:
  static A getEmptyKey() {
    return A(EmptyKey);
  }
  static A getTombstoneKey() {
    return A(TombstoneKey);
  }

  A(Tracker<const A *> *tracker, size_t value) : tracker(tracker), value(value) {
    if (tracker)
      tracker->insert(this);
  }

  A(const A &other) : tracker(other.tracker), value(other.value) {
    if (tracker) {
      tracker->insert(this);
      tracker->check(&other);
    }
  }

  A(A &&other) : tracker(other.tracker), value(other.value) {
    if (tracker) {
      tracker->insert(this);
      tracker->check(&other);
    }
  }

  A &operator=(const A &other) {
    assignTrackers(other);
    value = other.value;
    return *this;
  }

  A &operator=(A &&other) {
    assignTrackers(other);
    value = other.value;
    return *this;
  }

  ~A() {
    if (tracker)
      tracker->remove(this);
  }

  size_t getValue() const {
    if (tracker)
      tracker->check(this);
    return value;
  }

  friend bool operator==(const A &lhs, const A &rhs) {
    return lhs.getValue() == rhs.getValue();
  }
};

static void insertSuccess(SmallMap<A, A> &map, Tracker<const A*> &tracker,
                          size_t key, size_t value) {
  auto result = map.insert(A(&tracker, key), A(&tracker, value));
  EXPECT_TRUE(result.second);
  EXPECT_NE(map.end(), result.first);
  EXPECT_EQ(value, result.first->getValue());
}

static void insertFailure(SmallMap<A, A> &map, Tracker<const A*> &tracker,
                          size_t key, size_t value, size_t actualValue) {
  auto result = map.insert(A(&tracker, key), A(&tracker, value));
  EXPECT_FALSE(result.second);
  EXPECT_NE(map.end(), result.first);
  EXPECT_EQ(actualValue, result.first->getValue());
}

static void lookupSuccess(SmallMap<A, A> &map, Tracker<const A*> &tracker,
                          size_t key, size_t value) {
  auto result = map.find(A(&tracker, key));
  EXPECT_NE(map.end(), result);
  EXPECT_EQ(value, result->getValue());
}

static void lookupFailure(SmallMap<A, A> &map, Tracker<const A*> &tracker,
                          size_t key) {
  auto result = map.find(A(&tracker, key));
  EXPECT_EQ(map.end(), result);
}

#define INSERT_SUCCESS(KEY, VALUE) \
  insertSuccess(map, tracker, KEY, VALUE)
#define INSERT_FAILURE(KEY, VALUE, ACTUAL) \
  insertFailure(map, tracker, KEY, VALUE, ACTUAL)
#define LOOKUP_SUCCESS(KEY, VALUE) \
  lookupSuccess(map, tracker, KEY, VALUE)
#define LOOKUP_FAILURE(KEY) \
  lookupFailure(map, tracker, KEY)

struct entry {
  size_t key;
  size_t value;
};

static const entry globalEntries[] = {
  { 833286, 244010 },
  { 21885, 583865 },
  { 98803, 373843 },
  { 757849, 280197 },
  { 544837, 319456 },
  { 301715, 409382 },
  { 214164, 173603 },
  { 90472, 679461 },
  { 454735, 523445 },
  { 726077, 442142 },
  { 757356, 26085 },
  { 83528, 609269 },
  { 25506, 528950 },
  { 66693, 225472 },
  { 850311, 274721 },
  { 575211, 385129 },
  { 496336, 530893 },
  { 753928, 460664 },
  { 569603, 263213 },
  { 863114, 294890 },
  { 289913, 871387 },
  { 567663, 970826 },
  { 54922, 182147 },
  { 234275, 516764 },
  { 521608, 771620 },
  { 38169, 832007 },
  { 777822, 704626 },
  { 608984, 769469 },
  { 696833, 136927 },
  { 336429, 615964 },
  { 203555, 147525 },
  { 759946, 740892 },
  { 702926, 137033 },
  { 86701, 400847 },
  { 177435, 145944 },
  { 424806, 194239 },
  { 628673, 279972 },
  { 843621, 449262 },
  { 372083, 860665 },
  { 642760, 534411 },
  { 777604, 996069 },
  { 942048, 227549 },
  { 43009, 551907 },
  { 814924, 532395 },
  { 480414, 327500 },
  { 49853, 745810 },
  { 157379, 947358 },
  { 313310, 851746 },
  { 957411, 179233 },
  { 32217, 35134 },
  { 684458, 208518 },
  { 944720, 998758 },
  { 533638, 728837 },
  { 670556, 946584 },
  { 466090, 456504 },
  { 213558, 326747 },
  { 967293, 15416 },
  { 370014, 356011 },
};

} // end anonymous namespace


TEST(SmallMap, Basic) {
  Tracker<const A *> tracker;
  {
    SmallMap<A, A> map;

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
      EXPECT_EQ(entries[i].value, value.getValue());
      i++;
    }
    EXPECT_EQ(entries.size(), i);
  }
  EXPECT_TRUE(tracker.empty());
}

template <>
struct llvm::DenseMapInfo<A> {
  static inline A getEmptyKey() {
    return A::getEmptyKey();
  }
  static inline A getTombstoneKey() {
    return A::getTombstoneKey();
  }
  static unsigned getHashValue(const A &val) {
    return val.getValue();
  }
  static bool isEqual(const A &lhs, const A &rhs) {
    return lhs == rhs;
  }

};
