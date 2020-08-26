//===--- Concurrent.cpp - Concurrent data structure tests -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Concurrent.h"
#include "gtest/gtest.h"

#include "ThreadingHelpers.h"

using namespace swift;

TEST(ConcurrentReadableArrayTest, SingleThreaded) {
  ConcurrentReadableArray<size_t> array;
  
  auto add = [&](size_t limit) {
    for (size_t i = array.snapshot().count(); i < limit; i++)
      array.push_back(i);
  };
  auto check = [&]{
    size_t i = 0;
    for (auto element : array.snapshot()) {
      ASSERT_EQ(element, i);
      i++;
    }
  };
  
  check();
  add(1);
  check();
  add(16);
  check();
  add(100);
  check();
  add(1000);
  check();
  add(1000000);
  check();
}

TEST(ConcurrentReadableArrayTest, MultiThreaded) {
  const int writerCount = 16;
  const int readerCount = 8;
  const int insertCount = 100000;

  struct Value {
    int threadNumber;
    int x;
  };
  ConcurrentReadableArray<Value> array;

  // The writers will append values with their thread number and increasing
  // values of x.
  auto writer = [&](int threadNumber) {
    for (int i = 0; i < insertCount; i++)
      array.push_back({ threadNumber, i });
  };

  auto reader = [&] {
    // Track the maximum value we've seen for each writer thread.
    int maxByThread[writerCount];
    bool done = false;
    while (!done) {
      for (int i = 0; i < writerCount; i++)
        maxByThread[i] = -1;
      for (auto element : array.snapshot()) {
        ASSERT_LT(element.threadNumber, writerCount);
        // Each element we see must be larger than the maximum element we've
        // previously seen for that writer thread, otherwise that means that
        // we're seeing mutations out of order.
        ASSERT_GT(element.x, maxByThread[element.threadNumber]);
        maxByThread[element.threadNumber] = element.x;
      }

      // If the max for each thread is the max that'll be inserted, then we're
      // done and should exit.
      done = true;
      for (int i = 0; i < writerCount; i++) {
        if (maxByThread[i] < insertCount - 1)
          done = false;
      }
    }
  };

  threadedExecute(writerCount + readerCount, [&](int i) {
    if (i < writerCount)
      writer(i);
    else
      reader();
  });

  ASSERT_EQ(array.snapshot().count(), (size_t)writerCount * insertCount);
}

TEST(ConcurrentReadableArrayTest, MultiThreaded2) {
  const int writerCount = 16;
  const int readerCount = 8;
  const int insertCount = 100000;

  struct Value {
    int threadNumber;
    int x;
  };
  ConcurrentReadableArray<Value> array;

  // The writers will append values with their thread number and increasing
  // values of x.
  auto writer = [&](int threadNumber) {
    for (int i = 0; i < insertCount; i++)
      array.push_back({ threadNumber, i });
  };

  auto reader = [&] {
    // Track the maximum value we've seen for each writer thread.
    int maxByThread[writerCount];
    for (int i = 0; i < writerCount; i++)
      maxByThread[i] = -1;
    bool done = false;
    while (!done) {
      auto snapshot = array.snapshot();
      // Don't do anything until some data is actually added.
      if (snapshot.count() == 0)
        continue;

      // Grab the last element in the snapshot.
      auto element = snapshot.begin()[snapshot.count() - 1];
      ASSERT_LT(element.threadNumber, writerCount);
      // Each element we see must be equal to or larger than the maximum element
      // we've previously seen for that writer thread, otherwise that means that
      // we're seeing mutations out of order.
      ASSERT_GE(element.x, maxByThread[element.threadNumber]);
      maxByThread[element.threadNumber] = element.x;

      // We'll eventually see some thread add its maximum value. We'll call it
      // done when we reach that point.
      if (element.x == insertCount - 1)
        done = true;
    }
  };

  threadedExecute(writerCount + readerCount, [&](int i) {
    if (i < writerCount)
      writer(i);
    else
      reader();
  });

  ASSERT_EQ(array.snapshot().count(), (size_t)writerCount * insertCount);
}

struct SingleThreadedValue {
  size_t key;
  size_t x;
  SingleThreadedValue(size_t key, size_t x) : key(key), x(x) {}
  bool matchesKey(size_t key) { return this->key == key; }
  friend llvm::hash_code hash_value(const SingleThreadedValue &value) {
    return llvm::hash_value(value.key);
  }
};

TEST(ConcurrentReadableHashMapTest, SingleThreaded) {
  ConcurrentReadableHashMap<SingleThreadedValue> map;

  auto permute = [](size_t value) { return value ^ 0x33333333U; };

  auto add = [&](size_t limit) {
    for (size_t i = 0; i < limit; i++)
      map.getOrInsert(permute(i),
                      [&](SingleThreadedValue *value, bool created) {
                        if (created)
                          new (value) SingleThreadedValue(permute(i), i);
                        return true;
                      });
  };
  auto check = [&](size_t limit) {
    auto snapshot = map.snapshot();
    ASSERT_EQ(snapshot.find((size_t)~0), nullptr);
    for (size_t i = 0; i < limit; i++) {
      auto *value = snapshot.find(permute(i));
      ASSERT_NE(value, nullptr);
      ASSERT_EQ(permute(i), value->key);
      ASSERT_EQ(i, value->x);
    }
  };

  check(0);
  add(1);
  check(1);
  add(16);
  check(16);
  add(100);
  check(100);
  add(1000);
  check(1000);
  add(1000000);
  check(1000000);

  map.clear();
  check(0);

  add(1);
  check(1);
  map.clear();
  check(0);

  add(16);
  check(16);
  map.clear();
  check(0);

  add(100);
  check(100);
  map.clear();
  check(0);

  add(1000);
  check(1000);
  map.clear();
  check(0);

  add(1000000);
  check(1000000);
  map.clear();
  check(0);
}

struct MultiThreadedKey {
  int threadNumber;
  int n;
  friend llvm::hash_code hash_value(const MultiThreadedKey &value) {
    return llvm::hash_combine(value.threadNumber, value.n);
  }
};

struct MultiThreadedValue {
  int threadNumber;
  int n;
  int x;
  MultiThreadedValue(MultiThreadedKey key, int x)
      : threadNumber(key.threadNumber), n(key.n), x(x) {}
  bool matchesKey(const MultiThreadedKey &key) {
    return threadNumber == key.threadNumber && n == key.n;
  }
  friend llvm::hash_code hash_value(const MultiThreadedValue &value) {
    return llvm::hash_combine(value.threadNumber, value.n);
  }
};

// Test simultaneous readers and writers.
TEST(ConcurrentReadableHashMapTest, MultiThreaded) {
  const int writerCount = 16;
  const int readerCount = 8;
  const int insertCount = 10000;

  ConcurrentReadableHashMap<MultiThreadedValue> map;

  // NOTE: The bizarre lambdas around the ASSERT_ statements works around the
  // fact that these macros emit return statements, which conflict with our
  // need to return true/false from these lambdas. Wrapping them in a lambda
  // neutralizes the return.

  auto writer = [&](int threadNumber) {
    // Insert half, then insert all, to test adding an existing key.
    for (int i = 0; i < insertCount / 2; i++)
      map.getOrInsert(MultiThreadedKey{threadNumber, i}, [&](MultiThreadedValue
                                                                 *value,
                                                             bool created) {
        [&] { ASSERT_TRUE(created); }();
        new (value) MultiThreadedValue(MultiThreadedKey{threadNumber, i}, i);
        return true;
      });
    // Test discarding a new entry.
    for (int i = 0; i < insertCount; i++)
      map.getOrInsert(MultiThreadedKey{threadNumber, i},
                      [&](MultiThreadedValue *value, bool created) {
                        [&] { ASSERT_EQ(created, i >= insertCount / 2); }();
                        return false;
                      });
    for (int i = 0; i < insertCount; i++)
      map.getOrInsert(MultiThreadedKey{threadNumber, i}, [&](MultiThreadedValue
                                                                 *value,
                                                             bool created) {
        if (created) {
          [&] { ASSERT_GE(i, insertCount / 2); }();
          new (value) MultiThreadedValue(MultiThreadedKey{threadNumber, i}, i);
        } else {
          [&] { ASSERT_LT(i, insertCount / 2); }();
        }
        return true;
      });
  };

  auto reader = [&] {
    bool done = false;
    while (!done) {
      done = true;
      for (int threadNumber = 0; threadNumber < writerCount; threadNumber++) {
        // Read from the top down. We should see zero or more missing entries,
        // and then the rest are present. Any hole is a bug.
        int firstSeen = -1;
        auto snapshot = map.snapshot();
        for (int i = insertCount - 1; i >= 0; i--) {
          MultiThreadedKey key = {threadNumber, i};
          const MultiThreadedValue *value = snapshot.find(key);
          if (value) {
            if (firstSeen == -1)
              firstSeen = value->x;
            ASSERT_EQ(value->x, i);
          } else {
            ASSERT_EQ(firstSeen, -1);
            done = false;
          }
        }
      }
    }
  };

  threadedExecute(writerCount + readerCount, [&](int i) {
    if (i < writerCount)
      writer(i);
    else
      reader();
  });
}

// Test readers and writers while also constantly clearing the map.
TEST(ConcurrentReadableHashMapTest, MultiThreaded2) {
  const int writerCount = 16;
  const int readerCount = 8;
  const int insertCount = 10000;

  ConcurrentReadableHashMap<MultiThreadedValue> map;

  std::atomic<int> writerDoneCount = {0};
  auto writer = [&](int threadNumber) {
    for (int i = 0; i < insertCount; i++)
      map.getOrInsert(MultiThreadedKey{threadNumber, i}, [&](MultiThreadedValue
                                                                 *value,
                                                             bool created) {
        [&] { ASSERT_TRUE(created); }();
        new (value) MultiThreadedValue(MultiThreadedKey{threadNumber, i}, i);
        return true;
      });
    writerDoneCount.fetch_add(1, std::memory_order_relaxed);
  };

  auto reader = [&] {
    while (writerDoneCount.load(std::memory_order_relaxed) < writerCount) {
      for (int threadNumber = 0; threadNumber < writerCount; threadNumber++) {
        // Read from the top down. We should see a single contiguous region of
        // entries. Multiple regions indicates a bug.
        int firstSeen = -1;
        int lastSeen = -1;
        auto snapshot = map.snapshot();
        for (int i = insertCount - 1; i >= 0; i--) {
          MultiThreadedKey key = {threadNumber, i};
          const MultiThreadedValue *value = snapshot.find(key);
          if (value) {
            if (firstSeen == -1)
              firstSeen = value->x;
            if (lastSeen != -1)
              ASSERT_EQ(lastSeen, i + 1);
            lastSeen = value->x;
            ASSERT_EQ(value->x, i);
          }
        }
      }
    }
  };

  auto clear = [&] {
    while (writerDoneCount.load(std::memory_order_relaxed) < writerCount) {
      map.clear();
    }
  };

  threadedExecute(writerCount + readerCount + 1, [&](int i) {
    if (i < writerCount)
      writer(i);
    else if (i < writerCount + readerCount)
      reader();
    else
      clear();
  });
}

// Test readers and writers, with readers taking lots of snapshots.
TEST(ConcurrentReadableHashMapTest, MultiThreaded3) {
  const int writerCount = 16;
  const int readerCount = 8;
  const int insertCount = 10000;

  ConcurrentReadableHashMap<MultiThreadedValue> map;

  std::atomic<int> writerDoneCount = {0};
  auto writer = [&](int threadNumber) {
    for (int i = 0; i < insertCount; i++)
      map.getOrInsert(MultiThreadedKey{threadNumber, i}, [&](MultiThreadedValue
                                                                 *value,
                                                             bool created) {
        [&] { ASSERT_TRUE(created); }();
        new (value) MultiThreadedValue(MultiThreadedKey{threadNumber, i}, i);
        return true;
      });
    writerDoneCount.fetch_add(1, std::memory_order_relaxed);
  };

  auto reader = [&] {
    while (writerDoneCount.load(std::memory_order_relaxed) < writerCount) {
      for (int threadNumber = 0; threadNumber < writerCount; threadNumber++) {
        // Read from the top down. When we're not clearing the map, we should
        // see zero or more missing entries, and then the rest are present. Any
        // hole is a bug.
        int firstSeen = -1;
        int lastSeen = -1;
        for (int i = insertCount - 1; i >= 0; i--) {
          auto snapshot = map.snapshot();
          MultiThreadedKey key = {threadNumber, i};
          const MultiThreadedValue *value = snapshot.find(key);
          if (value) {
            if (firstSeen == -1)
              firstSeen = value->x;
            if (lastSeen != -1)
              ASSERT_EQ(lastSeen, i + 1);
            lastSeen = value->x;
            ASSERT_EQ(value->x, i);
          }
        }
      }
    }
  };

  threadedExecute(writerCount + readerCount, [&](int i) {
    if (i < writerCount)
      writer(i);
    else
      reader();
  });
}

// Test readers and writers, with readers taking lots of snapshots, and
// simultaneous clearing.
TEST(ConcurrentReadableHashMapTest, MultiThreaded4) {
  const int writerCount = 16;
  const int readerCount = 8;
  const int insertCount = 10000;

  ConcurrentReadableHashMap<MultiThreadedValue> map;

  std::atomic<int> writerDoneCount = {0};
  auto writer = [&](int threadNumber) {
    for (int i = 0; i < insertCount; i++)
      map.getOrInsert(MultiThreadedKey{threadNumber, i}, [&](MultiThreadedValue
                                                                 *value,
                                                             bool created) {
        [&] { ASSERT_TRUE(created); }();
        new (value) MultiThreadedValue(MultiThreadedKey{threadNumber, i}, i);
        return true;
      });
    writerDoneCount.fetch_add(1, std::memory_order_relaxed);
  };

  auto reader = [&] {
    while (writerDoneCount.load(std::memory_order_relaxed) < writerCount) {
      for (int threadNumber = 0; threadNumber < writerCount; threadNumber++) {
        // With clearing, we can't expect any particular pattern. Just validate
        // the values we do see, and make sure we don't crash.
        for (int i = insertCount - 1; i >= 0; i--) {
          auto snapshot = map.snapshot();
          MultiThreadedKey key = {threadNumber, i};
          const MultiThreadedValue *value = snapshot.find(key);
          if (value) {
            ASSERT_EQ(value->x, i);
          }
        }
      }
    }
  };

  auto clear = [&] {
    while (writerDoneCount.load(std::memory_order_relaxed) < writerCount) {
      map.clear();
    }
  };

  threadedExecute(writerCount + readerCount + 1, [&](int i) {
    if (i < writerCount)
      writer(i);
    else if (i < writerCount + readerCount)
      reader();
    else
      clear();
  });
}
