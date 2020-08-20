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
