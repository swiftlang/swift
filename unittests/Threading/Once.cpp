//===--- Once.cpp - Tests the swift::once() implementation ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Threading/Once.h"
#include "gtest/gtest.h"

#include <cstring>

#include "ThreadingHelpers.h"

using namespace swift;

// Check that swift::once calls the function, with the correct argument
TEST(OnceTest, once_calls_function) {
  static swift::once_t predicate;
  bool wasCalled = false;

  swift::once(
      predicate,
      [](void *ctx) {
        bool *pWasCalled = static_cast<bool *>(ctx);
        *pWasCalled = true;
      },
      &wasCalled);

  ASSERT_TRUE(wasCalled);
}

// Check that calling swift::once twice only calls the function once
TEST(OnceTest, once_calls_only_once) {
  static swift::once_t predicate;
  unsigned callCount = 0;

  void (*fn)(void *) = [](void *ctx) {
    unsigned *pCallCount = static_cast<unsigned *>(ctx);
    ++*pCallCount;
  };

  swift::once(predicate, fn, &callCount);
  swift::once(predicate, fn, &callCount);

  ASSERT_EQ(1u, callCount);
}

// Check that swift::once works when threaded
TEST(OnceTest, once_threaded) {
  void (*fn)(void *) = [](void *ctx) {
    unsigned *pCallCount = static_cast<unsigned *>(ctx);
    ++*pCallCount;
  };

  for (unsigned tries = 0; tries < 1000; ++tries) {
    swift::once_t predicate;
    unsigned callCount = 0;

    // We're being naughty here; swift::once_t is supposed to be global/static,
    // but since we know what we're doing, this should be OK.
    std::memset(&predicate, 0, sizeof(predicate));

    threadedExecute(16, [&](int) { swift::once(predicate, fn, &callCount); });

    ASSERT_EQ(1u, callCount);
  }
}

// Check that swift::once works with a C++ lambda
TEST(OnceTest, once_lambda) {
  static swift::once_t predicate;
  unsigned callCount = 0;

  auto fn = [&callCount]() {
    ++callCount;
  };

  swift::once(predicate, fn);
  swift::once(predicate, fn);

  ASSERT_EQ(1u, callCount);
}
