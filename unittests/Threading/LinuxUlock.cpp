//===--- LinuxULock.cpp - Tests the Linux ulock implementation ------------===//
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

#if SWIFT_THREADING_LINUX

#include "swift/Threading/Impl/Linux/ulock.h"
#include "gtest/gtest.h"

#include "LockingHelpers.h"

using namespace swift;
using namespace swift::threading_impl;

// -----------------------------------------------------------------------------

// Use the existing locking tests to check threaded operation
class UlockMutex {
private:
  linux::ulock_t lock_;

public:
  UlockMutex() : lock_(0) {}

  void lock() { linux::ulock_lock(&lock_); }
  void unlock() { linux::ulock_unlock(&lock_); }
  bool try_lock() { return linux::ulock_trylock(&lock_); }
};

TEST(LinuxUlockTest, SingleThreaded) {
  UlockMutex mutex;
  basicLockable(mutex);
}

TEST(LinuxUlockTest, SingleThreadedTryLock) {
  UlockMutex mutex;
  tryLockable(mutex);
}

TEST(LinuxULockTest, BasicLockableThreaded) {
  UlockMutex mutex;
  basicLockableThreaded(mutex);
}

TEST(LinuxULockTest, LockableThreaded) {
  UlockMutex mutex;
  lockableThreaded(mutex);
}

#endif // SWIFT_THREADING_LINUX
