//===--- Mutex.cpp - Mutex Tests ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Threading/Mutex.h"
#include "gtest/gtest.h"
#include <atomic>
#include <chrono>
#include <map>
#include <random>

#include "LockingHelpers.h"

using namespace swift;

// -----------------------------------------------------------------------------

TEST(MutexTest, BasicLockable) {
  Mutex mutex(/* checked = */ true);
  basicLockable(mutex);
}

TEST(LazyMutexTest, BasicLockable) {
  static LazyMutex mutex;
  basicLockable(mutex);
}

TEST(LazyUnsafeMutexTest, BasicLockable) {
  static LazyUnsafeMutex mutex;
  basicLockable(mutex);
}

TEST(SmallMutex, BasicLockable) {
  SmallMutex mutex;
  basicLockable(mutex);
}

TEST(MutexTest, TryLockable) {
  Mutex mutex(/* checked = */ true);
  tryLockable(mutex);
}

TEST(LazyMutexTest, TryLockable) {
  static LazyMutex mutex;
  tryLockable(mutex);
}

TEST(LazyUnsafeMutexTest, TryLockable) {
  static LazyUnsafeMutex mutex;
  tryLockable(mutex);
}

TEST(SmallMutex, TryLockable) {
  SmallMutex mutex;
  tryLockable(mutex);
}

TEST(MutexTest, BasicLockableThreaded) {
  Mutex mutex(/* checked = */ true);
  basicLockableThreaded(mutex);
}

TEST(LazyMutexTest, BasicLockableThreaded) {
  static LazyMutex mutex;
  basicLockableThreaded(mutex);
}

TEST(LazyUnsafeMutexTest, BasicLockableThreaded) {
  static LazyUnsafeMutex mutex;
  basicLockableThreaded(mutex);
}

TEST(SmallMutex, BasicLockableThreaded) {
  SmallMutex mutex;
  basicLockableThreaded(mutex);
}

TEST(MutexTest, LockableThreaded) {
  Mutex mutex(/* checked = */ true);
  lockableThreaded(mutex);
}

TEST(LazyMutexTest, LockableThreaded) {
  static LazyMutex Mutex;
  lockableThreaded(Mutex);
}

TEST(SmallMutexTest, LockableThreaded) {
  SmallMutex Mutex;
  lockableThreaded(Mutex);
}

TEST(MutexTest, ScopedLockThreaded) {
  Mutex mutex(/* checked = */ true);
  scopedLockThreaded<Mutex::ScopedLock>(mutex);
}

TEST(LazyMutexTest, ScopedLockThreaded) {
  static LazyMutex Mutex;
  scopedLockThreaded<LazyMutex::ScopedLock>(Mutex);
}

TEST(SmallMutexTest, ScopedLockThreaded) {
  SmallMutex mutex(/* checked = */ true);
  scopedLockThreaded<ScopedLockT<SmallMutex, false>>(mutex);
}

TEST(MutexTest, ScopedUnlockUnderScopedLockThreaded) {
  Mutex mutex(/* checked = */ true);
  scopedUnlockUnderScopedLockThreaded<Mutex::ScopedLock, Mutex::ScopedUnlock>(
      mutex);
}

TEST(LazyMutexTest, ScopedUnlockUnderScopedLockThreaded) {
  static LazyMutex Mutex;
  scopedUnlockUnderScopedLockThreaded<LazyMutex::ScopedLock,
                                      LazyMutex::ScopedUnlock>(Mutex);
}

TEST(SmallMutexTest, ScopedUnlockUnderScopedLockThreaded) {
  SmallMutex mutex(/* checked = */ true);
  scopedUnlockUnderScopedLockThreaded<SmallMutex::ScopedLock,
                                      SmallMutex::ScopedUnlock>(mutex);
}

TEST(MutexTest, CriticalSectionThreaded) {
  Mutex mutex(/* checked = */ true);
  criticalSectionThreaded(mutex);
}

TEST(LazyMutexTest, CriticalSectionThreaded) {
  static LazyMutex Mutex;
  criticalSectionThreaded(Mutex);
}
