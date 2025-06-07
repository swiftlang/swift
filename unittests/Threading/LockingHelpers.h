//===--- LockingHelpers.h - Utilities to help test locks ------------------===//
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

#ifndef LOCKING_HELPERS_H
#define LOCKING_HELPERS_H

#include "ThreadingHelpers.h"

// Test that a Mutex object can be locked and unlocked from a single thread
template <typename M>
void basicLockable(M &mutex) {
  // We can lock, unlock, lock and unlock an unlocked lock
  mutex.lock();
  mutex.unlock();
  mutex.lock();
  mutex.unlock();
}

// Test that a Mutex object's try_lock() method works.
template <typename M>
void tryLockable(M &mutex) {
  bool ret;

  // We can lock an unlocked lock
  ret = mutex.try_lock();
  ASSERT_TRUE(ret);

  // We cannot lock a locked lock
  ret = mutex.try_lock();
#if SWIFT_THREADING_NONE
  // Noop since none threading mode always succeeds getting lock
  (void)ret;
#else
  ASSERT_FALSE(ret);
#endif

  mutex.unlock();
}

// Test that a Mutex object can be locked and unlocked
template <typename M>
void basicLockableThreaded(M &mutex) {
  int count1 = 0;
  int count2 = 0;

  threadedExecute(10, [&](int) {
    for (int j = 0; j < 50; ++j) {
      mutex.lock();
      auto count = count2;
      count1++;
      count2 = count + 1;
      mutex.unlock();
    }
  });

  ASSERT_EQ(count1, 500);
  ASSERT_EQ(count2, 500);
}

#if SWIFT_THREADING_NONE
template <typename M>
void lockableThreaded(M &mutex) {
  // Noop since none threading mode always succeeds getting lock
}
#else
// More extensive tests
template <typename M>
void lockableThreaded(M &mutex) {
  mutex.lock();
  threadedExecute(5, [&](int) { ASSERT_FALSE(mutex.try_lock()); });
  mutex.unlock();
  threadedExecute(1, [&](int) {
    ASSERT_TRUE(mutex.try_lock());
    mutex.unlock();
  });

  int count1 = 0;
  int count2 = 0;
  threadedExecute(10, [&](int) {
    for (int j = 0; j < 50; ++j) {
      if (mutex.try_lock()) {
        auto count = count2;
        count1++;
        count2 = count + 1;
        mutex.unlock();
      } else {
        j--;
      }
    }
  });

  ASSERT_EQ(count1, 500);
  ASSERT_EQ(count2, 500);
}
#endif

// Test a scoped lock implementation
template <typename SL, typename M>
void scopedLockThreaded(M &mutex) {
  int count1 = 0;
  int count2 = 0;

  threadedExecute(10, [&](int) {
    for (int j = 0; j < 50; ++j) {
      SL guard(mutex);
      auto count = count2;
      count1++;
      count2 = count + 1;
    }
  });

  ASSERT_EQ(count1, 500);
  ASSERT_EQ(count2, 500);
}

// Test a scoped unlock nested in a scoped lock
template <typename SL, typename SU, typename M>
void scopedUnlockUnderScopedLockThreaded(M &mutex) {
  int count1 = 0;
  int count2 = 0;
  int badCount = 0;

  threadedExecute(10, [&](int) {
    for (int j = 0; j < 50; ++j) {
      SL guard(mutex);
      {
        SU unguard(mutex);
        badCount++;
      }
      auto count = count2;
      count1++;
      count2 = count + 1;
    }
  });

  ASSERT_EQ(count1, 500);
  ASSERT_EQ(count2, 500);
  (void)badCount; // FIXME: Is this value meant to be tested?
}

// Test a critical section
template <typename M>
void criticalSectionThreaded(M &mutex) {
  int count1 = 0;
  int count2 = 0;

  threadedExecute(10, [&](int) {
    for (int j = 0; j < 50; ++j) {
      mutex.withLock([&] {
        auto count = count2;
        count1++;
        count2 = count + 1;
      });
    }
  });

  ASSERT_EQ(count1, 500);
  ASSERT_EQ(count2, 500);
}

#endif
