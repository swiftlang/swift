//===--- MutexPThread.h - Supports Mutex.h using PThreads -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Mutex, ConditionVariable, Read/Write lock, and Scoped lock implementations
// using PThreads.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_MUTEX_PHTREAD_H
#define SWIFT_RUNTIME_MUTEX_PHTREAD_H

#include <pthread.h>

namespace swift {

typedef pthread_cond_t ConditionHandle;
typedef pthread_mutex_t MutexHandle;
typedef pthread_rwlock_t ReadWriteLockHandle;

/// PThread low-level implementation that supports ConditionVariable
/// found in Mutex.h
///
/// See ConditionVariable
struct ConditionPlatformHelper {
  static constexpr ConditionHandle staticInit() {
    return PTHREAD_COND_INITIALIZER;
  };
  static void init(ConditionHandle &condition);
  static void destroy(ConditionHandle &condition);
  static void notifyOne(ConditionHandle &condition);
  static void notifyAll(ConditionHandle &condition);
  static void wait(ConditionHandle &condition, MutexHandle &mutex);
};

/// PThread low-level implementation that supports Mutex
/// found in Mutex.h
///
/// See Mutex
struct MutexPlatformHelper {
  static constexpr MutexHandle staticInit() {
    return PTHREAD_MUTEX_INITIALIZER;
  };
  static void init(MutexHandle &mutex, bool checked = false);
  static void destroy(MutexHandle &mutex);
  static void lock(MutexHandle &mutex);
  static void unlock(MutexHandle &mutex);
  static bool try_lock(MutexHandle &mutex);

  // The unsafe versions don't do error checking.
  static void unsafeLock(MutexHandle &mutex) {
    (void)pthread_mutex_lock(&mutex);
  }
  static void unsafeUnlock(MutexHandle &mutex) {
    (void)pthread_mutex_unlock(&mutex);
  }
};

/// PThread low-level implementation that supports ReadWriteLock
/// found in Mutex.h
///
/// See ReadWriteLock
struct ReadWriteLockPlatformHelper {
  static constexpr ReadWriteLockHandle staticInit() {
    return PTHREAD_RWLOCK_INITIALIZER;
  };
  static void init(ReadWriteLockHandle &rwlock);
  static void destroy(ReadWriteLockHandle &rwlock);
  static void readLock(ReadWriteLockHandle &rwlock);
  static bool try_readLock(ReadWriteLockHandle &rwlock);
  static void readUnlock(ReadWriteLockHandle &rwlock);
  static void writeLock(ReadWriteLockHandle &rwlock);
  static bool try_writeLock(ReadWriteLockHandle &rwlock);
  static void writeUnlock(ReadWriteLockHandle &rwlock);
};
}

#endif
