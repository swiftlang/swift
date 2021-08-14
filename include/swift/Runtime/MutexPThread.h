//===--- MutexPThread.h - Supports Mutex.h using PThreads -------*- C++ -*-===//
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
//
// Mutex, ConditionVariable, Read/Write lock, and Scoped lock implementations
// using PThreads.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_MUTEX_PHTREAD_H
#define SWIFT_RUNTIME_MUTEX_PHTREAD_H

#include <pthread.h>

#if defined(__APPLE__) && defined(__MACH__)
#include <os/lock.h>
#define HAS_OS_UNFAIR_LOCK 1
#endif

namespace swift {

typedef pthread_cond_t ConditionHandle;
typedef pthread_mutex_t ConditionMutexHandle;
typedef pthread_rwlock_t ReadWriteLockHandle;

#if HAS_OS_UNFAIR_LOCK
typedef os_unfair_lock MutexHandle;
#else
typedef pthread_mutex_t MutexHandle;
#endif

#if defined(__CYGWIN__) || defined(__HAIKU__) || defined(__wasi__)
// At the moment CYGWIN pthreads implementation doesn't support the use of
// constexpr for static allocation versions. The way they define things
// results in a reinterpret_cast which violates constexpr.
// WASI currently doesn't support threading/locking at all.
#define SWIFT_CONDITION_SUPPORTS_CONSTEXPR 0
#define SWIFT_MUTEX_SUPPORTS_CONSTEXPR 0
#define SWIFT_READWRITELOCK_SUPPORTS_CONSTEXPR 0
#else
#define SWIFT_CONDITION_SUPPORTS_CONSTEXPR 1
#define SWIFT_MUTEX_SUPPORTS_CONSTEXPR 1
#define SWIFT_READWRITELOCK_SUPPORTS_CONSTEXPR 1
#endif

/// PThread low-level implementation that supports ConditionVariable
/// found in Mutex.h
///
/// See ConditionVariable
struct ConditionPlatformHelper {
#if SWIFT_CONDITION_SUPPORTS_CONSTEXPR
  static constexpr
#else
  static
#endif
      ConditionHandle
      staticInit() {
    return PTHREAD_COND_INITIALIZER;
  };
  static void init(ConditionHandle &condition);
  static void destroy(ConditionHandle &condition);
  static void notifyOne(ConditionHandle &condition);
  static void notifyAll(ConditionHandle &condition);
  static void wait(ConditionHandle &condition, ConditionMutexHandle &mutex);
};

/// PThread low-level implementation that supports Mutex
/// found in Mutex.h
///
/// See Mutex
struct MutexPlatformHelper {
#if SWIFT_MUTEX_SUPPORTS_CONSTEXPR
  static constexpr
#else
  static
#endif
      ConditionMutexHandle
      conditionStaticInit() {
    return PTHREAD_MUTEX_INITIALIZER;
  };
#if SWIFT_MUTEX_SUPPORTS_CONSTEXPR
  static constexpr
#else
  static
#endif
  MutexHandle
  staticInit() {
#if HAS_OS_UNFAIR_LOCK
    return OS_UNFAIR_LOCK_INIT;
#else
    return PTHREAD_MUTEX_INITIALIZER;
#endif  
  }
  static void init(ConditionMutexHandle &mutex, bool checked = false);
  static void destroy(ConditionMutexHandle &mutex);
  static void lock(ConditionMutexHandle &mutex);
  static void unlock(ConditionMutexHandle &mutex);
  static bool try_lock(ConditionMutexHandle &mutex);

  // The ConditionMutexHandle versions handle everything on-Apple platforms.
#if HAS_OS_UNFAIR_LOCK

  static void init(MutexHandle &mutex, bool checked = false);
  static void destroy(MutexHandle &mutex);
  static void lock(MutexHandle &mutex);
  static void unlock(MutexHandle &mutex);
  static bool try_lock(MutexHandle &mutex);

  // os_unfair_lock always checks for errors, so just call through.
  static void unsafeLock(MutexHandle &mutex) { lock(mutex); }
  static void unsafeUnlock(MutexHandle &mutex) { unlock(mutex); }
#endif

  // The unsafe versions don't do error checking.
  static void unsafeLock(ConditionMutexHandle &mutex) {
    (void)pthread_mutex_lock(&mutex);
  }
  static void unsafeUnlock(ConditionMutexHandle &mutex) {
    (void)pthread_mutex_unlock(&mutex);
  }
};

/// PThread low-level implementation that supports ReadWriteLock
/// found in Mutex.h
///
/// See ReadWriteLock
struct ReadWriteLockPlatformHelper {
#if SWIFT_READWRITELOCK_SUPPORTS_CONSTEXPR
  static constexpr
#else
  static
#endif
      ReadWriteLockHandle
      staticInit() {
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
