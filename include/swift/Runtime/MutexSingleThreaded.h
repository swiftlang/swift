//===--- MutexSingleThreaded.h - --------------------------------*- C++ -*-===//
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
//
// No-op implementation of locks for single-threaded environments.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_MUTEX_SINGLE_THREADED_H
#define SWIFT_RUNTIME_MUTEX_SINGLE_THREADED_H

#include "swift/Runtime/Debug.h"

namespace swift {

typedef void* ConditionHandle;
typedef void* MutexHandle;
typedef void* ReadWriteLockHandle;

#define SWIFT_CONDITION_SUPPORTS_CONSTEXPR 1
#define SWIFT_MUTEX_SUPPORTS_CONSTEXPR 1
#define SWIFT_READWRITELOCK_SUPPORTS_CONSTEXPR 1

struct ConditionPlatformHelper {
  static constexpr ConditionHandle staticInit() {
    return nullptr;
  };
  static void init(ConditionHandle &condition) {}
  static void destroy(ConditionHandle &condition) {}
  static void notifyOne(ConditionHandle &condition) {}
  static void notifyAll(ConditionHandle &condition) {}
  static void wait(ConditionHandle &condition, MutexHandle &mutex) {
    fatalError(0, "single-threaded runtime cannot wait for condition");
  }
};

struct MutexPlatformHelper {
  static constexpr MutexHandle staticInit() { return nullptr; }
  static void init(MutexHandle &mutex, bool checked = false) {}
  static void destroy(MutexHandle &mutex) {}
  static void lock(MutexHandle &mutex) {}
  static void unlock(MutexHandle &mutex) {}
  static bool try_lock(MutexHandle &mutex) { return true; }
  static void unsafeLock(MutexHandle &mutex) {}
  static void unsafeUnlock(MutexHandle &mutex) {}
};

struct ReadWriteLockPlatformHelper {
  static constexpr ReadWriteLockHandle staticInit() { return nullptr; }
  static void init(ReadWriteLockHandle &rwlock) {}
  static void destroy(ReadWriteLockHandle &rwlock) {}
  static void readLock(ReadWriteLockHandle &rwlock) {}
  static bool try_readLock(ReadWriteLockHandle &rwlock) { return true; }
  static void readUnlock(ReadWriteLockHandle &rwlock) {}
  static void writeLock(ReadWriteLockHandle &rwlock) {}
  static bool try_writeLock(ReadWriteLockHandle &rwlock) { return true; }
  static void writeUnlock(ReadWriteLockHandle &rwlock) {}
};
}

#endif
