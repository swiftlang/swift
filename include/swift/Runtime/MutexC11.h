//===--- MutexC11.h - Supports Mutex.h using C11 threading ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Mutex, Read/Write lock, and Scoped lock implementations
// using C11 threading primtives.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_MUTEX_C11_H
#define SWIFT_RUNTIME_MUTEX_C11_H

#include <threads.h>

namespace swift {

namespace c11threads {
  class rwlock;

  static void fatalError(int errcode);
  static inline void handleError(int errcode) {
    if (SWIFT_LIKELY(errcode == thrd_success))
      return;

    fatalError(errcode);
  }
}

typedef c11threads::rwlock *ReadWriteLockHandle;
typedef ::mtx_t MutexHandle;

#define SWIFT_MUTEX_SUPPORTS_CONSTEXPR 0
#define SWIFT_READWRITELOCK_SUPPORTS_CONSTEXPR 0

/// C11 low-level implementation that supports Mutex
/// found in Mutex.h
///
/// See Mutex
class MutexPlatformHelper {
public:
  static MutexHandle staticInit() {
    ::mtx_t mutex;
    ::mtx_init(&mutex, ::mtx_plain);
    return mutex;
  }

  static void init(MutexHandle &mutex, bool checked = false) {
    // C11 mutexes can't be checked
    c11threads::handleError(::mtx_init(&mutex, ::mtx_plain));
  }
  static void destroy(MutexHandle &mutex) {
    ::mtx_destroy(&mutex);
  }
  static void lock(MutexHandle &mutex) {
    c11threads::handleError(::mtx_lock(&mutex));
  }
  static void unlock(MutexHandle &mutex) {
    c11threads::handleError(::mtx_unlock(&mutex));
  }
  static bool try_lock(MutexHandle &mutex) {
    int err = ::mtx_trylock(&mutex);
    switch (err) {
    case thrd_success:
      return true;
    case thrd_busy:
      return false;
    default:
      c11threads::handleError(err);
    }
  }

  // Skip error checking for the unsafe versions.
  static void unsafeLock(MutexHandle &mutex) {
    (void)::mtx_lock(&mutex);
  }
  static void unsafeUnlock(MutexHandle &mutex) {
    (void)::mtx_unlock(&mutex);
  }
};

/// C11 low-level implementation that supports ReadWriteLock
/// found in Mutex.h
///
/// See ReadWriteLock
namespace c11threads {

class rwlock {
private:
  unsigned activeReaders_;
  unsigned waitingWriters_;
  bool     writerActive_;
  ::cnd_t  cond_;
  ::mtx_t  mutex_;

public:
  rwlock();
  ~rwlock();

  rwlock(const rwlock &other) = delete;
  rwlock &operator=(const rwlock &other) = delete;

  void readLock();
  bool try_readLock();
  void readUnlock();
  void writeLock();
  bool try_writeLock();
  void writeUnlock();
};

}

class ReadWriteLockPlatformHelper {
public:
  static ReadWriteLockHandle staticInit() {
    return new c11threads::rwlock();
  };

  static void init(ReadWriteLockHandle &rwlock) {
    rwlock = new c11threads::rwlock();
  }

  static void destroy(ReadWriteLockHandle &rwlock) {
    delete rwlock;
  }

  static void readLock(ReadWriteLockHandle &rwlock) {
    rwlock->readLock();
  }

  static bool try_readLock(ReadWriteLockHandle &rwlock) {
    rwlock->try_readLock();
  }

  static void readUnlock(ReadWriteLockHandle &rwlock) {
    rwlock->readUnlock();
  }

  static void writeLock(ReadWriteLockHandle &rwlock) {
    rwlock->writeLock();
  }

  static bool try_writeLock(ReadWriteLockHandle &rwlock) {
    rwlock->try_writeLock();
  }

  static void writeUnlock(ReadWriteLockHandle &rwlock) {
    rwlock->writeUnlock();
  }
};


}

#endif
