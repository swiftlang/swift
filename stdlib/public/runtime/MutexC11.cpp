//===--- MutexC11.cpp - Supports Mutex.h using C11 threads ----------------===//
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
//
// Mutex and Read/Write lock implementations using C11 threads.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/Debug.h"

#if SWIFT_STDLIB_THREADING_C11

using namespace swift;

namespace {

// A simple scoped locker class for a C11 mutex
class locker {
private:
  mtx_t &mutex_;

public:
  explicit locker(mtx_t &mutex) : mutex_(mutex) {
    c11threads::handleError(mtx_lock(&mutex_));
  }
  ~locker() {
    c11threads::handleError(mtx_unlock(&mutex_));
  }
};

}

namespace c11threads {

#ifndef SWIFT_FATAL_ERROR
#define SWIFT_FATAL_ERROR swift::fatalError
#endif

// Triggered if a C11 threads call fails
void fatalError(int errcode) {
  SWIFT_FATAL_ERROR(0, "C11 threads call failed with %d\n", errcode);
}

// A simple reader/writer lock implementation, with writer priority
rwlock::rwlock() : activeReaders_(0), waitingWriters_(0), writerActive_(false) {
  handleError(::cnd_init(&cond_));
  handleError(::mtx_init(&mutex_, ::mtx_plain));
}

rwlock::~rwlock() {
  ::cnd_destroy(&cond_);
  ::mtx_destroy(&mutex_);
}

void rwlock::readLock() {
  locker l(mutex_);
  while (waitingWriters_ || writerActive_)
    handleError(::cnd_wait(&cond_, &mutex_));
  ++activeReaders_;
}

bool rwlock::try_readLock() {
  locker l(mutex_);
  if (waitingWriters_ || writerActive_)
    return false;
  ++activeReaders_;
  return true;
}

void rwlock::readUnlock() {
  locker l(mutex_);
  if (!--activeReaders_)
    handleError(::cnd_broadcast(&cond_));
}

void rwlock::writeLock() {
  locker l(mutex_);
  ++waitingWriters_;
  while (activeReaders_ || writerActive_)
    handleError(::cnd_wait(&cond_, mutex_));
  --waitingWriters_;
  writerActive_ = true;
}

bool rwlock::try_writeLock() {
  locker l(mutex_);
  if (activeReaders_ || writerActive_)
    return false;
  writerActive_ = true;
  return true;
}

void rwlock::writeUnlock() {
  locker l(mutex_);
  writerActive_ = false;
  handleError(::cnd_broadcast(&cond_));
}

}

#endif // SWIFT_STDLIB_THREADING_C11
