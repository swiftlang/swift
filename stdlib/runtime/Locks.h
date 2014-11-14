//===--- Locks.h - A collection of lock utils  ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_RUNTIME_LOCKS_H
#define SWIFT_RUNTIME_LOCKS_H

#include <pthread.h>
#include <strings.h>

// FIXME: Can't use llvm's RWMutex because it isn't a header-only implementation
class RWMutex {
  pthread_rwlock_t rwlock;

public:

  RWMutex() {
#ifdef __APPLE__
    // Workaround a bug/mis-feature in Darwin's pthread_rwlock_init.
    bzero(&rwlock, sizeof(pthread_rwlock_t));
#endif
    int errorcode = pthread_rwlock_init(&rwlock, nullptr);
    (void)errorcode;
    assert(errorcode == 0);
  }

  ~RWMutex() {
    pthread_rwlock_destroy(&rwlock);
  }

  bool reader_acquire() {
    int errorcode = pthread_rwlock_rdlock(&rwlock);
    return errorcode == 0;
  }

  bool reader_release() {
    int errorcode = pthread_rwlock_unlock(&rwlock);
    return errorcode == 0;
  }

  bool writer_acquire() {
    int errorcode = pthread_rwlock_wrlock(&rwlock);
    return errorcode == 0;
  }

  bool writer_release() {
    int errorcode = pthread_rwlock_unlock(&rwlock);
    return errorcode == 0;
  }
};

class ScopedReader {
  RWMutex& mutex;

public:
  explicit ScopedReader(RWMutex& m) : mutex(m) {
    bool ok = mutex.reader_acquire();
    assert(ok);
    (void)ok;
  }

  ~ScopedReader() {
    bool ok = mutex.reader_release();
    assert(ok);
    (void)ok;
  }

  ScopedReader(const ScopedReader& rhs) = delete;
};

class ScopedWriter {
  RWMutex& mutex;

public:

  explicit ScopedWriter(RWMutex& m) : mutex(m) {
    bool ok = mutex.writer_acquire();
    assert(ok);
    (void)ok;
  }

  ~ScopedWriter() {
    bool ok = mutex.writer_release();
    assert(ok);
    (void)ok;
  }

  ScopedWriter(const ScopedWriter& rhs) = delete;
};


#endif // SWIFT_RUNTIME_LOCKS_H
