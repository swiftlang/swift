//===--- MutexPThread.cpp - Supports Mutex.h using PThreads ---------------===//
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
// Mutex and Read/Write lock implementations using pthreads.
//
// Darwin shares the pthreads implementation for read/write locks, but
// uses inline implementations for os_unfair_lock.
//
//===----------------------------------------------------------------------===//

#if SWIFT_STDLIB_THREADING_PTHREADS || SWIFT_STDLIB_THREADING_DARWIN

#if __has_include(<unistd.h>)
#include <unistd.h>
#endif

// Notes: swift::fatalError is not shared between libswiftCore and libswift_Concurrency
// and libswift_Concurrency uses swift_Concurrency_fatalError instead.
#ifndef SWIFT_FATAL_ERROR
#define SWIFT_FATAL_ERROR swift::fatalError
#endif

#include "swift/Runtime/Mutex.h"

#include "swift/Runtime/Debug.h"
#include <errno.h>
#include <stdlib.h>

using namespace swift;

#define reportError(PThreadFunction)                                           \
  do {                                                                         \
    int errorcode = PThreadFunction;                                           \
    if (errorcode != 0) {                                                      \
      SWIFT_FATAL_ERROR(/* flags = */ 0, "'%s' failed with error '%s'(%d)\n",  \
                        #PThreadFunction, errorName(errorcode), errorcode);    \
    }                                                                          \
  } while (false)

#define returnTrueOrReportError(PThreadFunction, returnFalseOnEBUSY)           \
  do {                                                                         \
    int errorcode = PThreadFunction;                                           \
    if (errorcode == 0)                                                        \
      return true;                                                             \
    if (returnFalseOnEBUSY && errorcode == EBUSY)                              \
      return false;                                                            \
    SWIFT_FATAL_ERROR(/* flags = */ 0, "'%s' failed with error '%s'(%d)\n",    \
                      #PThreadFunction, errorName(errorcode), errorcode);      \
  } while (false)

static const char *errorName(int errorcode) {
  switch (errorcode) {
  case EINVAL:
    return "EINVAL";
  case EPERM:
    return "EPERM";
  case EDEADLK:
    return "EDEADLK";
  case ENOMEM:
    return "ENOMEM";
  case EAGAIN:
    return "EAGAIN";
  case EBUSY:
    return "EBUSY";
  default:
    return "<unknown>";
  }
}

#if !HAS_OS_UNFAIR_LOCK

void MutexPlatformHelper::init(pthread_mutex_t &mutex, bool checked) {
  pthread_mutexattr_t attr;
  int kind = (checked ? PTHREAD_MUTEX_ERRORCHECK : PTHREAD_MUTEX_NORMAL);
  reportError(pthread_mutexattr_init(&attr));
  reportError(pthread_mutexattr_settype(&attr, kind));
  reportError(pthread_mutex_init(&mutex, &attr));
  reportError(pthread_mutexattr_destroy(&attr));
}

void MutexPlatformHelper::destroy(pthread_mutex_t &mutex) {
  reportError(pthread_mutex_destroy(&mutex));
}

void MutexPlatformHelper::lock(pthread_mutex_t &mutex) {
  reportError(pthread_mutex_lock(&mutex));
}

void MutexPlatformHelper::unlock(pthread_mutex_t &mutex) {
  reportError(pthread_mutex_unlock(&mutex));
}

bool MutexPlatformHelper::try_lock(pthread_mutex_t &mutex) {
  returnTrueOrReportError(pthread_mutex_trylock(&mutex),
                          /* returnFalseOnEBUSY = */ true);
}

#endif

#endif // SWIFT_STDLIB_THREADING_PTHREADS
