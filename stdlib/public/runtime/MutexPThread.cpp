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
// Mutex, ConditionVariable, Read/Write lock, and Scoped lock implementations
// using PThreads.
//
//===----------------------------------------------------------------------===//

#if !defined(_WIN32)
#include "swift/Runtime/Mutex.h"

#include "swift/Runtime/Debug.h"
#include <errno.h>
#include <stdlib.h>

using namespace swift;

#define reportError(PThreadFunction)                                           \
  do {                                                                         \
    int errorcode = PThreadFunction;                                           \
    if (errorcode != 0) {                                                      \
      fatalError(/* flags = */ 0, "'%s' failed with error '%s'(%d)\n",         \
                 #PThreadFunction, errorName(errorcode), errorcode);           \
    }                                                                          \
  } while (false)

#define returnTrueOrReportError(PThreadFunction, returnFalseOnEBUSY)           \
  do {                                                                         \
    int errorcode = PThreadFunction;                                           \
    if (errorcode == 0)                                                        \
      return true;                                                             \
    if (returnFalseOnEBUSY && errorcode == EBUSY)                              \
      return false;                                                            \
    fatalError(/* flags = */ 0, "'%s' failed with error '%s'(%d)\n",           \
               #PThreadFunction, errorName(errorcode), errorcode);             \
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

void ConditionPlatformHelper::init(pthread_cond_t &condition) {
  reportError(pthread_cond_init(&condition, nullptr));
}

void ConditionPlatformHelper::destroy(pthread_cond_t &condition) {
  reportError(pthread_cond_destroy(&condition));
}

void ConditionPlatformHelper::notifyOne(pthread_cond_t &condition) {
  reportError(pthread_cond_signal(&condition));
}

void ConditionPlatformHelper::notifyAll(pthread_cond_t &condition) {
  reportError(pthread_cond_broadcast(&condition));
}

void ConditionPlatformHelper::wait(pthread_cond_t &condition,
                                   pthread_mutex_t &mutex) {
  reportError(pthread_cond_wait(&condition, &mutex));
}

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

void ReadWriteLockPlatformHelper::init(pthread_rwlock_t &rwlock) {
  reportError(pthread_rwlock_init(&rwlock, nullptr));
}

void ReadWriteLockPlatformHelper::destroy(pthread_rwlock_t &rwlock) {
  reportError(pthread_rwlock_destroy(&rwlock));
}

void ReadWriteLockPlatformHelper::readLock(pthread_rwlock_t &rwlock) {
  reportError(pthread_rwlock_rdlock(&rwlock));
}

bool ReadWriteLockPlatformHelper::try_readLock(pthread_rwlock_t &rwlock) {
  returnTrueOrReportError(pthread_rwlock_tryrdlock(&rwlock),
                          /* returnFalseOnEBUSY = */ true);
}

void ReadWriteLockPlatformHelper::writeLock(pthread_rwlock_t &rwlock) {
  reportError(pthread_rwlock_wrlock(&rwlock));
}

bool ReadWriteLockPlatformHelper::try_writeLock(pthread_rwlock_t &rwlock) {
  returnTrueOrReportError(pthread_rwlock_trywrlock(&rwlock),
                          /* returnFalseOnEBUSY = */ true);
}

void ReadWriteLockPlatformHelper::readUnlock(pthread_rwlock_t &rwlock) {
  reportError(pthread_rwlock_unlock(&rwlock));
}

void ReadWriteLockPlatformHelper::writeUnlock(pthread_rwlock_t &rwlock) {
  reportError(pthread_rwlock_unlock(&rwlock));
}
#endif
