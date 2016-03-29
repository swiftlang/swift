//===--- Mutex.cpp - Lockables --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Mutex, Condition, and Scoped lock abstactions for use in Swift runtime.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Mutex.h"

#include "swift/Runtime/Debug.h"
#include <errno.h>
#include <stdlib.h>

using namespace swift;

#define reportError(PThreadFunction)                                           \
  {                                                                            \
    int errorcode = PThreadFunction;                                           \
    if (errorcode != 0) {                                                      \
      fatalError(/* flags = */ 0, "[%p] '%s' failed with error '%s'(%d)\n",    \
                 this, #PThreadFunction, errorName(errorcode), errorcode);     \
    }                                                                          \
  }

#define returnTrueOrReportError(PThreadFunction, returnFalseOnEBUSY)           \
  {                                                                            \
    int errorcode = PThreadFunction;                                           \
    if (errorcode == 0)                                                        \
      return true;                                                             \
    if (returnFalseOnEBUSY && errorcode == EBUSY)                              \
      return false;                                                            \
    fatalError(/* flags = */ 0, "[%p] '%s' failed with error '%s'(%d)\n",      \
               this, #PThreadFunction, errorName(errorcode), errorcode);       \
  }

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

Condition::Condition() {
  reportError(pthread_cond_init(&PThreadCond, nullptr));
}

Condition::~Condition() { reportError(pthread_cond_destroy(&PThreadCond)); }

void Condition::notifyOne() { reportError(pthread_cond_signal(&PThreadCond)); }

void Condition::notifyAll() {
  reportError(pthread_cond_broadcast(&PThreadCond));
}

MutexImpl::MutexImpl(bool checked) {
  pthread_mutexattr_t attr;
  int kind = (checked ? PTHREAD_MUTEX_ERRORCHECK : PTHREAD_MUTEX_NORMAL);
  reportError(pthread_mutexattr_init(&attr));
  reportError(pthread_mutexattr_settype(&attr, kind));
  reportError(pthread_mutex_init(&PThreadMutex, &attr));
  reportError(pthread_mutexattr_destroy(&attr));
}

MutexImpl::~MutexImpl() { reportError(pthread_mutex_destroy(&PThreadMutex)); }

void MutexImpl::lock() { reportError(pthread_mutex_lock(&PThreadMutex)); }

void MutexImpl::unlock() { reportError(pthread_mutex_unlock(&PThreadMutex)); }

bool MutexImpl::try_lock() {
  returnTrueOrReportError(pthread_mutex_trylock(&PThreadMutex),
                          /* returnFalseOnEBUSY = */ true);
}

void MutexImpl::wait(Condition &condition) {
  reportError(pthread_cond_wait(&condition.PThreadCond, &PThreadMutex));
}
