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

#include <pthread.h>
#include <stdlib.h>
#include <errno.h>
#include "swift/Runtime/Debug.h"

using namespace swift;

#define reportError(PThreadFunction) \
{ \
  int errorcode = PThreadFunction; \
  if (errorcode != 0) { \
    fatalError(/* flags = */ 0, \
               "[%p] '%s' failed with error '%s'(%d)\n", \
               this, #PThreadFunction, errorName(errorcode), errorcode); \
  } \
}

#define returnOrReportError(PThreadFunction, allowEBUSY) \
{ \
  int errorcode = PThreadFunction; \
  if (errorcode == 0 || (allowEBUSY && errorcode == EBUSY)) { \
    return errorcode != EBUSY; \
  } \
  fatalError(/* flags = */ 0, \
    "[%p] '%s' failed with error '%s'(%d)\n", \
    this, #PThreadFunction, errorName(errorcode), errorcode); \
}

static const char* errorName(int errorcode) {
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

Condition::Condition() : Data(nullptr) {
  pthread_cond_t* cond =
    static_cast<pthread_cond_t*>(malloc(sizeof(pthread_cond_t)));

  if (cond == nullptr) {
    fatalError(/* flags = */ 0, "pthread condition could not be allocated\n");
  }

  reportError( pthread_cond_init(cond, nullptr) );
  Data = cond;
}

Condition::~Condition() {
  pthread_cond_t* cond = static_cast<pthread_cond_t*>(Data);
  reportError( pthread_cond_destroy(cond) );
  free(cond);
}

void Condition::notifyOne() const {
  pthread_cond_t* cond = static_cast<pthread_cond_t*>(Data);
  reportError( pthread_cond_signal(cond) );
}

void Condition::notifyAll() const {
  pthread_cond_t* cond = static_cast<pthread_cond_t*>(Data);
  reportError( pthread_cond_broadcast(cond) );
}

MutexImplementation::MutexImplementation( bool checked ) : Data(nullptr) {

  int kind =
    ( checked ? PTHREAD_MUTEX_ERRORCHECK : PTHREAD_MUTEX_NORMAL );

  pthread_mutexattr_t attr;
  pthread_mutex_t* mutex =
    static_cast<pthread_mutex_t*>(malloc(sizeof(pthread_mutex_t)));

  if (mutex == nullptr) {
    fatalError(/* flags = */ 0, "pthread mutex could not be allocated\n");
  }

  reportError( pthread_mutexattr_init(&attr) );
  reportError( pthread_mutexattr_settype(&attr, kind) );
  reportError( pthread_mutex_init(mutex, &attr) );
  reportError( pthread_mutexattr_destroy(&attr) );
  Data = mutex;
}

MutexImplementation::~MutexImplementation() {
  pthread_mutex_t* mutex = static_cast<pthread_mutex_t*>(Data);
  reportError( pthread_mutex_destroy(mutex) );
  free(mutex);
}

void MutexImplementation::lock() const {
  pthread_mutex_t* mutex = static_cast<pthread_mutex_t*>(Data);
  reportError( pthread_mutex_lock(mutex) );
}

void MutexImplementation::unlock() const {
  pthread_mutex_t* mutex = static_cast<pthread_mutex_t*>(Data);
  reportError( pthread_mutex_unlock(mutex) );
}

bool MutexImplementation::try_lock() const {
  pthread_mutex_t* mutex = static_cast<pthread_mutex_t*>(Data);
  returnOrReportError( pthread_mutex_trylock(mutex), /* allowEBUSY = */ true );
}

void MutexImplementation::wait(const Condition& condition) const {
  pthread_mutex_t* mutex = static_cast<pthread_mutex_t*>(Data);
  pthread_cond_t* cond = static_cast<pthread_cond_t*>(condition.Data);
  reportError( pthread_cond_wait(cond, mutex) );
}
