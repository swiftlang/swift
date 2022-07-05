//===--- ConditionVariable.cpp - A condition variable ---------------------===//
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

#include "Error.h"
#include "ConditionVariable.h"

using namespace swift;

#define fatalError swift_Concurrency_fatalError

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

ConditionVariable::Mutex::Mutex(bool checked) {
  pthread_mutexattr_t attr;
  int kind = (checked ? PTHREAD_MUTEX_ERRORCHECK : PTHREAD_MUTEX_NORMAL);
  reportError(pthread_mutexattr_init(&attr));
  reportError(pthread_mutexattr_settype(&attr, kind));
  reportError(pthread_mutex_init(&Handle, &attr));
  reportError(pthread_mutexattr_destroy(&attr));
}

ConditionVariable::Mutex::~Mutex() {
  reportError(pthread_mutex_destroy(&Handle));
}

void ConditionVariable::Mutex::lock() {
  reportError(pthread_mutex_lock(&Handle));
}

void ConditionVariable::Mutex::unlock() {
  reportError(pthread_mutex_unlock(&Handle));
}

bool ConditionVariable::Mutex::try_lock() {
  returnTrueOrReportError(pthread_mutex_trylock(&Handle),
                          /* returnFalseOnEBUSY = */ true);
}

void ConditionVariable::Mutex::wait(ConditionVariable &condition) {
  reportError(pthread_cond_wait(&condition.Handle, &Handle));
}
