//===--- Mutex.cpp - Mutex support code -----------------------------------===//
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

#define SWIFT_FATAL_ERROR swift_Concurrency_fatalError

// Include the runtime's mutex support code.
// FIXME: figure out some reasonable way to share this stuff

#include "ConditionVariable.h"
#include "../runtime/MutexPThread.cpp"
#include "../runtime/MutexWin32.cpp"
#ifdef SWIFT_STDLIB_SINGLE_THREADED_RUNTIME
  #include "swift/Runtime/MutexSingleThreaded.h"
#endif

using namespace swift;

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
