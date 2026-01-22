//===--- ThreadLocalStorage.h - Wrapper for thread-local storage. --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_THREADLOCALSTORAGE_H
#define SWIFT_STDLIB_SHIMS_THREADLOCALSTORAGE_H

#include "Visibility.h"

SWIFT_RUNTIME_STDLIB_INTERNAL
void * _Nonnull _swift_stdlib_threadLocalStorageGet(void);

#if !defined(__swift_embedded__)
SWIFT_RUNTIME_STDLIB_INTERNAL
void * _Nullable _swift_getExclusivityTLSImpl(void);

SWIFT_RUNTIME_STDLIB_INTERNAL
void _swift_setExclusivityTLSImpl(void * _Nullable newValue);
#endif

#if defined(__swift_embedded__)
SWIFT_RUNTIME_STDLIB_INTERNAL
void * _Nullable _swift_getExclusivityTLS(void);

SWIFT_RUNTIME_STDLIB_INTERNAL
void _swift_setExclusivityTLS(void * _Nullable newValue);

#elif defined(__APPLE__) && __arm64__

// Use a fast path on Apple ARM64, where we have a dedicated TLS key and fast
// access to read/write it.

#ifndef __PTK_FRAMEWORK_SWIFT_KEY7
# define __PTK_FRAMEWORK_SWIFT_KEY7 107
#endif

#define SWIFT_RUNTIME_EXCLUSIVITY_KEY __PTK_FRAMEWORK_SWIFT_KEY7

static inline void * _Nullable * _Nonnull _swift_getExclusivityTLSPointer(void) {
  unsigned long tsd;
  __asm__ ("mrs %0, TPIDRRO_EL0" : "=r" (tsd));
  void **base = (void **)tsd;
  return &base[SWIFT_RUNTIME_EXCLUSIVITY_KEY];
}

static inline void * _Nullable _swift_getExclusivityTLS(void) {
  return *_swift_getExclusivityTLSPointer();
}

static inline void _swift_setExclusivityTLS(void * _Nullable newValue) {
  *_swift_getExclusivityTLSPointer() = newValue;
}

#else

static inline void * _Nullable _swift_getExclusivityTLS(void) {
  return _swift_getExclusivityTLSImpl();
}

static inline void _swift_setExclusivityTLS(void * _Nullable newValue) {
  _swift_setExclusivityTLSImpl(newValue);
}

#endif

#endif // SWIFT_STDLIB_SHIMS_THREADLOCALSTORAGE_H
