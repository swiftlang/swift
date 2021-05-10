//===--- ThreadLocal.h - Thread-local storage -------------------*- C++ -*-===//
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
// Declarations and macros for working with thread-local storage in the
// Swift runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_THREADLOCAL_H
#define SWIFT_RUNTIME_THREADLOCAL_H

/// SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL - Does the current configuration
/// allow the use of SWIFT_RUNTIME_ATTRIBUTE_THREAD_LOCAL?
#if defined(SWIFT_STDLIB_SINGLE_THREADED_RUNTIME)
// We define SWIFT_RUNTIME_ATTRIBUTE_THREAD_LOCAL to nothing in this
// configuration and just use a global variable, so this is okay.
#define SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL 1
#elif __has_feature(tls)
// If __has_feature reports that TLS is available, use it.
#define SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL 1
#elif !defined(__clang__)
// If we're not using Clang, assume that __has_feature is unreliable
// and that we can safely use TLS.
#else
// Otherwise we can't use TLS and have to fall back on something else.
#define SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL 0
#endif

/// SWIFT_RUNTIME_THREAD_LOCAL - Declare that something is a
/// thread-local variable in the runtime.
#if defined(SWIFT_STDLIB_SINGLE_THREADED_RUNTIME)
// In a single-threaded runtime, thread-locals are global.
#define SWIFT_RUNTIME_ATTRIBUTE_THREAD_LOCAL
#elif defined(__GNUC__)
// In GCC-compatible compilers, we prefer __thread because it's understood
// to guarantee a constant initializer, which permits more efficient access
// patterns.
#define SWIFT_RUNTIME_ATTRIBUTE_THREAD_LOCAL __thread
#else
// Otherwise, just fall back on the standard C++ feature.
#define SWIFT_RUNTIME_ATTRIBUTE_THREAD_LOCAL thread_local
#endif

// Implementation of SWIFT_RUNTIME_DECLARE_THREAD_LOCAL
#if !SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL
#include <pthread.h>
#include <dispatch/dispatch.h>
#endif

namespace swift {
// A wrapper class for thread-local storage.
//
// - On platforms that report SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL
//   above, an object of this type is declared with
//   SWIFT_RUNTIME_ATTRIBUTE_THREAD_LOCAL.  This makes the object
//   itself thread-local, and no internal support is required.
//
//   Note that this includes platforms that set
//   SWIFT_STDLIB_SINGLE_THREADED_RUNTIME, for which
//   SWIFT_RUNTIME_ATTRIBUTE_THREAD_LOCAL is empty;
//   thread-local declarations then create an ordinary global.
//
// - On platforms that don't report SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL,
//   we have to simulate thread-local storage.  Fortunately, all of
//   these platforms (at least for now) support pthread_getspecific.
template <class T>
class ThreadLocal {
  static_assert(sizeof(T) <= sizeof(void*), "cannot store more than a pointer");

#if SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL
  T value;
#else
  // We rely on the zero-initialization of objects with static storage
  // duration.
  dispatch_once_t once;
  pthread_key_t key;

  pthread_key_t getKey() {
    dispatch_once_f(&once, this, [](void *ctx) {
      pthread_key_create(&reinterpret_cast<ThreadLocal*>(ctx)->key, nullptr);
    });
    return key;
  }
#endif

public:
  constexpr ThreadLocal() {}

  T get() {
#if SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL
    return value;
#else
    void *storedValue = pthread_getspecific(getKey());
    T value;
    memcpy(&value, &storedValue, sizeof(T));
    return value;
#endif
  }

  void set(T newValue) {
#if SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL
    value = newValue;
#else
    void *storedValue;
    memcpy(&storedValue, &newValue, sizeof(T));
    pthread_setspecific(getKey(), storedValue);
#endif
  }
};
} // end namespace swift

/// SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(TYPE, NAME) - Declare a variable
/// to be a thread-local variable.  The declaration must have static
/// storage duration; it may be prefixed with "static".
///
/// Because of the fallback path, the default-initialization of the
/// type must be equivalent to a bitwise zero-initialization, and the
/// type must be small and trivially copyable.
#if SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL
#define SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(TYPE, NAME) \
  SWIFT_RUNTIME_ATTRIBUTE_THREAD_LOCAL swift::ThreadLocal<TYPE> NAME
#else
#define SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(TYPE, NAME) \
  swift::ThreadLocal<TYPE> NAME
#endif

#endif
