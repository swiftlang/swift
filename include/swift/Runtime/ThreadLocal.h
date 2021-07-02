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

#include <type_traits>
#include "ThreadLocalStorage.h"

/// SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL - Does the current configuration
/// allow the use of SWIFT_RUNTIME_ATTRIBUTE_THREAD_LOCAL?
#if defined(__APPLE__)
// The pthread TLS APIs work better than C++ TLS on Apple platforms.
#define SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL 0
#elif defined(SWIFT_STDLIB_SINGLE_THREADED_RUNTIME)
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
// Validate a type stored in thread-local storage, using static asserts. Such
// types must fit in a pointer and be trivially copyable/destructible.
#define VALIDATE_THREAD_LOCAL_TYPE(T)                                          \
  static_assert(sizeof(T) <= sizeof(void *),                                   \
                "cannot store more than a pointer");                           \
  static_assert(std::is_trivially_copyable<T>::value,                          \
                "ThreadLocal values must be trivially copyable");              \
  static_assert(std::is_trivially_destructible<T>::value,                      \
                "ThreadLocal cleanup is not supported, stored types must be "  \
                "trivially destructible");

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
#if SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL
template <class T>
class ThreadLocal {
  VALIDATE_THREAD_LOCAL_TYPE(T)

  T value;

public:
  constexpr ThreadLocal() {}

  T get() { return value; }

  void set(T newValue) { value = newValue; }
};
#else
// A wrapper around a pthread_key_t that is lazily initialized using
// dispatch_once.
class ThreadLocalKey {
  // We rely on the zero-initialization of objects with static storage
  // duration.
  dispatch_once_t once;
  pthread_key_t key;

public:
  pthread_key_t getKey() {
    dispatch_once_f(&once, &key, [](void *ctx) {
      pthread_key_create(reinterpret_cast<pthread_key_t *>(ctx), nullptr);
    });
    return key;
  }
};

// A type representing a constant pthread_key_t, for use on platforms that
// provide reserved keys.
template <pthread_key_t constantKey>
class ConstantThreadLocalKey {
public:
  pthread_key_t getKey() { return constantKey; }
};

template <class T, class Key>
class ThreadLocal {
  VALIDATE_THREAD_LOCAL_TYPE(T)

  Key key;

public:
  constexpr ThreadLocal() {}

  T get() {
    void *storedValue = SWIFT_THREAD_GETSPECIFIC(key.getKey());
    T value;
    memcpy(&value, &storedValue, sizeof(T));
    return value;
  }

  void set(T newValue) {
    void *storedValue;
    memcpy(&storedValue, &newValue, sizeof(T));
    SWIFT_THREAD_SETSPECIFIC(key.getKey(), storedValue);
  }
};
#endif

} // end namespace swift

/// SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(TYPE, NAME) - Declare a variable
/// to be a thread-local variable.  The declaration must have static
/// storage duration; it may be prefixed with "static".
///
/// Because of the fallback path, the default-initialization of the
/// type must be equivalent to a bitwise zero-initialization, and the
/// type must be small and trivially copyable and destructible.
#if SWIFT_RUNTIME_SUPPORTS_THREAD_LOCAL
#define SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(TYPE, NAME, KEY) \
  SWIFT_RUNTIME_ATTRIBUTE_THREAD_LOCAL swift::ThreadLocal<TYPE> NAME
#elif SWIFT_TLS_HAS_RESERVED_PTHREAD_SPECIFIC
#define SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(TYPE, NAME, KEY) \
  swift::ThreadLocal<TYPE, ConstantThreadLocalKey<KEY>> NAME
#else
#define SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(TYPE, NAME, KEY) \
  swift::ThreadLocal<TYPE, ThreadLocalKey> NAME
#endif

#endif
