//==--- Pthreads.h - Threading abstraction implementation ------ -*-C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implements threading support for Linux
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_IMPL_LINUX_H
#define SWIFT_THREADING_IMPL_LINUX_H

#include <errno.h>
#include <pthread.h>

#include <atomic>
#include <optional>

#include "llvm/ADT/Optional.h"

#include "swift/Threading/Errors.h"

#include "Linux/ulock.h"

namespace swift {
namespace threading_impl {

#define SWIFT_LINUXTHREADS_CHECK(expr)                                         \
  do {                                                                         \
    int res_ = (expr);                                                         \
    if (res_ != 0)                                                             \
      swift::threading::fatal(#expr " failed with error %d\n", res_);          \
  } while (0)

#define SWIFT_LINUXTHREADS_RETURN_TRUE_OR_FALSE(expr)                          \
  do {                                                                         \
    int res_ = (expr);                                                         \
    switch (res_) {                                                            \
    case 0:                                                                    \
      return true;                                                             \
    case EBUSY:                                                                \
      return false;                                                            \
    default:                                                                   \
      swift::threading::fatal(#expr " failed with error (%d)\n", res_);        \
    }                                                                          \
  } while (0)

// .. Thread related things ..................................................

using thread_id = ::pthread_t;

inline thread_id thread_get_current() { return ::pthread_self(); }

bool thread_is_main();

inline bool threads_same(thread_id a, thread_id b) {
  return ::pthread_equal(a, b);
}

llvm::Optional<stack_bounds> thread_get_current_stack_bounds();

// .. Mutex support ..........................................................

using mutex_handle = ::pthread_mutex_t;

inline void mutex_init(mutex_handle &handle, bool checked = false) {
  if (!checked) {
    handle = PTHREAD_MUTEX_INITIALIZER;
  } else {
    ::pthread_mutexattr_t attr;
    SWIFT_LINUXTHREADS_CHECK(::pthread_mutexattr_init(&attr));
    SWIFT_LINUXTHREADS_CHECK(
        ::pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK));
    SWIFT_LINUXTHREADS_CHECK(::pthread_mutex_init(&handle, &attr));
    SWIFT_LINUXTHREADS_CHECK(::pthread_mutexattr_destroy(&attr));
  }
}
inline void mutex_destroy(mutex_handle &handle) {
  SWIFT_LINUXTHREADS_CHECK(::pthread_mutex_destroy(&handle));
}

inline void mutex_lock(mutex_handle &handle) {
  SWIFT_LINUXTHREADS_CHECK(::pthread_mutex_lock(&handle));
}
inline void mutex_unlock(mutex_handle &handle) {
  SWIFT_LINUXTHREADS_CHECK(::pthread_mutex_unlock(&handle));
}
inline bool mutex_try_lock(mutex_handle &handle) {
  SWIFT_LINUXTHREADS_RETURN_TRUE_OR_FALSE(::pthread_mutex_trylock(&handle));
}

inline void mutex_unsafe_lock(mutex_handle &handle) {
  (void)::pthread_mutex_lock(&handle);
}
inline void mutex_unsafe_unlock(mutex_handle &handle) {
  (void)::pthread_mutex_unlock(&handle);
}

using lazy_mutex_handle = ::pthread_mutex_t;

// We don't actually need to be lazy here because pthreads has
// PTHREAD_MUTEX_INITIALIZER.
inline constexpr lazy_mutex_handle lazy_mutex_initializer() {
  return PTHREAD_MUTEX_INITIALIZER;
}
inline void lazy_mutex_destroy(lazy_mutex_handle &handle) {
  SWIFT_LINUXTHREADS_CHECK(::pthread_mutex_destroy(&handle));
}

inline void lazy_mutex_lock(lazy_mutex_handle &handle) {
  SWIFT_LINUXTHREADS_CHECK(::pthread_mutex_lock(&handle));
}
inline void lazy_mutex_unlock(lazy_mutex_handle &handle) {
  SWIFT_LINUXTHREADS_CHECK(::pthread_mutex_unlock(&handle));
}
inline bool lazy_mutex_try_lock(lazy_mutex_handle &handle) {
  SWIFT_LINUXTHREADS_RETURN_TRUE_OR_FALSE(::pthread_mutex_trylock(&handle));
}

inline void lazy_mutex_unsafe_lock(lazy_mutex_handle &handle) {
  (void)::pthread_mutex_lock(&handle);
}
inline void lazy_mutex_unsafe_unlock(lazy_mutex_handle &handle) {
  (void)::pthread_mutex_unlock(&handle);
}

// .. Once ...................................................................

struct once_t {
  std::atomic<std::int32_t> flag;
#if defined(__LP64__) || defined(_LP64)
  // On 32-bit Linux we can't have the lock, so we'll be less efficient
  linux::ulock_t lock;
#endif
};

void once_slow(once_t &predicate, void (*fn)(void *), void *context);

inline void once_impl(once_t &predicate, void (*fn)(void *), void *context) {
  // Sadly we can't use ::pthread_once() for this (no context)
  if (predicate.flag.load(std::memory_order_acquire) < 0)
    return;

  once_slow(predicate, fn, context);
}

// .. Thread local storage ...................................................

#if __cplusplus >= 201103L || __has_feature(cxx_thread_local)
#define SWIFT_THREAD_LOCAL thread_local
#endif

using tls_key_t = pthread_key_t;
using tls_dtor_t = void (*)(void *);

inline bool tls_alloc(tls_key_t &key, tls_dtor_t dtor) {
  return pthread_key_create(&key, dtor) == 0;
}

inline void *tls_get(tls_key_t key) { return pthread_getspecific(key); }

inline void tls_set(tls_key_t key, void *value) {
  pthread_setspecific(key, value);
}

} // namespace threading_impl

} // namespace swift

#endif // SWIFT_THREADING_IMPL_PTHREADS_H
