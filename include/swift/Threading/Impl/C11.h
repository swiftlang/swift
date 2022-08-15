//==--- C11.h - Threading abstraction implementation ----------- -*-C++ -*-===//
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
// Implements threading support for C11 threads
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_IMPL_C11_H
#define SWIFT_THREADING_IMPL_C11_H

#include <atomic>
#include <cstdint>

#include <threads.h>

#include "llvm/ADT/Optional.h"

#include "swift/Threading/Errors.h"

namespace swift {
namespace threading_impl {

#define SWIFT_C11THREADS_CHECK(expr)                                           \
  do {                                                                         \
    int res_ = (expr);                                                         \
    if (res_ != thrd_success)                                                  \
      swift::threading::fatal(#expr " failed with error %d\n", res_);          \
  } while (0)

#define SWIFT_C11THREADS_RETURN_TRUE_OR_FALSE(expr)                            \
  do {                                                                         \
    int res_ = (expr);                                                         \
    switch (res_) {                                                            \
    case thrd_success:                                                         \
      return true;                                                             \
    case thrd_busy:                                                            \
      return false;                                                            \
    default:                                                                   \
      swift::threading::fatal(#expr " failed with error (%d)\n", res_);        \
    }                                                                          \
  } while (0)

// .. Thread related things ..................................................

using thread_id = ::thrd_t;

inline thread_id thread_get_current() { return ::thrd_current(); }
bool thread_is_main();
inline bool threads_same(thread_id a, thread_id b) {
  return ::thrd_equal(a, b);
}
inline llvm::Optional<stack_bounds> thread_get_current_stack_bounds() {
  return {};
}

// .. Mutex support ..........................................................

using mutex_handle = ::mtx_t;

inline void mutex_init(mutex_handle &handle, bool checked = false) {
  SWIFT_C11THREADS_CHECK(::mtx_init(&handle, ::mtx_plain));
}
inline void mutex_destroy(mutex_handle &handle) { ::mtx_destroy(&handle); }

inline void mutex_lock(mutex_handle &handle) {
  SWIFT_C11THREADS_CHECK(::mtx_lock(&handle));
}
inline void mutex_unlock(mutex_handle &handle) {
  SWIFT_C11THREADS_CHECK(::mtx_unlock(&handle));
}
inline bool mutex_try_lock(mutex_handle &handle) {
  SWIFT_C11THREADS_RETURN_TRUE_OR_FALSE(::mtx_trylock(&handle));
}

inline void mutex_unsafe_lock(mutex_handle &handle) {
  (void)::mtx_lock(&handle);
}
inline void mutex_unsafe_unlock(mutex_handle &handle) {
  (void)::mtx_unlock(&handle);
}

struct lazy_mutex_handle {
  ::mtx_t mutex;
  std::int32_t once; // -1 = initialized, 0 = uninitialized, 1 = initializing
};

inline constexpr lazy_mutex_handle lazy_mutex_initializer() {
  return (lazy_mutex_handle){};
}
inline void lazy_mutex_init(lazy_mutex_handle &handle) {
  // Sadly, we can't use call_once() for this as it doesn't have a context
  if (std::atomic_load_explicit((std::atomic<std::int32_t> *)&handle.once,
                                std::memory_order_acquire) < 0)
    return;

  int zero = 0;
  if (std::atomic_compare_exchange_strong_explicit(
          (std::atomic<std::int32_t> *)&handle.once, &zero, 1,
          std::memory_order_relaxed, std::memory_order_relaxed)) {
    SWIFT_C11THREADS_CHECK(::mtx_init(&handle.mutex, ::mtx_plain));
    std::atomic_store_explicit((std::atomic<std::int32_t> *)&handle.once, -1,
                               std::memory_order_release);
    return;
  }

  while (std::atomic_load_explicit((std::atomic<std::int32_t> *)&handle.once,
                                   std::memory_order_acquire) >= 0) {
    // Just spin; ::mtx_init() is very likely to be fast
  }
}

inline void lazy_mutex_destroy(lazy_mutex_handle &handle) {
  if (std::atomic_load_explicit((std::atomic<std::int32_t> *)&handle.once,
                                std::memory_order_acquire) < 0)
    ::mtx_destroy(&handle.mutex);
}

inline void lazy_mutex_lock(lazy_mutex_handle &handle) {
  lazy_mutex_init(handle);
  SWIFT_C11THREADS_CHECK(::mtx_lock(&handle.mutex));
}
inline void lazy_mutex_unlock(lazy_mutex_handle &handle) {
  lazy_mutex_init(handle);
  SWIFT_C11THREADS_CHECK(::mtx_unlock(&handle.mutex));
}
inline bool lazy_mutex_try_lock(lazy_mutex_handle &handle) {
  lazy_mutex_init(handle);
  SWIFT_C11THREADS_RETURN_TRUE_OR_FALSE(::mtx_trylock(&handle.mutex));
}

inline void lazy_mutex_unsafe_lock(lazy_mutex_handle &handle) {
  lazy_mutex_init(handle);
  (void)::mtx_lock(&handle.mutex);
}
inline void lazy_mutex_unsafe_unlock(lazy_mutex_handle &handle) {
  lazy_mutex_init(handle);
  (void)::mtx_unlock(&handle.mutex);
}

// .. Once ...................................................................

typedef std::atomic<std::intptr_t> once_t;

void once_slow(once_t &predicate, void (*fn)(void *), void *context);

inline void once_impl(once_t &predicate, void (*fn)(void *), void *context) {
  // Sadly we can't use call_once() for this (no context)
  if (predicate.load(std::memory_order_acquire) < 0)
    return;

  once_slow(predicate, fn, context);
}

// .. Thread local storage ...................................................

// Get rid of this, because it causes clashes with TokenKinds.def
#undef thread_local

// We *can* use the C++ version though
#if __cplusplus >= 201103L || __has_feature(cxx_thread_local)
#define SWIFT_THREAD_LOCAL thread_local
#endif

using tls_key_t = ::tss_t;
using tls_dtor_t = void (*)(void *);

inline bool tls_alloc(tls_key_t &key, tls_dtor_t dtor) {
  return ::tss_create(&key, dtor) == thrd_success;
}

inline void *tls_get(tls_key_t key) { return ::tss_get(key); }

inline void tls_set(tls_key_t key, void *ptr) { ::tss_set(key, ptr); }

} // namespace threading_impl

} // namespace swift

#endif // SWIFT_THREADING_IMPL_C11_H
