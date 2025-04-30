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

#include "chrono_utils.h"

#include <optional>

#include "swift/Threading/Errors.h"

namespace swift {
namespace threading_impl {

#define SWIFT_C11THREADS_CHECK(expr)                                           \
  do {                                                                         \
    int res_ = (expr);                                                         \
    if (res_ != thrd_success)                                                  \
      swift::threading::fatal(#expr " failed with error %d\n", res_);          \
  } while (0)

#define SWIFT_C11THREADS_RETURN_TRUE_OR_FALSE(falseerr, expr)                  \
  do {                                                                         \
    int res_ = (expr);                                                         \
    switch (res_) {                                                            \
    case thrd_success:                                                         \
      return true;                                                             \
    case falseerr:                                                             \
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
inline std::optional<stack_bounds> thread_get_current_stack_bounds() {
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
  SWIFT_C11THREADS_RETURN_TRUE_OR_FALSE(thrd_busy, ::mtx_trylock(&handle));
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

#define SWIFT_LAZY_MUTEX_INITIALIZER ((threading_impl::lazy_mutex_handle){})

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
  SWIFT_C11THREADS_RETURN_TRUE_OR_FALSE(thrd_busy, ::mtx_trylock(&handle.mutex));
}

inline void lazy_mutex_unsafe_lock(lazy_mutex_handle &handle) {
  lazy_mutex_init(handle);
  (void)::mtx_lock(&handle.mutex);
}
inline void lazy_mutex_unsafe_unlock(lazy_mutex_handle &handle) {
  lazy_mutex_init(handle);
  (void)::mtx_unlock(&handle.mutex);
}

// .. Recursive mutex support .................................................

using recursive_mutex_handle = ::mtx_t;

inline void recursive_mutex_init(recursive_mutex_handle &handle,
                                 bool checked = false) {
  SWIFT_C11THREADS_CHECK(::mtx_init(&handle, ::mtx_recursive));
}
inline void recursive_mutex_destroy(recursive_mutex_handle &handle) {
  ::mtx_destroy(&handle);
}

inline void recursive_mutex_lock(recursive_mutex_handle &handle) {
  SWIFT_C11THREADS_CHECK(::mtx_lock(&handle));
}
inline void recursive_mutex_unlock(recursive_mutex_handle &handle) {
  SWIFT_C11THREADS_CHECK(::mtx_unlock(&handle));
}

// .. ConditionVariable support ..............................................

struct cond_handle {
  ::cnd_t condition;
  ::mtx_t mutex;
};

inline void cond_init(cond_handle &handle) {
  SWIFT_C11THREADS_CHECK(::cnd_init(&handle.condition));
  SWIFT_C11THREADS_CHECK(::mtx_init(&handle.mutex, ::mtx_plain));
}
inline void cond_destroy(cond_handle &handle) {
  ::cnd_destroy(&handle.condition);
  ::mtx_destroy(&handle.mutex);
}
inline void cond_lock(cond_handle &handle) {
  SWIFT_C11THREADS_CHECK(::mtx_lock(&handle.mutex));
}
inline void cond_unlock(cond_handle &handle) {
  SWIFT_C11THREADS_CHECK(::mtx_unlock(&handle.mutex));
}
inline void cond_signal(cond_handle &handle) {
  SWIFT_C11THREADS_CHECK(::cnd_signal(&handle.condition));
}
inline void cond_broadcast(cond_handle &handle) {
  SWIFT_C11THREADS_CHECK(::cnd_broadcast(&handle.condition));
}
inline void cond_wait(cond_handle &handle) {
  SWIFT_C11THREADS_CHECK(::cnd_wait(&handle.condition, &handle.mutex));
}
template <class Rep, class Period>
inline bool cond_wait(cond_handle &handle,
                      std::chrono::duration<Rep, Period> duration) {
  auto to_wait = chrono_utils::ceil<
    std::chrono::system_clock::duration>(duration);
  auto deadline = std::chrono::system_clock::now() + to_wait;
  return cond_wait(handle, deadline);
}
inline bool cond_wait(cond_handle &handle,
                      std::chrono::system_clock::time_point deadline) {
  auto ns = chrono_utils::ceil<std::chrono::nanoseconds>(
    deadline.time_since_epoch()).count();
  struct ::timespec ts = { ::time_t(ns / 1000000000), long(ns % 1000000000) };
  SWIFT_C11THREADS_RETURN_TRUE_OR_FALSE(
    thrd_timedout,
    ::cnd_timedwait(&handle.condition, &handle.mutex, &ts)
  );
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
using tls_dtor_t = ::tss_dtor_t;

inline bool tls_alloc(tls_key_t &key, tls_dtor_t dtor) {
  return ::tss_create(&key, dtor) == thrd_success;
}

inline void *tls_get(tls_key_t key) { return ::tss_get(key); }

inline void tls_set(tls_key_t key, void *ptr) { ::tss_set(key, ptr); }

} // namespace threading_impl

} // namespace swift

#endif // SWIFT_THREADING_IMPL_C11_H
