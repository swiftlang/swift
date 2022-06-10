//==--- Darwin.h - Threading abstraction implementation -------- -*-C++ -*-===//
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
// Implements threading support for Apple platforms
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_IMPL_DARWIN_H
#define SWIFT_THREADING_IMPL_DARWIN_H

#include <dispatch/dispatch.h>
#include <os/lock.h>
#include <pthread.h>

#include "llvm/ADT/Optional.h"

#include "swift/Threading/Errors.h"

namespace swift {
namespace threading_impl {

// .. Thread related things ..................................................

using thread_id = ::pthread_t;

inline thread_id thread_get_current() { return ::pthread_self(); }

inline bool thread_is_main() { return pthread_main_np(); }

inline bool threads_same(thread_id a, thread_id b) {
  return ::pthread_equal(a, b);
}

inline llvm::Optional<stack_bounds> thread_get_current_stack_bounds() {
  stack_bounds result;
  pthread_t thread = pthread_self();

  // On Apple platforms, pthread_get_stackaddr_np() gets the address of the
  // *end* of the stack (i.e. the highest address in stack space), *NOT* the
  // address of the *base* of the stack (the lowest address).
  result.high = pthread_get_stackaddr_np(thread);
  result.low = (char *)result.high - pthread_get_stacksize_np(thread);

  return result;
}

// .. Mutex support ..........................................................

using mutex_handle = ::os_unfair_lock;

inline void mutex_init(mutex_handle &handle, bool checked = false) {
  handle = OS_UNFAIR_LOCK_INIT;
}
inline void mutex_destroy(mutex_handle &handle) {}

inline void mutex_lock(mutex_handle &handle) { ::os_unfair_lock_lock(&handle); }
inline void mutex_unlock(mutex_handle &handle) {
  ::os_unfair_lock_unlock(&handle);
}
inline bool mutex_try_lock(mutex_handle &handle) {
  return ::os_unfair_lock_trylock(&handle);
}

inline void mutex_unsafe_lock(mutex_handle &handle) {
  ::os_unfair_lock_lock(&handle);
}
inline void mutex_unsafe_unlock(mutex_handle &handle) {
  ::os_unfair_lock_unlock(&handle);
}

using lazy_mutex_handle = ::os_unfair_lock;

// We don't need to be lazy here because Darwin has OS_UNFAIR_LOCK_INIT.
inline constexpr lazy_mutex_handle lazy_mutex_initializer() {
  return OS_UNFAIR_LOCK_INIT;
}
inline void lazy_mutex_destroy(lazy_mutex_handle &handle) {}

inline void lazy_mutex_lock(lazy_mutex_handle &handle) {
  ::os_unfair_lock_lock(&handle);
}
inline void lazy_mutex_unlock(lazy_mutex_handle &handle) {
  ::os_unfair_lock_unlock(&handle);
}
inline bool lazy_mutex_try_lock(lazy_mutex_handle &handle) {
  return ::os_unfair_lock_trylock(&handle);
}

inline void lazy_mutex_unsafe_lock(lazy_mutex_handle &handle) {
  ::os_unfair_lock_lock(&handle);
}
inline void lazy_mutex_unsafe_unlock(lazy_mutex_handle &handle) {
  ::os_unfair_lock_unlock(&handle);
}

// .. Once ...................................................................

using once_t = ::dispatch_once_t;

inline void once_impl(once_t &predicate, void (*fn)(void *), void *context) {
  dispatch_once_f(&predicate, context, fn);
}

// .. Thread local storage ...................................................

// On Darwin, we want to use the reserved keys
#define SWIFT_THREADING_USE_RESERVED_TLS_KEYS 1

#if __has_include(<pthread/tsd_private.h>)
} // namespace threading_impl
} // namespace swift

extern "C" {
#include <pthread/tsd_private.h>
}

namespace swift {
namespace threading_impl {
#else
#define __PTK_FRAMEWORK_SWIFT_KEY0 100
#define __PTK_FRAMEWORK_SWIFT_KEY1 101
#define __PTK_FRAMEWORK_SWIFT_KEY2 102
#define __PTK_FRAMEWORK_SWIFT_KEY3 103
#define __PTK_FRAMEWORK_SWIFT_KEY4 104
#define __PTK_FRAMEWORK_SWIFT_KEY5 105
#define __PTK_FRAMEWORK_SWIFT_KEY6 106
#define __PTK_FRAMEWORK_SWIFT_KEY7 107
#define __PTK_FRAMEWORK_SWIFT_KEY8 108
#define __PTK_FRAMEWORK_SWIFT_KEY9 109

extern "C" {

extern int pthread_key_init_np(int, void (*)(void *));

inline bool _pthread_has_direct_tsd() { return false; }
inline void *_pthread_getspecific_direct(pthread_key_t k) {
  return pthread_getspecific(k);
}
inline void _pthread_setspecific_direct(pthread_key_t k, void *v) {
  pthread_setspecific(k, v);
}
}
#endif

#define SWIFT_TLS_DECLARE_DTOR(name) void name(void *)

using tls_key_t = pthread_key_t;
using tls_dtor_t = void (*)(void *);

inline tls_key_t tls_get_key(tls_key k) {
  switch (k) {
  case tls_key::runtime:
    return __PTK_FRAMEWORK_SWIFT_KEY0;
  case tls_key::stdlib:
    return __PTK_FRAMEWORK_SWIFT_KEY1;
  case tls_key::compatibility50:
    return __PTK_FRAMEWORK_SWIFT_KEY2;
  case tls_key::concurrency_task:
    return __PTK_FRAMEWORK_SWIFT_KEY3;
  case tls_key::concurrency_executor_tracking_info:
    return __PTK_FRAMEWORK_SWIFT_KEY4;
  case tls_key::concurrency_fallback:
    return __PTK_FRAMEWORK_SWIFT_KEY5;
  }
}

inline bool tls_init(tls_key_t key, tls_dtor_t dtor) {
  return pthread_key_init_np(key, dtor) == 0;
}

inline bool tls_init(tls_key key, tls_dtor_t dtor) {
  return tls_init(tls_get_key(key), dtor);
}

inline bool tls_alloc(tls_key_t &key, tls_dtor_t dtor) {
  return pthread_key_create(&key, dtor) == 0;
}

inline void *tls_get(tls_key_t key) {
  if (_pthread_has_direct_tsd())
    return _pthread_getspecific_direct(key);
  else
    return pthread_getspecific(key);
}

inline void *tls_get(tls_key key) { return tls_get(tls_get_key(key)); }

inline void tls_set(tls_key_t key, void *value) {
  if (_pthread_has_direct_tsd())
    _pthread_setspecific_direct(key, value);
  else
    pthread_setspecific(key, value);
}

inline void tls_set(tls_key key, void *value) {
  tls_set(tls_get_key(key), value);
}

} // namespace threading_impl

} // namespace swift

#endif // SWIFT_THREADING_IMPL_DARWIN_H
