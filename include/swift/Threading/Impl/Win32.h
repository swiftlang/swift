//==--- Win32.h - Threading abstraction implementation --------- -*-C++ -*-===//
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
// Implements threading support for Windows threads
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_IMPL_WIN32_H
#define SWIFT_THREADING_IMPL_WIN32_H

#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>

#include <atomic>

namespace swift {
namespace threading_impl {

// .. Thread related things ..................................................

using thread_id = DWORD;

inline thread_id thread_get_current() { return ::GetCurrentThreadId(); }
thread_id thread_get_main();
bool thread_is_main();
inline bool threads_same(thread_id a, thread_id b) { return a == b; }

// .. Mutex support ..........................................................

using mutex_handle = SRWLOCK;

inline void mutex_init(mutex_handle &handle, bool checked=false) {
  handle = SRWLOCK_INIT;
}
inline void mutex_destroy(mutex_handle &handle) { }

inline void mutex_lock(mutex_handle &handle) {
  AcquireSRWLockExclusive(&handle);
}
inline void mutex_unlock(mutex_handle &handle) {
  ReleaseSRWLockExclusive(&handle);
}
inline bool mutex_try_lock(mutex_handle &handle) {
  return !!TryAcquireSRWLockExclusive(&handle);
}

inline void mutex_unsafe_lock(mutex_handle &handle) {
  AcquireSRWLockExclusive(&handle);
}
inline void mutex_unsafe_unlock(mutex_handle &handle) {
  ReleaseSRWLockExclusive(&handle);
}

using lazy_mutex_handle = SRWLOCK;

// We don't need to be lazy here because Win32 has SRWLOCK_INIT.
inline void lazy_mutex_init(lazy_mutex_handle &handle) {
  handle = SRWLOCK_INIT;
}
inline void lazy_mutex_destroy(lazy_mutex_handle &handle) { }

inline void lazy_mutex_lock(lazy_mutex_handle &handle) {
  AcquireSRWLockExclusive(&handle);
}
inline void lazy_mutex_unlock(lazy_mutex_handle &handle) {
  ReleaseSRWLockExclusive(&handle);
}
inline bool lazy_mutex_try_lock(lazy_mutex_handle &handle) {
  return !!TryAcquireSRWLockExclusive(&handle);
}

inline void lazy_mutex_unsafe_lock(lazy_mutex_handle &handle) {
  AcquireSRWLockExclusive(&handle);
}
inline void lazy_mutex_unsafe_unlock(lazy_mutex_handle &handle) {
  ReleaseSRWLockExclusive(&handle);
}

// .. Once ...................................................................

typedef std::atomic<int64_t> once_t;

inline void once(once_t &predicate, void (*fn)(void *), void *context) {
  // Using INIT_ONCE is slower than doing this.
  if (predicate.load(std::memory_order_acquire) < 0)
    return;

  once_slow(predicate, fn, context);
}

// .. Thread local storage ...................................................

#if __cplusplus >= 201103L || __has_feature(cxx_thread_local)
#define SWIFT_THREAD_LOCAL thread_local
#endif

#define SWIFT_TLS_DECLARE_DTOR(name) void NTAPI name(void *)

using tls_key = DWORD;
using tls_dtor = void NTAPI (*)(void *);

inline bool tls_alloc(tls_key &key, tls_dtor dtor) {
  key = FlsAlloc(dtor);
  return key != FLS_OUT_OF_INDEXES;
}

inline void *tls_get(tls_key key) {
  return FlsGetValue(key);
}

inline void tls_set(tls_key key, void *value) {
  FlsSetValue(key, value);
}

}

#endif // SWIFT_THREADING_IMPL_WIN32_H
