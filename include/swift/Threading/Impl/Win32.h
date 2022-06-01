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

#include "Win32/Win32Defs.h"

#include <atomic>

#include "llvm/ADT/Optional.h"

namespace swift {
namespace threading_impl {

// .. Thread related things ..................................................

using thread_id = ::DWORD;

inline thread_id thread_get_current() { return ::GetCurrentThreadId(); }
bool thread_is_main();
inline bool threads_same(thread_id a, thread_id b) { return a == b; }
llvm::Optional<stack_bounds> thread_get_current_stack_bounds();

// .. Mutex support ..........................................................

using mutex_handle = SWIFT_SRWLOCK;

inline void mutex_init(mutex_handle &handle, bool checked = false) {
  handle = SRWLOCK_INIT;
}
inline void mutex_destroy(mutex_handle &handle) {}

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

using lazy_mutex_handle = SWIFT_SRWLOCK;

// We don't need to be lazy here because Win32 has SRWLOCK_INIT.
inline constexpr lazy_mutex_handle lazy_mutex_initializer() {
  return SRWLOCK_INIT;
}
inline void lazy_mutex_destroy(lazy_mutex_handle &handle) {}

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

void once_slow(once_t &predicate, void (*fn)(void *), void *context);

inline void once_impl(once_t &predicate, void (*fn)(void *), void *context) {
  // Using INIT_ONCE is slower than doing this.
  if (predicate.load(std::memory_order_acquire) < 0)
    return;

  once_slow(predicate, fn, context);
}

// .. Thread local storage ...................................................

#ifdef __clang__
#if __has_feature(cxx_thread_local)
#define SWIFT_THREAD_LOCAL thread_local
#endif
#elif __cplusplus >= 201103L
#define SWIFT_THREAD_LOCAL thread_local
#endif

#define SWIFT_TLS_DECLARE_DTOR(name) void NTAPI name(void *)

using tls_key_t = ::DWORD;
using tls_dtor_t = ::PFLS_CALLBACK_FUNCTION;

inline bool tls_alloc(tls_key_t &key, tls_dtor_t dtor) {
  key = ::FlsAlloc(dtor);
  return key != FLS_OUT_OF_INDEXES;
}

inline void *tls_get(tls_key_t key) { return ::FlsGetValue(key); }

inline void tls_set(tls_key_t key, void *value) { ::FlsSetValue(key, value); }

} // namespace threading_impl

} // namespace swift

#endif // SWIFT_THREADING_IMPL_WIN32_H
