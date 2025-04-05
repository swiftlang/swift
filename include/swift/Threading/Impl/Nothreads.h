//==--- Nothreads.h - Threading abstraction implementation ----- -*-C++ -*-===//
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
// Implements threading support for platforms without threading
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_IMPL_NOTHREADS_H
#define SWIFT_THREADING_IMPL_NOTHREADS_H

#include <optional>

#include "chrono_utils.h"

namespace swift {
namespace threading_impl {

// .. Thread related things ..................................................

using thread_id = unsigned;

inline thread_id thread_get_current() { return 0; }
inline bool thread_is_main() { return true; }
inline bool threads_same(thread_id a, thread_id b) { return a == b; }
inline std::optional<stack_bounds> thread_get_current_stack_bounds() {
  return {};
}

// .. Mutex support ..........................................................

using mutex_handle = unsigned;

inline void mutex_init(mutex_handle &handle, bool checked = false) {}
inline void mutex_destroy(mutex_handle &handle) {}
inline void mutex_lock(mutex_handle &handle) {}
inline void mutex_unlock(mutex_handle &handle) {}
inline bool mutex_try_lock(mutex_handle &handle) { return true; }

inline void mutex_unsafe_lock(mutex_handle &handle) {}
inline void mutex_unsafe_unlock(mutex_handle &handle) {}

using lazy_mutex_handle = unsigned;

#define SWIFT_LAZY_MUTEX_INITIALIZER 0

inline void lazy_mutex_destroy(lazy_mutex_handle &handle) {}
inline void lazy_mutex_lock(lazy_mutex_handle &handle) {}
inline void lazy_mutex_unlock(lazy_mutex_handle &handle) {}
inline bool lazy_mutex_try_lock(lazy_mutex_handle &handle) { return true; }

inline void lazy_mutex_unsafe_lock(lazy_mutex_handle &handle) {}
inline void lazy_mutex_unsafe_unlock(lazy_mutex_handle &handle) {}

// .. Recursive mutex support .................................................

using recursive_mutex_handle = unsigned;

inline void recursive_mutex_init(recursive_mutex_handle &handle,
                                 bool checked = false) {}
inline void recursive_mutex_destroy(recursive_mutex_handle &handle) {}
inline void recursive_mutex_lock(recursive_mutex_handle &handle) {}
inline void recursive_mutex_unlock(recursive_mutex_handle &handle) {}

// .. ConditionVariable support ..............................................

using cond_handle = unsigned;

inline void cond_init(cond_handle &handle) {}
inline void cond_destroy(cond_handle &handle) {}
inline void cond_lock(cond_handle &handle) {}
inline void cond_unlock(cond_handle &handle) {}
inline void cond_signal(cond_handle &handle) {}
inline void cond_broadcast(cond_handle &handle) {}
inline void cond_wait(cond_handle &handle) {}
template <class Rep, class Period>
inline bool cond_wait(cond_handle &handle,
                      std::chrono::duration<Rep, Period> duration) {
  return true;
}
inline bool cond_wait(cond_handle &handle,
                      std::chrono::system_clock::time_point deadline) {
  return true;
}

// .. Once ...................................................................

typedef bool once_t;

inline void once_impl(once_t &predicate, void (*fn)(void *), void *ctx) {
  if (!predicate) {
    predicate = true;
    fn(ctx);
  }
}

// .. Thread local storage ...................................................

// If we have no threads, we can use the simple version of TLS
#define SWIFT_THREAD_LOCAL

} // namespace threading_impl

} // namespace swift

#endif // SWIFT_THREADING_IMPL_NOTHREADS_H
