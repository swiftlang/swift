//==--- ThreadEmbeddedImpl.h - Embedded platform threading glue ---- -*-C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implements the Swift threading abstraction by forwarding to the Embedded
// Swift platform hooks declared in swift/EmbeddedPlatform.h.
//
// This implementation intentionally does not define SWIFT_THREAD_LOCAL. That
// makes ThreadLocalStorage.h route reserved TLS slots through key-based storage
// instead of compiling them as ordinary globals.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_IMPL_THREADEMBEDDED_H
#define SWIFT_THREADING_IMPL_THREADEMBEDDED_H

#include <optional>
#include <stdint.h>

#include "swift/EmbeddedPlatform.h"
#include "swift/shims/Visibility.h"

#define SWIFT_THREADING_USE_RESERVED_TLS_KEYS 1
#define SWIFT_THREADING_HAS_LAZY_MUTEX 0
#define SWIFT_THREADING_HAS_CONDITION_VARIABLE 0

static_assert(SWIFT_TLS_KEY_COUNT ==
                  static_cast<int>(swift::tls_key::exclusivity) + 1,
              "EmbeddedPlatform TLS key count must match TLSKeys.h");

namespace swift {

SWIFT_RUNTIME_EXPORT
void swift_once(intptr_t *predicate, void (*fn)(void *), void *context);

namespace threading_impl {

using once_t = intptr_t;
using thread_id = uintptr_t;

inline thread_id thread_get_current() {
  return 0;
}

inline bool thread_is_main() {
  return _swift_thread_isMain() != 0;
}

inline bool threads_same(thread_id a, thread_id b) {
  return a == b;
}

inline std::optional<stack_bounds> thread_get_current_stack_bounds() {
  return {};
}

struct mutex_handle {
  uintptr_t storage[EMBEDDED_SWIFT_MUTEX_NUM_WORDS] = {};
};

inline void mutex_init(mutex_handle &handle, bool checked = false) {
  _swift_mutex_init(&handle, checked ? SWIFT_MUTEX_CHECKED : SWIFT_MUTEX_NONE);
}

inline void mutex_destroy(mutex_handle &handle) {
  _swift_mutex_destroy(&handle);
}

inline void mutex_lock(mutex_handle &handle) {
  _swift_mutex_lock(&handle);
}

inline void mutex_unlock(mutex_handle &handle) {
  _swift_mutex_unlock(&handle);
}

inline bool mutex_try_lock(mutex_handle &handle) {
  return _swift_mutex_tryLock(&handle) != 0;
}

inline void mutex_unsafe_lock(mutex_handle &handle) {
  _swift_mutex_lock(&handle);
}

inline void mutex_unsafe_unlock(mutex_handle &handle) {
  _swift_mutex_unlock(&handle);
}

using recursive_mutex_handle = mutex_handle;

inline void recursive_mutex_init(recursive_mutex_handle &handle,
                                 bool checked = false) {
  _swift_mutex_init(&handle,
                    static_cast<swift_mutex_flags_t>(
                        SWIFT_MUTEX_RECURSIVE |
                        (checked ? SWIFT_MUTEX_CHECKED : SWIFT_MUTEX_NONE)));
}

inline void recursive_mutex_destroy(recursive_mutex_handle &handle) {
  _swift_mutex_destroy(&handle);
}

inline void recursive_mutex_lock(recursive_mutex_handle &handle) {
  _swift_mutex_lock(&handle);
}

inline void recursive_mutex_unlock(recursive_mutex_handle &handle) {
  _swift_mutex_unlock(&handle);
}

inline void once_impl(once_t &predicate, void (*fn)(void *), void *ctx) {
  ::swift::swift_once(&predicate, fn, ctx);
}

using tls_key_t = swift_tls_key_t;
using tls_dtor_t = swift_tls_dtor_t;

inline tls_key_t tls_get_key(swift::tls_key key) {
  return static_cast<tls_key_t>(key);
}

inline bool tls_init(tls_key_t key, tls_dtor_t dtor) {
  _swift_tls_init(key, dtor);
  return true;
}

inline void *tls_get(tls_key_t key) {
  return _swift_tls_get(key);
}

inline void tls_set(tls_key_t key, void *ptr) {
  _swift_tls_set(key, ptr);
}

} // namespace threading_impl
} // namespace swift

#endif // SWIFT_THREADING_IMPL_THREADEMBEDDED_H
