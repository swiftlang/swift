//===--- ThreadSanitizer.h - Thread Sanitizer support --------- -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Helper functions for code that needs to integrate with the thread
// sanitizer.  In particular, TSan can't see inside the runtime libraries,
// so we occasionally need to give it a hint that we're doing synchronization
// in order to avoid false positives.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_THREAD_SANITIZER_H
#define SWIFT_THREADING_THREAD_SANITIZER_H

#include "swift/shims/Visibility.h"

namespace swift {

#if defined(_WIN32) || defined(__wasi__) || !__has_include(<dlfcn.h>)

#define SWIFT_THREADING_TSAN_SUPPORT 0

namespace tsan {

inline bool enabled() { return false; }
template <typename T> T *acquire(T *ptr) { return ptr; }
template <typename T> T *release(T *ptr) { return ptr; }

} // namespace tsan
#else

#define SWIFT_THREADING_TSAN_SUPPORT 1

// If we're static linking to libswiftThreading.a, these symbols can come
// from there.  If, on the other hand, we're dynamically linked, we want
// to get them from libswiftCore.dylib instead.
#if SWIFT_THREADING_STATIC
#define SWIFT_THREADING_EXPORT extern "C"
#else
#define SWIFT_THREADING_EXPORT SWIFT_RUNTIME_EXPORT
#endif

namespace threading_impl {

SWIFT_THREADING_EXPORT bool _swift_tsan_enabled;
SWIFT_THREADING_EXPORT void (*_swift_tsan_acquire)(const void *ptr);
SWIFT_THREADING_EXPORT void (*_swift_tsan_release)(const void *ptr);

} // namespace threading_impl

namespace tsan {

/// Returns true if TSan is enabled
inline bool enabled() {
  return threading_impl::_swift_tsan_enabled;
}

/// Indicate to TSan that an acquiring load has occurred on the current
/// thread.  If some other thread does a releasing store with the same
/// pointer, we are indicating to TSan that all writes that happened
/// before that store will be visible to the current thread after the
/// `acquire()`.
template <typename T>
T *acquire(T *ptr) {
  if (threading_impl::_swift_tsan_acquire) {
    threading_impl::_swift_tsan_acquire(ptr);
  }
  return ptr;
}

/// Indicate to TSan that a releasing store has occurred on the current
/// thread.  If some other thread does an acquiring load with the same
/// pointer, we are indicating to TSan that that thread will be able to
/// see all writes that happened before the `release()`.
template <typename T>
T *release(T *ptr) {
  if (threading_impl::_swift_tsan_release) {
    threading_impl::_swift_tsan_release(ptr);
  }
  return ptr;
}

} // namespace tsan

#endif

} // namespace swift

#endif
