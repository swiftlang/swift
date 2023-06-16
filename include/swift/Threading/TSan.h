//===--- TSan.h - TSan support functions ---------------------- -*- C++ -*-===//
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

#ifndef SWIFT_THREADING_TSAN_H
#define SWIFT_THREADING_TSAN_H

namespace swift {

#if defined(_WIN32) || defined(__wasi__) || !__has_include(<dlfcn.h>)
namespace tsan {

inline bool enabled() { return false; }
template <typename T> T *acquire(T *ptr) { return ptr; }
template <typename T> T *release(T *ptr) { return ptr; }
template <typename T> T *consume(T *ptr) { return ptr; }

} // namespace tsan
#else

namespace threading_impl {

extern bool tsan_enabled;
extern void (*tsan_acquire)(const void *ptr);
extern void (*tsan_release)(const void *ptr);

} // namespace threading_impl

namespace tsan {

/// Returns true if TSan is enabled
inline bool enabled() {
  return threading_impl::tsan_enabled;
}

/// Indicate to TSan that an acquiring load has occurred on the current
/// thread.  If some other thread does a releasing store with the same
/// pointer, we are indicating to TSan that all writes that happened
/// before that store will be visible to the current thread after the
/// `acquire()`.
template <typename T>
T *acquire(T *ptr) {
  if (threading_impl::tsan_acquire) {
    threading_impl::tsan_acquire(ptr);
  }
  return ptr;
}

/// Indicate to TSan that a releasing store has occurred on the current
/// thread.  If some other thread does an acquiring load with the same
/// pointer, we are indicating to TSan that that thread will be able to
/// see all writes that happened before the `release()`.
template <typename T>
T *release(T *ptr) {
  if (threading_impl::tsan_release) {
    threading_impl::tsan_release(ptr);
  }
  return ptr;
}

/// Indicate to TSan that a consuming load has occurred on the current
/// thread.  If some other thread does a releasing store with the same
/// pointer, we are indicating to TSan that all writes that happened
/// before that store will be visible *to those operations that carry a
/// dependency on the loaded value*.
///
/// TSan doesn't currently know about consume, so we lie and say it's an
/// acquire instead.
template <typename T>
T *consume(T *ptr) {
  return acquire(ptr);
}

} // namespace tsan

#endif

} // namespace swift

#endif
