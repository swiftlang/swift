//===----------------------------------------------------------------------===//
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

#ifndef SWIFT_STDLIB_SHIMS_LOCK_H
#define SWIFT_STDLIB_SHIMS_LOCK_H

#include "SwiftStdint.h"
#include "Visibility.h"

// We cannot '#include "swift/Threading/Mutex.h"' because it is a C++ header
// while SwiftShims mostly expects to parse C. Because of this limitation,
// ENSURE that what we're typealiasing to 'Lock' must be equivalent to
// threading_impl::mutex_handle on all platforms. We assume that 'UnsafeCell'
// has the same layout as this mutex handle.

#if defined(__APPLE__)

// We cannot directly reference os_unfair_lock because it will cause the
// clang importer to implicitly depend on the os overlay module and the
// standard library must not depend on any overlay.
typedef __swift_uint32_t _SwiftLock;

#elif defined(__linux__)

#include <pthread.h>

typedef pthread_mutex_t _SwiftLock;

#elif defined(_WIN32)

// From swift/Threading/Impl/Win32/Win32Defs.h
//
// We cannot include it because it is a C++ header.

// We do this because we can't declare _RTL_SRWLOCK here in case someone
// later includes <windows.h>
struct SWIFT_SRWLOCK {
  PVOID Ptr;
};

typedef SWIFT_SRWLOCK _SwiftLock;

#endif

SWIFT_RUNTIME_STDLIB_INTERNAL
void swift_stdlib_lock_init(_SwiftLock *lock);

SWIFT_RUNTIME_STDLIB_INTERNAL
void swift_stdlib_lock_lock(_SwiftLock *lock);

SWIFT_RUNTIME_STDLIB_INTERNAL
void swift_stdlib_lock_unlock(_SwiftLock *lock);

SWIFT_RUNTIME_STDLIB_INTERNAL
void swift_stdlib_lock_destroy(_SwiftLock *lock);

#endif // SWIFT_STDLIB_SHIMS_LOCK_H
