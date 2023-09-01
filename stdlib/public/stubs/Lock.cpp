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

#include "swift/shims/Lock.h"
#include "swift/Threading/Mutex.h"

using namespace swift;

static_assert(sizeof(threading_impl::mutex_handle) == sizeof(_SwiftLock));
static_assert(alignof(threading_impl::mutex_handle) == alignof(_SwiftLock));

SWIFT_RUNTIME_STDLIB_INTERNAL
void swift_stdlib_lock_init(_SwiftLock *lock) {
  threading_impl::mutex_init(*reinterpret_cast<threading_impl::mutex_handle *>(lock));
}

SWIFT_RUNTIME_STDLIB_INTERNAL
void swift_stdlib_lock_lock(_SwiftLock *lock) {
  threading_impl::mutex_lock(*reinterpret_cast<threading_impl::mutex_handle *>(lock));
}

SWIFT_RUNTIME_STDLIB_INTERNAL
void swift_stdlib_lock_unlock(_SwiftLock *lock) {
  threading_impl::mutex_unlock(*reinterpret_cast<threading_impl::mutex_handle *>(lock));
}

SWIFT_RUNTIME_STDLIB_INTERNAL
void swift_stdlib_lock_destroy(_SwiftLock *lock) {
  threading_impl::mutex_destroy(*reinterpret_cast<threading_impl::mutex_handle *>(lock));
}
