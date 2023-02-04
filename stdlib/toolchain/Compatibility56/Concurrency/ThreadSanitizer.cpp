//===--- ThreadSanitizer.cpp ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Thread Sanitizer support for the Swift Task runtime.
//
//===----------------------------------------------------------------------===//

#include "Concurrency/TaskPrivate.h"
#include "swift/Basic/Lazy.h"

#include <dlfcn.h>

namespace {
using TSanFunc = void(void *);
} // anonymous namespace

// Note: We can't use a proper interface to get the `__tsan_acquire` and
// `__tsan_release` from the public/Concurrency/ThreadSanitizer.cpp.
// Unfortunately, we can't do this because there is no interface in the runtimes
// we are backdeploying to. So we're stuck using this lazy dlsym game.
// Number of times I've tried to fix this: 3

void swift::_swift_tsan_acquire(void *addr) {
  const auto backdeploy_tsan_acquire =
    reinterpret_cast<TSanFunc *>(SWIFT_LAZY_CONSTANT(dlsym(RTLD_DEFAULT, "__tsan_acquire")));
  if (backdeploy_tsan_acquire) {
    backdeploy_tsan_acquire(addr);
    SWIFT_TASK_DEBUG_LOG("tsan_acquire on %p", addr);
  }
}

void swift::_swift_tsan_release(void *addr) {
  const auto backdeploy_tsan_release =
    reinterpret_cast<TSanFunc *>(SWIFT_LAZY_CONSTANT(dlsym(RTLD_DEFAULT, "__tsan_release")));
  if (backdeploy_tsan_release) {
    backdeploy_tsan_release(addr);
    SWIFT_TASK_DEBUG_LOG("tsan_release on %p", addr);
  }
}
