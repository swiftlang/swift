//===--- ThreadSanitizer.cpp - Thread Sanitizer support -------------------===//
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

#include "swift/Threading/ThreadSanitizer.h"
#include "swift/shims/Visibility.h"

#include <cstdio>

namespace swift {
namespace threading_impl {

extern "C" SWIFT_ATTRIBUTE_FOR_EXPORTS
bool _swift_tsan_enabled = false;
extern "C" SWIFT_ATTRIBUTE_FOR_EXPORTS
void (*_swift_tsan_acquire)(const void *) = nullptr;
extern "C" SWIFT_ATTRIBUTE_FOR_EXPORTS
void (*_swift_tsan_release)(const void *) = nullptr;

#if __has_include(<dlfcn.h>)
#include <dlfcn.h>

// The TSan library code will call this function when it starts up
extern "C" SWIFT_ATTRIBUTE_FOR_EXPORTS
void __tsan_on_initialize() {
  _swift_tsan_enabled = true;
  _swift_tsan_acquire = (void (*)(const void *))dlsym(RTLD_DEFAULT,
                                                      "__tsan_acquire");
  _swift_tsan_release = (void (*)(const void *))dlsym(RTLD_DEFAULT,
                                                      "__tsan_release");

  // Always call through to the next image; this won't work on macOS, but it's
  // important on Linux to allow others to hook into the thread sanitizer if
  // they wish.
  void (*next_init)(void);
  next_init = (void (*)(void))dlsym(RTLD_NEXT, "__tsan_on_initialize");
  if (next_init) {
    next_init();
  }
}
#endif

} // namespace threading_impl
} // namespace swift
