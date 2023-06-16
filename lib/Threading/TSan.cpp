//===--- TSan.h - TSan support functions ----------------------------------===//
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

#include "swift/Threading/TSan.h"
#include "swift/shims/Visibility.h"

namespace swift {
namespace threading_impl {

bool tsan_enabled = false;
void (*tsan_acquire)(const void *) = nullptr;
void (*tsan_release)(const void *) = nullptr;

#if __has_include(<dlfcn.h>)
#include <dlfcn.h>

// The TSan library code will call this function when it starts up
extern "C" SWIFT_ATTRIBUTE_FOR_EXPORTS
void __tsan_on_initialize() {
  tsan_enabled = true;
  tsan_acquire = (void (*)(const void *))dlsym(RTLD_DEFAULT, "__tsan_acquire");
  tsan_release = (void (*)(const void *))dlsym(RTLD_DEFAULT, "__tsan_release");

  // Always call through to the next image
  void (*next_init)(void);
  next_init = (void (*)(void))dlsym(RTLD_NEXT, "__tsan_on_initialize");
  if (next_init)
    next_init();
}
#endif

} // namespace threading_impl
} // namespace swift
