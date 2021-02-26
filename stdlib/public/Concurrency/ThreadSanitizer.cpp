//===--- ThreadSanitizer.cpp - Thread Sanitizer support -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Thread Sanitizer support for the Swift Task runtime
//
//===----------------------------------------------------------------------===//

#include "TaskPrivate.h"

#include <dlfcn.h>

using TSanFunc = void(void *);

void swift::_swift_tsan_acquire(void *addr) {
  static auto ptr = (TSanFunc *)dlsym(RTLD_DEFAULT, "__tsan_acquire");
  if (ptr) {
    ptr(addr);
  }
}

void swift::_swift_tsan_release(void *addr) {
  static auto ptr = (TSanFunc *)dlsym(RTLD_DEFAULT, "__tsan_release");
  if (ptr) {
    ptr(addr);
  }
}
