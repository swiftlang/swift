//===--- Shims.cpp - Operating system shims ---------------------*- C++ -*-===//
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
//  Defines operating system shims.
//
//===----------------------------------------------------------------------===//

// We do this here because we can safely include dlfcn.h from this file
// without worrying about it trying to drag in the Darwin module.
//
// If we did this in _SwiftBacktracing.h as an inline, we'd need to know
// what RTLD_LAZY was for every platform we support.

#if __has_include(<dlfcn.h>)
#include <dlfcn.h>

#ifdef __cplusplus
namespace swift {
extern "C" {
#endif

void *_swift_backtrace_dlopen_lazy(const char *path) {
  return dlopen(path, RTLD_LAZY);
}

void *_swift_backtrace_dlsym(void *handle, const char *name) {
  return dlsym(handle, name);
}

#ifdef __cplusplus
} // extern "C"
} // namespace swift
#endif

#endif

