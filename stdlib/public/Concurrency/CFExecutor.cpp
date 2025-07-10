//===--- CFExecutor.cpp ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#include "swift/Runtime/Concurrency.h"

#if !defined(_WIN32) && !defined(__wasi__) && __has_include(<dlfcn.h>)
#include <dlfcn.h>
#endif

using namespace swift;

SWIFT_CC(swift)
extern "C" void * swift_concurrency_dlopen_noload(const char * __path) {
  return dlopen( __path, RTLD_NOLOAD);
}

SWIFT_CC(swift)
extern "C" void * swift_concurrency_dlsym(void * __handle, const char * __symbol) {
  return dlsym(__handle, __symbol);
}
