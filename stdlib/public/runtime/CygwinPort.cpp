//===--- CygwinPort.cpp - Functions for Cygwin port -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implementations Cygwin specific functions needed for running Swift.
//
//===----------------------------------------------------------------------===//

#if (defined(_WIN32) || defined(__CYGWIN__)) && !defined(_MSC_VER)
#include "Private.h"
#include "swift/Runtime/Debug.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <mutex>
#include <vector>

#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#include <psapi.h>

using namespace swift;

static std::mutex swiftOnceMutex;

void swift::_swift_once_f(uintptr_t *predicate, void *context,
                          void (*function)(void *)) {
  // FIXME: This implementation does a global lock, which is much worse than
  // what we have on other platforms. Each swift_once should synchronize on the
  // token.
  swiftOnceMutex.lock();
  if (*predicate == 0) {
    *predicate = 1ul;
    swiftOnceMutex.unlock();

    function(context);
  } else
    swiftOnceMutex.unlock();
}
#endif // (defined(_WIN32) || defined(__CYGWIN__)) && !defined(_MSC_VER)
