//===--- MutexSingleThreaded.h - --------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// No-op implementation of locks for single-threaded environments.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_MUTEX_SINGLE_THREADED_H
#define SWIFT_RUNTIME_MUTEX_SINGLE_THREADED_H

#include "swift/Runtime/Debug.h"

namespace swift {

typedef void* MutexHandle;

#define SWIFT_MUTEX_SUPPORTS_CONSTEXPR 1

struct MutexPlatformHelper {
  static constexpr MutexHandle staticInit() { return nullptr; }
  static void init(MutexHandle &mutex, bool checked = false) {}
  static void destroy(MutexHandle &mutex) {}
  static void lock(MutexHandle &mutex) {}
  static void unlock(MutexHandle &mutex) {}
  static bool try_lock(MutexHandle &mutex) { return true; }
  static void unsafeLock(MutexHandle &mutex) {}
  static void unsafeUnlock(MutexHandle &mutex) {}
};

}

#endif
