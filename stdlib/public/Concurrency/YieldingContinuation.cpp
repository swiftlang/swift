//===--- YieldingContinuation.cpp - Multi-resume locking interface --------===//
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

#include "swift/Runtime/Mutex.h"

namespace swift {
// return the size in words for the given mutex primitive
extern "C"
size_t swift_yielding_continuation_lock_size() {
  size_t words = sizeof(MutexHandle) / sizeof(void *);
  if (words < 1) { return 1; }
  return words;
}

extern "C"
void swift_yielding_continuation_lock_init(MutexHandle &lock) {
  lock = MutexPlatformHelper::staticInit();
}

extern "C"
void swift_yielding_continuation_lock_lock(MutexHandle &lock) {
  MutexPlatformHelper::lock(lock);
}

extern "C"
void swift_yielding_continuation_lock_unlock(MutexHandle &lock) {
  MutexPlatformHelper::unlock(lock);
}
};
