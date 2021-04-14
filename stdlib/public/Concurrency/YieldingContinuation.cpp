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
extern "C"
size_t swift_yielding_continuation_lock_size() {
  return sizeof(MutexHandle);
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
