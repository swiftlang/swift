//===----------------------------------------------------------------------===//
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

#include "swift/Runtime/Config.h"
#include "swift/Threading/Mutex.h"

using namespace swift;

extern "C" SWIFT_CC(swift) __attribute__((visibility("hidden")))
size_t _swift_observation_lock_size() {
  size_t bytes = sizeof(Mutex);

  if (bytes < 1) {
    return 1;
  }

  return bytes;
}

extern "C" SWIFT_CC(swift) __attribute__((visibility("hidden")))
void _swift_observation_lock_init(Mutex &lock) {
  new (&lock) Mutex();
}

extern "C" SWIFT_CC(swift) __attribute__((visibility("hidden")))
void _swift_observation_lock_lock(Mutex &lock) {
  lock.lock();
}

extern "C" SWIFT_CC(swift) __attribute__((visibility("hidden")))
void _swift_observation_lock_unlock(Mutex &lock) {
  lock.unlock();
}
