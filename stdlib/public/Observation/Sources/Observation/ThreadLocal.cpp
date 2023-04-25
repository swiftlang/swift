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
#include "swift/Runtime/Once.h"
#include "swift/Threading/ThreadLocalStorage.h"

static SWIFT_THREAD_LOCAL_TYPE(void *, swift::tls_key::observation_transaction) Value;

extern "C" SWIFT_CC(swift) __attribute__((visibility("hidden")))
void *_swift_observation_tls_get() {
  return Value.get();
}

extern "C" SWIFT_CC(swift) __attribute__((visibility("hidden")))
void _swift_observation_tls_set(void *value) {
  Value.set(value);
}
