//===--- ErrorObjectCommon.cpp - Recoverable error object -----------------===//
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
// This implements the parts of the standard Error protocol type which are
// shared between the ObjC-interoperable implementation and the native
// implementation. The parts specific to each implementation can be found in
// ErrorObject.mm (for the ObjC-interoperable parts) and ErrorObjectNative.cpp.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/Config.h"
#include "ErrorObject.h"

using namespace swift;

// The list of will-throw callbacks.
static ConcurrentList<std::pair<WillThrowCallback, HeapObject *>>
  WillThrowCallbacks;

/// Register a callback to be called whenever an error is thrown.
/// Implementation of _addErrorWillThrowCallback Swift API.
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_SPI
void _swift_addErrorWillThrowCallback(WillThrowCallback callback,
                                      HeapObject *context) {
  swift_retain(context);
  WillThrowCallbacks.push_front({ callback, context });
}

/// Breakpoint hook for debuggers, and invokes any throw callbacks that have
/// been added.
SWIFT_CC(swift) void
swift::swift_willThrow(SWIFT_CONTEXT void *unused,
                       SWIFT_ERROR_RESULT SwiftError **error) {
  // Cheap check to bail out early, since we expect there to be no callbacks
  // the vast majority of the time.
  if (SWIFT_LIKELY(WillThrowCallbacks.isEmpty()))
    return;

  for (auto &elem : WillThrowCallbacks) {
    auto callback = std::get<WillThrowCallback>(elem);
    auto context = std::get<HeapObject *>(elem);
    callback(*error, context);
  }
}
