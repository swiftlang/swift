//===--- ErrorObjectCommon.cpp - Recoverable error object -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
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
#include "ErrorObjectTestSupport.h"

#if SWIFT_OBJC_INTEROP
#include "swift/Runtime/ObjCBridge.h"
#include <objc/runtime.h>
#endif

using namespace swift;

#if SWIFT_OBJC_INTEROP
static char backtraceKey;
#endif

OpaqueValue *SwiftError::copyBacktrace() const {
#if SWIFT_OBJC_INTEROP
  if (isPureNSError()) {
    auto nsError = reinterpret_cast<id>(const_cast<SwiftError *>(this));
    id value = objc_getAssociatedObject(nsError, &backtraceKey);
    return reinterpret_cast<OpaqueValue *>(swift_unknownObjectRetain(value));
  }
#endif

  return reinterpret_cast<OpaqueValue *>(swift_unknownObjectRetain(backtrace));
}

void SwiftError::setBacktrace(OpaqueValue *value, bool overwrite) {
#if SWIFT_OBJC_INTEROP
  if (isPureNSError()) {
    auto nsError = reinterpret_cast<id>(this);
    if (!overwrite && objc_getAssociatedObject(nsError, &backtraceKey)) {
      // Already set, but don't want to overwrite.
    } else {
      objc_setAssociatedObject(nsError,
                               &backtraceKey,
                               reinterpret_cast<id>(value),
                               OBJC_ASSOCIATION_RETAIN);
    }

    return;
  }
#endif

  if (overwrite) {
    value = reinterpret_cast<OpaqueValue *>(swift_unknownObjectRetain(value));
    if (auto oldValue = backtrace.exchange(value)) {
      swift_unknownObjectRelease(oldValue);
    }
  } else {
    OpaqueValue *expected = nullptr;
    if (backtrace.compare_exchange_strong(expected, value)) {
      swift_unknownObjectRetain(value);
    }
  }
}

void
swift::swift_deallocError(SwiftError *error, const Metadata *type) {
  if (!error->isPureNSError()) {
    OpaqueValue *backtrace = error->backtrace;
    if (SWIFT_UNLIKELY(backtrace)) {
      swift_unknownObjectRelease(backtrace);
    }
  }
#if SWIFT_OBJC_INTEROP
  object_dispose((id)error);
#else
  auto sizeAndAlign = _getErrorAllocatedSizeAndAlignmentMask(type);
  swift_deallocUninitializedObject(error, sizeAndAlign.first, sizeAndAlign.second);
#endif
}

void (*swift::_swift_willThrow)(SwiftError *error);

/// Breakpoint hook for debuggers, and calls _swift_willThrow if set.
SWIFT_CC(swift) void
swift::swift_willThrow(SWIFT_CONTEXT void *unused,
                       SWIFT_ERROR_RESULT SwiftError **error) {
  // Cheap check to bail out early, since we expect there to be no callbacks
  // the vast majority of the time.
  if (SWIFT_LIKELY(!_swift_willThrow))
    return;
  _swift_willThrow(*error);
}

SWIFT_CC(swift) OpaqueValue *
swift::swift_errorCopyBacktrace(SwiftError *object) {
    return object->copyBacktrace();
}

SWIFT_CC(swift) void
swift::swift_errorSetBacktrace(SwiftError *object, OpaqueValue *value,
                               bool overwrite) {
    object->setBacktrace(value, overwrite);
}
