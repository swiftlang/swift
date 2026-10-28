//===--- Error.h - Swift Runtime ABI for error values -----------*- C++ -*-===//
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
// Swift runtime support for working with error values.
//
// The ABI here is quite different in ObjC and non-ObjC modes.
// In ObjC mode, SwiftError is closely related to the NSError class:
// native errors are boxed as a subclass of NSError, but non-native
// errors may simply be NSError objects directly from Objective-C.
// In non-ObjC mode, SwiftError boxes are always native.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_ERROR_H
#define SWIFT_RUNTIME_ERROR_H

#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"

namespace swift {

struct SwiftError;

/// Allocate a catchable error object.
///
/// If value is nonnull, it should point to a value of \c type, which will be
/// copied (or taken if \c isTake is true) into the newly-allocated error box.
/// If value is null, the box's contents will be left uninitialized, and
/// \c isTake should be false.
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_API
BoxPair swift_allocError(const Metadata *type,
                         const WitnessTable *errorConformance,
                         OpaqueValue *value, bool isTake);

/// Deallocate an error object whose contained object has already been
/// destroyed.
SWIFT_RUNTIME_STDLIB_API
void swift_deallocError(SwiftError *error, const Metadata *type);

struct ErrorValueResult {
  const OpaqueValue *value;
  const Metadata *type;
  const WitnessTable *errorConformance;
};

/// Extract a pointer to the value, the type metadata, and the Error
/// protocol witness from an error object.
///
/// The "scratch" pointer should point to an uninitialized word-sized
/// temporary buffer. The implementation may write a reference to itself to
/// that buffer if the error object is a toll-free-bridged NSError instead of
/// a native Swift error, in which case the object itself is the "boxed" value.
SWIFT_RUNTIME_STDLIB_API
void swift_getErrorValue(const SwiftError *errorObject,
                         void **scratch,
                         ErrorValueResult *out);

/// Called when throwing an error.  Serves as a breakpoint hook
/// for debuggers.
SWIFT_CC(swift)
SWIFT_RUNTIME_STDLIB_API void
swift_willThrow(SWIFT_CONTEXT void *unused,
                SWIFT_ERROR_RESULT SwiftError **object);

/// A handler called by swift_willThrow when an untyped error is thrown.
using WillThrowHandler = void (*)(SwiftError *error);

/// A handler called by swift_willThrowTypedImpl when a typed error is thrown.
using WillThrowTypedHandler = void (*)(OpaqueValue *value,
                                       const Metadata *type,
                                       const WitnessTable *errorConformance);

/// Receives the handler that was installed for untyped throws before the
/// handler being installed by _swift_setWillThrowHandler.
using WillThrowOldHandlerCallback = void (*)(WillThrowHandler oldHandler);

/// Receives the handler that was installed for typed throws before the handler
/// being installed by _swift_setWillThrowTypedHandler.
using WillThrowTypedOldHandlerCallback =
    void (*)(WillThrowTypedHandler oldHandler);

/// Install \p handler as the handler for untyped throws, passing the handler it
/// replaces to \p saveOldHandler first.  Pass null for \p saveOldHandler when
/// the old handler is not wanted.
///
/// \p saveOldHandler runs before \p handler becomes reachable, so a handler that
/// chains to its predecessor can record it before a throw on another thread
/// reaches the chain.  It may be called more than once; the handler passed to the
/// last call is the one that was replaced.
SWIFT_CC(swift)
SWIFT_RUNTIME_STDLIB_SPI
void _swift_setWillThrowHandler(WillThrowHandler handler,
                                WillThrowOldHandlerCallback saveOldHandler);

/// Install \p handler as the handler for typed throws, passing the handler it
/// replaces to \p saveOldHandler first.  See _swift_setWillThrowHandler.
SWIFT_CC(swift)
SWIFT_RUNTIME_STDLIB_SPI
void _swift_setWillThrowTypedHandler(
    WillThrowTypedHandler handler,
    WillThrowTypedOldHandlerCallback saveOldHandler);

/// Called when throwing a typed error.  Serves as a breakpoint hook
/// for debuggers.
SWIFT_CC(swift)
SWIFT_RUNTIME_STDLIB_API void
swift_willThrowTypedImpl(OpaqueValue *value,
                         const Metadata *type,
                         const WitnessTable *errorConformance);

/// Called when an error is thrown out of the top level of a script.
SWIFT_CC(swift)
SWIFT_RUNTIME_STDLIB_API SWIFT_NORETURN void
swift_errorInMain(SwiftError *object);

/// Called when the try! operator fails.
SWIFT_CC(swift)
SWIFT_RUNTIME_STDLIB_API SWIFT_NORETURN void
swift_unexpectedError(SwiftError *object, OpaqueValue *filenameStart,
                      long filenameLength, bool isAscii, unsigned long line);

/// Retain an error box.
SWIFT_RUNTIME_STDLIB_API
SwiftError *swift_errorRetain(SwiftError *object);

/// Release an error box.
SWIFT_RUNTIME_STDLIB_API
void swift_errorRelease(SwiftError *object);

} // end namespace swift

#endif
