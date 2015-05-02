//===--- ErrorObject.h - Cocoa-interoperable recoverable error object -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This implements the object representation of the standard ErrorType protocol
// type, which represents recoverable errors in the language. This
// implementation is designed to interoperate efficiently with Cocoa libraries
// by:
// - allowing for NSError and CFError objects to "toll-free bridge" to
//   ErrorType existentials, which allows for cheap Cocoa to Swift interop
// - allowing a native Swift error to lazily "become" an NSError when
//   passed into Cocoa, allowing for cheap Swift to Cocoa interop
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_RUNTIME_ERROROBJECT_H__
#define __SWIFT_RUNTIME_ERROROBJECT_H__

#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/HeapObject.h"
#include <atomic>
#if SWIFT_OBJC_INTEROP
# include <CoreFoundation/CoreFoundation.h>
# include <CoreFoundation/CFRuntime.h>
# include <objc/objc.h>
#endif

namespace swift {

#if SWIFT_OBJC_INTEROP

/// When ObjC interop is enabled, SwiftError uses an NSError-layout-compatible
/// header.
struct SwiftErrorHeader {
  // CFError has a CF refcounting header. NSError reserves a word after the
  // 'isa' in order to be layout-compatible.
  CFRuntimeBase base;
  // The NSError part of the object is lazily initialized, so we need atomic
  // semantics.
  std::atomic<CFIndex> code;
  std::atomic<CFStringRef> domain;
  std::atomic<CFDictionaryRef> userInfo;
};

static_assert(sizeof(CFRuntimeBase) == sizeof(void*) * 2,
              "size of CFRuntimeBase changed");

#else

/// When ObjC interop is disabled, SwiftError uses a normal Swift heap object
/// header.
using SwiftErrorHeader = HeapObject;

#endif

/// The layout of the Swift ErrorType box.
struct SwiftError : SwiftErrorHeader {
  // By inheriting OpaqueNSError, the SwiftError structure reserves enough
  // space within itself to lazily emplace an NSError instance, and gets
  // Core Foundation's refcounting scheme.

  /// The type of Swift error value contained in the box.
  /// This is only available for native Swift errors.
  const Metadata *type;
  /// The ErrorType witness table.
  /// This is only available for native Swift errors.
  const WitnessTable *errorConformance;
  
  /// Get a pointer to the value contained inside the indirectly-referenced
  /// box reference.
  static const OpaqueValue *getIndirectValue(const SwiftError * const *ptr) {
    // If the box is a bridged NSError, then the box's address is itself the
    // value.
    if ((*ptr)->isPureNSError())
      return reinterpret_cast<const OpaqueValue *>(ptr);
    return (*ptr)->getValue();
  }
  static OpaqueValue *getIndirectValue(SwiftError * const *ptr) {
    return const_cast<OpaqueValue *>(getIndirectValue(
                                  const_cast<const SwiftError * const *>(ptr)));
  }
  
  /// Get a pointer to the value, which is tail-allocated after
  /// the fixed header.
  const OpaqueValue *getValue() const {
    // If the box is a bridged NSError, then the box's address is itself the
    // value. We can't provide an address for that; getIndirectValue must be
    // used if we haven't established this as an NSError yet..
    assert(!isPureNSError());
  
    auto baseAddr = reinterpret_cast<uintptr_t>(this + 1);
    // Round up to the value's alignment.
    unsigned alignMask = type->getValueWitnesses()->getAlignmentMask();
    baseAddr = (baseAddr + alignMask) & ~(uintptr_t)alignMask;
    return reinterpret_cast<const OpaqueValue *>(baseAddr);
  }
  OpaqueValue *getValue() {
    return const_cast<OpaqueValue*>(
             const_cast<const SwiftError *>(this)->getValue());
  }
  
#if SWIFT_OBJC_INTEROP
  // True if the object is really an NSError or CFError instance.
  // The type and errorConformance fields don't exist in an NSError.
  bool isPureNSError() const;
#else
  bool isPureNSError() const { return false; }
#endif
  
#if SWIFT_OBJC_INTEROP
  /// Get the type of the contained value.
  const Metadata *getType() const;
  /// Get the ErrorType protocol witness table for the contained type.
  const WitnessTable *getErrorConformance() const;
#else
  /// Get the type of the contained value.
  const Metadata *getType() const { return type; }
  /// Get the ErrorType protocol witness table for the contained type.
  const WitnessTable *getErrorConformance() const { return errorConformance; }
#endif
  
  // Don't copy or move, please.
  SwiftError(const SwiftError &) = delete;
  SwiftError(SwiftError &&) = delete;
  SwiftError &operator=(const SwiftError &) = delete;
  SwiftError &operator=(SwiftError &&) = delete;
};
  
/// Allocate a catchable error object.
extern "C" BoxPair::Return swift_allocError(const Metadata *type,
                                         const WitnessTable *errorConformance);
  
/// Deallocate an error object whose contained object has already been
/// destroyed.
extern "C" void swift_deallocError(SwiftError *error, const Metadata *type);

struct ErrorValueResult {
  const OpaqueValue *value;
  const Metadata *type;
  const WitnessTable *errorConformance;
};

/// Extract a pointer to the value, the type metadata, and the ErrorType
/// protocol witness from an error object.
///
/// The "scratch" pointer should point to an uninitialized word-sized
/// temporary buffer. The implementation may write a reference to itself to
/// that buffer if the error object is a toll-free-bridged NSError instead of
/// a native Swift error, in which case the object itself is the "boxed" value.
extern "C" void swift_getErrorValue(const SwiftError *errorObject,
                                    void **scratch,
                                    ErrorValueResult *out);

/// Retain and release SwiftError boxes.
extern "C" SwiftError *swift_errorRetain(SwiftError *object);
extern "C" void swift_errorRelease(SwiftError *object);
extern "C" void swift_willThrow(SwiftError *object);
extern "C" void swift_unexpectedError(SwiftError *object)
  __attribute__((noreturn));

#if SWIFT_OBJC_INTEROP

/// Initialize an ErrorType box to make it usable as an NSError instance.
extern "C" id swift_bridgeErrorTypeToNSError(SwiftError *errorObject);

/// Convert an (optional) NSError instance to a (non-optional)
/// ErrorType box.
extern "C" SwiftError *swift_convertNSErrorToErrorType(id errorObject);

/// Convert a (non-optional) ErrorType box to a (non-optional)
/// NSError instance.
extern "C" id swift_convertErrorTypeToNSError(SwiftError *errorObject);

/// Attempt to dynamically cast an NSError instance to a Swift ErrorType
/// implementation using the _ObjectiveCBridgeableErrorType protocol.
///
/// srcType must be some kind of class metadata.
bool tryDynamicCastNSErrorToValue(OpaqueValue *dest,
                                  OpaqueValue *src,
                                  const Metadata *srcType,
                                  const Metadata *destType,
                                  DynamicCastFlags flags);

/// Get the NSError Objective-C class.
Class getNSErrorClass();

#endif

} // namespace swift

#endif
