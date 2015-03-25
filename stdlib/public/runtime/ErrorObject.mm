//===--- ErrorObject.mm - Cocoa-interoperable recoverable error object ----===//
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

#include "ErrorObject.h"
#include <Foundation/Foundation.h>

using namespace swift;

// A dummy object we can use as a sentinel value to recognize SwiftErrors
// whose NSError bits haven't been initialized, in a way debuggers can easily
// recognize.
static const id UnbridgedSwiftError = @"<uninitialized Swift ErrorType>";

/// Allocate a catchable error object.
static BoxPair::Return
_swift_allocError_(const Metadata *type,
                   const WitnessTable *errorConformance) {
  static CFTypeID CFErrorID = CFErrorGetTypeID();
  
  // Determine the allocated space necessary to carry the value.
  // TODO: If the error type is a simple enum with no associated values, we
  // could emplace it in the "code" slot of the NSError and save ourselves
  // some work.
  
  unsigned size = type->getValueWitnesses()->getSize();
  unsigned alignMask = type->getValueWitnesses()->getAlignmentMask();

  size_t valueOffset = (sizeof(SwiftError) + alignMask) & ~alignMask;
  size_t totalSize = valueOffset + size;
  
  // Allocate the instance as if it were a CFError. We won't really initialize
  // the CFError parts until forced to though.
  auto instance = (SwiftError*)_CFRuntimeCreateInstance(kCFAllocatorDefault,
                                 CFErrorID, totalSize - sizeof(CFRuntimeBase),
                                 nullptr);

  // Initialize the Swift type metadata.
  instance->domain = (CFStringRef)UnbridgedSwiftError;
  instance->type = type;
  instance->errorConformance = errorConformance;
  
  // Return the SwiftError reference and a pointer to the uninitialized value
  // inside.
  auto valuePtr = reinterpret_cast<char*>(instance) + valueOffset;
  return BoxPair{reinterpret_cast<HeapObject*>(instance),
                 reinterpret_cast<OpaqueValue*>(valuePtr)};
}

extern "C" auto *_swift_allocError = _swift_allocError_;

BoxPair::Return
swift::swift_allocError(const Metadata *type,
                        const WitnessTable *errorConformance) {
  return _swift_allocError(type, errorConformance);
}

/// Deallocate an error object whose contained object has already been
/// destroyed.
static void
_swift_deallocError_(SwiftError *error,
                     const Metadata *type) {
  CFAllocatorDeallocate(kCFAllocatorDefault, error);
}

extern "C" auto *_swift_deallocError = _swift_deallocError_;

void
swift::swift_deallocError(SwiftError *error, const Metadata *type) {
  return _swift_deallocError(error, type);
}

static const WitnessTable *getNSErrorConformanceToErrorType() {
  // TODO
  assert(false && "not implemented");
}

const Metadata *SwiftError::getType() const {
  if (isPureNSError()) {
    auto asError = (NSError*)this;
    return swift_getObjCClassMetadata((ClassMetadata*)[asError class]);
  }
  return type;
}

const WitnessTable *SwiftError::getErrorConformance() const {
  if (isPureNSError()) {
    return getNSErrorConformanceToErrorType();
  }
  return errorConformance;
}

/// Extract a pointer to the value, the type metadata, and the ErrorType
/// protocol witness from an error object.
///
/// The "scratch" pointer should point to an uninitialized word-sized
/// temporary buffer. The implementation may write a reference to itself to
/// that buffer if the error object is a toll-free-bridged NSError instead of
/// a native Swift error, in which case the object itself is the "boxed" value.
static void
_swift_getErrorValue_(const SwiftError *errorObject,
                      void **scratch,
                      ErrorValueResult *out) {
  // TODO: Would be great if Clang had a return-three convention so we didn't
  // need the out parameter here.

  // Check for a bridged Cocoa NSError.
  if (errorObject->isPureNSError()) {
    // Return a pointer to the scratch buffer.
    auto asError = (NSError*)errorObject;
    
    *scratch = (void*)errorObject;
    out->value = (const OpaqueValue *)scratch;
    out->type = swift_getObjCClassMetadata((ClassMetadata*)[asError class]);

    out->errorConformance = getNSErrorConformanceToErrorType();
  }
  
  out->value = errorObject->getValue();
  out->type = errorObject->type;
  out->errorConformance = errorObject->errorConformance;
  return;
}

extern "C" auto *_swift_getErrorValue = _swift_getErrorValue_;

void
swift::swift_getErrorValue(const SwiftError *errorObject,
                           void **scratch,
                           ErrorValueResult *out) {
  return _swift_getErrorValue(errorObject, scratch, out);
}
