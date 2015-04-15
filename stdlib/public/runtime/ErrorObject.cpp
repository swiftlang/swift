//===--- ErrorObject.cpp - Recoverable error object -----------------------===//
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
// implementation is used when ObjC interop is disabled; the ObjC-interoperable
// version is implemented in ErrorObject.mm.
//
//===----------------------------------------------------------------------===//

#include "Debug.h"
#include "ErrorObject.h"
#include "Private.h"

#if !SWIFT_OBJC_INTEROP

using namespace swift;

/// Determine the size and alignment of an ErrorType box containing the given
/// type.
static std::pair<size_t, size_t>
_getErrorAllocatedSizeAndAlignmentMask(const Metadata *type) {
  // The value is tail-allocated after the SwiftError record with the
  // appropriate alignment.
  auto vw = type->getValueWitnesses();
  size_t size = sizeof(SwiftError);
  unsigned valueAlignMask = vw->getAlignmentMask();
  size = (size + valueAlignMask) & ~(size_t)valueAlignMask;
  size += vw->getSize();
  
  size_t alignMask = (alignof(SwiftError) - 1) | valueAlignMask;
  
  return {size, alignMask};
}

/// Destructor for an ErrorType box.
static void _destroyErrorObject(HeapObject *obj) {
  auto error = static_cast<SwiftError *>(obj);
  
  // Destroy the value inside.
  auto type = error->type;
  type->vw_destroy(error->getValue());
  
  // Deallocate the buffer.
  auto sizeAndAlign = _getErrorAllocatedSizeAndAlignmentMask(type);
  swift_deallocObject(obj, sizeAndAlign.first, sizeAndAlign.second);
}

/// Heap metadata for ErrorType boxes.
static const FullMetadata<HeapMetadata> ErrorTypeMetadata{
  HeapMetadataHeader{{_destroyErrorObject}, {&_TWVBo}},
  Metadata{MetadataKind::ErrorObject},
};

BoxPair::Return
swift::swift_allocError(const swift::Metadata *type,
                        const swift::WitnessTable *errorConformance) {
  auto sizeAndAlign = _getErrorAllocatedSizeAndAlignmentMask(type);
  
  auto allocated = swift_allocObject(&ErrorTypeMetadata,
                                     sizeAndAlign.first, sizeAndAlign.second);
  
  auto error = reinterpret_cast<SwiftError*>(allocated);
  
  error->type = type;
  error->errorConformance = errorConformance;
  
  return BoxPair{allocated, error->getValue()};
}

void
swift::swift_deallocError(SwiftError *error, const Metadata *type) {
  auto sizeAndAlign = _getErrorAllocatedSizeAndAlignmentMask(type);
  swift_deallocObject(error, sizeAndAlign.first, sizeAndAlign.second);
}

void
swift::swift_getErrorValue(const SwiftError *errorObject,
                           void **scratch,
                           ErrorValueResult *out) {
  out->value = errorObject->getValue();
  out->type = errorObject->type;
  out->errorConformance = errorObject->errorConformance;
}

SwiftError *
swift::swift_errorRetain(SwiftError *object) {
  return static_cast<SwiftError*>(swift_retain(object));
}

void
swift::swift_errorRelease(SwiftError *object) {
  swift_release(object);
}

#endif
