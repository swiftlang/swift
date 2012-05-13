//===--- Alloc.h - Swift Language Allocation ABI --------------------------===//
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
// Swift Allocation ABI
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_ALLOC_H
#define SWIFT_ABI_ALLOC_H

#include <cstddef>
#include <cstdint>

#ifdef __cplusplus
extern "C" {
#endif

/// The Swift heap-object header.
struct SwiftHeapObject {
  /// This is always a valid pointer to a metadata object.
  struct SwiftHeapMetadata *metadata;

  uint32_t refCount;
  /// The compiler assumes one "word" of runtime metadata
#ifdef __LP64__
  uint32_t runtimePrivateData;
#endif
};

/// The basic layout of a metadata object for a Swift heap object.
struct SwiftHeapMetadata {
  /// Returns the allocated size of the object, or 0 if the object
  /// shouldn't be deallocated.
  size_t (*destroy)(struct SwiftHeapObject *);

  /// Returns the allocated size of the object.
  size_t (*getSize)(struct SwiftHeapObject *);
};

/// Allocates a new heap object.  The returned memory may be
/// uninitialized outside of the heap-object header.  The object
/// has an initial retain count of 1, and its metadata is set to
/// the given value.
///
/// At some point "soon after return", it will become an
/// invariant that metadata->getSize(returnValue) will equal
/// requiredSize.
///
/// Either aborts or throws a swift exception if the allocation fails.
///
/// \param requiredSize - the required size of the allocation,
///   including the header
/// \param requiredAlignment - the required alignment of the allocation;
///   always a power of 2 no less than alignof(void*)
/// \return never null
///
/// POSSIBILITIES: The argument order is fair game.  It may be useful
/// to have a variant which guarantees zero-initialized memory.
struct SwiftHeapObject *swift_alloc(struct SwiftHeapMetadata *metadata,
                                    size_t requiredSize,
                                    size_t requiredAlignment);

/// Atomically increments the retain count of an object.
///
/// \param object - may be null, in which case this is a no-op
/// \return its argument value exactly
///
/// POSSIBILITIES: We may end up wanting a bunch of different variants:
///  - the general version which correctly handles null values, swift
///     objects, and ObjC objects
///    - a variant that assumes that its operand is a swift object
///      - a variant that can safely use non-atomic operations
///      - maybe a variant that can assume a non-null object
/// It may also prove worthwhile to have this use a custom CC
/// which preserves a larger set of registers.
struct SwiftHeapObject *swift_retain(struct SwiftHeapObject *object);

/// Atomically decrements the retain count of an object.  If the
/// retain count reaches zero, the object is destroyed as follows:
///
///   size_t allocSize = object->metadata->destroy(object);
///   if (allocSize) swift_dealloc(object, allocSize);
///
/// \param object - may be null, in which case this is a no-op
///
/// POSSIBILITIES: We may end up wanting a bunch of different variants:
///  - the general version which correctly handles null values, swift
///     objects, and ObjC objects
///    - a variant that assumes that its operand is a swift object
///      - a variant that can safely use non-atomic operations
///      - maybe a variant that can assume a non-null object
/// It's unlikely that a custom CC would be beneficial here.
void swift_release(struct SwiftHeapObject *object);

/// Deallocate the given memory; it was returned by swift_alloc
/// but is otherwise in an unknown state.
///
/// \param object - never null
/// \param allocatedSize - the allocated size of the object from the
///   program's perspective, i.e. the value 
///
/// POSSIBILITIES: It may be useful to have a variant which
/// requires the object to have been fully zeroed from offsets
/// sizeof(SwiftHeapObject) to allocatedSize.
void swift_dealloc(struct SwiftHeapObject *object, size_t allocatedSize);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* SWIFT_ABI_ALLOC_H */
