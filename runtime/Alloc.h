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

typedef unsigned long SwiftAllocIndex;

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
struct SwiftHeapObject *
swift_allocObject(struct SwiftHeapMetadata *metadata,
            size_t requiredSize, size_t requiredAlignment);

// Allocate plain old memory, this is the generalized entry point
//
// The default API will wait for available memory and return zero filled.
//
// The "try" flag tells the runtime to not wait for memory
// The "raw" flag allocates uninitialized memory.
// When neither flag is needed, pass zero.
//
// If alignment is needed, then please round up to the desired alignment.
// For example, a 12 byte allocation with 8 byte alignment becomes 16.
#define SWIFT_TRYALLOC 0x0001
#define SWIFT_RAWALLOC 0x0002
void *
swift_slowAlloc(size_t bytes, uint64_t flags);

// These exist as fast entry points for the above slow API.
//
// When the compiler knows that the bytes to be allocated are constant and the
// value is <= 4KB then the compiler precomputes an offset that the runtime uses
// to quickly allocate/free from a per-thread cache.
//
// The algorithm is like so:
//
// if (!__builtin_constant_p(bytes) || (bytes > 0x1000)) {
//   return swift_slowAlloc(bytes, 0);
// }
// if (bytes == 0) {
//   tinyIndex = 0;
// } else {
//   --bytes;
//   if (bytes < 0x80) {
//     _slot = (bytes >> 3);
//   } else if (bytes < 0x100) {
//     _slot = (bytes >> 4) + 0x8;
//   } else if (bytes < 0x200) {
//     _slot = (bytes >> 5) + 0x10;
//   } else if (bytes < 0x400) {
//     _slot = (bytes >> 6) + 0x18;
//   } else if (bytes < 0x800) {
//     _slot = (bytes >> 7) + 0x20;
//   } else if (bytes < 0x1000) {
//     _slot = (bytes >> 8) + 0x28;
//   } else {
//     __builtin_trap();
//   }
// }
// swift_alloc(tinyIndex);
//
// XXX FIXME -- adjust the algorithm to allow for 32-bit machines to use word
// sized allocations on the small end of the tiny API.
void *
swift_alloc(SwiftAllocIndex idx);
void *
swift_rawAlloc(SwiftAllocIndex idx);
void *
swift_tryAlloc(SwiftAllocIndex idx);
void *
swift_tryRawAlloc(SwiftAllocIndex idx);


// Plain old memory deallocation
//
// Like swift allocation tiny index trick, but for deallocation
// If bytes is knowable and fits within the tinyIndex rule:
void
swift_dealloc(void *ptr, SwiftAllocIndex idx);
// If bytes is knowable but is large OR if bytes is not knowable,
// then use the slow entry point and pass zero:
void
swift_slowDealloc(void *ptr, size_t bytes);

// If the caller cannot promise to zero the object during destruction,
// then call these corresponding APIs:
void
swift_rawDealloc(void *ptr, SwiftAllocIndex idx);
void
swift_slowRawDealloc(void *ptr, size_t bytes);

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
struct SwiftHeapObject *
swift_retain(struct SwiftHeapObject *object);

/// Atomically decrements the retain count of an object.  If the
/// retain count reaches zero, the object is destroyed as follows:
///
///   size_t allocSize = object->metadata->destroy(object);
///   if (allocSize) swift_deallocObject(object, allocSize);
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
void
swift_release(struct SwiftHeapObject *object);

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
void
swift_deallocObject(struct SwiftHeapObject *object, size_t allocatedSize);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* SWIFT_ABI_ALLOC_H */
