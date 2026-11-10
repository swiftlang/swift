//===--- ClassAllocator.h - Per-class allocators ----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Runtime support for classes with a class allocator: instances of a class
// declared with `@allocator(A)` (or of one of its subclasses, or of a class
// that gets `A` from `-default-class-allocator`) are allocated by
// `A.allocate` and freed by `A.deallocate`.
//
// Compiled code allocates every such instance and frees it in its
// deallocating deinit, by calling `A` directly. The runtime never allocates
// an instance. It frees one in only two cases: when the last unowned
// reference to it is released after its deinit has run, and when its
// initializer fails (and the concurrency runtime frees an actor whose deinit
// leaves the free to it). It finds the allocator through the class's value
// witness table: a class with ClassFlags::HasClassAllocator has a
// ClassAllocatorValueWitnessTable, whose `deallocate` witness is or calls
// `A.deallocate`. The weak reference side tables of the class's instances
// come from the runtime's own allocator, as for any other class.
//
// The inline functions here are shared by the runtime and the concurrency
// runtime. The others are internal to the runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_CLASSALLOCATOR_H
#define SWIFT_RUNTIME_CLASSALLOCATOR_H

#include "swift/Runtime/Config.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include <cassert>
#include <cstddef>

namespace swift {
namespace class_allocator {

/// True if objects with the given metadata are instances of a class with a
/// custom instance allocator (ClassFlags::HasClassAllocator).
SWIFT_ALWAYS_INLINE
static inline bool hasInstanceAllocator(const HeapMetadata *metadata) {
  if (!metadata->isClassObject())
    return false;
  auto *cls = static_cast<const ClassMetadata *>(metadata);
  return cls->isTypeMetadata() &&
         (cls->getFlags() & ClassFlags::HasClassAllocator);
}

/// Return \p allocation, the memory of an instance of \p cls, a class with
/// ClassFlags::HasClassAllocator, to the class's instance allocator by
/// calling the `deallocate` witness of the class's value witness table.
/// \p size and \p alignMask are the instance size and alignment mask the
/// memory was allocated with. The witness takes the alignment, like
/// `ClassAllocator.deallocate(_:byteCount:alignment:for:)`, whose signature
/// it has.
SWIFT_ALWAYS_INLINE
static inline void callDeallocateWitness(const ClassMetadata *cls,
                                         void *allocation, size_t size,
                                         size_t alignMask) {
  assert(hasInstanceAllocator(cls));
  // A class with ClassFlags::HasClassAllocator has the class allocator
  // witness. Loading it authenticates it.
  auto *witnesses = static_cast<const ClassAllocatorValueWitnessTable *>(
      cls->getValueWitnesses());
  witnesses->deallocate(allocation, size, alignMask + 1, cls);
}

/// Return the memory of \p object, an instance of a class with
/// ClassFlags::HasClassAllocator, to the class's instance allocator. The
/// object's header must still be intact.
SWIFT_RUNTIME_LIBRARY_VISIBILITY
void deallocate(HeapObject *object);

/// Stop the program because \p operation tried to allocate an instance of
/// \p metadata, a class with an instance allocator, without it.
SWIFT_RUNTIME_LIBRARY_VISIBILITY SWIFT_NORETURN void
fatalAllocationWithoutClassAllocator(const HeapMetadata *metadata,
                                     const char *operation);

} // namespace class_allocator
} // namespace swift

#endif
