//===--- ClassAllocator.h - Per-class custom allocators ---------*- C++ -*-===//
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
// Runtime support for classes declared with `@allocator(A)`. Instances of
// such a class, and of its subclasses, are allocated by `A.allocate` and freed
// by `A.deallocate`.
//
// The compiler sets ClassFlags::HasCustomAllocator in the metadata of every
// class with an allocator (its own or inherited), and records the allocator
// in the context descriptor of each class that declares one
// (TargetClassAllocatorDescriptor). It calls the entry points that look for
// an allocator (swift_allocObjectMaybeClassAllocated,
// swift_deallocClassInstanceMaybeClassAllocated) only for classes that may
// have one, so swift_allocObject and swift_deallocClassInstance never look at
// the metadata. The other paths that may free such an object -- the last
// unowned release, failed initializers, Objective-C allocation and
// deallocation -- test the flag.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_CLASSALLOCATOR_H
#define SWIFT_RUNTIME_CLASSALLOCATOR_H

#include "swift/Runtime/Config.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include <cstddef>

namespace swift {
namespace class_allocator {

/// True if objects with the given metadata are allocated by a class
/// allocator. Boxes, closure contexts and other non-class heap objects never
/// are, nor are instances of Objective-C classes.
SWIFT_ALWAYS_INLINE
static inline bool hasClassAllocator(const HeapMetadata *metadata) {
  if (!metadata->isClassObject())
    return false;
  auto *cls = static_cast<const ClassMetadata *>(metadata);
  return cls->isTypeMetadata() &&
         (cls->getFlags() & ClassFlags::HasCustomAllocator);
}

/// Allocate memory for an instance of \p cls, which has
/// ClassFlags::HasCustomAllocator set, from its class allocator. Returns the
/// base of the allocation; the object goes at the instance address point.
/// Returns null if no class in its superclass chain declares an allocator after
/// all (a resilient superclass may have dropped its `@allocator`); the caller
/// then uses the default allocator.
SWIFT_RUNTIME_LIBRARY_VISIBILITY
void *allocate(const ClassMetadata *cls, size_t size, size_t alignMask);

/// Return the allocation of \p object, whose class has
/// ClassFlags::HasCustomAllocator set, to its class allocator. The object's
/// header must still be intact.
SWIFT_RUNTIME_LIBRARY_VISIBILITY
void deallocate(HeapObject *object, size_t size, size_t alignMask);

} // namespace class_allocator
} // namespace swift

#endif
