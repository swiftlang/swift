//===--- ClassAllocator.cpp - Per-class allocators ------------------------===//
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
// Freeing instances through the class allocator witness of their class's
// value witness table (see swift/Runtime/ClassAllocator.h).
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/ClassAllocator.h"
#include "swift/Runtime/Debug.h"

using namespace swift;

void class_allocator::deallocate(HeapObject *object) {
  auto *cls = static_cast<const ClassMetadata *>(object->metadata);
  // The allocation begins the instance address point before the object.
  void *allocation =
      reinterpret_cast<char *>(object) - cls->getInstanceAddressPoint();
  callDeallocateWitness(cls, allocation, cls->getInstanceSize(),
                        cls->getInstanceAlignMask());
}

void class_allocator::fatalAllocationWithoutClassAllocator(
    const HeapMetadata *metadata, const char *operation) {
  auto className = swift_getTypeName(metadata, /*qualified=*/true);
  swift::fatalError(/*flags=*/0,
                    "%s cannot allocate an instance of class %.*s, which has "
                    "a class allocator\n",
                    operation, (int)className.length, className.data);
}
