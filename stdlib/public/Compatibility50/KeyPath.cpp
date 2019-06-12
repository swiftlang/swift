
//===--- Overrides.cpp - Compat override table for Swift 5.0 runtime ------===//
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
//  This file provides support for compatibility overrides for key path
//  instantiation.
//
//===----------------------------------------------------------------------===//

#include <cstdint>
#include "swift/ABI/Metadata.h"
#include "swift/Runtime/HeapObject.h"

using namespace swift;

/// Allocate an instance of a KeyPath class.
///
/// AnyKeyPath and its subclasses are normally resilient classes, but we can hardcode the layout
/// that the class had in the Swift 5.0 runtime here.
__attribute__((visibility("hidden")))
SWIFT_CC(swift)
extern "C" HeapObject *
swift50override_keyPath_create(intptr_t capacityInBytes,
  SWIFT_CC(swift) void (*initializer)(void *bufferStart, void *bufferEnd,
                                      SWIFT_CONTEXT void *context),
  void *initializerContext,
  SWIFT_CONTEXT const ClassMetadata *Self
) {
  // Instance size is 3*sizeof(void*) + capacityInBytes
  //   (2 for object header, 1 for KVC compatibility key)
  auto obj =
    swift_allocObject(Self, 3*sizeof(void*) + capacityInBytes,
                      alignof(void *) - 1);
  
  auto kvcPtr = (void**)((char*)obj + 2 * sizeof(void*));
  *kvcPtr = nullptr;
  
  auto tailElems = (char*)obj + 3 * sizeof(void*);
  
  initializer(tailElems, tailElems + capacityInBytes, initializerContext);
  
  return obj;
}
