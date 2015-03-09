//===--- HeapObject.h -----------------------------------------------------===//
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
#ifndef SWIFT_STDLIB_SHIMS_HEAPOBJECT_H
#define SWIFT_STDLIB_SHIMS_HEAPOBJECT_H

#include "RefCount.h"

#ifdef __cplusplus
#include <type_traits>
#include "swift/Basic/type_traits.h"

namespace swift {
#endif 

struct HeapMetadata;

// The members of the HeapObject header that are not shared by a
// standard Objective-C instance
#define SWIFT_HEAPOBJECT_NON_OBJC_MEMBERS       \
  StrongRefCount refCount;                      \
  WeakRefCount weakRefCount

/// The Swift heap-object header.
struct HeapObject {
  /// This is always a valid pointer to a metadata object.
  struct HeapMetadata const *metadata;

  SWIFT_HEAPOBJECT_NON_OBJC_MEMBERS;
  // FIXME: allocate two words of metadata on 32-bit platforms

#ifdef __cplusplus
  HeapObject() = default;

  // Initialize a HeapObject header as appropriate for a newly-allocated object.
  constexpr HeapObject(HeapMetadata const *newMetadata) 
    : metadata(newMetadata)
    , refCount(StrongRefCount::Initialized)
    , weakRefCount(WeakRefCount::Initialized)
  { }
#endif
};

#ifdef __cplusplus
static_assert(swift::IsTriviallyConstructible<HeapObject>::value,
              "HeapObject must be trivially initializable");
static_assert(std::is_trivially_destructible<HeapObject>::value,
              "HeapObject must be trivially destructible");

}
#endif 

#endif
