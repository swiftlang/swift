//===--- HeapObject.h -------------------------------------------*- C++ -*-===//
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
#ifndef SWIFT_STDLIB_SHIMS_HEAPOBJECT_H
#define SWIFT_STDLIB_SHIMS_HEAPOBJECT_H

#include "RefCount.h"

#ifdef __cplusplus
#include <type_traits>
#include "swift/Basic/type_traits.h"

namespace swift {

struct InProcess;

template <typename Target> struct TargetHeapMetadata;
using HeapMetadata = TargetHeapMetadata<InProcess>;
#else
typedef struct HeapMetadata HeapMetadata;
#endif

// The members of the HeapObject header that are not shared by a
// standard Objective-C instance
#define SWIFT_HEAPOBJECT_NON_OBJC_MEMBERS       \
  InlineRefCounts refCounts

/// The Swift heap-object header.
/// This must match RefCountedStructTy in IRGen.
struct HeapObject {
  /// This is always a valid pointer to a metadata object.
  HeapMetadata const *metadata;

  SWIFT_HEAPOBJECT_NON_OBJC_MEMBERS;

#ifdef __cplusplus
  HeapObject() = default;

  // Initialize a HeapObject header as appropriate for a newly-allocated object.
  constexpr HeapObject(HeapMetadata const *newMetadata) 
    : metadata(newMetadata)
    , refCounts(InlineRefCounts::Initialized)
  { }
#endif
};

#ifdef __cplusplus
static_assert(swift::IsTriviallyConstructible<HeapObject>::value,
              "HeapObject must be trivially initializable");
static_assert(std::is_trivially_destructible<HeapObject>::value,
              "HeapObject must be trivially destructible");
static_assert(sizeof(HeapObject) == 2*sizeof(void*),
              "HeapObject must be two pointers long");
static_assert(alignof(HeapObject) == alignof(void*),
              "HeapObject must be pointer-aligned");

} // end namespace swift
#endif

#endif
