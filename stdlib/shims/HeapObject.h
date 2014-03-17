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

#include <stdint.h>

#ifdef __cplusplus
namespace swift {
#endif 

struct HeapMetadata;

// The members of the HeapObject header that are not shared by a
// standard Objective-C instance
#define SWIFT_HEAPOBJECT_NON_OBJC_MEMBERS       \
  uint32_t refCount;                            \
  uint32_t weakRefCount

/// The Swift heap-object header.
struct HeapObject {
  /// This is always a valid pointer to a metadata object.
  struct HeapMetadata const *metadata;

  SWIFT_HEAPOBJECT_NON_OBJC_MEMBERS;
  // FIXME: allocate two words of metadata on 32-bit platforms
};

#ifdef __cplusplus
}
#endif 

#endif
