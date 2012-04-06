//===--- Alloc.cpp - Swift Language ABI Allocation Support ----------------===//
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
// Allocation ABI Shims While the Language is Bootstrapped
//
//===----------------------------------------------------------------------===//

#include "Alloc.h"
#include <stdlib.h>
#include <unistd.h>

struct SwiftHeapObject *
swift_alloc(struct SwiftHeapMetadata *metadata,
            size_t requiredSize,
            size_t requiredAlignment)
{
  size_t mask = requiredAlignment - 1;
  struct SwiftHeapMetadata **object;
  for (;;) {
    object = reinterpret_cast<struct SwiftHeapMetadata **>(
      calloc(1, (requiredSize + mask) & ~mask));
    if (object) {
      break;
    }
    sleep(1); // XXX FIXME -- Enqueue this thread and resume after free()
  }
  *object = metadata;
  return reinterpret_cast<struct SwiftHeapObject *>(object);
}

struct SwiftHeapObject *
swift_retain(struct SwiftHeapObject *object)
{
  if (!object) {
    return NULL;
  }
  ++object->runtimePrivateData;
  return object;
}

void
swift_release(struct SwiftHeapObject *object)
{
  if (!object) {
    return;
  }
  if (--object->runtimePrivateData > 0) {
    return;
  }
  size_t allocSize = object->metadata->destroy(object);
  if (allocSize) {
    swift_dealloc(object, allocSize);
  }
}

void
swift_dealloc(struct SwiftHeapObject *object, size_t allocatedSize)
{
  free(object);
}
