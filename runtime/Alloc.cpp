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
#include <llvm/Support/MathExtras.h>
#include <cstdlib>
#include <unistd.h>

struct SwiftHeapObject *
swift_alloc(struct SwiftHeapMetadata *metadata,
            size_t requiredSize,
            size_t requiredAlignment)
{
  struct SwiftHeapObject *object;
  for (;;) {
    object = reinterpret_cast<struct SwiftHeapObject *>(
      calloc(1, llvm::RoundUpToAlignment(requiredSize, requiredAlignment)));
    if (object) {
      break;
    }
    sleep(1); // XXX FIXME -- Enqueue this thread and resume after free()
  }
  object->metadata = metadata;
  object->refCount = 1;
  return object;
}

struct SwiftHeapObject *
swift_retain(struct SwiftHeapObject *object)
{
  if (object) {
    ++object->refCount;
  }
  return object;
}

static void
_swift_release_slow(struct SwiftHeapObject *object)
  __attribute__((noinline,used));

void
swift_release(struct SwiftHeapObject *object)
{
  if (object && (--object->refCount == 0)) {
    _swift_release_slow(object);
  }
}

void
_swift_release_slow(struct SwiftHeapObject *object)
{
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
