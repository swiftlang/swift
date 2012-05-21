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
// We'll include this and do per-thread clean up once we actually have threads
//#include <System/pthread_machdep.h>
#include <cstdlib>
#include <unistd.h>

struct AllocCacheEntry {
  struct AllocCacheEntry *next;
};

// XXX FIXME -- we need to clean this up when the project isn't a secret.
// There are only 256 slots, and the latter half is basically unused. We can
// go lower than 128, but we eventually begin to stomp on other frameworks.
static __attribute__((address_space(256)))
struct TSD {
  uintptr_t junk[128];
  AllocCacheEntry *cache[64];
  AllocCacheEntry *rawCache[64];
} *tsd = 0;

struct SwiftHeapObject *
swift_allocObject(struct SwiftHeapMetadata *metadata,
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
    swift_deallocObject(object, allocSize);
  }
}

void
swift_deallocObject(struct SwiftHeapObject *object, size_t allocatedSize)
{
  free(object);
}


// Plain old memory allocation

__attribute__((noinline,used))
static void *
_swift_slowAlloc_fixup(size_t off, uint64_t flags)
{
  size_t sz;

  off++;

  if (off <= 16) {
    sz = off << 3;
  } else if (off <= 24) {
    sz = (off - 8) << 4;
  } else if (off <= 32) {
    sz = (off - 16) << 5;
  } else if (off <= 40) {
    sz = (off - 24) << 6;
  } else if (off <= 48) {
    sz = (off - 32) << 7;
  } else if (off <= 56) {
    sz = (off - 40) << 8;
  } else {
    __builtin_trap();
  }

  return swift_slowAlloc(sz, flags);
}



void *
swift_slowAlloc(size_t bytes, uint64_t flags)
{
  void *r;

  do {
    if (flags & SWIFT_RAWALLOC) {
      r = malloc(bytes);
    } else {
      r = calloc(1, bytes);
    }
  } while (!r && !(flags & SWIFT_TRYALLOC));

  return r;
}

void *
swift_alloc(size_t idx)
{
  AllocCacheEntry *r = tsd->cache[idx];
  if (r) {
    tsd->cache[idx] = r->next;
    return r;
  }
  return _swift_slowAlloc_fixup(idx, 0);
}

void *
swift_rawAlloc(size_t idx)
{
  AllocCacheEntry *r = tsd->rawCache[idx];
  if (r) {
    tsd->rawCache[idx] = r->next;
    return r;
  }
  return _swift_slowAlloc_fixup(idx, SWIFT_RAWALLOC);
}

void *
swift_tryAlloc(size_t idx)
{
  AllocCacheEntry *r = tsd->cache[idx];
  if (r) {
    tsd->cache[idx] = r->next;
    return r;
  }
  return _swift_slowAlloc_fixup(idx, SWIFT_TRYALLOC);
}

void *
swift_tryRawAlloc(size_t idx)
{
  AllocCacheEntry *r = tsd->rawCache[idx];
  if (r) {
    tsd->rawCache[idx] = r->next;
    return r;
  }
  return _swift_slowAlloc_fixup(idx, SWIFT_TRYALLOC|SWIFT_RAWALLOC);
}

void
swift_dealloc(void *ptr, size_t idx)
{
  auto cur = static_cast<AllocCacheEntry *>(ptr);
  AllocCacheEntry *prev = tsd->cache[idx];
  cur->next = prev;
  tsd->cache[idx] = cur;
}

void
swift_rawDealloc(void *ptr, size_t idx)
{
  auto cur = static_cast<AllocCacheEntry *>(ptr);
  AllocCacheEntry *prev = tsd->rawCache[idx];
  cur->next = prev;
  tsd->rawCache[idx] = cur;
}

void
swift_slowDealloc(void *ptr, size_t bytes)
{
  size_t idx;

  bytes--;

  if (bytes == SIZE_MAX) {
    // bytes was zero, therefore bucket 0
    idx = 0;
  } else if (bytes < 0x80) {
    // about 90% of allocations
    idx = (bytes >> 3);
  } else if (bytes < 0x100) {
    idx = (bytes >> 4) + 0x8;
  } else if (bytes < 0x200) {
    idx = (bytes >> 5) + 0x10;
  } else if (bytes < 0x400) {
    idx = (bytes >> 6) + 0x18;
  } else if (bytes < 0x800) {
    idx = (bytes >> 7) + 0x20;
  } else if (bytes < 0x1000) {
    idx = (bytes >> 8) + 0x28;
  } else {
    // about 1% of allocations
    return free(ptr);
  }

  swift_dealloc(ptr, idx);
}

void
swift_slowRawDealloc(void *ptr, size_t bytes)
{
  size_t idx;

  bytes--;

  if (bytes == SIZE_MAX) {
    // bytes was zero, therefore bucket 0
    idx = 0;
  } else if (bytes < 0x80) {
    // about 90% of allocations
    idx = (bytes >> 3);
  } else if (bytes < 0x100) {
    idx = (bytes >> 4) + 0x8;
  } else if (bytes < 0x200) {
    idx = (bytes >> 5) + 0x10;
  } else if (bytes < 0x400) {
    idx = (bytes >> 6) + 0x18;
  } else if (bytes < 0x800) {
    idx = (bytes >> 7) + 0x20;
  } else if (bytes < 0x1000) {
    idx = (bytes >> 8) + 0x28;
  } else {
    // about 1% of allocations
    return free(ptr);
  }

  swift_rawDealloc(ptr, idx);
}
