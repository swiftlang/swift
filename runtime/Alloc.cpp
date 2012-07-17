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
//#ifdef __LP64__
//#define ALLOC_CACHE_BUCKETS 56
//#else
#define ALLOC_CACHE_BUCKETS 64
//#endif
static __attribute__((address_space(256)))
struct TSD {
  uintptr_t junk[128];
  AllocCacheEntry *cache[ALLOC_CACHE_BUCKETS];
  AllocCacheEntry *rawCache[ALLOC_CACHE_BUCKETS];
} *tsd = 0;

struct SwiftHeapObject *
swift_allocObject(struct SwiftHeapMetadata *metadata,
                  size_t requiredSize,
                  size_t requiredAlignment) {
  struct SwiftHeapObject *object;
  static_assert(offsetof(TSD, cache) == SWIFT_TSD_ALLOC_BASE, "Fix ASM");
  static_assert(offsetof(TSD, rawCache) == SWIFT_TSD_RAW_ALLOC_BASE, "Fix ASM");
  (void)tsd;
  for (;;) {
    object = reinterpret_cast<struct SwiftHeapObject *>(
      calloc(1, llvm::RoundUpToAlignment(requiredSize, requiredAlignment)));
    if (object) {
      break;
    }
    sleep(1); // XXX FIXME -- Enqueue this thread and resume after free()
  }
  object->metadata = metadata;
  object->refCount = RC_INTERVAL;
  return object;
}

struct SwiftHeapObject *
swift_allocClass(struct SwiftHeapMetadata *metadata,
                  size_t requiredSize,
                  size_t requiredAlignment) {
  return swift_allocObject(metadata, requiredSize, requiredAlignment);
}

extern "C" void
_swift_release_slow(struct SwiftHeapObject *object)
  __attribute__((noinline,used));

void
swift_retain_noresult(struct SwiftHeapObject *object) {
  swift_retain(object);
}

#ifndef __x86_64__
struct SwiftHeapObject *
swift_retain(struct SwiftHeapObject *object) {
  return _swift_retain(object);
}

void
swift_release(struct SwiftHeapObject *object) {
  if (object && ((object->refCount -= RC_INTERVAL) == 0)) {
    _swift_release_slow(object);
  }
}
#endif

void
_swift_release_slow(struct SwiftHeapObject *object) {
  size_t allocSize = object->metadata->destroy(object);
  if (allocSize) {
    swift_deallocObject(object, allocSize);
  }
}

void
swift_deallocObject(struct SwiftHeapObject *object, size_t allocatedSize) {
  free(object);
}


// Plain old memory allocation

__attribute__((noinline,used))
static void *
_swift_slowAlloc_fixup(SwiftAllocIndex idx, uint64_t flags)
{
  size_t sz;

  idx++;

  // we could do a table based lookup if we think it worthwhile
#ifdef __LP64__
  if        (idx <= 16) { sz =  idx       << 3;
  } else if (idx <= 24) { sz = (idx -  8) << 4;
  } else if (idx <= 32) { sz = (idx - 16) << 5;
  } else if (idx <= 40) { sz = (idx - 24) << 6;
  } else if (idx <= 48) { sz = (idx - 32) << 7;
  } else if (idx <= 56) { sz = (idx - 40) << 8;
#else
  if        (idx <= 16) { sz =  idx       << 2;
  } else if (idx <= 24) { sz = (idx -  8) << 3;
  } else if (idx <= 32) { sz = (idx - 16) << 4;
  } else if (idx <= 40) { sz = (idx - 24) << 5;
  } else if (idx <= 48) { sz = (idx - 32) << 6;
  } else if (idx <= 56) { sz = (idx - 40) << 7;
  } else if (idx <= 64) { sz = (idx - 48) << 8;
#endif
  } else {
    __builtin_trap();
  }

  return swift_slowAlloc(sz, flags);
}

extern "C" void
_swift_refillThreadAllocCache(SwiftAllocIndex idx, uint64_t flags) {
  void *tmp = _swift_slowAlloc_fixup(idx, flags);
  if (!tmp) {
    return;
  }
  if (flags & SWIFT_RAWALLOC) {
    swift_rawDealloc(tmp, idx);
  } else {
    swift_dealloc(tmp, idx);
  }
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

#ifndef __x86_64__
void *
swift_alloc(SwiftAllocIndex idx)
{
  AllocCacheEntry *r = tsd->cache[idx];
  if (r) {
    tsd->cache[idx] = r->next;
    return r;
  }
  return _swift_slowAlloc_fixup(idx, 0);
}

void *
swift_rawAlloc(SwiftAllocIndex idx)
{
  AllocCacheEntry *r = tsd->rawCache[idx];
  if (r) {
    tsd->rawCache[idx] = r->next;
    return r;
  }
  return _swift_slowAlloc_fixup(idx, SWIFT_RAWALLOC);
}

void *
swift_tryAlloc(SwiftAllocIndex idx)
{
  AllocCacheEntry *r = tsd->cache[idx];
  if (r) {
    tsd->cache[idx] = r->next;
    return r;
  }
  return _swift_slowAlloc_fixup(idx, SWIFT_TRYALLOC);
}

void *
swift_tryRawAlloc(SwiftAllocIndex idx)
{
  AllocCacheEntry *r = tsd->rawCache[idx];
  if (r) {
    tsd->rawCache[idx] = r->next;
    return r;
  }
  return _swift_slowAlloc_fixup(idx, SWIFT_TRYALLOC|SWIFT_RAWALLOC);
}

void
swift_dealloc(void *ptr, SwiftAllocIndex idx)
{
  auto cur = static_cast<AllocCacheEntry *>(ptr);
  AllocCacheEntry *prev = tsd->cache[idx];
  cur->next = prev;
  tsd->cache[idx] = cur;
}

void
swift_rawDealloc(void *ptr, SwiftAllocIndex idx)
{
  auto cur = static_cast<AllocCacheEntry *>(ptr);
  AllocCacheEntry *prev = tsd->rawCache[idx];
  cur->next = prev;
  tsd->rawCache[idx] = cur;
}
#endif

void
swift_slowDealloc(void *ptr, size_t bytes)
{
  SwiftAllocIndex idx;

  if (bytes == 0) {
    // the caller either doesn't know the size
    // or the caller really does think the size is zero
    // in any case, punt!
    free(ptr);
  }

  bytes--;

#ifdef __LP64__
  if        (bytes < 0x80)   { idx = (bytes >> 3);
  } else if (bytes < 0x100)  { idx = (bytes >> 4) + 0x8;
  } else if (bytes < 0x200)  { idx = (bytes >> 5) + 0x10;
  } else if (bytes < 0x400)  { idx = (bytes >> 6) + 0x18;
  } else if (bytes < 0x800)  { idx = (bytes >> 7) + 0x20;
  } else if (bytes < 0x1000) { idx = (bytes >> 8) + 0x28;
#else
  if        (bytes < 0x40)   { idx = (bytes >> 2);
  } else if (bytes < 0x80)   { idx = (bytes >> 3) + 0x8;
  } else if (bytes < 0x100)  { idx = (bytes >> 4) + 0x10;
  } else if (bytes < 0x200)  { idx = (bytes >> 5) + 0x18;
  } else if (bytes < 0x400)  { idx = (bytes >> 6) + 0x20;
  } else if (bytes < 0x800)  { idx = (bytes >> 7) + 0x28;
  } else if (bytes < 0x1000) { idx = (bytes >> 8) + 0x30;
#endif
  } else { return free(ptr);
  }

  swift_dealloc(ptr, idx);
}

void
swift_slowRawDealloc(void *ptr, size_t bytes)
{
  SwiftAllocIndex idx;

  if (bytes == 0) {
    // the caller either doesn't know the size
    // or the caller really does think the size is zero
    // in any case, punt!
    free(ptr);
  }

  bytes--;

#ifdef __LP64__
  if        (bytes < 0x80)   { idx = (bytes >> 3);
  } else if (bytes < 0x100)  { idx = (bytes >> 4) + 0x8;
  } else if (bytes < 0x200)  { idx = (bytes >> 5) + 0x10;
  } else if (bytes < 0x400)  { idx = (bytes >> 6) + 0x18;
  } else if (bytes < 0x800)  { idx = (bytes >> 7) + 0x20;
  } else if (bytes < 0x1000) { idx = (bytes >> 8) + 0x28;
#else
  if        (bytes < 0x40)   { idx = (bytes >> 2);
  } else if (bytes < 0x80)   { idx = (bytes >> 3) + 0x8;
  } else if (bytes < 0x100)  { idx = (bytes >> 4) + 0x10;
  } else if (bytes < 0x200)  { idx = (bytes >> 5) + 0x18;
  } else if (bytes < 0x400)  { idx = (bytes >> 6) + 0x20;
  } else if (bytes < 0x800)  { idx = (bytes >> 7) + 0x28;
  } else if (bytes < 0x1000) { idx = (bytes >> 8) + 0x30;
#endif
  } else { return free(ptr);
  }

  swift_rawDealloc(ptr, idx);
}
