//===--- Heap.cpp - Swift Language Heap Logic -----------------------------===//
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
// Implementations of the Swift heap
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Heap.h"
#include "Private.h"
#include <stdio.h>
#include <stdlib.h>
#include <cassert>
#include <pthread.h>

using namespace swift;

__attribute__((visibility("hidden")))
malloc_zone_t _swift_zone = {
  NULL, // ignore -- for CF
  NULL, // ignore -- for CF
  _swift_zone_size,
  _swift_zone_malloc,
  _swift_zone_calloc,
  _swift_zone_valloc,
  _swift_zone_free,
  _swift_zone_realloc,
  _swift_zone_destroy,
  "SwiftZone", // name
  NULL, // optional batch malloc
  NULL, // optional batch free
  NULL, // FIXME -- struct malloc_introspection_t *
  0, // version
  NULL, // XXX -- add support for memalign and free_definite_size?
  NULL, // XXX -- add support for memalign and free_definite_size?
  NULL, // XXX -- add support for pressure_relief?
};

extern "C" pthread_key_t _swift_alloc_offset = -1;

__attribute__((constructor))
static void registerZone() {
  assert(sizeof(pthread_key_t) == sizeof(long));
  malloc_zone_register(&_swift_zone);
  pthread_key_t key, prev_key;
  int r = pthread_key_create(&key, NULL);
  assert(r == 0);
  prev_key = key;
  _swift_alloc_offset = key;
  for (unsigned i = 0; i < 64; i++) {
    int r = pthread_key_create(&key, NULL);
    assert(r == 0);
    assert(key == prev_key + 1);
    prev_key = key;
  }
}

size_t swift::_swift_zone_size(malloc_zone_t *zone, const void *pointer) {
  return malloc_size(pointer);
}

void *swift::_swift_zone_malloc(malloc_zone_t *zone, size_t size) {
  return malloc(size);
}

void *swift::_swift_zone_calloc(malloc_zone_t *zone,
                                size_t count, size_t size) {
  return calloc(count, size);
}

void *swift::_swift_zone_valloc(malloc_zone_t *zone, size_t size) {
  return valloc(size);
}

void swift::_swift_zone_free(malloc_zone_t *zone, void *pointer) {
  return free(pointer);
}

void *swift::_swift_zone_realloc(malloc_zone_t *zone,
                                 void *pointer, size_t size) {
  return realloc(pointer, size);
}

void swift::_swift_zone_destroy(malloc_zone_t *zone) {
  // nobody should ever destroy this zone
  abort();
}

// Plain old memory allocation

__attribute__((noinline,used))
static void *
_swift_alloc_slow(AllocIndex idx, uintptr_t flags)
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

  void *r;
  do {
    if (flags & SWIFT_RAWALLOC) {
      r = _swift_zone.malloc(NULL, sz);
    } else {
      r = _swift_zone.calloc(NULL, 1, sz);
    }
  } while (!r && !(flags & SWIFT_TRYALLOC));

  return r;
}

extern "C" LLVM_LIBRARY_VISIBILITY
void _swift_refillThreadAllocCache(AllocIndex idx, uintptr_t flags) {
  void *tmp = _swift_alloc_slow(idx, flags);
  if (!tmp) {
    return;
  }
  assert(flags & SWIFT_RAWALLOC);
  swift_rawDealloc(tmp, idx);
}

void *swift::swift_slowAlloc(size_t size, uintptr_t flags) {
  size_t idx = SIZE_MAX;
  if (size == 0) idx = 0;
  --size;
  // we could do a table based lookup if we think it worthwhile
#ifdef __LP64__
  if      (size <   0x80) idx = (size >> 3) +  0x0;
  else if (size <  0x100) idx = (size >> 4) +  0x8;
  else if (size <  0x200) idx = (size >> 5) + 0x10;
  else if (size <  0x400) idx = (size >> 6) + 0x18;
  else if (size <  0x800) idx = (size >> 7) + 0x20;
  else if (size < 0x1000) idx = (size >> 8) + 0x28;
#else
  if      (size <   0x40) idx = (size >> 2) +  0x0;
  else if (size <   0x80) idx = (size >> 3) +  0x8;
  else if (size <  0x100) idx = (size >> 4) + 0x10;
  else if (size <  0x200) idx = (size >> 5) + 0x18;
  else if (size <  0x400) idx = (size >> 6) + 0x20;
  else if (size <  0x800) idx = (size >> 7) + 0x28;
  else if (size < 0x1000) idx = (size >> 8) + 0x30;
#endif

  void *r;
  if (idx != SIZE_MAX) {
    assert(flags & SWIFT_RAWALLOC);
    r = swift_tryRawAlloc(idx);
    if (r) return r;
  }

  do {
    if (flags & SWIFT_RAWALLOC) {
      r = _swift_zone.malloc(NULL, size);
    } else {
      r = _swift_zone.calloc(NULL, 1, size);
    }
  } while (!r && !(flags & SWIFT_TRYALLOC));

  return r;
}

// These are implemented in FastEntryPoints.s on some platforms.
#ifndef SWIFT_HAVE_FAST_ENTRY_POINTS

#if __has_include(<os/tsd.h>)
// OS X and iOS internal version

#include <os/tsd.h>

struct AllocCacheEntry {
  struct AllocCacheEntry *next;
};

static AllocCacheEntry *
getAllocCacheEntry(unsigned long idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  return (AllocCacheEntry *)_os_tsd_get_direct(idx + _swift_alloc_offset);
}

static void
setAllocCacheEntry(unsigned long idx, AllocCacheEntry *entry) {
  assert(idx < ALLOC_CACHE_COUNT);
  _os_tsd_set_direct(idx + _swift_alloc_offset, entry);
}

static AllocCacheEntry *
getRawAllocCacheEntry(unsigned long idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  return (AllocCacheEntry *)_os_tsd_get_direct(idx + _swift_alloc_offset);
}

static void
setRawAllocCacheEntry(unsigned long idx, AllocCacheEntry *entry) {
  assert(idx < ALLOC_CACHE_COUNT);
  _os_tsd_set_direct(idx + _swift_alloc_offset, entry);
}

void *swift::swift_rawAlloc(AllocIndex idx) {
  AllocCacheEntry *r = getRawAllocCacheEntry(idx);
  if (r) {
    setRawAllocCacheEntry(idx, r->next);
    return r;
  }
  return _swift_alloc_slow(idx, SWIFT_RAWALLOC);
}

void *swift::swift_tryRawAlloc(AllocIndex idx) {
  AllocCacheEntry *r = getRawAllocCacheEntry(idx);
  if (r) {
    setRawAllocCacheEntry(idx, r->next);
    return r;
  }
  return _swift_alloc_slow(idx, SWIFT_TRYALLOC|SWIFT_RAWALLOC);
}

void swift::swift_rawDealloc(void *ptr, AllocIndex idx) {
  auto cur = static_cast<AllocCacheEntry *>(ptr);
  AllocCacheEntry *prev = getRawAllocCacheEntry(idx);
  cur->next = prev;
  setRawAllocCacheEntry(idx, cur);
}

#else

# error no thread-local cache implementation for this platform

#endif

// !SWIFT_HAVE_FAST_ENTRY_POINTS
#endif

void swift::swift_slowDealloc(void *ptr, size_t bytes) {
  if (bytes == 0) {
    bytes = malloc_size(ptr);
  }

  bytes--;

  AllocIndex idx;
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

void swift::swift_slowRawDealloc(void *ptr, size_t bytes) {
  AllocIndex idx;

  if (bytes == 0) {
    // the caller either doesn't know the size
    // or the caller really does think the size is zero
    // in any case, punt!
    return free(ptr);
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
