/*
 * Copyright (c) 2010-2011 Apple Inc. All rights reserved.
 */

#include "MemMgmt.h"
#include "tsd.h"

#include <malloc/malloc.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

// branch hinting
// I wish we had __builtin_expect_range()
#define fastpath(x) ((__typeof__(x))__builtin_expect((long)(x), ~0l))
#define slowpath(x) ((__typeof__(x))__builtin_expect((long)(x), 0l))

#if defined(__i386__) || defined(__x86_64__)
#define _br_hardware_pause() asm("pause")
#define _BRDebugger() asm("int3")
#else
#define _br_hardware_pause() asm("")
#define _BRDebugger() asm("trap")
#endif

// really just a low level abort()
__attribute__((noreturn,always_inline))
static inline void
_br_hardware_crash(void)
{
  _BRDebugger();
  __builtin_unreachable();
}


struct BRAlloc_free_list_s {
  struct BRAlloc_free_list_s *next;
};

static inline void *
_BRTryRawCacheAllocFaster(size_t off)
{
  struct BRAlloc_free_list_s *rval =
    (__typeof__(rval))_br_thread_getspecific(_br_alloc_key + off);
  if (fastpath(rval)) {
    _br_thread_setspecific(_br_alloc_key + off, rval->next);
  }
  return rval;
}

#define _BRTryRawCacheAlloc(x) \
  _BRAllocFasterSlower(x, _BRTryRawCacheAllocFaster, BRALLOC_TRY|BRALLOC_RAW)

malloc_zone_t *_gc_zone;

__attribute__((constructor))
static void
alloc_init()
{
#if _BR_USE_GC
  _gc_zone = auto_zone_create("com.apple.Bedrock");
#else
  _gc_zone = malloc_default_zone();
#endif
}

__attribute__((noinline))
static void *
_BRGenericAllocSlower(size_t sz, unsigned long flags) throw()
{
  void *rval;

  if (flags & BRALLOC_TRY) {
    if (flags & BRALLOC_RAW) {
      return malloc_zone_malloc(_gc_zone, sz);
    }
    return malloc_zone_calloc(_gc_zone, 1, sz);
  }

  if (flags & BRALLOC_RAW) {
    while (!(rval = malloc_zone_malloc(_gc_zone, sz))) {
      sleep(1);	// FIXME -- refactor malloc as an actor
    }
  } else {
    while (!(rval = malloc_zone_calloc(_gc_zone, 1, sz))) {
      sleep(1);	// FIXME -- refactor malloc as an actor
    }
  }
  return rval;
}

__attribute__((noinline))
static void *
_BRGenericMemSet(size_t off, void *ptr) throw()
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
    _br_hardware_crash();
  }

  // XXX FIXME -- memset() is too generic for us, and therefore too expensive

  return memset(ptr, 0, sz);
}

__attribute__((noinline))
static void *
_BRGenericAllocFixup(size_t off, unsigned long flags) throw()
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
    _br_hardware_crash();
  }

  return _BRGenericAllocSlower(sz, flags);
}

void *
BRGenericAlloc(size_t sz, unsigned long flags) throw()
{
  size_t off;

  sz--;

  if (slowpath(sz == SIZE_MAX)) {
    // sz was zero, therefore bucket 0
    off = 0;
  } else if (fastpath(sz < 0x80)) {
    // about 90% of allocations
    off = (sz >> 3);
  } else if (sz < 0x100) {
    off = (sz >> 4) + 0x8;
  } else if (sz < 0x200) {
    off = (sz >> 5) + 0x10;
  } else if (sz < 0x400) {
    off = (sz >> 6) + 0x18;
  } else if (sz < 0x800) {
    off = (sz >> 7) + 0x20;
  } else if (sz < 0x1000) {
    off = (sz >> 8) + 0x28;
  } else {
    // about 1% of allocations
    return _BRGenericAllocSlower(sz, flags);
  }

  switch (flags) {
  case 0:
    return BRAllocFaster(off);
  case BRALLOC_TRY:
    return BRTryAllocFaster(off);
  case BRALLOC_RAW:
    return BRRawAllocFaster(off);
  case BRALLOC_TRY|BRALLOC_RAW:
    return BRTryRawAllocFaster(off);
  default:
    _br_hardware_crash();
  }
}

__attribute__((aligned(16)))
void *
BRAllocFaster(size_t off) throw()
{
  struct BRAlloc_free_list_s *rval =
    (__typeof__(rval))_BRTryRawCacheAllocFaster(off);
  if (slowpath(!rval)) {
    return _BRGenericAllocFixup(off, 0);
  }
  return _BRGenericMemSet(off, rval);
}

__attribute__((aligned(16)))
void *
BRTryAllocFaster(size_t off) throw()
{
  struct BRAlloc_free_list_s *rval =
    (__typeof__(rval))_BRTryRawCacheAllocFaster(off);
  if (slowpath(!rval)) {
    return _BRGenericAllocFixup(off, BRALLOC_TRY);
  }
  return _BRGenericMemSet(off, rval);
}

__attribute__((aligned(16)))
void *
BRRawAllocFaster(size_t off) throw()
{
  struct BRAlloc_free_list_s *rval =
    (__typeof__(rval))_BRTryRawCacheAllocFaster(off);
  if (slowpath(!rval)) {
    return _BRGenericAllocFixup(off, BRALLOC_RAW);
  }
  return rval;
}

__attribute__((aligned(16)))
void *
BRTryRawAllocFaster(size_t off) throw()
{
  struct BRAlloc_free_list_s *rval =
    (__typeof__(rval))_BRTryRawCacheAllocFaster(off);
  if (slowpath(!rval)) {
    return _BRGenericAllocFixup(off, BRALLOC_TRY|BRALLOC_RAW);
  }
  return rval;
}

void
BRDeallocSlower(void *ptr, size_t sz) throw()
{
  size_t off;

  sz--;

  if (slowpath(sz == SIZE_MAX)) {
    // sz was zero, therefore bucket 0
    off = 0;
  } else if (fastpath(sz < 0x80)) {
    // about 90% of allocations
    off = (sz >> 3);
  } else if (sz < 0x100) {
    off = (sz >> 4) + 0x8;
  } else if (sz < 0x200) {
    off = (sz >> 5) + 0x10;
  } else if (sz < 0x400) {
    off = (sz >> 6) + 0x18;
  } else if (sz < 0x800) {
    off = (sz >> 7) + 0x20;
  } else if (sz < 0x1000) {
    off = (sz >> 8) + 0x28;
  } else {
    // about 1% of allocations
    return malloc_zone_free(_gc_zone, ptr);
  }

  BRDeallocFaster(ptr, off);
}

__attribute__((aligned(16)))
void
BRDeallocFaster(void *_ptr, size_t off) throw()
{
#if _BR_USE_ALLOC_CACHE
  struct BRAlloc_free_list_s *ptr = (__typeof__(ptr))_ptr;
  struct BRAlloc_free_list_s *head =
    (__typeof__(head))_br_thread_getspecific(_br_alloc_key + off);
  ptr->next = head;
  _br_thread_setspecific(_br_alloc_key + off, ptr);
#else
  (void)off;
  malloc_zone_free(_gc_zone, _ptr);
#endif
}
