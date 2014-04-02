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

#include "llvm/ADT/DenseMap.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Heap.h"
#include "Private.h"
#include <mach/vm_statistics.h>
#include <stdio.h>
#include <stdlib.h>
#include <cassert>
#include <pthread.h>
#include <vector>
#include <functional>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>

using namespace swift;

static kern_return_t _swift_zone_enumerator(task_t task, void *,
                                            unsigned type_mask,
                                            vm_address_t zone_address,
                                            memory_reader_t reader,
                                            vm_range_recorder_t recorder);
static size_t _swift_zone_good_size(malloc_zone_t *zone, size_t size);
static boolean_t _swift_zone_check(malloc_zone_t *zone);
static void _swift_zone_print(malloc_zone_t *zone, boolean_t verbose);
static void _swift_zone_log(malloc_zone_t *zone, void *address);
static void _swift_zone_statistics(malloc_zone_t *zone,
                                   malloc_statistics_t *stats);


namespace {

class SwiftZone {
private:
  static SwiftZone swiftZone;
  static pthread_rwlock_t lock;

  void *operator new(size_t size) = delete;
  SwiftZone(SwiftZone const &) = delete;
  SwiftZone &operator=(SwiftZone const &) = delete;

  SwiftZone();
  static void threadExitCleanup(void *arg);
public:
  static malloc_introspection_t zoneInspection;

  // allocations are normally per-thread
  // this API goes straight to the global pool
  static void *globalAlloc(AllocIndex idx, uintptr_t flags);

  static void *alloc_optimized(AllocIndex idx);
  static void *alloc_unoptimized(AllocIndex idx);
  static void *tryAlloc_optimized(AllocIndex idx);
  static void *tryAlloc_unoptimized(AllocIndex idx);
  static void *slowAlloc_optimized(size_t size, uintptr_t flags);
  static void *slowAlloc_unoptimized(size_t size, uintptr_t flags);
  static void dealloc_optimized(void *ptr, AllocIndex idx);
  static void dealloc_unoptimized(void *ptr, AllocIndex idx);
  static void slowDealloc_optimized(void *ptr, size_t bytes);
  static void slowDealloc_unoptimized(void *ptr, size_t bytes);
  static bool tryWriteLock() {
    int r = pthread_rwlock_trywrlock(&lock);
    assert(r == 0 || errno == EBUSY);
    return r == 0;
  }
  static void writeLock() {
    int r = pthread_rwlock_wrlock(&lock);
    assert(r == 0);
  }
  static void writeUnlock() {
    int r = pthread_rwlock_unlock(&lock);
    assert(r == 0);
  }
  static void readLock() {
    int r = pthread_rwlock_rdlock(&lock);
    assert(r == 0);
  }
  static void readUnlock() {
    int r = pthread_rwlock_unlock(&lock);
    assert(r == 0);
  }
  void debug() {
    malloc_zone_print(&zoneShims, true);
  }
};

extern "C" pthread_key_t _swiftAllocOffset = -1;

const size_t  pageSize = getpagesize();
const size_t  pageMask = getpagesize() - 1;
const size_t arenaSize = 0x10000;
const size_t arenaMask =  0xFFFF;

auto _swift_alloc = SwiftZone::alloc_optimized;
auto _swift_tryAlloc = SwiftZone::tryAlloc_optimized;
auto _swift_slowAlloc = SwiftZone::slowAlloc_optimized;
auto _swift_dealloc = SwiftZone::dealloc_optimized;
auto _swift_slowDealloc = SwiftZone::slowDealloc_optimized;

size_t roundToPage(size_t size) {
  return (size + pageMask) & ~pageMask;
}

void *mmapWrapper(size_t size, bool huge) {
  int tag = VM_MAKE_TAG(huge ? VM_MEMORY_MALLOC_HUGE : VM_MEMORY_MALLOC_SMALL);
  return mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_ANON|MAP_PRIVATE, tag, 0);
}

typedef struct AllocCacheEntry_s {
  struct AllocCacheEntry_s *next;
} *AllocCacheEntry;

AllocCacheEntry globalCache[ALLOC_CACHE_COUNT];

class Arena;
static llvm::DenseMap<const void *, Arena> arenas;
static llvm::DenseMap<const void *, size_t> hugeAllocations;

class Arena {
public:
  void *base;
  uint16_t byteSize; // max 4096
  uint8_t  index;    // max 56 or 64
  Arena(size_t idx, size_t size) : byteSize(size), index(idx) {
    assert(size > 0);
    base = mmapWrapper(arenaSize * 2, /*huge*/ false);
    assert(base != MAP_FAILED);
    void *newBase = (void *)(((size_t)base & ~arenaMask) + arenaSize);
    size_t frontSlop = (size_t)newBase - (size_t)base;
    size_t backSlop = arenaSize - frontSlop;
    int r = munmap(base, frontSlop);
    assert(r != -1);
    if (backSlop) {
      r = munmap((uint8_t *)newBase + arenaSize, backSlop);
      assert(r != -1);
    }
    base = newBase;
    auto headNode = (AllocCacheEntry)base;
    auto node = headNode;
    size_t allocations = arenaSize / size;
    for (unsigned i = 1; i < allocations; i++) {
      auto nextNode = (AllocCacheEntry)((uint8_t *)base + i * size);
      node->next = nextNode;
      node = nextNode;
    }
    assert(globalCache[idx] == NULL);
    globalCache[idx] = headNode;
  }
  static void newArena(size_t idx, size_t size) {
    auto arena = Arena(idx, size);
    arenas.insert(std::pair<void *, Arena>(arena.base, arena));
  }
};

} // end anonymous namespace

SwiftZone::SwiftZone() {
  assert(sizeof(pthread_key_t) == sizeof(long));
  malloc_zone_register(&zoneShims);
  pthread_key_t key, prev_key;
  int r = pthread_key_create(&key, threadExitCleanup);
  assert(r == 0);
  (void)r;
  prev_key = key;
  _swiftAllocOffset = key;
  for (unsigned i = 0; i < ALLOC_CACHE_COUNT; i++) {
    int r = pthread_key_create(&key, threadExitCleanup);
    assert(r == 0);
    (void)r;
    assert(key == prev_key + 1);
    prev_key = key;
  }
  if (getenv("SWIFT_ZONE_DEBUG")) {
    _swift_alloc = SwiftZone::alloc_unoptimized;
    _swift_tryAlloc = SwiftZone::tryAlloc_unoptimized;
    _swift_slowAlloc = SwiftZone::slowAlloc_unoptimized;
    _swift_dealloc = SwiftZone::dealloc_unoptimized;
    _swift_slowDealloc = SwiftZone::slowDealloc_unoptimized;
  }
}

size_t swift::_swift_zone_size(malloc_zone_t *zone, const void *pointer) {
  void *ptr = (void *)((unsigned long)pointer & ~arenaMask);
  auto it = arenas.find(ptr);
  if (it != arenas.end()) {
    return it->second.byteSize;
  }
  auto it2 = hugeAllocations.find(pointer);
  if (it2 != hugeAllocations.end()) {
    return it2->second;
  }
  return 0;
}

void *swift::_swift_zone_malloc(malloc_zone_t *zone, size_t size) {
  return SwiftZone::slowAlloc_optimized(size, SWIFT_TRYALLOC);
}

void *swift::_swift_zone_calloc(malloc_zone_t *zone,
                                size_t count, size_t size) {
  void *pointer = swift::_swift_zone_malloc(zone, count * size);
  return pointer ? memset(pointer, 0, count * size) : pointer;
}

void *swift::_swift_zone_valloc(malloc_zone_t *zone, size_t size) {
  assert((pageMask & 0xFFF) == 0xFFF); // at least 4k
  return swift::_swift_zone_malloc(zone, roundToPage(size));
}

void swift::_swift_zone_free(malloc_zone_t *zone, void *pointer) {
  swift::swift_slowDealloc(pointer, 0);
}

void *swift::_swift_zone_realloc(malloc_zone_t *zone,
                                 void *pointer, size_t size) {
  abort(); // nobody should call this
  // but making it work is easy if we want:
  auto newPointer = swift::_swift_zone_malloc(zone, size);
  auto oldSize = swift::_swift_zone_size(zone, pointer);
  memcpy(newPointer, pointer, size < oldSize ? size : oldSize);
  swift::_swift_zone_free(zone, pointer);
  return newPointer;
}

void swift::_swift_zone_destroy(malloc_zone_t *zone) {
  // nobody should ever destroy this zone
  abort();
}

kern_return_t
_swift_zone_enumerator(task_t task, void *, unsigned type_mask,
                       vm_address_t zone_address, memory_reader_t reader,
                       vm_range_recorder_t recorder) {
  abort();
}

size_t _swift_zone_good_size(malloc_zone_t *zone, size_t size) {
  // XXX -- Switch to raw swift heap calls
  void *temp = _swift_zone_malloc(zone, size);
  size_t r = _swift_zone_size(zone, temp);
  _swift_zone_free(zone, temp);
  return r;
}

boolean_t _swift_zone_check(malloc_zone_t *zone) {
  return true; // we don't have any self-consistency checks; true == good/okay
}

void _swift_zone_log(malloc_zone_t *zone, void *address) {
  abort();
}

static inline size_t indexToSize(AllocIndex idx) {
  size_t size;
  idx++;

  // we could do a table based lookup if we think it worthwhile
#ifdef __LP64__
  if        (idx <= 16) { size =  idx       << 3;
  } else if (idx <= 24) { size = (idx -  8) << 4;
  } else if (idx <= 32) { size = (idx - 16) << 5;
  } else if (idx <= 40) { size = (idx - 24) << 6;
  } else if (idx <= 48) { size = (idx - 32) << 7;
  } else if (idx <= 56) { size = (idx - 40) << 8;
#else
  if        (idx <= 16) { size =  idx       << 2;
  } else if (idx <= 24) { size = (idx -  8) << 3;
  } else if (idx <= 32) { size = (idx - 16) << 4;
  } else if (idx <= 40) { size = (idx - 24) << 5;
  } else if (idx <= 48) { size = (idx - 32) << 6;
  } else if (idx <= 56) { size = (idx - 40) << 7;
  } else if (idx <= 64) { size = (idx - 48) << 8;
#endif
  } else {
    __builtin_trap();
  }

  return size;
}

__attribute__((noinline,used))
void *SwiftZone::globalAlloc(AllocIndex idx, uintptr_t flags)
{
  AllocCacheEntry ptr = NULL;
  size_t size;

again:
  writeLock();
  if ((ptr = globalCache[idx])) {
    globalCache[idx] = globalCache[idx]->next;
  }
  // we should probably refill the cache in bulk
  writeUnlock();

  if (ptr) {
    return ptr;
  }

  size = indexToSize(idx);

  Arena::newArena(idx, size);
  // FIXME -- SWIFT_TRYALLOC
  goto again;
}

extern "C" LLVM_LIBRARY_VISIBILITY
void _swift_refillThreadAllocCache(AllocIndex idx, uintptr_t flags) {
  void *tmp = SwiftZone::globalAlloc(idx, flags);
  if (!tmp) {
    return;
  }
  SwiftZone::dealloc_unoptimized(tmp, idx);
}

void *SwiftZone::slowAlloc_optimized(size_t size, uintptr_t flags) {
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
  else {
    ++size;
    // large allocations
    void *r = mmapWrapper(size, /*huge*/ true);
    if (r == MAP_FAILED) {
      if (flags & SWIFT_TRYALLOC) {
        return NULL;
      }
      abort();
    }
    hugeAllocations.insert(std::pair<void *, size_t>(r, size));
    return r;
  }

  void *r = tryAlloc_optimized(idx);
  if (r) return r;

  _swift_refillThreadAllocCache(idx, flags);

  if (flags & SWIFT_TRYALLOC) {
    return tryAlloc_optimized(idx);
  } else {
    return alloc_optimized(idx);
  }
}

#if __has_include(<os/tsd.h>)
// OS X and iOS internal version
#include <os/tsd.h>
#else
#define _os_tsd_get_direct pthread_getspecific
#define _os_tsd_set_direct pthread_setspecific
#endif

// These are implemented in FastEntryPoints.s on some platforms.
#ifndef SWIFT_HAVE_FAST_ENTRY_POINTS

static AllocCacheEntry
getAllocCacheEntry(unsigned long idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  return (AllocCacheEntry)_os_tsd_get_direct(idx + _swiftAllocOffset);
}

static void
setAllocCacheEntry(unsigned long idx, AllocCacheEntry entry) {
  assert(idx < ALLOC_CACHE_COUNT);
  _os_tsd_set_direct(idx + _swiftAllocOffset, entry);
}

void *SwiftZone::alloc_optimized(AllocIndex idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  AllocCacheEntry r = getAllocCacheEntry(idx);
  if (r) {
    setAllocCacheEntry(idx, r->next);
    return r;
  }
  return globalAlloc(idx, 0);
}

void *SwiftZone::tryAlloc_optimized(AllocIndex idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  AllocCacheEntry r = getAllocCacheEntry(idx);
  if (r) {
    setAllocCacheEntry(idx, r->next);
    return r;
  }
  return globalAlloc(idx, SWIFT_TRYALLOC);
}

void SwiftZone::dealloc_optimized(void *ptr, AllocIndex idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  auto cur = static_cast<AllocCacheEntry>(ptr);
  AllocCacheEntry prev = getAllocCacheEntry(idx);
  assert(cur != prev && "trivial double free!");
  cur->next = prev;
  setAllocCacheEntry(idx, cur);
}

// !SWIFT_HAVE_FAST_ENTRY_POINTS
#endif

void SwiftZone::slowDealloc_optimized(void *ptr, size_t bytes) {
  if (bytes == 0) {
    bytes = zoneShims.size(NULL, ptr);
  }
  assert(bytes != 0);

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
  } else {
    auto it2 = hugeAllocations.find(ptr);
    assert(it2 != hugeAllocations.end());
    int r = munmap(const_cast<void *>(it2->first), it2->second);
    assert(r != -1);
    hugeAllocations.erase(it2);
    return;
  }

  dealloc_optimized(ptr, idx);
}

void
SwiftZone::threadExitCleanup(void *arg) {
  (void)arg;
  AllocCacheEntry threadCache[ALLOC_CACHE_COUNT];
  AllocCacheEntry threadCacheTail[ALLOC_CACHE_COUNT];
  for (unsigned i = 0; i < ALLOC_CACHE_COUNT; i++) {
    threadCache[i] = (AllocCacheEntry)_os_tsd_get_direct(
                                                      _swiftAllocOffset + i);
    if (threadCache[i] == NULL) {
      continue;
    }
    _os_tsd_set_direct(_swiftAllocOffset + i, NULL);
    AllocCacheEntry temp = threadCache[i];
    while (temp->next) {
      temp = temp->next;
    }
    threadCacheTail[i] = temp;
  }
  writeLock();
  for (unsigned i = 0; i < ALLOC_CACHE_COUNT; i++) {
    if (threadCache[i] == NULL) {
      continue;
    }
    threadCacheTail[i]->next = globalCache[i];
    globalCache[i] = threadCache[i];
  }
  writeUnlock();
}

kern_return_t _swift_memory_reader(task_t task,
                                   vm_address_t remote_address,
                                   vm_size_t size, void **local_memory) {
    /* given a task, "reads" the memory at the given address and size
local_memory: set to a contiguous chunk of memory; validity of local_memory is assumed to be limited (until next call) */

#define MALLOC_PTR_IN_USE_RANGE_TYPE    1   /* for allocated pointers */
#define MALLOC_PTR_REGION_RANGE_TYPE    2   /* for region containing pointers */
#define MALLOC_ADMIN_REGION_RANGE_TYPE  4   /* for region used internally */
#define MALLOC_ZONE_SPECIFIC_FLAGS  0xff00  /* bits reserved for zone-specific purposes */
  abort();
}

void _swift_vm_range_recorder(task_t task, void *, unsigned type,
                              vm_range_t *, unsigned) {
    /* given a task and context, "records" the specified addresses */
}

static void
enumerateBlocks(std::function<void(const void *, size_t)> func) {
  // XXX switch to a bitmap or a set
  // FIXME scan other threads
  llvm::DenseMap<const void *, bool> unusedBlocks;
  for (unsigned i = 0; i < ALLOC_CACHE_COUNT; i++) {
    for (AllocCacheEntry pointer = globalCache[i]; pointer;
         pointer = pointer->next) {
      unusedBlocks.insert(std::pair<void *, bool>(pointer, true));
    }
    for (AllocCacheEntry pointer = getAllocCacheEntry(i); pointer;
         pointer = pointer->next) {
      unusedBlocks.insert(std::pair<void *, bool>(pointer, true));
    }
  }

  for (auto &pair : arenas) {
    Arena &arena = pair.second;
    size_t count = arenaSize / arena.byteSize;
    for (size_t i = 0; i < count; i += arena.byteSize) {
      auto pointer = (const void *)((uint8_t *)arena.base + i);
      if (!unusedBlocks[pointer]) {
        func(pointer, arena.byteSize);
      }
    }
  }
  for (auto &pair : hugeAllocations) {
    func(pair.first, pair.second);
  }
}

void _swift_zone_print(malloc_zone_t *zone, boolean_t verbose) {
  enumerateBlocks([&](const void *pointer, size_t size) {
    abort(); // FIXME -- what should we do?
  });
}

void _swift_zone_statistics(malloc_zone_t *zone,
                            malloc_statistics_t *statistics) {
  memset(statistics, 0, sizeof(*statistics));
  enumerateBlocks([&](const void *pointer, size_t size) {
    ++statistics->blocks_in_use;
    statistics->size_in_use += size;
    if (size > statistics->max_size_in_use) {
      statistics->max_size_in_use = size;
    }
  });
  statistics->size_allocated = arenas.size() * arenaSize;
  for (auto &pair : hugeAllocations) {
    statistics->size_allocated += pair.second;
  }
}

void *SwiftZone::alloc_unoptimized(AllocIndex idx) {
  return swiftZone.slowAlloc_unoptimized(indexToSize(idx), 0);
}

void *SwiftZone::tryAlloc_unoptimized(AllocIndex idx) {
  return swiftZone.slowAlloc_unoptimized(indexToSize(idx), SWIFT_TRYALLOC);
}

void *SwiftZone::slowAlloc_unoptimized(size_t size, uintptr_t flags) {
  void *r;
  // the zone API does not have a notion of try-vs-not
  do {
    r = malloc_zone_malloc(&zoneShims, size);
  } while ((r == NULL) && (flags & SWIFT_TRYALLOC));
  return r;
}

void SwiftZone::dealloc_unoptimized(void *ptr, AllocIndex idx) {
  malloc_zone_free(&zoneShims, ptr);
}

void SwiftZone::slowDealloc_unoptimized(void *ptr, size_t bytes) {
  malloc_zone_free(&zoneShims, ptr);
}

malloc_zone_t swift::zoneShims = {
  nullptr,
  nullptr,
  _swift_zone_size,
  _swift_zone_malloc,
  _swift_zone_calloc,
  _swift_zone_valloc,
  _swift_zone_free,
  _swift_zone_realloc,
  _swift_zone_destroy,
  "SwiftZone",
  nullptr,
  nullptr,
  &SwiftZone::zoneInspection,
  0,
  nullptr,
  nullptr,
  nullptr,
};

malloc_introspection_t SwiftZone::zoneInspection = {
  _swift_zone_enumerator,
  _swift_zone_good_size,
  _swift_zone_check,
  _swift_zone_print,
  _swift_zone_log,
  [] (malloc_zone_t *zone) {
    writeLock();
  },
  [] (malloc_zone_t *zone) {
    writeUnlock();
  },
  _swift_zone_statistics,
  [] (malloc_zone_t *zone) -> boolean_t {
      if (tryWriteLock()) {
      writeUnlock(); return false;
    }
    return true;
  },
  /* Discharge checking. Present in version >= 7. */
  NULL, // enable_discharge_checking
  NULL, // disable_discharge_checking
  NULL, // discharge
  NULL, // enumerate_discharged_pointers
};

SwiftZone SwiftZone::swiftZone;
pthread_rwlock_t SwiftZone::lock = PTHREAD_RWLOCK_INITIALIZER;

// Shims to select between the fast and
// introspectable/debugging paths at runtime
void *swift::swift_alloc(AllocIndex idx) {
  return _swift_alloc(idx);
}
void *swift::swift_tryAlloc(AllocIndex idx) {
  return _swift_tryAlloc(idx);
}
void *swift::swift_slowAlloc(size_t size, uintptr_t flags) {
  return _swift_slowAlloc(size, flags);
}
void swift::swift_dealloc(void *ptr, AllocIndex idx) {
  _swift_dealloc(ptr, idx);
}
void swift::swift_slowDealloc(void *ptr, size_t bytes) {
  return _swift_slowDealloc(ptr, bytes);
}
