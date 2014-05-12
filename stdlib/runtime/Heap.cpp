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
#include "swift/Runtime/InstrumentsSupport.h"
#include "swift/Runtime/Heap.h"
#include "Private.h"
#include "Debug.h"
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

static inline size_t indexToSize(AllocIndex idx);

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

static const AllocIndex badAllocIndex = AllocIndex(-1);

namespace {

void *mmapWrapper(size_t size, bool huge) {
  int tag = VM_MAKE_TAG(huge ? VM_MEMORY_MALLOC_HUGE : VM_MEMORY_MALLOC_SMALL);
  return mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_ANON|MAP_PRIVATE, tag, 0);
}

// HeapMap
//
// We need a trivial map that:
// 1) doesn't use the system allocator
// 2) is easy to programmatically introspect from outside of the process
//
// This is not on the critical path, so it doesn't need to be "perfect".
//
template <typename T, size_t (*hash)(const void *)>
class HeapMap {
private:
  std::pair<const void *, T> *nodes;
  size_t count;
  size_t mask;
  size_t bufSize() { return mask + 1; }
  void grow() {
    auto oldMask = mask;
    auto oldNodes = nodes;
    mask = mask * 2 + 1;
    nodes = reinterpret_cast<std::pair<const void *, T> *>(
      mmapWrapper(sizeof(nodes[0]) * bufSize(), false));
    for (size_t i = 0; i < (oldMask + 1); i++) {
      if (oldNodes[i].first == nullptr) continue;
      insert(std::pair<const void *, T>(oldNodes[i].first, oldNodes[i].second));
      oldNodes[i].second.~T();
    }
    int r = munmap(oldNodes, sizeof(oldNodes[0]) * (oldMask + 1));
    assert(r == 0);
  }
public:
  HeapMap() : count(0), mask(16-1) {
    nodes = reinterpret_cast<std::pair<const void *, T> *>(
      mmapWrapper(sizeof(nodes[0]) * bufSize(), false));
    assert(nodes != nullptr);
  }
  ~HeapMap() {
  }
  void insert(std::pair<const void *, T> p) {
    if (size() >= (bufSize() * 3 / 4)) {
      grow();
    }
    size_t i = hash(p.first) & mask;
    do {
      if (nodes[i].first == nullptr) {
        nodes[i].first = p.first;
        new (&nodes[i].second) T(p.second);
        ++count;
        return;
      }
    } while (++i < bufSize());
    for (i = 0; i < bufSize(); i++) {
      if (nodes[i].first == nullptr) {
        nodes[i].first = p.first;
        new (&nodes[i].second) T(p.second);
        ++count;
        return;
      }
    }
    abort();
  }
  std::pair<const void *, T> *find(const void *key) {
    size_t i = hash(key) & mask;
    do {
      if (nodes[i].first == key) return &nodes[i];
    } while (++i < bufSize());
    for (i = 0; i < bufSize(); i++) {
      if (nodes[i].first == key) return &nodes[i];
    }
    return nullptr;
  }
  void erase(std::pair<const void *, T> *p) {
    --count;
    p->first = nullptr;
    p->second.~T();
  }
  size_t size() const { return count; }
  T *operator [](const void *key) {
    return &find(key)->second;
  }
  void forEach(std::function<void(const void *, T)> func) {
    for (size_t i = 0, j = size(); j > 0; j--) {
      while (nodes[i].first == nullptr) i++;
      func(nodes[i].first, nodes[i].second);
    }
  }
};

class SwiftZone {
private:
  static SwiftZone swiftZone;
  static pthread_rwlock_t lock;

  void *operator new(size_t size) = delete;
  void operator delete(void *pointer) = delete;
  SwiftZone(SwiftZone const &) = delete;
  SwiftZone &operator=(SwiftZone const &) = delete;

  SwiftZone();
public:
  static void threadExitCleanup(void *arg);
  static malloc_introspection_t zoneInspection;

  // allocations are normally per-thread
  // this API goes straight to the global pool
  static void *globalAlloc(AllocIndex idx, uintptr_t flags);

  static void *alloc_optimized(AllocIndex idx);
  static void *tryAlloc_optimized(AllocIndex idx);
  static void *slowAlloc_optimized(size_t size, uintptr_t flags);
  static void dealloc_optimized(void *ptr, AllocIndex idx);
  static void slowDealloc_optimized(void *ptr, size_t bytes);

  static void *alloc_semi_optimized(AllocIndex idx);
  static void *tryAlloc_semi_optimized(AllocIndex idx);
  static void dealloc_semi_optimized(void *ptr, AllocIndex idx);

  static void *alloc_gmalloc(AllocIndex idx);
  static void *tryAlloc_gmalloc(AllocIndex idx);
  static void *slowAlloc_gmalloc(size_t size, uintptr_t flags);
  static void dealloc_gmalloc(void *ptr, AllocIndex idx);
  static void slowDealloc_gmalloc(void *ptr, size_t bytes);

  static bool tryWriteLock() {
    int r = pthread_rwlock_trywrlock(&lock);
    assert(r == 0 || errno == EBUSY);
    return r == 0;
  }
  static void writeLock() {
    int r = pthread_rwlock_wrlock(&lock);
    assert(r == 0);
    (void)r;
  }
  static void writeUnlock() {
    int r = pthread_rwlock_unlock(&lock);
    assert(r == 0);
    (void)r;
  }
  static void readLock() {
    int r = pthread_rwlock_rdlock(&lock);
    assert(r == 0);
    (void)r;
  }
  static void readUnlock() {
    int r = pthread_rwlock_unlock(&lock);
    assert(r == 0);
    (void)r;
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

size_t roundToPage(size_t size) {
  return (size + pageMask) & ~pageMask;
}

typedef struct AllocCacheEntry_s {
  struct AllocCacheEntry_s *next;
} *AllocCacheEntry;

AllocCacheEntry globalCache[ALLOC_CACHE_COUNT];

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

static AllocCacheEntry
getAllocCacheEntry_slow(unsigned long idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  return (AllocCacheEntry)pthread_getspecific(idx + _swiftAllocOffset);
}

static void
setAllocCacheEntry_slow(unsigned long idx, AllocCacheEntry entry) {
  assert(idx < ALLOC_CACHE_COUNT);
  auto result = pthread_setspecific(idx + _swiftAllocOffset, entry);
  assert(result == 0);
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

void *SwiftZone::alloc_semi_optimized(AllocIndex idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  AllocCacheEntry r = getAllocCacheEntry_slow(idx);
  if (r) {
    setAllocCacheEntry_slow(idx, r->next);
    return r;
  }
  return globalAlloc(idx, 0);
}

void *SwiftZone::alloc_gmalloc(AllocIndex idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  size_t size = indexToSize(idx);
  return malloc(size);
}

void *SwiftZone::tryAlloc_semi_optimized(AllocIndex idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  AllocCacheEntry r = getAllocCacheEntry_slow(idx);
  if (r) {
    setAllocCacheEntry_slow(idx, r->next);
    return r;
  }
  return globalAlloc(idx, SWIFT_TRYALLOC);
}

void *SwiftZone::tryAlloc_gmalloc(AllocIndex idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  size_t size = indexToSize(idx);
  return malloc(size);
}

void SwiftZone::dealloc_semi_optimized(void *ptr, AllocIndex idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  auto cur = static_cast<AllocCacheEntry>(ptr);
  AllocCacheEntry prev = getAllocCacheEntry_slow(idx);
  assert(cur != prev && "trivial double free!");
  cur->next = prev;
  setAllocCacheEntry_slow(idx, cur);
}

void SwiftZone::dealloc_gmalloc(void *ptr, AllocIndex idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  free(ptr);
}

// !SWIFT_HAVE_FAST_ENTRY_POINTS
#endif

static size_t arenaHash(const void *pointer) {
  return (size_t)pointer >> 16;
}
static size_t hugeAllocationHash(const void *pointer) {
  return (size_t)pointer >> 12;
}

class Arena;
static HeapMap<Arena, arenaHash> arenas;
static HeapMap<size_t, hugeAllocationHash> hugeAllocations;

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



static void _swift_zone_initImpl() {
  assert(sizeof(pthread_key_t) == sizeof(long));
  malloc_zone_register(&zoneShims);
  pthread_key_t key, prev_key;
  int r = pthread_key_create(&key, SwiftZone::threadExitCleanup);
  assert(r == 0);
  (void)r;
  prev_key = key;
  _swiftAllocOffset = key;
  for (unsigned i = 0; i < ALLOC_CACHE_COUNT; i++) {
    int r = pthread_key_create(&key, SwiftZone::threadExitCleanup);
    assert(r == 0);
    (void)r;
    assert(key == prev_key + 1);
    prev_key = key;
  }

  AllocCacheEntry magic = (AllocCacheEntry)1804289383; // result from random()
  setAllocCacheEntry_slow(0, magic);
  if (getAllocCacheEntry(0) != magic) {
    _swift_alloc = SwiftZone::alloc_semi_optimized;
    _swift_tryAlloc = SwiftZone::tryAlloc_semi_optimized;
    _swift_dealloc = SwiftZone::dealloc_semi_optimized;
  }
  setAllocCacheEntry_slow(0, NULL);
  const char *dyldMagic = getenv("DYLD_INSERT_LIBRARIES");
  if (dyldMagic && strstr(dyldMagic, "libgmalloc")) {
    _swift_alloc = SwiftZone::alloc_gmalloc;
    _swift_tryAlloc = SwiftZone::tryAlloc_gmalloc;
    _swift_slowAlloc = SwiftZone::slowAlloc_gmalloc;
    _swift_dealloc = SwiftZone::dealloc_gmalloc;
    _swift_slowDealloc = SwiftZone::slowDealloc_gmalloc;
  }
}
void swift::_swift_zone_init() {
  static pthread_once_t once = PTHREAD_ONCE_INIT;
  int r = pthread_once(&once, _swift_zone_initImpl);
  assert(r == 0);
}
SwiftZone::SwiftZone() {
  _swift_zone_init();
}

size_t swift::_swift_zone_size(malloc_zone_t *zone, const void *pointer) {
  SwiftZone::readLock();
  void *ptr = (void *)((unsigned long)pointer & ~arenaMask);
  size_t value = 0;
  auto it = arenas.find(ptr);
  if (it) {
    value = it->second.byteSize;
  } else {
    auto it2 = hugeAllocations.find(pointer);
    if (it2) {
      value = it2->second;
    }
  }
  SwiftZone::readUnlock();
  return value;
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
  auto newPointer = swift::_swift_zone_malloc(zone, size);
  auto oldSize = swift::_swift_zone_size(zone, pointer);
  memcpy(newPointer, pointer, size < oldSize ? size : oldSize);
  swift::_swift_zone_free(zone, pointer);
  return newPointer;
}

void swift::_swift_zone_destroy(malloc_zone_t *zone) {
  swift::crash("The Swift heap cannot be destroyed");
}

kern_return_t
_swift_zone_enumerator(task_t task, void *, unsigned type_mask,
                       vm_address_t zone_address, memory_reader_t reader,
                       vm_range_recorder_t recorder) {
  swift::crash("Swift Zone enumerator is unimplemented");
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
  swift::crash("Swift Zone log is unimplemented");
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

static inline AllocIndex sizeToIndex(size_t size) {
  assert(size != 0); // pass '1' if you want a placeholder object
  --size;
  // we could do a table based lookup if we think it worthwhile
#ifdef __LP64__
  if      (size <   0x80) return (size >> 3) +  0x0;
  else if (size <  0x100) return (size >> 4) +  0x8;
  else if (size <  0x200) return (size >> 5) + 0x10;
  else if (size <  0x400) return (size >> 6) + 0x18;
  else if (size <  0x800) return (size >> 7) + 0x20;
  else if (size < 0x1000) return (size >> 8) + 0x28;
#else
  if      (size <   0x40) return (size >> 2) +  0x0;
  else if (size <   0x80) return (size >> 3) +  0x8;
  else if (size <  0x100) return (size >> 4) + 0x10;
  else if (size <  0x200) return (size >> 5) + 0x18;
  else if (size <  0x400) return (size >> 6) + 0x20;
  else if (size <  0x800) return (size >> 7) + 0x28;
  else if (size < 0x1000) return (size >> 8) + 0x30;
#endif
  return badAllocIndex;
}

size_t swift::_swift_indexToSize(unsigned idx) {
  return indexToSize(idx);
}

int swift::_swift_sizeToIndex(size_t size) {
  return sizeToIndex(size);
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
  SwiftZone::dealloc_semi_optimized(tmp, idx);
}

void *SwiftZone::slowAlloc_optimized(size_t size, uintptr_t flags) {
  AllocIndex idx = sizeToIndex(size);
  if (idx == badAllocIndex) {
    // large allocations
    void *r = mmapWrapper(size, /*huge*/ true);
    if (r == MAP_FAILED) {
      if (flags & SWIFT_TRYALLOC) {
        return NULL;
      }
      swift::crash("Address space exhausted");
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

void *SwiftZone::slowAlloc_gmalloc(size_t size, uintptr_t flags) {
  return malloc(size);
}

void SwiftZone::slowDealloc_optimized(void *ptr, size_t bytes) {
  if (bytes == 0) {
    bytes = zoneShims.size(NULL, ptr);
  }
  assert(bytes != 0);
  AllocIndex idx = sizeToIndex(bytes);
  if (idx == badAllocIndex) {
    auto it2 = hugeAllocations.find(ptr);
    assert(it2);
    int r = munmap(const_cast<void *>(it2->first), it2->second);
    assert(r != -1);
    hugeAllocations.erase(it2);
    return;
  }

  dealloc_semi_optimized(ptr, idx);
}

void SwiftZone::slowDealloc_gmalloc(void *ptr, size_t bytes) {
  return free(ptr);
}

void SwiftZone::threadExitCleanup(void *arg) {
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
  swift::crash("Swift Zone memory reader not implemented");
}

void _swift_vm_range_recorder(task_t task, void *, unsigned type,
                              vm_range_t *, unsigned) {
    /* given a task and context, "records" the specified addresses */
}

static size_t boolHash(const void *pointer) {
  return (size_t)pointer;
}

static void
enumerateBlocks(std::function<void(const void *, size_t)> func) {
  // XXX switch to a bitmap or a set
  // FIXME scan other threads
  HeapMap<bool, boolHash> unusedBlocks;
  for (unsigned i = 0; i < ALLOC_CACHE_COUNT; i++) {
    for (AllocCacheEntry pointer = globalCache[i]; pointer;
         pointer = pointer->next) {
      unusedBlocks.insert(std::pair<void *, bool>(pointer, true));
    }
    for (AllocCacheEntry pointer = getAllocCacheEntry_slow(i); pointer;
         pointer = pointer->next) {
      unusedBlocks.insert(std::pair<void *, bool>(pointer, true));
    }
  }

  SwiftZone::readLock();
  arenas.forEach([&](const void *key, Arena arena) {
    size_t count = arenaSize / arena.byteSize;
    for (size_t i = 0; i < count; i += arena.byteSize) {
      auto pointer = (const void *)((uint8_t *)arena.base + i);
      if (!unusedBlocks[pointer]) {
        func(pointer, arena.byteSize);
      }
    }
  });
  hugeAllocations.forEach(func);
  SwiftZone::readUnlock();
}

void _swift_zone_print(malloc_zone_t *zone, boolean_t verbose) {
  enumerateBlocks([&](const void *pointer, size_t size) {
    swift::crash("Swift Zone print not implemented");
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
  hugeAllocations.forEach([&] (const void *key, size_t size) {
    statistics->size_allocated += size;
  });
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

auto swift::_swift_alloc = SwiftZone::alloc_optimized;
auto swift::_swift_tryAlloc = SwiftZone::tryAlloc_optimized;
auto swift::_swift_slowAlloc = SwiftZone::slowAlloc_optimized;
auto swift::_swift_dealloc = SwiftZone::dealloc_optimized;
auto swift::_swift_slowDealloc = SwiftZone::slowDealloc_optimized;
