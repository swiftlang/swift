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

#include "swift/Runtime/Alloc.h"
#include "swift/Runtime/Metadata.h"
#include "llvm/Support/MathExtras.h"
#include "Private.h"
#include <cassert>
#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <pthread.h>

using namespace swift;

namespace swift {
  extern "C" HeapObject *swift_tryRetain(HeapObject *object);
};


HeapObject *
swift::swift_allocObject(HeapMetadata const *metadata,
                         size_t requiredSize,
                         size_t requiredAlignmentMask) {
  HeapObject *object;

  for (;;) {
    object = reinterpret_cast<HeapObject *>(
      calloc(1, llvm::RoundUpToAlignment(requiredSize,
                                         requiredAlignmentMask+1)));
    if (object) {
      break;
    }
    if (pthread_is_threaded_np() == 0)
      abort();
    sleep(1); // XXX FIXME -- Enqueue this thread and resume after free()
  }
  object->metadata = metadata;
  object->refCount = RC_INTERVAL;
  object->weakRefCount = WRC_INTERVAL;
  return object;
}

/// \brief Allocate a reference-counted object on the heap that
/// occupies <size> bytes of maximally-aligned storage.  The object is
/// uninitialized except for its header.
extern "C" HeapObject* swift_bufferAllocate(
  HeapMetadata const* bufferType, int64_t size)
{
  return swift::swift_allocObject(bufferType, size, 0);
}

extern "C" int64_t swift_bufferHeaderSize() { return sizeof(HeapObject); }

/// A do-nothing destructor for POD metadata.
static void destroyPOD(HeapObject*) {}

/// Heap metadata for POD allocations.
static const FullMetadata<HeapMetadata> PODHeapMetadata{
  HeapMetadataHeader{{destroyPOD}, {nullptr}},
  HeapMetadata{{MetadataKind::HeapLocalVariable}}
};

namespace {
  /// Header for a POD allocation created by swift_allocPOD.
  struct PODBox : HeapObject {
    /// The size of the complete allocation.
    size_t allocatedSize;
    
    /// Returns the offset in bytes from the address of the header of a POD
    /// allocation with the given size and alignment.
    static size_t getValueOffset(size_t size, size_t alignMask) {
      return llvm::RoundUpToAlignment(sizeof(PODBox), alignMask+1);
    }
  };
}

BoxPair
swift::swift_allocPOD(size_t dataSize, size_t dataAlignmentMask) {
  // Allocate the heap object.
  size_t valueOffset = PODBox::getValueOffset(dataSize, dataAlignmentMask);
  size_t size = valueOffset + dataSize;
  auto *obj = swift_allocObject(&PODHeapMetadata, size, dataAlignmentMask);
  // Initialize the header for the box.
  static_cast<PODBox*>(obj)->allocatedSize = size;
  // Get the address of the value inside.
  auto *data = reinterpret_cast<char*>(obj) + valueOffset;
  return {obj, reinterpret_cast<OpaqueValue*>(data)};
}

namespace {
  /// Header for a generic box created by swift_allocBox in the worst case.
  struct GenericBox : HeapObject {
    /// The type of the value inside the box.
    Metadata const *type;
    
    /// Returns the offset in bytes from the address of the box header to the
    /// address of the value inside the box.
    size_t getValueOffset() const {
      return getValueOffset(type);
    }

    /// Returns the offset in bytes from the address of the box header for
    /// a box containing a value of the given type to the address of the value
    /// inside the box.
    static size_t getValueOffset(Metadata const *type) {
      return llvm::RoundUpToAlignment(sizeof(GenericBox),
                                  type->getValueWitnesses()->getAlignment());
    }

    /// Returns the size of the allocation for the box, including the header
    /// and the value.
    size_t getAllocatedSize() const {
      return getAllocatedSize(type);
    }
    
    /// Returns the size of the allocation that would be made for a box
    /// containing a value of the given type, including the header and the value.
    static size_t getAllocatedSize(Metadata const *type) {
      return getValueOffset(type) + type->getValueWitnesses()->stride;
    }

    /// Returns an opaque pointer to the value inside the box.
    OpaqueValue *getValuePointer() {
      char *p = reinterpret_cast<char*>(this) + getValueOffset();
      return reinterpret_cast<OpaqueValue*>(p);
    }

    /// Returns an opaque pointer to the value inside the box.
    OpaqueValue const *getValuePointer() const {
      auto *p = reinterpret_cast<char const *>(this) + getValueOffset();
      return reinterpret_cast<OpaqueValue const *>(p);
    }
  };
}

static inline size_t getBoxHeaderSize(size_t align) {
  return llvm::RoundUpToAlignment(sizeof(HeapObject) + sizeof(Metadata*),align);
}

/// Heap object destructor for a generic box allocated with swift_allocBox.
static void destroyGenericBox(HeapObject *o) {
  auto *box = static_cast<GenericBox*>(o);
  
  // Destroy the value inside the box.
  OpaqueValue *value = box->getValuePointer();
  box->type->getValueWitnesses()->destroy(value, box->type);
  
  // Deallocate the buffer.
  return swift_deallocObject(o, box->getAllocatedSize());
}

/// Generic heap metadata for generic allocBox allocations.
/// FIXME: It may be worth the tradeoff to instantiate type-specific
/// heap metadata at runtime.
static const FullMetadata<HeapMetadata> GenericBoxHeapMetadata{
  HeapMetadataHeader{{destroyGenericBox}, {nullptr}},
  HeapMetadata{{MetadataKind::HeapLocalVariable}}
};

BoxPair
swift::swift_allocBox(Metadata const *type) {
  // NB: Special cases here need to also be checked for and handled in
  // swift_deallocBox.
  
  // If the contained type is POD, perform a POD allocation.
  auto *vw = type->getValueWitnesses();
  if (vw->isPOD()) {
    return swift_allocPOD(vw->getSize(), vw->getAlignmentMask());
  }

  // Allocate the box.
  HeapObject *obj = swift_allocObject(&GenericBoxHeapMetadata,
                                      GenericBox::getAllocatedSize(type),
                                type->getValueWitnesses()->getAlignmentMask());
  // allocObject will initialize the heap metadata pointer and refcount for us.
  // We also need to store the type metadata between the header and the
  // value.
  auto *box = static_cast<GenericBox *>(obj);
  box->type = type;
  
  // Return the box and the value pointer.
  return {box, box->getValuePointer()};
}

void swift::swift_deallocBox(HeapObject *box, Metadata const *type) {
  // NB: Special cases here need to also be checked for and handled in
  // swift_allocBox.

  // First, we need to recover what the allocation size was.
  size_t allocatedSize;
  auto *vw = type->getValueWitnesses();
  if (vw->isPOD()) {
    // If the contained type is POD, use the POD allocation size.
    allocatedSize = static_cast<PODBox*>(box)->allocatedSize;
  } else {
    // Use the generic box size to deallocate the object.
    allocatedSize = GenericBox::getAllocatedSize(type);
  }

  // Deallocate the box.
  swift_deallocObject(box, allocatedSize);
}

void swift::swift_deallocPOD(HeapObject *obj) {
  swift_deallocObject(obj, static_cast<PODBox*>(obj)->allocatedSize);
}

// Forward-declare this, but define it after swift_release.
extern "C" LLVM_LIBRARY_VISIBILITY
void _swift_release_slow(HeapObject *object)
  __attribute__((noinline,used));

void
swift::swift_retain_noresult(HeapObject *object) {
  swift_retain(object);
}


// These are implemented in FastEntryPoints.s on some platforms.
#ifndef SWIFT_HAVE_FAST_ENTRY_POINTS

HeapObject *swift::swift_retain(HeapObject *object) {
  return _swift_retain(object);
}

void swift::swift_release(HeapObject *object) {
  if (object && ((object->refCount -= RC_INTERVAL) == 0)) {
    _swift_release_slow(object);
  }
}

void swift::swift_weakRetain(HeapObject *object) {
  if (!object) return;

  // FIXME: not thread-safe
  // FIXME: should check carry bit
  if ((object->weakRefCount += WRC_INTERVAL) < WRC_INTERVAL) {
    assert(0 && "weak retain count overflow");
  }
}

void swift::swift_weakRelease(HeapObject *object) {
  if (!object) return;

  // FIXME: not thread-safe
  uint32_t newCount = (object->weakRefCount -= WRC_INTERVAL);
  if (newCount >= (uint32_t)~WRC_INTERVAL) {
    assert(0 && "weak retain count underflow");
  }
  if (newCount == 0) {
    swift_slowRawDealloc(object, 0);
  }
}

HeapObject *swift::swift_tryRetain(HeapObject *object) {
  if (!object) return nullptr;

  // FIXME: not thread-safe
  uint32_t newCount = (object->refCount += RC_INTERVAL);
  assert(newCount >= RC_INTERVAL  &&  "retain count overflow");
  if (newCount & RC_DEALLOCATING_BIT) {
    object->refCount -= RC_INTERVAL;
    return nullptr;
  }
  return object;
}

#endif

void swift::swift_retainUnowned(HeapObject *object) {
  if (!object) return;
  assert((object->weakRefCount & WRC_MASK) &&
         "object is not currently weakly retained");

  // FIXME: this test should be atomic with the retain
  if (object->refCount & RC_DEALLOCATING_BIT)
    _swift_abortRetainUnowned(object);
  swift_retain(object);
}

// Declared extern "C" LLVM_LIBRARY_VISIBILITY above.
void _swift_release_slow(HeapObject *object) {
  // Bump the retain count so that retains/releases that occur during the
  // destructor don't recursively destroy the object.
  swift_retain_noresult(object);
  asFullMetadata(object->metadata)->destroy(object);
}

void swift::swift_deallocObject(HeapObject *object, size_t allocatedSize) {
#ifdef SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS
  memset_pattern8(object, "\xAB\xAD\x1D\xEA\xF4\xEE\xD0\bB9",
                  allocatedSize);
#endif
  swift_weakRelease(object);
}


// Plain old memory allocation

__attribute__((noinline,used))
static void *
_swift_slowAlloc_fixup(AllocIndex idx, uint64_t flags)
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

extern "C" LLVM_LIBRARY_VISIBILITY
void _swift_refillThreadAllocCache(AllocIndex idx, uint64_t flags) {
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

void *swift::swift_slowAlloc(size_t bytes, uint64_t flags) {
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
  return (AllocCacheEntry *)_os_tsd_get_direct(idx + ALLOC_CACHE_START);
}

static void
setAllocCacheEntry(unsigned long idx, AllocCacheEntry *entry) {
  assert(idx < ALLOC_CACHE_COUNT);
  _os_tsd_set_direct(idx + ALLOC_CACHE_START, entry);
}

static AllocCacheEntry *
getRawAllocCacheEntry(unsigned long idx) {
  assert(idx < ALLOC_CACHE_COUNT);
  return (AllocCacheEntry *)_os_tsd_get_direct(idx + ALLOC_RAW_CACHE_START);
}

static void
setRawAllocCacheEntry(unsigned long idx, AllocCacheEntry *entry) {
  assert(idx < ALLOC_CACHE_COUNT);
  _os_tsd_set_direct(idx + ALLOC_RAW_CACHE_START, entry);
}

void *swift::swift_alloc(AllocIndex idx) {
  AllocCacheEntry *r = getAllocCacheEntry(idx);
  if (r) {
    setAllocCacheEntry(idx, r->next);
    return r;
  }
  return _swift_slowAlloc_fixup(idx, 0);
}

void *swift::swift_rawAlloc(AllocIndex idx) {
  AllocCacheEntry *r = getRawAllocCacheEntry(idx);
  if (r) {
    setRawAllocCacheEntry(idx, r->next);
    return r;
  }
  return _swift_slowAlloc_fixup(idx, SWIFT_RAWALLOC);
}

void *swift::swift_tryAlloc(AllocIndex idx) {
  AllocCacheEntry *r = getAllocCacheEntry(idx);
  if (r) {
    setAllocCacheEntry(idx, r->next);
    return r;
  }
  return _swift_slowAlloc_fixup(idx, SWIFT_TRYALLOC);
}

void *swift::swift_tryRawAlloc(AllocIndex idx) {
  AllocCacheEntry *r = getRawAllocCacheEntry(idx);
  if (r) {
    setRawAllocCacheEntry(idx, r->next);
    return r;
  }
  return _swift_slowAlloc_fixup(idx, SWIFT_TRYALLOC|SWIFT_RAWALLOC);
}

void swift::swift_dealloc(void *ptr, AllocIndex idx) {
  auto cur = static_cast<AllocCacheEntry *>(ptr);
  AllocCacheEntry *prev = getAllocCacheEntry(idx);
  cur->next = prev;
  setAllocCacheEntry(idx, cur);
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

  swift_dealloc(ptr, idx);
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

/// This is a function that is opaque to the optimizer.  It is called to ensure
/// that an object is alive at least until that time.
extern "C" void swift_keepAlive(HeapObject *object) {
  // Parameters are passed at +1 reference count.  We need to release to
  // balance.
  swift_release(object);
}

/// \brief Lets us know whether the given Object is referenced
/// more than once.  This information is useful for implementing
/// copy-on-write in Swift.
extern "C" bool swift_isUniquelyReferenced(HeapObject *object) {

  // Sometimes we have a NULL "owner" object, e.g. because the data
  // being referenced (usually via UnsafePointer<T>) has infinite
  // lifetime, or lifetime managed outside the Swift object system.
  // In these cases we have to assume the data is shared among
  // multiple references, and needs to be copied before modification.
  if (object == nullptr) {
    return false;
  }
  
  bool result = (object->refCount <= 2 * RC_INTERVAL);
  swift_release(object);

  return result;
}

void swift::swift_weakInit(WeakReference *ref, HeapObject *value) {
  ref->Value = value;
  swift_weakRetain(value);
}

void swift::swift_weakAssign(WeakReference *ref, HeapObject *newValue) {
  swift_weakRetain(newValue);
  auto oldValue = ref->Value;
  ref->Value = newValue;
  swift_weakRelease(oldValue);
}

HeapObject *swift::swift_weakLoadStrong(WeakReference *ref) {
  auto object = ref->Value;
  if (object == nullptr) return nullptr;
  if (object->refCount & RC_DEALLOCATING_BIT) {
    swift_weakRelease(object);
    ref->Value = nullptr;
    return nullptr;
  }
  return swift_tryRetain(object);
}

HeapObject *swift::swift_weakTakeStrong(WeakReference *ref) {
  auto result = swift_weakLoadStrong(ref);
  swift_weakDestroy(ref);
  return result;
}

void swift::swift_weakDestroy(WeakReference *ref) {
  auto tmp = ref->Value;
  ref->Value = nullptr;
  swift_weakRelease(tmp);
}

void swift::swift_weakCopyInit(WeakReference *dest, WeakReference *src) {
  auto object = src->Value;
  if (object == nullptr) {
    dest->Value = nullptr;
  } else if (object->refCount & RC_DEALLOCATING_BIT) {
    src->Value = nullptr;
    dest->Value = nullptr;
    swift_weakRelease(object);
  } else {
    dest->Value = object;
    swift_weakRetain(object);
  }
}

void swift::swift_weakTakeInit(WeakReference *dest, WeakReference *src) {
  auto object = src->Value;
  dest->Value = object;
  if (object != nullptr && object->refCount & RC_DEALLOCATING_BIT) {
    dest->Value = nullptr;
    swift_weakRelease(object);
  }
}

void swift::swift_weakCopyAssign(WeakReference *dest, WeakReference *src) {
  if (auto object = dest->Value) {
    swift_weakRelease(object);
  }
  swift_weakCopyInit(dest, src);
}

void swift::swift_weakTakeAssign(WeakReference *dest, WeakReference *src) {
  if (auto object = dest->Value) {
    swift_weakRelease(object);
  }
  swift_weakTakeInit(dest, src);
}

void swift::_swift_abortRetainUnowned(const void *object) {
  fprintf(stderr, "attempting to retain deallocated object at %p", object);
  abort();
}
