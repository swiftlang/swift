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

#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/InstrumentsSupport.h"
#include "swift/Runtime/Heap.h"
#include "swift/Runtime/Metadata.h"
#include "swift/ABI/System.h"
#include "llvm/Support/MathExtras.h"
#include "Lazy.h"
#include "MetadataCache.h"
#include "Private.h"
#include "Debug.h"
#include <algorithm>
#include <cassert>
#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include "../SwiftShims/RuntimeShims.h"
#if SWIFT_OBJC_INTEROP
# include <objc/objc-runtime.h>
#endif
#if SWIFT_RUNTIME_ENABLE_DTRACE
# include "SwiftRuntimeDTraceProbes.h"
#else
# define SWIFT_ALLOCATEOBJECT()
# define SWIFT_DEALLOCATEOBJECT()
# define SWIFT_RELEASE()
# define SWIFT_RETAIN()
#endif
#include "Leaks.h"

using namespace swift;

HeapObject *
swift::swift_allocObject(HeapMetadata const *metadata,
                         size_t requiredSize,
                         size_t requiredAlignmentMask) {
  SWIFT_ALLOCATEOBJECT();
  return _swift_allocObject(metadata, requiredSize, requiredAlignmentMask);
}
static HeapObject *
_swift_allocObject_(HeapMetadata const *metadata, size_t requiredSize,
                    size_t requiredAlignmentMask) {
  assert(isAlignmentMask(requiredAlignmentMask));
  auto object = reinterpret_cast<HeapObject *>(
                  swift_slowAlloc(requiredSize, requiredAlignmentMask));
  // FIXME: this should be a placement new but that adds a null check
  object->metadata = metadata;
  object->refCount.init();
  object->weakRefCount.init();

  // If leak tracking is enabled, start tracking this object.
  SWIFT_LEAKS_START_TRACKING_OBJECT(object);

  return object;
}
auto swift::_swift_allocObject = _swift_allocObject_;

/// \brief Allocate a reference-counted object on the heap that
/// occupies <size> bytes of maximally-aligned storage.  The object is
/// uninitialized except for its header.
extern "C" HeapObject* swift_bufferAllocate(
  HeapMetadata const* bufferType, size_t size, size_t alignMask)
{
  return swift::swift_allocObject(bufferType, size, alignMask);
}

extern "C" intptr_t swift_bufferHeaderSize() { return sizeof(HeapObject); }

/// A do-nothing destructor for POD metadata.
static void destroyPOD(HeapObject *o);

/// Heap metadata for POD allocations.
static const FullMetadata<HeapMetadata> PODHeapMetadata{
  HeapMetadataHeader{{destroyPOD}, {nullptr}},
  HeapMetadata{Metadata{MetadataKind::HeapLocalVariable}}
};

namespace {
  /// Header for a POD allocation created by swift_allocPOD.
  struct PODBox : HeapObject {
    /// The size of the complete allocation.
    size_t allocatedSize;

    /// The required alignment of the complete allocation.
    size_t allocatedAlignMask;
    
    /// Returns the offset in bytes from the address of the header of a POD
    /// allocation with the given size and alignment.
    static size_t getValueOffset(size_t size, size_t alignMask) {
      // llvm::RoundUpToAlignment(size, mask + 1) generates terrible code
      return (sizeof(PODBox) + alignMask) & ~alignMask;
    }
  };
}

static void destroyPOD(HeapObject *o) {
  auto box = static_cast<PODBox*>(o);
  // Deallocate the buffer.
  return swift_deallocObject(box, box->allocatedSize, box->allocatedAlignMask);
}

BoxPair::Return
swift::swift_allocPOD(size_t dataSize, size_t dataAlignmentMask) {
  assert(isAlignmentMask(dataAlignmentMask));
  // Allocate the heap object.
  size_t valueOffset = PODBox::getValueOffset(dataSize, dataAlignmentMask);
  size_t size = valueOffset + dataSize;
  size_t alignMask = std::max(dataAlignmentMask, alignof(HeapObject) - 1);
  auto *obj = swift_allocObject(&PODHeapMetadata, size, alignMask);
  // Initialize the header for the box.
  static_cast<PODBox*>(obj)->allocatedSize = size;
  static_cast<PODBox*>(obj)->allocatedAlignMask = alignMask;
  // Get the address of the value inside.
  auto *data = reinterpret_cast<char*>(obj) + valueOffset;
  return BoxPair{obj, reinterpret_cast<OpaqueValue*>(data)};
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

    size_t getAllocatedAlignMask() const {
      return getAllocatedAlignMask(type);
    }

    /// Returns the alignment mask of the allocation that would be
    /// made for a box containing a value of the given type, including
    /// the header and the value.
    static size_t getAllocatedAlignMask(Metadata const *type) {
      return std::max(type->getValueWitnesses()->getAlignmentMask(),
                      alignof(GenericBox) - 1);
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

/// Heap object destructor for a generic box allocated with swift_allocBox.
static void destroyGenericBox(HeapObject *o) {
  auto *box = static_cast<GenericBox*>(o);
  
  // Destroy the value inside the box.
  OpaqueValue *value = box->getValuePointer();
  box->type->getValueWitnesses()->destroy(value, box->type);
  
  // Deallocate the buffer.
  return swift_deallocObject(o, box->getAllocatedSize(),
                             box->getAllocatedAlignMask());
}

/// Generic heap metadata for generic allocBox allocations.
/// FIXME: It may be worth the tradeoff to instantiate type-specific
/// heap metadata at runtime.
static const FullMetadata<HeapMetadata> GenericBoxHeapMetadata{
  HeapMetadataHeader{{destroyGenericBox}, {nullptr}},
  HeapMetadata{Metadata{MetadataKind::HeapLocalVariable}}
};

BoxPair::Return
swift::swift_allocBox(Metadata const *type) {
  return _swift_allocBox(type);
}
static BoxPair::Return _swift_allocBox_(Metadata const *type) {
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
                                      GenericBox::getAllocatedAlignMask(type));
  // allocObject will initialize the heap metadata pointer and refcount for us.
  // We also need to store the type metadata between the header and the
  // value.
  auto *box = static_cast<GenericBox *>(obj);
  box->type = type;
  
  // Return the box and the value pointer.
  return BoxPair{box, box->getValuePointer()};
}
auto swift::_swift_allocBox = _swift_allocBox_;

void swift::swift_deallocBox(HeapObject *box, Metadata const *type) {
  // NB: Special cases here need to also be checked for and handled in
  // swift_allocBox.

  // First, we need to recover what the allocation size was.
  size_t allocatedSize, allocatedAlignMask;
  auto *vw = type->getValueWitnesses();
  if (vw->isPOD()) {
    // If the contained type is POD, use the POD allocation size.
    allocatedSize = static_cast<PODBox*>(box)->allocatedSize;
    allocatedAlignMask = static_cast<PODBox*>(box)->allocatedAlignMask;
  } else {
    // Use the generic box size to deallocate the object.
    allocatedSize = GenericBox::getAllocatedSize(type);
    allocatedAlignMask = GenericBox::getAllocatedAlignMask(type);
  }

  // Deallocate the box.
  swift_deallocObject(box, allocatedSize, allocatedAlignMask);
}

void swift::swift_deallocPOD(HeapObject *obj) {
  swift_deallocObject(obj, static_cast<PODBox*>(obj)->allocatedSize,
                      static_cast<PODBox*>(obj)->allocatedAlignMask);
}

namespace {
/// Heap metadata for runtime-instantiated generic boxes.
struct GenericBox2HeapMetadata : public HeapMetadata {
  /// The type inside the box.
  const Metadata *BoxedType;

  constexpr GenericBox2HeapMetadata(MetadataKind kind,
                                    const Metadata *boxedType)
    : HeapMetadata{kind}, BoxedType(boxedType)
  {}

  unsigned getHeaderOffset() const {
    // Round up the header size to alignment.
    unsigned alignMask = BoxedType->getValueWitnesses()->getAlignmentMask();
    return (sizeof(HeapObject) + alignMask) & ~alignMask;
  }

  /// Project the value out of a box of this type.
  OpaqueValue *project(HeapObject *box) const {
    auto bytes = reinterpret_cast<char*>(box);
    return reinterpret_cast<OpaqueValue *>(bytes + getHeaderOffset());
  }

  /// Get the allocation size of this box.
  unsigned getAllocSize() const {
    return getHeaderOffset() + BoxedType->getValueWitnesses()->getSize();
  }

  /// Get the allocation alignment of this box.
  unsigned getAllocAlignMask() const {
    // Heap allocations are at least pointer aligned.
    return BoxedType->getValueWitnesses()->getAlignmentMask()
      | (alignof(void*) - 1);
  }
};

/// Heap object destructor for a generic box allocated with swift_allocBox2.
static void destroyGenericBox2(HeapObject *o) {
  auto metadata = static_cast<const GenericBox2HeapMetadata *>(o->metadata);
  // Destroy the object inside.
  auto *value = metadata->project(o);
  metadata->BoxedType->vw_destroy(value);

  // Deallocate the box.
  swift_deallocObject(o, metadata->getAllocSize(),
                      metadata->getAllocAlignMask());
}

class BoxCacheEntry : public CacheEntry<BoxCacheEntry> {
public:
  FullMetadata<GenericBox2HeapMetadata> Metadata;

  BoxCacheEntry(size_t numArguments)
    : Metadata{HeapMetadataHeader{{destroyGenericBox2}, {nullptr}},
               GenericBox2HeapMetadata{MetadataKind::HeapGenericLocalVariable,
                                       nullptr}} {
    assert(numArguments == 1);
  }

  size_t getNumArguments() const {
    return 1;
  }

  FullMetadata<GenericBox2HeapMetadata> *getData() {
    return &Metadata;
  }
  const FullMetadata<GenericBox2HeapMetadata> *getData() const {
    return &Metadata;
  }
};

} // end anonymous namespace

static Lazy<MetadataCache<BoxCacheEntry>> Boxes;

BoxPair::Return
swift::swift_allocBox2(const Metadata *type) {
  return _swift_allocBox2(type);
}
static BoxPair::Return _swift_allocBox2_(const Metadata *type) {
  // Get the heap metadata for the box.
  auto &B = Boxes.get();
  const void *typeArg = type;
  auto entry = B.findOrAdd(&typeArg, 1, [&]() -> BoxCacheEntry* {
    // Create a new entry for the box.
    auto entry = BoxCacheEntry::allocate(B.getAllocator(), &typeArg, 1, 0);

    auto metadata = entry->getData();
    metadata->BoxedType = type;

    return entry;
  });

  auto metadata = entry->getData();

  // Allocate and project the box.
  auto allocation = swift_allocObject(metadata, metadata->getAllocSize(),
                                      metadata->getAllocAlignMask());
  auto projection = metadata->project(allocation);

  return BoxPair{allocation, projection};
}
auto swift::_swift_allocBox2 = _swift_allocBox2_;

void swift::swift_deallocBox2(HeapObject *o) {
  auto metadata = static_cast<const GenericBox2HeapMetadata *>(o->metadata);
  swift_deallocObject(o, metadata->getAllocSize(),
                      metadata->getAllocAlignMask());
}

OpaqueValue *swift::swift_projectBox2(HeapObject *o) {
  auto metadata = static_cast<const GenericBox2HeapMetadata *>(o->metadata);
  return metadata->project(o);
}

// Forward-declare this, but define it after swift_release.
extern "C" LLVM_LIBRARY_VISIBILITY
void _swift_release_dealloc(HeapObject *object)
  __attribute__((noinline,used));

void
swift::swift_retain_noresult(HeapObject *object) {
  swift_retain(object);
}

HeapObject *swift::swift_retain(HeapObject *object) {
  SWIFT_RETAIN();
  return _swift_retain(object);
}
static HeapObject *_swift_retain_(HeapObject *object) {
  return _swift_retain_inlined(object);
}
auto swift::_swift_retain = _swift_retain_;

void swift::swift_release(HeapObject *object) {
  SWIFT_RELEASE();
  return _swift_release(object);
}
static void _swift_release_(HeapObject *object) {
  if (object  &&  object->refCount.decrementShouldDeallocate()) {
    _swift_release_dealloc(object);
  }
}
auto swift::_swift_release = _swift_release_;

size_t swift::swift_retainCount(HeapObject *object) {
  return object->refCount.getCount();
}

void swift::swift_weakRetain(HeapObject *object) {
  if (!object) return;

  object->weakRefCount.increment();
}

void swift::swift_weakRelease(HeapObject *object) {
  if (!object) return;

  if (object->weakRefCount.decrementShouldDeallocate()) {
    // Only class objects can be weak-retained and weak-released.
    auto metadata = object->metadata;
    assert(metadata->isClassObject());
    auto classMetadata = static_cast<const ClassMetadata*>(metadata);
    assert(classMetadata->isTypeMetadata());
    swift_slowDealloc(object, classMetadata->getInstanceSize(),
                      classMetadata->getInstanceAlignMask());
  }
}

HeapObject *swift::swift_tryPin(HeapObject *object) {
  assert(object);

  // Try to set the flag.  If this succeeds, the caller will be
  // responsible for clearing it.
  if (object->refCount.tryIncrementAndPin()) {
    return object;
  }

  // If setting the flag failed, it's because it was already set.
  // Return nil so that the object will be deallocated later.
  return nullptr;
}

void swift::swift_unpin(HeapObject *object) {
  if (object && object->refCount.decrementAndUnpinShouldDeallocate()) {
    _swift_release_dealloc(object);
  }
}

HeapObject *swift::swift_tryRetain(HeapObject *object) {
  return _swift_tryRetain(object);
}
static HeapObject *_swift_tryRetain_(HeapObject *object) {
  if (!object) return nullptr;

  if (object->refCount.tryIncrement()) return object;
  else return nullptr;
}
auto swift::_swift_tryRetain = _swift_tryRetain_;

bool swift::swift_isDeallocating(HeapObject *object) {
  return _swift_isDeallocating(object);
}
static bool _swift_isDeallocating_(HeapObject *object) {
  if (!object) return false;
  return object->refCount.isDeallocating();
}
auto swift::_swift_isDeallocating = _swift_isDeallocating_;

void swift::swift_retainUnowned(HeapObject *object) {
  if (!object) return;
  assert(object->weakRefCount.getCount() &&
         "object is not currently weakly retained");

  if (! object->refCount.tryIncrement())
    _swift_abortRetainUnowned(object);
}

// Declared extern "C" LLVM_LIBRARY_VISIBILITY above.
void _swift_release_dealloc(HeapObject *object) {
  asFullMetadata(object->metadata)->destroy(object);
}

/// Perform the root -dealloc operation for a class instance.
void swift::_swift_deallocClassInstance(HeapObject *self) {
  auto metadata = self->metadata;
  assert(metadata->isClassObject());
  auto classMetadata = static_cast<const ClassMetadata*>(metadata);
  assert(classMetadata->isTypeMetadata());
  swift_deallocClassInstance(self, classMetadata->getInstanceSize(),
                             classMetadata->getInstanceAlignMask());
}

void swift::swift_deallocClassInstance(HeapObject *object, size_t allocatedSize,
                                       size_t allocatedAlignMask) {
#if SWIFT_OBJC_INTEROP
  // We need to let the ObjC runtime clean up any associated objects or weak
  // references associated with this object.
  objc_destructInstance((id)object);
#endif
  swift_deallocObject(object, allocatedSize, allocatedAlignMask);
}

#if !defined(__APPLE__)
static inline void memset_pattern8(void *b, const void *pattern8, size_t len) {
  char *ptr = static_cast<char *>(b);
  while (len >= 8) {
    memcpy(ptr, pattern8, 8);
    ptr += 8;
    len -= 8;
  }
  memcpy(ptr, pattern8, len);
}
#endif

void swift::swift_deallocObject(HeapObject *object, size_t allocatedSize,
                                size_t allocatedAlignMask) {
  SWIFT_DEALLOCATEOBJECT();
  assert(isAlignmentMask(allocatedAlignMask));
  assert(object->refCount.isDeallocating());
#ifdef SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS
  memset_pattern8((uint8_t *)object + sizeof(HeapObject),
                  "\xAB\xAD\x1D\xEA\xF4\xEE\xD0\bB9",
                  allocatedSize - sizeof(HeapObject));
#endif

  // If we are tracking leaks, stop tracking this object.
  SWIFT_LEAKS_STOP_TRACKING_OBJECT(object);

  // Drop the initial weak retain of the object.
  //
  // If the outstanding weak retain count is 1 (i.e. only the initial
  // weak retain), we can immediately call swift_slowDealloc.  This is
  // useful both as a way to eliminate an unnecessary atomic
  // operation, and as a way to avoid calling swift_weakRelease on an
  // object that might be a class object, which simplifies the logic
  // required in swift_weakRelease for determining the size of the
  // object.
  //
  // If we see that there is an outstanding weak retain of the object,
  // we need to fall back on swift_release, because it's possible for
  // us to race against a weak retain or a weak release.  But if the
  // outstanding weak retain count is 1, then anyone attempting to
  // increase the weak reference count is inherently racing against
  // deallocation and thus in undefined-behavior territory.  And
  // we can even do this with a normal load!  Here's why:
  //
  // 1. There is an invariant that, if the strong reference count
  // is > 0, then the weak reference count is > 1.
  //
  // 2. The above lets us say simply that, in the absence of
  // races, once a reference count reaches 0, there are no points
  // which happen-after where the reference count is > 0.
  //
  // 3. To not race, a strong retain must happen-before a point
  // where the strong reference count is > 0, and a weak retain
  // must happen-before a point where the weak reference count
  // is > 0.
  //
  // 4. Changes to either the strong and weak reference counts occur
  // in a total order with respect to each other.  This can
  // potentially be done with a weaker memory ordering than
  // sequentially consistent if the architecture provides stronger
  // ordering for memory guaranteed to be co-allocated on a cache
  // line (which the reference count fields are).
  //
  // 5. This function happens-after a point where the strong
  // reference count was 0.
  //
  // 6. Therefore, if a normal load in this function sees a weak
  // reference count of 1, it cannot be racing with a weak retain
  // that is not racing with deallocation:
  //
  //   - A weak retain must happen-before a point where the weak
  //     reference count is > 0.
  //
  //   - This function logically decrements the weak reference
  //     count.  If it is possible for it to see a weak reference
  //     count of 1, then at the end of this function, the
  //     weak reference count will logically be 0.
  //
  //   - There can be no points after that point where the
  //     weak reference count will be > 0.
  //
  //   - Therefore either the weak retain must happen-before this
  //     function, or this function cannot see a weak reference
  //     count of 1, or there is a race.
  //
  // Note that it is okay for there to be a race involving a weak
  // *release* which happens after the strong reference count drops to
  // 0.  However, this is harmless: if our load fails to see the
  // release, we will fall back on swift_weakRelease, which does an
  // atomic decrement (and has the ability to reconstruct
  // allocatedSize and allocatedAlignMask).
  if (object->weakRefCount.getCount() == 1) {
    swift_slowDealloc(object, allocatedSize, allocatedAlignMask);
  } else {
    swift_weakRelease(object);
  }
}

/// This is a function that is opaque to the optimizer.  It is called to ensure
/// that an object is alive at least until that time.
extern "C" void swift_fixLifetime(OpaqueValue* value) {
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
  if (object->refCount.isDeallocating()) {
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
  } else if (object->refCount.isDeallocating()) {
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
  if (object != nullptr && object->refCount.isDeallocating()) {
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
  (void)object;
  swift::crash("attempted to retain deallocated object");
}
