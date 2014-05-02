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
#include "llvm/Support/MathExtras.h"
#include "Private.h"
#include "Debug.h"
#include <malloc/malloc.h>
#include <cassert>
#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <unistd.h>

using namespace swift;

namespace swift {
  extern "C" HeapObject *swift_tryRetain(HeapObject *object);
};


HeapObject *
swift::swift_allocObject(HeapMetadata const *metadata,
                         size_t requiredSize,
                         size_t requiredAlignmentMask) {
  return _swift_allocObject(metadata, requiredSize, requiredAlignmentMask);
}
static HeapObject *
_swift_allocObject_(HeapMetadata const *metadata, size_t requiredSize,
                    size_t requiredAlignmentMask) {
  // llvm::RoundUpToAlignment(size, mask + 1) generates terrible code
  auto size = (requiredSize + requiredAlignmentMask) & ~requiredAlignmentMask;
  auto object = reinterpret_cast<HeapObject *>(swift_slowAlloc(size, 0));
  object->metadata = metadata;
  object->refCount = RC_INTERVAL;
  object->weakRefCount = WRC_INTERVAL;
  return object;
}
auto swift::_swift_allocObject = _swift_allocObject_;

/// \brief Allocate a reference-counted object on the heap that
/// occupies <size> bytes of maximally-aligned storage.  The object is
/// uninitialized except for its header.
extern "C" HeapObject* swift_bufferAllocate(
  HeapMetadata const* bufferType, intptr_t size)
{
  return swift::swift_allocObject(bufferType, size, 0);
}

extern "C" intptr_t swift_bufferHeaderSize() { return sizeof(HeapObject); }

/// A do-nothing destructor for POD metadata.
static void destroyPOD(HeapObject *o);

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
      // llvm::RoundUpToAlignment(size, mask + 1) generates terrible code
      return (sizeof(PODBox) + alignMask) & ~alignMask;
    }
  };
}

static void destroyPOD(HeapObject *o) {
  auto box = static_cast<PODBox*>(o);
  // Deallocate the buffer.
  return swift_deallocObject(box, box->allocatedSize);
}

BoxPair::Return
swift::swift_allocPOD(size_t dataSize, size_t dataAlignmentMask) {
  // Allocate the heap object.
  size_t valueOffset = PODBox::getValueOffset(dataSize, dataAlignmentMask);
  size_t size = valueOffset + dataSize;
  auto *obj = swift_allocObject(&PODHeapMetadata, size, dataAlignmentMask);
  // Initialize the header for the box.
  static_cast<PODBox*>(obj)->allocatedSize = size;
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
  return swift_deallocObject(o, box->getAllocatedSize());
}

/// Generic heap metadata for generic allocBox allocations.
/// FIXME: It may be worth the tradeoff to instantiate type-specific
/// heap metadata at runtime.
static const FullMetadata<HeapMetadata> GenericBoxHeapMetadata{
  HeapMetadataHeader{{destroyGenericBox}, {nullptr}},
  HeapMetadata{{MetadataKind::HeapLocalVariable}}
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
                                type->getValueWitnesses()->getAlignmentMask());
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
static HeapObject *_swift_retain_(HeapObject *object) {
  return _swift_retain_inlined(object);
}
auto swift::_swift_retain = _swift_retain_;

void swift::swift_release(HeapObject *object) {
  return _swift_release(object);
}
static void _swift_release_(HeapObject *object) {
  if (object && (__sync_sub_and_fetch(&object->refCount, RC_INTERVAL) == 0)) {
    _swift_release_slow(object);
  }
}
auto swift::_swift_release = _swift_release_;

size_t swift::swift_retainCount(HeapObject *object) {
  return object->refCount >> RC_INTERVAL_SHIFT;
}

void swift::swift_weakRetain(HeapObject *object) {
  if (!object) return;

  // FIXME: should check carry bit
  if (__sync_add_and_fetch(&object->weakRefCount, WRC_INTERVAL) < WRC_INTERVAL){
    assert(0 && "weak retain count overflow");
  }
}

void swift::swift_weakRelease(HeapObject *object) {
  if (!object) return;

  uint32_t newCount = __sync_sub_and_fetch(&object->weakRefCount, WRC_INTERVAL);
  if (newCount >= (uint32_t)~WRC_INTERVAL) {
    assert(0 && "weak retain count underflow");
  }
  if (newCount == 0) {
    swift_slowDealloc(object, 0);
  }
}

HeapObject *swift::swift_tryRetain(HeapObject *object) {
  return _swift_tryRetain(object);
}
static HeapObject *_swift_tryRetain_(HeapObject *object) {
  if (!object) return nullptr;

  uint32_t newCount = __sync_add_and_fetch(&object->refCount, RC_INTERVAL);
  assert(newCount >= RC_INTERVAL  &&  "retain count overflow");
  if (newCount & RC_DEALLOCATING_BIT) {
    __sync_fetch_and_sub(&object->refCount, RC_INTERVAL);
    return nullptr;
  }
  return object;
}
auto swift::_swift_tryRetain = _swift_tryRetain_;

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
  assert(object->refCount == RC_INTERVAL);
#ifdef SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS
  memset_pattern8((uint8_t *)object + sizeof(HeapObject),
                  "\xAB\xAD\x1D\xEA\xF4\xEE\xD0\bB9",
                  allocatedSize - sizeof(HeapObject));
#endif
  swift_weakRelease(object);
}


/// This is a function that is opaque to the optimizer.  It is called to ensure
/// that an object is alive at least until that time.
extern "C" void swift_keepAlive(HeapObject *object) {
  // Parameters are passed at +1 reference count.  We need to release to
  // balance.
  swift_release(object);
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
  (void)object;
  swift::crash("attempted to retain deallocated object");
}
