//===--- Alloc.h - Swift Language Allocation ABI ---------------*- C++ -*--===//
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
// Swift Allocation ABI
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_ALLOC_H
#define SWIFT_RUNTIME_ALLOC_H

#include <cstddef>
#include <cstdint>
#include "swift/Runtime/Config.h"

// Bring in the definition of HeapObject 
#include "../../../stdlib/public/SwiftShims/HeapObject.h"

namespace swift {

struct Metadata;
struct HeapMetadata;
struct OpaqueValue;

/// Allocates a new heap object.  The returned memory is
/// uninitialized outside of the heap-object header.  The object
/// has an initial retain count of 1, and its metadata is set to
/// the given value.
///
/// At some point "soon after return", it will become an
/// invariant that metadata->getSize(returnValue) will equal
/// requiredSize.
///
/// Either aborts or throws a swift exception if the allocation fails.
///
/// \param requiredSize - the required size of the allocation,
///   including the header
/// \param requiredAlignmentMask - the required alignment of the allocation;
///   always one less than a power of 2 that's at least alignof(void*)
/// \return never null
///
/// POSSIBILITIES: The argument order is fair game.  It may be useful
/// to have a variant which guarantees zero-initialized memory.
extern "C" HeapObject *swift_allocObject(HeapMetadata const *metadata,
                                         size_t requiredSize,
                                         size_t requiredAlignmentMask);
  
/// The structure returned by swift_allocPOD and swift_allocBox.
struct BoxPair {
  /// The pointer to the heap object.
  HeapObject *heapObject;
  
  /// The pointer to the value inside the box.
  OpaqueValue *value;
  
  // FIXME: rdar://16257592 arm codegen does't call swift_allocBox correctly.
  // Structs are returned indirectly on these platforms, but we want to return
  // in registers, so cram the result into an unsigned long long.
  // Use an enum class with implicit conversions so we don't dirty C callers
  // too much.
#if __arm__ || __i386__
  enum class Return : unsigned long long {};
  
  operator Return() const {
    union {
      BoxPair value;
      Return mangled;
    } reinterpret = {*this};
    
    return reinterpret.mangled;
  }
  
  BoxPair() = default;
  BoxPair(HeapObject *h, OpaqueValue *v)
    : heapObject(h), value(v) {}
  
  /*implicit*/ BoxPair(Return r) {
    union {
      Return mangled;
      BoxPair value;
    } reinterpret = {r};
    
    *this = reinterpret.value;
  }
#else
  using Return = BoxPair;
#endif
};

/// Allocates a heap object with POD value semantics. The returned memory is
/// uninitialized outside of the heap object header. The object has an
/// initial retain count of 1, and its metadata is set to a predefined
/// POD heap metadata for which destruction is a no-op.
///
/// \param dataSize           The size of the data area for the allocation.
///                           Excludes the heap metadata header.
/// \param dataAlignmentMask  The alignment of the data area.
///
/// \returns a BoxPair in which the heapObject field points to the newly-created
///          HeapObject and the value field points to the data area inside the
///          allocation. The value pointer will have the alignment specified
///          by the dataAlignmentMask and point to dataSize bytes of memory.
extern "C" BoxPair::Return
swift_allocPOD(size_t dataSize, size_t dataAlignmentMask);

/// Deallocates a heap object known to have been allocated by swift_allocPOD and
/// to have no remaining owners.
extern "C" void swift_deallocPOD(HeapObject *obj);
  
/// Allocates a heap object that can contain a value of the given type.
/// Returns a Box structure containing a HeapObject* pointer to the
/// allocated object, and a pointer to the value inside the heap object.
/// The value pointer points to an uninitialized buffer of size and alignment
/// appropriate to store a value of the given type.
/// The heap object has an initial retain count of 1, and its metadata is set
/// such that destroying the heap object destroys the contained value.
extern "C" BoxPair::Return swift_allocBox(Metadata const *type);

// Allocate plain old memory. This is the generalized entry point
// Never returns nil. The returned memory is uninitialized. 
//
// An "alignment mask" is just the alignment (a power of 2) minus 1.
extern "C" void *swift_slowAlloc(size_t bytes, size_t alignMask);

// If the caller cannot promise to zero the object during destruction,
// then call these corresponding APIs:
extern "C" void swift_slowDealloc(void *ptr, size_t bytes, size_t alignMask);

/// Atomically increments the retain count of an object.
///
/// \param object - may be null, in which case this is a no-op
/// \return its argument value exactly
///
/// POSSIBILITIES: We may end up wanting a bunch of different variants:
///  - the general version which correctly handles null values, swift
///     objects, and ObjC objects
///    - a variant that assumes that its operand is a swift object
///      - a variant that can safely use non-atomic operations
///      - maybe a variant that can assume a non-null object
/// It may also prove worthwhile to have this use a custom CC
/// which preserves a larger set of registers.
extern "C" HeapObject *swift_retain(HeapObject *object);
extern "C" void swift_retain_noresult(HeapObject *object);

static inline HeapObject *_swift_retain_inlined(HeapObject *object) {
  if (object) {
    object->refCount.increment();
  }
  return object;
}

/// Atomically increments the reference count of an object, unless it has
/// already been destroyed. Returns nil if the object is dead.
extern "C" HeapObject *swift_tryRetain(HeapObject *object);

/// Returns true if an object is in the process of being deallocated.
extern "C" bool swift_isDeallocating(HeapObject *object);

/// Attempts to atomically pin an object and increment its reference
/// count.  Returns nil if the object was already pinned.
///
/// The standard protocol is that the caller is responsible for
/// calling swift_unpin on the return value.
///
/// The object reference may not be nil.
extern "C" HeapObject *swift_tryPin(HeapObject *object);

/// Given that an object is pinned, atomically unpin it and decrement
/// the reference count.
///
/// The object reference may be nil (to simplify the protocol).
extern "C" void swift_unpin(HeapObject *object);
  
/// Atomically decrements the retain count of an object.  If the
/// retain count reaches zero, the object is destroyed as follows:
///
///   size_t allocSize = object->metadata->destroy(object);
///   if (allocSize) swift_deallocObject(object, allocSize);
///
/// \param object - may be null, in which case this is a no-op
///
/// POSSIBILITIES: We may end up wanting a bunch of different variants:
///  - the general version which correctly handles null values, swift
///     objects, and ObjC objects
///    - a variant that assumes that its operand is a swift object
///      - a variant that can safely use non-atomic operations
///      - maybe a variant that can assume a non-null object
/// It's unlikely that a custom CC would be beneficial here.
extern "C" void swift_release(HeapObject *object);

/// ObjC compatibility. Never call this.
extern "C" size_t swift_retainCount(HeapObject *object);

/// Is this pointer a non-null unique reference to an object
/// that uses Swift reference counting?
extern "C" bool swift_isUniquelyReferencedNonObjC(const void *);

/// Is this non-null pointer a unique reference to an object
/// that uses Swift reference counting?
extern "C" bool swift_isUniquelyReferencedNonObjC_nonNull(const void *);

/// Is this non-null pointer a reference to an object that uses Swift
/// reference counting and is either uniquely referenced or pinned?
extern "C" bool swift_isUniquelyReferencedOrPinnedNonObjC_nonNull(const void *);

/// Is this non-null BridgeObject a unique reference to an object
/// that uses Swift reference counting?
extern "C" bool swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
  uintptr_t bits);

/// Is this non-null BridgeObject a unique or pinned reference to an
/// object that uses Swift reference counting?
extern "C" bool swift_isUniquelyReferencedOrPinnedNonObjC_nonNull_bridgeObject(
  uintptr_t bits);

/// Is this native Swift pointer a non-null unique reference to
/// an object?
extern "C" bool swift_isUniquelyReferenced_native(const struct HeapObject *);

/// Is this native Swift pointer a non-null unique or pinned reference
/// to an object?
extern "C" bool swift_isUniquelyReferencedOrPinned_native(
  const struct HeapObject *);

/// Is this non-null native Swift pointer a unique reference to
/// an object?
extern "C" bool swift_isUniquelyReferenced_nonNull_native(
  const struct HeapObject *);

/// Does this non-null native Swift pointer refer to an object that
/// is either uniquely referenced or pinned?
extern "C" bool swift_isUniquelyReferencedOrPinned_nonNull_native(
  const struct HeapObject *);

/// Deallocate the given memory; it was returned by swift_allocObject
/// but is otherwise in an unknown state.
///
/// \param object - never null
/// \param allocatedSize - the allocated size of the object from the
///   program's perspective, i.e. the value
/// \param allocatedAlignMask - the alignment requirement that was passed
///   to allocObject
///
/// POSSIBILITIES: It may be useful to have a variant which
/// requires the object to have been fully zeroed from offsets
/// sizeof(SwiftHeapObject) to allocatedSize.
extern "C" void swift_deallocObject(HeapObject *object, size_t allocatedSize,
                                    size_t allocatedAlignMask);

/// Deallocate the given class instance; it was returned by swift_allocObject
/// and possibly used as an Objective-C class instance, but is otherwise in an
/// unknown state.
///
/// \param object - never null
/// \param allocatedSize - the allocated size of the object from the
///   program's perspective, i.e. the value
/// \param allocatedAlignMask - the alignment requirement that was passed
///   to allocObject
///
/// POSSIBILITIES: It may be useful to have a variant which
/// requires the object to have been fully zeroed from offsets
/// sizeof(SwiftHeapObject) to allocatedSize.
extern "C" void swift_deallocClassInstance(HeapObject *object,
                                           size_t allocatedSize,
                                           size_t allocatedAlignMask);

/// Deallocate the given memory allocated by swift_allocBox; it was returned
/// by swift_allocBox but is otherwise in an unknown state. The given Metadata
/// pointer must be the same metadata pointer that was passed to swift_allocBox
/// when the memory was allocated.
extern "C" void swift_deallocBox(HeapObject *object, Metadata const *type);
  
/// RAII object that wraps a Swift heap object and releases it upon
/// destruction.
class SwiftRAII {
  HeapObject *object;

public:
  SwiftRAII(HeapObject *obj, bool AlreadyRetained) : object(obj) {
    if (!AlreadyRetained)
      swift_retain(obj);
  }

  ~SwiftRAII() {
    if (object)
      swift_release(object);
  }

  SwiftRAII(const SwiftRAII &other) : object(swift_retain(*other)) {
    ;
  }
  SwiftRAII(SwiftRAII &&other) : object(*other) {
    other.object = nullptr;
  }
  SwiftRAII &operator=(const SwiftRAII &other) {
    if (object)
      swift_release(object);
    object = swift_retain(*other);
    return *this;
  }
  SwiftRAII &operator=(SwiftRAII &&other) {
    if (object)
      swift_release(object);
    object = *other;
    other.object = nullptr;
    return *this;
  }

  HeapObject *operator *() const { return object; }
};

/// Increment the weak retain count.
extern "C" void swift_weakRetain(HeapObject *value);

/// Decrement the weak retain count.
extern "C" void swift_weakRelease(HeapObject *value);

/// Increment the strong retain count of an object which may have been
/// deallocated.
extern "C" void swift_retainUnowned(HeapObject *value);

/// A weak reference value object.  This is ABI.
struct WeakReference {
  HeapObject *Value;
};

/// Initialize a weak reference.
///
/// \param ref - never null
/// \param value - can be null
extern "C" void swift_weakInit(WeakReference *ref, HeapObject *value);

/// Assign a new value to a weak reference.
///
/// \param ref - never null
/// \param value - can be null
extern "C" void swift_weakAssign(WeakReference *ref, HeapObject *value);

/// Load a value from a weak reference.  If the current value is a
/// non-null object that has begun deallocation, returns null;
/// otherwise, retains the object before returning.
///
/// \param ref - never null
/// \return can be null
extern "C" HeapObject *swift_weakLoadStrong(WeakReference *ref);

/// Load a value from a weak reference as if by swift_weakLoadStrong,
/// but leaving the reference in an uninitialized state.
///
/// \param ref - never null
/// \return can be null
extern "C" HeapObject *swift_weakTakeStrong(WeakReference *ref);

/// Destroy a weak reference.
///
/// \param ref - never null, but can refer to a null object
extern "C" void swift_weakDestroy(WeakReference *ref);

/// Copy initialize a weak reference.
///
/// \param dest - never null, but can refer to a null object
/// \param src - never null, but can refer to a null object
extern "C" void swift_weakCopyInit(WeakReference *dest, WeakReference *src);

/// Take initialize a weak reference.
///
/// \param dest - never null, but can refer to a null object
/// \param src - never null, but can refer to a null object
extern "C" void swift_weakTakeInit(WeakReference *dest, WeakReference *src);

/// Copy assign a weak reference.
///
/// \param dest - never null, but can refer to a null object
/// \param src - never null, but can refer to a null object
extern "C" void swift_weakCopyAssign(WeakReference *dest, WeakReference *src);

/// Take assign a weak reference.
///
/// \param dest - never null, but can refer to a null object
/// \param src - never null, but can refer to a null object
extern "C" void swift_weakTakeAssign(WeakReference *dest, WeakReference *src);

extern "C" void *swift_bridgeObjectRetain(void *value);

#if SWIFT_OBJC_INTEROP

/// Increment the strong retain count of an object which might not be a native
/// Swift object.
extern "C" void *swift_unknownRetain(void *value);

#else

static inline void swift_unknownRetain(void *value) {
  swift_retain(static_cast<HeapObject *>(value));
}

#endif /* SWIFT_OBJC_INTEROP */

extern "C" void swift_bridgeObjectRelease(void *value);

#if SWIFT_OBJC_INTEROP

/// Decrement the strong retain count of an object which might not be a native
/// Swift object.
extern "C" void swift_unknownRelease(void *value);

#else

static inline void swift_unknownRelease(void *value) {
  swift_release(static_cast<HeapObject *>(value));
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP
  
/// Increment the strong retain count of an object which may have been
/// deallocated and which might not be a native Swift object.
extern "C" void swift_unknownRetainUnowned(void *value);

#else

static inline void swift_unknownRetainUnowned(void *value) {
  swift_retainUnowned(static_cast<HeapObject *>(value));
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Increment the weak-reference count of an object that might not be
/// a native Swift object.
extern "C" void swift_unknownWeakRetain(void *value);

#else

static inline void swift_unknownWeakRetain(void *value) {
  swift_weakRetain(static_cast<HeapObject *>(value));
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Decrement the weak-reference count of an object that might not be
/// a native Swift object.
extern "C" void swift_unknownWeakRelease(void *value);

#else

static inline void swift_unknownWeakRelease(void *value) {
  swift_weakRelease(static_cast<HeapObject *>(value));
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Initialize a weak reference.
///
/// \param ref - never null
/// \param value - not necessarily a native Swift object; can be null
extern "C" void swift_unknownWeakInit(WeakReference *ref, void *value);

#else

static inline void swift_unknownWeakInit(WeakReference *ref, void *value) {
  swift_weakInit(ref, static_cast<HeapObject *>(value));
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Assign a new value to a weak reference.
///
/// \param ref - never null
/// \param value - not necessarily a native Swift object; can be null
extern "C" void swift_unknownWeakAssign(WeakReference *ref, void *value);

#else

static inline void swift_unknownWeakAssign(WeakReference *ref, void *value) {
  swift_weakAssign(ref, static_cast<HeapObject *>(value));
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Load a value from a weak reference, much like swift_weakLoadStrong
/// but without requiring the variable to refer to a native Swift object.
///
/// \param ref - never null
/// \return can be null
extern "C" void *swift_unknownWeakLoadStrong(WeakReference *ref);

#else

static inline void swift_unknownWeakLoadStrong(WeakReference *ref) {
  swift_weakLoadStrong(ref);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Load a value from a weak reference as if by
/// swift_unknownWeakLoadStrong, but leaving the reference in an
/// uninitialized state.
///
/// \param ref - never null
/// \return can be null
extern "C" void *swift_unknownWeakTakeStrong(WeakReference *ref);

#else

static inline void swift_unknownWeakTakeStrong(WeakReference *ref) {
  swift_weakTakeStrong(ref);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Destroy a weak reference variable that might not refer to a native
/// Swift object.
extern "C" void swift_unknownWeakDestroy(WeakReference *object);

#else

static inline void swift_unknownWeakDestroy(WeakReference *object) {
  swift_weakDestroy(object);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Copy-initialize a weak reference variable from one that might not
/// refer to a native Swift object.
extern "C" void swift_unknownWeakCopyInit(WeakReference *dest, WeakReference *src);

#else

static inline void swift_unknownWeakCopyInit(WeakReference *dest, WeakReference *src) {
  swift_weakCopyInit(dest, src);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Take-initialize a weak reference variable from one that might not
/// refer to a native Swift object.
extern "C" void swift_unknownWeakTakeInit(WeakReference *dest, WeakReference *src);

#else

static inline void swift_unknownWeakTakeInit(WeakReference *dest, WeakReference *src) {
  swift_weakTakeInit(dest, src);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Copy-assign a weak reference variable from another when either
/// or both variables might not refer to a native Swift object.
extern "C" void swift_unknownWeakCopyAssign(WeakReference *dest, WeakReference *src);

#else

static inline void swift_unknownWeakCopyAssign(WeakReference *dest, WeakReference *src) {
  swift_weakCopyAssign(dest, src);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Take-assign a weak reference variable from another when either
/// or both variables might not refer to a native Swift object.
extern "C" void swift_unknownWeakTakeAssign(WeakReference *dest, WeakReference *src);

#else

static inline void swift_unknownWeakTakeAssign(WeakReference *dest, WeakReference *src) {
  swift_weakTakeAssign(dest, src);
}

#endif /* SWIFT_OBJC_INTEROP */

} // end namespace swift

#endif /* SWIFT_RUNTIME_ALLOC_H */
