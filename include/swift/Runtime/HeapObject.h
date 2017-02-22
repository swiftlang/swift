//===--- HeapObject.h - Swift Language Allocation ABI -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

#if SWIFT_OBJC_INTEROP
#include <objc/objc.h>
#endif /* SWIFT_OBJC_INTEROP */

// Bring in the definition of HeapObject 
#include "../../../stdlib/public/SwiftShims/HeapObject.h"

namespace swift {
  
struct InProcess;

template <typename Runtime> struct TargetMetadata;
using Metadata = TargetMetadata<InProcess>;
  
template <typename Runtime> struct TargetHeapMetadata;
using HeapMetadata = TargetHeapMetadata<InProcess>;

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
SWIFT_RT_ENTRY_VISIBILITY
HeapObject *swift_allocObject(HeapMetadata const *metadata,
                              size_t requiredSize,
                              size_t requiredAlignmentMask)
    SWIFT_CC(RegisterPreservingCC);

SWIFT_RUNTIME_EXPORT
HeapObject *(*SWIFT_CC(RegisterPreservingCC) _swift_allocObject)(
                                              HeapMetadata const *metadata,
                                              size_t requiredSize,
                                              size_t requiredAlignmentMask);

/// Initializes the object header of a stack allocated object.
///
/// \param metadata - the object's metadata which is stored in the header
/// \param object - the pointer to the object's memory on the stack
/// \returns the passed object pointer.
SWIFT_RUNTIME_EXPORT
HeapObject *swift_initStackObject(HeapMetadata const *metadata,
                                  HeapObject *object);

/// Performs verification that the lifetime of a stack allocated object has
/// ended. It aborts if the reference counts of the object indicate that the
/// object did escape to some other location.
SWIFT_RUNTIME_EXPORT
void swift_verifyEndOfLifetime(HeapObject *object);

/// A structure that's two pointers in size.
///
/// C functions can use the TwoWordPair::Return type to return a value in
/// two registers, compatible with Swift's calling convention for tuples
/// and structs of two word-sized elements.
template<typename A, typename B>
struct TwoWordPair {
  A first;
  B second;
  
  TwoWordPair() = default;
  TwoWordPair(A first, B second);

  // FIXME: rdar://16257592 arm codegen doesn't call swift_allocBox correctly.
  // Structs are returned indirectly on these platforms, but we want to return
  // in registers, so cram the result into an unsigned long long.
  // Use an enum class with implicit conversions so we don't dirty C callers
  // too much.
#if __arm__ || __i386__ || defined(__CYGWIN__) || defined(_MSC_VER)
#if defined(__CYGWIN__)
  enum class Return : unsigned __int128 {};
#else
  enum class Return : unsigned long long {};
#endif

  operator Return() const {
    union {
      TwoWordPair value;
      Return mangled;
    } reinterpret = {*this};
    
    return reinterpret.mangled;
  }
  
  /*implicit*/ TwoWordPair(Return r) {
    union {
      Return mangled;
      TwoWordPair value;
    } reinterpret = {r};
    
    *this = reinterpret.value;
  }
#else
  using Return = TwoWordPair;
#endif
};
  
template<typename A, typename B>
inline TwoWordPair<A,B>::TwoWordPair(A first, B second)
  : first(first), second(second)
{
  static_assert(sizeof(A) == sizeof(void*),
                "first type must be word-sized");
  static_assert(sizeof(B) == sizeof(void*),
                "second type must be word-sized");
  static_assert(alignof(TwoWordPair) == alignof(void*),
                "pair must be word-aligned");
}
  
using BoxPair = TwoWordPair<HeapObject *, OpaqueValue *>;

/// Allocates a heap object that can contain a value of the given type.
/// Returns a Box structure containing a HeapObject* pointer to the
/// allocated object, and a pointer to the value inside the heap object.
/// The value pointer points to an uninitialized buffer of size and alignment
/// appropriate to store a value of the given type.
/// The heap object has an initial retain count of 1, and its metadata is set
/// such that destroying the heap object destroys the contained value.
SWIFT_RUNTIME_EXPORT
BoxPair::Return swift_allocBox(Metadata const *type);

SWIFT_RUNTIME_EXPORT
BoxPair::Return (*_swift_allocBox)(Metadata const *type);


// Allocate plain old memory. This is the generalized entry point
// Never returns nil. The returned memory is uninitialized. 
//
// An "alignment mask" is just the alignment (a power of 2) minus 1.

SWIFT_RT_ENTRY_VISIBILITY
void *swift_slowAlloc(size_t bytes, size_t alignMask)
     SWIFT_CC(RegisterPreservingCC);


// If the caller cannot promise to zero the object during destruction,
// then call these corresponding APIs:
SWIFT_RT_ENTRY_VISIBILITY
void swift_slowDealloc(void *ptr, size_t bytes, size_t alignMask)
     SWIFT_CC(RegisterPreservingCC);

/// Atomically increments the retain count of an object.
///
/// \param object - may be null, in which case this is a no-op
///
/// POSSIBILITIES: We may end up wanting a bunch of different variants:
///  - the general version which correctly handles null values, swift
///     objects, and ObjC objects
///    - a variant that assumes that its operand is a swift object
///      - a variant that can safely use non-atomic operations
///      - maybe a variant that can assume a non-null object
/// It may also prove worthwhile to have this use a custom CC
/// which preserves a larger set of registers.
SWIFT_RT_ENTRY_VISIBILITY
void swift_retain(HeapObject *object)
    SWIFT_CC(RegisterPreservingCC);

SWIFT_RUNTIME_EXPORT
void (*SWIFT_CC(RegisterPreservingCC) _swift_retain)(HeapObject *object);

SWIFT_RT_ENTRY_VISIBILITY
void swift_retain_n(HeapObject *object, uint32_t n)
    SWIFT_CC(RegisterPreservingCC);

SWIFT_RUNTIME_EXPORT
void (*SWIFT_CC(RegisterPreservingCC) _swift_retain_n)(HeapObject *object,
                                                       uint32_t n);

SWIFT_RT_ENTRY_VISIBILITY
void swift_nonatomic_retain(HeapObject *object)
    SWIFT_CC(RegisterPreservingCC);

SWIFT_RUNTIME_EXPORT
void (*SWIFT_CC(RegisterPreservingCC) _swift_nonatomic_retain)(HeapObject *object);

SWIFT_RT_ENTRY_VISIBILITY
void swift_nonatomic_retain_n(HeapObject *object, uint32_t n)
    SWIFT_CC(RegisterPreservingCC);

SWIFT_RUNTIME_EXPORT
void (*SWIFT_CC(RegisterPreservingCC) _swift_nonatomic_retain_n)(HeapObject *object,
                                                       uint32_t n);

/// Atomically increments the reference count of an object, unless it has
/// already been destroyed. Returns nil if the object is dead.
SWIFT_RT_ENTRY_VISIBILITY
HeapObject *swift_tryRetain(HeapObject *object)
    SWIFT_CC(RegisterPreservingCC);

SWIFT_RUNTIME_EXPORT
HeapObject * (* SWIFT_CC(RegisterPreservingCC) _swift_tryRetain)(HeapObject *);

/// Returns true if an object is in the process of being deallocated.
SWIFT_RUNTIME_EXPORT
bool swift_isDeallocating(HeapObject *object);

SWIFT_RUNTIME_EXPORT
bool (* SWIFT_CC(RegisterPreservingCC) _swift_isDeallocating)(HeapObject *);


/// Attempts to atomically pin an object and increment its reference
/// count.  Returns nil if the object was already pinned.
///
/// The standard protocol is that the caller is responsible for
/// calling swift_unpin on the return value.
///
/// The object reference may not be nil.
SWIFT_RT_ENTRY_VISIBILITY
HeapObject *swift_tryPin(HeapObject *object)
    SWIFT_CC(RegisterPreservingCC);

SWIFT_RT_ENTRY_VISIBILITY
HeapObject *swift_nonatomic_tryPin(HeapObject *object)
    SWIFT_CC(RegisterPreservingCC);

/// Given that an object is pinned, atomically unpin it and decrement
/// the reference count.
///
/// The object reference may be nil (to simplify the protocol).
SWIFT_RT_ENTRY_VISIBILITY
void swift_unpin(HeapObject *object)
    SWIFT_CC(RegisterPreservingCC);

SWIFT_RT_ENTRY_VISIBILITY
void swift_nonatomic_unpin(HeapObject *object)
    SWIFT_CC(RegisterPreservingCC);

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
SWIFT_RT_ENTRY_VISIBILITY
void swift_release(HeapObject *object)
    SWIFT_CC(RegisterPreservingCC);

SWIFT_RUNTIME_EXPORT
void (*SWIFT_CC(RegisterPreservingCC)
                     _swift_release)(HeapObject *object);

SWIFT_RT_ENTRY_VISIBILITY
void swift_nonatomic_release(HeapObject *object)
    SWIFT_CC(RegisterPreservingCC);

SWIFT_RUNTIME_EXPORT
void (*SWIFT_CC(RegisterPreservingCC)
                     _swift_nonatomic_release)(HeapObject *object);


/// Atomically decrements the retain count of an object n times. If the retain
/// count reaches zero, the object is destroyed
SWIFT_RT_ENTRY_VISIBILITY
void swift_release_n(HeapObject *object, uint32_t n)
    SWIFT_CC(RegisterPreservingCC);

SWIFT_RUNTIME_EXPORT
void (*SWIFT_CC(RegisterPreservingCC)
                     _swift_release_n)(HeapObject *object, uint32_t n);

/// Sets the RC_DEALLOCATING_FLAG flag. This is done non-atomically.
/// The strong reference count of \p object must be 1 and no other thread may
/// retain the object during executing this function.
SWIFT_RUNTIME_EXPORT
void swift_setDeallocating(HeapObject *object);

SWIFT_RT_ENTRY_VISIBILITY
void swift_nonatomic_release_n(HeapObject *object, uint32_t n)
    SWIFT_CC(RegisterPreservingCC);

SWIFT_RUNTIME_EXPORT
void (*SWIFT_CC(RegisterPreservingCC)
                     _swift_nonatomic_release_n)(HeapObject *object, uint32_t n);

// Refcounting observation hooks for memory tools. Don't use these.
SWIFT_RUNTIME_EXPORT
size_t swift_retainCount(HeapObject *object);
SWIFT_RUNTIME_EXPORT
size_t swift_unownedRetainCount(HeapObject *object);

/// Is this pointer a non-null unique reference to an object
/// that uses Swift reference counting?
SWIFT_RUNTIME_EXPORT
bool swift_isUniquelyReferencedNonObjC(const void *);

/// Is this non-null pointer a unique reference to an object
/// that uses Swift reference counting?
SWIFT_RUNTIME_EXPORT
bool swift_isUniquelyReferencedNonObjC_nonNull(const void *);

/// Is this non-null pointer a reference to an object that uses Swift
/// reference counting and is either uniquely referenced or pinned?
SWIFT_RUNTIME_EXPORT
bool swift_isUniquelyReferencedOrPinnedNonObjC_nonNull(const void *);

/// Is this non-null BridgeObject a unique reference to an object
/// that uses Swift reference counting?
SWIFT_RUNTIME_EXPORT
bool swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
  uintptr_t bits);

/// Is this non-null BridgeObject a unique or pinned reference to an
/// object that uses Swift reference counting?
SWIFT_RUNTIME_EXPORT
bool swift_isUniquelyReferencedOrPinnedNonObjC_nonNull_bridgeObject(
  uintptr_t bits);

/// Is this native Swift pointer a non-null unique reference to
/// an object?
SWIFT_RUNTIME_EXPORT
bool swift_isUniquelyReferenced_native(const struct HeapObject *);

/// Is this native Swift pointer a non-null unique or pinned reference
/// to an object?
SWIFT_RT_ENTRY_VISIBILITY
bool swift_isUniquelyReferencedOrPinned_native(
  const struct HeapObject *) SWIFT_CC(RegisterPreservingCC);

/// Is this non-null native Swift pointer a unique reference to
/// an object?
SWIFT_RT_ENTRY_VISIBILITY
bool swift_isUniquelyReferenced_nonNull_native(
  const struct HeapObject *) SWIFT_CC(RegisterPreservingCC);

/// Does this non-null native Swift pointer refer to an object that
/// is either uniquely referenced or pinned?
SWIFT_RT_ENTRY_VISIBILITY
bool swift_isUniquelyReferencedOrPinned_nonNull_native(
  const struct HeapObject *) SWIFT_CC(RegisterPreservingCC);

/// Deallocate the given memory.
///
/// It must have been returned by swift_allocObject and the strong reference
/// must have the RC_DEALLOCATING_FLAG flag set, but otherwise the object is
/// in an unknown state.
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
SWIFT_RT_ENTRY_VISIBILITY
void swift_deallocObject(HeapObject *object, size_t allocatedSize,
                         size_t allocatedAlignMask)
    SWIFT_CC(RegisterPreservingCC);

/// Deallocate the given memory.
///
/// It must have been returned by swift_allocObject, possibly used as an
/// Objective-C class instance, and the strong reference must have the
/// RC_DEALLOCATING_FLAG flag set, but otherwise the object is in an unknown
/// state.
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
SWIFT_RUNTIME_EXPORT
void swift_deallocClassInstance(HeapObject *object,
                                 size_t allocatedSize,
                                 size_t allocatedAlignMask);

/// Deallocate the given memory after destroying instance variables.
///
/// Destroys instance variables in classes more derived than the given metatype.
///
/// It must have been returned by swift_allocObject, possibly used as an
/// Objective-C class instance, and the strong reference must be equal to 1.
///
/// \param object - may be null
/// \param type - most derived class whose instance variables do not need to
///   be destroyed
/// \param allocatedSize - the allocated size of the object from the
///   program's perspective, i.e. the value
/// \param allocatedAlignMask - the alignment requirement that was passed
///   to allocObject
SWIFT_RUNTIME_EXPORT
void swift_deallocPartialClassInstance(HeapObject *object,
                                       const HeapMetadata *type,
                                       size_t allocatedSize,
                                       size_t allocatedAlignMask);

/// Deallocate the given memory allocated by swift_allocBox; it was returned
/// by swift_allocBox but is otherwise in an unknown state. The given Metadata
/// pointer must be the same metadata pointer that was passed to swift_allocBox
/// when the memory was allocated.
SWIFT_RUNTIME_EXPORT
void swift_deallocBox(HeapObject *object);

/// Project the value out of a box. `object` must have been allocated
/// using `swift_allocBox`, or by the compiler using a statically-emitted
/// box metadata object.
SWIFT_RUNTIME_EXPORT
OpaqueValue *swift_projectBox(HeapObject *object);

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

  SwiftRAII(const SwiftRAII &other) {
    swift_retain(*other);
    object = *other;
    ;
  }
  SwiftRAII(SwiftRAII &&other) : object(*other) {
    other.object = nullptr;
  }
  SwiftRAII &operator=(const SwiftRAII &other) {
    if (object)
      swift_release(object);
    swift_retain(*other);
    object = *other;
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

/*****************************************************************************/
/**************************** UNOWNED REFERENCES *****************************/
/*****************************************************************************/

/// An unowned reference in memory.  This is ABI.
struct UnownedReference {
  HeapObject *Value;
};

/// Increment the unowned retain count.
SWIFT_RT_ENTRY_VISIBILITY
void swift_unownedRetain(HeapObject *value)
    SWIFT_CC(RegisterPreservingCC);

/// Decrement the unowned retain count.
SWIFT_RT_ENTRY_VISIBILITY
void swift_unownedRelease(HeapObject *value)
    SWIFT_CC(RegisterPreservingCC);

/// Increment the unowned retain count.
SWIFT_RT_ENTRY_VISIBILITY
void swift_nonatomic_unownedRetain(HeapObject *value)
    SWIFT_CC(RegisterPreservingCC);

/// Decrement the unowned retain count.
SWIFT_RT_ENTRY_VISIBILITY
void swift_nonatomic_unownedRelease(HeapObject *value)
    SWIFT_CC(RegisterPreservingCC);

/// Increment the unowned retain count by n.
SWIFT_RT_ENTRY_VISIBILITY
void swift_unownedRetain_n(HeapObject *value, int n)
    SWIFT_CC(RegisterPreservingCC);

/// Decrement the unowned retain count by n.
SWIFT_RT_ENTRY_VISIBILITY
void swift_unownedRelease_n(HeapObject *value, int n)
    SWIFT_CC(RegisterPreservingCC);

/// Increment the weak/unowned retain count by n.
SWIFT_RT_ENTRY_VISIBILITY
void swift_nonatomic_unownedRetain_n(HeapObject *value, int n)
    SWIFT_CC(RegisterPreservingCC);

/// Decrement the weak/unowned retain count by n.
SWIFT_RT_ENTRY_VISIBILITY
void swift_nonatomic_unownedRelease_n(HeapObject *value, int n)
    SWIFT_CC(RegisterPreservingCC);

/// Increment the strong retain count of an object, aborting if it has
/// been deallocated.
SWIFT_RT_ENTRY_VISIBILITY
void swift_unownedRetainStrong(HeapObject *value)
    SWIFT_CC(RegisterPreservingCC);

/// Increment the strong retain count of an object, aborting if it has
/// been deallocated.
SWIFT_RT_ENTRY_VISIBILITY
void swift_nonatomic_unownedRetainStrong(HeapObject *value)
    SWIFT_CC(RegisterPreservingCC);

/// Increment the strong retain count of an object which may have been
/// deallocated, aborting if it has been deallocated, and decrement its
/// unowned reference count.
SWIFT_RT_ENTRY_VISIBILITY
void swift_unownedRetainStrongAndRelease(HeapObject *value)
    SWIFT_CC(RegisterPreservingCC);

/// Increment the strong retain count of an object which may have been
/// deallocated, aborting if it has been deallocated, and decrement its
/// weak/unowned reference count.
SWIFT_RT_ENTRY_VISIBILITY
void swift_nonatomic_unownedRetainStrongAndRelease(HeapObject *value)
    SWIFT_CC(RegisterPreservingCC);

/// Aborts if the object has been deallocated.
SWIFT_RUNTIME_EXPORT
void swift_unownedCheck(HeapObject *value);

static inline void swift_unownedInit(UnownedReference *ref, HeapObject *value) {
  ref->Value = value;
  swift_unownedRetain(value);
}

static inline void swift_unownedAssign(UnownedReference *ref,
                                       HeapObject *value) {
  auto oldValue = ref->Value;
  if (value != oldValue) {
    swift_unownedRetain(value);
    ref->Value = value;
    swift_unownedRelease(oldValue);
  }
}

static inline HeapObject *swift_unownedLoadStrong(UnownedReference *ref) {
  auto value = ref->Value;
  swift_unownedRetainStrong(value);
  return value;
}

static inline void *swift_unownedTakeStrong(UnownedReference *ref) {
  auto value = ref->Value;
  swift_unownedRetainStrongAndRelease(value);
  return value;
}

static inline void swift_unownedDestroy(UnownedReference *ref) {
  swift_unownedRelease(ref->Value);
}

static inline void swift_unownedCopyInit(UnownedReference *dest,
                                         UnownedReference *src) {
  dest->Value = src->Value;
  swift_unownedRetain(dest->Value);
}

static inline void swift_unownedTakeInit(UnownedReference *dest,
                                         UnownedReference *src) {
  dest->Value = src->Value;
}

static inline void swift_unownedCopyAssign(UnownedReference *dest,
                                           UnownedReference *src) {
  auto newValue = src->Value;
  auto oldValue = dest->Value;
  if (newValue != oldValue) {
    dest->Value = newValue;
    swift_unownedRetain(newValue);
    swift_unownedRelease(oldValue);
  }
}

static inline void swift_unownedTakeAssign(UnownedReference *dest,
                                           UnownedReference *src) {
  auto newValue = src->Value;
  auto oldValue = dest->Value;
  dest->Value = newValue;
  swift_unownedRelease(oldValue);
}

/*****************************************************************************/
/****************************** WEAK REFERENCES ******************************/
/*****************************************************************************/

// Defined in Runtime/WeakReference.h
class WeakReference;

/// Initialize a weak reference.
///
/// \param ref - never null
/// \param value - can be null
SWIFT_RUNTIME_EXPORT
void swift_weakInit(WeakReference *ref, HeapObject *value);

/// Assign a new value to a weak reference.
///
/// \param ref - never null
/// \param value - can be null
SWIFT_RUNTIME_EXPORT
void swift_weakAssign(WeakReference *ref, HeapObject *value);

/// Load a value from a weak reference.  If the current value is a
/// non-null object that has begun deallocation, returns null;
/// otherwise, retains the object before returning.
///
/// \param ref - never null
/// \return can be null
SWIFT_RUNTIME_EXPORT
HeapObject *swift_weakLoadStrong(WeakReference *ref);

/// Load a value from a weak reference as if by swift_weakLoadStrong,
/// but leaving the reference in an uninitialized state.
///
/// \param ref - never null
/// \return can be null
SWIFT_RUNTIME_EXPORT
HeapObject *swift_weakTakeStrong(WeakReference *ref);

/// Destroy a weak reference.
///
/// \param ref - never null, but can refer to a null object
SWIFT_RUNTIME_EXPORT
void swift_weakDestroy(WeakReference *ref);

/// Copy initialize a weak reference.
///
/// \param dest - never null, but can refer to a null object
/// \param src - never null, but can refer to a null object
SWIFT_RUNTIME_EXPORT
void swift_weakCopyInit(WeakReference *dest, WeakReference *src);

/// Take initialize a weak reference.
///
/// \param dest - never null, but can refer to a null object
/// \param src - never null, but can refer to a null object
SWIFT_RUNTIME_EXPORT
void swift_weakTakeInit(WeakReference *dest, WeakReference *src);

/// Copy assign a weak reference.
///
/// \param dest - never null, but can refer to a null object
/// \param src - never null, but can refer to a null object
SWIFT_RUNTIME_EXPORT
void swift_weakCopyAssign(WeakReference *dest, WeakReference *src);

/// Take assign a weak reference.
///
/// \param dest - never null, but can refer to a null object
/// \param src - never null, but can refer to a null object
SWIFT_RUNTIME_EXPORT
void swift_weakTakeAssign(WeakReference *dest, WeakReference *src);

/*****************************************************************************/
/************************* OTHER REFERENCE-COUNTING **************************/
/*****************************************************************************/

SWIFT_RUNTIME_EXPORT
void *swift_bridgeObjectRetain(void *value)
    SWIFT_CC(DefaultCC);
/// Increment the strong retain count of a bridged object by n.
SWIFT_RUNTIME_EXPORT
void *swift_bridgeObjectRetain_n(void *value, int n)
    SWIFT_CC(DefaultCC);

SWIFT_RUNTIME_EXPORT
void *swift_nonatomic_bridgeObjectRetain(void *value)
    SWIFT_CC(DefaultCC);

/// Increment the strong retain count of a bridged object by n.
SWIFT_RUNTIME_EXPORT
void *swift_nonatomic_bridgeObjectRetain_n(void *value, int n)
    SWIFT_CC(DefaultCC);

/*****************************************************************************/
/************************ UNKNOWN REFERENCE-COUNTING *************************/
/*****************************************************************************/

#if SWIFT_OBJC_INTEROP

/// Increment the strong retain count of an object which might not be a native
/// Swift object.
SWIFT_RUNTIME_EXPORT
void swift_unknownRetain(void *value)
    SWIFT_CC(DefaultCC);
/// Increment the strong retain count of an object which might not be a native
/// Swift object by n.
SWIFT_RUNTIME_EXPORT
void swift_unknownRetain_n(void *value, int n)
    SWIFT_CC(DefaultCC);

/// Increment the strong retain count of an object which might not be a native
/// Swift object.
SWIFT_RUNTIME_EXPORT
void swift_nonatomic_unknownRetain(void *value)
    SWIFT_CC(DefaultCC);
/// Increment the strong retain count of an object which might not be a native
/// Swift object by n.
SWIFT_RUNTIME_EXPORT
void swift_nonatomic_unknownRetain_n(void *value, int n)
    SWIFT_CC(DefaultCC);


#else

static inline void swift_unknownRetain(void *value)
    SWIFT_CC(DefaultCC) {
  swift_retain(static_cast<HeapObject *>(value));
}

static inline void swift_unknownRetain_n(void *value, int n)
    SWIFT_CC(DefaultCC) {
  swift_retain_n(static_cast<HeapObject *>(value), n);
}

static inline void swift_nonatomic_unknownRetain(void *value)
    SWIFT_CC(DefaultCC) {
  swift_nonatomic_retain(static_cast<HeapObject *>(value));
}

static inline void swift_nonatomic_unknownRetain_n(void *value, int n)
    SWIFT_CC(DefaultCC) {
  swift_nonatomic_retain_n(static_cast<HeapObject *>(value), n);
}


#endif /* SWIFT_OBJC_INTEROP */

SWIFT_RUNTIME_EXPORT
void swift_bridgeObjectRelease(void *value)
    SWIFT_CC(DefaultCC);
/// Decrement the strong retain count of a bridged object by n.
SWIFT_RUNTIME_EXPORT
void swift_bridgeObjectRelease_n(void *value, int n)
    SWIFT_CC(DefaultCC);

SWIFT_RUNTIME_EXPORT
void swift_nonatomic_bridgeObjectRelease(void *value)
    SWIFT_CC(DefaultCC);
/// Decrement the strong retain count of a bridged object by n.
SWIFT_RUNTIME_EXPORT
void swift_nonatomic_bridgeObjectRelease_n(void *value, int n)
    SWIFT_CC(DefaultCC);

#if SWIFT_OBJC_INTEROP

/// Decrement the strong retain count of an object which might not be a native
/// Swift object.
SWIFT_RUNTIME_EXPORT
void swift_unknownRelease(void *value)
    SWIFT_CC(DefaultCC);
/// Decrement the strong retain count of an object which might not be a native
/// Swift object by n.
SWIFT_RUNTIME_EXPORT
void swift_unknownRelease_n(void *value, int n)
    SWIFT_CC(DefaultCC);

/// Decrement the strong retain count of an object which might not be a native
/// Swift object.
SWIFT_RUNTIME_EXPORT
void swift_nonatomic_unknownRelease(void *value)
    SWIFT_CC(DefaultCC);
/// Decrement the strong retain count of an object which might not be a native
/// Swift object by n.
SWIFT_RUNTIME_EXPORT
void swift_nonatomic_unknownRelease_n(void *value, int n)
    SWIFT_CC(DefaultCC);

#else

static inline void swift_unknownRelease(void *value)
    SWIFT_CC(RegisterPreservingCC) {
  swift_release(static_cast<HeapObject *>(value));
}

static inline void swift_unknownRelease_n(void *value, int n)
    SWIFT_CC(RegisterPreservingCC) {
  swift_release_n(static_cast<HeapObject *>(value), n);
}

static inline void swift_nonatomic_unknownRelease(void *value)
    SWIFT_CC(RegisterPreservingCC) {
  swift_nonatomic_release(static_cast<HeapObject *>(value));
}

static inline void swift_nonatomic_unknownRelease_n(void *value, int n)
    SWIFT_CC(RegisterPreservingCC) {
  swift_nonatomic_release_n(static_cast<HeapObject *>(value), n);
}

#endif /* SWIFT_OBJC_INTEROP */

/*****************************************************************************/
/************************** UNKNOWN WEAK REFERENCES **************************/
/*****************************************************************************/

#if SWIFT_OBJC_INTEROP

/// Initialize a weak reference.
///
/// \param ref - never null
/// \param value - not necessarily a native Swift object; can be null
SWIFT_RUNTIME_EXPORT
void swift_unknownWeakInit(WeakReference *ref, void *value);

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
SWIFT_RUNTIME_EXPORT
void swift_unknownWeakAssign(WeakReference *ref, void *value);

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
SWIFT_RUNTIME_EXPORT
void *swift_unknownWeakLoadStrong(WeakReference *ref);

#else

static inline void *swift_unknownWeakLoadStrong(WeakReference *ref) {
  return static_cast<void *>(swift_weakLoadStrong(ref));
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Load a value from a weak reference as if by
/// swift_unknownWeakLoadStrong, but leaving the reference in an
/// uninitialized state.
///
/// \param ref - never null
/// \return can be null
SWIFT_RUNTIME_EXPORT
void *swift_unknownWeakTakeStrong(WeakReference *ref);

#else

static inline void *swift_unknownWeakTakeStrong(WeakReference *ref) {
  return static_cast<void *>(swift_weakTakeStrong(ref));
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Destroy a weak reference variable that might not refer to a native
/// Swift object.
SWIFT_RUNTIME_EXPORT
void swift_unknownWeakDestroy(WeakReference *object);

#else

static inline void swift_unknownWeakDestroy(WeakReference *object) {
  swift_weakDestroy(object);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Copy-initialize a weak reference variable from one that might not
/// refer to a native Swift object.
SWIFT_RUNTIME_EXPORT
void swift_unknownWeakCopyInit(WeakReference *dest,
                               WeakReference *src);

#else

static inline void swift_unknownWeakCopyInit(WeakReference *dest,
                                             WeakReference *src) {
  swift_weakCopyInit(dest, src);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Take-initialize a weak reference variable from one that might not
/// refer to a native Swift object.
SWIFT_RUNTIME_EXPORT
void swift_unknownWeakTakeInit(WeakReference *dest,
                               WeakReference *src);

#else

static inline void swift_unknownWeakTakeInit(WeakReference *dest,
                                             WeakReference *src) {
  swift_weakTakeInit(dest, src);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Copy-assign a weak reference variable from another when either
/// or both variables might not refer to a native Swift object.
SWIFT_RUNTIME_EXPORT
void swift_unknownWeakCopyAssign(WeakReference *dest,
                                 WeakReference *src);

#else

static inline void swift_unknownWeakCopyAssign(WeakReference *dest,
                                               WeakReference *src) {
  swift_weakCopyAssign(dest, src);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Take-assign a weak reference variable from another when either
/// or both variables might not refer to a native Swift object.
SWIFT_RUNTIME_EXPORT
void swift_unknownWeakTakeAssign(WeakReference *dest,
                                 WeakReference *src);

#else

static inline void swift_unknownWeakTakeAssign(WeakReference *dest,
                                               WeakReference *src) {
  swift_weakTakeAssign(dest, src);
}

#endif /* SWIFT_OBJC_INTEROP */

/*****************************************************************************/
/************************ UNKNOWN UNOWNED REFERENCES *************************/
/*****************************************************************************/

#if SWIFT_OBJC_INTEROP

/// Initialize an unowned reference to an object with unknown reference
/// counting.
SWIFT_RUNTIME_EXPORT
void swift_unknownUnownedInit(UnownedReference *ref, void *value);

#else

static inline void swift_unknownUnownedInit(UnownedReference *ref,
                                            void *value) {
  swift_unownedInit(ref, static_cast<HeapObject*>(value));
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Assign to an unowned reference holding an object with unknown reference
/// counting.
SWIFT_RUNTIME_EXPORT
void swift_unknownUnownedAssign(UnownedReference *ref, void *value);

#else

static inline void swift_unknownUnownedAssign(UnownedReference *ref,
                                              void *value) {
  swift_unownedAssign(ref, static_cast<HeapObject*>(value));
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Load from an unowned reference to an object with unknown reference
/// counting.
SWIFT_RUNTIME_EXPORT
void *swift_unknownUnownedLoadStrong(UnownedReference *ref);

#else

static inline void *swift_unknownUnownedLoadStrong(UnownedReference *ref) {
  return swift_unownedLoadStrong(ref);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Take from an unowned reference to an object with unknown reference
/// counting.
SWIFT_RUNTIME_EXPORT
void *swift_unknownUnownedTakeStrong(UnownedReference *ref);

#else

static inline void *swift_unknownUnownedTakeStrong(UnownedReference *ref) {
  return swift_unownedTakeStrong(ref);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP
  
/// Destroy an unowned reference to an object with unknown reference counting.
SWIFT_RUNTIME_EXPORT
void swift_unknownUnownedDestroy(UnownedReference *ref);

#else

static inline void swift_unknownUnownedDestroy(UnownedReference *ref) {
  swift_unownedDestroy(ref);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Copy-initialize an unowned reference variable from one that might not
/// refer to a native Swift object.
SWIFT_RUNTIME_EXPORT
void swift_unknownUnownedCopyInit(UnownedReference *dest,
                                  UnownedReference *src);

#else

static inline void swift_unknownUnownedCopyInit(UnownedReference *dest,
                                                UnownedReference *src) {
  swift_unownedCopyInit(dest, src);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Take-initialize an unowned reference variable from one that might not
/// refer to a native Swift object.
SWIFT_RUNTIME_EXPORT
void swift_unknownUnownedTakeInit(UnownedReference *dest,
                                             UnownedReference *src);

#else

static inline void swift_unknownUnownedTakeInit(UnownedReference *dest,
                                                UnownedReference *src) {
  swift_unownedTakeInit(dest, src);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Copy-assign an unowned reference variable from another when either
/// or both variables might not refer to a native Swift object.
SWIFT_RUNTIME_EXPORT
void swift_unknownUnownedCopyAssign(UnownedReference *dest,
                                               UnownedReference *src);

#else

static inline void swift_unknownUnownedCopyAssign(UnownedReference *dest,
                                                  UnownedReference *src) {
  swift_unownedCopyAssign(dest, src);
}

#endif /* SWIFT_OBJC_INTEROP */

#if SWIFT_OBJC_INTEROP

/// Take-assign an unowned reference variable from another when either
/// or both variables might not refer to a native Swift object.
SWIFT_RUNTIME_EXPORT
void swift_unknownUnownedTakeAssign(UnownedReference *dest,
                                               UnownedReference *src);

#else

static inline void swift_unknownUnownedTakeAssign(UnownedReference *dest,
                                                  UnownedReference *src) {
  swift_unownedTakeAssign(dest, src);
}

#endif /* SWIFT_OBJC_INTEROP */

/// Return the name of a Swift type represented by a metadata object.
SWIFT_CC(swift) SWIFT_RUNTIME_EXPORT
TwoWordPair<const char *, uintptr_t>::Return
swift_getTypeName(const Metadata *type, bool qualified);  

} // end namespace swift

#endif /* SWIFT_RUNTIME_ALLOC_H */
