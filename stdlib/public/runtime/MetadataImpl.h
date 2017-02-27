//===--- MetadataImpl.h - Metadata implementation routines ------*- C++ -*-===//
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
// Declarations used to implement value witnesses for native C/C++ types.
//
// A box class defines some static members which describe the basic
// value-witness properties of a value:
//
//   - NativeBox derives a box from a C++ type
//   - SwiftRetainableBox is a box for Swift object pointers which uses
//     swift_{retain,release}.
//   - FunctionPointerBox is a box for function pointers.
//   - ObjCRetainableBox is a box for Objective-C object pointers,
//     using objc_{retain,release}.
//   - UnknownRetainableBox is a box for void* using
//     swift_unknown{Retain,Release}.
//   - AggregateBox<T...> is a box which uses swift layout rules to
//     combine a number of different boxes.
//
// ValueWitnesses<T> takes a box class and defines all the necessary
// values and functions necessary to build a value witness table.
//
// ValueWitnessTableGenerator<T> takes an instance of ValueWitnesses
// and uses it to build a static member 'table', which can be used to
// constant-initialize a value witness table.
//
// ValueWitnessTable
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_METADATAIMPL_H
#define SWIFT_RUNTIME_METADATAIMPL_H

#include "llvm/Support/Compiler.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/HeapObject.h"
#if SWIFT_OBJC_INTEROP
#include "swift/Runtime/ObjCBridge.h"
#endif

#include "WeakReference.h"

#include <cstring>
#include <type_traits>

namespace swift {
namespace metadataimpl {

// concept Box<typename T> {
//   using type = T;
//   static constexpr size_t size;
//   static constexpr size_t alignment;
//   static constexpr size_t stride;
//   static constexpr bool isPOD;
//   static constexpr bool isBitwiseTakable;
//   static constexpr unsigned numExtraInhabitants;
//   static void destroy(T *);
//   static T *initializeWithCopy(T *dest, T *src);
//   static T *initializeWithTake(T *dest, T *src);
//   static T *assignWithCopy(T *dest, T *src);
//   static T *assignWithTake(T *dest, T *src);
//   static void destroyArray(T *arr, size_t n);
//   static T *initializeArrayWithCopy(T *dest, T *src, size_t n);
//   static T *initializeArrayWithTakeFrontToBack(T *dest, T *src, size_t n);
//   static T *initializeArrayWithTakeBackToFront(T *dest, T *src, size_t n);
//   // Only if numExtraInhabitants is non-zero:
//   static void storeExtraInhabitant(T *dest, int index);
//   static int getExtraInhabitantIndex(const T *src);
// };

/// A box class implemented in terms of C/C++ primitive operations.
/// The type is assumed to be non-polymorphic and to have no extra
/// inhabitants.
///
/// The size/alignment/stride template arguments are for when we want
/// to override the language defaults for a type.
template <class T,
          size_t Alignment = alignof(T),
          size_t Size = sizeof(T),
          size_t Stride = sizeof(T)>
struct NativeBox {
  using type = T;

  static constexpr size_t size = Size;
  static constexpr size_t alignment = Alignment;
  static constexpr size_t stride = Stride;
  static constexpr size_t isPOD = std::is_pod<T>::value;
  static constexpr bool isBitwiseTakable = isPOD;
  static constexpr unsigned numExtraInhabitants = 0;

  static void destroy(T *value) {
    value->T::~T();
  }
  
  static void destroyArray(T *array, size_t n) {
    if (isPOD) return;
    while (n--) {
      array->T::~T();
      array = next(array);
    }
  }

  static T *initializeWithCopy(T *dest, T *src) {
    return new (dest) T(*src);
  }

  static T *initializeWithTake(T *dest, T *src) {
    T *result = new (dest) T(std::move(*src));
    src->T::~T();
    return result;
  }
  
  static T *initializeArrayWithCopy(T *dest, T *src, size_t n) {
    if (isPOD) {
      std::memcpy(dest, src, n * stride);
      return dest;
    }
    
    T *r = dest;
    while (n--) {
      new (dest) T(*src);
      dest = next(dest); src = next(src);
    }
    return r;
  }
  
  static T *initializeArrayWithTakeFrontToBack(T *dest, T *src, size_t n) {
    if (isPOD) {
      std::memmove(dest, src, n * stride);
      return dest;
    }
    
    T *r = dest;
    while (n--) {
      new (dest) T(*src);
      dest = next(dest); src = next(src);
    }
    return r;
  }
  
  static T *initializeArrayWithTakeBackToFront(T *dest, T *src, size_t n) {
    if (isPOD) {
      std::memmove(dest, src, n * stride);
      return dest;
    }
    
    T *r = dest;
    dest = next(dest, n); src = next(src, n);
    while (n--) {
      dest = prev(dest); src = prev(src);
      new (dest) T(*src);
    }
    return r;
  }
  
  static T *assignWithCopy(T *dest, T *src) {
    *dest = *src;
    return dest;
  }

  static T *assignWithTake(T *dest, T *src) {
    *dest = std::move(*src);
    src->T::~T();
    return dest;
  }

private:
  static T *next(T *ptr, size_t n = 1) {
    return (T*)((char*)ptr + stride * n);
  }
  static T *prev(T *ptr, size_t n = 1) {
    return (T*)((char*)ptr - stride * n);
  }
};

/// A CRTP base class for defining boxes of retainable pointers.
template <class Impl, class T> struct RetainableBoxBase {
  using type = T;
  static constexpr size_t size = sizeof(T);
  static constexpr size_t alignment = alignof(T);
  static constexpr size_t stride = sizeof(T);
  static constexpr bool isPOD = false;
  static constexpr bool isBitwiseTakable = true;
#ifdef SWIFT_STDLIB_USE_NONATOMIC_RC
  static constexpr bool isAtomic = false;
#else
  static constexpr bool isAtomic = true;
#endif

  static void destroy(T *addr) {
    Impl::release(*addr);
  }

  static T *initializeWithCopy(T *dest, T *src) {
    *dest = Impl::retain(*src);
    return dest;
  }

  static T *initializeWithTake(T *dest, T *src) {
    *dest = *src;
    return dest;
  }
  
  static void destroyArray(T *arr, size_t n) {
    while (n--)
      Impl::release(*arr++);
  }
  
  static T *initializeArrayWithCopy(T *dest, T *src, size_t n) {
    T *r = dest;
    memcpy(dest, src, n * sizeof(T));
    while (n--)
      Impl::retain(*dest++);
    return r;
  }
  
  static T *initializeArrayWithTakeFrontToBack(T *dest, T *src, size_t n) {
    memmove(dest, src, n * sizeof(T));
    return dest;
  }
  static T *initializeArrayWithTakeBackToFront(T *dest, T *src, size_t n) {
    memmove(dest, src, n * sizeof(T));
    return dest;
  }
  
  static T *assignWithCopy(T *dest, T *src) {
    T oldValue = *dest;
    *dest = Impl::retain(*src);
    Impl::release(oldValue);
    return dest;
  }

  static T *assignWithTake(T *dest, T *src) {
    T oldValue = *dest;
    *dest = *src;
    Impl::release(oldValue);
    return dest;
  }

  // Right now, all object pointers are brought down to the least
  // common denominator for extra inhabitants, so that we don't have
  // to worry about e.g. type substitution on an enum type
  // fundamentally changing the layout.
  static constexpr unsigned numExtraInhabitants =
    swift_getHeapObjectExtraInhabitantCount();

  static void storeExtraInhabitant(T *dest, int index) {
    swift_storeHeapObjectExtraInhabitant((HeapObject**) dest, index);
  }

  static int getExtraInhabitantIndex(const T *src) {
    return swift_getHeapObjectExtraInhabitantIndex((HeapObject* const *) src);
  }
};

/// A box implementation class for Swift object pointers.
struct SwiftRetainableBox :
    RetainableBoxBase<SwiftRetainableBox, HeapObject*> {
  static HeapObject *retain(HeapObject *obj) {
    if (isAtomic) {
      swift_retain(obj);
    } else {
      swift_nonatomic_retain(obj);
    }
    return obj;
  }

  static void release(HeapObject *obj) {
    if (isAtomic) {
      swift_release(obj);
    } else {
      swift_nonatomic_release(obj);
    }
  }
};

/// A box implementation class for Swift unowned object pointers.
struct SwiftUnownedRetainableBox :
    RetainableBoxBase<SwiftUnownedRetainableBox, HeapObject*> {
  static HeapObject *retain(HeapObject *obj) {
    if (isAtomic) {
      swift_unownedRetain(obj);
    } else {
      swift_nonatomic_unownedRetain(obj);
    }
    return obj;
  }

  static void release(HeapObject *obj) {
    if (isAtomic) {
      swift_unownedRelease(obj);
    } else {
      swift_nonatomic_unownedRelease(obj);
    }
  }

#if SWIFT_OBJC_INTEROP
  // The implementation from RetainableBoxBase is valid when interop is
  // disabled.
  static constexpr unsigned numExtraInhabitants = 1;

  static void storeExtraInhabitant(HeapObject **dest, int index) {
    assert(index == 0);
    *dest = nullptr;
  }

  static int getExtraInhabitantIndex(const HeapObject * const *src) {
    return (*src == nullptr ? 0 : -1);
  }
#endif
};

/// CRTP base class for weak reference boxes.
template<typename Impl, typename T>
struct WeakRetainableBoxBase {
  using type = T;
  static constexpr size_t size = sizeof(type);
  static constexpr size_t alignment = alignof(type);
  static constexpr size_t stride = sizeof(type);
  static constexpr bool isPOD = false;
  static constexpr bool isBitwiseTakable = false;
  static constexpr unsigned numExtraInhabitants = 0;

  // The implementation must provide implementations of:
  //   static void destroy(T *);
  //   static T *initializeWithCopy(T *dest, T *src);
  //   static T *initializeWithTake(T *dest, T *src);
  //   static T *assignWithCopy(T *dest, T *src);
  //   static T *assignWithTake(T *dest, T *src);
  // The array value witnesses are implemented pessimistically assuming the
  // type is nontrivially copyable and takable.

  static void destroyArray(T *arr, size_t n) {
    while (n--)
      Impl::destroy(arr++);
  }
  
  static T *initializeArrayWithCopy(T *dest, T *src, size_t n) {
    T *r = dest;
    while (n--)
      Impl::initializeWithCopy(dest++, src++);
    return r;
  }
  
  static T *initializeArrayWithTakeFrontToBack(T *dest, T *src, size_t n) {
    T *r = dest;
    while (n--)
      Impl::initializeWithTake(dest++, src++);
    return r;
  }
  static T *initializeArrayWithTakeBackToFront(T *dest, T *src, size_t n) {
    T *r = dest;

    dest += n;
    src  += n;

    while (n--)
      Impl::initializeWithTake(--dest, --src);

    return r;
  }
};

/// A box implementation class for Swift weak object pointers.
struct SwiftWeakRetainableBox :
    WeakRetainableBoxBase<SwiftWeakRetainableBox, WeakReference> {
  static void destroy(WeakReference *ref) {
    swift_weakDestroy(ref);
  }
  static WeakReference *initializeWithCopy(WeakReference *dest,
                                           WeakReference *src) {
    swift_weakCopyInit(dest, src);
    return dest;
  }
  static WeakReference *initializeWithTake(WeakReference *dest,
                                           WeakReference *src) {
    swift_weakTakeInit(dest, src);
    return dest;
  }
  static WeakReference *assignWithCopy(WeakReference *dest,
                                       WeakReference *src) {
    swift_weakCopyAssign(dest, src);
    return dest;
  }
  static WeakReference *assignWithTake(WeakReference *dest,
                                       WeakReference *src) {
    swift_weakTakeAssign(dest, src);
    return dest;
  }
};

#if SWIFT_OBJC_INTEROP
/// A box implementation class for Objective-C object pointers.
struct ObjCRetainableBox : RetainableBoxBase<ObjCRetainableBox, void*> {
  static constexpr unsigned numExtraInhabitants =
    swift_getHeapObjectExtraInhabitantCount();

  static void *retain(void *obj) {
    return objc_retain((id)obj);
  }

  static void release(void *obj) {
    objc_release((id)obj);
  }
};

/// A box implementation class for unowned Objective-C object pointers.
struct ObjCUnownedRetainableBox
    : WeakRetainableBoxBase<ObjCUnownedRetainableBox, UnownedReference> {

  static constexpr unsigned numExtraInhabitants = 1;
  static void storeExtraInhabitant(UnownedReference *dest, int index) {
    assert(index == 0);
    dest->Value = nullptr;
  }
  static int getExtraInhabitantIndex(const UnownedReference *src) {
    return (src->Value == nullptr ? 0 : -1);
  }

  static void destroy(UnownedReference *ref) {
    swift_unknownUnownedDestroy(ref);
  }
  static UnownedReference *initializeWithCopy(UnownedReference *dest,
                                              UnownedReference *src) {
    swift_unknownUnownedCopyInit(dest, src);
    return dest;
  }
  static UnownedReference *initializeWithTake(UnownedReference *dest,
                                              UnownedReference *src) {
    swift_unknownUnownedTakeInit(dest, src);
    return dest;
  }
  static UnownedReference *assignWithCopy(UnownedReference *dest,
                                          UnownedReference *src) {
    swift_unknownUnownedCopyAssign(dest, src);
    return dest;
  }
  static UnownedReference *assignWithTake(UnownedReference *dest,
                                          UnownedReference *src) {
    swift_unknownUnownedTakeAssign(dest, src);
    return dest;
  }
};

/// A box implementation class for ObjC weak object pointers.
struct ObjCWeakRetainableBox :
    WeakRetainableBoxBase<ObjCWeakRetainableBox, WeakReference> {
  static void destroy(WeakReference *ref) {
    swift_unknownWeakDestroy(ref);
  }
  static WeakReference *initializeWithCopy(WeakReference *dest,
                                           WeakReference *src) {
    swift_unknownWeakCopyInit(dest, src);
    return dest;
  }
  static WeakReference *initializeWithTake(WeakReference *dest,
                                           WeakReference *src) {
    swift_unknownWeakTakeInit(dest, src);
    return dest;
  }
  static WeakReference *assignWithCopy(WeakReference *dest,
                                       WeakReference *src) {
    swift_unknownWeakCopyAssign(dest, src);
    return dest;
  }
  static WeakReference *assignWithTake(WeakReference *dest,
                                       WeakReference *src) {
    swift_unknownWeakTakeAssign(dest, src);
    return dest;
  }
};

#endif

/// A box implementation class for unknown-retainable object pointers.
struct UnknownRetainableBox : RetainableBoxBase<UnknownRetainableBox, void*> {
  static void *retain(void *obj) {
#if SWIFT_OBJC_INTEROP
    swift_unknownRetain(obj);
    return obj;
#else
    if (isAtomic) {
      swift_retain(static_cast<HeapObject *>(obj));
    } else {
      swift_nonatomic_retain(static_cast<HeapObject *>(obj));
    }
    return static_cast<HeapObject *>(obj);
#endif
  }

  static void release(void *obj) {
#if SWIFT_OBJC_INTEROP
    swift_unknownRelease(obj);
#else
    if (isAtomic) {
      swift_release(static_cast<HeapObject *>(obj));
    } else {
      swift_nonatomic_release(static_cast<HeapObject *>(obj));
    }
#endif
  }
};

/// A box implementation class for BridgeObject.
struct BridgeObjectBox :
    RetainableBoxBase<BridgeObjectBox, void*> {
  // TODO: Enable the nil extra inhabitant.
  static constexpr unsigned numExtraInhabitants = 1;
      
  static void *retain(void *obj) {
    return swift_bridgeObjectRetain(obj);
  }

  static void release(void *obj) {
    swift_bridgeObjectRelease(obj);
  }
      
  static void storeExtraInhabitant(void **dest, int index) {
    *dest = nullptr;
  }

  static int getExtraInhabitantIndex(void* const *src) {
    return *src == nullptr ? 0 : -1;
  }
};
  
/// A box implementation class for unmanaged, pointer-aligned pointers.
/// Metatype values have this layout.
struct PointerPointerBox : NativeBox<void**> {
  // TODO: we can do a lot better than this: we don't need to mask off
  // the ObjC reserved bits, and we have spare bits.
  static constexpr unsigned numExtraInhabitants =
    swift_getHeapObjectExtraInhabitantCount();

  static void storeExtraInhabitant(void ***dest, int index) {
    swift_storeHeapObjectExtraInhabitant((HeapObject**) dest, index);
  }

  static int getExtraInhabitantIndex(void ** const *src) {
    return swift_getHeapObjectExtraInhabitantIndex((HeapObject* const *) src);
  }
};

/// A box implementation class for raw pointers.
///
/// Note that this is used for imported `void * _Nonnull`, which may include
/// reinterpret_cast-ed integers, so we only get NULL as an extra inhabitant.
struct RawPointerBox : NativeBox<void*> {
  static constexpr unsigned numExtraInhabitants = 1;

  static void storeExtraInhabitant(void **dest, int index) {
    *dest = nullptr;
  }

  static int getExtraInhabitantIndex(void* const *src) {
    return *src == nullptr ? 0 : -1;
  }
};

/// A box implementation class for unmanaged function pointers.
/// @convention(thin) functions have this layout, as do the first elements of
/// Swift thick functions.
struct FunctionPointerBox : NativeBox<void*> {
  static constexpr unsigned numExtraInhabitants =
    swift_getFunctionPointerExtraInhabitantCount();

  static void storeExtraInhabitant(void **dest, int index) {
    swift_storeFunctionPointerExtraInhabitant(dest, index);
  }

  static int getExtraInhabitantIndex(void * const *src) {
    return swift_getFunctionPointerExtraInhabitantIndex(src);
  }
};

constexpr size_t roundUpToAlignment(size_t offset, size_t alignment) {
  return ((offset + alignment - 1) & ~(alignment - 1));
}

// A helper template for building an AggregateBox.  The more natural
// way to do this would be to left-recurse, but we have to
// right-recurse because C++ only lets you pattern-match things off
// the beginning of a pack.
template <size_t StartOffset, class... EltBoxes>
struct AggregateBoxHelper;

// Base case: empty list.
template <size_t StartOffset>
struct AggregateBoxHelper<StartOffset> {
public:
  static constexpr size_t endOffset = StartOffset;
  static constexpr size_t alignment = 1;
  static constexpr bool isPOD = true;
  static constexpr bool isBitwiseTakable = true;

public:
#define COPY_OP(OP)                        \
  static char *OP(char *dest, char *src) { \
    return dest;                           \
  }
  COPY_OP(initializeWithCopy)
  COPY_OP(initializeWithTake)
  COPY_OP(assignWithCopy)
  COPY_OP(assignWithTake)
#undef COPY_OP

  static void destroy(char *addr) {}
};

// Recursive case: add an element to the start.
template <size_t StartOffset, class EltBox, class... NextBoxes>
struct AggregateBoxHelper<StartOffset, EltBox, NextBoxes...> {
private:
  static constexpr size_t eltOffset =
    roundUpToAlignment(StartOffset, EltBox::alignment);
  static constexpr size_t startToEltOffset = (eltOffset - StartOffset);
  static constexpr size_t nextOffset = eltOffset + EltBox::size;
  using NextHelper = AggregateBoxHelper<nextOffset, NextBoxes...>;

public:
  static constexpr size_t endOffset = NextHelper::endOffset;
  static constexpr size_t alignment =
    (NextHelper::alignment > EltBox::alignment
   ? NextHelper::alignment : EltBox::alignment);
  static constexpr bool isPOD = EltBox::isPOD && NextHelper::isPOD;
  static constexpr bool isBitwiseTakable =
    EltBox::isBitwiseTakable && NextHelper::isBitwiseTakable;

private:
  static constexpr size_t eltToNextOffset = (nextOffset - eltOffset);
  static constexpr size_t startToNextOffset = (nextOffset - StartOffset);

public:
#define COPY_OP(OP)                                                       \
  static char *OP(char *dest, char *src) {                                \
    dest += startToEltOffset;                                             \
    src += startToEltOffset;                                              \
    dest = (char*) EltBox::OP((typename EltBox::type*) dest,              \
                              (typename EltBox::type*) src);              \
    dest = NextHelper::OP(dest + eltToNextOffset, src + eltToNextOffset); \
    return dest - startToNextOffset;                                      \
  }
  COPY_OP(initializeWithCopy)
  COPY_OP(initializeWithTake)
  COPY_OP(assignWithCopy)
  COPY_OP(assignWithTake)
#undef COPY_OP

  static void destroy(char *addr) {
    // We have no particular reason to destroy in either order.
    addr += startToEltOffset;
    EltBox::destroy((typename EltBox::type*) addr);
    NextHelper::destroy(addr + eltToNextOffset);
  }
};

/// A class which produces a tuple-like box (with Swift layout rules)
/// for a list of element boxes.
///
/// The aggregate box is monomorphic and has no extra inhabitants.
template <class... EltBoxes>
struct AggregateBox {
  using type = char;

  using Helper = AggregateBoxHelper<0, EltBoxes...>;
  static constexpr size_t size = Helper::endOffset;
  static constexpr size_t alignment = Helper::alignment;
  static constexpr size_t rawStride = roundUpToAlignment(size, alignment);
  static constexpr size_t stride = rawStride == 0 ? 1 : rawStride;

  static constexpr bool isPOD = Helper::isPOD;
  static constexpr bool isBitwiseTakable = Helper::isBitwiseTakable;

  /// Don't collect extra inhabitants from the members by default.
  static constexpr unsigned numExtraInhabitants = 0;

  static void destroy(char *value) {
    Helper::destroy(value);
  }

  static char *initializeWithCopy(char *dest, char *src) {
    return Helper::initializeWithCopy(dest, src);
  }

  static char *initializeWithTake(char *dest, char *src) {
    return Helper::initializeWithTake(dest, src);
  }
    
  static char *assignWithCopy(char *dest, char *src) {
    return Helper::assignWithCopy(dest, src);
  }

  static char *assignWithTake(char *dest, char *src) {
    return Helper::assignWithTake(dest, src);
  }
  
  static void destroyArray(char *array, size_t n) {
    if (isPOD)
      return;
    while (n--) {
      destroy(array);
      array += stride;
    }
  }
  static char *initializeArrayWithCopy(char *dest, char *src, size_t n) {
    if (isPOD) {
      std::memcpy(dest, src, n * stride);
      return dest;
    }
    
    char *r = dest;
    while (n--) {
      initializeWithCopy(dest, src);
      dest += stride; src += stride;
    }
    return r;
  }
  static char *initializeArrayWithTakeFrontToBack(char *dest, char *src, size_t n) {
    if (isPOD) {
      std::memmove(dest, src, n * stride);
      return dest;
    }
    
    char *r = dest;
    while (n--) {
      initializeWithTake(dest, src);
      dest += stride; src += stride;
    }
    return r;
  }
  static char *initializeArrayWithTakeBackToFront(char *dest, char *src, size_t n) {
    if (isPOD) {
      std::memmove(dest, src, n * stride);
      return dest;
    }
    
    char *r = dest;
    dest += stride * n; src += stride * n;
    while (n--) {
      dest -= stride; src -= stride;
      initializeWithTake(dest, src);
    }
    return r;
  }
};
  
/// A template for using the Swift allocation APIs with a known size
/// and alignment.
template <size_t Size, size_t Alignment>
struct SwiftAllocator {
  static void *alloc() {
    return swift_slowAlloc(Size, Alignment-1);
  }

  static void dealloc(void *addr) {
    swift_slowDealloc(addr, Size, Alignment-1);
  }
};

/// A CRTP class which provides basic implementations for a number of
/// value witnesses relating to buffers.
template <class Impl> struct BufferValueWitnessesBase {
  static void destroyBuffer(ValueBuffer *buffer, const Metadata *self) {
    Impl::destroy(Impl::projectBuffer(buffer, self), self);
    Impl::deallocateBuffer(buffer, self);
  }

  static OpaqueValue *initializeBufferWithCopyOfBuffer(ValueBuffer *dest,
                                                       ValueBuffer *src,
                                                       const Metadata *self) {
    return Impl::initializeBufferWithCopy(dest,
                                          Impl::projectBuffer(src, self),
                                          self);
  }

  static OpaqueValue *initializeBufferWithCopy(ValueBuffer *dest,
                                               OpaqueValue *src,
                                               const Metadata *self) {
    return Impl::initializeWithCopy(Impl::allocateBuffer(dest, self), src, self);
  }

  static OpaqueValue *initializeBufferWithTake(ValueBuffer *dest,
                                               OpaqueValue *src,
                                               const Metadata *self) {
    return Impl::initializeWithTake(Impl::allocateBuffer(dest, self), src, self);
  }
};

/// How should a type be packed into a fixed-size buffer?
enum class FixedPacking {
  Allocate,
  OffsetZero
};
constexpr FixedPacking getFixedPacking(size_t size, size_t alignment) {
  return (canBeInline(size, alignment) ? FixedPacking::OffsetZero
                                       : FixedPacking::Allocate);
}

/// A CRTP base class which provides default implementations of a
/// number of value witnesses.
template <class Impl, size_t Size, size_t Alignment,
          FixedPacking Packing = getFixedPacking(Size, Alignment)>
struct BufferValueWitnesses;

/// An implementation of ValueBase suitable for classes that can be
/// allocated inline.
template <class Impl, size_t Size, size_t Alignment>
struct BufferValueWitnesses<Impl, Size, Alignment, FixedPacking::OffsetZero>
    : BufferValueWitnessesBase<Impl> {
  static constexpr bool isInline = true;

  static OpaqueValue *allocateBuffer(ValueBuffer *buffer, const Metadata *self) {
    return reinterpret_cast<OpaqueValue*>(buffer);
  }
  static OpaqueValue *projectBuffer(ValueBuffer *buffer, const Metadata *self) {
    return reinterpret_cast<OpaqueValue*>(buffer);
  }
  static OpaqueValue *initializeBufferWithTakeOfBuffer(ValueBuffer *dest,
                                                       ValueBuffer *src,
                                                       const Metadata *self) {
    return Impl::initializeWithTake(reinterpret_cast<OpaqueValue*>(dest),
                                    reinterpret_cast<OpaqueValue*>(src),
                                    self);
  }
  static void deallocateBuffer(ValueBuffer *buffer, const Metadata *self) {}
};

/// An implementation of BufferValueWitnesses suitable for types that
/// cannot be allocated inline.
template <class Impl, size_t Size, size_t Alignment>
struct BufferValueWitnesses<Impl, Size, Alignment, FixedPacking::Allocate>
    : BufferValueWitnessesBase<Impl> {
  static constexpr bool isInline = false;

  static OpaqueValue *allocateBuffer(ValueBuffer *buffer, const Metadata *self) {
    OpaqueValue *value =
      static_cast<OpaqueValue*>(SwiftAllocator<Size, Alignment>::alloc());
    buffer->PrivateData[0] = value;
    return value;
  }
  static OpaqueValue *projectBuffer(ValueBuffer *buffer, const Metadata *self) {
    return reinterpret_cast<OpaqueValue*>(buffer->PrivateData[0]);
  }
  static void deallocateBuffer(ValueBuffer *buffer, const Metadata *self) {
    SwiftAllocator<Size, Alignment>::dealloc(buffer->PrivateData[0]);
  }
  static OpaqueValue *initializeBufferWithTakeOfBuffer(ValueBuffer *dest,
                                                       ValueBuffer *src,
                                                       const Metadata *self) {
    dest->PrivateData[0] = src->PrivateData[0];
    return (OpaqueValue*) dest->PrivateData[0];
  }
};

/// A class which provides BufferValueWitnesses for types that are not
/// fixed in size.
template <class Impl, bool IsKnownAllocated>
struct NonFixedBufferValueWitnesses : BufferValueWitnessesBase<Impl> {
  static OpaqueValue *allocateBuffer(ValueBuffer *buffer, const Metadata *self) {
    auto vwtable = self->getValueWitnesses();
    if (!IsKnownAllocated && vwtable->isValueInline()) {
      return reinterpret_cast<OpaqueValue*>(buffer);
    } else {
      OpaqueValue *value =
        static_cast<OpaqueValue*>(swift_slowAlloc(vwtable->size,
                                                  vwtable->getAlignmentMask()));
      buffer->PrivateData[0] = value;
      return value;
    }
  }

  static OpaqueValue *projectBuffer(ValueBuffer *buffer, const Metadata *self) {
    auto vwtable = self->getValueWitnesses();
    if (!IsKnownAllocated && vwtable->isValueInline()) {
      return reinterpret_cast<OpaqueValue*>(buffer);
    } else {
      return reinterpret_cast<OpaqueValue*>(buffer->PrivateData[0]);
    }
  }

  static void deallocateBuffer(ValueBuffer *buffer, const Metadata *self) {
    auto vwtable = self->getValueWitnesses();
    if (IsKnownAllocated || !vwtable->isValueInline()) {
      swift_slowDealloc(buffer->PrivateData[0], vwtable->size,
                        vwtable->getAlignmentMask());
    }
  }

  static OpaqueValue *initializeBufferWithTakeOfBuffer(ValueBuffer *dest,
                                                       ValueBuffer *src,
                                                       const Metadata *self) {
    auto vwtable = self->getValueWitnesses();
    if (!IsKnownAllocated && !vwtable->isValueInline()) {
      return Impl::initializeWithTake(reinterpret_cast<OpaqueValue*>(dest),
                                      reinterpret_cast<OpaqueValue*>(src),
                                      self);
    } else {
      dest->PrivateData[0] = src->PrivateData[0];
      return (OpaqueValue*) dest->PrivateData[0];
    }
  }
};

/// A class which provides default implementations of various value
/// witnesses based on a box's value operations.
///
/// The box type has to provide a numExtraInhabitants member, but as
/// long as it's zero, the rest is fine.
template <class Box>
struct ValueWitnesses : BufferValueWitnesses<ValueWitnesses<Box>,
                                             Box::size, Box::alignment>
{
  using Base = BufferValueWitnesses<ValueWitnesses<Box>,
                                    Box::size, Box::alignment>;

  static constexpr size_t size = Box::size;
  static constexpr size_t stride = Box::stride;
  static constexpr size_t alignment = Box::alignment;
  static constexpr bool isPOD = Box::isPOD;
  static constexpr bool isBitwiseTakable = Box::isBitwiseTakable;
  static constexpr unsigned numExtraInhabitants = Box::numExtraInhabitants;
  static constexpr bool hasExtraInhabitants = (numExtraInhabitants != 0);
  static constexpr ValueWitnessFlags flags =
    ValueWitnessFlags().withAlignmentMask(alignment - 1)
                       .withInlineStorage(Base::isInline)
                       .withPOD(isPOD)
                       .withBitwiseTakable(isBitwiseTakable)
                       .withExtraInhabitants(hasExtraInhabitants);
  static constexpr ExtraInhabitantFlags extraInhabitantFlags =
    ExtraInhabitantFlags().withNumExtraInhabitants(numExtraInhabitants);

  static void destroy(OpaqueValue *value, const Metadata *self) {
    return Box::destroy((typename Box::type*) value);
  }

  static OpaqueValue *initializeWithCopy(OpaqueValue *dest, OpaqueValue *src,
                                         const Metadata *self) {
    return (OpaqueValue*) Box::initializeWithCopy((typename Box::type*) dest,
                                                  (typename Box::type*) src);
  }

  static OpaqueValue *initializeWithTake(OpaqueValue *dest, OpaqueValue *src,
                                         const Metadata *self) {
    return (OpaqueValue*) Box::initializeWithTake((typename Box::type*) dest,
                                                  (typename Box::type*) src);
  }

  static OpaqueValue *assignWithCopy(OpaqueValue *dest, OpaqueValue *src,
                                     const Metadata *self) {
    return (OpaqueValue*) Box::assignWithCopy((typename Box::type*) dest,
                                              (typename Box::type*) src);
  }

  static OpaqueValue *assignWithTake(OpaqueValue *dest, OpaqueValue *src,
                                     const Metadata *self) {
    return (OpaqueValue*) Box::assignWithTake((typename Box::type*) dest,
                                              (typename Box::type*) src);
  }

  static void destroyArray(OpaqueValue *array, size_t n, const Metadata *self) {
    return Box::destroyArray((typename Box::type*)array, n);
  }
  
  static OpaqueValue *initializeArrayWithCopy(OpaqueValue *dest,
                                              OpaqueValue *src,
                                              size_t n,
                                              const Metadata *self) {
    return (OpaqueValue*) Box::initializeArrayWithCopy((typename Box::type*) dest,
                                                 (typename Box::type*) src, n);
  }
  
  static OpaqueValue *initializeArrayWithTakeFrontToBack(OpaqueValue *dest,
                                              OpaqueValue *src,
                                              size_t n,
                                              const Metadata *self) {
    return (OpaqueValue*) Box::initializeArrayWithTakeFrontToBack(
                                                   (typename Box::type*) dest,
                                                   (typename Box::type*) src, n);
  }
  
  static OpaqueValue *initializeArrayWithTakeBackToFront(OpaqueValue *dest,
                                              OpaqueValue *src,
                                              size_t n,
                                              const Metadata *self) {
    return (OpaqueValue*) Box::initializeArrayWithTakeBackToFront(
                                                   (typename Box::type*) dest,
                                                   (typename Box::type*) src, n);
  }
  
  // These should not get instantiated if the type doesn't have extra
  // inhabitants.

  static void storeExtraInhabitant(OpaqueValue *dest, int index,
                                   const Metadata *self) {
    Box::storeExtraInhabitant((typename Box::type*) dest, index);
  }

  static int getExtraInhabitantIndex(const OpaqueValue *src,
                                     const Metadata *self) {
    return Box::getExtraInhabitantIndex((typename Box::type const *) src);
  }
};

/// A class which provides basic implementations of various function
/// value witnesses based on a type that is not fixed in size.
///
/// The 'Box' concept here is slightly different from the one for
/// fixed-size types: it does not need to provide size/alignment/isPOD
/// members, and its functions all take an extra 'const Metadata *self'
/// argument.
///
/// \tparam IsKnownAllocated - whether the type is known to not fit in
/// a fixed-size buffer
template <class Box, bool IsKnownAllocated>
struct NonFixedValueWitnesses :
  NonFixedBufferValueWitnesses<NonFixedValueWitnesses<Box, IsKnownAllocated>,
                               IsKnownAllocated>
{

  static constexpr unsigned numExtraInhabitants = Box::numExtraInhabitants;
  static constexpr bool hasExtraInhabitants = (numExtraInhabitants != 0);
  static constexpr ExtraInhabitantFlags extraInhabitantFlags =
    ExtraInhabitantFlags().withNumExtraInhabitants(numExtraInhabitants);

  static void destroy(OpaqueValue *value, const Metadata *self) {
    return Box::destroy((typename Box::type*) value, self);
  }
  
  static void destroyArray(OpaqueValue *array, size_t n,
                           const Metadata *self) {
    return Box::destroyArray((typename Box::type*) array, n, self);
  }
  
  static OpaqueValue *initializeWithCopy(OpaqueValue *dest, OpaqueValue *src,
                                         const Metadata *self) {
    return (OpaqueValue*) Box::initializeWithCopy((typename Box::type*) dest,
                                                  (typename Box::type*) src,
                                                  self);
  }
  
  static OpaqueValue *initializeWithTake(OpaqueValue *dest, OpaqueValue *src,
                                         const Metadata *self) {
    return (OpaqueValue*) Box::initializeWithTake((typename Box::type*) dest,
                                                  (typename Box::type*) src,
                                                  self);
  }
  
  static OpaqueValue *initializeArrayWithCopy(OpaqueValue *dest,
                                              OpaqueValue *src,
                                              size_t n,
                                              const Metadata *self) {
    return (OpaqueValue*) Box::initializeArrayWithCopy(
                                                  (typename Box::type*) dest,
                                                  (typename Box::type*) src,
                                                  n, self);
  }
  
  static OpaqueValue *initializeArrayWithTakeFrontToBack(OpaqueValue *dest,
                                                         OpaqueValue *src,
                                                         size_t n,
                                                         const Metadata *self) {
    return (OpaqueValue*) Box::initializeArrayWithTakeFrontToBack(
                                                  (typename Box::type*) dest,
                                                  (typename Box::type*) src,
                                                  n, self);
  }

  static OpaqueValue *initializeArrayWithTakeBackToFront(OpaqueValue *dest,
                                                         OpaqueValue *src,
                                                         size_t n,
                                                         const Metadata *self) {
    return (OpaqueValue*) Box::initializeArrayWithTakeBackToFront(
                                                  (typename Box::type*) dest,
                                                  (typename Box::type*) src,
                                                  n, self);
  }

  static OpaqueValue *assignWithCopy(OpaqueValue *dest, OpaqueValue *src,
                                     const Metadata *self) {
    return (OpaqueValue*) Box::assignWithCopy((typename Box::type*) dest,
                                              (typename Box::type*) src,
                                              self);
  }

  static OpaqueValue *assignWithTake(OpaqueValue *dest, OpaqueValue *src,
                                     const Metadata *self) {
    return (OpaqueValue*) Box::assignWithTake((typename Box::type*) dest,
                                              (typename Box::type*) src,
                                              self);
  }

  // These should not get instantiated if the type doesn't have extra
  // inhabitants.

  static void storeExtraInhabitant(OpaqueValue *dest, int index,
                                   const Metadata *self) {
    Box::storeExtraInhabitant((typename Box::type*) dest, index, self);
  }

  static int getExtraInhabitantIndex(const OpaqueValue *src,
                                     const Metadata *self) {
    return Box::getExtraInhabitantIndex((typename Box::type const *) src,
                                        self);
  }
};

/// A class which defines a ValueWitnessTable.
template <class Witnesses,
          bool HasExtraInhabitants = Witnesses::hasExtraInhabitants>
struct ValueWitnessTableGenerator;

template <class Witnesses> struct ValueWitnessTableGenerator<Witnesses, false> {
  static constexpr const ValueWitnessTable table = {
#define EACH_WITNESS(ID) Witnesses::ID,
    FOR_ALL_FUNCTION_VALUE_WITNESSES(EACH_WITNESS)
#undef EACH_WITNESS
    Witnesses::size,
    Witnesses::flags,
    Witnesses::stride,
  };
};

/// A class which defines an ExtraInhabitantsValueWitnessTable.
template <class Witnesses> struct ValueWitnessTableGenerator<Witnesses, true> {
  static constexpr const ExtraInhabitantsValueWitnessTable table = {
    {
#define EACH_WITNESS(ID) Witnesses::ID,
      FOR_ALL_FUNCTION_VALUE_WITNESSES(EACH_WITNESS)
#undef EACH_WITNESS
      Witnesses::size,
      Witnesses::flags,
      Witnesses::stride,
    },
    Witnesses::extraInhabitantFlags,
    Witnesses::storeExtraInhabitant,
    Witnesses::getExtraInhabitantIndex,
  };
};

/// A convenient way to get the value witness table for a box class.
template <class Box>
using ValueWitnessTableForBox = ValueWitnessTableGenerator<ValueWitnesses<Box>>;

} // end namespace metadataimpl
} // end namespace swift

#endif /* SWIFT_RUNTIME_METADATAIMPL_H */
