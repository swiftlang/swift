//===--- Metadata.cpp - Swift Language ABI Metdata Support ----------------===//
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
// Implementations of the metadata ABI functions.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/MathExtras.h"
#include "swift/Basic/Range.h"
#include "swift/Runtime/Alloc.h"
#include "swift/Runtime/Metadata.h"
#include <algorithm>
#include <new>
#include <string.h>
#include <unordered_map>

#ifndef SWIFT_DEBUG_RUNTIME
#define SWIFT_DEBUG_RUNTIME 0
#endif

using namespace swift;

namespace {
  template <class Entry> class MetadataCache;

  /// A CRTP class for defining entries in a metadata cache.
  template <class Impl> class CacheEntry {
    const Impl *Next;
    friend class MetadataCache<Impl>;

    CacheEntry(const CacheEntry &other) = delete;
    void operator=(const CacheEntry &other) = delete;

    Impl *asImpl() { return static_cast<Impl*>(this); }
    const Impl *asImpl() const { return static_cast<const Impl*>(this); }

  protected:
    CacheEntry() = default;

    /// Determine whether the arguments buffer matches the given data.
    /// Assumes that the number of arguments in the buffer is the same
    /// as the number in the data.
    bool argumentsBufferMatches(const void * const *arguments,
                                size_t numArguments) const {
      // TODO: exploit our knowledge about the pointer alignment of
      // the arguments.
      const void *storedArguments = getArgumentsBuffer();
      return memcmp(storedArguments, arguments, numArguments * sizeof(void*)) == 0;
    }

  public:
    static Impl *allocate(const void * const *arguments,
                          size_t numArguments, size_t payloadSize) {
      void *buffer = operator new(sizeof(Impl) +
                                  numArguments * sizeof(void*) +
                                  payloadSize);
      auto result = new (buffer) Impl(numArguments);

      // Copy the arguments into the right place for the key.
      memcpy(result->getArgumentsBuffer(), arguments,
             numArguments * sizeof(void*));

      return result;
    }

    const Impl *getNext() const { return Next; }

    void **getArgumentsBuffer() {
      return reinterpret_cast<void**>(asImpl() + 1);
    }
    void * const *getArgumentsBuffer() const {
      return reinterpret_cast<void * const *>(asImpl() + 1);
    }

    template <class T> T *getData(size_t numArguments) {
      return reinterpret_cast<T *>(getArgumentsBuffer() + numArguments);
    }
    template <class T> const T *getData(size_t numArguments) const {
      return const_cast<CacheEntry*>(this)->getData<T>(numArguments);
    }
  };

  /// A CacheEntry implementation where the entries in the cache may
  /// have different numbers of arguments.
  class HeterogeneousCacheEntry : public CacheEntry<HeterogeneousCacheEntry> {
    const size_t NumArguments;

  public:
    HeterogeneousCacheEntry(size_t numArguments) : NumArguments(numArguments) {}

    /// Does this cache entry match the given set of arguments?
    bool matches(const void * const *arguments, size_t numArguments) const {
      if (NumArguments != numArguments) return false;
      return argumentsBufferMatches(arguments, numArguments);
    }
  };

  /// A CacheEntry implementation where all the entries in the cache
  /// have the same number of arguments.
  class HomogeneousCacheEntry : public CacheEntry<HomogeneousCacheEntry> {
  public:
    HomogeneousCacheEntry(size_t numArguments) { /*do nothing*/ }

    /// Does this cache entry match the given set of arguments?
    bool matches(const void * const *arguments, size_t numArguments) const {
      return argumentsBufferMatches(arguments, numArguments);
    }
  };

  /// The implementation of a metadata cache.  Note that all-zero must
  /// be a valid state for the cache.
  template <class Entry> class MetadataCache {
    /// The head of a linked list of metadata cache entries.
    const Entry *Head;

  public:
    /// Try to find an existing entry in this cache.
    const Entry *find(const void * const *arguments, size_t numArguments) const {
      for (auto entry = Head; entry != nullptr; entry = entry->getNext())
        if (entry->matches(arguments, numArguments))
          return entry;
      return nullptr;
    }

    /// Add the given entry to the cache, taking responsibility for
    /// it.  Returns the entry that should be used, which might not be
    /// the same as the argument if we lost a race to instantiate it.
    /// Regardless, the argument should be considered potentially
    /// invalid after this call.
    const Entry *add(Entry *entry) {
      entry->Next = Head;
      Head = entry;
      return entry;
    }
  };
}

typedef HomogeneousCacheEntry GenericCacheEntry;
typedef MetadataCache<GenericCacheEntry> GenericMetadataCache; 

/// Fetch the metadata cache for a generic metadata structure.
static GenericMetadataCache &getCache(GenericMetadata *metadata) {
  // Keep this assert even if you change the representation above.
  static_assert(sizeof(GenericMetadataCache) <=
                sizeof(GenericMetadata::PrivateData),
                "metadata cache is larger than the allowed space");

  return *reinterpret_cast<GenericMetadataCache*>(metadata->PrivateData);
}

template <class T>
static const T *adjustAddressPoint(const T *raw, uint32_t offset) {
  return reinterpret_cast<const T*>(reinterpret_cast<const char*>(raw) + offset);
}

static const Metadata *
instantiateGenericMetadata(GenericMetadata *pattern,
                           const void *arguments) {
  size_t numGenericArguments = pattern->NumKeyArguments;
  void * const *argumentsAsArray = reinterpret_cast<void * const *>(arguments);

  // Allocate the new entry.
  auto entry = GenericCacheEntry::allocate(argumentsAsArray,
                                           numGenericArguments,
                                           pattern->MetadataSize);

  // Initialize the metadata by copying the template.
  auto fullMetadata = entry->getData<Metadata>(numGenericArguments);
  memcpy(fullMetadata, pattern->getMetadataTemplate(), pattern->MetadataSize);

  // Fill in the missing spaces from the arguments using the pattern's fill
  // function.
  pattern->FillFunction(fullMetadata, arguments);

  // The metadata is now valid.

  // Add the cache to the list.  This can in theory be made thread-safe,
  // but really this should use a non-linear lookup algorithm.
  auto canonFullMetadata =
    getCache(pattern).add(entry)->getData<Metadata>(numGenericArguments);
  return adjustAddressPoint(canonFullMetadata, pattern->AddressPoint);
}

/// The primary entrypoint.
const void *
swift::swift_dynamicCastClass(const void *object, 
                              const ClassMetadata *targetType) {
#if SWIFT_OBJC_INTEROP
  // If the object is an Objective-C object then we 
  // must not dereference it or its isa field directly.
  // FIXME: optimize this for objects that have no ObjC inheritance.
  return swift_dynamicCastObjCClass(object, targetType);      
#endif

  const ClassMetadata *isa = *reinterpret_cast<ClassMetadata *const*>(object);
  do {
    if (isa == targetType) {
      return object;
    }
    isa = isa->SuperClass;
  } while (isa);
  return NULL;
}

/// The primary entrypoint.
const void *
swift::swift_dynamicCastClassUnconditional(const void *object,
                                           const ClassMetadata *targetType) {
#if SWIFT_OBJC_INTEROP
  // If the object is an Objective-C object then we 
  // must not dereference it or its isa field directly.
  // FIXME: optimize this for objects that have no ObjC inheritance.
  return swift_dynamicCastObjCClassUnconditional(object, targetType);      
#endif

  const ClassMetadata *isa = *reinterpret_cast<ClassMetadata *const*>(object);
  do {
    if (isa == targetType) {
      return object;
    }
    isa = isa->SuperClass;
  } while (isa);
  abort();
}

const void *
swift::swift_dynamicCast(const void *object, const Metadata *targetType) {
  const ClassMetadata *targetClassType;
  switch (targetType->getKind()) {
  case MetadataKind::Class:
#if SWIFT_DEBUG_RUNTIME
    printf("casting to class\n");
#endif
    targetClassType = static_cast<const ClassMetadata *>(targetType);
    break;

  case MetadataKind::ObjCClassWrapper:
#if SWIFT_DEBUG_RUNTIME
    printf("casting to objc class wrapper\n");
#endif
    targetClassType
      = static_cast<const ObjCClassWrapperMetadata *>(targetType)->Class;
    break;

  case MetadataKind::Existential:
  case MetadataKind::Function:
  case MetadataKind::HeapArray:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    // FIXME: unreachable
    abort();
  }

  return swift_dynamicCastClass(object, targetClassType);
}

const void *
swift::swift_dynamicCastUnconditional(const void *object,
                                      const Metadata *targetType) {
  const ClassMetadata *targetClassType;
  switch (targetType->getKind()) {
  case MetadataKind::Class:
    targetClassType = static_cast<const ClassMetadata *>(targetType);
    break;

  case MetadataKind::ObjCClassWrapper:
    targetClassType
      = static_cast<const ObjCClassWrapperMetadata *>(targetType)->Class;
    break;

  case MetadataKind::Existential:
  case MetadataKind::Function:
  case MetadataKind::HeapArray:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    // FIXME: unreachable
    abort();
  }

  return swift_dynamicCastClassUnconditional(object, targetClassType);
}

const OpaqueValue *
swift::swift_dynamicCastIndirect(const OpaqueValue *value,
                                 const Metadata *sourceType,
                                 const Metadata *targetType) {
  switch (targetType->getKind()) {
  case MetadataKind::Class:
  case MetadataKind::ObjCClassWrapper:
    // The source value must also be a class; otherwise the cast fails.
    switch (sourceType->getKind()) {
    case MetadataKind::Class:
    case MetadataKind::ObjCClassWrapper: {
      // Do a dynamic cast on the instance pointer.
      const void *object
        = *reinterpret_cast<const void * const *>(value);
      if (!swift_dynamicCast(object, targetType))
        return nullptr;
      break;
    }
    case MetadataKind::Existential:
    case MetadataKind::Function:
    case MetadataKind::HeapArray:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::Metatype:
    case MetadataKind::Enum:
    case MetadataKind::Opaque:
    case MetadataKind::PolyFunction:
    case MetadataKind::Struct:
    case MetadataKind::Tuple:
      return nullptr;
    }
    break;
      
  case MetadataKind::Existential:
  case MetadataKind::Function:
  case MetadataKind::HeapArray:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    // The cast succeeds only if the metadata pointers are statically
    // equivalent.
    if (sourceType != targetType)
      return nullptr;
    break;
  }
  
  return value;
}

const OpaqueValue *
swift::swift_dynamicCastIndirectUnconditional(const OpaqueValue *value,
                                              const Metadata *sourceType,
                                              const Metadata *targetType) {
  switch (targetType->getKind()) {
  case MetadataKind::Class:
  case MetadataKind::ObjCClassWrapper:
    // The source value must also be a class; otherwise the cast fails.
    switch (sourceType->getKind()) {
    case MetadataKind::Class:
    case MetadataKind::ObjCClassWrapper: {
      // Do a dynamic cast on the instance pointer.
      const void *object
        = *reinterpret_cast<const void * const *>(value);
      swift_dynamicCastUnconditional(object, targetType);
      break;
    }
    case MetadataKind::Existential:
    case MetadataKind::Function:
    case MetadataKind::HeapArray:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::Metatype:
    case MetadataKind::Enum:
    case MetadataKind::Opaque:
    case MetadataKind::PolyFunction:
    case MetadataKind::Struct:
    case MetadataKind::Tuple:
      abort();
    }
    break;
      
  case MetadataKind::Existential:
  case MetadataKind::Function:
  case MetadataKind::HeapArray:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    // The cast succeeds only if the metadata pointers are statically
    // equivalent.
    if (sourceType != targetType)
      abort();
    break;
  }
  
  return value;  
}

/// The primary entrypoint.
const Metadata *
swift::swift_getGenericMetadata(GenericMetadata *pattern,
                                const void *arguments) {
  auto genericArgs = (const void * const *) arguments;
  size_t numGenericArgs = pattern->NumKeyArguments;

#if SWIFT_DEBUG_RUNTIME
  printf("swift_getGenericMetadata(%p):\n", pattern);
  for (unsigned i = 0; i != numGenericArgs; ++i) {
    printf("  %p\n", genericArgs[i]);
  }
#endif

  if (auto entry = getCache(pattern).find(genericArgs, numGenericArgs)) {
#if SWIFT_DEBUG_RUNTIME
    printf("found in cache!\n");
#endif
    return adjustAddressPoint(entry->getData<Metadata>(numGenericArgs),
                              pattern->AddressPoint);
  }

#if SWIFT_DEBUG_RUNTIME
  printf("not found in cache!\n");
#endif

  // Otherwise, instantiate a new one.
  return instantiateGenericMetadata(pattern, arguments);
}

namespace {
  class ObjCClassCacheEntry : public CacheEntry<ObjCClassCacheEntry> {
    FullMetadata<ObjCClassWrapperMetadata> Metadata;

  public:
    ObjCClassCacheEntry(size_t numArguments) {}

    FullMetadata<ObjCClassWrapperMetadata> *getData() {
      return &Metadata;
    }
    const FullMetadata<ObjCClassWrapperMetadata> *getData() const {
      return &Metadata;
    }

    /// Does this cache entry match the given set of arguments?
    bool matches(const void * const *arguments, size_t numArguments) const {
      assert(numArguments == 1);
      return (arguments[0] == Metadata.Class);
    }
  };
}

/// The uniquing structure for ObjC class-wrapper metadata.
static MetadataCache<ObjCClassCacheEntry> ObjCClassWrappers;

const Metadata *
swift::swift_getObjCClassMetadata(const ClassMetadata *theClass) {
  // If the class pointer is valid as metadata, no translation is required.
  if (theClass->isTypeMetadata()) {
    return theClass;
  }

  // Look for an existing entry.
  const size_t numGenericArgs = 1;
  const void *args[] = { theClass };
  if (auto entry = ObjCClassWrappers.find(args, numGenericArgs)) {
    return entry->getData();
  }

  auto entry = ObjCClassCacheEntry::allocate(args, numGenericArgs, 0);

  auto metadata = entry->getData();
  metadata->setKind(MetadataKind::ObjCClassWrapper);
  metadata->ValueWitnesses = &_TWVBO;
  metadata->Class = theClass;

  return ObjCClassWrappers.add(entry)->getData();
}

namespace {
  class FunctionCacheEntry : public CacheEntry<FunctionCacheEntry> {
    FullMetadata<FunctionTypeMetadata> Metadata;

  public:
    FunctionCacheEntry(size_t numArguments) {}

    FullMetadata<FunctionTypeMetadata> *getData() {
      return &Metadata;
    }
    const FullMetadata<FunctionTypeMetadata> *getData() const {
      return &Metadata;
    }

    /// Does this cache entry match the given set of arguments?
    bool matches(const void * const *arguments, size_t numArguments) const {
      assert(numArguments == 2);
      return (arguments[0] == Metadata.ArgumentType &&
              arguments[1] == Metadata.ResultType);
    }
  };
}

/// The uniquing structure for function type metadata.
static MetadataCache<FunctionCacheEntry> FunctionTypes;


const FunctionTypeMetadata *
swift::swift_getFunctionTypeMetadata(const Metadata *argMetadata,
                                     const Metadata *resultMetadata) {
  const size_t numGenericArgs = 2;

  typedef FullMetadata<FunctionTypeMetadata> FullFunctionTypeMetadata;

  const void *args[] = { argMetadata, resultMetadata };
  if (auto entry = FunctionTypes.find(args, numGenericArgs)) {
    return entry->getData();
  }

  auto entry = FunctionCacheEntry::allocate(args, numGenericArgs, 0);

  auto metadata = entry->getData();
  metadata->setKind(MetadataKind::Function);
  metadata->ValueWitnesses = &_TWVFT_T_; // standard function value witnesses
  metadata->ArgumentType = argMetadata;
  metadata->ResultType = resultMetadata;

  return FunctionTypes.add(entry)->getData();
}

/*** Tuples ****************************************************************/

namespace {
  class TupleCacheEntry : public CacheEntry<TupleCacheEntry> {
  public:
    ValueWitnessTable Witnesses;
    FullMetadata<TupleTypeMetadata> Metadata;

    TupleCacheEntry(size_t numArguments) {
      Metadata.NumElements = numArguments;
    }

    FullMetadata<TupleTypeMetadata> *getData() {
      return &Metadata;
    }
    const FullMetadata<TupleTypeMetadata> *getData() const {
      return &Metadata;
    }

    /// Does this cache entry match the given set of arguments?
    bool matches(const void * const *arguments, size_t numArguments) const {
      // Same number of elements.
      if (numArguments != Metadata.NumElements)
        return false;

      // Arguments match up element-wise.
      for (size_t i = 0; i != numArguments; ++i) {
        if (arguments[i] != Metadata.getElements()[i].Type)
          return false;
      }

      return true;
    }
  };
}

/// The uniquing structure for tuple type metadata.
static MetadataCache<TupleCacheEntry> TupleTypes;

/// Given a metatype pointer, produce the value-witness table for it.
/// This is equivalent to metatype->ValueWitnesses but more efficient.
static const ValueWitnessTable *tuple_getValueWitnesses(const Metadata *metatype) {
  return ((const ValueWitnessTable*) asFullMetadata(metatype)) - 1;
}

/// Generic tuple value witness for 'projectBuffer'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_projectBuffer(ValueBuffer *buffer,
                                        const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  if (IsInline)
    return reinterpret_cast<OpaqueValue*>(buffer);
  else
    return *reinterpret_cast<OpaqueValue**>(buffer);
}

/// Generic tuple value witness for 'allocateBuffer'
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_allocateBuffer(ValueBuffer *buffer,
                                         const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  if (IsInline)
    return reinterpret_cast<OpaqueValue*>(buffer);

  // It's important to use 'stride' instead of 'size' because slowAlloc
  // only guarantees alignment up to a multiple of the value passed.
  auto wtable = tuple_getValueWitnesses(metatype);
  auto value = (OpaqueValue*) swift_slowAlloc(wtable->stride, SWIFT_RAWALLOC);

  *reinterpret_cast<OpaqueValue**>(buffer) = value;
  return value;
}

/// Generic tuple value witness for 'deallocateBuffer'.
template <bool IsPOD, bool IsInline>
static void tuple_deallocateBuffer(ValueBuffer *buffer,
                                   const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  if (IsInline)
    return;

  auto wtable = tuple_getValueWitnesses(metatype);
  auto value = *reinterpret_cast<OpaqueValue**>(buffer);
  swift_slowRawDealloc(value, wtable->stride);
}

/// Generic tuple value witness for 'destroy'.
template <bool IsPOD, bool IsInline>
static void tuple_destroy(OpaqueValue *tuple, const Metadata *_metadata) {
  auto &metadata = *(const TupleTypeMetadata*) _metadata;
  assert(IsPOD == tuple_getValueWitnesses(&metadata)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(&metadata)->isValueInline());

  if (IsPOD) return;

  for (size_t i = 0, e = metadata.NumElements; i != e; ++i) {
    auto &eltInfo = metadata.getElements()[i];
    OpaqueValue *elt = eltInfo.findIn(tuple);
    auto eltWitnesses = eltInfo.Type->getValueWitnesses();
    eltWitnesses->destroy(elt, eltInfo.Type);
  }
}

/// Generic tuple value witness for 'destroyBuffer'.
template <bool IsPOD, bool IsInline>
static void tuple_destroyBuffer(ValueBuffer *buffer, const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  auto tuple = tuple_projectBuffer<IsPOD, IsInline>(buffer, metatype);
  tuple_destroy<IsPOD, IsInline>(tuple, metatype);
  tuple_deallocateBuffer<IsPOD, IsInline>(buffer, metatype);
}

// The operation doesn't have to be initializeWithCopy, but they all
// have basically the same type.
typedef value_witness_types::initializeWithCopy *
  ValueWitnessTable::*forEachOperation;

/// Perform an operation for each field of two tuples.
static OpaqueValue *tuple_forEachField(OpaqueValue *destTuple,
                                       OpaqueValue *srcTuple,
                                       const Metadata *_metatype,
                                       forEachOperation member) {
  auto &metatype = *(const TupleTypeMetadata*) _metatype;
  for (size_t i = 0, e = metatype.NumElements; i != e; ++i) {
    auto &eltInfo = metatype.getElements()[i];
    auto eltValueWitnesses = eltInfo.Type->getValueWitnesses();

    OpaqueValue *destElt = eltInfo.findIn(destTuple);
    OpaqueValue *srcElt = eltInfo.findIn(srcTuple);
    (eltValueWitnesses->*member)(destElt, srcElt, eltInfo.Type);
  }

  return destTuple;
}

/// Perform a naive memcpy of src into dest.
static OpaqueValue *tuple_memcpy(OpaqueValue *dest,
                                 OpaqueValue *src,
                                 const Metadata *metatype) {
  assert(metatype->getValueWitnesses()->isPOD());
  return (OpaqueValue*)
    memcpy(dest, src, metatype->getValueWitnesses()->getSize());
}

/// Generic tuple value witness for 'initializeWithCopy'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_initializeWithCopy(OpaqueValue *dest,
                                             OpaqueValue *src,
                                             const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  if (IsPOD) return tuple_memcpy(dest, src, metatype);
  return tuple_forEachField(dest, src, metatype,
                            &ValueWitnessTable::initializeWithCopy);
}

/// Generic tuple value witness for 'initializeWithTake'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_initializeWithTake(OpaqueValue *dest,
                                             OpaqueValue *src,
                                             const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  if (IsPOD) return tuple_memcpy(dest, src, metatype);
  return tuple_forEachField(dest, src, metatype,
                            &ValueWitnessTable::initializeWithTake);
}

/// Generic tuple value witness for 'assignWithCopy'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_assignWithCopy(OpaqueValue *dest,
                                         OpaqueValue *src,
                                         const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  if (IsPOD) return tuple_memcpy(dest, src, metatype);
  return tuple_forEachField(dest, src, metatype,
                            &ValueWitnessTable::assignWithCopy);
}

/// Generic tuple value witness for 'assignWithTake'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_assignWithTake(OpaqueValue *dest,
                                         OpaqueValue *src,
                                         const Metadata *metatype) {
  if (IsPOD) return tuple_memcpy(dest, src, metatype);
  return tuple_forEachField(dest, src, metatype,
                            &ValueWitnessTable::assignWithTake);
}

/// Generic tuple value witness for 'initializeBufferWithCopy'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_initializeBufferWithCopy(ValueBuffer *dest,
                                                   OpaqueValue *src,
                                                   const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  return tuple_initializeWithCopy<IsPOD, IsInline>(
                        tuple_allocateBuffer<IsPOD, IsInline>(dest, metatype),
                        src,
                        metatype);
}

/// Generic tuple value witness for 'initializeBufferWithTake'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_initializeBufferWithTake(ValueBuffer *dest,
                                                   OpaqueValue *src,
                                                   const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  return tuple_initializeWithTake<IsPOD, IsInline>(
                        tuple_allocateBuffer<IsPOD, IsInline>(dest, metatype),
                        src,
                        metatype);
}

/// Generic tuple value witness for 'initializeBufferWithCopyOfBuffer'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_initializeBufferWithCopyOfBuffer(ValueBuffer *dest,
                                                           ValueBuffer *src,
                                                     const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  return tuple_initializeBufferWithCopy<IsPOD, IsInline>(
                            dest,
                            tuple_projectBuffer<IsPOD, IsInline>(src, metatype),
                            metatype);
}

template <bool IsPOD, bool IsInline>
static const Metadata *tuple_typeOf(OpaqueValue *obj,
                                    const Metadata *metatype) {
  return metatype;
}

/// Various standard witness table for tuples.
static const ValueWitnessTable tuple_witnesses_pod_inline = {
#define TUPLE_WITNESS(NAME) &tuple_##NAME<true, true>,
  FOR_ALL_FUNCTION_VALUE_WITNESSES(TUPLE_WITNESS)
#undef TUPLE_WITNESS
  0,
  ValueWitnessFlags(),
  0
};
static const ValueWitnessTable tuple_witnesses_nonpod_inline = {
#define TUPLE_WITNESS(NAME) &tuple_##NAME<false, true>,
  FOR_ALL_FUNCTION_VALUE_WITNESSES(TUPLE_WITNESS)
#undef TUPLE_WITNESS
  0,
  ValueWitnessFlags(),
  0
};
static const ValueWitnessTable tuple_witnesses_pod_noninline = {
#define TUPLE_WITNESS(NAME) &tuple_##NAME<true, false>,
  FOR_ALL_FUNCTION_VALUE_WITNESSES(TUPLE_WITNESS)
#undef TUPLE_WITNESS
  0,
  ValueWitnessFlags(),
  0
};
static const ValueWitnessTable tuple_witnesses_nonpod_noninline = {
#define TUPLE_WITNESS(NAME) &tuple_##NAME<false, false>,
  FOR_ALL_FUNCTION_VALUE_WITNESSES(TUPLE_WITNESS)
#undef TUPLE_WITNESS
  0,
  ValueWitnessFlags(),
  0
};

namespace {
struct BasicLayout {
  size_t size;
  ValueWitnessFlags flags;
  size_t stride;
  
  static constexpr BasicLayout initialForValueType() {
    return {0, ValueWitnessFlags().withAlignment(1).withPOD(true), 0};
  }
  
  static constexpr BasicLayout initialForHeapObject() {
    return {sizeof(HeapObject),
            ValueWitnessFlags().withAlignment(alignof(HeapObject)),
            sizeof(HeapObject)};
  }
};
  
/// Perform basic sequential layout given a vector of metadata pointers,
/// calling a functor with the offset of each field, and returning the
/// final layout characteristics of the type.
/// FUNCTOR should have signature:
///   void (size_t index, const Metadata *type, size_t offset)
template<typename FUNCTOR>
void performBasicLayout(BasicLayout &layout,
                        const Metadata * const *elements,
                        size_t numElements,
                        FUNCTOR &&f) {
  size_t size = layout.size;
  size_t alignment = layout.flags.getAlignment();
  bool isPOD = layout.flags.isPOD();
  for (unsigned i = 0; i != numElements; ++i) {
    auto elt = elements[i];
    
    // Lay out this element.
    auto eltVWT = elt->getValueWitnesses();
    size = llvm::RoundUpToAlignment(size, eltVWT->getAlignment());

    // Report this record to the functor.
    f(i, elt, size);
    
    // Update the size and alignment of the aggregate..
    size += eltVWT->size;
    alignment = std::max(alignment, eltVWT->getAlignment());
    if (!eltVWT->isPOD()) isPOD = false;
  }
  bool isInline = ValueWitnessTable::isValueInline(size, alignment);
  
  layout.size = size;
  layout.flags = ValueWitnessFlags().withAlignment(alignment)
                                    .withPOD(isPOD)
                                    .withInlineStorage(isInline);
  layout.stride = llvm::RoundUpToAlignment(size, alignment);
}
} // end anonymous namespace

const TupleTypeMetadata *
swift::swift_getTupleTypeMetadata(size_t numElements,
                                  const Metadata * const *elements,
                                  const char *labels,
                                  const ValueWitnessTable *proposedWitnesses) {
  // FIXME: include labels when uniquing!
  auto genericArgs = (const void * const *) elements;
  if (auto entry = TupleTypes.find(genericArgs, numElements)) {
    return entry->getData();
  }

  // We might reasonably get called by generic code, like a demangler
  // that produces type objects.  As long as we sink this below the
  // fast-path map lookup, it doesn't really cost us anything.
  if (numElements == 0) return &_TMdT_;

  typedef TupleTypeMetadata::Element Element;

  // Allocate the tuple cache entry, which includes space for both the
  // metadata and a value-witness table.
  auto entry = TupleCacheEntry::allocate(genericArgs, numElements,
                                         numElements * sizeof(Element));

  auto witnesses = &entry->Witnesses;

  auto metadata = entry->getData();
  metadata->setKind(MetadataKind::Tuple);
  metadata->ValueWitnesses = witnesses;
  metadata->NumElements = numElements;
  metadata->Labels = labels;

  // Perform basic layout on the tuple.
  auto layout = BasicLayout::initialForValueType();
  performBasicLayout(layout, elements, numElements,
    [&](size_t i, const Metadata *elt, size_t offset) {
      metadata->getElements()[i].Type = elt;
      metadata->getElements()[i].Offset = offset;
    });
  
  witnesses->size = layout.size;
  witnesses->flags = layout.flags;
  witnesses->stride = layout.stride;

  // Copy the function witnesses in, either from the proposed
  // witnesses or from the standard table.
  if (!proposedWitnesses) {
    // For a tuple with a single element, just use the witnesses for
    // the element type.
    if (numElements == 1) {
      proposedWitnesses = elements[0]->getValueWitnesses();

    // Otherwise, use generic witnesses (when we can't pattern-match
    // into something better).
    } else if (layout.flags.isInlineStorage()
               && layout.flags.isPOD()) {
      if (layout.size == 8) proposedWitnesses = &_TWVBi64_;
      else if (layout.size == 4) proposedWitnesses = &_TWVBi32_;
      else if (layout.size == 2) proposedWitnesses = &_TWVBi16_;
      else if (layout.size == 1) proposedWitnesses = &_TWVBi8_;
      else proposedWitnesses = &tuple_witnesses_pod_inline;
    } else if (layout.flags.isInlineStorage()
               && !layout.flags.isPOD()) {
      proposedWitnesses = &tuple_witnesses_nonpod_inline;
    } else if (!layout.flags.isInlineStorage()
               && layout.flags.isPOD()) {
      proposedWitnesses = &tuple_witnesses_pod_noninline;
    } else {
      assert(!layout.flags.isInlineStorage()
             && !layout.flags.isPOD());
      proposedWitnesses = &tuple_witnesses_nonpod_noninline;
    }
  }
#define ASSIGN_TUPLE_WITNESS(NAME) \
  witnesses->NAME = proposedWitnesses->NAME;
  FOR_ALL_FUNCTION_VALUE_WITNESSES(ASSIGN_TUPLE_WITNESS)
#undef ASSIGN_TUPLE_WITNESS

  return TupleTypes.add(entry)->getData();
}

const TupleTypeMetadata *
swift::swift_getTupleTypeMetadata2(const Metadata *elt0, const Metadata *elt1,
                                   const char *labels,
                                   const ValueWitnessTable *proposedWitnesses) {
  const Metadata *elts[] = { elt0, elt1 };
  return swift_getTupleTypeMetadata(2, elts, labels, proposedWitnesses);
}

const TupleTypeMetadata *
swift::swift_getTupleTypeMetadata3(const Metadata *elt0, const Metadata *elt1,
                                   const Metadata *elt2,
                                   const char *labels,
                                   const ValueWitnessTable *proposedWitnesses) {
  const Metadata *elts[] = { elt0, elt1, elt2 };
  return swift_getTupleTypeMetadata(3, elts, labels, proposedWitnesses);
}

/*** Structs ***************************************************************/

/// Initialize the value witness table and struct field offset vector for a
/// struct, using the "Universal" layout strategy.
void swift::swift_initStructMetadata_UniversalStrategy(size_t numFields,
                                     const Metadata * const *fieldTypes,
                                     size_t *fieldOffsets,
                                     ValueWitnessTable *vwtable) {
  auto layout = BasicLayout::initialForValueType();
  performBasicLayout(layout, fieldTypes, numFields,
    [&](size_t i, const Metadata *fieldType, size_t offset) {
      fieldOffsets[i] = offset;
    });
  
  vwtable->size = layout.size;
  vwtable->flags = layout.flags;
  vwtable->stride = layout.stride;
}

/*** Classes ***************************************************************/

/// Initialize the field offset vector for a dependent-layout class, using the
/// "Universal" layout strategy.
void swift::swift_initClassMetadata_UniversalStrategy(ClassMetadata *self,
                                            const ClassMetadata *super,
                                            size_t numFields,
                                            const Metadata * const *fieldTypes,
                                            size_t *fieldOffsets) {
  // Start layout by appending to a standard heap object header.
  auto layout = BasicLayout::initialForHeapObject();
  // If we have a superclass, start from its size and alignment instead.
  if (super) {
    layout.size = super->InstanceSize;
    layout.flags = layout.flags.withAlignmentMask(super->InstanceAlignMask);
    layout.stride = llvm::RoundUpToAlignment(super->InstanceSize,
                                             super->InstanceAlignMask+1);
  }
  
  performBasicLayout(layout, fieldTypes, numFields,
    [&](size_t i, const Metadata *fieldType, size_t offset) {
      fieldOffsets[i] = offset;
    });

  // Save the final size and alignment into the metadata record.
  self->InstanceSize = layout.size;
  self->InstanceAlignMask = layout.flags.getAlignmentMask();
}

/*** Metatypes *************************************************************/

namespace {
  class MetatypeCacheEntry : public CacheEntry<MetatypeCacheEntry> {
    FullMetadata<MetatypeMetadata> Metadata;

  public:
    MetatypeCacheEntry(size_t numArguments) {}

    FullMetadata<MetatypeMetadata> *getData() {
      return &Metadata;
    }
    const FullMetadata<MetatypeMetadata> *getData() const {
      return &Metadata;
    }

    /// Does this cache entry match the given set of arguments?
    bool matches(const void * const *arguments, size_t numArguments) const {
      assert(numArguments == 1);
      return (arguments[0] == Metadata.InstanceType);
    }
  };
}

/// The uniquing structure for metatype type metadata.
static MetadataCache<MetatypeCacheEntry> MetatypeTypes;

/// \brief Find the appropriate value witness table for the given type.
static const ValueWitnessTable *
getMetatypeValueWitnesses(const Metadata *instanceType) {
  // The following metatypes have non-trivial representation
  // in the concrete:
  //   - class types
  //   - metatypes of types that require value witnesses

  // For class types, return the unmanaged-pointer witnesses.
  if (instanceType->isClassType())
    return &getUnmanagedPointerValueWitnesses();

  // Metatypes preserve the triviality of their instance type.
  if (instanceType->getKind() == MetadataKind::Metatype)
    return instanceType->getValueWitnesses();

  // Everything else is trivial and can use the empty-tuple metadata.
  return &_TWVT_;
}

/// \brief Fetch a uniqued metadata for a metatype type.
extern "C" const MetatypeMetadata *
swift::swift_getMetatypeMetadata(const Metadata *instanceMetadata) {
  const size_t numGenericArgs = 1;

  const void *args[] = { instanceMetadata };
  if (auto entry = MetatypeTypes.find(args, numGenericArgs)) {
    return entry->getData();
  }

  auto entry = MetatypeCacheEntry::allocate(args, numGenericArgs, 0);

  auto metadata = entry->getData();
  metadata->setKind(MetadataKind::Metatype);
  metadata->ValueWitnesses = getMetatypeValueWitnesses(instanceMetadata);
  metadata->InstanceType = instanceMetadata;

  return MetatypeTypes.add(entry)->getData();
}

/*** Existential types ********************************************************/

namespace {
  class ExistentialCacheEntry : public CacheEntry<ExistentialCacheEntry> {
  public:
    FullMetadata<ExistentialTypeMetadata> Metadata;

    ExistentialCacheEntry(size_t numArguments) {
      Metadata.Protocols.NumProtocols = numArguments;
    }

    FullMetadata<ExistentialTypeMetadata> *getData() {
      return &Metadata;
    }
    const FullMetadata<ExistentialTypeMetadata> *getData() const {
      return &Metadata;
    }

    /// Does this cache entry match the given set of arguments?
    bool matches(const void * const *arguments, size_t numArguments) const {
      // Same number of elements.
      if (numArguments != Metadata.Protocols.NumProtocols)
        return false;

      // Arguments match up element-wise.
      // The arguments must be sorted prior to searching the cache!
      for (size_t i = 0; i != numArguments; ++i) {
        if (arguments[i] != Metadata.Protocols[i])
          return false;
      }

      return true;
    }
  };
}

/// The uniquing structure for existential type metadata.
static MetadataCache<ExistentialCacheEntry> ExistentialTypes;

namespace {

/// Template parameter for the below templates that instantiates for a variable
/// number of witnesses.
static const unsigned VariableValueWitnesses = ~0U;
  
/// Prefab value witnesses for existential containers without a class constraint
/// and a fixed number of protocol witness table slots.
template<unsigned NUM_WITNESS_TABLES>
struct OpaqueExistentialValueWitnesses {
  /// The ABI layout of an opaque existential container.
  struct Container; /* {
    // Specializations have the following members:
                     
    // Metadata pointer.
    const Metadata *metadata;
    // Get a reference to the nth witness table.
    const void *&getWitness(unsigned i);
    const void *getWitness(unsigned i) const;
                     
    // Get a reference to the fixed-sized buffer for the value.
    ValueBuffer &getValueBuffer(const Metadata *self);
    const ValueBuffer &getValueBuffer(const Metadata *self) const;
                     
    // The size of the container.
    static unsigned size(const Metadata *self);
    // The alignment of the container.
    static unsigned alignment(const Metadata *self);
    // The stride of the container.
    static unsigned stride(const Metadata *self);
  }; */
  
  static void destroyBuffer(ValueBuffer *buffer, const Metadata *self) {
    auto value = projectBuffer(buffer, self);
    destroy(value, self);
  }
  
  static Container *initializeBufferWithCopyOfBuffer(ValueBuffer *dest,
                                                     ValueBuffer *src,
                                                     const Metadata *self) {
    auto destValue = allocateBuffer(dest, self),
         srcValue  = projectBuffer (src,  self);
    initializeWithCopy(destValue, srcValue, self);
    return destValue;
  }

  static Container *projectBuffer(ValueBuffer *buffer, const Metadata *self) {
    /// Opaque existentials never fit in a fixed-size buffer. They contain one
    /// as part of themselves.
    return *reinterpret_cast<Container**>(buffer);
  }

  static void deallocateBuffer(ValueBuffer *buffer, const Metadata *self) {
    swift_slowRawDealloc(projectBuffer(buffer, self), Container::size(self));
  }
  
  static void destroy(Container *value, const Metadata *self) {
    value->metadata->getValueWitnesses()
      ->destroyBuffer(value->getValueBuffer(self), value->metadata);
    value->metadata->getValueWitnesses()
      ->deallocateBuffer(value->getValueBuffer(self), value->metadata);
  }
  
  static Container *initializeBufferWithCopy(ValueBuffer *dest,
                                             Container *src,
                                             const Metadata *self) {
    auto destValue = allocateBuffer(dest, self);
    return initializeWithCopy(destValue, src, self);
  }
  
  static Container *initializeWithCopy(Container *dest,
                                       Container *src,
                                       const Metadata *self) {
    dest->metadata = src->metadata;
    for (unsigned i = 0; i != NUM_WITNESS_TABLES; ++i)
      dest->getWitness(i) = src->getWitness(i);
    src->metadata->getValueWitnesses()
      ->initializeBufferWithCopyOfBuffer(dest->getValueBuffer(self),
                                         src->getValueBuffer(self),
                                         src->metadata);
    return dest;
  }
  
  static Container *assignWithCopy(Container *dest,
                                   Container *src,
                                   const Metadata *self) {
    // If doing a self-assignment, we're done.
    if (dest == src)
      return dest;
    
    // Do the metadata records match?
    if (dest->metadata == src->metadata) {
      // If so, project down to the buffers and do direct assignment.
      auto destValue = dest->metadata->getValueWitnesses()
        ->projectBuffer(dest->getValueBuffer(self), dest->metadata);
      auto srcValue = src->metadata->getValueWitnesses()
        ->projectBuffer(src->getValueBuffer(self), src->metadata);
      
      dest->metadata->getValueWitnesses()->assignWithCopy(destValue, srcValue,
                                                          dest->metadata);
      return dest;
    }
    
    // Otherwise, destroy and copy-initialize.
    // TODO: should we copy-initialize and then destroy?  That's
    // possible if we copy aside, which is a small expense but
    // always safe.  Otherwise the destroy (which can invoke user code)
    // could see invalid memory at this address.  These are basically
    // the madnesses that boost::variant has to go through, with the
    // advantage of address-invariance.

    destroy(dest, self);
    return initializeWithCopy(dest, src, self);
  }
  
  static Container *initializeBufferWithTake(ValueBuffer *dest,
                                             Container *src,
                                             const Metadata *self) {
    auto destValue = allocateBuffer(dest, self);
    return initializeWithTake(destValue, src, self);
  }
  
  static Container *initializeWithTake(Container *dest,
                                       Container *src,
                                       const Metadata *self) {
    dest->metadata = src->metadata;
    for (unsigned i = 0; i != NUM_WITNESS_TABLES; ++i)
      dest->getWitness(i) = src->getWitness(i);
    auto srcValue = src->metadata->getValueWitnesses()
      ->projectBuffer(src->getValueBuffer(self), src->metadata);
    
    src->metadata->getValueWitnesses()
      ->initializeBufferWithTake(dest->getValueBuffer(self), srcValue,
                                 src->metadata);
    return dest;
  }
  
  static Container *assignWithTake(Container *dest,
                                   Container *src,
                                   const Metadata *self) {
    destroy(dest, self);
    return initializeWithTake(dest, src, self);
  }
  
  static Container *allocateBuffer(ValueBuffer *dest, const Metadata *self) {
    Container **valuePtr = reinterpret_cast<Container**>(dest);
    *valuePtr
      = reinterpret_cast<Container*>(swift_slowAlloc(Container::size(self),
                                                     SWIFT_RAWALLOC));
    return *valuePtr;
  }
  
  static const Metadata *typeOf(Container *obj, const Metadata *self) {
    auto value = obj->metadata->getValueWitnesses()
      ->projectBuffer(obj->getValueBuffer(self), obj->metadata);
    return obj->metadata->getValueWitnesses()->typeOf(value, obj->metadata);
  }
  
  static const ValueWitnessTable ValueWitnessTable;
};

/// Fixed-size existential container.
template<unsigned NUM_VALUE_WITNESSES>
struct OpaqueExistentialValueWitnesses<NUM_VALUE_WITNESSES>::Container {
  // Metadata pointer.
  const Metadata *metadata;
  // Protocol witness tables.
  const void *_witnesses[NUM_VALUE_WITNESSES];
  // Fixed-size buffer.
  ValueBuffer valueBuffer;
  
  // Get a reference to the nth witness table.
  const void *&getWitness(unsigned i) { return _witnesses[i]; }
  const void *getWitness(unsigned i) const { return _witnesses[i]; }
  
  // Get a reference to the fixed-sized buffer for the value.
  ValueBuffer *getValueBuffer(const Metadata *self) { return &valueBuffer; }
  const ValueBuffer *getValueBuffer(const Metadata *self) const {
    return &valueBuffer;
  }
  
  // The size of the container.
  static unsigned size(const Metadata *self) { return sizeof(Container); }
  static constexpr unsigned size() { return sizeof(Container); }
  // The alignment of the container.
  static unsigned alignment(const Metadata *self) { return alignof(Container); }
  static constexpr unsigned alignment() { return alignof(Container); }
  // The stride of the container.
  static unsigned stride(const Metadata *self) { return sizeof(Container); }
  static constexpr unsigned stride() { return sizeof(Container); }
};
  
/// Variable-sized existential container.
template<>
struct OpaqueExistentialValueWitnesses<VariableValueWitnesses>::Container {
  // Metadata pointer.
  const Metadata *metadata;

  const void **_getWitnesses() {
    return reinterpret_cast<const void**>(this + 1);
  }
  const void * const *_getWitnesses() const {
    return reinterpret_cast<const void* const *>(this + 1);
  }
  
  const void *&getWitness(unsigned i) { return _getWitnesses()[i]; }
  const void * const &getWitness(unsigned i) const {
    return _getWitnesses()[i];
  }
  
  ValueBuffer *getValueBuffer(const Metadata *self) {
    auto existSelf = static_cast<const ExistentialTypeMetadata*>(self);
    return reinterpret_cast<ValueBuffer*>(
                           &getWitness(existSelf->Flags.getNumWitnessTables()));
  }
  const ValueBuffer *getValueBuffer(const Metadata *self) const {
    auto existSelf = static_cast<const ExistentialTypeMetadata*>(self);
    return reinterpret_cast<const ValueBuffer *>(
                           &getWitness(existSelf->Flags.getNumWitnessTables()));
  }

  static unsigned size(unsigned numWitnessTables) {
    return sizeof(const Metadata *) + sizeof(ValueBuffer)
      + sizeof(const void *) * numWitnessTables;
  }
  static unsigned size(const Metadata *self) {
    auto existSelf = static_cast<const ExistentialTypeMetadata*>(self);
    return size(existSelf->Flags.getNumWitnessTables());
  }
  
  static unsigned alignment(unsigned numWitnessTables) {
    return alignof(void*);
  }
  static unsigned alignment(const Metadata *self) {
    return alignof(void*);
  }

  static unsigned stride(unsigned numWitnessTables) {
    return size(numWitnessTables);
  }
  static unsigned stride(const Metadata *self) {
    return size(self);
  }
};
  
template<unsigned NUM_VALUE_WITNESSES>
const ValueWitnessTable
OpaqueExistentialValueWitnesses<NUM_VALUE_WITNESSES>::ValueWitnessTable = {
#define FIXED_OPAQUE_EXISTENTIAL_WITNESS(WITNESS) \
  (value_witness_types::WITNESS*)WITNESS,
  
  FOR_ALL_FUNCTION_VALUE_WITNESSES(FIXED_OPAQUE_EXISTENTIAL_WITNESS)
#undef FIXED_OPAQUE_EXISTENTIAL_WITNESS
  /*size*/ Container::size(),
  /*flags*/ ValueWitnessFlags().withAlignment(Container::alignment())
    .withPOD(false)
    .withInlineStorage(false)
    .withExtraInhabitants(false),
  /*stride*/ Container::stride()
};

template<unsigned NUM_WITNESS_TABLES>
struct ClassExistentialValueWitnesses {
  /// The ABI layout of a class-constrained existential container.
  struct Container {
    const void *witnesses[NUM_WITNESS_TABLES];
    HeapObject *value;
  };
};
  
} // end anonymous namespace

static std::unordered_map<unsigned, const ValueWitnessTable*>
  OpaqueExistentialValueWitnessTables;

/// Instantiate a value witness table for an opaque existential container with
/// the given number of witness table pointers.
static const ValueWitnessTable *
existential_instantiateOpaqueValueWitnesses(unsigned numWitnessTables) {
  auto found = OpaqueExistentialValueWitnessTables.find(numWitnessTables);
  if (found != OpaqueExistentialValueWitnessTables.end())
    return found->second;
  
  using VarOpaqueValueWitnesses
    = OpaqueExistentialValueWitnesses<VariableValueWitnesses>;
  
  auto *vwt = new ValueWitnessTable;
#define STORE_VAR_OPAQUE_EXISTENTIAL_WITNESS(WITNESS) \
  vwt->WITNESS = (value_witness_types::WITNESS*)      \
    VarOpaqueValueWitnesses::WITNESS;
  FOR_ALL_FUNCTION_VALUE_WITNESSES(STORE_VAR_OPAQUE_EXISTENTIAL_WITNESS)
#undef STORE_VAR_OPAQUE_EXISTENTIAL_WITNESS
  
  vwt->size = VarOpaqueValueWitnesses::Container::size(numWitnessTables);
  vwt->flags = ValueWitnessFlags()
    .withAlignment(VarOpaqueValueWitnesses::Container::alignment(numWitnessTables))
    .withPOD(false)
    .withInlineStorage(false)
    .withExtraInhabitants(false);
  vwt->stride = VarOpaqueValueWitnesses::Container::stride(numWitnessTables);

  OpaqueExistentialValueWitnessTables.insert({numWitnessTables, vwt});
  
  return vwt;
}

/// Get the value witness table for an existential type, first trying to use a
/// shared specialized table for common cases.
static const ValueWitnessTable *
existential_getValueWitnesses(ProtocolClassConstraint classConstraint,
                              unsigned numWitnessTables) {
  // Pattern-match common cases.

  switch (classConstraint) {
  case ProtocolClassConstraint::Class:
    // A class-constrained existential with no witness tables can use the
    // Builtin.ObjCPointer witnesses.
    if (numWitnessTables == 0)
      return &_TWVBO;

    return nullptr;
    /* TODO
    // Use statically-instantiated witnesses for the common case of a
    // one-witness-table class existential.
    if (numWitnessTables == 1)
      return &ClassExistentialValueWitnesses<1>::ValueWitnessTable;
    
    // Otherwise, use dynamic value witnesses.
    return existential_getClassValueWitnesses(numWitnessTables);
     */

  case ProtocolClassConstraint::Any:
    // Use statically-instantiated witnesses for the common cases of zero- or
    // one-witness-table opaque existentials.
    if (numWitnessTables == 0)
      return &OpaqueExistentialValueWitnesses<0>::ValueWitnessTable;
    if (numWitnessTables == 1)
      return &OpaqueExistentialValueWitnesses<1>::ValueWitnessTable;
    
    // Otherwise, use dynamic value witnesses.
    return existential_instantiateOpaqueValueWitnesses(numWitnessTables);
  }
}

/// \brief Fetch a uniqued metadata for an existential type. The array
/// referenced by \c protocols will be sorted in-place.
const ExistentialTypeMetadata *
swift::swift_getExistentialMetadata(size_t numProtocols,
                                  const ProtocolDescriptor **protocols) {
  // Sort the protocol set.
  std::sort(protocols, protocols + numProtocols);
  
  // Calculate the class constraint and number of witness tables for the
  // protocol set.
  unsigned numWitnessTables = 0;
  ProtocolClassConstraint classConstraint = ProtocolClassConstraint::Any;
  for (auto p : make_range(protocols, protocols + numProtocols)) {
    if (p->Flags.needsWitnessTable()) {
      printf("%s\n", p->Name);
      ++numWitnessTables;
    }
    if (p->Flags.getClassConstraint() == ProtocolClassConstraint::Class)
      classConstraint = ProtocolClassConstraint::Class;
  }
  
  auto protocolArgs = reinterpret_cast<const void * const *>(protocols);
  
  if (auto entry = ExistentialTypes.find(protocolArgs, numProtocols)) {
    return entry->getData();
  }
  
  auto entry = ExistentialCacheEntry::allocate(protocolArgs, numProtocols,
                             sizeof(const ProtocolDescriptor *) * numProtocols);
  auto metadata = entry->getData();
  metadata->setKind(MetadataKind::Existential);
  metadata->ValueWitnesses = existential_getValueWitnesses(classConstraint,
                                                           numWitnessTables);
  metadata->Flags = ExistentialTypeFlags()
    .withNumWitnessTables(numWitnessTables)
    .withClassConstraint(classConstraint);
  metadata->Protocols.NumProtocols = numProtocols;
  for (size_t i = 0; i < numProtocols; ++i)
    metadata->Protocols[i] = protocols[i];
  
  return ExistentialTypes.add(entry)->getData();
}
