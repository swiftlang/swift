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
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include <algorithm>
#include <dlfcn.h>
#include <new>
#include <string.h>
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Hashing.h"

#ifndef SWIFT_DEBUG_RUNTIME
#define SWIFT_DEBUG_RUNTIME 0
#endif

using namespace swift;

namespace {
  template <class Entry> class MetadataCache;

  /// A CRTP class for defining entries in a metadata cache.
  template <class Impl> class alignas(void*) CacheEntry {
    const Impl *Next;
    friend class MetadataCache<Impl>;

    CacheEntry(const CacheEntry &other) = delete;
    void operator=(const CacheEntry &other) = delete;

    Impl *asImpl() { return static_cast<Impl*>(this); }
    const Impl *asImpl() const { return static_cast<const Impl*>(this); }

  protected:
    CacheEntry(unsigned NumArguments) : NumArguments(NumArguments) {}

  public:
    const unsigned NumArguments;
    
    static Impl *allocate(const void * const *arguments,
                          size_t numArguments, size_t payloadSize) {
      void *buffer = operator new(sizeof(Impl)  +
                                  numArguments * sizeof(void*) +
                                  payloadSize);
      void *resultPtr = (char*)buffer + numArguments * sizeof(void*);
      auto result = new (resultPtr) Impl(numArguments);

      // Copy the arguments into the right place for the key.
      memcpy(buffer, arguments,
             numArguments * sizeof(void*));

      return result;
    }

    void **getArgumentsBuffer() {
      return reinterpret_cast<void**>(this) - NumArguments;
    }
    void * const *getArgumentsBuffer() const {
      return reinterpret_cast<void * const*>(this) - NumArguments;
    }

    template <class T> T *getData() {
      return reinterpret_cast<T *>(asImpl() + 1);
    }
    template <class T> const T *getData() const {
      return const_cast<CacheEntry*>(this)->getData<T>();
    }
    
    static const Impl *fromArgumentsBuffer(const void * const *argsBuffer,
                                           unsigned numArguments) {
      return reinterpret_cast<const Impl *>(argsBuffer + numArguments);
    }
  };
  
  // A wrapper around a pointer to a metadata cache entry that provides
  // DenseMap semantics that compare values in the key vector for the metadata
  // instance.
  //
  // This is stored as a pointer to the arguments buffer, so that we can save
  // an offset while looking for the matching argument given a key.
  template<class Entry>
  class EntryRef {
    const void * const *args;
    unsigned length;
    
    EntryRef(const void * const *args, unsigned length)
    : args(args), length(length)
    {}

    friend struct llvm::DenseMapInfo<EntryRef>;
  public:
    static EntryRef forEntry(const Entry *e, unsigned numArguments) {
      return EntryRef(e->getArgumentsBuffer(), numArguments);
    }
    
    static EntryRef forArguments(const void * const *args,
                                 unsigned numArguments) {
      return EntryRef(args, numArguments);
    }
    
    const Entry *getEntry() const {
      return Entry::fromArgumentsBuffer(args, length);
    }
    
    const void * const *begin() const { return args; }
    const void * const *end() const { return args + length; }
    unsigned size() const { return length; }
  };
}

namespace llvm {
  template<class Entry>
  struct DenseMapInfo<EntryRef<Entry>> {
    static inline EntryRef<Entry> getEmptyKey() {
      // {nullptr, 0} is a legitimate "no arguments" representation.
      return {(const void * const *)UINTPTR_MAX, 1};
    }
    
    static inline EntryRef<Entry> getTombstoneKey() {
      return {(const void * const *)UINTPTR_MAX, 2};
    }
    
    static inline unsigned getHashValue(EntryRef<Entry> val) {
      llvm::hash_code hash
        = llvm::hash_combine_range(val.begin(), val.end());
      return (unsigned)hash;
    }
    
    static inline bool isEqual(EntryRef<Entry> a, EntryRef<Entry> b) {
      unsigned asize = a.size(), bsize = b.size();
      if (asize != bsize)
        return false;
      auto abegin = a.begin(), bbegin = b.begin();
      if (abegin == (const void * const *)UINTPTR_MAX
          || bbegin == (const void * const *)UINTPTR_MAX)
        return abegin == bbegin;
      for (unsigned i = 0; i < asize; ++i) {
        if (abegin[i] != bbegin[i])
          return false;
      }
      return true;
    }
  };
}

namespace {
  /// A CacheEntry implementation where the entries in the cache may
  /// have different numbers of arguments.
  class HeterogeneousCacheEntry : public CacheEntry<HeterogeneousCacheEntry> {
  public:
    HeterogeneousCacheEntry(size_t numArguments) : CacheEntry(numArguments) {}
  };

  /// A CacheEntry implementation where all the entries in the cache
  /// have the same number of arguments.
  class HomogeneousCacheEntry : public CacheEntry<HomogeneousCacheEntry> {
  public:
    HomogeneousCacheEntry(size_t numArguments) : CacheEntry(numArguments) {}
  };

  /// The implementation of a metadata cache.  Note that all-zero must
  /// be a valid state for the cache.
  template <class Entry> class MetadataCache {
    /// The head of a linked list connecting all the metadata cache entries.
    /// TODO: Remove this when LLDB is able to understand the final data
    /// structure for the metadata cache.
    const Entry *Head;
    
    /// The lookup table for cached entries.
    /// TODO: Consider a more tuned hashtable implementation.
    llvm::DenseMap<EntryRef<Entry>, bool> Entries;

  public:
    /// Try to find an existing entry in this cache.
    const Entry *find(const void * const *arguments, size_t numArguments) const{
      auto found
        = Entries.find(EntryRef<Entry>::forArguments(arguments, numArguments));
      if (found == Entries.end())
        return nullptr;
      return found->first.getEntry();
    }

    /// Add the given entry to the cache, taking responsibility for
    /// it.  Returns the entry that should be used, which might not be
    /// the same as the argument if we lost a race to instantiate it.
    /// Regardless, the argument should be considered potentially
    /// invalid after this call.
    ///
    /// FIXME: This doesn't actually handle races yet.
    const Entry *add(Entry *entry) {
      // Maintain the linked list.
      /// TODO: Remove this when LLDB is able to understand the final data
      /// structure for the metadata cache.
      entry->Next = Head;
      Head = entry;
      
      Entries[EntryRef<Entry>::forEntry(entry, entry->NumArguments)]
        = true;
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
  auto fullMetadata = entry->getData<Metadata>();
  memcpy(fullMetadata, pattern->getMetadataTemplate(), pattern->MetadataSize);

  // Fill in the missing spaces from the arguments using the pattern's fill
  // function.
  pattern->FillFunction(fullMetadata, arguments);

  // The metadata is now valid.

  // Add the cache to the list.  This can in theory be made thread-safe,
  // but really this should use a non-linear lookup algorithm.
  auto canonFullMetadata =
    getCache(pattern).add(entry)->getData<Metadata>();
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
#else
  const ClassMetadata *isa = *reinterpret_cast<ClassMetadata *const*>(object);
  do {
    if (isa == targetType) {
      return object;
    }
    isa = isa->SuperClass;
  } while (isa);
  return NULL;
#endif
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
#else
  const ClassMetadata *isa = *reinterpret_cast<ClassMetadata *const*>(object);
  do {
    if (isa == targetType) {
      return object;
    }
    isa = isa->SuperClass;
  } while (isa);
  abort();
#endif
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
    auto metadata = adjustAddressPoint(entry->getData<Metadata>(),
                                       pattern->AddressPoint);
#if SWIFT_DEBUG_RUNTIME
    printf(" -> %p\n", metadata);
#endif
    return metadata;
  }


  // Otherwise, instantiate a new one.
#if SWIFT_DEBUG_RUNTIME
  printf("not found in cache!\n");
#endif
  auto metadata = instantiateGenericMetadata(pattern, arguments);
#if SWIFT_DEBUG_RUNTIME
  printf(" -> %p\n", metadata);
#endif

  return metadata;
}

/// Fast entry points.
const Metadata *
swift::swift_getGenericMetadata1(GenericMetadata *pattern, const void*argument){
  return swift_getGenericMetadata(pattern, &argument);
}

const Metadata *
swift::swift_getGenericMetadata2(GenericMetadata *pattern,
                                 const void *arg0, const void *arg1) {
  const void *args[] = {arg0, arg1};
  return swift_getGenericMetadata(pattern, args);
}

const Metadata *
swift::swift_getGenericMetadata3(GenericMetadata *pattern,
                                 const void *arg0,
                                 const void *arg1,
                                 const void *arg2) {
  const void *args[] = {arg0, arg1, arg2};
  return swift_getGenericMetadata(pattern, args);
}

const Metadata *
swift::swift_getGenericMetadata4(GenericMetadata *pattern,
                                 const void *arg0,
                                 const void *arg1,
                                 const void *arg2,
                                 const void *arg3) {
  const void *args[] = {arg0, arg1, arg2, arg3};
  return swift_getGenericMetadata(pattern, args);
}

namespace {
  class ObjCClassCacheEntry : public CacheEntry<ObjCClassCacheEntry> {
    FullMetadata<ObjCClassWrapperMetadata> Metadata;

  public:
    ObjCClassCacheEntry(size_t numArguments) : CacheEntry(numArguments) {}

    FullMetadata<ObjCClassWrapperMetadata> *getData() {
      return &Metadata;
    }
    const FullMetadata<ObjCClassWrapperMetadata> *getData() const {
      return &Metadata;
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
    FunctionCacheEntry(size_t numArguments) : CacheEntry(numArguments) {}

    FullMetadata<FunctionTypeMetadata> *getData() {
      return &Metadata;
    }
    const FullMetadata<FunctionTypeMetadata> *getData() const {
      return &Metadata;
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

    TupleCacheEntry(size_t numArguments) : CacheEntry(numArguments) {
      Metadata.NumElements = numArguments;
    }
    
    FullMetadata<TupleTypeMetadata> *getData() {
      return &Metadata;
    }
    const FullMetadata<TupleTypeMetadata> *getData() const {
      return &Metadata;
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
  auto value = (OpaqueValue*) swift_slowAlloc(wtable->stride, 0);

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
  swift_slowDealloc(value, wtable->stride);
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
#if SWIFT_DEBUG_RUNTIME
  printf("looking up tuple type metadata\n");
  for (unsigned i = 0; i < numElements; ++i)
    printf("  %p\n", elements[0]);
#endif

  // FIXME: include labels when uniquing!
  auto genericArgs = (const void * const *) elements;
  if (auto entry = TupleTypes.find(genericArgs, numElements)) {
#if SWIFT_DEBUG_RUNTIME
    printf("found in cache! %p\n", entry->getData());
#endif
    return entry->getData();
  }
  
#if SWIFT_DEBUG_RUNTIME
  printf("not found in cache!\n");
#endif

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

  auto finalMetadata = TupleTypes.add(entry)->getData();
#if SWIFT_DEBUG_RUNTIME
  printf(" -> %p\n", finalMetadata);
#endif
  return finalMetadata;
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
    MetatypeCacheEntry(size_t numArguments) : CacheEntry(numArguments) {}

    FullMetadata<MetatypeMetadata> *getData() {
      return &Metadata;
    }
    const FullMetadata<MetatypeMetadata> *getData() const {
      return &Metadata;
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
    return &getUnmanagedPointerPointerValueWitnesses();

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

    ExistentialCacheEntry(size_t numArguments) : CacheEntry(numArguments) {
      Metadata.Protocols.NumProtocols = numArguments;
    }

    FullMetadata<ExistentialTypeMetadata> *getData() {
      return &Metadata;
    }
    const FullMetadata<ExistentialTypeMetadata> *getData() const {
      return &Metadata;
    }
  };
}

/// The uniquing structure for existential type metadata.
static MetadataCache<ExistentialCacheEntry> ExistentialTypes;

namespace {

/// Template parameter for the below templates that instantiates for a variable
/// number of witnesses.
static const unsigned VariableValueWitnesses = ~0U;
  
/// Value witnesses for existential containers without a class constraint
/// and an optional fixed number of protocol witness table slots.
template<unsigned NUM_WITNESS_TABLES>
struct OpaqueExistentialValueWitnesses {
  /// The ABI layout of an opaque existential container.
  struct Container; /* {
    // Specializations have the following members:
                     
    // Metadata pointer.
    const Metadata *metadata;
                     
    // Get the number of witness tables.
    static unsigned getNumWitnesses(const Metadata *self);

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
    return initializeWithCopy(destValue, srcValue, self);
  }

  static Container *projectBuffer(ValueBuffer *buffer, const Metadata *self) {
    /// Opaque existentials never fit in a fixed-size buffer. They contain one
    /// as part of themselves.
    return *reinterpret_cast<Container**>(buffer);
  }

  static void deallocateBuffer(ValueBuffer *buffer, const Metadata *self) {
    swift_slowDealloc(projectBuffer(buffer, self), Container::size(self));
  }
  
  static void destroy(Container *value, const Metadata *self) {
    value->metadata->vw_destroyBuffer(value->getValueBuffer(self));
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
    for (unsigned i = 0, e = Container::getNumWitnesses(self); i < e; ++i)
      dest->getWitness(i) = src->getWitness(i);
    src->metadata->vw_initializeBufferWithCopyOfBuffer(
                                         dest->getValueBuffer(self),
                                         src->getValueBuffer(self));
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
      auto destValue = dest->metadata->vw_projectBuffer(
                                                    dest->getValueBuffer(self));
      auto srcValue = src->metadata->vw_projectBuffer(
                                                    src->getValueBuffer(self));
      
      dest->metadata->vw_assignWithCopy(destValue, srcValue);
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
    for (unsigned i = 0, e = Container::getNumWitnesses(self); i < e; ++i)
      dest->getWitness(i) = src->getWitness(i);
    auto srcValue = src->metadata->vw_projectBuffer(src->getValueBuffer(self));
    
    src->metadata->vw_initializeBufferWithTake(dest->getValueBuffer(self),
                                               srcValue);
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
      = reinterpret_cast<Container*>(swift_slowAlloc(Container::size(self), 0));
    return *valuePtr;
  }
  
  static const Metadata *typeOf(Container *obj, const Metadata *self) {
    auto value = obj->metadata->vw_projectBuffer(obj->getValueBuffer(self));
    return obj->metadata->vw_typeOf(value);
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
  
  static unsigned getNumWitnesses(const Metadata *self) {
    return NUM_VALUE_WITNESSES;
  }
  
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
  
/// Fixed-size existential container with no witnesses.
template<>
struct OpaqueExistentialValueWitnesses<0>::Container {
  // Metadata pointer.
  const Metadata *metadata;
  // Fixed-size buffer.
  ValueBuffer valueBuffer;

  static unsigned getNumWitnesses(const Metadata *self) {
    return 0;
  }

  // Get a reference to the nth witness table. This shouldn't happen.
  const void *&getWitness(unsigned i) { abort(); }
  const void *getWitness(unsigned i) const { abort(); }
  
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

  static unsigned getNumWitnesses(const Metadata *self) {
    auto existSelf = static_cast<const ExistentialTypeMetadata*>(self);
    return existSelf->Flags.getNumWitnessTables();
  }

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

static llvm::DenseMap<unsigned, const ValueWitnessTable*>
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

/// Value witnesses for existential containers with a class constraint
/// and an optional fixed number of protocol witness table slots.
template<unsigned NUM_WITNESS_TABLES>
struct ClassExistentialValueWitnesses {
  /// The ABI layout of a class-constrained existential container.
  struct Container; /* {
    // Specializations have the following members:
                     
    // Get the number of witnesses.
    static unsigned getNumWitnesses(const Metadata *self);
                     
    // Get a reference to the nth witness table.
    const void *&getWitness(unsigned i);
    const void *getWitness(unsigned i) const;
                     
    // Get a reference to the instance pointer value.
    HeapObject *&getValue(const Metadata *self);
    HeapObject *getValue(const Metadata *self) const;
                     
    // The size of the container.
    static unsigned size(const Metadata *self);
    // The alignment of the container.
    static unsigned alignment(const Metadata *self);
    // The stride of the container.
    static unsigned stride(const Metadata *self);
                     
    // Whether the container fits inline in a fixed-size buffer.
    static bool isInline(const Metadata *self);
  }; */
  
  /// Whether the container fits in a fixed-size buffer without a side
  /// allocation.
  static void destroyBuffer(ValueBuffer *buffer, const Metadata *self) {
    HeapObject *value = projectBuffer(buffer, self)->getValue(self);
    swift_unknownRelease(value);
  }
  
  static Container *initializeBufferWithCopyOfBuffer(ValueBuffer *dest,
                                                     ValueBuffer *src,
                                                     const Metadata *self) {
    auto destValue = allocateBuffer(dest, self),
         srcValue  = projectBuffer (src,  self);
    return initializeWithCopy(destValue, srcValue, self);
  }
  
  static Container *projectBuffer(ValueBuffer *buffer, const Metadata *self) {
    if (Container::isInline(self)) {
      return reinterpret_cast<Container*>(buffer);
    } else {
      return *reinterpret_cast<Container**>(buffer);
    }
  }
  
  static void deallocateBuffer(ValueBuffer *buffer, const Metadata *self) {
    if (!Container::isInline(self))
      swift_slowDealloc(projectBuffer(buffer, self), Container::size(self));
  }
  
  static void destroy(Container *value, const Metadata *self) {
    swift_unknownRelease(value->getValue(self));
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
    for (unsigned i = 0, e = Container::getNumWitnesses(self); i < e; ++i)
      dest->getWitness(i) = src->getWitness(i);
    dest->getValue(self) = (HeapObject*)swift_unknownRetain(src->getValue(self));
    return dest;
  }
  
  static Container *assignWithCopy(Container *dest,
                                   Container *src,
                                   const Metadata *self) {
    for (unsigned i = 0, e = Container::getNumWitnesses(self); i < e; ++i)
      dest->getWitness(i) = src->getWitness(i);
    auto &destValue = dest->getValue(self), srcValue = src->getValue(self);
    auto old = destValue;
    swift_unknownRetain(srcValue);
    destValue = srcValue;
    swift_unknownRelease(old);
    return dest;
  }
  
  static Container *initializeBufferWithTake(ValueBuffer *dest, Container *src,
                                              const Metadata *self) {
    auto destValue = allocateBuffer(dest, self);
    return initializeWithTake(destValue, src, self);
  }
  
  static Container *initializeWithTake(Container *dest, Container *src,
                                       const Metadata *self) {
    for (unsigned i = 0, e = Container::getNumWitnesses(self); i < e; ++i)
      dest->getWitness(i) = src->getWitness(i);
    dest->getValue(self) = src->getValue(self);
    return dest;
  }
  
  static Container *assignWithTake(Container *dest, Container *src,
                                   const Metadata *self) {
    for (unsigned i = 0, e = Container::getNumWitnesses(self); i < e; ++i)
      dest->getWitness(i) = src->getWitness(i);
    swift_unknownRelease(dest->getValue(self));
    dest->getValue(self) = src->getValue(self);
    return dest;
  }
  
  static Container *allocateBuffer(ValueBuffer *dest, const Metadata *self) {
    if (Container::isInline(self))
      return reinterpret_cast<Container*>(dest);

    Container **valuePtr = reinterpret_cast<Container**>(dest);
    *valuePtr
      = reinterpret_cast<Container*>(swift_slowAlloc(Container::size(self), 0));
    return *valuePtr;
  }
  
  static const Metadata *typeOf(Container *obj, const Metadata *self) {
    return swift_unknownTypeOf(obj->getValue(self));
  }
  
  static void storeExtraInhabitant(Container *obj, int index,
                                   const Metadata *self) {
    swift_storeHeapObjectExtraInhabitant(&obj->getValue(self), index, self);
  }
  
  static int getExtraInhabitantIndex(Container *obj,
                                     const Metadata *self) {
    return swift_getHeapObjectExtraInhabitantIndex(&obj->getValue(self), self);
  }
  
  static const ExtraInhabitantsValueWitnessTable ValueWitnessTable;
};
  
/// Fixed-size class-constrained existential container.
template<unsigned NUM_VALUE_WITNESSES>
struct ClassExistentialValueWitnesses<NUM_VALUE_WITNESSES>::Container {
  // Protocol witness tables.
  const void *_witnesses[NUM_VALUE_WITNESSES];
  // Instance pointer.
  HeapObject *_value;
  
  static unsigned getNumWitnesses(const Metadata *self) {
    return NUM_VALUE_WITNESSES;
  }
  
  // Get a reference to the nth witness table.
  const void *&getWitness(unsigned i) { return _witnesses[i]; }
  const void *getWitness(unsigned i) const { return _witnesses[i]; }

  // Get a reference to the instance pointer for the value.
  HeapObject *&getValue(const Metadata *self) { return _value; }
  HeapObject *getValue(const Metadata *self) const { return _value; }
  
  // The size of the container.
  static unsigned size(const Metadata *self) { return sizeof(Container); }
  static constexpr unsigned size() { return sizeof(Container); }
  // The alignment of the container.
  static unsigned alignment(const Metadata *self) { return alignof(Container); }
  static constexpr unsigned alignment() { return alignof(Container); }
  // The stride of the container.
  static unsigned stride(const Metadata *self) { return sizeof(Container); }
  static constexpr unsigned stride() { return sizeof(Container); }
  
  // Whether the container fits in a fixed-size buffer.
  static bool isInline(const Metadata *self) { return isInline(); }
  static constexpr bool isInline() {
    return sizeof(Container) <= sizeof(ValueBuffer)
      && alignof(Container) <= alignof(ValueBuffer);
  }
};
  
/// Fixed-size class-constrained existential container with no witnesses.
template<>
struct ClassExistentialValueWitnesses<0>::Container {
  // Instance pointer.
  HeapObject *_value;
  
  static unsigned getNumWitnesses(const Metadata *self) {
    return 0;
  }
  
  // Get a reference to the nth witness table.
  const void *&getWitness(unsigned i) { abort(); }
  const void *getWitness(unsigned i) const { abort(); }

  // Get a reference to the instance pointer for the value.
  HeapObject *&getValue(const Metadata *self) { return _value; }
  HeapObject *getValue(const Metadata *self) const { return _value; }
  
  // The size of the container.
  static unsigned size(const Metadata *self) { return sizeof(Container); }
  static constexpr unsigned size() { return sizeof(Container); }
  // The alignment of the container.
  static unsigned alignment(const Metadata *self) { return alignof(Container); }
  static constexpr unsigned alignment() { return alignof(Container); }
  // The stride of the container.
  static unsigned stride(const Metadata *self) { return sizeof(Container); }
  static constexpr unsigned stride() { return sizeof(Container); }
  
  // Whether the container fits in a fixed-size buffer.
  static bool isInline(const Metadata *self) { return true; }
  static constexpr bool isInline() { return true; }
};
  
/// Variable-size class-constrained existential container.
template<>
struct ClassExistentialValueWitnesses<VariableValueWitnesses>::Container {
  static unsigned getNumWitnesses(const Metadata *self) {
    auto existSelf = static_cast<const ExistentialTypeMetadata*>(self);
    return existSelf->Flags.getNumWitnessTables();
  }
  
  // Get a reference to the nth witness table.
  void *&getWitness(unsigned i) {
    return reinterpret_cast<void**>(this)[i];
  }
  void * const &getWitness(unsigned i) const {
    return reinterpret_cast<void* const*>(this)[i];
  }
  
  // Get a reference to the instance pointer for the value.
  HeapObject *&getValue(const Metadata *self) {
    return *reinterpret_cast<HeapObject **>(&getWitness(getNumWitnesses(self)));
  }
  HeapObject *getValue(const Metadata *self) const {
    return *reinterpret_cast<HeapObject * const*>
              (&getWitness(getNumWitnesses(self)));
  }
  
  static unsigned size(unsigned numWitnessTables) {
    return sizeof(HeapObject*)
      + sizeof(void *) * numWitnessTables;
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
  
  static bool isInline(unsigned numWitnessTables) {
    return size(numWitnessTables) <= sizeof(ValueBuffer)
      && alignment(numWitnessTables) <= alignof(ValueBuffer);
  }
  static bool isInline(const Metadata *self) {
    auto existSelf = static_cast<const ExistentialTypeMetadata*>(self);
    return isInline(existSelf->Flags.getNumWitnessTables());
  }
};
  
template<unsigned NUM_VALUE_WITNESSES>
const ExtraInhabitantsValueWitnessTable
ClassExistentialValueWitnesses<NUM_VALUE_WITNESSES>::ValueWitnessTable = {
  {
#define FIXED_CLASS_EXISTENTIAL_WITNESS(WITNESS) \
    (value_witness_types::WITNESS*)WITNESS,
  
    FOR_ALL_FUNCTION_VALUE_WITNESSES(FIXED_CLASS_EXISTENTIAL_WITNESS)
    /*size*/ Container::size(),
    /*flags*/ ValueWitnessFlags().withAlignment(Container::alignment())
      .withPOD(false)
      .withInlineStorage(Container::isInline())
      .withExtraInhabitants(true),
    /*stride*/ Container::stride()
  },
  FIXED_CLASS_EXISTENTIAL_WITNESS(storeExtraInhabitant)
  FIXED_CLASS_EXISTENTIAL_WITNESS(getExtraInhabitantIndex)
  ExtraInhabitantFlags()
    .withNumExtraInhabitants(swift_getHeapObjectExtraInhabitantCount())
#undef FIXED_CLASS_EXISTENTIAL_WITNESS
};
  
static llvm::DenseMap<unsigned, const ValueWitnessTable*>
  ClassExistentialValueWitnessTables;

/// Instantiate a value witness table for a class-constrained existential
/// container with the given number of witness table pointers.
static const ValueWitnessTable *
existential_instantiateClassValueWitnesses(unsigned numWitnessTables) {
  auto found = ClassExistentialValueWitnessTables.find(numWitnessTables);
  if (found != ClassExistentialValueWitnessTables.end())
    return found->second;
  
  using VarClassValueWitnesses
    = ClassExistentialValueWitnesses<VariableValueWitnesses>;
  
  auto *vwt = new ExtraInhabitantsValueWitnessTable;
#define STORE_VAR_CLASS_EXISTENTIAL_WITNESS(WITNESS) \
  vwt->WITNESS = (value_witness_types::WITNESS*)      \
    VarClassValueWitnesses::WITNESS;
  FOR_ALL_FUNCTION_VALUE_WITNESSES(STORE_VAR_CLASS_EXISTENTIAL_WITNESS)
  
  vwt->size = VarClassValueWitnesses::Container::size(numWitnessTables);
  vwt->flags = ValueWitnessFlags()
    .withAlignment(VarClassValueWitnesses::Container::alignment(numWitnessTables))
    .withPOD(false)
    .withInlineStorage(false)
    .withExtraInhabitants(true);
  vwt->stride = VarClassValueWitnesses::Container::stride(numWitnessTables);

  STORE_VAR_CLASS_EXISTENTIAL_WITNESS(storeExtraInhabitant)
  STORE_VAR_CLASS_EXISTENTIAL_WITNESS(getExtraInhabitantIndex)
  
  vwt->extraInhabitantFlags = ExtraInhabitantFlags()
    .withNumExtraInhabitants(swift_getHeapObjectExtraInhabitantCount());
#undef STORE_VAR_CLASS_EXISTENTIAL_WITNESS
  
  ClassExistentialValueWitnessTables.insert({numWitnessTables, vwt});
  
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
    // A class-constrained existential with no witness tables can share the
    // Builtin.ObjCPointer witnesses.
    if (numWitnessTables == 0)
      return &_TWVBO;

    // Use statically-instantiated witnesses for the common case of a
    // one-witness-table class existential.
    if (numWitnessTables == 1)
      return &ClassExistentialValueWitnesses<1>::ValueWitnessTable;
    
    // Otherwise, use dynamic value witnesses.
    return existential_instantiateClassValueWitnesses(numWitnessTables);

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
  
} // end anonymous namespace

/// \brief Fetch a uniqued metadata for an existential type. The array
/// referenced by \c protocols will be sorted in-place.
const ExistentialTypeMetadata *
swift::swift_getExistentialTypeMetadata(size_t numProtocols,
                                        const ProtocolDescriptor **protocols) {
  // Sort the protocol set.
  std::sort(protocols, protocols + numProtocols);
  
  // Calculate the class constraint and number of witness tables for the
  // protocol set.
  unsigned numWitnessTables = 0;
  ProtocolClassConstraint classConstraint = ProtocolClassConstraint::Any;
  for (auto p : make_range(protocols, protocols + numProtocols)) {
    if (p->Flags.needsWitnessTable()) {
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

/// \brief Perform a copy-assignment from one existential container to another.
/// Both containers must be of the same existential type representable with no
/// witness tables.
OpaqueValue *swift::swift_assignExistentialWithCopy0(OpaqueValue *dest,
                                                     const OpaqueValue *src,
                                                     const Metadata *type) {
  auto destVal =
    reinterpret_cast<OpaqueExistentialValueWitnesses<0>::Container*>(dest);
  auto srcCVal =
    reinterpret_cast<const OpaqueExistentialValueWitnesses<0>::Container*>(src);
  auto srcVal =
    const_cast<OpaqueExistentialValueWitnesses<0>::Container*>(srcCVal);
  
  auto result =
    OpaqueExistentialValueWitnesses<0>::assignWithCopy(destVal, srcVal, type);
  
  return reinterpret_cast<OpaqueValue*>(result);
}

/// \brief Perform a copy-assignment from one existential container to another.
/// Both containers must be of the same existential type representable with one
/// witness table.
OpaqueValue *swift::swift_assignExistentialWithCopy1(OpaqueValue *dest,
                                                     const OpaqueValue *src,
                                                     const Metadata *type) {
  auto destVal =
    reinterpret_cast<OpaqueExistentialValueWitnesses<1>::Container*>(dest);
  auto srcCVal =
    reinterpret_cast<const OpaqueExistentialValueWitnesses<1>::Container*>(src);
  auto srcVal =
    const_cast<OpaqueExistentialValueWitnesses<1>::Container*>(srcCVal);
  
  auto result =
    OpaqueExistentialValueWitnesses<1>::assignWithCopy(destVal, srcVal, type);

  return reinterpret_cast<OpaqueValue*>(result);
}

/// \brief Perform a copy-assignment from one existential container to another.
/// Both containers must be of the same existential type representable with the
/// same number of witness tables.
OpaqueValue *swift::swift_assignExistentialWithCopy(OpaqueValue *dest,
                                                    const OpaqueValue *src,
                                                    const Metadata *type) {
  auto destVal =
    reinterpret_cast<OpaqueExistentialValueWitnesses<VariableValueWitnesses>
                     ::Container*>(dest);
  auto srcCVal =
    reinterpret_cast<const OpaqueExistentialValueWitnesses
                           <VariableValueWitnesses>::Container*>(src);
  auto srcVal =
    const_cast<OpaqueExistentialValueWitnesses<VariableValueWitnesses>
               ::Container*>(srcCVal);
  
  auto result =
    OpaqueExistentialValueWitnesses<VariableValueWitnesses>
      ::assignWithCopy(destVal, srcVal, type);

  return reinterpret_cast<OpaqueValue*>(result);
}

const NominalTypeDescriptor *
Metadata::getNominalTypeDescriptor() const {
  switch (getKind()) {
  case MetadataKind::Class:
    return static_cast<const ClassMetadata *>(this)->Description;
  case MetadataKind::Struct:
  case MetadataKind::Enum:
    return static_cast<const StructMetadata *>(this)->Description;
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::PolyFunction:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::HeapArray:
  case MetadataKind::HeapLocalVariable:
    return nullptr;
  }
}

/// \brief Check whether a type conforms to a given native Swift protocol,
/// visible from the named module.
///
/// If so, returns a pointer to the witness table for its conformance.
/// Returns void if the type does not conform to the protocol.
///
/// \param type The metadata for the type for which to do the conformance
///             check.
/// \param protocol The protocol descriptor for the protocol to check
///                 conformance for.
/// \param module The mangled name of the module from which to determine
///               conformance visibility.
const void *swift::swift_conformsToProtocol(const Metadata *type,
                                            const ProtocolDescriptor *protocol,
                                            const char *module) {
  // FIXME: This is an unconscionable hack that only works for 1.0 because
  // we brazenly assume that:
  // - witness tables never require runtime instantiation
  // - witness tables have external visibility
  // - we in practice only have one module per program
  // - all conformances are public, and defined in the same module as the
  //   conforming type
  // - only nominal types conform to protocols
  
  // FIXME: Only check nominal types for now.
  auto *descriptor = type->getNominalTypeDescriptor();
  if (!descriptor)
    return nullptr;
  
  // Derive the symbol name that the witness table ought to have.
  // _TWP <protocol conformance>
  // protocol conformance ::= <type> <protocol> <module>
  
  std::string mangledName = "_TWP";
  mangledName += descriptor->Name;
  // The name in the protocol descriptor gets mangled as a protocol type
  // P <name> _
  const char *begin = protocol->Name + 1;
  const char *end = protocol->Name + strlen(protocol->Name) - 1;
  mangledName.append(begin, end);
  
  // FIXME: Assume the conformance was declared in the same module as the type,
  // so it will be mangled either as the stdlib module 'Ss' or the first
  // substitution 'S_'.
  if (descriptor->Name[0] == 'S'
      || (descriptor->Name[1] == 'S' && descriptor->Name[2] == 's'))
    mangledName += "Ss";
  else
    mangledName += "S_";
  
  // Look up the symbol for the conformance everywhere.
  return dlsym(RTLD_DEFAULT, mangledName.c_str());
}

/// The protocol descriptor for Printable from the stdlib.
extern "C" const ProtocolDescriptor _TMpSs9Printable;

/// Default behavior for printAny.
static void defaultPrint(OpaqueValue *value, const Metadata *type) {
  switch (type->getKind()) {
  case MetadataKind::Tuple: {
    // Destructure the tuple and printAny its elements.
    auto tupleBytes = reinterpret_cast<char *>(value);
    auto tuple = static_cast<const TupleTypeMetadata *>(type);
    auto elts = tuple->getElements();
    printf("(");
    for (unsigned i = 0, e = tuple->NumElements; i < e; ++i) {
      if (i > 0)
        printf(", ");
      auto &elt = elts[i];
      swift_printAny(reinterpret_cast<OpaqueValue*>(tupleBytes + elt.Offset),
                     elt.Type);
    }
    printf(")");
    return;
  }
      
  case MetadataKind::Class:
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::Function:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
  case MetadataKind::ObjCClassWrapper:
    // TODO
    printf("<something>");
    type->getValueWitnesses()->destroy(value, type);
    return;
      
  // Values should never use these metadata kinds.
  case MetadataKind::PolyFunction:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapArray:
    assert(false);
    // Consume the value.
    type->getValueWitnesses()->destroy(value, type);
  }
}

/// FIXME: This doesn't belong in the runtime.
///
/// func printAny<T>(x: T)
void swift::swift_printAny(OpaqueValue *value,
                           const Metadata *type) {
  const void *witnessTable = swift_conformsToProtocol(type, &_TMpSs9Printable,
                                                      nullptr);
  
  if (!witnessTable) {
    return defaultPrint(value, type);
  }
  
  // Take some liberties in assuming the layout of witness tables to extract
  // the print() method.
  const void *printPtr = ((const void * const *)witnessTable)[0];
  auto print = (void (*)(const OpaqueValue *, const Metadata *))
    (uintptr_t)printPtr;
  
  print(value, type);
  
  // 'self' of witnesses is passed at +0, so we still need to consume the
  // value.
  type->getValueWitnesses()->destroy(value, type);
}

namespace llvm {
namespace hashing {
namespace detail {
  // An extern variable expected by LLVM's hashing templates. We don't link any
  // LLVM libs into the runtime, so define this here.
  size_t fixed_seed_override = 0;
}
}
}

