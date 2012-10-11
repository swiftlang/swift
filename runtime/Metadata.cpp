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
#include "Alloc.h"
#include "Metadata.h"
#include <algorithm>
#include <new>
#include <string.h>

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
    static Impl *allocate(size_t numArguments, size_t payloadSize) {
      void *buffer = operator new(sizeof(Impl) +
                                  numArguments * sizeof(void*) +
                                  payloadSize);
      return new (buffer) Impl(numArguments);
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

static const Metadata *
instantiateGenericMetadata(GenericMetadata *pattern,
                           const void *arguments) {
  size_t numGenericArguments = pattern->NumArguments;

  // Allocate the new entry.
  auto entry = GenericCacheEntry::allocate(numGenericArguments,
                                           pattern->MetadataSize);

  // Copy the arguments into the right place for the key.
  memcpy(entry->getArgumentsBuffer(), arguments,
         numGenericArguments * sizeof(void*));

  // Initialize the metadata by copying the template.
  auto metadata = entry->getData<Metadata>(numGenericArguments);
  memcpy(metadata, pattern->getMetadataTemplate(), pattern->MetadataSize);

  // Fill in the missing spaces from the arguments.
  void * const *argumentsAsArray = reinterpret_cast<void * const *>(arguments);
  void **metadataAsArray = reinterpret_cast<void**>(metadata);
  for (auto i = pattern->fill_ops_begin(),
            e = pattern->fill_ops_end(); i != e; ++i) {
    metadataAsArray[i->ToIndex] = argumentsAsArray[i->FromIndex];
  }

  // The metadata is now valid.

  // Add the cache to the list.  This can in theory be made thread-safe,
  // but really this should use a non-linear lookup algorithm.
  return getCache(pattern).add(entry)->getData<Metadata>(numGenericArguments);
}

/// The primary entrypoint.
const void *
swift::swift_dynamicCast(const void *object, const ClassMetadata *targetType) {
  const ClassMetadata *isa = *reinterpret_cast<ClassMetadata *const*>(object);
  do {
    if (isa == targetType) {
      return object;
    }
    isa = isa->SuperClass;
  } while (targetType);
  return NULL;
}

/// The primary entrypoint.
const Metadata *
swift::swift_getGenericMetadata(GenericMetadata *pattern,
                                const void *arguments) {
  auto genericArgs = (const void * const *) arguments;
  size_t numGenericArgs = pattern->NumArguments;

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
    return entry->getData<Metadata>(numGenericArgs);
  }

#if SWIFT_DEBUG_RUNTIME
  printf("not found in cache!\n");
#endif

  // Otherwise, instantiate a new one.
  return instantiateGenericMetadata(pattern, arguments);
}

typedef HomogeneousCacheEntry FunctionCacheEntry;

/// The uniquing structure for function type metadata.
static MetadataCache<FunctionCacheEntry> FunctionTypes;

const FunctionTypeMetadata *
swift::swift_getFunctionTypeMetadata(const Metadata *argMetadata,
                                     const Metadata *resultMetadata) {
  const size_t numGenericArgs = 2;

  const void *args[] = { argMetadata, resultMetadata };
  if (auto entry = FunctionTypes.find(args, numGenericArgs)) {
    return entry->getData<FunctionTypeMetadata>(numGenericArgs);
  }

  auto entry = FunctionCacheEntry::allocate(numGenericArgs,
                                            sizeof(FunctionTypeMetadata));

  auto metadata = entry->getData<FunctionTypeMetadata>(numGenericArgs);
  metadata->Kind = MetadataKind::Function;
  metadata->ValueWitnesses = &_TWVFT_T_; // standard function value witnesses
  metadata->ArgumentType = argMetadata;
  metadata->ResultType = resultMetadata;

  return FunctionTypes.add(entry)->getData<FunctionTypeMetadata>(numGenericArgs);
}

/*** Tuples ****************************************************************/

typedef HeterogeneousCacheEntry TupleCacheEntry;

/// The uniquing structure for tuple type metadata.
static MetadataCache<TupleCacheEntry> TupleTypes;

namespace {
  /// The data structure carried by the entries in TupleTypes.
  ///
  /// All of the generic tuple witnesses below expect to be able to
  /// cast their witness-table pointers to this type.
  struct TupleTypeData {
    ValueWitnessTable Witnesses;
    TupleTypeMetadata Metadata; // includes a variably-sized array of elements
  };
}

/// Generic tuple value witness for 'projectBuffer'.
static OpaqueValue *tuple_projectBuffer(ValueBuffer *buffer,
                                        const ValueWitnessTable *wtable) {
  if (wtable->isValueInline())
    return reinterpret_cast<OpaqueValue*>(buffer);
  return *reinterpret_cast<OpaqueValue**>(buffer);
}

/// Generic tuple value witness for 'allocateBuffer'.
static OpaqueValue *tuple_allocateBuffer(ValueBuffer *buffer,
                                         const ValueWitnessTable *wtable) {
  if (wtable->isValueInline())
    return reinterpret_cast<OpaqueValue*>(buffer);

  // It's important to use 'stride' instead of 'size' because slowAlloc
  // only guarantees alignment up to a multiple of the value passed.
  auto value = (OpaqueValue*) swift_slowAlloc(wtable->stride, SWIFT_RAWALLOC);

  *reinterpret_cast<OpaqueValue**>(buffer) = value;
  return value;
}

/// Generic tuple value witness for 'deallocateBuffer'.
static void tuple_deallocateBuffer(ValueBuffer *buffer,
                                   const ValueWitnessTable *wtable) {
  if (wtable->isValueInline())
    return;

  auto value = *reinterpret_cast<OpaqueValue**>(buffer);
  swift_slowRawDealloc(value, wtable->stride);
}

/// Generic tuple value witness for 'destroy'.
static void tuple_destroy(OpaqueValue *tuple, const ValueWitnessTable *wtable) {
  auto &metadata = ((TupleTypeData *) wtable)->Metadata;
  for (size_t i = 0, e = metadata.NumElements; i != e; ++i) {
    auto &eltInfo = metadata.getElements()[i];
    OpaqueValue *elt = eltInfo.findIn(tuple);
    auto eltWitnesses = eltInfo.Type->ValueWitnesses;
    eltWitnesses->destroy(elt, eltWitnesses);
  }
}

/// Generic tuple value witness for 'destroyBuffer'.
static void tuple_destroyBuffer(ValueBuffer *buffer,
                                const ValueWitnessTable *wtable) {
  auto tuple = tuple_projectBuffer(buffer, wtable);
  tuple_destroy(tuple, wtable);
  tuple_deallocateBuffer(buffer, wtable);
}

// The operation doesn't have to be initializeWithCopy, but they all
// have basically the same type.
typedef value_witness_types::initializeWithCopy *
  ValueWitnessTable::*forEachOperation;

/// Perform an operation for each field of two tuples.
static OpaqueValue *tuple_forEachField(OpaqueValue *destTuple,
                                       OpaqueValue *srcTuple,
                                       const ValueWitnessTable *wtable,
                                       forEachOperation member) {
  auto &metadata = ((TupleTypeData *) wtable)->Metadata;
  for (size_t i = 0, e = metadata.NumElements; i != e; ++i) {
    auto &eltInfo = metadata.getElements()[i];
    auto eltValueWitnesses = eltInfo.Type->ValueWitnesses;

    OpaqueValue *destElt = eltInfo.findIn(destTuple);
    OpaqueValue *srcElt = eltInfo.findIn(srcTuple);
    (eltValueWitnesses->*member)(destElt, srcElt, eltValueWitnesses);
  }

  return destTuple;
}

/// Generic tuple value witness for 'initializeWithCopy'.
static OpaqueValue *tuple_initializeWithCopy(OpaqueValue *dest,
                                             OpaqueValue *src,
                                             const ValueWitnessTable *wtable) {
  return tuple_forEachField(dest, src, wtable,
                            &ValueWitnessTable::initializeWithCopy);
}

/// Generic tuple value witness for 'initializeWithTake'.
static OpaqueValue *tuple_initializeWithTake(OpaqueValue *dest,
                                             OpaqueValue *src,
                                             const ValueWitnessTable *wtable) {
  return tuple_forEachField(dest, src, wtable,
                            &ValueWitnessTable::initializeWithTake);
}

/// Generic tuple value witness for 'assignWithCopy'.
static OpaqueValue *tuple_assignWithCopy(OpaqueValue *dest,
                                         OpaqueValue *src,
                                         const ValueWitnessTable *wtable) {
  return tuple_forEachField(dest, src, wtable,
                            &ValueWitnessTable::assignWithCopy);
}

/// Generic tuple value witness for 'assignWithTake'.
static OpaqueValue *tuple_assignWithTake(OpaqueValue *dest,
                                         OpaqueValue *src,
                                         const ValueWitnessTable *wtable) {
  return tuple_forEachField(dest, src, wtable,
                            &ValueWitnessTable::assignWithTake);
}

/// Generic tuple value witness for 'initializeBufferWithCopy'.
static OpaqueValue *tuple_initializeBufferWithCopy(ValueBuffer *dest,
                                                   OpaqueValue *src,
                                             const ValueWitnessTable *wtable) {
  return tuple_initializeWithCopy(tuple_allocateBuffer(dest, wtable),
                                  src,
                                  wtable);
}

/// Generic tuple value witness for 'initializeBufferWithTake'.
static OpaqueValue *tuple_initializeBufferWithTake(ValueBuffer *dest,
                                                   OpaqueValue *src,
                                             const ValueWitnessTable *wtable) {
  return tuple_initializeWithTake(tuple_allocateBuffer(dest, wtable),
                                  src,
                                  wtable);
}

/// Generic tuple value witness for 'initializeBufferWithCopyOfBuffer'.
static OpaqueValue *tuple_initializeBufferWithCopyOfBuffer(ValueBuffer *dest,
                                                           ValueBuffer *src,
                                             const ValueWitnessTable *wtable) {
  return tuple_initializeBufferWithCopy(dest,
                                        tuple_projectBuffer(src, wtable),
                                        wtable);
}

/// Standard, inefficient witness table for tuples.
static const ValueWitnessTable tuple_witnesses = {
#define TUPLE_WITNESS(NAME) &tuple_##NAME,
  FOR_ALL_FUNCTION_VALUE_WITNESSES(TUPLE_WITNESS)
#undef TUPLE_WITNESS
  0,
  0,
  0
};

const TupleTypeMetadata *
swift::swift_getTupleTypeMetadata(size_t numElements,
                                  const Metadata * const *elements,
                                  const char *labels,
                                  const ValueWitnessTable *proposedWitnesses) {
  if (numElements == 0)
    return &_TMdT_;

  // FIXME: include labels when uniquing!

  auto genericArgs = (const void * const *) elements;
  if (auto entry = TupleTypes.find(genericArgs, numElements)) {
    return &entry->getData<TupleTypeData>(numElements)->Metadata;
  }

  typedef TupleTypeMetadata::Element Element;

  // This is actually a bit silly because the data is *completely*
  // shared between the header and the tuple metadata itself.
  auto entry = TupleCacheEntry::allocate(numElements,
                                         sizeof(TupleTypeData) +
                                         numElements * sizeof(Element));

  auto data = entry->getData<TupleTypeData>(numElements);
  auto witnesses = &data->Witnesses;

  auto metadata = &data->Metadata;
  metadata->Base.Kind = MetadataKind::Tuple;
  metadata->Base.ValueWitnesses = witnesses;
  metadata->NumElements = numElements;
  metadata->Labels = labels;

  size_t size = 0;
  size_t alignment = 1;
  for (unsigned i = 0; i != numElements; ++i) {
    auto elt = elements[i];

    metadata->getElements()[i].Type = elt;
    metadata->getElements()[i].Offset = size;

    // Lay out this tuple element.
    size = llvm::RoundUpToAlignment(size, elt->ValueWitnesses->alignment);
    size += elt->ValueWitnesses->size;
    alignment = std::max(alignment, elt->ValueWitnesses->alignment);
  }

  witnesses->size = size;
  witnesses->alignment = alignment;
  witnesses->stride = llvm::RoundUpToAlignment(size, alignment);

  // Copy the function witnesses in, either from the proposed
  // witnesses or from the standard table.
  if (!proposedWitnesses) proposedWitnesses = &tuple_witnesses;
#define ASSIGN_TUPLE_WITNESS(NAME) \
  witnesses->NAME = proposedWitnesses->NAME;
  FOR_ALL_FUNCTION_VALUE_WITNESSES(ASSIGN_TUPLE_WITNESS)
#undef ASSIGN_TUPLE_WITNESS

  return &TupleTypes.add(entry)->getData<TupleTypeData>(numElements)->Metadata;
}
