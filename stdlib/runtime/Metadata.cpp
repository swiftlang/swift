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
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Range.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Strings.h"
#include "MetadataCache.h"
#include <algorithm>
#include <condition_variable>
#include <new>
#include <cctype>
#include <pthread.h>
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Hashing.h"
#include "ExistentialMetadataImpl.h"
#include "Lazy.h"
#include "Debug.h"
#include "Private.h"

using namespace swift;
using namespace metadataimpl;

namespace {
  struct GenericCacheEntry;

  // The cache entries in a generic cache are laid out like this:
  struct GenericCacheEntryHeader : CacheEntry<GenericCacheEntry> {
    const Metadata *Value;
    size_t NumArguments;
  };

  struct GenericCacheEntry
      : CacheEntry<GenericCacheEntry, GenericCacheEntryHeader> {

    static const char *getName() { return "GenericCache"; }

    GenericCacheEntry(unsigned numArguments) {
      NumArguments = numArguments;
    }

    size_t getNumArguments() const { return NumArguments; }

    static GenericCacheEntry *getFromMetadata(GenericMetadata *pattern,
                                              Metadata *metadata) {
      char *bytes = (char*) metadata;
      if (auto classType = dyn_cast<ClassMetadata>(metadata)) {
        assert(classType->isTypeMetadata());
        bytes -= classType->getClassAddressPoint();
      } else {
        bytes -= pattern->AddressPoint;
      }
      bytes -= sizeof(GenericCacheEntry);
      return reinterpret_cast<GenericCacheEntry*>(bytes);
    }
  };
}

using GenericMetadataCache = MetadataCache<GenericCacheEntry>;
using LazyGenericMetadataCache = Lazy<GenericMetadataCache>;

/// Fetch the metadata cache for a generic metadata structure.
static GenericMetadataCache &getCache(GenericMetadata *metadata) {
  // Keep this assert even if you change the representation above.
  static_assert(sizeof(LazyGenericMetadataCache) <=
                sizeof(GenericMetadata::PrivateData),
                "metadata cache is larger than the allowed space");

  auto lazyCache =
    reinterpret_cast<LazyGenericMetadataCache*>(metadata->PrivateData);
  return lazyCache->get();
}

ClassMetadata *
swift::swift_allocateGenericClassMetadata(GenericMetadata *pattern,
                                          const void *arguments,
                                          ClassMetadata *superclass) {
  void * const *argumentsAsArray = reinterpret_cast<void * const *>(arguments);
  size_t numGenericArguments = pattern->NumKeyArguments;

  // Right now, we only worry about there being a difference in prefix matter.
  size_t metadataSize = pattern->MetadataSize;
  size_t prefixSize = pattern->AddressPoint;
  size_t extraPrefixSize = 0;
  if (superclass && superclass->isTypeMetadata()) {
    if (superclass->getClassAddressPoint() > prefixSize) {
      extraPrefixSize = (superclass->getClassAddressPoint() - prefixSize);
      prefixSize += extraPrefixSize;
      metadataSize += extraPrefixSize;
    }
  }
  assert(metadataSize == pattern->MetadataSize + extraPrefixSize);
  assert(prefixSize == pattern->AddressPoint + extraPrefixSize);

  char *bytes = GenericCacheEntry::allocate(argumentsAsArray,
                                            numGenericArguments,
                                            metadataSize)->getData<char>();

  // Copy any extra prefix bytes in from the superclass.
  if (extraPrefixSize) {
    memcpy(bytes, (const char*) superclass - prefixSize, extraPrefixSize);
    bytes += extraPrefixSize;
  }

  // Copy in the metadata template.
  memcpy(bytes, pattern->getMetadataTemplate(), pattern->MetadataSize);

  // Okay, move to the address point.
  bytes += pattern->AddressPoint;
  ClassMetadata *metadata = reinterpret_cast<ClassMetadata*>(bytes);
  assert(metadata->isTypeMetadata());

  // Overwrite the superclass field.
  metadata->SuperClass = superclass;

  // Adjust the class object extents.
  if (extraPrefixSize) {
    metadata->setClassSize(metadata->getClassSize() + extraPrefixSize);
    metadata->setClassAddressPoint(prefixSize);
  }
  assert(metadata->getClassAddressPoint() == prefixSize);

  return metadata;
}

Metadata *
swift::swift_allocateGenericValueMetadata(GenericMetadata *pattern,
                                          const void *arguments) {
  void * const *argumentsAsArray = reinterpret_cast<void * const *>(arguments);
  size_t numGenericArguments = pattern->NumKeyArguments;

  char *bytes =
    GenericCacheEntry::allocate(argumentsAsArray, numGenericArguments,
                                pattern->MetadataSize)->getData<char>();

  // Copy in the metadata template.
  memcpy(bytes, pattern->getMetadataTemplate(), pattern->MetadataSize);

  // Okay, move to the address point.
  bytes += pattern->AddressPoint;
  Metadata *metadata = reinterpret_cast<Metadata*>(bytes);
  return metadata;
}

/// The primary entrypoint.
const Metadata *
swift::swift_getGenericMetadata(GenericMetadata *pattern,
                                const void *arguments) {
  auto genericArgs = (const void * const *) arguments;
  size_t numGenericArgs = pattern->NumKeyArguments;

  auto entry = getCache(pattern).findOrAdd(genericArgs, numGenericArgs,
    [&]() -> GenericCacheEntry* {
      // Create new metadata to cache.
      auto metadata = pattern->CreateFunction(pattern, arguments);
      auto entry = GenericCacheEntry::getFromMetadata(pattern, metadata);
      entry->Value = metadata;
      return entry;
    });

  return entry->Value;
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
    static const char *getName() { return "ObjCClassCache"; }

    ObjCClassCacheEntry(size_t numArguments) {}

    static constexpr size_t getNumArguments() {
      return 1;
    }

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

#if SWIFT_OBJC_INTEROP
  // Search the cache.

  const size_t numGenericArgs = 1;
  const void *args[] = { theClass };
  auto entry = ObjCClassWrappers.findOrAdd(args, numGenericArgs,
    [&]() -> ObjCClassCacheEntry* {
      // Create a new entry for the cache.
      auto entry = ObjCClassCacheEntry::allocate(args, numGenericArgs, 0);

      auto metadata = entry->getData();
      metadata->setKind(MetadataKind::ObjCClassWrapper);
      metadata->ValueWitnesses = &_TWVBO;
      metadata->Class = theClass;

      return entry;
    });

  return entry->getData();
#else
  fatalError("swift_getObjCClassMetadata: no Objective-C interop");
#endif
}

namespace {
  class FunctionCacheEntry;
  struct FunctionCacheEntryHeader : CacheEntryHeader<FunctionCacheEntry> {
    size_t NumArguments;
  };
  class FunctionCacheEntry
    : public CacheEntry<FunctionCacheEntry, FunctionCacheEntryHeader> {
  public:
    FullMetadata<FunctionTypeMetadata> Metadata;

    static const char *getName() { return "FunctionCache"; }

    FunctionCacheEntry(size_t numArguments) {
      NumArguments = numArguments;
    }

    size_t getNumArguments() const {
      return NumArguments;
    }

    FullMetadata<FunctionTypeMetadata> *getData() {
      return &Metadata;
    }
    const FullMetadata<FunctionTypeMetadata> *getData() const {
      return &Metadata;
    }
  };
}

/// The uniquing structure for function type metadata.
namespace {
  MetadataCache<FunctionCacheEntry> FunctionTypes;
  MetadataCache<FunctionCacheEntry> ThinFunctionTypes;
#if SWIFT_OBJC_INTEROP
  MetadataCache<FunctionCacheEntry> BlockTypes;
#endif

  const FunctionTypeMetadata *
  _getFunctionTypeMetadata(size_t numArguments,
                           const void * argsAndResult [],
                           MetadataKind Kind,
                           MetadataCache<FunctionCacheEntry> &Cache,
                           const ValueWitnessTable &ValueWitnesses) {
    // Search the cache.

    // N argument types (with inout bit set)
    // and 1 result type (a tuple with M elements)
    auto entry = Cache.findOrAdd(argsAndResult, numArguments + 1,
      [&]() -> FunctionCacheEntry* {
        // Create a new entry for the cache.
        auto entry = FunctionCacheEntry::allocate(
          argsAndResult,
          numArguments + 1,
          numArguments * sizeof(FunctionTypeMetadata::Argument));

        auto metadata = entry->getData();
        metadata->setKind(Kind);
        metadata->ValueWitnesses = &ValueWitnesses;
        metadata->NumArguments = numArguments;
        metadata->ResultType = reinterpret_cast<const Metadata *>(
          argsAndResult[numArguments]);

        for (size_t i = 0; i < numArguments; ++i) {
          auto arg = FunctionTypeMetadata::Argument::getFromOpaqueValue(
            argsAndResult[i]);
          metadata->getArguments()[i] = arg;
        }

        return entry;
      });

    return entry->getData();
  }
}

/// A macro to define convenience accessors for 0, 1, 2, and
/// 3-argument function metadata.
#define DEFINE_FUNCTION_CONVENIENCE_ACCESSORS(KIND)                     \
  const FunctionTypeMetadata *                                          \
  swift::swift_get##KIND##Metadata0(const Metadata *result) {           \
    const void *argsAndResult[] = {                                     \
      static_cast<const void *>(result)                                 \
    };                                                                  \
    return swift_get##KIND##Metadata(0, argsAndResult);                 \
  }                                                                     \
  const FunctionTypeMetadata *                                          \
  swift::swift_get##KIND##Metadata1(const void *arg0,                   \
                                    const Metadata *result) {           \
    const void *argsAndResult[] = {                                     \
      arg0,                                                             \
      static_cast<const void *>(result)                                 \
    };                                                                  \
    return swift_get##KIND##Metadata(1, argsAndResult);                 \
  }                                                                     \
  const FunctionTypeMetadata *                                          \
  swift::swift_get##KIND##Metadata2(const void *arg0,                   \
                                    const void *arg1,                   \
                                    const Metadata *result) {           \
    const void *argsAndResult[] = {                                     \
      arg0,                                                             \
      arg1,                                                             \
      static_cast<const void *>(result)                                 \
    };                                                                  \
    return swift_get##KIND##Metadata(2, argsAndResult);                 \
  }                                                                     \
  const FunctionTypeMetadata *                                          \
  swift::swift_get##KIND##Metadata3(const void *arg0,                   \
                                    const void *arg1,                   \
                                    const void *arg2,                   \
                                    const Metadata *result) {           \
    const void *argsAndResult[] = {                                     \
      arg0,                                                             \
      arg1,                                                             \
      arg2,                                                             \
      static_cast<const void *>(result)                                 \
    };                                                                  \
    return swift_get##KIND##Metadata(3, argsAndResult);                 \
  }

const FunctionTypeMetadata *
swift::swift_getFunctionTypeMetadata(size_t numArguments,
                                     const void *argsAndResult[]) {

  return _getFunctionTypeMetadata(numArguments,
                                  argsAndResult,
                                  MetadataKind::Function,
                                  FunctionTypes,
                                  _TWVFT_T_);
}

DEFINE_FUNCTION_CONVENIENCE_ACCESSORS(FunctionType)

const FunctionTypeMetadata *
swift::swift_getThinFunctionTypeMetadata(size_t numArguments,
                                         const void *argsAndResult[]) {

  return _getFunctionTypeMetadata(numArguments,
                                  argsAndResult,
                                  MetadataKind::ThinFunction,
                                  ThinFunctionTypes,
                                  _TWVXfT_T_);
}

DEFINE_FUNCTION_CONVENIENCE_ACCESSORS(ThinFunctionType)

// Only define the block-type accessors if we need ObjC interop.
#if SWIFT_OBJC_INTEROP

const FunctionTypeMetadata *
swift::swift_getBlockTypeMetadata(size_t numArguments,
                                  const void *argsAndResult[]) {
  return _getFunctionTypeMetadata(numArguments,
                                  argsAndResult,
                                  MetadataKind::Block,
                                  BlockTypes,
                                  _TWVBO);
}

DEFINE_FUNCTION_CONVENIENCE_ACCESSORS(BlockType)

#endif // SWIFT_OBJC_INTEROP

#undef DEFINE_FUNCTION_CONVENIENCE_ACCESSORS


/*** Tuples ****************************************************************/

namespace {
  class TupleCacheEntry;
  struct TupleCacheEntryHeader : CacheEntryHeader<TupleCacheEntry> {
    size_t NumArguments;
  };
  class TupleCacheEntry
    : public CacheEntry<TupleCacheEntry, TupleCacheEntryHeader> {
  public:
    // NOTE: if you change the layout of this type, you'll also need
    // to update tuple_getValueWitnesses().
    ExtraInhabitantsValueWitnessTable Witnesses;
    FullMetadata<TupleTypeMetadata> Metadata;

    static const char *getName() { return "TupleCache"; }

    TupleCacheEntry(size_t numArguments) {
      NumArguments = numArguments;
    }

    size_t getNumArguments() const {
      return Metadata.NumElements;
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
  return ((const ExtraInhabitantsValueWitnessTable*) asFullMetadata(metatype)) - 1;
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

  auto wtable = tuple_getValueWitnesses(metatype);
  auto value = (OpaqueValue*) swift_slowAlloc(wtable->size,
                                              wtable->getAlignmentMask());

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
  swift_slowDealloc(value, wtable->size, wtable->getAlignmentMask());
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

/// Generic tuple value witness for 'destroyArray'.
template <bool IsPOD, bool IsInline>
static void tuple_destroyArray(OpaqueValue *array, size_t n,
                               const Metadata *_metadata) {
  auto &metadata = *(const TupleTypeMetadata*) _metadata;
  assert(IsPOD == tuple_getValueWitnesses(&metadata)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(&metadata)->isValueInline());

  if (IsPOD) return;

  size_t stride = tuple_getValueWitnesses(&metadata)->stride;
  char *bytes = (char*)array;

  while (n--) {
    tuple_destroy<IsPOD, IsInline>((OpaqueValue*)bytes, _metadata);
    bytes += stride;
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
/// Perform a naive memcpy of n tuples from src into dest.
static OpaqueValue *tuple_memcpy_array(OpaqueValue *dest,
                                       OpaqueValue *src,
                                       size_t n,
                                       const Metadata *metatype) {
  assert(metatype->getValueWitnesses()->isPOD());
  return (OpaqueValue*)
    memcpy(dest, src, metatype->getValueWitnesses()->stride * n);
}
/// Perform a naive memmove of n tuples from src into dest.
static OpaqueValue *tuple_memmove_array(OpaqueValue *dest,
                                        OpaqueValue *src,
                                        size_t n,
                                        const Metadata *metatype) {
  assert(metatype->getValueWitnesses()->isPOD());
  return (OpaqueValue*)
    memmove(dest, src, metatype->getValueWitnesses()->stride * n);
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

/// Generic tuple value witness for 'initializeArrayWithCopy'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_initializeArrayWithCopy(OpaqueValue *dest,
                                                  OpaqueValue *src,
                                                  size_t n,
                                                  const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  if (IsPOD) return tuple_memcpy_array(dest, src, n, metatype);

  char *destBytes = (char*)dest;
  char *srcBytes = (char*)src;
  size_t stride = tuple_getValueWitnesses(metatype)->stride;

  while (n--) {
    tuple_initializeWithCopy<IsPOD, IsInline>((OpaqueValue*)destBytes,
                                              (OpaqueValue*)srcBytes,
                                              metatype);
    destBytes += stride; srcBytes += stride;
  }

  return dest;
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

/// Generic tuple value witness for 'initializeArrayWithTakeFrontToBack'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_initializeArrayWithTakeFrontToBack(
                                             OpaqueValue *dest,
                                             OpaqueValue *src,
                                             size_t n,
                                             const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  if (IsPOD) return tuple_memmove_array(dest, src, n, metatype);

  char *destBytes = (char*)dest;
  char *srcBytes = (char*)src;
  size_t stride = tuple_getValueWitnesses(metatype)->stride;

  while (n--) {
    tuple_initializeWithTake<IsPOD, IsInline>((OpaqueValue*)destBytes,
                                              (OpaqueValue*)srcBytes,
                                              metatype);
    destBytes += stride; srcBytes += stride;
  }

  return dest;
}

/// Generic tuple value witness for 'initializeArrayWithTakeBackToFront'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_initializeArrayWithTakeBackToFront(
                                             OpaqueValue *dest,
                                             OpaqueValue *src,
                                             size_t n,
                                             const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  if (IsPOD) return tuple_memmove_array(dest, src, n, metatype);

  size_t stride = tuple_getValueWitnesses(metatype)->stride;
  char *destBytes = (char*)dest + n * stride;
  char *srcBytes = (char*)src + n * stride;

  while (n--) {
    destBytes -= stride; srcBytes -= stride;
    tuple_initializeWithTake<IsPOD, IsInline>((OpaqueValue*)destBytes,
                                              (OpaqueValue*)srcBytes,
                                              metatype);
  }

  return dest;
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

/// Generic tuple value witness for 'initializeBufferWithTakeOfBuffer'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_initializeBufferWithTakeOfBuffer(ValueBuffer *dest,
                                                           ValueBuffer *src,
                                                     const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  if (IsInline) {
    return tuple_initializeWithTake<IsPOD, IsInline>(
                      tuple_projectBuffer<IsPOD, IsInline>(dest, metatype),
                      tuple_projectBuffer<IsPOD, IsInline>(src, metatype),
                      metatype);
  } else {
    dest->PrivateData[0] = src->PrivateData[0];
    return (OpaqueValue*) dest->PrivateData[0];
  }
}

static void tuple_storeExtraInhabitant(OpaqueValue *tuple,
                                       int index,
                                       const Metadata *_metatype) {
  auto &metatype = *(const TupleTypeMetadata*) _metatype;
  auto &eltInfo = metatype.getElements()[0];

  assert(eltInfo.Offset == 0);
  OpaqueValue *elt = tuple;

  eltInfo.Type->vw_storeExtraInhabitant(elt, index);
}

static int tuple_getExtraInhabitantIndex(const OpaqueValue *tuple,
                                         const Metadata *_metatype) {
  auto &metatype = *(const TupleTypeMetadata*) _metatype;
  auto &eltInfo = metatype.getElements()[0];

  assert(eltInfo.Offset == 0);
  const OpaqueValue *elt = tuple;

  return eltInfo.Type->vw_getExtraInhabitantIndex(elt);
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

static size_t roundUpToAlignMask(size_t size, size_t alignMask) {
  return (size + alignMask) & ~alignMask;
}

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
  size_t alignMask = layout.flags.getAlignmentMask();
  bool isPOD = layout.flags.isPOD();
  bool isBitwiseTakable = layout.flags.isBitwiseTakable();
  for (unsigned i = 0; i != numElements; ++i) {
    auto elt = elements[i];

    // Lay out this element.
    auto eltVWT = elt->getValueWitnesses();
    size = roundUpToAlignMask(size, eltVWT->getAlignmentMask());

    // Report this record to the functor.
    f(i, elt, size);

    // Update the size and alignment of the aggregate..
    size += eltVWT->size;
    alignMask = std::max(alignMask, eltVWT->getAlignmentMask());
    if (!eltVWT->isPOD()) isPOD = false;
    if (!eltVWT->isBitwiseTakable()) isBitwiseTakable = false;
  }
  bool isInline = ValueWitnessTable::isValueInline(size, alignMask + 1);

  layout.size = size;
  layout.flags = ValueWitnessFlags().withAlignmentMask(alignMask)
                                    .withPOD(isPOD)
                                    .withBitwiseTakable(isBitwiseTakable)
                                    .withInlineStorage(isInline);
  layout.stride = roundUpToAlignMask(size, alignMask);
}
} // end anonymous namespace

const TupleTypeMetadata *
swift::swift_getTupleTypeMetadata(size_t numElements,
                                  const Metadata * const *elements,
                                  const char *labels,
                                  const ValueWitnessTable *proposedWitnesses) {
  // Bypass the cache for the empty tuple. We might reasonably get called
  // by generic code, like a demangler that produces type objects.
  if (numElements == 0) return &_TMdT_;

  // Search the cache.

  // FIXME: include labels when uniquing!
  auto genericArgs = (const void * const *) elements;
  auto entry = TupleTypes.findOrAdd(genericArgs, numElements,
    [&]() -> TupleCacheEntry* {
      // Create a new entry for the cache.

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

      // We have extra inhabitants if the first element does.
      // FIXME: generalize this.
      if (auto firstEltEIVWT = dyn_cast<ExtraInhabitantsValueWitnessTable>(
                                 elements[0]->getValueWitnesses())) {
        witnesses->flags = witnesses->flags.withExtraInhabitants(true);
        witnesses->extraInhabitantFlags = firstEltEIVWT->extraInhabitantFlags;
        witnesses->storeExtraInhabitant = tuple_storeExtraInhabitant;
        witnesses->getExtraInhabitantIndex = tuple_getExtraInhabitantIndex;
      }

      return entry;
    });

  return entry->getData();
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

/*** Common value witnesses ************************************************/

// Value witness methods for an arbitrary trivial type.
// The buffer operations assume that the value is stored indirectly, because
// installCommonValueWitnesses will install the direct equivalents instead.

namespace {
  template<typename T>
  struct pointer_function_cast_impl;
  
  template<typename OutRet, typename...OutArgs>
  struct pointer_function_cast_impl<OutRet * (OutArgs *...)> {
    template<typename InRet, typename...InArgs>
    static constexpr auto perform(InRet * (*function)(InArgs *...))
      -> OutRet * (*)(OutArgs *...)
    {
      static_assert(sizeof...(InArgs) == sizeof...(OutArgs),
                    "cast changed number of arguments");
      return (OutRet *(*)(OutArgs *...))function;
    }
  };

  template<typename...OutArgs>
  struct pointer_function_cast_impl<void (OutArgs *...)> {
    template<typename...InArgs>
    static constexpr auto perform(void (*function)(InArgs *...))
      -> void (*)(OutArgs *...)
    {
      static_assert(sizeof...(InArgs) == sizeof...(OutArgs),
                    "cast changed number of arguments");
      return (void (*)(OutArgs *...))function;
    }
  };
}

/// Cast a function that takes all pointer arguments and returns to a
/// function type that takes different pointer arguments and returns.
/// In any reasonable calling convention the input and output function types
/// should be ABI-compatible.
template<typename Out, typename In>
static constexpr Out *pointer_function_cast(In *function) {
  return pointer_function_cast_impl<Out>::perform(function);
}

static void pod_indirect_deallocateBuffer(ValueBuffer *buffer,
                                          const Metadata *self) {
  auto value = *reinterpret_cast<OpaqueValue**>(buffer);
  auto wtable = self->getValueWitnesses();
  swift_slowDealloc(value, wtable->size, wtable->getAlignmentMask());
}
#define pod_indirect_destroyBuffer \
  pointer_function_cast<value_witness_types::destroyBuffer>(pod_indirect_deallocateBuffer)

static OpaqueValue *pod_indirect_initializeBufferWithCopyOfBuffer(
                    ValueBuffer *dest, ValueBuffer *src, const Metadata *self) {
  auto wtable = self->getValueWitnesses();
  auto destBuf = (OpaqueValue*)swift_slowAlloc(wtable->size,
                                               wtable->getAlignmentMask());
  *reinterpret_cast<OpaqueValue**>(dest) = destBuf;
  OpaqueValue *srcBuf = *reinterpret_cast<OpaqueValue**>(src);
  memcpy(destBuf, srcBuf, wtable->size);
  return destBuf;
}
#define pod_indirect_initializeBufferWithTakeOfBuffer \
  pod_indirect_initializeBufferWithCopyOfBuffer

static OpaqueValue *pod_indirect_projectBuffer(ValueBuffer *buffer,
                                               const Metadata *self) {
  return *reinterpret_cast<OpaqueValue**>(buffer);
}

static OpaqueValue *pod_indirect_allocateBuffer(ValueBuffer *buffer,
                                                const Metadata *self) {
  auto wtable = self->getValueWitnesses();
  auto destBuf = (OpaqueValue*)swift_slowAlloc(wtable->size,
                                               wtable->getAlignmentMask());
  *reinterpret_cast<OpaqueValue**>(buffer) = destBuf;
  return destBuf;
}

static void pod_noop(void *object, const Metadata *self) {
}
#define pod_direct_destroy \
  pointer_function_cast<value_witness_types::destroy>(pod_noop)
#define pod_indirect_destroy pod_direct_destroy
#define pod_direct_destroyBuffer \
  pointer_function_cast<value_witness_types::destroyBuffer>(pod_noop)
#define pod_direct_deallocateBuffer \
  pointer_function_cast<value_witness_types::deallocateBuffer>(pod_noop)

static void *pod_noop_return(void *object, const Metadata *self) {
  return object;
}
#define pod_direct_projectBuffer \
  pointer_function_cast<value_witness_types::projectBuffer>(pod_noop_return)
#define pod_direct_allocateBuffer \
  pointer_function_cast<value_witness_types::allocateBuffer>(pod_noop_return)

static OpaqueValue *pod_indirect_initializeBufferWithCopy(ValueBuffer *dest,
                                                          OpaqueValue *src,
                                                          const Metadata *self){
  auto wtable = self->getValueWitnesses();
  auto destBuf = (OpaqueValue*)swift_slowAlloc(wtable->size,
                                               wtable->getAlignmentMask());
  *reinterpret_cast<OpaqueValue**>(dest) = destBuf;
  memcpy(destBuf, src, wtable->size);
  return destBuf;
}
#define pod_indirect_initializeBufferWithTake pod_indirect_initializeBufferWithCopy

static OpaqueValue *pod_direct_initializeWithCopy(OpaqueValue *dest,
                                                  OpaqueValue *src,
                                                  const Metadata *self) {
  memcpy(dest, src, self->getValueWitnesses()->size);
  return dest;
}
#define pod_indirect_initializeWithCopy pod_direct_initializeWithCopy
#define pod_direct_initializeBufferWithCopyOfBuffer \
  pointer_function_cast<value_witness_types::initializeBufferWithCopyOfBuffer> \
    (pod_direct_initializeWithCopy)
#define pod_direct_initializeBufferWithTakeOfBuffer \
  pointer_function_cast<value_witness_types::initializeBufferWithTakeOfBuffer> \
    (pod_direct_initializeWithCopy)
#define pod_direct_initializeBufferWithCopy \
  pointer_function_cast<value_witness_types::initializeBufferWithCopy> \
    (pod_direct_initializeWithCopy)
#define pod_direct_initializeBufferWithTake \
  pointer_function_cast<value_witness_types::initializeBufferWithTake> \
    (pod_direct_initializeWithCopy)
#define pod_direct_assignWithCopy pod_direct_initializeWithCopy
#define pod_indirect_assignWithCopy pod_direct_initializeWithCopy
#define pod_direct_initializeWithTake pod_direct_initializeWithCopy
#define pod_indirect_initializeWithTake pod_direct_initializeWithCopy
#define pod_direct_assignWithTake pod_direct_initializeWithCopy
#define pod_indirect_assignWithTake pod_direct_initializeWithCopy

static void pod_direct_destroyArray(OpaqueValue *, size_t, const Metadata *) {
  // noop
}
#define pod_indirect_destroyArray pod_direct_destroyArray

static OpaqueValue *pod_direct_initializeArrayWithCopy(OpaqueValue *dest,
                                                       OpaqueValue *src,
                                                       size_t n,
                                                       const Metadata *self) {
  auto totalSize = self->getValueWitnesses()->stride * n;
  memcpy(dest, src, totalSize);
  return dest;
}
#define pod_indirect_initializeArrayWithCopy pod_direct_initializeArrayWithCopy

static OpaqueValue *pod_direct_initializeArrayWithTakeFrontToBack(
                                                        OpaqueValue *dest,
                                                        OpaqueValue *src,
                                                        size_t n,
                                                        const Metadata *self) {
  auto totalSize = self->getValueWitnesses()->stride * n;
  memmove(dest, src, totalSize);
  return dest;
}
#define pod_direct_initializeArrayWithTakeBackToFront \
  pod_direct_initializeArrayWithTakeFrontToBack
#define pod_indirect_initializeArrayWithTakeFrontToBack \
  pod_direct_initializeArrayWithTakeFrontToBack
#define pod_indirect_initializeArrayWithTakeBackToFront \
  pod_direct_initializeArrayWithTakeFrontToBack

static constexpr uintptr_t sizeWithAlignmentMask(uintptr_t size,
                                                 uintptr_t alignmentMask) {
  return (size << 16) | alignmentMask;
}

void swift::installCommonValueWitnesses(ValueWitnessTable *vwtable) {
  auto flags = vwtable->flags;
  if (flags.isPOD()) {
    // Use POD value witnesses.
    // If the value has a common size and alignment, use specialized value
    // witnesses we already have lying around for the builtin types.
    const ValueWitnessTable *commonVWT;
    switch (sizeWithAlignmentMask(vwtable->size, vwtable->getAlignmentMask())) {
    default:
      // For uncommon layouts, use value witnesses that work with an arbitrary
      // size and alignment.
      if (flags.isInlineStorage()) {
  #define INSTALL_POD_DIRECT_WITNESS(NAME) vwtable->NAME = pod_direct_##NAME;
        FOR_ALL_FUNCTION_VALUE_WITNESSES(INSTALL_POD_DIRECT_WITNESS)
  #undef INSTALL_POD_DIRECT_WITNESS
      } else {
  #define INSTALL_POD_INDIRECT_WITNESS(NAME) vwtable->NAME = pod_indirect_##NAME;
        FOR_ALL_FUNCTION_VALUE_WITNESSES(INSTALL_POD_INDIRECT_WITNESS)
  #undef INSTALL_POD_INDIRECT_WITNESS
      }
      return;
      
    case sizeWithAlignmentMask(1, 0):
      commonVWT = &_TWVBi8_;
      break;
    case sizeWithAlignmentMask(2, 1):
      commonVWT = &_TWVBi16_;
      break;
    case sizeWithAlignmentMask(4, 3):
      commonVWT = &_TWVBi32_;
      break;
    case sizeWithAlignmentMask(8, 7):
      commonVWT = &_TWVBi64_;
      break;
    case sizeWithAlignmentMask(16, 15):
      commonVWT = &_TWVBi128_;
      break;
    }
    
  #define INSTALL_POD_COMMON_WITNESS(NAME) vwtable->NAME = commonVWT->NAME;
    FOR_ALL_FUNCTION_VALUE_WITNESSES(INSTALL_POD_COMMON_WITNESS)
  #undef INSTALL_POD_COMMON_WITNESS
    
    return;
  }
  
  if (vwtable->flags.isBitwiseTakable()) {
    // Use POD value witnesses for operations that do an initializeWithTake.
    if (flags.isInlineStorage()) {
      vwtable->initializeWithTake = pod_direct_initializeWithTake;
      vwtable->initializeBufferWithTakeOfBuffer
        = pod_direct_initializeBufferWithTakeOfBuffer;
      vwtable->initializeArrayWithTakeFrontToBack
        = pod_direct_initializeArrayWithTakeFrontToBack;
      vwtable->initializeArrayWithTakeBackToFront
        = pod_direct_initializeArrayWithTakeBackToFront;
    } else {
      vwtable->initializeWithTake = pod_indirect_initializeWithTake;
      vwtable->initializeBufferWithTakeOfBuffer
        = pod_indirect_initializeBufferWithTakeOfBuffer;
      vwtable->initializeArrayWithTakeFrontToBack
        = pod_indirect_initializeArrayWithTakeFrontToBack;
      vwtable->initializeArrayWithTakeBackToFront
        = pod_indirect_initializeArrayWithTakeBackToFront;
    }
    return;
  }

  if (!vwtable->flags.isInlineStorage()) {
    // For values stored out-of-line, initializeBufferWithTakeOfBuffer is
    // always a memcpy.
    vwtable->initializeBufferWithTakeOfBuffer
      = pod_indirect_initializeBufferWithTakeOfBuffer;
    return;
  }
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
  
  // Substitute in better value witnesses if we have them.
  installCommonValueWitnesses(vwtable);

  // We have extra inhabitants if the first element does.
  // FIXME: generalize this.
  if (auto firstFieldVWT = dyn_cast<ExtraInhabitantsValueWitnessTable>(
                                        fieldTypes[0]->getValueWitnesses())) {
    vwtable->flags = vwtable->flags.withExtraInhabitants(true);
    auto xiVWT = cast<ExtraInhabitantsValueWitnessTable>(vwtable);
    xiVWT->extraInhabitantFlags = firstFieldVWT->extraInhabitantFlags;

    // The compiler should already have initialized these.
    assert(xiVWT->storeExtraInhabitant);
    assert(xiVWT->getExtraInhabitantIndex);
  }
}

/*** Classes ***************************************************************/

namespace {
  /// The structure of ObjC class ivars as emitted by compilers.
  struct ClassIvarEntry {
    size_t *Offset;
    const char *Name;
    const char *Type;
    uint32_t Log2Alignment;
    uint32_t Size;
  };

  /// The structure of ObjC class ivar lists as emitted by compilers.
  struct ClassIvarList {
    uint32_t EntrySize;
    uint32_t Count;

    ClassIvarEntry *getIvars() {
      return reinterpret_cast<ClassIvarEntry*>(this+1);
    }
    const ClassIvarEntry *getIvars() const {
      return reinterpret_cast<const ClassIvarEntry*>(this+1);
    }
  };

  /// The structure of ObjC class rodata as emitted by compilers.
  struct ClassROData {
    uint32_t Flags;
    uint32_t InstanceStart;
    uint32_t InstanceSize;
#ifdef __LP64__
    uint32_t Reserved;
#endif
    const uint8_t *IvarLayout;
    const char *Name;
    const void *MethodList;
    const void *ProtocolList;
    ClassIvarList *IvarList;
    const uint8_t *WeakIvarLayout;
    const void *PropertyList;
  };
}

static uint32_t getLog2AlignmentFromMask(size_t alignMask) {
  assert(((alignMask + 1) & alignMask) == 0 &&
         "not an alignment mask!");

  uint32_t log2 = 0;
  while ((1 << log2) != (alignMask + 1))
    log2++;
  return log2;
}

/// Initialize the field offset vector for a dependent-layout class, using the
/// "Universal" layout strategy.
void swift::swift_initClassMetadata_UniversalStrategy(ClassMetadata *self,
                                            const ClassMetadata *super,
                                            size_t numFields,
                                      const ClassFieldLayout *fieldLayouts,
                                            size_t *fieldOffsets) {
  // Start layout by appending to a standard heap object header.
  size_t size, alignMask;

  // If we have a superclass, start from its size and alignment instead.
  if (super) {
    // This is straightforward if the superclass is Swift.
    if (super->isTypeMetadata()) {
      size = super->getInstanceSize();
      alignMask = super->getInstanceAlignMask();

    // If it's Objective-C, we need to clone the ivar descriptors.
    // The data pointer will still be the value we set up according
    // to the compiler ABI.
    } else {
      ClassROData *rodata = (ClassROData*) (self->Data & ~uintptr_t(1));

      // Do layout starting from our notion of where the superclass starts.
      size = rodata->InstanceStart;
      alignMask = 0xF; // malloc alignment guarantee

      if (numFields) {
        // Clone the ivar list.
        const ClassIvarList *dependentIvars = rodata->IvarList;
        assert(dependentIvars->Count == numFields);
        assert(dependentIvars->EntrySize == sizeof(ClassIvarEntry));

        auto ivarListSize = sizeof(ClassIvarList) +
                            numFields * sizeof(ClassIvarEntry);
        auto ivars = (ClassIvarList*) permanentAlloc(ivarListSize);
        memcpy(ivars, dependentIvars, ivarListSize);
        rodata->IvarList = ivars;

        for (unsigned i = 0; i != numFields; ++i) {
          ClassIvarEntry &ivar = ivars->getIvars()[i];

          // The offset variable for the ivar is the respective entry in
          // the field-offset vector.
          ivar.Offset = &fieldOffsets[i];

          // If the ivar's size doesn't match the field layout we
          // computed, overwrite it and give it better type information.
          if (ivar.Size != fieldLayouts[i].Size) {
            ivar.Size = fieldLayouts[i].Size;
            ivar.Type = nullptr;
            ivar.Log2Alignment =
              getLog2AlignmentFromMask(fieldLayouts[i].AlignMask);
          }
        }
      }
    }

  // If we don't have a formal superclass, start with the basic heap header.
  } else {
    auto heapLayout = BasicLayout::initialForHeapObject();
    size = heapLayout.size;
    alignMask = heapLayout.flags.getAlignmentMask();
  }

  for (unsigned i = 0; i != numFields; ++i) {
    auto offset = roundUpToAlignMask(size, fieldLayouts[i].AlignMask);
    fieldOffsets[i] = offset;
    size = offset + fieldLayouts[i].Size;
    alignMask = std::max(alignMask, fieldLayouts[i].AlignMask);
  }

  // Save the final size and alignment into the metadata record.
  assert(self->isTypeMetadata());
  self->setInstanceSize(size);
  self->setInstanceAlignMask(alignMask);
}

/// \brief Fetch the type metadata associated with the formal dynamic
/// type of the given (possibly Objective-C) object.  The formal
/// dynamic type ignores dynamic subclasses such as those introduced
/// by KVO.
///
/// The object pointer may be a tagged pointer, but cannot be null.
const Metadata *swift::swift_getObjectType(HeapObject *object) {
  auto classAsMetadata = _swift_getClass(object);
  if (classAsMetadata->isTypeMetadata()) return classAsMetadata;

  return swift_getObjCClassMetadata(classAsMetadata);
}

/*** Metatypes *************************************************************/

namespace {
  class MetatypeCacheEntry : public CacheEntry<MetatypeCacheEntry> {
    FullMetadata<MetatypeMetadata> Metadata;

  public:
    static const char *getName() { return "MetatypeCache"; }

    MetatypeCacheEntry(size_t numArguments) {}

    static constexpr size_t getNumArguments() {
      return 1;
    }

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
  // When metatypes are accessed opaquely, they always have a "thick"
  // representation.
  return &getUnmanagedPointerPointerValueWitnesses();
}

/// \brief Fetch a uniqued metadata for a metatype type.
extern "C" const MetatypeMetadata *
swift::swift_getMetatypeMetadata(const Metadata *instanceMetadata) {
  // Search the cache.
  const size_t numGenericArgs = 1;
  const void *args[] = { instanceMetadata };
  auto entry = MetatypeTypes.findOrAdd(args, numGenericArgs,
    [&]() -> MetatypeCacheEntry* {
      // Create a new entry for the cache.
      auto entry = MetatypeCacheEntry::allocate(args, numGenericArgs, 0);

      auto metadata = entry->getData();
      metadata->setKind(MetadataKind::Metatype);
      metadata->ValueWitnesses = getMetatypeValueWitnesses(instanceMetadata);
      metadata->InstanceType = instanceMetadata;

      return entry;
    });

  return entry->getData();
}

/*** Existential Metatypes *************************************************/

namespace {
  class ExistentialMetatypeCacheEntry :
      public CacheEntry<ExistentialMetatypeCacheEntry> {
    FullMetadata<ExistentialMetatypeMetadata> Metadata;

  public:
    static const char *getName() { return "ExistentialMetatypeCache"; }

    ExistentialMetatypeCacheEntry(size_t numArguments) {}

    static constexpr size_t getNumArguments() {
      return 1;
    }

    FullMetadata<ExistentialMetatypeMetadata> *getData() {
      return &Metadata;
    }
    const FullMetadata<ExistentialMetatypeMetadata> *getData() const {
      return &Metadata;
    }
  };
}

/// The uniquing structure for existential metatype type metadata.
static MetadataCache<ExistentialMetatypeCacheEntry> ExistentialMetatypeTypes;

static const ExtraInhabitantsValueWitnessTable
ExistentialMetatypeValueWitnesses_1 =
  ValueWitnessTableForBox<ExistentialMetatypeBox<1>>::table;
static const ExtraInhabitantsValueWitnessTable
ExistentialMetatypeValueWitnesses_2 =
  ValueWitnessTableForBox<ExistentialMetatypeBox<2>>::table;

static llvm::DenseMap<unsigned, const ExtraInhabitantsValueWitnessTable*>
  ExistentialMetatypeValueWitnessTables;

/// Instantiate a value witness table for an existential metatype
/// container with the given number of witness table pointers.
static const ExtraInhabitantsValueWitnessTable *
getExistentialMetatypeValueWitnesses(unsigned numWitnessTables) {
  if (numWitnessTables == 0)
    return &getUnmanagedPointerPointerValueWitnesses();
  if (numWitnessTables == 1)
    return &ExistentialMetatypeValueWitnesses_1;
  if (numWitnessTables == 2)
    return &ExistentialMetatypeValueWitnesses_2;

  static_assert(3 * sizeof(void*) >= sizeof(ValueBuffer),
                "not handling all possible inline-storage class existentials!");

  auto found = ExistentialMetatypeValueWitnessTables.find(numWitnessTables);
  if (found != ExistentialMetatypeValueWitnessTables.end())
    return found->second;

  using Box = NonFixedExistentialMetatypeBox;
  using Witnesses = NonFixedValueWitnesses<Box, /*known allocated*/ true>;

  auto *vwt = new ExtraInhabitantsValueWitnessTable;
#define STORE_VAR_EXISTENTIAL_METATYPE_WITNESS(WITNESS) \
  vwt->WITNESS = Witnesses::WITNESS;
  FOR_ALL_FUNCTION_VALUE_WITNESSES(STORE_VAR_EXISTENTIAL_METATYPE_WITNESS)
  STORE_VAR_EXISTENTIAL_METATYPE_WITNESS(storeExtraInhabitant)
  STORE_VAR_EXISTENTIAL_METATYPE_WITNESS(getExtraInhabitantIndex)
#undef STORE_VAR_EXISTENTIAL_METATYPE_WITNESS

  vwt->size = Box::Container::getSize(numWitnessTables);
  vwt->flags = ValueWitnessFlags()
    .withAlignment(Box::Container::getAlignment(numWitnessTables))
    .withPOD(true)
    .withBitwiseTakable(true)
    .withInlineStorage(false)
    .withExtraInhabitants(true);
  vwt->stride = Box::Container::getStride(numWitnessTables);
  vwt->extraInhabitantFlags = ExtraInhabitantFlags()
    .withNumExtraInhabitants(Witnesses::numExtraInhabitants);

  ExistentialMetatypeValueWitnessTables.insert({numWitnessTables, vwt});

  return vwt;
}

/// \brief Fetch a uniqued metadata for a metatype type.
extern "C" const ExistentialMetatypeMetadata *
swift::swift_getExistentialMetatypeMetadata(const Metadata *instanceMetadata) {
  // Search the cache.
  const size_t numGenericArgs = 1;
  const void *args[] = { instanceMetadata };
  auto entry = ExistentialMetatypeTypes.findOrAdd(args, numGenericArgs,
    [&]() -> ExistentialMetatypeCacheEntry* {
      // Create a new entry for the cache.
      auto entry =
        ExistentialMetatypeCacheEntry::allocate(args, numGenericArgs, 0);

      ExistentialTypeFlags flags;
      if (instanceMetadata->getKind() == MetadataKind::Existential) {
        flags = static_cast<const ExistentialTypeMetadata*>(instanceMetadata)->Flags;
      } else {
        assert(instanceMetadata->getKind()==MetadataKind::ExistentialMetatype);
        flags = static_cast<const ExistentialMetatypeMetadata*>(instanceMetadata)->Flags;
      }

      auto metadata = entry->getData();
      metadata->setKind(MetadataKind::ExistentialMetatype);
      metadata->ValueWitnesses =
        getExistentialMetatypeValueWitnesses(flags.getNumWitnessTables());
      metadata->InstanceType = instanceMetadata;
      metadata->Flags = flags;

      return entry;
    });

  return entry->getData();
}

/*** Existential types ********************************************************/

namespace {
  class ExistentialCacheEntry : public CacheEntry<ExistentialCacheEntry> {
  public:
    FullMetadata<ExistentialTypeMetadata> Metadata;

    static const char *getName() { return "ExistentialCache"; }

    ExistentialCacheEntry(size_t numArguments) {
      Metadata.Protocols.NumProtocols = numArguments;
    }

    size_t getNumArguments() const {
      return Metadata.Protocols.NumProtocols;
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

static const ValueWitnessTable OpaqueExistentialValueWitnesses_0 =
  ValueWitnessTableForBox<OpaqueExistentialBox<0>>::table;
static const ValueWitnessTable OpaqueExistentialValueWitnesses_1 =
  ValueWitnessTableForBox<OpaqueExistentialBox<1>>::table;

static llvm::DenseMap<unsigned, const ValueWitnessTable*>
  OpaqueExistentialValueWitnessTables;

/// Instantiate a value witness table for an opaque existential container with
/// the given number of witness table pointers.
static const ValueWitnessTable *
getOpaqueExistentialValueWitnesses(unsigned numWitnessTables) {
  // We pre-allocate a couple of important cases.
  if (numWitnessTables == 0)
    return &OpaqueExistentialValueWitnesses_0;
  if (numWitnessTables == 1)
    return &OpaqueExistentialValueWitnesses_1;

  // FIXME: make thread-safe

  auto found = OpaqueExistentialValueWitnessTables.find(numWitnessTables);
  if (found != OpaqueExistentialValueWitnessTables.end())
    return found->second;

  using Box = NonFixedOpaqueExistentialBox;
  using Witnesses = NonFixedValueWitnesses<Box, /*known allocated*/ true>;
  static_assert(!Witnesses::hasExtraInhabitants, "no extra inhabitants");

  auto *vwt = new ValueWitnessTable;
#define STORE_VAR_OPAQUE_EXISTENTIAL_WITNESS(WITNESS) \
  vwt->WITNESS = Witnesses::WITNESS;
  FOR_ALL_FUNCTION_VALUE_WITNESSES(STORE_VAR_OPAQUE_EXISTENTIAL_WITNESS)
#undef STORE_VAR_OPAQUE_EXISTENTIAL_WITNESS

  vwt->size = Box::Container::getSize(numWitnessTables);
  vwt->flags = ValueWitnessFlags()
    .withAlignment(Box::Container::getAlignment(numWitnessTables))
    .withPOD(false)
    .withBitwiseTakable(false)
    .withInlineStorage(false)
    .withExtraInhabitants(false);
  vwt->stride = Box::Container::getStride(numWitnessTables);

  OpaqueExistentialValueWitnessTables.insert({numWitnessTables, vwt});

  return vwt;
}

static const ExtraInhabitantsValueWitnessTable ClassExistentialValueWitnesses_1 =
  ValueWitnessTableForBox<ClassExistentialBox<1>>::table;
static const ExtraInhabitantsValueWitnessTable ClassExistentialValueWitnesses_2 =
  ValueWitnessTableForBox<ClassExistentialBox<2>>::table;

static llvm::DenseMap<unsigned, const ExtraInhabitantsValueWitnessTable*>
  ClassExistentialValueWitnessTables;

/// Instantiate a value witness table for a class-constrained existential
/// container with the given number of witness table pointers.
static const ExtraInhabitantsValueWitnessTable *
getClassExistentialValueWitnesses(unsigned numWitnessTables) {
  if (numWitnessTables == 0) {
#if SWIFT_OBJC_INTEROP
    return &_TWVBO;
#else
    return &_TWVBo;
#endif
  }
  if (numWitnessTables == 1)
    return &ClassExistentialValueWitnesses_1;
  if (numWitnessTables == 2)
    return &ClassExistentialValueWitnesses_2;

  static_assert(3 * sizeof(void*) >= sizeof(ValueBuffer),
                "not handling all possible inline-storage class existentials!");

  auto found = ClassExistentialValueWitnessTables.find(numWitnessTables);
  if (found != ClassExistentialValueWitnessTables.end())
    return found->second;

  using Box = NonFixedClassExistentialBox;
  using Witnesses = NonFixedValueWitnesses<Box, /*known allocated*/ true>;

  auto *vwt = new ExtraInhabitantsValueWitnessTable;
#define STORE_VAR_CLASS_EXISTENTIAL_WITNESS(WITNESS) \
  vwt->WITNESS = Witnesses::WITNESS;
  FOR_ALL_FUNCTION_VALUE_WITNESSES(STORE_VAR_CLASS_EXISTENTIAL_WITNESS)
  STORE_VAR_CLASS_EXISTENTIAL_WITNESS(storeExtraInhabitant)
  STORE_VAR_CLASS_EXISTENTIAL_WITNESS(getExtraInhabitantIndex)
#undef STORE_VAR_CLASS_EXISTENTIAL_WITNESS

  vwt->size = Box::Container::getSize(numWitnessTables);
  vwt->flags = ValueWitnessFlags()
    .withAlignment(Box::Container::getAlignment(numWitnessTables))
    .withPOD(false)
    .withBitwiseTakable(true)
    .withInlineStorage(false)
    .withExtraInhabitants(true);
  vwt->stride = Box::Container::getStride(numWitnessTables);
  vwt->extraInhabitantFlags = ExtraInhabitantFlags()
    .withNumExtraInhabitants(Witnesses::numExtraInhabitants);

  ClassExistentialValueWitnessTables.insert({numWitnessTables, vwt});

  return vwt;
}

/// Get the value witness table for an existential type, first trying to use a
/// shared specialized table for common cases.
static const ValueWitnessTable *
getExistentialValueWitnesses(ProtocolClassConstraint classConstraint,
                             unsigned numWitnessTables) {
  switch (classConstraint) {
  case ProtocolClassConstraint::Class:
    return getClassExistentialValueWitnesses(numWitnessTables);
  case ProtocolClassConstraint::Any:
    return getOpaqueExistentialValueWitnesses(numWitnessTables);
  }
}

const OpaqueValue *
ExistentialTypeMetadata::projectValue(const OpaqueValue *container) const {
  // The layout of the container depends on whether it's class-constrained.
  if (Flags.getClassConstraint() == ProtocolClassConstraint::Class) {
    auto classContainer =
      reinterpret_cast<const ClassExistentialContainer*>(container);
    return reinterpret_cast<const OpaqueValue *>(&classContainer->Value);
  } else {
    auto opaqueContainer =
      reinterpret_cast<const OpaqueExistentialContainer*>(container);
    return opaqueContainer->Type->vw_projectBuffer(
                         const_cast<ValueBuffer*>(&opaqueContainer->Buffer));
  }
}

const Metadata *
ExistentialTypeMetadata::getDynamicType(const OpaqueValue *container) const {
  // The layout of the container depends on whether it's class-constrained.
  if (isClassBounded()) {
    auto classContainer =
      reinterpret_cast<const ClassExistentialContainer*>(container);
    void *obj = classContainer->Value;
    return swift_getObjectType(reinterpret_cast<HeapObject*>(obj));
  } else {
    auto opaqueContainer =
      reinterpret_cast<const OpaqueExistentialContainer*>(container);
    return opaqueContainer->Type;
  }
}

const WitnessTable * const *
ExistentialTypeMetadata::getWitnessTable(const OpaqueValue *container,
                                         unsigned i) const {
  assert(i < Flags.getNumWitnessTables());

  // The layout of the container depends on whether it's class-constrained.
  const WitnessTable * const * witnessTables;
  if (isClassBounded()) {
    auto classContainer =
      reinterpret_cast<const ClassExistentialContainer*>(container);
    witnessTables = classContainer->getWitnessTables();
  } else {
    auto opaqueContainer =
      reinterpret_cast<const OpaqueExistentialContainer*>(container);
    witnessTables = opaqueContainer->getWitnessTables();
  }

  // The return type here describes extra structure for the protocol
  // witness table for some reason.  We should probaby have a nominal
  // type for these, just for type safety reasons.
  return reinterpret_cast<const WitnessTable * const *>(witnessTables[i]);
}

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

  // Search the cache.

  auto protocolArgs = reinterpret_cast<const void * const *>(protocols);

  auto entry = ExistentialTypes.findOrAdd(protocolArgs, numProtocols,
    [&]() -> ExistentialCacheEntry* {
      // Create a new entry for the cache.
      auto entry = ExistentialCacheEntry::allocate(protocolArgs, numProtocols,
                             sizeof(const ProtocolDescriptor *) * numProtocols);
      auto metadata = entry->getData();
      metadata->setKind(MetadataKind::Existential);
      metadata->ValueWitnesses = getExistentialValueWitnesses(classConstraint,
                                                              numWitnessTables);
      metadata->Flags = ExistentialTypeFlags()
        .withNumWitnessTables(numWitnessTables)
        .withClassConstraint(classConstraint);
      metadata->Protocols.NumProtocols = numProtocols;
      for (size_t i = 0; i < numProtocols; ++i)
        metadata->Protocols[i] = protocols[i];

      return entry;
    });
  return entry->getData();
}

/// \brief Perform a copy-assignment from one existential container to another.
/// Both containers must be of the same existential type representable with no
/// witness tables.
OpaqueValue *swift::swift_assignExistentialWithCopy0(OpaqueValue *dest,
                                                     const OpaqueValue *src,
                                                     const Metadata *type) {
  using Witnesses = ValueWitnesses<OpaqueExistentialBox<0>>;
  return Witnesses::assignWithCopy(dest, const_cast<OpaqueValue*>(src), type);
}

/// \brief Perform a copy-assignment from one existential container to another.
/// Both containers must be of the same existential type representable with one
/// witness table.
OpaqueValue *swift::swift_assignExistentialWithCopy1(OpaqueValue *dest,
                                                     const OpaqueValue *src,
                                                     const Metadata *type) {
  using Witnesses = ValueWitnesses<OpaqueExistentialBox<1>>;
  return Witnesses::assignWithCopy(dest, const_cast<OpaqueValue*>(src), type);
}

/// \brief Perform a copy-assignment from one existential container to another.
/// Both containers must be of the same existential type representable with the
/// same number of witness tables.
OpaqueValue *swift::swift_assignExistentialWithCopy(OpaqueValue *dest,
                                                    const OpaqueValue *src,
                                                    const Metadata *type) {
  assert(!type->getValueWitnesses()->isValueInline());
  using Witnesses = NonFixedValueWitnesses<NonFixedOpaqueExistentialBox,
                                           /*known allocated*/ true>;
  return Witnesses::assignWithCopy(dest, const_cast<OpaqueValue*>(src), type);
}

/*** Foreign types *********************************************************/

namespace {
  /// A string whose data is globally-allocated.
  struct GlobalString {
    StringRef Data;
    /*implicit*/ GlobalString(StringRef data) : Data(data) {}
  };
}

template <>
struct llvm::DenseMapInfo<GlobalString> {
  static GlobalString getEmptyKey() {
    return StringRef((const char*) 0, 0);
  }
  static GlobalString getTombstoneKey() {
    return StringRef((const char*) 1, 0);
  }
  static unsigned getHashValue(const GlobalString &val) {
    // llvm::hash_value(StringRef) is, unfortunately, defined out of
    // line in a library we otherwise would not need to link against.
    return llvm::hash_combine_range(val.Data.begin(), val.Data.end());
  }
  static bool isEqual(const GlobalString &lhs, const GlobalString &rhs) {
    return lhs.Data == rhs.Data;
  }
};

// We use a DenseMap over what are essentially StringRefs instead of a
// StringMap because we don't need to actually copy the string.
static pthread_mutex_t ForeignTypesLock = PTHREAD_MUTEX_INITIALIZER;
static llvm::DenseMap<GlobalString, const ForeignTypeMetadata *> ForeignTypes;

const ForeignTypeMetadata *
swift::swift_getForeignTypeMetadata(ForeignTypeMetadata *nonUnique) {
  // Fast path: check the invasive cache.
  if (auto unique = nonUnique->getCachedUniqueMetadata()) {
    return unique;
  }

  // Okay, insert a new row.
  pthread_mutex_lock(&ForeignTypesLock);
  auto insertResult = ForeignTypes.insert({GlobalString(nonUnique->getName()),
                                           nonUnique});
  auto uniqueMetadata = insertResult.first->second;

  // If the insertion created a new entry, set up the metadata we were
  // passed as the insertion result.
  if (insertResult.second) {
    // Call the initialization callback if present.
    if (nonUnique->hasInitializationFunction())
      nonUnique->getInitializationFunction()(nonUnique);
  }

  // Remember the unique result in the invasive cache.  We don't want
  // to do this until after the initialization completes; otherwise,
  // it will be possible for code to fast-path through this function
  // too soon.
  nonUnique->setCachedUniqueMetadata(uniqueMetadata);
  pthread_mutex_unlock(&ForeignTypesLock);
  return uniqueMetadata;
}

/*** Other metadata routines ***********************************************/

const NominalTypeDescriptor *
Metadata::getNominalTypeDescriptor() const {
  switch (getKind()) {
  case MetadataKind::Class: {
    const ClassMetadata *cls = static_cast<const ClassMetadata *>(this);
    if (!cls->isTypeMetadata())
      return nullptr;
    if (cls->isArtificialSubclass())
      return nullptr;
    return cls->getDescription();
  }
  case MetadataKind::Struct:
  case MetadataKind::Enum:
    return static_cast<const StructMetadata *>(this)->Description;
  case MetadataKind::ForeignClass:
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::PolyFunction:
  case MetadataKind::Existential:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Metatype:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::HeapLocalVariable:
    return nullptr;
  }
}

const GenericMetadata *
Metadata::getGenericPattern() const {
  auto ntd = getNominalTypeDescriptor();
  if (!ntd)
    return nullptr;
  return ntd->GenericMetadataPattern;
}

const ClassMetadata *
Metadata::getClassObject() const {
  switch (getKind()) {
  case MetadataKind::Class: {
    // Native Swift class metadata is also the class object.
    return static_cast<const ClassMetadata *>(this);
  }
  case MetadataKind::ObjCClassWrapper: {
    // Objective-C class objects are referenced by their Swift metadata wrapper.
    auto wrapper = static_cast<const ObjCClassWrapperMetadata *>(this);
    return wrapper->Class;
  }
  // Other kinds of types don't have class objects.
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::ForeignClass:
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::PolyFunction:
  case MetadataKind::Existential:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Metatype:
  case MetadataKind::HeapLocalVariable:
    return nullptr;
  }
}

/// Scan and return a single run-length encoded identifier.
/// Returns a malloc-allocated string, or nullptr on failure.
/// mangled is advanced past the end of the scanned token.
static char *scanIdentifier(const char *&mangled)
{
  const char *original = mangled;

  {
    if (*mangled == '0') goto fail;  // length may not be zero

    size_t length = 0;
    while (isdigit(*mangled)) {
      size_t oldlength = length;
      length *= 10;
      length += *mangled++ - '0';
      if (length <= oldlength) goto fail;  // integer overflow
    }

    if (length == 0) goto fail;
    if (length > strlen(mangled)) goto fail;

    char *result = strndup(mangled, length);
    assert(result);
    mangled += length;
    return result;
  }

fail:
  mangled = original;  // rewind
  return nullptr;
}


/// \brief Demangle a mangled class name into module+class.
/// Returns true if the name was successfully decoded.
/// On success, *outModule and *outClass must be freed with free().
/// FIXME: this should be replaced by a real demangler
bool swift::swift_demangleSimpleClass(const char *mangledName,
                                      char **outModule, char **outClass) {
  char *moduleName = nullptr;
  char *className = nullptr;

  {
    // Prefix for a mangled class
    const char *m = mangledName;
    if (0 != strncmp(m, "_TtC", 4))
      goto fail;
    m += 4;

    // Module name
    if (strncmp(m, "Ss", 2) == 0) {
      moduleName = strdup(swift::STDLIB_NAME);
      assert(moduleName);
      m += 2;
    } else {
      moduleName = scanIdentifier(m);
      if (!moduleName)
        goto fail;
    }

    // Class name
    className = scanIdentifier(m);
    if (!className)
      goto fail;

    // Nothing else
    if (strlen(m))
      goto fail;

    *outModule = moduleName;
    *outClass = className;
    return true;
  }

fail:
  if (moduleName) free(moduleName);
  if (className) free(className);
  *outModule = nullptr;
  *outClass = nullptr;
  return false;
}

namespace llvm { namespace hashing { namespace detail {
  // An extern variable expected by LLVM's hashing templates. We don't link any
  // LLVM libs into the runtime, so define this here.
  size_t fixed_seed_override = 0;
} } }

