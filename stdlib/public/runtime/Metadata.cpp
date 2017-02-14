//===--- Metadata.cpp - Swift Language ABI Metadata Support ---------------===//
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
// Implementations of the metadata ABI functions.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/MathExtras.h"
#include "swift/Basic/Demangle.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Mutex.h"
#include "swift/Strings.h"
#include "MetadataCache.h"
#include <algorithm>
#include <condition_variable>
#include <new>
#include <cctype>
#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
// Avoid defining macro max(), min() which conflict with std::max(), std::min()
#define NOMINMAX
#include <windows.h>
#else
#include <sys/mman.h>
#include <unistd.h>
#endif
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Hashing.h"
#include "ErrorObject.h"
#include "ExistentialMetadataImpl.h"
#include "swift/Runtime/Debug.h"
#include "Private.h"

#if defined(__APPLE__)
#include <mach/vm_page_size.h>
#endif

#if SWIFT_OBJC_INTEROP
#include <objc/runtime.h>
#endif

#include <cstdio>

#if defined(__APPLE__) && defined(VM_MEMORY_SWIFT_METADATA)
#define VM_TAG_FOR_SWIFT_METADATA VM_MAKE_TAG(VM_MEMORY_SWIFT_METADATA)
#else
#define VM_TAG_FOR_SWIFT_METADATA (-1)
#endif

using namespace swift;
using namespace metadataimpl;

template <class T>
static int compareIntegers(T left, T right) {
  return (left == right ? 0 : left < right ? -1 : 1);
}

namespace {
  struct GenericCacheEntry;

  // The cache entries in a generic cache are laid out like this:
  struct GenericCacheEntryHeader {
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
} // end anonymous namespace

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

/// Fetch the metadata cache for a generic metadata structure,
/// in a context where it must have already been initialized.
static GenericMetadataCache &unsafeGetInitializedCache(GenericMetadata *metadata) {
  // Keep this assert even if you change the representation above.
  static_assert(sizeof(LazyGenericMetadataCache) <=
                sizeof(GenericMetadata::PrivateData),
                "metadata cache is larger than the allowed space");

  auto lazyCache =
    reinterpret_cast<LazyGenericMetadataCache*>(metadata->PrivateData);
  return lazyCache->unsafeGetAlreadyInitialized();
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

  char *bytes = GenericCacheEntry::allocate(
                              unsafeGetInitializedCache(pattern).getAllocator(),
                              argumentsAsArray,
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
  // Adjust the relative reference to the nominal type descriptor.
  if (!metadata->isArtificialSubclass()) {
    auto patternBytes =
      reinterpret_cast<const char*>(pattern->getMetadataTemplate()) +
      pattern->AddressPoint;
    metadata->setDescription(
        reinterpret_cast<const ClassMetadata*>(patternBytes)->getDescription());
  }

  // Adjust the class object extents.
  if (extraPrefixSize) {
    metadata->setClassSize(metadata->getClassSize() + extraPrefixSize);
    metadata->setClassAddressPoint(prefixSize);
  }
  assert(metadata->getClassAddressPoint() == prefixSize);

  return metadata;
}

ValueMetadata *
swift::swift_allocateGenericValueMetadata(GenericMetadata *pattern,
                                          const void *arguments) {
  void * const *argumentsAsArray = reinterpret_cast<void * const *>(arguments);
  size_t numGenericArguments = pattern->NumKeyArguments;

  char *bytes =
    GenericCacheEntry::allocate(
                              unsafeGetInitializedCache(pattern).getAllocator(),
                              argumentsAsArray, numGenericArguments,
                              pattern->MetadataSize)->getData<char>();

  // Copy in the metadata template.
  memcpy(bytes, pattern->getMetadataTemplate(), pattern->MetadataSize);

  // Okay, move to the address point.
  bytes += pattern->AddressPoint;
  auto *metadata = reinterpret_cast<ValueMetadata*>(bytes);
  
  // Adjust the relative references to the nominal type descriptor and
  // parent type.
  auto patternBytes =
    reinterpret_cast<const char*>(pattern->getMetadataTemplate()) +
    pattern->AddressPoint;
  auto patternMetadata = reinterpret_cast<const ValueMetadata*>(patternBytes);
  metadata->Description = patternMetadata->Description.get();
  metadata->Parent = patternMetadata->Parent;
  
  return metadata;
}

/// The primary entrypoint.
const Metadata *swift::swift_getGenericMetadata(GenericMetadata *pattern,
                                                const void *arguments)
    SWIFT_CC(RegisterPreservingCC_IMPL) {
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

/***************************************************************************/
/*** Objective-C class wrappers ********************************************/
/***************************************************************************/

#if SWIFT_OBJC_INTEROP

namespace {
  class ObjCClassCacheEntry {
  public:
    FullMetadata<ObjCClassWrapperMetadata> Data;

    ObjCClassCacheEntry(const ClassMetadata *theClass) {
      Data.setKind(MetadataKind::ObjCClassWrapper);
      Data.ValueWitnesses = &VALUE_WITNESS_SYM(BO);
      Data.Class = theClass;
    }

    intptr_t getKeyIntValueForDump() {
      return reinterpret_cast<intptr_t>(Data.Class);
    }

    int compareWithKey(const ClassMetadata *theClass) const {
      return comparePointers(theClass, Data.Class);
    }

    static size_t getExtraAllocationSize(const ClassMetadata *key) {
      return 0;
    }
    size_t getExtraAllocationSize() const {
      return 0;
    }
  };
}

/// The uniquing structure for ObjC class-wrapper metadata.
static SimpleGlobalCache<ObjCClassCacheEntry> ObjCClassWrappers;

#endif

const Metadata *
swift::swift_getObjCClassMetadata(const ClassMetadata *theClass) {
  // Make calls resilient against receiving a null Objective-C class. This can
  // happen when classes are weakly linked and not available.
  if (theClass == nullptr)
    return nullptr;

  // If the class pointer is valid as metadata, no translation is required.
  if (theClass->isTypeMetadata()) {
    return theClass;
  }

#if SWIFT_OBJC_INTEROP
  return &ObjCClassWrappers.getOrInsert(theClass).first->Data;
#else
  fatalError(/* flags = */ 0,
             "swift_getObjCClassMetadata: no Objective-C interop");
#endif
}

/***************************************************************************/
/*** Functions *************************************************************/
/***************************************************************************/

namespace {

class FunctionCacheEntry {
public:
  FullMetadata<FunctionTypeMetadata> Data;

  struct Key {
    const void * const *FlagsArgsAndResult;

    FunctionTypeFlags getFlags() const {
      return FunctionTypeFlags::fromIntValue(size_t(FlagsArgsAndResult[0]));
    }

    const Metadata *getResult() const {
      auto opaqueResult = FlagsArgsAndResult[getFlags().getNumArguments() + 1];
      return reinterpret_cast<const Metadata *>(opaqueResult);
    }

    const void * const *getArguments() const {
      return &FlagsArgsAndResult[1];
    }
  };

  FunctionCacheEntry(Key key);

  intptr_t getKeyIntValueForDump() {
    return 0; // No single meaningful value here.
  }

  int compareWithKey(Key key) const {
    auto keyFlags = key.getFlags();
    if (auto result = compareIntegers(keyFlags.getIntValue(),
                                      Data.Flags.getIntValue()))
      return result;

    if (auto result = comparePointers(key.getResult(), Data.ResultType))
      return result;

    for (unsigned i = 0, e = keyFlags.getNumArguments(); i != e; ++i) {
      if (auto result =
            comparePointers(key.getArguments()[i],
                            Data.getArguments()[i].getOpaqueValue()))
        return result;
    }

    return 0;
  }

  static size_t getExtraAllocationSize(Key key) {
    return key.getFlags().getNumArguments()
         * sizeof(FunctionTypeMetadata::Argument);
  }
  size_t getExtraAllocationSize() const {
    return Data.Flags.getNumArguments()
         * sizeof(FunctionTypeMetadata::Argument);
  }
};

} // end anonymous namespace

/// The uniquing structure for function type metadata.
static SimpleGlobalCache<FunctionCacheEntry> FunctionTypes;

const FunctionTypeMetadata *
swift::swift_getFunctionTypeMetadata1(FunctionTypeFlags flags,
                                      const void *arg0,
                                      const Metadata *result) {
  assert(flags.getNumArguments() == 1
         && "wrong number of arguments in function metadata flags?!");
  const void *flagsArgsAndResult[] = {
    reinterpret_cast<const void*>(flags.getIntValue()),
    arg0,
    static_cast<const void *>(result)                      
  };                                                       
  return swift_getFunctionTypeMetadata(flagsArgsAndResult);
}                                                          
const FunctionTypeMetadata *                               
swift::swift_getFunctionTypeMetadata2(FunctionTypeFlags flags,
                                      const void *arg0,
                                      const void *arg1,
                                      const Metadata *result) {
  assert(flags.getNumArguments() == 2
         && "wrong number of arguments in function metadata flags?!");
  const void *flagsArgsAndResult[] = {
    reinterpret_cast<const void*>(flags.getIntValue()),
    arg0,
    arg1,                                                  
    static_cast<const void *>(result)                      
  };                                                       
  return swift_getFunctionTypeMetadata(flagsArgsAndResult);
}                                                          
const FunctionTypeMetadata *                               
swift::swift_getFunctionTypeMetadata3(FunctionTypeFlags flags,
                                      const void *arg0,
                                      const void *arg1,
                                      const void *arg2,
                                      const Metadata *result) {
  assert(flags.getNumArguments() == 3
         && "wrong number of arguments in function metadata flags?!");
  const void *flagsArgsAndResult[] = {
    reinterpret_cast<const void*>(flags.getIntValue()),
    arg0,                                                  
    arg1,                                                  
    arg2,                                                  
    static_cast<const void *>(result)                      
  };                                                       
  return swift_getFunctionTypeMetadata(flagsArgsAndResult);
}

const FunctionTypeMetadata *
swift::swift_getFunctionTypeMetadata(const void *flagsArgsAndResult[]) {
  FunctionCacheEntry::Key key = { flagsArgsAndResult };
  return &FunctionTypes.getOrInsert(key).first->Data;
}

FunctionCacheEntry::FunctionCacheEntry(Key key) {
  auto flags = key.getFlags();

  // Pick a value witness table appropriate to the function convention.
  // All function types of a given convention have the same value semantics,
  // so they share a value witness table.
  switch (flags.getConvention()) {
  case FunctionMetadataConvention::Swift:
    Data.ValueWitnesses = &VALUE_WITNESS_SYM(FUNCTION_MANGLING);
    break;

  case FunctionMetadataConvention::Thin:
  case FunctionMetadataConvention::CFunctionPointer:
    Data.ValueWitnesses = &VALUE_WITNESS_SYM(THIN_FUNCTION_MANGLING);
    break;

  case FunctionMetadataConvention::Block:
#if SWIFT_OBJC_INTEROP
    // Blocks are ObjC objects, so can share the Builtin.UnknownObject value
    // witnesses.
    Data.ValueWitnesses = &VALUE_WITNESS_SYM(BO);
#else
    assert(false && "objc block without objc interop?");
#endif
    break;
  }

  unsigned numArguments = flags.getNumArguments();

  Data.setKind(MetadataKind::Function);
  Data.Flags = flags;
  Data.ResultType = key.getResult();

  for (size_t i = 0; i < numArguments; ++i) {
    auto opaqueArg = key.getArguments()[i];
    auto arg = FunctionTypeMetadata::Argument::getFromOpaqueValue(opaqueArg);
    Data.getArguments()[i] = arg;
  }
}

/***************************************************************************/
/*** Tuples ****************************************************************/
/***************************************************************************/

namespace {

class TupleCacheEntry {
public:
  // NOTE: if you change the layout of this type, you'll also need
  // to update tuple_getValueWitnesses().
  ExtraInhabitantsValueWitnessTable Witnesses;
  FullMetadata<TupleTypeMetadata> Data;

  struct Key {
    size_t NumElements;
    const Metadata * const *Elements;
    const char *Labels;
  };

  TupleCacheEntry(const Key &key, const ValueWitnessTable *proposedWitnesses);

  size_t getNumElements() const {
    return Data.NumElements;
  }

  intptr_t getKeyIntValueForDump() {
    return 0; // No single meaningful value
  }

  int compareWithKey(const Key &key) const {
    // Order by the cheaper comparisons first:

    // The number of elements.
    if (auto result = compareIntegers(key.NumElements, Data.NumElements))
      return result;

    // The element types.
    for (size_t i = 0, e = key.NumElements; i != e; ++i) {
      if (auto result = comparePointers(key.Elements[i],
                                        Data.getElements()[i].Type))
        return result;
    }

    // It's unlikely that we'll get pointer-equality here unless we're being
    // called from the same module or both label strings are null, but
    // those are important cases.
    if (key.Labels != Data.Labels) {
      // Order no-labels before labels.
      if (!key.Labels) return -1;
      if (!Data.Labels) return 1;

      // Just do a strcmp.
      if (auto result = strcmp(key.Labels, Data.Labels))
        return result;
    }

    return 0;
  }

  static size_t getExtraAllocationSize(const Key &key,
                                       const ValueWitnessTable *proposed) {
    return key.NumElements * sizeof(TupleTypeMetadata::Element);
  }
  size_t getExtraAllocationSize() const {
    return Data.NumElements * sizeof(TupleTypeMetadata::Element);
  }
};

} // end anonymous namespace

/// The uniquing structure for tuple type metadata.
static SimpleGlobalCache<TupleCacheEntry> TupleTypes;

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
    auto &eltInfo = metatype.getElement(i);
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
  auto &eltInfo = metatype.getElement(0);

  assert(eltInfo.Offset == 0);
  OpaqueValue *elt = tuple;

  eltInfo.Type->vw_storeExtraInhabitant(elt, index);
}

static int tuple_getExtraInhabitantIndex(const OpaqueValue *tuple,
                                         const Metadata *_metatype) {
  auto &metatype = *(const TupleTypeMetadata*) _metatype;
  auto &eltInfo = metatype.getElement(0);

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
template<typename FUNCTOR, typename LAYOUT>
void performBasicLayout(BasicLayout &layout,
                        const LAYOUT * const *elements,
                        size_t numElements,
                        FUNCTOR &&f) {
  size_t size = layout.size;
  size_t alignMask = layout.flags.getAlignmentMask();
  bool isPOD = layout.flags.isPOD();
  bool isBitwiseTakable = layout.flags.isBitwiseTakable();
  for (unsigned i = 0; i != numElements; ++i) {
    auto elt = elements[i];

    // Lay out this element.
    const TypeLayout *eltLayout = elt->getTypeLayout();
    size = roundUpToAlignMask(size, eltLayout->flags.getAlignmentMask());

    // Report this record to the functor.
    f(i, elt, size);

    // Update the size and alignment of the aggregate..
    size += eltLayout->size;
    alignMask = std::max(alignMask, eltLayout->flags.getAlignmentMask());
    if (!eltLayout->flags.isPOD()) isPOD = false;
    if (!eltLayout->flags.isBitwiseTakable()) isBitwiseTakable = false;
  }
  bool isInline = ValueWitnessTable::isValueInline(size, alignMask + 1);

  layout.size = size;
  layout.flags = ValueWitnessFlags().withAlignmentMask(alignMask)
                                    .withPOD(isPOD)
                                    .withBitwiseTakable(isBitwiseTakable)
                                    .withInlineStorage(isInline);
  layout.stride = std::max(size_t(1), roundUpToAlignMask(size, alignMask));
}
} // end anonymous namespace

const TupleTypeMetadata *
swift::swift_getTupleTypeMetadata(size_t numElements,
                                  const Metadata * const *elements,
                                  const char *labels,
                                  const ValueWitnessTable *proposedWitnesses) {
  // Bypass the cache for the empty tuple. We might reasonably get called
  // by generic code, like a demangler that produces type objects.
  if (numElements == 0) return &METADATA_SYM(EMPTY_TUPLE_MANGLING);

  // Search the cache.
  TupleCacheEntry::Key key = { numElements, elements, labels };
  return &TupleTypes.getOrInsert(key, proposedWitnesses).first->Data;
}

TupleCacheEntry::TupleCacheEntry(const Key &key,
                                 const ValueWitnessTable *proposedWitnesses) {
  Data.setKind(MetadataKind::Tuple);
  Data.ValueWitnesses = &Witnesses;
  Data.NumElements = key.NumElements;
  Data.Labels = key.Labels;

  // Perform basic layout on the tuple.
  auto layout = BasicLayout::initialForValueType();
  performBasicLayout(layout, key.Elements, key.NumElements,
    [&](size_t i, const Metadata *elt, size_t offset) {
      Data.getElement(i).Type = elt;
      Data.getElement(i).Offset = offset;
    });

  Witnesses.size = layout.size;
  Witnesses.flags = layout.flags;
  Witnesses.stride = layout.stride;

  // Copy the function witnesses in, either from the proposed
  // witnesses or from the standard table.
  if (!proposedWitnesses) {
    // For a tuple with a single element, just use the witnesses for
    // the element type.
    if (key.NumElements == 1) {
      proposedWitnesses = key.Elements[0]->getValueWitnesses();

      // Otherwise, use generic witnesses (when we can't pattern-match
      // into something better).
    } else if (layout.flags.isInlineStorage()
               && layout.flags.isPOD()) {
      if (layout.size == 8 && layout.flags.getAlignmentMask() == 7)
        proposedWitnesses = &VALUE_WITNESS_SYM(Bi64_);
      else if (layout.size == 4 && layout.flags.getAlignmentMask() == 3)
        proposedWitnesses = &VALUE_WITNESS_SYM(Bi32_);
      else if (layout.size == 2 && layout.flags.getAlignmentMask() == 1)
        proposedWitnesses = &VALUE_WITNESS_SYM(Bi16_);
      else if (layout.size == 1)
        proposedWitnesses = &VALUE_WITNESS_SYM(Bi8_);
      else
        proposedWitnesses = &tuple_witnesses_pod_inline;
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
  Witnesses.NAME = proposedWitnesses->NAME;
  FOR_ALL_FUNCTION_VALUE_WITNESSES(ASSIGN_TUPLE_WITNESS)
#undef ASSIGN_TUPLE_WITNESS

  // We have extra inhabitants if the first element does.
  // FIXME: generalize this.
  if (auto firstEltEIVWT = dyn_cast<ExtraInhabitantsValueWitnessTable>(
                             key.Elements[0]->getValueWitnesses())) {
    Witnesses.flags = Witnesses.flags.withExtraInhabitants(true);
    Witnesses.extraInhabitantFlags = firstEltEIVWT->extraInhabitantFlags;
    Witnesses.storeExtraInhabitant = tuple_storeExtraInhabitant;
    Witnesses.getExtraInhabitantIndex = tuple_getExtraInhabitantIndex;
  }
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

/***************************************************************************/
/*** Common value witnesses ************************************************/
/***************************************************************************/

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
} // end anonymous namespace

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

static OpaqueValue *pod_indirect_initializeBufferWithTakeOfBuffer(
                    ValueBuffer *dest, ValueBuffer *src, const Metadata *self) {
  memcpy(dest, src, sizeof(ValueBuffer));
  return *reinterpret_cast<OpaqueValue**>(dest);
}

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

static constexpr uint64_t sizeWithAlignmentMask(uint64_t size,
                                                uint64_t alignmentMask) {
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
      commonVWT = &VALUE_WITNESS_SYM(Bi8_);
      break;
    case sizeWithAlignmentMask(2, 1):
      commonVWT = &VALUE_WITNESS_SYM(Bi16_);
      break;
    case sizeWithAlignmentMask(4, 3):
      commonVWT = &VALUE_WITNESS_SYM(Bi32_);
      break;
    case sizeWithAlignmentMask(8, 7):
      commonVWT = &VALUE_WITNESS_SYM(Bi64_);
      break;
    case sizeWithAlignmentMask(16, 15):
      commonVWT = &VALUE_WITNESS_SYM(Bi128_);
      break;
    case sizeWithAlignmentMask(32, 31):
      commonVWT = &VALUE_WITNESS_SYM(Bi256_);
      break;
    }
    
  #define INSTALL_POD_COMMON_WITNESS(NAME) vwtable->NAME = commonVWT->NAME;
    FOR_ALL_FUNCTION_VALUE_WITNESSES(INSTALL_POD_COMMON_WITNESS)
  #undef INSTALL_POD_COMMON_WITNESS
    
    return;
  }
  
  if (flags.isBitwiseTakable()) {
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

  if (!flags.isInlineStorage()) {
    // For values stored out-of-line, initializeBufferWithTakeOfBuffer is
    // always a memcpy.
    vwtable->initializeBufferWithTakeOfBuffer
      = pod_indirect_initializeBufferWithTakeOfBuffer;
    return;
  }
}

/***************************************************************************/
/*** Structs ***************************************************************/
/***************************************************************************/

/// Initialize the value witness table and struct field offset vector for a
/// struct, using the "Universal" layout strategy.
void swift::swift_initStructMetadata_UniversalStrategy(size_t numFields,
                                     const TypeLayout * const *fieldTypes,
                                     size_t *fieldOffsets,
                                     ValueWitnessTable *vwtable) {
  auto layout = BasicLayout::initialForValueType();
  performBasicLayout(layout, fieldTypes, numFields,
    [&](size_t i, const TypeLayout *fieldType, size_t offset) {
      assignUnlessEqual(fieldOffsets[i], offset);
    });

  vwtable->size = layout.size;
  vwtable->flags = layout.flags;
  vwtable->stride = layout.stride;
  
  // Substitute in better value witnesses if we have them.
  installCommonValueWitnesses(vwtable);

  // We have extra inhabitants if the first element does.
  // FIXME: generalize this.
  if (fieldTypes[0]->flags.hasExtraInhabitants()) {
    vwtable->flags = vwtable->flags.withExtraInhabitants(true);
    auto xiVWT = cast<ExtraInhabitantsValueWitnessTable>(vwtable);
    xiVWT->extraInhabitantFlags = fieldTypes[0]->getExtraInhabitantFlags();

    // The compiler should already have initialized these.
    assert(xiVWT->storeExtraInhabitant);
    assert(xiVWT->getExtraInhabitantIndex);
  }
}

/***************************************************************************/
/*** Classes ***************************************************************/
/***************************************************************************/

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
} // end anonymous namespace

#if SWIFT_OBJC_INTEROP
static uint32_t getLog2AlignmentFromMask(size_t alignMask) {
  assert(((alignMask + 1) & alignMask) == 0 &&
         "not an alignment mask!");

  uint32_t log2 = 0;
  while ((1 << log2) != (alignMask + 1))
    log2++;
  return log2;
}

static inline ClassROData *getROData(ClassMetadata *theClass) {
  return (ClassROData*) (theClass->Data & ~uintptr_t(1));
}

static void _swift_initGenericClassObjCName(ClassMetadata *theClass) {
  // Use the remangler to generate a mangled name from the type metadata.
  auto demangling = _swift_buildDemanglingForMetadata(theClass);

  // Remangle that into a new type mangling string.
  auto typeNode
    = Demangle::NodeFactory::create(Demangle::Node::Kind::TypeMangling);
  typeNode->addChild(demangling);
  auto globalNode
    = Demangle::NodeFactory::create(Demangle::Node::Kind::Global);
  globalNode->addChild(typeNode);
  
  auto string = Demangle::mangleNode(globalNode);
  
  auto fullNameBuf = (char*)swift_slowAlloc(string.size() + 1, 0);
  memcpy(fullNameBuf, string.c_str(), string.size() + 1);

  auto theMetaclass = (ClassMetadata *)object_getClass((id)theClass);

  getROData(theClass)->Name = fullNameBuf;
  getROData(theMetaclass)->Name = fullNameBuf;
}
#endif

/// Initialize the invariant superclass components of a class metadata,
/// such as the generic type arguments, field offsets, and so on.
///
/// This may also relocate the metadata object if it wasn't allocated
/// with enough space.
static ClassMetadata *_swift_initializeSuperclass(ClassMetadata *theClass,
                                                  bool copyFieldOffsetVectors) {
#if SWIFT_OBJC_INTEROP
  // If the class is generic, we need to give it a name for Objective-C.
  if (theClass->getDescription()->GenericParams.isGeneric())
    _swift_initGenericClassObjCName(theClass);
#endif

  const ClassMetadata *theSuperclass = theClass->SuperClass;
  if (theSuperclass == nullptr)
    return theClass;

  // Relocate the metadata if necessary.
  //
  // For now, we assume that relocation is only required when the parent
  // class has prefix matter we didn't know about.  This isn't consistent
  // with general class resilience, however.
  if (theSuperclass->isTypeMetadata()) {
    auto superAP = theSuperclass->getClassAddressPoint();
    auto oldClassAP = theClass->getClassAddressPoint();
    if (superAP > oldClassAP) {
      size_t extraPrefixSize = superAP - oldClassAP;
      size_t oldClassSize = theClass->getClassSize();

      // Allocate a new metadata object.
      auto rawNewClass = (char*) malloc(extraPrefixSize + oldClassSize);
      auto rawOldClass = (const char*) theClass;
      auto rawSuperclass = (const char*) theSuperclass;

      // Copy the extra prefix from the superclass.
      memcpy((void**) (rawNewClass),
             (void* const *) (rawSuperclass - superAP),
             extraPrefixSize);
      // Copy the rest of the data from the derived class.
      memcpy((void**) (rawNewClass + extraPrefixSize),
             (void* const *) (rawOldClass - oldClassAP),
             oldClassSize);

      // Update the class extents on the new metadata object.
      theClass = reinterpret_cast<ClassMetadata*>(rawNewClass + oldClassAP);
      theClass->setClassAddressPoint(superAP);
      theClass->setClassSize(extraPrefixSize + oldClassSize);

      // The previous metadata should be global data, so we have no real
      // choice but to drop it on the floor.
    }
  }

  // If any ancestor classes have generic parameters or field offset
  // vectors, inherit them.
  auto ancestor = theSuperclass;
  auto *classWords = reinterpret_cast<uintptr_t *>(theClass);
  auto *superWords = reinterpret_cast<const uintptr_t *>(theSuperclass);
  while (ancestor && ancestor->isTypeMetadata()) {
    auto &description = ancestor->getDescription();
    auto &genericParams = description->GenericParams;

    // Copy the parent type.
    if (genericParams.Flags.hasParent()) {
      memcpy(classWords + genericParams.Offset - 1,
             superWords + genericParams.Offset - 1,
             sizeof(uintptr_t));
    }

    // Copy the generic requirements.
    if (genericParams.hasGenericRequirements()) {
      unsigned numParamWords = genericParams.NumGenericRequirements;
      memcpy(classWords + genericParams.Offset,
             superWords + genericParams.Offset,
             numParamWords * sizeof(uintptr_t));
    }

    // Copy the field offsets.
    if (copyFieldOffsetVectors &&
        description->Class.hasFieldOffsetVector()) {
      unsigned fieldOffsetVector = description->Class.FieldOffsetVectorOffset;
      memcpy(classWords + fieldOffsetVector,
             superWords + fieldOffsetVector,
             description->Class.NumFields * sizeof(uintptr_t));
    }
    ancestor = ancestor->SuperClass;
  }

#if SWIFT_OBJC_INTEROP
  // Set up the superclass of the metaclass, which is the metaclass of the
  // superclass.
  auto theMetaclass = (ClassMetadata *)object_getClass((id)theClass);
  auto theSuperMetaclass
    = (const ClassMetadata *)object_getClass((id)theSuperclass);
  theMetaclass->SuperClass = theSuperMetaclass;
#endif

  return theClass;
}

#if SWIFT_OBJC_INTEROP
static MetadataAllocator &getResilientMetadataAllocator() {
  // This should be constant-initialized, but this is safe.
  static MetadataAllocator allocator;
  return allocator;
}
#endif

/// Initialize the field offset vector for a dependent-layout class, using the
/// "Universal" layout strategy.
ClassMetadata *
swift::swift_initClassMetadata_UniversalStrategy(ClassMetadata *self,
                                                 size_t numFields,
                                           const ClassFieldLayout *fieldLayouts,
                                                 size_t *fieldOffsets) {
  self = _swift_initializeSuperclass(self, /*copyFieldOffsetVectors=*/true);

  // Start layout by appending to a standard heap object header.
  size_t size, alignMask;

#if SWIFT_OBJC_INTEROP
  ClassROData *rodata = getROData(self);
#endif

  // If we have a superclass, start from its size and alignment instead.
  if (classHasSuperclass(self)) {
    const ClassMetadata *super = self->SuperClass;

    // This is straightforward if the superclass is Swift.
#if SWIFT_OBJC_INTEROP
    if (super->isTypeMetadata()) {
#endif
      size = super->getInstanceSize();
      alignMask = super->getInstanceAlignMask();

#if SWIFT_OBJC_INTEROP
    // If it's Objective-C, start layout from our static notion of
    // where the superclass starts.  Objective-C expects us to have
    // generated a correct ivar layout, which it will simply slide if
    // it needs to.
    } else {
      size = rodata->InstanceStart;
      alignMask = 0xF; // malloc alignment guarantee
    }
#endif

  // If we don't have a formal superclass, start with the basic heap header.
  } else {
    auto heapLayout = BasicLayout::initialForHeapObject();
    size = heapLayout.size;
    alignMask = heapLayout.flags.getAlignmentMask();
  }

#if SWIFT_OBJC_INTEROP
  // In ObjC interop mode, we have up to two places we need each correct
  // ivar offset to end up:
  //
  // - the global ivar offset in the RO-data; this should only exist
  //   if the class layout (up to this ivar) is not actually dependent
  //
  // - the field offset vector (fieldOffsets)
  //
  // When we ask the ObjC runtime to lay out this class, we need the
  // RO-data to point to the field offset vector, even if the layout
  // is not dependent.  The RO-data is not shared between
  // instantiations, but the global ivar offset is (by definition).
  // If the compiler didn't have the correct static size for the
  // superclass (i.e. if rodata->InstanceStart is wrong), a previous
  // instantiation might have already slid the global offset to the
  // correct place; we need the ObjC runtime to see a pre-slid value,
  // and it's not safe to briefly unslide it and let the runtime slide
  // it back because there might already be concurrent code relying on
  // the global ivar offset.
  //
  // So we need to the remember the addresses of the global ivar offsets.
  // We use this lazily-filled SmallVector to do so.
  const unsigned NumInlineGlobalIvarOffsets = 8;
  size_t *_inlineGlobalIvarOffsets[NumInlineGlobalIvarOffsets];
  size_t **_globalIvarOffsets = nullptr;
  auto getGlobalIvarOffsets = [&]() -> size_t** {
    if (!_globalIvarOffsets) {
      if (numFields <= NumInlineGlobalIvarOffsets) {
        _globalIvarOffsets = _inlineGlobalIvarOffsets;
      } else {
        _globalIvarOffsets = new size_t*[numFields];
      }

      // Make sure all the entries start out null.
      memset(_globalIvarOffsets, 0, sizeof(size_t*) * numFields);
    }
    return _globalIvarOffsets;
  };

  // Ensure that Objective-C does layout starting from the right
  // offset.  This needs to exactly match the superclass rodata's
  // InstanceSize in cases where the compiler decided that we didn't
  // really have a resilient ObjC superclass, because the compiler
  // might hardcode offsets in that case, so we can't slide ivars.
  // Fortunately, the cases where that happens are exactly the
  // situations where our entire superclass hierarchy is defined
  // in Swift.  (But note that ObjC might think we have a superclass
  // even if Swift doesn't, because of SwiftObject.)
  rodata->InstanceStart = size;

  auto genericPattern = self->getDescription()->getGenericMetadataPattern();
  auto &allocator =
    genericPattern ? unsafeGetInitializedCache(genericPattern).getAllocator()
                   : getResilientMetadataAllocator();

  // Always clone the ivar descriptors.
  if (numFields) {
    const ClassIvarList *dependentIvars = rodata->IvarList;
    assert(dependentIvars->Count == numFields);
    assert(dependentIvars->EntrySize == sizeof(ClassIvarEntry));

    auto ivarListSize = sizeof(ClassIvarList) +
                        numFields * sizeof(ClassIvarEntry);
    auto ivars = (ClassIvarList*) allocator.Allocate(ivarListSize,
                                                     alignof(ClassIvarList));
    memcpy(ivars, dependentIvars, ivarListSize);
    rodata->IvarList = ivars;

    for (unsigned i = 0; i != numFields; ++i) {
      ClassIvarEntry &ivar = ivars->getIvars()[i];

      // Remember the global ivar offset if present.
      if (ivar.Offset) {
        getGlobalIvarOffsets()[i] = ivar.Offset;
      }

      // Change the ivar offset to point to the respective entry of
      // the field-offset vector, as discussed above.
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
#endif

  // Okay, now do layout.
  for (unsigned i = 0; i != numFields; ++i) {
    // Skip empty fields.
    if (fieldOffsets[i] == 0 && fieldLayouts[i].Size == 0)
      continue;
    auto offset = roundUpToAlignMask(size, fieldLayouts[i].AlignMask);
    fieldOffsets[i] = offset;
    size = offset + fieldLayouts[i].Size;
    alignMask = std::max(alignMask, fieldLayouts[i].AlignMask);
  }

  // Save the final size and alignment into the metadata record.
  assert(self->isTypeMetadata());
  self->setInstanceSize(size);
  self->setInstanceAlignMask(alignMask);

#if SWIFT_OBJC_INTEROP
  // Save the size into the Objective-C metadata as well.
  rodata->InstanceSize = size;

  // Register this class with the runtime.  This will also cause the
  // runtime to lay us out.
  swift_instantiateObjCClass(self);

  // If we saved any global ivar offsets, make sure we write back to them.
  if (_globalIvarOffsets) {
    for (unsigned i = 0; i != numFields; ++i) {
      if (!_globalIvarOffsets[i]) continue;

      // To avoid dirtying memory, only write to the global ivar
      // offset if it's actually wrong.
      if (*_globalIvarOffsets[i] != fieldOffsets[i])
        *_globalIvarOffsets[i] = fieldOffsets[i];
    }

    // Free the out-of-line if we allocated one.
    if (_globalIvarOffsets != _inlineGlobalIvarOffsets) {
      delete [] _globalIvarOffsets;
    }
  }
#endif

  return self;
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

/***************************************************************************/
/*** Metatypes *************************************************************/
/***************************************************************************/

/// \brief Find the appropriate value witness table for the given type.
static const ValueWitnessTable *
getMetatypeValueWitnesses(const Metadata *instanceType) {
  // When metatypes are accessed opaquely, they always have a "thick"
  // representation.
  return &getUnmanagedPointerPointerValueWitnesses();
}

namespace {
  class MetatypeCacheEntry {
  public:
    FullMetadata<MetatypeMetadata> Data;

    MetatypeCacheEntry(const Metadata *instanceType) {
      Data.setKind(MetadataKind::Metatype);
      Data.ValueWitnesses = getMetatypeValueWitnesses(instanceType);
      Data.InstanceType = instanceType;
    }

    intptr_t getKeyIntValueForDump() {
      return reinterpret_cast<intptr_t>(Data.InstanceType);
    }

    int compareWithKey(const Metadata *instanceType) const {
      return comparePointers(instanceType, Data.InstanceType);
    }

    static size_t getExtraAllocationSize(const Metadata *instanceType) {
      return 0;
    }
    size_t getExtraAllocationSize() const {
      return 0;
    }
  };
} // end anonymous namespace

/// The uniquing structure for metatype type metadata.
static SimpleGlobalCache<MetatypeCacheEntry> MetatypeTypes;

/// \brief Fetch a uniqued metadata for a metatype type.
SWIFT_RUNTIME_EXPORT
const MetatypeMetadata *
swift::swift_getMetatypeMetadata(const Metadata *instanceMetadata) {
  return &MetatypeTypes.getOrInsert(instanceMetadata).first->Data;
}

/***************************************************************************/
/*** Existential Metatypes *************************************************/
/***************************************************************************/

namespace {

/// A cache entry for existential metatype witness tables.
class ExistentialMetatypeValueWitnessTableCacheEntry {
public:
  ExtraInhabitantsValueWitnessTable Data;

  unsigned getNumWitnessTables() const {
    return (Data.size - sizeof(ExistentialMetatypeContainer))
              / sizeof(const ValueWitnessTable*);
  }

  ExistentialMetatypeValueWitnessTableCacheEntry(unsigned numWitnessTables);

  intptr_t getKeyIntValueForDump() {
    return static_cast<intptr_t>(getNumWitnessTables());
  }

  int compareWithKey(unsigned key) const {
    return compareIntegers(key, getNumWitnessTables());
  }

  static size_t getExtraAllocationSize(unsigned numTables) {
    return 0;
  }
  size_t getExtraAllocationSize() const {
    return 0;
  }
};

class ExistentialMetatypeCacheEntry {
public:
  FullMetadata<ExistentialMetatypeMetadata> Data;

  ExistentialMetatypeCacheEntry(const Metadata *instanceMetadata);

  intptr_t getKeyIntValueForDump() {
    return reinterpret_cast<intptr_t>(Data.InstanceType);
  }

  int compareWithKey(const Metadata *instanceType) const {
    return comparePointers(instanceType, Data.InstanceType);
  }

  static size_t getExtraAllocationSize(const Metadata *key) {
    return 0;
  }
  size_t getExtraAllocationSize() const {
    return 0;
  }
};

} // end anonymous namespace

/// The uniquing structure for existential metatype value witness tables.
static SimpleGlobalCache<ExistentialMetatypeValueWitnessTableCacheEntry>
ExistentialMetatypeValueWitnessTables;

/// The uniquing structure for existential metatype type metadata.
static SimpleGlobalCache<ExistentialMetatypeCacheEntry> ExistentialMetatypes;

static const ExtraInhabitantsValueWitnessTable
ExistentialMetatypeValueWitnesses_1 =
  ValueWitnessTableForBox<ExistentialMetatypeBox<1>>::table;
static const ExtraInhabitantsValueWitnessTable
ExistentialMetatypeValueWitnesses_2 =
  ValueWitnessTableForBox<ExistentialMetatypeBox<2>>::table;

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

  return &ExistentialMetatypeValueWitnessTables.getOrInsert(numWitnessTables)
                                               .first->Data;
}

ExistentialMetatypeValueWitnessTableCacheEntry::
ExistentialMetatypeValueWitnessTableCacheEntry(unsigned numWitnessTables) {
  using Box = NonFixedExistentialMetatypeBox;
  using Witnesses = NonFixedValueWitnesses<Box, /*known allocated*/ true>;

#define STORE_VAR_EXISTENTIAL_METATYPE_WITNESS(WITNESS) \
  Data.WITNESS = Witnesses::WITNESS;
  FOR_ALL_FUNCTION_VALUE_WITNESSES(STORE_VAR_EXISTENTIAL_METATYPE_WITNESS)
  STORE_VAR_EXISTENTIAL_METATYPE_WITNESS(storeExtraInhabitant)
  STORE_VAR_EXISTENTIAL_METATYPE_WITNESS(getExtraInhabitantIndex)
#undef STORE_VAR_EXISTENTIAL_METATYPE_WITNESS

  Data.size = Box::Container::getSize(numWitnessTables);
  Data.flags = ValueWitnessFlags()
    .withAlignment(Box::Container::getAlignment(numWitnessTables))
    .withPOD(true)
    .withBitwiseTakable(true)
    .withInlineStorage(false)
    .withExtraInhabitants(true);
  Data.stride = Box::Container::getStride(numWitnessTables);
  Data.extraInhabitantFlags = ExtraInhabitantFlags()
    .withNumExtraInhabitants(Witnesses::numExtraInhabitants);

  assert(getNumWitnessTables() == numWitnessTables);
}

/// \brief Fetch a uniqued metadata for a metatype type.
SWIFT_RUNTIME_EXPORT
const ExistentialMetatypeMetadata *
swift::swift_getExistentialMetatypeMetadata(const Metadata *instanceMetadata) {
  return &ExistentialMetatypes.getOrInsert(instanceMetadata).first->Data;
}

ExistentialMetatypeCacheEntry::ExistentialMetatypeCacheEntry(
                                            const Metadata *instanceMetadata) {
  ExistentialTypeFlags flags;
  if (instanceMetadata->getKind() == MetadataKind::Existential) {
    flags = static_cast<const ExistentialTypeMetadata*>(instanceMetadata)
      ->Flags;
  } else {
    assert(instanceMetadata->getKind() == MetadataKind::ExistentialMetatype);
    flags = static_cast<const ExistentialMetatypeMetadata*>(instanceMetadata)
      ->Flags;
  }

  Data.setKind(MetadataKind::ExistentialMetatype);
  Data.ValueWitnesses =
    getExistentialMetatypeValueWitnesses(flags.getNumWitnessTables());
  Data.InstanceType = instanceMetadata;
  Data.Flags = flags;
}

/***************************************************************************/
/*** Existential types *****************************************************/
/***************************************************************************/

namespace {

class ExistentialCacheEntry {
public:
  FullMetadata<ExistentialTypeMetadata> Data;

  struct Key {
    size_t NumProtocols;
    const ProtocolDescriptor * const *Protocols;
  };

  ExistentialCacheEntry(Key key);

  intptr_t getKeyIntValueForDump() {
    return 0;
  }

  int compareWithKey(Key key) const {
    if (auto result = compareIntegers(key.NumProtocols,
                                      Data.Protocols.NumProtocols))
      return result;

    for (size_t i = 0; i != key.NumProtocols; ++i) {
      if (auto result = comparePointers(key.Protocols[i], Data.Protocols[i]))
        return result;
    }

    return 0;
  }

  static size_t getExtraAllocationSize(Key key) {
    return sizeof(const ProtocolDescriptor *) * key.NumProtocols;
  }
  size_t getExtraAllocationSize() const {
    return sizeof(const ProtocolDescriptor *) * Data.Protocols.NumProtocols;
  }
};

class OpaqueExistentialValueWitnessTableCacheEntry {
public:
  ValueWitnessTable Data;

  OpaqueExistentialValueWitnessTableCacheEntry(unsigned numTables);

  unsigned getNumWitnessTables() const {
    return (Data.size - sizeof(OpaqueExistentialContainer))
              / sizeof(const WitnessTable *);
  }

  intptr_t getKeyIntValueForDump() {
    return getNumWitnessTables();
  }

  int compareWithKey(unsigned key) const {
    return compareIntegers(key, getNumWitnessTables());
  }

  static size_t getExtraAllocationSize(unsigned numTables) {
    return 0;
  }
  size_t getExtraAllocationSize() const {
    return 0;
  }
};

class ClassExistentialValueWitnessTableCacheEntry {
public:
  ExtraInhabitantsValueWitnessTable Data;

  ClassExistentialValueWitnessTableCacheEntry(unsigned numTables);

  unsigned getNumWitnessTables() const {
    return (Data.size - sizeof(ClassExistentialContainer))
              / sizeof(const WitnessTable *);
  }

  intptr_t getKeyIntValueForDump() {
    return getNumWitnessTables();
  }

  int compareWithKey(unsigned key) const {
    return compareIntegers(key, getNumWitnessTables());
  }

  static size_t getExtraAllocationSize(unsigned numTables) {
    return 0;
  }
  size_t getExtraAllocationSize() const {
    return 0;
  }
};

} // end anonymous namespace

/// The uniquing structure for existential type metadata.
static SimpleGlobalCache<ExistentialCacheEntry> ExistentialTypes;

static const ValueWitnessTable OpaqueExistentialValueWitnesses_0 =
  ValueWitnessTableForBox<OpaqueExistentialBox<0>>::table;
static const ValueWitnessTable OpaqueExistentialValueWitnesses_1 =
  ValueWitnessTableForBox<OpaqueExistentialBox<1>>::table;

/// The uniquing structure for opaque existential value witness tables.
static SimpleGlobalCache<OpaqueExistentialValueWitnessTableCacheEntry>
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

  return &OpaqueExistentialValueWitnessTables.getOrInsert(numWitnessTables)
                                             .first->Data;
}

OpaqueExistentialValueWitnessTableCacheEntry::
OpaqueExistentialValueWitnessTableCacheEntry(unsigned numWitnessTables) {
  using Box = NonFixedOpaqueExistentialBox;
  using Witnesses = NonFixedValueWitnesses<Box, /*known allocated*/ true>;
  static_assert(!Witnesses::hasExtraInhabitants, "no extra inhabitants");

#define STORE_VAR_OPAQUE_EXISTENTIAL_WITNESS(WITNESS) \
  Data.WITNESS = Witnesses::WITNESS;
  FOR_ALL_FUNCTION_VALUE_WITNESSES(STORE_VAR_OPAQUE_EXISTENTIAL_WITNESS)
#undef STORE_VAR_OPAQUE_EXISTENTIAL_WITNESS

  Data.size = Box::Container::getSize(numWitnessTables);
  Data.flags = ValueWitnessFlags()
    .withAlignment(Box::Container::getAlignment(numWitnessTables))
    .withPOD(false)
    .withBitwiseTakable(false)
    .withInlineStorage(false)
    .withExtraInhabitants(false);
  Data.stride = Box::Container::getStride(numWitnessTables);

  assert(getNumWitnessTables() == numWitnessTables);
}

static const ExtraInhabitantsValueWitnessTable ClassExistentialValueWitnesses_1 =
  ValueWitnessTableForBox<ClassExistentialBox<1>>::table;
static const ExtraInhabitantsValueWitnessTable ClassExistentialValueWitnesses_2 =
  ValueWitnessTableForBox<ClassExistentialBox<2>>::table;

/// The uniquing structure for class existential value witness tables.
static SimpleGlobalCache<ClassExistentialValueWitnessTableCacheEntry>
ClassExistentialValueWitnessTables;

/// Instantiate a value witness table for a class-constrained existential
/// container with the given number of witness table pointers.
static const ExtraInhabitantsValueWitnessTable *
getClassExistentialValueWitnesses(unsigned numWitnessTables) {
  if (numWitnessTables == 0) {
#if SWIFT_OBJC_INTEROP
    return &VALUE_WITNESS_SYM(BO);
#else
    return &VALUE_WITNESS_SYM(Bo);
#endif
  }
  if (numWitnessTables == 1)
    return &ClassExistentialValueWitnesses_1;
  if (numWitnessTables == 2)
    return &ClassExistentialValueWitnesses_2;

  static_assert(3 * sizeof(void*) >= sizeof(ValueBuffer),
                "not handling all possible inline-storage class existentials!");

  return &ClassExistentialValueWitnessTables.getOrInsert(numWitnessTables)
                                            .first->Data;
}

ClassExistentialValueWitnessTableCacheEntry::
ClassExistentialValueWitnessTableCacheEntry(unsigned numWitnessTables) {
  using Box = NonFixedClassExistentialBox;
  using Witnesses = NonFixedValueWitnesses<Box, /*known allocated*/ true>;

#define STORE_VAR_CLASS_EXISTENTIAL_WITNESS(WITNESS) \
  Data.WITNESS = Witnesses::WITNESS;
  FOR_ALL_FUNCTION_VALUE_WITNESSES(STORE_VAR_CLASS_EXISTENTIAL_WITNESS)
  STORE_VAR_CLASS_EXISTENTIAL_WITNESS(storeExtraInhabitant)
  STORE_VAR_CLASS_EXISTENTIAL_WITNESS(getExtraInhabitantIndex)
#undef STORE_VAR_CLASS_EXISTENTIAL_WITNESS

  Data.size = Box::Container::getSize(numWitnessTables);
  Data.flags = ValueWitnessFlags()
    .withAlignment(Box::Container::getAlignment(numWitnessTables))
    .withPOD(false)
    .withBitwiseTakable(true)
    .withInlineStorage(false)
    .withExtraInhabitants(true);
  Data.stride = Box::Container::getStride(numWitnessTables);
  Data.extraInhabitantFlags = ExtraInhabitantFlags()
    .withNumExtraInhabitants(Witnesses::numExtraInhabitants);

  assert(getNumWitnessTables() == numWitnessTables);
}

/// Get the value witness table for an existential type, first trying to use a
/// shared specialized table for common cases.
static const ValueWitnessTable *
getExistentialValueWitnesses(ProtocolClassConstraint classConstraint,
                             unsigned numWitnessTables,
                             SpecialProtocol special) {
  // Use special representation for special protocols.
  switch (special) {
  case SpecialProtocol::Error:
#if SWIFT_OBJC_INTEROP
    // Error always has a single-ObjC-refcounted representation.
    return &VALUE_WITNESS_SYM(BO);
#else
    // Without ObjC interop, Error is native-refcounted.
    return &VALUE_WITNESS_SYM(Bo);
#endif
      
  // Other existentials use standard representation.
  case SpecialProtocol::AnyObject:
  case SpecialProtocol::None:
    break;
  }
  
  switch (classConstraint) {
  case ProtocolClassConstraint::Class:
    return getClassExistentialValueWitnesses(numWitnessTables);
  case ProtocolClassConstraint::Any:
    return getOpaqueExistentialValueWitnesses(numWitnessTables);
  }

  swift_runtime_unreachable("Unhandled ProtocolClassConstraint in switch.");
}

template<> ExistentialTypeRepresentation
ExistentialTypeMetadata::getRepresentation() const {
  // Some existentials use special containers.
  switch (Flags.getSpecialProtocol()) {
  case SpecialProtocol::Error:
    return ExistentialTypeRepresentation::Error;
  case SpecialProtocol::AnyObject:
  case SpecialProtocol::None:
    break;
  }
  // The layout of standard containers depends on whether the existential is
  // class-constrained.
  if (isClassBounded())
    return ExistentialTypeRepresentation::Class;
  return ExistentialTypeRepresentation::Opaque;
}

template<> bool
ExistentialTypeMetadata::mayTakeValue(const OpaqueValue *container) const {
  switch (getRepresentation()) {
  // Owning a reference to a class existential is equivalent to owning a
  // reference to the contained class instance.
  case ExistentialTypeRepresentation::Class:
    return true;
  // Opaque existential containers uniquely own their contained value.
  case ExistentialTypeRepresentation::Opaque:
    return true;
    
  // References to boxed existential containers may be shared.
  case ExistentialTypeRepresentation::Error: {
    // We can only take the value if the box is a bridged NSError, in which case
    // owning a reference to the box is owning a reference to the NSError.
    // TODO: Or if the box is uniquely referenced. We don't have intimate
    // enough knowledge of CF refcounting to check for that dynamically yet.
    const SwiftError *errorBox
      = *reinterpret_cast<const SwiftError * const *>(container);
    return errorBox->isPureNSError();
  }
  }

  swift_runtime_unreachable(
      "Unhandled ExistentialTypeRepresentation in switch.");
}

template<> void
ExistentialTypeMetadata::deinitExistentialContainer(OpaqueValue *container)
const {
  switch (getRepresentation()) {
  case ExistentialTypeRepresentation::Class:
    // Nothing to clean up after taking the class reference.
    break;
  
  case ExistentialTypeRepresentation::Opaque: {
    // Containing the value may require a side allocation, which we need
    // to clean up.
    auto opaque = reinterpret_cast<OpaqueExistentialContainer *>(container);
    opaque->Type->vw_deallocateBuffer(&opaque->Buffer);
    break;
  }
  
  case ExistentialTypeRepresentation::Error:
    // TODO: If we were able to claim the value from a uniquely-owned
    // existential box, we would want to deallocError here.
    break;
  }
}

template<> const OpaqueValue *
ExistentialTypeMetadata::projectValue(const OpaqueValue *container) const {
  switch (getRepresentation()) {
  case ExistentialTypeRepresentation::Class: {
    auto classContainer =
      reinterpret_cast<const ClassExistentialContainer*>(container);
    return reinterpret_cast<const OpaqueValue *>(&classContainer->Value);
  }
  case ExistentialTypeRepresentation::Opaque: {
    auto opaqueContainer =
      reinterpret_cast<const OpaqueExistentialContainer*>(container);
    return opaqueContainer->Type->vw_projectBuffer(
                         const_cast<ValueBuffer*>(&opaqueContainer->Buffer));
  }
  case ExistentialTypeRepresentation::Error: {
    const SwiftError *errorBox
      = *reinterpret_cast<const SwiftError * const *>(container);
    // If the error is a bridged NSError, then the "box" is in fact itself
    // the value.
    if (errorBox->isPureNSError())
      return container;
    return errorBox->getValue();
  }
  }

  swift_runtime_unreachable(
      "Unhandled ExistentialTypeRepresentation in switch.");
}

template<> const Metadata *
ExistentialTypeMetadata::getDynamicType(const OpaqueValue *container) const {
  switch (getRepresentation()) {
  case ExistentialTypeRepresentation::Class: {
    auto classContainer =
      reinterpret_cast<const ClassExistentialContainer*>(container);
    void *obj = classContainer->Value;
    return swift_getObjectType(reinterpret_cast<HeapObject*>(obj));
  }
  case ExistentialTypeRepresentation::Opaque: {
    auto opaqueContainer =
      reinterpret_cast<const OpaqueExistentialContainer*>(container);
    return opaqueContainer->Type;
  }
  case ExistentialTypeRepresentation::Error: {
    const SwiftError *errorBox
      = *reinterpret_cast<const SwiftError * const *>(container);
    return errorBox->getType();
  }
  }

  swift_runtime_unreachable(
      "Unhandled ExistentialTypeRepresentation in switch.");
}

template<> const WitnessTable *
ExistentialTypeMetadata::getWitnessTable(const OpaqueValue *container,
                                         unsigned i) const {
  assert(i < Flags.getNumWitnessTables());

  // The layout of the container depends on whether it's class-constrained
  // or a special protocol.
  const WitnessTable * const *witnessTables;
  
  switch (getRepresentation()) {
  case ExistentialTypeRepresentation::Class: {
    auto classContainer =
      reinterpret_cast<const ClassExistentialContainer*>(container);
    witnessTables = classContainer->getWitnessTables();
    break;
  }
  case ExistentialTypeRepresentation::Opaque: {
    auto opaqueContainer =
      reinterpret_cast<const OpaqueExistentialContainer*>(container);
    witnessTables = opaqueContainer->getWitnessTables();
    break;
  }
  case ExistentialTypeRepresentation::Error: {
    // Only one witness table we should be able to return, which is the
    // Error.
    assert(i == 0 && "only one witness table in an Error box");
    const SwiftError *errorBox
      = *reinterpret_cast<const SwiftError * const *>(container);
    return errorBox->getErrorConformance();
  }
  }

  // The return type here describes extra structure for the protocol
  // witness table for some reason.  We should probably have a nominal
  // type for these, just for type safety reasons.
  return witnessTables[i];
}

/// \brief Fetch a uniqued metadata for an existential type. The array
/// referenced by \c protocols will be sorted in-place.
const ExistentialTypeMetadata *
swift::swift_getExistentialTypeMetadata(size_t numProtocols,
                                        const ProtocolDescriptor **protocols)
    SWIFT_CC(RegisterPreservingCC_IMPL) {

  // Sort the protocol set.
  std::sort(protocols, protocols + numProtocols);

  ExistentialCacheEntry::Key key = { numProtocols, protocols };
  return &ExistentialTypes.getOrInsert(key).first->Data;
}

ExistentialCacheEntry::ExistentialCacheEntry(Key key) {
  // Calculate the class constraint and number of witness tables for the
  // protocol set.
  unsigned numWitnessTables = 0;
  ProtocolClassConstraint classConstraint = ProtocolClassConstraint::Any;
  for (auto p : make_range(key.Protocols, key.Protocols + key.NumProtocols)) {
    if (p->Flags.needsWitnessTable()) {
      ++numWitnessTables;
    }
    if (p->Flags.getClassConstraint() == ProtocolClassConstraint::Class)
      classConstraint = ProtocolClassConstraint::Class;
  }

  // Get the special protocol kind for an uncomposed protocol existential.
  // Protocol compositions are currently never special.
  auto special = SpecialProtocol::None;
  if (key.NumProtocols == 1)
    special = key.Protocols[0]->Flags.getSpecialProtocol();
      
  Data.setKind(MetadataKind::Existential);
  Data.ValueWitnesses = getExistentialValueWitnesses(classConstraint,
                                                     numWitnessTables,
                                                     special);
  Data.Flags = ExistentialTypeFlags()
    .withNumWitnessTables(numWitnessTables)
    .withClassConstraint(classConstraint)
    .withSpecialProtocol(special);
  Data.Protocols.NumProtocols = key.NumProtocols;
  for (size_t i = 0; i < key.NumProtocols; ++i)
    Data.Protocols[i] = key.Protocols[i];
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

/***************************************************************************/
/*** Foreign types *********************************************************/
/***************************************************************************/

namespace {
  /// A string whose data is globally-allocated.
  struct GlobalString {
    StringRef Data;
    /*implicit*/ GlobalString(StringRef data) : Data(data) {}
  };
} // end anonymous namespace

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
namespace {
struct ForeignTypeState {
  Mutex Lock;
  ConditionVariable InitializationWaiters;
  llvm::DenseMap<GlobalString, const ForeignTypeMetadata *> Types;
};
} // end anonymous namespace

static Lazy<ForeignTypeState> ForeignTypes;

const ForeignTypeMetadata *
swift::swift_getForeignTypeMetadata(ForeignTypeMetadata *nonUnique) {
  // Fast path: check the invasive cache.
  if (auto unique = nonUnique->getCachedUniqueMetadata()) {
    return unique;
  }

  // Okay, check the global map.
  auto &foreignTypes = ForeignTypes.get();
  GlobalString key(nonUnique->getName());
  bool hasInit = nonUnique->hasInitializationFunction();

  const ForeignTypeMetadata *uniqueMetadata;
  bool inserted;

  // A helper function to find the current entry for the key using the
  // saved iterator if it's still valid.  This should only be called
  // while the lock is held.
  decltype(foreignTypes.Types.begin()) savedIterator;
  size_t savedSize = 0;
  auto getCurrentEntry = [&]() -> const ForeignTypeMetadata *& {
    // The iterator may have been invalidated if the size of the map
    // has changed since the last lookup.
    if (foreignTypes.Types.size() != savedSize) {
      savedSize = foreignTypes.Types.size();
      savedIterator = foreignTypes.Types.find(key);
      assert(savedIterator != foreignTypes.Types.end() &&
             "entries cannot be removed from foreign types metadata map");
    }
    return savedIterator->second;
  };

  {
    ScopedLock guard(foreignTypes.Lock);

    // Try to create an entry in the map.  The initial value of the entry
    // is our copy of the metadata unless it has an initialization function,
    // in which case we have to insert null as a placeholder to tell others
    // to wait while we call the initializer.
    auto valueToInsert = (hasInit ? nullptr : nonUnique);
    auto insertResult = foreignTypes.Types.insert({key, valueToInsert});
    inserted = insertResult.second;
    savedIterator = insertResult.first;
    savedSize = foreignTypes.Types.size();
    uniqueMetadata = savedIterator->second;

    // If we created the entry, then the unique metadata is our copy.
    if (inserted) {
      uniqueMetadata = nonUnique;

    // If we didn't create the entry, but it's null, then we have to wait
    // until it becomes non-null.
    } else {
      while (uniqueMetadata == nullptr) {
        foreignTypes.Lock.wait(foreignTypes.InitializationWaiters);
        uniqueMetadata = getCurrentEntry();
      }
    }
  }

  // If we inserted the entry and there's an initialization function,
  // call it.  This has to be done with the lock dropped.
  if (inserted && hasInit) {
    nonUnique->getInitializationFunction()(nonUnique);

    // Update the cache entry:

    //   - Reacquire the lock.
    ScopedLock guard(foreignTypes.Lock);

    //   - Change the entry.
    auto &entry = getCurrentEntry();
    assert(entry == nullptr);
    entry = nonUnique;

    //   - Notify waiters.
    foreignTypes.InitializationWaiters.notifyAll();
  }

  // Remember the unique result in the invasive cache.  We don't want
  // to do this until after the initialization completes; otherwise,
  // it will be possible for code to fast-path through this function
  // too soon.
  nonUnique->setCachedUniqueMetadata(uniqueMetadata);

  return uniqueMetadata;
}

/***************************************************************************/
/*** Other metadata routines ***********************************************/
/***************************************************************************/

template<> const GenericMetadata *
Metadata::getGenericPattern() const {
  auto &ntd = getNominalTypeDescriptor();
  if (!ntd)
    return nullptr;
  return ntd->getGenericMetadataPattern();
}

template<> const ClassMetadata *
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
  case MetadataKind::Optional:
  case MetadataKind::ForeignClass:
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::Existential:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Metatype:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
    return nullptr;
  }

  swift_runtime_unreachable("Unhandled MetadataKind in switch.");
}

#ifndef NDEBUG
SWIFT_RUNTIME_EXPORT
void _swift_debug_verifyTypeLayoutAttribute(Metadata *type,
                                            const void *runtimeValue,
                                            const void *staticValue,
                                            size_t size,
                                            const char *description) {
  auto presentValue = [&](const void *value) {
    if (size < sizeof(long long)) {
      long long intValue = 0;
      memcpy(&intValue, value, size);
      fprintf(stderr, "%lld (%#llx)\n                  ", intValue, intValue);
    }
    auto bytes = reinterpret_cast<const uint8_t *>(value);
    for (unsigned i = 0; i < size; ++i) {
      fprintf(stderr, "%02x ", bytes[i]);
    }
    fprintf(stderr, "\n");
  };
  
  if (memcmp(runtimeValue, staticValue, size) != 0) {
    auto typeName = nameForMetadata(type);
    fprintf(stderr, "*** Type verification of %s %s failed ***\n",
            typeName.c_str(), description);
    
    fprintf(stderr, "  runtime value:  ");
    presentValue(runtimeValue);
    fprintf(stderr, "  compiler value: ");
    presentValue(staticValue);
  }
}
#endif

/***************************************************************************/
/*** Protocol witness tables ***********************************************/
/***************************************************************************/

namespace {
  class WitnessTableCacheEntry : public CacheEntry<WitnessTableCacheEntry> {
  public:
    static const char *getName() { return "WitnessTableCache"; }

    WitnessTableCacheEntry(size_t numArguments) {
      assert(numArguments == getNumArguments());
    }

    static constexpr size_t getNumArguments() {
      return 1;
    }

    /// Advance the address point to the end of the private storage area.
    WitnessTable *get(GenericWitnessTable *genericTable) const {
      return reinterpret_cast<WitnessTable *>(
          const_cast<void **>(getData<void *>()) +
          genericTable->WitnessTablePrivateSizeInWords);
    }
  };
} // end anonymous namespace

using GenericWitnessTableCache = MetadataCache<WitnessTableCacheEntry>;
using LazyGenericWitnessTableCache = Lazy<GenericWitnessTableCache>;

/// Fetch the cache for a generic witness-table structure.
static GenericWitnessTableCache &getCache(GenericWitnessTable *gen) {
  // Keep this assert even if you change the representation above.
  static_assert(sizeof(LazyGenericWitnessTableCache) <=
                sizeof(GenericWitnessTable::PrivateData),
                "metadata cache is larger than the allowed space");

  auto lazyCache =
    reinterpret_cast<LazyGenericWitnessTableCache*>(gen->PrivateData);
  return lazyCache->get();
}

/// If there's no initializer, no private storage, and all requirements
/// are present, we don't have to instantiate anything; just return the
/// witness table template.
///
/// Most of the time IRGen should be able to determine this statically;
/// the one case is with resilient conformances, where the resilient
/// protocol has not yet changed in a way that's incompatible with the
/// conformance.
static bool doesNotRequireInstantiation(GenericWitnessTable *genericTable) {
  if (genericTable->Instantiator.isNull() &&
      genericTable->WitnessTablePrivateSizeInWords == 0 &&
      (genericTable->Protocol.isNull() ||
       genericTable->WitnessTableSizeInWords -
       genericTable->Protocol->MinimumWitnessTableSizeInWords ==
       genericTable->Protocol->DefaultWitnessTableSizeInWords)) {
    return true;
  }

  return false;
}

/// Instantiate a brand new witness table for a resilient or generic
/// protocol conformance.
static WitnessTableCacheEntry *
allocateWitnessTable(GenericWitnessTable *genericTable,
                     MetadataAllocator &allocator,
                     const void *args[],
                     size_t numGenericArgs) {

  // Number of bytes for any private storage used by the conformance itself.
  size_t privateSize = genericTable->WitnessTablePrivateSizeInWords * sizeof(void *);

  size_t minWitnessTableSize, expectedWitnessTableSize;
  size_t actualWitnessTableSize = genericTable->WitnessTableSizeInWords * sizeof(void *);

  auto protocol = genericTable->Protocol.get();

  if (protocol != nullptr && protocol->Flags.isResilient()) {
    // The protocol and conforming type are in different resilience domains.
    // Allocate the witness table with the correct size, and fill in default
    // requirements at the end as needed.
    minWitnessTableSize = (protocol->MinimumWitnessTableSizeInWords *
                           sizeof(void *));
    expectedWitnessTableSize = ((protocol->MinimumWitnessTableSizeInWords +
                                 protocol->DefaultWitnessTableSizeInWords) *
                                sizeof(void *));
    assert(actualWitnessTableSize >= minWitnessTableSize &&
           actualWitnessTableSize <= expectedWitnessTableSize);
  } else {
    // The protocol and conforming type are in the same resilience domain.
    // Trust that the witness table template already has the correct size.
    minWitnessTableSize = expectedWitnessTableSize = actualWitnessTableSize;
  }

  // Create a new entry for the cache.
  auto entry = WitnessTableCacheEntry::allocate(
      allocator, args, numGenericArgs,
      privateSize + expectedWitnessTableSize);

  char *fullTable = entry->getData<char>();

  // Zero out the private storage area.
  memset(fullTable, 0, privateSize);

  // Advance the address point; the private storage area is accessed via
  // negative offsets.
  auto *table = entry->get(genericTable);

  // Fill in the provided part of the requirements from the pattern.
  memcpy(table, (void * const *) &*genericTable->Pattern,
         actualWitnessTableSize);

  // If this is a resilient conformance, copy in the rest.
  if (protocol != nullptr && protocol->Flags.isResilient()) {
    memcpy((char *) table + actualWitnessTableSize,
           (char *) protocol->getDefaultWitnesses() +
              (actualWitnessTableSize - minWitnessTableSize),
           expectedWitnessTableSize - actualWitnessTableSize);
  }

  return entry;
}

const WitnessTable *swift::swift_getGenericWitnessTable(
    GenericWitnessTable *genericTable, const Metadata *type,
    void *const *instantiationArgs) SWIFT_CC(RegisterPreservingCC_IMPL) {
  if (doesNotRequireInstantiation(genericTable)) {
    return genericTable->Pattern;
  }

  // If type is not nullptr, the witness table depends on the substituted
  // conforming type, so use that are the key.
  constexpr const size_t numGenericArgs = 1;
  const void *args[] = { type };

  auto &cache = getCache(genericTable);
  auto entry = cache.findOrAdd(args, numGenericArgs,
    [&]() -> WitnessTableCacheEntry* {
      // Allocate the witness table and fill it in.
      auto entry = allocateWitnessTable(genericTable,
                                        cache.getAllocator(),
                                        args, numGenericArgs);

      // Call the instantiation function to initialize
      // dependent associated type metadata.
      if (!genericTable->Instantiator.isNull()) {
        genericTable->Instantiator(entry->get(genericTable),
                                   type, instantiationArgs);
      }

      return entry;
    });

  return entry->get(genericTable);
}

uint64_t swift::RelativeDirectPointerNullPtr = 0;
