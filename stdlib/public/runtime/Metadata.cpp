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

#include "swift/Runtime/Metadata.h"
#include "MetadataCache.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Lazy.h"
#include "swift/Basic/Range.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/ExistentialContainer.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Mutex.h"
#include "swift/Strings.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/PointerLikeTypeTraits.h"
#include <algorithm>
#include <cctype>
#include <condition_variable>
#include <new>
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

static const size_t ValueTypeMetadataAddressPoint = sizeof(TypeMetadataHeader);

static ClassMetadataBounds
computeMetadataBoundsForSuperclass(const void *ref,
                                   TypeMetadataRecordKind refKind) {
  switch (refKind) {
  case TypeMetadataRecordKind::IndirectNominalTypeDescriptor: {
    auto description = *reinterpret_cast<const ClassDescriptor * const *>(ref);
    if (!description) {
      swift::fatalError(0, "instantiating class metadata for class with "
                           "missing weak-linked ancestor");
    }
    return description->getMetadataBounds();
  }

  case TypeMetadataRecordKind::DirectNominalTypeDescriptor: {
    auto description = reinterpret_cast<const ClassDescriptor *>(ref);
    return description->getMetadataBounds();
  }

  case TypeMetadataRecordKind::IndirectObjCClass:
#if SWIFT_OBJC_INTEROP
    {
      auto cls = *reinterpret_cast<const Class *>(ref);
      cls = swift_getInitializedObjCClass(cls);
      auto metadata = reinterpret_cast<const ClassMetadata *>(cls);
      return metadata->getClassBoundsAsSwiftSuperclass();
    }
#else
    // fallthrough
#endif

  case TypeMetadataRecordKind::Reserved:
    break;
  }
  swift_runtime_unreachable("unsupported superclass reference kind");
}

static ClassMetadataBounds computeMetadataBoundsFromSuperclass(
                                      const ClassDescriptor *description,
                                      StoredClassMetadataBounds &storedBounds) {
  ClassMetadataBounds bounds;

  // Compute the bounds for the superclass, extending it to the minimum
  // bounds of a Swift class.
  if (const void *superRef = description->Superclass.get()) {
    bounds = computeMetadataBoundsForSuperclass(superRef,
                                     description->getSuperclassReferenceKind());
  } else {
    bounds = ClassMetadataBounds::forSwiftRootClass();
  }

  // Add the subclass's immediate members.
  bounds.adjustForSubclass(description->areImmediateMembersNegative(),
                           description->NumImmediateMembers);

  // Cache before returning.
  storedBounds.initialize(bounds);
  return bounds;
}

ClassMetadataBounds
swift::getResilientMetadataBounds(const ClassDescriptor *description) {
  assert(description->hasResilientSuperclass());
  auto &storedBounds = *description->ResilientMetadataBounds.get();

  ClassMetadataBounds bounds;
  if (storedBounds.tryGet(bounds)) {
    return bounds;
  }

  return computeMetadataBoundsFromSuperclass(description, storedBounds);
}

int32_t
swift::getResilientImmediateMembersOffset(const ClassDescriptor *description) {
  assert(description->hasResilientSuperclass());
  auto &storedBounds = *description->ResilientMetadataBounds.get();

  ptrdiff_t result;
  if (storedBounds.tryGetImmediateMembersOffset(result)) {
    return result / sizeof(void*);
  }

  auto bounds = computeMetadataBoundsFromSuperclass(description, storedBounds);
  return bounds.ImmediateMembersOffset / sizeof(void*);
}

namespace {
  struct GenericCacheEntry final : MetadataCacheEntryBase<GenericCacheEntry> {
    static const char *getName() { return "GenericCache"; }

    template <class... Args>
    GenericCacheEntry(MetadataCacheKey key, Args &&...args)
      : MetadataCacheEntryBase(key) {}

    AllocationResult allocate(MetadataRequest request,
                              const TypeContextDescriptor *description,
                              const void * const *arguments) {
      // Find a pattern.  Currently we always use the default pattern.
      auto &generics = description->getFullGenericContextHeader();
      auto pattern = generics.DefaultInstantiationPattern.get();

      // Call the pattern's instantiation function.
      auto metadata =
        pattern->InstantiationFunction(description, arguments, pattern);

      MetadataRequest::BasicKind state;
      if (pattern->CompletionFunction.isNull()) {
        state = MetadataRequest::Complete;
      } else {
        state = inferStateForMetadata(metadata);
      }
      return { metadata, state };
    }

    MetadataRequest::BasicKind inferStateForMetadata(Metadata *metadata) {
      // FIXME: base this on whether the VWT is incomplete.
      if (isa<ClassMetadata>(metadata))
        return MetadataRequest::LayoutComplete;
      else
        return MetadataRequest::Abstract;
    }

    // Note that we have to pass 'arguments' separately here instead of
    // using the key data because there might be non-key arguments,
    // like protocol conformances.
    TryInitializeResult tryInitialize(Metadata *metadata,
                                      MetadataCompletionContext *context,
                                      const TypeContextDescriptor *description,
                                      const void * const *arguments) {
      // Find a pattern.  Currently we always use the default pattern.
      auto &generics = description->getFullGenericContextHeader();
      auto pattern = generics.DefaultInstantiationPattern.get();

      // Complete the metadata's instantiation.
      auto dependency = pattern->CompletionFunction(metadata, context, pattern);

      // FIXME: use a more precise dependency requirement returned by the
      // completion function.
      auto dependencyRequirement = MetadataRequest::Complete;

      auto state = dependency == nullptr
                     ? MetadataRequest::Complete
                     : inferStateForMetadata(metadata);

      return { state, dependencyRequirement, dependency };
    }
  };
} // end anonymous namespace

using GenericMetadataCache = MetadataCache<GenericCacheEntry, false>;
using LazyGenericMetadataCache = Lazy<GenericMetadataCache>;

/// Fetch the metadata cache for a generic metadata structure.
static GenericMetadataCache &getCache(
    const TypeGenericContextDescriptorHeader &generics) {
  // Keep this assert even if you change the representation above.
  static_assert(sizeof(LazyGenericMetadataCache) <=
                sizeof(GenericMetadataInstantiationCache::PrivateData),
                "metadata cache is larger than the allowed space");

  auto lazyCache =
    reinterpret_cast<LazyGenericMetadataCache*>(
      generics.getInstantiationCache()->PrivateData);
  return lazyCache->get();
}

/// Fetch the metadata cache for a generic metadata structure,
/// in a context where it must have already been initialized.
static GenericMetadataCache &unsafeGetInitializedCache(
    const TypeGenericContextDescriptorHeader &generics) {
  // Keep this assert even if you change the representation above.
  static_assert(sizeof(LazyGenericMetadataCache) <=
                sizeof(GenericMetadataInstantiationCache::PrivateData),
                "metadata cache is larger than the allowed space");

  auto lazyCache =
    reinterpret_cast<LazyGenericMetadataCache*>(
      generics.getInstantiationCache()->PrivateData);
  return lazyCache->unsafeGetAlreadyInitialized();
}

#if SWIFT_OBJC_INTEROP
extern "C" void *_objc_empty_cache;
#endif

static void copyMetadataPattern(void **section,
                                const GenericMetadataPartialPattern *pattern) {
  memcpy(section + pattern->OffsetInWords,
         pattern->Pattern.get(),
         size_t(pattern->SizeInWords) * sizeof(void*));
}

static void
initializeClassMetadataFromPattern(ClassMetadata *metadata,
                                   ClassMetadataBounds bounds,
                                   const ClassDescriptor *description,
                                   const GenericClassMetadataPattern *pattern) {
  auto fullMetadata = asFullMetadata(metadata);
  char *rawMetadata = reinterpret_cast<char*>(metadata);

  // Install the extra-data pattern.
  void **metadataExtraData =
    reinterpret_cast<void**>(rawMetadata) + bounds.PositiveSizeInWords;
  if (pattern->hasExtraDataPattern()) {
    auto extraDataPattern = pattern->getExtraDataPattern();

    // Zero memory up to the offset.
    memset(metadataExtraData, 0, size_t(extraDataPattern->OffsetInWords));

    // Copy the pattern into the rest of the extra data.
    copyMetadataPattern(metadataExtraData, extraDataPattern);
  }

  // Install the immediate members pattern:
  void **immediateMembers =
    reinterpret_cast<void**>(rawMetadata + bounds.ImmediateMembersOffset);

  // Zero out the entire immediate-members section.
  // TODO: only memset the parts that aren't covered by the pattern.
  memset(immediateMembers, 0, description->getImmediateMembersSize());

  // Copy in the immediate arguments.

  // Copy the immediate-members pattern.
  if (pattern->hasImmediateMembersPattern()) {
    auto immediateMembersPattern = pattern->getImmediateMembersPattern();
    copyMetadataPattern(immediateMembers, immediateMembersPattern);
  }

  // Initialize the header:

  // Heap destructor.
  fullMetadata->destroy = pattern->Destroy;

  // Value witness table.
#if SWIFT_OBJC_INTEROP
  fullMetadata->ValueWitnesses =
    (pattern->Flags & ClassFlags::UsesSwiftRefcounting)
       ? &VALUE_WITNESS_SYM(Bo)
       : &VALUE_WITNESS_SYM(BO);
#else
  fullMetadata->ValueWitnesses = &VALUE_WITNESS_SYM(Bo);
#endif

#if SWIFT_OBJC_INTEROP
  // Install the metaclass's RO-data pointer.
  auto metaclass = reinterpret_cast<AnyClassMetadata *>(
      metadataExtraData + pattern->MetaclassObjectOffset);
  auto metaclassRO = metadataExtraData + pattern->MetaclassRODataOffset;
  metaclass->Data = reinterpret_cast<uintptr_t>(metaclassRO);
#endif

  // MetadataKind / isa.
#if SWIFT_OBJC_INTEROP
  metadata->setClassISA(metaclass);
#else
  metadata->setKind(MetadataKind::Class);
#endif

  // Superclass.
  metadata->Superclass = nullptr;
#if SWIFT_OBJC_INTEROP
  // If the class doesn't have a formal superclass, automatically set
  // it to SwiftObject.
  if (!description->hasSuperclass()) {
    metadata->Superclass = getRootSuperclass();
  }
#endif

#if SWIFT_OBJC_INTEROP
  // Cache data.  Install the same initializer that the compiler is
  // required to use.  We don't need to do this in non-ObjC-interop modes.
  metadata->CacheData[0] = &_objc_empty_cache;
  metadata->CacheData[1] = nullptr;
#endif

  // RO-data pointer.
#if SWIFT_OBJC_INTEROP
  auto classRO = metadataExtraData + pattern->ClassRODataOffset;
  metadata->Data =
    reinterpret_cast<uintptr_t>(classRO) | SWIFT_CLASS_IS_SWIFT_MASK;
#else
  metadata->Data = SWIFT_CLASS_IS_SWIFT_MASK;
#endif

  // Class flags.
  metadata->Flags = pattern->Flags;

  // Instance layout.
  metadata->InstanceAddressPoint = 0;
  metadata->InstanceSize = 0;
  metadata->InstanceAlignMask = 0;

  // Reserved.
  metadata->Reserved = 0;

  // Class metadata layout.
  metadata->ClassSize = bounds.getTotalSizeInBytes();
  metadata->ClassAddressPoint = bounds.getAddressPointInBytes();

  // Class descriptor.
  metadata->setDescription(description);

  // I-var destroyer.
  metadata->IVarDestroyer = pattern->IVarDestroyer;
}

ClassMetadata *
swift::swift_allocateGenericClassMetadata(const ClassDescriptor *description,
                                          const void *arguments,
                                    const GenericClassMetadataPattern *pattern){
  auto &generics = description->getFullGenericContextHeader();
  auto &cache = unsafeGetInitializedCache(generics);

  // Compute the formal bounds of the metadata.
  auto bounds = description->getMetadataBounds();

  // Augment that with any required extra data from the pattern.
  auto allocationBounds = bounds;
  if (pattern->hasExtraDataPattern()) {
    auto extraDataPattern = pattern->getExtraDataPattern();
    allocationBounds.PositiveSizeInWords +=
      extraDataPattern->OffsetInWords + extraDataPattern->SizeInWords;
  }

  auto bytes = (char*) 
    cache.getAllocator().Allocate(allocationBounds.getTotalSizeInBytes(),
                                  alignof(void*));

  auto addressPoint = bytes + allocationBounds.getAddressPointInBytes();
  auto metadata = reinterpret_cast<ClassMetadata *>(addressPoint);

  initializeClassMetadataFromPattern(metadata, bounds, description, pattern);

  assert(metadata->isTypeMetadata());

  return metadata;
}

static void
initializeValueMetadataFromPattern(ValueMetadata *metadata,
                                   const ValueTypeDescriptor *description,
                                   const GenericValueMetadataPattern *pattern) {
  auto fullMetadata = asFullMetadata(metadata);
  char *rawMetadata = reinterpret_cast<char*>(metadata);

  if (pattern->hasExtraDataPattern()) {
    void **metadataExtraData =
      reinterpret_cast<void**>(rawMetadata + sizeof(ValueMetadata));
    auto extraDataPattern = pattern->getExtraDataPattern();

    // Zero memory up to the offset.
    memset(metadataExtraData, 0, size_t(extraDataPattern->OffsetInWords));

    // Copy the pattern into the rest of the extra data.
    copyMetadataPattern(metadataExtraData, extraDataPattern);
  }

  // Put the VWT pattern in place as if it was the real VWT.
  // The various initialization functions will instantiate this as
  // necessary.
  fullMetadata->setValueWitnesses(pattern->getValueWitnessesPattern());

  // Set the metadata kind.
  metadata->setKind(pattern->getMetadataKind());

  // Set the type descriptor.
  metadata->Description = description;
}

ValueMetadata *
swift::swift_allocateGenericValueMetadata(const ValueTypeDescriptor *description,
                                          const void *arguments,
                                    const GenericValueMetadataPattern *pattern,
                                          size_t extraDataSize) {
  auto &generics = description->getFullGenericContextHeader();
  auto &cache = unsafeGetInitializedCache(generics);

  static_assert(sizeof(StructMetadata::HeaderType)
                  == sizeof(ValueMetadata::HeaderType),
                "struct metadata header unexpectedly has extra members");
  static_assert(sizeof(StructMetadata) == sizeof(ValueMetadata),
                "struct metadata unexpectedly has extra members");
  static_assert(sizeof(EnumMetadata::HeaderType)
                  == sizeof(ValueMetadata::HeaderType),
                "enum metadata header unexpectedly has extra members");
  static_assert(sizeof(EnumMetadata) == sizeof(ValueMetadata),
                "enum metadata unexpectedly has extra members");

  size_t totalSize = sizeof(FullMetadata<ValueMetadata>) + extraDataSize;

  auto bytes = (char*) cache.getAllocator().Allocate(totalSize, alignof(void*));

  auto addressPoint = bytes + sizeof(ValueMetadata::HeaderType);
  auto metadata = reinterpret_cast<ValueMetadata *>(addressPoint);

  initializeValueMetadataFromPattern(metadata, description, pattern);

  return metadata;
}

/// The primary entrypoint.
const Metadata *
swift::swift_getGenericMetadata(const TypeContextDescriptor *description,
                                const void *arguments) {
  auto genericArgs = (const void * const *) arguments;
  auto &generics = description->getFullGenericContextHeader();
  size_t numGenericArgs = generics.Base.NumKeyArguments;

  MetadataRequest request(MetadataRequest::Complete);

  auto key = MetadataCacheKey(genericArgs, numGenericArgs);
  auto result =
    getCache(generics).getOrInsert(key, request, description, genericArgs);

  // TODO: report cases where the state is inadequate
  return result.second.Value;
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

  return &ObjCClassWrappers.getOrInsert(theClass).first->Data;
}

const ClassMetadata *
swift::swift_getObjCClassFromMetadata(const Metadata *theMetadata) {
  // Unwrap ObjC class wrappers.
  if (auto wrapper = dyn_cast<ObjCClassWrapperMetadata>(theMetadata)) {
    return wrapper->Class;
  }

  // Otherwise, the input should already be a Swift class object.
  auto theClass = cast<ClassMetadata>(theMetadata);
  assert(theClass->isTypeMetadata() && !theClass->isArtificialSubclass());
  return theClass;
}

#endif

/***************************************************************************/
/*** Functions *************************************************************/
/***************************************************************************/

namespace {

class FunctionCacheEntry {
public:
  FullMetadata<FunctionTypeMetadata> Data;

  struct Key {
    const FunctionTypeFlags Flags;

    const Metadata *const *Parameters;
    const uint32_t *ParameterFlags;
    const Metadata *Result;

    FunctionTypeFlags getFlags() const { return Flags; }
    const Metadata *getParameter(unsigned index) const {
      assert(index < Flags.getNumParameters());
      return Parameters[index];
    }
    const Metadata *getResult() const { return Result; }

    const uint32_t *getParameterFlags() const {
      return ParameterFlags;
    }

    ::ParameterFlags getParameterFlags(unsigned index) const {
      assert(index < Flags.getNumParameters());
      auto flags = Flags.hasParameterFlags() ? ParameterFlags[index] : 0;
      return ParameterFlags::fromIntValue(flags);
    }
  };

  FunctionCacheEntry(const Key &key);

  intptr_t getKeyIntValueForDump() {
    return 0; // No single meaningful value here.
  }

  int compareWithKey(const Key &key) const {
    auto keyFlags = key.getFlags();
    if (auto result = compareIntegers(keyFlags.getIntValue(),
                                      Data.Flags.getIntValue()))
      return result;

    if (auto result = comparePointers(key.getResult(), Data.ResultType))
      return result;

    for (unsigned i = 0, e = keyFlags.getNumParameters(); i != e; ++i) {
      if (auto result =
              comparePointers(key.getParameter(i), Data.getParameter(i)))
        return result;

      if (auto result =
              compareIntegers(key.getParameterFlags(i).getIntValue(),
                              Data.getParameterFlags(i).getIntValue()))
        return result;
    }

    return 0;
  }
  static size_t getExtraAllocationSize(const Key &key) {
    return getExtraAllocationSize(key.Flags);
  }

  size_t getExtraAllocationSize() const {
    return getExtraAllocationSize(Data.Flags);
  }

  static size_t getExtraAllocationSize(const FunctionTypeFlags &flags) {
    const auto numParams = flags.getNumParameters();
    auto size = numParams * sizeof(FunctionTypeMetadata::Parameter);
    if (flags.hasParameterFlags())
      size += numParams * sizeof(uint32_t);
    return roundUpToAlignment(size, sizeof(void *));
  }
};

} // end anonymous namespace

/// The uniquing structure for function type metadata.
static SimpleGlobalCache<FunctionCacheEntry> FunctionTypes;

const FunctionTypeMetadata *
swift::swift_getFunctionTypeMetadata0(FunctionTypeFlags flags,
                                      const Metadata *result) {
  assert(flags.getNumParameters() == 0
         && "wrong number of arguments in function metadata flags?!");
  return swift_getFunctionTypeMetadata(flags, nullptr, nullptr, result);
}

const FunctionTypeMetadata *
swift::swift_getFunctionTypeMetadata1(FunctionTypeFlags flags,
                                      const Metadata *arg0,
                                      const Metadata *result) {
  assert(flags.getNumParameters() == 1
         && "wrong number of arguments in function metadata flags?!");
  const Metadata *parameters[] = { arg0 };
  return swift_getFunctionTypeMetadata(flags, parameters, nullptr, result);
}

const FunctionTypeMetadata *
swift::swift_getFunctionTypeMetadata2(FunctionTypeFlags flags,
                                      const Metadata *arg0,
                                      const Metadata *arg1,
                                      const Metadata *result) {
  assert(flags.getNumParameters() == 2
         && "wrong number of arguments in function metadata flags?!");
  const Metadata *parameters[] = { arg0, arg1 };
  return swift_getFunctionTypeMetadata(flags, parameters, nullptr, result);
}

const FunctionTypeMetadata *
swift::swift_getFunctionTypeMetadata3(FunctionTypeFlags flags,
                                      const Metadata *arg0,
                                      const Metadata *arg1,
                                      const Metadata *arg2,
                                      const Metadata *result) {
  assert(flags.getNumParameters() == 3
         && "wrong number of arguments in function metadata flags?!");
  const Metadata *parameters[] = { arg0, arg1, arg2 };
  return swift_getFunctionTypeMetadata(flags, parameters, nullptr, result);
}

const FunctionTypeMetadata *
swift::swift_getFunctionTypeMetadata(FunctionTypeFlags flags,
                                     const Metadata *const *parameters,
                                     const uint32_t *parameterFlags,
                                     const Metadata *result) {
  FunctionCacheEntry::Key key = { flags, parameters, parameterFlags, result };
  return &FunctionTypes.getOrInsert(key).first->Data;
}

FunctionCacheEntry::FunctionCacheEntry(const Key &key) {
  auto flags = key.getFlags();

  // Pick a value witness table appropriate to the function convention.
  // All function types of a given convention have the same value semantics,
  // so they share a value witness table.
  switch (flags.getConvention()) {
  case FunctionMetadataConvention::Swift:
    if (!flags.isEscaping()) {
      Data.ValueWitnesses = &VALUE_WITNESS_SYM(NOESCAPE_FUNCTION_MANGLING);
    } else {
      Data.ValueWitnesses = &VALUE_WITNESS_SYM(FUNCTION_MANGLING);
    }
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

  unsigned numParameters = flags.getNumParameters();

  Data.setKind(MetadataKind::Function);
  Data.Flags = flags;
  Data.ResultType = key.getResult();

  for (unsigned i = 0; i < numParameters; ++i) {
    Data.getParameters()[i] = key.getParameter(i);
    if (flags.hasParameterFlags())
      Data.getParameterFlags()[i] = key.getParameterFlags(i).getIntValue();
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

  auto wtable = tuple_getValueWitnesses(metatype);
  unsigned alignMask = wtable->getAlignmentMask();
  // Compute the byte offset of the object in the box.
  unsigned byteOffset = (sizeof(HeapObject) + alignMask) & ~alignMask;
  auto *bytePtr =
      reinterpret_cast<char *>(*reinterpret_cast<HeapObject **>(buffer));
  return reinterpret_cast<OpaqueValue *>(bytePtr + byteOffset);
}

/// Generic tuple value witness for 'allocateBuffer'
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_allocateBuffer(ValueBuffer *buffer,
                                         const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());

  if (IsInline)
    return reinterpret_cast<OpaqueValue*>(buffer);
  BoxPair refAndValueAddr(swift_allocBox(metatype));
  *reinterpret_cast<HeapObject **>(buffer) = refAndValueAddr.object;
  return refAndValueAddr.buffer;
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

// The operation doesn't have to be initializeWithCopy, but they all
// have basically the same type.
typedef value_witness_types::initializeWithCopy forEachOperation;

/// Perform an operation for each field of two tuples.
static OpaqueValue *tuple_forEachField(OpaqueValue *destTuple,
                                       OpaqueValue *srcTuple,
                                       const Metadata *_metatype,
                                       forEachOperation operation) {
  auto &metatype = *(const TupleTypeMetadata*) _metatype;
  for (size_t i = 0, e = metatype.NumElements; i != e; ++i) {
    auto &eltInfo = metatype.getElement(i);
    OpaqueValue *destElt = eltInfo.findIn(destTuple);
    OpaqueValue *srcElt = eltInfo.findIn(srcTuple);
    operation(destElt, srcElt, eltInfo.Type);
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
      [](OpaqueValue *dest, OpaqueValue *src, const Metadata *eltType) {
    return eltType->vw_initializeWithCopy(dest, src);
  });
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
      [](OpaqueValue *dest, OpaqueValue *src, const Metadata *eltType) {
    return eltType->vw_initializeWithTake(dest, src);
  });
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
      [](OpaqueValue *dest, OpaqueValue *src, const Metadata *eltType) {
    return eltType->vw_assignWithCopy(dest, src);
  });
}

/// Generic tuple value witness for 'assignWithTake'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_assignWithTake(OpaqueValue *dest,
                                         OpaqueValue *src,
                                         const Metadata *metatype) {
  if (IsPOD) return tuple_memcpy(dest, src, metatype);
  return tuple_forEachField(dest, src, metatype,
      [](OpaqueValue *dest, OpaqueValue *src, const Metadata *eltType) {
    return eltType->vw_assignWithTake(dest, src);
  });
}

/// Generic tuple value witness for 'initializeBufferWithCopyOfBuffer'.
template <bool IsPOD, bool IsInline>
static OpaqueValue *tuple_initializeBufferWithCopyOfBuffer(ValueBuffer *dest,
                                                           ValueBuffer *src,
                                                     const Metadata *metatype) {
  assert(IsPOD == tuple_getValueWitnesses(metatype)->isPOD());
  assert(IsInline == tuple_getValueWitnesses(metatype)->isValueInline());
  if (IsInline) {
    return tuple_initializeWithCopy<IsPOD, IsInline>(
        tuple_projectBuffer<IsPOD, IsInline>(dest, metatype),
        tuple_projectBuffer<IsPOD, IsInline>(src, metatype), metatype);
  }

  auto *srcReference = *reinterpret_cast<HeapObject**>(src);
  *reinterpret_cast<HeapObject**>(dest) = srcReference;
  swift_retain(srcReference);
  return tuple_projectBuffer<IsPOD, IsInline>(dest, metatype);
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
        tuple_projectBuffer<IsPOD, IsInline>(src, metatype), metatype);
  }
  auto *srcReference = *reinterpret_cast<HeapObject**>(src);
  *reinterpret_cast<HeapObject**>(dest) = srcReference;
  return tuple_projectBuffer<IsPOD, IsInline>(dest, metatype);
}

template <bool IsPOD, bool IsInline>
static int tuple_getEnumTagSinglePayload(const OpaqueValue *enumAddr,
                                         unsigned numEmptyCases,
                                         const Metadata *self) {
  auto *witnesses = self->getValueWitnesses();
  auto size = witnesses->getSize();
  auto numExtraInhabitants = witnesses->getNumExtraInhabitants();
  auto getExtraInhabitantIndex =
      (static_cast<const ExtraInhabitantsValueWitnessTable *>(witnesses)
           ->getExtraInhabitantIndex);

  return getEnumTagSinglePayloadImpl(enumAddr, numEmptyCases, self, size,
                                     numExtraInhabitants,
                                     getExtraInhabitantIndex);
}

template <bool IsPOD, bool IsInline>
static void
tuple_storeEnumTagSinglePayload(OpaqueValue *enumAddr, int whichCase,
                                unsigned numEmptyCases, const Metadata *self) {
  auto *witnesses = self->getValueWitnesses();
  auto size = witnesses->getSize();
  auto numExtraInhabitants = witnesses->getNumExtraInhabitants();
  auto storeExtraInhabitant =
      (static_cast<const ExtraInhabitantsValueWitnessTable *>(witnesses)
           ->storeExtraInhabitant);

  storeEnumTagSinglePayloadImpl(enumAddr, whichCase, numEmptyCases, self, size,
                                numExtraInhabitants, storeExtraInhabitant);
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
#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) &tuple_##LOWER_ID<true, true>,
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)
#include "swift/ABI/ValueWitness.def"
  0,
  ValueWitnessFlags(),
  0
};
static const ValueWitnessTable tuple_witnesses_nonpod_inline = {
#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) &tuple_##LOWER_ID<false, true>,
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)
#include "swift/ABI/ValueWitness.def"
  0,
  ValueWitnessFlags(),
  0
};
static const ValueWitnessTable tuple_witnesses_pod_noninline = {
#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) &tuple_##LOWER_ID<true, false>,
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)
#include "swift/ABI/ValueWitness.def"
  0,
  ValueWitnessFlags(),
  0
};
static const ValueWitnessTable tuple_witnesses_nonpod_noninline = {
#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) &tuple_##LOWER_ID<false, false>,
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)
#include "swift/ABI/ValueWitness.def"
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
swift::swift_getTupleTypeMetadata(TupleTypeFlags flags,
                                  const Metadata * const *elements,
                                  const char *labels,
                                  const ValueWitnessTable *proposedWitnesses) {
  auto numElements = flags.getNumElements();

  // Bypass the cache for the empty tuple. We might reasonably get called
  // by generic code, like a demangler that produces type objects.
  if (numElements == 0) return &METADATA_SYM(EMPTY_TUPLE_MANGLING);

  // Search the cache.
  TupleCacheEntry::Key key = { numElements, elements, labels };

  // If we have constant labels, directly check the cache.
  if (!flags.hasNonConstantLabels())
    return &TupleTypes.getOrInsert(key, proposedWitnesses).first->Data;

  // If we have non-constant labels, we can't simply record the result.
  // Look for an existing result, first.
  if (auto found = TupleTypes.find(key))
    return &found->Data;

  // Allocate a copy of the labels string within the tuple type allocator.
  size_t labelsLen = strlen(labels);
  size_t labelsAllocSize = roundUpToAlignment(labelsLen + 1, sizeof(void*));
  char *newLabels =
    (char *)TupleTypes.getAllocator().Allocate(labelsAllocSize, alignof(char));
  strcpy(newLabels, labels);
  key.Labels = newLabels;

  // Update the metadata cache.
  auto result = TupleTypes.getOrInsert(key, proposedWitnesses);

  // If we didn't manage to perform the insertion, free the memory associated
  // with the copy of the labels: nobody else can reference it.
  if (!result.second) {
    TupleTypes.getAllocator().Deallocate(newLabels, labelsAllocSize);
  }

  // Done.
  return &result.first->Data;
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

  // We have extra inhabitants if the first element does.
  // FIXME: generalize this.
  bool hasExtraInhabitants = false;
  if (auto firstEltEIVWT = dyn_cast<ExtraInhabitantsValueWitnessTable>(
                             key.Elements[0]->getValueWitnesses())) {
    hasExtraInhabitants = true;
    Witnesses.flags = Witnesses.flags.withExtraInhabitants(true);
    Witnesses.extraInhabitantFlags = firstEltEIVWT->extraInhabitantFlags;
    Witnesses.storeExtraInhabitant = tuple_storeExtraInhabitant;
    Witnesses.getExtraInhabitantIndex = tuple_getExtraInhabitantIndex;
  }

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
      if (!hasExtraInhabitants && layout.size == 8 && layout.flags.getAlignmentMask() == 7)
        proposedWitnesses = &VALUE_WITNESS_SYM(Bi64_);
      else if (!hasExtraInhabitants && layout.size == 4 && layout.flags.getAlignmentMask() == 3)
        proposedWitnesses = &VALUE_WITNESS_SYM(Bi32_);
      else if (!hasExtraInhabitants && layout.size == 2 && layout.flags.getAlignmentMask() == 1)
        proposedWitnesses = &VALUE_WITNESS_SYM(Bi16_);
      else if (!hasExtraInhabitants && layout.size == 1)
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
#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) \
  Witnesses.LOWER_ID = proposedWitnesses->LOWER_ID;
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)
#include "swift/ABI/ValueWitness.def"
}

const TupleTypeMetadata *
swift::swift_getTupleTypeMetadata2(const Metadata *elt0, const Metadata *elt1,
                                   const char *labels,
                                   const ValueWitnessTable *proposedWitnesses) {
  const Metadata *elts[] = { elt0, elt1 };
  return swift_getTupleTypeMetadata(TupleTypeFlags().withNumElements(2),
                                    elts, labels, proposedWitnesses);
}

const TupleTypeMetadata *
swift::swift_getTupleTypeMetadata3(const Metadata *elt0, const Metadata *elt1,
                                   const Metadata *elt2,
                                   const char *labels,
                                   const ValueWitnessTable *proposedWitnesses) {
  const Metadata *elts[] = { elt0, elt1, elt2 };
  return swift_getTupleTypeMetadata(TupleTypeFlags().withNumElements(3),
                                    elts, labels, proposedWitnesses);
}

/***************************************************************************/
/*** Nominal type descriptors **********************************************/
/***************************************************************************/
bool swift::equalContexts(const ContextDescriptor *a,
                          const ContextDescriptor *b)
{
  // Fast path: pointer equality.
  if (a == b) return true;

  // If either context is null, we're done.
  if (a == nullptr || b == nullptr)
    return false;

  // If either descriptor is known to be unique, we're done.
  if (a->isUnique() || b->isUnique()) return false;
  
  // Do the kinds match?
  if (a->getKind() != b->getKind()) return false;
  
  // Do the parents match?
  if (!equalContexts(a->Parent.get(), b->Parent.get()))
    return false;
  
  // Compare kind-specific details.
  switch (auto kind = a->getKind()) {
  case ContextDescriptorKind::Module: {
    // Modules with the same name are equivalent.
    auto moduleA = cast<ModuleContextDescriptor>(a);
    auto moduleB = cast<ModuleContextDescriptor>(b);
    return strcmp(moduleA->Name.get(), moduleB->Name.get()) == 0;
  }
  
  case ContextDescriptorKind::Extension:
  case ContextDescriptorKind::Anonymous:
    // These context kinds are always unique.
    return false;
  
  default:
    // Types in the same context with the same name are equivalent.
    if (kind >= ContextDescriptorKind::Type_First
        && kind <= ContextDescriptorKind::Type_Last) {
      auto typeA = cast<TypeContextDescriptor>(a);
      auto typeB = cast<TypeContextDescriptor>(b);
      return strcmp(typeA->Name.get(), typeB->Name.get()) == 0;
    }
    
    // Otherwise, this runtime doesn't know anything about this context kind.
    // Conservatively return false.
    return false;
  }
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
  struct pointer_function_cast_impl<OutRet * (*)(OutArgs *...)> {
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
  struct pointer_function_cast_impl<void (*)(OutArgs *...)> {
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
static constexpr Out pointer_function_cast(In *function) {
  return pointer_function_cast_impl<Out>::perform(function);
}

static OpaqueValue *pod_indirect_initializeBufferWithCopyOfBuffer(
                    ValueBuffer *dest, ValueBuffer *src, const Metadata *self) {
  auto wtable = self->getValueWitnesses();
  auto *srcReference = *reinterpret_cast<HeapObject**>(src);
  *reinterpret_cast<HeapObject**>(dest) = srcReference;
  swift_retain(srcReference);

  // Project the address of the value in the buffer.
  unsigned alignMask = wtable->getAlignmentMask();
  // Compute the byte offset of the object in the box.
  unsigned byteOffset = (sizeof(HeapObject) + alignMask) & ~alignMask;
  auto *bytePtr = reinterpret_cast<char *>(srcReference);
  return reinterpret_cast<OpaqueValue *>(bytePtr + byteOffset);
}

static OpaqueValue *pod_indirect_initializeBufferWithTakeOfBuffer(
                    ValueBuffer *dest, ValueBuffer *src, const Metadata *self) {
  auto wtable = self->getValueWitnesses();
  auto *srcReference = *reinterpret_cast<HeapObject**>(src);
  *reinterpret_cast<HeapObject**>(dest) = srcReference;

  // Project the address of the value in the buffer.
  unsigned alignMask = wtable->getAlignmentMask();
  // Compute the byte offset of the object in the box.
  unsigned byteOffset = (sizeof(HeapObject) + alignMask) & ~alignMask;
  auto *bytePtr = reinterpret_cast<char *>(srcReference);
  return reinterpret_cast<OpaqueValue *>(bytePtr + byteOffset);
}

static void pod_noop(void *object, const Metadata *self) {
}
#define pod_direct_destroy \
  pointer_function_cast<value_witness_types::destroy>(pod_noop)
#define pod_indirect_destroy pod_direct_destroy

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
#define pod_direct_assignWithCopy pod_direct_initializeWithCopy
#define pod_indirect_assignWithCopy pod_direct_initializeWithCopy
#define pod_direct_initializeWithTake pod_direct_initializeWithCopy
#define pod_indirect_initializeWithTake pod_direct_initializeWithCopy
#define pod_direct_assignWithTake pod_direct_initializeWithCopy
#define pod_indirect_assignWithTake pod_direct_initializeWithCopy

static int pod_direct_getEnumTagSinglePayload(const OpaqueValue *enumAddr,
                                              unsigned numEmptyCases,
                                              const Metadata *self) {
  auto *witnesses = self->getValueWitnesses();
  auto size = witnesses->getSize();
  auto numExtraInhabitants = witnesses->getNumExtraInhabitants();
  auto getExtraInhabitantIndex =
      (static_cast<const ExtraInhabitantsValueWitnessTable *>(witnesses)
           ->getExtraInhabitantIndex);

  return getEnumTagSinglePayloadImpl(enumAddr, numEmptyCases, self, size,
                                     numExtraInhabitants,
                                     getExtraInhabitantIndex);
}

static void pod_direct_storeEnumTagSinglePayload(OpaqueValue *enumAddr,
                                                 int whichCase,
                                                 unsigned numEmptyCases,
                                                 const Metadata *self) {
  auto *witnesses = self->getValueWitnesses();
  auto size = witnesses->getSize();
  auto numExtraInhabitants = witnesses->getNumExtraInhabitants();
  auto storeExtraInhabitant =
      (static_cast<const ExtraInhabitantsValueWitnessTable *>(witnesses)
           ->storeExtraInhabitant);

  storeEnumTagSinglePayloadImpl(enumAddr, whichCase, numEmptyCases, self, size,
                                numExtraInhabitants, storeExtraInhabitant);
}

#define pod_indirect_getEnumTagSinglePayload pod_direct_getEnumTagSinglePayload
#define pod_indirect_storeEnumTagSinglePayload \
  pod_direct_storeEnumTagSinglePayload

static constexpr uint64_t sizeWithAlignmentMask(uint64_t size,
                                                uint64_t alignmentMask,
                                                uint64_t hasExtraInhabitants) {
  return (hasExtraInhabitants << 48) | (size << 16) | alignmentMask;
}

void swift::installCommonValueWitnesses(ValueWitnessTable *vwtable) {
  auto flags = vwtable->flags;
  if (flags.isPOD()) {
    // Use POD value witnesses.
    // If the value has a common size and alignment, use specialized value
    // witnesses we already have lying around for the builtin types.
    const ValueWitnessTable *commonVWT;
    bool hasExtraInhabitants = flags.hasExtraInhabitants();
    switch (sizeWithAlignmentMask(vwtable->size, vwtable->getAlignmentMask(),
                                  hasExtraInhabitants)) {
    default:
      // For uncommon layouts, use value witnesses that work with an arbitrary
      // size and alignment.
      if (flags.isInlineStorage()) {
#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) \
        vwtable->LOWER_ID = pod_direct_##LOWER_ID;
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)
#include "swift/ABI/ValueWitness.def"
      } else {
#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) \
        vwtable->LOWER_ID = pod_indirect_##LOWER_ID;
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)
#include "swift/ABI/ValueWitness.def"
      }
      return;
      
    case sizeWithAlignmentMask(1, 0, 0):
      commonVWT = &VALUE_WITNESS_SYM(Bi8_);
      break;
    case sizeWithAlignmentMask(2, 1, 0):
      commonVWT = &VALUE_WITNESS_SYM(Bi16_);
      break;
    case sizeWithAlignmentMask(4, 3, 0):
      commonVWT = &VALUE_WITNESS_SYM(Bi32_);
      break;
    case sizeWithAlignmentMask(8, 7, 0):
      commonVWT = &VALUE_WITNESS_SYM(Bi64_);
      break;
    case sizeWithAlignmentMask(16, 15, 0):
      commonVWT = &VALUE_WITNESS_SYM(Bi128_);
      break;
    case sizeWithAlignmentMask(32, 31, 0):
      commonVWT = &VALUE_WITNESS_SYM(Bi256_);
      break;
    case sizeWithAlignmentMask(64, 63, 0):
      commonVWT = &VALUE_WITNESS_SYM(Bi512_);
      break;
    }

#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) \
    vwtable->LOWER_ID = commonVWT->LOWER_ID;
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)
#include "swift/ABI/ValueWitness.def"
    
    return;
  }
  
  if (flags.isBitwiseTakable()) {
    // Use POD value witnesses for operations that do an initializeWithTake.
    if (flags.isInlineStorage()) {
      vwtable->initializeWithTake = pod_direct_initializeWithTake;
      vwtable->initializeBufferWithTakeOfBuffer
        = pod_direct_initializeBufferWithTakeOfBuffer;
    } else {
      vwtable->initializeWithTake = pod_indirect_initializeWithTake;
      vwtable->initializeBufferWithTakeOfBuffer
        = pod_indirect_initializeBufferWithTakeOfBuffer;
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

static ValueWitnessTable *getMutableVWTableForInit(StructMetadata *self,
                                                   StructLayoutFlags flags,
                                                   bool hasExtraInhabitants) {
  auto oldTable = self->getValueWitnesses();

  // If we can alter the existing table in-place, do so.
  if (isValueWitnessTableMutable(flags))
    return const_cast<ValueWitnessTable*>(oldTable);

  // Otherwise, allocate permanent memory for it and copy the existing table.
  ValueWitnessTable *newTable;
  if (hasExtraInhabitants) {
    void *memory = allocateMetadata(sizeof(ExtraInhabitantsValueWitnessTable),
                                    alignof(ExtraInhabitantsValueWitnessTable));
    newTable = new (memory) ExtraInhabitantsValueWitnessTable(
              *static_cast<const ExtraInhabitantsValueWitnessTable*>(oldTable));
  } else {
    void *memory = allocateMetadata(sizeof(ValueWitnessTable),
                                    alignof(ValueWitnessTable));
    newTable = new (memory) ValueWitnessTable(*oldTable);
  }
  self->setValueWitnesses(newTable);

  return newTable;
}

/// Initialize the value witness table and struct field offset vector for a
/// struct, using the "Universal" layout strategy.
void swift::swift_initStructMetadata(StructMetadata *structType,
                                     StructLayoutFlags layoutFlags,
                                     size_t numFields,
                                     const TypeLayout * const *fieldTypes,
                                     size_t *fieldOffsets) {
  auto layout = BasicLayout::initialForValueType();
  performBasicLayout(layout, fieldTypes, numFields,
    [&](size_t i, const TypeLayout *fieldType, size_t offset) {
      assignUnlessEqual(fieldOffsets[i], offset);
    });

  bool hasExtraInhabitants = fieldTypes[0]->flags.hasExtraInhabitants();

  auto vwtable =
    getMutableVWTableForInit(structType, layoutFlags, hasExtraInhabitants);

  vwtable->size = layout.size;
  vwtable->flags = layout.flags;
  vwtable->stride = layout.stride;
  
  // We have extra inhabitants if the first element does.
  // FIXME: generalize this.
  if (hasExtraInhabitants) {
    vwtable->flags = vwtable->flags.withExtraInhabitants(true);
    auto xiVWT = cast<ExtraInhabitantsValueWitnessTable>(vwtable);
    xiVWT->extraInhabitantFlags = fieldTypes[0]->getExtraInhabitantFlags();

    // The compiler should already have initialized these.
    assert(xiVWT->storeExtraInhabitant);
    assert(xiVWT->getExtraInhabitantIndex);
  }

  // Substitute in better value witnesses if we have them.
  installCommonValueWitnesses(vwtable);
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
#if __POINTER_WIDTH__ == 64
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
  Demangle::Demangler Dem;
  // Resolve symbolic references to a unique mangling that can be encoded in
  // the class name.
  Dem.setSymbolicReferenceResolver(ResolveToDemanglingForContext(Dem));

  auto demangling = _swift_buildDemanglingForMetadata(theClass, Dem);

  // Remangle that into a new type mangling string.
  auto typeNode = Dem.createNode(Demangle::Node::Kind::TypeMangling);
  typeNode->addChild(demangling, Dem);
  auto globalNode = Dem.createNode(Demangle::Node::Kind::Global);
  globalNode->addChild(typeNode, Dem);

  auto string = Demangle::mangleNodeOld(globalNode);

  auto fullNameBuf = (char*)swift_slowAlloc(string.size() + 1, 0);
  memcpy(fullNameBuf, string.c_str(), string.size() + 1);

  auto theMetaclass = (ClassMetadata *)object_getClass((id)theClass);

  getROData(theClass)->Name = fullNameBuf;
  getROData(theMetaclass)->Name = fullNameBuf;
}
#endif

/// Initialize the invariant superclass components of a class metadata,
/// such as the generic type arguments, field offsets, and so on.
static void _swift_initializeSuperclass(ClassMetadata *theClass) {
#if SWIFT_OBJC_INTEROP
  // If the class is generic, we need to give it a name for Objective-C.
  if (theClass->getDescription()->isGeneric())
    _swift_initGenericClassObjCName(theClass);
#endif

  const ClassMetadata *theSuperclass = theClass->Superclass;

  // Copy the class's immediate methods from the nominal type descriptor
  // to the class metadata.
  {
    const auto *description = theClass->getDescription();
    auto *classWords = reinterpret_cast<void **>(theClass);

    if (description->hasVTable()) {
      auto *vtable = description->getVTableDescriptor();
      for (unsigned i = 0, e = vtable->VTableSize; i < e; ++i) {
        classWords[vtable->getVTableOffset(theClass) + i]
          = description->getMethod(i);
      }
    }
  }

  if (theSuperclass == nullptr)
    return;

  // If any ancestor classes have generic parameters, field offset vectors
  // or virtual methods, inherit them.
  //
  // Note that the caller is responsible for installing overrides of
  // superclass methods; here we just copy them verbatim.
  auto ancestor = theSuperclass;
  auto *classWords = reinterpret_cast<uintptr_t *>(theClass);
  auto *superWords = reinterpret_cast<const uintptr_t *>(theSuperclass);
  while (ancestor && ancestor->isTypeMetadata()) {
    const auto *description = ancestor->getDescription();

    // Copy the generic requirements.
    if (description->isGeneric()
        && description->getGenericContextHeader().hasArguments()) {
      memcpy(classWords + description->getGenericArgumentOffset(),
             superWords + description->getGenericArgumentOffset(),
             description->getGenericContextHeader().getNumArguments() *
               sizeof(uintptr_t));
    }

    // Copy the vtable entries.
    if (description->hasVTable()) {
      auto *vtable = description->getVTableDescriptor();
      memcpy(classWords + vtable->getVTableOffset(ancestor),
             superWords + vtable->getVTableOffset(ancestor),
             vtable->VTableSize * sizeof(uintptr_t));
    }

    // Copy the field offsets.
    if (description->hasFieldOffsetVector()) {
      unsigned fieldOffsetVector =
        description->getFieldOffsetVectorOffset(ancestor);
      memcpy(classWords + fieldOffsetVector,
             superWords + fieldOffsetVector,
             description->NumFields * sizeof(uintptr_t));
    }
    ancestor = ancestor->Superclass;
  }

#if SWIFT_OBJC_INTEROP
  // Set up the superclass of the metaclass, which is the metaclass of the
  // superclass.
  auto theMetaclass = (ClassMetadata *)object_getClass((id)theClass);
  auto theSuperMetaclass
    = (const ClassMetadata *)object_getClass(id_const_cast(theSuperclass));
  theMetaclass->Superclass = theSuperMetaclass;
#endif
}

#if SWIFT_OBJC_INTEROP
static MetadataAllocator &getResilientMetadataAllocator() {
  // This should be constant-initialized, but this is safe.
  static MetadataAllocator allocator;
  return allocator;
}
#endif

ClassMetadata *
swift::swift_relocateClassMetadata(ClassMetadata *self,
                                   size_t templateSize,
                                   size_t numImmediateMembers) {
  // Force the initialization of the metadata layout.
  (void) self->getDescription()->getMetadataBounds();

  const ClassMetadata *superclass = self->Superclass;

  size_t metadataSize;
  if (superclass && superclass->isTypeMetadata()) {
    metadataSize = (superclass->getClassSize() -
                    superclass->getClassAddressPoint() +
                    self->getClassAddressPoint() +
                    numImmediateMembers * sizeof(void *));
  } else {
    metadataSize = (templateSize +
                    numImmediateMembers * sizeof(void *));
  }

  if (templateSize < metadataSize) {
    auto rawNewClass = (char*) malloc(metadataSize);
    auto rawOldClass = (const char*) self;
    rawOldClass -= self->getClassAddressPoint();

    memcpy(rawNewClass, rawOldClass, templateSize);
    memset(rawNewClass + templateSize, 0,
           metadataSize - templateSize);

    rawNewClass += self->getClassAddressPoint();
    auto *newClass = (ClassMetadata *) rawNewClass;
    newClass->setClassSize(metadataSize);

    assert(newClass->isTypeMetadata());

    return newClass;
  }

  return self;
}

/// Initialize the field offset vector for a dependent-layout class, using the
/// "Universal" layout strategy.
void
swift::swift_initClassMetadata_UniversalStrategy(ClassMetadata *self,
                                                 size_t numFields,
                                           const TypeLayout * const *fieldTypes,
                                                 size_t *fieldOffsets) {
  _swift_initializeSuperclass(self);

  // Start layout by appending to a standard heap object header.
  size_t size, alignMask;

#if SWIFT_OBJC_INTEROP
  ClassROData *rodata = getROData(self);
#endif

  // If we have a superclass, start from its size and alignment instead.
  if (classHasSuperclass(self)) {
    const ClassMetadata *super = self->Superclass;

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

  // Always clone the ivar descriptors.
  if (numFields) {
    const ClassIvarList *dependentIvars = rodata->IvarList;
    assert(dependentIvars->Count == numFields);
    assert(dependentIvars->EntrySize == sizeof(ClassIvarEntry));

    auto ivarListSize = sizeof(ClassIvarList) +
                        numFields * sizeof(ClassIvarEntry);
    auto ivars = (ClassIvarList*) getResilientMetadataAllocator()
      .Allocate(ivarListSize, alignof(ClassIvarList));
    memcpy(ivars, dependentIvars, ivarListSize);
    rodata->IvarList = ivars;

    for (unsigned i = 0; i != numFields; ++i) {
      auto *eltLayout = fieldTypes[i];

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
      if (ivar.Size != eltLayout->size) {
        ivar.Size = eltLayout->size;
        ivar.Type = nullptr;
        ivar.Log2Alignment =
          getLog2AlignmentFromMask(eltLayout->flags.getAlignmentMask());
      }
    }
  }
#endif

  // Okay, now do layout.
  for (unsigned i = 0; i != numFields; ++i) {
    auto *eltLayout = fieldTypes[i];

    // Skip empty fields.
    if (fieldOffsets[i] == 0 && eltLayout->size == 0)
      continue;
    auto offset = roundUpToAlignMask(size,
                                     eltLayout->flags.getAlignmentMask());
    fieldOffsets[i] = offset;
    size = offset + eltLayout->size;
    alignMask = std::max(alignMask, eltLayout->flags.getAlignmentMask());
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

#define WANT_REQUIRED_VALUE_WITNESSES 1
#define WANT_EXTRA_INHABITANT_VALUE_WITNESSES 1
#define WANT_ENUM_VALUE_WITNESSES 0
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) \
  Data.LOWER_ID = Witnesses::LOWER_ID;
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)
#include "swift/ABI/ValueWitness.def"

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
    const Metadata *SuperclassConstraint;
    ProtocolClassConstraint ClassConstraint : 1;
    size_t NumProtocols : 31;
    const ProtocolDescriptor * const *Protocols;
  };

  ExistentialCacheEntry(Key key);

  intptr_t getKeyIntValueForDump() {
    return 0;
  }

  int compareWithKey(Key key) const {
    if (auto result = compareIntegers(key.ClassConstraint,
                                      Data.Flags.getClassConstraint()))
      return result;

    if (auto result = comparePointers(key.SuperclassConstraint,
                                      Data.getSuperclassConstraint()))
      return result;

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
    return (sizeof(const ProtocolDescriptor *) * key.NumProtocols +
            (key.SuperclassConstraint != nullptr
             ? sizeof(const Metadata *)
             : 0));
  }
  size_t getExtraAllocationSize() const {
    return (sizeof(const ProtocolDescriptor *) * Data.Protocols.NumProtocols +
            (Data.Flags.hasSuperclassConstraint()
             ? sizeof(const Metadata *)
             : 0));
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

/// The standard metadata for Any.
const FullMetadata<ExistentialTypeMetadata> swift::
METADATA_SYM(ANY_MANGLING) = {
  { &OpaqueExistentialValueWitnesses_0 }, // ValueWitnesses
  ExistentialTypeMetadata(
    ExistentialTypeFlags() // Flags
      .withNumWitnessTables(0)
      .withClassConstraint(ProtocolClassConstraint::Any)
      .withHasSuperclass(false)
      .withSpecialProtocol(SpecialProtocol::None)),
};

/// The standard metadata for AnyObject.
const FullMetadata<ExistentialTypeMetadata> swift::
METADATA_SYM(ANYOBJECT_MANGLING) = {
  {
#if SWIFT_OBJC_INTEROP
    &VALUE_WITNESS_SYM(BO)
#else
    &VALUE_WITNESS_SYM(Bo)
#endif
  },
  ExistentialTypeMetadata(
    ExistentialTypeFlags() // Flags
      .withNumWitnessTables(0)
      .withClassConstraint(ProtocolClassConstraint::Class)
      .withHasSuperclass(false)
      .withSpecialProtocol(SpecialProtocol::None)),
};

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

#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) \
  Data.LOWER_ID = Witnesses::LOWER_ID;
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)
#include "swift/ABI/ValueWitness.def"

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
getClassExistentialValueWitnesses(const Metadata *superclass,
                                  unsigned numWitnessTables) {
  // FIXME: If the superclass is not @objc, use native reference counting.
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

#define WANT_REQUIRED_VALUE_WITNESSES 1
#define WANT_EXTRA_INHABITANT_VALUE_WITNESSES 1
#define WANT_ENUM_VALUE_WITNESSES 0
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) \
  Data.LOWER_ID = Witnesses::LOWER_ID;
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)
#include "swift/ABI/ValueWitness.def"

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
                             const Metadata *superclassConstraint,
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
  case SpecialProtocol::None:
    break;
  }
  
  switch (classConstraint) {
  case ProtocolClassConstraint::Class:
    return getClassExistentialValueWitnesses(superclassConstraint,
                                             numWitnessTables);
  case ProtocolClassConstraint::Any:
    assert(superclassConstraint == nullptr);
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
  case ExistentialTypeRepresentation::Opaque: {
    // We can't take from a shared existential box without checking uniqueness.
    auto *opaque =
        reinterpret_cast<const OpaqueExistentialContainer *>(container);
    return opaque->isValueInline();
  }
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
    auto *opaque = reinterpret_cast<OpaqueExistentialContainer *>(container);
    opaque->deinit();
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
    auto *opaqueContainer =
      reinterpret_cast<const OpaqueExistentialContainer*>(container);
    return opaqueContainer->projectValue();
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

#ifndef NDEBUG
/// Determine whether any of the given protocols is class-bound.
static bool anyProtocolIsClassBound(
                                size_t numProtocols,
                                const ProtocolDescriptor * const *protocols) {
  for (unsigned i = 0; i != numProtocols; ++i) {
    if (protocols[i]->Flags.getClassConstraint()
          == ProtocolClassConstraint::Class)
      return true;
  }

  return false;
}
#endif

/// \brief Fetch a uniqued metadata for an existential type. The array
/// referenced by \c protocols will be sorted in-place.
const ExistentialTypeMetadata *
swift::swift_getExistentialTypeMetadata(
                                  ProtocolClassConstraint classConstraint,
                                  const Metadata *superclassConstraint,
                                  size_t numProtocols,
                                  const ProtocolDescriptor * const *protocols) {

  // The empty compositions Any and AnyObject have fixed metadata.
  if (numProtocols == 0 && !superclassConstraint) {
    switch (classConstraint) {
    case ProtocolClassConstraint::Any:
      return &METADATA_SYM(ANY_MANGLING);
    case ProtocolClassConstraint::Class:
      return &METADATA_SYM(ANYOBJECT_MANGLING);
    }
  }

  // We entrust that the compiler emitting the call to
  // swift_getExistentialTypeMetadata always sorts the `protocols` array using
  // a globally stable ordering that's consistent across modules.
  
  // Ensure that the "class constraint" bit is set whenever we have a
  // superclass or a one of the protocols is class-bound.
  assert(classConstraint == ProtocolClassConstraint::Class ||
         (!superclassConstraint &&
          !anyProtocolIsClassBound(numProtocols, protocols)));
  ExistentialCacheEntry::Key key = {
    superclassConstraint, classConstraint, numProtocols, protocols
  };
  return &ExistentialTypes.getOrInsert(key).first->Data;
}

ExistentialCacheEntry::ExistentialCacheEntry(Key key) {
  // Calculate the class constraint and number of witness tables for the
  // protocol set.
  unsigned numWitnessTables = 0;
  for (auto p : make_range(key.Protocols, key.Protocols + key.NumProtocols)) {
    if (p->Flags.needsWitnessTable())
      ++numWitnessTables;
  }

  // Get the special protocol kind for an uncomposed protocol existential.
  // Protocol compositions are currently never special.
  auto special = SpecialProtocol::None;
  if (key.NumProtocols == 1)
    special = key.Protocols[0]->Flags.getSpecialProtocol();

  Data.setKind(MetadataKind::Existential);
  Data.ValueWitnesses = getExistentialValueWitnesses(key.ClassConstraint,
                                                     key.SuperclassConstraint,
                                                     numWitnessTables,
                                                     special);
  Data.Flags = ExistentialTypeFlags()
    .withNumWitnessTables(numWitnessTables)
    .withClassConstraint(key.ClassConstraint)
    .withSpecialProtocol(special);

  if (key.SuperclassConstraint != nullptr) {
    Data.Flags = Data.Flags.withHasSuperclass(true);

    // Get a pointer to tail-allocated storage for this metadata record.
    auto Pointer = reinterpret_cast<
      const Metadata **>(&Data + 1);

    // The superclass immediately follows the list of protocol descriptors.
    Pointer[key.NumProtocols] = key.SuperclassConstraint;
  }

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
  /// A reference to a context descriptor, used as a uniquing key.
  struct ContextDescriptorKey {
    const TypeContextDescriptor *Data;
  };
} // end anonymous namespace

template <>
struct llvm::DenseMapInfo<ContextDescriptorKey> {
  static ContextDescriptorKey getEmptyKey() {
    return ContextDescriptorKey{(const TypeContextDescriptor*) 0};
  }
  static ContextDescriptorKey getTombstoneKey() {
    return ContextDescriptorKey{(const TypeContextDescriptor*) 1};
  }
  static unsigned getHashValue(ContextDescriptorKey val) {
    if ((uintptr_t)val.Data <= 1) {
      return llvm::hash_value(val.Data);
    }

    // Hash by name.
    // In full generality, we'd get a better hash by walking up the entire
    // descriptor tree and hashing names all along the way, and we'd be faster
    // if we special cased unique keys by hashing pointers. In practice, this
    // is only used to unique foreign metadata records, which only ever appear
    // in the "C" or "ObjC" special context, and are never unique.
    
    // llvm::hash_value(StringRef) is, unfortunately, defined out of
    // line in a library we otherwise would not need to link against.
    StringRef name(val.Data->Name.get());
    return llvm::hash_combine_range(name.begin(), name.end());
  }
  static bool isEqual(ContextDescriptorKey lhs, ContextDescriptorKey rhs) {
    if ((uintptr_t)lhs.Data <= 1 || (uintptr_t)rhs.Data <= 1) {
      return lhs.Data == rhs.Data;
    }
    return equalContexts(lhs.Data, rhs.Data);
  }
};

// We use a DenseMap over what are essentially StringRefs instead of a
// StringMap because we don't need to actually copy the string.
namespace {
struct ForeignTypeState {
  Mutex Lock;
  ConditionVariable InitializationWaiters;
  llvm::DenseMap<ContextDescriptorKey, const ForeignTypeMetadata *> Types;
};
} // end anonymous namespace

static Lazy<ForeignTypeState> ForeignTypes;

const ForeignTypeMetadata *
swift::swift_getForeignTypeMetadata(ForeignTypeMetadata *nonUnique) {
  // Fast path: check the invasive cache.
  auto cache = nonUnique->getCacheValue();
  if (cache.isInitialized()) {
    return cache.getCachedUniqueMetadata();
  }

  // Okay, check the global map.
  auto &foreignTypes = ForeignTypes.get();
  ContextDescriptorKey key{nonUnique->getTypeContextDescriptor()};
  assert(key.Data
         && "all foreign metadata should have a type context descriptor");
  bool hasInit = cache.hasInitializationFunction();

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

template <> OpaqueValue *Metadata::allocateBoxForExistentialIn(ValueBuffer *buffer) const {
  auto *vwt = getValueWitnesses();
  if (vwt->isValueInline())
    return reinterpret_cast<OpaqueValue *>(buffer);

  // Allocate the box.
  BoxPair refAndValueAddr(swift_allocBox(this));
  buffer->PrivateData[0] = refAndValueAddr.object;
  return refAndValueAddr.buffer;
}

template <> OpaqueValue *Metadata::allocateBufferIn(ValueBuffer *buffer) const {
  auto *vwt = getValueWitnesses();
  if (vwt->isValueInline())
    return reinterpret_cast<OpaqueValue *>(buffer);
  // Allocate temporary outline buffer.
  auto size = vwt->getSize();
  auto alignMask = vwt->getAlignmentMask();
  auto *ptr = swift_slowAlloc(size, alignMask);
  buffer->PrivateData[0] = ptr;
  return reinterpret_cast<OpaqueValue *>(ptr);
}

template <> void Metadata::deallocateBufferIn(ValueBuffer *buffer) const {
  auto *vwt = getValueWitnesses();
  if (vwt->isValueInline())
    return;
  auto size = vwt->getSize();
  auto alignMask = vwt->getAlignmentMask();
  swift_slowDealloc(buffer->PrivateData[0], size, alignMask);
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

StringRef swift::getStringForMetadataKind(MetadataKind kind) {
  switch (kind) {
#define METADATAKIND(NAME, VALUE) \
    case MetadataKind::NAME: \
      return #NAME;
#include "swift/ABI/MetadataKind.def"
  }

  swift_runtime_unreachable("Unhandled metadata kind?!");
}

#ifndef NDEBUG
template <> void Metadata::dump() const {
  printf("TargetMetadata.\n");
  printf("Kind: %s.\n", getStringForMetadataKind(getKind()).data());
  printf("Value Witnesses: %p.\n", getValueWitnesses());
  printf("Class Object: %p.\n", getClassObject());
  printf("Type Context Description: %p.\n", getTypeContextDescriptor());
  printf("Generic Args: %p.\n", getGenericArgs());
}
#endif

/***************************************************************************/
/*** Protocol witness tables ***********************************************/
/***************************************************************************/

namespace {

/// A cache-entry type suitable for use with LockingConcurrentMap.
class WitnessTableCacheEntry :
    public SimpleLockingCacheEntryBase<WitnessTableCacheEntry, WitnessTable*> {
  /// The type for which this table was instantiated.
  const Metadata * const Type;

  /// The generic table.  This is only kept around so that we can
  /// compute the size of an entry correctly in case of a race to
  /// allocate the entry.
  GenericWitnessTable * const GenericTable;

public:
  /// Do the structural initialization necessary for this entry to appear
  /// in a concurrent map.
  WitnessTableCacheEntry(const Metadata *type,
                         GenericWitnessTable *genericTable,
                         void ** const *instantiationArgs)
    : Type(type), GenericTable(genericTable) {}

  intptr_t getKeyIntValueForDump() const {
    return reinterpret_cast<intptr_t>(Type);
  }

  /// The key value of the entry is just its type pointer.
  int compareWithKey(const Metadata *type) const {
    return comparePointers(Type, type);
  }

  static size_t getExtraAllocationSize(const Metadata *type,
                                       GenericWitnessTable *genericTable,
                                       void ** const *instantiationArgs) {
    return getWitnessTableSize(genericTable);
  }

  size_t getExtraAllocationSize() const {
    return getWitnessTableSize(GenericTable);
  }

  static size_t getWitnessTableSize(GenericWitnessTable *genericTable) {
    auto protocol = genericTable->Protocol.get();
    size_t numPrivateWords = genericTable->WitnessTablePrivateSizeInWords;
    size_t numRequirementWords =
      WitnessTableFirstRequirementOffset + protocol->NumRequirements;
    return (numPrivateWords + numRequirementWords) * sizeof(void*);
  }

  WitnessTable *allocate(GenericWitnessTable *genericTable,
                         void ** const *instantiationArgs);
};

} // end anonymous namespace

using GenericWitnessTableCache =
  LockingConcurrentMap<WitnessTableCacheEntry, /*destructor*/ false>;
using LazyGenericWitnessTableCache = Lazy<GenericWitnessTableCache>;

/// Fetch the cache for a generic witness-table structure.
static GenericWitnessTableCache &getCache(GenericWitnessTable *gen) {
  // Keep this assert even if you change the representation above.
  static_assert(sizeof(LazyGenericWitnessTableCache) <=
                sizeof(GenericWitnessTable::PrivateDataType),
                "metadata cache is larger than the allowed space");

  auto lazyCache =
    reinterpret_cast<LazyGenericWitnessTableCache*>(gen->PrivateData.get());
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
      genericTable->WitnessTableSizeInWords ==
        (genericTable->Protocol->NumRequirements +
           WitnessTableFirstRequirementOffset)) {
    return true;
  }

  return false;
}

/// Instantiate a brand new witness table for a resilient or generic
/// protocol conformance.
WitnessTable *
WitnessTableCacheEntry::allocate(GenericWitnessTable *genericTable,
                                 void ** const *instantiationArgs) {
  // The number of witnesses provided by the table pattern.
  size_t numPatternWitnesses = genericTable->WitnessTableSizeInWords;

  auto protocol = genericTable->Protocol.get();

  // The number of mandatory requirements, i.e. requirements lacking
  // default implementations.
  assert(numPatternWitnesses >= protocol->NumMandatoryRequirements +
                                    WitnessTableFirstRequirementOffset);

  // The total number of requirements.
  size_t numRequirements =
    protocol->NumRequirements + WitnessTableFirstRequirementOffset;
  assert(numPatternWitnesses <= numRequirements);

  // Number of bytes for any private storage used by the conformance itself.
  size_t privateSizeInWords = genericTable->WitnessTablePrivateSizeInWords;

  // Find the allocation.
  void **fullTable = reinterpret_cast<void**>(this + 1);

  // Zero out the private storage area.
  memset(fullTable, 0, privateSizeInWords * sizeof(void*));

  // Advance the address point; the private storage area is accessed via
  // negative offsets.
  auto table = fullTable + privateSizeInWords;
  auto pattern = reinterpret_cast<void * const *>(&*genericTable->Pattern);
  auto requirements = protocol->Requirements.get();

  // Fill in the provided part of the requirements from the pattern.
  for (size_t i = 0, e = numPatternWitnesses; i < e; ++i) {
    table[i] = pattern[i];
  }

  // Fill in any default requirements.
  for (size_t i = numPatternWitnesses, e = numRequirements; i < e; ++i) {
    size_t requirementIndex = i - WitnessTableFirstRequirementOffset;
    void *defaultImpl =
      requirements[requirementIndex].DefaultImplementation.get();
    assert(defaultImpl &&
           "no default implementation for missing requirement");
    table[i] = defaultImpl;
  }

  auto castTable = reinterpret_cast<WitnessTable*>(table);

  // Call the instantiation function if present.
  if (!genericTable->Instantiator.isNull()) {
    genericTable->Instantiator(castTable, Type, instantiationArgs);
  }

  return castTable;
}

const WitnessTable *
swift::swift_getGenericWitnessTable(GenericWitnessTable *genericTable,
                                    const Metadata *type,
                                    void **const *instantiationArgs) {
  if (doesNotRequireInstantiation(genericTable)) {
    return genericTable->Pattern;
  }

  auto &cache = getCache(genericTable);
  auto result = cache.getOrInsert(type, genericTable, instantiationArgs);

  // Our returned 'status' is the witness table itself.
  return result.second;
}

/***************************************************************************/
/*** Recursive metadata dependencies ***************************************/
/***************************************************************************/

template <class Result, class Callbacks>
static Result performOnMetadataCache(Metadata *metadata,
                                     Callbacks &&callbacks) {
  // Handle different kinds of type that can delay their metadata.
  const TypeContextDescriptor *description;
  if (auto classMetadata = dyn_cast<ClassMetadata>(metadata)) {
    description = classMetadata->getDescription();
  } else {
    auto valueMetadata = cast<ValueMetadata>(metadata);
    description = valueMetadata->getDescription();
  }

  assert(description->isGeneric() &&
         "only generic metadata can delay evaluation for now");
  auto &generics = description->getFullGenericContextHeader();

  auto genericArgs =
    reinterpret_cast<const void * const *>(
                                    description->getGenericArguments(metadata));
  size_t numGenericArgs = generics.Base.NumKeyArguments;
  auto key = MetadataCacheKey(genericArgs, numGenericArgs);

  return std::move(callbacks).forGenericMetadata(metadata, description,
                                                 getCache(generics), key);
}

bool swift::addToMetadataQueue(
                  std::unique_ptr<MetadataCompletionQueueEntry> &&queueEntry,
                  Metadata *dependency,
                  MetadataRequest::BasicKind dependencyRequirement) {
  struct EnqueueCallbacks {
    std::unique_ptr<MetadataCompletionQueueEntry> &&QueueEntry;

    bool forGenericMetadata(Metadata *metadata,
                            const TypeContextDescriptor *description,
                            GenericMetadataCache &cache,
                            MetadataCacheKey key) && {
      return cache.enqueue(key, std::move(QueueEntry));
    }
  } callbacks = { std::move(queueEntry) };

  // Set the requirement.
  queueEntry->DependencyRequirement = dependencyRequirement;

  return performOnMetadataCache<bool>(dependency, std::move(callbacks));
}

void swift::resumeMetadataCompletion(
                   std::unique_ptr<MetadataCompletionQueueEntry> &&queueEntry) {
  struct ResumeCallbacks {
    std::unique_ptr<MetadataCompletionQueueEntry> &&QueueEntry;

    static MetadataRequest getRequest() {
      return MetadataRequest(MetadataRequest::Complete,
                             /*non-blocking*/ true);
    }

    void forGenericMetadata(Metadata *metadata,
                            const TypeContextDescriptor *description,
                            GenericMetadataCache &cache,
                            MetadataCacheKey key) && {
      cache.resumeInitialization(key, std::move(QueueEntry),
                                 getRequest(),
                                 description,
                                 // This array comes from the metadata, so
                                 // we can just "unslice" it to get the
                                 // non-key arguments.
                                 key.KeyData.begin());
    }
  } callbacks = { std::move(queueEntry) };

  auto metadata = queueEntry->Value;
  performOnMetadataCache<void>(metadata, std::move(callbacks));
}

/***************************************************************************/
/*** Allocator implementation **********************************************/
/***************************************************************************/

namespace {
  struct PoolRange {
    static constexpr uintptr_t PageSize = 16 * 1024;
    static constexpr uintptr_t MaxPoolAllocationSize = PageSize / 2;

    /// The start of the allocation.
    char *Begin;

    /// The number of bytes remaining.
    size_t Remaining;
  };
} // end anonymous namespace

// A statically-allocated pool.  It's zero-initialized, so this
// doesn't cost us anything in binary size.
LLVM_ALIGNAS(alignof(void*)) static char InitialAllocationPool[64*1024];
static std::atomic<PoolRange>
AllocationPool{PoolRange{InitialAllocationPool,
                         sizeof(InitialAllocationPool)}};

void *MetadataAllocator::Allocate(size_t size, size_t alignment) {
  assert(alignment <= alignof(void*));
  assert(size % alignof(void*) == 0);

  // If the size is larger than the maximum, just use malloc.
  if (size > PoolRange::MaxPoolAllocationSize)
    return malloc(size);

  // Allocate out of the pool.
  PoolRange curState = AllocationPool.load(std::memory_order_relaxed);
  while (true) {
    char *allocation;
    PoolRange newState;
    bool allocatedNewPage;

    // Try to allocate out of the current page.
    if (size <= curState.Remaining) {
      allocatedNewPage = false;
      allocation = curState.Begin;
      newState = PoolRange{curState.Begin + size, curState.Remaining - size};
    } else {
      allocatedNewPage = true;
      allocation = new char[PoolRange::PageSize];
      newState = PoolRange{allocation + size, PoolRange::PageSize - size};
      __asan_poison_memory_region(allocation, PoolRange::PageSize);
    }

    // Swap in the new state.
    if (std::atomic_compare_exchange_weak_explicit(&AllocationPool,
                                                   &curState, newState,
                                              std::memory_order_relaxed,
                                              std::memory_order_relaxed)) {
      // If that succeeded, we've successfully allocated.
      __msan_allocated_memory(allocation, size);
      __asan_unpoison_memory_region(allocation, size);
      return allocation;
    }

    // If it failed, go back to a neutral state and try again.
    if (allocatedNewPage) {
      delete[] allocation;
    }
  }
}

void MetadataAllocator::Deallocate(const void *allocation, size_t size) {
  __asan_poison_memory_region(allocation, size);

  if (size > PoolRange::MaxPoolAllocationSize) {
    free(const_cast<void*>(allocation));
    return;
  }

  // Check whether the allocation pool is still in the state it was in
  // immediately after the given allocation.
  PoolRange curState = AllocationPool.load(std::memory_order_relaxed);
  if (reinterpret_cast<const char*>(allocation) + size != curState.Begin) {
    return;
  }

  // Try to swap back to the pre-allocation state.  If this fails,
  // don't bother trying again; we'll just leak the allocation.
  PoolRange newState = { reinterpret_cast<char*>(const_cast<void*>(allocation)),
                         curState.Remaining + size };
  (void)
    std::atomic_compare_exchange_strong_explicit(&AllocationPool,
                                                 &curState, newState,
                                                 std::memory_order_relaxed,
                                                 std::memory_order_relaxed);
}

void *swift::allocateMetadata(size_t size, size_t alignment) {
  return MetadataAllocator().Allocate(size, alignment);
}

template<>
bool Metadata::satisfiesClassConstraint() const {
  // existential types marked with @objc satisfy class requirement.
  if (auto *existential = dyn_cast<ExistentialTypeMetadata>(this))
    return existential->isObjC();

  // or it's a class.
  return isAnyClass();
}
