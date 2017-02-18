//===--- Casting.cpp - Swift Language Dynamic Casting Support -------------===//
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
// Implementations of the dynamic cast runtime functions.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "swift/Basic/Demangle.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/Enum.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/Unreachable.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/Compiler.h"
#include "swift/Runtime/Debug.h"
#include "ErrorObject.h"
#include "ExistentialMetadataImpl.h"
#include "Private.h"
#include "SwiftHashableSupport.h"
#include "../SwiftShims/RuntimeShims.h"
#include "stddef.h"
#if SWIFT_OBJC_INTEROP
#include "swift/Runtime/ObjCBridge.h"
#include "SwiftValue.h"
#endif

#include <cstring>
#include <type_traits>

using namespace swift;
using namespace swift::hashable_support;
using namespace metadataimpl;

#if SWIFT_OBJC_INTEROP
#include <objc/NSObject.h>
#include <objc/runtime.h>
#include <objc/message.h>
#include <objc/objc.h>

// Aliases for Objective-C runtime entry points.
static const char *class_getName(const ClassMetadata* type) {
  return class_getName(
    reinterpret_cast<Class>(const_cast<ClassMetadata*>(type)));
}

// Aliases for Swift runtime entry points for Objective-C types.
extern "C" const void *swift_dynamicCastObjCProtocolConditional(
                         const void *object,
                         size_t numProtocols,
                         const ProtocolDescriptor * const *protocols);
#endif

// Build a user-comprehensible name for a type.
static void _buildNameForMetadata(const Metadata *type,
                                  bool qualified,
                                  std::string &result) {
#if SWIFT_OBJC_INTEROP
  if (type->getKind() == MetadataKind::Class) {
    auto classType = static_cast<const ClassMetadata *>(type);
    // Look through artificial subclasses.
    while (classType->isTypeMetadata() && classType->isArtificialSubclass())
      classType = classType->SuperClass;

    // Ask the Objective-C runtime to name ObjC classes.
    if (!classType->isTypeMetadata()) {
      result += class_getName(classType);
      return;
    }
  } else if (type->getKind() == MetadataKind::ObjCClassWrapper) {
    auto objcWrapper = static_cast<const ObjCClassWrapperMetadata *>(type);
    const char *className = class_getName((Class)objcWrapper->Class);
    result = className;
    return;
  }
#endif

  // Use the remangler to generate a mangled name from the type metadata.
  auto demangling = _swift_buildDemanglingForMetadata(type);
  if (demangling == nullptr) {
    result = "<<< invalid type >>>";
    return;
  }

  Demangle::DemangleOptions options;
  options.QualifyEntities = qualified;
  result = Demangle::nodeToString(demangling, options);
}

/// Return a user-comprehensible name for the given type.
std::string swift::nameForMetadata(const Metadata *type,
                                   bool qualified) {
  std::string result;
  _buildNameForMetadata(type, qualified, result);
  return result;
}

SWIFT_CC(swift)
TwoWordPair<const char *, uintptr_t>::Return
swift::swift_getTypeName(const Metadata *type, bool qualified) {
  using Pair = TwoWordPair<const char *, uintptr_t>;
  using Key = llvm::PointerIntPair<const Metadata *, 1, bool>;

  static StaticReadWriteLock TypeNameCacheLock;
  static Lazy<llvm::DenseMap<Key, std::pair<const char *, size_t>>>
    TypeNameCache;
  
  Key key(type, qualified);
  auto &cache = TypeNameCache.get();

  // Attempt read-only lookup of cache entry.
  {
    StaticScopedReadLock guard(TypeNameCacheLock);

    auto found = cache.find(key);
    if (found != cache.end()) {
      auto result = found->second;
      return Pair{result.first, result.second};
    }
  }

  // Read-only lookup failed to find item, we may need to create it.
  {
    StaticScopedWriteLock guard(TypeNameCacheLock);

    // Do lookup again just to make sure it wasn't created by another
    // thread before we acquired the write lock.
    auto found = cache.find(key);
    if (found != cache.end()) {
      auto result = found->second;
      return Pair{result.first, result.second};
    }

    // Build the metadata name.
    auto name = nameForMetadata(type, qualified);
    // Copy it to memory we can reference forever.
    auto size = name.size();
    auto result = (char *)malloc(size + 1);
    memcpy(result, name.data(), size);
    result[size] = 0;

    cache.insert({key, {result, size}});
    return Pair{result, size};
  }
}

/// Report a dynamic cast failure.
// This is noinline with asm("") to preserve this frame in stack traces.
// We want "dynamicCastFailure" to appear in crash logs even we crash 
// during the diagnostic because some Metadata is invalid.
LLVM_ATTRIBUTE_NORETURN
LLVM_ATTRIBUTE_NOINLINE
void 
swift::swift_dynamicCastFailure(const void *sourceType, const char *sourceName, 
                                const void *targetType, const char *targetName, 
                                const char *message) {
  asm("");

  swift::fatalError(/* flags = */ 0,
                    "Could not cast value of type '%s' (%p) to '%s' (%p)%s%s\n",
                    sourceName, sourceType, 
                    targetName, targetType, 
                    message ? ": " : ".", 
                    message ? message : "");
}

LLVM_ATTRIBUTE_NORETURN
void 
swift::swift_dynamicCastFailure(const Metadata *sourceType,
                                const Metadata *targetType, 
                                const char *message) {
  std::string sourceName = nameForMetadata(sourceType);
  std::string targetName = nameForMetadata(targetType);

  swift_dynamicCastFailure(sourceType, sourceName.c_str(), 
                           targetType, targetName.c_str(), message);
}

#if SWIFT_OBJC_INTEROP
// Objective-C bridging helpers.
namespace {
  struct _ObjectiveCBridgeableWitnessTable;
}
static const _ObjectiveCBridgeableWitnessTable *
findBridgeWitness(const Metadata *T);

static bool _dynamicCastValueToClassViaObjCBridgeable(
              OpaqueValue *dest,
              OpaqueValue *src,
              const Metadata *srcType,
              const Metadata *targetType,
              const _ObjectiveCBridgeableWitnessTable *srcBridgeWitness,
              DynamicCastFlags flags);

static bool _dynamicCastValueToClassExistentialViaObjCBridgeable(
              OpaqueValue *dest,
              OpaqueValue *src,
              const Metadata *srcType,
              const ExistentialTypeMetadata *targetType,
              const _ObjectiveCBridgeableWitnessTable *srcBridgeWitness,
              DynamicCastFlags flags);

static bool _dynamicCastClassToValueViaObjCBridgeable(
               OpaqueValue *dest,
               OpaqueValue *src,
               const Metadata *srcType,
               const Metadata *targetType,
               const _ObjectiveCBridgeableWitnessTable *targetBridgeWitness,
               DynamicCastFlags flags);
#endif

/// A convenient method for failing out of a dynamic cast.
static bool _fail(OpaqueValue *srcValue, const Metadata *srcType,
                  const Metadata *targetType, DynamicCastFlags flags,
                  const Metadata *srcDynamicType = nullptr) {
  if (flags & DynamicCastFlags::Unconditional) {
    const Metadata *srcTypeToReport =
        srcDynamicType ? srcDynamicType
                       : srcType;
    swift_dynamicCastFailure(srcTypeToReport, targetType);
  }
  if (flags & DynamicCastFlags::DestroyOnFailure)
    srcType->vw_destroy(srcValue);
  return false;
}

/// A convenient method for succeeding at a dynamic cast.
static bool _succeed(OpaqueValue *dest, OpaqueValue *src,
                     const Metadata *srcType, DynamicCastFlags flags) {
  if (flags & DynamicCastFlags::TakeOnSuccess) {
    srcType->vw_initializeWithTake(dest, src);
  } else {
    srcType->vw_initializeWithCopy(dest, src);
  }
  return true;
}

/// Dynamically cast a class metatype to a Swift class metatype.
static const ClassMetadata *
_dynamicCastClassMetatype(const ClassMetadata *sourceType,
                          const ClassMetadata *targetType) {
  do {
    if (sourceType == targetType) {
      return sourceType;
    }
    sourceType = _swift_getSuperclass(sourceType);
  } while (sourceType);
  
  return nullptr;
}

/// Dynamically cast a class instance to a Swift class type.
const void *swift::swift_dynamicCastClass(const void *object,
                                          const ClassMetadata *targetType)
    SWIFT_CC(RegisterPreservingCC_IMPL) {
#if SWIFT_OBJC_INTEROP
  assert(!targetType->isPureObjC());

  // Swift native classes never have a tagged-pointer representation.
  if (isObjCTaggedPointerOrNull(object)) {
    return nullptr;
  }
#endif

  auto isa = _swift_getClassOfAllocated(object);

  if (_dynamicCastClassMetatype(isa, targetType))
    return object;
  return nullptr;
}

/// Dynamically cast a class object to a Swift class type.
const void *
swift::swift_dynamicCastClassUnconditional(const void *object,
                                           const ClassMetadata *targetType) {
  auto value = swift_dynamicCastClass(object, targetType);
  if (value) return value;

  swift_dynamicCastFailure(_swift_getClass(object), targetType);
}

#if SWIFT_OBJC_INTEROP
static bool _unknownClassConformsToObjCProtocol(const OpaqueValue *value,
                                          const ProtocolDescriptor *protocol) {
  const void *object
    = *reinterpret_cast<const void * const *>(value);
  return swift_dynamicCastObjCProtocolConditional(object, 1, &protocol);
}
#endif

/// Check whether a type conforms to a protocol.
///
/// \param value - can be null, in which case the question should
///   be answered abstractly if possible
/// \param conformance - if non-null, and the protocol requires a
///   witness table, and the type implements the protocol, the witness
///   table will be placed here
static bool _conformsToProtocol(const OpaqueValue *value,
                                const Metadata *type,
                                const ProtocolDescriptor *protocol,
                                const WitnessTable **conformance) {
  // Handle AnyObject directly.
  if (protocol->Flags.getSpecialProtocol() == SpecialProtocol::AnyObject) {
    switch (type->getKind()) {
    case MetadataKind::Class:
    case MetadataKind::ObjCClassWrapper:
    case MetadataKind::ForeignClass:
      // Classes conform to AnyObject.
      return true;

    case MetadataKind::Existential: {
      auto sourceExistential = cast<ExistentialTypeMetadata>(type);
      // The existential conforms to AnyObject if it's class-constrained.
      // FIXME: It also must not carry witness tables.
      return sourceExistential->isClassBounded();
    }
      
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Metatype:
    case MetadataKind::Function:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::HeapGenericLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Enum:
    case MetadataKind::Optional:
    case MetadataKind::Opaque:
    case MetadataKind::Struct:
    case MetadataKind::Tuple:
      return false;
    }
    _failCorruptType(type);
  }

  // Look up the witness table for protocols that need them.
  if (protocol->Flags.needsWitnessTable()) {
    auto witness = swift_conformsToProtocol(type, protocol);
    if (!witness)
      return false;
    if (conformance)
      *conformance = witness;
    return true;
  }

  // For Objective-C protocols, check whether we have a class that
  // conforms to the given protocol.
  switch (type->getKind()) {
  case MetadataKind::Class:
#if SWIFT_OBJC_INTEROP
    if (value) {
      return _unknownClassConformsToObjCProtocol(value, protocol);
    } else {
      return classConformsToObjCProtocol(type, protocol);
    }
#endif
    return false;

  case MetadataKind::ObjCClassWrapper: {
#if SWIFT_OBJC_INTEROP
    if (value) {
      return _unknownClassConformsToObjCProtocol(value, protocol);
    } else {
      auto wrapper = cast<ObjCClassWrapperMetadata>(type);
      return classConformsToObjCProtocol(wrapper->Class, protocol);
    }
#endif
    return false;
  }

  case MetadataKind::ForeignClass:
#if SWIFT_OBJC_INTEROP
    if (value)
      return _unknownClassConformsToObjCProtocol(value, protocol);
    return false;
#else
    _failCorruptType(type);
#endif

  case MetadataKind::Existential: // FIXME
  case MetadataKind::ExistentialMetatype: // FIXME
  case MetadataKind::Function:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    return false;
  }

  return false;
}

/// Check whether a type conforms to the given protocols, filling in a
/// list of conformances.
static bool _conformsToProtocols(const OpaqueValue *value,
                                 const Metadata *type,
                                 const ProtocolDescriptorList &protocols,
                                 const WitnessTable **conformances) {
  for (unsigned i = 0, n = protocols.NumProtocols; i != n; ++i) {
    const ProtocolDescriptor *protocol = protocols[i];
    if (!_conformsToProtocol(value, type, protocol, conformances))
      return false;
    if (protocol->Flags.needsWitnessTable()) {
      assert(*conformances != nullptr);
      ++conformances;
    }
  }
  
  return true;
}

static bool shouldDeallocateSource(bool castSucceeded, DynamicCastFlags flags) {
  return (castSucceeded && (flags & DynamicCastFlags::TakeOnSuccess)) ||
        (!castSucceeded && (flags & DynamicCastFlags::DestroyOnFailure));
}

/// Given that a cast operation is complete, maybe deallocate an
/// opaque existential value.
static void _maybeDeallocateOpaqueExistential(OpaqueValue *srcExistential,
                                              bool castSucceeded,
                                              DynamicCastFlags flags) {
  if (shouldDeallocateSource(castSucceeded, flags)) {
    auto container =
      reinterpret_cast<OpaqueExistentialContainer *>(srcExistential);
    container->Type->vw_deallocateBuffer(&container->Buffer);
  }
}

static bool
isAnyObjectExistentialType(const ExistentialTypeMetadata *targetType) {
  unsigned numProtos =  targetType->Protocols.NumProtocols;
  if (numProtos != 1)
    return false;
  const ProtocolDescriptor *protocol = targetType->Protocols[0];
  bool isAnyObjectProtocol =
      protocol->Flags.getSpecialProtocol() == SpecialProtocol::AnyObject;
  // Assert that AnyObject does not need any witness tables. We rely on this.
  assert(!isAnyObjectProtocol || !protocol->Flags.needsWitnessTable() &&
         "AnyObject should not require witness tables");
  return isAnyObjectProtocol;
}

/// Given a possibly-existential value, find its dynamic type and the
/// address of its storage.
static void
findDynamicValueAndType(OpaqueValue *value, const Metadata *type,
                        OpaqueValue *&outValue, const Metadata *&outType,
                        bool &inoutCanTake,
                        bool isTargetTypeAnyObject,
                        bool isTargetExistentialMetatype) {
  switch (type->getKind()) {
  case MetadataKind::Class:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass: {
    // TODO: avoid unnecessary repeat lookup of
    // ObjCClassWrapper/ForeignClass when the type matches.
    outValue = value;
    outType = swift_getObjectType(*reinterpret_cast<HeapObject**>(value));
    return;
  }

  case MetadataKind::Existential: {
    // We can't drill through existential containers unless the result is an
    // existential metatype.
    if (!isTargetExistentialMetatype) {
      outValue = value;
      outType = type;
      return;
    }
    auto existentialType = cast<ExistentialTypeMetadata>(type);
    
    switch (existentialType->getRepresentation()) {
    case ExistentialTypeRepresentation::Class: {
      // Class existentials can't recursively contain existential containers,
      // so we can fast-path by not bothering to recur.
      auto existential =
        reinterpret_cast<ClassExistentialContainer*>(value);
      outValue = (OpaqueValue*) &existential->Value;
      outType = swift_getObjectType((HeapObject*) existential->Value);
      return;
    }
    
    case ExistentialTypeRepresentation::Opaque:
    case ExistentialTypeRepresentation::Error: {
      const Metadata *innerType = existentialType->getDynamicType(value);

      // Short cut class in existential as AnyObject casts.
      if (isTargetTypeAnyObject  &&
          innerType->getKind() == MetadataKind::Class) {
        // inline value buffer storage.
        outValue = value;
        outType = 0;
        inoutCanTake = true;
        return;
      }
      OpaqueValue *innerValue
        = existentialType->projectValue(value);
      inoutCanTake &= existentialType->mayTakeValue(value);

      return findDynamicValueAndType(innerValue, innerType,
                                     outValue, outType, inoutCanTake, false,
                                     isTargetExistentialMetatype);
    }
    }
  }
    
  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype: {
    auto storedType = *(const Metadata **) value;
    outValue = value;
    outType = swift_getMetatypeMetadata(storedType);
    return;
  }

  // Non-polymorphic types.
  case MetadataKind::Function:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    outValue = value;
    outType = type;
    return;
  }
  _failCorruptType(type);
}

extern "C" const Metadata *
swift::swift_getDynamicType(OpaqueValue *value, const Metadata *self,
                            bool existentialMetatype) {
  OpaqueValue *outValue;
  const Metadata *outType;
  bool canTake = false;
  findDynamicValueAndType(value, self, outValue, outType, canTake,
                          /*isAnyObject*/ false,
                          existentialMetatype);
  return outType;
}

/// Given a possibly-existential value, deallocate any buffer in its storage.
static void deallocateDynamicValue(OpaqueValue *value, const Metadata *type) {
  switch (type->getKind()) {
  case MetadataKind::Existential: {
    auto existentialType = cast<ExistentialTypeMetadata>(type);
    
    switch (existentialType->getRepresentation()) {
    case ExistentialTypeRepresentation::Class:
      // Nothing to clean up.
      break;
      
    case ExistentialTypeRepresentation::Error:
      // TODO: We could clean up from a reclaimed uniquely-referenced error box.
      break;
      
    case ExistentialTypeRepresentation::Opaque:
      auto existential =
        reinterpret_cast<OpaqueExistentialContainer*>(value);

      // Handle the possibility of nested existentials.
      OpaqueValue *existentialValue =
        existential->Type->vw_projectBuffer(&existential->Buffer);
      deallocateDynamicValue(existentialValue, existential->Type);

      // Deallocate the buffer.
      existential->Type->vw_deallocateBuffer(&existential->Buffer);
      break;
    }
    return;
  }

  // None of the rest of these require deallocation.
  case MetadataKind::Class:
  case MetadataKind::ForeignClass:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    return;
  }
  _failCorruptType(type);
}

#if SWIFT_OBJC_INTEROP
SWIFT_CC(c) SWIFT_RUNTIME_EXPORT
id
swift_dynamicCastMetatypeToObjectConditional(const Metadata *metatype) {
  switch (metatype->getKind()) {
  case MetadataKind::Class:
    // Swift classes are objects in and of themselves.
    return (id)metatype;

  case MetadataKind::ObjCClassWrapper: {
    // Unwrap ObjC class objects.
    auto wrapper = static_cast<const ObjCClassWrapperMetadata*>(metatype);
    return (id)wrapper->getClassObject();
  }
  
  // Other kinds of metadata don't cast to AnyObject.
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::ForeignClass:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
    return nullptr;
  }
}

SWIFT_CC(c) SWIFT_RUNTIME_EXPORT
id
swift_dynamicCastMetatypeToObjectUnconditional(const Metadata *metatype) {
  switch (metatype->getKind()) {
  case MetadataKind::Class:
    // Swift classes are objects in and of themselves.
    return (id)metatype;

  case MetadataKind::ObjCClassWrapper: {
    // Unwrap ObjC class objects.
    auto wrapper = static_cast<const ObjCClassWrapperMetadata*>(metatype);
    return (id)wrapper->getClassObject();
  }
  
  // Other kinds of metadata don't cast to AnyObject.
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::ForeignClass:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject: {
    std::string sourceName = nameForMetadata(metatype);
    swift_dynamicCastFailure(metatype, sourceName.c_str(),
                             nullptr, "AnyObject",
                         "only class metatypes can be converted to AnyObject");
  }
  }
}

// @_silgen_name("swift_stdlib_getErrorEmbeddedNSErrorIndirect")
// public func _stdlib_getErrorEmbeddedNSErrorIndirect<T : Error>(
///    _ x: UnsafePointer<T>) -> AnyObject?
SWIFT_CC(swift)
extern "C" id swift_stdlib_getErrorEmbeddedNSErrorIndirect(
                const OpaqueValue *error,
                const Metadata *T,
                const WitnessTable *Error);

#endif

/******************************************************************************/
/******************************** AnyHashable *********************************/
/******************************************************************************/

/// Nominal type descriptor for Swift.AnyHashable.
extern "C" const NominalTypeDescriptor STRUCT_TYPE_DESCR_SYM(s11AnyHashable);

static bool isAnyHashableType(const StructMetadata *type) {
  return type->getDescription() == &STRUCT_TYPE_DESCR_SYM(s11AnyHashable);
}

static bool isAnyHashableType(const Metadata *type) {
  if (auto structType = dyn_cast<StructMetadata>(type)) {
    return isAnyHashableType(structType);
  }
  return false;
}

/// Perform a dynamic cast from a nominal type to AnyHashable.
static bool _dynamicCastToAnyHashable(OpaqueValue *destination,
                                      OpaqueValue *source,
                                      const Metadata *sourceType,
                                      const Metadata *targetType,
                                      DynamicCastFlags flags) {
  // Look for a conformance to Hashable.
  auto hashableConformance = reinterpret_cast<const HashableWitnessTable *>(
      swift_conformsToProtocol(sourceType, &HashableProtocolDescriptor));

  // If we don't find one, the cast fails.
  if (!hashableConformance) {
    return _fail(source, sourceType, targetType, flags);
  }

  // If we do find one, the cast succeeds.

  // The intrinsic wants the value at +1, so we have to copy it into
  // a temporary.
  ValueBuffer buffer;
  bool mustDeallocBuffer = false;
  if (!(flags & DynamicCastFlags::TakeOnSuccess)) {
    source = sourceType->vw_initializeBufferWithCopy(&buffer, source);
    mustDeallocBuffer = true;
  }

  // Initialize the destination.
  _swift_convertToAnyHashableIndirect(source, destination,
                                      sourceType, hashableConformance);

  // Deallocate the buffer if we used it.
  if (mustDeallocBuffer) {
    sourceType->vw_deallocateBuffer(&buffer);
  }

  // The cast succeeded.
  return true;
}

/// Perform a dynamic cast to an arbitrary type from AnyHashable.
static bool _dynamicCastFromAnyHashable(OpaqueValue *destination,
                                        OpaqueValue *source,
                                        const Metadata *sourceType,
                                        const Metadata *targetType,
                                        DynamicCastFlags flags) {
  // Perform a conditional, non-consuming cast into the destination.
  if (_swift_anyHashableDownCastConditionalIndirect(source, destination,
                                                    targetType)) {
    // If that succeeded, and we were supposed to claim the source,
    // destroy it now.
    if (flags & DynamicCastFlags::TakeOnSuccess) {
      sourceType->vw_destroy(source);
    }

    return true;
  }

  return _fail(source, sourceType, targetType, flags);
}

/******************************************************************************/
/******************************** Existentials ********************************/
/******************************************************************************/

/// Perform a dynamic cast to an existential type.
static bool _dynamicCastToExistential(OpaqueValue *dest,
                                      OpaqueValue *src,
                                      const Metadata *srcType,
                                      const ExistentialTypeMetadata *targetType,
                                      DynamicCastFlags flags) {
#if SWIFT_OBJC_INTEROP
  // This variable's lifetime needs to be for the whole function, but is
  // only valid with Objective-C interop enabled.
  id tmp;
#endif
  // Find the actual type of the source.
  OpaqueValue *srcDynamicValue;
  const Metadata *srcDynamicType;
  bool canConsumeDynamicValue = true;

  // Are we casting to AnyObject? In this case we can fast-path casts from
  // classes.
  bool isTargetTypeAnyObject =
      targetType->getKind() == MetadataKind::Existential &&
      isAnyObjectExistentialType(targetType);

  // We don't care what the target type of a cast from class to AnyObject is.
  // srcDynamicType will be set to a nullptr in this case to save a lookup.
  findDynamicValueAndType(src, srcType, srcDynamicValue, srcDynamicType,
                          canConsumeDynamicValue, isTargetTypeAnyObject,
                          /*isExistentialMetatype*/ true);

  // Recursive casts on the dynamic value should not destroy the source
  // if findDynamicValueAndType doesn't allow it.
  DynamicCastFlags dynamicFlags = flags;
  if (!canConsumeDynamicValue) {
    dynamicFlags = dynamicFlags - (DynamicCastFlags::TakeOnSuccess |
                                   DynamicCastFlags::DestroyOnFailure);
  }

  // Given that we performed a cast on srcDynamicValue and obeyed
  // dynamicFlags there, clean up after src.
  auto maybeDeallocateSource = [&](bool success) {
    // If the flags don't say to destroy src, we're done.
    if (!shouldDeallocateSource(success, flags)) return;

    // If findDynamicValueAndType didn't give us a consumable interior
    // value, then src is still completely intact.
    if (!canConsumeDynamicValue) {
      srcType->vw_destroy(src);

    // Otherwise, if we destroyed an interior value, we need to clean
    // up any leftover buffers it may have been contained in.
    } else if (src != srcDynamicValue) {
      deallocateDynamicValue(src, srcType);
    }
  };

  // A function to call if the dynamic type does not conform to the
  // protocols.  Typically fails, but if the source type is AnyHashable,
  // tries to unwrap that.
  auto fallbackForNonDirectConformance = [&] {
    // As a fallback, if the source type is AnyHashable, perform a cast
    // on its interior value.
    if (isAnyHashableType(srcDynamicType)) {
      bool success =
        _dynamicCastFromAnyHashable(dest, srcDynamicValue, srcDynamicType,
                                    targetType, dynamicFlags);
      maybeDeallocateSource(success);
      return success;
    }

    return _fail(src, srcType, targetType, flags, srcDynamicType);
  };

  // The representation of an existential is different for some protocols.
  switch (targetType->getRepresentation()) {
  case ExistentialTypeRepresentation::Class: {
    auto destExistential =
      reinterpret_cast<ClassExistentialContainer*>(dest);
    MetadataKind kind =
        srcDynamicType ? srcDynamicType->getKind() : MetadataKind::Class;

    // A fallback to use if we don't have a more specialized approach
    // for a non-class type.
    auto fallbackForNonClass = [&] {
#if SWIFT_OBJC_INTEROP
      // If the destination type is a set of protocols that SwiftValue
      // implements, we're fine.
      if (findSwiftValueConformances(targetType->Protocols,
                                     destExistential->getWitnessTables())) {
        bool consumeValue = dynamicFlags & DynamicCastFlags::TakeOnSuccess;
        destExistential->Value =
          bridgeAnythingToSwiftValueObject(srcDynamicValue, srcDynamicType,
                                           consumeValue);
        maybeDeallocateSource(true);
        return true;
      }
#endif

      return _fail(src, srcType, targetType, flags);
    };

    // If the source type is a value type, it cannot possibly conform
    // to a class-bounded protocol. 
    switch (kind) {
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Metatype: {
#if SWIFT_OBJC_INTEROP
      // Class metadata can be used as an object when ObjC interop is available.
      auto metatypePtr = reinterpret_cast<const Metadata **>(src);
      auto metatype = *metatypePtr;
      tmp = swift_dynamicCastMetatypeToObjectConditional(metatype);
      // If the cast succeeded, use the result value as the class instance
      // below.
      if (tmp) {
        srcDynamicValue = reinterpret_cast<OpaqueValue*>(&tmp);
        srcDynamicType = reinterpret_cast<const Metadata*>(tmp);
        break;
      }
#endif
      // Otherwise, metatypes aren't class objects.
      return fallbackForNonClass();
    }
    
    case MetadataKind::Class:
    case MetadataKind::ObjCClassWrapper:
    case MetadataKind::ForeignClass:
    case MetadataKind::Existential:
      // Handle the class cases below.  Note that opaque existentials
      // shouldn't get here because we should have drilled into them above.
      break;

    case MetadataKind::Struct:
      // If the source type is AnyHashable, cast from that.
      if (isAnyHashableType(cast<StructMetadata>(srcDynamicType))) {
        bool result =
          _dynamicCastFromAnyHashable(dest, srcDynamicValue, srcDynamicType,
                                      targetType, dynamicFlags);
        maybeDeallocateSource(result);
        return result;
      }
      LLVM_FALLTHROUGH;

    case MetadataKind::Enum:
    case MetadataKind::Optional:
#if SWIFT_OBJC_INTEROP
      // If the source type is bridged to Objective-C, try to bridge.
      if (auto srcBridgeWitness = findBridgeWitness(srcDynamicType)) {
        bool success = _dynamicCastValueToClassExistentialViaObjCBridgeable(
                         dest,
                         srcDynamicValue,
                         srcDynamicType,
                         targetType,
                         srcBridgeWitness,
                         dynamicFlags);
        maybeDeallocateSource(success);
        return success;
      }
#endif
      LLVM_FALLTHROUGH;

    case MetadataKind::Function:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::HeapGenericLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Opaque:
    case MetadataKind::Tuple:
      return fallbackForNonClass();
    }

    // Check for protocol conformances and fill in the witness tables. If
    // srcDynamicType equals nullptr we have a cast from an existential
    // container with a class instance to AnyObject. In this case no check is
    // necessary.
    if (srcDynamicType && !_conformsToProtocols(srcDynamicValue, srcDynamicType,
                              targetType->Protocols,
                              destExistential->getWitnessTables()))
      return fallbackForNonDirectConformance();

    auto object = *(reinterpret_cast<HeapObject**>(srcDynamicValue));
    destExistential->Value = object;
    if (!canConsumeDynamicValue || !(flags & DynamicCastFlags::TakeOnSuccess)) {
      swift_retain(object);
    }
    maybeDeallocateSource(true);
    return true;
  }
  case ExistentialTypeRepresentation::Opaque: {
    auto destExistential =
      reinterpret_cast<OpaqueExistentialContainer*>(dest);

    // Check for protocol conformances and fill in the witness tables.
    if (!_conformsToProtocols(srcDynamicValue, srcDynamicType,
                              targetType->Protocols,
                              destExistential->getWitnessTables()))
      return fallbackForNonDirectConformance();

    // Fill in the type and value.
    destExistential->Type = srcDynamicType;
    if (canConsumeDynamicValue && (flags & DynamicCastFlags::TakeOnSuccess)) {
      srcDynamicType->vw_initializeBufferWithTake(&destExistential->Buffer,
                                                  srcDynamicValue);
    } else {
      srcDynamicType->vw_initializeBufferWithCopy(&destExistential->Buffer,
                                                  srcDynamicValue);
    }
    maybeDeallocateSource(true);
    return true;
  }
  case ExistentialTypeRepresentation::Error: {
    auto destBoxAddr =
      reinterpret_cast<SwiftError**>(dest);
    // Check for the Error protocol conformance, which should be the only
    // one we need.
    assert(targetType->Protocols.NumProtocols == 1);
    const WitnessTable *errorWitness;
    if (!_conformsToProtocols(srcDynamicValue, srcDynamicType,
                              targetType->Protocols,
                              &errorWitness))
      return fallbackForNonDirectConformance();

#if SWIFT_OBJC_INTEROP
    // Check whether there is an embedded NSError. If so, use that for our Error
    // representation.
    if (auto embedded =
          swift_stdlib_getErrorEmbeddedNSErrorIndirect(srcDynamicValue,
                                                       srcDynamicType,
                                                       errorWitness)) {
      *destBoxAddr = reinterpret_cast<SwiftError*>(embedded);
      maybeDeallocateSource(true);
      return true;
    }
#endif

    bool isTake =
      (canConsumeDynamicValue && (flags & DynamicCastFlags::TakeOnSuccess));
    BoxPair destBox = swift_allocError(srcDynamicType, errorWitness,
                                       srcDynamicValue, isTake);
    *destBoxAddr = reinterpret_cast<SwiftError*>(destBox.first);
    maybeDeallocateSource(true);
    return true;
  }
  }

  swift_runtime_unreachable("Unhandled ExistentialTypeRepresentation in switch.");
}

/******************************************************************************/
/********************************** Classes ***********************************/
/******************************************************************************/

static const void *
_dynamicCastUnknownClassToExistential(const void *object,
                                    const ExistentialTypeMetadata *targetType) {
  for (unsigned i = 0, e = targetType->Protocols.NumProtocols; i < e; ++i) {
    const ProtocolDescriptor *protocol = targetType->Protocols[i];

    switch (protocol->Flags.getDispatchStrategy()) {
    case ProtocolDispatchStrategy::Swift:
      // If the target existential requires witness tables, we can't do this cast.
      // The result type would not have a single-refcounted-pointer rep.
      return nullptr;
    case ProtocolDispatchStrategy::ObjC:
#if SWIFT_OBJC_INTEROP
      // All classes conform to AnyObject.
      if (protocol->Flags.getSpecialProtocol() == SpecialProtocol::AnyObject)
        break;

      if (!objectConformsToObjCProtocol(object, protocol))
        return nullptr;
      break;
#else
      assert(false && "ObjC interop disabled?!");
      return nullptr;
#endif
    case ProtocolDispatchStrategy::Empty:
      // The only non-@objc, non-witness-table-requiring protocol should be
      // AnyObject for now.
      assert(protocol->Flags.getSpecialProtocol() == SpecialProtocol::AnyObject
             && "swift protocols besides AnyObject should always require a "
                "witness table");
      break;
    }
  }
  
  return object;
}

/// Perform a dynamic class of some sort of class instance to some
/// sort of class type.
const void *
swift::swift_dynamicCastUnknownClass(const void *object,
                                     const Metadata *targetType) {
  switch (targetType->getKind()) {
  case MetadataKind::Class: {
    auto targetClassType = static_cast<const ClassMetadata *>(targetType);
    return swift_dynamicCastClass(object, targetClassType);
  }

  case MetadataKind::ObjCClassWrapper: {
#if SWIFT_OBJC_INTEROP
    auto targetClassType
      = static_cast<const ObjCClassWrapperMetadata *>(targetType)->Class;
    return swift_dynamicCastObjCClass(object, targetClassType);
#else
    _failCorruptType(targetType);
#endif
  }

  case MetadataKind::ForeignClass: {
#if SWIFT_OBJC_INTEROP
    auto targetClassType = static_cast<const ForeignClassMetadata*>(targetType);
    return swift_dynamicCastForeignClass(object, targetClassType);
#else
    _failCorruptType(targetType);
#endif
  }

  case MetadataKind::Existential: {
    return _dynamicCastUnknownClassToExistential(object,
                      static_cast<const ExistentialTypeMetadata *>(targetType));
  }
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    return nullptr;
  }
  _failCorruptType(targetType);
}

/// Perform a dynamic class of some sort of class instance to some
/// sort of class type.
const void *
swift::swift_dynamicCastUnknownClassUnconditional(const void *object,
                                                  const Metadata *targetType) {
  switch (targetType->getKind()) {
  case MetadataKind::Class: {
    auto targetClassType = static_cast<const ClassMetadata *>(targetType);
    return swift_dynamicCastClassUnconditional(object, targetClassType);
  }

  case MetadataKind::ObjCClassWrapper: {
#if SWIFT_OBJC_INTEROP
    auto targetClassType
      = static_cast<const ObjCClassWrapperMetadata *>(targetType)->Class;
    return swift_dynamicCastObjCClassUnconditional(object, targetClassType);
#else
    _failCorruptType(targetType);
#endif
  }

  case MetadataKind::ForeignClass: {
#if SWIFT_OBJC_INTEROP
    auto targetClassType = static_cast<const ForeignClassMetadata*>(targetType);
    return swift_dynamicCastForeignClassUnconditional(object, targetClassType);
#else
    _failCorruptType(targetType);
#endif
  }

  case MetadataKind::Existential: {
    // We can cast to ObjC existentials. Non-ObjC existentials don't have
    // a single-refcounted-pointer representation.
    if (auto result = _dynamicCastUnknownClassToExistential(object,
                     static_cast<const ExistentialTypeMetadata *>(targetType)))
      return result;
    
    swift_dynamicCastFailure(_swift_getClass(object), targetType);
  }
      
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    swift_dynamicCastFailure(_swift_getClass(object), targetType);
  }
  _failCorruptType(targetType);
}

/******************************************************************************/
/********************************* Metatypes **********************************/
/******************************************************************************/

const Metadata *
swift::swift_dynamicCastMetatype(const Metadata *sourceType,
                                 const Metadata *targetType) {
  auto origSourceType = sourceType;

  switch (targetType->getKind()) {
  case MetadataKind::ObjCClassWrapper:
    // Get the actual class object.
    targetType = static_cast<const ObjCClassWrapperMetadata*>(targetType)
      ->Class;
    LLVM_FALLTHROUGH;
  case MetadataKind::Class:
    // The source value must also be a class; otherwise the cast fails.
    switch (sourceType->getKind()) {
    case MetadataKind::ObjCClassWrapper:
      // Get the actual class object.
      sourceType = static_cast<const ObjCClassWrapperMetadata*>(sourceType)
        ->Class;
      LLVM_FALLTHROUGH;
    case MetadataKind::Class: {
      // Check if the source is a subclass of the target.
#if SWIFT_OBJC_INTEROP
      // We go through ObjC lookup to deal with potential runtime magic in ObjC
      // land.
      if (swift_dynamicCastObjCClassMetatype((const ClassMetadata*)sourceType,
                                             (const ClassMetadata*)targetType))
        return origSourceType;
#else
      if (_dynamicCastClassMetatype((const ClassMetadata*)sourceType,
                                    (const ClassMetadata*)targetType))
        return origSourceType;
#endif
      return nullptr;
    }
    case MetadataKind::ForeignClass: {
      // Check if the source is a subclass of the target.
      if (swift_dynamicCastForeignClassMetatype(
            (const ClassMetadata*)sourceType,
              (const ClassMetadata*)targetType))
        return origSourceType;
      return nullptr;
    }

    case MetadataKind::Existential:
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Function:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::HeapGenericLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Metatype:
    case MetadataKind::Enum:
    case MetadataKind::Optional:
    case MetadataKind::Opaque:
    case MetadataKind::Struct:
    case MetadataKind::Tuple:
      return nullptr;
    }
    break;
      
  case MetadataKind::ForeignClass:
    switch (sourceType->getKind()) {
    case MetadataKind::ObjCClassWrapper:
      // Get the actual class object.
      sourceType = static_cast<const ObjCClassWrapperMetadata*>(sourceType)
        ->Class;
      LLVM_FALLTHROUGH;
    case MetadataKind::Class:
    case MetadataKind::ForeignClass:
      // Check if the source is a subclass of the target.
      if (swift_dynamicCastForeignClassMetatype(
            (const ClassMetadata*)sourceType,
              (const ClassMetadata*)targetType))
        return origSourceType;
      return nullptr;
    case MetadataKind::Existential:
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Function:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::HeapGenericLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Metatype:
    case MetadataKind::Enum:
    case MetadataKind::Optional:
    case MetadataKind::Opaque:
    case MetadataKind::Struct:
    case MetadataKind::Tuple:
      return nullptr;
    }
    break;

  case MetadataKind::Existential:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    // The cast succeeds only if the metadata pointers are statically
    // equivalent.
    if (sourceType != targetType)
      return nullptr;
    return origSourceType;
  }

  swift_runtime_unreachable("Unhandled MetadataKind in switch.");
}

const Metadata *
swift::swift_dynamicCastMetatypeUnconditional(const Metadata *sourceType,
                                              const Metadata *targetType) {
  auto origSourceType = sourceType;

  switch (targetType->getKind()) {
  case MetadataKind::ObjCClassWrapper:
    // Get the actual class object.
    targetType = static_cast<const ObjCClassWrapperMetadata*>(targetType)
      ->Class;
    LLVM_FALLTHROUGH;
  case MetadataKind::Class:
    // The source value must also be a class; otherwise the cast fails.
    switch (sourceType->getKind()) {
    case MetadataKind::ObjCClassWrapper:
      // Get the actual class object.
      sourceType = static_cast<const ObjCClassWrapperMetadata*>(sourceType)
        ->Class;
      LLVM_FALLTHROUGH;
    case MetadataKind::Class: {
      // Check if the source is a subclass of the target.
#if SWIFT_OBJC_INTEROP
      // We go through ObjC lookup to deal with potential runtime magic in ObjC
      // land.
      swift_dynamicCastObjCClassMetatypeUnconditional(
                                            (const ClassMetadata*)sourceType,
                                            (const ClassMetadata*)targetType);
#else
      if (!_dynamicCastClassMetatype((const ClassMetadata*)sourceType,
                                     (const ClassMetadata*)targetType))
        swift_dynamicCastFailure(sourceType, targetType);
#endif
      // If we returned, then the cast succeeded.
      return origSourceType;
    }
    case MetadataKind::ForeignClass: {
      // Check if the source is a subclass of the target.
      swift_dynamicCastForeignClassMetatypeUnconditional(
                                            (const ClassMetadata*)sourceType,
                                            (const ClassMetadata*)targetType);
      // If we returned, then the cast succeeded.
      return origSourceType;
    }
    case MetadataKind::Existential:
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Function:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::HeapGenericLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Metatype:
    case MetadataKind::Enum:
    case MetadataKind::Optional:
    case MetadataKind::Opaque:
    case MetadataKind::Struct:
    case MetadataKind::Tuple:
      swift_dynamicCastFailure(sourceType, targetType);
    }
    break;
    
  case MetadataKind::ForeignClass:
    // The source value must also be a class; otherwise the cast fails.
    switch (sourceType->getKind()) {
    case MetadataKind::ObjCClassWrapper:
      // Get the actual class object.
      sourceType = static_cast<const ObjCClassWrapperMetadata*>(sourceType)
        ->Class;
      LLVM_FALLTHROUGH;
    case MetadataKind::Class:
    case MetadataKind::ForeignClass:
      // Check if the source is a subclass of the target.
      swift_dynamicCastForeignClassMetatypeUnconditional(
                                            (const ClassMetadata*)sourceType,
                                            (const ClassMetadata*)targetType);
      // If we returned, then the cast succeeded.
      return origSourceType;
    case MetadataKind::Existential:
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Function:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::HeapGenericLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Metatype:
    case MetadataKind::Enum:
    case MetadataKind::Optional:
    case MetadataKind::Opaque:
    case MetadataKind::Struct:
    case MetadataKind::Tuple:
      swift_dynamicCastFailure(sourceType, targetType);
    }
    break;
  case MetadataKind::Existential:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    // The cast succeeds only if the metadata pointers are statically
    // equivalent.
    if (sourceType != targetType)
      swift_dynamicCastFailure(sourceType, targetType);
    return origSourceType;
  }

  swift_runtime_unreachable("Unhandled MetadataKind in switch.");
}

/******************************************************************************/
/********************************** Classes ***********************************/
/******************************************************************************/

#if SWIFT_OBJC_INTEROP
/// Do a dynamic cast to the target class.
static void *_dynamicCastUnknownClass(void *object,
                                      const Metadata *targetType,
                                      bool unconditional) {
  // The unconditional path avoids some failure logic.
  if (unconditional) {
    return const_cast<void*>(
          swift_dynamicCastUnknownClassUnconditional(object, targetType));
  }

  return const_cast<void*>(swift_dynamicCastUnknownClass(object, targetType));
}
#endif

static bool _dynamicCastUnknownClassIndirect(OpaqueValue *dest,
                                             void *object,
                                             const Metadata *targetType,
                                             DynamicCastFlags flags) {
    void **destSlot = reinterpret_cast<void **>(dest);

  // The unconditional path avoids some failure logic.
  if (flags & DynamicCastFlags::Unconditional) {
    void *result = const_cast<void*>(
          swift_dynamicCastUnknownClassUnconditional(object, targetType));
    *destSlot = result;

    if (!(flags & DynamicCastFlags::TakeOnSuccess)) {
      swift_unknownRetain(result);
    }
    return true;
  }

  // Okay, we're doing a conditional cast.
  void *result =
    const_cast<void*>(swift_dynamicCastUnknownClass(object, targetType));
  assert(result == nullptr || object == result);

  // If the cast failed, destroy the input and return false.
  if (!result) {
    if (flags & DynamicCastFlags::DestroyOnFailure) {
      swift_unknownRelease(object);
    }
    return false;
  }

  // Otherwise, store to the destination and return true.
  *destSlot = result;
  if (!(flags & DynamicCastFlags::TakeOnSuccess)) {
    swift_unknownRetain(result);
  }
  return true;
}

/******************************************************************************/
/******************************** Existentials ********************************/
/******************************************************************************/

#if SWIFT_OBJC_INTEROP
extern "C" const ProtocolDescriptor PROTOCOL_DESCR_SYM(s5Error);

static const WitnessTable *findErrorWitness(const Metadata *srcType) {
  return swift_conformsToProtocol(srcType, &PROTOCOL_DESCR_SYM(s5Error));
}
#endif

/// Perform a dynamic cast from an existential type to some kind of
/// class type.
static bool _dynamicCastToUnknownClassFromExistential(OpaqueValue *dest,
                                                      OpaqueValue *src,
                                        const ExistentialTypeMetadata *srcType,
                                        const Metadata *targetType,
                                        DynamicCastFlags flags) {
  switch (srcType->getRepresentation()) {
  case ExistentialTypeRepresentation::Class: {
    auto classContainer =
      reinterpret_cast<ClassExistentialContainer*>(src);
    void *obj = classContainer->Value;
#if SWIFT_OBJC_INTEROP
    // If we're casting to NSError, we may need a representation change,
    // so fall into the general swift_dynamicCast path.
    if (targetType == getNSErrorMetadata()) {
      return swift_dynamicCast(dest, src, swift_getObjectType((HeapObject*)obj),
                               targetType, flags);
    }
#endif
    return _dynamicCastUnknownClassIndirect(dest, obj, targetType, flags);
  }
  case ExistentialTypeRepresentation::Opaque: {
    auto opaqueContainer =
      reinterpret_cast<OpaqueExistentialContainer*>(src);
    auto srcCapturedType = opaqueContainer->Type;
    OpaqueValue *srcValue =
      srcCapturedType->vw_projectBuffer(&opaqueContainer->Buffer);
    bool result = swift_dynamicCast(dest,
                                    srcValue,
                                    srcCapturedType,
                                    targetType,
                                    flags);
    if (src != srcValue)
      _maybeDeallocateOpaqueExistential(src, result, flags);
    return result;
  }
  case ExistentialTypeRepresentation::Error: {
    const SwiftError *errorBox =
      *reinterpret_cast<const SwiftError * const *>(src);
    auto srcCapturedType = errorBox->getType();
    const OpaqueValue *srcValue;
    // A bridged NSError is itself the value.
    if (errorBox->isPureNSError())
      srcValue = src;
    else
      srcValue = errorBox->getValue();
    
    // We can't take or destroy the value out of the box since it might be
    // shared.
    auto subFlags = flags - (DynamicCastFlags::TakeOnSuccess
                             | DynamicCastFlags::DestroyOnFailure);
    bool result = swift_dynamicCast(dest,
                                    const_cast<OpaqueValue*>(srcValue),
                                    srcCapturedType, targetType,
                                    subFlags);
    if (shouldDeallocateSource(result, flags))
      srcType->vw_destroy(src);
    return result;
  }
  }

  swift_runtime_unreachable(
      "Unhandled ExistentialTypeRepresentation in switch.");
}

static void unwrapExistential(OpaqueValue *src,
                              const ExistentialTypeMetadata *srcType,
                              OpaqueValue *&srcValue,
                              const Metadata *&srcCapturedType,
                              bool &isOutOfLine,
                              bool &canTake) {
  switch (srcType->getRepresentation()) {
    case ExistentialTypeRepresentation::Class: {
      auto classContainer =
      reinterpret_cast<const ClassExistentialContainer*>(src);
      srcValue = (OpaqueValue*) &classContainer->Value;
      void *obj = classContainer->Value;
      srcCapturedType = swift_getObjectType(reinterpret_cast<HeapObject*>(obj));
      isOutOfLine = false;
      canTake = true;
      break;
    }
    case ExistentialTypeRepresentation::Opaque: {
      auto opaqueContainer = reinterpret_cast<OpaqueExistentialContainer*>(src);
      srcCapturedType = opaqueContainer->Type;
      srcValue = srcCapturedType->vw_projectBuffer(&opaqueContainer->Buffer);
      isOutOfLine = (src != srcValue);
      canTake = true;
      break;
    }
    case ExistentialTypeRepresentation::Error: {
      const SwiftError *errorBox
      = *reinterpret_cast<const SwiftError * const *>(src);
      
      srcCapturedType = errorBox->getType();
      // A bridged NSError is itself the value.
      if (errorBox->isPureNSError())
        srcValue = src;
      else
        srcValue = const_cast<OpaqueValue*>(errorBox->getValue());
      
      // The value is out-of-line, but we can't take it, since it may be shared.
      isOutOfLine = true;
      canTake = false;
      break;
    }
  }
}

/// Perform a dynamic cast from an existential type to a
/// non-existential type.
static bool _dynamicCastFromExistential(OpaqueValue *dest,
                                        OpaqueValue *src,
                                        const ExistentialTypeMetadata *srcType,
                                        const Metadata *targetType,
                                        DynamicCastFlags flags) {
  OpaqueValue *srcValue;
  const Metadata *srcCapturedType;
  bool isOutOfLine;
  bool canTake;

  unwrapExistential(src, srcType,
                    srcValue, srcCapturedType, isOutOfLine, canTake);
  
  auto subFlags = flags;
  if (!canTake)
    subFlags = subFlags - (DynamicCastFlags::DestroyOnFailure
                           | DynamicCastFlags::TakeOnSuccess);

  bool result = swift_dynamicCast(dest, srcValue, srcCapturedType,
                                  targetType, subFlags);

  if (!canTake) {
    // swift_dynamicCast performed no memory management.
    // Destroy the value if requested.
    if (shouldDeallocateSource(result, flags))
      srcType->vw_destroy(src);
  } else {
    // swift_dynamicCast took or destroyed the value as per the original request
    // We may still have an opaque existential container to deallocate.
    if (isOutOfLine) {
      assert(srcType->getRepresentation()
               == ExistentialTypeRepresentation::Opaque);
      _maybeDeallocateOpaqueExistential(src, result, flags);
    }
  }

  return result;
}

/******************************************************************************/
/********************************* Metatypes **********************************/
/******************************************************************************/

/// Perform a dynamic cast of a metatype to a metatype.
///
/// Note that the check is whether 'metatype' is an *instance of*
/// 'targetType', not a *subtype of it*.
static bool _dynamicCastMetatypeToMetatype(OpaqueValue *dest,
                                           const Metadata *metatype,
                                           const MetatypeMetadata *targetType,
                                           DynamicCastFlags flags) {
  const Metadata *result;
  if (flags & DynamicCastFlags::Unconditional) {
    result = swift_dynamicCastMetatypeUnconditional(metatype,
                                                    targetType->InstanceType);
  } else {
    result = swift_dynamicCastMetatype(metatype, targetType->InstanceType);
    if (!result) return false;
  }

  *((const Metadata **) dest) = result;
  return true;
}

/// Check whether an unknown class instance is actually a class object.
static const Metadata *_getUnknownClassAsMetatype(void *object) {
#if SWIFT_OBJC_INTEROP
  // Objective-C class metadata are objects, so an AnyObject (or NSObject)
  // may refer to a class object.

  // Test whether the object's isa is a metaclass, which indicates that the
  // object is a class.

  Class isa = object_getClass((id)object);
  if (class_isMetaClass(isa)) {
    return swift_getObjCClassMetadata((const ClassMetadata *)object);
  }
#endif

  // Class values are currently never metatypes in the native runtime.
  return nullptr;
}

/// Perform a dynamic cast of a class value to a metatype type.
static bool _dynamicCastUnknownClassToMetatype(OpaqueValue *dest,
                                               void *object,
                                             const MetatypeMetadata *targetType,
                                               DynamicCastFlags flags) {
  if (auto metatype = _getUnknownClassAsMetatype(object))
    return _dynamicCastMetatypeToMetatype(dest, metatype, targetType, flags);

  if (flags & DynamicCastFlags::Unconditional)
    swift_dynamicCastFailure(_swift_getClass(object), targetType);
  if (flags & DynamicCastFlags::DestroyOnFailure)
    swift_unknownRelease((HeapObject*) object);
  return false;
}

/// Perform a dynamic cast to a metatype type.
static bool _dynamicCastToMetatype(OpaqueValue *dest,
                                   OpaqueValue *src,
                                   const Metadata *srcType,
                                   const MetatypeMetadata *targetType,
                                   DynamicCastFlags flags) {

  switch (srcType->getKind()) {
  case MetadataKind::Metatype: {
    const Metadata *srcMetatype = *(const Metadata * const *) src;
    return _dynamicCastMetatypeToMetatype(dest, srcMetatype,
                                          targetType, flags);
  }

  case MetadataKind::ExistentialMetatype: {
    const Metadata *srcMetatype = *(const Metadata * const *) src;
    return _dynamicCastMetatypeToMetatype(dest, srcMetatype,
                                          targetType, flags);
  }

  case MetadataKind::Existential: {
    auto srcExistentialType = cast<ExistentialTypeMetadata>(srcType);
    switch (srcExistentialType->getRepresentation()) {
    case ExistentialTypeRepresentation::Class: {
      auto srcExistential = (ClassExistentialContainer*) src;
      return _dynamicCastUnknownClassToMetatype(dest,
                                                srcExistential->Value,
                                                targetType, flags);
    }
    case ExistentialTypeRepresentation::Opaque: {
      auto srcExistential = (OpaqueExistentialContainer*) src;
      auto srcValueType = srcExistential->Type;
      auto srcValue = srcValueType->vw_projectBuffer(&srcExistential->Buffer);
      bool result = _dynamicCastToMetatype(dest, srcValue, srcValueType,
                                           targetType, flags);
      if (src != srcValue)
        _maybeDeallocateOpaqueExistential(src, result, flags);
      return result;
    }
    case ExistentialTypeRepresentation::Error: {
      const SwiftError *srcBox
        = *reinterpret_cast<const SwiftError * const *>(src);
      
      auto srcValueType = srcBox->getType();
      const OpaqueValue *srcValue;
      if (srcBox->isPureNSError())
        srcValue = src;
      else
        srcValue = srcBox->getValue();
      
      // Can't take from a box since the value may be shared.
      auto subFlags = flags - (DynamicCastFlags::TakeOnSuccess
                               | DynamicCastFlags::DestroyOnFailure);
      bool result = _dynamicCastToMetatype(dest,
                                           const_cast<OpaqueValue*>(srcValue),
                                           srcValueType,
                                           targetType, subFlags);
      if (shouldDeallocateSource(result, flags))
        srcType->vw_destroy(src);
      return result;
    }
    }
  }

  case MetadataKind::Class:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass: {
    void *object = *reinterpret_cast<void**>(src);
    return _dynamicCastUnknownClassToMetatype(dest, object, targetType, flags);
  }

  case MetadataKind::Function:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Struct: // AnyHashable, if metatypes implement Hashable
  case MetadataKind::Tuple:
    return _fail(src, srcType, targetType, flags);
  }
  _failCorruptType(srcType);
}

/// Perform a dynamic cast of a metatype to an existential metatype type.
static bool _dynamicCastMetatypeToExistentialMetatype(OpaqueValue *dest,
                                 const Metadata *srcMetatype,
                                 const ExistentialMetatypeMetadata *targetType,
                                 DynamicCastFlags flags,
                                 bool writeDestMetatype = true) {
  // The instance type of an existential metatype must be either an
  // existential or an existential metatype.
  auto destMetatype = reinterpret_cast<ExistentialMetatypeContainer*>(dest);

  // If it's an existential, we need to check for conformances.
  auto targetInstanceType = targetType->InstanceType;
  if (auto targetInstanceTypeAsExistential =
        dyn_cast<ExistentialTypeMetadata>(targetInstanceType)) {
    // Check for conformance to all the protocols.
    // TODO: collect the witness tables.
    auto &protocols = targetInstanceTypeAsExistential->Protocols;
    const WitnessTable **conformance
      = writeDestMetatype ? destMetatype->getWitnessTables() : nullptr;
    for (unsigned i = 0, n = protocols.NumProtocols; i != n; ++i) {
      const ProtocolDescriptor *protocol = protocols[i];
      if (!_conformsToProtocol(nullptr, srcMetatype, protocol, conformance)) {
        if (flags & DynamicCastFlags::Unconditional)
          swift_dynamicCastFailure(srcMetatype, targetType);
        return false;
      }
      if (conformance && protocol->Flags.needsWitnessTable())
        ++conformance;
    }

    if (writeDestMetatype)
      destMetatype->Value = srcMetatype;
    return true;
  }

  // Otherwise, we're casting to SomeProtocol.Type.Type.
  auto targetInstanceTypeAsMetatype =
    cast<ExistentialMetatypeMetadata>(targetInstanceType);

  // If the source type isn't a metatype, the cast fails.
  auto srcMetatypeMetatype = dyn_cast<MetatypeMetadata>(srcMetatype);
  if (!srcMetatypeMetatype) {
    if (flags & DynamicCastFlags::Unconditional)
      swift_dynamicCastFailure(srcMetatype, targetType);
    return false;
  }

  // The representation of an existential metatype remains consistent
  // arbitrarily deep: a metatype, followed by some protocols.  The
  // protocols are the same at every level, so we can just set the
  // metatype correctly and then recurse, letting the recursive call
  // fill in the conformance information correctly.

  // Proactively set the destination metatype so that we can tail-recur,
  // unless we've already done so.  There's no harm in doing this even if
  // the cast fails.
  if (writeDestMetatype)
    *((const Metadata **) dest) = srcMetatype;  

  // Recurse.
  auto srcInstanceType = srcMetatypeMetatype->InstanceType;
  return _dynamicCastMetatypeToExistentialMetatype(dest, srcInstanceType,
                                             targetInstanceTypeAsMetatype,
                                                   flags,
                                                   /*overwrite*/ false);
}

/// Perform a dynamic cast of a class value to an existential metatype type.
static bool _dynamicCastUnknownClassToExistentialMetatype(OpaqueValue *dest,
                                                          void *object,
                                const ExistentialMetatypeMetadata *targetType,
                                                       DynamicCastFlags flags) {
  if (auto metatype = _getUnknownClassAsMetatype(object))
    return _dynamicCastMetatypeToExistentialMetatype(dest, metatype,
                                                     targetType, flags);
  
  // Class values are currently never metatypes (?).
  if (flags & DynamicCastFlags::Unconditional)
    swift_dynamicCastFailure(_swift_getClass(object), targetType);
  if (flags & DynamicCastFlags::DestroyOnFailure)
    swift_unknownRelease((HeapObject*) object);
  return false;
}

/// Perform a dynamic cast to an existential metatype type.
static bool _dynamicCastToExistentialMetatype(OpaqueValue *dest,
                                              OpaqueValue *src,
                                              const Metadata *srcType,
                           const ExistentialMetatypeMetadata *targetType,
                                              DynamicCastFlags flags) {
  
  switch (srcType->getKind()) {
  case MetadataKind::Metatype: {
    const Metadata *srcMetatype = *(const Metadata * const *) src;
    return _dynamicCastMetatypeToExistentialMetatype(dest, srcMetatype,
                                                     targetType, flags);
  }

  // TODO: take advantage of protocol conformances already known.
  case MetadataKind::ExistentialMetatype: {
    const Metadata *srcMetatype = *(const Metadata * const *) src;
    return _dynamicCastMetatypeToExistentialMetatype(dest, srcMetatype,
                                                     targetType, flags);
  }

  case MetadataKind::Existential: {
    auto srcExistentialType = cast<ExistentialTypeMetadata>(srcType);
    switch (srcExistentialType->getRepresentation()) {
    case ExistentialTypeRepresentation::Class: {
      auto srcExistential = (ClassExistentialContainer*) src;
      return _dynamicCastUnknownClassToExistentialMetatype(dest,
                                                srcExistential->Value,
                                                targetType, flags);
    }
    case ExistentialTypeRepresentation::Opaque: {
      auto srcExistential = (OpaqueExistentialContainer*) src;
      auto srcValueType = srcExistential->Type;
      auto srcValue = srcValueType->vw_projectBuffer(&srcExistential->Buffer);
      bool result = _dynamicCastToExistentialMetatype(dest, srcValue, srcValueType,
                                                      targetType, flags);
      if (src != srcValue)
        _maybeDeallocateOpaqueExistential(src, result, flags);
      return result;
    }
    case ExistentialTypeRepresentation::Error: {
      const SwiftError *srcBox
        = *reinterpret_cast<const SwiftError * const *>(src);
      
      auto srcValueType = srcBox->getType();
      const OpaqueValue *srcValue;
      if (srcBox->isPureNSError())
        srcValue = src;
      else
        srcValue = srcBox->getValue();
      
      // Can't take from a box since the value may be shared.
      auto subFlags = flags - (DynamicCastFlags::TakeOnSuccess
                               | DynamicCastFlags::DestroyOnFailure);
      bool result = _dynamicCastToExistentialMetatype(dest,
                                           const_cast<OpaqueValue*>(srcValue),
                                           srcValueType,
                                           targetType, subFlags);
      if (shouldDeallocateSource(result, flags))
        srcType->vw_destroy(src);
      return result;
    }
    }
  }

  case MetadataKind::Class:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass: {
    void *object = *reinterpret_cast<void**>(src);
    return _dynamicCastUnknownClassToExistentialMetatype(dest, object,
                                                         targetType, flags);
  }

  case MetadataKind::Function:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Struct:  // AnyHashable, if metatypes implement Hashable
  case MetadataKind::Tuple:
    return _fail(src, srcType, targetType, flags);
  }
  _failCorruptType(srcType);
}

/******************************************************************************/
/********************************* Functions **********************************/
/******************************************************************************/

static bool _dynamicCastToFunction(OpaqueValue *dest,
                                   OpaqueValue *src,
                                   const Metadata *srcType,
                                   const FunctionTypeMetadata *targetType,
                                   DynamicCastFlags flags) {
  // Function casts succeed on exact matches, or if the target type is
  // throwier than the source type.
  //
  // TODO: We could also allow ABI-compatible variance, such as casting
  // a dynamic Base -> Derived to Derived -> Base. We wouldn't be able to
  // perform a dynamic cast that required any ABI adjustment without a JIT
  // though.

  // Check for an exact type match first.
  if (srcType == targetType) {
    return _succeed(dest, src, srcType, flags);
  }

  switch (srcType->getKind()) {
  case MetadataKind::Function: {
    auto srcFn = static_cast<const FunctionTypeMetadata *>(srcType);
    auto targetFn = static_cast<const FunctionTypeMetadata *>(targetType);
    
    // Check that argument counts and convention match. "throws" can vary.
    if (srcFn->Flags.withThrows(false) != targetFn->Flags.withThrows(false))
      return _fail(src, srcType, targetType, flags);
    
    // If the target type can't throw, neither can the source.
    if (srcFn->throws() && !targetFn->throws())
      return _fail(src, srcType, targetType, flags);
    
    // The result and argument types must match.
    if (srcFn->ResultType != targetFn->ResultType)
      return _fail(src, srcType, targetType, flags);
    if (srcFn->getNumArguments() != targetFn->getNumArguments())
      return _fail(src, srcType, targetType, flags);
    for (unsigned i = 0, e = srcFn->getNumArguments(); i < e; ++i)
      if (srcFn->getArguments()[i] != targetFn->getArguments()[i])
        return _fail(src, srcType, targetType, flags);
    
    return _succeed(dest, src, srcType, flags);
  }
  
  case MetadataKind::Existential:
    return _dynamicCastFromExistential(dest, src,
                         static_cast<const ExistentialTypeMetadata*>(srcType),
                         targetType, flags);
    
  case MetadataKind::Class:
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Metatype:
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
    return _fail(src, srcType, targetType, flags);
  }

  swift_runtime_unreachable("Unhandled MetadataKind in switch.");
}

/******************************************************************************/
/****************************** Bridging NSError ******************************/
/******************************************************************************/

#if SWIFT_OBJC_INTEROP
static id dynamicCastValueToNSError(OpaqueValue *src,
                                    const Metadata *srcType,
                                    const WitnessTable *srcErrorWitness,
                                    DynamicCastFlags flags) {
  // Check whether there is an embedded NSError.
  if (auto embedded =
          swift_stdlib_getErrorEmbeddedNSErrorIndirect(src, srcType,
                                                       srcErrorWitness)) {
    if (flags & DynamicCastFlags::TakeOnSuccess)
      srcType->vw_destroy(src);

    return embedded;
  }

  BoxPair errorBox = swift_allocError(srcType, srcErrorWitness, src,
                            /*isTake*/ flags & DynamicCastFlags::TakeOnSuccess);
  return swift_bridgeErrorToNSError((SwiftError*)errorBox.first);
}

#endif

/******************************************************************************/
/********************************* Optionals **********************************/
/******************************************************************************/

namespace {

struct OptionalCastResult {
  bool success;
  const Metadata* payloadType;
};

} // end anonymous namespace

/// Handle optional unwrapping of the cast source.
/// \returns {true, nullptr} if the cast succeeds without unwrapping.
/// \returns {false, nullptr} if the cast fails before unwrapping.
/// \returns {false, payloadType} if the cast should be attempted using an
/// equivalent payloadType.
static OptionalCastResult
checkDynamicCastFromOptional(OpaqueValue *dest,
                             OpaqueValue *src,
                             const Metadata *srcType,
                             const Metadata *targetType,
                             DynamicCastFlags flags) {
  if (srcType->getKind() != MetadataKind::Optional)
    return {false, srcType};

  // Check if the target is an existential that Optional always conforms to.
  if (targetType->getKind() == MetadataKind::Existential) {
    // Attempt a conditional cast without destroying on failure.
    DynamicCastFlags checkCastFlags
      = flags - (DynamicCastFlags::Unconditional
                 | DynamicCastFlags::DestroyOnFailure);
    assert((checkCastFlags - DynamicCastFlags::TakeOnSuccess)
           == DynamicCastFlags::Default && "Unhandled DynamicCastFlag");
    if (_dynamicCastToExistential(dest, src, srcType,
                                  cast<ExistentialTypeMetadata>(targetType),
                                  checkCastFlags)) {
      return {true, nullptr};
    }
  }
  const Metadata *payloadType =
    cast<EnumMetadata>(srcType)->getGenericArgs()[0];
  int enumCase =
    swift_getEnumCaseSinglePayload(src, payloadType, 1 /*emptyCases=*/);
  if (enumCase != -1) {
    // Allow Optional<T>.none -> Optional<U>.none
    if (targetType->getKind() != MetadataKind::Optional) {
      _fail(src, srcType, targetType, flags);
      return {false, nullptr};
    }

    // Get the destination payload type
    const Metadata *targetPayloadType =
      cast<EnumMetadata>(targetType)->getGenericArgs()[0];

    // Inject the .none tag
    swift_storeEnumTagSinglePayload(dest, targetPayloadType, enumCase,
                                    1 /*emptyCases=*/);

    // We don't have to destroy the source, because it was nil.
    return {true, nullptr};
  }
  // .Some
  // Single payload enums are guaranteed layout compatible with their
  // payload. Only the source's payload needs to be taken or destroyed.
  return {false, payloadType};
}

/******************************************************************************/
/**************************** Bridging _SwiftValue ****************************/
/******************************************************************************/

#if SWIFT_OBJC_INTEROP
/// Try to unbox a _SwiftValue box to perform a dynamic cast.
static bool tryDynamicCastBoxedSwiftValue(OpaqueValue *dest,
                                          OpaqueValue *src,
                                          const Metadata *srcType,
                                          const Metadata *targetType,
                                          DynamicCastFlags flags) {
  // Swift type should be AnyObject or a class type.
  if (!srcType->isAnyClass()) {
    auto existential = dyn_cast<ExistentialTypeMetadata>(srcType);
    if (!existential ||
        existential->Flags.getSpecialProtocol()
          != SpecialProtocol::AnyObject)
      return false;
  }
  
  id srcObject;
  memcpy(&srcObject, src, sizeof(id));
  
  // Do we have a _SwiftValue?
  _SwiftValue *srcSwiftValue = getAsSwiftValue(srcObject);
  if (!srcSwiftValue)
    return false;
  
  // If so, extract the boxed value and try to cast it.
  const Metadata *boxedType;
  const OpaqueValue *boxedValue;
  std::tie(boxedType, boxedValue)
    = getValueFromSwiftValue(srcSwiftValue);
  
  // We can't touch the value from the box because it may be
  // multiply-referenced.
  // TODO: Check for uniqueness and consume if box is unique?
  
  // Does the boxed type exactly match the target type we're looking for?
  if (boxedType == targetType) {
    targetType->vw_initializeWithCopy(dest,
                                      const_cast<OpaqueValue*>(boxedValue));
    // Release the box if we need to.
    if (flags & DynamicCastFlags::TakeOnSuccess)
      objc_release((id)srcSwiftValue);
    return true;
  }
  
  // Maybe we can cast the boxed value to our destination type somehow.
  auto innerFlags = flags - DynamicCastFlags::TakeOnSuccess
                          - DynamicCastFlags::DestroyOnFailure;
  if (swift_dynamicCast(dest, const_cast<OpaqueValue*>(boxedValue),
                        boxedType, targetType, innerFlags)) {
    // Release the box if we need to.
    if (flags & DynamicCastFlags::TakeOnSuccess)
      objc_release((id)srcSwiftValue);
    return true;
  }
  
  return false;
}
#endif

/******************************************************************************/
/******************************** Collections *********************************/
/******************************************************************************/

/// Nominal type descriptor for Swift.Array.
extern "C" const NominalTypeDescriptor NOMINAL_TYPE_DESCR_SYM(Sa);

/// Nominal type descriptor for Swift.Dictionary.
extern "C" const NominalTypeDescriptor STRUCT_TYPE_DESCR_SYM(s10Dictionary);

/// Nominal type descriptor for Swift.Set.
extern "C" const NominalTypeDescriptor STRUCT_TYPE_DESCR_SYM(s3Set);

SWIFT_CC(swift)
extern "C"
void _swift_arrayDownCastIndirect(OpaqueValue *destination,
                                  OpaqueValue *source,
                                  const Metadata *sourceValueType,
                                  const Metadata *targetValueType);

SWIFT_CC(swift)
extern "C"
bool _swift_arrayDownCastConditionalIndirect(OpaqueValue *destination,
                                             OpaqueValue *source,
                                             const Metadata *sourceValueType,
                                             const Metadata *targetValueType);

SWIFT_CC(swift)
extern "C"
void _swift_setDownCastIndirect(OpaqueValue *destination,
                                OpaqueValue *source,
                                const Metadata *sourceValueType,
                                const Metadata *targetValueType,
                                const void *sourceValueHashable,
                                const void *targetValueHashable);

SWIFT_CC(swift)
extern "C"
bool _swift_setDownCastConditionalIndirect(OpaqueValue *destination,
                                       OpaqueValue *source,
                                       const Metadata *sourceValueType,
                                       const Metadata *targetValueType,
                                       const void *sourceValueHashable,
                                       const void *targetValueHashable);

SWIFT_CC(swift)
extern "C"
void _swift_dictionaryDownCastIndirect(OpaqueValue *destination,
                                       OpaqueValue *source,
                                       const Metadata *sourceKeyType,
                                       const Metadata *sourceValueType,
                                       const Metadata *targetKeyType,
                                       const Metadata *targetValueType,
                                       const void *sourceKeyHashable,
                                       const void *targetKeyHashable);

SWIFT_CC(swift)
extern "C"
bool _swift_dictionaryDownCastConditionalIndirect(OpaqueValue *destination,
                                        OpaqueValue *source,
                                        const Metadata *sourceKeyType,
                                        const Metadata *sourceValueType,
                                        const Metadata *targetKeyType,
                                        const Metadata *targetValueType,
                                        const void *sourceKeyHashable,
                                        const void *targetKeyHashable);

static bool _dynamicCastStructToStruct(OpaqueValue *destination,
                                       OpaqueValue *source,
                                       const StructMetadata *sourceType,
                                       const StructMetadata *targetType,
                                       DynamicCastFlags flags) {
  if (sourceType == targetType)
    return _succeed(destination, source, sourceType, flags);

  // The two types have to be instantiations of the same type.
  auto descriptor = sourceType->Description.get();
  auto targetDescriptor = targetType->Description.get();
  if (descriptor != targetDescriptor) {
    if (descriptor == &STRUCT_TYPE_DESCR_SYM(s11AnyHashable)) {
      return _dynamicCastFromAnyHashable(destination, source,
                                         sourceType, targetType, flags);
    } else if (targetDescriptor == &STRUCT_TYPE_DESCR_SYM(s11AnyHashable)) {
      return _dynamicCastToAnyHashable(destination, source,
                                       sourceType, targetType, flags);
    } else {
      return _fail(source, sourceType, targetType, flags);
    }
  }

  auto sourceArgs = sourceType->getGenericArgs();
  auto targetArgs = targetType->getGenericArgs();

  bool result;

  // Arrays.
  if (descriptor == &NOMINAL_TYPE_DESCR_SYM(Sa)) {
    if (flags & DynamicCastFlags::Unconditional) {
      _swift_arrayDownCastIndirect(source, destination,
                                   sourceArgs[0], targetArgs[0]);
      result = true;
    } else {
      result =
        _swift_arrayDownCastConditionalIndirect(source, destination,
                                                sourceArgs[0], targetArgs[0]);
    }

  // Dictionaries.
  } else if (descriptor == &STRUCT_TYPE_DESCR_SYM(s10Dictionary)) {
    if (flags & DynamicCastFlags::Unconditional) {
      _swift_dictionaryDownCastIndirect(source, destination,
                                        sourceArgs[0], sourceArgs[1],
                                        targetArgs[0], targetArgs[1],
                                        sourceArgs[2], targetArgs[2]);
      result = true;
    } else {
      result =
        _swift_dictionaryDownCastConditionalIndirect(source, destination,
                                        sourceArgs[0], sourceArgs[1],
                                        targetArgs[0], targetArgs[1],
                                        sourceArgs[2], targetArgs[2]);
    }

  // Sets.
  } else if (descriptor == &STRUCT_TYPE_DESCR_SYM(s3Set)) {
    if (flags & DynamicCastFlags::Unconditional) {
      _swift_setDownCastIndirect(source, destination,
                                 sourceArgs[0], targetArgs[0],
                                 sourceArgs[1], targetArgs[1]);
      result = true;
    } else {
      result =
        _swift_setDownCastConditionalIndirect(source, destination,
                                              sourceArgs[0], targetArgs[0],
                                              sourceArgs[1], targetArgs[1]);
    }

  // Other struct types don't support dynamic covariance for now.
  } else {
    return _fail(source, sourceType, targetType, flags);
  }

  // The intrinsics above never consume the source value.
  bool shouldDestroySource =
    result ? flags & DynamicCastFlags::TakeOnSuccess
           : flags & DynamicCastFlags::DestroyOnFailure;
  if (shouldDestroySource) {
    sourceType->vw_destroy(source);
  }

  return result;
}

static bool _dynamicCastTupleToTuple(OpaqueValue *destination,
                                     OpaqueValue *source,
                                     const TupleTypeMetadata *sourceType,
                                     const TupleTypeMetadata *targetType,
                                     DynamicCastFlags flags) {
  assert(sourceType != targetType &&
         "Caller should handle exact tuple matches");


  // Simple case: number of elements mismatches.
  if (sourceType->NumElements != targetType->NumElements)
    return _fail(source, sourceType, targetType, flags);

  // Check that the elements line up.
  const char *sourceLabels = sourceType->Labels;
  const char *targetLabels = targetType->Labels;
  bool anyTypeMismatches = false;
  for (unsigned i = 0, n = sourceType->NumElements; i != n; ++i) {
    // Check the label, if there is one.
    if (sourceLabels && targetLabels && sourceLabels != targetLabels) {
      const char *sourceSpace = strchr(sourceLabels, ' ');
      const char *targetSpace = strchr(targetLabels, ' ');

      // If both have labels, and the labels mismatch, we fail.
      if (sourceSpace && sourceSpace != sourceLabels &&
          targetSpace && targetSpace != targetLabels) {
        unsigned sourceLen = sourceSpace - sourceLabels;
        unsigned targetLen = targetSpace - targetLabels;
        if (sourceLen != targetLen ||
            strncmp(sourceLabels, targetLabels, sourceLen) != 0)
          return _fail(source, sourceType, targetType, flags);
      }

      sourceLabels = sourceSpace ? sourceSpace + 1 : nullptr;
      targetLabels = targetSpace ? targetSpace + 1 : nullptr;
    }

    // If the types don't match exactly, make a note of it. We'll try to
    // convert them in a second pass.
    if (sourceType->getElement(i).Type != targetType->getElement(i).Type)
      anyTypeMismatches = true;
  }

  // If there were no type mismatches, the only difference was in the argument
  // labels. We can directly map from the source to the destination type.
  if (!anyTypeMismatches)
    return _succeed(destination, source, targetType, flags);

  // Determine how the individual elements will get casted.
  // If both success and failure will destroy the source, then
  // we can be destructively cast each element. Otherwise, we'll have to
  // manually handle them.
  const DynamicCastFlags alwaysDestroySourceFlags =
    DynamicCastFlags::TakeOnSuccess | DynamicCastFlags::DestroyOnFailure;
  bool alwaysDestroysSource =
    (static_cast<DynamicCastFlags>(flags & alwaysDestroySourceFlags)
       == alwaysDestroySourceFlags);
  DynamicCastFlags elementFlags = flags;
  if (!alwaysDestroysSource)
    elementFlags = elementFlags - alwaysDestroySourceFlags;

  // Local function to destroy the elements in the range [start, end).
  auto destroyRange = [](const TupleTypeMetadata *type, OpaqueValue *value,
                        unsigned start, unsigned end) {
    assert(start <= end && "invalid range in destroyRange");
    for (unsigned i = start; i != end; ++i) {
      const auto &elt = type->getElement(i);
      elt.Type->vw_destroy(elt.findIn(value));
    }
  };

  // Cast each element.
  for (unsigned i = 0, n = sourceType->NumElements; i != n; ++i) {
    // Cast the element. If it succeeds, keep going.
    const auto &sourceElt = sourceType->getElement(i);
    const auto &targetElt = targetType->getElement(i);
    if (swift_dynamicCast(targetElt.findIn(destination),
                          sourceElt.findIn(source),
                          sourceElt.Type, targetElt.Type, elementFlags))
      continue;

    // Casting failed, so clean up.

    // Destroy all of the elements that got casted into the destination buffer.
    destroyRange(targetType, destination, 0, i);

    // If we're supposed to destroy on failure, destroy any elements from the
    // source buffer that haven't been destroyed/taken from yet.
    if (flags & DynamicCastFlags::DestroyOnFailure)
      destroyRange(sourceType, source, alwaysDestroysSource ? i : 0, n);

    // If an unconditional cast failed, complain.
    if (flags & DynamicCastFlags::Unconditional)
      swift_dynamicCastFailure(sourceType, targetType);
    return false;
  }

  // Casting succeeded.

  // If we were supposed to take on success from the source buffer but couldn't
  // before, destroy the source buffer now.
  if (!alwaysDestroysSource && (flags & DynamicCastFlags::TakeOnSuccess))
    destroyRange(sourceType, source, 0, sourceType->NumElements);

  return true;
}

/******************************************************************************/
/****************************** Main Entrypoint *******************************/
/******************************************************************************/

/// Perform a dynamic cast to an arbitrary type.

bool swift::swift_dynamicCast(OpaqueValue *dest, OpaqueValue *src,
                              const Metadata *srcType,
                              const Metadata *targetType,
                              DynamicCastFlags flags)
    SWIFT_CC(RegisterPreservingCC_IMPL) {
  auto unwrapResult = checkDynamicCastFromOptional(dest, src, srcType,
                                                   targetType, flags);
  srcType = unwrapResult.payloadType;
  if (!srcType)
    return unwrapResult.success;

#if SWIFT_OBJC_INTEROP
  // A class or AnyObject reference may point at a boxed _SwiftValue.
  if (tryDynamicCastBoxedSwiftValue(dest, src, srcType,
                                    targetType, flags)) {
    return true;
  }
#endif

  switch (targetType->getKind()) {
  // Handle wrapping an Optional target.
  case MetadataKind::Optional: {
    // If the source is an existential, attempt to cast it first without
    // unwrapping the target. This handles an optional source wrapped within an
    // existential that Optional conforms to (Any).
    if (auto srcExistentialType = dyn_cast<ExistentialTypeMetadata>(srcType)) {
#if SWIFT_OBJC_INTEROP
      // If coming from AnyObject, we may want to bridge.
      if (srcExistentialType->Flags.getSpecialProtocol()
            == SpecialProtocol::AnyObject) {
        if (auto targetBridgeWitness = findBridgeWitness(targetType)) {
          return _dynamicCastClassToValueViaObjCBridgeable(dest, src, srcType,
                                                           targetType,
                                                           targetBridgeWitness,
                                                           flags);
        }
      }
#endif
      return _dynamicCastFromExistential(dest, src, srcExistentialType,
                                         targetType, flags);
    }
    // Recursively cast into the layout compatible payload area.
    const Metadata *payloadType =
      cast<EnumMetadata>(targetType)->getGenericArgs()[0];
    if (swift_dynamicCast(dest, src, srcType, payloadType, flags)) {
      swift_storeEnumTagSinglePayload(dest, payloadType, -1 /*case*/,
                                      1 /*emptyCases*/);
      return true;
    }
    return false;
  }

  // Casts to class type.
  case MetadataKind::Class:
  case MetadataKind::ObjCClassWrapper:
#if SWIFT_OBJC_INTEROP
    // If the destination type is an NSError, and the source type is an
    // Error, then the cast can succeed by NSError bridging.
    if (targetType == getNSErrorMetadata()) {
      // Don't rebridge if the source is already some kind of NSError.
      if (srcType->isAnyClass()
          && swift_dynamicCastObjCClass(*reinterpret_cast<id*>(src),
               static_cast<const ObjCClassWrapperMetadata*>(targetType)->Class))
        return _succeed(dest, src, srcType, flags);
      if (auto srcErrorWitness = findErrorWitness(srcType)) {
        auto error = dynamicCastValueToNSError(src, srcType,
                                               srcErrorWitness, flags);
        *reinterpret_cast<id *>(dest) = error;
        return true;
      }
    }
    LLVM_FALLTHROUGH;
#endif

  case MetadataKind::ForeignClass:
    switch (srcType->getKind()) {
    case MetadataKind::Class:
    case MetadataKind::ObjCClassWrapper:
    case MetadataKind::ForeignClass: {
      // Do a dynamic cast on the instance pointer.
      void *object = *reinterpret_cast<void * const *>(src);
      return _dynamicCastUnknownClassIndirect(dest, object,
                                              targetType, flags);
    }

    case MetadataKind::Existential: {
      auto srcExistentialType = cast<ExistentialTypeMetadata>(srcType);
      return _dynamicCastToUnknownClassFromExistential(dest, src,
                                                       srcExistentialType,
                                                       targetType, flags);
    }

    case MetadataKind::Struct:
      // If the source type is AnyHashable, cast from that.
      if (isAnyHashableType(cast<StructMetadata>(srcType))) {
        return _dynamicCastFromAnyHashable(dest, src, srcType,
                                           targetType, flags);
      }
      LLVM_FALLTHROUGH;

    case MetadataKind::Enum:
    case MetadataKind::Optional: {
#if SWIFT_OBJC_INTEROP
      // If the source type is bridged to Objective-C, try to bridge.
      if (auto srcBridgeWitness = findBridgeWitness(srcType)) {
        return _dynamicCastValueToClassViaObjCBridgeable(dest, src, srcType,
                                                         targetType,
                                                         srcBridgeWitness,
                                                         flags);
      }
#endif
      return _fail(src, srcType, targetType, flags);
    }

    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Function:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::HeapGenericLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Metatype:
    case MetadataKind::Opaque:
    case MetadataKind::Tuple:
      return _fail(src, srcType, targetType, flags);
    }
    break;

  case MetadataKind::Existential:
    return _dynamicCastToExistential(dest, src, srcType,
                                     cast<ExistentialTypeMetadata>(targetType),
                                     flags);

  case MetadataKind::Metatype:
    return _dynamicCastToMetatype(dest, src, srcType,
                                  cast<MetatypeMetadata>(targetType),
                                  flags);
    
  case MetadataKind::ExistentialMetatype:
    return _dynamicCastToExistentialMetatype(dest, src, srcType,
                                 cast<ExistentialMetatypeMetadata>(targetType),
                                             flags);

  // Function types.
  case MetadataKind::Function: {
    return _dynamicCastToFunction(dest, src, srcType,
                                  cast<FunctionTypeMetadata>(targetType),
                                  flags);
  }

  case MetadataKind::Struct:
  case MetadataKind::Enum:
    switch (srcType->getKind()) {
    case MetadataKind::Class:
    case MetadataKind::ObjCClassWrapper:
    case MetadataKind::ForeignClass: {
      // Casts to AnyHashable.
      if (isAnyHashableType(targetType)) {
        return _dynamicCastToAnyHashable(dest, src, srcType, targetType, flags);
      }

#if SWIFT_OBJC_INTEROP
      // If the target type is bridged to Objective-C, try to bridge.
      if (auto targetBridgeWitness = findBridgeWitness(targetType)) {
        return _dynamicCastClassToValueViaObjCBridgeable(dest, src, srcType,
                                                         targetType,
                                                         targetBridgeWitness,
                                                         flags);
      }
      
      // If the source is an NSError, and the target is a bridgeable
      // Error, try to bridge.
      if (tryDynamicCastNSErrorToValue(dest, src, srcType, targetType, flags)) {
        return true;
      }
#endif

      break;
    }

    case MetadataKind::Struct:
      // Collection and AnyHashable casts.
      if (targetType->getKind() == MetadataKind::Struct) {
        return _dynamicCastStructToStruct(dest, src,
                                          cast<StructMetadata>(srcType),
                                          cast<StructMetadata>(targetType),
                                          flags);
      }
      break;

    case MetadataKind::Optional:
    case MetadataKind::Enum:
      // Casts to AnyHashable.
      if (isAnyHashableType(targetType)) {
        return _dynamicCastToAnyHashable(dest, src, srcType, targetType, flags);
      }
      break;

    case MetadataKind::Existential:
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Function:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::HeapGenericLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Metatype:
    case MetadataKind::Opaque:
    case MetadataKind::Tuple:
      break;
    }

    LLVM_FALLTHROUGH;

  // The non-polymorphic types.
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
    // If there's an exact type match, we're done.
    if (srcType == targetType)
      return _succeed(dest, src, srcType, flags);

    // If we have an existential, look at its dynamic type.
    if (auto srcExistentialType = dyn_cast<ExistentialTypeMetadata>(srcType)) {
      return _dynamicCastFromExistential(dest, src, srcExistentialType,
                                         targetType, flags);
    }

    // If both are tuple types, allow the cast to add/remove labels.
    if (srcType->getKind() == MetadataKind::Tuple &&
        targetType->getKind() == MetadataKind::Tuple) {
      return _dynamicCastTupleToTuple(dest, src,
                                      cast<TupleTypeMetadata>(srcType),
                                      cast<TupleTypeMetadata>(targetType),
                                      flags);
    }

    // Otherwise, we have a failure.
    return _fail(src, srcType, targetType, flags);
  }
  _failCorruptType(srcType);
}

static inline bool swift_isClassOrObjCExistentialTypeImpl(const Metadata *T) {
  auto kind = T->getKind();
  // Classes.
  if (Metadata::isAnyKindOfClass(kind))
    return true;
#if SWIFT_OBJC_INTEROP
  // ObjC existentials.
  if (kind == MetadataKind::Existential &&
      static_cast<const ExistentialTypeMetadata *>(T)->isObjC())
    return true;
  
  // Blocks are ObjC objects.
  if (kind == MetadataKind::Function) {
    auto fT = static_cast<const FunctionTypeMetadata *>(T);
    return fT->getConvention() == FunctionMetadataConvention::Block;
  }
#endif

  return false;
}

/******************************************************************************/
/********************************** Bridging **********************************/
/******************************************************************************/

#if SWIFT_OBJC_INTEROP
//===----------------------------------------------------------------------===//
// Bridging to and from Objective-C
//===----------------------------------------------------------------------===//

namespace {

// protocol _ObjectiveCBridgeable {
struct _ObjectiveCBridgeableWitnessTable {
  // associatedtype _ObjectiveCType : class
  const Metadata * (*ObjectiveCType)(
                     const Metadata *parentMetadata,
                     const _ObjectiveCBridgeableWitnessTable *witnessTable);

  // func _bridgeToObjectiveC() -> _ObjectiveCType
  SWIFT_CC(swift)
  HeapObject *(*bridgeToObjectiveC)(
                SWIFT_CONTEXT OpaqueValue *self, const Metadata *Self,
                const _ObjectiveCBridgeableWitnessTable *witnessTable);

  // class func _forceBridgeFromObjectiveC(x: _ObjectiveCType,
  //                                       inout result: Self?)
  SWIFT_CC(swift)
  void (*forceBridgeFromObjectiveC)(
         HeapObject *sourceValue,
         OpaqueValue *result,
         SWIFT_CONTEXT const Metadata *self,
         const Metadata *selfType,
         const _ObjectiveCBridgeableWitnessTable *witnessTable);

  // class func _conditionallyBridgeFromObjectiveC(x: _ObjectiveCType,
  //                                              inout result: Self?) -> Bool
  SWIFT_CC(swift)
  bool (*conditionallyBridgeFromObjectiveC)(
         HeapObject *sourceValue,
         OpaqueValue *result,
         SWIFT_CONTEXT const Metadata *self,
         const Metadata *selfType,
         const _ObjectiveCBridgeableWitnessTable *witnessTable);
};
// }

} // unnamed namespace

extern "C" const ProtocolDescriptor PROTOCOL_DESCR_SYM(s21_ObjectiveCBridgeable);

/// Dynamic cast from a value type that conforms to the _ObjectiveCBridgeable
/// protocol to a class type, first by bridging the value to its Objective-C
/// object representation and then by dynamic casting that object to the
/// resulting target type.
static bool _dynamicCastValueToClassViaObjCBridgeable(
               OpaqueValue *dest,
               OpaqueValue *src,
               const Metadata *srcType,
               const Metadata *targetType,
               const _ObjectiveCBridgeableWitnessTable *srcBridgeWitness,
               DynamicCastFlags flags) {
  // Bridge the source value to an object.
  auto srcBridgedObject =
    srcBridgeWitness->bridgeToObjectiveC(src, srcType, srcBridgeWitness);

  // Dynamic cast the object to the resulting class type.
  bool success;
  if (auto cast = _dynamicCastUnknownClass(srcBridgedObject, targetType,
                               flags & DynamicCastFlags::Unconditional)) {
    *reinterpret_cast<void **>(dest) = cast;
    success = true;
  } else {
    // We don't need the object anymore.
    swift_unknownRelease(srcBridgedObject);
    success = false;
  }

  // Clean up the source if we're supposed to.
  if (shouldDeallocateSource(success, flags)) {
    srcType->vw_destroy(src);
  }

  // We're done.
  return success;
}

/// Dynamic cast from a value type that conforms to the
/// _ObjectiveCBridgeable protocol to a class-bounded existential,
/// first by bridging the value to its Objective-C object
/// representation and then by dynamic-casting that object to the
/// resulting target type.
static bool _dynamicCastValueToClassExistentialViaObjCBridgeable(
              OpaqueValue *dest,
              OpaqueValue *src,
              const Metadata *srcType,
              const ExistentialTypeMetadata *targetType,
              const _ObjectiveCBridgeableWitnessTable *srcBridgeWitness,
              DynamicCastFlags flags) {
  // Bridge the source value to an object.
  auto srcBridgedObject =
    srcBridgeWitness->bridgeToObjectiveC(src, srcType, srcBridgeWitness);

  // Try to cast the object to the destination existential.
  DynamicCastFlags subFlags = DynamicCastFlags::TakeOnSuccess
                            | DynamicCastFlags::DestroyOnFailure;
  if (flags & DynamicCastFlags::Unconditional)
    subFlags |= DynamicCastFlags::Unconditional;
  bool success = _dynamicCastToExistential(
                   dest, 
                   (OpaqueValue *)&srcBridgedObject,
                   swift_getObjectType(srcBridgedObject),
                   targetType,
                   subFlags);

  // Clean up the source if we're supposed to.
  if (shouldDeallocateSource(success, flags)) {
    srcType->vw_destroy(src);
  }

  // We're done.
  return success;
}

/// Dynamic cast from a class type to a value type that conforms to the
/// _ObjectiveCBridgeable, first by dynamic casting the object to the
/// Objective-C class to which the value type is bridged, and then bridging
/// from that object to the value type via the witness table.
static bool _dynamicCastClassToValueViaObjCBridgeable(
               OpaqueValue *dest,
               OpaqueValue *src,
               const Metadata *srcType,
               const Metadata *targetType,
               const _ObjectiveCBridgeableWitnessTable *targetBridgeWitness,
               DynamicCastFlags flags) {
  // Determine the class type to which the target value type is bridged.
  auto targetBridgedClass =
    targetBridgeWitness->ObjectiveCType(targetType, targetBridgeWitness);

  // Dynamic cast the source object to the class type to which the target value
  // type is bridged. If we succeed, we can bridge from there; if we fail,
  // there's nothing more to do.
  void *srcObject = *reinterpret_cast<void * const *>(src);
  if (!_dynamicCastUnknownClass(srcObject,
                                targetBridgedClass,
                                flags & DynamicCastFlags::Unconditional)) {
    return _fail(src, srcType, targetType, flags);
  }

  // Unless we're always supposed to consume the input, retain the
  // object because the witness takes it at +1.
  bool alwaysConsumeSrc = (flags & DynamicCastFlags::TakeOnSuccess) &&
                          (flags & DynamicCastFlags::DestroyOnFailure);
  if (!alwaysConsumeSrc) {
    swift_unknownRetain(srcObject);
  }

  // The extra byte is for the tag.
  auto targetSize = targetType->getValueWitnesses()->size + 1;
  auto targetAlignMask = targetType->getValueWitnesses()->getAlignmentMask();

  // Object that frees a buffer when it goes out of scope.
  struct FreeBuffer {
    void *Buffer = nullptr;
    size_t size, alignMask;
    FreeBuffer(size_t size, size_t alignMask) :
      size(size), alignMask(alignMask) {}

    ~FreeBuffer() {
      if (Buffer)
        swift_slowDealloc(Buffer, size, alignMask);
    }
  } freeBuffer{targetSize, targetAlignMask};

  // Allocate a buffer to store the T? returned by bridging.
  // The extra byte is for the tag.
  const std::size_t inlineValueSize = 3 * sizeof(void*);
  alignas(std::max_align_t) char inlineBuffer[inlineValueSize + 1];
  void *optDestBuffer;
  if (targetType->getValueWitnesses()->getStride() <= inlineValueSize) {
    // Use the inline buffer.
    optDestBuffer = inlineBuffer;
  } else {
    // Allocate a buffer.
    optDestBuffer = swift_slowAlloc(targetSize, targetAlignMask);
    freeBuffer.Buffer = optDestBuffer;
  }

  // Initialize the buffer as an empty optional.
  swift_storeEnumTagSinglePayload((OpaqueValue *)optDestBuffer, targetType, 
                                  0, 1);

  // Perform the bridging operation.
  bool success;
  if (flags & DynamicCastFlags::Unconditional) {
    // For an unconditional dynamic cast, use forceBridgeFromObjectiveC.
    targetBridgeWitness->forceBridgeFromObjectiveC(
      (HeapObject *)srcObject, (OpaqueValue *)optDestBuffer,
      targetType, targetType, targetBridgeWitness);
    success = true;
  } else {
    // For a conditional dynamic cast, use conditionallyBridgeFromObjectiveC.
    success = targetBridgeWitness->conditionallyBridgeFromObjectiveC(
                (HeapObject *)srcObject, (OpaqueValue *)optDestBuffer,
                targetType, targetType, targetBridgeWitness);
  }

  // If we succeeded, take from the optional buffer into the
  // destination buffer.
  if (success) {
    targetType->vw_initializeWithTake(dest, (OpaqueValue *)optDestBuffer);
  }

  // Unless we're always supposed to consume the input, release the
  // input if we need to now.
  if (!alwaysConsumeSrc && shouldDeallocateSource(success, flags)) {
    swift_unknownRelease(srcObject);
  }

  return success;
}

static id bridgeAnythingNonVerbatimToObjectiveC(OpaqueValue *src,
                                                const Metadata *srcType,
                                                bool consume) {
  // We can always bridge objects verbatim.
  if (srcType->isAnyClass()) {
    id result;
    memcpy(&result, src, sizeof(id));
    if (!consume)
      swift_unknownRetain(result);
    return result;
  }
  
  // Dig through existential types.
  if (auto srcExistentialTy = dyn_cast<ExistentialTypeMetadata>(srcType)) {
    OpaqueValue *srcInnerValue;
    const Metadata *srcInnerType;
    bool isOutOfLine;
    bool canTake;
    
    unwrapExistential(src, srcExistentialTy,
                      srcInnerValue, srcInnerType, isOutOfLine, canTake);
    auto result = bridgeAnythingNonVerbatimToObjectiveC(srcInnerValue,
                                                        srcInnerType,
                                                        consume && canTake);
    // Clean up the existential, or its remains after taking the value from
    // it.
    if (consume) {
      if (canTake) {
        if (isOutOfLine) {
          // Should only be true of opaque existentials right now.
          assert(srcExistentialTy->getRepresentation()
                   == ExistentialTypeRepresentation::Opaque);
          auto container = reinterpret_cast<OpaqueExistentialContainer*>(src);
          srcInnerType->vw_deallocateBuffer(&container->Buffer);
        }
      } else {
        // We didn't take the value, so clean up the existential value.
        srcType->vw_destroy(src);
      }
    }
    return result;
  }
  
  // Handle metatypes.
  if (isa<ExistentialMetatypeMetadata>(srcType)
      || isa<MetatypeMetadata>(srcType)) {
    const Metadata *srcMetatypeValue;
    memcpy(&srcMetatypeValue, src, sizeof(srcMetatypeValue));
    
    // Class metatypes bridge to their class object.
    if (isa<ClassMetadata>(srcMetatypeValue)
        || isa<ObjCClassWrapperMetadata>(srcMetatypeValue)) {
      return (id)srcMetatypeValue->getClassObject();
    
    // ObjC protocols bridge to their Protocol object.
    } else if (auto existential
               = dyn_cast<ExistentialTypeMetadata>(srcMetatypeValue)) {
      if (existential->isObjC() && existential->Protocols.NumProtocols == 1) {
        // Though they're statically-allocated globals, Protocol inherits
        // NSObject's default refcounting behavior so must be retained.
        auto protocolObj = (id)existential->Protocols[0];
        return objc_retain(protocolObj);
      }
    }
  } else if (auto srcBridgeWitness = findBridgeWitness(srcType)) {
    // Bridge the source value to an object.
    auto srcBridgedObject =
      srcBridgeWitness->bridgeToObjectiveC(src, srcType, srcBridgeWitness);

    // Consume if the source object was passed in +1.
    if (consume)
      srcType->vw_destroy(src);

    return (id)srcBridgedObject;
  }

  // Fall back to boxing.
  return (id)bridgeAnythingToSwiftValueObject(src, srcType, consume);
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
id _swift_bridgeAnythingNonVerbatimToObjectiveC(OpaqueValue *src,
                                                const Metadata *srcType) {
  return bridgeAnythingNonVerbatimToObjectiveC(src, srcType, /*consume*/ true);
}

//===--- Bridging helpers for the Swift stdlib ----------------------------===//
// Functions that must discover and possibly use an arbitrary type's
// conformance to a given protocol.  See ../core/BridgeObjectiveC.swift for
// documentation.
//===----------------------------------------------------------------------===//

#define BRIDGING_CONFORMANCE_SYM \
  SELECT_MANGLING(WPVs19_BridgeableMetatypes21_ObjectiveCBridgeables, \
                  s19_BridgeableMetatypeVs21_ObjectiveCBridgeablesWP)

extern "C" const _ObjectiveCBridgeableWitnessTable BRIDGING_CONFORMANCE_SYM;

static const _ObjectiveCBridgeableWitnessTable *
findBridgeWitness(const Metadata *T) {
  auto w = swift_conformsToProtocol(T,
                                &PROTOCOL_DESCR_SYM(s21_ObjectiveCBridgeable));
  if (LLVM_LIKELY(w))
    return reinterpret_cast<const _ObjectiveCBridgeableWitnessTable *>(w);
  // Class and ObjC existential metatypes can be bridged, but metatypes can't
  // directly conform to protocols yet. Use a stand-in conformance for a type
  // that looks like a metatype value if the metatype can be bridged.
  switch (T->getKind()) {
  case MetadataKind::Metatype: {
    auto metaTy = static_cast<const MetatypeMetadata *>(T);
    if (metaTy->InstanceType->isAnyClass())
      return &BRIDGING_CONFORMANCE_SYM;
    break;
  }
  case MetadataKind::ExistentialMetatype: {
    auto existentialMetaTy =
      static_cast<const ExistentialMetatypeMetadata *>(T);
    if (existentialMetaTy->isObjC())
      return &BRIDGING_CONFORMANCE_SYM;
    break;
  }

  case MetadataKind::Class:
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::Existential:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
    break;
  }
  return nullptr;
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
const Metadata *_swift_getBridgedNonVerbatimObjectiveCType(
  const Metadata *value, const Metadata *T
) {
  // Classes and Objective-C existentials bridge verbatim.
  assert(!swift_isClassOrObjCExistentialTypeImpl(T));

  // Check if the type conforms to _BridgedToObjectiveC, in which case
  // we'll extract its associated type.
  if (const auto *bridgeWitness = findBridgeWitness(T)) {
    return bridgeWitness->ObjectiveCType(T, bridgeWitness);
  }
  
  return nullptr;
}

// @_silgen_name("_swift_bridgeNonVerbatimFromObjectiveCToAny")
// func _bridgeNonVerbatimFromObjectiveCToAny(
//     x: AnyObject,
//     inout result: Any?
// )
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
void
_swift_bridgeNonVerbatimFromObjectiveCToAny(HeapObject *sourceValue,
                                            OpaqueValue *destValue);

// @_silgen_name("_swift_bridgeNonVerbatimBoxedValue")
// func _bridgeNonVerbatimBoxedValue<NativeType>(
//     x: UnsafePointer<NativeType>,
//     inout result: NativeType?
// )
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
void
_swift_bridgeNonVerbatimBoxedValue(const OpaqueValue *sourceValue,
                                   OpaqueValue *destValue,
                                   const Metadata *nativeType);

// Try bridging by conversion to Any or boxing if applicable.
static bool tryBridgeNonVerbatimFromObjectiveCUniversal(
  HeapObject *sourceValue,
  const Metadata *nativeType,
  OpaqueValue *destValue
) {
  // If the type is the Any type, we can bridge by "upcasting" the object
  // to Any.
  if (auto nativeExistential = dyn_cast<ExistentialTypeMetadata>(nativeType)) {
    if (nativeExistential->Protocols.NumProtocols == 0) {
      _swift_bridgeNonVerbatimFromObjectiveCToAny(sourceValue,
                                                  destValue);
      return true;
    }
  }
  
  // Check if the value is a box containing a value of the desired type.
  if (auto srcBox = getAsSwiftValue((id)sourceValue)) {
    const Metadata *sourceType;
    const OpaqueValue *sourceBoxedValue;
    
    std::tie(sourceType, sourceBoxedValue) = getValueFromSwiftValue(srcBox);
    if (sourceType == nativeType) {
      _swift_bridgeNonVerbatimBoxedValue(sourceBoxedValue,
                                         destValue,
                                         nativeType);
      return true;
    }
  }
  
  return false;
}

// @_silgen_name("_swift_bridgeNonVerbatimFromObjectiveC")
// func _bridgeNonVerbatimFromObjectiveC<NativeType>(
//     x: AnyObject, 
//     nativeType: NativeType.Type
//     inout result: T?
// )
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
void
_swift_bridgeNonVerbatimFromObjectiveC(
  HeapObject *sourceValue,
  const Metadata *nativeType,
  OpaqueValue *destValue,
  const Metadata *nativeType_
) {
  if (tryBridgeNonVerbatimFromObjectiveCUniversal(sourceValue, nativeType,
                                                  destValue))
    return;

  // Check if the type conforms to _BridgedToObjectiveC.
  if (const auto *bridgeWitness = findBridgeWitness(nativeType)) {
    // Check if sourceValue has the _ObjectiveCType type required by the
    // protocol.
    const Metadata *objectiveCType =
        bridgeWitness->ObjectiveCType(nativeType, bridgeWitness);
      
    auto sourceValueAsObjectiveCType =
        const_cast<void*>(swift_dynamicCastUnknownClass(sourceValue,
                                                        objectiveCType));
      
    if (sourceValueAsObjectiveCType) {
      // The type matches.  _forceBridgeFromObjectiveC returns `Self`, so
      // we can just return it directly.
      bridgeWitness->forceBridgeFromObjectiveC(
        static_cast<HeapObject*>(sourceValueAsObjectiveCType),
        destValue, nativeType, nativeType, bridgeWitness);
      return;
    }
  }
  
  // Fail.
  swift::crash("value type is not bridged to Objective-C");
}

// @_silgen_name("_swift_bridgeNonVerbatimFromObjectiveCConditional")
// func _bridgeNonVerbatimFromObjectiveCConditional<NativeType>(
//   x: AnyObject, 
//   nativeType: T.Type,
//   inout result: T?
// ) -> Bool
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
bool
_swift_bridgeNonVerbatimFromObjectiveCConditional(
  HeapObject *sourceValue,
  const Metadata *nativeType,
  OpaqueValue *destValue,
  const Metadata *nativeType_
) {
  if (tryBridgeNonVerbatimFromObjectiveCUniversal(sourceValue, nativeType,
                                                  destValue))
    return true;

  // Local function that releases the source and returns false.
  auto fail = [&] () -> bool {
    swift_unknownRelease(sourceValue);
    return false;
  };
  
  // Check if the type conforms to _BridgedToObjectiveC.
  const auto *bridgeWitness = findBridgeWitness(nativeType);
  if (!bridgeWitness)
    return fail();

  // Dig out the Objective-C class type through which the native type
  // is bridged.
  const Metadata *objectiveCType =
    bridgeWitness->ObjectiveCType(nativeType, bridgeWitness);
        
  // Check whether we can downcast the source value to the Objective-C
  // type.
  auto sourceValueAsObjectiveCType =
    const_cast<void*>(swift_dynamicCastUnknownClass(sourceValue, 
                                                    objectiveCType));
  if (!sourceValueAsObjectiveCType)
    return fail();

  // If the type also conforms to _ConditionallyBridgedToObjectiveC,
  // use conditional bridging.
  return bridgeWitness->conditionallyBridgeFromObjectiveC(
    static_cast<HeapObject*>(sourceValueAsObjectiveCType),
    destValue, nativeType, nativeType, bridgeWitness);
}

// func _isBridgedNonVerbatimToObjectiveC<T>(x: T.Type) -> Bool
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
bool _swift_isBridgedNonVerbatimToObjectiveC(
  const Metadata *value, const Metadata *T
) {
  assert(!swift_isClassOrObjCExistentialTypeImpl(T));

  auto bridgeWitness = findBridgeWitness(T);
  return (bool)bridgeWitness;
}
#endif

// func _isClassOrObjCExistential<T>(x: T.Type) -> Bool
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
bool _swift_isClassOrObjCExistentialType(const Metadata *value,
                                                    const Metadata *T) {
  return swift_isClassOrObjCExistentialTypeImpl(T);
}

SWIFT_CC(swift)
const Metadata *swift::_swift_class_getSuperclass(const Metadata *theClass) {
  if (const ClassMetadata *classType = theClass->getClassObject())
    if (classHasSuperclass(classType))
      return swift_getObjCClassMetadata(classType->SuperClass);
  return nullptr;
}

SWIFT_CC(c) SWIFT_RUNTIME_EXPORT
bool swift_isClassType(const Metadata *type) {
  return Metadata::isAnyKindOfClass(type->getKind());
}

SWIFT_CC(c) SWIFT_RUNTIME_EXPORT
bool swift_isOptionalType(const Metadata *type) {
  return type->getKind() == MetadataKind::Optional;
}
