//===--- Casting.cpp - Swift Language Dynamic Casting Support -------------===//
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
// Implementations of the dynamic cast runtime functions.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Fallthrough.h"
#include "swift/Runtime/Enum.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "llvm/ADT/DenseMap.h"
#include "Debug.h"
#include "ExistentialMetadataImpl.h"

#include <dlfcn.h>

#include <cstring>
#include <mutex>
#include <sstream>

using namespace swift;
using namespace metadataimpl;

// Objective-C runtime entry points.
extern "C" const ClassMetadata* object_getClass(const void *);
extern "C" const char* class_getName(const ClassMetadata*);
extern "C" const ClassMetadata* class_getSuperclass(const ClassMetadata*);

// Aliases for Swift runtime entry points for Objective-C types.
extern "C" const void *swift_dynamicCastObjCProtocolConditional(
                         const void *object,
                         size_t numProtocols,
                         const ProtocolDescriptor * const *protocols);

static size_t
_setupClassMask() {
  void *handle = dlopen(nullptr, RTLD_LAZY);
  assert(handle);
  void *symbol = dlsym(handle, "objc_debug_isa_class_mask");
  if (symbol) {
    return *(uintptr_t *)symbol;
  }
  return ~(size_t)0;
}

size_t swift::swift_classMask = _setupClassMask();
uint8_t swift::swift_classShift = 0;

#if SWIFT_OBJC_INTEROP
/// Does this object use a tagged-pointer representation?
static bool isTaggedPointerOrNull(const void *object) {
  return ((long)object & 1) || ((long)object <= 0);
}
#endif

/// Dynamically cast a class object to a Swift class type.
const void *
swift::swift_dynamicCastClass(const void *object,
                              const ClassMetadata *targetType) {
#if SWIFT_OBJC_INTEROP
  assert(!targetType->isPureObjC());

  // Swift native classes never have a tagged-pointer representation.
  if (isTaggedPointerOrNull(object)) {
    return NULL;
  }

  auto isa = reinterpret_cast<const ClassMetadata *>(object_getClass(object));
#else
  auto isa = *reinterpret_cast<const ClassMetadata *const*>(object);
#endif

  do {
    if (isa == targetType) {
      return object;
    }
    isa = isa->SuperClass;
  } while (isa);

  return NULL;
}

/// Dynamically cast a class object to a Swift class type.
const void *
swift::swift_dynamicCastClassUnconditional(const void *object,
                                           const ClassMetadata *targetType) {
  auto value = swift_dynamicCastClass(object, targetType);
  if (value == nullptr) {
    swift::crash("Swift dynamic cast failed");
  }
  return value;
}

static const OpaqueValue *
_dynamicCastToExistential(const OpaqueValue *value,
                          const Metadata *sourceType,
                          const ExistentialTypeMetadata *targetType) {
  for (unsigned i = 0, n = targetType->Protocols.NumProtocols; i != n; ++i) {
    auto *protocol = targetType->Protocols[i];

    // Handle AnyObject directly.
    // FIXME: strcmp here is horribly slow.
    if (strcmp(protocol->Name, "_TtPSs9AnyObject_") == 0) {
      switch (sourceType->getKind()) {
      case MetadataKind::Class:
      case MetadataKind::ObjCClassWrapper:
      case MetadataKind::ForeignClass: // FIXME
        // Classes conform to AnyObject.
        break;

      case MetadataKind::Existential: {
        auto sourceExistential
          = static_cast<const ExistentialTypeMetadata*>(sourceType);
        // The existential conforms to AnyObject if it's class-constrained.
        if (sourceExistential->Flags.getClassConstraint()
            != ProtocolClassConstraint::Class)
          return nullptr;
        break;
      }
      
      case MetadataKind::ExistentialMetatype: // FIXME
      case MetadataKind::Function:
      case MetadataKind::Block: // FIXME
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

      continue;
    }

    // FIXME: Can't handle protocols that require witness tables.
    if (protocol->Flags.needsWitnessTable())
      return nullptr;

    // For Objective-C protocols, check whether we have a class that
    // conforms to the given protocol.
    switch (sourceType->getKind()) {
    case MetadataKind::Class: 
    case MetadataKind::ObjCClassWrapper: {
      const void *object
        = *reinterpret_cast<const void * const *>(value);
      if (swift_dynamicCastObjCProtocolConditional(object, 1, &protocol)) {
        continue;
      }

      return nullptr;
    }

    case MetadataKind::ForeignClass:
      return nullptr;

    case MetadataKind::Existential: // FIXME
    case MetadataKind::ExistentialMetatype: // FIXME
    case MetadataKind::Function:
    case MetadataKind::Block: // FIXME
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

    return nullptr;
  }

  return value;
}

const void *
swift::swift_dynamicCastUnknownClass(const void *object,
                                     const Metadata *targetType) {
  switch (targetType->getKind()) {
  case MetadataKind::Class: {
    auto targetClassType = static_cast<const ClassMetadata *>(targetType);
    return swift_dynamicCastClass(object, targetClassType);
  }

  case MetadataKind::ObjCClassWrapper: {
    auto targetClassType
      = static_cast<const ObjCClassWrapperMetadata *>(targetType)->Class;
    return swift_dynamicCastObjCClass(object, targetClassType);
  }

  case MetadataKind::ForeignClass: // FIXME

  case MetadataKind::Existential:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::Block:
  case MetadataKind::HeapArray:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    swift::crash("Swift dynamic cast failed");
  }
  swift::crash("bad metadata kind!");
}

const void *
swift::swift_dynamicCastUnknownClassUnconditional(const void *object,
                                                  const Metadata *targetType) {
  switch (targetType->getKind()) {
  case MetadataKind::Class: {
    auto targetClassType = static_cast<const ClassMetadata *>(targetType);
    return swift_dynamicCastClassUnconditional(object, targetClassType);
  }

  case MetadataKind::ObjCClassWrapper: {
    auto targetClassType
      = static_cast<const ObjCClassWrapperMetadata *>(targetType)->Class;
    return swift_dynamicCastObjCClassUnconditional(object, targetClassType);
  }

  case MetadataKind::ForeignClass: // FIXME

  case MetadataKind::Existential:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::Block:
  case MetadataKind::HeapArray:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    swift::crash("Swift dynamic cast failed");
  }
  swift::crash("bad metadata kind!");
}

const Metadata *
swift::swift_dynamicCastMetatype(const Metadata *sourceType,
                                 const Metadata *targetType) {
  auto origSourceType = sourceType;

  switch (targetType->getKind()) {
  case MetadataKind::ObjCClassWrapper:
    // Get the actual class object.
    targetType = static_cast<const ObjCClassWrapperMetadata*>(targetType)
      ->Class;
    SWIFT_FALLTHROUGH;
  case MetadataKind::Class:
    // The source value must also be a class; otherwise the cast fails.
    switch (sourceType->getKind()) {
    case MetadataKind::ObjCClassWrapper:
      // Get the actual class object.
      sourceType = static_cast<const ObjCClassWrapperMetadata*>(sourceType)
        ->Class;
      SWIFT_FALLTHROUGH;
    case MetadataKind::Class: {
      // Check if the source is a subclass of the target.
      // We go through ObjC lookup to deal with potential runtime magic in ObjC
      // land.
      if (swift_dynamicCastObjCClassMetatype((const ClassMetadata*)sourceType,
                                             (const ClassMetadata*)targetType))
        return origSourceType;
      return nullptr;
    }
    case MetadataKind::ForeignClass: // FIXME
    case MetadataKind::Existential:
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Function:
    case MetadataKind::Block:
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
      
  case MetadataKind::ForeignClass: // FIXME
  case MetadataKind::Existential:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::Block:
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
    return origSourceType;
  }
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
    SWIFT_FALLTHROUGH;
  case MetadataKind::Class:
    // The source value must also be a class; otherwise the cast fails.
    switch (sourceType->getKind()) {
    case MetadataKind::ObjCClassWrapper:
      // Get the actual class object.
      sourceType = static_cast<const ObjCClassWrapperMetadata*>(sourceType)
        ->Class;
      SWIFT_FALLTHROUGH;
    case MetadataKind::Class: {
      // Check if the source is a subclass of the target.
      // We go through ObjC lookup to deal with potential runtime magic in ObjC
      // land.
      swift_dynamicCastObjCClassMetatypeUnconditional(
                                            (const ClassMetadata*)sourceType,
                                            (const ClassMetadata*)targetType);
      // If we returned, then the cast succeeded.
      return origSourceType;
    }
    case MetadataKind::ForeignClass: // FIXME
    case MetadataKind::Existential:
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Function:
    case MetadataKind::Block:
    case MetadataKind::HeapArray:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::Metatype:
    case MetadataKind::Enum:
    case MetadataKind::Opaque:
    case MetadataKind::PolyFunction:
    case MetadataKind::Struct:
    case MetadataKind::Tuple:
      swift::crash("Swift dynamic cast failed");
    }
    break;
      
  case MetadataKind::ForeignClass: // FIXME
  case MetadataKind::Existential:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::Block:
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
      swift::crash("Swift dynamic cast failed");
    return origSourceType;
  }
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
      if (!swift_dynamicCastUnknownClass(object, targetType))
        return nullptr;
      break;
    }
    case MetadataKind::ForeignClass: // FIXME
    case MetadataKind::Existential:
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Function:
    case MetadataKind::Block:
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
    return _dynamicCastToExistential(value, sourceType,
                                   (const ExistentialTypeMetadata*)targetType);
    
  case MetadataKind::ForeignClass: // FIXME
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::Block:
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
      swift_dynamicCastUnknownClassUnconditional(object, targetType);
      break;
    }
    case MetadataKind::ForeignClass: // FIXME
    case MetadataKind::Existential:
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Function:
    case MetadataKind::Block:
    case MetadataKind::HeapArray:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::Metatype:
    case MetadataKind::Enum:
    case MetadataKind::Opaque:
    case MetadataKind::PolyFunction:
    case MetadataKind::Struct:
    case MetadataKind::Tuple:
      swift::crash("Swift dynamic cast failed");
    }
    break;

  case MetadataKind::Existential: {
    auto r = _dynamicCastToExistential(value, sourceType,
                                   (const ExistentialTypeMetadata*)targetType);
    if (!r)
      swift::crash("Swift dynamic cast failed");
    return r;
  }
    
  case MetadataKind::ForeignClass: // FIXME
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::Block:
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
      swift::crash("Swift dynamic cast failed");
    break;
  }
  
  return value;  
}

static std::string typeNameForObjCClass(const ClassMetadata *cls)
{
    const char* objc_class_name = class_getName(cls);
    std::stringstream ostream;
    ostream << "CSo" << strlen(objc_class_name) << objc_class_name;
    ostream.flush();
    return ostream.str();
}

/// A cache used for swift_conformsToProtocol.
static llvm::DenseMap<std::pair<const Metadata*, const ProtocolDescriptor*>,
                      const void *> FoundProtocolConformances;
/// Read-write lock used to guard FoundProtocolConformances during lookup
static pthread_rwlock_t FoundProtocolConformancesLock
  = PTHREAD_RWLOCK_INITIALIZER;

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

  // See whether we cached this lookup.
  pthread_rwlock_rdlock(&FoundProtocolConformancesLock);
  auto cached = FoundProtocolConformances.find({type, protocol});
  if (cached != FoundProtocolConformances.end()) {
    pthread_rwlock_unlock(&FoundProtocolConformancesLock);
    return cached->second;
  }
  pthread_rwlock_unlock(&FoundProtocolConformancesLock);

  auto origType = type;
  auto origProtocol = protocol;
  /// Cache and return the result.
  auto cacheResult = [&](const void *result) -> const void * {
    pthread_rwlock_wrlock(&FoundProtocolConformancesLock);
    FoundProtocolConformances.insert({{origType, origProtocol}, result});
    pthread_rwlock_unlock(&FoundProtocolConformancesLock);
    return result;
  };

recur:

  std::string TypeName;

  switch (type->getKind()) {
  case MetadataKind::ObjCClassWrapper: {
    auto wrapper = static_cast<const ObjCClassWrapperMetadata*>(type);
    TypeName = typeNameForObjCClass(wrapper->Class);
    break;
  }
  case MetadataKind::ForeignClass: {
    auto metadata = static_cast<const ForeignClassMetadata*>(type);
    TypeName = metadata->Name;
    break;
  }
  case MetadataKind::Class: {
    auto theClass = static_cast<const ClassMetadata *>(type);
    if (theClass->isPureObjC()) {
      TypeName = typeNameForObjCClass(theClass);
      break;
    }
  }
  [[clang::fallthrough]];  // FALL THROUGH to nominal type check
  case MetadataKind::Tuple:
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::Function:
  case MetadataKind::Block:
  case MetadataKind::Existential:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Metatype: {
    // FIXME: Only check nominal types for now.
    auto *descriptor = type->getNominalTypeDescriptor();
    if (!descriptor)
      return cacheResult(nullptr);
    TypeName = std::string(descriptor->Name);
    break;
  }
      
  // Values should never use these metadata kinds.
  case MetadataKind::PolyFunction:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapArray:
    assert(false);
    return nullptr;
  }
  
  // Derive the symbol name that the witness table ought to have.
  // _TWP <protocol conformance>
  // protocol conformance ::= <type> <protocol> <module>
  
  std::string mangledName = "_TWP";
  mangledName += TypeName;
  // The name in the protocol descriptor gets mangled as a protocol type
  // P <name> _
  const char *begin = protocol->Name + 1;
  const char *end = protocol->Name + strlen(protocol->Name) - 1;
  mangledName.append(begin, end);
  
  // Look up the symbol for the conformance everywhere.
  if (const void *result = dlsym(RTLD_DEFAULT, mangledName.c_str())) {
    return cacheResult(result);
  }
  
  // If the type was a class, try again with the superclass.
  // FIXME: This isn't sound if the conformance isn't heritable, but the
  // protocols we're using with this hack all should be.
  switch (type->getKind()) {
  case MetadataKind::Class: {
    auto theClass = static_cast<const ClassMetadata *>(type);
    type = theClass->SuperClass;
    if (!type)
      return cacheResult(nullptr);
    goto recur;
  }
  case MetadataKind::ObjCClassWrapper: {
    auto wrapper = static_cast<const ObjCClassWrapperMetadata *>(type);
    auto super = class_getSuperclass(wrapper->Class);
    if (!super)
      return cacheResult(nullptr);
    
    type = swift_getObjCClassMetadata(super);
    goto recur;
  }
  case MetadataKind::ForeignClass: {
    auto theClass = static_cast<const ForeignClassMetadata *>(type);
    auto super = theClass->SuperClass;
    if (!super)
      return cacheResult(nullptr);

    type = super;
    goto recur;
  }

  case MetadataKind::Tuple:
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::Function:
  case MetadataKind::Block:
  case MetadataKind::Existential:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Metatype:
    return cacheResult(nullptr);
      
  // Values should never use these metadata kinds.
  case MetadataKind::PolyFunction:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapArray:
    assert(false);
    return nullptr;
  }
}

static const Metadata *getDynamicTypeMetadata(OpaqueValue *value,
                                              const Metadata *type) {
  if (type->getKind() == MetadataKind::Existential) {
    const auto existentialMetadata =
        static_cast<const ExistentialTypeMetadata *>(type);
    return existentialMetadata->getDynamicType(value);
  }
  return type;
}

static const void *
findWitnessTableForDynamicCastToExistential1(OpaqueValue *sourceValue,
                                             const Metadata *sourceType,
                                             const Metadata *destType) {
  if (destType->getKind() != MetadataKind::Existential)
    swift::crash("Swift protocol conformance check failed: "
                 "destination type is not an existential");

  auto destExistentialMetadata =
      static_cast<const ExistentialTypeMetadata *>(destType);

  if (destExistentialMetadata->Protocols.NumProtocols != 1)
    swift::crash("Swift protocol conformance check failed: "
                 "destination type conforms more than to one protocol");

  auto destProtocolDescriptor = destExistentialMetadata->Protocols[0];

  if (sourceType->getKind() == MetadataKind::Existential)
    swift::crash("Swift protocol conformance check failed: "
                 "source type is an existential");

  return swift_conformsToProtocol(sourceType, destProtocolDescriptor, nullptr);
}

// func _stdlib_conformsToProtocol<SourceType, DestType>(
//     value: SourceType, _: DestType.Type
// ) -> Bool
extern "C" bool
swift_stdlib_conformsToProtocol(
    OpaqueValue *sourceValue, const Metadata *_destType,
    const Metadata *sourceType, const Metadata *destType) {
  // Existentials don't carry complete type information about the value, but
  // it is necessary to find the witness tables.  Find the dynamic type and
  // use it instead.
  sourceType = getDynamicTypeMetadata(sourceValue, sourceType);
  auto vw = findWitnessTableForDynamicCastToExistential1(sourceValue,
                                                         sourceType, destType);
  sourceType->vw_destroy(sourceValue);
  return vw != nullptr;
}

// Work around a really dumb clang bug where it doesn't instantiate
// the return type first.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

// func _stdlib_dynamicCastToExistential1Unconditional<SourceType, DestType>(
//     value: SourceType,
//     _: DestType.Type
// ) -> DestType
extern "C" FixedOpaqueExistentialContainer<1>
swift_stdlib_dynamicCastToExistential1Unconditional(
    OpaqueValue *sourceValue, const Metadata *_destType,
    const Metadata *sourceType, const Metadata *destType) {
  // Existentials don't carry complete type information about the value, but
  // it is necessary to find the witness tables.  Find the dynamic type and
  // use it instead.
  sourceType = getDynamicTypeMetadata(sourceValue, sourceType);
  auto vw = findWitnessTableForDynamicCastToExistential1(sourceValue,
                                                         sourceType, destType);
  if (!vw)
    swift::crash("Swift dynamic cast failed: "
                 "type does not conform to the protocol");

  // Note: the 'sourceType' has been adjusted to the dynamic type of the value.
  // It is important so that we don't return a value with Existential metadata.
  using box = OpaqueExistentialBox<1>;

  box::Container outValue;
  outValue.Header.Type = sourceType;
  outValue.WitnessTables[0] = vw;
  sourceType->vw_initializeBufferWithTake(outValue.getBuffer(), sourceValue);

  return outValue;
}

#pragma clang diagnostic pop

// The return type is incorrect.  It is only important that it is
// passed using 'sret'.
extern "C" OpaqueExistentialContainer
_TFSs24_injectValueIntoOptionalU__FQ_GSqQ__(OpaqueValue *value,
                                            const Metadata *T);

// The return type is incorrect.  It is only important that it is
// passed using 'sret'.
extern "C" OpaqueExistentialContainer
_TFSs26_injectNothingIntoOptionalU__FT_GSqQ__(const Metadata *T);

// func _stdlib_dynamicCastToExistential1<SourceType, DestType>(
//     value: SourceType,
//     _: DestType.Type
// ) -> DestType?
//
// The return type is incorrect.  It is only important that it is
// passed using 'sret'.
extern "C" OpaqueExistentialContainer swift_stdlib_dynamicCastToExistential1(
    OpaqueValue *sourceValue, const Metadata *_destType,
    const Metadata *sourceType, const Metadata *destType) {
  // Existentials don't carry complete type information about the value, but
  // it is necessary to find the witness tables.  Find the dynamic type and
  // use it instead.
  sourceType = getDynamicTypeMetadata(sourceValue, sourceType);
  auto vw = findWitnessTableForDynamicCastToExistential1(sourceValue,
                                                         sourceType, destType);
  if (!vw) {
    sourceType->vw_destroy(sourceValue);
    return _TFSs26_injectNothingIntoOptionalU__FT_GSqQ__(destType);
  }

  // Note: the 'sourceType' has been adjusted to the dynamic type of the value.
  // It is important so that we don't return a value with Existential metadata.
  using box = OpaqueExistentialBox<1>;

  box::Container outValue;
  outValue.Header.Type = sourceType;
  outValue.WitnessTables[0] = vw;
  sourceType->vw_initializeBufferWithTake(outValue.getBuffer(), sourceValue);

  return _TFSs24_injectValueIntoOptionalU__FQ_GSqQ__(
      reinterpret_cast<OpaqueValue *>(&outValue), destType);
}

//===----------------------------------------------------------------------===//
// Bridging to and from Objective-C
//===----------------------------------------------------------------------===//

namespace {

// protocol _BridgedToObjectiveC {
struct _BridgedToObjectiveCWitnessTable {
  // typealias ObjectiveCType: class
  const Metadata *ObjectiveCType;

  // class func getObjectiveCType() -> Any.Type
  const Metadata *(*getObjectiveCType)(const Metadata *self,
                                       const Metadata *selfType);

  // func bridgeToObjectiveC() -> ObjectiveCType
  HeapObject *(*bridgeToObjectiveC)(OpaqueValue *self, const Metadata *Self);
  // func bridgeFromObjectiveC(x: ObjectiveCType) -> Self?
  OpaqueExistentialContainer (*bridgeFromObjectiveC)(HeapObject *sourceValue,
                                                     const Metadata *self,
                                                     const Metadata *selfType);
};
// }

// protocol _ConditionallyBridgedToObjectiveC {
struct _ConditionallyBridgedToObjectiveCWitnessTable {
  // My untrained eye can't find this offset in the generated LLVM IR,
  // but I do see it being applied in x86 assembly.  It disappears
  // when inheritance from _BridgedToObjectiveC is removed.  If it
  // presents any portability problems we can drop that inheritance
  // relationship.
  const void *const probablyPointsAtBridgedToObjectiveCWitnessTable;

  // class func isBridgedToObjectiveC() -> bool
  bool (*isBridgedToObjectiveC)(const Metadata *value, const Metadata *T);
};
// }

} // unnamed namespace

extern "C" const ProtocolDescriptor _TMpSs20_BridgedToObjectiveC;
extern "C" const ProtocolDescriptor _TMpSs33_ConditionallyBridgedToObjectiveC;

//===--- Bridging helpers for the Swift stdlib ----------------------------===//
// Functions that must discover and possibly use an arbitrary type's
// conformance to a given protocol.  See ../core/BridgeObjectiveC.swift for
// documentation.
//===----------------------------------------------------------------------===//
static const _BridgedToObjectiveCWitnessTable *
findBridgeWitness(const Metadata *T) {
  auto w = swift_conformsToProtocol(T, &_TMpSs20_BridgedToObjectiveC, nullptr);
  return reinterpret_cast<const _BridgedToObjectiveCWitnessTable *>(w);
}

static const _ConditionallyBridgedToObjectiveCWitnessTable *
findConditionalBridgeWitness(const Metadata *T) {
  auto w = swift_conformsToProtocol(
      T, &_TMpSs33_ConditionallyBridgedToObjectiveC, nullptr);

  return reinterpret_cast<
      const _ConditionallyBridgedToObjectiveCWitnessTable *>(w);
}

static inline bool swift_isClassOrObjCExistentialImpl(const Metadata *T) {
  auto kind = T->getKind();
  return kind == MetadataKind::Class ||
         kind == MetadataKind::ObjCClassWrapper ||
         (kind == MetadataKind::Existential &&
          static_cast<const ExistentialTypeMetadata *>(T)->isObjC());
}

extern "C" HeapObject *swift_bridgeNonVerbatimToObjectiveC(
  OpaqueValue *value, const Metadata *T
) {
  assert(!swift_isClassOrObjCExistentialImpl(T));

  auto const bridgeWitness = findBridgeWitness(T);

  if (bridgeWitness) {
    if (auto conditionalWitness = findConditionalBridgeWitness(T)) {
      if (!conditionalWitness->isBridgedToObjectiveC(T, T))
        return nullptr;
    }
    auto result = bridgeWitness->bridgeToObjectiveC(value, T);
    // Witnesses take 'self' at +0, so we still need to consume the +1 argument.
    T->vw_destroy(value);
    return result;
  }

  return nullptr;
}

extern "C" const Metadata *swift_getBridgedNonVerbatimObjectiveCType(
  const Metadata *value, const Metadata *T
) {
  // Classes and Objective-C existentials bridge verbatim.
  assert(!swift_isClassOrObjCExistentialImpl(T));

  // Check if the type conforms to _BridgedToObjectiveC, in which case
  // we'll extract its associated type.
  if (const auto *bridgeWitness = findBridgeWitness(T)) {
    return bridgeWitness->getObjectiveCType(T, T);
  }
  
  return nullptr;
}

// @asmname("swift_bridgeNonVerbatimFromObjectiveC")
// func _bridgeNonVerbatimFromObjectiveC<NativeType>(
//     x: AnyObject, nativeType: NativeType.Type
// ) -> NativeType?
extern "C" OpaqueExistentialContainer
swift_bridgeNonVerbatimFromObjectiveC(
  HeapObject *sourceValue,
  const Metadata *nativeType,
  const Metadata *nativeType_
) {
  // Check if the type conforms to _BridgedToObjectiveC.
  const auto *bridgeWitness = findBridgeWitness(nativeType);
  if (bridgeWitness) {
    // if the type also conforms to _ConditionallyBridgedToObjectiveC,
    // make sure it bridges at runtime
    auto conditionalWitness = findConditionalBridgeWitness(nativeType);
    if (
      conditionalWitness == nullptr
      || conditionalWitness->isBridgedToObjectiveC(nativeType, nativeType)
    ) {
      // Check if sourceValue has the ObjectiveCType type required by the
      // protocol.
      const Metadata *objectiveCType =
          bridgeWitness->getObjectiveCType(nativeType, nativeType);
        
      auto sourceValueAsObjectiveCType =
          const_cast<void*>(swift_dynamicCastUnknownClass(sourceValue,
                                                          objectiveCType));
        
      if (sourceValueAsObjectiveCType) {
        // The type matches.  bridgeFromObjectiveC returns `Self?`;
        // this function returns `NativeType`, so we don't need to
        // re-wrap the optional.
        return bridgeWitness->bridgeFromObjectiveC(
          static_cast<HeapObject*>(sourceValueAsObjectiveCType),
            nativeType, nativeType);
      }
    }
  }

  // return nil
  swift_unknownRelease(sourceValue);
  return _TFSs26_injectNothingIntoOptionalU__FT_GSqQ__(nativeType);
}

// func isBridgedNonVerbatimToObjectiveC<T>(x: T.Type) -> Bool
extern "C" bool swift_isBridgedNonVerbatimToObjectiveC(
  const Metadata *value, const Metadata *T
) {
  assert(!swift_isClassOrObjCExistentialImpl(T));

  auto bridgeWitness = findBridgeWitness(T);

  if (bridgeWitness) {
    auto conditionalWitness = findConditionalBridgeWitness(T);
    return !conditionalWitness ||
           conditionalWitness->isBridgedToObjectiveC(value, T);
  }

  return false;
}

// func isClassOrObjCExistential<T>(x: T.Type) -> Bool
extern "C" bool swift_isClassOrObjCExistential(const Metadata *value,
                                               const Metadata *T) {
  return swift_isClassOrObjCExistentialImpl(T);
}
