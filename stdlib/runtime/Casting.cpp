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

#include <dlfcn.h>

#include <cstring>
#include <mutex>
#include <sstream>

using namespace swift;

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

/// The primary entrypoint.
const void *
swift::swift_dynamicCastClass(const void *object,
                              const ClassMetadata *targetType) {
#if SWIFT_OBJC_INTEROP
  if (targetType->isPureObjC()) {
    return swift_dynamicCastObjCClass(object, targetType);
  }
  // Swift cannot subclass tagged classes
  // The tag big is either high or low.
  // We need to handle both scenarios for now.
  if (((long)object & 1) || ((long)object <= 0)) {
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

/// The primary entrypoint.
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

  case MetadataKind::Existential: {
    auto r = _dynamicCastToExistential((const OpaqueValue*)&object,
                                     object_getClass(object),
                                     (const ExistentialTypeMetadata*)targetType);
    if (!r)
      return nullptr;
    return object;
  }
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::ForeignClass: // FIXME
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

  case MetadataKind::Existential: {
    auto r = _dynamicCastToExistential((const OpaqueValue*)&object,
                                     object_getClass(object),
                                     (const ExistentialTypeMetadata*)targetType);
    if (!r)
      swift::crash("Swift dynamic cast failed");
    return object;
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
    swift::crash("Swift dynamic cast failed");
  }

  return swift_dynamicCastClassUnconditional(object, targetClassType);
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
      if (!swift_dynamicCast(object, targetType))
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
      swift_dynamicCastUnconditional(object, targetType);
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
