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

#include "swift/Basic/LLVM.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Runtime/Enum.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "llvm/ADT/DenseMap.h"
#include "Debug.h"
#include "ExistentialMetadataImpl.h"
#include "Private.h"

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
extern "C" const Metadata *swift_getObjectType(const void *);

// Aliases for Swift runtime entry points for Objective-C types.
extern "C" const void *swift_dynamicCastObjCProtocolConditional(
                         const void *object,
                         size_t numProtocols,
                         const ProtocolDescriptor * const *protocols);

/// Report a dynamic cast failure.
LLVM_ATTRIBUTE_NORETURN
LLVM_ATTRIBUTE_ALWAYS_INLINE // Minimize trashed registers
static void _dynamicCastFailure(const Metadata *sourceType,
                                const Metadata *targetType) {
  swift::crash("Swift dynamic cast failure");
}

/// Report a corrupted type object.
LLVM_ATTRIBUTE_NORETURN
LLVM_ATTRIBUTE_ALWAYS_INLINE // Minimize trashed registers
static void _failCorruptType(const Metadata *type) {
  swift::crash("Corrupt Swift type object");
}

/// A convenient method for failing out of a dynamic cast.
static bool _fail(OpaqueValue *srcValue, const Metadata *srcType,
                  const Metadata *targetType, DynamicCastFlags flags) {
  if (flags & DynamicCastFlags::Unconditional)
    _dynamicCastFailure(srcType, targetType);
  if (flags & DynamicCastFlags::DestroyOnFailure)
    srcType->vw_destroy(srcValue);
  return false;
}

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

static bool _unknownClassConformsToObjCProtocol(const OpaqueValue *value,
                                          const ProtocolDescriptor *protocol) {
  const void *object
    = *reinterpret_cast<const void * const *>(value);
  return swift_dynamicCastObjCProtocolConditional(object, 1, &protocol);
}

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
                                const void **conformance) {
  // Handle AnyObject directly.
  // FIXME: strcmp here is horribly slow.
  if (strcmp(protocol->Name, "_TtPSs9AnyObject_") == 0) {
    switch (type->getKind()) {
    case MetadataKind::Class:
    case MetadataKind::ObjCClassWrapper:
    case MetadataKind::ForeignClass:
      // Classes conform to AnyObject.
      return true;

    case MetadataKind::Existential: {
      auto sourceExistential = cast<ExistentialTypeMetadata>(type);
      // The existential conforms to AnyObject if it's class-constrained.
      return sourceExistential->isClassBounded();
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
      return false;
    }
    _failCorruptType(type);
  }

  // FIXME: Can't handle protocols that require witness tables.
  if (protocol->Flags.needsWitnessTable())
    return false;

  // For Objective-C protocols, check whether we have a class that
  // conforms to the given protocol.
  switch (type->getKind()) {
  case MetadataKind::Class:
    if (value) {
      return _unknownClassConformsToObjCProtocol(value, protocol);
    } else {
      return _swift_classConformsToObjCProtocol(type, protocol);
    }

  case MetadataKind::ObjCClassWrapper: {
    if (value) {
      return _unknownClassConformsToObjCProtocol(value, protocol);
    } else {
      auto wrapper = cast<ObjCClassWrapperMetadata>(type);
      return _swift_classConformsToObjCProtocol(wrapper->Class, protocol);
    }
  }

  case MetadataKind::ForeignClass:
    if (value)
      return _unknownClassConformsToObjCProtocol(value, protocol);
    return false;

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
    return false;
  }

  return false;
}

/// Check whether a type conforms to the given protocols, filling in a
/// list of conformances.
static bool _conformsToProtocols(const OpaqueValue *value,
                                 const Metadata *type,
                                 const ProtocolDescriptorList &protocols,
                                 const void **conformances) {
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

static const OpaqueValue *
_dynamicCastToExistential(const OpaqueValue *value,
                          const Metadata *sourceType,
                          const ExistentialTypeMetadata *targetType) {
   for (unsigned i = 0, n = targetType->Protocols.NumProtocols; i != n; ++i) {
    auto *protocol = targetType->Protocols[i];
    if (!_conformsToProtocol(value, sourceType, protocol, nullptr))
      return nullptr;
  }

  return value;
}

/// Given a possibly-existential value, find its dynamic type and the
/// address of its storage.
static void findDynamicValueAndType(OpaqueValue *value, const Metadata *type,
                                    OpaqueValue *&outValue,
                                    const Metadata *&outType) {
  switch (type->getKind()) {
  case MetadataKind::Class:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass: {
    // TODO: avoid unnecessary repeat lookup of
    // ObjCClassWrapper/ForeignClass when the type matches.
    outValue = value;
    outType = swift_getObjectType(*reinterpret_cast<void**>(value));
    return;
  }

  case MetadataKind::Existential: {
    auto existentialType = cast<ExistentialTypeMetadata>(type);
    if (existentialType->isClassBounded()) {
      auto existential =
        reinterpret_cast<ClassExistentialContainer*>(value);
      outValue = (OpaqueValue*) &existential->Value;
      outType = swift_getObjectType(existential->Value);
      return;
    } else {
      auto existential =
        reinterpret_cast<OpaqueExistentialContainer*>(value);
      OpaqueValue *existentialValue =
        existential->Type->vw_projectBuffer(&existential->Buffer);
      findDynamicValueAndType(existentialValue, existential->Type,
                              outValue, outType);
      return;
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
  case MetadataKind::Block:
  case MetadataKind::HeapArray:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    outValue = value;
    outType = type;
    return;
  }
  _failCorruptType(type);
}

/// Perform a dynamic cast to an existential type.
static bool _dynamicCastToExistential(OpaqueValue *dest,
                                      OpaqueValue *src,
                                      const Metadata *srcType,
                                      const ExistentialTypeMetadata *targetType,
                                      DynamicCastFlags flags) {
  // Find the actual type of the source.
  OpaqueValue *srcDynamicValue;
  const Metadata *srcDynamicType;
  findDynamicValueAndType(src, srcType, srcDynamicValue, srcDynamicType);

  // The representation of an existential is different for
  // class-bounded protocols.
  if (targetType->isClassBounded()) {
    auto destExistential =
      reinterpret_cast<ClassExistentialContainer*>(dest);

    // Check for protocol conformances and fill in the witness tables.
    if (!_conformsToProtocols(srcDynamicValue, srcDynamicType,
                              targetType->Protocols,
                              destExistential->getWitnessTables()))
      return _fail(srcDynamicValue, srcDynamicType, targetType, flags);

    auto object = *(reinterpret_cast<HeapObject**>(srcDynamicValue));
    destExistential->Value = object;
    if (!(flags & DynamicCastFlags::TakeOnSuccess)) {
      swift_retain_noresult(object);
    }
    return true;

  } else {
    auto destExistential =
      reinterpret_cast<OpaqueExistentialContainer*>(dest);

    // Check for protocol conformances and fill in the witness tables.
    if (!_conformsToProtocols(srcDynamicValue, srcDynamicType,
                              targetType->Protocols,
                              destExistential->getWitnessTables()))
      return _fail(srcDynamicValue, srcDynamicType, targetType, flags);

    // Fill in the type and value.
    destExistential->Type = srcDynamicType;
    if (flags & DynamicCastFlags::TakeOnSuccess) {
      srcDynamicType->vw_initializeBufferWithTake(&destExistential->Buffer,
                                                  srcDynamicValue);
    } else {
      srcDynamicType->vw_initializeBufferWithCopy(&destExistential->Buffer,
                                                  srcDynamicValue);
    }
    return true;
  }
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
      _dynamicCastFailure(sourceType, targetType);
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
      _dynamicCastFailure(sourceType, targetType);
    return origSourceType;
  }
}

/// Do a dynamic cast to the target class.
static bool _dynamicCastUnknownClass(OpaqueValue *dest,
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
#if SWIFT_OBJC_INTEROP
      swift_unknownRetain(result);
#else
      swift_retain(result);
#endif
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
#if SWIFT_OBJC_INTEROP
      swift_unknownRelease(object);
#else
      swift_release(object);
#endif
    }
    return false;
  }

  // Otherwise, store to the destination and return true.
  *destSlot = result;
  if (!(flags & DynamicCastFlags::TakeOnSuccess)) {
#if SWIFT_OBJC_INTEROP
    swift_unknownRetain(result);
#else
    swift_retain(result);
#endif
  }
  return true;
}

/// Perform a dynamic cast from an existential type to some kind of
/// class type.
static bool _dynamicCastToUnknownClassFromExistential(OpaqueValue *dest,
                                                      OpaqueValue *src,
                                        const ExistentialTypeMetadata *srcType,
                                        const Metadata *targetType,
                                        DynamicCastFlags flags) {
  if (srcType->isClassBounded()) {
    auto classContainer =
      reinterpret_cast<ClassExistentialContainer*>(src);
    void *obj = classContainer->Value;
    return _dynamicCastUnknownClass(dest, obj, targetType, flags);
  } else {
    auto opaqueContainer =
      reinterpret_cast<OpaqueExistentialContainer*>(src);
    auto srcCapturedType = opaqueContainer->Type;
    return swift_dynamicCast(dest,
                  srcCapturedType->vw_projectBuffer(&opaqueContainer->Buffer),
                             srcCapturedType,
                             targetType,
                             flags);
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

  if (srcType->isClassBounded()) {
    auto classContainer =
      reinterpret_cast<const ClassExistentialContainer*>(src);
    srcValue = (OpaqueValue*) &classContainer->Value;
    void *obj = classContainer->Value;
    srcCapturedType = swift_unknownTypeOf(reinterpret_cast<HeapObject*>(obj));
  } else {
    auto opaqueContainer = reinterpret_cast<OpaqueExistentialContainer*>(src);
    srcCapturedType = opaqueContainer->Type;
    srcValue = srcCapturedType->vw_projectBuffer(&opaqueContainer->Buffer);
  }

  return swift_dynamicCast(dest, srcValue, srcCapturedType,
                           targetType, flags);
}

/// Perform a dynamic cast of a metatype to a metatype.
static bool _dynamicCastMetatypeToMetatype(OpaqueValue *dest,
                                           const Metadata *metatype,
                                           const Metadata *targetType,
                                           DynamicCastFlags flags) {
  const Metadata *result;
  if (flags & DynamicCastFlags::Unconditional) {
    result = swift_dynamicCastMetatypeUnconditional(metatype, targetType);
  } else {
    result = swift_dynamicCastMetatype(metatype, targetType);
    if (!result) return false;
  }

  *((const Metadata **) dest) = result;
  return true;
}

/// Check whether an unknown class instance is actually a class object.
static const Metadata *_getUnknownClassAsMetatype(void *object) {
  // Class values are currently never metatypes (?).
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
    _dynamicCastFailure(swift_getObjectType(object), targetType);
  if (flags & DynamicCastFlags::DestroyOnFailure)
    swift_release((HeapObject*) object);
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
    if (srcExistentialType->isClassBounded()) {
      auto srcExistential = (ClassExistentialContainer*) src;
      return _dynamicCastUnknownClassToMetatype(dest,
                                                srcExistential->Value,
                                                targetType, flags);
    } else {
      auto srcExistential = (OpaqueExistentialContainer*) src;
      auto srcValueType = srcExistential->Type;
      auto srcValue = srcValueType->vw_projectBuffer(&srcExistential->Buffer);
      return _dynamicCastToMetatype(dest, srcValue, srcValueType,
                                    targetType, flags);
    }
  }

  case MetadataKind::Class:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass: {
    auto object = reinterpret_cast<void**>(src);
    return _dynamicCastUnknownClassToMetatype(dest, object, targetType, flags);
  }

  case MetadataKind::Function:
  case MetadataKind::Block:
  case MetadataKind::HeapArray:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
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

  // If it's an existential, we need to check for conformances.
  auto targetInstanceType = targetType->InstanceType;
  if (auto targetInstanceTypeAsExistential =
        dyn_cast<ExistentialTypeMetadata>(targetInstanceType)) {
    // Check for conformance to all the protocols.
    // TODO: collect the witness tables.
    auto &protocols = targetInstanceTypeAsExistential->Protocols;
    for (unsigned i = 0, n = protocols.NumProtocols; i != n; ++i) {
      const ProtocolDescriptor *protocol = protocols[i];
      if (!_conformsToProtocol(nullptr, srcMetatype, protocol, nullptr)) {
        if (flags & DynamicCastFlags::Unconditional)
          _dynamicCastFailure(srcMetatype, targetType);
        return false;
      }
    }

    if (writeDestMetatype)
      *((const Metadata **) dest) = srcMetatype;
    return true;
  }

  // Otherwise, we're casting to SomeProtocol.Type.Type.
  auto targetInstanceTypeAsMetatype =
    cast<ExistentialMetatypeMetadata>(targetInstanceType);

  // If the source type isn't a metatype, the cast fails.
  auto srcMetatypeMetatype = dyn_cast<MetatypeMetadata>(srcMetatype);
  if (!srcMetatypeMetatype) {
    if (flags & DynamicCastFlags::Unconditional)
      _dynamicCastFailure(srcMetatype, targetType);
    return false;
  }

  // The representation of an existential metatype remains consistent
  // arbitrarily deep: a metatype, followed by some protocols.  The
  // protocols are the same at every level, so we can just set the
  // metatype correctly and then recurse, letting the recursive call
  // fill in the conformance information correctly.

  // Proactively set the destination metatype so that we can tail-recurse,
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
    _dynamicCastFailure(swift_getObjectType(object), targetType);
  if (flags & DynamicCastFlags::DestroyOnFailure)
    swift_release((HeapObject*) object);
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
    if (srcExistentialType->isClassBounded()) {
      auto srcExistential = (ClassExistentialContainer*) src;
      return _dynamicCastUnknownClassToExistentialMetatype(dest,
                                                srcExistential->Value,
                                                targetType, flags);
    } else {
      auto srcExistential = (OpaqueExistentialContainer*) src;
      auto srcValueType = srcExistential->Type;
      auto srcValue = srcValueType->vw_projectBuffer(&srcExistential->Buffer);
      return _dynamicCastToExistentialMetatype(dest, srcValue, srcValueType,
                                               targetType, flags);
    }
  }

  case MetadataKind::Class:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass:
  case MetadataKind::Function:
  case MetadataKind::Block:
  case MetadataKind::HeapArray:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    if (flags & DynamicCastFlags::Unconditional) {
      _dynamicCastFailure(srcType, targetType);
    }
    return false;
  }
  _failCorruptType(srcType);
}

/// Perform a dynamic cast to an arbitrary type.
bool swift::swift_dynamicCast(OpaqueValue *dest,
                              OpaqueValue *src,
                              const Metadata *srcType,
                              const Metadata *targetType,
                              DynamicCastFlags flags) {
  switch (targetType->getKind()) {

  // Casts to class type.
  case MetadataKind::Class:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass:
    switch (srcType->getKind()) {
    case MetadataKind::Class:
    case MetadataKind::ObjCClassWrapper:
    case MetadataKind::ForeignClass: {
      // Do a dynamic cast on the instance pointer.
      void *object = *reinterpret_cast<void * const *>(src);
      return _dynamicCastUnknownClass(dest, object,
                                      targetType, flags);
    }

    case MetadataKind::Existential: {
      auto srcExistentialType = cast<ExistentialTypeMetadata>(srcType);
      return _dynamicCastToUnknownClassFromExistential(dest, src,
                                                       srcExistentialType,
                                                       targetType, flags);
    }

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

  // The non-polymorphic types.
  case MetadataKind::Function:
  case MetadataKind::Block:
  case MetadataKind::HeapArray:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    // If there's an exact type match, we're done.
    if (srcType == targetType) {
      if (flags & DynamicCastFlags::TakeOnSuccess) {
        srcType->vw_initializeWithTake(dest, src);
      } else {
        srcType->vw_initializeWithCopy(dest, src);
      }
      return true;
    }

    // If we have an existential, look at its dynamic type.
    if (auto srcExistentialType = dyn_cast<ExistentialTypeMetadata>(srcType)) {
      return _dynamicCastFromExistential(dest, src, srcExistentialType,
                                         targetType, flags);
    }

    // Otherwise, we have a failure.
    return _fail(src, srcType, targetType, flags);
  }
  _failCorruptType(srcType);
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
