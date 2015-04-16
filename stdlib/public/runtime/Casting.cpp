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
#include "swift/Basic/Demangle.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/Enum.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "llvm/ADT/DenseMap.h"
#include "Debug.h"
#include "ErrorObject.h"
#include "ExistentialMetadataImpl.h"
#include "Private.h"
#include "../SwiftShims/RuntimeShims.h"
#include "stddef.h"

#if defined(__APPLE__) && defined(__MACH__)
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>
#elif defined(__ELF__)
#include <elf.h>
#include <link.h>
#endif

#include <dlfcn.h>
#include <cstring>
#include <deque>
#include <mutex>
#include <atomic>
#include <sstream>
#include <type_traits>

// FIXME: Clang defines max_align_t in stddef.h since 3.6.
// Remove this hack when we don't care about older Clangs on all platforms.
#ifdef __APPLE__
typedef std::max_align_t swift_max_align_t;
#else
typedef long double swift_max_align_t;
#endif

using namespace swift;
using namespace metadataimpl;

#if SWIFT_OBJC_INTEROP
#include <objc/objc-runtime.h>

// Aliases for Objective-C runtime entry points.
const char *class_getName(const ClassMetadata* type) {
  return class_getName(
    reinterpret_cast<Class>(const_cast<ClassMetadata*>(type)));
}

// Aliases for Swift runtime entry points for Objective-C types.
extern "C" const void *swift_dynamicCastObjCProtocolConditional(
                         const void *object,
                         size_t numProtocols,
                         const ProtocolDescriptor * const *protocols);
#endif

namespace {
  enum class TypeSyntaxLevel {
    /// Any type syntax is valid.
    Type,
    /// Function types must be parenthesized.
    TypeSimple,
  };
}

static void _buildNameForMetadata(const Metadata *type,
                                  TypeSyntaxLevel level,
                                  std::string &result);

static void _buildNominalTypeName(const NominalTypeDescriptor *ntd,
                                  const Metadata *type,
                                  std::string &result) {
  // Demangle the basic type name.
  result += Demangle::demangleTypeAsString(ntd->Name, strlen(ntd->Name));
  
  // If generic, demangle the type parameters.
  if (ntd->GenericParams.NumPrimaryParams > 0) {
    result += "<";
    
    auto typeBytes = reinterpret_cast<const char *>(type);
    auto genericParam = reinterpret_cast<const Metadata * const *>(
                         typeBytes + sizeof(void*) * ntd->GenericParams.Offset);
    for (unsigned i = 0, e = ntd->GenericParams.NumPrimaryParams;
         i < e; ++i, ++genericParam) {
      if (i > 0)
        result += ", ";
      _buildNameForMetadata(*genericParam, TypeSyntaxLevel::Type, result);
    }
    
    result += ">";
  }
}

static const char *_getProtocolName(const ProtocolDescriptor *protocol) {
  const char *name = protocol->Name;
  // Protocol names are emitted with the _Tt prefix so that ObjC can
  // recognize them as mangled Swift names.
  assert(name[0] == '_' && name[1] == 'T' && name[2] == 't');
  return name + 3;
}

static void _buildExistentialTypeName(const ProtocolDescriptorList *protocols,
                                      std::string &result) {
  // If there's only one protocol, the existential type name is the protocol
  // name.
  auto descriptors = protocols->getProtocols();
  
  if (protocols->NumProtocols == 1) {
    auto name = _getProtocolName(descriptors[0]);
    result += Demangle::demangleTypeAsString(name, strlen(name));
    return;
  }
  
  result += "protocol<";
  for (unsigned i = 0, e = protocols->NumProtocols; i < e; ++i) {
    if (i > 0)
      result += ", ";
    auto name = _getProtocolName(descriptors[i]);
    result += Demangle::demangleTypeAsString(name, strlen(name));
  }
  result += ">";
}

static void _buildFunctionTypeName(const FunctionTypeMetadata *func,
                                   std::string &result) {

  if (func->NumArguments == 1) {
    auto firstArgument = func->getArguments()[0].getPointer();
    bool isInout = func->getArguments()[0].getFlag();

    // This could be a single input tuple, with one or more arguments inside,
    // but guaranteed to not have inout types.
    if (auto tupleMetadata = dyn_cast<TupleTypeMetadata>(firstArgument)) {
          _buildNameForMetadata(tupleMetadata,
                                TypeSyntaxLevel::TypeSimple,
                                result);
    } else {
      if (isInout)
        result += "inout ";

      _buildNameForMetadata(firstArgument,
                            TypeSyntaxLevel::TypeSimple,
                            result);
    }
  } else {
      result += "(";
      for (size_t i = 0; i < func->NumArguments; ++i) {
        auto arg = func->getArguments()[i].getPointer();
        bool isInout = func->getArguments()[i].getFlag();
        if (isInout)
          result += "inout ";
        _buildNameForMetadata(arg, TypeSyntaxLevel::TypeSimple, result);
        if (i < func->NumArguments - 1) {
          result += ", ";
        }
      }
      result += ")";
  }
  
  if (func->throws()) {
    result += " throws";
  }

  result += " -> ";
  _buildNameForMetadata(func->ResultType.getPointer(),
                        TypeSyntaxLevel::Type,
                        result);
}

// Build a user-comprehensible name for a type.
static void _buildNameForMetadata(const Metadata *type,
                           TypeSyntaxLevel level,
                           std::string &result) {
  switch (type->getKind()) {
  case MetadataKind::Class: {
    auto classType = static_cast<const ClassMetadata *>(type);
#if SWIFT_OBJC_INTEROP
    // Ask the Objective-C runtime to name ObjC classes.
    if (!classType->isTypeMetadata()) {
      result += class_getName(classType);
      return;
    }
#endif
    return _buildNominalTypeName(classType->getDescription(),
                                    classType,
                                    result);
  }
  case MetadataKind::Enum:
  case MetadataKind::Struct: {
    auto structType = static_cast<const StructMetadata *>(type);
    return _buildNominalTypeName(structType->Description,
                                 type, result);
  }
  case MetadataKind::ObjCClassWrapper: {
#if SWIFT_OBJC_INTEROP
    auto objcWrapper = static_cast<const ObjCClassWrapperMetadata *>(type);
    result += class_getName(objcWrapper->Class);
#else
    assert(false && "no ObjC interop");
#endif
    return;
  }
  case MetadataKind::ForeignClass: {
    auto foreign = static_cast<const ForeignClassMetadata *>(type);
    const char *name = foreign->getName();
    size_t len = strlen(name);
    result += Demangle::demangleTypeAsString(name, len);
    return;
  }
  case MetadataKind::Existential: {
    auto exis = static_cast<const ExistentialTypeMetadata *>(type);
    _buildExistentialTypeName(&exis->Protocols, result);
    return;
  }
  case MetadataKind::ExistentialMetatype: {
    auto metatype = static_cast<const ExistentialMetatypeMetadata *>(type);
    _buildNameForMetadata(metatype->InstanceType, TypeSyntaxLevel::TypeSimple,
                          result);
    result += ".Type";
    return;
  }
  case MetadataKind::Block: {
    if (level >= TypeSyntaxLevel::TypeSimple)
      result += "(";

    result += "@convention(block) ";

    auto func = static_cast<const FunctionTypeMetadata *>(type);
    _buildFunctionTypeName(func, result);

    if (level >= TypeSyntaxLevel::TypeSimple)
      result += ")";
    return;
  }
  case MetadataKind::ThinFunction: {
    if (level >= TypeSyntaxLevel::TypeSimple)
      result += "(";

    result += "@convention(thin) ";

    auto func = static_cast<const FunctionTypeMetadata *>(type);
    _buildFunctionTypeName(func, result);

    if (level >= TypeSyntaxLevel::TypeSimple)
      result += ")";
    return;
  }
  case MetadataKind::Function: {
    if (level >= TypeSyntaxLevel::TypeSimple)
      result += "(";

    auto func = static_cast<const FunctionTypeMetadata *>(type);
    _buildFunctionTypeName(func, result);

    if (level >= TypeSyntaxLevel::TypeSimple)
      result += ")";
    return;
  }
  case MetadataKind::Metatype: {
    auto metatype = static_cast<const MetatypeMetadata *>(type);
    _buildNameForMetadata(metatype->InstanceType, TypeSyntaxLevel::TypeSimple,
                          result);
    if (metatype->InstanceType->isAnyExistentialType())
      result += ".Protocol";
    else
      result += ".Type";
    return;
  }
  case MetadataKind::Tuple: {
    auto tuple = static_cast<const TupleTypeMetadata *>(type);
    result += "(";
    auto elts = tuple->getElements();
    for (unsigned i = 0, e = tuple->NumElements; i < e; ++i) {
      if (i > 0)
        result += ", ";
      _buildNameForMetadata(elts[i].Type, TypeSyntaxLevel::Type,
                            result);
    }
    result += ")";
    return;
  }
  case MetadataKind::Opaque: {
    // TODO
    result += "<<<opaque type>>>";
    return;
  }
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::PolyFunction:
    break;
  }
  result += "<<<invalid type>>>";
}

// Return a user-comprehensible name for the given type.
std::string swift::nameForMetadata(const Metadata *type) {
  std::string result;
  _buildNameForMetadata(type, TypeSyntaxLevel::Type, result);
  return result;
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

  swift::fatalError("Could not cast value of type '%s' (%p) to '%s' (%p)%s%s\n",
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


/// Report a corrupted type object.
LLVM_ATTRIBUTE_NORETURN
LLVM_ATTRIBUTE_ALWAYS_INLINE // Minimize trashed registers
static void _failCorruptType(const Metadata *type) {
  swift::crash("Corrupt Swift type object");
}

#if SWIFT_OBJC_INTEROP
// Objective-c bridging helpers.
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
                  const Metadata *targetType, DynamicCastFlags flags) {
  if (flags & DynamicCastFlags::Unconditional)
    swift_dynamicCastFailure(srcType, targetType);
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

/// Dynamically cast a class object to a Swift class type.
const void *
swift::swift_dynamicCastClass(const void *object,
                              const ClassMetadata *targetType) {
#if SWIFT_OBJC_INTEROP
  assert(!targetType->isPureObjC());

  // Swift native classes never have a tagged-pointer representation.
  if (isObjCTaggedPointerOrNull(object)) {
    return nullptr;
  }
#endif

  auto isa = _swift_getClassOfAllocated(object);

  do {
    if (isa == targetType) {
      return object;
    }
    isa = _swift_getSuperclass(isa);
  } while (isa);

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
    case MetadataKind::ThinFunction:
    case MetadataKind::Block: // FIXME
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Enum:
    case MetadataKind::Opaque:
    case MetadataKind::PolyFunction:
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
      return _swift_classConformsToObjCProtocol(type, protocol);
    }
#endif
    return false;

  case MetadataKind::ObjCClassWrapper: {
#if SWIFT_OBJC_INTEROP
    if (value) {
      return _unknownClassConformsToObjCProtocol(value, protocol);
    } else {
      auto wrapper = cast<ObjCClassWrapperMetadata>(type);
      return _swift_classConformsToObjCProtocol(wrapper->Class, protocol);
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
  case MetadataKind::ThinFunction:
  case MetadataKind::Block: // FIXME
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::ErrorObject:
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

/// Given a possibly-existential value, find its dynamic type and the
/// address of its storage.
static void findDynamicValueAndType(OpaqueValue *value, const Metadata *type,
                                    OpaqueValue *&outValue,
                                    const Metadata *&outType,
                                    bool &inoutCanTake) {
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
    case ExistentialTypeRepresentation::ErrorType: {
      OpaqueValue *innerValue
        = existentialType->projectValue(value);
      const Metadata *innerType = existentialType->getDynamicType(value);
      inoutCanTake &= existentialType->mayTakeValue(value);
      return findDynamicValueAndType(innerValue, innerType,
                                     outValue, outType, inoutCanTake);
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
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::ErrorObject:
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

extern "C" const Metadata *
swift::swift_getDynamicType(OpaqueValue *value, const Metadata *self) {
  OpaqueValue *outValue;
  const Metadata *outType;
  bool canTake = false;
  findDynamicValueAndType(value, self, outValue, outType, canTake);
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
      
    case ExistentialTypeRepresentation::ErrorType:
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
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    return;
  }
  _failCorruptType(type);
}

#if SWIFT_OBJC_INTEROP
extern "C" id
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
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::PolyFunction:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::ForeignClass:
  case MetadataKind::Block:
  case MetadataKind::ThinFunction:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::ErrorObject:
    return nullptr;
  }
}

extern "C" id
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
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::PolyFunction:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::ForeignClass:
  case MetadataKind::Block:
  case MetadataKind::ThinFunction:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::ErrorObject: {
    std::string sourceName = nameForMetadata(metatype);
    swift_dynamicCastFailure(metatype, sourceName.c_str(),
                             nullptr, "AnyObject",
                         "only class metatypes can be converted to AnyObject");
  }
  }
}
#endif

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
  bool canTake = true;
  findDynamicValueAndType(src, srcType, srcDynamicValue, srcDynamicType,
                          canTake);

  auto maybeDeallocateSourceAfterSuccess = [&] {
    if (shouldDeallocateSource(/*succeeded*/ true, flags)) {
      // If we're able to take the dynamic value, then clean up any leftover
      // buffers it may have been contained in.
      if (canTake && src != srcDynamicValue)
        deallocateDynamicValue(src, srcType);
      // Otherwise, deallocate the original value wholesale if we couldn't take
      // it.
      else if (!canTake)
        srcType->vw_destroy(src);
    }
  };

  // The representation of an existential is different for some protocols.
  switch (targetType->getRepresentation()) {
  case ExistentialTypeRepresentation::Class: {
    auto destExistential =
      reinterpret_cast<ClassExistentialContainer*>(dest);

    // If the source type is a value type, it cannot possibly conform
    // to a class-bounded protocol. 
    switch (srcDynamicType->getKind()) {
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
      return _fail(src, srcType, targetType, flags);
    }
    
    case MetadataKind::Class:
    case MetadataKind::ObjCClassWrapper:
    case MetadataKind::ForeignClass:
    case MetadataKind::Existential:
      // Handle these cases below.
      break;

    case MetadataKind::Struct:
    case MetadataKind::Enum:
#if SWIFT_OBJC_INTEROP
      // If the source type is bridged to Objective-C, try to bridge.
      if (auto srcBridgeWitness = findBridgeWitness(srcDynamicType)) {
        DynamicCastFlags subFlags 
          = flags - (DynamicCastFlags::TakeOnSuccess |
                     DynamicCastFlags::DestroyOnFailure);
        bool success = _dynamicCastValueToClassExistentialViaObjCBridgeable(
                         dest,
                         srcDynamicValue,
                         srcDynamicType,
                         targetType,
                         srcBridgeWitness,
                         subFlags);

        // Destroy the source value, since we avoided taking or destroying
        // it above.
        if (shouldDeallocateSource(success, flags)) {
          srcType->vw_destroy(src);
        }

        return success;
      }
#endif
      break;

    case MetadataKind::Function:
    case MetadataKind::ThinFunction:
    case MetadataKind::Block:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Opaque:
    case MetadataKind::PolyFunction:
    case MetadataKind::Tuple:
      // Will never succeed.
      return _fail(src, srcType, targetType, flags);
    }

    // Check for protocol conformances and fill in the witness tables.
    if (!_conformsToProtocols(srcDynamicValue, srcDynamicType,
                              targetType->Protocols,
                              destExistential->getWitnessTables())) {
      return _fail(src, srcType, targetType, flags);
    }

    auto object = *(reinterpret_cast<HeapObject**>(srcDynamicValue));
    destExistential->Value = object;
    if (!canTake || !(flags & DynamicCastFlags::TakeOnSuccess)) {
      swift_retain_noresult(object);
    }
    maybeDeallocateSourceAfterSuccess();
    return true;
  }
  case ExistentialTypeRepresentation::Opaque: {
    auto destExistential =
      reinterpret_cast<OpaqueExistentialContainer*>(dest);

    // Check for protocol conformances and fill in the witness tables.
    if (!_conformsToProtocols(srcDynamicValue, srcDynamicType,
                              targetType->Protocols,
                              destExistential->getWitnessTables()))
      return _fail(src, srcType, targetType, flags);

    // Fill in the type and value.
    destExistential->Type = srcDynamicType;
    if (canTake && (flags & DynamicCastFlags::TakeOnSuccess)) {
      srcDynamicType->vw_initializeBufferWithTake(&destExistential->Buffer,
                                                  srcDynamicValue);
    } else {
      srcDynamicType->vw_initializeBufferWithCopy(&destExistential->Buffer,
                                                  srcDynamicValue);
    }
    maybeDeallocateSourceAfterSuccess();
    return true;
  }
  case ExistentialTypeRepresentation::ErrorType: {
    auto destBoxAddr =
      reinterpret_cast<SwiftError**>(dest);
    // Check for the ErrorType protocol conformance, which should be the only
    // one we need.
    assert(targetType->Protocols.NumProtocols == 1);
    const WitnessTable *errorWitness;
    if (!_conformsToProtocols(srcDynamicValue, srcDynamicType,
                              targetType->Protocols,
                              &errorWitness))
      return _fail(src, srcType, targetType, flags);
    
    BoxPair destBox = swift_allocError(srcDynamicType, errorWitness);
    *destBoxAddr = reinterpret_cast<SwiftError*>(destBox.heapObject);
    if (canTake && (flags & DynamicCastFlags::TakeOnSuccess)) {
      srcDynamicType->vw_initializeWithTake(destBox.value, srcDynamicValue);
    } else {
      srcDynamicType->vw_initializeWithCopy(destBox.value, srcDynamicValue);
    }
    maybeDeallocateSourceAfterSuccess();
    return true;
  }
  }
}

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
      if (!_swift_objectConformsToObjCProtocol(object, protocol))
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
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
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
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    swift_dynamicCastFailure(_swift_getClass(object), targetType);
  }
  _failCorruptType(targetType);
}

#if SWIFT_OBJC_INTEROP
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
    case MetadataKind::ThinFunction:
    case MetadataKind::Block:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Metatype:
    case MetadataKind::Enum:
    case MetadataKind::Opaque:
    case MetadataKind::PolyFunction:
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
      SWIFT_FALLTHROUGH;
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
    case MetadataKind::ThinFunction:
    case MetadataKind::Block:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::ErrorObject:
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
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::ErrorObject:
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
    case MetadataKind::ThinFunction:
    case MetadataKind::Block:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Metatype:
    case MetadataKind::Enum:
    case MetadataKind::Opaque:
    case MetadataKind::PolyFunction:
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
      SWIFT_FALLTHROUGH;
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
    case MetadataKind::ThinFunction:
    case MetadataKind::Block:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Metatype:
    case MetadataKind::Enum:
    case MetadataKind::Opaque:
    case MetadataKind::PolyFunction:
    case MetadataKind::Struct:
    case MetadataKind::Tuple:
      swift_dynamicCastFailure(sourceType, targetType);
    }
    break;
  case MetadataKind::Existential:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Metatype:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    // The cast succeeds only if the metadata pointers are statically
    // equivalent.
    if (sourceType != targetType)
      swift_dynamicCastFailure(sourceType, targetType);
    return origSourceType;
  }
}
#endif

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
    return _dynamicCastUnknownClass(dest, obj, targetType, flags);
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
  case ExistentialTypeRepresentation::ErrorType: {
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
  case ExistentialTypeRepresentation::ErrorType: {
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
  
  auto subFlags = flags;
  if (!canTake)
    subFlags = subFlags - (DynamicCastFlags::DestroyOnFailure
                           | DynamicCastFlags::TakeOnSuccess);

  bool result = swift_dynamicCast(dest, srcValue, srcCapturedType,
                                  targetType, subFlags);
  // Deallocate the existential husk if we took from it.
  if (canTake && result && isOutOfLine)
    _maybeDeallocateOpaqueExistential(src, result, flags);
  // If we couldn't take, we still may need to destroy the whole value.
  else if (!canTake && shouldDeallocateSource(result, flags))
    srcType->vw_destroy(src);

  return result;
}

#if SWIFT_OBJC_INTEROP
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
    swift_dynamicCastFailure(_swift_getClass(object), targetType);
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
    case ExistentialTypeRepresentation::ErrorType: {
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
    auto object = reinterpret_cast<void**>(src);
    return _dynamicCastUnknownClassToMetatype(dest, object, targetType, flags);
  }

  case MetadataKind::Function:
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::ErrorObject:
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
    swift_dynamicCastFailure(_swift_getClass(object), targetType);
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
    case ExistentialTypeRepresentation::ErrorType: {
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
  case MetadataKind::ForeignClass:
  case MetadataKind::Function:
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    if (flags & DynamicCastFlags::Unconditional) {
      swift_dynamicCastFailure(srcType, targetType);
    }
    return false;
  }
  _failCorruptType(srcType);
}

extern "C" const ProtocolDescriptor _TMpSs10_ErrorType;

static const WitnessTable *findErrorTypeWitness(const Metadata *srcType) {
  return swift_conformsToProtocol(srcType, &_TMpSs10_ErrorType);
}

static const Metadata *getNSErrorTypeMetadata() {
  static auto TheNSErrorMetadata
    = swift_getObjCClassMetadata((const ClassMetadata *)getNSErrorClass());
  return TheNSErrorMetadata;
}

static id dynamicCastValueToNSError(OpaqueValue *src,
                                    const Metadata *srcType,
                                    const WitnessTable *srcErrorTypeWitness,
                                    DynamicCastFlags flags) {
  BoxPair errorBox = swift_allocError(srcType, srcErrorTypeWitness);
  
  if (flags & DynamicCastFlags::TakeOnSuccess)
    srcType->vw_initializeWithTake(errorBox.value, src);
  else
    srcType->vw_initializeWithCopy(errorBox.value, src);
  
  return swift_bridgeErrorTypeToNSError((SwiftError*)errorBox.heapObject);
}
#endif

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

    case MetadataKind::Enum:
    case MetadataKind::Struct: {
#if SWIFT_OBJC_INTEROP
      // If the source type is bridged to Objective-C, try to bridge.
      if (auto srcBridgeWitness = findBridgeWitness(srcType)) {
        return _dynamicCastValueToClassViaObjCBridgeable(dest, src, srcType,
                                                         targetType,
                                                         srcBridgeWitness,
                                                         flags);
      }
      // If the destination type is an NSError, and the source type is an
      // ErrorType, then the cast can succeed by NSError bridging.
      if (targetType == getNSErrorTypeMetadata()) {
        if (auto srcErrorTypeWitness = findErrorTypeWitness(srcType)) {
          auto error = dynamicCastValueToNSError(src, srcType,
                                                 srcErrorTypeWitness, flags);
          *reinterpret_cast<id *>(dest) = error;
          return true;
        }
      }
#endif
      return _fail(src, srcType, targetType, flags);
    }

    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Function:
    case MetadataKind::ThinFunction:
    case MetadataKind::Block:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Metatype:
    case MetadataKind::Opaque:
    case MetadataKind::PolyFunction:
    case MetadataKind::Tuple:
      return _fail(src, srcType, targetType, flags);
    }
    break;

  case MetadataKind::Existential:
    return _dynamicCastToExistential(dest, src, srcType,
                                     cast<ExistentialTypeMetadata>(targetType),
                                     flags);

  case MetadataKind::Metatype:
#if SWIFT_OBJC_INTEROP
    return _dynamicCastToMetatype(dest, src, srcType,
                                  cast<MetatypeMetadata>(targetType),
                                  flags);
#else
    return _fail(src, srcType, targetType, flags);
#endif
    
  case MetadataKind::ExistentialMetatype:
#if SWIFT_OBJC_INTEROP
    return _dynamicCastToExistentialMetatype(dest, src, srcType,
                                 cast<ExistentialMetatypeMetadata>(targetType),
                                             flags);
#else
    return _fail(src, srcType, targetType, flags);
#endif

  case MetadataKind::Struct:
  case MetadataKind::Enum:
    switch (srcType->getKind()) {
    case MetadataKind::Class:
    case MetadataKind::ObjCClassWrapper:
    case MetadataKind::ForeignClass: {
#if SWIFT_OBJC_INTEROP
      // If the target type is bridged to Objective-C, try to bridge.
      if (auto targetBridgeWitness = findBridgeWitness(targetType)) {
        return _dynamicCastClassToValueViaObjCBridgeable(dest, src, srcType,
                                                         targetType,
                                                         targetBridgeWitness,
                                                         flags);
      }
      
      // If the source is an NSError, and the target is a bridgeable ErrorType,
      // try to bridge.
      if (tryDynamicCastNSErrorToValue(dest, src, srcType, targetType, flags)) {
        return true;
      }
#endif
      break;
    }

    case MetadataKind::Enum:
    case MetadataKind::Existential:
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Function:
    case MetadataKind::ThinFunction:
    case MetadataKind::Block:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::ErrorObject:
    case MetadataKind::Metatype:
    case MetadataKind::Opaque:
    case MetadataKind::PolyFunction:
    case MetadataKind::Struct:
    case MetadataKind::Tuple:
      break;
    }

    SWIFT_FALLTHROUGH;

  // The non-polymorphic types.
  case MetadataKind::Function:
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
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

#if defined(NDEBUG) && SWIFT_OBJC_INTEROP
void ProtocolConformanceRecord::dump() const {
  auto symbolName = [&](const void *addr) -> const char * {
    Dl_info info;
    int ok = dladdr(addr, &info);
    if (!ok)
      return "<unknown addr>";
    return info.dli_sname;
  };

  switch (auto kind = getTypeKind()) {
    case ProtocolConformanceTypeKind::Universal:
      printf("universal");
      break;
    case ProtocolConformanceTypeKind::UniqueDirectType:
    case ProtocolConformanceTypeKind::NonuniqueDirectType:
      printf("%s direct type ",
             kind == ProtocolConformanceTypeKind::UniqueDirectType
             ? "unique" : "nonunique");
      if (auto ntd = getDirectType()->getNominalTypeDescriptor()) {
        printf("%s", ntd->Name);
      } else {
        printf("<structural type>");
      }
      break;
    case ProtocolConformanceTypeKind::UniqueDirectClass:
      printf("unique direct class %s",
             class_getName(getDirectClass()));
      break;
    case ProtocolConformanceTypeKind::UniqueIndirectClass:
      printf("unique indirect class %s",
             class_getName(*getIndirectClass()));
      break;
      
    case ProtocolConformanceTypeKind::UniqueGenericPattern:
      printf("unique generic type %s", symbolName(getGenericPattern()));
      break;
  }
  
  printf(" => ");
  
  switch (getConformanceKind()) {
    case ProtocolConformanceReferenceKind::WitnessTable:
      printf("witness table %s\n", symbolName(getStaticWitnessTable()));
      break;
    case ProtocolConformanceReferenceKind::WitnessTableAccessor:
      printf("witness table accessor %s\n",
             symbolName((const void *)(uintptr_t)getWitnessTableAccessor()));
      break;
  }
}
#endif

/// Take the type reference inside a protocol conformance record and fetch the
/// canonical metadata pointer for the type it refers to.
/// Returns nil for universal or generic type references.
const Metadata *ProtocolConformanceRecord::getCanonicalTypeMetadata()
const {
  switch (getTypeKind()) {
  case ProtocolConformanceTypeKind::UniqueDirectType:
    // Already unique.
    return getDirectType();
  case ProtocolConformanceTypeKind::NonuniqueDirectType:
    // Ask the runtime for the unique metadata record we've canonized.
    return swift_getForeignTypeMetadata((ForeignTypeMetadata*)getDirectType());
  case ProtocolConformanceTypeKind::UniqueIndirectClass:
    // The class may be ObjC, in which case we need to instantiate its Swift
    // metadata.
    return swift_getObjCClassMetadata(*getIndirectClass());
      
  case ProtocolConformanceTypeKind::UniqueDirectClass:
    // The class may be ObjC, in which case we need to instantiate its Swift
    // metadata.
    return swift_getObjCClassMetadata(getDirectClass());
      
  case ProtocolConformanceTypeKind::UniqueGenericPattern:
  case ProtocolConformanceTypeKind::Universal:
    // The record does not apply to a single type.
    return nullptr;
  }
}

const WitnessTable *ProtocolConformanceRecord::getWitnessTable(const Metadata *type)
const {
  switch (getConformanceKind()) {
  case ProtocolConformanceReferenceKind::WitnessTable:
    return getStaticWitnessTable();

  case ProtocolConformanceReferenceKind::WitnessTableAccessor:
    return getWitnessTableAccessor()(type);
  }
}

#if defined(__APPLE__) && defined(__MACH__)
#define SWIFT_PROTOCOL_CONFORMANCES_SECTION "__swift1_proto"
#elif defined(__ELF__)
#define SWIFT_PROTOCOL_CONFORMANCES_SECTION ".swift1_protocol_conformances_start"
#endif

// FIXME: Implement this callback on non-apple platforms.

// std:once_flag token to install the dyld callback to enqueue images for
// protocol conformance lookup.
static std::once_flag InstallProtocolConformanceAddImageCallbackOnce;

// Monotonic generation number that is increased when we load an image with
// new protocol conformances.
//
// Although this is atomically readable, writes or cached stores of the value
// must be guarded by the SectionsToScanLock in order to ensure the generation
// number agrees with the state of the queue at the time of caching.
static std::atomic<unsigned> ProtocolConformanceGeneration
  = ATOMIC_VAR_INIT(0);

namespace {
  struct ConformanceSection {
    const ProtocolConformanceRecord *Begin, *End;
    const ProtocolConformanceRecord *begin() const {
      return Begin;
    }
    const ProtocolConformanceRecord *end() const {
      return End;
    }
  };

  struct ConformanceCacheEntry {
  private:
    const void *Type; 
    const ProtocolDescriptor *Proto;
    uintptr_t Data;
    // All Darwin 64-bit platforms reserve the low 2^32 of address space, which
    // is more than enough invalid pointer values for any realistic generation
    // number. It's a little easier to overflow on 32-bit, so we need an extra
    // bit there.
#if !__LP64__
    bool Success;
#endif

    ConformanceCacheEntry(const void *type,
                          const ProtocolDescriptor *proto,
                          uintptr_t Data, bool Success)
      : Type(type), Proto(proto), Data(Data)
#if !__LP64__
        , Success(Success)
#endif
    {
#if __LP64__
#  if __APPLE__
      assert((!Success && Data <= 0xFFFFFFFFU) ||
             (Success && Data > 0xFFFFFFFFU));
#  elif __linux__
      assert((!Success && Data <= 0x0FFFU) ||
             (Success && Data > 0x0FFFU));
#  else
#    error "port me"
#  endif
#endif
  }

  public:
    ConformanceCacheEntry() = default;

    static ConformanceCacheEntry createSuccess(
        const void *type, const ProtocolDescriptor *proto,
        const swift::WitnessTable *witness) {
      return ConformanceCacheEntry(type, proto, (uintptr_t) witness, true);
    }

    static ConformanceCacheEntry createFailure(
        const void *type, const ProtocolDescriptor *proto,
        unsigned failureGeneration) {
      return ConformanceCacheEntry(type, proto, (uintptr_t) failureGeneration,
          false);
    }

    /// \returns true if the entry represents an entry for the pair \p type
    /// and \p proto.
    bool matches(const void *type, const ProtocolDescriptor *proto) {
      return type == Type && Proto == proto;
    }
   
    bool isSuccessful() const {
#if __LP64__
#  if __APPLE__
      return Data > 0xFFFFFFFFU;
#  elif __linux__
      return Data > 0x0FFFU;
#  else
#    error "port me"
#  endif
#else
      return Success;
#endif
    }
    
    /// Get the cached witness table, if successful.
    const WitnessTable *getWitnessTable() const {
      assert(isSuccessful());
      return (const WitnessTable *)Data;
    }
    
    /// Get the generation number under which this lookup failed.
    unsigned getFailureGeneration() const {
      assert(!isSuccessful());
      return Data;
    }
  };
}

// Conformance Cache.
static ConcurrentMap<size_t, ConformanceCacheEntry> ConformanceCache;
// This variable is used to signal when a cache was generated and
// it is correct to avoid a new scan.
unsigned ConformanceCacheGeneration = 0;

// Conformance sections pending a scan.
// TODO: This could easily be a lock-free FIFO.
static pthread_mutex_t SectionsToScanLock = PTHREAD_MUTEX_INITIALIZER;
static std::deque<ConformanceSection> SectionsToScan;

void
swift::swift_registerProtocolConformances(const ProtocolConformanceRecord *begin,
                                          const ProtocolConformanceRecord *end){
  pthread_mutex_lock(&SectionsToScanLock);
  // Increase the generation to invalidate cached negative lookups.
  ++ProtocolConformanceGeneration;
  
  SectionsToScan.push_back(ConformanceSection{begin, end});
  pthread_mutex_unlock(&SectionsToScanLock);
}

static void _addImageProtocolConformancesBlock(const uint8_t *conformances,
                                               size_t conformancesSize) {
  assert(conformancesSize % sizeof(ProtocolConformanceRecord) == 0
         && "weird-sized conformances section?!");

  // If we have a section, enqueue the conformances for lookup.
  auto recordsBegin
    = reinterpret_cast<const ProtocolConformanceRecord*>(conformances);
  auto recordsEnd
    = reinterpret_cast<const ProtocolConformanceRecord*>
                                            (conformances + conformancesSize);
  swift_registerProtocolConformances(recordsBegin, recordsEnd);
}

#if defined(__APPLE__) && defined(__MACH__)
static void _addImageProtocolConformances(const mach_header *mh,
                                          intptr_t vmaddr_slide) {
#ifdef __LP64__
  using mach_header_platform = mach_header_64;
  assert(mh->magic == MH_MAGIC_64 && "loaded non-64-bit image?!");
#else
  using mach_header_platform = mach_header;
#endif
  
  // Look for a __swift1_proto section.
  unsigned long conformancesSize;
  const uint8_t *conformances =
    getsectiondata(reinterpret_cast<const mach_header_platform *>(mh),
                   SEG_DATA, SWIFT_PROTOCOL_CONFORMANCES_SECTION,
                   &conformancesSize);
  
  if (!conformances)
    return;
  
  _addImageProtocolConformancesBlock(conformances, conformancesSize);
}
#elif defined(__ELF__)
static int _addImageProtocolConformances(struct dl_phdr_info *info,
                                          size_t size, void * /*data*/) {
  void *handle;
  if (!info->dlpi_name || info->dlpi_name[0] == '\0') {
    handle = dlopen(nullptr, RTLD_LAZY);
  } else
    handle = dlopen(info->dlpi_name, RTLD_LAZY | RTLD_NOLOAD);
  auto conformances = reinterpret_cast<const uint8_t*>(
      dlsym(handle, SWIFT_PROTOCOL_CONFORMANCES_SECTION));

  if (!conformances) {
    // if there are no conformances, don't hold this handle open.
    dlclose(handle);
    return 0;
  }

  // Extract the size of the conformances block from the head of the section
  auto conformancesSize = *reinterpret_cast<const uint64_t*>(conformances);
  conformances += sizeof(conformancesSize);

  _addImageProtocolConformancesBlock(conformances, conformancesSize);

  dlclose(handle);
  return 0;
}
#endif

static void installCallbacksToInspectDylib() {
  // TODO: Generic types, subclasses, foreign classes
  std::call_once(InstallProtocolConformanceAddImageCallbackOnce, [](){
#if defined(__APPLE__) && defined(__MACH__)
    // Install our dyld callback if we haven't already.
    // Dyld will invoke this on our behalf for all images that have already been
    // loaded.
    _dyld_register_func_for_add_image(_addImageProtocolConformances);
#elif defined(__ELF__)
    // Search the loaded dls. Unlike the above, this only searches the already
    // loaded ones.
    // FIXME: Find a way to have this continue to happen after.
    // rdar://problem/19045112
    dl_iterate_phdr(_addImageProtocolConformances, nullptr);
#else
#error No known mechanism to inspect loaded dynamic libraries on this platform.
#endif
  });
}

static size_t hashTypeProtocolPair(const void *type,
                                   const ProtocolDescriptor *protocol) {
  // A simple hash function for the conformance pair.
  return (size_t)type + ((size_t)protocol >> 2);
}

/// Search the witness table in the ConformanceCache. \returns a pair of the
/// WitnessTable pointer and a boolean value True if a definitive value is
/// found. \returns false if the type or its superclasses were not found in
/// the cache.
static
std::pair<const WitnessTable *, bool>
searchInConformanceCache(const Metadata *type,
                         const ProtocolDescriptor *protocol){
recur_inside_cache_lock:

  // See if we have a cached conformance. Try the specific type first.

  // Hash and lookup the type-protocol pair in the cache.
  size_t hash = hashTypeProtocolPair(type, protocol);
  ConcurrentList<ConformanceCacheEntry> &Bucket =
    ConformanceCache.findOrAllocateNode(hash);

  // Check if the type-protocol entry exists in the cache entry that we found.
  for (auto &Entry : Bucket) {
    if (!Entry.matches(type, protocol)) continue;

    if (Entry.isSuccessful()) {
      return std::make_pair(Entry.getWitnessTable(), true);
    }

    // If we got a cached negative response, check the generation number.
    if (Entry.getFailureGeneration() == ProtocolConformanceGeneration) {
      // We found an entry with a negative value.
      return std::make_pair(nullptr, true);
    }
  }

  // If the type is generic, see if there's a shared nondependent witness table
  // for its instances.
  if (auto generic = type->getGenericPattern()) {
    // Hash and lookup the type-protocol pair in the cache.
    size_t hash = hashTypeProtocolPair(generic, protocol);
    ConcurrentList<ConformanceCacheEntry> &Bucket =
      ConformanceCache.findOrAllocateNode(hash);

    for (auto &Entry : Bucket) {
      if (!Entry.matches(generic, protocol)) continue;
      if (Entry.isSuccessful()) {
        return std::make_pair(Entry.getWitnessTable(), true);
      }
      // We don't try to cache negative responses for generic
      // patterns.
    }
  }

  // If the type is a class, try its superclass.
  if (const ClassMetadata *classType = type->getClassObject()) {
    if (auto super = classType->SuperClass) {
      if (super != getRootSuperclass()) {
        type = swift_getObjCClassMetadata(super);
        goto recur_inside_cache_lock;
      }
    }
  }

  // We did not find an entry.
  return std::make_pair(nullptr, false);
}

const WitnessTable *
swift::swift_conformsToProtocol(const Metadata *type,
                                const ProtocolDescriptor *protocol) {
  // Install callbacks for tracking when a new dylib is loaded so we can 
  // scan it.
  installCallbacksToInspectDylib();
  auto origType = type;

recur:
  // See if we have a cached conformance. The ConcurrentMap data structure
  // allows us to insert and search the map concurrently without locking.
  // We do lock the slow path because the SectionsToScan data structure is not
  // concurrent.
  auto FoundConformance = searchInConformanceCache(type, protocol);
  if (FoundConformance.second) {
    return FoundConformance.first;
  }

  unsigned failedGeneration = ConformanceCacheGeneration;

  // If we didn't have an up-to-date cache entry, scan the conformance records.
  pthread_mutex_lock(&SectionsToScanLock);

  // If we have no new information to pull in (and nobody else pulled in
  // new information while we waited on the lock), we're done.
  if (SectionsToScan.empty()) {
    if (failedGeneration != ConformanceCacheGeneration) {
      // Someone else pulled in new conformances while we were waiting.
      // Start over with our newly-populated cache.
      pthread_mutex_unlock(&SectionsToScanLock);
      type = origType;
      goto recur;
    }


    // Hash and lookup the type-protocol pair in the cache.
    size_t hash = hashTypeProtocolPair(type, protocol);
    ConcurrentList<ConformanceCacheEntry> &Bucket =
      ConformanceCache.findOrAllocateNode(hash);
    Bucket.push_front(ConformanceCacheEntry::createFailure(
        type, protocol, ProtocolConformanceGeneration));
    pthread_mutex_unlock(&SectionsToScanLock);
    return nullptr;
  }

  while (!SectionsToScan.empty()) {
    auto section = SectionsToScan.front();
    SectionsToScan.pop_front();

    // Eagerly pull records for nondependent witnesses into our cache.
    for (const auto &record : section) {
      // If the record applies to a specific type, cache it.
      if (auto metadata = record.getCanonicalTypeMetadata()) {
        auto P = record.getProtocol();

        // Hash and lookup the type-protocol pair in the cache.
        size_t hash = hashTypeProtocolPair(metadata, P);
        ConcurrentList<ConformanceCacheEntry> &Bucket =
          ConformanceCache.findOrAllocateNode(hash);

        auto witness = record.getWitnessTable(metadata);
        if (witness)
          Bucket.push_front(
              ConformanceCacheEntry::createSuccess(metadata, P, witness));
        else
          Bucket.push_front(ConformanceCacheEntry::createFailure(
              metadata, P, ProtocolConformanceGeneration));

      // If the record provides a nondependent witness table for all instances
      // of a generic type, cache it for the generic pattern.
      // TODO: "Nondependent witness table" probably deserves its own flag.
      // An accessor function might still be necessary even if the witness table
      // can be shared.
      } else if (record.getTypeKind()
                   == ProtocolConformanceTypeKind::UniqueGenericPattern
                 && record.getConformanceKind()
                   == ProtocolConformanceReferenceKind::WitnessTable) {

        auto R = record.getGenericPattern();
        auto P = record.getProtocol();

        // Hash and lookup the type-protocol pair in the cache.
        size_t hash = hashTypeProtocolPair(R, P);
        ConcurrentList<ConformanceCacheEntry> &Bucket =
          ConformanceCache.findOrAllocateNode(hash);
          Bucket.push_front(ConformanceCacheEntry::createSuccess(
              R, P, record.getStaticWitnessTable()));
      }
    }
  }
  ++ConformanceCacheGeneration;

  pthread_mutex_unlock(&SectionsToScanLock);
  // Start over with our newly-populated cache.
  type = origType;
  goto recur;
}

// The return type is incorrect.  It is only important that it is
// passed using 'sret'.
extern "C" OpaqueExistentialContainer
_TFSs24_injectValueIntoOptionalU__FQ_GSqQ__(OpaqueValue *value,
                                            const Metadata *T);
// The return type is incorrect.  It is only important that it is
// passed using 'sret'.
extern "C" OpaqueExistentialContainer
_TFSs26_injectNothingIntoOptionalU__FT_GSqQ__(const Metadata *T);

static inline bool swift_isClassOrObjCExistentialImpl(const Metadata *T) {
  auto kind = T->getKind();
#if SWIFT_OBJC_INTEROP
  return Metadata::isAnyKindOfClass(kind) ||
         (kind == MetadataKind::Existential &&
          static_cast<const ExistentialTypeMetadata *>(T)->isObjC());
#else
  return Metadata::isAnyKindOfClass(kind);
#endif
}

#if SWIFT_OBJC_INTEROP
//===----------------------------------------------------------------------===//
// Bridging to and from Objective-C
//===----------------------------------------------------------------------===//

namespace {

// protocol _ObjectiveCBridgeable {
struct _ObjectiveCBridgeableWitnessTable {
  // typealias _ObjectiveCType : class
  const Metadata *ObjectiveCType;

  // class func _isBridgedToObjectiveC() -> bool
  bool (*isBridgedToObjectiveC)(const Metadata *value, const Metadata *T);

  // class func _getObjectiveCType() -> Any.Type
  const Metadata *(*getObjectiveCType)(const Metadata *self,
                                       const Metadata *selfType);

  // func _bridgeToObjectiveC() -> _ObjectiveCType
  HeapObject *(*bridgeToObjectiveC)(OpaqueValue *self, const Metadata *Self);
  // class func _forceBridgeFromObjectiveC(x: _ObjectiveCType,
  //                                       inout result: Self?)
  void (*forceBridgeFromObjectiveC)(HeapObject *sourceValue,
                                    OpaqueValue *result,
                                    const Metadata *self,
                                    const Metadata *selfType);

  // class func _conditionallyBridgeFromObjectiveC(x: _ObjectiveCType,
  //                                              inout result: Self?) -> Bool
  bool (*conditionallyBridgeFromObjectiveC)(HeapObject *sourceValue,
                                            OpaqueValue *result,
                                            const Metadata *self,
                                            const Metadata *selfType);
};
// }

} // unnamed namespace

extern "C" const ProtocolDescriptor _TMpSs21_ObjectiveCBridgeable;

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
  // Check whether the source is bridged to Objective-C.
  if (!srcBridgeWitness->isBridgedToObjectiveC(srcType, srcType)) {
    return _fail(src, srcType, targetType, flags);
  }

  // Bridge the source value to an object.
  auto srcBridgedObject = srcBridgeWitness->bridgeToObjectiveC(src, srcType);

  // Dynamic cast the object to the resulting class type. The
  // additional flags essneitally make this call act as taking the
  // source object at +1.
  DynamicCastFlags classCastFlags = flags | DynamicCastFlags::TakeOnSuccess
                                  | DynamicCastFlags::DestroyOnFailure;
  bool success = _dynamicCastUnknownClass(dest, srcBridgedObject, targetType,
                                          classCastFlags);

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
  // Check whether the source is bridged to Objective-C.
  if (!srcBridgeWitness->isBridgedToObjectiveC(srcType, srcType)) {
    return _fail(src, srcType, targetType, flags);
  }

  // Bridge the source value to an object.
  auto srcBridgedObject = srcBridgeWitness->bridgeToObjectiveC(src, srcType);

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
  // Check whether the target is bridged to Objective-C.
  if (!targetBridgeWitness->isBridgedToObjectiveC(targetType, targetType)) {
    return _fail(src, srcType, targetType, flags);
  }

  // Determine the class type to which the target value type is bridged.
  auto targetBridgedClass = targetBridgeWitness->getObjectiveCType(targetType,
                                                                   targetType);

  // Dynamic cast the source object to the class type to which the target value
  // type is bridged. If we succeed, we can bridge from there; if we fail,
  // there's nothing more to do.
  void *srcObject = *reinterpret_cast<void * const *>(src);
  DynamicCastFlags classCastFlags = flags;
  void *srcBridgedObject = nullptr;
  if (!_dynamicCastUnknownClass(
         reinterpret_cast<OpaqueValue *>(&srcBridgedObject), srcObject,
         targetBridgedClass, classCastFlags)) {
    return false;
  }

  // Unless we're always supposed to consume the input, retain the
  // object because the witness takes it at +1.
  bool alwaysConsumeSrc = (flags & DynamicCastFlags::TakeOnSuccess) &&
                          (flags & DynamicCastFlags::DestroyOnFailure);
  if (!alwaysConsumeSrc) {
    swift_unknownRetain(srcBridgedObject);
  }

  // Object that frees a buffer when it goes out of scope.
  struct FreeBuffer {
    void *Buffer = nullptr;
    ~FreeBuffer() { free(Buffer); }
  } freeBuffer;

  // Allocate a buffer to store the T? returned by bridging.
  // The extra byte is for the tag.
  const std::size_t inlineValueSize = 3 * sizeof(void*);
  alignas(swift_max_align_t) char inlineBuffer[inlineValueSize + 1];
  void *optDestBuffer;
  if (targetType->getValueWitnesses()->getStride() <= inlineValueSize) {
    // Use the inline buffer.
    optDestBuffer = inlineBuffer;
  } else {
    // Allocate a buffer.
    optDestBuffer = malloc(targetType->getValueWitnesses()->size);
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
      (HeapObject *)srcBridgedObject, (OpaqueValue *)optDestBuffer, 
      targetType, targetType);
    success = true;
  } else {
    // For a conditional dynamic cast, use conditionallyBridgeFromObjectiveC.
    success = targetBridgeWitness->conditionallyBridgeFromObjectiveC(
                (HeapObject *)srcBridgedObject, (OpaqueValue *)optDestBuffer,
                targetType, targetType);
  }

  // If we succeeded, take from the optional buffer into the
  // destination buffer.
  if (success) {
    targetType->vw_initializeWithTake(dest, (OpaqueValue *)optDestBuffer);
  }

  // Unless we're always supposed to consume the input, release the
  // input if we need to now.
  if (!alwaysConsumeSrc && shouldDeallocateSource(success, flags)) {
    swift_unknownRelease(srcBridgedObject);
  }

  return success;
}

//===--- Bridging helpers for the Swift stdlib ----------------------------===//
// Functions that must discover and possibly use an arbitrary type's
// conformance to a given protocol.  See ../core/BridgeObjectiveC.swift for
// documentation.
//===----------------------------------------------------------------------===//
static const _ObjectiveCBridgeableWitnessTable *
findBridgeWitness(const Metadata *T) {
  auto w = swift_conformsToProtocol(T, &_TMpSs21_ObjectiveCBridgeable);
  return reinterpret_cast<const _ObjectiveCBridgeableWitnessTable *>(w);
}

/// \param value passed at +1, consumed.
extern "C" HeapObject *swift_bridgeNonVerbatimToObjectiveC(
  OpaqueValue *value, const Metadata *T
) {
  assert(!swift_isClassOrObjCExistentialImpl(T));

  if (const auto *bridgeWitness = findBridgeWitness(T)) {
    if (!bridgeWitness->isBridgedToObjectiveC(T, T)) {
      // Witnesses take 'self' at +0, so we still need to consume the +1 argument.
      T->vw_destroy(value);
      return nullptr;
    }
    auto result = bridgeWitness->bridgeToObjectiveC(value, T);
    // Witnesses take 'self' at +0, so we still need to consume the +1 argument.
    T->vw_destroy(value);
    return result;
  }

  // Consume the +1 argument.
  T->vw_destroy(value);
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
//     x: AnyObject, 
//     nativeType: NativeType.Type
//     inout result: T?
// )
extern "C" void
swift_bridgeNonVerbatimFromObjectiveC(
  HeapObject *sourceValue,
  const Metadata *nativeType,
  OpaqueValue *destValue,
  const Metadata *nativeType_
) {
  // Check if the type conforms to _BridgedToObjectiveC.
  if (const auto *bridgeWitness = findBridgeWitness(nativeType)) {
    // if the type also conforms to _ConditionallyBridgedToObjectiveC,
    // make sure it bridges at runtime
    if (bridgeWitness->isBridgedToObjectiveC(nativeType, nativeType)) {
      // Check if sourceValue has the _ObjectiveCType type required by the
      // protocol.
      const Metadata *objectiveCType =
          bridgeWitness->getObjectiveCType(nativeType, nativeType);
        
      auto sourceValueAsObjectiveCType =
          const_cast<void*>(swift_dynamicCastUnknownClass(sourceValue,
                                                          objectiveCType));
        
      if (sourceValueAsObjectiveCType) {
        // The type matches.  _forceBridgeFromObjectiveC returns `Self`, so
        // we can just return it directly.
        bridgeWitness->forceBridgeFromObjectiveC(
          static_cast<HeapObject*>(sourceValueAsObjectiveCType),
          destValue, nativeType, nativeType);
        return;
      }
    }
  }

  // Fail.
  swift::crash("value type is not bridged to Objective-C");
}

// @asmname("swift_bridgeNonVerbatimFromObjectiveCConditional")
// func _bridgeNonVerbatimFromObjectiveCConditional<NativeType>(
//   x: AnyObject, 
//   nativeType: T.Type,
//   inout result: T?
// ) -> Bool
extern "C" bool
swift_bridgeNonVerbatimFromObjectiveCConditional(
  HeapObject *sourceValue,
  const Metadata *nativeType,
  OpaqueValue *destValue,
  const Metadata *nativeType_
) {
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
    bridgeWitness->getObjectiveCType(nativeType, nativeType);
        
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
    destValue, nativeType, nativeType);
}

// func isBridgedNonVerbatimToObjectiveC<T>(x: T.Type) -> Bool
extern "C" bool swift_isBridgedNonVerbatimToObjectiveC(
  const Metadata *value, const Metadata *T
) {
  assert(!swift_isClassOrObjCExistentialImpl(T));

  auto bridgeWitness = findBridgeWitness(T);
  return bridgeWitness && bridgeWitness->isBridgedToObjectiveC(value, T);
}
#endif

// func isClassOrObjCExistential<T>(x: T.Type) -> Bool
extern "C" bool swift_isClassOrObjCExistential(const Metadata *value,
                                               const Metadata *T) {
  return swift_isClassOrObjCExistentialImpl(T);
}

// func _swift_isClass(x: Any) -> Bool
extern "C" bool _swift_isClass(OpaqueExistentialContainer *value) {
  bool Result = Metadata::isAnyKindOfClass(value->Type->getKind());

  // Destroy value->Buffer since the Any is passed in at +1.
  value->Type->vw_destroyBuffer(&value->Buffer);

  return Result;
}
