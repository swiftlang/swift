//===----------------------------------------------------------------------===//
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

#include "swift/Runtime/Reflection.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Enum.h"
#include "swift/Runtime/Unreachable.h"
#include "swift/Basic/Demangle.h"
#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Portability.h"
#include "Private.h"
#include "WeakReference.h"
#include "llvm/Support/Compiler.h"
#include <cassert>
#include <cstdio>
#include <cstring>
#include <new>
#include <string>
#include <tuple>

#if SWIFT_OBJC_INTEROP
#include "swift/Runtime/ObjCBridge.h"
#include <Foundation/Foundation.h>
#include <objc/objc.h>
#include <objc/runtime.h>
#endif

using namespace swift;

#if SWIFT_OBJC_INTEROP
// Declare the debugQuickLookObject selector.
@interface DeclareSelectors

- (id)debugQuickLookObject;
@end

@class SwiftObject;
#endif

namespace {

/// The layout of Any.
using Any = OpaqueExistentialContainer;

// Swift assumes Any is returned in memory.
// Use AnyReturn to guarantee that even on architectures
// where Any would be returned in registers.
struct AnyReturn {
  Any any;
  AnyReturn(Any a) : any(a) { }
  operator Any() { return any; }
  ~AnyReturn() { }
};

struct MagicMirrorData;

struct String;

SWIFT_CC(swift)
extern "C" void swift_stringFromUTF8InRawMemory(String *out,
                                                const char *start,
                                                intptr_t len);

struct String {
  // Keep the details of String's implementation opaque to the runtime.
  const void *x, *y, *z;

  /// Keep String trivial on the C++ side so we can control its instantiation.
  String() = default;

  /// Wrap a string literal in a swift String.
  template<size_t N>
  explicit String(const char (&s)[N]) {
    swift_stringFromUTF8InRawMemory(this, s, N-1);
  }

  /// Copy an ASCII string into a swift String on the heap.
  explicit String(const char *ptr, size_t size) {
    swift_stringFromUTF8InRawMemory(this, ptr, size);
  }

  explicit String(const char *ptr)
    : String(ptr, strlen(ptr))
  {}

#if SWIFT_OBJC_INTEROP
  explicit String(NSString *s)
    // FIXME: Use the usual NSString bridging entry point.
    : String([s UTF8String])
  {}
#endif
};

/// A Mirror witness table for use by MagicMirror.
struct MirrorWitnessTable;

// This structure needs to mirror _MagicMirrorData in the stdlib.
struct MagicMirrorData {
  /// The owner pointer for the buffer the value lives in. For class values
  /// this is the class instance itself. The mirror owns a strong reference to
  /// this object.
  HeapObject *Owner;
  /// The pointer to the value. The mirror does not own the referenced value.
  const OpaqueValue *Value;
  /// The type metadata for the referenced value. For an ObjC witness, this is
  /// the ObjC class.
  const Metadata *Type;
};
static_assert(sizeof(MagicMirrorData) == sizeof(ValueBuffer),
              "MagicMirrorData doesn't exactly fill a ValueBuffer");

/// A magic implementation of Mirror that can use runtime metadata to walk an
/// arbitrary object.
///
/// This type is layout-compatible with a Swift existential container for the
/// _Mirror protocol.
class MagicMirror {
public:
  // The data for the mirror.
  MagicMirrorData Data;

  // The existential header.
  const Metadata *Self;
  const MirrorWitnessTable *MirrorWitness;

  MagicMirror() = default;

  /// Build a new MagicMirror for type T by taking ownership of the referenced
  /// value.
  MagicMirror(OpaqueValue *value, const Metadata *T, bool take);

  /// Build a new MagicMirror for type T, sharing ownership with an existing
  /// heap object, which is retained.
  MagicMirror(HeapObject *owner, const OpaqueValue *value, const Metadata *T);
};

static_assert(alignof(MagicMirror) == alignof(Mirror),
              "MagicMirror layout does not match existential container");
static_assert(sizeof(MagicMirror) == sizeof(Mirror),
              "MagicMirror layout does not match existential container");
static_assert(offsetof(MagicMirror, Data) == offsetof(OpaqueExistentialContainer, Buffer),
              "MagicMirror layout does not match existential container");
static_assert(offsetof(MagicMirror, Self) == offsetof(OpaqueExistentialContainer, Type),
              "MagicMirror layout does not match existential container");
static_assert(offsetof(MagicMirror, MirrorWitness) ==
              offsetof(Mirror, MirrorWitness),
              "MagicMirror layout does not match existential container");

// -- Build an Any from an arbitrary value unowned-referenced by a mirror.

// We intentionally use a non-POD return type with these entry points to give
// them an indirect return ABI for compatibility with Swift.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"


SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
AnyReturn swift_MagicMirrorData_value(HeapObject *owner,
                                      const OpaqueValue *value,
                                      const Metadata *type) {
  Any result;

  result.Type = type;
  type->vw_initializeBufferWithCopy(&result.Buffer,
                                    const_cast<OpaqueValue*>(value));

  return AnyReturn(result);
}
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
const Metadata *swift_MagicMirrorData_valueType(HeapObject *owner,
                                                const OpaqueValue *value,
                                                const Metadata *type) {
  return swift_getDynamicType(const_cast<OpaqueValue*>(value), type,
                              /*existential metatype*/ true);
}

#if SWIFT_OBJC_INTEROP
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
AnyReturn swift_MagicMirrorData_objcValue(HeapObject *owner,
                                          const OpaqueValue *value,
                                          const Metadata *type) {
  Any result;

  void *object = *reinterpret_cast<void * const *>(value);
  auto isa = _swift_getClass(object);
  result.Type = swift_getObjCClassMetadata(isa);
  swift_unknownRetain(object);
  *reinterpret_cast<void **>(&result.Buffer) = object;
  return AnyReturn(result);
}
#endif

#pragma clang diagnostic pop

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
const char *swift_OpaqueSummary(const Metadata *T) {
  switch (T->getKind()) {
    case MetadataKind::Class:
    case MetadataKind::Struct:
    case MetadataKind::Enum:
    case MetadataKind::Optional:
    case MetadataKind::Metatype:
      return nullptr;
    case MetadataKind::Opaque:
      return "(Opaque Value)";
    case MetadataKind::Tuple:
      return "(Tuple)";
    case MetadataKind::Function:
      return "(Function)";
    case MetadataKind::Existential:
      return "(Existential)";
    case MetadataKind::ObjCClassWrapper:
      return "(Objective-C Class Wrapper)";
    case MetadataKind::ExistentialMetatype:
      return "(Existential Metatype)";
    case MetadataKind::ForeignClass:
      return "(Foreign Class)";
    case MetadataKind::HeapLocalVariable:
      return "(Heap Local Variable)";
    case MetadataKind::HeapGenericLocalVariable:
      return "(Heap Generic Local Variable)";
    case MetadataKind::ErrorObject:
      return "(ErrorType Object)";
  }

  swift_runtime_unreachable("Unhandled MetadataKind in switch.");
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
void swift_MagicMirrorData_summary(const Metadata *T, String *result) {
  switch (T->getKind()) {
    case MetadataKind::Class:
      new (result) String("(Class)");
      break;
    case MetadataKind::Struct:
      new (result) String("(Struct)");
      break;
    case MetadataKind::Enum:
    case MetadataKind::Optional:
      new (result) String("(Enum Value)");
      break;
    case MetadataKind::Opaque:
      new (result) String("(Opaque Value)");
      break;
    case MetadataKind::Tuple:
      new (result) String("(Tuple)");
      break;
    case MetadataKind::Function:
      new (result) String("(Function)");
      break;
    case MetadataKind::Existential:
      new (result) String("(Existential)");
      break;
    case MetadataKind::Metatype:
      new (result) String("(Metatype)");
      break;
    case MetadataKind::ObjCClassWrapper:
      new (result) String("(Objective-C Class Wrapper)");
      break;
    case MetadataKind::ExistentialMetatype:
      new (result) String("(ExistentialMetatype)");
      break;
    case MetadataKind::ForeignClass:
      new (result) String("(Foreign Class)");
      break;
    case MetadataKind::HeapLocalVariable:
      new (result) String("(Heap Local Variable)");
      break;
    case MetadataKind::HeapGenericLocalVariable:
      new (result) String("(Heap Generic Local Variable)");
      break;
    case MetadataKind::ErrorObject:
      new (result) String("(Error Object)");
      break;
  }
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
const Metadata *swift_MagicMirrorData_objcValueType(HeapObject *owner,
                                                    const OpaqueValue *value,
                                                    const Metadata *type) {
  void *object = *reinterpret_cast<void * const *>(value);
  auto isa = _swift_getClass(object);
  return swift_getObjCClassMetadata(isa);
}

static std::tuple<const Metadata *, const OpaqueValue *>
unwrapExistential(const Metadata *T, const OpaqueValue *Value) {
  // If the value is an existential container, look through it to reflect the
  // contained value.
  // TODO: Should look through existential metatypes too, but it doesn't
  // really matter yet since we don't have any special mirror behavior for
  // concrete metatypes yet.
  while (T->getKind() == MetadataKind::Existential) {
    auto existential
      = static_cast<const ExistentialTypeMetadata *>(T);

    // Unwrap the existential container.
    T = existential->getDynamicType(Value);
    Value = existential->projectValue(Value);

    // Existential containers can end up nested in some cases due to generic
    // abstraction barriers.  Repeat in case we have a nested existential.
  }
  return std::make_tuple(T, Value);
}

/// Produce a mirror for any value, like swift_reflectAny, but do not consume
/// the value, so we can produce a mirror for a subobject of a value already
/// owned by a mirror.
///
/// \param owner passed at +1, consumed.
/// \param value passed unowned.
static Mirror reflect(HeapObject *owner,
                      const OpaqueValue *value,
                      const Metadata *T) {
  const Metadata *mirrorType;
  const OpaqueValue *mirrorValue;
  std::tie(mirrorType, mirrorValue) = unwrapExistential(T, value);

  // Use MagicMirror.
  // Consumes 'owner'.
  Mirror result;
  ::new (&result) MagicMirror(owner, mirrorValue, mirrorType);
  return result;
}

// -- Tuple destructuring.

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
intptr_t swift_TupleMirror_count(HeapObject *owner,
                                 const OpaqueValue *value,
                                 const Metadata *type) {
  auto Tuple = static_cast<const TupleTypeMetadata *>(type);
  swift_release(owner);
  return Tuple->NumElements;
}

/// \param owner passed at +1, consumed.
/// \param value passed unowned.
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
void swift_TupleMirror_subscript(String *outString,
                                 Mirror *outMirror,
                                 intptr_t i,
                                 HeapObject *owner,
                                 const OpaqueValue *value,
                                 const Metadata *type) {
  auto Tuple = static_cast<const TupleTypeMetadata *>(type);

  if (i < 0 || (size_t)i > Tuple->NumElements)
    swift::crash("Swift mirror subscript bounds check failure");

  // Determine whether there is a label.
  bool hasLabel = false;
  if (const char *labels = Tuple->Labels) {
    const char *space = strchr(labels, ' ');
    for (intptr_t j = 0; j != i && space; ++j) {
      labels = space + 1;
      space = strchr(labels, ' ');
    }

    // If we have a label, create it.
    if (labels && space && labels != space) {
      new (outString) String(labels, space - labels);
      hasLabel = true;
    }
  }

  if (!hasLabel) {
    // The name is the stringized element number '.0'.
    char buf[32];
    snprintf(buf, sizeof(buf), ".%zd", i);
    new (outString) String(buf, strlen(buf));
  }

  // Get a Mirror for the nth element.
  auto &elt = Tuple->getElement(i);
  auto bytes = reinterpret_cast<const char*>(value);
  auto eltData = reinterpret_cast<const OpaqueValue *>(bytes + elt.Offset);

  // 'owner' is consumed by this call.
  new (outMirror) Mirror(reflect(owner, eltData, elt.Type));
}

// Get a field name from a doubly-null-terminated list.
static const char *getFieldName(const char *fieldNames, size_t i) {
  const char *fieldName = fieldNames;
  for (size_t j = 0; j < i; ++j) {
    size_t len = strlen(fieldName);
    assert(len != 0);
    fieldName += len + 1;
  }

  return fieldName;
}


static bool loadSpecialReferenceStorage(HeapObject *owner,
                                        OpaqueValue *fieldData,
                                        const FieldType fieldType,
                                        Mirror *outMirror) {
  // isWeak() implies a reference type via Sema.
  if (!fieldType.isWeak())
    return false;

  auto type = fieldType.getType();
  assert(type->getKind() == MetadataKind::Optional);

  auto weakField = reinterpret_cast<WeakReference *>(fieldData);
  auto strongValue = swift_unknownWeakLoadStrong(weakField);

  // Now that we have a strong reference, we need to create a temporary buffer
  // from which to copy the whole value, which might be a native class-bound
  // existential, which means we also need to copy n witness tables, for
  // however many protocols are in the protocol composition. For example, if we
  // are copying a:
  // weak var myWeakProperty : (Protocol1 & Protocol2)?
  // then we need to copy three values:
  // - the instance
  // - the witness table for Protocol1
  // - the witness table for Protocol2

  auto weakContainer =
    reinterpret_cast<WeakClassExistentialContainer *>(fieldData);

  // Create a temporary existential where we can put the strong reference.
  // The allocateBuffer value witness requires a ValueBuffer to own the
  // allocated storage.
  ValueBuffer temporaryBuffer;

  auto temporaryValue =
    reinterpret_cast<ClassExistentialContainer *>(
      type->vw_allocateBuffer(&temporaryBuffer));

  // Now copy the entire value out of the parent, which will include the
  // witness tables.
  temporaryValue->Value = strongValue;
  auto valueWitnessesSize = type->getValueWitnesses()->getSize() -
                            sizeof(WeakClassExistentialContainer);
  memcpy(temporaryValue->getWitnessTables(), weakContainer->getWitnessTables(),
         valueWitnessesSize);

  // This MagicMirror constructor creates a box to hold the loaded reference
  // value, which becomes the new owner for the value.
  new (outMirror) MagicMirror(reinterpret_cast<OpaqueValue *>(temporaryValue),
                              type, /*take*/ true);

  type->vw_deallocateBuffer(&temporaryBuffer);

  // swift_StructMirror_subscript and swift_ClassMirror_subscript
  // requires that the owner be consumed. Since we have the new heap box as the
  // owner now, we need to release the old owner to maintain the contract.
  if (owner->metadata->isAnyClass())
    swift_unknownRelease(owner);
  else
    swift_release(owner);

  return true;
}

// -- Struct destructuring.

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
intptr_t swift_StructMirror_count(HeapObject *owner,
                                  const OpaqueValue *value,
                                  const Metadata *type) {
  auto Struct = static_cast<const StructMetadata *>(type);
  swift_release(owner);
  return Struct->Description->Struct.NumFields;
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
void swift_StructMirror_subscript(String *outString,
                                  Mirror *outMirror,
                                  intptr_t i,
                                  HeapObject *owner,
                                  OpaqueValue *value,
                                  const Metadata *type) {
  auto Struct = static_cast<const StructMetadata *>(type);

  if (i < 0 || (size_t)i > Struct->Description->Struct.NumFields)
    swift::crash("Swift mirror subscript bounds check failure");

  // Load the type and offset from their respective vectors.
  auto fieldType = Struct->getFieldTypes()[i];
  auto fieldOffset = Struct->getFieldOffsets()[i];

  auto bytes = reinterpret_cast<char*>(value);
  auto fieldData = reinterpret_cast<OpaqueValue *>(bytes + fieldOffset);

  new (outString) String(getFieldName(Struct->Description->Struct.FieldNames, i));

  // 'owner' is consumed by this call.
  assert(!fieldType.isIndirect() && "indirect struct fields not implemented");

  if (loadSpecialReferenceStorage(owner, fieldData, fieldType, outMirror))
    return;

  new (outMirror) Mirror(reflect(owner, fieldData, fieldType.getType()));
}

// -- Enum destructuring.

static bool isEnumReflectable(const Metadata *type) {
  const auto Enum = static_cast<const EnumMetadata *>(type);
  const auto &Description = Enum->Description->Enum;

  // No metadata for C and @objc enums yet
  if (Description.CaseNames == nullptr)
    return false;

  return true;
}

static void getEnumMirrorInfo(const OpaqueValue *value,
                              const Metadata *type,
                              unsigned *tagPtr,
                              const Metadata **payloadTypePtr,
                              bool *indirectPtr) {
  const auto Enum = static_cast<const EnumMetadata *>(type);
  const auto &Description = Enum->Description->Enum;

  unsigned payloadCases = Description.getNumPayloadCases();

  // 'tag' is in the range [-ElementsWithPayload..ElementsWithNoPayload-1].
  int tag = type->vw_getEnumTag(value);

  // Convert resilient tag index to fragile tag index.
  tag += payloadCases;

  const Metadata *payloadType = nullptr;
  bool indirect = false;

  if (static_cast<unsigned>(tag) < payloadCases) {
    auto payload = Description.GetCaseTypes(type)[tag];
    payloadType = payload.getType();
    indirect = payload.isIndirect();
  }

  if (tagPtr)
    *tagPtr = tag;
  if (payloadTypePtr)
    *payloadTypePtr = payloadType;
  if (indirectPtr)
    *indirectPtr = indirect;
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
const char *swift_EnumMirror_caseName(HeapObject *owner,
                                      const OpaqueValue *value,
                                      const Metadata *type) {
  if (!isEnumReflectable(type)) {
    swift_release(owner);
    return nullptr;
  }

  const auto Enum = static_cast<const EnumMetadata *>(type);
  const auto &Description = Enum->Description->Enum;

  unsigned tag;
  getEnumMirrorInfo(value, type, &tag, nullptr, nullptr);

  swift_release(owner);

  return getFieldName(Description.CaseNames, tag);
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
const char *swift_EnumCaseName(OpaqueValue *value, const Metadata *type) {
  // Build a magic mirror. Unconditionally destroy the value at the end.
  const Metadata *mirrorType;
  const OpaqueValue *cMirrorValue;
  std::tie(mirrorType, cMirrorValue) = unwrapExistential(type, value);

  OpaqueValue *mirrorValue = const_cast<OpaqueValue*>(cMirrorValue);
  Mirror mirror;

  bool take = mirrorValue == value;
  ::new (&mirror) MagicMirror(mirrorValue, mirrorType, take);

  MagicMirror *theMirror = reinterpret_cast<MagicMirror *>(&mirror);
  MagicMirrorData data = theMirror->Data;
  const char *result = swift_EnumMirror_caseName(data.Owner, data.Value, data.Type);
  return result;
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
intptr_t swift_EnumMirror_count(HeapObject *owner,
                                const OpaqueValue *value,
                                const Metadata *type) {
  if (!isEnumReflectable(type)) {
    swift_release(owner);
    return 0;
  }

  const Metadata *payloadType;
  getEnumMirrorInfo(value, type, nullptr, &payloadType, nullptr);
  swift_release(owner);
  return (payloadType != nullptr) ? 1 : 0;
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
void swift_EnumMirror_subscript(String *outString,
                                Mirror *outMirror,
                                intptr_t i,
                                HeapObject *owner,
                                const OpaqueValue *value,
                                const Metadata *type) {
  const auto Enum = static_cast<const EnumMetadata *>(type);
  const auto &Description = Enum->Description->Enum;

  unsigned tag;
  const Metadata *payloadType;
  bool indirect;

  getEnumMirrorInfo(value, type, &tag, &payloadType, &indirect);

  // Copy the enum payload into a box
  const Metadata *boxType = (indirect ? &METADATA_SYM(Bo).base : payloadType);
  BoxPair pair = swift_allocBox(boxType);

  type->vw_destructiveProjectEnumData(const_cast<OpaqueValue *>(value));
  boxType->vw_initializeWithCopy(pair.second, const_cast<OpaqueValue *>(value));
  type->vw_destructiveInjectEnumTag(const_cast<OpaqueValue *>(value),
                                    (int) (tag - Description.getNumPayloadCases()));

  swift_release(owner);

  owner = pair.first;
  value = pair.second;

  // If the payload is indirect, we need to jump through the box to get it.
  if (indirect) {
    owner = *reinterpret_cast<HeapObject * const *>(value);
    value = swift_projectBox(const_cast<HeapObject *>(owner));
    swift_retain(owner);
    swift_release(pair.first);
  }

  new (outString) String(getFieldName(Description.CaseNames, tag));
  new (outMirror) Mirror(reflect(owner, value, payloadType));
}

// -- Class destructuring.
static Mirror getMirrorForSuperclass(const ClassMetadata *sup,
                                     HeapObject *owner,
                                     const OpaqueValue *value,
                                     const Metadata *type);

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
intptr_t swift_ClassMirror_count(HeapObject *owner,
                                 const OpaqueValue *value,
                                 const Metadata *type) {
  auto Clas = static_cast<const ClassMetadata*>(type);
  swift_release(owner);
  auto count = Clas->getDescription()->Class.NumFields;

  // If the class has a superclass, the superclass instance is treated as the
  // first child.
  if (classHasSuperclass(Clas))
    count += 1;

  return count;
}

/// \param owner passed at +1, consumed.
/// \param value passed unowned.
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
void swift_ClassMirror_subscript(String *outString,
                                 Mirror *outMirror,
                                 intptr_t i,
                                 HeapObject *owner,
                                 OpaqueValue *value,
                                 const Metadata *type) {
  auto Clas = static_cast<const ClassMetadata*>(type);

  if (classHasSuperclass(Clas)) {
    // If the class has a superclass, the superclass instance is treated as the
    // first child.
    if (i == 0) {
      // FIXME: Put superclass name here
      new (outString) String("super");
      new (outMirror) Mirror(
        getMirrorForSuperclass(Clas->SuperClass, owner, value, type));
      return;
    }
    --i;
  }

  if (i < 0 || (size_t)i > Clas->getDescription()->Class.NumFields)
    swift::crash("Swift mirror subscript bounds check failure");

  // Load the type and offset from their respective vectors.
  auto fieldType = Clas->getFieldTypes()[i];
  assert(!fieldType.isIndirect()
         && "class indirect properties not implemented");

  // FIXME: If the class has ObjC heritage, get the field offset using the ObjC
  // metadata, because we don't update the field offsets in the face of
  // resilient base classes.
  uintptr_t fieldOffset;
  if (usesNativeSwiftReferenceCounting(Clas)) {
    fieldOffset = Clas->getFieldOffsets()[i];
  } else {
#if SWIFT_OBJC_INTEROP
    Ivar *ivars = class_copyIvarList((Class)Clas, nullptr);
    fieldOffset = ivar_getOffset(ivars[i]);
    free(ivars);
#else
    swift::crash("Object appears to be Objective-C, but no runtime.");
#endif
  }

  auto bytes = *reinterpret_cast<char * const *>(value);
  auto fieldData = reinterpret_cast<OpaqueValue *>(bytes + fieldOffset);

  new (outString) String(getFieldName(Clas->getDescription()->Class.FieldNames,
                                      i));

 if (loadSpecialReferenceStorage(owner, fieldData, fieldType, outMirror))
   return;

  // 'owner' is consumed by this call.
  new (outMirror) Mirror(reflect(owner, fieldData, fieldType.getType()));
}

// -- Mirror witnesses for ObjC classes.

#if SWIFT_OBJC_INTEROP

extern "C" const Metadata METADATA_SYM(Sb); // Bool
extern "C" const Metadata METADATA_SYM(Si); // Int
extern "C" const Metadata METADATA_SYM(Su); // UInt
extern "C" const Metadata METADATA_SYM(Sf); // Float
extern "C" const Metadata METADATA_SYM(Sd); // Double
extern "C" const Metadata STRUCT_METADATA_SYM(s4Int8);
extern "C" const Metadata STRUCT_METADATA_SYM(s5Int16);
extern "C" const Metadata STRUCT_METADATA_SYM(s5Int32);
extern "C" const Metadata STRUCT_METADATA_SYM(s5Int64);
extern "C" const Metadata STRUCT_METADATA_SYM(s5UInt8);
extern "C" const Metadata STRUCT_METADATA_SYM(s6UInt16);
extern "C" const Metadata STRUCT_METADATA_SYM(s6UInt32);
extern "C" const Metadata STRUCT_METADATA_SYM(s6UInt64);

// Set to 1 to enable reflection of objc ivars.
#define REFLECT_OBJC_IVARS 0

/// Map an ObjC type encoding string to a Swift type metadata object.
///
#if REFLECT_OBJC_IVARS
static const Metadata *getMetadataForEncoding(const char *encoding) {
  switch (*encoding) {
  case 'c': // char
    return &STRUCT_METADATA_SYM(s4Int8);
  case 's': // short
    return &STRUCT_METADATA_SYM(s5Int16);
  case 'i': // int
    return &STRUCT_METADATA_SYM(s5Int32);
  case 'l': // long
    return &METADATA_SYM(Si);
  case 'q': // long long
    return &STRUCT_METADATA_SYM(s5Int64);

  case 'C': // unsigned char
    return &STRUCT_METADATA_SYM(s5UInt8);
  case 'S': // unsigned short
    return &STRUCT_METADATA_SYM(s6UInt16);
  case 'I': // unsigned int
    return &STRUCT_METADATA_SYM(s6UInt32);
  case 'L': // unsigned long
    return &METADATA_SYM(Su);
  case 'Q': // unsigned long long
    return &STRUCT_METADATA_SYM(s6UInt64);

  case 'B': // _Bool
    return &METADATA_SYM(Sb);

  case '@': { // Class
    // TODO: Better metadata?
    const OpaqueMetadata *M = &METADATA_SYM(BO);
    return &M->base;
  }

  default: // TODO
    // Return 'void' as the type of fields we don't understand.
    return &METADATA_SYM(EMPTY_TUPLE_MANGLING);
  }
}
#endif

/// \param owner passed at +1, consumed.
/// \param value passed unowned.
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
intptr_t swift_ObjCMirror_count(HeapObject *owner,
                                const OpaqueValue *value,
                                const Metadata *type) {
  auto isa = (Class)type;

  unsigned count = 0;
#if REFLECT_OBJC_IVARS
  // Don't reflect ivars of classes that lie about their layout.
  if (objcClassLiesAboutLayout(isa)) {
    count = 0;
  } else {
    // Copying the ivar list just to free it is lame, but we have
    // nowhere to save it.
    Ivar *ivars = class_copyIvarList(isa, &count);
    free(ivars);
  }
#else
  // ObjC makes no guarantees about the state of ivars, so we can't safely
  // introspect them in the general case.

  // The superobject counts as a child.
  if (_swift_getSuperclass((const ClassMetadata*) isa))
    count += 1;

  swift_release(owner);
  return count;
#endif
}

static Mirror ObjC_getMirrorForSuperclass(Class sup,
                                          HeapObject *owner,
                                          const OpaqueValue *value,
                                          const Metadata *type);

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
void swift_ObjCMirror_subscript(String *outString,
                                Mirror *outMirror,
                                intptr_t i,
                                HeapObject *owner,
                                const OpaqueValue *value,
                                const Metadata *type) {
#if REFLECT_OBJC_IVARS
  id object = *reinterpret_cast<const id *>(value);
#endif
  auto isa = (Class)type;

  // If there's a superclass, it becomes the first child.
  if (auto sup = (Class) _swift_getSuperclass((const ClassMetadata*) isa)) {
    if (i == 0) {
      const char *supName = class_getName(sup);
      new (outString) String(supName, strlen(supName));
      new (outMirror) Mirror(
                        ObjC_getMirrorForSuperclass(sup, owner, value, type));
      return;
    }
    --i;
  }

#if REFLECT_OBJC_IVARS
  // Copying the ivar list just to free it is lame, but we have
  // no room to save it.
  unsigned count;
  Ivar *ivars;
  // Don't reflect ivars of classes that lie about their layout.
  if (objcClassLiesAboutLayout(isa)) {
    count = 0;
    ivars = nullptr;
  } else {
    // Copying the ivar list just to free it is lame, but we have
    // nowhere to save it.
    ivars = class_copyIvarList(isa, &count);
  }

  if (i < 0 || (uintptr_t)i >= (uintptr_t)count)
    swift::crash("Swift mirror subscript bounds check failure");

  const char *name = ivar_getName(ivars[i]);
  ptrdiff_t offset = ivar_getOffset(ivars[i]);
  const char *typeEncoding = ivar_getTypeEncoding(ivars[i]);
  free(ivars);

  const OpaqueValue *ivar =
    reinterpret_cast<const OpaqueValue *>(
    reinterpret_cast<const char*>(object) + offset);

  const Metadata *ivarType = getMetadataForEncoding(typeEncoding);

  new (outString) String(name, strlen(name));
  // 'owner' is consumed by this call.
  new (outMirror) Mirror(reflect(owner, ivar, ivarType));
#else
  // ObjC makes no guarantees about the state of ivars, so we can't safely
  // introspect them in the general case.
  abort();
#endif
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
id
swift_ClassMirror_quickLookObject(HeapObject *owner, const OpaqueValue *value,
                                  const Metadata *type) {
  id object = [*reinterpret_cast<const id *>(value) retain];
  swift_release(owner);
  if ([object respondsToSelector:@selector(debugQuickLookObject)]) {
    id quickLookObject = [object debugQuickLookObject];
    [quickLookObject retain];
    [object release];
    return quickLookObject;
  }

  return object;
}

#endif

// -- MagicMirror implementation.

#define MIRROR_CONFORMANCE_SYM(Mirror, Subst) \
  SELECT_MANGLING(WPV##Mirror##s7_Mirrors , Mirror##Vs01_##Subst##0sWP)
#define OBJC_MIRROR_CONFORMANCE_SYM() \
  SELECT_MANGLING(WPVs11_ObjCMirrors7_Mirrors, s11_ObjCMirrorVs7_MirrorsWP)

// Addresses of the type metadata and Mirror witness tables for the primitive
// mirrors.
typedef const Metadata *(*MetadataFn)();

extern "C" Metadata *OpaqueMirrorMetadata()
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(STRUCT_MD_ACCESSOR_SYM(s13_OpaqueMirror)));
extern "C" const MirrorWitnessTable OpaqueMirrorWitnessTable
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(MIRROR_CONFORMANCE_SYM(s13_OpaqueMirror, B)));
extern "C" Metadata *TupleMirrorMetadata()
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(STRUCT_MD_ACCESSOR_SYM(s12_TupleMirror)));
extern "C" const MirrorWitnessTable TupleMirrorWitnessTable
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(MIRROR_CONFORMANCE_SYM(s12_TupleMirror, B)));

extern "C" Metadata *StructMirrorMetadata()
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(STRUCT_MD_ACCESSOR_SYM(s13_StructMirror)));
extern "C" const MirrorWitnessTable StructMirrorWitnessTable
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(MIRROR_CONFORMANCE_SYM(s13_StructMirror, B)));

extern "C" Metadata *EnumMirrorMetadata()
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(STRUCT_MD_ACCESSOR_SYM(s11_EnumMirror)));
extern "C" const MirrorWitnessTable EnumMirrorWitnessTable
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(MIRROR_CONFORMANCE_SYM(s11_EnumMirror, B)));

extern "C" Metadata *ClassMirrorMetadata()
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(STRUCT_MD_ACCESSOR_SYM(s12_ClassMirror)));
extern "C" const MirrorWitnessTable ClassMirrorWitnessTable
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(MIRROR_CONFORMANCE_SYM(s12_ClassMirror, B)));

extern "C" Metadata *ClassSuperMirrorMetadata()
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(STRUCT_MD_ACCESSOR_SYM(s17_ClassSuperMirror)));
extern "C" const MirrorWitnessTable ClassSuperMirrorWitnessTable
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(MIRROR_CONFORMANCE_SYM(s17_ClassSuperMirror, C)));

extern "C" Metadata *MetatypeMirrorMetadata()
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(STRUCT_MD_ACCESSOR_SYM(s15_MetatypeMirror)));
extern "C" const MirrorWitnessTable MetatypeMirrorWitnessTable
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(MIRROR_CONFORMANCE_SYM(s15_MetatypeMirror, B)));

#if SWIFT_OBJC_INTEROP
extern "C" Metadata *ObjCMirrorMetadata()
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(STRUCT_MD_ACCESSOR_SYM(s11_ObjCMirror)));
extern "C" const MirrorWitnessTable ObjCMirrorWitnessTable
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(OBJC_MIRROR_CONFORMANCE_SYM()));
extern "C" Metadata *ObjCSuperMirrorMetadata()
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(STRUCT_MD_ACCESSOR_SYM(s16_ObjCSuperMirror)));
extern "C" const MirrorWitnessTable ObjCSuperMirrorWitnessTable
  __asm__(SWIFT_QUOTED_SYMBOL_NAME(MIRROR_CONFORMANCE_SYM(s16_ObjCSuperMirror, C)));
#endif

/// \param owner passed at +1, consumed.
/// \param value passed unowned.
static Mirror getMirrorForSuperclass(const ClassMetadata *sup,
                                     HeapObject *owner,
                                     const OpaqueValue *value,
                                     const Metadata *type) {
#if SWIFT_OBJC_INTEROP
  // If the superclass is natively ObjC, cut over to the ObjC mirror
  // implementation.
  if (!sup->isTypeMetadata())
    return ObjC_getMirrorForSuperclass((Class)sup, owner, value, type);
#endif

  Mirror resultBuf;
  MagicMirror *result = ::new (&resultBuf) MagicMirror;

  result->Self = ClassSuperMirrorMetadata();
  result->MirrorWitness = &ClassSuperMirrorWitnessTable;
  result->Data.Owner = owner;
  result->Data.Type = sup;
  result->Data.Value = value;

  return resultBuf;
}

#if SWIFT_OBJC_INTEROP
/// \param owner passed at +1, consumed.
/// \param value passed unowned.
static Mirror ObjC_getMirrorForSuperclass(Class sup,
                                          HeapObject *owner,
                                          const OpaqueValue *value,
                                          const Metadata *type) {
  Mirror resultBuf;
  MagicMirror *result = ::new (&resultBuf) MagicMirror;

  result->Self = ObjCSuperMirrorMetadata();
  result->MirrorWitness = &ObjCSuperMirrorWitnessTable;
  result->Data.Owner = owner;
  result->Data.Type = reinterpret_cast<ClassMetadata*>(sup);
  result->Data.Value = value;
  return resultBuf;
}
#endif

// (type being mirrored, mirror type, mirror witness)
using MirrorTriple
  = std::tuple<const Metadata *, const Metadata *, const MirrorWitnessTable *>;

static MirrorTriple
getImplementationForClass(const OpaqueValue *Value) {
  // Get the runtime type of the object.
  const void *obj = *reinterpret_cast<const void * const *>(Value);
  auto isa = _swift_getClass(obj);

  // Look through artificial subclasses.
  while (isa->isTypeMetadata() && isa->isArtificialSubclass()) {
    isa = isa->SuperClass;
  }

#if SWIFT_OBJC_INTEROP
  // If this is a pure ObjC class, reflect it using ObjC's runtime facilities.
  if (!isa->isTypeMetadata())
    return {isa, ObjCMirrorMetadata(), &ObjCMirrorWitnessTable};
#endif

  // Otherwise, use the native Swift facilities.
  return std::make_tuple(
      isa, ClassMirrorMetadata(), &ClassMirrorWitnessTable);
}

/// Get the magic mirror witnesses appropriate to a particular type.
static MirrorTriple
getImplementationForType(const Metadata *T, const OpaqueValue *Value) {
  switch (T->getKind()) {
  case MetadataKind::Tuple:
    return std::make_tuple(
        T, TupleMirrorMetadata(), &TupleMirrorWitnessTable);

  case MetadataKind::Struct:
    return std::make_tuple(
        T, StructMirrorMetadata(), &StructMirrorWitnessTable);

  case MetadataKind::Enum:
  case MetadataKind::Optional:
    return std::make_tuple(
        T, EnumMirrorMetadata(), &EnumMirrorWitnessTable);

  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass:
  case MetadataKind::Class: {
    return getImplementationForClass(Value);
  }

  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype: {
    return std::make_tuple(T, MetatypeMirrorMetadata(),
                           &MetatypeMirrorWitnessTable);
  }

  case MetadataKind::Opaque: {
#if SWIFT_OBJC_INTEROP
    // If this is the Builtin.UnknownObject type, use the dynamic type of the
    // object reference.
    if (T == &METADATA_SYM(BO).base) {
      return getImplementationForClass(Value);
    }
#endif
    // If this is the Builtin.NativeObject type, and the heap object is a
    // class instance, use the dynamic type of the object reference.
    if (T == &METADATA_SYM(Bo).base) {
      const HeapObject *obj
        = *reinterpret_cast<const HeapObject * const*>(Value);
      if (obj->metadata->getKind() == MetadataKind::Class)
        return getImplementationForClass(Value);
    }
    LLVM_FALLTHROUGH;
  }

  /// TODO: Implement specialized mirror witnesses for all kinds.
  case MetadataKind::Function:
  case MetadataKind::Existential:
    return std::make_tuple(
        T, OpaqueMirrorMetadata(), &OpaqueMirrorWitnessTable);

  // Types can't have these kinds.
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
    swift::crash("Swift mirror lookup failure");
  }

  swift_runtime_unreachable("Unhandled MetadataKind in switch.");
}

/// MagicMirror ownership-taking whole-value constructor.
///
/// \param owner passed at +1, consumed.
MagicMirror::MagicMirror(OpaqueValue *value, const Metadata *T,
                         bool take) {
  // Put value types into a box so we can take stable interior pointers.
  // TODO: Specialize behavior here. If the value is a swift-refcounted class
  // we don't need to put it in a box to point into it.
  BoxPair box = swift_allocBox(T);

  if (take)
    T->vw_initializeWithTake(box.second, value);
  else
    T->vw_initializeWithCopy(box.second, value);
  std::tie(T, Self, MirrorWitness) = getImplementationForType(T, box.second);

  Data = {box.first, box.second, T};
}

/// MagicMirror ownership-sharing subvalue constructor.
///
/// \param owner passed at +1, consumed.
MagicMirror::MagicMirror(HeapObject *owner,
                         const OpaqueValue *value, const Metadata *T) {
  std::tie(T, Self, MirrorWitness) = getImplementationForType(T, value);
  Data = {owner, value, T};
}

} // end anonymous namespace

/// func reflect<T>(x: T) -> Mirror
///
/// Produce a mirror for any value.  The runtime produces a mirror that
/// structurally reflects values of any type.
///
/// This function consumes 'value', following Swift's +1 convention for "in"
/// arguments.
SWIFT_CC(swift)
MirrorReturn swift::swift_reflectAny(OpaqueValue *value, const Metadata *T) {
  const Metadata *mirrorType;
  const OpaqueValue *cMirrorValue;
  std::tie(mirrorType, cMirrorValue) = unwrapExistential(T, value);

  OpaqueValue *mirrorValue = const_cast<OpaqueValue*>(cMirrorValue);

  // Use MagicMirror.
  Mirror result;
  // Take the value, unless we projected a subvalue from it. We don't want to
  // deal with partial value deinitialization.
  bool take = mirrorValue == value;
  ::new (&result) MagicMirror(mirrorValue, mirrorType, take);
  // Destroy the whole original value if we couldn't take it.
  if (!take)
    T->vw_destroy(value);
  return MirrorReturn(result);
}

// NB: This function is not used directly in the Swift codebase, but is
// exported for Xcode support and is used by the sanitizers. Please coordinate
// before changing.
//
/// Demangles a Swift symbol name.
///
/// \param mangledName is the symbol name that needs to be demangled.
/// \param mangledNameLength is the length of the string that should be
/// demangled.
/// \param outputBuffer is the user provided buffer where the demangled name
/// will be placed. If nullptr, a new buffer will be malloced. In that case,
/// the user of this API is responsible for freeing the returned buffer.
/// \param outputBufferSize is the size of the output buffer. If the demangled
/// name does not fit into the outputBuffer, the output will be truncated and
/// the size will be updated, indicating how large the buffer should be.
/// \param flags can be used to select the demangling style. TODO: We should
//// define what these will be.
/// \returns the demangled name. Returns nullptr if the input String is not a
/// Swift mangled name.
SWIFT_RUNTIME_EXPORT
char *swift_demangle(const char *mangledName,
                     size_t mangledNameLength,
                     char *outputBuffer,
                     size_t *outputBufferSize,
                     uint32_t flags) {
  if (flags != 0) {
    swift::fatalError(0, "Only 'flags' value of '0' is currently supported.");
  }
  if (outputBuffer != nullptr && outputBufferSize == nullptr) {
    swift::fatalError(0, "'outputBuffer' is passed but the size is 'nullptr'.");
  }

  // Check if we are dealing with Swift mangled name, otherwise, don't try
  // to demangle and send indication to the user.
  if (mangledName[0] != '_' || mangledName[1] != 'T') {
    return nullptr;
  }

  // Demangle the name.
  auto options = Demangle::DemangleOptions();
  options.DisplayDebuggerGeneratedModule = false;
  auto result =
      Demangle::demangleSymbolAsString(mangledName,
                                       mangledNameLength,
                                       options);

  // If the output buffer is not provided, malloc memory ourselves.
  if (outputBuffer == nullptr || *outputBufferSize == 0) {
    return strdup(result.c_str());
  }

  // Indicate a failure if the result does not fit and will be truncated
  // and set the required outputBufferSize.
  if (*outputBufferSize < result.length() + 1) {
    *outputBufferSize = result.length() + 1;
  }

  // Copy into the provided buffer.
  _swift_strlcpy(outputBuffer, result.c_str(), *outputBufferSize);
  return outputBuffer;
}
