//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Fallthrough.h"
#include "swift/Runtime/Reflection.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Enum.h"
#include "swift/Basic/Demangle.h"
#include "swift/Runtime/Debug.h"
#include "Private.h"
#include <cassert>
#include <cstring>
#include <new>
#include <string>
#include <dlfcn.h>

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
  
/// The layout of protocol<>.
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

/// A _Reflectable witness table.
struct _ReflectableWitnessTable {
  /// func _getMirror() -> Mirror
  Mirror (*getMirror)(OpaqueValue *self, const Metadata *Self);
};
  
struct MagicMirrorData;
  
struct String;
  
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

  /// Create a Swift String from two concatenated nul-terminated strings.
  explicit String(const char *ptr1, const char *ptr2) {
    size_t len1 = strlen(ptr1);
    size_t len2 = strlen(ptr2);
    char *concatenated = static_cast<char *>(malloc(len1 + len2));
    memcpy(concatenated, ptr1, len1);
    memcpy(concatenated + len1, ptr2, len2);
    swift_stringFromUTF8InRawMemory(this, concatenated, len1 + len2);
    free(concatenated);
  }
#if SWIFT_OBJC_INTEROP  
  explicit String(NSString *s)
    // FIXME: Use the usual NSString bridging entry point.
    : String([s UTF8String])
  {}
#endif
};
  
struct Array {
  // Keep the details of Array's implementation opaque to the runtime.
  const void *x;
};
  
struct PlaygroundQuickLook {
  struct RawData {
    Array Data;
    String Type;
  };
  struct Rectangle {
    double x, y, w, h;
  };
  struct Point {
    double x, y;
  };
  struct Interval {
    int64_t loc,len;
  };
  
  union {
    String TextOrURL;
    int64_t Int;
    uint64_t UInt;
    float Float;
    double Double;
    Any Any;
    RawData Raw;
    Rectangle Rect;
    Point PointOrSize;
    bool Logical;
    Interval Range;
  };
  enum class Tag : uint8_t {
    Text,
    Int,
    UInt,
    Float,
    Double,
    Image,
    Sound,
    Color,
    BezierPath,
    AttributedString,
    Rectangle,
    Point,
    Size,
    Logical,
    Range,
    View,
    Sprite,
    URL,
    Raw,
  } Kind;
};
  
struct StringMirrorTuple {
  String first;
  Mirror second;
};
struct OptionalPlaygroundQuickLook {
  union {
    struct {
      union {
        String TextOrURL;
        int64_t Int;
        uint64_t UInt;
        float Float;
        double Double;
        Any Any;
        PlaygroundQuickLook::RawData Raw;
        PlaygroundQuickLook::Rectangle Rect;
        PlaygroundQuickLook::Point PointOrSize;
        bool Logical;
        PlaygroundQuickLook::Interval Range;
      };
      PlaygroundQuickLook::Tag Kind;
      bool isNone;
    } optional;
    PlaygroundQuickLook payload;
  };
};
  
/// A Mirror witness table for use by MagicMirror.
struct MirrorWitnessTable;
  
/// The protocol descriptor for _Reflectable from the stdlib.
extern "C" const ProtocolDescriptor _TMps12_Reflectable;
  
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
/// _MirrorType protocol.
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
  

extern "C"
AnyReturn swift_MagicMirrorData_value(HeapObject *owner,
                                      const OpaqueValue *value,
                                      const Metadata *type) {
  Any result;
  
  result.Type = type;
  type->vw_initializeBufferWithCopy(&result.Buffer,
                                    const_cast<OpaqueValue*>(value));

  return AnyReturn(result);
}
extern "C"
const Metadata *swift_MagicMirrorData_valueType(HeapObject *owner,
                                                const OpaqueValue *value,
                                                const Metadata *type) {
  return swift_getDynamicType(const_cast<OpaqueValue*>(value), type);
}

#if SWIFT_OBJC_INTEROP
extern "C"
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

extern "C"
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
      new (result) String("(ErrorType Object)");
      break;
  }
}

  
extern "C"
const Metadata *swift_MagicMirrorData_objcValueType(HeapObject *owner,
                                                    const OpaqueValue *value,
                                                    const Metadata *type) {
  void *object = *reinterpret_cast<void * const *>(value);
  auto isa = _swift_getClass(object);
  return swift_getObjCClassMetadata(isa);
}
  
// -- Tuple destructuring.
  
extern "C"
intptr_t swift_TupleMirror_count(HeapObject *owner,
                                 const OpaqueValue *value,
                                 const Metadata *type) {
  auto Tuple = static_cast<const TupleTypeMetadata *>(type);
  return Tuple->NumElements;
}
  
static std::tuple<const _ReflectableWitnessTable *, const Metadata *,
                  const OpaqueValue *>
getReflectableConformance(const Metadata *T, const OpaqueValue *Value) {
recur:
  // If the value is an existential container, look through it to reflect the
  // contained value.
  switch (T->getKind()) {
  case MetadataKind::Tuple:
  case MetadataKind::Struct:
  case MetadataKind::ForeignClass:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::Class:
  case MetadataKind::Opaque:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Function:
  case MetadataKind::Metatype:
    break;
      
  case MetadataKind::Existential: {
    auto existential
      = static_cast<const ExistentialTypeMetadata *>(T);
    
    // If the existential happens to include the _Reflectable protocol, use
    // the witness table from the container.
    unsigned wtOffset = 0;
    for (unsigned i = 0; i < existential->Protocols.NumProtocols; ++i) {
      if (existential->Protocols[i] == &_TMps12_Reflectable) {
        return std::make_tuple(
            reinterpret_cast<const _ReflectableWitnessTable*>(
              existential->getWitnessTable(Value, wtOffset)),
            existential->getDynamicType(Value),
            existential->projectValue(Value));
      }
      if (existential->Protocols[i]->Flags.needsWitnessTable())
        ++wtOffset;
    }
    
    // Otherwise, unwrap the existential container and do a runtime lookup on
    // its contained value as usual.
    T = existential->getDynamicType(Value);
    Value = existential->projectValue(Value);

    // Existential containers can end up nested in some cases due to generic
    // abstraction barriers. Recur in case we have a nested existential.
    goto recur;
  }
  case MetadataKind::ExistentialMetatype:
    // TODO: Should look through existential metatypes too, but it doesn't
    // really matter yet since we don't have any special mirror behavior for
    // concrete metatypes yet.
    break;
      
  // Types can't have these kinds.
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
    swift::crash("Swift mirror lookup failure");
  }
  
  return std::make_tuple(
      reinterpret_cast<const _ReflectableWitnessTable*>(
        swift_conformsToProtocol(T, &_TMps12_Reflectable)),
      T,
      Value);
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
  const _ReflectableWitnessTable *witness;
  const Metadata *mirrorType;
  const OpaqueValue *mirrorValue;
  std::tie(witness, mirrorType, mirrorValue)
    = getReflectableConformance(T, value);
  
  // Use the _Reflectable conformance if the object has one.
  if (witness) {
    auto result =
    witness->getMirror(const_cast<OpaqueValue*>(mirrorValue), mirrorType);
    swift_release(owner);
    return MirrorReturn(result);
  }
  // Otherwise, fall back to MagicMirror.
  // Consumes 'owner'.
  Mirror result;
  ::new (&result) MagicMirror(owner, mirrorValue, mirrorType);
  return result;
}
  
/// \param owner passed at +1, consumed.
/// \param value passed unowned.
extern "C"
StringMirrorTuple swift_TupleMirror_subscript(intptr_t i,
                                              HeapObject *owner,
                                              const OpaqueValue *value,
                                              const Metadata *type) {
  StringMirrorTuple result;
  
  auto Tuple = static_cast<const TupleTypeMetadata *>(type);
  
  if (i < 0 || (size_t)i > Tuple->NumElements)
    swift::crash("Swift mirror subscript bounds check failure");
  
  // The name is the stringized element number '.0'.
  char buf[32];
  snprintf(buf, 31, ".%zd", i);
  buf[31] = 0;
  result.first = String(buf, strlen(buf));
  
  // Get a Mirror for the nth element.
  auto &elt = Tuple->getElement(i);
  auto bytes = reinterpret_cast<const char*>(value);
  auto eltData = reinterpret_cast<const OpaqueValue *>(bytes + elt.Offset);

  // This retain matches the -1 in reflect.
  swift_retain(owner);

  // 'owner' is consumed by this call.
  result.second = reflect(owner, eltData, elt.Type);

  return result;
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

// -- Struct destructuring.
  
extern "C"
intptr_t swift_StructMirror_count(HeapObject *owner,
                                  const OpaqueValue *value,
                                  const Metadata *type) {
  auto Struct = static_cast<const StructMetadata *>(type);
  return Struct->Description->Struct.NumFields;
}

extern "C"
StringMirrorTuple swift_StructMirror_subscript(intptr_t i,
                                               HeapObject *owner,
                                               const OpaqueValue *value,
                                               const Metadata *type) {
  StringMirrorTuple result;
  
  auto Struct = static_cast<const StructMetadata *>(type);
  
  if (i < 0 || (size_t)i > Struct->Description->Struct.NumFields)
    swift::crash("Swift mirror subscript bounds check failure");
  
  // Load the type and offset from their respective vectors.
  auto fieldType = Struct->getFieldTypes()[i];
  auto fieldOffset = Struct->getFieldOffsets()[i];
  
  auto bytes = reinterpret_cast<const char*>(value);
  auto fieldData = reinterpret_cast<const OpaqueValue *>(bytes + fieldOffset);

  result.first = String(getFieldName(Struct->Description->Struct.FieldNames, i));

  // This matches the -1 in reflect.
  swift_retain(owner);

  // 'owner' is consumed by this call.
  assert(!fieldType.isIndirect() && "indirect struct fields not implemented");
  result.second = reflect(owner, fieldData,
                                         fieldType.getType());

  return result;
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

  unsigned tag = type->vw_getEnumTag(value);
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

extern "C"
const char *swift_EnumMirror_caseName(HeapObject *owner,
                                      const OpaqueValue *value,
                                      const Metadata *type) {
  if (!isEnumReflectable(type))
    return nullptr;

  const auto Enum = static_cast<const EnumMetadata *>(type);
  const auto &Description = Enum->Description->Enum;

  unsigned tag;
  getEnumMirrorInfo(value, type, &tag, nullptr, nullptr);
  return getFieldName(Description.CaseNames, tag);
}

extern "C"
intptr_t swift_EnumMirror_count(HeapObject *owner,
                                const OpaqueValue *value,
                                const Metadata *type) {
  if (!isEnumReflectable(type))
    return 0;

  const Metadata *payloadType;
  getEnumMirrorInfo(value, type, nullptr, &payloadType, nullptr);
  return (payloadType != nullptr) ? 1 : 0;
}

extern "C"
StringMirrorTuple swift_EnumMirror_subscript(intptr_t i,
                                             HeapObject *owner,
                                             const OpaqueValue *value,
                                             const Metadata *type) {
  StringMirrorTuple result;

  const auto Enum = static_cast<const EnumMetadata *>(type);
  const auto &Description = Enum->Description->Enum;

  unsigned tag;
  const Metadata *payloadType;
  bool indirect;

  getEnumMirrorInfo(value, type, &tag, &payloadType, &indirect);

  // Copy the payload since the projection is destructive.
  BoxPair pair = swift_allocBox(type);

  owner = pair.first;
  type->vw_initializeWithTake(pair.second, const_cast<OpaqueValue *>(value));
  type->vw_destructiveProjectEnumData(pair.second);
  value = pair.second;

  // If the payload is indirect, we need to jump through the box to get it.
  if (indirect) {
    owner = *reinterpret_cast<HeapObject * const *>(value);
    value = swift_projectBox(const_cast<HeapObject *>(owner));
  }

  // This matches the -1 in reflect.
  swift_retain(owner);

  result.first = String(getFieldName(Description.CaseNames, tag));
  result.second = reflect(owner, value, payloadType);

  return result;
}
  
// -- Class destructuring.
static Mirror getMirrorForSuperclass(const ClassMetadata *sup,
                                     HeapObject *owner,
                                     const OpaqueValue *value,
                                     const Metadata *type);

extern "C"
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
extern "C"
StringMirrorTuple swift_ClassMirror_subscript(intptr_t i,
                                              HeapObject *owner,
                                              const OpaqueValue *value,
                                              const Metadata *type) {
  StringMirrorTuple result;
  
  auto Clas = static_cast<const ClassMetadata*>(type);

  if (classHasSuperclass(Clas)) {
    // If the class has a superclass, the superclass instance is treated as the
    // first child.
    if (i == 0) {
      // FIXME: Put superclass name here
      result.first = String("super");
      result.second
        = getMirrorForSuperclass(Clas->SuperClass, owner, value, type);
      return result;
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
  
  auto bytes = *reinterpret_cast<const char * const*>(value);
  auto fieldData = reinterpret_cast<const OpaqueValue *>(bytes + fieldOffset);
  
  result.first = String(getFieldName(Clas->getDescription()->Class.FieldNames, i));
  // 'owner' is consumed by this call.
  result.second = reflect(owner, fieldData, fieldType.getType());
  return result;
}
  
// -- Mirror witnesses for ObjC classes.

#if SWIFT_OBJC_INTEROP  

extern "C" const Metadata _TMSb; // Bool
extern "C" const Metadata _TMSi; // Int
extern "C" const Metadata _TMSu; // UInt
extern "C" const Metadata _TMSf; // Float
extern "C" const Metadata _TMSd; // Double
extern "C" const Metadata _TMVs4Int8;
extern "C" const Metadata _TMVs5Int16;
extern "C" const Metadata _TMVs5Int32;
extern "C" const Metadata _TMVs5Int64;
extern "C" const Metadata _TMVs5UInt8;
extern "C" const Metadata _TMVs6UInt16;
extern "C" const Metadata _TMVs6UInt32;
extern "C" const Metadata _TMVs6UInt64;
  
// Set to 1 to enable reflection of objc ivars.
#define REFLECT_OBJC_IVARS 0
  
/// Map an ObjC type encoding string to a Swift type metadata object.
///
#if REFLECT_OBJC_IVARS
static const Metadata *getMetadataForEncoding(const char *encoding) {
  switch (*encoding) {
  case 'c': // char
    return &_TMVs4Int8;
  case 's': // short
    return &_TMVs5Int16;
  case 'i': // int
    return &_TMVs5Int32;
  case 'l': // long
    return &_TMSi;
  case 'q': // long long
    return &_TMVs5Int64;
      
  case 'C': // unsigned char
    return &_TMVs5UInt8;
  case 'S': // unsigned short
    return &_TMVs6UInt16;
  case 'I': // unsigned int
    return &_TMVs6UInt32;
  case 'L': // unsigned long
    return &_TMSu;
  case 'Q': // unsigned long long
    return &_TMVs6UInt64;
      
  case 'B': // _Bool
    return &_TMSb;
      
  case '@': { // Class
    // TODO: Better metadata?
    const OpaqueMetadata *M = &_TMBO;
    return &M->base;
  }
      
  default: // TODO
    // Return 'void' as the type of fields we don't understand.
    return &_TMT_;
  }
}
#endif

/// \param owner passed at +1, consumed.
/// \param value passed unowned.
extern "C"
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
  
extern "C"
StringMirrorTuple swift_ObjCMirror_subscript(intptr_t i,
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
      StringMirrorTuple result;
      const char *supName = class_getName(sup);
      result.first = String(supName, strlen(supName));
      result.second = ObjC_getMirrorForSuperclass(sup, owner, value, type);
      return result;
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
  
  StringMirrorTuple result;
  result.first = String(name, strlen(name));
  // 'owner' is consumed by this call.
  result.second = reflect(owner, ivar, ivarType);
  return result;
#else
  // ObjC makes no guarantees about the state of ivars, so we can't safely
  // introspect them in the general case.
  abort();  
#endif
}

extern "C" OptionalPlaygroundQuickLook
swift_ClassMirror_quickLookObject(HeapObject *owner, const OpaqueValue *value,
                                  const Metadata *type) {
  OptionalPlaygroundQuickLook result;
  memset(&result, 0, sizeof(result));
  
  id object = [*reinterpret_cast<const id *>(value) retain];
  swift_release(owner);
  if ([object respondsToSelector:@selector(debugQuickLookObject)]) {
    id quickLookObject = [object debugQuickLookObject];
    [quickLookObject retain];
    [object release];
    object = quickLookObject;
  }
  
  // NSNumbers quick-look as integers or doubles, depending on type.
  if ([object isKindOfClass:[NSNumber class]]) {
    NSNumber *n = object;
    
    switch ([n objCType][0]) {
    case 'd': // double
      result.payload.Double = [n doubleValue];
      result.payload.Kind = PlaygroundQuickLook::Tag::Double;
      break;
    case 'f': // float
      result.payload.Float = [n floatValue];
      result.payload.Kind = PlaygroundQuickLook::Tag::Float;
      break;
        
    case 'Q': // unsigned long long
      result.payload.UInt = [n unsignedLongLongValue];
      result.payload.Kind = PlaygroundQuickLook::Tag::UInt;
      break;

    // FIXME: decimals?
    default:
      result.payload.Int = [n longLongValue];
      result.payload.Kind = PlaygroundQuickLook::Tag::Int;
      break;
    }
    
    [object release];
    result.optional.isNone = false;
    return result;
  }
  
  // Various other framework types are used for rich representations.
  
  /// Store an ObjC reference into an Any.
  auto initializeAnyWithTakeOfObject = [](Any &any, id obj) {
    any.Type = swift_getObjCClassMetadata(_swift_getClass((const void*) obj));
    *reinterpret_cast<id *>(&any.Buffer) = obj;
  };
  
  if ([object isKindOfClass:NSClassFromString(@"NSAttributedString")]) {
    initializeAnyWithTakeOfObject(result.payload.Any, object);
    result.payload.Kind = PlaygroundQuickLook::Tag::AttributedString;
    result.optional.isNone = false;
    return result;
  } else if ([object isKindOfClass:NSClassFromString(@"NSImage")]
      || [object isKindOfClass:NSClassFromString(@"UIImage")]
      || [object isKindOfClass:NSClassFromString(@"NSImageView")]
      || [object isKindOfClass:NSClassFromString(@"UIImageView")]
      || [object isKindOfClass:NSClassFromString(@"CIImage")]
      || [object isKindOfClass:NSClassFromString(@"NSBitmapImageRep")]) {
    initializeAnyWithTakeOfObject(result.payload.Any, object);
    result.payload.Kind = PlaygroundQuickLook::Tag::Image;
    result.optional.isNone = false;
    return result;
  } else if ([object isKindOfClass:NSClassFromString(@"NSColor")]
             || [object isKindOfClass:NSClassFromString(@"UIColor")]) {
    initializeAnyWithTakeOfObject(result.payload.Any, object);
    result.payload.Kind = PlaygroundQuickLook::Tag::Color;
    result.optional.isNone = false;
    return result;
  } else if ([object isKindOfClass:NSClassFromString(@"NSBezierPath")]
             || [object isKindOfClass:NSClassFromString(@"UIBezierPath")]) {
    initializeAnyWithTakeOfObject(result.payload.Any, object);
    result.payload.Kind = PlaygroundQuickLook::Tag::BezierPath;
    result.optional.isNone = false;
    return result;
  } else if ([object isKindOfClass:[NSString class]]) {
    result.payload.TextOrURL = String((NSString*)object);
    [object release];
    result.payload.Kind = PlaygroundQuickLook::Tag::Text;
    result.optional.isNone = false;
    return result;
  }
  
  // Return none if we didn't get a suitable object.
  [object release];
  result.optional.isNone = true;
  return result;
}
#endif
  
// -- MagicMirror implementation.

// TODO: There are other non-Apple platforms that underscore asm symbols.
#if defined(__APPLE__)
# define UNDERSCORE "_"
#else
# define UNDERSCORE
#endif

// Addresses of the type metadata and Mirror witness tables for the primitive
// mirrors.
extern "C" const Metadata OpaqueMirrorMetadata
  __asm__(UNDERSCORE "_TMVs13_OpaqueMirror");
extern "C" const MirrorWitnessTable OpaqueMirrorWitnessTable
  __asm__(UNDERSCORE "_TWPVs13_OpaqueMirrors11_MirrorTypes");
extern "C" const Metadata TupleMirrorMetadata
  __asm__(UNDERSCORE "_TMVs12_TupleMirror");
extern "C" const MirrorWitnessTable TupleMirrorWitnessTable
  __asm__(UNDERSCORE "_TWPVs12_TupleMirrors11_MirrorTypes");

extern "C" const Metadata StructMirrorMetadata
  __asm__(UNDERSCORE "_TMVs13_StructMirror");
extern "C" const MirrorWitnessTable StructMirrorWitnessTable
  __asm__(UNDERSCORE "_TWPVs13_StructMirrors11_MirrorTypes");

extern "C" const Metadata EnumMirrorMetadata
  __asm__(UNDERSCORE "_TMVs11_EnumMirror");
extern "C" const MirrorWitnessTable EnumMirrorWitnessTable
  __asm__(UNDERSCORE "_TWPVs11_EnumMirrors11_MirrorTypes");

extern "C" const Metadata ClassMirrorMetadata
  __asm__(UNDERSCORE "_TMVs12_ClassMirror");
extern "C" const MirrorWitnessTable ClassMirrorWitnessTable
  __asm__(UNDERSCORE "_TWPVs12_ClassMirrors11_MirrorTypes");

extern "C" const Metadata ClassSuperMirrorMetadata
  __asm__(UNDERSCORE "_TMVs17_ClassSuperMirror");
extern "C" const MirrorWitnessTable ClassSuperMirrorWitnessTable
  __asm__(UNDERSCORE "_TWPVs17_ClassSuperMirrors11_MirrorTypes");

extern "C" const Metadata MetatypeMirrorMetadata
  __asm__(UNDERSCORE "_TMVs15_MetatypeMirror");
extern "C" const MirrorWitnessTable MetatypeMirrorWitnessTable
  __asm__(UNDERSCORE "_TWPVs15_MetatypeMirrors11_MirrorTypes");
  
#if SWIFT_OBJC_INTEROP
extern "C" const Metadata ObjCMirrorMetadata
  __asm__(UNDERSCORE "_TMVs11_ObjCMirror");
extern "C" const MirrorWitnessTable ObjCMirrorWitnessTable
  __asm__(UNDERSCORE "_TWPVs11_ObjCMirrors11_MirrorTypes");
extern "C" const Metadata ObjCSuperMirrorMetadata
  __asm__(UNDERSCORE "_TMVs16_ObjCSuperMirror");
extern "C" const MirrorWitnessTable ObjCSuperMirrorWitnessTable
  __asm__(UNDERSCORE "_TWPVs16_ObjCSuperMirrors11_MirrorTypes");
#endif

#undef UNDERSCORE

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
  
  result->Self = &ClassSuperMirrorMetadata;
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
  
  result->Self = &ObjCSuperMirrorMetadata;
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
    return {isa, &ObjCMirrorMetadata, &ObjCMirrorWitnessTable};
#endif

  // Otherwise, use the native Swift facilities.
  return std::make_tuple(
      isa, &ClassMirrorMetadata, &ClassMirrorWitnessTable);
}
  
/// Get the magic mirror witnesses appropriate to a particular type.
static MirrorTriple
getImplementationForType(const Metadata *T, const OpaqueValue *Value) {
  switch (T->getKind()) {
  case MetadataKind::Tuple:
    return std::make_tuple(
        T, &TupleMirrorMetadata, &TupleMirrorWitnessTable);
      
  case MetadataKind::Struct:
    return std::make_tuple(
        T, &StructMirrorMetadata, &StructMirrorWitnessTable);
      
  case MetadataKind::Enum:
  case MetadataKind::Optional:
    return std::make_tuple(
        T, &EnumMirrorMetadata, &EnumMirrorWitnessTable);

  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass:
  case MetadataKind::Class: {
    return getImplementationForClass(Value);
  }
      
  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype: {
    return std::make_tuple(T, &MetatypeMirrorMetadata,
                           &MetatypeMirrorWitnessTable);
  }
      
  case MetadataKind::Opaque: {
#if SWIFT_OBJC_INTEROP
    // If this is the Builtin.UnknownObject type, use the dynamic type of the
    // object reference.
    if (T == &_TMBO.base) {
      return getImplementationForClass(Value);
    }
#endif
    // If this is the Builtin.NativeObject type, and the heap object is a
    // class instance, use the dynamic type of the object reference.
    if (T == &_TMBo.base) {
      const HeapObject *obj
        = *reinterpret_cast<const HeapObject * const*>(Value);
      if (obj->metadata->getKind() == MetadataKind::Class)
        return getImplementationForClass(Value);
    }
    SWIFT_FALLTHROUGH;
  }
    
  /// TODO: Implement specialized mirror witnesses for all kinds.
  case MetadataKind::Function:
  case MetadataKind::Existential:
    return std::make_tuple(
        T, &OpaqueMirrorMetadata, &OpaqueMirrorWitnessTable);
      
  // Types can't have these kinds.
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
    swift::crash("Swift mirror lookup failure");
  }
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
/// Produce a mirror for any value. If the value's type conforms to _Reflectable,
/// invoke its _getMirror() method; otherwise, fall back to an implementation
/// in the runtime that structurally reflects values of any type.
///
/// This function consumes 'value', following Swift's +1 convention for "in"
/// arguments.
MirrorReturn swift::swift_reflectAny(OpaqueValue *value, const Metadata *T) {
  const _ReflectableWitnessTable *witness;
  const Metadata *mirrorType;
  const OpaqueValue *cMirrorValue;
  std::tie(witness, mirrorType, cMirrorValue)
    = getReflectableConformance(T, value);
  
  OpaqueValue *mirrorValue = const_cast<OpaqueValue*>(cMirrorValue);
  
  // Use the _Reflectable conformance if the object has one.
  if (witness) {
    auto result = witness->getMirror(mirrorValue, mirrorType);
    // 'self' of witnesses is passed at +0, so we still need to consume the
    // value.
    T->vw_destroy(value);
    return MirrorReturn(result);
  }
  
  // Otherwise, fall back to MagicMirror.
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
// exported for Xcode support. Please coordinate before changing.
extern "C" void swift_stdlib_demangleName(const char *mangledName,
                                          size_t mangledNameLength,
                                          String *demangledName) {
  auto options = Demangle::DemangleOptions();
  options.DisplayDebuggerGeneratedModule = false;
  auto result =
      Demangle::demangleSymbolAsString(mangledName,
                                       mangledNameLength,
                                       options);
  new (demangledName) String(result.data(), result.size());
}
