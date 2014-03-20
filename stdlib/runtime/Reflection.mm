//===----------------------------------------------------------------------===//
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

#include "swift/Basic/Fallthrough.h"
#include "swift/Runtime/Reflection.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include <cassert>
#include <cstring>
#include <new>
#include <string>
#include <Foundation/Foundation.h>
#include <dlfcn.h>
#include <objc/objc.h>
#include <objc/runtime.h>

using namespace swift;

// Declare the debugQuickLookObject selector.
@protocol DeclareQuickLookObject

- (id)debugQuickLookObject;

@end


namespace {
  
/// The layout of protocol<>.
struct Any {
  const Metadata *Self;
  ValueBuffer Value;
};
  
/// A Reflectable witness table.
struct ReflectableWitnessTable {
  /// func getMirror() -> Mirror
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
  
  explicit String(NSString *s)
    // FIXME: Use the usual NSString bridging entry point.
    : String([s UTF8String])
  {}
};
  
struct Array {
  // Keep the details of Array's implementation opaque to the runtime.
  const void *x, *y, *z;
};
  
struct QuickLookObject {
  struct RawData {
    Array Data;
    String Type;
  };
  
  union {
    String Text;
    int64_t Int;
    uint64_t UInt;
    double Float;
    Any Any;
    RawData Raw;
  };
  enum class Tag : uint8_t {
    Text,
    Int,
    UInt,
    Float,
    Image,
    Sound,
    Color,
    BezierPath,
    Raw,
  } Kind;
};
  
struct StringMirrorTuple {
  String first;
  Mirror second;
};
struct OptionalQuickLookObject {
  union {
    struct {
      union {
        String Text;
        int64_t Int;
        uint64_t UInt;
        double Float;
        Any Any;
        QuickLookObject::RawData Raw;
      };
      QuickLookObject::Tag Kind;
      bool isNone;
    } optional;
    QuickLookObject payload;
  };
};
  
/// The enumeration of mirror dispositions.
enum class MirrorDisposition : bool {
  Aggregate,
  Container,
};
  
/// A Mirror witness table for use by MagicMirror.
struct MirrorWitnessTable;
  
/// The protocol descriptor for Reflectable from the stdlib.
extern "C" const ProtocolDescriptor _TMpSs11Reflectable;
  
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
  
/// A magic implementation of Mirror that can^Wwill be able to look at runtime
/// metadata to walk an arbitrary object.
///
/// This type is layout-compatible with a Swift existential
/// container for the Mirror protocol.
class MagicMirror {
public:
  // The existential header.
  const Metadata *Self;
  const MirrorWitnessTable *MirrorWitness;
  
  // The data for the mirror.
  MagicMirrorData Data;
  
  MagicMirror() = default;
  
  /// Build a new MagicMirror for type T by taking ownership of the referenced
  /// value.
  MagicMirror(OpaqueValue *value, const Metadata *T);
  
  /// Build a new MagicMirror for type T, sharing ownership with an existing
  /// heap object, which is retained.
  MagicMirror(HeapObject *owner, const OpaqueValue *value, const Metadata *T);
};
    
static_assert(alignof(MagicMirror) == alignof(Mirror),
              "MagicMirror layout does not match existential container");
static_assert(sizeof(MagicMirror) == sizeof(Mirror),
              "MagicMirror layout does not match existential container");
  
// -- Build an Any from an arbitrary value unowned-referenced by a mirror.
  
extern "C"
Any swift_MagicMirrorData_value(HeapObject *owner,
                                const OpaqueValue *value,
                                const Metadata *type) {
  Any result;
  
  result.Self = type;
  type->vw_initializeBufferWithCopy(&result.Value,
                                    const_cast<OpaqueValue*>(value));
  
  swift_release(owner);
  
  return result;
}
extern "C"
const Metadata *swift_MagicMirrorData_valueType(HeapObject *owner,
                                                const OpaqueValue *value,
                                                const Metadata *type) {
  auto r = type->vw_typeOf(const_cast<OpaqueValue*>(value));
  swift_release(owner);
  return r;
}
  
extern "C"
Any swift_MagicMirrorData_objcValue(HeapObject *owner,
                                    const OpaqueValue *value,
                                    const Metadata *type) {
  Any result;
    
  id object = *reinterpret_cast<const id *>(value);
  auto isa = reinterpret_cast<const ClassMetadata *>(object_getClass(object));
  result.Self = swift_getObjCClassMetadata(isa);
  *reinterpret_cast<id *>(&result.Value) = [object retain];
  swift_release(owner);
  return result;
}
  
extern "C"
const Metadata *swift_MagicMirrorData_objcValueType(HeapObject *owner,
                                                    const OpaqueValue *value,
                                                    const Metadata *type) {
  id object = *reinterpret_cast<const id *>(value);
  auto isa = reinterpret_cast<const ClassMetadata *>(object_getClass(object));
  swift_release(owner);
  return swift_getObjCClassMetadata(isa);
}
  
// -- Tuple destructuring.
  
extern "C"
intptr_t swift_TupleMirror_count(HeapObject *owner,
                                 const OpaqueValue *value,
                                 const Metadata *type) {
  auto Tuple = static_cast<const TupleTypeMetadata *>(type);
  swift_release(owner);
  return Tuple->NumElements;
}
  
extern "C"
StringMirrorTuple swift_TupleMirror_subscript(intptr_t i,
                                              HeapObject *owner,
                                              const OpaqueValue *value,
                                              const Metadata *type) {
  StringMirrorTuple result;
  
  auto Tuple = static_cast<const TupleTypeMetadata *>(type);
  
  if (i < 0 || (size_t)i > Tuple->NumElements)
    abort();
  
  // The name is the stringized element number '.0'.
  char buf[32];
  snprintf(buf, 31, ".%zd", i);
  buf[31] = 0;
  result.first = String(buf, strlen(buf));
  
  // Get a Mirror for the nth element.
  auto &elt = Tuple->getElements()[i];
  auto bytes = reinterpret_cast<const char*>(value);
  auto eltData = reinterpret_cast<const OpaqueValue *>(bytes + elt.Offset);
  
  result.second = swift_unsafeReflectAny(owner, eltData, elt.Type);
  return result;
}
  
// -- Struct destructuring.
  
extern "C"
intptr_t swift_StructMirror_count(HeapObject *owner,
                                  const OpaqueValue *value,
                                  const Metadata *type) {
  auto Struct = static_cast<const StructMetadata *>(type);
  swift_release(owner);
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
    abort();
  
  // Load the type and offset from their respective vectors.
  auto fieldType = Struct->getFieldTypes()[i];
  auto fieldOffset = Struct->getFieldOffsets()[i];
  
  auto bytes = reinterpret_cast<const char*>(value);
  auto fieldData = reinterpret_cast<const OpaqueValue *>(bytes + fieldOffset);
  
  // Get the field name from the doubly-null-terminated list.
  const char *fieldName = Struct->Description->Struct.FieldNames;
  for (unsigned j = 0; j < i; ++j) {
    while (*fieldName++);
  }
  
  result.first = String(fieldName);
  result.second = swift_unsafeReflectAny(owner, fieldData, fieldType);
  return result;
}
  
// -- Mirror witnesses for ObjC classes.
  
extern "C" const FullMetadata<Metadata> _TMdSb; // Bool
extern "C" const FullMetadata<Metadata> _TMdSi; // Int
extern "C" const FullMetadata<Metadata> _TMdSu; // UInt
extern "C" const FullMetadata<Metadata> _TMdSf; // Float
extern "C" const FullMetadata<Metadata> _TMdSd; // Double
extern "C" const FullMetadata<Metadata> _TMdVSs4Int8;
extern "C" const FullMetadata<Metadata> _TMdVSs5Int16;
extern "C" const FullMetadata<Metadata> _TMdVSs5Int32;
extern "C" const FullMetadata<Metadata> _TMdVSs5Int64;
extern "C" const FullMetadata<Metadata> _TMdVSs5UInt8;
extern "C" const FullMetadata<Metadata> _TMdVSs6UInt16;
extern "C" const FullMetadata<Metadata> _TMdVSs6UInt32;
extern "C" const FullMetadata<Metadata> _TMdVSs6UInt64;
  
/// Map an ObjC type encoding string to a Swift type metadata object.
static const Metadata *getMetadataForEncoding(const char *encoding) {
  switch (*encoding) {
  case 'c': // char
    return &_TMdVSs4Int8;
  case 's': // short
    return &_TMdVSs5Int16;
  case 'i': // int
    return &_TMdVSs5Int32;
  case 'l': // long
    return &_TMdSi;
  case 'q': // long long
    return &_TMdVSs5Int64;
      
  case 'C': // unsigned char
    return &_TMdVSs5UInt8;
  case 'S': // unsigned short
    return &_TMdVSs6UInt16;
  case 'I': // unsigned int
    return &_TMdVSs6UInt32;
  case 'L': // unsigned long
    return &_TMdSu;
  case 'Q': // unsigned long long
    return &_TMdVSs6UInt64;
      
  case 'B': // _Bool
    return &_TMdSb;
      
  case '@': { // Class
    // TODO: Better metadata?
    const OpaqueMetadata *M = &_TMdBO;
    return &M->base;
  }
      
  default: // TODO
    // Return 'void' as the type of fields we don't understand.
    return &_TMdT_;
  }
}
  
extern "C"
intptr_t swift_ObjCMirror_count(HeapObject *owner,
                                const OpaqueValue *value,
                                const Metadata *type) {
  // Copying the ivar list just to free it is lame, but we have
  // nowhere to save it.
  auto isa = (Class)type;
  
  unsigned count;
  Ivar *ivars = class_copyIvarList(isa, &count);
  free(ivars);
  
  // The superobject counts as a subobject.
  if (class_getSuperclass(isa))
    count += 1;
  
  swift_release(owner);
  return count;
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
  id object = *reinterpret_cast<const id *>(value);
  auto isa = (Class)type;
  
  // If there's a superclass, it becomes the first child.
  if (auto sup = class_getSuperclass(isa)) {
    if (i == 0) {
      StringMirrorTuple result;
      const char *supName = class_getName(sup);
      result.first = String(supName, strlen(supName));
      result.second = ObjC_getMirrorForSuperclass(sup, owner, value, type);
      return result;
    }
    --i;
  }
  
  // Copying the ivar list just to free it is lame, but we have
  // no room to save it.
  unsigned count;
  Ivar *ivars = class_copyIvarList(isa, &count);
  
  if (i < 0 || (uintptr_t)i >= (uintptr_t)count)
    abort();
  
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
  result.second = swift_unsafeReflectAny(owner, ivar, ivarType);
  return result;
}
  
extern "C"
OptionalQuickLookObject swift_ObjCMirror_quickLookObject(HeapObject *owner,
                                                       const OpaqueValue *value,
                                                       const Metadata *type) {
  OptionalQuickLookObject result;
  memset(&result, 0, sizeof(result));
  
  id object = [*reinterpret_cast<const id *>(value) retain];
  swift_release(owner);
  if ([object respondsToSelector:@selector(debugQuickLookObject)])
    object = [object debugQuickLookObject];
  
  // NSStrings quick-look as text.
  if ([object isKindOfClass:[NSString class]]) {
    result.payload.Text = String((NSString*)object);
    result.payload.Kind = QuickLookObject::Tag::Text;
    result.optional.isNone = false;
    return result;
  }
  
  // NSNumbers quick-look as integers or doubles, depending on type.
  if ([object isKindOfClass:[NSNumber class]]) {
    NSNumber *n = object;
    
    switch ([n objCType][0]) {
    case 'd': // double
    case 'f': // float
      result.payload.Float = [n doubleValue];
      result.payload.Kind = QuickLookObject::Tag::Float;
      break;
        
    case 'Q': // unsigned long long
      result.payload.UInt = [n unsignedLongLongValue];
      result.payload.Kind = QuickLookObject::Tag::UInt;
      break;

    // FIXME: decimals?
    default:
      result.payload.Int = [n longLongValue];
      result.payload.Kind = QuickLookObject::Tag::Int;
      break;
    }
    
    result.optional.isNone = false;
    return result;
  }
  
  // Various other framework types are used for rich representations.
  
  /// Store an ObjC reference into an Any.
  auto setAnyToObject = [](Any &any, id obj) {
    any.Self = swift_getObjCClassMetadata(
                  reinterpret_cast<const ClassMetadata*>(object_getClass(obj)));
    *reinterpret_cast<id *>(&any.Value) = [obj retain];
  };
  
  if ([object isKindOfClass:NSClassFromString(@"NSImage")]
      || [object isKindOfClass:NSClassFromString(@"UIImage")]
      || [object isKindOfClass:NSClassFromString(@"NSImageView")]
      || [object isKindOfClass:NSClassFromString(@"UIImageView")]
      || [object isKindOfClass:NSClassFromString(@"CIImage")]
      || [object isKindOfClass:NSClassFromString(@"NSBitmapImageRep")]) {
    setAnyToObject(result.payload.Any, object);
    result.payload.Kind = QuickLookObject::Tag::Image;
    result.optional.isNone = false;
    return result;
  } else if ([object isKindOfClass:NSClassFromString(@"NSColor")]
             || [object isKindOfClass:NSClassFromString(@"UIColor")]) {
    setAnyToObject(result.payload.Any, object);
    result.payload.Kind = QuickLookObject::Tag::Color;
    result.optional.isNone = false;
    return result;
  } else if ([object isKindOfClass:NSClassFromString(@"NSBezierPath")]
             || [object isKindOfClass:NSClassFromString(@"UIBezierPath")]) {
    setAnyToObject(result.payload.Any, object);
    result.payload.Kind = QuickLookObject::Tag::BezierPath;
    result.optional.isNone = false;
    return result;
  }
  
  // Return none if we didn't get a suitable object.
  result.optional.isNone = true;
  return result;
}
  
// -- MagicMirror implementation.
  
// Addresses of the type metadata and Mirror witness tables for the primitive
// mirrors.
extern "C" const FullMetadata<Metadata> _TMdVSs13_OpaqueMirror;
extern "C" const MirrorWitnessTable _TWPVSs13_OpaqueMirrorSs6MirrorSs;
extern "C" const FullMetadata<Metadata> _TMdVSs12_TupleMirror;
extern "C" const MirrorWitnessTable _TWPVSs12_TupleMirrorSs6MirrorSs;
extern "C" const FullMetadata<Metadata> _TMdVSs13_StructMirror;
extern "C" const MirrorWitnessTable _TWPVSs13_StructMirrorSs6MirrorSs;
extern "C" const FullMetadata<Metadata> _TMdVSs12_ClassMirror;
extern "C" const MirrorWitnessTable _TWPVSs12_ClassMirrorSs6MirrorSs;
  
// These type metadata objects are kept in swiftFoundation because they rely
// on string bridging being installed.
static const Metadata *getObjCMirrorMetadata() {
  static const Metadata *metadata = nullptr;
  if (!metadata)
    metadata = reinterpret_cast<const FullMetadata<Metadata>*>(
      dlsym(RTLD_DEFAULT, "_TMdV10Foundation11_ObjCMirror"));
  assert(metadata);
  return metadata;
}
static const MirrorWitnessTable *getObjCMirrorWitness() {
  static const MirrorWitnessTable *witness = nullptr;
  if (!witness)
    witness = reinterpret_cast<const MirrorWitnessTable*>(
      dlsym(RTLD_DEFAULT, "_TWPV10Foundation11_ObjCMirrorSs6MirrorS_"));
  assert(witness);
  return witness;
}
static const Metadata *getObjCSuperMirrorMetadata() {
  static const Metadata *metadata = nullptr;
  if (!metadata)
    metadata = reinterpret_cast<const FullMetadata<Metadata>*>(
      dlsym(RTLD_DEFAULT, "_TMdV10Foundation16_ObjCSuperMirror"));
  assert(metadata);
  return metadata;
}
static const MirrorWitnessTable *getObjCSuperMirrorWitness() {
  static const MirrorWitnessTable *witness = nullptr;
  if (!witness)
    witness = reinterpret_cast<const MirrorWitnessTable*>(
      dlsym(RTLD_DEFAULT, "_TWPV10Foundation16_ObjCSuperMirrorSs6MirrorS_"));
  assert(witness);
  
  return witness;
}
  
static Mirror ObjC_getMirrorForSuperclass(Class sup,
                                          HeapObject *owner,
                                          const OpaqueValue *value,
                                          const Metadata *type) {
  Mirror resultBuf;
  MagicMirror *result = ::new (&resultBuf) MagicMirror;
  
  result->Self = getObjCSuperMirrorMetadata();
  result->MirrorWitness = getObjCSuperMirrorWitness();
  result->Data.Owner = owner;
  result->Data.Type = reinterpret_cast<ClassMetadata*>(sup);
  result->Data.Value = value;
  return resultBuf;
}
  
// (type being mirrored, mirror type, mirror witness)
using MirrorTriple
  = std::tuple<const Metadata *, const Metadata *, const MirrorWitnessTable *>;
  
static MirrorTriple
getImplementationForClass(const OpaqueValue *Value) {
  // Get the runtime type of the object.
  id obj = *reinterpret_cast<const id *>(Value);
  auto isa = reinterpret_cast<const ClassMetadata*>(object_getClass(obj));
  
  // If this is a pure ObjC class, reflect it using ObjC's runtime facilities.
  if (!isa->isTypeMetadata())
    return {isa, getObjCMirrorMetadata(), getObjCMirrorWitness()};
  
  // Otherwise, use the (currently nonexistent) native Swift facilities.
  return {isa, &_TMdVSs12_ClassMirror, &_TWPVSs12_ClassMirrorSs6MirrorSs};
}
  
/// Get the magic mirror witnesses appropriate to a particular type.
static MirrorTriple
getImplementationForType(const Metadata *T, const OpaqueValue *Value) {
  switch (T->getKind()) {
  case MetadataKind::Tuple:
    return {T, &_TMdVSs12_TupleMirror, &_TWPVSs12_TupleMirrorSs6MirrorSs};
      
  case MetadataKind::Struct:
    return {T, &_TMdVSs13_StructMirror, &_TWPVSs13_StructMirrorSs6MirrorSs};
      
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::Class: {
    return getImplementationForClass(Value);
  }
      
  case MetadataKind::Opaque: {
    // If this is the Builtin.ObjCPointer type, use the dynamic type of the
    // object reference.
    if (T == &_TMdBO.base) {
      return getImplementationForClass(Value);
    }
    
    // If this is the Builtin.ObjectPointer type, and the heap object is a
    // class instance, use the dynamic type of the object reference.
    if (T == &_TMdBo.base) {
      const HeapObject *obj
        = *reinterpret_cast<const HeapObject * const*>(Value);
      if (obj->metadata->getKind() == MetadataKind::Class)
        return getImplementationForClass(Value);
    }
    SWIFT_FALLTHROUGH;
  }
      
  /// TODO: Implement specialized mirror witnesses for all kinds.
  case MetadataKind::Enum:
  case MetadataKind::Function:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
    return {T, &_TMdVSs13_OpaqueMirror, &_TWPVSs13_OpaqueMirrorSs6MirrorSs};
      
  // Types can't have these kinds.
  case MetadataKind::PolyFunction:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapArray:
    abort();
  }
}
  
/// MagicMirror ownership-taking whole-value constructor.
MagicMirror::MagicMirror(OpaqueValue *value, const Metadata *T) {
  // Put value types into a box so we can take stable interior pointers.
  // TODO: Specialize behavior here. If the value is a swift-refcounted class
  // we don't need to put it in a box to point into it.
  BoxPair box = swift_allocBox(T);
  
  T->vw_initializeWithTake(box.value, value);
  std::tie(T, Self, MirrorWitness) = getImplementationForType(T, value);
  
  Data = {box.heapObject, box.value, T};
}
  
/// MagicMirror ownership-sharing subvalue constructor.
MagicMirror::MagicMirror(HeapObject *owner,
                         const OpaqueValue *value, const Metadata *T) {
  std::tie(T, Self, MirrorWitness) = getImplementationForType(T, value);
  Data = {owner, value, T};
}
  
static const ReflectableWitnessTable *
getReflectableConformance(const Metadata *T) {
  return reinterpret_cast<const ReflectableWitnessTable*>(
    swift_conformsToProtocol(T, &_TMpSs11Reflectable, nullptr));
}
  
} // end anonymous namespace

/// func reflect<T>(x: T) -> Mirror
///
/// Produce a mirror for any value. If the value's type conforms to Reflectable,
/// invoke its getMirror() method; otherwise, fall back to an implementation
/// in the runtime that structurally reflects values of any type.
///
/// This function consumes 'value', following Swift's +1 convention for in
/// arguments.
Mirror swift::swift_reflectAny(OpaqueValue *value, const Metadata *T) {
  auto witness = getReflectableConformance(T);
                                                                  
  // Use the Reflectable conformance if the object has one.
  if (witness) {
    auto result = witness->getMirror(value, T);
    // 'self' of witnesses is passed at +0, so we still need to consume the
    // value.
    T->vw_destroy(value);
    return result;
  }
  
  // Otherwise, fall back to MagicMirror.
  {
    Mirror result;
    ::new (&result) MagicMirror(value, T);
    return result;
  }
}

/// Produce a mirror for any value, like swift_reflectAny, but do not consume
/// the value, so we can produce a mirror for a subobject of a value already
/// owned by a mirror.
Mirror swift::swift_unsafeReflectAny(HeapObject *owner,
                                     const OpaqueValue *value,
                                     const Metadata *T) {
  auto witness = getReflectableConformance(T);
  // Use the Reflectable conformance if the object has one.
  if (witness) {
    return witness->getMirror(const_cast<OpaqueValue*>(value), T);
  }
  // Otherwise, fall back to MagicMirror.
  Mirror result;
  ::new (&result) MagicMirror(owner, value, T);
  return result;
}
