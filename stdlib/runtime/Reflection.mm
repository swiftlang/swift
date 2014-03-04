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
#include <objc/objc.h>
#include <objc/runtime.h>

using namespace swift;

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
  
struct OptionalObjectIdentifier {
  const void *id;
  bool isNone;
};
  
struct String;
  
extern "C" void swift_stringFromUTF8InRawMemory(String *out,
                                                const char *start,
                                                intptr_t len);
  
struct String {
  const void *data;
  intptr_t countAndFlags;
  HeapObject *owner;
  
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
  
  String(const void *data, intptr_t countAndFlags, HeapObject *owner)
    : data(data), countAndFlags(countAndFlags), owner(owner) {
  }
};
  
struct StringMirrorTuple {
  String first;
  Mirror second;
};
struct OptionalIDERepresentable {
  // FIXME: Not the right existential type, but still the right layout.
  Mirror existential;
  bool isNone;
};
  
/// We can't return String accurately from C on some platforms, so use a Swift
/// thunk we can tail-call to do it for us.
using StringReturn = void;
extern "C" StringReturn swift_returnString(String *s);
  
/// A Mirror witness table for use by MagicMirror.
struct MirrorWitnessTable {
  /// var value: Any { get }
  Any (*getValue)(MagicMirrorData *self, const Metadata *Self);
  /// var valueType: Any.Type { get }
  const Metadata * (*getType)(MagicMirrorData *self, const Metadata *Self);
  
  /// var objectIdentifier: ObjectIdentifier? { get }
  OptionalObjectIdentifier (*getObjectIdentifier)(MagicMirrorData *self,
                                                  const Metadata *Self);
  /// var count: Int { get }
  intptr_t (*getCount)(MagicMirrorData *self, const Metadata *Self);
  
  /// subscript(Int) -> (String, Mirror) { get }
  StringMirrorTuple (*getChild)(intptr_t i, MagicMirrorData *self,
                                const Metadata *Self);
  /// var summary: String { get }
  StringReturn (*getString)(MagicMirrorData *self, const Metadata *Self);
  
  /// var IDERepresentation: IDERepresentable? { get }
  OptionalIDERepresentable (*getIDERepresentation)
    (MagicMirrorData *self, const Metadata *Self);
};
  
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
  
/// Type metadata for MagicMirrorData from the stdlib.
extern "C" const FullMetadata<StructMetadata> _TMdVSs16_MagicMirrorData;
  
/// A magic implementation of Mirror that can^Wwill be able to look at runtime
/// metadata to walk an arbitrary object.
///
/// This type is layout-compatible with a Swift existential
/// container for the Mirror protocol.
class MagicMirror {
public:
  // The existential header.
  const Metadata *Self = &_TMdVSs16_MagicMirrorData;
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
  
// -- Mirror witnesses for a type we don't know how to reflect.
  
Any Opaque_getValue(MagicMirrorData *self, const Metadata *Self) {
  Any result;
  
  auto T = self->Type;
  result.Self = T;
  T->vw_initializeBufferWithCopy(&result.Value,
                                 const_cast<OpaqueValue*>(self->Value));
  
  return result;
}
const Metadata *Opaque_getType(MagicMirrorData *self, const Metadata *Self) {
  return self->Type->vw_typeOf(const_cast<OpaqueValue*>(self->Value));
}
OptionalObjectIdentifier Opaque_getObjectIdentifier(MagicMirrorData *self,
                                                    const Metadata *Self) {
  return {nullptr, 1};
}
intptr_t Opaque_getCount(MagicMirrorData *self, const Metadata *Self) {
  return 0;
}
StringMirrorTuple Opaque_getChild(intptr_t i, MagicMirrorData *self,
                                  const Metadata *Self) {
  // unreachable
  abort();
}
StringReturn Opaque_getString(MagicMirrorData *self, const Metadata *Self) {
  String s("<something>");
  
  return swift_returnString(&s);
}
OptionalIDERepresentable Opaque_getIDERepresentation(MagicMirrorData *self,
                                                     const Metadata *Self) {
  OptionalIDERepresentable result;
  memset(&result.existential,
         0, sizeof(result.existential));
  result.isNone = true;
  return result;
}
static const MirrorWitnessTable OpaqueMirrorWitness{
  Opaque_getValue,
  Opaque_getType,
  Opaque_getObjectIdentifier,
  Opaque_getCount,
  Opaque_getChild,
  Opaque_getString,
  Opaque_getIDERepresentation,
};
  
// -- Mirror witnesses for a tuple type.
  
intptr_t Tuple_getCount(MagicMirrorData *self, const Metadata *Self) {
  auto Tuple = static_cast<const TupleTypeMetadata *>(self->Type);
  return Tuple->NumElements;
}

StringMirrorTuple Tuple_getChild(intptr_t i, MagicMirrorData *self,
                                 const Metadata *Self) {
  StringMirrorTuple result;
  
  auto Tuple = static_cast<const TupleTypeMetadata *>(self->Type);
  
  if (i < 0 || (size_t)i > Tuple->NumElements)
    abort();
  
  // The name is the stringized element number '.0'.
  char buf[32];
  snprintf(buf, 31, ".%td", i);
  buf[31] = 0;
  result.first = String(buf, strlen(buf));
  
  // Get a Mirror for the nth element.
  auto &elt = Tuple->getElements()[i];
  auto bytes = reinterpret_cast<const char*>(self->Value);
  auto eltData = reinterpret_cast<const OpaqueValue *>(bytes + elt.Offset);
  
  result.second = swift_unsafeReflectAny(self->Owner, eltData, elt.Type);
  return result;
}
  
StringReturn Tuple_getString(MagicMirrorData *self, const Metadata *Self) {
  auto Tuple = static_cast<const TupleTypeMetadata *>(self->Type);
  
  auto buf = reinterpret_cast<char*>(malloc(128));
  snprintf(buf, 127, "(%td elements)", Tuple->NumElements);
  buf[127] = 0;
  
  String s(buf, intptr_t(strlen(buf)));
  free(buf);
  return swift_returnString(&s);
}
  
static const MirrorWitnessTable TupleMirrorWitness{
  Opaque_getValue,
  Opaque_getType,
  Opaque_getObjectIdentifier,
  Tuple_getCount,
  Tuple_getChild,
  Tuple_getString,
  Opaque_getIDERepresentation,
};
  
// -- Mirror witnesses for classes.
  
OptionalObjectIdentifier Class_getObjectIdentifier(MagicMirrorData *self,
                                                   const Metadata *Self) {
  const void *object = *reinterpret_cast<const void * const*>(self->Value);
  return {object, false};
}
  
// TODO: Structural reflection of native classes.
static const MirrorWitnessTable ClassMirrorWitness{
  Opaque_getValue,
  Opaque_getType,
  Class_getObjectIdentifier,
  Opaque_getCount,
  Opaque_getChild,
  Opaque_getString,
  Opaque_getIDERepresentation,
};
  
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
const Metadata *getMetadataForEncoding(const char *encoding) {
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
  
Any ObjC_getValue(MagicMirrorData *self, const Metadata *Self) {
  Any result;
  
  id object = *reinterpret_cast<const id *>(self->Value);
  auto isa = reinterpret_cast<const ClassMetadata *>(object_getClass(object));
  result.Self = swift_getObjCClassMetadata(isa);
  *reinterpret_cast<id *>(&result.Value) = [object retain];
  return result;
}
  
const Metadata *ObjC_getType(MagicMirrorData *self, const Metadata *Self) {
  id object = *reinterpret_cast<const id *>(self->Value);
  auto isa = reinterpret_cast<const ClassMetadata *>(object_getClass(object));
  return swift_getObjCClassMetadata(isa);
}
  
intptr_t ObjC_getCount(MagicMirrorData *self, const Metadata *Self) {
  // Copying the ivar list just to free it is lame, but we have
  // no room to save it.
  auto isa = (Class)self->Type;
  
  unsigned count;
  Ivar *ivars = class_copyIvarList(isa, &count);
  free(ivars);
  
  // The superobject counts as a subobject.
  if (class_getSuperclass(isa))
    count += 1;
  
  return count;
}
Mirror ObjC_getMirrorForSuperclass(Class sup, MagicMirrorData *orig);
StringMirrorTuple ObjC_getChild(intptr_t i, MagicMirrorData *self,
                                const Metadata *Self) {
  id object = *reinterpret_cast<const id *>(self->Value);
  auto isa = (Class)self->Type;
  
  // If there's a superclass, it becomes the first child.
  if (auto sup = class_getSuperclass(isa)) {
    if (i == 0) {
      StringMirrorTuple result;
      const char *supName = class_getName(sup);
      result.first = String(supName, strlen(supName));
      result.second = ObjC_getMirrorForSuperclass(sup, self);
      return result;
    }
    --i;
  }
  
  // Copying the ivar list just to free it is lame, but we have
  // no room to save it.
  unsigned count;
  Ivar *ivars = class_copyIvarList(isa, &count);
  
  if (i < 0 || i > count)
    abort();
  
  const char *name = ivar_getName(ivars[i]);
  ptrdiff_t offset = ivar_getOffset(ivars[i]);
  const char *typeEncoding = ivar_getTypeEncoding(ivars[i]);
  free(ivars);
  
  const OpaqueValue *value =
    reinterpret_cast<const OpaqueValue *>(
    reinterpret_cast<const char*>(object) + offset);
  
  const Metadata *type = getMetadataForEncoding(typeEncoding);
  
  StringMirrorTuple result;
  result.first = String(name, strlen(name));
  result.second = swift_unsafeReflectAny(self->Owner, value, type);
  return result;
}
StringReturn ObjC_getString(MagicMirrorData *self, const Metadata *Self) {
  id object = *reinterpret_cast<const id *>(self->Value);
  
  NSString *result = [object debugDescription];
  const char *cResult = [result UTF8String];
  String s(cResult, strlen(cResult));
  return swift_returnString(&s);
}
  
static const MirrorWitnessTable ObjCMirrorWitness{
  ObjC_getValue,
  ObjC_getType,
  Class_getObjectIdentifier,
  ObjC_getCount,
  ObjC_getChild,
  ObjC_getString,
  // TODO: call down to -debugQuickLookObject.
  // We need to settle on the representation of this API first.
  Opaque_getIDERepresentation,
};
  
// For super mirrors, we suppress the object identifier.
static const MirrorWitnessTable ObjCSuperMirrorWitness{
  ObjC_getValue,
  ObjC_getType,
  Opaque_getObjectIdentifier,
  ObjC_getCount,
  ObjC_getChild,
  ObjC_getString,
  // TODO: call down to -debugQuickLookObject.
  // We need to settle on the representation of this API first.
  Opaque_getIDERepresentation,
};
  
Mirror ObjC_getMirrorForSuperclass(Class sup, MagicMirrorData *orig) {
  Mirror resultBuf;
  MagicMirror *result = ::new (&resultBuf) MagicMirror;
  
  result->MirrorWitness = &ObjCSuperMirrorWitness;
  result->Data.Owner = swift_retain(orig->Owner);
  result->Data.Type = reinterpret_cast<ClassMetadata*>(sup);
  result->Data.Value = orig->Value;
  return resultBuf;
}
  
// -- MagicMirror implementation.
  
static std::pair<const Metadata *, const MirrorWitnessTable *>
getWitnessForClass(const OpaqueValue *Value) {
  // Get the runtime type of the object.
  id obj = *reinterpret_cast<const id *>(Value);
  auto isa = reinterpret_cast<const ClassMetadata*>(object_getClass(obj));
  
  // If this is a pure ObjC class, reflect it using ObjC's runtime facilities.
  if (!isa->isTypeMetadata())
    return {isa, &ObjCMirrorWitness};
  
  // Otherwise, use the (currently nonexistent) native Swift facilities.
  return {isa, &ClassMirrorWitness};
}
  
/// Get the magic mirror witnesses appropriate to a particular type.
static std::pair<const Metadata *, const MirrorWitnessTable *>
getWitnessForType(const Metadata *T, const OpaqueValue *Value) {
  switch (T->getKind()) {
  case MetadataKind::Tuple:
    return {T, &TupleMirrorWitness};
      
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::Class: {
    return getWitnessForClass(Value);
  }
      
  case MetadataKind::Opaque: {
    // If this is the Builtin.ObjCPointer type, use the dynamic type of the
    // object reference.
    if (T == &_TMdBO.base) {
      return getWitnessForClass(Value);
    }
    
    // If this is the Builtin.ObjectPointer type, and the heap object is a
    // class instance, use the dynamic type of the object reference.
    if (T == &_TMdBo.base) {
      const HeapObject *obj
        = *reinterpret_cast<const HeapObject * const*>(Value);
      if (obj->metadata->getKind() == MetadataKind::Class)
        return getWitnessForClass(Value);
    }
    SWIFT_FALLTHROUGH;
  }
      
  /// TODO: Implement specialized mirror witnesses for all kinds.
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Function:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
    return {T, &OpaqueMirrorWitness};
      
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
  auto box = swift_allocBox(T);
  
  T->vw_initializeWithTake(box.value, value);
  std::tie(T, MirrorWitness) = getWitnessForType(T, value);
  
  Data = {box.heapObject, box.value, T};
}
  
/// MagicMirror ownership-sharing subvalue constructor.
MagicMirror::MagicMirror(HeapObject *owner,
                         const OpaqueValue *value, const Metadata *T) {
  swift_retain(owner);
  
  std::tie(T, MirrorWitness) = getWitnessForType(T, value);
  Data = {owner, value, T};
}
  
const ReflectableWitnessTable *getReflectableConformance(const Metadata *T) {
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
