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

#include "swift/Runtime/Reflection.h"
#include "swift/Runtime/Alloc.h"
#include "swift/Runtime/Metadata.h"
#include <cassert>
#include <cstring>
#include <new>
#include <string>

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
struct String {
  const void *data;
  intptr_t countAndFlags;
  HeapObject *owner;
  
  String() = default;
  
  /// Wrap a string literal in a swift String.
  template<size_t N>
  explicit String(const char (&s)[N])
    : data(s), countAndFlags(N-1), owner(nullptr) {}
  
  /// Copy an ASCII string into a swift String on the heap.
  explicit String(const char *ptr, size_t size)
    // FIXME: leaks
    : data(malloc(size)), countAndFlags(size), owner(nullptr)
  {
    memcpy(const_cast<void*>(data), ptr, size);
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
extern "C" StringReturn _TFSs13_returnStringFT1sRSS_SS(String *s);
  
/// A Mirror witness table for use by MagicMirror.
struct MirrorWitnessTable {
  /// func getValue() -> Any
  Any (*getValue)(MagicMirrorData *self, const Metadata *Self);
  /// func getType() -> Any.metatype
  const Metadata * (*getType)(MagicMirrorData *self, const Metadata *Self);
  
  /// func getObjectIdentifier() -> ObjectIdentifier?
  OptionalObjectIdentifier (*getObjectIdentifier)(MagicMirrorData *self,
                                                  const Metadata *Self);
  /// func getCount() -> Int
  intptr_t (*getCount)(MagicMirrorData *self, const Metadata *Self);
  
  /// func getChild(Int) -> (String, Mirror)
  StringMirrorTuple (*getChild)(intptr_t i, MagicMirrorData *self,
                                const Metadata *Self);
  /// func getString() -> String
  StringReturn (*getString)(MagicMirrorData *self, const Metadata *Self);
  
  /// func getIDERepresentation() -> IDERepresentable?
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
  /// The type metadata for the referenced value.
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
  
// Mirror witnesses for a type we don't know how to reflect.
  
Any Opaque_getValue(MagicMirrorData *self, const Metadata *Self) {
  Any result;
  
  auto T = self->Type;
  result.Self = T;
  T->vw_initializeBufferWithCopy(&result.Value,
                                 const_cast<OpaqueValue*>(self->Value));
  
  return result;
}
const Metadata *Opaque_getType(MagicMirrorData *self, const Metadata *Self) {
  return self->Type;
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
  
  return _TFSs13_returnStringFT1sRSS_SS(&s);
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
  
// Mirror witnesses for a tuple type.
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
  
  String s{buf, intptr_t(strlen(buf)), nullptr};
  return _TFSs13_returnStringFT1sRSS_SS(&s);
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
  
/// Get the magic mirror witnesses appropriate to a particular type.
static const MirrorWitnessTable *getWitnessForType(const Metadata *T) {
  switch (T->getKind()) {
  case MetadataKind::Tuple:
    return &TupleMirrorWitness;
      
  /// TODO: Implement specialized mirror witnesses for all kinds.
  case MetadataKind::Class:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::Function:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
    return &OpaqueMirrorWitness;
      
  // Types can't have these kinds.
  case MetadataKind::PolyFunction:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapArray:
    abort();
  }
}

  
/// MagicMirror ownership-taking constructor.
MagicMirror::MagicMirror(OpaqueValue *value, const Metadata *T) {
  // Put value types into a box so we can take stable interior pointers.
  // TODO: Specialize behavior here. If the value is a swift-refcounted class
  // we don't need to put it in a box to point into it.
  auto box = swift_allocBox(T);
  T->vw_initializeWithTake(box.value, value);
  
  MirrorWitness = getWitnessForType(T);
  Data = {box.heapObject, box.value, T};
}
  
/// MagicMirror ownership-sharing constructor.
MagicMirror::MagicMirror(HeapObject *owner,
                         const OpaqueValue *value, const Metadata *T) {
  swift_retain(owner);
  
  MirrorWitness = getWitnessForType(T);
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
