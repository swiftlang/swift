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
#include "swift/Basic/Demangle.h"
#include "Debug.h"
#include "Private.h"
#include <cassert>
#include <cstring>
#include <new>
#include <string>
#include <regex>
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
  
struct QuickLookObject {
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
    uint64_t loc,len;
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
struct OptionalQuickLookObject {
  union {
    struct {
      union {
        String TextOrURL;
        int64_t Int;
        uint64_t UInt;
        float Float;
        double Double;
        Any Any;
        QuickLookObject::RawData Raw;
        QuickLookObject::Rectangle Rect;
        QuickLookObject::Point PointOrSize;
        bool Logical;
        QuickLookObject::Interval Range;
      };
      QuickLookObject::Tag Kind;
      bool isNone;
    } optional;
    QuickLookObject payload;
  };
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
static_assert(sizeof(MagicMirrorData) == sizeof(ValueBuffer),
              "MagicMirrorData doesn't exactly fill a ValueBuffer");
  
/// A magic implementation of Mirror that can^Wwill be able to look at runtime
/// metadata to walk an arbitrary object.
///
/// This type is layout-compatible with a Swift existential
/// container for the MirrorType protocol.
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
  
extern "C"
Any swift_MagicMirrorData_value(HeapObject *owner,
                                const OpaqueValue *value,
                                const Metadata *type) {
  Any result;
  
  result.Type = type;
  type->vw_initializeBufferWithCopy(&result.Buffer,
                                    const_cast<OpaqueValue*>(value));
  
  swift_release(owner);
  
  return result;
}
extern "C"
const Metadata *swift_MagicMirrorData_valueType(HeapObject *owner,
                                                const OpaqueValue *value,
                                                const Metadata *type) {
  auto r = swift_getDynamicType(const_cast<OpaqueValue*>(value), type);
  swift_release(owner);
  return r;
}

#if SWIFT_OBJC_INTEROP
extern "C"
Any swift_MagicMirrorData_objcValue(HeapObject *owner,
                                    const OpaqueValue *value,
                                    const Metadata *type) {
  Any result;
    
  void *object = *reinterpret_cast<void * const *>(value);
  auto isa = _swift_getClass(object);
  result.Type = swift_getObjCClassMetadata(isa);
  *reinterpret_cast<void **>(&result.Buffer) = swift_unknownRetain(object);
  swift_release(owner);
  return result;
}
#endif

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
    case MetadataKind::ThinFunction:
      new (result) String("(Thin Function)");
      break;
    case MetadataKind::PolyFunction:
      new (result) String("(Polymorphic Function)");
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
    case MetadataKind::Block:
      new (result) String("(Block)");
      break;
    case MetadataKind::HeapLocalVariable:
      new (result) String("(Heap Local Variable)");
      break;
  }
}

  
extern "C"
const Metadata *swift_MagicMirrorData_objcValueType(HeapObject *owner,
                                                    const OpaqueValue *value,
                                                    const Metadata *type) {
  void *object = *reinterpret_cast<void * const *>(value);
  auto isa = _swift_getClass(object);
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
  auto &elt = Tuple->getElements()[i];
  auto bytes = reinterpret_cast<const char*>(value);
  auto eltData = reinterpret_cast<const OpaqueValue *>(bytes + elt.Offset);
  
  // 'owner' is consumed by this call.
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
    swift::crash("Swift mirror subscript bounds check failure");
  
  // Load the type and offset from their respective vectors.
  auto fieldType = Struct->getFieldTypes()[i];
  auto fieldOffset = Struct->getFieldOffsets()[i];
  
  auto bytes = reinterpret_cast<const char*>(value);
  auto fieldData = reinterpret_cast<const OpaqueValue *>(bytes + fieldOffset);
  
  // Get the field name from the doubly-null-terminated list.
  const char *fieldName = Struct->Description->Struct.FieldNames;
  for (size_t j = 0; j < (size_t)i; ++j) {
    while (*fieldName++);
  }

  result.first = String(fieldName);
  
  result.second = swift_unsafeReflectAny(owner, fieldData, fieldType);
  return result;
}
  
// -- Class destructuring.
static bool classHasSuperclass(const ClassMetadata *c) {
#if SWIFT_OBJC_INTEROP
  // A class does not have a superclass if its ObjC superclass is the
  // "SwiftObject" root class.
  return c->SuperClass
    && (Class)c->SuperClass != NSClassFromString(@"SwiftObject");
#else
  // In non-objc mode, the test is just if it has a non-null superclass.
  return c->SuperClass != nullptr;
#endif
}

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
  
  // Get the field name from the doubly-null-terminated list.
  const char *fieldName = Clas->getDescription()->Class.FieldNames;
  for (size_t j = 0; j < (size_t)i; ++j) {
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
  
// Set to 1 to enable reflection of objc ivars.
#define REFLECT_OBJC_IVARS 0
  
/// Map an ObjC type encoding string to a Swift type metadata object.
///
#if REFLECT_OBJC_IVARS
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
#endif

/// \param owner passed at +1, consumed.
/// \param value passed unowned.
extern "C"
intptr_t swift_ObjCMirror_count(HeapObject *owner,
                                const OpaqueValue *value,
                                const Metadata *type) {
#if REFLECT_OBJC_IVARS
  auto isa = (Class)type;
  
  unsigned count;
  // Don't reflect ivars of classes that lie about their layout.
  if (objcClassLiesAboutLayout(isa)) {
    count = 0;
  } else {
    // Copying the ivar list just to free it is lame, but we have
    // nowhere to save it.
    Ivar *ivars = class_copyIvarList(isa, &count);
    free(ivars);
  }
  
  // The superobject counts as a child.
  if (_swift_getSuperclass((const ClassMetadata*) isa))
    count += 1;
  
  swift_release(owner);
  return count;
#else
  // ObjC makes no guarantees about the state of ivars, so we can't safely
  // introspect them in the general case.
  swift_release(owner);
  return 0;
#endif
}
#if SWIFT_OBJC_INTEROP
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
  result.second = swift_unsafeReflectAny(owner, ivar, ivarType);
  return result;
#else
  // ObjC makes no guarantees about the state of ivars, so we can't safely
  // introspect them in the general case.
  abort();  
#endif
}

extern "C"
OptionalQuickLookObject swift_ClassMirror_quickLookObject(HeapObject *owner,
                                                          const OpaqueValue *value,
                                                          const Metadata *type) {
  OptionalQuickLookObject result;
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
      result.payload.Kind = QuickLookObject::Tag::Double;
      break;
    case 'f': // float
      result.payload.Float = [n floatValue];
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
    result.payload.Kind = QuickLookObject::Tag::AttributedString;
    result.optional.isNone = false;
    return result;
  } else if ([object isKindOfClass:NSClassFromString(@"NSImage")]
      || [object isKindOfClass:NSClassFromString(@"UIImage")]
      || [object isKindOfClass:NSClassFromString(@"NSImageView")]
      || [object isKindOfClass:NSClassFromString(@"UIImageView")]
      || [object isKindOfClass:NSClassFromString(@"CIImage")]
      || [object isKindOfClass:NSClassFromString(@"NSBitmapImageRep")]) {
    initializeAnyWithTakeOfObject(result.payload.Any, object);
    result.payload.Kind = QuickLookObject::Tag::Image;
    result.optional.isNone = false;
    return result;
  } else if ([object isKindOfClass:NSClassFromString(@"NSColor")]
             || [object isKindOfClass:NSClassFromString(@"UIColor")]) {
    initializeAnyWithTakeOfObject(result.payload.Any, object);
    result.payload.Kind = QuickLookObject::Tag::Color;
    result.optional.isNone = false;
    return result;
  } else if ([object isKindOfClass:NSClassFromString(@"NSBezierPath")]
             || [object isKindOfClass:NSClassFromString(@"UIBezierPath")]) {
    initializeAnyWithTakeOfObject(result.payload.Any, object);
    result.payload.Kind = QuickLookObject::Tag::BezierPath;
    result.optional.isNone = false;
    return result;
  } else if ([object isKindOfClass:[NSString class]]) {
    result.payload.TextOrURL = String((NSString*)object);
    [object release];
    result.payload.Kind = QuickLookObject::Tag::Text;
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
  
// Addresses of the type metadata and Mirror witness tables for the primitive
// mirrors.
extern "C" const FullMetadata<Metadata> _TMdVSs13_OpaqueMirror;
extern "C" const MirrorWitnessTable _TWPVSs13_OpaqueMirrorSs10MirrorTypeSs;

extern "C" const FullMetadata<Metadata> _TMdVSs12_TupleMirror;
extern "C" const MirrorWitnessTable _TWPVSs12_TupleMirrorSs10MirrorTypeSs;

extern "C" const FullMetadata<Metadata> _TMdVSs13_StructMirror;
extern "C" const MirrorWitnessTable _TWPVSs13_StructMirrorSs10MirrorTypeSs;

extern "C" const FullMetadata<Metadata> _TMdVSs12_ClassMirror;
extern "C" const MirrorWitnessTable _TWPVSs12_ClassMirrorSs10MirrorTypeSs;

extern "C" const FullMetadata<Metadata> _TMdVSs17_ClassSuperMirror;
extern "C" const MirrorWitnessTable _TWPVSs17_ClassSuperMirrorSs10MirrorTypeSs;

extern "C" const FullMetadata<Metadata> _TMdVSs15_MetatypeMirror;
extern "C" const MirrorWitnessTable _TWPVSs15_MetatypeMirrorSs10MirrorTypeSs;
  
#if SWIFT_OBJC_INTEROP
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
      dlsym(RTLD_DEFAULT, "_TWPV10Foundation11_ObjCMirrorSs10MirrorTypeS_"));
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
      dlsym(RTLD_DEFAULT, "_TWPV10Foundation16_ObjCSuperMirrorSs10MirrorTypeS_"));
  assert(witness);
  
  return witness;
}
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
  
  result->Self = &_TMdVSs17_ClassSuperMirror;
  result->MirrorWitness = &_TWPVSs17_ClassSuperMirrorSs10MirrorTypeSs;
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
  
  result->Self = getObjCSuperMirrorMetadata();
  result->MirrorWitness = getObjCSuperMirrorWitness();
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
    return {isa, getObjCMirrorMetadata(), getObjCMirrorWitness()};
#endif

  // Otherwise, use the native Swift facilities.
  return std::make_tuple(
      isa, &_TMdVSs12_ClassMirror, &_TWPVSs12_ClassMirrorSs10MirrorTypeSs);
}
  
/// Get the magic mirror witnesses appropriate to a particular type.
static MirrorTriple
getImplementationForType(const Metadata *T, const OpaqueValue *Value) {
  switch (T->getKind()) {
  case MetadataKind::Tuple:
    return std::make_tuple(
        T, &_TMdVSs12_TupleMirror, &_TWPVSs12_TupleMirrorSs10MirrorTypeSs);
      
  case MetadataKind::Struct:
    return std::make_tuple(
        T, &_TMdVSs13_StructMirror, &_TWPVSs13_StructMirrorSs10MirrorTypeSs);
      
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass:
  case MetadataKind::Class: {
    return getImplementationForClass(Value);
  }
      
  case MetadataKind::Opaque: {
#if SWIFT_OBJC_INTEROP
    // If this is the Builtin.UnknownObject type, use the dynamic type of the
    // object reference.
    if (T == &_TMdBO.base) {
      return getImplementationForClass(Value);
    }
#endif
    // If this is the Builtin.NativeObject type, and the heap object is a
    // class instance, use the dynamic type of the object reference.
    if (T == &_TMdBo.base) {
      const HeapObject *obj
        = *reinterpret_cast<const HeapObject * const*>(Value);
      if (obj->metadata->getKind() == MetadataKind::Class)
        return getImplementationForClass(Value);
    }
    SWIFT_FALLTHROUGH;
  }
  
  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype: {
    return std::make_tuple(T, &_TMdVSs15_MetatypeMirror,
                           &_TWPVSs15_MetatypeMirrorSs10MirrorTypeSs);
  }
      
  /// TODO: Implement specialized mirror witnesses for all kinds.
  case MetadataKind::Enum:
  case MetadataKind::Function:
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::Existential:
    return std::make_tuple(
        T, &_TMdVSs13_OpaqueMirror, &_TWPVSs13_OpaqueMirrorSs10MirrorTypeSs);
      
  // Types can't have these kinds.
  case MetadataKind::PolyFunction:
  case MetadataKind::HeapLocalVariable:
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
    T->vw_initializeWithTake(box.value, value);
  else
    T->vw_initializeWithCopy(box.value, value);
  std::tie(T, Self, MirrorWitness) = getImplementationForType(T, box.value);
  
  Data = {box.heapObject, box.value, T};
}
  
/// MagicMirror ownership-sharing subvalue constructor.
///
/// \param owner passed at +1, consumed.
MagicMirror::MagicMirror(HeapObject *owner,
                         const OpaqueValue *value, const Metadata *T) {
  std::tie(T, Self, MirrorWitness) = getImplementationForType(T, value);
  Data = {owner, value, T};
}
  
static std::tuple<const ReflectableWitnessTable *, const Metadata *,
                  const OpaqueValue *>
getReflectableConformance(const Metadata *T, const OpaqueValue *Value) {
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
  case MetadataKind::Function:
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::Metatype:
    break;
      
  case MetadataKind::Existential: {
    auto existential
      = static_cast<const ExistentialTypeMetadata *>(T);
    
    // If the existential happens to include the Reflectable protocol, use
    // the witness table from the container.
    unsigned wtOffset = 0;
    for (unsigned i = 0; i < existential->Protocols.NumProtocols; ++i) {
      if (existential->Protocols[i] == &_TMpSs11Reflectable) {
        return std::make_tuple(
            reinterpret_cast<const ReflectableWitnessTable*>(
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
    break;
  }
  case MetadataKind::ExistentialMetatype:
    // TODO: Should look through existential metatypes too, but it doesn't
    // really matter yet since we don't have any special mirror behavior for
    // concrete metatypes yet.
    break;
      
  // Types can't have these kinds.
  case MetadataKind::PolyFunction:
  case MetadataKind::HeapLocalVariable:
    swift::crash("Swift mirror lookup failure");
  }
  
  return std::make_tuple(
      reinterpret_cast<const ReflectableWitnessTable*>(
        swift_conformsToProtocol(T, &_TMpSs11Reflectable)),
      T,
      Value);
}
  
} // end anonymous namespace

/// func reflect<T>(x: T) -> Mirror
///
/// Produce a mirror for any value. If the value's type conforms to Reflectable,
/// invoke its getMirror() method; otherwise, fall back to an implementation
/// in the runtime that structurally reflects values of any type.
///
/// This function consumes 'value', following Swift's +1 convention for "in"
/// arguments.
Mirror swift::swift_reflectAny(OpaqueValue *value, const Metadata *T) {
  const ReflectableWitnessTable *witness;
  const Metadata *mirrorType;
  const OpaqueValue *cMirrorValue;
  std::tie(witness, mirrorType, cMirrorValue)
    = getReflectableConformance(T, value);
  
  OpaqueValue *mirrorValue = const_cast<OpaqueValue*>(cMirrorValue);
  
  // Use the Reflectable conformance if the object has one.
  if (witness) {
    auto result = witness->getMirror(mirrorValue, mirrorType);
    // 'self' of witnesses is passed at +0, so we still need to consume the
    // value.
    T->vw_destroy(value);
    return result;
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
  return result;
}

/// Produce a mirror for any value, like swift_reflectAny, but do not consume
/// the value, so we can produce a mirror for a subobject of a value already
/// owned by a mirror.
///
/// \param owner passed at +1, consumed.
/// \param value passed unowned.
Mirror swift::swift_unsafeReflectAny(HeapObject *owner,
                                     const OpaqueValue *value,
                                     const Metadata *T) {
  const ReflectableWitnessTable *witness;
  const Metadata *mirrorType;
  const OpaqueValue *mirrorValue;
  std::tie(witness, mirrorType, mirrorValue)
    = getReflectableConformance(T, value);
  
  // Use the Reflectable conformance if the object has one.
  if (witness) {
    auto result =
        witness->getMirror(const_cast<OpaqueValue*>(mirrorValue), mirrorType);
    swift_release(owner);
    return result;
  }
  // Otherwise, fall back to MagicMirror.
  // Consumes 'owner'.
  Mirror result;
  ::new (&result) MagicMirror(owner, mirrorValue, mirrorType);
  return result;
}

extern "C" void
swift_stdlib_getDemangledMetatypeName(const Metadata *type, String *outString) {
  std::string name = nameForMetadata(type);
  swift_stringFromUTF8InRawMemory(outString, name.data(), name.length());
}

static void swift_stdlib_getDemangledTypeNameImpl(OpaqueValue *value,
                                         const Metadata *T,
                                         const Metadata *dynamicType,
                                         String *result) {
  switch (dynamicType->getKind()) {
  // Drill through existentials to properly get the dynamic type of their
  // contained value.
  case MetadataKind::Existential: {
    auto existentialMetadata =
      static_cast<const ExistentialTypeMetadata *>(dynamicType);
    return swift_stdlib_getDemangledTypeNameImpl(
                  value, T, existentialMetadata->getDynamicType(value), result);
  }

  // TODO: Do we need something similar for ExistentialMetatype?
  
  case MetadataKind::Class: {
    // If the class is an artificial subclass, jump up to the "real" base
    // class.
    for (;;) {
      auto dynamicClass = static_cast<const ClassMetadata *>(dynamicType);
      if (dynamicClass->isTypeMetadata()
          && dynamicClass->isArtificialSubclass())
        dynamicType = dynamicClass->SuperClass;
      else
        break;
    }
    SWIFT_FALLTHROUGH;
  }
  case MetadataKind::Tuple:
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Opaque:
  case MetadataKind::Function:
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Metatype:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass:
    return swift_stdlib_getDemangledMetatypeName(dynamicType, result);

  // Values should never use these metadata kinds.
  case MetadataKind::PolyFunction:
  case MetadataKind::HeapLocalVariable:
    assert(false);
    new (result) String("");
    return;
  }
}

extern "C" void swift_stdlib_getDemangledTypeName(OpaqueValue *value,
                                                  String *result,
                                                  const Metadata *T) {
  swift_stdlib_getDemangledTypeNameImpl(value, T, T, result);
  T->vw_destroy(value);
}

extern "C" void swift_stdlib_demangleName(const char *mangledName,
                                          size_t mangledNameLength,
                                          String *demangledName) {
  auto result =
      Demangle::demangleSymbolAsString(mangledName, mangledNameLength);
  new (demangledName) String(result.data(), result.size());
}

