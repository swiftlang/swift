//===--- MetadataValues.h - Compiler/runtime ABI Metadata -------*- C++ -*-===//
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
// This header is shared between the runtime and the compiler and
// includes target-independent information which can be usefully shared
// between them.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_METADATAVALUES_H
#define SWIFT_ABI_METADATAVALUES_H

#include <stdlib.h>
#include <stdint.h>

namespace swift {

/// Kinds of Swift metadata records.  Some of these are types, some
/// aren't.
enum class MetadataKind : uintptr_t {
  /// A class type.
  Class         = 0,

  /// A struct type.
  Struct        = 1,

  /// An enum type.
  /// If we add reference enums, that needs to go here.
  Enum          = 2,

  /// A type whose value is not exposed in the metadata system.
  Opaque        = 8,

  /// A tuple.
  Tuple         = 9,

  /// A monomorphic function.
  Function      = 10,

  /// A polymorphic function.
  PolyFunction  = 11,

  /// An existential type.
  Existential   = 12,

  /// A metatype.
  Metatype      = 13,

  /// An ObjC class wrapper.
  ObjCClassWrapper = 14,

  /// An existential metatype.
  ExistentialMetatype = 15,

  /// A foreign class, such as a Core Foundation class.
  ForeignClass = 16,
  
  /// An Objective-C block type.
  Block = 17,

  /// A thin function type.
  ThinFunction = 18,

  // Array types?
  // L-value types?

  /// After this point start the non-type metadata.
  NonTypeMetadata_First = 64,

  /// A heap-allocated local variable.
  HeapLocalVariable = 64,

  /// Anything greater than this is a class isa pointer.
  MetadataKind_Last = HeapLocalVariable,
};
  
/// Kinds of Swift nominal type descriptor records.
enum class NominalTypeKind : uintptr_t {
  /// A class type.
  Class = uintptr_t(MetadataKind::Class),
  /// A struct type.
  Struct = uintptr_t(MetadataKind::Struct),
  /// An enum type.
  Enum = uintptr_t(MetadataKind::Enum),
};

/// Flags for dynamic-cast operations.
enum class DynamicCastFlags : size_t {
  /// All flags clear.
  Default = 0x0,

  /// True if the cast is not permitted to fail.
  Unconditional = 0x1,

  /// True if the cast should 'take' the source value on success;
  /// false if the value should be copied.
  TakeOnSuccess = 0x2,

  /// True if the cast should destroy the source value on failure;
  /// false if the value should be left in place.
  DestroyOnFailure = 0x4,
};
inline bool operator&(DynamicCastFlags a, DynamicCastFlags b) {
  return (size_t(a) & size_t(b)) != 0;
}
inline DynamicCastFlags operator|(DynamicCastFlags a, DynamicCastFlags b) {
  return DynamicCastFlags(size_t(a) | size_t(b));
}
inline DynamicCastFlags operator-(DynamicCastFlags a, DynamicCastFlags b) {
  return DynamicCastFlags(size_t(a) & ~size_t(b));
}
inline DynamicCastFlags &operator|=(DynamicCastFlags &a, DynamicCastFlags b) {
  return a = (a | b);
}

/// Swift class flags.
enum class ClassFlags : uint32_t {
  /// Is this a Swift 1 class?
  IsSwift1 = 0x1,

  /// Does this class use Swift 1.0 refcounting?
  UsesSwift1Refcounting = 0x2,
};
inline bool operator&(ClassFlags a, ClassFlags b) {
  return (uint32_t(a) & uint32_t(b)) != 0;
}
inline ClassFlags operator|(ClassFlags a, ClassFlags b) {
  return ClassFlags(uint32_t(a) | uint32_t(b));
}
inline ClassFlags &operator|=(ClassFlags &a, ClassFlags b) {
  return a = (a | b);
}

enum : unsigned {
  /// Number of words reserved in generic metadata patterns.
  NumGenericMetadataPrivateDataWords = 16,
};
  
/// Kinds of protocol conformance record.
enum class ProtocolConformanceTypeKind : unsigned {
  /// The conformance is universal and might apply to any type.
  /// getDirectType() is nil.
  Universal,

  /// The conformance is for a nongeneric native struct or enum type.
  /// getDirectType() points to the canonical metadata for the type.
  UniqueDirectType,
  
  /// The conformance is for a nongeneric foreign struct or enum type.
  /// getDirectType() points to a nonunique metadata record for the type, which
  /// needs to be uniqued by the runtime.
  NonuniqueDirectType,
  
  /// The conformance is for a nongeneric class type.
  /// getIndirectClass() points to a variable that contains the pointer to the
  /// class object, which may be ObjC and thus require a runtime call to get
  /// metadata.
  ///
  /// On platforms without ObjC interop, this indirection isn't necessary,
  /// and classes could be emitted as UniqueDirectType.
  UniqueIndirectClass,
  
  /// The conformance is for a generic type.
  /// getGenericPattern() points to the generic metadata pattern used to
  /// form instances of the type.
  UniqueGenericPattern,
  
  /// The conformance is for a nongeneric class type.
  /// getDirectType() points to the unique class object.
  ///
  /// FIXME: This shouldn't exist. On ObjC interop platforms, class references
  /// must be indirected (using UniqueIndirectClass). On non-ObjC interop
  /// platforms, the class object always is the type metadata.
  UniqueDirectClass = 0xF,
};
  
/// Kinds of reference to protocol conformance.
enum class ProtocolConformanceReferenceKind : unsigned {
  /// A direct reference to a protocol witness table.
  WitnessTable,
  /// A function pointer that can be called to access the protocol witness
  /// table.
  WitnessTableAccessor,
};
  
struct ProtocolConformanceFlags {
private:
  using int_type = unsigned;
  int_type Data;
  
  enum : int_type {
    TypeKindMask = 0x0000000FU,
    TypeKindShift = 0,
    ConformanceKindMask = 0x00000010U,
    ConformanceKindShift = 4,
  };
  
public:
  constexpr ProtocolConformanceFlags() : Data(0) {}
  constexpr ProtocolConformanceFlags(int_type Data) : Data(Data) {}
  
  constexpr ProtocolConformanceTypeKind getTypeKind() const {
    return ProtocolConformanceTypeKind((Data >> TypeKindShift) & TypeKindMask);
  }
  constexpr ProtocolConformanceFlags withTypeKind(
                                        ProtocolConformanceTypeKind ptk) const {
    return ProtocolConformanceFlags(
                     (Data & ~TypeKindMask) | (int_type(ptk) << TypeKindShift));
  }
  
  constexpr ProtocolConformanceReferenceKind getConformanceKind() const {
    return ProtocolConformanceReferenceKind((Data >> ConformanceKindShift)
                                     & ConformanceKindMask);
  }
  constexpr ProtocolConformanceFlags withConformanceKind(
                                  ProtocolConformanceReferenceKind pck) const {
    return ProtocolConformanceFlags(
       (Data & ~ConformanceKindMask) | (int_type(pck) << ConformanceKindShift));
  }
  
  int_type getValue() const { return Data; }
};
  
}

#endif
