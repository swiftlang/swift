//===--- MetadataValues.h - Compiler/runtime ABI Metadata -------*- C++ -*-===//
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
//
// This header is shared between the runtime and the compiler and
// includes target-independent information which can be usefully shared
// between them.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_METADATAVALUES_H
#define SWIFT_ABI_METADATAVALUES_H

#include "swift/AST/Ownership.h"
#include "swift/Runtime/Unreachable.h"

#include <stdlib.h>
#include <stdint.h>

namespace swift {

struct InProcess;
template <typename Runtime> struct TargetMetadata;
using Metadata = TargetMetadata<InProcess>;

/// Kinds of Swift metadata records.  Some of these are types, some
/// aren't.
enum class MetadataKind : uint32_t {
#define METADATAKIND(name, value) name = value,
#define ABSTRACTMETADATAKIND(name, start, end)                                 \
  name##_Start = start, name##_End = end,
#include "MetadataKind.def"
};

const unsigned LastEnumeratedMetadataKind = 2047;

/// Try to translate the 'isa' value of a type/heap metadata into a value
/// of the MetadataKind enum.
inline MetadataKind getEnumeratedMetadataKind(uint64_t kind) {
  if (kind > LastEnumeratedMetadataKind)
    return MetadataKind::Class;
  return MetadataKind(kind);
}

/// Kinds of Swift nominal type descriptor records.
enum class NominalTypeKind : uint32_t {
#define NOMINALTYPEMETADATAKIND(name, value) name = value,
#include "MetadataKind.def"
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
  
/// Kinds of type metadata/protocol conformance records.
enum class TypeMetadataRecordKind : unsigned {
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
  
  /// The conformance is for a generic or resilient type.
  /// getNominalTypeDescriptor() points to the nominal type descriptor shared
  /// by all metadata instantiations of this type.
  UniqueNominalTypeDescriptor,
  
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

// Type metadata record discriminant
struct TypeMetadataRecordFlags {
protected:
  using int_type = unsigned;
  int_type Data;
  
  enum : int_type {
    TypeKindMask = 0x0000000FU,
    TypeKindShift = 0,
  };
  
public:
  constexpr TypeMetadataRecordFlags() : Data(0) {}
  constexpr TypeMetadataRecordFlags(int_type Data) : Data(Data) {}
  
  constexpr TypeMetadataRecordKind getTypeKind() const {
    return TypeMetadataRecordKind((Data & TypeKindMask) >> TypeKindShift);
  }
  constexpr TypeMetadataRecordFlags withTypeKind(
                                        TypeMetadataRecordKind ptk) const {
    return TypeMetadataRecordFlags(
                     (Data & ~TypeKindMask) | (int_type(ptk) << TypeKindShift));
  }
  
  int_type getValue() const { return Data; }
};

// Protocol conformance discriminant
struct ProtocolConformanceFlags : public TypeMetadataRecordFlags {
private:
  enum : int_type {
    ConformanceKindMask = 0x00000010U,
    ConformanceKindShift = 4,
  };

public:
  constexpr ProtocolConformanceFlags() : TypeMetadataRecordFlags(0) {}
  constexpr ProtocolConformanceFlags(int_type Data) : TypeMetadataRecordFlags(Data) {}

  constexpr ProtocolConformanceFlags withTypeKind(
                                        TypeMetadataRecordKind ptk) const {
    return ProtocolConformanceFlags(
                     (Data & ~TypeKindMask) | (int_type(ptk) << TypeKindShift));
  }
  constexpr ProtocolConformanceReferenceKind getConformanceKind() const {
    return ProtocolConformanceReferenceKind((Data & ConformanceKindMask)
                                     >> ConformanceKindShift);
  }
  constexpr ProtocolConformanceFlags withConformanceKind(
                                  ProtocolConformanceReferenceKind pck) const {
    return ProtocolConformanceFlags(
       (Data & ~ConformanceKindMask) | (int_type(pck) << ConformanceKindShift));
  }
};

/// Flag that indicates whether an existential type is class-constrained or not.
enum class ProtocolClassConstraint : bool {
  /// The protocol is class-constrained, so only class types can conform to it.
  ///
  /// This must be 0 for ABI compatibility with Objective-C protocol_t records.
  Class = false,
  /// Any type can conform to the protocol.
  Any = true,
};

/// Identifiers for protocols with special meaning to the Swift runtime.
enum class SpecialProtocol: uint8_t {
  /// Not a special protocol.
  ///
  /// This must be 0 for ABI compatibility with Objective-C protocol_t records.
  None = 0,
  /// The AnyObject protocol.
  AnyObject = 1,
  /// The Error protocol.
  Error = 2,
};

/// Identifiers for protocol method dispatch strategies.
enum class ProtocolDispatchStrategy: uint8_t {
  /// Uses ObjC method dispatch.
  ///
  /// This must be 0 for ABI compatibility with Objective-C protocol_t records.
  ObjC = 0,
  
  /// Uses Swift protocol witness table dispatch.
  ///
  /// To invoke methods of this protocol, a pointer to a protocol witness table
  /// corresponding to the protocol conformance must be available.
  Swift = 1,
  
  /// The protocol guarantees that it has no methods to dispatch. It requires
  /// neither Objective-C metadata nor a witness table.
  Empty = 2,
};

/// Flags in a generic nominal type descriptor.
class GenericParameterDescriptorFlags {
  typedef uint32_t int_type;
  enum : int_type {
    HasParent        = 0x01,
    HasGenericParent = 0x02,
  };
  int_type Data;
  
  constexpr GenericParameterDescriptorFlags(int_type data) : Data(data) {}
public:
  constexpr GenericParameterDescriptorFlags() : Data(0) {}

  constexpr GenericParameterDescriptorFlags withHasParent(bool b) const {
    return GenericParameterDescriptorFlags(b ? (Data | HasParent)
                                             : (Data & ~HasParent));
  }

  constexpr GenericParameterDescriptorFlags withHasGenericParent(bool b) const {
    return GenericParameterDescriptorFlags(b ? (Data | HasGenericParent)
                                             : (Data & ~HasGenericParent));
  }

  /// Does this type have a lexical parent type?
  ///
  /// For class metadata, if this is true, the storage for the parent type
  /// appears immediately prior to the first generic argument.  Other
  /// metadata always have a slot for their parent type.
  bool hasParent() const {
    return Data & HasParent;
  }

  /// Given that this type has a parent type, is that type generic?  If so,
  /// it forms part of the key distinguishing this metadata from other
  /// metadata, and the parent metadata will be the first argument to
  /// the generic metadata access function.
  bool hasGenericParent() const {
    return Data & HasGenericParent;
  }

  int_type getIntValue() const {
    return Data;
  }
  
  static GenericParameterDescriptorFlags fromIntValue(int_type Data) {
    return GenericParameterDescriptorFlags(Data);
  }
  
  bool operator==(GenericParameterDescriptorFlags other) const {
    return Data == other.Data;
  }
  bool operator!=(GenericParameterDescriptorFlags other) const {
    return Data != other.Data;
  }
};


/// Flags for protocol descriptors.
class ProtocolDescriptorFlags {
  typedef uint32_t int_type;
  enum : int_type {
    IsSwift           =   1U <<  0U,
    ClassConstraint   =   1U <<  1U,

    DispatchStrategyMask  = 0xFU << 2U,
    DispatchStrategyShift = 2,

    SpecialProtocolMask  = 0x000003C0U,
    SpecialProtocolShift = 6,

    IsResilient       =   1U <<  10U,

    /// Reserved by the ObjC runtime.
    _ObjCReserved        = 0xFFFF0000U,
  };

  int_type Data;
  
  constexpr ProtocolDescriptorFlags(int_type Data) : Data(Data) {}
public:
  constexpr ProtocolDescriptorFlags() : Data(0) {}
  constexpr ProtocolDescriptorFlags withSwift(bool s) const {
    return ProtocolDescriptorFlags((Data & ~IsSwift) | (s ? IsSwift : 0));
  }
  constexpr ProtocolDescriptorFlags withClassConstraint(
                                              ProtocolClassConstraint c) const {
    return ProtocolDescriptorFlags((Data & ~ClassConstraint)
                                     | (bool(c) ? ClassConstraint : 0));
  }
  constexpr ProtocolDescriptorFlags withDispatchStrategy(
                                             ProtocolDispatchStrategy s) const {
    return ProtocolDescriptorFlags((Data & ~DispatchStrategyMask)
                                     | (int_type(s) << DispatchStrategyShift));
  }
  constexpr ProtocolDescriptorFlags
  withSpecialProtocol(SpecialProtocol sp) const {
    return ProtocolDescriptorFlags((Data & ~SpecialProtocolMask)
                                     | (int_type(sp) << SpecialProtocolShift));
  }
  constexpr ProtocolDescriptorFlags withResilient(bool s) const {
    return ProtocolDescriptorFlags((Data & ~IsResilient) | (s ? IsResilient : 0));
  }
  
  /// Was the protocol defined in Swift 1 or 2?
  bool isSwift() const { return Data & IsSwift; }

  /// Is the protocol class-constrained?
  ProtocolClassConstraint getClassConstraint() const {
    return ProtocolClassConstraint(bool(Data & ClassConstraint));
  }
  
  /// What dispatch strategy does this protocol use?
  ProtocolDispatchStrategy getDispatchStrategy() const {
    return ProtocolDispatchStrategy((Data & DispatchStrategyMask)
                                      >> DispatchStrategyShift);
  }
  
  /// Does the protocol require a witness table for method dispatch?
  bool needsWitnessTable() const {
    return needsWitnessTable(getDispatchStrategy());
  }
  
  static bool needsWitnessTable(ProtocolDispatchStrategy strategy) {
    switch (strategy) {
    case ProtocolDispatchStrategy::ObjC:
    case ProtocolDispatchStrategy::Empty:
      return false;
    case ProtocolDispatchStrategy::Swift:
      return true;
    }

    swift_runtime_unreachable("Unhandled ProtocolDispatchStrategy in switch.");
  }
  
  /// Return the identifier if this is a special runtime-known protocol.
  SpecialProtocol getSpecialProtocol() const {
    return SpecialProtocol(uint8_t((Data & SpecialProtocolMask)
                                 >> SpecialProtocolShift));
  }
  
  /// Can new requirements with default witnesses be added resiliently?
  bool isResilient() const { return Data & IsResilient; }

  int_type getIntValue() const {
    return Data;
  }
};

/// Flags in an existential type metadata record.
class ExistentialTypeFlags {
  typedef size_t int_type;
  enum : int_type {
    NumWitnessTablesMask  = 0x00FFFFFFU,
    ClassConstraintMask   = 0x80000000U,
    SpecialProtocolMask   = 0x7F000000U,
    SpecialProtocolShift  = 24U,
  };
  int_type Data;

  constexpr ExistentialTypeFlags(int_type Data) : Data(Data) {}
public:
  constexpr ExistentialTypeFlags() : Data(0) {}
  constexpr ExistentialTypeFlags withNumWitnessTables(unsigned numTables) const {
    return ExistentialTypeFlags((Data & ~NumWitnessTablesMask) | numTables);
  }
  constexpr ExistentialTypeFlags
  withClassConstraint(ProtocolClassConstraint c) const {
    return ExistentialTypeFlags((Data & ~ClassConstraintMask)
                                  | (bool(c) ? ClassConstraintMask : 0));
  }
  constexpr ExistentialTypeFlags
  withSpecialProtocol(SpecialProtocol sp) const {
    return ExistentialTypeFlags((Data & ~SpecialProtocolMask)
                                  | (int_type(sp) << SpecialProtocolShift));
  }
  
  unsigned getNumWitnessTables() const {
    return Data & NumWitnessTablesMask;
  }
  
  ProtocolClassConstraint getClassConstraint() const {
    return ProtocolClassConstraint(bool(Data & ClassConstraintMask));
  }
  
  /// Return whether this existential type represents an uncomposed special
  /// protocol.
  SpecialProtocol getSpecialProtocol() const {
    return SpecialProtocol(uint8_t((Data & SpecialProtocolMask)
                                     >> SpecialProtocolShift));
  }
  
  int_type getIntValue() const {
    return Data;
  }
};

/// Convention values for function type metadata.
enum class FunctionMetadataConvention: uint8_t {
  Swift = 0,
  Block = 1,
  Thin = 2,
  CFunctionPointer = 3,
};

/// Flags in a function type metadata record.
template <typename int_type>
class TargetFunctionTypeFlags {
  enum : int_type {
    NumArgumentsMask = 0x00FFFFFFU,
    ConventionMask   = 0x0F000000U,
    ConventionShift  = 24U,
    ThrowsMask       = 0x10000000U,
  };
  int_type Data;
  
  constexpr TargetFunctionTypeFlags(int_type Data) : Data(Data) {}
public:
  constexpr TargetFunctionTypeFlags() : Data(0) {}

  constexpr TargetFunctionTypeFlags withNumArguments(unsigned numArguments) const {
    return TargetFunctionTypeFlags((Data & ~NumArgumentsMask) | numArguments);
  }
  
  constexpr TargetFunctionTypeFlags<int_type>
  withConvention(FunctionMetadataConvention c) const {
    return TargetFunctionTypeFlags((Data & ~ConventionMask)
                             | (int_type(c) << ConventionShift));
  }
  
  constexpr TargetFunctionTypeFlags<int_type>
  withThrows(bool throws) const {
    return TargetFunctionTypeFlags<int_type>((Data & ~ThrowsMask) |
                                             (throws ? ThrowsMask : 0));
  }
  
  unsigned getNumArguments() const {
    return Data & NumArgumentsMask;
  }
  
  FunctionMetadataConvention getConvention() const {
    return FunctionMetadataConvention((Data&ConventionMask) >> ConventionShift);
  }
  
  bool throws() const {
    return bool(Data & ThrowsMask);
  }
  
  int_type getIntValue() const {
    return Data;
  }
  
  static TargetFunctionTypeFlags<int_type> fromIntValue(int_type Data) {
    return TargetFunctionTypeFlags(Data);
  }
  
  bool operator==(TargetFunctionTypeFlags<int_type> other) const {
    return Data == other.Data;
  }
  bool operator!=(TargetFunctionTypeFlags<int_type> other) const {
    return Data != other.Data;
  }
};
using FunctionTypeFlags = TargetFunctionTypeFlags<size_t>;

/// Field types and flags as represented in a nominal type's field/case type
/// vector.
class FieldType {
  typedef uintptr_t int_type;
  // Type metadata is always at least pointer-aligned, so we get at least two
  // low bits to stash flags. We could use three low bits on 64-bit, and maybe
  // some high bits as well.
  enum : int_type {
    Indirect = 1,
    Weak = 2,

    TypeMask = ((uintptr_t)-1) & ~(alignof(void*) - 1),
  };
  int_type Data;

  constexpr FieldType(int_type Data) : Data(Data) {}
public:
  constexpr FieldType() : Data(0) {}
  FieldType withType(const Metadata *T) const {
    return FieldType((Data & ~TypeMask) | (uintptr_t)T);
  }

  constexpr FieldType withIndirect(bool indirect) const {
    return FieldType((Data & ~Indirect)
                     | (indirect ? Indirect : 0));
  }

  constexpr FieldType withWeak(bool weak) const {
    return FieldType((Data & ~Weak)
                     | (weak ? Weak : 0));
  }

  bool isIndirect() const {
    return bool(Data & Indirect);
  }

  bool isWeak() const {
    return bool(Data & Weak);
  }

  const Metadata *getType() const {
    return (const Metadata *)(Data & TypeMask);
  }

  int_type getIntValue() const {
    return Data;
  }
};

}

#endif /* SWIFT_ABI_METADATAVALUES_H */
