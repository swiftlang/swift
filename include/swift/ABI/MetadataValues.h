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
#include "swift/Basic/LLVM.h"
#include "swift/Basic/FlagSet.h"
#include "swift/Runtime/Unreachable.h"

#include <stdlib.h>
#include <stdint.h>

namespace swift {

enum {
  /// The number of words (pointers) in a value buffer.
  NumWords_ValueBuffer = 3,

  /// The number of words in a metadata completion context.
  NumWords_MetadataCompletionContext = 4,

  /// The number of words in a yield-once coroutine buffer.
  NumWords_YieldOnceBuffer = 4,

  /// The number of words in a yield-many coroutine buffer.
  NumWords_YieldManyBuffer = 8,
};

struct InProcess;
template <typename Runtime> struct TargetMetadata;
using Metadata = TargetMetadata<InProcess>;

/// Non-type metadata kinds have this bit set.
const unsigned MetadataKindIsNonType = 0x400;

/// Non-heap metadata kinds have this bit set.
const unsigned MetadataKindIsNonHeap = 0x200;

// The above two flags are negative because the "class" kind has to be zero,
// and class metadata is both type and heap metadata.

/// Runtime-private metadata has this bit set. The compiler must not statically
/// generate metadata objects with these kinds, and external tools should not
/// rely on the stability of these values or the precise binary layout of
/// their associated data structures.
const unsigned MetadataKindIsRuntimePrivate = 0x100;

/// Kinds of Swift metadata records.  Some of these are types, some
/// aren't.
enum class MetadataKind : uint32_t {
#define METADATAKIND(name, value) name = value,
#define ABSTRACTMETADATAKIND(name, start, end)                                 \
  name##_Start = start, name##_End = end,
#include "MetadataKind.def"
  
  /// The largest possible non-isa-pointer metadata kind value.
  ///
  /// This is included in the enumeration to prevent against attempts to
  /// exhaustively match metadata kinds. Future Swift runtimes or compilers
  /// may introduce new metadata kinds, so for forward compatibility, the
  /// runtime must tolerate metadata with unknown kinds.
  /// This specific value is not mapped to a valid metadata kind at this time,
  /// however.
  LastEnumerated = 0x7FF,
};

const unsigned LastEnumeratedMetadataKind =
  (unsigned)MetadataKind::LastEnumerated;

inline bool isHeapMetadataKind(MetadataKind k) {
  return !((uint32_t)k & MetadataKindIsNonHeap);
}
inline bool isTypeMetadataKind(MetadataKind k) {
  return !((uint32_t)k & MetadataKindIsNonType);
}
inline bool isRuntimePrivateMetadataKind(MetadataKind k) {
  return (uint32_t)k & MetadataKindIsRuntimePrivate;
}

/// Try to translate the 'isa' value of a type/heap metadata into a value
/// of the MetadataKind enum.
inline MetadataKind getEnumeratedMetadataKind(uint64_t kind) {
  if (kind > LastEnumeratedMetadataKind)
    return MetadataKind::Class;
  return MetadataKind(kind);
}

StringRef getStringForMetadataKind(MetadataKind kind);

/// Kinds of Swift nominal type descriptor records.
enum class NominalTypeKind : uint32_t {
#define NOMINALTYPEMETADATAKIND(name, value) name = value,
#include "MetadataKind.def"
};

/// The maximum supported type alignment.
const size_t MaximumAlignment = 16;

/// Flags stored in the value-witness table.
template <typename int_type>
class TargetValueWitnessFlags {
public:
  // The polarity of these bits is chosen so that, when doing struct layout, the
  // flags of the field types can be mostly bitwise-or'ed together to derive the
  // flags for the struct. (The "non-inline" and "has-extra-inhabitants" bits
  // still require additional fixup.)
  enum : int_type {
    AlignmentMask =       0x000000FF,
    IsNonPOD =            0x00010000,
    IsNonInline =         0x00020000,
    HasExtraInhabitants = 0x00040000,
    HasSpareBits =        0x00080000,
    IsNonBitwiseTakable = 0x00100000,
    HasEnumWitnesses =    0x00200000,
    Incomplete =          0x00400000,

    // Everything else is reserved.
  };

private:
  int_type Data;

public:
  explicit constexpr TargetValueWitnessFlags(int_type data) : Data(data) {}
  constexpr TargetValueWitnessFlags() : Data(0) {}

  /// The required alignment of the first byte of an object of this
  /// type, expressed as a mask of the low bits that must not be set
  /// in the pointer.
  ///
  /// This representation can be easily converted to the 'alignof'
  /// result by merely adding 1, but it is more directly useful for
  /// performing dynamic structure layouts, and it grants an
  /// additional bit of precision in a compact field without needing
  /// to switch to an exponent representation.
  ///
  /// For example, if the type needs to be 8-byte aligned, the
  /// appropriate alignment mask should be 0x7.
  size_t getAlignmentMask() const {
    return (Data & AlignmentMask);
  }
  constexpr TargetValueWitnessFlags withAlignmentMask(size_t alignMask) const {
    return TargetValueWitnessFlags((Data & ~AlignmentMask) | alignMask);
  }

  size_t getAlignment() const { return getAlignmentMask() + 1; }
  constexpr TargetValueWitnessFlags withAlignment(size_t alignment) const {
    return withAlignmentMask(alignment - 1);
  }

  /// True if the type requires out-of-line allocation of its storage.
  /// This can be the case because the value requires more storage or if it is
  /// not bitwise takable.
  bool isInlineStorage() const { return !(Data & IsNonInline); }
  constexpr TargetValueWitnessFlags withInlineStorage(bool isInline) const {
    return TargetValueWitnessFlags((Data & ~IsNonInline) |
                                   (isInline ? 0 : IsNonInline));
  }

  /// True if values of this type can be copied with memcpy and
  /// destroyed with a no-op.
  bool isPOD() const { return !(Data & IsNonPOD); }
  constexpr TargetValueWitnessFlags withPOD(bool isPOD) const {
    return TargetValueWitnessFlags((Data & ~IsNonPOD) |
                                   (isPOD ? 0 : IsNonPOD));
  }

  /// True if values of this type can be taken with memcpy. Unlike C++ 'move',
  /// 'take' is a destructive operation that invalidates the source object, so
  /// most types can be taken with a simple bitwise copy. Only types with side
  /// table references, like @weak references, or types with opaque value
  /// semantics, like imported C++ types, are not bitwise-takable.
  bool isBitwiseTakable() const { return !(Data & IsNonBitwiseTakable); }
  constexpr TargetValueWitnessFlags withBitwiseTakable(bool isBT) const {
    return TargetValueWitnessFlags((Data & ~IsNonBitwiseTakable) |
                                   (isBT ? 0 : IsNonBitwiseTakable));
  }
  /// True if this type's binary representation has extra inhabitants, that is,
  /// bit patterns that do not form valid values of the type.
  ///
  /// If true, then the extra inhabitant value witness table entries are
  /// available in this type's value witness table.
  bool hasExtraInhabitants() const { return Data & HasExtraInhabitants; }
  /// True if this type's binary representation is that of an enum, and the
  /// enum value witness table entries are available in this type's value
  /// witness table.
  bool hasEnumWitnesses() const { return Data & HasEnumWitnesses; }
  constexpr TargetValueWitnessFlags
  withExtraInhabitants(bool hasExtraInhabitants) const {
    return TargetValueWitnessFlags((Data & ~HasExtraInhabitants) |
                               (hasExtraInhabitants ? HasExtraInhabitants : 0));
  }
  constexpr TargetValueWitnessFlags
  withEnumWitnesses(bool hasEnumWitnesses) const {
    return TargetValueWitnessFlags((Data & ~HasEnumWitnesses) |
                                   (hasEnumWitnesses ? HasEnumWitnesses : 0));
  }

  /// True if the type with this value-witness table is incomplete,
  /// meaning that its external layout (size, etc.) is meaningless
  /// pending completion of the metadata layout.
  bool isIncomplete() const { return Data & Incomplete; }
  constexpr TargetValueWitnessFlags
  withIncomplete(bool isIncomplete) const {
    return TargetValueWitnessFlags((Data & ~Incomplete) |
                                   (isIncomplete ? Incomplete : 0));
  }

  constexpr int_type getOpaqueValue() const { return Data; }
  static constexpr TargetValueWitnessFlags getFromOpaqueValue(int_type data) {
    return TargetValueWitnessFlags(data);
  }
};
using ValueWitnessFlags = TargetValueWitnessFlags<size_t>;

/// Flags stored in a value-witness table with extra inhabitants.
template <typename int_type>
class TargetExtraInhabitantFlags {
public:
  enum : int_type {
    NumExtraInhabitantsMask = 0x7FFFFFFFU,
    ExtraInhabitantFlags
  };
  int_type Data;

  constexpr TargetExtraInhabitantFlags(int_type data) : Data(data) {}

public:
  constexpr TargetExtraInhabitantFlags() : Data(0) {}
  /// The number of extra inhabitants in the type's representation.
  int getNumExtraInhabitants() const { return Data & NumExtraInhabitantsMask; }

  constexpr TargetExtraInhabitantFlags
  withNumExtraInhabitants(unsigned numExtraInhabitants) const {
    return TargetExtraInhabitantFlags((Data & ~NumExtraInhabitantsMask) |
                                      numExtraInhabitants);
  }

  constexpr int_type getOpaqueValue() const { return Data; }
  static constexpr TargetExtraInhabitantFlags getFromOpaqueValue(int_type data){
    return TargetExtraInhabitantFlags(data);
  }
};
using ExtraInhabitantFlags =
  TargetExtraInhabitantFlags<size_t>;

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
/// These flags are valid only when isTypeMetadata().
/// When !isTypeMetadata() these flags will collide with other Swift ABIs.
enum class ClassFlags : uint32_t {
  /// Is this a Swift class from the Darwin pre-stable ABI?
  /// This bit is clear in stable ABI Swift classes.
  /// The Objective-C runtime also reads this bit.
  IsSwiftPreStableABI = 0x1,

  /// Does this class use Swift refcounting?
  UsesSwiftRefcounting = 0x2,

  /// Has this class a custom name, specified with the @objc attribute?
  HasCustomObjCName = 0x4
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

/// Flags that go in a MethodDescriptor structure.
class MethodDescriptorFlags {
public:
  typedef uint32_t int_type;
  enum class Kind {
    Method,
    Init,
    Getter,
    Setter,
    ModifyCoroutine,
    ReadCoroutine,
  };

private:
  enum : int_type {
    KindMask = 0x0F,                // 16 kinds should be enough for anybody
    IsInstanceMask = 0x10,
    IsDynamicMask = 0x20,
  };

  int_type Value;

public:
  MethodDescriptorFlags(Kind kind) : Value(unsigned(kind)) {}

  MethodDescriptorFlags withIsInstance(bool isInstance) const {
    auto copy = *this;
    if (isInstance) {
      copy.Value |= IsInstanceMask;
    } else {
      copy.Value &= ~IsInstanceMask;
    }
    return copy;
  }

  MethodDescriptorFlags withIsDynamic(bool isDynamic) const {
    auto copy = *this;
    if (isDynamic)
      copy.Value |= IsDynamicMask;
    else
      copy.Value &= ~IsDynamicMask;
    return copy;
  }

  Kind getKind() const { return Kind(Value & KindMask); }

  /// Is the method marked 'dynamic'?
  bool isDynamic() const { return Value & IsDynamicMask; }

  /// Is the method an instance member?
  ///
  /// Note that 'init' is not considered an instance member.
  bool isInstance() const { return Value & IsInstanceMask; }

  int_type getIntValue() const { return Value; }
};

enum : unsigned {
  /// Number of words reserved in generic metadata patterns.
  NumGenericMetadataPrivateDataWords = 16,
};

/// Kinds of type metadata/protocol conformance records.
enum class TypeReferenceKind : unsigned {
  /// The conformance is for a nominal type referenced directly;
  /// getNominalTypeDescriptor() points to the nominal type descriptor.
  DirectNominalTypeDescriptor = 0x00,

  /// The conformance is for a nominal type referenced indirectly;
  /// getNominalTypeDescriptor() points to the nominal type descriptor.
  IndirectNominalTypeDescriptor = 0x01,

  /// The conformance is for an Objective-C class that should be looked up
  /// by class name.
  DirectObjCClassName = 0x02,

  /// The conformance is for an Objective-C class that has no nominal type
  /// descriptor.
  /// getIndirectObjCClass() points to a variable that contains the pointer to
  /// the class object, which then requires a runtime call to get metadata.
  ///
  /// On platforms without Objective-C interoperability, this case is
  /// unused.
  IndirectObjCClass = 0x03,

  // We only reserve three bits for this in the various places we store it.

  First_Kind = DirectNominalTypeDescriptor,
  Last_Kind = IndirectObjCClass,
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
  /// The Error protocol.
  Error = 1,
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

#ifndef NDEBUG
  LLVM_ATTRIBUTE_DEPRECATED(void dump() const LLVM_ATTRIBUTE_USED,
                            "Only for use in the debugger");
#endif
};

/// Flags that go in a ProtocolRequirement structure.
class ProtocolRequirementFlags {
public:
  typedef uint32_t int_type;
  enum class Kind {
    BaseProtocol,
    Method,
    Init,
    Getter,
    Setter,
    ReadCoroutine,
    ModifyCoroutine,
    AssociatedTypeAccessFunction,
    AssociatedConformanceAccessFunction,
  };

private:
  enum : int_type {
    KindMask = 0x0F,                // 16 kinds should be enough for anybody
    IsInstanceMask = 0x10,
  };

  int_type Value;

public:
  ProtocolRequirementFlags(Kind kind) : Value(unsigned(kind)) {}

  ProtocolRequirementFlags withIsInstance(bool isInstance) const {
    auto copy = *this;
    if (isInstance) {
      copy.Value |= IsInstanceMask;
    } else {
      copy.Value &= ~IsInstanceMask;
    }
    return copy;
  }

  Kind getKind() const { return Kind(Value & KindMask); }

  /// Is the method an instance member?
  ///
  /// Note that 'init' is not considered an instance member.
  bool isInstance() const { return Value & IsInstanceMask; }

  int_type getIntValue() const { return Value; }

  enum : uintptr_t {
    /// Bit used to indicate that an associated type witness is a pointer to
    /// a mangled name (vs. a pointer to metadata).
    AssociatedTypeMangledNameBit = 0x01,
  };

  enum : uint8_t {
    /// Prefix byte used to identify an associated type whose mangled name
    /// is relative to the protocol's context rather than the conforming
    /// type's context.
    AssociatedTypeInProtocolContextByte = 0xFF
  };
};

/// Flags that go in a TargetConformanceDescriptor structure.
class ConformanceFlags {
public:
  typedef uint32_t int_type;

  enum class ConformanceKind {
    /// A direct reference to a protocol witness table.
    WitnessTable,
    /// A function pointer that can be called to access the protocol witness
    /// table.
    WitnessTableAccessor,
    /// A function pointer that can be called to access the protocol witness
    /// table whose conformance is conditional on additional requirements that
    /// must first be evaluated and then provided to the accessor function.
    ConditionalWitnessTableAccessor,

    First_Kind = WitnessTable,
    Last_Kind = ConditionalWitnessTableAccessor,
  };

private:
  enum : int_type {
    ConformanceKindMask = 0x07,      // 8 conformance kinds

    TypeMetadataKindMask = 0x7 << 3, // 8 type reference kinds
    TypeMetadataKindShift = 3,

    IsRetroactiveMask = 0x01 << 6,
    IsSynthesizedNonUniqueMask = 0x01 << 7,

    NumConditionalRequirementsMask = 0xFF << 8,
    NumConditionalRequirementsShift = 8,

    HasResilientWitnessesMask = 0x01 << 16,
  };

  int_type Value;

public:
  ConformanceFlags(int_type value = 0) : Value(value) {}

  ConformanceFlags withConformanceKind(ConformanceKind kind) const {
    return ConformanceFlags((Value & ~ConformanceKindMask) | int_type(kind));
  }

  ConformanceFlags withTypeReferenceKind(TypeReferenceKind kind) const {
    return ConformanceFlags((Value & ~TypeMetadataKindMask)
                            | (int_type(kind) << TypeMetadataKindShift));
  }

  ConformanceFlags withIsRetroactive(bool isRetroactive) const {
    return ConformanceFlags((Value & ~IsRetroactiveMask)
                            | (isRetroactive? IsRetroactiveMask : 0));
  }

  ConformanceFlags withIsSynthesizedNonUnique(
                                          bool isSynthesizedNonUnique) const {
    return ConformanceFlags(
                  (Value & ~IsSynthesizedNonUniqueMask)
                  | (isSynthesizedNonUnique ? IsSynthesizedNonUniqueMask : 0));
  }

  ConformanceFlags withNumConditionalRequirements(unsigned n) const {
    return ConformanceFlags((Value & ~NumConditionalRequirementsMask)
                            | (n << NumConditionalRequirementsShift));
  }

  ConformanceFlags withHasResilientWitnesses(bool hasResilientWitnesses) const {
    return ConformanceFlags((Value & ~HasResilientWitnessesMask)
                            | (hasResilientWitnesses? HasResilientWitnessesMask
                                                    : 0));
  }

  /// Retrieve the conformance kind.
  ConformanceKind getConformanceKind() const {
    return ConformanceKind(Value & ConformanceKindMask);
  }

  /// Retrieve the type reference kind kind.
  TypeReferenceKind getTypeReferenceKind() const {
    return TypeReferenceKind(
                      (Value & TypeMetadataKindMask) >> TypeMetadataKindShift);
  }

  /// Is the conformance "retroactive"?
  ///
  /// A conformance is retroactive when it occurs in a module that is
  /// neither the module in which the protocol is defined nor the module
  /// in which the conforming type is defined. With retroactive conformance,
  /// it is possible to detect a conflict at run time.
  bool isRetroactive() const { return Value & IsRetroactiveMask; }

  /// Is the conformance synthesized in a non-unique manner?
  ///
  /// The Swift compiler will synthesize conformances on behalf of some
  /// imported entities (e.g., C typedefs with the swift_wrapper attribute).
  /// Such conformances are retroactive by nature, but the presence of multiple
  /// such conformances is not a conflict because all synthesized conformances
  /// will be equivalent.
  bool isSynthesizedNonUnique() const {
    return Value & IsSynthesizedNonUniqueMask;
  }

  /// Retrieve the # of conditional requirements.
  unsigned getNumConditionalRequirements() const {
    return (Value & NumConditionalRequirementsMask)
              >> NumConditionalRequirementsShift;
  }

  /// Whether this conformance has any resilient witnesses.
  bool hasResilientWitnesses() const {
    return Value & HasResilientWitnessesMask;
  }

  int_type getIntValue() const { return Value; }
};

/// Flags in an existential type metadata record.
class ExistentialTypeFlags {
public:
  typedef uint32_t int_type;

private:
  enum : int_type {
    NumWitnessTablesMask  = 0x00FFFFFFU,
    ClassConstraintMask   = 0x80000000U,
    HasSuperclassMask     = 0x40000000U,
    SpecialProtocolMask   = 0x3F000000U,
    SpecialProtocolShift  = 24U,
  };
  int_type Data;

public:
  constexpr ExistentialTypeFlags(int_type Data) : Data(Data) {}
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
  withHasSuperclass(bool hasSuperclass) const {
    return ExistentialTypeFlags((Data & ~HasSuperclassMask)
                                  | (hasSuperclass ? HasSuperclassMask : 0));
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

  bool hasSuperclassConstraint() const {
    return bool(Data & HasSuperclassMask);
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
  // If we were ever to run out of space for function flags (8 bits)
  // one of the flag bits could be used to identify that the rest of
  // the flags is going to be stored somewhere else in the metadata.
  enum : int_type {
    NumParametersMask = 0x0000FFFFU,
    ConventionMask    = 0x00FF0000U,
    ConventionShift   = 16U,
    ThrowsMask        = 0x01000000U,
    ParamFlagsMask    = 0x02000000U,
    EscapingMask      = 0x04000000U,
  };
  int_type Data;
  
  constexpr TargetFunctionTypeFlags(int_type Data) : Data(Data) {}
public:
  constexpr TargetFunctionTypeFlags() : Data(0) {}

  constexpr TargetFunctionTypeFlags
  withNumParameters(unsigned numParams) const {
    return TargetFunctionTypeFlags((Data & ~NumParametersMask) | numParams);
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

  constexpr TargetFunctionTypeFlags<int_type>
  withParameterFlags(bool hasFlags) const {
    return TargetFunctionTypeFlags<int_type>((Data & ~ParamFlagsMask) |
                                             (hasFlags ? ParamFlagsMask : 0));
  }

  constexpr TargetFunctionTypeFlags<int_type>
  withEscaping(bool isEscaping) const {
    return TargetFunctionTypeFlags<int_type>((Data & ~EscapingMask) |
                                             (isEscaping ? EscapingMask : 0));
  }

  unsigned getNumParameters() const { return Data & NumParametersMask; }

  FunctionMetadataConvention getConvention() const {
    return FunctionMetadataConvention((Data&ConventionMask) >> ConventionShift);
  }
  
  bool throws() const {
    return bool(Data & ThrowsMask);
  }

  bool isEscaping() const {
    return bool (Data & EscapingMask);
  }

  bool hasParameterFlags() const { return bool(Data & ParamFlagsMask); }

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

template <typename int_type>
class TargetParameterTypeFlags {
  enum : int_type { ValueOwnershipMask = 0x7F, VariadicMask = 0x80 };
  int_type Data;

  constexpr TargetParameterTypeFlags(int_type Data) : Data(Data) {}

public:
  constexpr TargetParameterTypeFlags() : Data(0) {}

  constexpr TargetParameterTypeFlags<int_type>
  withValueOwnership(ValueOwnership ownership) const {
    return TargetParameterTypeFlags<int_type>((Data & ~ValueOwnershipMask) |
                                              (int_type)ownership);
  }

  constexpr TargetParameterTypeFlags<int_type>
  withVariadic(bool isVariadic) const {
    return TargetParameterTypeFlags<int_type>((Data & ~VariadicMask) |
                                              (isVariadic ? VariadicMask : 0));
  }

  bool isNone() const { return Data == 0; }
  bool isVariadic() const { return Data & VariadicMask; }

  ValueOwnership getValueOwnership() const {
    return (ValueOwnership)(Data & ValueOwnershipMask);
  }

  int_type getIntValue() const { return Data; }

  static TargetParameterTypeFlags<int_type> fromIntValue(int_type Data) {
    return TargetParameterTypeFlags(Data);
  }

  bool operator==(TargetParameterTypeFlags<int_type> other) const {
    return Data == other.Data;
  }
  bool operator!=(TargetParameterTypeFlags<int_type> other) const {
    return Data != other.Data;
  }
};
using ParameterFlags = TargetParameterTypeFlags<uint32_t>;

template <typename int_type>
class TargetTupleTypeFlags {
  enum : int_type {
    NumElementsMask = 0x0000FFFFU,
    NonConstantLabelsMask = 0x00010000U,
  };
  int_type Data;

public:
  constexpr TargetTupleTypeFlags() : Data(0) {}
  constexpr TargetTupleTypeFlags(int_type Data) : Data(Data) {}

  constexpr TargetTupleTypeFlags
  withNumElements(unsigned numElements) const {
    return TargetTupleTypeFlags((Data & ~NumElementsMask) | numElements);
  }

  constexpr TargetTupleTypeFlags<int_type> withNonConstantLabels(
                                             bool hasNonConstantLabels) const {
    return TargetTupleTypeFlags<int_type>(
                        (Data & ~NonConstantLabelsMask) |
                          (hasNonConstantLabels ? NonConstantLabelsMask : 0));
  }

  unsigned getNumElements() const { return Data & NumElementsMask; }

  bool hasNonConstantLabels() const { return Data & NonConstantLabelsMask; }

  int_type getIntValue() const { return Data; }

  static TargetTupleTypeFlags<int_type> fromIntValue(int_type Data) {
    return TargetTupleTypeFlags(Data);
  }

  bool operator==(TargetTupleTypeFlags<int_type> other) const {
    return Data == other.Data;
  }
  bool operator!=(TargetTupleTypeFlags<int_type> other) const {
    return Data != other.Data;
  }
};
using TupleTypeFlags = TargetTupleTypeFlags<size_t>;

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

/// Flags for exclusivity-checking operations.
enum class ExclusivityFlags : uintptr_t {
  Read             = 0x0,
  Modify           = 0x1,
  // ActionMask can grow without breaking the ABI because the runtime controls
  // how these flags are encoded in the "value buffer". However, any additional
  // actions must be compatible with the original behavior for the old, smaller
  // ActionMask (older runtimes will continue to treat them as either a simple
  // Read or Modify).
  ActionMask       = 0x1,

  // The runtime should track this access to check against subsequent accesses.
  Tracking         = 0x20
};
static inline ExclusivityFlags operator|(ExclusivityFlags lhs,
                                         ExclusivityFlags rhs) {
  return ExclusivityFlags(uintptr_t(lhs) | uintptr_t(rhs));
}
static inline ExclusivityFlags &operator|=(ExclusivityFlags &lhs,
                                           ExclusivityFlags rhs) {
  return (lhs = (lhs | rhs));
}
static inline ExclusivityFlags getAccessAction(ExclusivityFlags flags) {
  return ExclusivityFlags(uintptr_t(flags)
                        & uintptr_t(ExclusivityFlags::ActionMask));
}
static inline bool isTracking(ExclusivityFlags flags) {
  return uintptr_t(flags) & uintptr_t(ExclusivityFlags::Tracking);
}

/// Flags for struct layout.
enum class StructLayoutFlags : uintptr_t {
  /// Reserve space for 256 layout algorithms.
  AlgorithmMask     = 0xff,

  /// The ABI baseline algorithm, i.e. the algorithm implemented in Swift 5.
  Swift5Algorithm   = 0x00,

  /// Is the value-witness table mutable in place, or does layout need to
  /// clone it?
  IsVWTMutable      = 0x100,
};
static inline StructLayoutFlags operator|(StructLayoutFlags lhs,
                                          StructLayoutFlags rhs) {
  return StructLayoutFlags(uintptr_t(lhs) | uintptr_t(rhs));
}
static inline StructLayoutFlags &operator|=(StructLayoutFlags &lhs,
                                            StructLayoutFlags rhs) {
  return (lhs = (lhs | rhs));
}
static inline StructLayoutFlags getLayoutAlgorithm(StructLayoutFlags flags) {
  return StructLayoutFlags(uintptr_t(flags)
                             & uintptr_t(StructLayoutFlags::AlgorithmMask));
}
static inline bool isValueWitnessTableMutable(StructLayoutFlags flags) {
  return uintptr_t(flags) & uintptr_t(StructLayoutFlags::IsVWTMutable);
}

/// Flags for class layout.
enum class ClassLayoutFlags : uintptr_t {
  /// Reserve space for 256 layout algorithms.
  AlgorithmMask     = 0xff,

  /// The ABI baseline algorithm, i.e. the algorithm implemented in Swift 5.
  Swift5Algorithm   = 0x00,

  /// If true, the vtable for this class and all of its superclasses was emitted
  /// statically in the class metadata. If false, the superclass vtable is
  /// copied from superclass metadata, and the immediate class vtable is
  /// initialized from the type context descriptor.
  HasStaticVTable   = 0x100,
};
static inline ClassLayoutFlags operator|(ClassLayoutFlags lhs,
                                         ClassLayoutFlags rhs) {
  return ClassLayoutFlags(uintptr_t(lhs) | uintptr_t(rhs));
}
static inline ClassLayoutFlags &operator|=(ClassLayoutFlags &lhs,
                                           ClassLayoutFlags rhs) {
  return (lhs = (lhs | rhs));
}
static inline ClassLayoutFlags getLayoutAlgorithm(ClassLayoutFlags flags) {
  return ClassLayoutFlags(uintptr_t(flags)
                             & uintptr_t(ClassLayoutFlags::AlgorithmMask));
}
static inline bool hasStaticVTable(ClassLayoutFlags flags) {
  return uintptr_t(flags) & uintptr_t(ClassLayoutFlags::HasStaticVTable);
}

/// Flags for enum layout.
enum class EnumLayoutFlags : uintptr_t {
  /// Reserve space for 256 layout algorithms.
  AlgorithmMask     = 0xff,

  /// The ABI baseline algorithm, i.e. the algorithm implemented in Swift 5.
  Swift5Algorithm   = 0x00,

  /// Is the value-witness table mutable in place, or does layout need to
  /// clone it?
  IsVWTMutable      = 0x100,
};
static inline EnumLayoutFlags operator|(EnumLayoutFlags lhs,
                                        EnumLayoutFlags rhs) {
  return EnumLayoutFlags(uintptr_t(lhs) | uintptr_t(rhs));
}
static inline EnumLayoutFlags &operator|=(EnumLayoutFlags &lhs,
                                          EnumLayoutFlags rhs) {
  return (lhs = (lhs | rhs));
}
static inline EnumLayoutFlags getLayoutAlgorithm(EnumLayoutFlags flags) {
  return EnumLayoutFlags(uintptr_t(flags)
                           & uintptr_t(EnumLayoutFlags::AlgorithmMask));
}
static inline bool isValueWitnessTableMutable(EnumLayoutFlags flags) {
  return uintptr_t(flags) & uintptr_t(EnumLayoutFlags::IsVWTMutable);
}

/// The number of arguments that will be passed directly to a generic
/// nominal type access function. The remaining arguments (if any) will be
/// passed as an array. That array has enough storage for all of the arguments,
/// but only fills in the elements not passed directly. The callee may
/// mutate the array to fill in the direct arguments.
constexpr unsigned NumDirectGenericTypeMetadataAccessFunctionArgs = 3;

/// The offset (in pointers) to the first requirement in a witness table.
constexpr unsigned WitnessTableFirstRequirementOffset = 1;

/// Kinds of context descriptor.
enum class ContextDescriptorKind : uint8_t {
  /// This context descriptor represents a module.
  Module = 0,
  
  /// This context descriptor represents an extension.
  Extension = 1,
  
  /// This context descriptor represents an anonymous possibly-generic context
  /// such as a function body.
  Anonymous = 2,

  /// This context descriptor represents a protocol context.
  Protocol = 3,

  /// First kind that represents a type of any sort.
  Type_First = 16,
  
  /// This context descriptor represents a class.
  Class = Type_First,
  
  /// This context descriptor represents a struct.
  Struct = Type_First + 1,
  
  /// This context descriptor represents an enum.
  Enum = Type_First + 2,
  
  /// Last kind that represents a type of any sort.
  Type_Last = 31,
};

/// Common flags stored in the first 32-bit word of any context descriptor.
struct ContextDescriptorFlags {
private:
  uint32_t Value;

  explicit constexpr ContextDescriptorFlags(uint32_t Value)
    : Value(Value) {}
public:
  constexpr ContextDescriptorFlags() : Value(0) {}
  constexpr ContextDescriptorFlags(ContextDescriptorKind kind,
                                   bool isGeneric,
                                   bool isUnique,
                                   uint8_t version,
                                   uint16_t kindSpecificFlags)
    : ContextDescriptorFlags(ContextDescriptorFlags()
                               .withKind(kind)
                               .withGeneric(isGeneric)
                               .withUnique(isUnique)
                               .withVersion(version)
                               .withKindSpecificFlags(kindSpecificFlags))
  {}

  /// The kind of context this descriptor describes.
  constexpr ContextDescriptorKind getKind() const {
    return ContextDescriptorKind(Value & 0x1Fu);
  }
  
  /// Whether the context being described is generic.
  constexpr bool isGeneric() const {
    return (Value & 0x80u) != 0;
  }
  
  /// Whether this is a unique record describing the referenced context.
  constexpr bool isUnique() const {
    return (Value & 0x40u) != 0;
  }
  
  /// The format version of the descriptor. Higher version numbers may have
  /// additional fields that aren't present in older versions.
  constexpr uint8_t getVersion() const {
    return (Value >> 8u) & 0xFFu;
  }
  
  /// The most significant two bytes of the flags word, which can have
  /// kind-specific meaning.
  constexpr uint16_t getKindSpecificFlags() const {
    return (Value >> 16u) & 0xFFFFu;
  }
  
  constexpr ContextDescriptorFlags withKind(ContextDescriptorKind kind) const {
    return assert((uint8_t(kind) & 0x1F) == uint8_t(kind)),
      ContextDescriptorFlags((Value & 0xFFFFFFE0u) | uint8_t(kind));
  }
  
  constexpr ContextDescriptorFlags withGeneric(bool isGeneric) const {
    return ContextDescriptorFlags((Value & 0xFFFFFF7Fu)
                                  | (isGeneric ? 0x80u : 0));
  }

  constexpr ContextDescriptorFlags withUnique(bool isUnique) const {
    return ContextDescriptorFlags((Value & 0xFFFFFFBFu)
                                  | (isUnique ? 0x40u : 0));
  }

  constexpr ContextDescriptorFlags withVersion(uint8_t version) const {
    return ContextDescriptorFlags((Value & 0xFFFF00FFu) | (version << 8u));
  }

  constexpr ContextDescriptorFlags
  withKindSpecificFlags(uint16_t flags) const {
    return ContextDescriptorFlags((Value & 0xFFFFu) | (flags << 16u));
  }
  
  constexpr uint32_t getIntValue() const {
    return Value;
  }
};

/// Flags for nominal type context descriptors. These values are used as the
/// kindSpecificFlags of the ContextDescriptorFlags for the type.
class TypeContextDescriptorFlags : public FlagSet<uint16_t> {
  enum {
    // All of these values are bit offsets or widths.
    // Generic flags build upwards from 0.
    // Type-specific flags build downwards from 15.

    /// Whether there's something unusual about how the metadata is
    /// initialized.
    ///
    /// Meaningful for all type-descriptor kinds.
    MetadataInitialization = 0,
    MetadataInitialization_width = 2,

    /// Set if the type has extended import information.
    ///
    /// If true, a sequence of strings follow the null terminator in the
    /// descriptor, terminated by an empty string (i.e. by two null
    /// terminators in a row).  See TypeImportInfo for the details of
    /// these strings and the order in which they appear.
    ///
    /// Meaningful for all type-descriptor kinds.
    HasImportInfo = 2,

    // Type-specific flags:

    /// The kind of reference that this class makes to its resilient superclass
    /// descriptor.  A TypeReferenceKind.
    ///
    /// Only meaningful for class descriptors.
    Class_ResilientSuperclassReferenceKind = 9,
    Class_ResilientSuperclassReferenceKind_width = 3,

    /// Whether the immediate class members in this metadata are allocated
    /// at negative offsets.  For now, we don't use this.
    Class_AreImmediateMembersNegative = 12,

    /// Set if the context descriptor is for a class with resilient ancestry.
    ///
    /// Only meaningful for class descriptors.
    Class_HasResilientSuperclass = 13,

    /// Set if the context descriptor includes metadata for dynamically
    /// installing method overrides at metadata instantiation time.
    Class_HasOverrideTable = 14,

    /// Set if the context descriptor includes metadata for dynamically
    /// constructing a class's vtables at metadata instantiation time.
    ///
    /// Only meaningful for class descriptors.
    Class_HasVTable = 15,
  };

public:
  explicit TypeContextDescriptorFlags(uint16_t bits) : FlagSet(bits) {}
  constexpr TypeContextDescriptorFlags() {}

  enum MetadataInitializationKind {
    /// There are either no special rules for initializing the metadata
    /// or the metadata is generic.  (Genericity is set in the
    /// non-kind-specific descriptor flags.)
    NoMetadataInitialization = 0,

    /// The type requires non-trivial singleton initialization using the
    /// "in-place" code pattern.
    SingletonMetadataInitialization = 1,

    /// The type requires non-trivial singleton initialization using the
    /// "foreign" code pattern.
    ForeignMetadataInitialization = 2,

    // We only have two bits here, so if you add a third special kind,
    // include more flag bits in its out-of-line storage.
  };

  FLAGSET_DEFINE_FIELD_ACCESSORS(MetadataInitialization,
                                 MetadataInitialization_width,
                                 MetadataInitializationKind,
                                 getMetadataInitialization,
                                 setMetadataInitialization)

  bool hasSingletonMetadataInitialization() const {
    return getMetadataInitialization() == SingletonMetadataInitialization;
  }

  bool hasForeignMetadataInitialization() const {
    return getMetadataInitialization() == ForeignMetadataInitialization;
  }

  FLAGSET_DEFINE_FLAG_ACCESSORS(HasImportInfo, hasImportInfo, setHasImportInfo)

  FLAGSET_DEFINE_FLAG_ACCESSORS(Class_HasVTable,
                                class_hasVTable,
                                class_setHasVTable)
  FLAGSET_DEFINE_FLAG_ACCESSORS(Class_HasOverrideTable,
                                class_hasOverrideTable,
                                class_setHasOverrideTable)
  FLAGSET_DEFINE_FLAG_ACCESSORS(Class_HasResilientSuperclass,
                                class_hasResilientSuperclass,
                                class_setHasResilientSuperclass)
  FLAGSET_DEFINE_FLAG_ACCESSORS(Class_AreImmediateMembersNegative,
                                class_areImmediateMembersNegative,
                                class_setAreImmediateMembersNegative)

  FLAGSET_DEFINE_FIELD_ACCESSORS(Class_ResilientSuperclassReferenceKind,
                                 Class_ResilientSuperclassReferenceKind_width,
                                 TypeReferenceKind,
                                 class_getResilientSuperclassReferenceKind,
                                 class_setResilientSuperclassReferenceKind)
};

/// Flags for protocol context descriptors. These values are used as the
/// kindSpecificFlags of the ContextDescriptorFlags for the protocol.
class ProtocolContextDescriptorFlags : public FlagSet<uint16_t> {
  enum {
    /// Whether this protocol is class-constrained.
    HasClassConstraint = 0,
    HasClassConstraint_width = 1,

    /// Whether this protocol is resilient.
    IsResilient = 1,

    /// Special protocol value.
    SpecialProtocolKind = 2,
    SpecialProtocolKind_width = 6,
  };

public:
  explicit ProtocolContextDescriptorFlags(uint16_t bits) : FlagSet(bits) {}
  constexpr ProtocolContextDescriptorFlags() {}

  FLAGSET_DEFINE_FLAG_ACCESSORS(IsResilient, isResilient, setIsResilient)

  FLAGSET_DEFINE_FIELD_ACCESSORS(HasClassConstraint,
                                 HasClassConstraint_width,
                                 ProtocolClassConstraint,
                                 getClassConstraint,
                                 setClassConstraint)

  FLAGSET_DEFINE_FIELD_ACCESSORS(SpecialProtocolKind,
                                 SpecialProtocolKind_width,
                                 SpecialProtocol,
                                 getSpecialProtocol,
                                 setSpecialProtocol)
};

enum class GenericParamKind : uint8_t {
  /// A type parameter.
  Type = 0,
  
  Max = 0x3F,
};

class GenericParamDescriptor {
  uint8_t Value;
  
  explicit constexpr GenericParamDescriptor(uint8_t Value)
    : Value(Value) {}
public:
  constexpr GenericParamDescriptor(GenericParamKind kind,
                                   bool hasKeyArgument,
                                   bool hasExtraArgument)
    : GenericParamDescriptor(GenericParamDescriptor(0)
                         .withKind(kind)
                         .withKeyArgument(hasKeyArgument)
                         .withExtraArgument(hasExtraArgument))
  {}
  
  constexpr bool hasKeyArgument() const {
    return (Value & 0x80u) != 0;
  }

  constexpr bool hasExtraArgument() const {
    return (Value & 0x40u) != 0;
  }

  constexpr GenericParamKind getKind() const {
    return GenericParamKind(Value & 0x3Fu);
  }
  
  constexpr GenericParamDescriptor
  withKeyArgument(bool hasKeyArgument) const {
    return GenericParamDescriptor((Value & 0x7Fu)
      | (hasKeyArgument ? 0x80u : 0));
  }
  
  constexpr GenericParamDescriptor
  withExtraArgument(bool hasExtraArgument) const {
    return GenericParamDescriptor((Value & 0xBFu)
      | (hasExtraArgument ? 0x40u : 0));
  }
  
  constexpr GenericParamDescriptor withKind(GenericParamKind kind) const {
    return assert((uint8_t(kind) & 0x3Fu) == uint8_t(kind)),
      GenericParamDescriptor((Value & 0xC0u) | uint8_t(kind));
  }
  
  constexpr uint8_t getIntValue() const {
    return Value;
  }
};

enum class GenericRequirementKind : uint8_t {
  /// A protocol requirement.
  Protocol = 0,
  /// A same-type requirement.
  SameType = 1,
  /// A base class requirement.
  BaseClass = 2,
  /// A "same-conformance" requirement, implied by a same-type or base-class
  /// constraint that binds a parameter with protocol requirements.
  SameConformance = 3,
  /// A layout constraint.
  Layout = 0x1F,
};

class GenericRequirementFlags {
  uint32_t Value;
  
  explicit constexpr GenericRequirementFlags(uint32_t Value)
    : Value(Value) {}
public:
  constexpr GenericRequirementFlags(GenericRequirementKind kind,
                                    bool hasKeyArgument,
                                    bool hasExtraArgument)
    : GenericRequirementFlags(GenericRequirementFlags(0)
                         .withKind(kind)
                         .withKeyArgument(hasKeyArgument)
                         .withExtraArgument(hasExtraArgument))
  {}
  
  constexpr bool hasKeyArgument() const {
    return (Value & 0x80u) != 0;
  }

  constexpr bool hasExtraArgument() const {
    return (Value & 0x40u) != 0;
  }

  constexpr GenericRequirementKind getKind() const {
    return GenericRequirementKind(Value & 0x1Fu);
  }
  
  constexpr GenericRequirementFlags
  withKeyArgument(bool hasKeyArgument) const {
    return GenericRequirementFlags((Value & 0x7Fu)
      | (hasKeyArgument ? 0x80u : 0));
  }
  
  constexpr GenericRequirementFlags
  withExtraArgument(bool hasExtraArgument) const {
    return GenericRequirementFlags((Value & 0xBFu)
      | (hasExtraArgument ? 0x40u : 0));
  }
  
  constexpr GenericRequirementFlags
  withKind(GenericRequirementKind kind) const {
    return assert((uint8_t(kind) & 0x1Fu) == uint8_t(kind)),
      GenericRequirementFlags((Value & 0xE0u) | uint8_t(kind));
  }
  
  constexpr uint32_t getIntValue() const {
    return Value;
  }
};

enum class GenericRequirementLayoutKind : uint32_t {
  // A class constraint.
  Class = 0,
};

/// Flags used by generic metadata patterns.
class GenericMetadataPatternFlags : public FlagSet<uint32_t> {
  enum {
    // All of these values are bit offsets or widths.
    // General flags build up from 0.
    // Kind-specific flags build down from 31.

    /// Does this pattern have an extra-data pattern?
    HasExtraDataPattern = 0,

    // Class-specific flags.

    /// Does this pattern have an immediate-members pattern?
    Class_HasImmediateMembersPattern = 31,

    // Value-specific flags.

    /// For value metadata: the metadata kind of the type.
    Value_MetadataKind = 21,
    Value_MetadataKind_width = 11,
  };

public:
  explicit GenericMetadataPatternFlags(uint32_t bits) : FlagSet(bits) {}
  constexpr GenericMetadataPatternFlags() {}

  FLAGSET_DEFINE_FLAG_ACCESSORS(Class_HasImmediateMembersPattern,
                                class_hasImmediateMembersPattern,
                                class_setHasImmediateMembersPattern)

  FLAGSET_DEFINE_FLAG_ACCESSORS(HasExtraDataPattern,
                                hasExtraDataPattern,
                                setHasExtraDataPattern)

  FLAGSET_DEFINE_FIELD_ACCESSORS(Value_MetadataKind,
                                 Value_MetadataKind_width,
                                 MetadataKind,
                                 value_getMetadataKind,
                                 value_setMetadataKind)
};

/// The public state of a metadata.
enum class MetadataState : size_t {
  // The values of this enum are set up to give us some future flexibility
  // in adding states.  The compiler emits unsigned comparisons against
  // these values, so adding states that aren't totally ordered with at
  // least the existing values will pose a problem; but we also use a
  // gradually-shrinking bitset in case it's useful to track states as
  // separate capabilities.  Specific values have been chosen so that a
  // MetadataRequest of 0 represents a blocking complete request, which
  // is the most likely request from ordinary code.  The total size of a
  // state is kept to 8 bits so that a full request, even with additional
  // flags, can be materialized as a single immediate on common ISAs, and
  // so that the state can be extracted with a byte truncation.
  // The spacing between states reflects guesswork about where new
  // states/capabilities are most likely to be added.

  /// The metadata is fully complete.  By definition, this is the
  /// end-state of all metadata.  Generally, metadata is expected to be
  /// complete before it can be passed to arbitrary code, e.g. as
  /// a generic argument to a function or as a metatype value.
  ///
  /// In addition to the requirements of NonTransitiveComplete, certain
  /// transitive completeness guarantees must hold.  Most importantly,
  /// complete nominal type metadata transitively guarantee the completion
  /// of their stored generic type arguments and superclass metadata.
  Complete = 0x00,

  /// The metadata is fully complete except for any transitive completeness
  /// guarantees.
  ///
  /// In addition to the requirements of LayoutComplete, metadata in this
  /// state must be prepared for all basic type operations.  This includes:
  ///
  ///   - any sort of internal layout necessary to allocate and work
  ///     with concrete values of the type, such as the instance layout
  ///     of a class;
  ///
  ///   - any sort of external dynamic registration that might be required
  ///     for the type, such as the realization of a class by the Objective-C
  ///     runtime; and
  ///
  ///   - the initialization of any other information kept in the metadata
  ///     object, such as a class's v-table.
  NonTransitiveComplete = 0x01,

  /// The metadata is ready for the layout of other types that store values
  /// of this type.
  ///
  /// In addition to the requirements of Abstract, metadata in this state
  /// must have a valid value witness table, meaning that its size,
  /// alignment, and basic type properties (such as POD-ness) have been
  /// computed.
  LayoutComplete = 0x3F,

  /// The metadata has its basic identity established.  It is possible to
  /// determine what formal type it corresponds to.  Among other things, it
  /// is possible to use the runtime mangling facilities with the type.
  ///
  /// For example, a metadata for a generic struct has a metadata kind,
  /// a type descriptor, and all of its type arguments.  However, it does not
  /// necessarily have a meaningful value-witness table.
  ///
  /// References to other types that are not part of the type's basic identity
  /// may not yet have been established.  Most crucially, this includes the
  /// superclass pointer.
  Abstract = 0xFF,
};

/// Something that can be static_asserted in all the places where we do
/// comparisons on metadata states.
constexpr const bool MetadataStateIsReverseOrdered = true;

/// Return true if the first metadata state is at least as advanced as the
/// second.
inline bool isAtLeast(MetadataState lhs, MetadataState rhs) {
  static_assert(MetadataStateIsReverseOrdered,
                "relying on the ordering of MetadataState here");
  return size_t(lhs) <= size_t(rhs);
}

/// Kinds of requests for metadata.
class MetadataRequest : public FlagSet<size_t> {
  using IntType = size_t;
  using super = FlagSet<IntType>;

public:
  enum : IntType {
    State_bit = 0,
    State_width = 8,

    /// A blocking request will not return until the runtime is able to produce
    /// metadata with the given kind.  A non-blocking request will return
    /// "immediately", producing an abstract metadata and a flag saying that
    /// the operation failed.
    ///
    /// An abstract request will never be non-zero.
    NonBlocking_bit = 8,
  };

  MetadataRequest(MetadataState state, bool isNonBlocking = false) {
    setState(state);
    setIsNonBlocking(isNonBlocking);
  }
  explicit MetadataRequest(IntType bits) : super(bits) {}
  constexpr MetadataRequest() {}

  FLAGSET_DEFINE_EQUALITY(MetadataRequest)

  FLAGSET_DEFINE_FIELD_ACCESSORS(State_bit,
                                 State_width,
                                 MetadataState,
                                 getState,
                                 setState)

  FLAGSET_DEFINE_FLAG_ACCESSORS(NonBlocking_bit,
                                isNonBlocking,
                                setIsNonBlocking)
  bool isBlocking() const { return !isNonBlocking(); }

  /// Is this request satisfied by a metadata that's in the given state?
  bool isSatisfiedBy(MetadataState state) const {
    return isAtLeast(state, getState());
  }
};

} // end namespace swift

#endif /* SWIFT_ABI_METADATAVALUES_H */
