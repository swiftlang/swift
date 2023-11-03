//===--- TypeLowering.h - Swift Type Lowering for Reflection ----*- C++ -*-===//
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
// Implements logic for computing in-memory layouts from TypeRefs loaded from
// reflection metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_TYPELOWERING_H
#define SWIFT_REFLECTION_TYPELOWERING_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/Casting.h"
#include "swift/Remote/MetadataReader.h"
#include "swift/Remote/TypeInfoProvider.h"

#include <iostream>
#include <limits>
#include <memory>
#include <sstream>

namespace swift {
namespace reflection {

using llvm::cast;
using llvm::dyn_cast;
using remote::RemoteRef;

class TypeRef;
class TypeRefBuilder;
class BuiltinTypeDescriptor;

class LowerType;
class EnumTypeInfoBuilder;
class RecordTypeInfoBuilder;
class ExistentialTypeInfoBuilder;

enum class EnumKind : unsigned {
  // An enum with no payload cases. The record will have no fields, but
  // will have the correct size.
  NoPayloadEnum,

  // An enum with a single payload case and zero or more no-payload
  // cases.  The no-payload cases may be encoded with an extra tag
  // byte or as invalid payload values ("extra inhabitants").
  SinglePayloadEnum,

  // An enum with multiple payload cases and zero or more non-payload
  // cases.  The selector that indicates what case is currently active
  // may be encoded in unused "spare bits" common to all payloads and/or
  // may use a separate tag byte.
  MultiPayloadEnum,
};

enum class RecordKind : unsigned {
  Invalid,

  // A Swift tuple type.
  Tuple,

  // A Swift struct type.
  Struct,

  // A Swift-native function is always a function pointer followed by a
  // retainable, nullable context pointer.
  ThickFunction,

  // An existential is a three-word buffer followed by value metadata and
  // witness tables.
  OpaqueExistential,

  // A class existential is a retainable pointer followed by witness
  // tables.
  ClassExistential,

  // An existential metatype.
  ExistentialMetatype,

  // An error existential is a special kind of heap object, so is a retainable
  // pointer, with no witness tables.
  ErrorExistential,

  // A class instance layout, consisting of the stored properties of
  // one class, excluding superclasses.
  ClassInstance,

  // A closure context instance layout, consisting of the captured values.
  // For now, captured values do not retain their names.
  ClosureContext,
};

enum class ReferenceCounting : unsigned {
  Native,
  Unknown
};

enum class ReferenceKind : unsigned {
  Strong,
#define REF_STORAGE(Name, ...) Name,
#include "swift/AST/ReferenceStorage.def"
};

enum class TypeInfoKind : unsigned {
  Builtin,
  Record,
  Reference,
  Invalid,
  Enum,
};

class TypeInfo {
  TypeInfoKind Kind;
  unsigned Size, Alignment, Stride, NumExtraInhabitants;
  bool BitwiseTakable;

public:
  TypeInfo(TypeInfoKind Kind,
           unsigned Size, unsigned Alignment,
           unsigned Stride, unsigned NumExtraInhabitants,
           bool BitwiseTakable)
    : Kind(Kind), Size(Size), Alignment(Alignment), Stride(Stride),
      NumExtraInhabitants(NumExtraInhabitants),
      BitwiseTakable(BitwiseTakable) {
    assert(Alignment > 0);
  }

  TypeInfo(): Kind(TypeInfoKind::Invalid), Size(0), Alignment(0), Stride(0),
              NumExtraInhabitants(0), BitwiseTakable(true) {
  }

  TypeInfoKind getKind() const { return Kind; }

  unsigned getSize() const { return Size; }
  unsigned getAlignment() const { return Alignment; }
  unsigned getStride() const { return Stride; }
  unsigned getNumExtraInhabitants() const { return NumExtraInhabitants; }
  bool isBitwiseTakable() const { return BitwiseTakable; }

  void dump() const;
  void dump(std::ostream &stream, unsigned Indent = 0) const;

  // Using the provided reader, inspect our value.
  // Return false if we can't inspect value.
  // Set *inhabitant to <0 if the value is valid (not an XI)
  // Else set *inhabitant to the XI value (counting from 0)
  virtual bool readExtraInhabitantIndex(remote::MemoryReader &reader,
                                        remote::RemoteAddress address,
                                        int *index) const {
    return false;
  }

  virtual ~TypeInfo() { }
};

struct FieldInfo {
  std::string Name;
  unsigned Offset;
  int Value;
  const TypeRef *TR;
  const TypeInfo &TI;
};

/// Builtins and (opaque) imported value types
class BuiltinTypeInfo : public TypeInfo {
  std::string Name;

public:
  explicit BuiltinTypeInfo(TypeRefBuilder &builder,
                           RemoteRef<BuiltinTypeDescriptor> descriptor);

  /// Construct an empty builtin type info.
  BuiltinTypeInfo()
      : TypeInfo(TypeInfoKind::Builtin,
                 /*Size=*/0,
                 /*Alignment=*/1,
                 /*Stride=*/1,
                 /*ExtraInhabitants=*/0,
                 /*BitwiseTakable=*/true),
        Name("") {}

  const std::string &getMangledTypeName() const {
    return Name;
  }

  bool readExtraInhabitantIndex(remote::MemoryReader &reader,
                                remote::RemoteAddress address,
                                int *extraInhabitantIndex) const override;

  static bool classof(const TypeInfo *TI) {
    return TI->getKind() == TypeInfoKind::Builtin;
  }
};

/// Class instances, structs, tuples
class RecordTypeInfo : public TypeInfo {
  RecordKind SubKind;
  std::vector<FieldInfo> Fields;

public:
  RecordTypeInfo(unsigned Size, unsigned Alignment,
                 unsigned Stride, unsigned NumExtraInhabitants,
                 bool BitwiseTakable,
                 RecordKind SubKind, const std::vector<FieldInfo> &Fields)
    : TypeInfo(TypeInfoKind::Record, Size, Alignment, Stride,
               NumExtraInhabitants, BitwiseTakable),
      SubKind(SubKind), Fields(Fields) {}

  RecordKind getRecordKind() const { return SubKind; }
  unsigned getNumFields() const { return Fields.size(); }
  const std::vector<FieldInfo> &getFields() const { return Fields; }

  bool readExtraInhabitantIndex(remote::MemoryReader &reader,
                                remote::RemoteAddress address,
                                int *index) const override;

  static bool classof(const TypeInfo *TI) {
    return TI->getKind() == TypeInfoKind::Record;
  }
};

/// Enums
class EnumTypeInfo : public TypeInfo {
  EnumKind SubKind;
  std::vector<FieldInfo> Cases;

protected:
  EnumTypeInfo(unsigned Size, unsigned Alignment,
               unsigned Stride, unsigned NumExtraInhabitants,
               bool BitwiseTakable,
               EnumKind SubKind, const std::vector<FieldInfo> &Cases)
    : TypeInfo(TypeInfoKind::Enum, Size, Alignment, Stride,
               NumExtraInhabitants, BitwiseTakable),
      SubKind(SubKind), Cases(Cases) {}

public:
  EnumKind getEnumKind() const { return SubKind; }
  const std::vector<FieldInfo> &getCases() const { return Cases; }
  unsigned getNumCases() const { return Cases.size(); }
  unsigned getNumPayloadCases() const {
    auto Cases = getCases();
    return std::count_if(Cases.begin(), Cases.end(),
                         [](const FieldInfo &Case){return Case.TR != 0;});
  }
  unsigned getNumNonEmptyPayloadCases() const;
  // Size of the payload area.
  unsigned getPayloadSize() const {
    return EnumTypeInfo::getPayloadSizeForCases(Cases);
  }

  static unsigned getPayloadSizeForCases(const std::vector<FieldInfo> &Cases);

  // Returns true if this enum is `Optional`
  bool isOptional() const;

  virtual bool projectEnumValue(remote::MemoryReader &reader,
                                remote::RemoteAddress address,
                                int *CaseIndex) const = 0;

  static bool classof(const TypeInfo *TI) {
    return TI->getKind() == TypeInfoKind::Enum;
  }
};

class UnsupportedEnumTypeInfo : public EnumTypeInfo {
public:
  UnsupportedEnumTypeInfo(unsigned Size, unsigned Alignment, unsigned Stride,
                          unsigned NumExtraInhabitants, bool BitwiseTakable,
                          EnumKind Kind, const std::vector<FieldInfo> &Cases)
      : EnumTypeInfo(Size, Alignment, Stride, NumExtraInhabitants,
                     BitwiseTakable, Kind, Cases) {}

  bool readExtraInhabitantIndex(remote::MemoryReader &reader,
                                remote::RemoteAddress address,
                                int *index) const override {
    return false;
  }

  bool projectEnumValue(remote::MemoryReader &reader,
                        remote::RemoteAddress address,
                        int *CaseIndex) const override {
    return false;
  }
};

// An Enum with no cases has no values, requires no storage,
// and cannot be instantiated.
// It is an uninhabited type (similar to Never).
class EmptyEnumTypeInfo : public EnumTypeInfo {
public:
  EmptyEnumTypeInfo(const std::vector<FieldInfo> &Cases)
      : EnumTypeInfo(/*Size*/ 0, /* Alignment*/ 1, /*Stride*/ 1,
                     /*NumExtraInhabitants*/ 0, /*BitwiseTakable*/ true,
                     EnumKind::NoPayloadEnum, Cases) {
    // No cases
    assert(Cases.size() == 0);
  }

  bool readExtraInhabitantIndex(remote::MemoryReader &reader,
                                remote::RemoteAddress address,
                                int *index) const override {
    return false;
  }

  bool projectEnumValue(remote::MemoryReader &reader,
                        remote::RemoteAddress address,
                        int *CaseIndex) const override {
    return false;
  }
};

// Non-generic Enum with a single non-payload case
// This enum requires no storage, since it only has
// one possible value.
class TrivialEnumTypeInfo : public EnumTypeInfo {
public:
  TrivialEnumTypeInfo(EnumKind Kind, const std::vector<FieldInfo> &Cases)
      : EnumTypeInfo(/*Size*/ 0,
                     /* Alignment*/ 1,
                     /*Stride*/ 1,
                     /*NumExtraInhabitants*/ 0,
                     /*BitwiseTakable*/ true, Kind, Cases) {
    // Exactly one case
    assert(Cases.size() == 1);
    // The only case has no payload, or a zero-sized payload
    assert(Cases[0].TR == 0 || Cases[0].TI.getSize() == 0);
  }

  bool readExtraInhabitantIndex(remote::MemoryReader &reader,
                                remote::RemoteAddress address,
                                int *index) const override {
    *index = -1;
    return true;
  }

  bool projectEnumValue(remote::MemoryReader &reader,
                        remote::RemoteAddress address,
                        int *CaseIndex) const override {
    *CaseIndex = 0;
    return true;
  }
};

// Enum with 2 or more non-payload cases and no payload cases
class NoPayloadEnumTypeInfo : public EnumTypeInfo {
public:
  NoPayloadEnumTypeInfo(unsigned Size, unsigned Alignment, unsigned Stride,
                        unsigned NumExtraInhabitants, EnumKind Kind,
                        const std::vector<FieldInfo> &Cases)
      : EnumTypeInfo(Size, Alignment, Stride, NumExtraInhabitants,
                     /*BitwiseTakable*/ true, Kind, Cases) {
    // There are at least 2 cases
    // (one case would be trivial, zero is impossible)
    assert(Cases.size() >= 2);
    // No non-empty payloads
    assert(getNumNonEmptyPayloadCases() == 0);
  }

  bool readExtraInhabitantIndex(remote::MemoryReader &reader,
                                remote::RemoteAddress address,
                                int *index) const override;

  bool projectEnumValue(remote::MemoryReader &reader,
                        remote::RemoteAddress address,
                        int *CaseIndex) const override;
};

// Enum with 1 payload case and zero or more non-payload cases
class SinglePayloadEnumTypeInfo : public EnumTypeInfo {
public:
  SinglePayloadEnumTypeInfo(unsigned Size, unsigned Alignment, unsigned Stride,
                            unsigned NumExtraInhabitants, bool BitwiseTakable,
                            EnumKind Kind, const std::vector<FieldInfo> &Cases)
      : EnumTypeInfo(Size, Alignment, Stride, NumExtraInhabitants,
                     BitwiseTakable, Kind, Cases) {
    // The first case has a payload (possibly empty)
    assert(Cases[0].TR != 0);
    // At most one non-empty payload case
    assert(getNumNonEmptyPayloadCases() <= 1);
  }

  bool readExtraInhabitantIndex(remote::MemoryReader &reader,
                                remote::RemoteAddress address,
                                int *extraInhabitantIndex) const override;

  bool projectEnumValue(remote::MemoryReader &reader,
                        remote::RemoteAddress address,
                        int *CaseIndex) const override;
};

// *Tagged* Multi-payload enums use a separate tag value exclusively.
// This may be because it only has one payload (with no XIs) or
// because it's a true MPE but with no "spare bits" in the payload area.
// This includes cases such as:
//
// ```
// // Enums with non-pointer payloads (only pointers carry spare bits)
// enum A {
//   case a(Int)
//   case b(Double)
//   case c((Int8, UInt8))
// }
//
// // Generic enums (compiler doesn't have layout details)
// enum Either<T,U>{
//   case a(T)
//   case b(U)
// }
//
// // Enums where payload is covered by a non-pointer
// enum A {
//   case a(ClassTypeA)
//   case b(ClassTypeB)
//   case c(Int)
// }
//
// // Enums with one non-empty payload but that has no XIs
// // (This is almost but not quite the same as the single-payload
// // case.  Different in that this MPE exposes extra tag values
// // as XIs to an enclosing enum; SPEs don't do that.)
// enum A {
//   case a(Int)
//   case b(Void)
// }
// ```
class TaggedMultiPayloadEnumTypeInfo : public EnumTypeInfo {
  unsigned NumEffectivePayloadCases;

public:
  TaggedMultiPayloadEnumTypeInfo(unsigned Size, unsigned Alignment,
                                 unsigned Stride, unsigned NumExtraInhabitants,
                                 bool BitwiseTakable,
                                 const std::vector<FieldInfo> &Cases,
                                 unsigned NumEffectivePayloadCases)
      : EnumTypeInfo(Size, Alignment, Stride, NumExtraInhabitants,
                     BitwiseTakable, EnumKind::MultiPayloadEnum, Cases),
        NumEffectivePayloadCases(NumEffectivePayloadCases) {
    // Definition of "multi-payload enum"
    assert(getCases().size() > 1); // At least 2 cases
    assert(Cases[0].TR != 0);      // At least 2 payloads
    // assert(Cases[1].TR != 0);
    // At least one payload is non-empty (otherwise this would get
    // laid out as a non-payload enum). Commented out this assert
    // because it doesn't hold when there are generic cases with
    // zero-sized payload.
    // assert(getNumNonEmptyPayloadCases() > 0);
    // There's a tag, so the total size must be bigger than any payload
    // assert(getSize() > getPayloadSize());
  }

  bool readExtraInhabitantIndex(remote::MemoryReader &reader,
                                remote::RemoteAddress address,
                                int *extraInhabitantIndex) const override;

  bool projectEnumValue(remote::MemoryReader &reader,
                        remote::RemoteAddress address,
                        int *CaseIndex) const override;
};

// A variable-length bitmap used to track "spare bits" for general multi-payload
// enums.
class BitMask {
  static constexpr unsigned maxSize = 128 * 1024 * 1024; // 128MB

  unsigned size; // Size of mask in bytes
  uint8_t *mask;

public:
  ~BitMask() { free(mask); }
  // Construct a bitmask of the appropriate number of bytes
  // initialized to all bits set
  BitMask(unsigned sizeInBytes);
  // Construct a bitmask of the appropriate number of bytes
  // initialized with bits from the specified buffer
  BitMask(unsigned sizeInBytes, const uint8_t *initialValue,
          unsigned initialValueBytes, unsigned offset);
  // Move constructor moves ownership and zeros the src
  BitMask(BitMask &&src) noexcept;
  // Copy constructor makes a copy of the mask storage
  BitMask(const BitMask &src) noexcept;
  std::string str() const;

  bool operator==(const BitMask &rhs) const;

  bool operator!=(const BitMask &rhs) const { return !(*this == rhs); }

  bool isNonZero() const { return !isZero(); }

  bool isZero() const;

  void makeZero() { memset(mask, 0, size * sizeof(mask[0])); }

  void complement();

  int countSetBits() const;

  int countZeroBits() const;

  // Treat the provided value as a mask, `and` it with
  // the part of the mask at the provided byte offset.
  // Bits outside the specified area are unchanged.
  template <typename IntegerType>
  void andMask(IntegerType value, unsigned byteOffset) {
    andMask((void *)&value, sizeof(value), byteOffset);
  }

  // As above, but using the provided bitmask instead
  // of an integer.
  void andMask(BitMask mask, unsigned offset) {
    andMask(mask.mask, mask.size, offset);
  }

  // As above, but using the complement of the
  // provided mask.
  void andNotMask(BitMask mask, unsigned offset) {
    if (offset < size) {
      andNotMask(mask.mask, mask.size, offset);
    }
  }

  // Zero all bits except for the `n` most significant ones.
  // XXX TODO: Big-endian support?
  void keepOnlyMostSignificantBits(unsigned n);

  unsigned numBits() const { return size * 8; }

  unsigned numSetBits() const;

  // Read a mask-sized area from the target and collect
  // the masked bits into a single integer.
  template <typename IntegerType>
  bool readMaskedInteger(remote::MemoryReader &reader,
                         remote::RemoteAddress address,
                         IntegerType *dest) const {
    auto data = reader.readBytes(address, size);
    if (!data) {
      return false;
    }
#if defined(__BIG_ENDIAN__)
    assert(false && "Big endian not supported for readMaskedInteger");
#else
    IntegerType result = 0;
    IntegerType resultBit = 1; // Start from least-significant bit
    auto bytes = static_cast<const uint8_t *>(data.get());
    for (unsigned i = 0; i < size; ++i) {
      for (unsigned b = 1; b < 256; b <<= 1) {
        if ((mask[i] & b) != 0) {
          if ((bytes[i] & b) != 0) {
            result |= resultBit;
          }
          resultBit <<= 1;
        }
      }
    }
    *dest = result;
    return true;
#endif
  }

private:
  void andMask(void *maskData, unsigned len, unsigned offset);

  void andNotMask(void *maskData, unsigned len, unsigned offset);
};

// General multi-payload enum support for enums that do use spare
// bits in the payload.
class MultiPayloadEnumTypeInfo : public EnumTypeInfo {
  BitMask spareBitsMask;
  // "Effective" payload cases includes those with
  // generic payload and non-generic cases that are
  // statically known to have non-zero size.
  // It does not include cases with payloads that are
  // non-generic and zero-sized (these are treated as
  // non-payload cases for many purposes).
  unsigned NumEffectivePayloadCases;

public:
  MultiPayloadEnumTypeInfo(unsigned Size, unsigned Alignment, unsigned Stride,
                           unsigned NumExtraInhabitants, bool BitwiseTakable,
                           const std::vector<FieldInfo> &Cases,
                           BitMask spareBitsMask,
                           unsigned NumEffectivePayloadCases)
      : EnumTypeInfo(Size, Alignment, Stride, NumExtraInhabitants,
                     BitwiseTakable, EnumKind::MultiPayloadEnum, Cases),
        spareBitsMask(spareBitsMask),
        NumEffectivePayloadCases(NumEffectivePayloadCases) {
    assert(Cases[0].TR != 0);
    assert(Cases[1].TR != 0);
    assert(getNumNonEmptyPayloadCases() > 1);
  }

  bool readExtraInhabitantIndex(remote::MemoryReader &reader,
                                remote::RemoteAddress address,
                                int *extraInhabitantIndex) const override;

  bool projectEnumValue(remote::MemoryReader &reader,
                        remote::RemoteAddress address,
                        int *CaseIndex) const override;

  // The case value is stored in three pieces:
  // * A separate "discriminator" tag appended to the payload (if necessary)
  // * A "payload tag" that uses (a subset of) the spare bits
  // * The remainder of the payload bits (for non-payload cases)
  // This computes the bits used for the payload tag.
  BitMask getMultiPayloadTagBitsMask() const;
};

/// References to classes, closure contexts and anything else with an
/// 'isa' pointer
class ReferenceTypeInfo : public TypeInfo {
  ReferenceKind SubKind;
  ReferenceCounting Refcounting;

public:
  ReferenceTypeInfo(unsigned Size, unsigned Alignment,
                    unsigned Stride, unsigned NumExtraInhabitants,
                    bool BitwiseTakable, ReferenceKind SubKind,
                    ReferenceCounting Refcounting)
    : TypeInfo(TypeInfoKind::Reference, Size, Alignment, Stride,
               NumExtraInhabitants, BitwiseTakable),
      SubKind(SubKind), Refcounting(Refcounting) {}

  ReferenceKind getReferenceKind() const {
    return SubKind;
  }

  ReferenceCounting getReferenceCounting() const {
    return Refcounting;
  }

  bool readExtraInhabitantIndex(remote::MemoryReader &reader,
                                remote::RemoteAddress address,
                                int *extraInhabitantIndex) const override {
    if (getNumExtraInhabitants() == 0) {
      *extraInhabitantIndex = -1;
      return true;
    }
    return reader.readHeapObjectExtraInhabitantIndex(address, extraInhabitantIndex);
  }

  static bool classof(const TypeInfo *TI) {
    return TI->getKind() == TypeInfoKind::Reference;
  }
};

/// This class owns the memory for all TypeInfo instances that it vends.
class TypeConverter {
  TypeRefBuilder &Builder;
  std::vector<std::unique_ptr<const TypeInfo>> Pool;
  llvm::DenseMap<std::pair<const TypeRef *, remote::TypeInfoProvider::IdType>,
                 const TypeInfo *> Cache;
  llvm::DenseSet<const TypeRef *> RecursionCheck;
  llvm::DenseMap<std::pair<unsigned, unsigned>,
                 const ReferenceTypeInfo *> ReferenceCache;

  const TypeRef *RawPointerTR = nullptr;
  const TypeRef *NativeObjectTR = nullptr;
  const TypeRef *UnknownObjectTR = nullptr;
  const TypeRef *ThinFunctionTR = nullptr;
  const TypeRef *AnyMetatypeTR = nullptr;

  const TypeInfo *ThinFunctionTI = nullptr;
  const TypeInfo *ThickFunctionTI = nullptr;
  const TypeInfo *AnyMetatypeTI = nullptr;
  const TypeInfo *EmptyTI = nullptr;

public:
  explicit TypeConverter(TypeRefBuilder &Builder) : Builder(Builder) {}

  TypeRefBuilder &getBuilder() { return Builder; }

  /// Tests if the type is concrete enough that its size is known.
  /// For example, a bound generic class is fixed size even if some
  /// of the generic argument types contain generic parameters.
  bool hasFixedSize(const TypeRef *TR);

  /// Returns layout information for a value of the given type.
  /// For a class, this returns the lowering of the reference value.
  ///
  /// The type must either be concrete, or at least fixed-size, as
  /// determined by the isFixedSize() predicate.
  const TypeInfo *getTypeInfo(const TypeRef *TR,
                              remote::TypeInfoProvider *externalInfo);

  /// Returns layout information for an instance of the given
  /// class.
  ///
  /// Not cached.
  const RecordTypeInfo *
  getClassInstanceTypeInfo(const TypeRef *TR, unsigned start,
                           remote::TypeInfoProvider *ExternalTypeInfo);

  unsigned targetPointerSize() {
    auto *rawPointerTI = getTypeInfo(getRawPointerTypeRef(), nullptr);
    return rawPointerTI->getSize();
  }

private:
  friend class swift::reflection::LowerType;
  friend class swift::reflection::EnumTypeInfoBuilder;
  friend class swift::reflection::RecordTypeInfoBuilder;
  friend class swift::reflection::ExistentialTypeInfoBuilder;

  const ReferenceTypeInfo *
  getReferenceTypeInfo(ReferenceKind Kind,
                       ReferenceCounting Refcounting);

  /// TypeRefs for special types for which we need to know the layout
  /// intrinsically in order to layout anything else.
  ///
  /// IRGen emits BuiltinTypeDescriptors for these when compiling the
  /// standard library.
  const TypeRef *getRawPointerTypeRef();
  const TypeRef *getNativeObjectTypeRef();
  const TypeRef *getUnknownObjectTypeRef();
  const TypeRef *getThinFunctionTypeRef();
  const TypeRef *getAnyMetatypeTypeRef();

  const TypeInfo *getThinFunctionTypeInfo();
  const TypeInfo *getThickFunctionTypeInfo();
  const TypeInfo *getAnyMetatypeTypeInfo();
  const TypeInfo *getEmptyTypeInfo();

  template <typename TypeInfoTy, typename... Args>
  const TypeInfoTy *makeTypeInfo(Args &&... args) {
    auto TI = new TypeInfoTy(::std::forward<Args>(args)...);
    Pool.push_back(std::unique_ptr<const TypeInfo>(TI));
    return TI;
  }
};

} // namespace reflection
} // namespace swift

#endif
