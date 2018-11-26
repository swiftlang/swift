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

#include <iostream>
#include <memory>

namespace swift {
namespace reflection {

using llvm::cast;
using llvm::dyn_cast;

class TypeRef;
class TypeRefBuilder;
class BuiltinTypeDescriptor;

// Defined in TypeLowering.cpp, not public -- they're friends below
class LowerType;
class EnumTypeInfoBuilder;
class RecordTypeInfoBuilder;
class ExistentialTypeInfoBuilder;

enum class RecordKind : unsigned {
  Invalid,

  // A Swift tuple type.
  Tuple,

  // A Swift struct type.
  Struct,

  // An enum with no payload cases. The record will have no fields, but
  // will have the correct size.
  NoPayloadEnum,

  // An enum with a single payload case. The record consists of a single
  // field, being the enum payload.
  SinglePayloadEnum,

  // An enum with multiple payload cases. The record consists of a multiple
  // fields, one for each enum payload.
  MultiPayloadEnum,

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
};

class TypeInfo {
  TypeInfoKind Kind;
  unsigned Size, Alignment, Stride, NumExtraInhabitants;

public:
  TypeInfo(TypeInfoKind Kind,
           unsigned Size, unsigned Alignment,
           unsigned Stride, unsigned NumExtraInhabitants)
    : Kind(Kind), Size(Size), Alignment(Alignment), Stride(Stride),
      NumExtraInhabitants(NumExtraInhabitants) {
    assert(Alignment > 0);
  }

  TypeInfoKind getKind() const { return Kind; }

  unsigned getSize() const { return Size; }
  unsigned getAlignment() const { return Alignment; }
  unsigned getStride() const { return Stride; }
  unsigned getNumExtraInhabitants() const { return NumExtraInhabitants; }

  void dump() const;
  void dump(std::ostream &OS, unsigned Indent = 0) const;
};

struct FieldInfo {
  std::string Name;
  unsigned Offset;
  const TypeRef *TR;
  const TypeInfo &TI;
};

/// Builtins and (opaque) imported value types
class BuiltinTypeInfo : public TypeInfo {
  std::string Name;

public:
  explicit BuiltinTypeInfo(const BuiltinTypeDescriptor *descriptor);

  const std::string &getMangledTypeName() const {
    return Name;
  }

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
                 RecordKind SubKind, const std::vector<FieldInfo> &Fields)
    : TypeInfo(TypeInfoKind::Record, Size, Alignment, Stride,
               NumExtraInhabitants),
      SubKind(SubKind), Fields(Fields) {}

  RecordKind getRecordKind() const { return SubKind; }
  unsigned getNumFields() const { return Fields.size(); }
  const std::vector<FieldInfo> &getFields() const { return Fields; }

  static bool classof(const TypeInfo *TI) {
    return TI->getKind() == TypeInfoKind::Record;
  }
};

/// References to classes, closure contexts and anything else with an
/// 'isa' pointer
class ReferenceTypeInfo : public TypeInfo {
  ReferenceKind SubKind;
  ReferenceCounting Refcounting;

public:
  ReferenceTypeInfo(unsigned Size, unsigned Alignment,
                    unsigned Stride, unsigned NumExtraInhabitants,
                    ReferenceKind SubKind, ReferenceCounting Refcounting)
    : TypeInfo(TypeInfoKind::Reference, Size, Alignment, Stride,
               NumExtraInhabitants),
      SubKind(SubKind), Refcounting(Refcounting) {}

  ReferenceKind getReferenceKind() const {
    return SubKind;
  }

  ReferenceCounting getReferenceCounting() const {
    return Refcounting;
  }

  static bool classof(const TypeInfo *TI) {
    return TI->getKind() == TypeInfoKind::Reference;
  }
};

/// This class owns the memory for all TypeInfo instances that it vends.
class TypeConverter {
  TypeRefBuilder &Builder;
  std::vector<std::unique_ptr<const TypeInfo>> Pool;
  llvm::DenseMap<const TypeRef *, const TypeInfo *> Cache;
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
  const TypeInfo *getTypeInfo(const TypeRef *TR);

  /// Returns layout information for an instance of the given
  /// class.
  ///
  /// Not cached.
  const TypeInfo *getClassInstanceTypeInfo(const TypeRef *TR,
                                           unsigned start);

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
  const TypeInfoTy *makeTypeInfo(Args... args) {
    auto TI = new TypeInfoTy(::std::forward<Args>(args)...);
    Pool.push_back(std::unique_ptr<const TypeInfo>(TI));
    return TI;
  }
};

/// Utility class for performing universal layout for types such as
/// tuples, structs, thick functions, etc.
class RecordTypeInfoBuilder {
  TypeConverter &TC;
  unsigned Size, Alignment, NumExtraInhabitants;
  RecordKind Kind;
  std::vector<FieldInfo> Fields;
  bool Empty;
  bool Invalid;

public:
  RecordTypeInfoBuilder(TypeConverter &TC, RecordKind Kind)
    : TC(TC), Size(0), Alignment(1), NumExtraInhabitants(0),
      Kind(Kind), Empty(true), Invalid(false) {}

  bool isInvalid() const {
    return Invalid;
  }

  unsigned addField(unsigned fieldSize, unsigned fieldAlignment,
                    unsigned numExtraInhabitants);

  // Add a field of a record type, such as a struct.
  void addField(const std::string &Name, const TypeRef *TR);

  const RecordTypeInfo *build();

  unsigned getNumFields() const {
    return Fields.size();
  }

  unsigned getFieldOffset(unsigned Index) const {
    return Fields[Index].Offset;
  }
};

}
}

#endif
