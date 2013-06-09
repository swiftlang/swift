//===--- SILType.h - Defines the SILType type -------------------*- C++ -*-===//
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
// This file defines the SILType class, which is used to refer to SIL
// representation types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILType_H
#define SWIFT_SIL_SILType_H

#include "swift/AST/Types.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/ErrorHandling.h"
#include "swift/SIL/SILAllocated.h"

namespace swift {
  class ASTContext;
  class VarDecl;
  class SILFunctionTypeInfo;

namespace Lowering {
  class TypeConverter;
}

} // end namespace swift
  
namespace swift {

/// SILType - A Swift type that has been lowered to a SIL representation type.
/// In addition to the Swift type system, SIL adds "address" types that can
/// reference any Swift type (but cannot take the address of an address). *T
/// is the type of an address pointing at T.
///
class SILType {
public:
  // The bool value of the PointerIntPair is the "isAddress" bit.
  using ValueType = llvm::PointerIntPair<TypeBase *, 1, bool>;
private:
  ValueType value;

  /// Private constructor. SILTypes are normally vended by
  /// TypeConverter::getLoweredType().
  SILType(CanType ty, bool isAddress) : value(ty.getPointer(), isAddress) {
    if (!ty) return;

    assert(!ty->is<LValueType>() &&
           "LValueTypes should be eliminated by SIL lowering");
  }
  
  SILType(ValueType value) : value(value) {
  }

  friend class Lowering::TypeConverter;
  friend struct llvm::DenseMapInfo<SILType>;
public:
  SILType() = default;
  
  /// getPrimitiveType - Form a SILType for a primitive type that does not
  /// require any special handling (i.e., not a function or aggregate type).
  static SILType getPrimitiveType(CanType T, bool isAddress = false) {
    return SILType(T, isAddress);
  }

  bool isNull() const { return bool(value.getPointer()); }
  explicit operator bool() const { return bool(value.getPointer()); }
  
  /// Gets the address type referencing this type, or the type itself if it is
  /// already an address type.
  SILType getAddressType() const {
    return {{value.getPointer(), true}};
  }

  /// Gets the type referenced by an address type, or the type itself if it is
  /// not an address type. Invalid for address-only types.
  SILType getObjectType() const {
    return {{value.getPointer(), false}};
  }
  
  /// Returns the Swift type referenced by this SIL type.
  CanType getSwiftRValueType() const {
    return CanType(value.getPointer());
  }

  /// Returns the Swift type equivalent to this SIL type. If the SIL type is
  /// an address type, returns an LValueType.
  CanType getSwiftType() const {
    CanType rvalueTy = getSwiftRValueType();
    if (isAddress())
      return LValueType::get(rvalueTy, LValueType::Qual::DefaultForType,
                             rvalueTy->getASTContext())
              ->getCanonicalType();
    return rvalueTy;
  }
  
  /// Gives the SILFunctionTypeInfo for a function type. The type must be
  /// derived from a Swift FunctionType or PolymorphicFunctionType.
  SILFunctionTypeInfo *getFunctionTypeInfo(SILModule &M) const;
  
  /// Returns the Swift return type of a function type.
  /// The SILType must refer to a function type.
  CanType getFunctionResultType() const {
    auto *fty = castTo<AnyFunctionType>();
    return CanType(fty->getResult());
  }
  
  /// Returns the AbstractCC of a function type.
  /// The SILType must refer to a function type.
  AbstractCC getAbstractCC() const {
    return castTo<AnyFunctionType>()->getAbstractCC();
  }

  /// Cast the Swift type referenced by this SIL type, or return null if the
  /// cast fails.
  template<typename TYPE>
  TYPE *getAs() const { return dyn_cast<TYPE>(getSwiftRValueType()); }
  /// Cast the Swift type referenced by this SIL type, which must be of the
  /// specified subtype.
  template<typename TYPE>
  TYPE *castTo() const { return cast<TYPE>(getSwiftRValueType()); }
  /// Returns true if the Swift type referenced by this SIL type is of the
  /// specified subtype.
  template<typename TYPE>
  bool is() const { return isa<TYPE>(getSwiftRValueType()); }
  
  /// Retrieve the ClassDecl for a type that maps to a Swift class or
  /// bound generic class type.
  ClassDecl *getClassOrBoundGenericClass() const {
    return getSwiftRValueType()->getClassOrBoundGenericClass();
  }
  
  /// True if the type is an address type.
  bool isAddress() const { return value.getInt(); }

  /// isAddressOnly - True if the type, or the referenced type of an address
  /// type, is address-only.  For example, it could be a resilient struct or
  /// something of unknown size.
  static bool isAddressOnly(CanType T, SILModule &M);

  /// True if the type, or the referenced type of an address type, is loadable.
  /// This is the opposite of isAddressOnly.
  bool isLoadable(SILModule &M) const {
    return !isAddressOnly(getSwiftRValueType(), M);
  }
  /// True if the type, or the referenced type of an address type, is
  /// address-only. This is the opposite of isLoadable.
  bool isAddressOnly(SILModule &M) const {
    return isAddressOnly(getSwiftRValueType(), M);
  }

  /// Returns true if the referenced type has reference semantics.
  bool hasReferenceSemantics() const {
    return getSwiftRValueType()->hasReferenceSemantics();
  }
  /// Returns true if the referenced type is an existential type.
  bool isExistentialType() const {
    return getSwiftRValueType()->isExistentialType();
  }
  /// Returns true if the referenced type is a class-bound existential type.
  bool isClassBoundExistentialType() const {
    return getSwiftRValueType()->isClassBoundExistentialType();
  }
  
  /// Returns the ASTContext for the referenced Swift type.
  ASTContext &getASTContext() const {
    return getSwiftRValueType()->getASTContext();
  }
  
  //
  // Accessors for types used in SIL instructions:
  //
  
  /// Get the ObjectPointer type as a SILType.
  static SILType getObjectPointerType(ASTContext &C);
  /// Get the ObjCPointer type as a SILType.
  static SILType getObjCPointerType(ASTContext &C);
  /// Get the RawPointer type as a SILType.
  static SILType getRawPointerType(ASTContext &C);
  /// Get the OpaquePointer type as a SILType.
  static SILType getOpaquePointerType(ASTContext &C);
  /// Get a builtin integer type as a SILType.
  static SILType getBuiltinIntegerType(unsigned bitWidth, ASTContext &C);
  /// Get a builtin floating-point type as a SILType.
  static SILType getBuiltinFloatType(BuiltinFloatType::FPKind Kind,
                                     ASTContext &C);
  
  //
  // Utilities for treating SILType as a pointer-like type.
  //
  static SILType getFromOpaqueValue(void *P) {
    return SILType(ValueType::getFromOpaqueValue(P));
  }
  void *getOpaqueValue() const {
    return value.getOpaqueValue();
  }
  
  bool operator==(SILType rhs) const {
    return value.getOpaqueValue() == rhs.value.getOpaqueValue();
  }
  bool operator!=(SILType rhs) const {
    return value.getOpaqueValue() != rhs.value.getOpaqueValue();
  }
  
  std::string getAsString() const;
  void dump() const;
  void print(raw_ostream &OS) const;
};

inline raw_ostream &operator<<(raw_ostream &OS, SILType T) {
  T.print(OS);
  return OS;
}

/// Redundant but expensive-to-recompute information about a SIL
/// FunctionType or PolymorphicFunctionType. Contains the details of the
/// SIL-level calling convention for the function, including its exploded
/// input argument types and whether it uses an indirect return argument.
class SILFunctionTypeInfo {
  CanType swiftType;
  SILType resultType;
  unsigned inputTypeCount : 31;
  unsigned indirectReturn : 1;

  SILType *getInputTypeBuffer() {
    return reinterpret_cast<SILType*>(this+1);
  }
  SILType const *getInputTypeBuffer() const {
    return reinterpret_cast<SILType const *>(this+1);
  }
  
  SILFunctionTypeInfo(CanType swiftType,
                      unsigned inputTypeCount,
                      SILType resultType,
                      bool hasIndirectReturn)
    : swiftType(swiftType),
      resultType(resultType),
      inputTypeCount(inputTypeCount),
      indirectReturn(hasIndirectReturn)
  {}

public:
  static SILFunctionTypeInfo *create(CanType swiftType,
                                     ArrayRef<SILType> inputTypes,
                                     SILType resultType,
                                     bool hasIndirectReturn,
                                     SILModule &M);
  
  CanType getSwiftType() const { return swiftType; }
  
  /// Returns the list of input types needed to fully apply a function of
  /// this function type with an ApplyInst.
  ArrayRef<SILType> getInputTypes() const {
    return ArrayRef<SILType>(getInputTypeBuffer(), inputTypeCount);
  }
  
  /// Returns the result of an ApplyInst applied to this function type.
  SILType getResultType() const {
    return resultType;
  }
  
  /// True if this function type takes an indirect return address as its
  /// final argument.
  bool hasIndirectReturn() const {
    return bool(indirectReturn);
  }
  
  /// Get the indirect return argument type. Always an address.
  SILType getIndirectReturnType() const {
    assert(hasIndirectReturn() && "type doesn't have an indirect return?!");
    return getInputTypes().front();
  }
  
  /// Returns the list of input types, excluding the indirect return argument,
  /// if any.
  ArrayRef<SILType> getInputTypesWithoutIndirectReturnType() const {
    auto inputs = getInputTypes();
    return hasIndirectReturn() ? inputs.slice(1) : inputs;
  }
  
  /// Returns the type of the return type or the indirect return slot if
  /// present.
  SILType getSemanticResultType() const {
    return hasIndirectReturn() ? getIndirectReturnType() : getResultType();
  }  
};
  
} // end swift namespace

namespace llvm {

// Allow the low bit of SILType to be used for nefarious purposes, e.g. putting
// a SILType into a PointerUnion.
template<>
class PointerLikeTypeTraits<swift::SILType> {
public:
  static inline void *getAsVoidPointer(swift::SILType T) {
    return T.getOpaqueValue();
  }
  static inline swift::SILType getFromVoidPointer(void *P) {
    return swift::SILType::getFromOpaqueValue(P);
  }
  // SILType is just a wrapper around its ValueType, so it has a bit available.
  enum { NumLowBitsAvailable =
    PointerLikeTypeTraits<swift::SILType::ValueType>::NumLowBitsAvailable };
};


// Allow SILType to be used as a DenseMap key.
template<>
struct DenseMapInfo<swift::SILType> {
  using SILType = swift::SILType;
  using PointerMapInfo = DenseMapInfo<void*>;
public:
  static SILType getEmptyKey() {
    return SILType::getFromOpaqueValue(PointerMapInfo::getEmptyKey());
  }
  static SILType getTombstoneKey() {
    return SILType::getFromOpaqueValue(PointerMapInfo::getTombstoneKey());
  }
  static unsigned getHashValue(SILType t) {
    return PointerMapInfo::getHashValue(t.getOpaqueValue());
  }
  static bool isEqual(SILType LHS, SILType RHS) {
    return LHS == RHS;
  }
};

} // end llvm namespace


#endif
