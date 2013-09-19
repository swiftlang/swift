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

#include "swift/AST/CanTypeVisitor.h"
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

/// The value category.
enum class SILValueCategory {
  /// An object is a value of the type.
  Object,

  /// An address is a pointer to an allocated variable of the type
  /// (possibly uninitialized).
  Address,

  /// Local storage is the container for a local allocation.  For
  /// statically-sized types, this is just the allocation itself.
  /// However, for dynamically-sized types (like archetypes or
  /// resilient structs), it may be some sort of fixed-size buffer,
  /// stack depth, or the like.
  LocalStorage
};

/// SILType - A Swift type that has been lowered to a SIL representation type.
/// In addition to the Swift type system, SIL adds "address" types that can
/// reference any Swift type (but cannot take the address of an address). *T
/// is the type of an address pointing at T.
///
class SILType {
public:
  /// The unsigned is a SILValueCategory.
  using ValueType = llvm::PointerIntPair<TypeBase *, 2, unsigned>;
private:
  ValueType value;

  /// Private constructor. SILTypes are normally vended by
  /// TypeConverter::getLoweredType().
  SILType(CanType ty, SILValueCategory category)
      : value(ty.getPointer(), unsigned(category)) {
    if (!ty) return;

    assert(!isa<LValueType>(ty) &&
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
  static SILType getPrimitiveType(CanType T, SILValueCategory category) {
    return SILType(T, category);
  }

  /// Form the type of an r-value, given a Swift type that either does
  /// not require any special handling or has already been
  /// appropriately lowered.
  static SILType getPrimitiveObjectType(CanType T) {
    return SILType(T, SILValueCategory::Object);
  }

  /// Form the type for the address of an object, given a Swift type
  /// that either does not require any special handling or has already
  /// been appropriately lowered.
  static SILType getPrimitiveAddressType(CanType T) {
    return SILType(T, SILValueCategory::Address);
  }

  /// Form the type for the backing storage of a locally-allocated
  /// object, given a Swift type that either does not require any
  /// special handling or has already been appropriately lowered.
  static SILType getPrimitiveLocalStorageType(CanType T) {
    return SILType(T, SILValueCategory::LocalStorage);
  }

  bool isNull() const { return bool(value.getPointer()); }
  explicit operator bool() const { return bool(value.getPointer()); }

  SILValueCategory getCategory() const {
    return SILValueCategory(value.getInt());
  }
  
  /// Returns the address variant of this type.  Instructions which
  /// manipulate memory will generally work with object addresses.
  SILType getAddressType() const {
    return SILType(getSwiftRValueType(), SILValueCategory::Address);
  }

  /// Returns the object variant of this type.  Note that address-only
  /// types are not legal to manipulate directly as objects in SIL.
  SILType getObjectType() const {
    return SILType(getSwiftRValueType(), SILValueCategory::Object);
  }

  /// Returns the local storage variant of this type.  Local
  /// allocations of dynamically-sized types generally require some
  /// sort of buffer.
  SILType getLocalStorageType() const {
    return SILType(getSwiftRValueType(), SILValueCategory::LocalStorage);
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
    return castTo<AnyFunctionType>().getResult();
  }
  
  /// Returns the AbstractCC of a function type.
  /// The SILType must refer to a function type.
  AbstractCC getAbstractCC() const {
    return castTo<AnyFunctionType>()->getAbstractCC();
  }

  /// Cast the Swift type referenced by this SIL type, or return null if the
  /// cast fails.
  template<typename TYPE>
  typename CanTypeWrapperTraits<TYPE>::type
  getAs() const { return dyn_cast<TYPE>(getSwiftRValueType()); }

  /// Cast the Swift type referenced by this SIL type, which must be of the
  /// specified subtype.
  template<typename TYPE>
  typename CanTypeWrapperTraits<TYPE>::type
  castTo() const { return cast<TYPE>(getSwiftRValueType()); }

  /// Returns true if the Swift type referenced by this SIL type is of the
  /// specified subtype.
  template<typename TYPE>
  bool is() const { return isa<TYPE>(getSwiftRValueType()); }
  
  /// Retrieve the ClassDecl for a type that maps to a Swift class or
  /// bound generic class type.
  ClassDecl *getClassOrBoundGenericClass() const {
    return getSwiftRValueType()->getClassOrBoundGenericClass();
  }
  /// Retrieve the StructDecl for a type that maps to a Swift struct or
  /// bound generic struct type.
  StructDecl *getStructOrBoundGenericStruct() const {
    return getSwiftRValueType()->getStructOrBoundGenericStruct();
  }
  /// Retrieve the UnionDecl for a type that maps to a Swift union or
  /// bound generic union type.
  UnionDecl *getUnionOrBoundGenericUnion() const {
    return getSwiftRValueType()->getUnionOrBoundGenericUnion();
  }
  
  /// True if the type is an address type.
  bool isAddress() const { return getCategory() == SILValueCategory::Address; }

  /// True if the type is an object type.
  bool isObject() const { return getCategory() == SILValueCategory::Object; }

  /// True if the type is a local-storage type.
  bool isLocalStorage() const {
    return getCategory() == SILValueCategory::LocalStorage;
  }

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
    return getSwiftRValueType().hasReferenceSemantics();
  }
  /// Returns true if the referenced type is an existential type.
  bool isExistentialType() const {
    return getSwiftRValueType()->isExistentialType();
  }
  /// Returns true if the referenced type is a class existential type.
  bool isClassExistentialType() const {
    return getSwiftRValueType()->isClassExistentialType();
  }
  
  /// Returns the ASTContext for the referenced Swift type.
  const ASTContext &getASTContext() const {
    return getSwiftRValueType()->getASTContext();
  }
  
  //
  // Accessors for types used in SIL instructions:
  //
  
  /// Get the ObjectPointer type as a SILType.
  static SILType getObjectPointerType(const ASTContext &C);
  /// Get the ObjCPointer type as a SILType.
  static SILType getObjCPointerType(const ASTContext &C);
  /// Get the RawPointer type as a SILType.
  static SILType getRawPointerType(const ASTContext &C);
  /// Get a builtin integer type as a SILType.
  static SILType getBuiltinIntegerType(unsigned bitWidth, const ASTContext &C);
  /// Get a builtin floating-point type as a SILType.
  static SILType getBuiltinFloatType(BuiltinFloatType::FPKind Kind,
                                     const ASTContext &C);
  
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

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, SILType T) {
  T.print(OS);
  return OS;
}

/// Redundant but expensive-to-recompute information about a SIL
/// FunctionType or PolymorphicFunctionType. Contains the details of the
/// SIL-level calling convention for the function, including its exploded
/// input argument types and whether it uses an indirect return argument.
class SILFunctionTypeInfo {
  /// Recursively destructure and visit tuple-type arguments
  template<typename F>
  class DestructuredArgumentTypeVisitor
    : public CanTypeVisitor<DestructuredArgumentTypeVisitor<F>>
  {
    const F &fn;
  public:
    DestructuredArgumentTypeVisitor(const F &fn) : fn(fn) { }

    void visitType(CanType t) {
      fn(t);
    }

    void visitTupleType(CanTupleType tt) {
      for (auto eltType : tt.getElementTypes()) {
        CanTypeVisitor<DestructuredArgumentTypeVisitor<F>>::visit(eltType);
      }
    }
  };

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

  friend class SILType;

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
  /// first argument.
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

  /// getSwiftArgumentType - Return the swift type of the argument, numbered by
  /// the arguments to an apply or partial_apply instruction.  If the function
  /// has an indirect return, this cannot ask about its argument slot.
  CanType getSwiftArgumentType(unsigned ArgNo) const;

  template <typename F>
  void visitSwiftArgumentTypes(const F &fn) const {
    DestructuredArgumentTypeVisitor<F> visitor(fn);
    visitor.visit(cast<AnyFunctionType>(getSwiftType()).getInput());
  }

  /// getSwiftResultType - Return the swift type of the result
  CanType getSwiftResultType() const;
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
