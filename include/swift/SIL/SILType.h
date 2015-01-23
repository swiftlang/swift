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

#ifndef SWIFT_SIL_SILTYPE_H
#define SWIFT_SIL_SILTYPE_H

#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/ErrorHandling.h"
#include "swift/SIL/SILAllocated.h"
#include "llvm/ADT/Hashing.h"

namespace swift {
  class ASTContext;
  class VarDecl;

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
    assert(ty->isLegalSILType() &&
           "constructing SILType with type that should have been "
           "eliminated by SIL lowering");
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

  ///  Apply a substitution to the type to produce another lowered SIL type.
  static SILType substType(SILModule &silModule, Module *astModule,
                           TypeSubstitutionMap &subs, SILType SrcTy);

  ///  Apply a substitution to the function type.
  static CanSILFunctionType substFuncType(SILModule &silModule,
                                          Module *astModule,
                                          TypeSubstitutionMap &subs,
                                          CanSILFunctionType SrcTy,
                                          bool dropGenerics);

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
  /// an address type, returns an InOutType.
  CanType getSwiftType() const {
    CanType rvalueTy = getSwiftRValueType();
    if (isAddress())
      return CanInOutType::get(rvalueTy);
    return rvalueTy;
  }
  
  /// Returns the Swift return type of a function type.
  /// The SILType must refer to a function type.
  SILType getFunctionInterfaceResultType() const {
    return castTo<SILFunctionType>()->getSemanticResultSILType();
  }

  /// Returns true if this function type has an indirect argument.
  ///
  /// The SILType must refer to a function type.
  bool isFunctionTypeWithIndirectResult() const {
    return castTo<SILFunctionType>()->hasIndirectResult();
  }
  
  /// Returns the AbstractCC of a function type.
  /// The SILType must refer to a function type.
  AbstractCC getAbstractCC() const {
    return castTo<SILFunctionType>()->getAbstractCC();
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

  bool isVoid() const {
    return value.getPointer()->isVoid();
  }

  /// Retrieve the ClassDecl for a type that maps to a Swift class or
  /// bound generic class type.
  ClassDecl *getClassOrBoundGenericClass() const {
    return getSwiftRValueType().getClassOrBoundGenericClass();
  }
  /// Retrieve the StructDecl for a type that maps to a Swift struct or
  /// bound generic struct type.
  StructDecl *getStructOrBoundGenericStruct() const {
    return getSwiftRValueType().getStructOrBoundGenericStruct();
  }
  /// Retrieve the EnumDecl for a type that maps to a Swift enum or
  /// bound generic enum type.
  EnumDecl *getEnumOrBoundGenericEnum() const {
    return getSwiftRValueType().getEnumOrBoundGenericEnum();
  }
  /// Retrieve the NominalTypeDecl for a type that maps to a Swift
  /// nominal or bound generic nominal type.
  NominalTypeDecl *getNominalOrBoundGenericNominal() const {
    return getSwiftRValueType().getNominalOrBoundGenericNominal();
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
  ///
  /// This is equivalent to, but possibly faster than, calling
  /// M.Types.getTypeLowering(type).isAddressOnly().
  static bool isAddressOnly(CanType T, SILModule &M);

  /// Return true if this type must be returned indirectly.
  ///
  /// This is equivalent to, but possibly faster than, calling
  /// M.Types.getTypeLowering(type).isReturnedIndirectly().
  static bool isReturnedIndirectly(CanType type, SILModule &M) {
    return isAddressOnly(type, M);
  }

  /// Return true if this type must be passed indirectly.
  ///
  /// This is equivalent to, but possibly faster than, calling
  /// M.Types.getTypeLowering(type).isPassedIndirectly().
  static bool isPassedIndirectly(CanType type, SILModule &M) {
    return isAddressOnly(type, M);
  }

  /// Returns true if this type is or may be resilient.
  ///
  /// This is currently only implemented for nominal types.
  bool isResilient(SILModule &M) const;
  
  /// True if the type, or the referenced type of an address type, is loadable.
  /// This is the opposite of isAddressOnly.
  bool isLoadable(SILModule &M) const {
    return !isAddressOnly(M);
  }
  /// True if the type, or the referenced type of an address type, is
  /// address-only. This is the opposite of isLoadable.
  bool isAddressOnly(SILModule &M) const;

  /// True if the type, or the referenced type of an address type, is trivial.
  bool isTrivial(SILModule &M) const;

  /// Returns true if the referenced type has reference semantics.
  bool hasReferenceSemantics() const {
    return getSwiftRValueType().hasReferenceSemantics();
  }
  /// Returns true if the referenced type is guaranteed to have a
  /// single-retainable-pointer representation.
  bool hasRetainablePointerRepresentation() const {
    return getSwiftRValueType()->hasRetainablePointerRepresentation();
  }
  /// Returns true if the referenced type is an existential type.
  bool isExistentialType() const {
    return getSwiftRValueType().isExistentialType();
  }
  /// Returns true if the referenced type is any kind of existential type.
  bool isAnyExistentialType() const {
    return getSwiftRValueType().isAnyExistentialType();
  }
  /// Returns true if the referenced type is a class existential type.
  bool isClassExistentialType() const {
    return getSwiftRValueType()->isClassExistentialType();
  }
  
  /// True if the type is dependent on a generic signature.
  bool isDependentType() const {
    return getSwiftRValueType()->isDependentType();
  }

  /// True if the type involves any archetypes.
  bool hasArchetype() const {
    return getSwiftRValueType()->hasArchetype();
  }
  
  /// Returns the ASTContext for the referenced Swift type.
  const ASTContext &getASTContext() const {
    return getSwiftRValueType()->getASTContext();
  }
  
  /// True if the type is block-pointer-compatible, meaning it either is a block
  /// or is an Optional or ImplicitlyUnwrappedOptional with a block payload.
  bool isBlockPointerCompatible() const {
    CanType ty = getSwiftRValueType();
    if (auto optPayload = ty->getAnyOptionalObjectType()) {
      // The object type of Optional<T> is an unlowered AST type.
      auto fTy = optPayload->getAs<FunctionType>();
      if (!fTy)
        return false;
      return fTy->getRepresentation() == FunctionType::Representation::Block;
    }
      
    auto fTy = dyn_cast<SILFunctionType>(ty);
    if (!fTy)
      return false;
    return fTy->getRepresentation() == FunctionType::Representation::Block;
  }

  /// Given that this is a nominal type, return the lowered type of
  /// the given field.  Applies substitutions as necessary.  The
  /// result will be an address type if the base type is an address
  /// type or a class.
  SILType getFieldType(VarDecl *field, SILModule &M) const;

  /// Given that this is an enum type, return the lowered type of the
  /// data for the given element.  Applies substitutions as necessary.
  /// The result will have the same value category as the base type.
  SILType getEnumElementType(EnumElementDecl *elt, SILModule &M) const;

  /// Given that this is a tuple type, return the lowered type of the
  /// given tuple element.  The result will have the same value
  /// category as the base type.
  SILType getTupleElementType(unsigned index) const {
    return SILType(castTo<TupleType>().getElementType(index), getCategory());
  }

  /// Return the immediate superclass type of this type, or null if
  /// it's the most-derived type.
  SILType getSuperclass(LazyResolver *resolver) const {
    auto superclass = getSwiftRValueType()->getSuperclass(resolver);
    if (!superclass) return SILType();
    return SILType::getPrimitiveObjectType(superclass->getCanonicalType());
  }

  /// Return true if Ty is a subtype of this SILType, or null otherwise.
  bool isSuperclassOf(SILType Ty) const {
    return getSwiftRValueType()->isSuperclassOf(Ty.getSwiftRValueType(),
                                                nullptr);
  }

  /// Transform the function type SILType by replacing all of its interface
  /// generic args with the appropriate item from the substitution.
  ///
  /// Only call this with function types!
  SILType substGenericArgs(SILModule &M,
                                    ArrayRef<Substitution> Subs) const;

  SILType subst(SILModule &silModule, Module *astModule,
                TypeSubstitutionMap &subs) const;

  /// If this is a specialized generic type, return all substitutions used to
  /// generate it.
  ArrayRef<Substitution> gatherAllSubstitutions(SILModule &M);

  /// Return true if this type references a "ref" type.
  bool isHeapObjectReferenceType() const;

  /// Return the SILType corresponding to the underlying type of the given
  /// metatype type.
  ///
  /// *NOTE* Only call on SILTypes for metatype types.
  SILType getMetatypeInstanceType(SILModule& M) const;

  /// Returns true if this SILType is an aggregate that contains \p Ty
  bool aggregateContainsRecord(SILType Ty, SILModule &SILMod) const;
  
  /// Returns true if this SILType is an aggregate with unreferenceable storage,
  /// meaning it cannot be fully destructured in SIL.
  bool aggregateHasUnreferenceableStorage() const;

  /// Returns the lowered type for T if this type is Optional<T> or
  /// ImplicitlyUnwrappedOptional<T>; otherwise, return the null type.
  SILType getAnyOptionalObjectType(SILModule &SILMod,
                                   OptionalTypeKind &OTK) const;

  /// Returns true if this is the AnyObject SILType;
  bool isAnyObject() const { return getSwiftRValueType()->isAnyObject(); }

  //
  // Accessors for types used in SIL instructions:
  //
  
  /// Get the NativeObject type as a SILType.
  static SILType getNativeObjectType(const ASTContext &C);
  /// Get the UnknownObject type as a SILType.
  static SILType getUnknownObjectType(const ASTContext &C);
  /// Get the BridgeObject type as a SILType.
  static SILType getBridgeObjectType(const ASTContext &C);
  /// Get the RawPointer type as a SILType.
  static SILType getRawPointerType(const ASTContext &C);
  /// Get a builtin integer type as a SILType.
  static SILType getBuiltinIntegerType(unsigned bitWidth, const ASTContext &C);
  /// Get a builtin floating-point type as a SILType.
  static SILType getBuiltinFloatType(BuiltinFloatType::FPKind Kind,
                                     const ASTContext &C);
  /// Get the builtin word type as a SILType;
  static SILType getBuiltinWordType(const ASTContext &C);
  
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

// Statically prevent SILTypes from being directly cast to a type
// that's not legal as a SIL value.
#define NON_SIL_TYPE(ID)                                             \
template<> Can##ID##Type SILType::getAs<ID##Type>() const = delete;  \
template<> Can##ID##Type SILType::castTo<ID##Type>() const = delete; \
template<> bool SILType::is<ID##Type>() const = delete;
NON_SIL_TYPE(Function)
NON_SIL_TYPE(PolymorphicFunction)
NON_SIL_TYPE(AnyFunction)
NON_SIL_TYPE(LValue)
#undef NON_SIL_TYPE

CanSILFunctionType getNativeSILFunctionType(SILModule &M,
                                            CanType orig,
                                            CanAnyFunctionType subst,
                                            CanAnyFunctionType substInterface);

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, SILType T) {
  T.print(OS);
  return OS;
}

inline SILType SILParameterInfo::getSILType() const {
  if (isIndirect()) {
    return SILType::getPrimitiveAddressType(getType());
  } else {
    return SILType::getPrimitiveObjectType(getType());
  }
}

inline SILType
SILFunctionType::getParameterSILType(const SILParameterInfo &param) {
  return param.getSILType();
}

inline SILType SILFunctionType::getSILResult() const {
  return getResult().getSILType();
}

inline SILType SILResultInfo::getSILType() const {
  return SILType::getPrimitiveObjectType(getType());
}

inline SILType SILFunctionType::getSemanticResultSILType() const {
  return (hasIndirectResult() ? getIndirectResult().getSILType()
                              : getResult().getSILType());
}  
  
inline SILType SILBlockStorageType::getCaptureAddressType() const {
  return SILType::getPrimitiveAddressType(getCaptureType());
}
  
/// The hash of a SILType is the hash of its opaque value.
static inline llvm::hash_code hash_value(SILType V) {
  return llvm::hash_value(V.getOpaqueValue());
}
  
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
