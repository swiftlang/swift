//===--- SILType.h - Defines the SILType type -------------------*- C++ -*-===//
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
#include "swift/SIL/SILArgumentConvention.h"
#include "llvm/ADT/Hashing.h"
#include "swift/SIL/SILDeclRef.h"

namespace swift {
  class ASTContext;
  class VarDecl;

namespace Lowering {
  class AbstractionPattern;
  class TypeConverter;
}

} // end namespace swift
  
namespace swift {

/// How an existential type container is represented.
enum class ExistentialRepresentation {
  /// The type is not existential.
  None,
  /// The container uses an opaque existential container, with a fixed-sized
  /// buffer. The type is address-only and is manipulated using the
  /// {init,open,deinit}_existential_addr family of instructions.
  Opaque,
  /// The container uses a class existential container, which holds a reference
  /// to the class instance that conforms to the protocol. The type is
  /// reference-counted and is manipulated using the
  /// {init,open}_existential_ref family of instructions.
  Class,
  /// The container uses a metatype existential container, which holds a
  /// reference to the type metadata for a type that
  /// conforms to the protocol. The type is trivial, and is manipulated using
  /// the {init,open}_existential_metatype family of instructions.
  Metatype,
  /// The container uses a boxed existential container, which is a
  /// reference-counted buffer that indirectly contains the
  /// conforming value. The type is manipulated
  /// using the {alloc,open,dealloc}_existential_box family of instructions.
  /// The container may be able to directly adopt a class reference using
  /// init_existential_ref for some class types.
  Boxed,
};

/// The value category.
enum class SILValueCategory : uint8_t {
  /// An object is a value of the type.
  Object,

  /// An address is a pointer to an allocated variable of the type
  /// (possibly uninitialized).
  Address,
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

  ///  Apply a substitution to the function type.
  static CanSILFunctionType substFuncType(SILModule &silModule,
                                          const SubstitutionMap &subs,
                                          CanSILFunctionType SrcTy,
                                          bool dropGenerics);

  bool isNull() const { return value.getPointer() == nullptr; }
  explicit operator bool() const { return bool(value.getPointer()); }

  SILValueCategory getCategory() const {
    return SILValueCategory(value.getInt());
  }

  /// Returns the \p Category variant of this type.
  SILType getCategoryType(SILValueCategory Category) const {
    return SILType(getSwiftRValueType(), Category);
  }

  /// Returns the variant of this type that matches \p Ty.getCategory()
  SILType copyCategory(SILType Ty) const {
    return getCategoryType(Ty.getCategory());
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
  
  /// Returns the AbstractCC of a function type.
  /// The SILType must refer to a function type.
  SILFunctionTypeRepresentation getFunctionRepresentation() const {
    return castTo<SILFunctionType>()->getRepresentation();
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

  /// isAddressOnly - True if the type, or the referenced type of an address
  /// type, is address-only.  For example, it could be a resilient struct or
  /// something of unknown size.
  ///
  /// This is equivalent to, but possibly faster than, calling
  /// M.Types.getTypeLowering(type).isAddressOnly().
  static bool isAddressOnly(CanType T, SILModule &M,
                            CanGenericSignature Sig,
                            ResilienceExpansion Expansion);
  /// Return true if this type must be returned indirectly.
  ///
  /// This is equivalent to, but possibly faster than, calling
  /// M.Types.getTypeLowering(type).isReturnedIndirectly().
  static bool isFormallyReturnedIndirectly(CanType type, SILModule &M,
                                           CanGenericSignature Sig,
                                           ResilienceExpansion Expansion) {
    return isAddressOnly(type, M, Sig, Expansion);
  }

  /// Return true if this type must be passed indirectly.
  ///
  /// This is equivalent to, but possibly faster than, calling
  /// M.Types.getTypeLowering(type).isPassedIndirectly().
  static bool isFormallyPassedIndirectly(CanType type, SILModule &M,
                                         CanGenericSignature Sig,
                                         ResilienceExpansion Expansion) {
    return isAddressOnly(type, M, Sig, Expansion);
  }

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

  /// True if the type, or the referenced type of an address type, is a
  /// scalar reference-counted type.
  bool isReferenceCounted(SILModule &M) const;

  /// Returns true if the referenced type is a function type that never
  /// returns.
  bool isNoReturnFunction() const;

  /// Returns true if the referenced type has reference semantics.
  bool hasReferenceSemantics() const {
    return getSwiftRValueType().hasReferenceSemantics();
  }

  /// Returns true if the referenced type is any sort of class-reference type,
  /// meaning anything with reference semantics that is not a function type.
  bool isAnyClassReferenceType() const {
    return getSwiftRValueType().isAnyClassReferenceType();
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
  
  /// Returns the representation used by an existential type. If the concrete
  /// type is provided, this may return a specialized representation kind that
  /// can be used for that type. Otherwise, returns the most general
  /// representation kind for the type. Returns None if the type is not an
  /// existential type.
  ExistentialRepresentation
  getPreferredExistentialRepresentation(SILModule &M,
                                        Type containedType = Type()) const;
  
  /// Returns true if the existential type can use operations for the given
  /// existential representation when working with values of the given type,
  /// or when working with an unknown type if containedType is null.
  bool
  canUseExistentialRepresentation(SILModule &M,
                                  ExistentialRepresentation repr,
                                  Type containedType = Type()) const;
  
  /// True if the type contains a type parameter.
  bool hasTypeParameter() const {
    return getSwiftRValueType()->hasTypeParameter();
  }
  
  /// True if the type is bridgeable to an ObjC object pointer type.
  bool isBridgeableObjectType() const {
    return getSwiftRValueType()->isBridgeableObjectType();
  }

  static bool isClassOrClassMetatype(Type t) {
    if (auto *meta = t->getAs<AnyMetatypeType>()) {
      return bool(meta->getInstanceType()->getClassOrBoundGenericClass());
    } else {
      return bool(t->getClassOrBoundGenericClass());
    }
  }

  /// True if the type is a class type or class metatype type.
  bool isClassOrClassMetatype() {
    return isObject() && isClassOrClassMetatype(getSwiftRValueType());
  }

  /// True if the type involves any archetypes.
  bool hasArchetype() const {
    return getSwiftRValueType()->hasArchetype();
  }
  
  /// Returns the ASTContext for the referenced Swift type.
  const ASTContext &getASTContext() const {
    return getSwiftRValueType()->getASTContext();
  }

  /// True if the given type has at least the size and alignment of a native
  /// pointer.
  bool isPointerSizeAndAligned();

  /// True if the layout of `fromType` is known to cover the layout of
  /// `totype`. This is conservatively imprecise and is not
  /// reflexive. `fromType` may be larger than the given type and still be
  /// castable. It is the caller's responsibility to ensure that the overlapping
  /// fields are layout compatible.
  static bool canUnsafeCastValue(SILType fromType, SILType toType,
                                 SILModule &M);

  /// True if `operTy` can be cast by single-reference value into `resultTy`.
  static bool canRefCast(SILType operTy, SILType resultTy, SILModule &M);

  /// True if the type is block-pointer-compatible, meaning it either is a block
  /// or is an Optional with a block payload.
  bool isBlockPointerCompatible() const {
    // Look through one level of optionality.
    SILType ty = *this;
    if (auto optPayload = ty.getAnyOptionalObjectType()) {
      ty = optPayload;
    }
      
    auto fTy = ty.getAs<SILFunctionType>();
    if (!fTy)
      return false;
    return fTy->getRepresentation() == SILFunctionType::Representation::Block;
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

  /// Return true if Ty is a subtype of this exact SILType, or false otherwise.
  bool isExactSuperclassOf(SILType Ty) const {
    return getSwiftRValueType()->isExactSuperclassOf(Ty.getSwiftRValueType(),
                                                     nullptr);
  }

  /// Return true if Ty is a subtype of this SILType, or if this SILType
  /// contains archetypes that can be found to form a supertype of Ty, or false
  /// otherwise.
  bool isBindableToSuperclassOf(SILType Ty) const {
    return getSwiftRValueType()->isBindableToSuperclassOf(
                                                        Ty.getSwiftRValueType(),
                                                        nullptr);
  }

  /// Transform the function type SILType by replacing all of its interface
  /// generic args with the appropriate item from the substitution.
  ///
  /// Only call this with function types!
  SILType substGenericArgs(SILModule &M,
                           SubstitutionList Subs) const;

  SILType subst(SILModule &silModule,
                TypeSubstitutionFn subs,
                LookupConformanceFn conformances) const;

  SILType subst(SILModule &silModule, const SubstitutionMap &subs) const;

  /// If this is a specialized generic type, return all substitutions used to
  /// generate it.
  SubstitutionList gatherAllSubstitutions(SILModule &M);

  /// Return true if this type references a "ref" type that has a single pointer
  /// representation. Class existentials do not always qualify.
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

  /// Returns the lowered type for T if this type is Optional<T>;
  /// otherwise, return the null type.
  SILType getAnyOptionalObjectType() const;

  /// Unwraps one level of optional type.
  /// Returns the lowered T if the given type is Optional<T>.
  /// Otherwise directly returns the given type.
  SILType unwrapAnyOptionalType() const;

  /// Returns true if this is the AnyObject SILType;
  bool isAnyObject() const { return getSwiftRValueType()->isAnyObject(); }

  /// Returns the underlying referent SILType of an @sil_unowned or @sil_weak
  /// Type.
  SILType getReferentType(SILModule &M) const;

  /// Returns the hash code for the SILType.
  llvm::hash_code getHashCode() const {
    return llvm::hash_combine(*this);
  }

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

  /// Get the standard exception type.
  static SILType getExceptionType(const ASTContext &C);

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
NON_SIL_TYPE(AnyFunction)
NON_SIL_TYPE(LValue)
#undef NON_SIL_TYPE

CanSILFunctionType getNativeSILFunctionType(SILModule &M,
                        Lowering::AbstractionPattern orig,
                        CanAnyFunctionType substInterface,
                        Optional<SILDeclRef> constant = None,
                        SILDeclRef::Kind kind = SILDeclRef::Kind::Func);

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, SILType T) {
  T.print(OS);
  return OS;
}

inline SILType SILBlockStorageType::getCaptureAddressType() const {
  return SILType::getPrimitiveAddressType(getCaptureType());
}

/// The hash of a SILType is the hash of its opaque value.
static inline llvm::hash_code hash_value(SILType V) {
  return llvm::hash_value(V.getOpaqueValue());
}

inline SILType SILBoxType::getFieldType(SILModule &M, unsigned index) const {
  return SILType::getPrimitiveAddressType(getFieldLoweredType(M, index));
}

inline SILType SILField::getAddressType() const {
  return SILType::getPrimitiveAddressType(getLoweredType());
}
inline SILType SILField::getObjectType() const {
  return SILType::getPrimitiveObjectType(getLoweredType());
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
