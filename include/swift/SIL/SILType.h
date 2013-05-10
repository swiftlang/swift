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

#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILBase.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/ErrorHandling.h"

namespace swift {
  class ASTContext;
  class VarDecl;
  
namespace Lowering {
  class TypeConverter;
}

enum class SILTypeInfoKind {
  // SILTypeInfo,
  SILTypeInfo_First,
    SILFunctionTypeInfo = SILTypeInfo_First,
    SILCompoundTypeInfo,
  SILTypeInfo_Last = SILCompoundTypeInfo
};

/// SILTypeInfo - Abstract base class for SIL types that need to carry
/// additional information along with the underlying Swift type.
class alignas(8) SILTypeInfo : public SILAllocated<SILTypeInfo> {
  // alignas(8) because we need three tag bits on SILTypeInfo* for SILType.
  
  enum : unsigned { KindBits = 1 };
  static_assert(1U << KindBits > unsigned(SILTypeInfoKind::SILTypeInfo_Last),
                "not enough kind bits for all SILTypeInfo kinds");
  
  llvm::PointerIntPair<TypeBase*, KindBits, SILTypeInfoKind> SwiftTypeAndKind;

public:
  SILTypeInfo(SILTypeInfoKind kind, CanType swiftType)
    : SwiftTypeAndKind(swiftType.getPointer(), kind) {}
  
  SILTypeInfo(const SILTypeInfo &) = delete;
  SILTypeInfo &operator=(const SILTypeInfo &) = delete;
  
  SILTypeInfoKind getKind() const {
    return SwiftTypeAndKind.getInt();
  }
  CanType getSwiftType() const {
    return CanType(SwiftTypeAndKind.getPointer());
  }
  
  static bool classof(SILTypeInfo const *ti) {
    assert(ti->getKind() >= SILTypeInfoKind::SILTypeInfo_First
           && ti->getKind() <= SILTypeInfoKind::SILTypeInfo_Last
           && "invalid SILTypeInfo kind!");
    return true;
  }
};

class SILFunctionTypeInfo;
class SILCompoundTypeInfo;

}
  
namespace llvm {
  template<typename T>
  class PointerLikeTypeTraits;
  
  // SILTypeInfo* is always at least eight-byte aligned; make the three tag bits
  // available through PointerLikeTypeTraits.
  template<>
  class PointerLikeTypeTraits<swift::SILTypeInfo*> {
  public:
    static inline void *getAsVoidPointer(swift::SILTypeInfo *I) {
      return (void*)I;
    }
    static inline swift::SILTypeInfo *getFromVoidPointer(void *P) {
      return (swift::SILTypeInfo*)P;
    }
    enum { NumLowBitsAvailable = 3 };
  };
}
 
namespace swift {

/// SILType - A Swift type that has been lowered to a SIL representation type.
/// In addition to the Swift type system, SIL adds the following types:
///
/// - an "address" type that can reference any Swift type (but cannot take the
///   address of an address). *T is the type of an address pointing at T.
/// - uncurried function types. For the function type (T) -> (U) -> (V) -> W,
///   there are uncurried function type levels:
///     (T)(U) -> (V) -> W
///     (T)(U)(V) -> W
///
/// SIL also has the notion of "loadable" vs "address-only" types: loadable
/// types have a fixed size and compile-time binary representation and thus
/// can be loaded from memory and represented as rvalues, whereas address-only
/// types do not have a known size or layout and must always be handled
/// indirectly in memory.
class SILType {
  using PointerType = llvm::PointerUnion<TypeBase *, SILTypeInfo *>;
  using ValueType = llvm::PointerIntPair<PointerType, 2>;
  ValueType value;

  enum {
    // Set if this is an address type.
    IsAddressFlag = 1 << 0,
    // Set if the underlying type is loadable. !IsAddress && !IsLoadable is
    // invalid.
    IsLoadableFlag = 1 << 1,
    
    // A SILType cannot be non-address and non-loadable. SILType can be unioned
    // with an eight-byte-aligned pointer type; SILType::isInvalid() will return
    // true for a pointer value that isn't a SILType.
    InvalidFlags = 0
  };
  
  static inline unsigned makeFlags(bool address, bool loadable) {
    return (address << 0) | (loadable << 1);
  }
  
  /// Private constructor. SILTypes are normally vended by
  /// TypeConverter::getLoweredType().
  SILType(CanType ty,
          bool address,
          bool loadable)
    : value(ty.getPointer(), makeFlags(address, loadable))
  {
    assert((address || loadable) &&
           "SILType can't be the value of an address-only type");
    assert(!(ty && ty->is<LValueType>()) &&
           "LValueTypes should be eliminated by SIL lowering");
    assert(!(ty && ty->is<AnyFunctionType>()) &&
           "SIL lowering must produce a SILFunctionTypeInfo for function types");
  }
  
  SILType(SILTypeInfo *ti,
          bool address,
          bool loadable)
    : value(ti, makeFlags(address, loadable))
  {
    assert((address || loadable) &&
           "SILType can't be the value of an address-only type");    
  }
  
  SILType(ValueType value)
    : value(value) {}

  friend class Lowering::TypeConverter;
  friend struct llvm::DenseMapInfo<SILType>;
public:
  SILType() = default;
  
  bool isNull() const { return value.getPointer().isNull(); }
  explicit operator bool() const { return bool(value.getPointer()); }
  
  bool isInvalid() const { return value.getInt() == InvalidFlags; }
  
  void dump() const;
  void print(raw_ostream &OS) const;
  
  /// Gets the address type referencing this type, or the type itself if it is
  /// already an address type.
  SILType getAddressType() const {
    return {{value.getPointer(), value.getInt() | IsAddressFlag}};
  }

  /// Gets the type referenced by an address type, or the type itself if it is
  /// not an address type. Invalid for address-only types.
  SILType getObjectType() const {
    assert((value.getInt() | IsLoadableFlag) &&
           "dereferencing an address-only address");
    return {{value.getPointer(), value.getInt() & ~IsAddressFlag}};
  }
  
  /// Returns the Swift type referenced by this SIL type.
  CanType getSwiftRValueType() const {
    PointerType p = value.getPointer();
    if (auto *ty = p.dyn_cast<TypeBase*>()) {
      return CanType(ty);
    }
    if (auto *ti = p.dyn_cast<SILTypeInfo*>()) {
      return ti->getSwiftType();
    }
    llvm_unreachable("unknown SILType pointer type");
  }

  /// Returns the Swift type equivalent to this SIL type. If the SIL type is
  /// an address type, returns an LValueType.
  CanType getSwiftType() const {
    CanType rvalueTy = getSwiftRValueType();
    if (isAddress()) {
      return LValueType::get(rvalueTy,
                             LValueType::Qual::DefaultForType,
                             rvalueTy->getASTContext())
              ->getCanonicalType();
    } else
      return rvalueTy;
  }
  
  /// Gives the SILFunctionTypeInfo for a function type. The type must be
  /// derived from a Swift FunctionType or PolymorphicFunctionType.
  SILFunctionTypeInfo *getFunctionTypeInfo() const {
    return cast<SILFunctionTypeInfo>(value.getPointer().get<SILTypeInfo*>());
  }
  
  /// Gets the SILCompoundTypeInfo for a compound type. The type must be
  /// derived from a Swift struct, tuple, oneof, or class type.
  SILCompoundTypeInfo *getCompoundTypeInfo() const {
    return cast<SILCompoundTypeInfo>(value.getPointer().get<SILTypeInfo*>());
  }

  /// Returns the uncurry level of the type. Returns zero for non-function
  /// types.
  unsigned getUncurryLevel() const;
  
  /// Returns the Swift return type of a function type at the right uncurry
  /// level.
  /// The SILType must refer to a function type.
  CanType getFunctionResultType() const {
    auto *fty = castTo<AnyFunctionType>();
    for (unsigned uncurry = 0;
         uncurry < getUncurryLevel();
         ++uncurry) {
      fty = fty->getResult()->castTo<AnyFunctionType>();
    }
    return CanType(fty->getResult());
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
  bool isAddress() const { return value.getInt() & IsAddressFlag; }
  /// True if the type, or the referenced type of an address type, is loadable.
  /// This is the opposite of isAddressOnly.
  bool isLoadable() const { return value.getInt() & IsLoadableFlag; }
  /// True if the type, or the referenced type of an address type, is
  /// address-only. This is the opposite of isLoadable.
  bool isAddressOnly() const { return !isLoadable(); }

  /// Returns true if the referenced type has reference semantics.
  bool hasReferenceSemantics() const {
    return getSwiftRValueType()->hasReferenceSemantics();
  }
  /// Returns true if the referenced type is an existential type.
  bool isExistentialType() const {
    return getSwiftRValueType()->isExistentialType();
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
};

/// A high-level calling convention.
enum class AbstractCC : unsigned char {
  /// The C calling convention.
  C,
  
  /// The calling convention used for calling a normal function.
  Freestanding,
  
  /// The calling convention used for calling an instance method.
  Method
};
  
/// SILFunctionTypeInfo - SILType for a FunctionType or PolymorphicFunctionType.
/// Specifies the uncurry level and SIL-level calling convention for the
/// function.
class SILFunctionTypeInfo : public SILTypeInfo {
  SILType resultType;
  unsigned inputTypeCount : 24;
  AbstractCC cc : 8;
  unsigned uncurryCount : 31;
  unsigned indirectReturn : 1;

  SILType *getInputTypeBuffer() {
    return reinterpret_cast<SILType*>(this+1);
  }
  SILType const *getInputTypeBuffer() const {
    return reinterpret_cast<SILType const *>(this+1);
  }
  
  unsigned *getUncurryBuffer() {
    return reinterpret_cast<unsigned*>(getInputTypeBuffer() + inputTypeCount);
  }
  unsigned const *getUncurryBuffer() const {
    return reinterpret_cast<unsigned const *>(
                                        getInputTypeBuffer() + inputTypeCount);
  }
  
  SILFunctionTypeInfo(CanType swiftType,
                      unsigned inputTypeCount,
                      SILType resultType,
                      unsigned uncurryCount,
                      bool hasIndirectReturn,
                      AbstractCC cc)
    : SILTypeInfo(SILTypeInfoKind::SILFunctionTypeInfo, swiftType),
      resultType(resultType),
      inputTypeCount(inputTypeCount),
      cc(cc),
      uncurryCount(uncurryCount),
      indirectReturn(hasIndirectReturn)
  {
    assert(uncurryCount >= 1 && "negative uncurry level?!");
  }

public:
  static SILFunctionTypeInfo *create(CanType swiftType,
                                     ArrayRef<SILType> inputTypes,
                                     SILType resultType,
                                     ArrayRef<unsigned> uncurriedInputCounts,
                                     bool hasIndirectReturn,
                                     AbstractCC cc,
                                     SILBase &base);
  
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
    return getInputTypes().back();
  }
  
  /// Returns the list of input types, excluding the indirect return argument,
  /// if any.
  ArrayRef<SILType> getInputTypesWithoutIndirectReturnType() const {
    auto inputs = getInputTypes();
    return hasIndirectReturn()
      ? inputs.slice(0, inputs.size() - 1)
      : inputs;
  }
  
  /// Returns the type of the return type or the indirect return slot if
  /// present.
  SILType getSemanticResultType() const {
    return hasIndirectReturn() ? getIndirectReturnType() : getResultType();
  }
  
  /// Get the uncurry level of this type.
  unsigned getUncurryLevel() const {
    return uncurryCount - 1;
  }
  
  /// True if this function type can be curried with a CurryInst.
  bool isUncurried() const {
    return uncurryCount > 1;
  }

  /// Returns an ArrayRef containing the offset of the first SIL argument
  /// used by each uncurry level of the function. For example, for a simple
  /// function of type (Int, Int) -> Int, this will contain {0}. For a curried
  /// function (Int, Int)(Int)(Int, (Int, Int)) -> Int, this will contain
  /// {0, 2, 3}.
  ArrayRef<unsigned> getUncurriedInputBegins() const {
    return {getUncurryBuffer(), uncurryCount};
  }
  
  /// Returns an ArrayRef containing the offset of the last SIL argument
  /// used by each uncurry level of the function. For example, for a simple
  /// function of type (Int, Int) -> Int, this will contain {2}. For a curried
  /// function (Int, Int)(Int)(Int, (Int, Int)) -> Int, this will contain
  /// {2, 3, 6}.
  ArrayRef<unsigned> getUncurriedInputEnds() const {
    return {getUncurryBuffer()+1, uncurryCount};
  }
  
  /// Returns the list of input types needed to partially apply a function of
  /// this function type with a CurryInst.
  ArrayRef<SILType> getCurryInputTypes() const {
    assert(isUncurried());
    return {getInputTypeBuffer(), getUncurryBuffer()[uncurryCount-1]};
  }
  
  /// Returns the list of input types corresponding to an uncurry level.
  ArrayRef<SILType> getInputTypesForCurryLevel(unsigned level) const {
    assert(level < uncurryCount && "uncurry level out of range");
    return getInputTypes().slice(
                       getUncurryBuffer()[level],
                       getUncurryBuffer()[level+1] - getUncurryBuffer()[level]);
  }
  
  /// Returns the abstract calling convention of the function type.
  AbstractCC getAbstractCC() const { return cc; }
  
  static bool classof(SILTypeInfo const *ti) {
    return ti->getKind() == SILTypeInfoKind::SILFunctionTypeInfo;
  }
};

/// SILCompoundTypeInfo - SILType for a compound type, such as a tuple,
/// struct, or class. This contains the type information for the type's elements
/// indexable by Extract, ElementAddr, or RefElementAddr instructions. For
/// nominal types, this also provides a mapping from Swift VarDecls to field
/// indices.
class SILCompoundTypeInfo : public SILTypeInfo {
public:
  struct Element {
    /// The type of the element in the compound type.
    SILType type;
    /// The decl associated with the element, if any. Null for tuple types.
    VarDecl *decl;
  };
private:
  size_t elementCount;
  
  Element *getElementBuffer() {
    return reinterpret_cast<Element*>(this+1);
  }
  
  Element const *getElementBuffer() const {
    return reinterpret_cast<Element const *>(this+1);
  }
  
  SILCompoundTypeInfo(CanType t, size_t elementCount)
    : SILTypeInfo(SILTypeInfoKind::SILCompoundTypeInfo, t),
      elementCount(elementCount) {}
  
public:
  static SILCompoundTypeInfo *create(CanType swiftType,
                                     ArrayRef<Element> elements,
                                     SILBase &base);
  
  ArrayRef<Element> getElements() const {
    return ArrayRef<Element>(getElementBuffer(), elementCount);
  }
  
  size_t getIndexOfMemberDecl(VarDecl *vd) const;
  
  static bool classof(SILTypeInfo const *ti) {
    return ti->getKind() == SILTypeInfoKind::SILCompoundTypeInfo;
  }
};
  
} // end swift namespace

namespace llvm {

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
