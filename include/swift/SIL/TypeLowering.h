//===--- TypeLowering.h - Convert Swift Types to SILTypes -------*- C++ -*-===//
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

#ifndef SWIFT_SIL_TYPELOWERING_H
#define SWIFT_SIL_TYPELOWERING_H

#include "swift/ABI/MetadataValues.h"
#include "swift/AST/CaptureInfo.h"
#include "swift/SIL/AbstractionPattern.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Allocator.h"

namespace clang {
  class Type;
}

namespace swift {
  class AnyFunctionRef;
  class ForeignErrorConvention;
  enum IsInitialization_t : bool;
  enum IsTake_t : bool;
  class SILBuilder;
  class SILLocation;
  class SILModule;
  class ValueDecl;

namespace Lowering {

/// The default convention for handling the callee object on thick
/// callees.
const ParameterConvention DefaultThickCalleeConvention =
  ParameterConvention::Direct_Owned;

/// Given an AST function type, return a type that is identical except
/// for using the given ExtInfo.
CanAnyFunctionType adjustFunctionType(CanAnyFunctionType type,
                                      AnyFunctionType::ExtInfo extInfo);

/// Change the given function type's representation.
inline CanAnyFunctionType adjustFunctionType(CanAnyFunctionType t,
                                          SILFunctionType::Representation rep) {
  auto extInfo = t->getExtInfo().withSILRepresentation(rep);
  return adjustFunctionType(t, extInfo);  
}

/// Change the given function type's representation.
inline CanAnyFunctionType adjustFunctionType(CanAnyFunctionType t,
                                          AnyFunctionType::Representation rep) {
  auto extInfo = t->getExtInfo().withRepresentation(rep);
  return adjustFunctionType(t, extInfo);  
}
  
/// Given a SIL function type, return a type that is identical except
/// for using the given ExtInfo.
CanSILFunctionType adjustFunctionType(CanSILFunctionType type,
                                      SILFunctionType::ExtInfo extInfo,
                                      ParameterConvention calleeConv);
inline CanSILFunctionType adjustFunctionType(CanSILFunctionType type,
                                      SILFunctionType::ExtInfo extInfo) {
  return adjustFunctionType(type, extInfo, type->getCalleeConvention());
}
inline CanSILFunctionType adjustFunctionType(CanSILFunctionType t,
                                         SILFunctionType::Representation rep) {
  if (t->getRepresentation() == rep) return t;
  auto extInfo = t->getExtInfo().withRepresentation(rep);
  
  return adjustFunctionType(t, extInfo,
                    extInfo.hasContext() ? DefaultThickCalleeConvention
                                         : ParameterConvention::Direct_Unowned);
}
  

/// Flag used to place context-dependent TypeLowerings in their own arena which
/// can be disposed when a generic context is exited.
enum IsDependent_t : unsigned {
  IsNotDependent = false,
  IsDependent = true
};
  
/// Extended type information used by SIL.
class TypeLowering {
public:
  enum IsTrivial_t : bool { IsNotTrivial, IsTrivial };
  enum IsAddressOnly_t : bool { IsNotAddressOnly, IsAddressOnly };
  enum IsReferenceCounted_t : bool {
    IsNotReferenceCounted,
    IsReferenceCounted
  };

private:
  /// The SIL type of values with this Swift type.
  SILType LoweredType;

  enum : unsigned {
    IsTrivialFlag     = 0x1,
    IsAddressOnlyFlag = 0x2,
    IsReferenceCountedFlag = 0x4,
  };
  unsigned Flags;

protected:  
  TypeLowering(SILType type, IsTrivial_t isTrivial,
               IsAddressOnly_t isAddressOnly,
               IsReferenceCounted_t isRefCounted)
    : LoweredType(type), Flags((isTrivial ? IsTrivialFlag : 0U) | 
                               (isAddressOnly ? IsAddressOnlyFlag : 0U) |
                               (isRefCounted ? IsReferenceCountedFlag : 0U)) {}

public:
  TypeLowering(const TypeLowering &) = delete;
  TypeLowering &operator=(const TypeLowering &) = delete;

  virtual ~TypeLowering() {}

  /// \brief Are r-values of this type passed as arguments indirectly by formal
  /// convention?
  ///
  /// This is independent of whether the SIL argument is address type.
  bool isFormallyPassedIndirectly() const { return isAddressOnly(); }

  /// \brief Are r-values of this type returned indirectly by formal convention?
  ///
  /// This is independent of whether the SIL result is address type.
  bool isFormallyReturnedIndirectly() const { return isAddressOnly(); }

  /// isAddressOnly - Returns true if the type is an address-only type. A type
  /// is address-only if it is a resilient value type, or if it is a fragile
  /// value type with a resilient member. In either case, the full layout of
  /// values of the type is unavailable to the compiler.
  bool isAddressOnly() const {
    return Flags & IsAddressOnlyFlag;
  }
  /// isLoadable - Returns true if the type is loadable, in other words, its
  /// full layout is available to the compiler. This is the inverse of
  /// isAddressOnly.
  bool isLoadable() const {
    return !isAddressOnly();
  }
  
  /// Returns true if the type is trivial, meaning it is a loadable
  /// value type with no reference type members that require releasing.
  bool isTrivial() const {
    return Flags & IsTrivialFlag;
  }
  
  /// Returns true if the type is a scalar reference-counted reference, which
  /// can be retained and released.
  bool isReferenceCounted() const {
    return Flags & IsReferenceCountedFlag;
  }

  /// getLoweredType - Get the type used to represent values of the Swift type
  /// in SIL.
  SILType getLoweredType() const {
    return LoweredType;
  }

  /// Returns true if the SIL type is an address.
  bool isAddress() const {
    return LoweredType.isAddress();
  }

  /// Return the semantic type.
  ///
  /// The semantic type is what a type pretends to be during
  /// type-checking: that is, the type that getTypeOfRValue would
  /// return on a variable of this type.
  SILType getSemanticType() const {
    // If you change this, change getSemanticTypeLowering() too.
    auto storageType = getLoweredType().getSwiftRValueType();
    if (auto refType = dyn_cast<ReferenceStorageType>(storageType))
      return SILType::getPrimitiveType(refType.getReferentType(),
                                       SILValueCategory::Object);
    return getLoweredType();
  }
  
  /// Return the lowering for the semantic type.
  inline const TypeLowering &getSemanticTypeLowering(TypeConverter &TC) const;

  /// Produce an exact copy of the value in the given address as a
  /// scalar.  The caller is responsible for destroying this value,
  /// e.g. by releasing it.
  ///
  /// This type must be loadable.
  virtual SILValue emitLoadOfCopy(SILBuilder &B,
                                  SILLocation loc,
                                  SILValue addr,
                                  IsTake_t isTake) const = 0;

  /// Store an exact copy of the given value in the destination
  /// address, taking ownership of it.
  ///
  /// This type must be loadable.
  ///
  /// Note that, for an assignment, this produces lowered code: that
  /// is, for non-POD types, an explicit load and release.  Therefore,
  /// it is generally not correct to call this during SIL-gen.
  virtual void emitStoreOfCopy(SILBuilder &B,
                               SILLocation loc,
                               SILValue value,
                               SILValue addr,
                               IsInitialization_t isInit) const = 0;

  /// Emit a store of \p value into \p addr given the StoreOwnershipQualifier
  /// qual.
  ///
  /// This abstracts over the differences in between trivial and non-trivial
  /// types.
  virtual void emitStore(SILBuilder &B, SILLocation loc, SILValue value,
                         SILValue addr, StoreOwnershipQualifier qual) const = 0;

  /// Emit a load from \p addr given the LoadOwnershipQualifier \p qual.
  ///
  /// This abstracts over the differences in between trivial and non-trivial
  /// types.
  virtual SILValue emitLoad(SILBuilder &B, SILLocation loc, SILValue addr,
                            LoadOwnershipQualifier qual) const = 0;

  /// Put an exact copy of the value in the source address in the
  /// destination address.
  virtual void emitCopyInto(SILBuilder &B,
                            SILLocation loc,
                            SILValue src,
                            SILValue dest,
                            IsTake_t isTake,
                            IsInitialization_t isInit) const = 0;

  /// Given an address, emit operations to destroy it.
  ///
  /// This produces canonicalized SIL.
  virtual void emitDestroyAddress(SILBuilder &B, SILLocation loc,
                                  SILValue value) const = 0;

  /// Given a +1 r-value which we are claiming ownership of, destroy it.
  ///
  /// Note that an r-value might be an address.
  virtual void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                                 SILValue value) const = 0;

  enum class LoweringStyle { Shallow, Deep };

  /// Emit a lowered 'release_value' operation.
  ///
  /// This type must be loadable.
  virtual void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc,
                                       SILValue value,
                                       LoweringStyle loweringStyle) const = 0;

  void emitLoweredDestroyChildValue(SILBuilder &B, SILLocation loc,
                                    SILValue value,
                                    LoweringStyle loweringStyle) const {
    if (loweringStyle != LoweringStyle::Shallow)
      return emitLoweredDestroyValue(B, loc, value, loweringStyle);
    return emitDestroyValue(B, loc, value);
  }

  /// Emit a lowered 'release_value' operation.
  ///
  /// This type must be loadable.
  void emitLoweredDestroyValueShallow(SILBuilder &B, SILLocation loc,
                                      SILValue value) const {
    emitLoweredDestroyValue(B, loc, value, LoweringStyle::Shallow);
  }

  /// Emit a lowered 'release_value' operation.
  ///
  /// This type must be loadable.
  void emitLoweredDestroyValueDeep(SILBuilder &B, SILLocation loc,
                                   SILValue value) const {
    emitLoweredDestroyValue(B, loc, value, LoweringStyle::Deep);
  }

  /// Given a primitively loaded value of this type (which must be
  /// loadable), -1 it.
  ///
  /// This should be used when dropping a value which has been copied
  /// from place to place with exactly the same semantics.  For
  /// example, it performs an unowned_release on a value of [unknown]
  /// type.  It is therefore not necessarily the right thing to do on
  /// a semantic load.
  virtual void emitDestroyValue(SILBuilder &B, SILLocation loc,
                                SILValue value) const = 0;

  /// Emit a lowered 'retain_value' operation.
  ///
  /// This type must be loadable.
  virtual SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                        SILValue value,
                                        LoweringStyle style) const = 0;

  /// Emit a lowered 'retain_value' operation.
  ///
  /// This type must be loadable.
  SILValue emitLoweredCopyValueShallow(SILBuilder &B, SILLocation loc,
                                       SILValue value) const {
    return emitLoweredCopyValue(B, loc, value, LoweringStyle::Shallow);
  }

  /// Emit a lowered 'retain_value' operation.
  ///
  /// This type must be loadable.
  SILValue emitLoweredCopyValueDeep(SILBuilder &B, SILLocation loc,
                                    SILValue value) const {
    return emitLoweredCopyValue(B, loc, value, LoweringStyle::Deep);
  }

  /// Given a primitively loaded value of this type (which must be
  /// loadable), Perform a copy of this value. This is equivalent to performing
  /// +1 on class values.
  ///
  /// This should be used for duplicating a value from place to place
  /// with exactly the same semantics.  For example, it performs an
  /// unowned_retain on a value of [unknown] type.  It is therefore
  /// not necessarily the right thing to do on a semantic load.
  virtual SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                                 SILValue value) const = 0;

  SILValue emitLoweredCopyChildValue(SILBuilder &B, SILLocation loc,
                                     SILValue value,
                                     LoweringStyle style) const {
    if (style != LoweringStyle::Shallow) {
      return emitLoweredCopyValue(B, loc, value, style);
    } else {
      return emitCopyValue(B, loc, value);
    }
  }

  /// Allocate a new TypeLowering using the TypeConverter's allocator.
  void *operator new(size_t size, TypeConverter &tc,
                     IsDependent_t dependent);
  void *operator new[](size_t size, TypeConverter &tc,
                       IsDependent_t dependent);

  // Forbid 'new FooTypeLowering' and try to forbid 'delete tl'.
  // The latter is made challenging because the existence of the
  // virtual destructor requires an accessible 'operator delete'.
  void *operator new(size_t) = delete;

protected:
  void operator delete(void*) {}
};

/// Type and lowering information about a constant function.
struct SILConstantInfo {
  /// The formal type of the constant, still curried.  For a normal
  /// function, this is just its declared type; for a getter or
  /// setter, computing this can be more involved.
  CanAnyFunctionType FormalInterfaceType;

  /// The uncurried and bridged type of the constant.
  CanAnyFunctionType LoweredInterfaceType;

  /// The SIL function type of the constant.
  CanSILFunctionType SILFnType;
  
  /// The generic environment used by the constant.
  GenericEnvironment *GenericEnv;
  
  SILType getSILType() const {
    return SILType::getPrimitiveObjectType(SILFnType);
  }

  friend bool operator==(SILConstantInfo lhs, SILConstantInfo rhs) {
    return lhs.FormalInterfaceType == rhs.FormalInterfaceType &&
           lhs.LoweredInterfaceType == rhs.LoweredInterfaceType &&
           lhs.SILFnType == rhs.SILFnType &&
           lhs.GenericEnv == rhs.GenericEnv;
  }
  friend bool operator!=(SILConstantInfo lhs, SILConstantInfo rhs) {
    return !(lhs == rhs);
  }
};

/// Different ways in which a function can capture context.
enum class CaptureKind {
  /// No context arguments are necessary.
  None,
  /// A local value captured as a mutable box.
  Box,
  /// A local value captured as a single pointer to storage (formed with
  /// @noescape closures).
  StorageAddress,
  /// A local value captured as a constant.
  Constant,
};


/// TypeConverter - helper class for creating and managing TypeLowerings.
class TypeConverter {
  friend class TypeLowering;

  llvm::BumpPtrAllocator IndependentBPA;
  /// BumpPtrAllocator for types dependent on contextual generic parameters,
  /// which is reset when the generic context is popped.
  llvm::BumpPtrAllocator DependentBPA;

  enum : unsigned {
    /// There is a unique entry with this uncurry level in the
    /// type-lowering map for every TLI we create.  The map has the
    /// responsibility to call the destructor for these entries.
    UniqueLoweringEntry = ~0U
  };

  struct CachingTypeKey {
    GenericSignature *Sig;
    AbstractionPattern::CachingKey OrigType;
    CanType SubstType;

    friend bool operator==(const CachingTypeKey &lhs,
                           const CachingTypeKey &rhs) {
      return lhs.Sig == rhs.Sig
          && lhs.OrigType == rhs.OrigType
          && lhs.SubstType == rhs.SubstType;
    }
    friend bool operator!=(const CachingTypeKey &lhs,
                           const CachingTypeKey &rhs) {
      return !(lhs == rhs);
    }
  };

  struct TypeKey {
    /// An unsubstituted version of a type, dictating its abstraction patterns.
    AbstractionPattern OrigType;

    /// The substituted version of the type, dictating the types that
    /// should be used in the lowered type.
    CanType SubstType;

    CachingTypeKey getCachingKey() const {
      assert(isCacheable());
      return { (OrigType.hasGenericSignature()
                   ? OrigType.getGenericSignature()
                   : nullptr),
               OrigType.getCachingKey(),
               SubstType };
    }

    bool isCacheable() const {
      return OrigType.hasCachingKey();
    }
    
    IsDependent_t isDependent() const {
      if (SubstType->hasTypeParameter())
        return IsDependent;
      return IsNotDependent;
    }
  };

  friend struct llvm::DenseMapInfo<CachingTypeKey>;
  
  TypeKey getTypeKey(AbstractionPattern origTy, CanType substTy) {
    return {origTy, substTy};
  }
  
  struct OverrideKey {
    SILDeclRef derived;
    SILDeclRef base;
    
    friend bool operator==(const OverrideKey &lhs,
                           const OverrideKey &rhs) {
      return lhs.derived == rhs.derived
          && lhs.base == rhs.base;
    }
    friend bool operator!=(const OverrideKey &lhs,
                           const OverrideKey &rhs) {
      return !(lhs == rhs);
    }
  };
  
  friend struct llvm::DenseMapInfo<OverrideKey>;

  /// Find a cached TypeLowering by TypeKey, or return null if one doesn't
  /// exist.
  const TypeLowering *find(TypeKey k);
  /// Insert a mapping into the cache.
  void insert(TypeKey k, const TypeLowering *tl);
  
  /// Mapping for types independent on contextual generic parameters, which is
  /// cleared when the generic context is popped.
  llvm::DenseMap<CachingTypeKey, const TypeLowering *> IndependentTypes;
  /// Mapping for types dependent on contextual generic parameters, which is
  /// cleared when the generic context is popped.
  llvm::DenseMap<CachingTypeKey, const TypeLowering *> DependentTypes;
  
  llvm::DenseMap<SILDeclRef, SILConstantInfo> ConstantTypes;
  
  llvm::DenseMap<OverrideKey, SILConstantInfo> ConstantOverrideTypes;
  
  llvm::DenseMap<AnyFunctionRef, CaptureInfo> LoweredCaptures;
  
  /// The current generic context signature.
  CanGenericSignature CurGenericContext;
  
  CanAnyFunctionType makeConstantInterfaceType(SILDeclRef constant);
  
  /// Get the generic environment for a constant.
  GenericEnvironment *getConstantGenericEnvironment(SILDeclRef constant);
  
  // Types converted during foreign bridging.
#define BRIDGING_KNOWN_TYPE(BridgedModule,BridgedType) \
  Optional<CanType> BridgedType##Ty;
#include "swift/SIL/BridgedTypes.def"

  const TypeLowering &getTypeLoweringForLoweredType(TypeKey key);
  const TypeLowering &getTypeLoweringForUncachedLoweredType(TypeKey key);

public:
  SILModule &M;
  ASTContext &Context;

  TypeConverter(SILModule &m);
  ~TypeConverter();
  TypeConverter(TypeConverter const &) = delete;
  TypeConverter &operator=(TypeConverter const &) = delete;

  /// Return the CaptureKind to use when capturing a decl.
  CaptureKind getDeclCaptureKind(CapturedValue capture);

  /// Return a most-general-possible abstraction pattern.
  AbstractionPattern getMostGeneralAbstraction() {
    return AbstractionPattern::getOpaque();
  }

  /// Get the calling convention used by witnesses of a protocol.
  static SILFunctionTypeRepresentation
  getProtocolWitnessRepresentation(ProtocolDecl *P) {
    // ObjC protocols use the objc method convention.
    if (P->isObjC())
      return SILFunctionTypeRepresentation::ObjCMethod;

    // Native protocols use the witness calling convention.
    return SILFunctionTypeRepresentation::WitnessMethod;
  }
  
  /// Get the calling convention used to call a declaration.
  SILFunctionTypeRepresentation getDeclRefRepresentation(SILDeclRef c);
  
  /// Get the method dispatch strategy for a protocol.
  static ProtocolDispatchStrategy getProtocolDispatchStrategy(ProtocolDecl *P);
  
  /// True if a protocol uses witness tables for dynamic dispatch.
  static bool protocolRequiresWitnessTable(ProtocolDecl *P) {
    return ProtocolDescriptorFlags::needsWitnessTable
             (getProtocolDispatchStrategy(P));
  }
  
  /// True if a type is passed indirectly at +0 when used as the "self"
  /// parameter of its own methods.
  ///
  /// TODO: We want this always to hold.
  static bool isIndirectPlusZeroSelfParameter(Type T) {
    // Calls through opaque protocols can be done with +0 rvalues.  This allows
    // us to avoid materializing copies of existentials.
    return !T->hasReferenceSemantics()
        && (T->isExistentialType() || T->is<ArchetypeType>());
  }
  
  static bool isIndirectPlusZeroSelfParameter(SILType T) {
    return isIndirectPlusZeroSelfParameter(T.getSwiftRValueType());
  }
  
  /// Lowers a Swift type to a SILType, and returns the SIL TypeLowering
  /// for that type.
  const TypeLowering &getTypeLowering(Type t) {
    AbstractionPattern pattern(CurGenericContext, t->getCanonicalType());
    return getTypeLowering(pattern, t);
  }

  /// Lowers a Swift type to a SILType according to the abstraction
  /// patterns of the given original type.
  const TypeLowering &getTypeLowering(AbstractionPattern origType,
                                      Type substType);

  /// Returns the SIL TypeLowering for an already lowered SILType. If the
  /// SILType is an address, returns the TypeLowering for the pointed-to
  /// type.
  const TypeLowering &getTypeLowering(SILType t);

  // Returns the lowered SIL type for a Swift type.
  SILType getLoweredType(Type t) {
    return getTypeLowering(t).getLoweredType();
  }

  // Returns the lowered SIL type for a Swift type.
  SILType getLoweredType(AbstractionPattern origType, Type substType) {
    return getTypeLowering(origType, substType).getLoweredType();
  }

  SILType getLoweredLoadableType(Type t) {
    const TypeLowering &ti = getTypeLowering(t);
    assert(ti.isLoadable() && "unexpected address-only type");
    return ti.getLoweredType();
  }

  AbstractionPattern getAbstractionPattern(AbstractStorageDecl *storage);
  AbstractionPattern getAbstractionPattern(VarDecl *var);
  AbstractionPattern getAbstractionPattern(SubscriptDecl *subscript);
  AbstractionPattern getIndicesAbstractionPattern(SubscriptDecl *subscript);
  AbstractionPattern getAbstractionPattern(EnumElementDecl *element);

  SILType getLoweredTypeOfGlobal(VarDecl *var);

  /// The return type of a materializeForSet contains a callback
  /// whose type cannot be represented in the AST because it is
  /// a polymorphic function value. This function returns the
  /// unsubstituted lowered type of this callback.
  CanSILFunctionType
  getMaterializeForSetCallbackType(AbstractStorageDecl *storage,
                                   CanGenericSignature genericSig,
                                   Type selfType);

  /// Return the SILFunctionType for a native function value of the
  /// given type.
  CanSILFunctionType getSILFunctionType(AbstractionPattern origType,
                                        CanFunctionType substFnType);

  /// Convert an AST function type into a SILFunctionType at a non-standard
  /// uncurry level.
  CanSILFunctionType getSILFunctionType(AbstractionPattern origType,
                                        CanFunctionType substFnType,
                                        unsigned uncurryLevel);

  /// Returns the formal type, lowered AST type, and SILFunctionType
  /// for a constant reference.
  SILConstantInfo getConstantInfo(SILDeclRef constant);
  
  /// Returns the SIL type of a constant reference.
  SILType getConstantType(SILDeclRef constant) {
    return getConstantInfo(constant).getSILType();
  }

  /// Returns the SILFunctionType for the given declaration.
  CanSILFunctionType getConstantFunctionType(SILDeclRef constant) {
    return getConstantInfo(constant).SILFnType;
  }
  
  /// Returns the SILParameterInfo for the given declaration's `self` parameter.
  /// `constant` must refer to a method.
  SILParameterInfo getConstantSelfParameter(SILDeclRef constant);
  
  /// Returns the SILFunctionType the given declaration must use to override.
  /// Will be the same as getConstantFunctionType if the declaration does
  /// not override.
  CanSILFunctionType getConstantOverrideType(SILDeclRef constant) {
    // Fast path if the constant isn't overridden.
    if (constant.getNextOverriddenVTableEntry().isNull())
      return getConstantFunctionType(constant);
    SILDeclRef base = constant;
    while (SILDeclRef overridden = base.getOverridden())
      base = overridden;
    
    return getConstantOverrideType(constant, base);
  }

  CanSILFunctionType getConstantOverrideType(SILDeclRef constant,
                                             SILDeclRef base) {
    return getConstantOverrideInfo(constant, base).SILFnType;
  }

  SILConstantInfo getConstantOverrideInfo(SILDeclRef constant,
                                          SILDeclRef base);

  /// Substitute the given function type so that it implements the
  /// given substituted type.
  CanSILFunctionType substFunctionType(CanSILFunctionType origFnType,
                                 CanAnyFunctionType origLoweredType,
                                 CanAnyFunctionType substLoweredInterfaceType,
                         const Optional<ForeignErrorConvention> &foreignError);
  
  /// Get the empty tuple type as a SILType.
  SILType getEmptyTupleType() {
    return getLoweredType(TupleType::getEmpty(Context));
  }
  
  /// Get a function type curried with its capture context.
  CanAnyFunctionType getFunctionInterfaceTypeWithCaptures(
                                              CanAnyFunctionType funcType,
                                              AnyFunctionRef closure);

  /// Describes what we're trying to compute a bridged type for.
  ///
  /// \see getLoweredBridgedType
  enum BridgedTypePurpose {
    ForArgument,
    ForNonOptionalResult, // A result that should not be made more optional
    ForResult,
    ForMemory,
  };

  /// Map an AST-level type to the corresponding foreign representation type we
  /// implicitly convert to for a given calling convention.
  Type getLoweredBridgedType(AbstractionPattern pattern, Type t,
                             SILFunctionTypeRepresentation rep,
                             BridgedTypePurpose purpose);

  /// Convert a nested function type into an uncurried AST representation.
  CanAnyFunctionType getLoweredASTFunctionType(CanAnyFunctionType t,
                                               unsigned uncurryLevel,
                                               AnyFunctionType::ExtInfo info,
                                               Optional<SILDeclRef> constant);

  /// Convert a nested function type into an uncurried AST representation.
  CanAnyFunctionType getLoweredASTFunctionType(CanAnyFunctionType t,
                                               unsigned uncurryLevel,
                                               Optional<SILDeclRef> constant) {
    return getLoweredASTFunctionType(t, uncurryLevel, t->getExtInfo(),
                                     constant);
  }

  /// Given a referenced value and the substituted formal type of a
  /// resulting l-value expression, produce the substituted formal
  /// type of the storage of the value.
  ///
  /// \return - always an address type
  SILType getSubstitutedStorageType(AbstractStorageDecl *value,
                                    Type lvalueType);

  /// Retrieve the set of archetypes closed over by the given function.
  GenericEnvironment *getEffectiveGenericEnvironment(AnyFunctionRef fn,
                                                     CaptureInfo captureInfo);

  /// Retrieve the set of generic parameters closed over by the given function.
  CanGenericSignature getEffectiveGenericSignature(AnyFunctionRef fn,
                                                   CaptureInfo captureInfo);

  /// Push a generic function context. See GenericContextScope for an RAII
  /// interface to this function.
  ///
  /// Types containing generic parameter references must be lowered in a generic
  /// context. There can be at most one level of generic context active at any
  /// point in time.
  void pushGenericContext(CanGenericSignature sig);

  /// Return the current generic context.  This should only be used in
  /// the type-conversion routines.
  CanGenericSignature getCurGenericContext() const {
    return CurGenericContext;
  }
  
  /// Pop a generic function context. See GenericContextScope for an RAII
  /// interface to this function. There must be an active generic context.
  void popGenericContext(CanGenericSignature sig);
  
  /// Known types for bridging.
#define BRIDGING_KNOWN_TYPE(BridgedModule,BridgedType) \
  CanType get##BridgedType##Type();
#include "swift/SIL/BridgedTypes.def"

  /// Get the capture list from a closure, with transitive function captures
  /// flattened.
  CaptureInfo getLoweredLocalCaptures(AnyFunctionRef fn);
  bool hasLoweredLocalCaptures(AnyFunctionRef fn);

  enum class ABIDifference : uint8_t {
    // No ABI differences, function can be trivially bitcast to result type.
    Trivial,
    // Representation difference requires thin-to-thick conversion.
    ThinToThick,
    // Non-trivial difference requires thunk.
    NeedsThunk
  };
  
  /// \brief Test if type1 is ABI compatible with type2, and can be converted
  /// with a trivial bitcast.
  ///
  /// Note that type1 and type2 must be lowered types, and type1 must be a
  /// subtype of type2.
  ///
  /// The ABI compatible relation is not symmetric on function types -- while
  /// T and T! are both subtypes of each other, a calling convention conversion
  /// of T! to T always requires a thunk.
  ABIDifference checkForABIDifferences(SILType type1, SILType type2);

  /// \brief Same as above but for SIL function types.
  ABIDifference checkFunctionForABIDifferences(SILFunctionType *fnTy1,
                                               SILFunctionType *fnTy2);


  /// Lower the function type as a possible substitution for the type of
  /// \p constant. The result is not cached as part of the constant's normal
  /// ConstantInfo.
  CanSILFunctionType
  getUncachedSILFunctionTypeForConstant(SILDeclRef constant,
                                  CanAnyFunctionType origInterfaceType);
  
  /// Get the boxed interface type to use for a capture of the given decl.
  CanSILBoxType
  getInterfaceBoxTypeForCapture(ValueDecl *captured,
                                CanType loweredInterfaceType,
                                bool isMutable);
  /// Get the boxed contextual type to use for a capture of the given decl
  /// in the given generic environment.
  CanSILBoxType
  getContextBoxTypeForCapture(ValueDecl *captured,
                              CanType loweredContextType,
                              GenericEnvironment *env,
                              bool isMutable);

private:
  CanType getLoweredRValueType(AbstractionPattern origType, CanType substType);

  Type getLoweredCBridgedType(AbstractionPattern pattern, Type t,
                              bool canBridgeBool,
                              bool bridgedCollectionsAreOptional);

  CanType getBridgedInputType(SILFunctionTypeRepresentation rep,
                              AbstractionPattern pattern,
                              CanType input);

  CanType getBridgedResultType(SILFunctionTypeRepresentation rep,
                               AbstractionPattern pattern,
                               CanType result,
                               bool suppressOptional);

  CanAnyFunctionType getBridgedFunctionType(AbstractionPattern fnPattern,
                                            CanAnyFunctionType fnType,
                                            AnyFunctionType::ExtInfo extInfo);
};

inline const TypeLowering &
TypeLowering::getSemanticTypeLowering(TypeConverter &TC) const {
  // If you change this, change getSemanticType() too.
  auto storageType = getLoweredType().getSwiftRValueType();
  if (auto refType = dyn_cast<ReferenceStorageType>(storageType))
    return TC.getTypeLowering(refType.getReferentType());
  return *this;
}

/// RAII interface to push a generic context.
class GenericContextScope {
  TypeConverter &TC;
  CanGenericSignature Sig;
public:
  GenericContextScope(TypeConverter &TC, CanGenericSignature sig)
    : TC(TC), Sig(sig)
  {
    TC.pushGenericContext(sig);
  }
  
  ~GenericContextScope() {
    TC.popGenericContext(Sig);
  }
  
private:
  GenericContextScope(const GenericContextScope&) = delete;
  GenericContextScope &operator=(const GenericContextScope&) = delete;
};
  
} // namespace Lowering
} // namespace swift

namespace llvm {
  template<> struct DenseMapInfo<swift::Lowering::TypeConverter::CachingTypeKey> {
    typedef swift::Lowering::TypeConverter::CachingTypeKey CachingTypeKey;

    using APCachingKey = swift::Lowering::AbstractionPattern::CachingKey;
    using CachingKeyInfo = DenseMapInfo<APCachingKey>;

    using CanTypeInfo = DenseMapInfo<swift::CanType>;

    // Use the second field because the first field can validly be null.
    static CachingTypeKey getEmptyKey() {
      return {nullptr, APCachingKey(), CanTypeInfo::getEmptyKey()};
    }
    static CachingTypeKey getTombstoneKey() {
      return {nullptr, APCachingKey(), CanTypeInfo::getTombstoneKey()};
    }
    static unsigned getHashValue(CachingTypeKey val) {
      auto hashSig =
        DenseMapInfo<swift::GenericSignature *>::getHashValue(val.Sig);
      auto hashOrig =
        CachingKeyInfo::getHashValue(val.OrigType);
      auto hashSubst =
        DenseMapInfo<swift::CanType>::getHashValue(val.SubstType);
      return hash_combine(hashSig, hashOrig, hashSubst);
    }
    static bool isEqual(CachingTypeKey LHS, CachingTypeKey RHS) {
      return LHS == RHS;
    }
  };

  template<> struct DenseMapInfo<swift::Lowering::TypeConverter::OverrideKey> {
    typedef swift::Lowering::TypeConverter::OverrideKey OverrideKey;

    using SILDeclRefInfo = DenseMapInfo<swift::SILDeclRef>;

    static OverrideKey getEmptyKey() {
      return {SILDeclRefInfo::getEmptyKey(), SILDeclRefInfo::getEmptyKey()};
    }
    static OverrideKey getTombstoneKey() {
      return {SILDeclRefInfo::getTombstoneKey(), SILDeclRefInfo::getTombstoneKey()};
    }
    static unsigned getHashValue(OverrideKey val) {
      return hash_combine(SILDeclRefInfo::getHashValue(val.base),
                          SILDeclRefInfo::getHashValue(val.derived));
    }
    static bool isEqual(OverrideKey LHS, OverrideKey RHS) {
      return LHS == RHS;
    }
  };
}

#endif
