//===--- TypeLowering.h - Convert Swift Types to SILTypes -------*- C++ -*-===//
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

#ifndef SIL_TypeLowering_h
#define SIL_TypeLowering_h

#include "swift/Basic/Optional.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILDeclRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Allocator.h"

namespace swift {
  class ValueDecl;
  class SILBuilder;
  class SILLocation;
  class SILModule;

namespace Lowering {

/// Given a function type or polymorphic function type, returns the same type
/// with [thin] and calling convention attributes added.
/// FIXME: The thinness of func decls should be checked by the Swift
/// typechecker.
Type getThinFunctionType(Type t, AbstractCC cc);
Type getThinFunctionType(Type t);

/// Given a function type or polymorphic function type, returns the same type
/// with the [thin] attribute removed and a calling convention attribute added.
/// FIXME: The thinness of func decls should be checked by the Swift
/// typechecker.
Type getThickFunctionType(Type t, AbstractCC cc);
Type getThickFunctionType(Type t);

/// CaptureKind - Different ways in which a function can capture context.
enum class CaptureKind {
  /// A local value captured as a mutable box.
  Box,
  /// A local value captured by value.
  Constant,
  /// A byref argument captured by address.
  Byref,
  /// A getter-only property.
  Getter,
  /// A settable property.
  GetterSetter
};
  
/// getDeclCaptureKind - Return the CaptureKind to use when capturing a decl.
CaptureKind getDeclCaptureKind(ValueDecl *capture);
  
/// TypeLowering - Extended type information used by SILGen.
class TypeLowering {
public:
  enum IsTrivial_t : bool { IsNotTrivial, IsTrivial };
  enum IsAddressOnly_t : bool { IsNotAddressOnly, IsAddressOnly };

private:
  /// The SIL type of values with this Swift type.
  SILType LoweredType;

  enum : unsigned {
    IsTrivialFlag     = 0x1,
    IsAddressOnlyFlag = 0x2,
  };
  unsigned Flags;

protected:  
  TypeLowering(SILType type, IsTrivial_t isTrivial,
               IsAddressOnly_t isAddressOnly)
    : LoweredType(type), Flags((isTrivial ? IsTrivialFlag : 0U) | 
                               (isAddressOnly ? IsAddressOnlyFlag : 0U)) {}

public:
  TypeLowering(const TypeLowering &) = delete;
  TypeLowering &operator=(const TypeLowering &) = delete;

  virtual ~TypeLowering() {}

  /// \brief Are r-values of this type passed as arguments indirectly?
  bool isPassedIndirectly() const {
    return isAddressOnly();
  }

  /// \brief Are r-values of this type returned indirectly?
  bool isReturnedIndirectly() const {
    return isAddressOnly();
  }

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
  
  /// isTrivial - Returns true if the type is trivial, meaning it is a loadable
  /// value type with no reference type members that require releasing.
  bool isTrivial() const {
    return Flags & IsTrivialFlag;
  }

  /// getLoweredType - Get the type used to represent values of the Swift type
  /// in SIL.
  SILType getLoweredType() const {
    return LoweredType;
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

  /// Given a +1 value of the semantic type which we are claiming
  /// ownership of, put it into the given address with either
  /// initialization or assignment semantics.
  virtual void emitSemanticStore(SILBuilder &B,
                                 SILLocation loc,
                                 SILValue value,
                                 SILValue addr,
                                 IsInitialization_t isInit) const = 0;
  void emitSemanticInitialize(SILBuilder &B, SILLocation loc,
                              SILValue value, SILValue addr) const {
    emitSemanticStore(B, loc, value, addr, IsInitialization);
  }
  void emitSemanticAssign(SILBuilder &B, SILLocation loc,
                          SILValue value, SILValue addr) const {
    emitSemanticStore(B, loc, value, addr, IsNotInitialization);
  }

  /// Given an address, emit operations to destroy it.
  ///
  /// This produces canonicalized SIL.
  virtual void emitDestroyAddress(SILBuilder &B, SILLocation loc,
                                  SILValue value) const = 0;

  /// Produce a +1 semantic value loaded from the given address.  This
  /// type must be loadable.
  ///
  /// For [unowned] types, the result will be of the referent type.
  virtual SILValue emitSemanticLoad(SILBuilder &B,
                                    SILLocation loc,
                                    SILValue addr,
                                    IsTake_t isTake) const = 0;

  /// Copy a semantic value from the given address into the given
  /// uninitialized result.  The type need not be loadable.
  virtual void emitSemanticLoadInto(SILBuilder &B,
                                    SILLocation loc,
                                    SILValue src,
                                    SILValue dest,
                                    IsTake_t isTake,
                                    IsInitialization_t isInit) const = 0;

  /// Given a +1 r-value which are are claiming ownership of, destroy it.
  ///
  /// Note that an r-value might be an address.
  virtual void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                                 SILValue value) const = 0;

  /// Given a primitively loaded value of this type (which must be
  /// loadable), +1 it.
  ///
  /// This should be used for duplicating a value from place to place
  /// with exactly the same semantics.  For example, it performs an
  /// unowned_release on a value of [unknown] type.  It is therefore
  /// not necessarily the right thing to do on a semantic load.
  virtual void emitRetain(SILBuilder &B, SILLocation loc,
                          SILValue value) const = 0;

  /// Given a primitively loaded value of this type (which must be
  /// loadable), -1 it.
  ///
  /// This should be used when dropping a value which has been copied
  /// from place to place with exactly the same semantics.  For
  /// example, it performs an unowned_release on a value of [unknown]
  /// type.  It is therefore not necessarily the right thing to do on
  /// a semantic load.
  virtual void emitRelease(SILBuilder &B, SILLocation loc,
                           SILValue value) const = 0;

  /// Allocate a new TypeLowering using the TypeConverter's allocator.
  void *operator new(size_t size, TypeConverter &tc);

  // Forbid 'new FooTypeLowering' and try to forbid 'delete tl'.
  // The latter is made challenging because the existence of the
  // virtual destructor requires an accessible 'operator delete'.
  void *operator new(size_t) = delete;
protected:
  void operator delete(void*) {}
};
  
/// Argument order of uncurried functions.
enum class UncurryDirection {
  LeftToRight,
  RightToLeft
};

/// TypeConverter - helper class for creating and managing TypeLowerings.
class TypeConverter {
  friend class TypeLowering;

  llvm::BumpPtrAllocator TypeLoweringBPA;

  enum : unsigned {
    /// There is a unique entry with this uncurry level in the
    /// type-lowering map for every TLI we create.  The map has the
    /// responsibility to call the destructor for these entries.
    UniqueLoweringEntry = ~0U
  };
  
  using TypeKey = std::pair<TypeBase *, unsigned>;
  TypeKey getTypeKey(CanType t, unsigned uncurryLevel) {
    return {t.getPointer(), uncurryLevel};
  }
  
  llvm::DenseMap<TypeKey, const TypeLowering *> Types;
  llvm::DenseMap<SILDeclRef, SILType> constantTypes;
  
  Type makeConstantType(SILDeclRef constant);
  
  // Types converted during foreign bridging.
#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType) \
  Optional<CanType> BridgedType##Ty; \
  Optional<CanType> NativeType##Ty;
#include "swift/SIL/BridgedTypes.def"

  const TypeLowering &getTypeLoweringForLoweredType(CanType type);
  const TypeLowering &getTypeLoweringForUncachedLoweredType(CanType type);
  
public:
  SILModule &M;
  ASTContext &Context;

  TypeConverter(SILModule &m);
  ~TypeConverter();
  TypeConverter(TypeConverter const &) = delete;
  TypeConverter &operator=(TypeConverter const &) = delete;

  /// Is the given declaration resilient from the current context?
  bool isResilient(Decl *D) {
    // FIXME
    return false;
  }

  /// Is the given declaration resilient in any theoretical component,
  /// possibly one compiled in the past?  This can lead to some
  /// sub-optimal decisions sometimes, but it has the merit of being
  /// guaranteed stable.
  bool isAnywhereResilient(Decl *D) {
    // FIXME
    return false;
  }

  /// Lowers a Swift type to a SILType, and returns the SIL TypeLowering
  /// for that type.
  const TypeLowering &getTypeLowering(Type t,unsigned uncurryLevel = 0);
  
  /// Returns the SIL TypeLowering for an already lowered SILType. If the
  /// SILType is an address, returns the TypeLowering for the pointed-to
  /// type.
  const TypeLowering &getTypeLowering(SILType t);
  
  // Returns the lowered SIL type for a Swift type.
  SILType getLoweredType(Type t, unsigned uncurryLevel = 0) {
    return getTypeLowering(t, uncurryLevel).getLoweredType();
  }

  SILType getLoweredLoadableType(Type t, unsigned uncurryLevel = 0) {
    const TypeLowering &ti = getTypeLowering(t, uncurryLevel);
    assert(ti.isLoadable() && "unexpected address-only type");
    return ti.getLoweredType();
  }
  
  /// Returns the SIL type of a constant reference.
  SILType getConstantType(SILDeclRef constant);
  
  /// Get the empty tuple type as a SILType.
  SILType getEmptyTupleType() {
    return getLoweredType(TupleType::getEmpty(Context));
  }
  
  /// Get a function type curried with its capture context.
  Type getFunctionTypeWithCaptures(AnyFunctionType *funcType,
                                   ArrayRef<ValueDecl*> captures,
                                   DeclContext *parentContext);
  
  /// Returns the type of the "this" parameter to methods of a type.
  Type getMethodThisType(Type thisType) const;
  
  /// Returns the type of a property accessor, () -> T for a getter,
  /// or (value:T) -> () for a setter. 'kind' must be one of the Kind constants
  /// from SILDeclRef, SILDeclRef::Getter or SILDeclRef::Setter.
  Type getPropertyType(SILDeclRef::Kind kind, Type propType) const;
  
  /// Returns the type of a subscript property accessor, Index -> () -> T
  /// for a getter, or Index -> (value:T) -> () for a setter.
  /// 'kind' must be one of the Kind constants
  /// from SILDeclRef, SILDeclRef::Getter or SILDeclRef::Setter.
  Type getSubscriptPropertyType(SILDeclRef::Kind kind,
                                Type indexType,
                                Type elementType) const;

  /// Get the type of a method of function type M for a type:
  ///   This -> M for a concrete This,
  ///   <T,U,...> This -> M for an unbound generic This,
  ///   or the type M of the function itself if the context type is null.
  Type getMethodTypeInContext(Type /*nullable*/ contextType,
                              Type methodType,
                              GenericParamList *genericParams = nullptr) const;
  
  /// Convert a nested function type into an uncurried representation.
  CanAnyFunctionType getUncurriedFunctionType(CanAnyFunctionType t,
                                              unsigned uncurryLevel);
  
  /// Get the uncurried argument order for a calling convention.
  static UncurryDirection getUncurryDirection(AbstractCC cc);
  
  /// Map an AST-level type to the corresponding foreign representation type we
  /// implicitly convert to for a given calling convention.
  Type getLoweredBridgedType(Type t, AbstractCC cc);

  /// Given a referenced value and the substituted formal type of a
  /// resulting l-value expression, produce the substituted formal
  /// type of the storage of the value.
  ///
  /// \return - always an address type
  SILType getSubstitutedStorageType(ValueDecl *value, Type lvalueType);
  
  /// Known types for bridging.
#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType) \
  CanType get##BridgedType##Type(); \
  CanType get##NativeType##Type();
#include "swift/SIL/BridgedTypes.def"
};

inline const TypeLowering &
TypeLowering::getSemanticTypeLowering(TypeConverter &TC) const {
  // If you change this, change getSemanticType() too.
  auto storageType = getLoweredType().getSwiftRValueType();
  if (auto refType = dyn_cast<ReferenceStorageType>(storageType))
    return TC.getTypeLowering(refType.getReferentType());
  return *this;
}
  
} // namespace Lowering
} // namespace swift

#endif
