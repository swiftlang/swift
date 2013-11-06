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
/// FIXME: The thinness of function decls should be checked by the Swift
/// typechecker.
Type getThinFunctionType(Type t, AbstractCC cc);
Type getThinFunctionType(Type t);

/// Given a function type or polymorphic function type, returns the same type
/// with the [thin] attribute removed and a calling convention attribute added.
/// FIXME: The thinness of function decls should be checked by the Swift
/// typechecker.
Type getThickFunctionType(Type t, AbstractCC cc);
Type getThickFunctionType(Type t);

/// CaptureKind - Different ways in which a function can capture context.
enum class CaptureKind {
  /// No context arguments are necessary.
  None,
  /// A local value captured as a mutable box.
  Box,
  /// A local value captured by value.
  Constant,
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

  /// Produce a exact copy of the value in the given address as a
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

  /// Given a +1 r-value which are are claiming ownership of, destroy it.
  ///
  /// Note that an r-value might be an address.
  virtual void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                                 SILValue value) const = 0;

  /// Emit a lowered 'copy_value' operation.
  ///
  /// This type must be loadable.
  virtual SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                        SILValue value, bool deep) const = 0;

  /// Emit a lowered 'destroy_value' operation.
  ///
  /// This type must be loadable.
  virtual void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc,
                                       SILValue value, bool deep) const = 0;

  void emitLoweredDestroyChildValue(SILBuilder &B, SILLocation loc,
                                    SILValue value, bool wasDeep) const {
    if (wasDeep) {
      return emitLoweredDestroyValue(B, loc, value, true);
    } else {
      return emitDestroyValue(B, loc, value);
    }
  }

  /// Emit a lowered 'destroy_value' operation.
  ///
  /// This type must be loadable.
  void emitLoweredDestroyValueShallow(SILBuilder &B, SILLocation loc,
                                      SILValue value) const {
    emitLoweredDestroyValue(B, loc, value, false);
  }

  /// Emit a lowered 'destroy_value' operation.
  ///
  /// This type must be loadable.
  void emitLoweredDestroyValueDeep(SILBuilder &B, SILLocation loc,
                                   SILValue value) const {
    emitLoweredDestroyValue(B, loc, value, true);
  }

  /// Given a primitively loaded value of this type (which must be
  /// loadable), +1 it.
  ///
  /// This should be used for duplicating a value from place to place
  /// with exactly the same semantics.  For example, it performs an
  /// unowned_retain on a value of [unknown] type.  It is therefore
  /// not necessarily the right thing to do on a semantic load.
  virtual SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                                 SILValue value) const = 0;

  SILValue emitLoweredCopyChildValue(SILBuilder &B, SILLocation loc,
                                     SILValue value, bool wasDeep) const {
    if (wasDeep) {
      return emitLoweredCopyValue(B, loc, value, true);
    } else {
      return emitCopyValue(B, loc, value);
    }
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

  /// Allocate a new TypeLowering using the TypeConverter's allocator.
  void *operator new(size_t size, TypeConverter &tc);

  // Forbid 'new FooTypeLowering' and try to forbid 'delete tl'.
  // The latter is made challenging because the existence of the
  // virtual destructor requires an accessible 'operator delete'.
  void *operator new(size_t) = delete;
protected:
  void operator delete(void*) {}
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
  
  /// Returns the type of the "self" parameter to methods of a type.
  Type getMethodSelfType(Type selfType) const;
  
  /// Convert a nested function type into an uncurried representation.
  CanAnyFunctionType getUncurriedFunctionType(CanAnyFunctionType t,
                                              unsigned uncurryLevel);
  
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
