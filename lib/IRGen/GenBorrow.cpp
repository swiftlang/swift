//===--- GenBorrow.cpp - LLVM type lowering of borrow types ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements TypeInfo subclasses for `Builtin.Borrow`.
//
//===----------------------------------------------------------------------===//

#include "FixedTypeInfo.h"
#include "GenBorrow.h"
#include "GenType.h"
#include "IRGen.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"
#include "NativeConventionSchema.h"
#include "ScalarTypeInfo.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/AST/TypeExpansionContext.h"
#include "llvm/Support/ErrorHandling.h"

using namespace swift;
using namespace irgen;

enum class BorrowTypeInfoSubclassKind : unsigned {
  BorrowByPointer,
  BorrowInline,
  BorrowNonFixed,
};

// Type info for `Builtin.Borrow`s that are represented as a pointer.
class BorrowByPointerTypeInfo final
  : public PODSingleScalarTypeInfo<BorrowByPointerTypeInfo, LoadableTypeInfo>
{
public:
  BorrowByPointerTypeInfo(IRGenModule &IGM)
    : PODSingleScalarTypeInfo(IGM.PtrTy, IGM.getPointerSize(),
                       IGM.TargetInfo.PointerSpareBits,
                       IGM.getPointerAlignment())
  {
    setSubclassKind((unsigned)BorrowTypeInfoSubclassKind::BorrowByPointer);
  }

  static bool classof(const BorrowByPointerTypeInfo *) { return true; }
  // NB: this classof implementation assumes that it is already known that
  // the `TypeInfo` being cast is converted from a `Builtin.Borrow`. Other
  // kinds of type may reuse the same subclass kind with other meanings.
  static bool classof(const TypeInfo *t) {
    return t->getSubclassKind()
      == (unsigned)BorrowTypeInfoSubclassKind::BorrowByPointer;
  }
};

// Type info for `Builtin.Borrow`s that are represented with an inline
// bitwise copy of the borrowed value.
class BorrowInlineTypeInfo final
  : public ScalarTypeInfo<BorrowInlineTypeInfo, LoadableTypeInfo>
{
  const LoadableTypeInfo &ReferentTI;
public:
  BorrowInlineTypeInfo(IRGenModule &IGM,
                       const LoadableTypeInfo &referentTI)
    : ScalarTypeInfo(referentTI.getStorageType(),
                     referentTI.getFixedSize(),
                     referentTI.getSpareBits(),
                     referentTI.getFixedAlignment(),
                     // Borrow is trivially destroyed and copyable even if
                     // the referent is not.
                     IsTriviallyDestroyable,
                     IsCopyable,
                     IsFixedSize,
                     IsABIAccessible),
      ReferentTI(referentTI)
  {
    setSubclassKind((unsigned)BorrowTypeInfoSubclassKind::BorrowInline);
  }

  unsigned getExplosionSize() const override {
    return ReferentTI.getExplosionSize();
  }

  // Copying the borrow is done by bitwise copying the referent's
  // representation.
  void loadAsCopy(IRGenFunction &IGF, Address addr, Explosion &explosion)
  const override {
    loadAsTake(IGF, addr, explosion);
  }
  void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &explosion)
  const override {
    ReferentTI.loadAsTake(IGF, addr, explosion);
  }

  void assign(IRGenFunction &IGF, Explosion &explosion, Address addr,
              bool isOutlined, SILType T) const override {
    initialize(IGF, explosion, addr, isOutlined);
  }
  void initialize(IRGenFunction &IGF, Explosion &explosion, Address addr,
                  bool isOutlined) const override {
    ReferentTI.initialize(IGF, explosion, addr, isOutlined);
  }

  void reexplode(Explosion &src, Explosion &dest) const override {
    ReferentTI.reexplode(src, dest);
  }
  void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest,
            Atomicity atomicity) const override {
    reexplode(src, dest);
  }
  void consume(IRGenFunction &IGF, Explosion &explosion,
               Atomicity atomicity, SILType T) const override {
    Explosion discard;
    reexplode(explosion, discard);
    (void)discard.claimAll();
  }

  void fixLifetime(IRGenFunction &IGF, Explosion &explosion) const override {
    Explosion discard;
    reexplode(explosion, discard);
    (void)discard.claimAll();
  }  

  void destroy(IRGenFunction &IGF, Address address, SILType T,
               bool isOutlined) const override {
    /*nop*/
  }

  void getSchema(ExplosionSchema &schema) const override {
    ReferentTI.getSchema(schema);
  }

  void addToAggLowering(IRGenModule &IGM, SwiftAggLowering &lowering,
                        Size offset) const override {
    ReferentTI.addToAggLowering(IGM, lowering, offset);
  }

  void packIntoEnumPayload(IRGenModule &IGM,
                           IRBuilder &builder,
                           EnumPayload &payload,
                           Explosion &source,
                           unsigned offset) const override {
    ReferentTI.packIntoEnumPayload(IGM, builder, payload, source, offset);
  }
  
  void unpackFromEnumPayload(IRGenFunction &IGF,
                             const EnumPayload &payload,
                             Explosion &target,
                             unsigned offset) const override {
    ReferentTI.unpackFromEnumPayload(IGF, payload, target, offset);
  }

  TypeLayoutEntry *buildTypeLayoutEntry(IRGenModule &IGM, SILType T,
                                        bool useStructLayouts) const override {
    return ReferentTI.buildTypeLayoutEntry(IGM, T, useStructLayouts);
  }

  static bool classof(const BorrowByPointerTypeInfo *) { return true; }
  // NB: this classof implementation assumes that it is already known that
  // the `TypeInfo` being cast is converted from a `Builtin.Borrow`. Other
  // kinds of type may reuse the same subclass kind with other meanings.
  static bool classof(const TypeInfo *t) {
    return t->getSubclassKind()
      == (unsigned)BorrowTypeInfoSubclassKind::BorrowInline;
  }
};

const TypeInfo *
TypeConverter::convertBuiltinBorrowType(BuiltinBorrowType *T) {
  // NB: Builtin.Borrow is reabstracted through, so we lower the referent as a
  // SIL type.
  auto referentTy = SILType::getPrimitiveObjectType(T->getReferentType());
  auto &referentTI = IGM.getTypeInfo(referentTy);
  
  // If the referent doesn't have fixed layout here, then the borrow layout
  // is also dependent.
  auto *fixedReferent = dyn_cast<FixedTypeInfo>(&referentTI);
  if (!fixedReferent) {
    // TODO
    llvm_unreachable("non-fixed borrow not implemented");
  } 

  // TODO: If the referent type's layout is dependent on a type defined in a
  // library-evolution-enabled module whose interface has been compiled with a
  // toolchain older than Xcode 26, then its runtime metadata won't have
  // the "bitwise borrowable" or "addressable for dependencies" value witness
  // flags set accurately. To ensure compatible runtime and compile-time
  // layout with older and newer versions of the binary framework, we should
  // also fall back to opaque non-fixed borrow layout for these types.

  // If the type parameter to `Builtin.Borrow` has any of the following
  // properties:
  // - it is passed indirectly, or
  // - it is non-bitwise-borrowable, or
  // - it is addressable-for-dependencies
  // then `Builtin.Borrow` uses a pointer representation, where the pointer
  // references the original value's location in memory.
  if (fixedReferent->nativeParameterValueSchema(IGM).requiresIndirect()
      || !fixedReferent->isBitwiseBorrowable(ResilienceExpansion::Maximal)
      || referentTy.isAddressableForDeps(IGM.getSILModule(),
                                         TypeExpansionContext::minimal())) {
    return new BorrowByPointerTypeInfo(IGM);
  }

  // Otherwise, the referent uses an inline representation for borrows.
  auto *loadableReferent = cast<LoadableTypeInfo>(fixedReferent);
  return new BorrowInlineTypeInfo(IGM, *loadableReferent);
}

void irgen::emitMakeBorrow(IRGenFunction &IGF, SILType borrowTy,
                           Explosion &referent,
                           Explosion &borrow) {
  ASSERT(borrowTy.is<BuiltinBorrowType>());

  // The borrow must use an inline representation.
  auto *tl = cast<BorrowInlineTypeInfo>(&IGF.IGM.getTypeInfo(borrowTy));

  // The borrow representation is a bitwise copy of the referent's
  // representation.
  tl->reexplode(referent, borrow);
}

void irgen::emitMakeBorrowFromAddress(IRGenFunction &IGF,
                                      SILType borrowTy,
                                      Address referent,
                                      Explosion &explosion) {
  ASSERT(borrowTy.is<BuiltinBorrowType>());

  // The borrow must use a pointer representation.
  ASSERT(isa<BorrowByPointerTypeInfo>(&IGF.IGM.getTypeInfo(borrowTy)));

  // The borrow representation just holds the address of the referent.
  explosion.add(referent.getAddress());
}

void irgen::emitInitBorrowAtAddress(IRGenFunction &IGF,
                                    SILType borrowTy,
                                    Address destBorrow,
                                    Address srcReferent) {
  ASSERT(borrowTy.is<BuiltinBorrowType>());

  auto &tl = IGF.IGM.getTypeInfo(borrowTy);

  // The address-only instruction form can handle any borrow representation.
  switch ((BorrowTypeInfoSubclassKind)tl.getSubclassKind()) {
  case BorrowTypeInfoSubclassKind::BorrowInline: {
    // The borrow is a bitwise copy of the referent's representation.
    auto *inlineTL = cast<BorrowInlineTypeInfo>(&tl);
    IGF.emitMemCpy(destBorrow, srcReferent, inlineTL->getFixedSize());
    return;
  }
  case BorrowTypeInfoSubclassKind::BorrowByPointer:
    // The borrow is a pointer to the referent.
    IGF.Builder.CreateStore(srcReferent.getAddress(), destBorrow);
    return;
    
  case BorrowTypeInfoSubclassKind::BorrowNonFixed:
    // The borrow representation is dependent on the referent type.
    // Ask the runtime to handle it.    
    llvm_unreachable("todo");
  }
  llvm_unreachable("invalid borrow representation!");
}

void irgen::emitDereferenceBorrow(IRGenFunction &IGF,
                                  SILType borrowTy,
                                  Explosion &borrow,
                                  Explosion &referent) {
  ASSERT(borrowTy.is<BuiltinBorrowType>());

  // The borrow must use an inline representation.
  auto *tl = cast<BorrowInlineTypeInfo>(&IGF.IGM.getTypeInfo(borrowTy));

  // The borrow representation is a bitwise copy of the referent's
  // representation.
  tl->reexplode(borrow, referent);
}

Address irgen::emitDereferenceBorrowToAddress(IRGenFunction &IGF,
                                              SILType borrowTy,
                                              Explosion &borrow) {
  auto builtinBorrowTy = borrowTy.castTo<BuiltinBorrowType>();

  // The borrow must use a pointer representation.
  ASSERT(isa<BorrowByPointerTypeInfo>(&IGF.IGM.getTypeInfo(borrowTy)));

  auto &referentTL = IGF.IGM.getTypeInfo(
    SILType::getPrimitiveObjectType(builtinBorrowTy->getReferentType()));

  auto addr = borrow.claimNext();
  
  return Address(addr, referentTL.getStorageType(),
                 referentTL.getBestKnownAlignment());
}

Address irgen::emitDereferenceBorrowAtAddress(IRGenFunction &IGF,
                                              SILType borrowTy,
                                              Address borrow) {
  auto builtinBorrowTy = borrowTy.castTo<BuiltinBorrowType>();

  auto &tl = IGF.IGM.getTypeInfo(borrowTy);

  // The address-only instruction form can handle any borrow representation.
  switch ((BorrowTypeInfoSubclassKind)tl.getSubclassKind()) {
  case BorrowTypeInfoSubclassKind::BorrowInline:
    // The borrow is a bitwise copy of the referent's representation, so
    // we can use the address of the borrow value as the address of the value
    // itself.
    return borrow;

  case BorrowTypeInfoSubclassKind::BorrowByPointer: {
    auto &referentTL = IGF.IGM.getTypeInfo(
      SILType::getPrimitiveObjectType(builtinBorrowTy->getReferentType()));

    // The borrow is a pointer to the referent. Load the pointer and use it as
    // the address of the value.
    auto addr = IGF.Builder.CreateLoad(borrow.getAddress(),
                                       IGF.IGM.PtrTy,
                                       IGF.IGM.getPointerAlignment());
    
    return Address(addr, referentTL.getStorageType(),
                   referentTL.getBestKnownAlignment());
  }
    
  case BorrowTypeInfoSubclassKind::BorrowNonFixed:
    // The borrow representation is dependent on the referent type.
    // Ask the runtime to handle it.    
    llvm_unreachable("todo");
  }
  llvm_unreachable("invalid borrow representation!");
}
