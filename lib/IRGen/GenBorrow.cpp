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
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"
#include "NativeConventionSchema.h"
#include "NonFixedTypeInfo.h"
#include "ScalarTypeInfo.h"
#include "swift/ABI/MetadataValues.h"
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
          SpareBitVector::getConstant(IGM.getPointerSize().getValueInBits(),
                                      false),
          IGM.getPointerAlignment())
  {
    setSubclassKind((unsigned)BorrowTypeInfoSubclassKind::BorrowByPointer);
  }

  void printForAbstractTypeLayoutInfo(
      IRGenModule &IGM, llvm::raw_ostream &OS,
      unsigned indentation) const override {
    printForAbstractTypeLayoutInfoBase(IGM, OS, indentation, this);
  }

  std::unique_ptr<SerializableHiddenTypeInfoRepresentation>
  createSerializableHiddenTypeInfoRepresentation(
      IRGenModule &) const override {
    unsupportedSerializableHiddenTypeInfoRepresentation();
  }

  static bool classof(const BorrowByPointerTypeInfo *) { return true; }
  // NB: this classof implementation assumes that it is already known that
  // the `TypeInfo` being cast is converted from a `Builtin.Borrow`. Other
  // kinds of type may reuse the same subclass kind with other meanings.
  static bool classof(const TypeInfo *t) {
    return t->getSubclassKind()
      == (unsigned)BorrowTypeInfoSubclassKind::BorrowByPointer;
  }

  bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
    return true;
  }

  unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const override {
    return 1;
  }

  APInt getFixedExtraInhabitantValue(IRGenModule &IGM, unsigned bits,
                                     unsigned index) const override {
    assert(index == 0);
    return APInt(bits, 0);
  }

  llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                       Address src,
                                       SILType T,
                                       bool isOutlined) const override {
    // Copied from BridgeObjectTypeInfo.
    src = IGF.Builder.CreateElementBitCast(src, IGF.IGM.IntPtrTy);
    auto val = IGF.Builder.CreateLoad(src);
    auto zero = llvm::ConstantInt::get(IGF.IGM.IntPtrTy, 0);
    auto isNonzero = IGF.Builder.CreateICmpNE(val, zero);
    // We either have extra inhabitant 0 or no extra inhabitant (-1).
    // Conveniently, this is just a sext i1 -> i32 away.
    return IGF.Builder.CreateSExt(isNonzero, IGF.IGM.Int32Ty);
  }

  void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                            Address dest, SILType T,
                            bool isOutlined) const override {
    // Copied from BridgeObjectTypeInfo.
    // There's only one extra inhabitant, 0.
    dest = IGF.Builder.CreateElementBitCast(dest, IGF.IGM.IntPtrTy);
    IGF.Builder.CreateStore(llvm::ConstantInt::get(IGF.IGM.IntPtrTy, 0),dest);
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

  void printForAbstractTypeLayoutInfo(
      IRGenModule &IGM, llvm::raw_ostream &OS,
      unsigned indentation) const override {
    printForAbstractTypeLayoutInfoBase(IGM, OS, indentation, this);
  }

  std::unique_ptr<SerializableHiddenTypeInfoRepresentation>
  createSerializableHiddenTypeInfoRepresentation(
      IRGenModule &) const override {
    unsupportedSerializableHiddenTypeInfoRepresentation();
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

  bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
    return ReferentTI.mayHaveExtraInhabitants(IGM);
  }

  unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const override {
    return ReferentTI.getFixedExtraInhabitantCount(IGM);
  }

  APInt getFixedExtraInhabitantValue(IRGenModule &IGM, unsigned bits,
                                     unsigned index) const override {
    return ReferentTI.getFixedExtraInhabitantValue(IGM, bits, index);
  }

  llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                       Address src,
                                       SILType T,
                                       bool isOutlined) const override {
    auto astBorrowTy = T.getASTType()->castTo<BuiltinBorrowType>();
    auto referentTy =
        SILType::getPrimitiveObjectType(astBorrowTy->getReferentType());
    return ReferentTI.getExtraInhabitantIndex(IGF, src, referentTy, isOutlined);
  }

  void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                            Address dest, SILType T,
                            bool isOutlined) const override {
    auto astBorrowTy = T.getASTType()->castTo<BuiltinBorrowType>();
    auto referentTy =
        SILType::getPrimitiveObjectType(astBorrowTy->getReferentType());
    return ReferentTI.storeExtraInhabitant(IGF, index, dest, referentTy,
                                           isOutlined);
  }

  APInt getFixedExtraInhabitantMask(IRGenModule &IGM) const override {
    return ReferentTI.getFixedExtraInhabitantMask(IGM);
  }

  TypeLayoutEntry *buildTypeLayoutEntry(IRGenModule &IGM, SILType T,
                                        bool useStructLayouts) const override {
    auto astBorrowTy = T.getASTType()->castTo<BuiltinBorrowType>();
    auto referentTy =
        SILType::getPrimitiveObjectType(astBorrowTy->getReferentType());
    return ReferentTI.buildTypeLayoutEntry(IGM, referentTy, useStructLayouts);
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

/// Whether a borrow of a value with the given layout properties is represented
/// by a bitwise copy of the value, rather than by a pointer to it. This must
/// match swift_getBorrowRepresentation in the runtime.
static llvm::Value *emitIsBorrowValueRepresentation(
    IRGenFunction &IGF, llvm::Value *size, llvm::Value *isBitwiseBorrowable,
    llvm::Value *isAddressableForDependencies) {
  auto isSmall = IGF.Builder.CreateICmpULE(size,
     llvm::ConstantInt::get(IGF.IGM.IntPtrTy,
                            IGF.IGM.getPointerSize().getValue() * 4));

  llvm::Value *v = IGF.Builder.CreateNot(isAddressableForDependencies);
  v = IGF.Builder.CreateAnd(v, isBitwiseBorrowable);
  v = IGF.Builder.CreateAnd(v, isSmall);
  return v;
}

class BorrowNonFixedTypeInfo final
  : public WitnessSizedTypeInfo<BorrowNonFixedTypeInfo>
{
public:
  BorrowNonFixedTypeInfo(IRGenModule &IGM)
    : WitnessSizedTypeInfo(IGM.OpaqueTy,
                           Alignment(1),
                           IsTriviallyDestroyable,
                           IsBitwiseTakableAndBorrowable,
                           IsCopyable,
                           IsABIAccessible)
  {
    setSubclassKind((unsigned)BorrowTypeInfoSubclassKind::BorrowNonFixed);
  }

  void printForAbstractTypeLayoutInfo(
      IRGenModule &IGM, llvm::raw_ostream &OS,
      unsigned indentation) const override {
    printForAbstractTypeLayoutInfoBase(IGM, OS, indentation, this);
  }

  std::unique_ptr<SerializableHiddenTypeInfoRepresentation>
  createSerializableHiddenTypeInfoRepresentation(
      IRGenModule &) const override {
    unsupportedSerializableHiddenTypeInfoRepresentation();
  }

  TypeLayoutEntry *buildTypeLayoutEntry(IRGenModule &IGM, SILType T,
                                        bool useStructLayouts) const override {
    return IGM.typeLayoutCache.getOrCreateArchetypeEntry(T.getObjectType());
  }

  void assignWithCopy(IRGenFunction &IGF, Address destAddr, Address srcAddr,
                      SILType T, bool isOutlined) const override {
    // 'Builtin.Borrow' is always bitwise copyable.
    IGF.Builder.CreateMemCpy(destAddr, srcAddr, getSize(IGF, T));
  }

  void initializeWithTake(IRGenFunction &IGF, Address destAddr, Address srcAddr,
                          SILType T, bool isOutlined,
                          bool zeroizeIfSensitive) const override {
    // 'Builtin.Borrow' is always bitwise copyable.
    IGF.Builder.CreateMemCpy(destAddr, srcAddr, getSize(IGF, T));
  }

  void initializeWithCopy(IRGenFunction &IGF, Address destAddr, Address srcAddr,
                          SILType T, bool isOutlined) const override {
    // 'Builtin.Borrow' is always bitwise copyable.
    IGF.Builder.CreateMemCpy(destAddr, srcAddr, getSize(IGF, T));
  }

  void assignWithTake(IRGenFunction &IGF, Address destAddr, Address srcAddr,
                      SILType T, bool isOutlined) const override {
    // 'Builtin.Borrow' is always bitwise copyable.
    IGF.Builder.CreateMemCpy(destAddr, srcAddr, getSize(IGF, T));
  }

  void destroy(IRGenFunction &IGF, Address address, SILType T,
               bool isOutlined) const override {
    // 'Builtin.Borrow' is always trivial to destroy, thus it's a nop.
  }

  llvm::Value *getIsValueRepresentation(IRGenFunction &IGF,
                                        SILType referentTy,
                                        const TypeInfo &referentTI) const {
    return emitIsBorrowValueRepresentation(
        IGF, referentTI.getSize(IGF, referentTy),
        referentTI.getIsBitwiseBorrowable(IGF, referentTy),
        referentTI.getIsAddressableForDependencies(IGF, referentTy));
  }

  llvm::Value *getEnumTagSinglePayload(IRGenFunction &IGF,
                                       llvm::Value *numEmptyCases,
                                       Address enumAddr, SILType T,
                                       bool isOutlined) const override {
    auto astBorrowTy = T.getASTType()->castTo<BuiltinBorrowType>();
    auto referentTy = SILType::getPrimitiveObjectType(astBorrowTy->getReferentType());

    auto &referentTI = IGF.getTypeInfo(referentTy);

    auto isValueRep = getIsValueRepresentation(IGF, referentTy, referentTI);
    auto isValueRepBB = IGF.createBasicBlock("is_value_representation");
    auto isAddrRepBB = IGF.createBasicBlock("is_address_representation");
    
    auto doneBB = IGF.createBasicBlock("done");

    IGF.Builder.CreateCondBr(isValueRep, isValueRepBB, isAddrRepBB);

    // We can use the referent's TypeInfo to emit the tag in value
    // representation.
    IGF.Builder.emitBlock(isValueRepBB);

    auto inlineResult = referentTI.getEnumTagSinglePayload(IGF, numEmptyCases,
                                                           enumAddr,
                                                           referentTy,
                                                           isOutlined);

    // Calling the referent's getEnumTagSinglePayload may have introduced new
    // blocks. Get the current one that contains the result and save it for the
    // phi node later.
    auto valueRepPhiBB = IGF.Builder.GetInsertBlock();

    IGF.Builder.CreateBr(doneBB);

    // In pointer representation, we take on RawPointer's extra inhabitant. 
    IGF.Builder.emitBlock(isAddrRepBB);

    auto pointerTy = SILType::getPrimitiveObjectType(IGF.IGM.Context.TheRawPointerType);
    auto &rawPointerTI = IGF.IGM.getRawPointerTypeInfo();
    auto pointerResult = rawPointerTI.getEnumTagSinglePayload(IGF, numEmptyCases,
                                                              enumAddr,
                                                              pointerTy,
                                                              isOutlined);

    // Calling the raw pointer's getEnumTagSinglePayload may have introduced new
    // blocks. Get the current one that contains the result and save it for the
    // phi node later.
    auto addrRepPhiBB = IGF.Builder.GetInsertBlock();

    IGF.Builder.CreateBr(doneBB);

    IGF.Builder.emitBlock(doneBB);
    auto phi = IGF.Builder.CreatePHI(IGF.IGM.Int32Ty, 2);
    phi->addIncoming(inlineResult, valueRepPhiBB);
    phi->addIncoming(pointerResult, addrRepPhiBB);

    return phi;
  }

  void storeEnumTagSinglePayload(IRGenFunction &IGF, llvm::Value *whichCase,
                                 llvm::Value *numEmptyCases, Address enumAddr,
                                 SILType T, bool isOutlined) const override {
    auto astBorrowTy = T.getASTType()->castTo<BuiltinBorrowType>();
    auto referentTy = SILType::getPrimitiveObjectType(astBorrowTy->getReferentType());

    auto &referentTI = IGF.getTypeInfo(referentTy);

    auto isValueRep = getIsValueRepresentation(IGF, referentTy, referentTI);
    auto isValueRepBB = IGF.createBasicBlock("is_value_representation");
    auto isAddrRepBB = IGF.createBasicBlock("is_address_representation");
    
    auto doneBB = IGF.createBasicBlock("done");

    IGF.Builder.CreateCondBr(isValueRep, isValueRepBB, isAddrRepBB);

    // We can use the referent's TypeInfo to emit the tag in value
    // representation.
    IGF.Builder.emitBlock(isValueRepBB);

    referentTI.storeEnumTagSinglePayload(IGF, whichCase, numEmptyCases, enumAddr,
                                         referentTy, isOutlined);

    IGF.Builder.CreateBr(doneBB);

    // In pointer representation, we take on RawPointer's extra inhabitant. 
    IGF.Builder.emitBlock(isAddrRepBB);

    auto pointerTy = SILType::getPrimitiveObjectType(IGF.IGM.Context.TheRawPointerType);
    auto &rawPointerTI = IGF.IGM.getRawPointerTypeInfo();
    rawPointerTI.storeEnumTagSinglePayload(IGF, whichCase, numEmptyCases, enumAddr,
                                           pointerTy, isOutlined);

    IGF.Builder.CreateBr(doneBB);

    IGF.Builder.emitBlock(doneBB);
  }
};

const TypeInfo *
TypeConverter::convertBuiltinBorrowType(BuiltinBorrowType *T) {
  // NB: Builtin.Borrow is reabstracted through, so we lower the referent as a
  // SIL type.
  auto referentTy = SILType::getPrimitiveObjectType(T->getReferentType());
  auto &referentTI = IGM.getTypeInfo(referentTy);

  // If the referent is always known to be or contain a raw layout type or a
  // known addressable for dependencies type, then its borrow representation
  // will always be by pointer regardless of if the type itself is fixed or not.
  auto referentProps = IGM.getTypeProperties(referentTy);

  if (referentProps.definitelyIsOrContainsRawLayout() ||
      referentProps.definitelyIsAddressableForDependencies()) {
    return new BorrowByPointerTypeInfo(IGM);
  }

  // If the referent doesn't have fixed layout here, then the borrow layout
  // is also dependent.
  auto *fixedReferent = dyn_cast<FixedTypeInfo>(&referentTI);
  if (!fixedReferent) {
    return new BorrowNonFixedTypeInfo(IGM);
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
    auto astBorrowTy = borrowTy.getASTType()->castTo<BuiltinBorrowType>();

    // If the runtime might not provide 'swift_initBorrow', do the work here.
    if (!IGF.IGM.isBorrowRuntimeAvailable()) {
      auto referentTy =
          SILType::getPrimitiveObjectType(astBorrowTy->getReferentType());
      auto &nonFixedTL = static_cast<const BorrowNonFixedTypeInfo &>(tl);
      auto &referentTI = IGF.getTypeInfo(referentTy);
      auto isValueRep =
          nonFixedTL.getIsValueRepresentation(IGF, referentTy, referentTI);
      auto valueBB = IGF.createBasicBlock("init_borrow.value");
      auto pointerBB = IGF.createBasicBlock("init_borrow.pointer");
      auto doneBB = IGF.createBasicBlock("init_borrow.done");
      IGF.Builder.CreateCondBr(isValueRep, valueBB, pointerBB);

      IGF.Builder.emitBlock(valueBB);
      IGF.Builder.CreateMemCpy(destBorrow, srcReferent,
                               referentTI.getSize(IGF, referentTy));
      IGF.Builder.CreateBr(doneBB);

      IGF.Builder.emitBlock(pointerBB);
      IGF.Builder.CreateStore(srcReferent.getAddress(), destBorrow);
      IGF.Builder.CreateBr(doneBB);

      IGF.Builder.emitBlock(doneBB);
      return;
    }

    // Otherwise, ask the runtime 'swift_initBorrow' to handle it.
    auto metadata = IGF.emitTypeMetadataRef(astBorrowTy->getReferentType());
    auto initBorrow = IGF.IGM.getInitBorrowFunctionPointer();

    IGF.Builder.CreateCall(initBorrow, {metadata, destBorrow.getAddress(),
                                        srcReferent.getAddress()});

    return;
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
  auto &referentTL = IGF.IGM.getTypeInfo(
      SILType::getPrimitiveObjectType(builtinBorrowTy->getReferentType()));

  // The address-only instruction form can handle any borrow representation.
  switch ((BorrowTypeInfoSubclassKind)tl.getSubclassKind()) {
  case BorrowTypeInfoSubclassKind::BorrowInline:
    // The borrow is a bitwise copy of the referent's representation, so
    // we can use the address of the borrow value as the address of the value
    // itself.
    return borrow;

  case BorrowTypeInfoSubclassKind::BorrowByPointer: {
    // The borrow is a pointer to the referent. Load the pointer and use it as
    // the address of the value.
    auto addr = IGF.Builder.CreateLoad(borrow.getAddress(),
                                       IGF.IGM.PtrTy,
                                       IGF.IGM.getPointerAlignment());
    
    return Address(addr, referentTL.getStorageType(),
                   referentTL.getBestKnownAlignment());
  }
    
  case BorrowTypeInfoSubclassKind::BorrowNonFixed: {
    // The borrow representation is dependent on the referent type.
    auto astBorrowTy = borrowTy.getASTType()->castTo<BuiltinBorrowType>();

    // If the runtime might not provide 'swift_dereferenceBorrow', do the work
    // here.
    if (!IGF.IGM.isBorrowRuntimeAvailable()) {
      auto referentSILTy =
          SILType::getPrimitiveObjectType(astBorrowTy->getReferentType());
      auto &nonFixedTL = static_cast<const BorrowNonFixedTypeInfo &>(tl);
      auto &referentTI = IGF.getTypeInfo(referentSILTy);
      auto isValueRep =
          nonFixedTL.getIsValueRepresentation(IGF, referentSILTy, referentTI);
      auto valueBB = IGF.createBasicBlock("deref_borrow.value");
      auto pointerBB = IGF.createBasicBlock("deref_borrow.pointer");
      auto doneBB = IGF.createBasicBlock("deref_borrow.done");
      IGF.Builder.CreateCondBr(isValueRep, valueBB, pointerBB);

      // The borrow is a bitwise copy of the referent, so the borrow's own
      // address is the referent's address.
      IGF.Builder.emitBlock(valueBB);
      IGF.Builder.CreateBr(doneBB);

      IGF.Builder.emitBlock(pointerBB);
      auto loaded = IGF.Builder.CreateLoad(borrow.getAddress(), IGF.IGM.PtrTy,
                                           IGF.IGM.getPointerAlignment());
      auto loadedBB = IGF.Builder.GetInsertBlock();
      IGF.Builder.CreateBr(doneBB);

      IGF.Builder.emitBlock(doneBB);
      auto phi = IGF.Builder.CreatePHI(IGF.IGM.PtrTy, 2);
      phi->addIncoming(borrow.getAddress(), valueBB);
      phi->addIncoming(loaded, loadedBB);
      return Address(phi, referentTL.getStorageType(),
                     referentTL.getBestKnownAlignment());
    }

    // Otherwise, ask the runtime 'swift_dereferenceBorrow' to handle it.
    auto metadata = IGF.emitTypeMetadataRef(astBorrowTy->getReferentType());
    auto dereferenceBorrow = IGF.IGM.getDereferenceBorrowFunctionPointer();

    auto addr = IGF.Builder.CreateCall(dereferenceBorrow,
                                       {metadata, borrow.getAddress()});

    return Address(addr, referentTL.getStorageType(),
                   referentTL.getBestKnownAlignment());
  }
  }
  llvm_unreachable("invalid borrow representation!");
}

bool irgen::shouldComputeBorrowLayoutInline(IRGenFunction &IGF, SILType T) {
  if (!T.is<BuiltinBorrowType>() || IGF.IGM.isBorrowRuntimeAvailable()) {
    return false;
  }
  auto &ti = IGF.IGM.getTypeInfo(T);
  return ti.getSubclassKind()
    == (unsigned)BorrowTypeInfoSubclassKind::BorrowNonFixed;
}

namespace {
/// The layout properties of a borrow, as computed from its referent's.
struct InlineBorrowLayout {
  llvm::Value *size, *stride, *flags, *extraInhabitantCount;
};
} // end anonymous namespace

/// Compute the layout of a borrow from the layout of its referent, following
/// swift_getBorrowRepresentation and the BorrowCacheEntry initialization in
/// the runtime: a borrow of a small, bitwise-borrowable value that isn't
/// addressable for dependencies has the layout of the value; any other borrow
/// has the layout of a pointer. Old runtimes never set the flags that
/// describe addressability or non-borrowability, so this agrees with newer
/// runtimes for any referent that an older runtime could have produced.
static InlineBorrowLayout
computeInlineBorrowLayout(IRGenFunction &IGF, llvm::Value *size,
                          llvm::Value *stride, llvm::Value *flags,
                          llvm::Value *extraInhabitantCount) {
  auto &IGM = IGF.IGM;
  auto &B = IGF.Builder;
  auto ptrSize = IGM.getPointerSize().getValue();

  // Same logic as emitLoadOfIsBitwiseBorrowable and
  // emitLoadOfIsAddressableForDependencies.
  auto nonBorrowableMask = IGM.getInt32(ValueWitnessFlags::IsNonBitwiseBorrowable
                                        | ValueWitnessFlags::IsNonBitwiseTakable);
  auto isBitwiseBorrowable = B.CreateICmpEQ(
      B.CreateAnd(flags, nonBorrowableMask), IGM.getInt32(0));
  auto isAddressable = B.CreateICmpNE(
      B.CreateAnd(flags,
                  IGM.getInt32(ValueWitnessFlags::IsAddressableForDependencies)),
      IGM.getInt32(0));
  auto isValueRep = emitIsBorrowValueRepresentation(
      IGF, size, isBitwiseBorrowable, isAddressable);

  auto &rawPointerTI = IGM.getRawPointerTypeInfo();
  auto ptrSizeValue = llvm::ConstantInt::get(IGM.SizeTy, ptrSize);
  auto ptrXI = IGM.getInt32(rawPointerTI.getFixedExtraInhabitantCount(IGM));

  InlineBorrowLayout result;
  result.size = B.CreateSelect(isValueRep, size, ptrSizeValue);
  result.stride = B.CreateSelect(isValueRep, stride, ptrSizeValue);
  result.extraInhabitantCount =
      B.CreateSelect(isValueRep, extraInhabitantCount, ptrXI);

  auto alignMask = B.CreateSelect(
      isValueRep,
      B.CreateAnd(flags, IGM.getInt32(ValueWitnessFlags::AlignmentMask)),
      IGM.getInt32(ptrSize - 1));

  // Match ValueWitnessTable::isValueInline for a bitwise-takable type.
  auto fitsInBuffer = B.CreateICmpULE(
      result.size, llvm::ConstantInt::get(IGM.SizeTy, ptrSize * 3));
  auto alignFits = B.CreateICmpULE(alignMask, IGM.getInt32(ptrSize - 1));
  auto isInline = B.CreateAnd(fitsInBuffer, alignFits);
  result.flags = B.CreateOr(
      alignMask,
      B.CreateSelect(isInline, IGM.getInt32(0),
                     IGM.getInt32(ValueWitnessFlags::IsNonInline)));
  return result;
}

llvm::Value *irgen::emitInlineBorrowValueWitnessValue(IRGenFunction &IGF,
                                                      SILType T,
                                                      ValueWitness index) {
  auto astBorrowTy = T.castTo<BuiltinBorrowType>();
  auto referentTy =
      SILType::getPrimitiveObjectType(astBorrowTy->getReferentType());
  auto layout = computeInlineBorrowLayout(
      IGF, IGF.emitValueWitnessValue(referentTy, ValueWitness::Size),
      IGF.emitValueWitnessValue(referentTy, ValueWitness::Stride),
      IGF.emitValueWitnessValue(referentTy, ValueWitness::Flags),
      IGF.emitValueWitnessValue(referentTy, ValueWitness::ExtraInhabitantCount));

  switch (index) {
  case ValueWitness::Size:
    return layout.size;
  case ValueWitness::Stride:
    return layout.stride;
  case ValueWitness::Flags:
    return layout.flags;
  case ValueWitness::ExtraInhabitantCount:
    return layout.extraInhabitantCount;
  default:
    llvm_unreachable("not a value witness value");
  }
}

llvm::Value *irgen::emitInlineBorrowTypeLayout(IRGenFunction &IGF,
                                               llvm::Value *referentLayout) {
  auto &IGM = IGF.IGM;
  auto &B = IGF.Builder;

  // The layout of the referent is a pointer to a TypeLayout.
  auto layoutTy = IGM.TypeLayoutTy;
  auto loadField = [&](llvm::Value *layoutPtr, unsigned field,
                       llvm::Type *ty) -> llvm::Value * {
    auto gep = B.CreateStructGEP(layoutTy, layoutPtr, field);
    return B.CreateLoad(gep, ty, IGM.getPointerAlignment());
  };
  auto layout = computeInlineBorrowLayout(
      IGF, loadField(referentLayout, 0, IGM.SizeTy),
      loadField(referentLayout, 1, IGM.SizeTy),
      loadField(referentLayout, 2, IGM.Int32Ty),
      loadField(referentLayout, 3, IGM.Int32Ty));

  // Like the layouts of tuples, this has no reasonable scope, so we use an
  // alloca in the entry block.
  auto result = IGF.createAlloca(layoutTy, IGM.getPointerAlignment()).getAddress();
  auto storeField = [&](unsigned field, llvm::Value *value) {
    B.CreateStore(value, B.CreateStructGEP(layoutTy, result, field),
                  IGM.getPointerAlignment());
  };
  storeField(0, layout.size);
  storeField(1, layout.stride);
  storeField(2, layout.flags);
  storeField(3, layout.extraInhabitantCount);
  return result;
}
