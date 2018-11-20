//===--- GenType.cpp - Swift IR Generation For Types ----------------------===//
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
//  This file implements IR generation for types in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/ABI/MetadataValues.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/Types.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/SILModule.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"
#include "clang/CodeGen/SwiftCallingConv.h"

#include "EnumPayload.h"
#include "LegacyLayoutFormat.h"
#include "LoadableTypeInfo.h"
#include "GenMeta.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Address.h"
#include "Explosion.h"
#include "GenOpaque.h"
#include "HeapTypeInfo.h"
#include "IndirectTypeInfo.h"
#include "Outlining.h"
#include "ProtocolInfo.h"
#include "ReferenceTypeInfo.h"
#include "ScalarTypeInfo.h"
#include "NativeConventionSchema.h"
#include "IRGenMangler.h"
#include "NonFixedTypeInfo.h"

using namespace swift;
using namespace irgen;

Alignment IRGenModule::getCappedAlignment(Alignment align) {
  return std::min(align, Alignment(MaximumAlignment));
}

llvm::DenseMap<TypeBase *, const TypeInfo *> &
TypeConverter::Types_t::getCacheFor(bool isDependent, TypeConverter::Mode mode) {
  return (isDependent
          ? DependentCache[unsigned(mode)]
          : IndependentCache[unsigned(mode)]);
}

void TypeInfo::assign(IRGenFunction &IGF, Address dest, Address src,
                      IsTake_t isTake, SILType T, bool isOutlined) const {
  if (isTake) {
    assignWithTake(IGF, dest, src, T, isOutlined);
  } else {
    assignWithCopy(IGF, dest, src, T, isOutlined);
  }
}

void TypeInfo::initialize(IRGenFunction &IGF, Address dest, Address src,
                          IsTake_t isTake, SILType T, bool isOutlined) const {
  if (isTake) {
    initializeWithTake(IGF, dest, src, T, isOutlined);
  } else {
    initializeWithCopy(IGF, dest, src, T, isOutlined);
  }
}

bool TypeInfo::isSingleRetainablePointer(ResilienceExpansion expansion,
                                         ReferenceCounting *refcounting) const {
  return false;
}

ExplosionSchema TypeInfo::getSchema() const {
  ExplosionSchema schema;
  getSchema(schema);
  return schema;
}

TypeInfo::~TypeInfo() {
  if (nativeReturnSchema)
    delete nativeReturnSchema;
  if (nativeParameterSchema)
    delete nativeParameterSchema;
}

Address TypeInfo::getAddressForPointer(llvm::Value *ptr) const {
  assert(ptr->getType()->getPointerElementType() == StorageType);
  return Address(ptr, getBestKnownAlignment());
}

Address TypeInfo::getUndefAddress() const {
  return Address(llvm::UndefValue::get(getStorageType()->getPointerTo(0)),
                 getBestKnownAlignment());
}

/// Whether this type is known to be empty.
bool TypeInfo::isKnownEmpty(ResilienceExpansion expansion) const {
  if (auto fixed = dyn_cast<FixedTypeInfo>(this))
    return fixed->isKnownEmpty(expansion);
  return false;
}

const NativeConventionSchema &
TypeInfo::nativeReturnValueSchema(IRGenModule &IGM) const {
  if (nativeReturnSchema == nullptr)
    nativeReturnSchema = new NativeConventionSchema(IGM, this, true);
  return *nativeReturnSchema;
}

const NativeConventionSchema &
TypeInfo::nativeParameterValueSchema(IRGenModule &IGM) const {
  if (nativeParameterSchema == nullptr)
    nativeParameterSchema = new NativeConventionSchema(IGM, this, false);
  return *nativeParameterSchema;
}

/// Copy a value from one object to a new object, directly taking
/// responsibility for anything it might have.  This is like C++
/// move-initialization, except the old object will not be destroyed.
void FixedTypeInfo::initializeWithTake(IRGenFunction &IGF, Address destAddr,
                                       Address srcAddr, SILType T,
                                       bool isOutlined) const {
  assert(isBitwiseTakable(ResilienceExpansion::Maximal)
        && "non-bitwise-takable type must override default initializeWithTake");
  
  // Prefer loads and stores if we won't make a million of them.
  // Maybe this should also require the scalars to have a fixed offset.
  ExplosionSchema schema = getSchema();
  if (!schema.containsAggregate() && schema.size() <= 2) {
    auto &loadableTI = cast<LoadableTypeInfo>(*this);
    Explosion copy;
    loadableTI.loadAsTake(IGF, srcAddr, copy);
    loadableTI.initialize(IGF, copy, destAddr, isOutlined);
    return;
  }

  // Otherwise, use a memcpy.
  IGF.emitMemCpy(destAddr, srcAddr, getFixedSize());
}

/// Copy a value from one object to a new object.  This is just the
/// default implementation.
void LoadableTypeInfo::initializeWithCopy(IRGenFunction &IGF, Address destAddr,
                                          Address srcAddr, SILType T,
                                          bool isOutlined) const {
  // Use memcpy if that's legal.
  if (isPOD(ResilienceExpansion::Maximal)) {
    return initializeWithTake(IGF, destAddr, srcAddr, T, isOutlined);
  }

  // Otherwise explode and re-implode.
  if (isOutlined) {
    Explosion copy;
    loadAsCopy(IGF, srcAddr, copy);
    initialize(IGF, copy, destAddr, true);
  } else {
    OutliningMetadataCollector collector(IGF);
    // No need to collect anything because we assume loadable types can be
    // loaded without enums.
    collector.emitCallToOutlinedCopy(
        destAddr, srcAddr, T, *this, IsInitialization, IsNotTake);
  }
}

LoadedRef LoadableTypeInfo::loadRefcountedPtr(IRGenFunction &IGF,
                                              SourceLoc loc,
                                              Address addr) const {
  IGF.IGM.error(loc, "Can only load from an address that holds a reference to "
                "a refcounted type or an address of an optional reference.");
  llvm::report_fatal_error("loadRefcountedPtr: Invalid SIL in IRGen");
}

void LoadableTypeInfo::addScalarToAggLowering(IRGenModule &IGM,
                                              SwiftAggLowering &lowering,
                                              llvm::Type *type, Size offset,
                                              Size storageSize) {
  lowering.addTypedData(type, offset.asCharUnits(),
                        offset.asCharUnits() + storageSize.asCharUnits());
}

static llvm::Constant *asSizeConstant(IRGenModule &IGM, Size size) {
  return llvm::ConstantInt::get(IGM.SizeTy, size.getValue());
}

llvm::Value *FixedTypeInfo::getSize(IRGenFunction &IGF, SILType T) const {
  return FixedTypeInfo::getStaticSize(IGF.IGM);
}
llvm::Constant *FixedTypeInfo::getStaticSize(IRGenModule &IGM) const {
  return asSizeConstant(IGM, getFixedSize());
}

llvm::Value *FixedTypeInfo::getAlignmentMask(IRGenFunction &IGF,
                                             SILType T) const {
  return FixedTypeInfo::getStaticAlignmentMask(IGF.IGM);
}
llvm::Constant *FixedTypeInfo::getStaticAlignmentMask(IRGenModule &IGM) const {
  return asSizeConstant(IGM, Size(getFixedAlignment().getValue() - 1));
}

llvm::Value *FixedTypeInfo::getStride(IRGenFunction &IGF, SILType T) const {
  return FixedTypeInfo::getStaticStride(IGF.IGM);
}
llvm::Value *FixedTypeInfo::getIsPOD(IRGenFunction &IGF, SILType T) const {
  return llvm::ConstantInt::get(IGF.IGM.Int1Ty,
                                isPOD(ResilienceExpansion::Maximal) == IsPOD);
}
llvm::Value *FixedTypeInfo::getIsBitwiseTakable(IRGenFunction &IGF, SILType T) const {
  return llvm::ConstantInt::get(IGF.IGM.Int1Ty,
                                isBitwiseTakable(ResilienceExpansion::Maximal) == IsBitwiseTakable);
}
llvm::Constant *FixedTypeInfo::getStaticStride(IRGenModule &IGM) const {
  return asSizeConstant(IGM, getFixedStride());
}

llvm::Value *FixedTypeInfo::isDynamicallyPackedInline(IRGenFunction &IGF,
                                                      SILType T) const {
  auto packing = getFixedPacking(IGF.IGM);
  assert(packing == FixedPacking::Allocate ||
         packing == FixedPacking::OffsetZero);
  return llvm::ConstantInt::get(IGF.IGM.Int1Ty,
                                packing == FixedPacking::OffsetZero);
}

unsigned FixedTypeInfo::getSpareBitExtraInhabitantCount() const {
  if (SpareBits.none())
    return 0;
  // The runtime supports a max of 0x7FFFFFFF extra inhabitants, which ought
  // to be enough for anybody.
  if (getFixedSize().getValue() >= 4)
    return 0x7FFFFFFF;
  unsigned spareBitCount = SpareBits.count();
  assert(spareBitCount <= getFixedSize().getValueInBits()
         && "more spare bits than storage bits?!");
  unsigned inhabitedBitCount = getFixedSize().getValueInBits() - spareBitCount;
  return ((1U << spareBitCount) - 1U) << inhabitedBitCount;
}

void FixedTypeInfo::applyFixedSpareBitsMask(SpareBitVector &mask,
                                            const SpareBitVector &spareBits) {
  // If the mask is no longer than the stored spare bits, we can just
  // apply the stored spare bits.
  if (mask.size() <= spareBits.size()) {
    // Grow the mask out if necessary; the tail padding is all spare bits.
    mask.extendWithSetBits(spareBits.size());
    mask &= spareBits;

  // Otherwise, we have to grow out the stored spare bits before we
  // can intersect.
  } else {
    auto paddedSpareBits = spareBits;
    paddedSpareBits.extendWithSetBits(mask.size());
    mask &= paddedSpareBits;
  }
}

void FixedTypeInfo::applyFixedSpareBitsMask(SpareBitVector &mask) const {
  return applyFixedSpareBitsMask(mask, SpareBits);
}

APInt
FixedTypeInfo::getSpareBitFixedExtraInhabitantValue(IRGenModule &IGM,
                                                    unsigned bits,
                                                    unsigned index) const {
  // Factor the index into the part that goes in the occupied bits and the
  // part that goes in the spare bits.
  unsigned occupiedIndex, spareIndex = 0;
  
  unsigned spareBitCount = SpareBits.count();
  unsigned occupiedBitCount = SpareBits.size() - spareBitCount;
  
  if (occupiedBitCount >= 31) {
    occupiedIndex = index;
    // The spare bit value is biased by one because all zero spare bits
    // represents a valid value of the type.
    spareIndex = 1;
  } else {
    occupiedIndex = index & ((1 << occupiedBitCount) - 1);
    // The spare bit value is biased by one because all zero spare bits
    // represents a valid value of the type.
    spareIndex = (index >> occupiedBitCount) + 1;
  }

  return interleaveSpareBits(IGM, SpareBits, bits, spareIndex, occupiedIndex);
}

llvm::Value *
FixedTypeInfo::getSpareBitExtraInhabitantIndex(IRGenFunction &IGF,
                                               Address src) const {
  assert(!SpareBits.none() && "no spare bits");
  
  auto &C = IGF.IGM.getLLVMContext();
  
  // Load the value.
  auto payloadTy = llvm::IntegerType::get(C, getFixedSize().getValueInBits());
  src = IGF.Builder.CreateBitCast(src, payloadTy->getPointerTo());
  auto val = IGF.Builder.CreateLoad(src);
  
  // If the spare bits are all zero, then we have a valid value and not an
  // extra inhabitant.
  auto spareBitsMask
    = llvm::ConstantInt::get(C, SpareBits.asAPInt());
  auto valSpareBits = IGF.Builder.CreateAnd(val, spareBitsMask);
  auto isValid = IGF.Builder.CreateICmpEQ(valSpareBits,
                                          llvm::ConstantInt::get(payloadTy, 0));
  
  auto *origBB = IGF.Builder.GetInsertBlock();
  auto *endBB = llvm::BasicBlock::Create(C);
  auto *spareBB = llvm::BasicBlock::Create(C);
  IGF.Builder.CreateCondBr(isValid, endBB, spareBB);

  IGF.Builder.emitBlock(spareBB);
  ConditionalDominanceScope condition(IGF);
  
  // Gather the occupied bits.
  auto OccupiedBits = SpareBits;
  OccupiedBits.flipAll();
  llvm::Value *idx = emitGatherSpareBits(IGF, OccupiedBits, val, 0, 31);
  
  // See if spare bits fit into the 31 bits of the index.
  unsigned numSpareBits = SpareBits.count();
  unsigned numOccupiedBits = getFixedSize().getValueInBits() - numSpareBits;
  if (numOccupiedBits < 31) {
    // Gather the spare bits.
    llvm::Value *spareIdx
      = emitGatherSpareBits(IGF, SpareBits, val, numOccupiedBits, 31);
    // Unbias by subtracting one.

    uint64_t shifted = static_cast<uint64_t>(1) << numOccupiedBits;
    spareIdx = IGF.Builder.CreateSub(spareIdx,
            llvm::ConstantInt::get(spareIdx->getType(), shifted));
    idx = IGF.Builder.CreateOr(idx, spareIdx);
  }
  idx = IGF.Builder.CreateZExt(idx, IGF.IGM.Int32Ty);
  
  IGF.Builder.CreateBr(endBB);
  IGF.Builder.emitBlock(endBB);
  
  // If we had a valid value, return -1. Otherwise, return the index.
  auto phi = IGF.Builder.CreatePHI(IGF.IGM.Int32Ty, 2);
  phi->addIncoming(llvm::ConstantInt::get(IGF.IGM.Int32Ty, -1), origBB);
  phi->addIncoming(idx, spareBB);
  
  return phi;
}

static llvm::Value *computeExtraTagBytes(IRGenFunction &IGF, IRBuilder &Builder,
                                         Size fixedSize,
                                         llvm::Value *numEmptyCases) {
  // We can use the payload area with a tag bit set somewhere outside of the
  // payload area to represent cases. See how many bytes we need to cover
  // all the empty cases.

  // Algorithm:
  // unsigned numTags = 1;
  // if (size >= 4)
  //   // Assume that one tag bit is enough if the precise calculation overflows
  //   // an int32.
  //   numTags += 1;
  // else {
  //   unsigned bits = size * 8U;
  //   unsigned casesPerTagBitValue = 1U << bits;
  //   numTags += ((emptyCases + (casesPerTagBitValue - 1U)) >> bits);
  // }
  // return (numTags < 256 ? 1 :
  // 				 numTags < 65536 ? 2 : 4);

  auto &IGM = IGF.IGM;
  auto &Ctx = Builder.getContext();
  auto *int32Ty = IGM.Int32Ty;

  auto *one = llvm::ConstantInt::get(int32Ty, 1U);
  if (fixedSize >= Size(4)) {
    return one;
  }

  auto *entryBB = Builder.GetInsertBlock();
  llvm::Value *size = asSizeConstant(IGM, fixedSize);
  auto *returnBB = llvm::BasicBlock::Create(Ctx);
  size = Builder.CreateTrunc(size, int32Ty); // We know size < 4.

  auto *two = llvm::ConstantInt::get(int32Ty, 2U);
  auto *four = llvm::ConstantInt::get(int32Ty, 4U);

  auto *bits = Builder.CreateMul(size, llvm::ConstantInt::get(int32Ty, 8U));
  auto *casesPerTagBitValue = Builder.CreateShl(one, bits);

  auto *numTags = Builder.CreateSub(casesPerTagBitValue, one);
  numTags = Builder.CreateAdd(numTags, numEmptyCases);
  numTags = Builder.CreateLShr(numTags, bits);
  numTags = Builder.CreateAdd(numTags, one);

  auto *notLT256BB = llvm::BasicBlock::Create(Ctx);
  auto *isLT256 =
      Builder.CreateICmpULT(numTags, llvm::ConstantInt::get(int32Ty, 256U));
  Builder.CreateCondBr(isLT256, returnBB, notLT256BB);

  Builder.emitBlock(notLT256BB);
  auto *isLT65536 =
      Builder.CreateICmpULT(numTags, llvm::ConstantInt::get(int32Ty, 65536U));
  numTags = Builder.CreateSelect(isLT65536, two, four);
  Builder.CreateBr(returnBB);

  Builder.emitBlock(returnBB);
  auto *phi = Builder.CreatePHI(int32Ty, 3);
  phi->addIncoming(one, entryBB);
  phi->addIncoming(numTags, notLT256BB);
  return phi;
}

static Address alignAddress(IRGenFunction &IGF, Address originAddress,
			    llvm::Value *shift) {
  auto &Builder = IGF.Builder;
  auto &IGM = IGF.IGM;
  auto *alignedAddressValue =
      Builder.CreateBitCast(originAddress, IGM.Int8PtrTy).getAddress();
  Address alignedAddress = Builder.CreateBitCast(originAddress, IGM.Int8PtrTy);
  if (!IGM.Triple.isLittleEndian()) {
    alignedAddress = Builder.CreateConstByteArrayGEP(alignedAddress, Size(4));
    alignedAddressValue = alignedAddress.getAddress();
    alignedAddressValue =
	Builder.CreateGEP(alignedAddressValue, Builder.CreateNeg(shift));
    alignedAddress =
	Address(alignedAddressValue, alignedAddress.getAlignment()); 
  }
  return alignedAddress;
}

llvm::Value *FixedTypeInfo::getEnumTagSinglePayload(IRGenFunction &IGF,
                                                    llvm::Value *numEmptyCases,
                                                    Address enumAddr,
                                                    SILType T) const {
  auto &IGM = IGF.IGM;
  auto &Ctx = IGF.IGM.getLLVMContext();
  auto &Builder = IGF.Builder;

  auto *size = getSize(IGF, T);
  Size fixedSize = getFixedSize();
  auto *numExtraInhabitants =
      llvm::ConstantInt::get(IGM.Int32Ty, getFixedExtraInhabitantCount(IGM));

  auto *zero = llvm::ConstantInt::get(IGM.Int32Ty, 0U);
  auto *one = llvm::ConstantInt::get(IGM.Int32Ty, 1U);
  auto *four = llvm::ConstantInt::get(IGM.Int32Ty, 4U);
  auto *eight = llvm::ConstantInt::get(IGM.Int32Ty, 8U);

  auto *extraTagBitsBB = llvm::BasicBlock::Create(Ctx);
  auto *noExtraTagBitsBB = llvm::BasicBlock::Create(Ctx);
  auto *hasEmptyCasesBB = llvm::BasicBlock::Create(Ctx);
  auto *singleCaseEnumBB = llvm::BasicBlock::Create(Ctx);

  // No empty cases so we must be the payload.
  auto *hasNoEmptyCases = Builder.CreateICmpEQ(zero, numEmptyCases);
  Builder.CreateCondBr(hasNoEmptyCases, singleCaseEnumBB, hasEmptyCasesBB);

  // Otherwise, check whether we need extra tag bits.
  Builder.emitBlock(hasEmptyCasesBB);
  auto *hasExtraTagBits =
      Builder.CreateICmpUGT(numEmptyCases, numExtraInhabitants);
  Builder.CreateCondBr(hasExtraTagBits, extraTagBitsBB, noExtraTagBitsBB);

  // There are extra tag bits to check.
  Builder.emitBlock(extraTagBitsBB);
  Address extraTagBitsSlot = IGF.createAlloca(IGM.Int32Ty, Alignment(4));
  Builder.CreateStore(zero, extraTagBitsSlot);

  // Compute the number of extra tag bytes.
  auto *emptyCases = Builder.CreateSub(numEmptyCases, numExtraInhabitants);
  auto *numExtraTagBytes =
      computeExtraTagBytes(IGF, Builder, fixedSize, emptyCases);

  // Read the extra tag bytes.
  auto *valueAddr =
      Builder.CreateBitOrPointerCast(enumAddr.getAddress(), IGM.Int8PtrTy);
  auto *extraTagBitsAddr =
      Builder.CreateConstInBoundsGEP1_32(IGM.Int8Ty, valueAddr,
                                         fixedSize.getValue());

  Address alignedExtraTagBitsSlot =
      alignAddress(IGF, extraTagBitsSlot, numExtraTagBytes);
  numExtraTagBytes = IGF.Builder.CreateZExt(numExtraTagBytes, IGM.SizeTy);
  Builder.CreateMemCpy(
      Builder.CreateBitCast(alignedExtraTagBitsSlot, IGM.Int8PtrTy)
          .getAddress(),
      1, extraTagBitsAddr, 1, numExtraTagBytes);
  auto extraTagBits = Builder.CreateLoad(extraTagBitsSlot);

  extraTagBitsBB = llvm::BasicBlock::Create(Ctx);
  Builder.CreateCondBr(Builder.CreateICmpEQ(extraTagBits, zero),
                       noExtraTagBitsBB, extraTagBitsBB);

  auto *resultBB = llvm::BasicBlock::Create(Ctx);

  Builder.emitBlock(extraTagBitsBB);

  auto *truncSize = Builder.CreateTrunc(size, IGM.Int32Ty);
  Address caseIndexFromValueSlot = IGF.createAlloca(IGM.Int32Ty, Alignment(4));
  Builder.CreateStore(zero, caseIndexFromValueSlot);

  auto *caseIndexFromExtraTagBits = Builder.CreateSelect(
      Builder.CreateICmpUGE(truncSize, four), zero,
      Builder.CreateShl(Builder.CreateSub(extraTagBits, one),
                        Builder.CreateMul(eight, truncSize)));

  Address alignedCaseIndex = alignAddress(
      IGF, caseIndexFromValueSlot,
      llvm::ConstantInt::getSigned(
	  IGM.Int32Ty, std::min(Size(4U).getValue(), fixedSize.getValue())));
  Builder.CreateMemCpy(alignedCaseIndex, Address(valueAddr, Alignment(1)),
                       std::min(Size(4U), fixedSize));
  auto caseIndexFromValue = Builder.CreateLoad(caseIndexFromValueSlot);

  auto *result1 = Builder.CreateAdd(
      numExtraInhabitants,
      Builder.CreateOr(caseIndexFromValue, caseIndexFromExtraTagBits));
  Builder.CreateBr(resultBB);

  // Extra tag bits were considered and zero or there are not extra tag
  // bits.
  Builder.emitBlock(noExtraTagBitsBB);
  // If there are extra inhabitants, see whether the payload is valid.
  llvm::Value *result0;
  if (mayHaveExtraInhabitants(IGM)) {
    result0 = getExtraInhabitantIndex(IGF, enumAddr, T, false);
    noExtraTagBitsBB = Builder.GetInsertBlock();
  } else {
    result0 = llvm::ConstantInt::getSigned(IGM.Int32Ty, -1);
  }
  Builder.CreateBr(resultBB);

  Builder.emitBlock(singleCaseEnumBB);
  // Otherwise, we have a valid payload.
  auto *result2 = llvm::ConstantInt::getSigned(IGM.Int32Ty, -1);
  Builder.CreateBr(resultBB);

  Builder.emitBlock(resultBB);
  auto *result = Builder.CreatePHI(IGM.Int32Ty, 3);
  result->addIncoming(result0, noExtraTagBitsBB);
  result->addIncoming(result1, extraTagBitsBB);
  result->addIncoming(result2, singleCaseEnumBB);

  return Builder.CreateAdd(result, llvm::ConstantInt::get(IGM.Int32Ty, 1));
}

/// Emit a speciaize memory operation for a \p size of 0 to 4 bytes.
static void emitSpecializedMemOperation(
    IRGenFunction &IGF,
    llvm::function_ref<void(IRBuilder &, Size)> emitMemOpFn,
    llvm::Value *size) {
  auto &IGM = IGF.IGM;
  auto &Ctx = IGF.IGM.getLLVMContext();
  auto &Builder = IGF.Builder;
  auto *returnBB = llvm::BasicBlock::Create(Ctx);
  auto *oneBB = llvm::BasicBlock::Create(Ctx);
  auto *twoBB = llvm::BasicBlock::Create(Ctx);
  auto *fourBB = llvm::BasicBlock::Create(Ctx);
  auto *int32Ty = IGM.Int32Ty;
  auto *zero = llvm::ConstantInt::get(int32Ty, 0U);
  auto *one = llvm::ConstantInt::get(int32Ty, 1U);
  auto *two = llvm::ConstantInt::get(int32Ty, 2U);

  auto *continueBB = llvm::BasicBlock::Create(Ctx);
  auto *isZero = Builder.CreateICmpEQ(size, zero);
  Builder.CreateCondBr(isZero, returnBB, continueBB);

  Builder.emitBlock(continueBB);
  continueBB = llvm::BasicBlock::Create(Ctx);
  auto *isOne = Builder.CreateICmpEQ(size, one);
  Builder.CreateCondBr(isOne, oneBB, continueBB);

  Builder.emitBlock(continueBB);
  auto *isTwo = Builder.CreateICmpEQ(size, two);
  Builder.CreateCondBr(isTwo, twoBB, fourBB);

  Builder.emitBlock(oneBB);
  emitMemOpFn(Builder, Size(1));
  Builder.CreateBr(returnBB);

  Builder.emitBlock(twoBB);
  emitMemOpFn(Builder, Size(2));
  Builder.CreateBr(returnBB);

  Builder.emitBlock(fourBB);
  emitMemOpFn(Builder, Size(4));
  Builder.CreateBr(returnBB);

  Builder.emitBlock(returnBB);
}

/// Emit a memset of zero operation for a \p size of 0 to 4 bytes.
static void emitMemZero(IRGenFunction &IGF, Address addr,
                        llvm::Value *size) {
  auto *zeroByte = llvm::ConstantInt::get(IGF.IGM.Int8Ty, 0U);
  emitSpecializedMemOperation(IGF,
                              [=](IRBuilder &B, Size numBytes) {
                                B.CreateMemSet(addr, zeroByte, numBytes);
                              },
                              size);
}

/// Emit a memcpy operation for a \p size of 0 to 4 bytes.
static void emitMemCpy(IRGenFunction &IGF, Address to, Address from,
                       llvm::Value *size) {
  emitSpecializedMemOperation(IGF,
                              [=](IRBuilder &B, Size numBytes) {
                                B.CreateMemCpy(to, from, numBytes);
                              },
                              size);
}

void FixedTypeInfo::storeEnumTagSinglePayload(IRGenFunction &IGF,
                                              llvm::Value *whichCase,
                                              llvm::Value *numEmptyCases,
                                              Address enumAddr,
                                              SILType T) const {
  auto &IGM = IGF.IGM;
  auto &Ctx = IGF.IGM.getLLVMContext();
  auto &Builder = IGF.Builder;
  auto &int32Ty = IGM.Int32Ty;

  auto *zero = llvm::ConstantInt::get(int32Ty, 0U);
  auto *one = llvm::ConstantInt::get(int32Ty, 1U);
  auto *four = llvm::ConstantInt::get(int32Ty, 4U);
  auto *eight = llvm::ConstantInt::get(int32Ty, 8U);

  auto fixedSize = getFixedSize();

  Address valueAddr = Builder.CreateElementBitCast(enumAddr, IGM.Int8Ty);
  Address extraTagBitsAddr =
    Builder.CreateConstByteArrayGEP(valueAddr, fixedSize);

  auto *numExtraInhabitants =
      llvm::ConstantInt::get(IGM.Int32Ty, getFixedExtraInhabitantCount(IGM));
  auto *size = getSize(IGF, T);

  // Do we need extra tag bytes.
  auto *entryBB = Builder.GetInsertBlock();
  auto *continueBB = llvm::BasicBlock::Create(Ctx);
  auto *computeExtraTagBytesBB = llvm::BasicBlock::Create(Ctx);
  auto *hasExtraTagBits =
      Builder.CreateICmpUGT(numEmptyCases, numExtraInhabitants);
  Builder.CreateCondBr(hasExtraTagBits, computeExtraTagBytesBB, continueBB);

  Builder.emitBlock(computeExtraTagBytesBB);
  // Compute the number of extra tag bytes.
  auto *emptyCases = Builder.CreateSub(numEmptyCases, numExtraInhabitants);
  auto *numExtraTagBytes0 =
      computeExtraTagBytes(IGF, Builder, fixedSize, emptyCases);
  computeExtraTagBytesBB = Builder.GetInsertBlock();
  Builder.CreateBr(continueBB);

  Builder.emitBlock(continueBB);
  auto *numExtraTagBytes = Builder.CreatePHI(int32Ty, 2);
  numExtraTagBytes->addIncoming(zero, entryBB);
  numExtraTagBytes->addIncoming(numExtraTagBytes0, computeExtraTagBytesBB);

  // Check whether we need to set the extra tag bits to non zero.
  auto *isExtraTagBitsCaseBB = llvm::BasicBlock::Create(Ctx);
  auto *isPayloadOrInhabitantCaseBB = llvm::BasicBlock::Create(Ctx);
  auto *isPayloadOrExtraInhabitant =
      Builder.CreateICmpULE(whichCase, numExtraInhabitants);
  Builder.CreateCondBr(isPayloadOrExtraInhabitant, isPayloadOrInhabitantCaseBB,
                       isExtraTagBitsCaseBB);

  // We are the payload or fit within the extra inhabitants.
  Builder.emitBlock(isPayloadOrInhabitantCaseBB);
  // Zero the tag bits.
  emitMemZero(IGF, extraTagBitsAddr, numExtraTagBytes);
  isPayloadOrInhabitantCaseBB = Builder.GetInsertBlock();
  auto *storeInhabitantBB = llvm::BasicBlock::Create(Ctx);
  auto *returnBB = llvm::BasicBlock::Create(Ctx);
  auto *isPayload = Builder.CreateICmpEQ(whichCase, zero);
  Builder.CreateCondBr(isPayload, returnBB, storeInhabitantBB);

  Builder.emitBlock(storeInhabitantBB);
  if (mayHaveExtraInhabitants(IGM)) {
    // Store an index in the range [0..ElementsWithNoPayload-1].
    auto *nonPayloadElementIndex = Builder.CreateSub(whichCase, one);
    storeExtraInhabitant(IGF, nonPayloadElementIndex, enumAddr, T,
                         /*outlined*/ false);
  }
  Builder.CreateBr(returnBB);

  // There are extra tag bits to consider.
  Builder.emitBlock(isExtraTagBitsCaseBB);

  // Write the extra tag bytes. At this point we know we have an no payload case
  // and therefore the index we should store is in the range
  // [0..ElementsWithNoPayload-1].
  auto *nonPayloadElementIndex = Builder.CreateSub(whichCase, one);
  auto *caseIndex =
      Builder.CreateSub(nonPayloadElementIndex, numExtraInhabitants);
  auto *truncSize = Builder.CreateTrunc(size, IGM.Int32Ty);
  auto *isFourBytesPayload = Builder.CreateICmpUGE(truncSize, four);
  auto *payloadGE4BB = Builder.GetInsertBlock();
  auto *payloadLT4BB = llvm::BasicBlock::Create(Ctx);
  continueBB = llvm::BasicBlock::Create(Ctx);
  Builder.CreateCondBr(isFourBytesPayload, continueBB, payloadLT4BB);

  Builder.emitBlock(payloadLT4BB);
  auto *payloadBits = Builder.CreateMul(truncSize, eight);
  auto *extraTagIndex0 = Builder.CreateLShr(caseIndex, payloadBits);
  extraTagIndex0 = Builder.CreateAdd(one, extraTagIndex0);
  auto *payloadIndex0 = Builder.CreateShl(one, payloadBits);
  payloadIndex0 = Builder.CreateSub(payloadIndex0, one);
  payloadIndex0 = Builder.CreateAnd(payloadIndex0, caseIndex);
  Builder.CreateBr(continueBB);

  Builder.emitBlock(continueBB);
  auto *extraTagIndex = Builder.CreatePHI(int32Ty, 2);
  extraTagIndex->addIncoming(llvm::ConstantInt::get(int32Ty, 1), payloadGE4BB);
  extraTagIndex->addIncoming(extraTagIndex0, payloadLT4BB);

  auto *payloadIndex = Builder.CreatePHI(int32Ty, 2);
  payloadIndex->addIncoming(caseIndex, payloadGE4BB);
  payloadIndex->addIncoming(payloadIndex0, payloadLT4BB);

  Address payloadIndexAddr = IGF.createAlloca(int32Ty, Alignment(4));
  Builder.CreateStore(payloadIndex, payloadIndexAddr);
  Address extraTagIndexAddr = IGF.createAlloca(int32Ty, Alignment(4));
  Builder.CreateStore(extraTagIndex, extraTagIndexAddr);

  Address alignedPayloadIndexAddr = alignAddress(
      IGF, payloadIndexAddr,
      llvm::ConstantInt::getSigned(
	  IGM.Int32Ty, std::min(Size(4U).getValue(), fixedSize.getValue())));
  Builder.CreateMemCpy(valueAddr, alignedPayloadIndexAddr,
                       std::min(Size(4U), fixedSize));
  Address extraZeroAddr = Builder.CreateConstByteArrayGEP(valueAddr, Size(4));
  if (fixedSize > Size(4))
    Builder.CreateMemSet(
        extraZeroAddr, llvm::ConstantInt::get(IGM.Int8Ty, 0),
        Builder.CreateSub(size, llvm::ConstantInt::get(size->getType(), 4)));
  extraTagIndexAddr = alignAddress(IGF, extraTagIndexAddr, numExtraTagBytes);
  emitMemCpy(IGF, extraTagBitsAddr, extraTagIndexAddr, numExtraTagBytes);
  Builder.CreateBr(returnBB);

  Builder.emitBlock(returnBB);
}

llvm::Value *irgen::emitGetEnumTagSinglePayload(IRGenFunction &IGF,
                                                llvm::Value *numEmptyCases,
                                                Address enumAddr, SILType T) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  auto opaqueAddr =
      IGF.Builder.CreateBitCast(enumAddr.getAddress(), IGF.IGM.OpaquePtrTy);

  return IGF.Builder.CreateCall(IGF.IGM.getGetEnumCaseSinglePayloadFn(),
                                {opaqueAddr, metadata, numEmptyCases});
}

void irgen::emitStoreEnumTagSinglePayload(IRGenFunction &IGF,
                                          llvm::Value *whichCase,
                                          llvm::Value *numEmptyCases,
                                          Address enumAddr,
                                          SILType T) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  auto opaqueAddr =
      IGF.Builder.CreateBitCast(enumAddr.getAddress(), IGF.IGM.OpaquePtrTy);

  IGF.Builder.CreateCall(IGF.IGM.getStoreEnumTagSinglePayloadFn(),
                         {opaqueAddr, metadata, whichCase, numEmptyCases});
}

void
FixedTypeInfo::storeSpareBitExtraInhabitant(IRGenFunction &IGF,
                                            llvm::Value *index,
                                            Address dest) const {
  assert(!SpareBits.none() && "no spare bits");
  
  auto &C = IGF.IGM.getLLVMContext();

  auto payloadTy = llvm::IntegerType::get(C, getFixedSize().getValueInBits());

  unsigned spareBitCount = SpareBits.count();
  unsigned occupiedBitCount = SpareBits.size() - spareBitCount;
  llvm::Value *occupiedIndex;
  llvm::Value *spareIndex;
  
  // The spare bit value is biased by one because all zero spare bits
  // represents a valid value of the type.
  auto spareBitBias = llvm::ConstantInt::get(IGF.IGM.Int32Ty, 1U);
  
  // Factor the spare and occupied bit values from the index.
  if (occupiedBitCount >= 31) {
    occupiedIndex = index;
    spareIndex = spareBitBias;
  } else {
    auto occupiedBitMask = APInt::getAllOnesValue(occupiedBitCount);
    occupiedBitMask = occupiedBitMask.zext(32);
    auto occupiedBitMaskValue = llvm::ConstantInt::get(C, occupiedBitMask);
    occupiedIndex = IGF.Builder.CreateAnd(index, occupiedBitMaskValue);
    
    auto occupiedBitCountValue
      = llvm::ConstantInt::get(IGF.IGM.Int32Ty, occupiedBitCount);
    spareIndex = IGF.Builder.CreateLShr(index, occupiedBitCountValue);
    spareIndex = IGF.Builder.CreateAdd(spareIndex, spareBitBias);
  }
  
  // Scatter the occupied bits.
  auto OccupiedBits = SpareBits;
  OccupiedBits.flipAll();
  llvm::Value *occupied = emitScatterSpareBits(IGF, OccupiedBits,
                                               occupiedIndex, 0);
  
  // Scatter the spare bits.
  llvm::Value *spare = emitScatterSpareBits(IGF, SpareBits, spareIndex, 0);
  
  // Combine the values and store to the destination.
  llvm::Value *inhabitant = IGF.Builder.CreateOr(occupied, spare);
  
  dest = IGF.Builder.CreateBitCast(dest, payloadTy->getPointerTo());
  IGF.Builder.CreateStore(inhabitant, dest);
}

namespace {
  /// A TypeInfo implementation for empty types.
  struct EmptyTypeInfo : ScalarTypeInfo<EmptyTypeInfo, LoadableTypeInfo> {
    EmptyTypeInfo(llvm::Type *ty)
      : ScalarTypeInfo(ty, Size(0), SpareBitVector{}, Alignment(1), IsPOD,
                       IsFixedSize) {}
    unsigned getExplosionSize() const override { return 0; }
    void getSchema(ExplosionSchema &schema) const override {}
    void addToAggLowering(IRGenModule &IGM, SwiftAggLowering &lowering,
                          Size offset) const override {}
    void loadAsCopy(IRGenFunction &IGF, Address addr,
                    Explosion &e) const override {}
    void loadAsTake(IRGenFunction &IGF, Address addr,
                    Explosion &e) const override {}
    void assign(IRGenFunction &IGF, Explosion &e, Address addr,
                bool isOutlined) const override {}
    void initialize(IRGenFunction &IGF, Explosion &e, Address addr,
                    bool isOutlined) const override {}
    void copy(IRGenFunction &IGF, Explosion &src,
              Explosion &dest, Atomicity atomicity) const override {}
    void consume(IRGenFunction &IGF, Explosion &src,
                 Atomicity atomicity) const override {}
    void fixLifetime(IRGenFunction &IGF, Explosion &src) const override {}
    void destroy(IRGenFunction &IGF, Address addr, SILType T,
                 bool isOutlined) const override {}
    void packIntoEnumPayload(IRGenFunction &IGF, EnumPayload &payload,
                             Explosion &src, unsigned offset) const override {}
    void unpackFromEnumPayload(IRGenFunction &IGF,
                               const EnumPayload &payload,
                               Explosion &dest,
                               unsigned offset) const override {}
  };

  /// A TypeInfo implementation for types represented as a single
  /// scalar type.
  class PrimitiveTypeInfo final :
    public PODSingleScalarTypeInfo<PrimitiveTypeInfo, LoadableTypeInfo> {
  public:
    PrimitiveTypeInfo(llvm::Type *storage, Size size,
                      SpareBitVector &&spareBits,
                      Alignment align)
      : PODSingleScalarTypeInfo(storage, size, std::move(spareBits), align) {}
  };

  /// A TypeInfo implementation for bare non-null pointers (like `void *`).
  class RawPointerTypeInfo final :
    public PODSingleScalarTypeInfo<RawPointerTypeInfo, LoadableTypeInfo> {
  public:
    RawPointerTypeInfo(llvm::Type *storage, Size size, Alignment align)
      : PODSingleScalarTypeInfo(
          storage, size,
          SpareBitVector::getConstant(size.getValueInBits(), false),
          align) {}

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
      src = IGF.Builder.CreateBitCast(src, IGF.IGM.IntPtrTy->getPointerTo());
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
      dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.IntPtrTy->getPointerTo());
      IGF.Builder.CreateStore(llvm::ConstantInt::get(IGF.IGM.IntPtrTy, 0),dest);
    }
  };

  /// A TypeInfo implementation for opaque storage. Swift will preserve any
  /// data stored into this arbitrarily sized and aligned field, but doesn't
  /// know anything about the data.
  class OpaqueStorageTypeInfo final :
    public ScalarTypeInfo<OpaqueStorageTypeInfo, LoadableTypeInfo>
  {
    llvm::IntegerType *ScalarType;
  public:
    OpaqueStorageTypeInfo(llvm::ArrayType *storage,
                          llvm::IntegerType *scalarType,
                          Size size,
                          SpareBitVector &&spareBits,
                          Alignment align)
      : ScalarTypeInfo(storage, size, std::move(spareBits), align, IsPOD,
                       IsFixedSize),
        ScalarType(scalarType)
    {}
    
    llvm::ArrayType *getStorageType() const {
      return cast<llvm::ArrayType>(ScalarTypeInfo::getStorageType());
    }
    
    unsigned getExplosionSize() const override {
      return 1;
    }
    
    void loadAsCopy(IRGenFunction &IGF, Address addr,
                    Explosion &explosion) const override {
      loadAsTake(IGF, addr, explosion);
    }
    
    void loadAsTake(IRGenFunction &IGF, Address addr,
                    Explosion &explosion) const override {
      addr = IGF.Builder.CreateElementBitCast(addr, ScalarType);
      explosion.add(IGF.Builder.CreateLoad(addr));
    }

    void assign(IRGenFunction &IGF, Explosion &explosion, Address addr,
                bool isOutlined) const override {
      initialize(IGF, explosion, addr, isOutlined);
    }

    void initialize(IRGenFunction &IGF, Explosion &explosion, Address addr,
                    bool isOutlined) const override {
      addr = IGF.Builder.CreateElementBitCast(addr, ScalarType);
      IGF.Builder.CreateStore(explosion.claimNext(), addr);
    }
    
    void reexplode(IRGenFunction &IGF, Explosion &sourceExplosion,
                   Explosion &targetExplosion) const override {
      targetExplosion.add(sourceExplosion.claimNext());
    }
    
    void copy(IRGenFunction &IGF, Explosion &sourceExplosion,
              Explosion &targetExplosion, Atomicity atomicity) const override {
      reexplode(IGF, sourceExplosion, targetExplosion);
    }

    void consume(IRGenFunction &IGF, Explosion &explosion,
                 Atomicity atomicity) const override {
      explosion.claimNext();
    }
    
    void fixLifetime(IRGenFunction &IGF, Explosion &explosion) const override {
      explosion.claimNext();
    }

    void destroy(IRGenFunction &IGF, Address address, SILType T,
                 bool isOutlined) const override {
      /* nop */
    }
    
    void getSchema(ExplosionSchema &schema) const override {
      schema.add(ExplosionSchema::Element::forScalar(ScalarType));
    }

    void addToAggLowering(IRGenModule &IGM, SwiftAggLowering &lowering,
                          Size offset) const override {
      lowering.addOpaqueData(offset.asCharUnits(),
                             (offset + getFixedSize()).asCharUnits());
    }
    
    void packIntoEnumPayload(IRGenFunction &IGF,
                             EnumPayload &payload,
                             Explosion &source,
                             unsigned offset) const override {
      payload.insertValue(IGF, source.claimNext(), offset);
    }
    
    void unpackFromEnumPayload(IRGenFunction &IGF,
                               const EnumPayload &payload,
                               Explosion &target,
                               unsigned offset) const override {
      target.add(payload.extractValue(IGF, ScalarType, offset));
    }
  };

  /// A TypeInfo implementation for address-only types which can never
  /// be copied.
  class ImmovableTypeInfo :
    public IndirectTypeInfo<ImmovableTypeInfo, FixedTypeInfo> {
  public:
    ImmovableTypeInfo(llvm::Type *storage, Size size,
                      SpareBitVector &&spareBits,
                      Alignment align)
      : IndirectTypeInfo(storage, size, std::move(spareBits), align,
                         IsNotPOD, IsNotBitwiseTakable, IsFixedSize) {}

    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src,
                        SILType T, bool isOutlined) const override {
      llvm_unreachable("cannot opaquely manipulate immovable types!");
    }

    void assignWithTake(IRGenFunction &IGF, Address dest, Address src,
                        SILType T, bool isOutlined) const override {
      llvm_unreachable("cannot opaquely manipulate immovable types!");
    }

    void initializeWithTake(IRGenFunction &IGF, Address destAddr,
                            Address srcAddr, SILType T,
                            bool isOutlined) const override {
      llvm_unreachable("cannot opaquely manipulate immovable types!");
    }

    void initializeWithCopy(IRGenFunction &IGF, Address destAddr,
                            Address srcAddr, SILType T,
                            bool isOutlined) const override {
      llvm_unreachable("cannot opaquely manipulate immovable types!");
    }

    void destroy(IRGenFunction &IGF, Address address, SILType T,
                 bool isOutlined) const override {
      llvm_unreachable("cannot opaquely manipulate immovable types!");
    }
  };
} // end anonymous namespace

/// Constructs a type info which performs simple loads and stores of
/// the given IR type.
const LoadableTypeInfo *
TypeConverter::createPrimitive(llvm::Type *type, Size size, Alignment align) {
  return new PrimitiveTypeInfo(type, size, IGM.getSpareBitsForType(type, size),
                               align);
}

/// Constructs a type info which performs simple loads and stores of
/// the given IR type, given that it's a pointer to an aligned pointer
/// type.
const LoadableTypeInfo *
TypeConverter::createPrimitiveForAlignedPointer(llvm::PointerType *type,
                                                Size size,
                                                Alignment align,
                                                Alignment pointerAlignment) {
  SpareBitVector spareBits = IGM.TargetInfo.PointerSpareBits;
  for (unsigned bit = 0; Alignment(1 << bit) != pointerAlignment; ++bit) {
    spareBits.setBit(bit);
  }

  return new PrimitiveTypeInfo(type, size, std::move(spareBits), align);
}

/// Constructs a fixed-size type info which asserts if you try to copy
/// or destroy it.
const FixedTypeInfo *
TypeConverter::createImmovable(llvm::Type *type, Size size, Alignment align) {
  auto spareBits = SpareBitVector::getConstant(size.getValueInBits(), false);
  return new ImmovableTypeInfo(type, size, std::move(spareBits), align);
}

static TypeInfo *invalidTypeInfo() { return (TypeInfo*) 1; }

bool TypeConverter::readLegacyTypeInfo(StringRef path) {
  auto fileOrErr = llvm::MemoryBuffer::getFile(path);
  if (!fileOrErr)
    return true;

  auto file = std::move(fileOrErr.get());

  llvm::yaml::Input yin(file->getBuffer());

  // Read the document list.
  std::vector<YAMLModuleNode> modules;
  yin >> modules;

  if (yin.error())
    return true;

  for (auto &module : modules) {
    for (auto &decl : module.Decls) {
      auto result = LegacyTypeInfos.insert(std::make_pair(
                                             decl.Name,
                                             decl));
      assert(result.second);
      (void) result;
    }
  }

  return false;
}

static std::string mangleTypeAsContext(const NominalTypeDecl *decl) {
  Mangle::ASTMangler Mangler;
  return Mangler.mangleTypeAsContextUSR(decl);
}

Optional<YAMLTypeInfoNode>
TypeConverter::getLegacyTypeInfo(NominalTypeDecl *decl) const {
  auto &mangledName = const_cast<TypeConverter *>(this)->DeclMangledNames[decl];
  if (mangledName.empty())
    mangledName = mangleTypeAsContext(decl);
  assert(!mangledName.empty());

  auto found = LegacyTypeInfos.find(mangledName);
  if (found == LegacyTypeInfos.end())
    return None;

  return found->second;
}

TypeConverter::TypeConverter(IRGenModule &IGM)
  : IGM(IGM),
    FirstType(invalidTypeInfo()) {
  // FIXME: In LLDB, everything is completely fragile, so that IRGen can query
  // the size of resilient types. Of course this is not the right long term
  // solution, because it won't work once the swiftmodule file is not in
  // sync with the binary module. Once LLDB can calculate type layouts at
  // runtime (using remote mirrors or some other mechanism), we can remove this.
  if (IGM.IRGen.Opts.EnableResilienceBypass)
    LoweringMode = Mode::CompletelyFragile;

  StringRef path = IGM.IRGen.Opts.ReadTypeInfoPath;
  if (!path.empty()) {
    bool error = readLegacyTypeInfo(path);
    if (error) {
      llvm::report_fatal_error("Cannot read '" + path + "'");
    }
  }
}

TypeConverter::~TypeConverter() {
  // Delete all the converted type infos.
  for (const TypeInfo *I = FirstType; I != invalidTypeInfo(); ) {
    const TypeInfo *Cur = I;
    I = Cur->NextConverted;
    delete Cur;
  }
}

void TypeConverter::pushGenericContext(CanGenericSignature signature) {
  if (!signature)
    return;
  
  // Push the generic context down to the SIL TypeConverter, so we can share
  // archetypes with SIL.
  IGM.getSILTypes().pushGenericContext(signature);

  // Clear the dependent type info cache since we have a new active signature
  // now.
  Types.getCacheFor(/*isDependent*/ true, Mode::Normal).clear();
  Types.getCacheFor(/*isDependent*/ true, Mode::Legacy).clear();
  Types.getCacheFor(/*isDependent*/ true, Mode::CompletelyFragile).clear();
}

void TypeConverter::popGenericContext(CanGenericSignature signature) {
  if (!signature)
    return;

  // Pop the SIL TypeConverter's generic context too.
  IGM.getSILTypes().popGenericContext(signature);
  
  Types.getCacheFor(/*isDependent*/ true, Mode::Normal).clear();
  Types.getCacheFor(/*isDependent*/ true, Mode::Legacy).clear();
  Types.getCacheFor(/*isDependent*/ true, Mode::CompletelyFragile).clear();
}

GenericEnvironment *TypeConverter::getGenericEnvironment() {
  auto genericSig = IGM.getSILTypes().getCurGenericContext();
  return genericSig->getCanonicalSignature().getGenericEnvironment();
}

GenericEnvironment *IRGenModule::getGenericEnvironment() {
  return Types.getGenericEnvironment();
}

/// Add a temporary forward declaration for a type.  This will live
/// only until a proper mapping is added.
void TypeConverter::addForwardDecl(TypeBase *key) {
  assert(key->isCanonical());
  assert(!key->hasTypeParameter());
  auto &Cache = Types.getCacheFor(/*isDependent*/ false, LoweringMode);
  auto result = Cache.insert(std::make_pair(key, nullptr));
  assert(result.second && "entry already exists for type!");
  (void) result;
}

const TypeInfo &IRGenModule::getWitnessTablePtrTypeInfo() {
  return Types.getWitnessTablePtrTypeInfo();
}

const LoadableTypeInfo &TypeConverter::getWitnessTablePtrTypeInfo() {
  if (WitnessTablePtrTI) return *WitnessTablePtrTI;
  WitnessTablePtrTI =
    createPrimitiveForAlignedPointer(IGM.WitnessTablePtrTy,
                                     IGM.getPointerSize(),
                                     IGM.getPointerAlignment(),
                                     IGM.getWitnessTableAlignment());
  WitnessTablePtrTI->NextConverted = FirstType;
  FirstType = WitnessTablePtrTI;
  return *WitnessTablePtrTI;
}

const SpareBitVector &IRGenModule::getWitnessTablePtrSpareBits() const {
  // Witness tables are pointers and have pointer spare bits.
  return TargetInfo.PointerSpareBits;
}

const TypeInfo &IRGenModule::getTypeMetadataPtrTypeInfo() {
  return Types.getTypeMetadataPtrTypeInfo();
}

const TypeInfo &TypeConverter::getTypeMetadataPtrTypeInfo() {
  if (TypeMetadataPtrTI) return *TypeMetadataPtrTI;
  TypeMetadataPtrTI = createUnmanagedStorageType(IGM.TypeMetadataPtrTy,
                                                 ReferenceCounting::Unknown,
                                                 /*isOptional*/false);
  TypeMetadataPtrTI->NextConverted = FirstType;
  FirstType = TypeMetadataPtrTI;
  return *TypeMetadataPtrTI;
}

const LoadableTypeInfo &
IRGenModule::getReferenceObjectTypeInfo(ReferenceCounting refcounting) {
  switch (refcounting) {
  case ReferenceCounting::Native:
    return getNativeObjectTypeInfo();
  case ReferenceCounting::Unknown:
    return getUnknownObjectTypeInfo();
  case ReferenceCounting::Bridge:
    return getBridgeObjectTypeInfo();
  case ReferenceCounting::Block:
  case ReferenceCounting::Error:
  case ReferenceCounting::ObjC:
    llvm_unreachable("not implemented");
  }

  llvm_unreachable("Not a valid ReferenceCounting.");
}

const LoadableTypeInfo &IRGenModule::getNativeObjectTypeInfo() {
  return Types.getNativeObjectTypeInfo();
}

const LoadableTypeInfo &TypeConverter::getNativeObjectTypeInfo() {
  if (NativeObjectTI) return *NativeObjectTI;
  NativeObjectTI = convertBuiltinNativeObject();
  NativeObjectTI->NextConverted = FirstType;
  FirstType = NativeObjectTI;
  return *NativeObjectTI;
}

const LoadableTypeInfo &IRGenModule::getUnknownObjectTypeInfo() {
  return Types.getUnknownObjectTypeInfo();
}

const LoadableTypeInfo &TypeConverter::getUnknownObjectTypeInfo() {
  if (UnknownObjectTI) return *UnknownObjectTI;
  UnknownObjectTI = convertBuiltinUnknownObject();
  UnknownObjectTI->NextConverted = FirstType;
  FirstType = UnknownObjectTI;
  return *UnknownObjectTI;
}

const LoadableTypeInfo &IRGenModule::getBridgeObjectTypeInfo() {
  return Types.getBridgeObjectTypeInfo();
}

const LoadableTypeInfo &TypeConverter::getBridgeObjectTypeInfo() {
  if (BridgeObjectTI) return *BridgeObjectTI;
  BridgeObjectTI = convertBuiltinBridgeObject();
  BridgeObjectTI->NextConverted = FirstType;
  FirstType = BridgeObjectTI;
  return *BridgeObjectTI;
}

const LoadableTypeInfo &IRGenModule::getRawPointerTypeInfo() {
  return Types.getRawPointerTypeInfo();
}

const LoadableTypeInfo &TypeConverter::getRawPointerTypeInfo() {
  if (RawPointerTI) return *RawPointerTI;
  RawPointerTI = new RawPointerTypeInfo(IGM.Int8PtrTy,
                                        IGM.getPointerSize(),
                                        IGM.getPointerAlignment());
  RawPointerTI->NextConverted = FirstType;
  FirstType = RawPointerTI;
  return *RawPointerTI;
}

const LoadableTypeInfo &TypeConverter::getEmptyTypeInfo() {
  if (EmptyTI) return *EmptyTI;
  EmptyTI = new EmptyTypeInfo(IGM.Int8Ty);
  EmptyTI->NextConverted = FirstType;
  FirstType = EmptyTI;
  return *EmptyTI;
}

const TypeInfo &
TypeConverter::getResilientStructTypeInfo(IsABIAccessible_t isAccessible) {
  auto &cache = isAccessible ? AccessibleResilientStructTI
                             : InaccessibleResilientStructTI;
  if (cache) return *cache;
  cache = convertResilientStruct(isAccessible);
  cache->NextConverted = FirstType;
  FirstType = cache;
  return *cache;
}

/// Get the fragile type information for the given type, which may not
/// have yet undergone SIL type lowering.  The type can serve as its own
/// abstraction pattern.
const TypeInfo &IRGenFunction::getTypeInfoForUnlowered(Type subst) {
  return IGM.getTypeInfoForUnlowered(subst);
}

/// Get the fragile type information for the given type, which may not
/// have yet undergone SIL type lowering.
const TypeInfo &
IRGenFunction::getTypeInfoForUnlowered(AbstractionPattern orig, Type subst) {
  return IGM.getTypeInfoForUnlowered(orig, subst);
}

/// Get the fragile type information for the given type, which may not
/// have yet undergone SIL type lowering.
const TypeInfo &
IRGenFunction::getTypeInfoForUnlowered(AbstractionPattern orig, CanType subst) {
  return IGM.getTypeInfoForUnlowered(orig, subst);
}

/// Get the fragile type information for the given type, which is known
/// to have undergone SIL type lowering (or be one of the types for
/// which that lowering is the identity function).
const TypeInfo &IRGenFunction::getTypeInfoForLowered(CanType T) {
  return IGM.getTypeInfoForLowered(T);
}

/// Get the fragile type information for the given type.
const TypeInfo &IRGenFunction::getTypeInfo(SILType T) {
  return IGM.getTypeInfo(T);
}

/// Return the SIL-lowering of the given type.
SILType IRGenModule::getLoweredType(AbstractionPattern orig, Type subst) {
  return getSILTypes().getLoweredType(orig, subst);
}

/// Return the SIL-lowering of the given type.
SILType IRGenModule::getLoweredType(Type subst) {
  return getSILTypes().getLoweredType(subst);
}

/// Get a pointer to the storage type for the given type.  Note that,
/// unlike fetching the type info and asking it for the storage type,
/// this operation will succeed for forward-declarations.
llvm::PointerType *IRGenModule::getStoragePointerType(SILType T) {
  return getStoragePointerTypeForLowered(T.getASTType());
}
llvm::PointerType *IRGenModule::getStoragePointerTypeForUnlowered(Type T) {
  return getStorageTypeForUnlowered(T)->getPointerTo();
}
llvm::PointerType *IRGenModule::getStoragePointerTypeForLowered(CanType T) {
  return getStorageTypeForLowered(T)->getPointerTo();
}

llvm::Type *IRGenModule::getStorageTypeForUnlowered(Type subst) {
  return getStorageType(getSILTypes().getLoweredType(subst));
}

llvm::Type *IRGenModule::getStorageType(SILType T) {
  return getStorageTypeForLowered(T.getASTType());
}

/// Get the storage type for the given type.
llvm::Type *IRGenModule::getStorageTypeForLowered(CanType T) {
  return Types.getTypeEntry(T)->getStorageType();
}

/// Get the type information for the given type, which may not have
/// yet undergone SIL type lowering.  The type can serve as its own
/// abstraction pattern.
const TypeInfo &IRGenModule::getTypeInfoForUnlowered(Type subst) {
  return getTypeInfoForUnlowered(AbstractionPattern(subst), subst);
}

/// Get the type information for the given type, which may not
/// have yet undergone SIL type lowering.
const TypeInfo &
IRGenModule::getTypeInfoForUnlowered(AbstractionPattern orig, Type subst) {
  return getTypeInfoForUnlowered(orig, subst->getCanonicalType());
}

/// Get the type information for the given type, which may not
/// have yet undergone SIL type lowering.
const TypeInfo &
IRGenModule::getTypeInfoForUnlowered(AbstractionPattern orig, CanType subst) {
  return getTypeInfo(getSILTypes().getLoweredType(orig, subst));
}

/// Get the fragile type information for the given type, which is known
/// to have undergone SIL type lowering (or be one of the types for
/// which that lowering is the identity function).
const TypeInfo &IRGenModule::getTypeInfo(SILType T) {
  return getTypeInfoForLowered(T.getASTType());
}

/// Get the fragile type information for the given type.
const TypeInfo &IRGenModule::getTypeInfoForLowered(CanType T) {
  return Types.getCompleteTypeInfo(T);
}

/// 
const TypeInfo &TypeConverter::getCompleteTypeInfo(CanType T) {
  return *getTypeEntry(T);
}

ArchetypeType *TypeConverter::getExemplarArchetype(ArchetypeType *t) {
  // Retrieve the generic environment of the archetype.
  auto genericEnv = t->getGenericEnvironment();

  // If there is no generic environment, the archetype is an exemplar.
  if (!genericEnv) return t;

  // Dig out the canonical generic environment.
  auto genericSig = genericEnv->getGenericSignature();
  auto canGenericSig = genericSig->getCanonicalSignature();
  auto canGenericEnv = canGenericSig.getGenericEnvironment();
  if (canGenericEnv == genericEnv) return t;

  // Map the archetype out of its own generic environment and into the
  // canonical generic environment.
  auto interfaceType = t->getInterfaceType();
  auto exemplar = canGenericEnv->mapTypeIntoContext(interfaceType)
                    ->castTo<ArchetypeType>();
  assert(isExemplarArchetype(exemplar));
  return exemplar;
}

/// Fold archetypes to unique exemplars. Any archetype with the same
/// constraints is equivalent for type lowering purposes.
CanType TypeConverter::getExemplarType(CanType contextTy) {
  // FIXME: A generic SILFunctionType should not contain any nondependent
  // archetypes.
  if (isa<SILFunctionType>(contextTy)
      && cast<SILFunctionType>(contextTy)->isPolymorphic()) {
    return contextTy;
  } else {
    auto exemplified = contextTy.subst(
      [&](SubstitutableType *type) -> Type {
        if (auto arch = dyn_cast<ArchetypeType>(type))
          return getExemplarArchetype(arch);
        return type;
      },
      MakeAbstractConformanceForGenericType(),
      SubstFlags::AllowLoweredTypes);
    return CanType(exemplified);
  }
}

const TypeInfo *TypeConverter::getTypeEntry(CanType canonicalTy) {
  // Cache this entry in the dependent or independent cache appropriate to it.
  auto &Cache = Types.getCacheFor(canonicalTy->hasTypeParameter(),
                                  LoweringMode);

  {
    auto it = Cache.find(canonicalTy.getPointer());
    if (it != Cache.end()) {
      return it->second;
    }
  }
  
  // If the type is dependent, substitute it into our current context.
  auto contextTy = canonicalTy;
  if (contextTy->hasTypeParameter()) {
    // The type we got should be lowered, so lower it like a SILType.
    contextTy = getGenericEnvironment()->mapTypeIntoContext(
                  IGM.getSILModule(),
                  SILType::getPrimitiveAddressType(contextTy)).getASTType();
  }
  
  // Fold archetypes to unique exemplars. Any archetype with the same
  // constraints is equivalent for type lowering purposes.
  CanType exemplarTy = getExemplarType(contextTy);
  assert(!exemplarTy->hasTypeParameter());
  
  // See whether we lowered a type equivalent to this one.
  if (exemplarTy != canonicalTy) {
    auto &Cache = Types.getCacheFor(/*isDependent*/ false, LoweringMode);
    auto it = Cache.find(exemplarTy.getPointer());
    if (it != Cache.end()) {
      // Record the object under the original type.
      auto result = it->second;
      Cache[canonicalTy.getPointer()] = result;
      return result;
    }
  }

  // Convert the type.
  auto *convertedTI = convertType(exemplarTy);

  // Cache the entry under the original type and the exemplar type, so that
  // we can avoid relowering equivalent types.
  auto insertEntry = [&](const TypeInfo *&entry) {
    assert(entry == nullptr);
    entry = convertedTI;
  };
  insertEntry(Cache[canonicalTy.getPointer()]);
  if (canonicalTy != exemplarTy) {
    auto &IndependentCache = Types.getCacheFor(/*isDependent*/ false,
                                               LoweringMode);
    insertEntry(IndependentCache[exemplarTy.getPointer()]);
  }
  
  // If the type info hasn't been added to the list of types, do so.
  if (!convertedTI->NextConverted) {
    convertedTI->NextConverted = FirstType;
    FirstType = convertedTI;
  }

  return convertedTI;
}

/// Return a TypeInfo the represents opaque storage for a loadable POD value
/// with the given storage size.
///
/// The formal alignment of the opaque storage will be 1.
///
/// The TypeInfo will completely ignore any type passed to its
/// implementation methods; it is safe to pass a null type.
const LoadableTypeInfo &
IRGenModule::getOpaqueStorageTypeInfo(Size size, Alignment align) {
  return Types.getOpaqueStorageTypeInfo(size, align);
}

const LoadableTypeInfo &
TypeConverter::getOpaqueStorageTypeInfo(Size size, Alignment align) {
  assert(!size.isZero());
  std::pair<unsigned, unsigned> key = {size.getValue(), align.getValue()};
  auto existing = OpaqueStorageTypes.find(key);
  if (existing != OpaqueStorageTypes.end())
    return *existing->second;

  // Use an [N x i8] array for storage, but load and store as a single iNNN
  // scalar.
  auto storageType = llvm::ArrayType::get(IGM.Int8Ty, size.getValue());
  auto intType = llvm::IntegerType::get(IGM.LLVMContext, size.getValueInBits());
  // There are no spare bits in an opaque storage type.
  auto type = new OpaqueStorageTypeInfo(storageType, intType, size,
                    SpareBitVector::getConstant(size.getValueInBits(), false),
                    align);
  
  type->NextConverted = FirstType;
  FirstType = type;

  OpaqueStorageTypes[key] = type;

  return *type;
}

/// \brief Convert a primitive builtin type to its LLVM type, size, and
/// alignment.
static std::tuple<llvm::Type *, Size, Alignment>
convertPrimitiveBuiltin(IRGenModule &IGM, CanType canTy) {
  using RetTy = std::tuple<llvm::Type *, Size, Alignment>;
  llvm::LLVMContext &ctx = IGM.getLLVMContext();
  TypeBase *ty = canTy.getPointer();
  switch (ty->getKind()) {
  case TypeKind::BuiltinRawPointer:
    return RetTy{ IGM.Int8PtrTy, IGM.getPointerSize(),
                  IGM.getPointerAlignment() };
  case TypeKind::BuiltinFloat:
    switch (cast<BuiltinFloatType>(ty)->getFPKind()) {
    case BuiltinFloatType::IEEE16:
      return RetTy{ llvm::Type::getHalfTy(ctx), Size(2), Alignment(2) };
    case BuiltinFloatType::IEEE32:
      return RetTy{ llvm::Type::getFloatTy(ctx), Size(4), Alignment(4) };
    case BuiltinFloatType::IEEE64:
      return RetTy{ llvm::Type::getDoubleTy(ctx), Size(8), Alignment(8) };
    case BuiltinFloatType::IEEE80: {
      llvm::Type *floatTy = llvm::Type::getX86_FP80Ty(ctx);
      uint64_t ByteSize = IGM.DataLayout.getTypeAllocSize(floatTy);
      unsigned align = IGM.DataLayout.getABITypeAlignment(floatTy);
      return RetTy{ floatTy, Size(ByteSize), Alignment(align) };
    }
    case BuiltinFloatType::IEEE128:
      return RetTy{ llvm::Type::getFP128Ty(ctx), Size(16), Alignment(16) };
    case BuiltinFloatType::PPC128:
      return RetTy{ llvm::Type::getPPC_FP128Ty(ctx),Size(16), Alignment(16) };
    }
    llvm_unreachable("bad builtin floating-point type kind");
  case TypeKind::BuiltinInteger: {
    unsigned BitWidth = IGM.getBuiltinIntegerWidth(cast<BuiltinIntegerType>(ty));
    unsigned ByteSize = (BitWidth+7U)/8U;
    // Round up the memory size and alignment to a power of 2.
    if (!llvm::isPowerOf2_32(ByteSize))
      ByteSize = llvm::NextPowerOf2(ByteSize);

    return RetTy{ llvm::IntegerType::get(ctx, BitWidth), Size(ByteSize),
             Alignment(ByteSize) };
  }
  case TypeKind::BuiltinVector: {
    auto vecTy = ty->castTo<BuiltinVectorType>();
    llvm::Type *elementTy;
    Size size;
    Alignment align;
    std::tie(elementTy, size, align)
      = convertPrimitiveBuiltin(IGM,
                                vecTy->getElementType()->getCanonicalType());

    auto llvmVecTy = llvm::VectorType::get(elementTy, vecTy->getNumElements());
    unsigned bitSize = size.getValue() * vecTy->getNumElements() * 8;
    if (!llvm::isPowerOf2_32(bitSize))
      bitSize = llvm::NextPowerOf2(bitSize);

    return RetTy{ llvmVecTy, Size(bitSize / 8), align };
  }
  default:
    llvm_unreachable("Not a primitive builtin type");
  }
}

const TypeInfo *TypeConverter::convertType(CanType ty) {
  PrettyStackTraceType stackTrace(IGM.Context, "converting", ty);

  switch (ty->getKind()) {
  case TypeKind::Error:
    llvm_unreachable("found an ErrorType in IR-gen");

#define UNCHECKED_TYPE(id, parent) \
  case TypeKind::id: \
    llvm_unreachable("found a " #id "Type in IR-gen");
#define SUGARED_TYPE(id, parent) \
  case TypeKind::id: \
    llvm_unreachable("converting a " #id "Type after canonicalization");
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::LValue: llvm_unreachable("@lvalue type made it to irgen");
  case TypeKind::ExistentialMetatype:
    return convertExistentialMetatypeType(cast<ExistentialMetatypeType>(ty));
  case TypeKind::Metatype:
    return convertMetatypeType(cast<MetatypeType>(ty));
  case TypeKind::Module:
    return convertModuleType(cast<ModuleType>(ty));
  case TypeKind::DynamicSelf: {
    // DynamicSelf has the same representation as its superclass type.
    auto dynamicSelf = cast<DynamicSelfType>(ty);
    auto nominal = dynamicSelf->getSelfType()->getAnyNominal();
    return convertAnyNominalType(ty, nominal);
  }
  case TypeKind::BuiltinNativeObject:
    return &getNativeObjectTypeInfo();
  case TypeKind::BuiltinUnknownObject:
    return &getUnknownObjectTypeInfo();
  case TypeKind::BuiltinBridgeObject:
    return &getBridgeObjectTypeInfo();
  case TypeKind::BuiltinUnsafeValueBuffer:
    return createImmovable(IGM.getFixedBufferTy(),
                           getFixedBufferSize(IGM),
                           getFixedBufferAlignment(IGM));
  case TypeKind::BuiltinRawPointer:
    return &getRawPointerTypeInfo();
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinVector: {
    llvm::Type *llvmTy;
    Size size;
    Alignment align;
    std::tie(llvmTy, size, align) = convertPrimitiveBuiltin(IGM, ty);
    align = IGM.getCappedAlignment(align);
    return createPrimitive(llvmTy, size, align);
  }

  case TypeKind::Archetype:
    return convertArchetypeType(cast<ArchetypeType>(ty));
  case TypeKind::Class:
  case TypeKind::Enum:
  case TypeKind::Struct:
    return convertAnyNominalType(ty, cast<NominalType>(ty)->getDecl());
  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
    return convertAnyNominalType(ty, cast<BoundGenericType>(ty)->getDecl());
  case TypeKind::InOut:
    return convertInOutType(cast<InOutType>(ty));
  case TypeKind::Tuple:
    return convertTupleType(cast<TupleType>(ty));
  case TypeKind::Function:
  case TypeKind::GenericFunction:
    llvm_unreachable("AST FunctionTypes should be lowered by SILGen");
  case TypeKind::SILFunction:
    return convertFunctionType(cast<SILFunctionType>(ty));
  case TypeKind::Protocol:
    return convertProtocolType(cast<ProtocolType>(ty));
  case TypeKind::ProtocolComposition:
    return convertProtocolCompositionType(cast<ProtocolCompositionType>(ty));
  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
    llvm_unreachable("can't convert dependent type");
#define REF_STORAGE(Name, ...) \
  case TypeKind::Name##Storage: \
    return convert##Name##StorageType(cast<Name##StorageType>(ty));
#include "swift/AST/ReferenceStorage.def"
  case TypeKind::SILBlockStorage: {
    return convertBlockStorageType(cast<SILBlockStorageType>(ty));
  case TypeKind::SILBox:
    return convertBoxType(cast<SILBoxType>(ty));
  case TypeKind::SILToken:
    llvm_unreachable("should not be asking for representation of a SILToken");
  }
  }
  llvm_unreachable("bad type kind");
}

/// Convert an inout type.  This is always just a bare pointer.
const TypeInfo *TypeConverter::convertInOutType(InOutType *T) {
  auto referenceType =
    IGM.getStoragePointerTypeForUnlowered(CanType(T->getObjectType()));
  
  // Just use the reference type as a primitive pointer.
  return createPrimitive(referenceType, IGM.getPointerSize(),
                         IGM.getPointerAlignment());
}

/// Convert a reference storage type. The implementation here depends on the
/// underlying reference type. The type may be optional.
#define REF_STORAGE(Name, ...) \
const TypeInfo * \
TypeConverter::convert##Name##StorageType(Name##StorageType *refType) { \
  CanType referent(refType->getReferentType()); \
  bool isOptional = false; \
  if (auto referentObj = referent.getOptionalObjectType()) { \
    referent = referentObj; \
    isOptional = true; \
  } \
  assert(referent->allowsOwnership()); \
  auto &referentTI = cast<ReferenceTypeInfo>(getCompleteTypeInfo(referent)); \
  return referentTI.create##Name##StorageType(*this, isOptional); \
}
#include "swift/AST/ReferenceStorage.def"

static void overwriteForwardDecl(llvm::DenseMap<TypeBase *, const TypeInfo *> &cache,
                                 TypeBase *key, const TypeInfo *result) {
  assert(cache.count(key) && "no forward declaration?");
  assert(cache[key] == nullptr && "overwriting real entry!");
  cache[key] = result;
}

namespace {
  /// Is IR-gen type-dependent for the given type?  Specifically, will
  /// basic operations on the type misbehave (e.g. by having an IR
  /// type mismatch) if an aggregate type containing a value of this
  /// type is generated generically rather than independently for
  /// different specializations?
  class IsIRTypeDependent : public CanTypeVisitor<IsIRTypeDependent, bool> {
    IRGenModule &IGM;
  public:
    IsIRTypeDependent(IRGenModule &IGM) : IGM(IGM) {}

    // If the type isn't actually dependent, we're okay.
    bool visit(CanType type) {
      if (!type->hasArchetype() && !type->hasTypeParameter())
        return false;
      return CanTypeVisitor::visit(type);
    }

    // Dependent struct types need their own implementation if any
    // field type might need its own implementation.
    bool visitStructType(CanStructType type) {
      return visitStructDecl(type->getDecl());
    }
    bool visitBoundGenericStructType(CanBoundGenericStructType type) {
      return visitStructDecl(type->getDecl());
    }
    bool visitStructDecl(StructDecl *decl) {
      if (IGM.isResilient(decl, ResilienceExpansion::Maximal))
        return true;

      for (auto field : decl->getStoredProperties()) {
        if (visit(field->getInterfaceType()->getCanonicalType()))
          return true;
      }
      return false;
    }

    // Dependent enum types need their own implementation if any
    // element payload type might need its own implementation.
    bool visitEnumType(CanEnumType type) {
      return visitEnumDecl(type->getDecl());
    }
    bool visitBoundGenericEnumType(CanBoundGenericEnumType type) {
      return visitEnumDecl(type->getDecl());
    }
    bool visitEnumDecl(EnumDecl *decl) {
      if (IGM.isResilient(decl, ResilienceExpansion::Maximal))
        return true;
      if (decl->isIndirect())
        return false;

      for (auto elt : decl->getAllElements()) {
        if (elt->hasAssociatedValues() &&
            !elt->isIndirect() &&
            visit(elt->getArgumentInterfaceType()->getCanonicalType()))
          return true;
      }
      return false;
    }

    // Conservatively assume classes need unique implementations.
    bool visitClassType(CanClassType type) {
      return visitClassDecl(type->getDecl());
     }
    bool visitBoundGenericClassType(CanBoundGenericClassType type) {
      return visitClassDecl(type->getDecl());
    }
    bool visitClassDecl(ClassDecl *theClass) {
      return true;
    }

    // Reference storage types propagate the decision.
    bool visitReferenceStorageType(CanReferenceStorageType type) {
      return visit(type.getReferentType());
    }

    // The IR-generation for function types is specifically not
    // type-dependent.
    bool visitAnyFunctionType(CanAnyFunctionType type) {
      return false;
    }

    // The safe default for a dependent type is to assume that it
    // needs its own implementation.
    bool visitType(CanType type) {
      return true;
    }
  };
} // end anonymous namespace

static bool isIRTypeDependent(IRGenModule &IGM, NominalTypeDecl *decl) {
  assert(!isa<ProtocolDecl>(decl));
  if (auto classDecl = dyn_cast<ClassDecl>(decl)) {
    return IsIRTypeDependent(IGM).visitClassDecl(classDecl);
  } else if (auto structDecl = dyn_cast<StructDecl>(decl)) {
    return IsIRTypeDependent(IGM).visitStructDecl(structDecl);
  } else {
    auto enumDecl = cast<EnumDecl>(decl);
    return IsIRTypeDependent(IGM).visitEnumDecl(enumDecl);
  }
}

namespace {

class LegacyTypeInfo : public FixedTypeInfo {
  unsigned NumExtraInhabitants;

public:
  LegacyTypeInfo(llvm::Type *type, const SpareBitVector &spareBits,
                 const YAMLTypeInfoNode &node)
    : FixedTypeInfo(type,
                    Size(node.Size),
                    spareBits,
                    Alignment(node.Alignment),
                    IsNotPOD, /* irrelevant */
                    IsNotBitwiseTakable, /* irrelevant */
                    IsFixedSize /* irrelevant */),
      NumExtraInhabitants(node.NumExtraInhabitants) {}

  virtual unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const override {
    return NumExtraInhabitants;
  }

  virtual APInt getFixedExtraInhabitantMask(IRGenModule &IGM) const override {
    llvm_unreachable("TypeConverter::Mode::Legacy is not for real values");
  }

  virtual APInt getFixedExtraInhabitantValue(IRGenModule &IGM,
                                             unsigned bits,
                                             unsigned index) const override {
    llvm_unreachable("TypeConverter::Mode::Legacy is not for real values");
  }

  virtual void getSchema(ExplosionSchema &schema) const override {
    llvm_unreachable("TypeConverter::Mode::Legacy is not for real values");
  }

  virtual void assignWithCopy(IRGenFunction &IGF, Address dest, Address src,
                              SILType T, bool isOutlined) const override {
    llvm_unreachable("TypeConverter::Mode::Legacy is not for real values");
  }

  virtual void assignWithTake(IRGenFunction &IGF, Address dest, Address src,
                              SILType T, bool isOutlined) const override {
    llvm_unreachable("TypeConverter::Mode::Legacy is not for real values");
  }

  virtual void initializeWithCopy(IRGenFunction &IGF, Address destAddr,
                                  Address srcAddr, SILType T,
                                  bool isOutlined) const override {
    llvm_unreachable("TypeConverter::Mode::Legacy is not for real values");
  }

  virtual void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                                    Address src, SILType T,
                                    bool isOutlined) const override {
    llvm_unreachable("TypeConverter::Mode::Legacy is not for real values");
  }

  virtual void destroy(IRGenFunction &IGF, Address address, SILType T,
                       bool isOutlined) const override {
    llvm_unreachable("TypeConverter::Mode::Legacy is not for real values");
  }
};

} // namespace

const TypeInfo *TypeConverter::convertAnyNominalType(CanType type,
                                                     NominalTypeDecl *decl) {
  // By "any", we don't mean existentials.
  assert(!isa<ProtocolDecl>(decl));

  // If we're producing a legacy type layout, and we have a serialized
  // record for this type, produce it now.
  if (LoweringMode == Mode::Legacy) {
    auto node = getLegacyTypeInfo(decl);

    if (node) {
      Size size(node->Size);

      auto ty = IGM.createNominalType(type);
      ty->setBody(llvm::ArrayType::get(IGM.Int8Ty, size.getValue()));

      SpareBitVector spareBits;
      spareBits.appendClearBits(size.getValueInBits());

      return new LegacyTypeInfo(ty, spareBits, *node);
    }
  }

  // We need to give generic specializations distinct TypeInfo objects
  // if their IR-gen might be different, e.g. if they use different IR
  // types or if type-specific operations like projections might need
  // to be handled differently.
  if (!decl->isGenericContext() || isIRTypeDependent(IGM, decl)) {
    switch (decl->getKind()) {
#define NOMINAL_TYPE_DECL(ID, PARENT)
#define DECL(ID, PARENT) \
    case DeclKind::ID:
#include "swift/AST/DeclNodes.def"
      llvm_unreachable("not a nominal type declaration");
    case DeclKind::Protocol:
      llvm_unreachable("protocol types shouldn't be handled here");

    case DeclKind::Class:
      return convertClassType(type, cast<ClassDecl>(decl));
    case DeclKind::Enum:
      return convertEnumType(type.getPointer(), type, cast<EnumDecl>(decl));
    case DeclKind::Struct:
      return convertStructType(type.getPointer(), type, cast<StructDecl>(decl));
    }
    llvm_unreachable("bad declaration kind");
  }

  assert(decl->isGenericContext());

  // Look to see if we've already emitted this type under a different
  // set of arguments.  We cache under the unbound type, which should
  // never collide with anything.
  //
  // FIXME: this isn't really inherently good; we might want to use
  // different type implementations for different applications.
  assert(decl->getDeclaredType()->isCanonical());
  assert(decl->getDeclaredType()->hasUnboundGenericType());
  TypeBase *key = decl->getDeclaredType().getPointer();
  auto &Cache = Types.getCacheFor(/*isDependent*/ false, LoweringMode);
  auto entry = Cache.find(key);
  if (entry != Cache.end())
    return entry->second;

  switch (decl->getKind()) {
#define NOMINAL_TYPE_DECL(ID, PARENT)
#define DECL(ID, PARENT) \
  case DeclKind::ID:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("not a nominal type declaration");

  case DeclKind::Protocol:
    llvm_unreachable("protocol types don't take generic parameters");

  case DeclKind::Class:
    llvm_unreachable("classes are always considered dependent for now");

  case DeclKind::Enum: {
    auto type = decl->getDeclaredTypeInContext()->getCanonicalType();
    auto result = convertEnumType(key, type, cast<EnumDecl>(decl));
    overwriteForwardDecl(Cache, key, result);
    return result;
  }

  case DeclKind::Struct: {
    auto type = decl->getDeclaredTypeInContext()->getCanonicalType();
    auto result = convertStructType(key, type, cast<StructDecl>(decl));
    overwriteForwardDecl(Cache, key, result);
    return result;
  }
  }
  llvm_unreachable("bad declaration kind");
}

const TypeInfo *TypeConverter::convertModuleType(ModuleType *T) {
  return new EmptyTypeInfo(IGM.Int8Ty);
}

const TypeInfo *TypeConverter::convertMetatypeType(MetatypeType *T) {
  assert(T->hasRepresentation() &&
         "metatype should have been assigned a representation by SIL");

  return &getMetatypeTypeInfo(T->getRepresentation());
}

const TypeInfo &
TypeConverter::getMetatypeTypeInfo(MetatypeRepresentation representation) {
  switch (representation) {
  case MetatypeRepresentation::Thin:
    // Thin metatypes are empty.
    return getEmptyTypeInfo();

  case MetatypeRepresentation::Thick:
    // Thick metatypes are represented with a metadata pointer.
    return getTypeMetadataPtrTypeInfo();

  case MetatypeRepresentation::ObjC:
    // ObjC metatypes are represented with an objc_class pointer.
    return getObjCClassPtrTypeInfo();
  }
  llvm_unreachable("bad representation");
}

/// createNominalType - Create a new nominal type.
llvm::StructType *IRGenModule::createNominalType(CanType type) {
  assert(type.getNominalOrBoundGenericNominal());

  // We share type infos for different instantiations of a generic type
  // when the archetypes have the same exemplars.  We cannot mangle
  // archetypes, and the mangling does not have to be unique, so we just
  // mangle the unbound generic form of the type.
  if (type->hasArchetype())
    type = type.getNominalOrBoundGenericNominal()->getDeclaredType()
                                                 ->getCanonicalType();

  IRGenMangler Mangler;
  std::string typeName = Mangler.mangleTypeForLLVMTypeName(type);
  return llvm::StructType::create(getLLVMContext(), StringRef(typeName));
}

/// createNominalType - Create a new nominal LLVM type for the given
/// protocol composition type.  Protocol composition types are
/// structural in the swift type system, but LLVM's type system
/// doesn't really care about this distinction, and it's nice to
/// distinguish different cases.
llvm::StructType *
IRGenModule::createNominalType(ProtocolCompositionType *type) {
  IRGenMangler Mangler;
  std::string typeName = Mangler.mangleProtocolForLLVMTypeName(type);
  return llvm::StructType::create(getLLVMContext(), StringRef(typeName));
}

SpareBitVector IRGenModule::getSpareBitsForType(llvm::Type *scalarTy, Size size) {
  auto it = SpareBitsForTypes.find(scalarTy);
  if (it != SpareBitsForTypes.end())
    return it->second;

  assert(DataLayout.getTypeAllocSizeInBits(scalarTy) <= size.getValueInBits() &&
         "using a size that's smaller than LLVM's alloc size?");
  
  {
    // FIXME: Currently we only implement spare bits for primitive integer
    // types.
    assert(!isa<llvm::StructType>(scalarTy));

    auto *intTy = dyn_cast<llvm::IntegerType>(scalarTy);
    if (!intTy)
      goto no_spare_bits;

    // Round Integer-Of-Unusual-Size types up to their allocation size.
    unsigned allocBits = size.getValueInBits();
    assert(allocBits >= intTy->getBitWidth());
        
    // FIXME: Endianness.
    SpareBitVector &result = SpareBitsForTypes[scalarTy];
    result.appendClearBits(intTy->getBitWidth());
    result.extendWithSetBits(allocBits);
    return result;
  }
  
no_spare_bits:
  SpareBitVector &result = SpareBitsForTypes[scalarTy];
  result.appendClearBits(size.getValueInBits());
  return result;
}

unsigned IRGenModule::getBuiltinIntegerWidth(BuiltinIntegerType *t) {
  return getBuiltinIntegerWidth(t->getWidth());
}

unsigned IRGenModule::getBuiltinIntegerWidth(BuiltinIntegerWidth w) {
  if (w.isFixedWidth())
    return w.getFixedWidth();
  if (w.isPointerWidth())
    return getPointerSize().getValueInBits();
  llvm_unreachable("impossible width value");
}

void IRGenFunction::setLocalSelfMetadata(llvm::Value *value,
                                         IRGenFunction::LocalSelfKind kind) {
  assert(!LocalSelf && "already have local self metadata");
  LocalSelf = value;
  SelfKind = kind;
}

#ifndef NDEBUG
bool TypeConverter::isExemplarArchetype(ArchetypeType *arch) const {
  auto genericEnv = arch->getGenericEnvironment();
  if (!genericEnv) return true;

  // Dig out the canonical generic environment.
  auto genericSig = genericEnv->getGenericSignature();
  auto canGenericSig = genericSig->getCanonicalSignature();
  auto canGenericEnv = canGenericSig.getGenericEnvironment();

  // If this archetype is in the canonical generic environment, it's an
  // exemplar archetype.
  return canGenericEnv == genericEnv;
}
#endif

SILType irgen::getSingletonAggregateFieldType(IRGenModule &IGM, SILType t,
                                              ResilienceExpansion expansion) {
  if (auto tuple = t.getAs<TupleType>())
    if (tuple->getNumElements() == 1)
      return t.getTupleElementType(0);

  if (auto structDecl = t.getStructOrBoundGenericStruct()) {
    // If the struct has to be accessed resiliently from this resilience domain,
    // we can't assume anything about its layout.
    if (IGM.isResilient(structDecl, expansion))
      return SILType();

    // C ABI wackiness may cause a single-field struct to have different layout
    // from its field.
    if (structDecl->hasUnreferenceableStorage()
        || structDecl->hasClangNode())
      return SILType();

    // A single-field struct with custom alignment has different layout from its
    // field.
    if (structDecl->getAttrs().hasAttribute<AlignmentAttr>())
      return SILType();

    // If there's only one stored property, we have the layout of its field.
    auto allFields = structDecl->getStoredProperties();
    
    auto field = allFields.begin();
    if (!allFields.empty() && std::next(field) == allFields.end())
      return t.getFieldType(*field, IGM.getSILModule());

    return SILType();
  }

  if (auto enumDecl = t.getEnumOrBoundGenericEnum()) {
    // If the enum has to be accessed resiliently from this resilience domain,
    // we can't assume anything about its layout.
    if (IGM.isResilient(enumDecl, expansion))
      return SILType();

    auto allCases = enumDecl->getAllElements();
    
    auto theCase = allCases.begin();
    if (!allCases.empty() && std::next(theCase) == allCases.end()
        && (*theCase)->hasAssociatedValues())
      return t.getEnumElementType(*theCase, IGM.getSILModule());

    return SILType();
  }

  return SILType();
}

void TypeInfo::verify(IRGenTypeVerifierFunction &IGF,
                      llvm::Value *typeMetadata,
                      SILType T) const {
  // By default, no type-specific verifier behavior.
}
