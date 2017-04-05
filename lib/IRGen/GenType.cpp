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
#include "ProtocolInfo.h"
#include "ReferenceTypeInfo.h"
#include "ScalarTypeInfo.h"
#include "WeakTypeInfo.h"
#include "NativeConventionSchema.h"
#include "IRGenMangler.h"

using namespace swift;
using namespace irgen;

llvm::DenseMap<TypeBase*, TypeCacheEntry> &
TypeConverter::Types_t::getCacheFor(TypeBase *t) {
  return t->hasTypeParameter() ? DependentCache : IndependentCache;
}

void TypeInfo:: assign(IRGenFunction &IGF, Address dest, Address src,
                       IsTake_t isTake, SILType T) const {
  if (isTake) {
    assignWithTake(IGF, dest, src, T);
  } else {
    assignWithCopy(IGF, dest, src, T);
  }
}

void TypeInfo::initialize(IRGenFunction &IGF, Address dest, Address src,
                          IsTake_t isTake, SILType T) const {
  if (isTake) {
    initializeWithTake(IGF, dest, src, T);
  } else {
    initializeWithCopy(IGF, dest, src, T);
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
  return Address(ptr, StorageAlignment);
}

Address TypeInfo::getUndefAddress() const {
  return Address(llvm::UndefValue::get(getStorageType()->getPointerTo(0)),
                 StorageAlignment);
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
void FixedTypeInfo::initializeWithTake(IRGenFunction &IGF,
                                       Address destAddr,
                                       Address srcAddr,
                                       SILType T) const {
  assert(isBitwiseTakable(ResilienceExpansion::Maximal)
        && "non-bitwise-takable type must override default initializeWithTake");
  
  // Prefer loads and stores if we won't make a million of them.
  // Maybe this should also require the scalars to have a fixed offset.
  ExplosionSchema schema = getSchema();
  if (!schema.containsAggregate() && schema.size() <= 2) {
    auto &loadableTI = cast<LoadableTypeInfo>(*this);
    Explosion copy;
    loadableTI.loadAsTake(IGF, srcAddr, copy);
    loadableTI.initialize(IGF, copy, destAddr);
    return;
  }

  // Otherwise, use a memcpy.
  IGF.emitMemCpy(destAddr, srcAddr, getFixedSize());
}

/// Copy a value from one object to a new object.  This is just the
/// default implementation.
void LoadableTypeInfo::initializeWithCopy(IRGenFunction &IGF,
                                          Address destAddr,
                                          Address srcAddr,
                                          SILType T) const {
  // Use memcpy if that's legal.
  if (isPOD(ResilienceExpansion::Maximal)) {
    return initializeWithTake(IGF, destAddr, srcAddr, T);
  }

  // Otherwise explode and re-implode.
  Explosion copy;
  loadAsCopy(IGF, srcAddr, copy);
  initialize(IGF, copy, destAddr);
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

/// Return the size and alignment of this type.
std::pair<llvm::Value*,llvm::Value*>
FixedTypeInfo::getSizeAndAlignmentMask(IRGenFunction &IGF,
                                       SILType T) const {
  return {FixedTypeInfo::getSize(IGF, T),
          FixedTypeInfo::getAlignmentMask(IGF, T)};
}
std::tuple<llvm::Value*,llvm::Value*,llvm::Value*>
FixedTypeInfo::getSizeAndAlignmentMaskAndStride(IRGenFunction &IGF,
                                                SILType T) const {
  return std::make_tuple(FixedTypeInfo::getSize(IGF, T),
                         FixedTypeInfo::getAlignmentMask(IGF, T),
                         FixedTypeInfo::getStride(IGF, T));
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
  if (StorageSize.getValue() >= 4)
    return 0x7FFFFFFF;
  unsigned spareBitCount = SpareBits.count();
  assert(spareBitCount <= StorageSize.getValueInBits()
         && "more spare bits than storage bits?!");
  unsigned inhabitedBitCount = StorageSize.getValueInBits() - spareBitCount;
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
  auto payloadTy = llvm::IntegerType::get(C, StorageSize.getValueInBits());
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
  unsigned numOccupiedBits = StorageSize.getValueInBits() - numSpareBits;
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

void
FixedTypeInfo::storeSpareBitExtraInhabitant(IRGenFunction &IGF,
                                            llvm::Value *index,
                                            Address dest) const {
  assert(!SpareBits.none() && "no spare bits");
  
  auto &C = IGF.IGM.getLLVMContext();

  auto payloadTy = llvm::IntegerType::get(C, StorageSize.getValueInBits());

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
    void assign(IRGenFunction &IGF, Explosion &e,
                Address addr) const override {}
    void initialize(IRGenFunction &IGF, Explosion &e,
                    Address addr) const override {}
    void copy(IRGenFunction &IGF, Explosion &src,
              Explosion &dest, Atomicity atomicity) const override {}
    void consume(IRGenFunction &IGF, Explosion &src,
                 Atomicity atomicity) const override {}
    void fixLifetime(IRGenFunction &IGF, Explosion &src) const override {}
    void destroy(IRGenFunction &IGF, Address addr, SILType T) const override {}
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
                                         SILType T) const override {
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
                              Address dest, SILType T) const override {
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
    
    void assign(IRGenFunction &IGF, Explosion &explosion,
                Address addr) const override {
      initialize(IGF, explosion, addr);
    }
    
    void initialize(IRGenFunction &IGF, Explosion &explosion,
                    Address addr) const override {
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

    void destroy(IRGenFunction &IGF, Address address, SILType T) const override {
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

    void assignWithCopy(IRGenFunction &IGF, Address dest,
                        Address src, SILType T) const override {
      llvm_unreachable("cannot opaquely manipulate immovable types!");
    }

    void assignWithTake(IRGenFunction &IGF, Address dest,
                        Address src, SILType T) const override {
      llvm_unreachable("cannot opaquely manipulate immovable types!");
    }

    void initializeWithTake(IRGenFunction &IGF, Address destAddr,
                            Address srcAddr, SILType T) const override {
      llvm_unreachable("cannot opaquely manipulate immovable types!");
    }

    void initializeWithCopy(IRGenFunction &IGF, Address destAddr,
                            Address srcAddr, SILType T) const override {
      llvm_unreachable("cannot opaquely manipulate immovable types!");
    }

    void destroy(IRGenFunction &IGF, Address address,
                 SILType T) const override {
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
static ProtocolInfo *invalidProtocolInfo() { return (ProtocolInfo*) 1; }

TypeConverter::TypeConverter(IRGenModule &IGM)
  : IGM(IGM),
    FirstType(invalidTypeInfo()),
    FirstProtocol(invalidProtocolInfo()) {}

TypeConverter::~TypeConverter() {
  // Delete all the converted type infos.
  for (const TypeInfo *I = FirstType; I != invalidTypeInfo(); ) {
    const TypeInfo *Cur = I;
    I = Cur->NextConverted;
    delete Cur;
  }
  
  for (const ProtocolInfo *I = FirstProtocol; I != invalidProtocolInfo(); ) {
    const ProtocolInfo *Cur = I;
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
}

void TypeConverter::popGenericContext(CanGenericSignature signature) {
  if (!signature)
    return;

  // Pop the SIL TypeConverter's generic context too.
  IGM.getSILTypes().popGenericContext(signature);
  
  Types.DependentCache.clear();
}

GenericEnvironment *TypeConverter::getGenericEnvironment() {
  auto moduleDecl = IGM.getSwiftModule();
  auto genericSig = IGM.getSILTypes().getCurGenericContext();
  return genericSig->getCanonicalSignature().getGenericEnvironment(*moduleDecl);
}

GenericEnvironment *IRGenModule::getGenericEnvironment() {
  return Types.getGenericEnvironment();
}

/// Add a temporary forward declaration for a type.  This will live
/// only until a proper mapping is added.
void TypeConverter::addForwardDecl(TypeBase *key, llvm::Type *type) {
  assert(key->isCanonical());
  assert(!key->hasTypeParameter());
  assert(!Types.IndependentCache.count(key) && "entry already exists for type!");
  Types.IndependentCache.insert(std::make_pair(key, type));
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

const LoadableTypeInfo &TypeConverter::getTypeMetadataPtrTypeInfo() {
  if (TypeMetadataPtrTI) return *TypeMetadataPtrTI;
  TypeMetadataPtrTI =
    createUnmanagedStorageType(IGM.TypeMetadataPtrTy);
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

const TypeInfo &TypeConverter::getResilientStructTypeInfo() {
  if (ResilientStructTI) return *ResilientStructTI;
  ResilientStructTI = convertResilientStruct();
  ResilientStructTI->NextConverted = FirstType;
  FirstType = ResilientStructTI;
  return *ResilientStructTI;
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
  return getStoragePointerTypeForLowered(T.getSwiftRValueType());
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
  return getStorageTypeForLowered(T.getSwiftRValueType());
}

/// Get the storage type for the given type.  Note that, unlike
/// fetching the type info and asking it for the storage type, this
/// operation will succeed for forward-declarations.
llvm::Type *IRGenModule::getStorageTypeForLowered(CanType T) {
  // TODO: we can avoid creating entries for some obvious cases here.
  auto entry = Types.getTypeEntry(T);
  if (auto ti = entry.dyn_cast<const TypeInfo*>()) {
    return ti->getStorageType();
  } else {
    return entry.get<llvm::Type*>();
  }
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
  return getTypeInfoForLowered(T.getSwiftRValueType());
}

/// Get the fragile type information for the given type.
const TypeInfo &IRGenModule::getTypeInfoForLowered(CanType T) {
  return Types.getCompleteTypeInfo(T);
}

/// 
const TypeInfo &TypeConverter::getCompleteTypeInfo(CanType T) {
  auto entry = getTypeEntry(T);
  assert(entry.is<const TypeInfo*>() && "getting TypeInfo recursively!");
  auto &ti = *entry.get<const TypeInfo*>();
  assert(ti.isComplete());
  return ti;
}

const TypeInfo *TypeConverter::tryGetCompleteTypeInfo(CanType T) {
  auto entry = getTypeEntry(T);
  if (!entry.is<const TypeInfo*>()) return nullptr;
  auto &ti = *entry.get<const TypeInfo*>();
  if (!ti.isComplete()) return nullptr;
  return &ti;
}

ArchetypeType *TypeConverter::getExemplarArchetype(ArchetypeType *t) {
  // Retrieve the generic environment of the archetype.
  auto genericEnv = t->getGenericEnvironment();

  // If there is no generic environment, the archetype is an exemplar.
  if (!genericEnv) return t;

  // Dig out the canonical generic environment.
  auto genericSig = genericEnv->getGenericSignature();
  auto canGenericSig = genericSig->getCanonicalSignature();
  auto module = IGM.getSwiftModule();
  auto canGenericEnv = canGenericSig.getGenericEnvironment(*module);
  if (canGenericEnv == genericEnv) return t;

  // Map the archetype out of its own generic environment and into the
  // canonical generic environment.
  auto interfaceType = genericEnv->mapTypeOutOfContext(t);
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

TypeCacheEntry TypeConverter::getTypeEntry(CanType canonicalTy) {
  // Cache this entry in the dependent or independent cache appropriate to it.
  auto &Cache = Types.getCacheFor(canonicalTy.getPointer());

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
                  SILType::getPrimitiveAddressType(contextTy))
      .getSwiftRValueType();
  }
  
  // Fold archetypes to unique exemplars. Any archetype with the same
  // constraints is equivalent for type lowering purposes.
  CanType exemplarTy = getExemplarType(contextTy);
  assert(!exemplarTy->hasTypeParameter());
  
  // See whether we lowered a type equivalent to this one.
  if (exemplarTy != canonicalTy) {
    auto it = Types.IndependentCache.find(exemplarTy.getPointer());
    if (it != Types.IndependentCache.end()) {
      // Record the object under the original type.
      auto result = it->second;
      Cache[canonicalTy.getPointer()] = result;
      return result;
    }
  }

  // Convert the type.
  TypeCacheEntry convertedEntry = convertType(exemplarTy);
  auto convertedTI = convertedEntry.dyn_cast<const TypeInfo*>();

  // If that gives us a forward declaration (which can happen with
  // bound generic types), don't propagate that into the cache here,
  // because we won't know how to clear it later.
  if (!convertedTI) {
    return convertedEntry;
  }

  // Cache the entry under the original type and the exemplar type, so that
  // we can avoid relowering equivalent types.
  auto insertEntry = [&](TypeCacheEntry &entry) {
    assert(entry == TypeCacheEntry() ||
           (entry.is<llvm::Type*>() &&
            entry.get<llvm::Type*>() == convertedTI->getStorageType()));
    entry = convertedTI;
  };
  insertEntry(Cache[canonicalTy.getPointer()]);
  if (canonicalTy != exemplarTy)
    insertEntry(Types.IndependentCache[exemplarTy.getPointer()]);
  
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
    case BuiltinFloatType::IEEE80:
      return RetTy{ llvm::Type::getX86_FP80Ty(ctx), Size(16), Alignment(16) };
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

TypeCacheEntry TypeConverter::convertType(CanType ty) {
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
  case TypeKind::UnmanagedStorage:
    return convertUnmanagedStorageType(cast<UnmanagedStorageType>(ty));
  case TypeKind::UnownedStorage:
    return convertUnownedStorageType(cast<UnownedStorageType>(ty));
  case TypeKind::WeakStorage:
    return convertWeakStorageType(cast<WeakStorageType>(ty));
  case TypeKind::SILBlockStorage: {
    return convertBlockStorageType(cast<SILBlockStorageType>(ty));
  case TypeKind::SILBox:
    return convertBoxType(cast<SILBoxType>(ty));
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

/// Convert an [unowned] storage type.  The implementation here
/// depends on the underlying reference type.
const TypeInfo *
TypeConverter::convertUnownedStorageType(UnownedStorageType *refType) {
  // The type may be optional.
  CanType referent(refType->getReferentType());
  if (auto referentObj = referent.getAnyOptionalObjectType())
    referent = referentObj;
  assert(referent->allowsOwnership());
  auto &referentTI = cast<ReferenceTypeInfo>(getCompleteTypeInfo(referent));
  return referentTI.createUnownedStorageType(*this);
}

/// Convert an @unowned(unsafe) storage type.  The implementation here
/// depends on the underlying reference type.
const TypeInfo *
TypeConverter::convertUnmanagedStorageType(UnmanagedStorageType *refType) {
  // The type may be optional.
  CanType referent(refType->getReferentType());
  if (auto referentObj = referent.getAnyOptionalObjectType())
    referent = referentObj;
  assert(referent->allowsOwnership());
  auto &referentTI = cast<ReferenceTypeInfo>(getCompleteTypeInfo(referent));
  return referentTI.createUnmanagedStorageType(*this);
}

/// Convert a weak storage type.  The implementation here
/// depends on the underlying reference type.
const TypeInfo *
TypeConverter::convertWeakStorageType(WeakStorageType *refType) {
  CanType referent =
    CanType(refType->getReferentType()->getAnyOptionalObjectType());
  assert(referent->allowsOwnership());
  auto &referentTI = cast<ReferenceTypeInfo>(getCompleteTypeInfo(referent));
  return referentTI.createWeakStorageType(*this);
}

static void overwriteForwardDecl(llvm::DenseMap<TypeBase*, TypeCacheEntry> &cache,
                                 TypeBase *key, const TypeInfo *result) {
  assert(cache.count(key) && "no forward declaration?");
  assert(cache[key].is<llvm::Type*>() && "overwriting real entry!");
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
        if (elt->getArgumentInterfaceType() &&
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

TypeCacheEntry TypeConverter::convertAnyNominalType(CanType type,
                                                    NominalTypeDecl *decl) {
  // By "any", we don't mean existentials.
  assert(!isa<ProtocolDecl>(decl));

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
  auto &Cache = Types.IndependentCache;
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

const LoadableTypeInfo &
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
  // when the archetypes have the same exemplars.  Mangling the archetypes
  // in this case can be very misleading, so we just mangle the base name.
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

/// Compute the explosion schema for the given type.
ExplosionSchema IRGenModule::getSchema(SILType type) {
  ExplosionSchema schema;
  getSchema(type, schema);
  return schema;
}

/// Compute the explosion schema for the given type.
void IRGenModule::getSchema(SILType type, ExplosionSchema &schema) {
  // As an optimization, avoid actually building a TypeInfo for any
  // obvious TupleTypes.  This assumes that a TupleType's explosion
  // schema is always the concatenation of its component's schemas.
  if (CanTupleType tuple = type.getAs<TupleType>()) {
    for (auto index : indices(tuple.getElementTypes()))
      getSchema(type.getTupleElementType(index), schema);
    return;
  }

  // Okay, that didn't work;  just do the general thing.
  getTypeInfo(type).getSchema(schema);
}

/// Compute the explosion schema for the given type.
unsigned IRGenModule::getExplosionSize(SILType type) {
  // As an optimization, avoid actually building a TypeInfo for any
  // obvious TupleTypes.  This assumes that a TupleType's explosion
  // schema is always the concatenation of its component's schemas.
  if (auto tuple = type.getAs<TupleType>()) {
    unsigned count = 0;
    for (auto index : indices(tuple.getElementTypes()))
      count += getExplosionSize(type.getTupleElementType(index));
    return count;
  }

  // If the type isn't loadable, the explosion size is always 1.
  auto *loadableTI = dyn_cast<LoadableTypeInfo>(&getTypeInfo(type));
  if (!loadableTI) return 1;

  // Okay, that didn't work;  just do the general thing.
  return loadableTI->getExplosionSize();
}

/// Determine whether this type is a single value that is passed
/// indirectly at the given level.
llvm::PointerType *IRGenModule::isSingleIndirectValue(SILType type) {
  if (auto archetype = type.getAs<ArchetypeType>()) {
    if (!archetype->requiresClass())
      return OpaquePtrTy;
  }

  ExplosionSchema schema;
  getSchema(type, schema);
  if (schema.size() == 1 && schema.begin()->isAggregate())
    return schema.begin()->getAggregateType()->getPointerTo(0);
  return nullptr;
}

/// Determine whether this type is known to be POD.
bool IRGenModule::isPOD(SILType type, ResilienceExpansion expansion) {
  if (type.is<ArchetypeType>()) return false;
  if (type.is<ClassType>()) return false;
  if (type.is<BoundGenericClassType>()) return false;
  if (auto tuple = type.getAs<TupleType>()) {
    for (auto index : indices(tuple.getElementTypes()))
      if (!isPOD(type.getTupleElementType(index), expansion))
        return false;
    return true;
  }
  return getTypeInfo(type).isPOD(expansion);
}


SpareBitVector IRGenModule::getSpareBitsForType(llvm::Type *scalarTy, Size size) {
  auto it = SpareBitsForTypes.find(scalarTy);
  if (it != SpareBitsForTypes.end())
    return it->second;

  assert(DataLayout.getTypeAllocSizeInBits(scalarTy) <= size.getValueInBits() &&
         "using a size that's smaller than LLVM's alloc size?");
  
  {
    // FIXME: Currently we only implement spare bits for single-element
    // primitive integer types.
    while (auto structTy = dyn_cast<llvm::StructType>(scalarTy)) {
      if (structTy->getNumElements() != 1)
        goto no_spare_bits;
      scalarTy = structTy->getElementType(0);
    }

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

llvm::Value *IRGenFunction::getLocalSelfMetadata() {
  assert(LocalSelf && "no local self metadata");
  switch (SelfKind) {
  case SwiftMetatype:
    return LocalSelf;
  case ObjCMetatype:
    return emitObjCMetadataRefForMetadata(*this, LocalSelf);
  case ObjectReference:
    return emitDynamicTypeOfOpaqueHeapObject(*this, LocalSelf);
  }

  llvm_unreachable("Not a valid LocalSelfKind.");
}

void IRGenFunction::setLocalSelfMetadata(llvm::Value *value,
                                         IRGenFunction::LocalSelfKind kind) {
  assert(!LocalSelf && "already have local self metadata");
  LocalSelf = value;
  SelfKind = kind;
}

#ifndef NDEBUG
CanType TypeConverter::getTypeThatLoweredTo(llvm::Type *t) const {
  for (auto &mapping : Types.IndependentCache) {
    if (auto fwd = mapping.second.dyn_cast<llvm::Type*>())
      if (fwd == t)
        return CanType(mapping.first);
    if (auto *ti = mapping.second.dyn_cast<const TypeInfo *>())
      if (ti->getStorageType() == t)
        return CanType(mapping.first);
  }
  return CanType();
}

bool TypeConverter::isExemplarArchetype(ArchetypeType *arch) const {
  auto genericEnv = arch->getGenericEnvironment();
  if (!genericEnv) return true;

  // Dig out the canonical generic environment.
  auto genericSig = genericEnv->getGenericSignature();
  auto canGenericSig = genericSig->getCanonicalSignature();
  auto module = IGM.getSwiftModule();
  auto canGenericEnv = canGenericSig.getGenericEnvironment(*module);

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
        && (*theCase)->getArgumentInterfaceType())
      return t.getEnumElementType(*theCase, IGM.getSILModule());

    return SILType();
  }

  return SILType();
}
