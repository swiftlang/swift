//===--- GenArray.cpp - LLVM type lowering of fixed-size array types ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements TypeInfo subclasses for `Builtin.FixedArray`.
//
//===----------------------------------------------------------------------===//

#include "FixedTypeInfo.h"
#include "GenType.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"
#include "NonFixedTypeInfo.h"

using namespace swift;
using namespace irgen;

template<typename BaseTypeInfo, typename ElementTypeInfo = BaseTypeInfo>
class ArrayTypeInfoBase : public BaseTypeInfo {
protected:
  const ElementTypeInfo &Element;

  template<typename...Args>
  ArrayTypeInfoBase(const ElementTypeInfo &elementTI, Args &&...args)
    : BaseTypeInfo(std::forward<Args>(args)...),
      Element(elementTI)
  {}

  static SILType getElementSILType(IRGenModule &IGM,
                                   SILType arrayType) {
    return IGM.getLoweredType(AbstractionPattern::getOpaque(),
                  arrayType.castTo<BuiltinFixedArrayType>()->getElementType());
  }

  virtual llvm::Value *getArraySize(IRGenFunction &IGF, SILType T) const = 0;
  virtual std::optional<uint64_t> getFixedArraySize(SILType T) const = 0;

  void eachElementAddrLoop(IRGenFunction &IGF,
                           SILType T,
                           llvm::function_ref<void (ArrayRef<Address>)> body,
                           ArrayRef<Address> addrs) const {
    auto fixedSize = getFixedArraySize(T);
    if (fixedSize == 0) {
      // empty type, nothing to do
      return;
    }
    if (fixedSize == 1) {
      // only one element to operate on
      return body(addrs);
    }
    
    auto arraySize = getArraySize(IGF, T);
    auto predBB = IGF.Builder.GetInsertBlock();
    
    auto loopBB = IGF.createBasicBlock("each_array_element");
    auto endBB = IGF.createBasicBlock("end_array_element");

    auto one = llvm::ConstantInt::get(IGF.IGM.IntPtrTy, 1);
    auto zero = llvm::ConstantInt::get(IGF.IGM.IntPtrTy, 0);

    if (!fixedSize.has_value()) {
      // If the size isn't statically known, we have to dynamically check the
      // zero case.
      auto isEmptyArray = IGF.Builder.CreateICmpEQ(arraySize, zero);
      IGF.Builder.CreateCondBr(isEmptyArray, endBB, loopBB);
    } else {
      // Otherwise, we statically handled the zero case above.
      IGF.Builder.CreateBr(loopBB);
    }
    
    IGF.Builder.emitBlock(loopBB);

    auto countPhi = IGF.Builder.CreatePHI(IGF.IGM.IntPtrTy, 2);
    countPhi->addIncoming(arraySize, predBB);

    ConditionalDominanceScope scope(IGF);

    SmallVector<llvm::PHINode*, 2> addrPhis;
    SmallVector<Address, 2> eltAddrs;
    for (auto a : addrs) {
      auto *addrPhi = IGF.Builder.CreatePHI(a.getType(), 2);
      addrPhi->addIncoming(a.getAddress(), predBB);
      addrPhis.push_back(addrPhi);
      eltAddrs.push_back(Address(addrPhi, Element.getStorageType(),
                                 a.getAlignment()));
    }
    
    body(eltAddrs);

    // The just ran body may have generated new blocks. Get the current
    // insertion block which will become the other incoming block to the phis
    // we've generated.
    predBB = IGF.Builder.GetInsertBlock();

    for (unsigned i : indices(addrPhis)) {
      addrPhis[i]->addIncoming(Element.indexArray(IGF, eltAddrs[i], one,
                                                  getElementSILType(IGF.IGM, T))
                                      .getAddress(),
                               predBB);
    }

    auto nextCount = IGF.Builder.CreateSub(countPhi, one);
    countPhi->addIncoming(nextCount, predBB);
    
    auto done = IGF.Builder.CreateICmpEQ(nextCount, zero);
    IGF.Builder.CreateCondBr(done, endBB, loopBB);
    
    IGF.Builder.emitBlock(endBB);
  }

public:
  void assignWithCopy(IRGenFunction &IGF, Address dest, Address src,
                      SILType T, bool isOutlined) const override {
    auto eltTy = getElementSILType(IGF.IGM, T);
    eachElementAddrLoop(IGF, T,
                        [&](ArrayRef<Address> destAndSrc) {
                          Element.assignWithCopy(IGF, destAndSrc[0],
                                                 destAndSrc[1],
                                                 eltTy, isOutlined);
                        }, {dest, src});
  }

  void assignWithTake(IRGenFunction &IGF, Address dest, Address src,
                      SILType T, bool isOutlined) const override {
    auto eltTy = getElementSILType(IGF.IGM, T);
    eachElementAddrLoop(IGF, T,
                        [&](ArrayRef<Address> destAndSrc) {
                          Element.assignWithTake(IGF, destAndSrc[0],
                                                 destAndSrc[1],
                                                 eltTy, isOutlined);
                        }, {dest, src});
  }

  void initializeWithCopy(IRGenFunction &IGF, Address dest, Address src,
                          SILType T, bool isOutlined) const override {
    auto eltTy = getElementSILType(IGF.IGM, T);
    eachElementAddrLoop(IGF, T,
                        [&](ArrayRef<Address> destAndSrc) {
                          Element.initializeWithCopy(IGF, destAndSrc[0],
                                                     destAndSrc[1],
                                                     eltTy, isOutlined);
                        }, {dest, src});
  }
  

  void initializeWithTake(IRGenFunction &IGF, Address dest, Address src,
                          SILType T,
                          bool isOutlined,
                          bool zeroizeIfSensitive) const override {
    auto eltTy = getElementSILType(IGF.IGM, T);
    eachElementAddrLoop(IGF, T,
                        [&](ArrayRef<Address> destAndSrc) {
                          Element.initializeWithTake(IGF, destAndSrc[0],
                                                     destAndSrc[1],
                                                     eltTy, isOutlined,
                                                     zeroizeIfSensitive);
                        }, {dest, src});
  }
  
  virtual void destroy(IRGenFunction &IGF, Address address, SILType T,
                       bool isOutlined) const override {
    auto eltTy = getElementSILType(IGF.IGM, T);
    eachElementAddrLoop(IGF, T,
                        [&](ArrayRef<Address> elt) {
                          Element.destroy(IGF, elt[0], eltTy, isOutlined);
                        }, {address});
  }
};

template<typename BaseTypeInfo>
class FixedArrayTypeInfoBase : public ArrayTypeInfoBase<BaseTypeInfo> {
protected:
  using ArrayTypeInfoBase<BaseTypeInfo>::Element;
  const uint64_t ArraySize;

  using ArrayTypeInfoBase<BaseTypeInfo>::getElementSILType;

  template<typename...Args>
  FixedArrayTypeInfoBase(unsigned arraySize,
                         const BaseTypeInfo &elementTI, Args &&...args)
    : ArrayTypeInfoBase<BaseTypeInfo>(elementTI, std::forward<Args>(args)...),
      ArraySize(arraySize)
  {}

  static Size getArraySize(uint64_t arraySize,
                           const FixedTypeInfo &elementTI) {
    // We always pad out the stride, even for the final element.
    return Size(arraySize * elementTI.getFixedStride().getValue());
  }

  static llvm::Type *getArrayType(uint64_t arraySize,
                                  const FixedTypeInfo &elementTI) {
    // Start with the element's storage type.
    llvm::Type *elementTy = elementTI.getStorageType();
    auto &LLVMContext = elementTy->getContext();

    if (arraySize == 0) {
      return llvm::StructType::get(LLVMContext, {});
    }
    
    // If we need to, pad it to stride.
    if (elementTI.getFixedSize() < elementTI.getFixedStride()) {
      uint64_t paddingBytes = elementTI.getFixedStride().getValue()
        - elementTI.getFixedSize().getValue();
      auto byteTy = llvm::IntegerType::get(LLVMContext, 8);
      auto paddingArrayTy = llvm::ArrayType::get(byteTy, paddingBytes);

      if (elementTI.getFixedSize() == Size(0)) {
        elementTy = paddingArrayTy;
      } else {
        elementTy = llvm::StructType::get(LLVMContext,
          {elementTy, paddingArrayTy},
          /*packed*/ true);
      }
    }
    
    return llvm::ArrayType::get(elementTy, arraySize);
  }

  static SpareBitVector getArraySpareBits(uint64_t arraySize,
                                          const FixedTypeInfo &elementTI) {
    if (arraySize == 0) {
      return SpareBitVector();
    }
    
    // Take spare bits from the first element only.
    SpareBitVector result = elementTI.getSpareBits();
    // We can use the padding to the next element as spare bits too.
    result.appendSetBits(getArraySize(arraySize, elementTI).getValueInBits()
                           - result.size());
    return result;
  }
  
  void eachElement(llvm::function_ref<void()> body) const {
    for (uint64_t i = 0; i < ArraySize; ++i) {
      body();
    }
  }

  void eachElementAddr(IRGenFunction &IGF, Address addr,
                       llvm::function_ref<void(Address)> body) const {
    for (uint64_t i = 0; i < ArraySize; ++i) {
      auto elementAddr = Element.indexArray(IGF, addr,
        llvm::ConstantInt::get(IGF.IGM.IntPtrTy, i),
        SILType());
      
      body(elementAddr);
    }
  }
  
  std::optional<uint64_t> getFixedArraySize(SILType T) const override {
    return ArraySize;
  }
  
  llvm::Value *getArraySize(IRGenFunction &IGF, SILType T) const override {
    return llvm::ConstantInt::get(IGF.IGM.IntPtrTy, ArraySize);
  }

public:
  void getSchema(ExplosionSchema &schema) const override {
    eachElement([&]{
      Element.getSchema(schema);
    });
  }

  TypeLayoutEntry *
  buildTypeLayoutEntry(IRGenModule &IGM,
                       SILType T,
                       bool useStructLayouts) const override {
    auto eltTy = getElementSILType(IGM, T);
    auto elementLayout = Element.buildTypeLayoutEntry(IGM, eltTy,
                                                      useStructLayouts);
    return IGM.typeLayoutCache.getOrCreateArrayEntry(elementLayout, eltTy,
                                  T.castTo<BuiltinFixedArrayType>()->getSize());
  }

  void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                            Address src, SILType T,
                            bool isOutlined) const override {
    auto eltTy = getElementSILType(IGF.IGM, T);
    eachElementAddr(IGF, src,
                    [&](Address elementAddr) {
                      Element.initializeFromParams(IGF, params, elementAddr,
                                                   eltTy, isOutlined);
                    });
  }

  // We take extra inhabitants from the first element, if any.
  unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const override {
    if (ArraySize == 0)
      return 0;
    return Element.getFixedExtraInhabitantCount(IGM);
  }

  APInt getFixedExtraInhabitantMask(IRGenModule &IGM) const override {
    if (ArraySize == 0)
      return APInt::getAllOnes(0);
    APInt elementMask = Element.getFixedExtraInhabitantMask(IGM);
    return elementMask.zext(this->getFixedSize().getValueInBits());
  }

  /// Create a constant of the given bit width holding one of the extra
  /// inhabitants of the type.
  /// The index must be less than the value returned by
  /// getFixedExtraInhabitantCount().
  APInt getFixedExtraInhabitantValue(IRGenModule &IGM,
                                     unsigned bits,
                                     unsigned index) const override {
    return Element.getFixedExtraInhabitantValue(IGM, bits, index);
  }
    
  llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                       Address src, SILType T,
                                       bool isOutlined) const override {
    if (ArraySize == 0)
      return llvm::ConstantInt::get(IGF.IGM.Int32Ty, -1);
      
    auto firstElementAddr
      = IGF.Builder.CreateElementBitCast(src, Element.getStorageType());

    return Element.getExtraInhabitantIndex(IGF, firstElementAddr,
                                           getElementSILType(IGF.IGM, T),
                                           isOutlined);
  }
    
  void storeExtraInhabitant(IRGenFunction &IGF,
                            llvm::Value *index,
                            Address dest, SILType T,
                            bool isOutlined) const override {
    auto firstElementAddr
      = IGF.Builder.CreateElementBitCast(dest, Element.getStorageType());

    Element.storeExtraInhabitant(IGF, index, firstElementAddr,
                                 getElementSILType(IGF.IGM, T),
                                 isOutlined);
  }
};

class LoadableArrayTypeInfo final
  : public FixedArrayTypeInfoBase<LoadableTypeInfo>
{
public:
  LoadableArrayTypeInfo(uint64_t arraySize,
                        const LoadableTypeInfo &elementTI)
    : FixedArrayTypeInfoBase(arraySize, elementTI,
                       getArrayType(arraySize, elementTI),
                       getArraySize(arraySize, elementTI),
                       getArraySpareBits(arraySize, elementTI),
                       elementTI.getFixedAlignment(),
                       elementTI.isTriviallyDestroyable(ResilienceExpansion::Maximal),
                       elementTI.isCopyable(ResilienceExpansion::Maximal),
                       elementTI.isFixedSize(ResilienceExpansion::Minimal),
                       elementTI.isABIAccessible())
  {
  }
  
  unsigned getExplosionSize() const override {
    return Element.getExplosionSize() * ArraySize;
  }

  void loadAsCopy(IRGenFunction &IGF, Address addr,
                  Explosion &explosion) const override {
    eachElementAddr(IGF, addr,
                    [&](Address elementAddr) {
                      Element.loadAsCopy(IGF, elementAddr, explosion);
                    });
  }

  void loadAsTake(IRGenFunction &IGF, Address addr,
                  Explosion &explosion) const override {
    eachElementAddr(IGF, addr,
                    [&](Address elementAddr) {
                      Element.loadAsTake(IGF, elementAddr, explosion);
                    });
  }
  
  void assign(IRGenFunction &IGF, Explosion &explosion, Address addr,
              bool isOutlined, SILType T) const override {
    auto eltTy = getElementSILType(IGF.IGM, T);
    eachElementAddr(IGF, addr,
                    [&](Address elementAddr) {
                      Element.assign(IGF, explosion, elementAddr, isOutlined,
                                     eltTy);
                    });
  }

  void initialize(IRGenFunction &IGF, Explosion &explosion, Address addr,
                  bool isOutlined) const override {
    eachElementAddr(IGF, addr,
                    [&](Address elementAddr) {
                      Element.initialize(IGF, explosion, elementAddr, isOutlined);
                    });
  }
  
  void reexplode(Explosion &sourceExplosion,
                 Explosion &targetExplosion) const override {
    eachElement([&]{
      Element.reexplode(sourceExplosion, targetExplosion);
    });
  }
  
  void copy(IRGenFunction &IGF,
            Explosion &sourceExplosion,
            Explosion &targetExplosion,
            Atomicity atomicity) const override {
    eachElement([&]{
      Element.copy(IGF, sourceExplosion, targetExplosion, atomicity);
    });
  }
  
  void consume(IRGenFunction &IGF, Explosion &explosion,
               Atomicity atomicity,
               SILType T) const override {
    auto eltTy = getElementSILType(IGF.IGM, T);
    eachElement([&]{
      Element.consume(IGF, explosion, atomicity, eltTy);
    });
  }

  void fixLifetime(IRGenFunction &IGF,
                   Explosion &explosion) const override {
    eachElement([&]{
      Element.fixLifetime(IGF, explosion);
    });
  }

  template<typename Body>
  void eachElementOffset(Body &&body) const {
    for (unsigned i = 0; i < ArraySize; ++i) {
      body(i * Element.getFixedStride().getValue());
    }
  }
  
  void packIntoEnumPayload(IRGenModule &IGM,
                           IRBuilder &builder,
                           EnumPayload &payload,
                           Explosion &sourceExplosion,
                           unsigned offset) const override {
    eachElementOffset([&](unsigned eltByteOffset){
      Element.packIntoEnumPayload(IGM, builder, payload,
                                  sourceExplosion,
                                  offset + eltByteOffset * 8);
    });
  }
  
  void unpackFromEnumPayload(IRGenFunction &IGF,
                             const EnumPayload &payload,
                             Explosion &targetExplosion,
                             unsigned offset) const override {
    eachElementOffset([&](unsigned eltByteOffset){
      Element.unpackFromEnumPayload(IGF, payload,
                                    targetExplosion,
                                    offset + eltByteOffset * 8);
    });
  }
  
  void addToAggLowering(IRGenModule &IGM, SwiftAggLowering &lowering,
                        Size offset) const override {
    eachElementOffset([&](unsigned eltByteOffset){
      Element.addToAggLowering(IGM, lowering,
                               Size(offset.getValue() + eltByteOffset));
    });
  }
};

class FixedArrayTypeInfo final
  : public FixedArrayTypeInfoBase<FixedTypeInfo>
{
public:
  FixedArrayTypeInfo(uint64_t arraySize,
                     const FixedTypeInfo &elementTI)
    : FixedArrayTypeInfoBase(arraySize, elementTI,
                       getArrayType(arraySize, elementTI),
                       getArraySize(arraySize, elementTI),
                       getArraySpareBits(arraySize, elementTI),
                       elementTI.getFixedAlignment(),
                       elementTI.isTriviallyDestroyable(ResilienceExpansion::Maximal),
                       elementTI.getBitwiseTakable(ResilienceExpansion::Maximal),
                       elementTI.isCopyable(ResilienceExpansion::Maximal),
                       elementTI.isFixedSize(ResilienceExpansion::Minimal),
                       elementTI.isABIAccessible())
  {
  }
};

// NOTE: This does not simply use WitnessSizedTypeInfo in order to avoid
// dependency on a Swift runtime for handling fixed-size arrays that are
// unspecialized in their size parameter only, so that embedded Swift can
// work with unspecialized integer parameters.
class NonFixedArrayTypeInfo final
  : public ArrayTypeInfoBase<IndirectTypeInfo<NonFixedArrayTypeInfo, TypeInfo>,
                             TypeInfo> {
  using super = ArrayTypeInfoBase<IndirectTypeInfo<NonFixedArrayTypeInfo, TypeInfo>,
                                  TypeInfo>;
  
  llvm::Value *getArraySize(IRGenFunction &IGF, SILType T) const override {
    if (auto fixedSize = getFixedArraySize(T)) {
      return llvm::ConstantInt::get(IGF.IGM.IntPtrTy, *fixedSize);
    }
    
    CanType sizeParam = T.castTo<BuiltinFixedArrayType>()->getSize();
    
    auto arg = IGF.emitValueGenericRef(sizeParam);
    auto zero = llvm::ConstantInt::get(IGF.IGM.IntPtrTy, 0);
    auto isNegative = IGF.Builder.CreateICmpSLT(arg, zero);
    return IGF.Builder.CreateSelect(isNegative, zero, arg);
  }
  
  std::optional<uint64_t> getFixedArraySize(SILType T) const override {
    CanType sizeParam = T.castTo<BuiltinFixedArrayType>()->getSize();
    
    if (auto integer = sizeParam->getAs<IntegerType>()) {
      if (integer->getValue().isNonNegative()) {
        return integer->getValue().getLimitedValue();
      }
    }
    return std::nullopt;
  }
  
public:
  NonFixedArrayTypeInfo(llvm::Type *opaqueTy,
                        const TypeInfo &Element)
    : super(Element,
        opaqueTy, Element.getBestKnownAlignment(),
        Element.isTriviallyDestroyable(ResilienceExpansion::Maximal),
        Element.getBitwiseTakable(ResilienceExpansion::Maximal),
        Element.isCopyable(ResilienceExpansion::Maximal),
        IsNotFixedSize,
        Element.isABIAccessible(),
        SpecialTypeInfoKind::None)
  {}

  llvm::Value *getSize(IRGenFunction &IGF, SILType T) const override {
    auto elementStride
      = Element.getStride(IGF, getElementSILType(IGF.IGM, T));
    return IGF.Builder.CreateMul(elementStride, getArraySize(IGF, T));
  }
  llvm::Value *getAlignmentMask(IRGenFunction &IGF,
                                SILType T) const override {
    return Element.getAlignmentMask(IGF, getElementSILType(IGF.IGM, T));
  }
  llvm::Value *getStride(IRGenFunction &IGF, SILType T) const override {
    return getSize(IGF, T);
  }
  llvm::Value *getIsTriviallyDestroyable(IRGenFunction &IGF,
                                         SILType T) const override {
    return Element.getIsTriviallyDestroyable(IGF,
                                             getElementSILType(IGF.IGM, T));
  }
  llvm::Value *getIsBitwiseTakable(IRGenFunction &IGF,
                                   SILType T) const override {
    return Element.getIsBitwiseTakable(IGF,
                                       getElementSILType(IGF.IGM, T));
  }
  llvm::Value *isDynamicallyPackedInline(IRGenFunction &IGF,
                                         SILType T) const override {
    auto startBB = IGF.Builder.GetInsertBlock();
    auto no = llvm::ConstantInt::getBool(IGF.IGM.getLLVMContext(),
                                         false);
    // Prefetch the necessary info from the element type info. 
    auto isBT = getIsBitwiseTakable(IGF, T);
    auto size = getSize(IGF, T);
    auto align = getAlignmentMask(IGF, T);
    
    auto endBB = IGF.createBasicBlock("array_is_packed_inline");
    IGF.Builder.SetInsertPoint(endBB);
    auto result = IGF.Builder.CreatePHI(IGF.IGM.Int1Ty, 3);
    IGF.Builder.SetInsertPoint(startBB);
    
    // packed inline if the payload is bitwise-takable...
    auto isBT_BB = IGF.createBasicBlock("array_is_bt");
    IGF.Builder.CreateCondBr(isBT, isBT_BB, endBB);
    result->addIncoming(no, startBB);
    
    IGF.Builder.emitBlock(isBT_BB);
    
    // ...size fits the fixed-size buffer...
    auto bufferSize = llvm::ConstantInt::get(IGF.IGM.IntPtrTy,
                                        getFixedBufferSize(IGF.IGM).getValue());
    auto sizeFits = IGF.Builder.CreateICmpULE(size, bufferSize);
    auto sizeFitsBB = IGF.createBasicBlock("array_size_fits");
    IGF.Builder.CreateCondBr(sizeFits, sizeFitsBB, endBB);
    result->addIncoming(no, isBT_BB);
    
    IGF.Builder.emitBlock(sizeFitsBB);

    // ...and so does alignment
    auto bufferAlign = llvm::ConstantInt::get(IGF.IGM.IntPtrTy,
                               getFixedBufferAlignment(IGF.IGM).getMaskValue());
    auto alignFits =  IGF.Builder.CreateICmpULE(align, bufferAlign);
    IGF.Builder.CreateBr(endBB);
    result->addIncoming(alignFits, sizeFitsBB);

    IGF.Builder.emitBlock(endBB);
    return result;
  }

  bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
    return Element.mayHaveExtraInhabitants(IGM);
  }

  llvm::Constant *getStaticSize(IRGenModule &IGM) const override {
    return nullptr;
  }
  llvm::Constant *getStaticAlignmentMask(IRGenModule &IGM) const override {
    return nullptr;
  }
  llvm::Constant *getStaticStride(IRGenModule &IGM) const override {
    return nullptr;
  }

  StackAddress allocateStack(IRGenFunction &IGF, SILType T,
                             const llvm::Twine &name) const override {
    // Allocate memory on the stack.
    auto alloca = IGF.emitDynamicAlloca(T, name);
    IGF.Builder.CreateLifetimeStart(alloca.getAddressPointer());
    return alloca.withAddress(getAddressForPointer(alloca.getAddressPointer()));
  }

  void deallocateStack(IRGenFunction &IGF, StackAddress stackAddress,
                       SILType T) const override {
    IGF.Builder.CreateLifetimeEnd(stackAddress.getAddress().getAddress());
    IGF.emitDeallocateDynamicAlloca(stackAddress);
  }

  void destroyStack(IRGenFunction &IGF, StackAddress stackAddress, SILType T,
                    bool isOutlined) const override {
    emitDestroyCall(IGF, T, stackAddress.getAddress());
    deallocateStack(IGF, stackAddress, T);
  }

  TypeLayoutEntry *
  buildTypeLayoutEntry(IRGenModule &IGM,
                       SILType T,
                       bool useStructLayouts) const override {
    return IGM.typeLayoutCache.getOrCreateResilientEntry(T);
  }

  llvm::Value *getEnumTagSinglePayload(IRGenFunction &IGF,
                                       llvm::Value *numEmptyCases,
                                       Address arrayAddr,
                                       SILType arrayType,
                                       bool isOutlined) const override {
    // take extra inhabitants from the first element
    auto firstElementAddr
      = IGF.Builder.CreateElementBitCast(arrayAddr, Element.getStorageType());
    return Element.getEnumTagSinglePayload(IGF,
                                           numEmptyCases,
                                           firstElementAddr,
                                           getElementSILType(IGF.IGM, arrayType),
                                           isOutlined);
  }

  void storeEnumTagSinglePayload(IRGenFunction &IGF,
                                 llvm::Value *index,
                                 llvm::Value *numEmptyCases,
                                 Address arrayAddr,
                                 SILType arrayType,
                                 bool isOutlined) const override {
    // take extra inhabitants from the first element
    auto firstElementAddr
      = IGF.Builder.CreateElementBitCast(arrayAddr, Element.getStorageType());
    return Element.storeEnumTagSinglePayload(IGF,
                                          index,
                                          numEmptyCases,
                                          firstElementAddr,
                                          getElementSILType(IGF.IGM, arrayType),
                                          isOutlined);
  }
};

const TypeInfo *
TypeConverter::convertBuiltinFixedArrayType(BuiltinFixedArrayType *T) {
  // Most of our layout properties come from the element type.
  auto &elementTI = IGM.getTypeInfoForUnlowered(AbstractionPattern::getOpaque(),
                                                T->getElementType());
  // ...unless the array size is not fixed, then the array layout is never
  // fixed.
  auto fixedSize = T->getFixedInhabitedSize();
  
  // Statically zero or negative-sized array types are empty.
  if (fixedSize == 0 || T->isFixedNegativeSize()) {
    return &getEmptyTypeInfo();
  }
                                  
  if (!fixedSize.has_value() || !elementTI.isFixedSize()) {
    return new NonFixedArrayTypeInfo(IGM.OpaqueTy, elementTI);
  }
  
  if (*fixedSize <= BuiltinFixedArrayType::MaximumLoadableSize) {
    if (auto *loadableTI = dyn_cast<LoadableTypeInfo>(&elementTI)) {
      return new LoadableArrayTypeInfo(fixedSize.value(), *loadableTI);
    }
  }
  
  return new FixedArrayTypeInfo(fixedSize.value(),
                                *cast<FixedTypeInfo>(&elementTI));
}
