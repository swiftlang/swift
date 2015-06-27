//===--- UnimplementedTypeInfo.cpp - Unimplemented Type Lowering ----------===//
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
//  This file implements a stub TypeInfo for unimplemented types.
//
//===----------------------------------------------------------------------===//

#include "IRGenModule.h"
#include "IRGenFunction.h"
#include "UnimplementedTypeInfo.h"
#include "swift/SIL/SILType.h"

using namespace swift;
using namespace irgen;

UnimplementedTypeInfo::UnimplementedTypeInfo(IRGenModule &IGM,
                                             llvm::Type *storageTy)
  : TypeInfo(storageTy,
             Alignment(1),
             IsNotPOD,
             IsNotBitwiseTakable,
             STIK_Unimplemented)
{}

static llvm::Constant *
getUndefSize(IRGenModule &IGM) {
  return llvm::UndefValue::get(IGM.SizeTy);
}

static llvm::Constant *
getUndefOpaqueAddressValue(llvm::Type *storageTy) {
  return llvm::UndefValue::get(storageTy->getPointerTo());
}

static Address
getUndefOpaqueAddress(llvm::Type *storageTy) {
  return Address(getUndefOpaqueAddressValue(storageTy), Alignment(1));
}

std::pair<llvm::Value*,llvm::Value*>
UnimplementedTypeInfo::getSizeAndAlignmentMask(IRGenFunction &IGF, SILType T)
const
{
  llvm::Value *undef = getUndefSize(IGF.IGM);
  return {undef, undef};
}

std::tuple<llvm::Value*,llvm::Value*,llvm::Value*>
UnimplementedTypeInfo::getSizeAndAlignmentMaskAndStride(IRGenFunction &IGF,
                                                        SILType T) const {
  llvm::Value *undef = getUndefSize(IGF.IGM);
  return std::make_tuple(undef, undef, undef);
}

llvm::Value *UnimplementedTypeInfo::getSize(IRGenFunction &IGF, SILType T)
const {
  return getUndefSize(IGF.IGM);
}
llvm::Value *UnimplementedTypeInfo::getAlignmentMask(IRGenFunction &IGF,
                                                     SILType T) const {
  return getUndefSize(IGF.IGM);
}
llvm::Value *UnimplementedTypeInfo::getStride(IRGenFunction &IGF, SILType T)
const {
  return getUndefSize(IGF.IGM);
}
llvm::Constant *UnimplementedTypeInfo::getStaticSize(IRGenModule &IGM) const {
  return nullptr;
}
llvm::Constant *UnimplementedTypeInfo::getStaticAlignmentMask(IRGenModule &IGM)
const {
  return nullptr;
}
llvm::Constant *UnimplementedTypeInfo::getStaticStride(IRGenModule &IGM)
const {
  return nullptr;
}

void UnimplementedTypeInfo::getSchema(ExplosionSchema &schema) const {
}

ContainedAddress UnimplementedTypeInfo::allocateStack(IRGenFunction &IGF,
                                                      SILType T,
                                                      const llvm::Twine &name)
const {
  return ContainedAddress(Address(llvm::UndefValue::get(IGF.IGM.getFixedBufferTy()),
                                  IGF.IGM.getPointerAlignment()),
                          getUndefOpaqueAddress(getStorageType()));
}

void UnimplementedTypeInfo::deallocateStack(IRGenFunction &IGF, Address addr,
                                            SILType T) const {
  
}

void UnimplementedTypeInfo::assignWithCopy(IRGenFunction &IGF, Address dest,
                                           Address src, SILType T) const {
  
}

void UnimplementedTypeInfo::assignWithTake(IRGenFunction &IGF, Address dest,
                                           Address src, SILType T) const {
  
}

void UnimplementedTypeInfo::initializeWithTake(IRGenFunction &IGF, Address dest,
                                               Address src, SILType T) const {
  
}

void UnimplementedTypeInfo::initializeWithCopy(IRGenFunction &IGF, Address dest,
                                               Address src, SILType T) const {
  
}

void UnimplementedTypeInfo::initializeFromParams(IRGenFunction &IGF,
                                                 Explosion &params,
                                                 Address src, SILType T) const {
  
}

void UnimplementedTypeInfo::destroy(IRGenFunction &IGF, Address address,
                                    SILType T) const {
  
}

bool UnimplementedTypeInfo::mayHaveExtraInhabitants(IRGenModule &IGM) const {
  return false;
}

llvm::Value *UnimplementedTypeInfo::getExtraInhabitantIndex(IRGenFunction &IGF,
                                                            Address src,
                                                            SILType T) const {
  return llvm::UndefValue::get(IGF.IGM.Int32Ty);
}

void UnimplementedTypeInfo::storeExtraInhabitant(IRGenFunction &IGF,
                                                 llvm::Value *index,
                                                 Address dest,
                                                 SILType T) const {
  
}
void UnimplementedTypeInfo::initializeMetadata(IRGenFunction &IGF,
                                               llvm::Value *metadata,
                                               llvm::Value *vwtable,
                                               SILType T) const {
  
}

llvm::Value *UnimplementedTypeInfo::isDynamicallyPackedInline(IRGenFunction &IGF,
                                                              SILType T) const {
  return llvm::UndefValue::get(IGF.IGM.Int1Ty);
}
