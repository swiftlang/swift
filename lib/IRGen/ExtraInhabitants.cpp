//===--- ExtraInhabitants.cpp - Routines for extra inhabitants ------------===//
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
//  This file implements routines for working with extra inhabitants.
//
//===----------------------------------------------------------------------===//

#include "ExtraInhabitants.h"

#include "IRGenModule.h"
#include "IRGenFunction.h"
#include "SwiftTargetInfo.h"

using namespace swift;
using namespace irgen;

static unsigned getNumLowObjCReservedBits(IRGenModule &IGM) {
  // Get the index of the first non-reserved bit.
  SpareBitVector ObjCMask = IGM.TargetInfo.ObjCPointerReservedBits;
  ObjCMask.flipAll();
  return ObjCMask.enumerateSetBits().findNext().getValue();
}

/*****************************************************************************/

/// Return the number of extra inhabitants for a pointer that reserves
/// the given number of low bits.
static unsigned getPointerExtraInhabitantCount(IRGenModule &IGM,
                                               unsigned numReservedLowBits) {  
  // FIXME: We could also make extra inhabitants using spare bits, but we
  // probably don't need to.
  uint64_t rawCount =
    IGM.TargetInfo.LeastValidPointerValue >> numReservedLowBits;
  
  // The runtime limits the count to INT_MAX.
  return std::min((uint64_t)INT_MAX, rawCount);
}

unsigned irgen::getHeapObjectExtraInhabitantCount(IRGenModule &IGM) {
  // This must be consistent with the extra inhabitant count produced
  // by the runtime's getHeapObjectExtraInhabitantCount function in
  // KnownMetadata.cpp.
  return getPointerExtraInhabitantCount(IGM, getNumLowObjCReservedBits(IGM));
}

unsigned irgen::getFunctionPointerExtraInhabitantCount(IRGenModule &IGM) {
  return getPointerExtraInhabitantCount(IGM, 0);
}

/*****************************************************************************/

static APInt
getPointerFixedExtraInhabitantValue(IRGenModule &IGM, unsigned bits,
                                    unsigned index, unsigned offset,
                                    unsigned numReservedLowBits) {
  assert(index < getPointerExtraInhabitantCount(IGM, numReservedLowBits) &&
         "pointer extra inhabitant out of bounds");
  uint64_t value = (uint64_t)index << numReservedLowBits;
  APInt apValue(bits, value);
  if (offset > 0)
    apValue = apValue.shl(offset);
  
  return apValue;
}

APInt irgen::getHeapObjectFixedExtraInhabitantValue(IRGenModule &IGM,
                                                    unsigned bits,
                                                    unsigned index,
                                                    unsigned offset) {
  // This must be consistent with the extra inhabitant calculation implemented
  // in the runtime's storeHeapObjectExtraInhabitant and
  // getHeapObjectExtraInhabitantIndex functions in KnownMetadata.cpp.
  return getPointerFixedExtraInhabitantValue(IGM, bits, index, offset,
                                             getNumLowObjCReservedBits(IGM));
}

APInt irgen::getFunctionPointerFixedExtraInhabitantValue(IRGenModule &IGM,
                                                         unsigned bits,
                                                         unsigned index,
                                                         unsigned offset) {
  return getPointerFixedExtraInhabitantValue(IGM, bits, index, offset, 0);
}

/*****************************************************************************/

static llvm::Value *getPointerExtraInhabitantIndex(IRGenFunction &IGF,
                                                   Address src,
                                                   unsigned numReservedLowBits) {
  llvm::BasicBlock *contBB = IGF.createBasicBlock("is-valid-pointer");
  SmallVector<std::pair<llvm::BasicBlock*, llvm::Value*>, 3> phiValues;
  auto invalidIndex = llvm::ConstantInt::getSigned(IGF.IGM.Int32Ty, -1);
  
  src = IGF.Builder.CreateBitCast(src, IGF.IGM.SizeTy->getPointerTo());

  // Check if the inhabitant is below the least valid pointer value.
  llvm::Value *val = IGF.Builder.CreateLoad(src);
  {
    llvm::Value *leastValid = llvm::ConstantInt::get(IGF.IGM.SizeTy,
                                     IGF.IGM.TargetInfo.LeastValidPointerValue);
    llvm::Value *isValid = IGF.Builder.CreateICmpUGE(val, leastValid);

    phiValues.push_back({IGF.Builder.GetInsertBlock(), invalidIndex});
    llvm::BasicBlock *invalidBB = IGF.createBasicBlock("is-invalid-pointer");
    IGF.Builder.CreateCondBr(isValid, contBB, invalidBB);
    IGF.Builder.emitBlock(invalidBB);
  }
                       
  // Check if the inhabitant has any reserved low bits set.
  // FIXME: This check is unneeded if the type is known to be pure Swift.
  if (numReservedLowBits) {
    auto objcMask =
      llvm::ConstantInt::get(IGF.IGM.SizeTy, (1 << numReservedLowBits) - 1);
    llvm::Value *masked = IGF.Builder.CreateAnd(val, objcMask);
    llvm::Value *maskedZero = IGF.Builder.CreateICmpEQ(masked,
                                     llvm::ConstantInt::get(IGF.IGM.SizeTy, 0));

    phiValues.push_back({IGF.Builder.GetInsertBlock(), invalidIndex});
    llvm::BasicBlock *untaggedBB = IGF.createBasicBlock("is-untagged-pointer");
    IGF.Builder.CreateCondBr(maskedZero, untaggedBB, contBB);
    IGF.Builder.emitBlock(untaggedBB);
  }
  
  // The inhabitant is an invalid pointer. Derive its extra inhabitant index.
  {
    llvm::Value *index = val;

    // Shift away the reserved bits.
    if (numReservedLowBits) {
      index = IGF.Builder.CreateLShr(index, 
                    llvm::ConstantInt::get(IGF.IGM.SizeTy, numReservedLowBits));
    }

    // Truncate down to i32 if necessary.
    if (index->getType() != IGF.IGM.Int32Ty) {
      index = IGF.Builder.CreateTrunc(index, IGF.IGM.Int32Ty);
    }

    phiValues.push_back({IGF.Builder.GetInsertBlock(), index});
    IGF.Builder.CreateBr(contBB);
    IGF.Builder.emitBlock(contBB);
  }

  // Build the result phi.
  auto phi = IGF.Builder.CreatePHI(IGF.IGM.Int32Ty, phiValues.size());
  for (auto &entry : phiValues) {
    phi->addIncoming(entry.second, entry.first);
  }  
  return phi;
}

llvm::Value *irgen::getHeapObjectExtraInhabitantIndex(IRGenFunction &IGF,
                                                      Address src) {
  // This must be consistent with the extra inhabitant calculation implemented
  // in the runtime's getHeapObjectExtraInhabitantIndex function in
  // KnownMetadata.cpp.
  return getPointerExtraInhabitantIndex(IGF, src,
                                        getNumLowObjCReservedBits(IGF.IGM));
}

llvm::Value *irgen::getFunctionPointerExtraInhabitantIndex(IRGenFunction &IGF,
                                                           Address src) {
  return getPointerExtraInhabitantIndex(IGF, src, 0);
}

/*****************************************************************************/

static void storePointerExtraInhabitant(IRGenFunction &IGF,
                                        llvm::Value *index,
                                        Address dest,
                                        unsigned numReservedLowBits) {
  if (index->getType() != IGF.IGM.SizeTy) {
    index = IGF.Builder.CreateZExt(index, IGF.IGM.SizeTy);
  }

  if (numReservedLowBits) {
    index = IGF.Builder.CreateShl(index,
                  llvm::ConstantInt::get(IGF.IGM.SizeTy, numReservedLowBits));
  }

  dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.SizeTy->getPointerTo());
  IGF.Builder.CreateStore(index, dest);
}

void irgen::storeHeapObjectExtraInhabitant(IRGenFunction &IGF,
                                           llvm::Value *index,
                                           Address dest) {
  // This must be consistent with the extra inhabitant calculation implemented
  // in the runtime's storeHeapObjectExtraInhabitant function in
  // KnownMetadata.cpp.
  storePointerExtraInhabitant(IGF, index, dest,
                              getNumLowObjCReservedBits(IGF.IGM));
}

void irgen::storeFunctionPointerExtraInhabitant(IRGenFunction &IGF,
                                                llvm::Value *index,
                                                Address dest) {
  storePointerExtraInhabitant(IGF, index, dest, 0);
}
