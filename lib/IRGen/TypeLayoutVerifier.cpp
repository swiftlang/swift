//===--- TypeLayoutVerifier.cpp -------------------------------------------===//
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
// This file defines a generator that produces code to verify that IRGen's
// static assumptions about data layout for a Swift type correspond to the
// runtime's understanding of data layout.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "swift/AST/Types.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "GenOpaque.h"
#include "GenType.h"
#include "FixedTypeInfo.h"

using namespace swift;
using namespace irgen;

void
irgen::emitTypeLayoutVerifier(IRGenFunction &IGF,
                              ArrayRef<CanType> formalTypes) {
  llvm::Type *verifierArgTys[] = {
    IGF.IGM.TypeMetadataPtrTy,
    IGF.IGM.Int8PtrTy,
    IGF.IGM.Int8PtrTy,
    IGF.IGM.SizeTy,
    IGF.IGM.Int8PtrTy,
  };
  auto verifierFnTy = llvm::FunctionType::get(IGF.IGM.VoidTy,
                                              verifierArgTys,
                                              /*var arg*/ false);
  auto verifierFn = IGF.IGM.Module.getOrInsertFunction(
                                       "_swift_debug_verifyTypeLayoutAttribute",
                                       verifierFnTy);
  struct VerifierArgumentBuffers {
    Address runtimeBuf, staticBuf;
  };
  llvm::DenseMap<llvm::Type *, VerifierArgumentBuffers>
    verifierArgBufs;
  
  auto getSizeConstant = [&](Size sz) -> llvm::Constant * {
    return llvm::ConstantInt::get(IGF.IGM.SizeTy, sz.getValue());
  };
  auto getAlignmentMaskConstant = [&](Alignment a) -> llvm::Constant * {
    return llvm::ConstantInt::get(IGF.IGM.SizeTy, a.getValue() - 1);
  };
  auto getBoolConstant = [&](bool b) -> llvm::Constant * {
    return llvm::ConstantInt::get(IGF.IGM.Int1Ty, b);
  };

  SmallString<20> numberBuf;

  for (auto formalType : formalTypes) {
    // Runtime type metadata always represents the maximal abstraction level of
    // the type.
    auto anyTy = ProtocolCompositionType::get(IGF.IGM.Context, {});
    auto openedAnyTy = ArchetypeType::getOpened(anyTy);
    auto maxAbstraction = AbstractionPattern(openedAnyTy);
    auto &ti = IGF.getTypeInfoForUnlowered(maxAbstraction, formalType);
    
    // If there's no fixed type info, we rely on the runtime anyway, so there's
    // nothing to verify.
    // TODO: There are some traits of partially-fixed layouts we could check too.
    auto *fixedTI = dyn_cast<FixedTypeInfo>(&ti);
    if (!fixedTI)
      return;
    
    auto metadata = IGF.emitTypeMetadataRef(formalType);
    
    auto verify = [&](llvm::Value *runtimeVal,
                      llvm::Value *staticVal,
                      const llvm::Twine &description) {
      assert(runtimeVal->getType() == staticVal->getType());
      // Get or create buffers for the arguments.
      VerifierArgumentBuffers bufs;
      auto foundBufs = verifierArgBufs.find(runtimeVal->getType());
      if (foundBufs != verifierArgBufs.end()) {
        bufs = foundBufs->second;
      } else {
        Address runtimeBuf = IGF.createAlloca(runtimeVal->getType(),
                                              IGF.IGM.getPointerAlignment(),
                                              "runtime");
        Address staticBuf = IGF.createAlloca(staticVal->getType(),
                                             IGF.IGM.getPointerAlignment(),
                                             "static");
        bufs = {runtimeBuf, staticBuf};
        verifierArgBufs[runtimeVal->getType()] = bufs;
      }
      
      IGF.Builder.CreateStore(runtimeVal, bufs.runtimeBuf);
      IGF.Builder.CreateStore(staticVal, bufs.staticBuf);
      
      auto runtimePtr = IGF.Builder.CreateBitCast(bufs.runtimeBuf.getAddress(),
                                                  IGF.IGM.Int8PtrTy);
      auto staticPtr = IGF.Builder.CreateBitCast(bufs.staticBuf.getAddress(),
                                                 IGF.IGM.Int8PtrTy);
      auto count = llvm::ConstantInt::get(IGF.IGM.SizeTy,
                    IGF.IGM.DataLayout.getTypeStoreSize(runtimeVal->getType()));
      auto msg
        = IGF.IGM.getAddrOfGlobalString(description.str());
      
      IGF.Builder.CreateCall5(verifierFn, metadata,
                              runtimePtr, staticPtr, count, msg);
    };

    // Check that the fixed layout matches the runtime layout.
    SILType layoutType = SILType::getPrimitiveObjectType(formalType);
    verify(emitLoadOfSize(IGF, layoutType),
           getSizeConstant(fixedTI->getFixedSize()),
           "size");
    verify(emitLoadOfAlignmentMask(IGF, layoutType),
           getAlignmentMaskConstant(fixedTI->getFixedAlignment()),
           "alignment mask");
    verify(emitLoadOfStride(IGF, layoutType),
           getSizeConstant(fixedTI->getFixedStride()),
           "stride");
    verify(emitLoadOfIsInline(IGF, layoutType),
           getBoolConstant(fixedTI->getFixedPacking(IGF.IGM)
                             == FixedPacking::OffsetZero),
           "is-inline bit");
    verify(emitLoadOfIsPOD(IGF, layoutType),
           getBoolConstant(fixedTI->isPOD(ResilienceScope::Local)),
           "is-POD bit");
    verify(emitLoadOfIsBitwiseTakable(IGF, layoutType),
           getBoolConstant(fixedTI->isBitwiseTakable(ResilienceScope::Local)),
           "is-bitwise-takable bit");
    unsigned xiCount = fixedTI->getFixedExtraInhabitantCount(IGF.IGM);
    verify(emitLoadOfHasExtraInhabitants(IGF, layoutType),
           getBoolConstant(xiCount != 0),
           "has-extra-inhabitants bit");

    // Check extra inhabitants.
    if (xiCount > 0) {
      verify(emitLoadOfExtraInhabitantCount(IGF, layoutType),
             getSizeConstant(Size(xiCount)),
             "extra inhabitant count");
      
      // Verify that the extra inhabitant representations are consistent.
      
      auto xiBuf = IGF.createAlloca(fixedTI->getStorageType(),
                                    fixedTI->getFixedAlignment(),
                                    "extra-inhabitant");
      auto xiOpaque = IGF.Builder.CreateBitCast(xiBuf, IGF.IGM.OpaquePtrTy);
      
      // TODO: Randomize the set of extra inhabitants we check.
      unsigned bits = fixedTI->getFixedSize().getValueInBits();
      for (unsigned i = 0, e = std::min(xiCount, 1024u);
           i < e; ++i) {
        // Initialize the buffer with junk, to help ensure we're insensitive to
        // insignificant bits.
        // TODO: Randomize the filler.
        IGF.Builder.CreateMemSet(xiBuf.getAddress(),
                                 llvm::ConstantInt::get(IGF.IGM.Int8Ty, 0x5A),
                                 fixedTI->getFixedSize().getValue(),
                                 fixedTI->getFixedAlignment().getValue());
        
        // Ask the runtime to store an extra inhabitant.
        auto index = llvm::ConstantInt::get(IGF.IGM.Int32Ty, i);
        emitStoreExtraInhabitantCall(IGF, layoutType, index,
                                     xiOpaque.getAddress());
        
        // Compare the stored extra inhabitant against the fixed extra
        // inhabitant pattern.
        auto fixedXI = fixedTI->getFixedExtraInhabitantValue(IGF.IGM, bits, i);
        auto xiBuf2 = IGF.Builder.CreateBitCast(xiBuf,
                                            fixedXI->getType()->getPointerTo());
        llvm::Value *runtimeXI = IGF.Builder.CreateLoad(xiBuf2);
        runtimeXI = fixedTI->maskFixedExtraInhabitant(IGF, runtimeXI);
        
        numberBuf.clear();
        {
          llvm::raw_svector_ostream os(numberBuf);
          os << i;
          os.flush();
        }
        
        verify(runtimeXI, fixedXI,
               llvm::Twine("stored extra inhabitant ") + numberBuf.str());
        
        // Now store the fixed extra inhabitant and ask the runtime to identify
        // it.
        // Mask in junk to make sure the runtime correctly ignores it.
        auto xiMask = fixedTI->getFixedExtraInhabitantMask(IGF.IGM).asAPInt();
        auto maskVal = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(), xiMask);
        auto notMaskVal
          = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(), ~xiMask);
        // TODO: Randomize the filler.
        auto xiFill = llvm::ConstantInt::getAllOnesValue(fixedXI->getType());
        llvm::Value *xiFillMask = IGF.Builder.CreateAnd(notMaskVal, xiFill);
        llvm::Value *xiValMask = IGF.Builder.CreateAnd(maskVal, fixedXI);
        llvm::Value *filledXI = IGF.Builder.CreateOr(xiFillMask, xiValMask);
        
        IGF.Builder.CreateStore(filledXI, xiBuf2);
        
        auto runtimeIndex = emitGetExtraInhabitantIndexCall(IGF, layoutType,
                                                        xiOpaque.getAddress());
        verify(runtimeIndex, index,
               llvm::Twine("extra inhabitant index calculation ")
                 + numberBuf.str());
      }
    }

    // TODO: Verify interesting layout properties specific to the kind of type,
    // such as struct or class field offsets, enum case tags, vtable entries,
    // etc.
  }
}
