//===--- StackAllocation.cpp - Optimize alloc_stacks ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "irgen-stackallocation"
#include "StackAllocation.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "GenOpaque.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace irgen;

bool isOpenedArchetype(SILType t) {
  ArchetypeType *archetype = dyn_cast<ArchetypeType>(t.getSwiftRValueType());
  if (!archetype)
    return false;
  
  return !archetype->getOpenedExistentialType().isNull();
}

void StackAllocation::collectAllocs(AllocStackInst *FirstASI) {
  int NumArgsNeeded = 0;
  llvm::SmallPtrSet<SILInstruction *, 32> VisitedInsts;
  bool NextCopyEndsSequence = false;
  auto EndIter = FirstASI->getParent()->end();
  for (auto Iter = FirstASI->getIterator(); Iter != EndIter; ++Iter) {
    SILInstruction *I = &*Iter;
    VisitedInsts.insert(I);
    if (AllocStackInst *ASI = dyn_cast<AllocStackInst>(I)) {
      SILType AllocedTy = ASI->getElementType();
      const TypeInfo &TI = IGF.IGM.getTypeInfo(AllocedTy);
      if (!TI.isFixedSize() && !isOpenedArchetype(AllocedTy)) {
        
        if (AllocInfos.size() == 0 || AllocInfos[0].getType() != AllocedTy) {
          if (NumArgsNeeded >= MaxNumArguments) {
            return;
          }
          NumArgsNeeded++;
        }
        if (AllocInfos.size() >= MaxCombinedAllocs)
          return;

        DEBUG(llvm::dbgs() << "  add alloc " << *ASI);
        AllocInfos.push_back(AllocInfo(ASI,
                                       TI.getStorageType()->getPointerTo(),
                                       TI.getBestKnownAlignment()));
      }
      continue;
    }
    CopyAddrInst *CAI = dyn_cast<CopyAddrInst>(I);
    if (CAI && CAI->isInitializationOfDest() &&
        VisitedInsts.count(dyn_cast<SILInstruction>(CAI->getSrc())) == 0) {
      if (AllocStackInst *ASI = dyn_cast<AllocStackInst>(CAI->getDest())) {
        if (AllocInfo *AI = getAllocInfo(ASI)) {
          if (AI->copyDeferable) {
            if (NextCopyEndsSequence || NumArgsNeeded >= MaxNumArguments) {
              if (AI == &AllocInfos.back()) {
                DEBUG(llvm::dbgs() << "  remove last\n");
                AllocInfos.erase(AllocInfos.begin() + AllocInfos.size() - 1);
              }
              return;
            }
            NumArgsNeeded++;
            DEBUG(llvm::dbgs() << "  add copy " << *CAI);
            AI->CopyAddr = CAI;
            ++NumCopyAddrToHandle;
            continue;
          }
        }
      }
    }
    for (Operand &Op : I->getAllOperands()) {
      if (AllocStackInst *ASI = dyn_cast<AllocStackInst>(Op.get())) {
        if (AllocInfo *AI = getAllocInfo(ASI))
          AI->copyDeferable = false;
      }
    }
    if (I->mayWriteToMemory()) {
      NextCopyEndsSequence = true;
    }
  }
}

Address StackAllocation::loadBuffer(AllocInfo *AI) {
  --NumAllocsToHandle;
  DEBUG(llvm::dbgs() << "    load buffer at index " << AI->Index << "(" <<
        NumAllocsToHandle << " left) for " << *AI->Alloc);
  assert(AI->Index >= 0);
  llvm::Value *BufPtr;
  if (AllocInfos.size() == 1) {
    BufPtr = RTCall;
  } else {
    BufPtr = IGF.Builder.CreateExtractValue(RTCall, {(unsigned)AI->Index});
  }
  llvm::Value *BufPtrCast = IGF.Builder.CreateBitCast(BufPtr, AI->AddrType);
  return Address(BufPtrCast, AI->AddrAlign);
}

Address StackAllocation::allocStack(AllocStackInst *ASI) {
  const TypeInfo &TI = IGF.IGM.getTypeInfo(ASI->getElementType());
  if (TI.isFixedSize() || isOpenedArchetype(ASI->getElementType()))
    return Address();

  if (AllocInfo *AI = getAllocInfo(ASI)) {
    return loadBuffer(AI);
  }

  DEBUG(llvm::dbgs() << "Start collecting at " << *ASI);
  if (!MemAllocCast) {
    llvm::Instruction *AllocaIP = IGF.getAllocaIP();
    IRBuilder B(IGF.IGM.getLLVMContext(), IGF.IGM.DebugInfo);
    B.SetInsertPoint(AllocaIP->getParent(), std::next(AllocaIP->getIterator()));
    MemAllocCast = cast<llvm::BitCastInst>(B.CreateBitCast(AllocaIP,
                                           IGF.IGM.Int8PtrTy->getPointerTo()));
    llvm::Constant *NullPtr = llvm::Constant::getNullValue(IGF.IGM.Int8PtrTy);
    B.CreateStore(NullPtr, MemAllocCast, IGF.IGM.getPointerAlignment());
  }

  assert(NumAllocsToHandle == 0);
  assert(NumCopyAddrToHandle == 0);
  AllocInfos.clear();

  collectAllocs(ASI);
  unsigned NumAllocs = AllocInfos.size();
  NumAllocsToHandle = NumAllocs;
  assert(NumAllocs >= 1 && NumAllocs <= 2);
  
  if (NumAllocs == 2 && !AllocInfos[0].CopyAddr && AllocInfos[1].CopyAddr) {
    std::swap(AllocInfos[0], AllocInfos[1]);
  }
  
  assert(getFixedBufferAlignment(IGF.IGM) == IGF.IGM.getPointerAlignment());
  int NumPtrsInFixedBuf = getFixedBufferSize(IGF.IGM) / IGF.IGM.getPointerSize();
  int BufferSize = NumAllocs * NumPtrsInFixedBuf;

  llvm::Type *BufferTy = llvm::ArrayType::get(IGF.IGM.Int8PtrTy, BufferSize);

  Address AllocAddr = IGF.createAlloca(BufferTy, IGF.IGM.getPointerAlignment(),
                                       "stackbuffer");
  BufAlloc = cast<llvm::AllocaInst>(AllocAddr.getAddress());
  
  int Index = 0;
  for (AllocInfo &AI : AllocInfos) {
    AI.Index = Index++;
    NoDeallocNeededFor.insert(AI.Alloc);
  }

  auto *BufAllocCast = IGF.Builder.CreateBitCast(BufAlloc, IGF.IGM.Int8PtrTy);
  llvm::Value *AllocID = llvm::Constant::getIntegerValue(IGF.IGM.SizeTy,
                        APInt(IGF.IGM.SizeTy->getBitWidth(), CurrentBufferIdx));
  llvm::Value *Undef = llvm::UndefValue::get(IGF.IGM.OpaquePtrTy);
  llvm::Value *RTCallArgs[6] = {
    MemAllocCast, BufAllocCast, AllocID, Undef, Undef, Undef
  };
  
  int ArgIdx = 3;
  AllocInfo &AI0 = AllocInfos[0];
  llvm::Value *MD0 = IGF.emitTypeMetadataRefForLayout(AI0.getType());
  RTCallArgs[ArgIdx++] = MD0;
  if (AI0.CopyAddr) {
    AI0.CopyAddrArgIdx = ArgIdx++;
  }
  
  llvm::Value *Fn = nullptr;
  if (NumAllocs == 1) {
    if (AI0.CopyAddr) {
      Fn = IGF.IGM.getStackAllocCFn();
    } else {
      Fn = IGF.IGM.getStackAllocAFn();
    }
  } else if (NumAllocs == 2) {
    AllocInfo &AI1 = AllocInfos[1];
    if (AI1.getType() != AI0.getType()) {
      llvm::Value *MD1 = IGF.emitTypeMetadataRefForLayout(AI1.getType());
      RTCallArgs[ArgIdx++] = MD1;
    }
    if (AI0.CopyAddr) {
      if (AI1.CopyAddr) {
        assert(AI1.getType() == AI0.getType());
        AI1.CopyAddrArgIdx = ArgIdx++;
        Fn = IGF.IGM.getStackAllocCcFn();
      } else {
        if (AI1.getType() != AI0.getType()) {
          Fn = IGF.IGM.getStackAllocCAFn();
        } else {
          Fn = IGF.IGM.getStackAllocCaFn();
        }
      }
    } else {
      assert(!AI1.CopyAddr);
      if (AI1.getType() != AI0.getType()) {
        Fn = IGF.IGM.getStackAllocAAFn();
      } else {
        Fn = IGF.IGM.getStackAllocAaFn();
      }
    }
  }
  assert(Fn);
  assert(ArgIdx <= 6);
  RTCall = IGF.Builder.CreateCall(Fn, ArrayRef<llvm::Value *>(RTCallArgs, ArgIdx));
  ++CurrentBufferIdx;
  return loadBuffer(getAllocInfo(ASI));
}

bool StackAllocation::copyAddr(CopyAddrInst *CAI, Address Src) {
  AllocInfo *AI = getAllocInfo(CAI);
  if (!AI)
    return false;

  --NumCopyAddrToHandle;
  DEBUG(llvm::dbgs() << "    store copy addr (" << NumCopyAddrToHandle <<
        " left) for #" << AI->Index << ": " << *CAI);

  assert(AI->CopyAddr);

  llvm::IRBuilder<> B(IGF.IGM.getLLVMContext());
  B.SetInsertPoint(RTCall);
  llvm::Value *CastSrc = B.CreateBitCast(Src.getAddress(), IGF.IGM.OpaquePtrTy);
  if (AI->CopyAddrArgIdx >= 0) {
    assert(!AI->CopyAddrDest);
    RTCall->setArgOperand(AI->CopyAddrArgIdx, CastSrc);
  } else {
    assert(AI->CopyAddrDest);
    B.CreateStore(CastSrc, AI->CopyAddrDest);
  }
  
  AI->CopyAddr = nullptr;
  return true;
}

bool StackAllocation::deallocStack(DeallocStackInst *DSI) {
  if (noDeallocNeededFor(cast<AllocStackInst>(DSI->getOperand()))) {
    DEBUG(llvm::dbgs() << "    ignore dealloc " << *DSI);
    return true;
  }
  return false;
}

void StackAllocation::functionExit() {
  if (!MemAllocCast)
    return;
  RTCall = IGF.Builder.CreateCall(IGF.IGM.getStackDeallocFn(), {MemAllocCast});
}

void StackAllocation::finish() {
  assert(NumAllocsToHandle == 0);
  assert(NumCopyAddrToHandle == 0);
  if (MemAllocCast) {
    llvm::Type *DescrTy = llvm::ArrayType::get(IGF.IGM.Int8PtrTy,
                                               2 + CurrentBufferIdx * 2);
    Address ListAlloc = IGF.createAlloca(DescrTy, IGF.IGM.getPointerAlignment(),
                                         "allocdescr");
    MemAllocCast->setOperand(0, ListAlloc.getAddress());
  }
}
