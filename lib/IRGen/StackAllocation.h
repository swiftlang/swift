//===--- StackAllocation.h - Optimize alloc_stacks --------------*- C++ -*-===//
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

#ifndef SWIFT_IRGEN_STACKALLOCATION_H
#define SWIFT_IRGEN_STACKALLOCATION_H

#include "swift/SIL/SILInstruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"
#include "TypeInfo.h"
#include "Address.h"

namespace swift {

namespace irgen {
class IRGenFunction;

class StackAllocation {
  
  IRGenFunction &IGF;
  
  llvm::BitCastInst *MemAllocCast = nullptr;
  llvm::Instruction *BufAlloc = nullptr;
  llvm::CallInst *RTCall = nullptr;
  
  unsigned CurrentBufferIdx = 0;
  int NumAllocsToHandle = 0;
  int NumCopyAddrToHandle = 0;
  
  enum {
    MaxNumArguments = 3,
    MaxCombinedAllocs = 2
  };
  
  struct AllocInfo {
    int Index = -1;
    AllocStackInst *Alloc = nullptr;
    CopyAddrInst *CopyAddr = nullptr;
    llvm::Type *AddrType;
    Alignment AddrAlign;
    llvm::Value *CopyAddrDest = nullptr;
    int CopyAddrArgIdx = -1;
    bool copyDeferable = true;
    
    AllocInfo(AllocStackInst *Alloc, llvm::Type *AddrType, Alignment AddrAlign)
      : Alloc(Alloc), AddrType(AddrType), AddrAlign(AddrAlign)
    { }

    SILType getType() const { return Alloc->getElementType(); }
  };
  
  llvm::SmallVector<AllocInfo, 2> AllocInfos;
  llvm::SmallPtrSet<AllocStackInst *, 16> NoDeallocNeededFor;

  AllocInfo *getAllocInfo(AllocStackInst *ASI) {
    for (AllocInfo &AInfo : AllocInfos) {
      if (AInfo.Alloc == ASI)
        return &AInfo;
    }
    return nullptr;
  }

  AllocInfo *getAllocInfo(CopyAddrInst *CAI) {
    for (AllocInfo &AInfo : AllocInfos) {
      if (AInfo.CopyAddr == CAI)
        return &AInfo;
    }
    return nullptr;
  }

  void collectAllocs(AllocStackInst *FirstASI);

  Address loadBuffer(AllocInfo *AI);

public:
  
  
  StackAllocation(IRGenFunction &IGF) : IGF(IGF) { }
  
  bool noDeallocNeededFor(AllocStackInst *ASI) const {
    return NoDeallocNeededFor.count(ASI) != 0;
  }
  
  Address allocStack(AllocStackInst *ASI);
  
  bool copyAddr(CopyAddrInst *CAI, Address Src);
  
  bool deallocStack(DeallocStackInst *DSI);
  
  void functionExit();
  
  void finish();
};

}
}

#endif
