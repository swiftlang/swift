//===--- LLVMStackPromotion.cpp - Replace allocation calls with alloca ----===//
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
// This pass performs the last part of stack promotion for array buffers.
// The SIL StackPromotion pass generates a pair of swift_bufferAllocateOnStack
// and swift_bufferDeallocateFromStack calls. In this pass the final decision
// is made if stack promotion should be done. If yes, the
// swift_bufferAllocateOnStack is replace with an alloca plus a call to
// swift_initStackObject and the swift_bufferDeallocateFromStack is removed.
// TODO: This is a hack and eventually this pass should not be required at all.
// For details see the comments for the SIL StackPromoter.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "swift-stack-promotion"
#include "swift/LLVMPasses/Passes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Pass.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace swift;

STATISTIC(NumBufferAllocsPromoted,
          "Number of swift_bufferAllocate promoted");

cl::opt<int> LimitOpt("stack-promotion-limit",
                           llvm::cl::init(1024), llvm::cl::Hidden);

//===----------------------------------------------------------------------===//
//                            SwiftStackPromotion Pass
//===----------------------------------------------------------------------===//

char SwiftStackPromotion::ID = 0;

INITIALIZE_PASS_BEGIN(SwiftStackPromotion,
                      "swift-stack-promotion", "Swift stack promotion pass",
                      false, false)
INITIALIZE_PASS_END(SwiftStackPromotion,
                    "swift-stack-promotion", "Swift stack promotion pass",
                    false, false)

llvm::FunctionPass *swift::createSwiftStackPromotionPass() {
  initializeSwiftStackPromotionPass(*llvm::PassRegistry::getPassRegistry());
  return new SwiftStackPromotion();
}

void SwiftStackPromotion::getAnalysisUsage(llvm::AnalysisUsage &AU) const {
  AU.setPreservesCFG();
}

/// Checks if we can promote a buffer and returns the size of the buffer.
/// The \a align parameter is set to the alignment of the buffer.
int canPromote(CallInst *CI, unsigned &align, int maxSize) {
  if (CI->getNumArgOperands() != 3)
    return 0;

  auto *SizeConst = dyn_cast<ConstantInt>(CI->getArgOperand(1));
  if (!SizeConst)
    return 0;

  auto *AlignMaskConst = dyn_cast<ConstantInt>(CI->getArgOperand(2));
  if (!AlignMaskConst)
    return 0;

  int size = SizeConst->getValue().getSExtValue();
  if (size > maxSize)
    return 0;

  align = AlignMaskConst->getValue().getZExtValue() + 1;

  return size;
}

bool SwiftStackPromotion::runOnFunction(Function &F) {

  bool Changed = false;
  Constant *allocFunc = nullptr;
  Constant *initFunc = nullptr;
  int maxSize = LimitOpt;
  Module *M = F.getParent();
  const DataLayout &DL = M->getDataLayout();
  IntegerType *AllocType = nullptr;
  IntegerType *IntType = nullptr;

  SmallVector<CallInst *, 8> BufferAllocs;
  SmallPtrSet<CallInst *, 8> PromotedAllocs;
  SmallVector<CallInst *, 8> BufferDeallocs;

  // Search for allocation- and deallocation-calls in the function.
  for (BasicBlock &BB : F) {
    for (auto Iter = BB.begin(); Iter != BB.end(); ) {
      Instruction *I = &*Iter;
      Iter++;

      if (auto *AI = dyn_cast<AllocaInst>(I)) {
        int Size = 1;
        if (auto *SizeConst = dyn_cast<ConstantInt>(AI->getArraySize()))
          Size = SizeConst->getValue().getSExtValue();

          // Count the existing alloca sizes against the limit.
        maxSize -= DL.getTypeAllocSize(AI->getAllocatedType()) * Size;
      }

      auto *CI = dyn_cast<CallInst>(I);
      if (!CI)
        continue;

      Function *Callee = CI->getCalledFunction();
      if (!Callee)
        continue;

      if (Callee->getName() == "swift_bufferAllocateOnStack") {
        BufferAllocs.push_back(CI);
      } else if (Callee->getName() == "swift_bufferDeallocateFromStack") {
        BufferDeallocs.push_back(CI);
      }
    }
  }

  // First handle allocations.
  for (CallInst *CI : BufferAllocs) {
    Function *Callee = CI->getCalledFunction();
    assert(Callee);
    unsigned align = 0;
    if (int size = canPromote(CI, align, maxSize)) {
      maxSize -= size;
      if (!AllocType) {
        // Create the swift_initStackObject function and all required types.
        AllocType = IntegerType::get(M->getContext(), 8);
        IntType = IntegerType::get(M->getContext(), 32);
        auto *OrigFT = Callee->getFunctionType();
        auto *HeapObjTy = OrigFT->getReturnType();
        auto *MetaDataTy = OrigFT->getParamType(0);
        auto *NewFTy = FunctionType::get(HeapObjTy,
                                         {MetaDataTy, HeapObjTy},
                                         false);
        initFunc = M->getOrInsertFunction("swift_initStackObject", NewFTy);
      }
      // Replace the allocation call with an alloca.
      Value *AllocA = new AllocaInst(AllocType, ConstantInt::get(IntType, size),
                                     align, "buffer", &*F.front().begin());
      // And initialize it with a call to swift_initStackObject.
      IRBuilder<> B(CI);
      Value *casted = B.CreateBitCast(AllocA, CI->getType());
      CallInst *initCall = B.CreateCall(initFunc,
                                        {CI->getArgOperand(0), casted});
      CI->replaceAllUsesWith(initCall);
      CI->eraseFromParent();
      PromotedAllocs.insert(initCall);
      ++NumBufferAllocsPromoted;
    } else {
      // We don't do stack promotion. Replace the call with a call to the
      // regular swift_bufferAllocate.
      if (!allocFunc) {
        allocFunc = M->getOrInsertFunction("swift_bufferAllocate",
                                           Callee->getFunctionType());
      }
      CI->setCalledFunction(allocFunc);
    }
    Changed = true;
  }

  // After we made the decision for all allocations we can handle the
  // deallocations.
  for (CallInst *CI : BufferDeallocs) {
    CallInst *Alloc = dyn_cast<CallInst>(CI->getArgOperand(0));
    assert(Alloc && "alloc buffer obfuscated");
    if (PromotedAllocs.count(Alloc)) {
      IRBuilder<> B(CI);
      // This has two purposes:
      // 1. Tell LLVM the lifetime of the allocated stack memory.
      // 2. Avoid tail-call optimization which may convert the call to the final
      //    release to a jump, which is done after the stack frame is
      //    destructed.
      B.CreateLifetimeEnd(CI->getArgOperand(0));
    }
    // Other than inserting the end-of-lifetime, the deallocation is a no-op.
    CI->eraseFromParent();
    Changed = true;
  }
  return Changed;
}
