//===--- PrepareForIRGen.cpp - Peephole opts before IRGen -----------------===//
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

#define DEBUG_TYPE "prepare-for-irgen"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"

using namespace swift;



//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

namespace {

class PrepareForIRGen : public SILFunctionTransform {
  typedef llvm::SmallPtrSet<SILInstruction *, 16> InstructionSet;
  
  bool Changed = false;
  
  /// The entry point to the transformation.
  void run() override;

  void optimizeBlock(SILBasicBlock &BB);

  bool tryMoveCopySrc(SILValue V, SILInstruction *InsertionPoint,
                      InstructionSet &VisitedInsts);

  StringRef getName() override { return "PrepareForIRGen"; }

public:
  PrepareForIRGen() {}
  
};

void PrepareForIRGen::run() {
  DEBUG(llvm::dbgs() << "** PrepareForIRGen **\n");
  
  SILFunction *F = getFunction();
  Changed = false;
  for (SILBasicBlock &BB : *F) {
    optimizeBlock(BB);
  }
  if (Changed)
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
}

void PrepareForIRGen::optimizeBlock(SILBasicBlock &BB) {
  InstructionSet VisitedInsts;
  AllocStackInst *InsertionPoint = nullptr;
  for (SILInstruction &I : BB) {
    if (auto *ASI = dyn_cast<AllocStackInst>(&I)) {
      if (!InsertionPoint)
        InsertionPoint = ASI;
    }
    if (InsertionPoint)
      VisitedInsts.insert(&I);
    
    auto *CAI = dyn_cast<CopyAddrInst>(&I);
    if (CAI && CAI->isInitializationOfDest()) {
      if (InsertionPoint)
        tryMoveCopySrc(CAI->getSrc(), InsertionPoint, VisitedInsts);
      continue;
    }
    if (I.mayWriteToMemory()) {
      InsertionPoint = nullptr;
      VisitedInsts.clear();
    }
  }
}

bool PrepareForIRGen::tryMoveCopySrc(SILValue V, SILInstruction *InsertionPoint,
                                     InstructionSet &VisitedInsts) {
  SILInstruction *I = dyn_cast<SILInstruction>(V);
  if (!I)
    return true;
  
  if (VisitedInsts.count(I) == 0)
    return true;
  
  if (I->getMemoryBehavior() != SILInstruction::MemoryBehavior::None)
    return false;
  
  if (isa<AllocStackInst>(I))
    return false;

  for (Operand &Op : I->getAllOperands()) {
    if (!tryMoveCopySrc(Op.get(), InsertionPoint, VisitedInsts))
      return false;
  }
  I->moveBefore(InsertionPoint);
  VisitedInsts.erase(I);
  Changed = true;
  return true;
}

} // end anonymous namespace

SILTransform *swift::createPrepareForIRGen() {
  return new PrepareForIRGen();
}
