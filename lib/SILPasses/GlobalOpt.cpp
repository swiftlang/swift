//===---------- SILGlobalOpt.cpp - Optimize global initializers -----------===//
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

#define DEBUG_TYPE "globalopt"
#include "swift/Basic/Demangle.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/Support/Debug.h"
using namespace swift;

namespace {
/// Optimize the placement of global initializers.
///
/// TODO:
///
/// - Use CallGraphAnalysis to move initializers to the module's public entry
///   points.
///
/// - Convert trivial initializers to static initialization. This requires
///   serializing globals.
///
/// - For global "lets", generate addressors that return by value. If we also
///  converted to a static initializer, then remove the load from the addressor.
///
/// - When the addressor is local to the module, be sure it is inlined to allow
///   constant propagation in case of statically initialized "lets".
class SILGlobalOpt : public SILModuleTransform
{
  // Map each global initializer to a list of call sites.
  typedef SmallVector<ApplyInst *, 4> GlobalInitCalls;
  llvm::MapVector<SILFunction*, GlobalInitCalls> GlobalInitCallMap;

  // Mark any block that this pass has determined to be inside a loop.
  llvm::DenseSet<SILBasicBlock*> LoopBlocks;
  // Mark any functions for which loops have been analyzed.
  llvm::DenseSet<SILFunction*> LoopCheckedFunctions;

public:
  void run() override;

  StringRef getName() override { return "SIL Global Optimization"; }

protected:
  void collectGlobalInitCall(ApplyInst *AI);
  bool isInLoop(SILBasicBlock *CurBB);
  void placeInitializers(SILFunction *InitF, ArrayRef<ApplyInst*> Calls);
};
} // namespace

/// If this is a call to a global initializer, map it.
void SILGlobalOpt::collectGlobalInitCall(ApplyInst *AI) {
  FunctionRefInst *FR = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (!FR)
    return;

  SILFunction *F = FR->getReferencedFunction();
  if (!F->isGlobalInit())
    return;

  GlobalInitCallMap[F].push_back(AI);
}

/// return true if this block is inside a loop.
bool SILGlobalOpt::isInLoop(SILBasicBlock *CurBB) {
  SILFunction *F = CurBB->getParent();
  // Catch the common case in which we've already hoisted the initializer.
  if (CurBB == &F->front())
    return false;

  if (LoopCheckedFunctions.insert(F).second) {
    for (auto I = scc_begin(F); !I.isAtEnd(); ++I) {
      if (I.hasLoop())
        for (SILBasicBlock *BB : *I)
          LoopBlocks.insert(BB);
    }
  }
  return LoopBlocks.count(CurBB);
}

/// Optimize placement of initializer calls given a list of calls to the
/// same initializer. All original initialization points must be dominated by
/// the final initialization calls.
///
/// For now, just hoist all initializers to their function entry.
void SILGlobalOpt::placeInitializers(SILFunction *InitF,
                                     ArrayRef<ApplyInst*> Calls) {
  DEBUG(llvm::dbgs() << "GlobalOpt: calls to "
        << Demangle::demangleSymbolAsString(InitF->getName())
        << " : " << Calls.size() << "\n");
  // Map each initializer-containing function to its final initializer call.
  llvm::DenseMap<SILFunction*, ApplyInst*> ParentFuncs;
  for (auto *AI : Calls) {
    assert(AI->getNumArguments() == 0 && "ill-formed global init call");

    auto PFI = ParentFuncs.find(AI->getFunction());
    if (PFI != ParentFuncs.end()) {
      // This call is redundant. Replace it.
      assert(cast<FunctionRefInst>(AI->getCallee())->getReferencedFunction()
             == InitF &&
             "ill-formed global init call");
      AI->replaceAllUsesWith(PFI->second);
      AI->eraseFromParent();
      continue;
    }
    // Check if this call is inside a loop. If not, don't move it.
    if (!isInLoop(AI->getParent())) {
      DEBUG(llvm::dbgs() << "  skipping (not in a loop): " << *AI
            << "  in " << AI->getFunction()->getName() << "\n");
      continue;
    }
    DEBUG(llvm::dbgs() << "  hoisting: " << *AI
          << "  in " << AI->getFunction()->getName() << "\n");

    // Move this call to the parent function's entry.
    FunctionRefInst *FuncRef = cast<FunctionRefInst>(AI->getOperand(0));
    assert(FuncRef->getReferencedFunction() == InitF && "wrong init call");
    SILFunction *ParentF = AI->getFunction();
    AI->moveBefore(ParentF->front().begin());
    FuncRef->moveBefore(AI);
    ParentFuncs[ParentF] = AI;
  }
}

void SILGlobalOpt::run() {
  for (auto &F : *getModule())
    for (auto &BB : F)
      for (auto &I : BB)
        if (ApplyInst *AI = dyn_cast<ApplyInst>(&I))
          collectGlobalInitCall(AI);

  for (auto &InitCalls : GlobalInitCallMap)
    placeInitializers(InitCalls.first, InitCalls.second);

  GlobalInitCallMap.clear();
}

SILTransform *swift::createGlobalOpt() {
  return new SILGlobalOpt();
}
