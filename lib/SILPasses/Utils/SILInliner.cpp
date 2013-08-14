//===--- SILInliner.cpp - Inlines SIL functions ------------------*- C++ -*-==//
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

#include "llvm/ADT/STLExtras.h"
#include "swift/SILPasses/Utils/SILInliner.h"

using namespace swift;

/// \brief Inlines the callee of a given ApplyInst (which must be the value of a
/// FunctionRefInst referencing a function with a known body), into the caller
/// containing the ApplyInst, which must be the same function as provided to the
/// constructor of SILInliner. It only performs one step of inlining: it does
/// not recursively inline functions called by the callee.
///
/// \returns true on success or false if it is unable to inline the function
/// (for any reason).
bool SILInliner::inlineFunction(ApplyInst *AI) {
  assert(AI->getParent() && AI->getParent()->getParent() &&
         AI->getParent()->getParent() == &getBuilder().getFunction() &&
         "Inliner called on apply instruction in wrong function?");

  // We do not support inlining of Objective-C methods
  SILFunction &F = getBuilder().getFunction();
  if (F.getAbstractCC() == AbstractCC::ObjCMethod)
    return false;

  SILValue Callee = AI->getCallee();
  // I don't yet know how to handle anything other than direct function refs.
  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(Callee.getDef());
  assert(FRI && "Callee to be inlined is not a function ref?");
  assert(Callee.getResultNumber() == 0);
  SILFunction *CalledFunc = FRI->getFunction();
  assert(CalledFunc && "No callee function being referenced while inlining?");
  assert(!CalledFunc->empty() && "No callee body while inlining?");
  CalleeEntryBB = CalledFunc->begin();

  // If the caller's BB is not the last BB in the calling function, then keep
  // track of the next BB so we always insert new BBs before it; otherwise,
  // we just leave the new BBs at the end as they are by default.
  auto IBI = llvm::next(SILFunction::iterator(AI->getParent()));
  InsertBeforeBB = IBI != F.end() ? IBI : nullptr;

  // Clear argument map and map ApplyInst arguments to the arguments of the
  // callee's entry block.
  ArgumentMap.clear();
  assert(CalleeEntryBB->bbarg_size() == AI->getArguments().size() &&
         "Unexpected number of arguments to entry block of function?");
  auto BAI = CalleeEntryBB->bbarg_begin();
  for (auto OI = AI->getArguments().begin(), OE = AI->getArguments().end();
       OI != OE; ++OI, ++BAI)
    ArgumentMap.insert(std::make_pair(*BAI, *OI));

  InstructionMap.clear();
  BBMap.clear();
  getBuilder().setInsertionPoint(AI);
  // Recursively visit callee's BB in depth-first preorder, starting with the
  // entry block, cloning all instructions other than terminators.
  visitSILBasicBlock(CalleeEntryBB);

  // If the callee's entry block ends in a return, then we can avoid a split.
  if (ReturnInst *RI = dyn_cast<ReturnInst>(CalleeEntryBB->getTerminator())) {
    // Replace all uses of the apply instruction with the operands of the
    // return instruction, appropriately mapped.
    SILValue(AI).replaceAllUsesWith(remapValue(RI->getOperand()));
    // And get rid of the no-longer-needed ApplyInst.
    AI->eraseFromParent();
  } else {
    // Otherwise, split the caller's basic block to create a return-to BB.
    SILBasicBlock *CallerBB = AI->getParent();
    // Split the BB and do NOT create a branch between the old and new
    // BBs; we will create the appropriate terminator manually later.
    SILBasicBlock *ReturnToBB =
      CallerBB->splitBasicBlock(AI, /*CreateBranch=*/false);
    // Place the return-to BB after all the other mapped BBs.
    if (InsertBeforeBB)
      F.getBlocks().splice(SILFunction::iterator(InsertBeforeBB), F.getBlocks(),
                           SILFunction::iterator(ReturnToBB));
    else
      F.getBlocks().splice(F.getBlocks().end(), F.getBlocks(),
                           SILFunction::iterator(ReturnToBB));
    // Create an argument on the return-to BB representing the returned value.
    SILValue RetArg = new (F.getModule()) SILArgument(AI->getType(),
                                                      ReturnToBB);
    // Replace all uses of the ApplyInst with the new argument.
    SILValue(AI).replaceAllUsesWith(RetArg);
    // And get rid of the no-longer-needed ApplyInst.
    AI->eraseFromParent();

    // Now iterate over the callee BBs and fix up the terminators.
    getBuilder().setInsertionPoint(CallerBB);
    // We already know that the callee's entry block does not terminiate with a
    // Return Inst, so it can definitely be cloned with the normal SILCloner
    // visit function.
    visit(CalleeEntryBB->getTerminator());
    for (auto I = BBMap.begin(), E = BBMap.end(); I != E; ++I) {
      getBuilder().setInsertionPoint(I->second);

      // Modify return terminators to branch to the return-to BB, rather than
      // trying to clone the ReturnInst.
      if (ReturnInst *RI = dyn_cast<ReturnInst>(I->first->getTerminator())) {
        getBuilder().createBranch(RI->getLoc(), ReturnToBB,
                                  remapValue(RI->getOperand()));
        continue;
      }

      assert(!isa<AutoreleaseReturnInst>(I->first->getTerminator()) &&
             "Unexpected autorelease return while inlining non-Objective-C "
             "function?");
      // Otherwise use normal visitor, which clones the existing instruction
      // but remaps basic blocks and values.
      visit(I->first->getTerminator());
    }
  }

  // If there's no longer any uses of the function ref, then we can get rid of
  // it, too.
  if (FRI->use_empty())
    FRI->eraseFromParent();

  return true;
}

// \brief Recursively visit a callee's BBs in depth-first preorder (only
/// processing blocks on the first visit), mapping newly visited BBs to new BBs
/// in the caller and cloning all instructions into the caller other than
/// terminators (which are handled separately later).
void SILInliner::visitSILBasicBlock(SILBasicBlock* BB) {
  SILFunction &F = getBuilder().getFunction();
  // Iterate over and visit all instructions other than the terminator to clone.
  for (auto I = BB->begin(), E = --BB->end(); I != E; ++I)
    visit(I);
  // Iterate over successors to do the depth-first search.
  for (auto &Succ : BB->getSuccs()) {
    assert (Succ.getBB() != CalleeEntryBB &&
            "Entry block should not be a successor when inlining");
    SILBasicBlock *&MappedBB = BBMap[Succ];
    // Only visit a successor that has not already been visisted.
    if (!MappedBB) {
      // Map the successor to a new BB.
      MappedBB = new (F.getModule()) SILBasicBlock(&F);
      // Create new arguments for each of the original block's arguments.
      for (auto &Arg : Succ.getBB()->getBBArgs()) {
        SILValue MappedArg = new (F.getModule()) SILArgument(Arg->getType(),
                                                             MappedBB);
        ArgumentMap.insert(std::make_pair(Arg, MappedArg));
      }
      // Also, move the new mapped BB to the right position in the caller
      if (InsertBeforeBB)
        F.getBlocks().splice(SILFunction::iterator(InsertBeforeBB),
                             F.getBlocks(), SILFunction::iterator(MappedBB));
      // Set the insertion point to the new mapped BB
      getBuilder().setInsertionPoint(MappedBB);
      // Recurse into the successor
      visitSILBasicBlock(Succ.getBB());
    }
  }
}
