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

#include "swift/SILPasses/Utils/SILInliner.h"
#include "llvm/ADT/STLExtras.h"
using namespace swift;

/// \brief Inlines the callee of a given ApplyInst (which must be the value of a
/// FunctionRefInst referencing a function with a known body), into the caller
/// containing the ApplyInst, which must be the same function as provided to the
/// constructor of SILInliner. It only performs one step of inlining: it does
/// not recursively inline functions called by the callee.
///
/// \returns true on success or false if it is unable to inline the function
/// (for any reason).
bool SILInliner::inlineFunction(SILBasicBlock::iterator &I,
                                SILFunction *CalleeFunction,
                                ArrayRef<Substitution> Subs,
                                ArrayRef<SILValue> Args) {
  SILFunction &F = getBuilder().getFunction();
  assert(I->getParent()->getParent() && I->getParent()->getParent() == &F &&
         "Inliner called on apply instruction in wrong function?");
  assert(I != I->getParent()->end() && "Inliner called on a terminator?");
  assert(CalleeFunction->getAbstractCC() != AbstractCC::ObjCMethod &&
         CalleeFunction->getAbstractCC() != AbstractCC::C &&
         "Cannot inline Objective-C method or C function");

  // We can't handle specializations yet.
  if (!Subs.empty())
    return false;
  
  CalleeEntryBB = CalleeFunction->begin();

  // Compute the SILLocation which should be used by all the inlined
  // instructions.
  Loc = InlinedLocation::getInlinedLocation(I->getLoc());
  DebugScope = I->getDebugScope();

  // If the caller's BB is not the last BB in the calling function, then keep
  // track of the next BB so we always insert new BBs before it; otherwise,
  // we just leave the new BBs at the end as they are by default.
  auto IBI = llvm::next(SILFunction::iterator(I->getParent()));
  InsertBeforeBB = IBI != F.end() ? IBI : nullptr;

  // Clear argument map and map ApplyInst arguments to the arguments of the
  // callee's entry block.
  ArgumentMap.clear();
  assert(CalleeEntryBB->bbarg_size() == Args.size() &&
         "Unexpected number of arguments to entry block of function?");
  auto BAI = CalleeEntryBB->bbarg_begin();
  for (auto AI = Args.begin(), AE = Args.end(); AI != AE; ++AI, ++BAI)
    ArgumentMap.insert(std::make_pair(*BAI, *AI));

  InstructionMap.clear();
  BBMap.clear();
  SILBasicBlock::iterator InsertPoint = llvm::next(I);
  getBuilder().setInsertionPoint(InsertPoint);
  // Recursively visit callee's BB in depth-first preorder, starting with the
  // entry block, cloning all instructions other than terminators.
  visitSILBasicBlock(CalleeEntryBB);

  // If the callee's entry block ends in a return, then we can avoid a split.
  if (ReturnInst *RI = dyn_cast<ReturnInst>(CalleeEntryBB->getTerminator())) {
    // Replace all uses of the apply instruction with the operands of the
    // return instruction, appropriately mapped.
    SILValue(I).replaceAllUsesWith(remapValue(RI->getOperand()));
    // And get rid of the no-longer-needed ApplyInst.
    SILBasicBlock::iterator II = I++;
    II->eraseFromParent();
  } else {
    // Otherwise, split the caller's basic block to create a return-to BB.
    SILBasicBlock *CallerBB = I->getParent();
    // Split the BB and do NOT create a branch between the old and new
    // BBs; we will create the appropriate terminator manually later.
    SILBasicBlock *ReturnToBB = CallerBB->splitBasicBlock(InsertPoint);
    // Place the return-to BB after all the other mapped BBs.
    if (InsertBeforeBB)
      F.getBlocks().splice(SILFunction::iterator(InsertBeforeBB), F.getBlocks(),
                           SILFunction::iterator(ReturnToBB));
    else
      F.getBlocks().splice(F.getBlocks().end(), F.getBlocks(),
                           SILFunction::iterator(ReturnToBB));
    // Create an argument on the return-to BB representing the returned value.
    SILValue RetArg = new (F.getModule()) SILArgument(I->getType(0),
                                                      ReturnToBB);
    // Replace all uses of the ApplyInst with the new argument.
    SILValue(I).replaceAllUsesWith(RetArg);
    // And get rid of the no-longer-needed ApplyInst.
    SILBasicBlock::iterator II = I++;
    II->eraseFromParent();

    // Now iterate over the callee BBs and fix up the terminators.
    getBuilder().setInsertionPoint(CallerBB);
    // We already know that the callee's entry block does not terminate with a
    // Return Inst, so it can definitely be cloned with the normal SILCloner
    // visit function.
    visit(CalleeEntryBB->getTerminator());
    for (auto BI = BBMap.begin(), BE = BBMap.end(); BI != BE; ++BI) {
      getBuilder().setInsertionPoint(BI->second);

      // Modify return terminators to branch to the return-to BB, rather than
      // trying to clone the ReturnInst.
      if (ReturnInst *RI = dyn_cast<ReturnInst>(BI->first->getTerminator())) {
        getBuilder().createBranch(Loc.getValue(), ReturnToBB,
                                  remapValue(RI->getOperand()));
        continue;
      }

      assert(!isa<AutoreleaseReturnInst>(BI->first->getTerminator()) &&
             "Unexpected autorelease return while inlining non-Objective-C "
             "function?");
      // Otherwise use normal visitor, which clones the existing instruction
      // but remaps basic blocks and values.
      visit(BI->first->getTerminator());
    }
  }

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
