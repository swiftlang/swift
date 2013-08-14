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

#include "swift/SIL/SILInliner.h"

using namespace swift;

/// inlineFunction - This method inlines the callee of a given ApplyInst,
/// which must a FunctionRefInst, into the caller containing the ApplyInst,
/// which must be the same function as provided to the constructor of
/// SILInliner. It only performs one step of inlining: it does not recursively
/// inline functions called by the callee.
///
/// Returns true on success or false if it is unable to inline the function
/// (for any reason).
bool SILInliner::inlineFunction(ApplyInst *AI) {
  assert(AI->getParent() && AI->getParent()->getParent() &&
         AI->getParent()->getParent() == &getBuilder().getFunction() &&
         "Inliner called on apply instruction in wrong function?");

  SILValue Callee = AI->getCallee();
  // I don't know how to handle anything other than direct function refs
  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(Callee.getDef());
  if (!FRI)
    return false;
  assert(Callee.getResultNumber() == 0);

  // FIXME: This should handle functions with more than one basic block
  SILFunction *CalledFunc = FRI->getFunction();
  if (!CalledFunc || CalledFunc->size() != 1)
    return false;

  // FIXME: This should handle functions that terminate with UnreachableInst
  // (and AutoreleaseReturn?)
  SILBasicBlock *CalledBB = CalledFunc->begin();
  ReturnInst *RI = dyn_cast<ReturnInst>(CalledBB->getTerminator());
  if (!RI)
    return false;

  ArgumentMap.clear();
  assert(CalledBB->bbarg_size() == AI->getArguments().size() &&
         "Unexpected number of arguments to entry block of function?");
  auto BAI = CalledBB->bbarg_begin();
  for (auto OI = AI->getArguments().begin(), OE = AI->getArguments().end();
       OI != OE; ++OI)
    ArgumentMap.insert(std::make_pair(*BAI, *OI));

  InstructionMap.clear();
  getBuilder().setInsertionPoint(AI);
  // Iterate over and visit all instructions other than the return
  for (auto I = CalledBB->begin(), E = --CalledBB->end(); I != E; ++I)
    visit(I);

  SILValue NewRetVal = remapValue(RI->getOperand());
  SILValue(AI).replaceAllUsesWith(NewRetVal);
  AI->eraseFromParent();
  if (FRI->use_empty())
    FRI->eraseFromParent();
  return true;
}
