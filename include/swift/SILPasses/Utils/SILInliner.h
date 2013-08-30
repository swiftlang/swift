//===--- SILInliner.h - Inlines SIL functions --------------------*- C++ -*-==//
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
// This file defines the SILInliner class, used for inlining SIL functions into
// function application sites
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILINLINER_H
#define SWIFT_SIL_SILINLINER_H

#include "llvm/ADT/DenseMap.h"
#include "swift/SIL/SILCloner.h"

namespace swift {

class SILInliner : private SILCloner<SILInliner> {
public:
  friend class SILCloner<SILInliner>;

  explicit SILInliner(SILFunction &F, bool MakeTransparent = false)
    : SILCloner<SILInliner>(F, MakeTransparent) {
  }

  /// inlineFunction - This method inlines a callee function, assuming that it
  /// is called with the given arguments, into the caller at a given instruction
  /// (as specified by a basic block iterator), assuming that the instruction
  /// corresponds semantically to an application of the function. It only
  /// performs one step of inlining: it does not recursively inline functions
  /// called by the callee.
  ///
  /// Returns true on success or false if it is unable to inline the function
  /// (for any reason). If successful, I now points to the first inlined
  /// instruction, or the next instruction after the removed instruction in the
  /// original function, in case the inlined function is completely trivial
  bool inlineFunction(SILBasicBlock::iterator &I, SILFunction *CalleeFunction,
                      ArrayRef<SILValue> Args);

  bool inlineFunction(SILInstruction *AI, SILFunction *CalleeFunction,
                      ArrayRef<SILValue> Args) {
    assert(AI->getParent() && "Inliner called on uninserted instruction");
    SILBasicBlock::iterator I(AI);
    return inlineFunction(I, CalleeFunction, Args);
  }

private:
  void visitSILBasicBlock(SILBasicBlock* BB);

  SILValue remapValue(SILValue Value) {
    if (SILArgument* A = dyn_cast<SILArgument>(Value.getDef())) {
      assert(Value.getResultNumber() == 0 &&
             "Non-zero result number of argument used?");
      SILValue MappedValue = ArgumentMap[A];
      assert (MappedValue && "Unmapped argument while inlining");
      return MappedValue;
    }

    if (SILInstruction* I = dyn_cast<SILInstruction>(Value.getDef())) {
      ValueBase* V = InstructionMap[I];
      assert(V && "Unmapped instruction while inlining?");
      return SILValue(V, Value.getResultNumber());
    }

    llvm_unreachable("Unknown value type while inlining?");
  }

  SILBasicBlock *remapBasicBlock(SILBasicBlock *BB) {
    SILBasicBlock* MappedBB = BBMap[BB];
    assert(MappedBB && "Unmapped basic block while inlining?");
    return MappedBB;
  }

  SILValue postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    InstructionMap.insert(std::make_pair(Orig, Cloned));
    return Cloned;
  }

  SILBasicBlock* CalleeEntryBB;
  SILBasicBlock* InsertBeforeBB;
  llvm::DenseMap<SILArgument*, SILValue> ArgumentMap;
  llvm::DenseMap<SILInstruction*, SILInstruction*> InstructionMap;
  llvm::DenseMap<SILBasicBlock*, SILBasicBlock*> BBMap;
};

} // end namespace swift

#endif
