//===--- Verifier.cpp - Verification of Swift CFGs ------------------------===//
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
// This file defines the high-level Instruction classes used for Swift CFGs.
//
//===----------------------------------------------------------------------===//

#include "swift/CFG/CFG.h"
#include "swift/CFG/CFGVisitor.h"
#include "swift/AST/Types.h"
using namespace swift;

namespace {
/// CFGVerifier class - This class implements the CFG verifier, which walks over
/// an instance of the CFG, checking and enforcing its invariants.
class CFGVerifier : public CFGVisitor<CFGVerifier> {
public:
  
  void visit(Instruction *I) {
    
    const BasicBlock *BB = I->getParent();
    // Check that non-terminators look ok.
    if (!isa<TermInst>(I)) {
      assert(!BB->empty() &&
             "Can't be in a parent block if it is empty");
      assert(&*BB->getInsts().rbegin() != I &&
             "Non-terminators cannot be the last in a block");
    } else {
      assert(&*BB->getInsts().rbegin() == I &&
             "Terminator must be the last in block");
    }

    
    // Dispatch to our more-specialized instances below.
    ((CFGVisitor<CFGVerifier>*)this)->visit(I);
  }

  void visitAllocInst(AllocInst *AI) {
    assert(AI->getType()->is<LValueType>() &&"Allocation should return lvalue");
  }

  void visitAllocVarInst(AllocVarInst *AI) {
    visitAllocInst(AI);
  }

  void visitAllocTmpInst(AllocTmpInst *AI) {
    visitAllocInst(AI);
  }


  void visitApplyInst(ApplyInst *AI) {
  }

  void visitConstantRefInst(ConstantRefInst *DRI) {
    assert(!DRI->getType()->is<LValueType>() &&
           "ConstantRef should return not produce an lvalue");
  }

  void visitIntegerLiteralInst(IntegerLiteralInst *ILI) {
    assert(ILI->getType()->is<BuiltinIntegerType>() &&
           "invalid integer literal type");
  }
  void visitLoadInst(LoadInst *LI) {
    assert(!LI->getType()->is<LValueType>() && "Load should produce rvalue");
    assert(LI->getLValue().getType()->is<LValueType>() &&
           "Load op should be lvalue");
    assert(LI->getLValue().getType()->getRValueType()->isEqual(LI->getType()) &&
           "Load operand type and result type mismatch");
  }

  void visitStoreInst(StoreInst *SI) {
    assert(!SI->getSrc().getType()->is<LValueType>() &&
           "Src value should be rvalue");
    assert(SI->getDest().getType()->is<LValueType>() &&
           "Dest address should be lvalue");
    assert(SI->getDest().getType()->getRValueType()->
              isEqual(SI->getSrc().getType()) &&
           "Store operand type and dest type mismatch");
  }

  void visitTupleInst(TupleInst *TI) {
  }
  void visitTypeOfInst(TypeOfInst *TOI) {
  }
  
  void visitReturnInst(ReturnInst *RI) {
    assert(!RI->getReturnValue().isNull() && "Return of null value is invalid");
  }
  
  void visitBranchInst(BranchInst *BI) {
  }
  
  void visitCondBranchInst(CondBranchInst *CBI) {
    assert(!CBI->getCondition().isNull() &&
           "Condition of conditional branch can't be missing");
  }
};
} // end anonymous namespace


/// verify - Run the IR verifier to make sure that the CFG follows invariants.
void CFG::verify() const {
  CFGVerifier().visitCFG(const_cast<CFG*>(this));
}
