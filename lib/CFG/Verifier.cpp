//===--- Verifier - Verification of Swift CFGs -----------------------------==//
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
  
  void visitCallInst(CallInst *CI) {
  }
  
  void visitDeclRefInst(DeclRefInst *DRI) {
  }
  void visitIntegerLiteralInst(IntegerLiteralInst *ILI) {
  }
  void visitLoadInst(LoadInst *LI) {
  }
  void visitThisApplyInst(ThisApplyInst *TAI) {
  }
  void visitTupleInst(TupleInst *TI) {
  }
  void visitTypeOfInst(TypeOfInst *TOI) {
  }
  
  void visitReturnInst(ReturnInst *RI) {
  }
  
  void visitBranchInst(BranchInst *BI) {
  }
  
  void visitCondBranchInst(CondBranchInst *CBI) {
  }
};
} // end anonymous namespace


/// verify - Run the IR verifier to make sure that the CFG follows invariants.
void CFG::verify() const {
  CFGVerifier().visitCFG(const_cast<CFG*>(this));
}
