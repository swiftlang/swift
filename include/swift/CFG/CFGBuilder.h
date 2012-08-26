//===--- CFGBuilder.h - Class for creating CFG Constructs --------*- C++ -*-==//
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

#ifndef SWIFT_CFG_CFGBUILDER_H
#define SWIFT_CFG_CFGBUILDER_H

#include "swift/CFG/BasicBlock.h"
#include "swift/CFG/Instruction.h"

namespace swift {

class CFGBuilder {
  /// BB - If this is non-null, the instruction is inserted in the specified
  /// basic block, at the specified InsertPt.  If null, created instructions
  /// are not auto-inserted.
  CFG &C;
  BasicBlock *BB;
  BasicBlock::iterator InsertPt;
public:

  CFGBuilder(CFG &C) : C(C), BB(0) {}

  CFGBuilder(Instruction *I, CFG &C) : C(C) {
    setInsertionPoint(I);
  }

  CFGBuilder(BasicBlock *BB, CFG &C) : C(C) {
    setInsertionPoint(BB);
  }

  CFGBuilder(BasicBlock *BB, BasicBlock::iterator InsertPt, CFG &C) : C(C) {
    setInsertionPoint(BB, InsertPt);
  }

  //===--------------------------------------------------------------------===//
  // Insertion Point Management
  //===--------------------------------------------------------------------===//

  BasicBlock *getInsertionBB() { return BB; }
  BasicBlock::iterator getInsertionPoint() { return InsertPt; }
  
  /// clearInsertionPoint - Clear the insertion point: created instructions will
  /// not be inserted into a block.
  void clearInsertionPoint() {
    BB = 0;
  }

  /// setInsertionPoint - Set the insertion point.
  void setInsertionPoint(BasicBlock *BB, BasicBlock::iterator InsertPt) {
    this->BB = BB;
    this->InsertPt = InsertPt;
  }

  /// setInsertionPoint - Set the insertion point to insert before the specified
  /// instruction.
  void setInsertionPoint(Instruction *I) {
    setInsertionPoint(I->getParent(), I);
  }

  /// setInsertionPoint - Set the insertion point to insert at the end of the
  /// specified block.
  void setInsertionPoint(BasicBlock *BB) {
    setInsertionPoint(BB, BB->end());
  }

  CallInst *createCall(CallExpr *Expr, CFGValue Fn, ArrayRef<CFGValue> Args) {
    return insert(CallInst::create(Expr, Fn, Args, C));
  }

  DeclRefInst *createDeclRef(DeclRefExpr *Expr) {
    return insert(new DeclRefInst(Expr));
  }

  IntegerLiteralInst *createIntegerLiteral(IntegerLiteralExpr *Expr) {
    return insert(new IntegerLiteralInst(Expr));
  }

  LoadInst *createLoad(LoadExpr *Expr, CFGValue LV) {
    return insert(new LoadInst(Expr, LV));
  }

  ThisApplyInst *createThisApply(ThisApplyExpr *Expr, CFGValue Fn,
                                 CFGValue Arg) {
    return insert(new ThisApplyInst(Expr, Fn, Arg));
  }

  TupleInst *createTuple(TupleExpr *Expr, ArrayRef<CFGValue> Elements) {
    return insert(TupleInst::create(Expr, Elements, C));
  }

  TypeOfInst *createTypeOf(TypeOfExpr *Expr) {
    return insert(new TypeOfInst(Expr));
  }
  
  ReturnInst *createReturn(ReturnStmt *Stmt, CFGValue ReturnValue) {
    return insert(new ReturnInst(Stmt, ReturnValue));
  }
  
  CondBranchInst *createCondBranch(Stmt *BranchStmt, CFGValue Cond,
                                   BasicBlock *Target1, BasicBlock *Target2) {
    return insert(new CondBranchInst(BranchStmt, Cond, Target1, Target2));
  }
  
  UncondBranchInst *createUncondBranch(BasicBlock *TargetBlock,
                                       ArrayRef<CFGValue> BlockArgs, CFG &C) {
    return insert(new UncondBranchInst(TargetBlock, BlockArgs, C));
  }

private:
  /// insert - This is a template to avoid losing type info on the result.
  template <typename T>
  T *insert(T *TheInst) {
    insertImpl(TheInst);
    return TheInst;
  }

  void insertImpl(Instruction *TheInst) {
    if (BB == 0) return;
    BB->getInsts().insert(InsertPt, TheInst);
  }
};

} // end swift namespace

#endif
