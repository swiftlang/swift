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

  bool hasValidInsertionPoint() const { return BB != nullptr; }
  BasicBlock *getInsertionBB() { return BB; }
  BasicBlock::iterator getInsertionPoint() { return InsertPt; }
  
  /// clearInsertionPoint - Clear the insertion point: created instructions will
  /// not be inserted into a block.
  void clearInsertionPoint() {
    BB = nullptr;
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

  /// emitBlock - Each basic block is individually new'd, then them emitted with
  /// this function.  Since each block is implicitly added to the CFG's list of
  /// blocks when created, the construction order is not particularly useful.
  ///
  /// Instead, we want blocks to end up in the order that they are *emitted*.
  /// The cheapest way to ensure this is to just move each block to the end of
  /// the block list when emitted: as later blocks are emitted, they'll be moved
  /// after this, giving us a block list order that matches emission order when
  /// the function is done.
  ///
  /// This function also sets the insertion point of the builder to be the newly
  /// emitted block.
  void emitBlock(BasicBlock *BB) {
    CFG *C = BB->getParent();
    // If this is a fall through into BB, emit the fall through branch.
    if (hasValidInsertionPoint())
      createBranch(BB);
    
    // Start inserting into that block.
    setInsertionPoint(BB);
    
    // Move block to the end of the list.
    if (&C->getBlocks().back() != BB)
      C->getBlocks().splice(C->end(), C->getBlocks(), BB);
  }
  
  //===--------------------------------------------------------------------===//
  // Instruction Creation Methods
  //===--------------------------------------------------------------------===//

  AllocVarInst *createAllocVar(VarDecl *VD) {
    return insert(new AllocVarInst(VD));
  }

  AllocTmpInst *createAllocTmp(MaterializeExpr *Expr) {
    return insert(new AllocTmpInst(Expr));
  }

  ApplyInst *createApply(ApplyExpr *Expr, CFGValue Fn, ArrayRef<CFGValue> Args){
    return insert(ApplyInst::create(Expr, Fn, Args, C));
  }

  ConstantRefInst *createConstantRef(DeclRefExpr *Expr) {
    return insert(new ConstantRefInst(Expr));
  }

  ZeroValueInst *createZeroValue(VarDecl *D) {
    return insert(new ZeroValueInst(D));
  }

  IntegerLiteralInst *createIntegerLiteral(IntegerLiteralExpr *E) {
    return insert(new IntegerLiteralInst(E));
  }
  FloatLiteralInst *createFloatLiteral(FloatLiteralExpr *E) {
    return insert(new FloatLiteralInst(E));
  }
  CharacterLiteralInst *createCharacterLiteral(CharacterLiteralExpr *E) {
    return insert(new CharacterLiteralInst(E));
  }
  StringLiteralInst *createStringLiteral(StringLiteralExpr *E) {
    return insert(new StringLiteralInst(E));
  }

  LoadInst *createLoad(LoadExpr *Expr, CFGValue LV) {
    return insert(new LoadInst(Expr, LV));
  }

  StoreInst *createStore(AssignStmt *S, CFGValue Src, CFGValue DestLValue) {
    return insert(new StoreInst(S, Src, DestLValue));
  }

  StoreInst *createInitialization(VarDecl *VD, CFGValue Src,
                                  CFGValue DestLValue) {
    return insert(new StoreInst(VD, Src, DestLValue));
  }
  StoreInst *createInitialization(MaterializeExpr *E, CFGValue Src,
                                  CFGValue DestLValue) {
    return insert(new StoreInst(E, Src, DestLValue));
  }

  RequalifyInst *createRequalify(RequalifyExpr *Expr, CFGValue Op) {
    return insert(new RequalifyInst(Expr, Op));
  }

  TupleInst *createTuple(TupleExpr *Expr, ArrayRef<CFGValue> Elements) {
    return insert(TupleInst::create(Expr, Elements, C));
  }
  TupleInst *createTuple(TupleShuffleExpr *Expr, ArrayRef<CFGValue> Elements) {
    return insert(TupleInst::create(Expr, Elements, C));
  }

  ScalarToTupleInst *createScalarToTuple(ScalarToTupleExpr *Expr, CFGValue Op) {
    return insert(new ScalarToTupleInst(Expr, Op));
  }

  TupleElementInst *createTupleElement(TupleElementExpr *E, CFGValue Operand,
                                       unsigned FieldNo) {
    return insert(new TupleElementInst(E, Operand, FieldNo));
  }
  TupleElementInst *createTupleElement(Type ResultTy, CFGValue Operand,
                                       unsigned FieldNo) {
    return insert(new TupleElementInst(ResultTy, Operand, FieldNo));
  }

  TypeOfInst *createTypeOf(TypeOfExpr *Expr) {
    return insert(new TypeOfInst(Expr));
  }
 
  
  //===--------------------------------------------------------------------===//
  // Terminator Instruction Creation Methods
  //===--------------------------------------------------------------------===//

  UnreachableInst *createUnreachable() {
    return insertTerminator(new UnreachableInst(C));
  }

  ReturnInst *createReturn(ReturnStmt *Stmt, CFGValue ReturnValue) {
    return insertTerminator(new ReturnInst(Stmt, ReturnValue));
  }
  
  CondBranchInst *createCondBranch(Stmt *BranchStmt, CFGValue Cond,
                                   BasicBlock *Target1, BasicBlock *Target2) {
    return insertTerminator(new CondBranchInst(BranchStmt, Cond,
                                               Target1, Target2));
  }
    
  BranchInst *createBranch(BasicBlock *TargetBlock) {
    return insertTerminator(new BranchInst(TargetBlock, C));
  }


  //===--------------------------------------------------------------------===//
  // Private Helper Methods
  //===--------------------------------------------------------------------===//

private:
  /// insert - This is a template to avoid losing type info on the result.
  template <typename T>
  T *insert(T *TheInst) {
    insertImpl(TheInst);
    return TheInst;
  }
  
  /// insertTerminator - This is the same as insert, but clears the insertion
  /// point after doing the insertion.  This is used by terminators, since it
  /// isn't valid to insert something after a terminator.
  template <typename T>
  T *insertTerminator(T *TheInst) {
    insertImpl(TheInst);
    clearInsertionPoint();
    return TheInst;
  }

  void insertImpl(Instruction *TheInst) {
    if (BB == 0) return;
    BB->getInsts().insert(InsertPt, TheInst);
  }
};

} // end swift namespace

#endif
