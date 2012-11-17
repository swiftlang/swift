//===--- SILBuilder.h - Class for creating SIL Constructs --------*- C++ -*-==//
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

#ifndef SWIFT_SIL_SILBUILDER_H
#define SWIFT_SIL_SILBUILDER_H

#include "swift/SIL/BasicBlock.h"
#include "swift/SIL/Instruction.h"

namespace swift {

class SILBuilder {
  /// BB - If this is non-null, the instruction is inserted in the specified
  /// basic block, at the specified InsertPt.  If null, created instructions
  /// are not auto-inserted.
  Function &F;
  BasicBlock *BB;
  BasicBlock::iterator InsertPt;
public:

  SILBuilder(Function &F) : F(F), BB(0) {}

  SILBuilder(Instruction *I, Function &F) : F(F) {
    setInsertionPoint(I);
  }

  SILBuilder(BasicBlock *BB, Function &F) : F(F) {
    setInsertionPoint(BB);
  }

  SILBuilder(BasicBlock *BB, BasicBlock::iterator InsertPt, Function &F) : F(F){
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
  /// this function.  Since each block is implicitly added to the Function's list of
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
    Function *F = BB->getParent();
    // If this is a fall through into BB, emit the fall through branch.
    if (hasValidInsertionPoint())
      createBranch(BB);
    
    // Start inserting into that block.
    setInsertionPoint(BB);
    
    // Move block to the end of the list.
    if (&F->getBlocks().back() != BB)
      F->getBlocks().splice(F->end(), F->getBlocks(), BB);
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

  AllocArrayInst *createAllocArray(Expr *E, Type ElementType,
                                   Value NumElements) {
    return insert(new AllocArrayInst(E, ElementType, NumElements, F));
  }


  ApplyInst *createApply(ApplyExpr *Expr, Value Fn, ArrayRef<Value> Args) {
    return insert(ApplyInst::create(Expr, Fn, Args, F));
  }
  ApplyInst *createApply(Value Fn, ArrayRef<Value> Args) {
    return insert(ApplyInst::create(Fn, Args, F));
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

  LoadInst *createLoad(LoadExpr *Expr, Value LV) {
    return insert(new LoadInst(Expr, LV));
  }

  StoreInst *createStore(AssignStmt *S, Value Src, Value DestLValue) {
    return insert(new StoreInst(S, Src, DestLValue));
  }

  CopyInst *createCopy(Expr *E, Value SrcLValue, Value DestLValue) {
    return insert(new CopyInst(E, SrcLValue, DestLValue, false, false));
  }
  
  StoreInst *createInitialization(VarDecl *VD, Value Src,
                                  Value DestLValue) {
    return insert(new StoreInst(VD, Src, DestLValue));
  }
  StoreInst *createInitialization(Expr *E, Value Src, Value DestLValue) {
    return insert(new StoreInst(E, true, Src, DestLValue));
  }

  SpecializeInst *createSpecialize(SpecializeExpr *SE, Value Operand,
                                   Type DestTy) {
    return insert(new SpecializeInst(SE, Operand, DestTy));
  }


  TypeConversionInst *createTypeConversion(ImplicitConversionExpr *E,
                                           Value Op) {
    return insert(new TypeConversionInst(E, Op));
  }
  TypeConversionInst *createTypeConversion(Type Ty, Value Op) {
    return insert(new TypeConversionInst(Ty, Op));
  }

  TupleInst *createTuple(Expr *E, ArrayRef<Value> Elements) {
    return insert(TupleInst::create(E, Elements, F));
  }
  TupleInst *createTuple(Type Ty, ArrayRef<Value> Elements) {
    return insert(TupleInst::create(Ty, Elements, F));
  }

  Value createTupleElement(TupleElementExpr *E, Value Operand,
                           unsigned FieldNo) {
    // Fold tupleelement(tuple(a,b,c), 1) -> b.
    if (TupleInst *TI = dyn_cast<TupleInst>(Operand))
      return TI->getElements()[FieldNo];

    return insert(new TupleElementInst(E, Operand, FieldNo));
  }
  Value createTupleElement(Type ResultTy, Value Operand, unsigned FieldNo) {
    // Fold tupleelement(tuple(a,b,c), 1) -> b.
    if (TupleInst *TI = dyn_cast<TupleInst>(Operand))
      return TI->getElements()[FieldNo];

    return insert(new TupleElementInst(ResultTy, Operand, FieldNo));
  }

  MetatypeInst *createMetatype(MetatypeExpr *Expr) {
    return insert(new MetatypeInst(Expr));
  }
  
  RetainInst *createRetain(Expr *E, Value Operand) {
    return insert(new RetainInst(E, Operand));
  }
  ReleaseInst *createRelease(Expr *E, Value Operand) {
    return insert(new ReleaseInst(E, Operand));
  }
  DeallocInst *createDealloc(Expr *E, Value Operand) {
    return insert(new DeallocInst(E, Operand));
  }
  DestroyInst *createDestroy(Expr *E, Value Operand) {
    return insert(new DestroyInst(E, Operand));
  }

  //===--------------------------------------------------------------------===//
  // SIL-only instructions that don't have an AST analog
  //===--------------------------------------------------------------------===//

  IndexLValueInst *createIndexLValue(Expr *E, Value Operand, unsigned Index) {
    return insert(new IndexLValueInst(E, Operand, Index));
  }

  IntegerValueInst *createIntegerValueInst(uint64_t Val, Type Ty) {
    return insert(new IntegerValueInst(Val, Ty));
  }


  //===--------------------------------------------------------------------===//
  // Terminator Instruction Creation Methods
  //===--------------------------------------------------------------------===//

  UnreachableInst *createUnreachable() {
    return insertTerminator(new UnreachableInst(F));
  }

  ReturnInst *createReturn(ReturnStmt *Stmt, Value ReturnValue) {
    return insertTerminator(new ReturnInst(Stmt, ReturnValue));
  }
  
  CondBranchInst *createCondBranch(Stmt *BranchStmt, Value Cond,
                                   BasicBlock *Target1, BasicBlock *Target2) {
    return insertTerminator(new CondBranchInst(BranchStmt, Cond,
                                               Target1, Target2));
  }
    
  BranchInst *createBranch(BasicBlock *TargetBlock) {
    return insertTerminator(new BranchInst(TargetBlock, F));
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
