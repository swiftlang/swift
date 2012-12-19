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

  AllocVarInst *createAllocVar(SILLocation Loc, AllocKind allocKind,
                               SILType elementType) {
    return insert(new AllocVarInst(Loc, allocKind, elementType, F));
  }

  AllocBoxInst *createAllocBox(SILLocation Loc, SILType ElementType) {
    return insert(new AllocBoxInst(Loc, ElementType, F));
  }

  AllocArrayInst *createAllocArray(Expr *E, SILType ElementType,
                                   Value NumElements) {
    return insert(new AllocArrayInst(E, ElementType, NumElements, F));
  }

  ApplyInst *createApply(SILLocation Loc, Value Fn,
                         SILType Result, ArrayRef<Value> Args) {
    return insert(ApplyInst::create(Loc, Fn, Result, Args, F));
  }

  ClosureInst *createClosure(SILLocation Loc, Value Fn, ArrayRef<Value> Args) {
    return insert(ClosureInst::create(Loc, Fn, Args, F));
  }

  ConstantRefInst *createConstantRef(SILLocation loc, SILConstant c, SILType ty) {
    return insert(new ConstantRefInst(loc, c, ty));
  }

  ZeroValueInst *createZeroValue(SILLocation Loc, SILType Ty) {
    return insert(new ZeroValueInst(Loc, Ty));
  }

  IntegerLiteralInst *createIntegerLiteral(IntegerLiteralExpr *E) {
    return insert(new IntegerLiteralInst(E));
  }
  IntegerLiteralInst *createIntegerLiteral(CharacterLiteralExpr *E) {
    return insert(new IntegerLiteralInst(E));
  }
  FloatLiteralInst *createFloatLiteral(FloatLiteralExpr *E) {
    return insert(new FloatLiteralInst(E));
  }
  StringLiteralInst *createStringLiteral(StringLiteralExpr *E) {
    return insert(new StringLiteralInst(E));
  }

  LoadInst *createLoad(SILLocation Loc, Value LV) {
    return insert(new LoadInst(Loc, LV));
  }

  StoreInst *createStore(SILLocation Loc, Value Src, Value DestLValue) {
    return insert(new StoreInst(Loc, Src, DestLValue));
  }

  ZeroAddrInst *createZeroAddr(SILLocation Loc,
                               Value DestLValue) {
    return insert(new ZeroAddrInst(Loc, DestLValue));
  }

  CopyAddrInst *createCopyAddr(SILLocation Loc,
                               Value SrcLValue, Value DestLValue,
                               bool isTake = false,
                               bool isInitialize = false) {
    return insert(new CopyAddrInst(Loc, SrcLValue, DestLValue,
                                   isTake, isInitialize));
  }
  
  SpecializeInst *createSpecialize(SILLocation Loc, Value Operand,
                                   ArrayRef<Substitution> Substitutions,
                                   SILType DestTy) {
    return insert(SpecializeInst::create(Loc, Operand, Substitutions, DestTy,
                                         F));
  }


  ImplicitConvertInst *createImplicitConvert(SILLocation Loc, Value Op,
                                             SILType Ty) {
    return insert(new ImplicitConvertInst(Loc, Op, Ty));
  }

  CoerceInst *createCoerce(SILLocation Loc, Value Op, SILType Ty) {
    return insert(new CoerceInst(Loc, Op, Ty));
  }
  
  DowncastInst *createDowncast(SILLocation Loc, Value Op, SILType Ty) {
    return insert(new DowncastInst(Loc, Op, Ty));
  }
  
  ArchetypeToSuperInst *createArchetypeToSuper(SILLocation Loc,
                                               Value Archetype, SILType BaseTy) {
    return insert(new ArchetypeToSuperInst(Loc, Archetype, BaseTy));
  }
  
  TupleInst *createTuple(SILLocation Loc, SILType Ty, ArrayRef<Value> Elements) {
    return insert(TupleInst::create(Loc, Ty, Elements, F));
  }
  
  TupleInst *createEmptyTuple(SILLocation Loc);

  Value createExtract(SILLocation Loc, Value Operand, unsigned FieldNo,
                           SILType ResultTy) {
    // Fold extract(tuple(a,b,c), 1) -> b.
    if (TupleInst *TI = dyn_cast<TupleInst>(Operand))
      return TI->getElements()[FieldNo];

    return insert(new ExtractInst(Loc, Operand, FieldNo, ResultTy));
  }

  Value createElementAddr(SILLocation Loc, Value Operand, unsigned FieldNo,
                          SILType ResultTy) {
    return insert(new ElementAddrInst(Loc, Operand, FieldNo, ResultTy));
  }
  
  Value createRefElementAddr(SILLocation Loc, Value Operand, unsigned FieldNo,
                          SILType ResultTy) {
    return insert(new RefElementAddrInst(Loc, Operand, FieldNo, ResultTy));
  }
  
  ArchetypeMethodInst *createArchetypeMethod(SILLocation Loc, Value Operand,
                                             SILConstant Member, SILType MethodTy)
  {
    return insert(new ArchetypeMethodInst(Loc, Operand, Member, MethodTy, F));
  }
  
  ExistentialMethodInst *createExistentialMethod(SILLocation Loc,
                                                 Value Operand,
                                                 SILConstant Member,
                                                 SILType MethodTy) {
    return insert(new ExistentialMethodInst(Loc, Operand, Member, MethodTy, F));
  }
  
  ProjectExistentialInst *createProjectExistential(SILLocation Loc,
                                                   Value Operand) {
    return insert(new ProjectExistentialInst(Loc, Operand, F));
  }
  
  AllocExistentialInst *createAllocExistential(SILLocation Loc,
                                               Value Existential,
                                               SILType ConcreteType) {
    return insert(new AllocExistentialInst(Loc, Existential, ConcreteType, F));
  }
  
  MetatypeInst *createMetatype(SILLocation Loc, SILType Metatype) {
    return insert(new MetatypeInst(Loc, Metatype));
  }
  
  void createRetain(SILLocation Loc, Value Operand) {
    // Retaining a constant_ref is a no-op.
    if (!isa<ConstantRefInst>(Operand))
      insert(new RetainInst(Loc, Operand));
  }
  void createRelease(SILLocation Loc, Value Operand) {
    // Releasing a constant_ref is a no-op.
    if (!isa<ConstantRefInst>(Operand))
      insert(new ReleaseInst(Loc, Operand));
  }
  DeallocVarInst *createDeallocVar(SILLocation loc, AllocKind allocKind,
                                   Value operand) {
    return insert(new DeallocVarInst(loc, allocKind, operand));
  }
  DestroyAddrInst *createDestroyAddr(SILLocation Loc, Value Operand) {
    return insert(new DestroyAddrInst(Loc, Operand));
  }

  //===--------------------------------------------------------------------===//
  // SIL-only instructions that don't have an AST analog
  //===--------------------------------------------------------------------===//

  IndexAddrInst *createIndexAddr(Expr *E, Value Operand, unsigned Index) {
    return insert(new IndexAddrInst(E, Operand, Index));
  }

  IntegerValueInst *createIntegerValueInst(uint64_t Val, SILType Ty) {
    return insert(new IntegerValueInst(Val, Ty));
  }


  //===--------------------------------------------------------------------===//
  // Terminator Instruction Creation Methods
  //===--------------------------------------------------------------------===//

  UnreachableInst *createUnreachable() {
    return insertTerminator(new UnreachableInst(F));
  }

  ReturnInst *createReturn(SILLocation Loc, Value ReturnValue) {
    return insertTerminator(new ReturnInst(Loc, ReturnValue));
  }
  
  CondBranchInst *createCondBranch(SILLocation Loc, Value Cond,
                                   BasicBlock *Target1, BasicBlock *Target2) {
    return insertTerminator(new CondBranchInst(Loc, Cond, Target1, Target2));
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
