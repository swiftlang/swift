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

#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"

namespace swift {

class SILBuilder {
  /// BB - If this is non-null, the instruction is inserted in the specified
  /// basic block, at the specified InsertPt.  If null, created instructions
  /// are not auto-inserted.
  SILFunction &F;
  SILBasicBlock *BB;
  SILBasicBlock::iterator InsertPt;
public:

  SILBuilder(SILFunction &F) : F(F), BB(0) {}

  SILBuilder(SILInstruction *I, SILFunction &F) : F(F) {
    setInsertionPoint(I);
  }

  SILBuilder(SILBasicBlock *BB, SILFunction &F) : F(F) {
    setInsertionPoint(BB);
  }

  SILBuilder(SILBasicBlock *BB, SILBasicBlock::iterator InsertPt,
             SILFunction &F) : F(F) {
    setInsertionPoint(BB, InsertPt);
  }

  //===--------------------------------------------------------------------===//
  // Insertion Point Management
  //===--------------------------------------------------------------------===//

  bool hasValidInsertionPoint() const { return BB != nullptr; }
  SILBasicBlock *getInsertionBB() { return BB; }
  SILBasicBlock::iterator getInsertionPoint() { return InsertPt; }
  
  /// clearInsertionPoint - Clear the insertion point: created instructions will
  /// not be inserted into a block.
  void clearInsertionPoint() {
    BB = nullptr;
  }

  /// setInsertionPoint - Set the insertion point.
  void setInsertionPoint(SILBasicBlock *BB, SILBasicBlock::iterator InsertPt) {
    this->BB = BB;
    this->InsertPt = InsertPt;
  }

  /// setInsertionPoint - Set the insertion point to insert before the specified
  /// instruction.
  void setInsertionPoint(SILInstruction *I) {
    setInsertionPoint(I->getParent(), I);
  }

  /// setInsertionPoint - Set the insertion point to insert at the end of the
  /// specified block.
  void setInsertionPoint(SILBasicBlock *BB) {
    setInsertionPoint(BB, BB->end());
  }
  
  SILBasicBlock *getInsertionPoint() const {
    return BB;
  }

  /// emitBlock - Each basic block is individually new'd then emitted with
  /// this function.  Since each block is implicitly added to the Function's
  /// list of blocks when created, the construction order is not particularly
  /// useful.
  ///
  /// Instead, we want blocks to end up in the order that they are *emitted*.
  /// The cheapest way to ensure this is to just move each block to the end of
  /// the block list when emitted: as later blocks are emitted, they'll be moved
  /// after this, giving us a block list order that matches emission order when
  /// the function is done.
  ///
  /// This function also sets the insertion point of the builder to be the newly
  /// emitted block.
  void emitBlock(SILBasicBlock *BB) {
    SILFunction *F = BB->getParent();
    // If this is a fall through into BB, emit the fall through branch.
    if (hasValidInsertionPoint()) {
      assert(BB->bbarg_empty() && "cannot fall through to bb with args");
      createBranch(SILLocation(), BB);
    }
    
    // Start inserting into that block.
    setInsertionPoint(BB);
    
    // Move block to the end of the list.
    if (&F->getBlocks().back() != BB)
      F->getBlocks().splice(F->end(), F->getBlocks(), BB);
  }
  
  //===--------------------------------------------------------------------===//
  // SILInstruction Creation Methods
  //===--------------------------------------------------------------------===//

  AllocVarInst *createAllocVar(SILLocation Loc, AllocKind allocKind,
                               SILType elementType) {
    return insert(new AllocVarInst(Loc, allocKind, elementType, F));
  }

  AllocRefInst *createAllocRef(SILLocation Loc, AllocKind allocKind,
                               SILType elementType) {
    return insert(new AllocRefInst(Loc, allocKind, elementType, F));
  }
  
  AllocBoxInst *createAllocBox(SILLocation Loc, SILType ElementType) {
    return insert(new AllocBoxInst(Loc, ElementType, F));
  }

  AllocArrayInst *createAllocArray(SILLocation Loc, SILType ElementType,
                                   SILValue NumElements) {
    return insert(new AllocArrayInst(Loc, ElementType, NumElements, F));
  }

  ApplyInst *createApply(SILLocation Loc, SILValue Fn,
                         SILType Result, ArrayRef<SILValue> Args) {
    return insert(ApplyInst::create(Loc, Fn, Result, Args, F));
  }

  PartialApplyInst *createPartialApply(SILLocation Loc, SILValue Fn,
                                       ArrayRef<SILValue> Args,
                                       SILType ClosureTy) {
    return insert(PartialApplyInst::create(Loc, Fn, Args, ClosureTy, F));
  }

  BuiltinFunctionRefInst *createBuiltinFunctionRef(SILLocation loc,
                                                   FuncDecl *f,
                                                   SILType ty) {
    return insert(new BuiltinFunctionRefInst(loc, f, ty));
  }
  FunctionRefInst *createFunctionRef(SILLocation loc, SILFunction *f) {
    return insert(new FunctionRefInst(loc, f));
  }
  GlobalAddrInst *createGlobalAddr(SILLocation loc, VarDecl *g, SILType ty) {
    return insert(new GlobalAddrInst(loc, g, ty));
  }

  IntegerLiteralInst *createIntegerLiteral(IntegerLiteralExpr *E) {
    return insert(IntegerLiteralInst::create(E, F));
  }
  IntegerLiteralInst *createIntegerLiteral(CharacterLiteralExpr *E) {
    return insert(IntegerLiteralInst::create(E, F));
  }
  IntegerLiteralInst *createIntegerLiteral(SILLocation Loc, SILType Ty,
                                           StringRef Text) {
    return insert(IntegerLiteralInst::create(Loc, Ty, Text, F));
  }
  IntegerLiteralInst *createIntegerLiteral(SILLocation Loc, SILType Ty,
                                           intmax_t Value) {
    return insert(IntegerLiteralInst::create(Loc, Ty, Value, F));
  }

  FloatLiteralInst *createFloatLiteral(FloatLiteralExpr *E) {
    return insert(FloatLiteralInst::create(E, F));
  }
  FloatLiteralInst *createFloatLiteral(SILLocation Loc, SILType Ty,
                                       StringRef Text) {
    return insert(FloatLiteralInst::create(Loc, Ty, Text, F));
  }
  
  StringLiteralInst *createStringLiteral(StringLiteralExpr *E, SILType Ty) {
    return insert(StringLiteralInst::create(E, Ty, F));
  }
  StringLiteralInst *createStringLiteral(SILLocation Loc, SILType Ty,
                                         StringRef Text) {
    return insert(StringLiteralInst::create(Loc, Ty, Text, F));
  }

  LoadInst *createLoad(SILLocation Loc, SILValue LV) {
    return insert(new LoadInst(Loc, LV));
  }

  StoreInst *createStore(SILLocation Loc, SILValue Src, SILValue DestLValue) {
    return insert(new StoreInst(Loc, Src, DestLValue));
  }

  InitializeVarInst *createInitializeVar(SILLocation Loc,
                                         SILValue DestLValue,
                                         bool canDefaultConstruct) {
    return insert(new InitializeVarInst(Loc, DestLValue, canDefaultConstruct));
  }

  CopyAddrInst *createCopyAddr(SILLocation Loc,
                               SILValue SrcLValue, SILValue DestLValue,
                               bool isTake,
                               bool isInitialize) {
    return insert(new CopyAddrInst(Loc, SrcLValue, DestLValue,
                                   isTake, isInitialize));
  }
  
  SpecializeInst *createSpecialize(SILLocation Loc, SILValue Operand,
                                   ArrayRef<Substitution> Substitutions,
                                   SILType DestTy) {
    return insert(SpecializeInst::create(Loc, Operand, Substitutions, DestTy,
                                         F));
  }

  ConvertFunctionInst *createConvertFunction(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new ConvertFunctionInst(Loc, Op, Ty));
  }

  CoerceInst *createCoerce(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new CoerceInst(Loc, Op, Ty));
  }
  
  UpcastInst *createUpcast(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new UpcastInst(Loc, Op, Ty));
  }
  
  DowncastInst *createDowncast(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new DowncastInst(Loc, Op, Ty));
  }
  
  AddressToPointerInst *createAddressToPointer(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(new AddressToPointerInst(Loc, Op, Ty));
  }
  
  PointerToAddressInst *createPointerToAddress(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(new PointerToAddressInst(Loc, Op, Ty));
  }
  
  RefToObjectPointerInst *createRefToObjectPointer(SILLocation Loc, SILValue Op,
                                                   SILType Ty) {
    return insert(new RefToObjectPointerInst(Loc, Op, Ty));
  }
  
  ObjectPointerToRefInst *createObjectPointerToRef(SILLocation Loc, SILValue Op,
                                                   SILType Ty) {
    return insert(new ObjectPointerToRefInst(Loc, Op, Ty));
  }
  
  RefToRawPointerInst *createRefToRawPointer(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new RefToRawPointerInst(Loc, Op, Ty));
  }
  
  RawPointerToRefInst *createRawPointerToRef(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new RawPointerToRefInst(Loc, Op, Ty));
  }

  ConvertCCInst *createConvertCC(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new ConvertCCInst(Loc, Op, Ty));
  }

  ThinToThickFunctionInst *createThinToThickFunction(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(new ThinToThickFunctionInst(Loc, Op, Ty));
  }

  BridgeToBlockInst *createBridgeToBlock(SILLocation Loc,
                                         SILValue Op, SILType Ty) {
    return insert(new BridgeToBlockInst(Loc, Op, Ty));
  }

  ArchetypeToSuperInst *createArchetypeToSuper(SILLocation Loc,
                                               SILValue Archetype,
                                               SILType BaseTy) {
    return insert(new ArchetypeToSuperInst(Loc, Archetype, BaseTy));
  }
  
  SuperToArchetypeInst *createSuperToArchetype(SILLocation Loc,
                                               SILValue SrcBase,
                                               SILValue DestArchetypeAddr) {
    return insert(new SuperToArchetypeInst(Loc, SrcBase, DestArchetypeAddr));
  }
  
  IsaInst *createIsa(SILLocation Loc,
                     SILValue Operand,
                     SILType TestType,
                     SILType ResultType) {
    return insert(new IsaInst(Loc, Operand, TestType, ResultType));
  }

  StructInst *createStruct(SILLocation Loc, SILType Ty,
                           ArrayRef<SILValue> Elements){
    return insert(StructInst::create(Loc, Ty, Elements, F));
  }

  TupleInst *createTuple(SILLocation Loc, SILType Ty,
                         ArrayRef<SILValue> Elements){
    return insert(TupleInst::create(Loc, Ty, Elements, F));
  }
  
  SILValue createExtract(SILLocation Loc, SILValue Operand, unsigned FieldNo,
                           SILType ResultTy) {
    // Fold extract(tuple(a,b,c), 1) -> b.
    if (TupleInst *TI = dyn_cast<TupleInst>(Operand))
      return TI->getElements()[FieldNo];

    return insert(new ExtractInst(Loc, Operand, FieldNo, ResultTy));
  }

  SILValue createElementAddr(SILLocation Loc, SILValue Operand,
                             unsigned FieldNo, SILType ResultTy) {
    return insert(new ElementAddrInst(Loc, Operand, FieldNo, ResultTy));
  }
  
  SILValue createRefElementAddr(SILLocation Loc, SILValue Operand,
                                VarDecl *Field, SILType ResultTy) {
    return insert(new RefElementAddrInst(Loc, Operand, Field, ResultTy));
  }
  
  ClassMethodInst *createClassMethod(SILLocation Loc, SILValue Operand,
                                     SILConstant Member,
                                     SILType MethodTy)
  {
    return insert(new ClassMethodInst(Loc, Operand, Member, MethodTy));
  }
  
  SuperMethodInst *createSuperMethod(SILLocation Loc, SILValue Operand,
                                     SILConstant Member,
                                     SILType MethodTy)
  {
    return insert(new SuperMethodInst(Loc, Operand, Member, MethodTy));
  }
  
  ArchetypeMethodInst *createArchetypeMethod(SILLocation Loc,
                                             SILType LookupTy,
                                             SILConstant Member,
                                             SILType MethodTy)
  {
    return insert(new ArchetypeMethodInst(Loc, LookupTy, Member, MethodTy));
  }
  
  ProtocolMethodInst *createProtocolMethod(SILLocation Loc,
                                                 SILValue Operand,
                                                 SILConstant Member,
                                                 SILType MethodTy) {
    return insert(new ProtocolMethodInst(Loc, Operand, Member, MethodTy));
  }
  
  ProjectExistentialInst *createProjectExistential(SILLocation Loc,
                                                   SILValue Operand) {
    return insert(new ProjectExistentialInst(Loc, Operand, F));
  }
  
  InitExistentialInst *createInitExistential(SILLocation Loc,
                                 SILValue Existential,
                                 SILType ConcreteType,
                                 ArrayRef<ProtocolConformance*> Conformances) {
    return insert(new InitExistentialInst(Loc,
                                          Existential,
                                          ConcreteType,
                                          Conformances));
  }
  
  UpcastExistentialInst *createUpcastExistential(SILLocation Loc,
                                 SILValue SrcExistential,
                                 SILValue DestExistential,
                                 bool isTakeOfSrc,
                                 ArrayRef<ProtocolConformance*> Conformances) {
    return insert(new UpcastExistentialInst(Loc,
                                            SrcExistential,
                                            DestExistential,
                                            isTakeOfSrc,
                                            Conformances));
  }
  
  DeinitExistentialInst *createDeinitExistential(SILLocation Loc,
                                                 SILValue Existential) {
    return insert(new DeinitExistentialInst(Loc, Existential));
  }
  
  BuiltinZeroInst *createBuiltinZero(SILLocation Loc, SILType Type) {
    return insert(new BuiltinZeroInst(Loc, Type));
  }
  
  MetatypeInst *createMetatype(SILLocation Loc, SILType Metatype) {
    return insert(new MetatypeInst(Loc, Metatype));
  }

  ClassMetatypeInst *createClassMetatype(SILLocation Loc, SILType Metatype,
                                         SILValue Base) {
    return insert(new ClassMetatypeInst(Loc, Metatype, Base));
  }

  ArchetypeMetatypeInst *createArchetypeMetatype(SILLocation Loc,
                                                 SILType Metatype,
                                                 SILValue Base) {
    return insert(new ArchetypeMetatypeInst(Loc, Metatype, Base));
  }
  
  ProtocolMetatypeInst *createProtocolMetatype(SILLocation Loc,
                                               SILType Metatype,
                                               SILValue Base) {
    return insert(new ProtocolMetatypeInst(Loc, Metatype, Base));
  }
  
  ModuleInst *createModule(SILLocation Loc, SILType ModuleType) {
    return insert(new ModuleInst(Loc, ModuleType));
  }
  
  AssociatedMetatypeInst *createAssociatedMetatype(SILLocation Loc,
                                                   SILValue MetatypeSrc,
                                                   SILType MetatypeDest) {
    return insert(new AssociatedMetatypeInst(Loc, MetatypeSrc, MetatypeDest));
  }
  
  void createRetain(SILLocation Loc, SILValue Operand) {
    // Retaining a function_ref is a no-op.
    if (!isa<FunctionRefInst>(Operand))
      insert(new RetainInst(Loc, Operand));
  }
  void createRelease(SILLocation Loc, SILValue Operand) {
    // Releasing a function_ref is a no-op.
    if (!isa<FunctionRefInst>(Operand))
      insert(new ReleaseInst(Loc, Operand));
  }
  void createRetainAutoreleased(SILLocation Loc, SILValue Operand) {
    insert(new RetainAutoreleasedInst(Loc, Operand));
  }
  DeallocVarInst *createDeallocVar(SILLocation loc, AllocKind allocKind,
                                   SILValue operand) {
    return insert(new DeallocVarInst(loc, allocKind, operand));
  }
  DeallocRefInst *createDeallocRef(SILLocation loc, SILValue operand) {
    return insert(new DeallocRefInst(loc, operand));
  }
  DestroyAddrInst *createDestroyAddr(SILLocation Loc, SILValue Operand) {
    return insert(new DestroyAddrInst(Loc, Operand));
  }

  //===--------------------------------------------------------------------===//
  // SIL-only instructions that don't have an AST analog
  //===--------------------------------------------------------------------===//

  IndexAddrInst *createIndexAddr(SILLocation loc, SILValue Operand,
                                 unsigned Index) {
    return insert(new IndexAddrInst(loc, Operand, Index));
  }

  //===--------------------------------------------------------------------===//
  // Terminator SILInstruction Creation Methods
  //===--------------------------------------------------------------------===//

  UnreachableInst *createUnreachable() {
    return insertTerminator(new UnreachableInst(F));
  }

  ReturnInst *createReturn(SILLocation Loc, SILValue ReturnValue) {
    return insertTerminator(new ReturnInst(Loc, ReturnValue));
  }

  AutoreleaseReturnInst *createAutoreleaseReturn(SILLocation Loc,
                                                 SILValue ReturnValue) {
    return insertTerminator(new AutoreleaseReturnInst(Loc, ReturnValue));
  }
  
  CondBranchInst *createCondBranch(SILLocation Loc, SILValue Cond,
                                   SILBasicBlock *Target1,
                                   SILBasicBlock *Target2) {
    return insertTerminator(CondBranchInst::create(Loc, Cond,
                                                   Target1, Target2, F));
  }
    
  CondBranchInst *createCondBranch(SILLocation Loc, SILValue Cond,
                                   SILBasicBlock *Target1,
                                   ArrayRef<SILValue> Args1,
                                   SILBasicBlock *Target2,
                                   ArrayRef<SILValue> Args2) {
    return insertTerminator(CondBranchInst::create(Loc, Cond,
                                                   Target1, Args1,
                                                   Target2, Args2,
                                                   F));
  }
  
  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock) {
    return insertTerminator(BranchInst::create(Loc, TargetBlock, F));
  }

  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock,
                           ArrayRef<SILValue> Args) {
    return insertTerminator(BranchInst::create(Loc, TargetBlock, Args, F));
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

  void insertImpl(SILInstruction *TheInst) {
    if (BB == 0) return;
    BB->getInsts().insert(InsertPt, TheInst);
  }
};

} // end swift namespace

#endif
