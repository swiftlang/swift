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

#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"

namespace swift {

class SILDebugScope;

class SILBuilder {
  /// BB - If this is non-null, the instruction is inserted in the specified
  /// basic block, at the specified InsertPt.  If null, created instructions
  /// are not auto-inserted.
  SILFunction &F;
  SILBasicBlock *BB;
  SILBasicBlock::iterator InsertPt;
  /// Keep track of our current nested scope.
  std::vector<SILDebugScope*> DebugScopeStack;
public:
  SILBuilder(SILFunction &F) : F(F), BB(0) {}

  explicit SILBuilder(SILInstruction *I)
    : F(*I->getParent()->getParent()) {
    setInsertionPoint(I);
  }

  explicit SILBuilder(SILBasicBlock *BB) : F(*BB->getParent()) {
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

  /// enterDebugScope - Push a new debug scope and set its parent pointer.
  void enterDebugScope(SILDebugScope *DS) {
    if (DebugScopeStack.size())
      DS->setParent(DebugScopeStack.back());
    DebugScopeStack.push_back(DS);
  }

  /// enterDebugScope - return to the previous debug scope.
  void leaveDebugScope() {
    assert(DebugScopeStack.size());
    DebugScopeStack.pop_back();
  }

  SILFunction& getFunction() const { return F; }
  
  //===--------------------------------------------------------------------===//
  // SILInstruction Creation Methods
  //===--------------------------------------------------------------------===//

  AllocVarInst *createAllocVar(SILLocation Loc, AllocKind allocKind,
                               SILType elementType) {
    return insert(new (F.getModule())
                    AllocVarInst(Loc, allocKind, elementType, F));
  }

  AllocRefInst *createAllocRef(SILLocation Loc, AllocKind allocKind,
                               SILType elementType) {
    return insert(new (F.getModule())
                    AllocRefInst(Loc, allocKind, elementType, F));
  }
  
  AllocBoxInst *createAllocBox(SILLocation Loc, SILType ElementType) {
    return insert(new (F.getModule())
                    AllocBoxInst(Loc, ElementType, F));
  }

  AllocArrayInst *createAllocArray(SILLocation Loc, SILType ElementType,
                                   SILValue NumElements) {
    return insert(new (F.getModule())
                    AllocArrayInst(Loc, ElementType, NumElements, F));
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
    return insert(new (F.getModule())
                    BuiltinFunctionRefInst(loc, f, ty));
  }
  FunctionRefInst *createFunctionRef(SILLocation loc, SILFunction *f) {
    return insert(new (F.getModule())
                    FunctionRefInst(loc, f));
  }
  GlobalAddrInst *createGlobalAddr(SILLocation loc, VarDecl *g, SILType ty) {
    return insert(new (F.getModule())
                    GlobalAddrInst(loc, g, ty));
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
    return insert(new (F.getModule())
                    LoadInst(Loc, LV));
  }

  StoreInst *createStore(SILLocation Loc, SILValue Src, SILValue DestLValue) {
    return insert(new (F.getModule())
                    StoreInst(Loc, Src, DestLValue));
  }

  InitializeVarInst *createInitializeVar(SILLocation Loc,
                                         SILValue DestLValue,
                                         bool canDefaultConstruct) {
    return insert(new (F.getModule())
                    InitializeVarInst(Loc, DestLValue, canDefaultConstruct));
  }

  CopyAddrInst *createCopyAddr(SILLocation Loc,
                               SILValue SrcLValue, SILValue DestLValue,
                               bool isTake,
                               bool isInitialize) {
    return insert(new (F.getModule())
                    CopyAddrInst(Loc, SrcLValue, DestLValue,
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
    return insert(new (F.getModule())
                    ConvertFunctionInst(Loc, Op, Ty));
  }

  CoerceInst *createCoerce(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule())
                    CoerceInst(Loc, Op, Ty));
  }
  
  UpcastInst *createUpcast(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule())
                    UpcastInst(Loc, Op, Ty));
  }
  
  AddressToPointerInst *createAddressToPointer(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(new (F.getModule())
                    AddressToPointerInst(Loc, Op, Ty));
  }
  
  PointerToAddressInst *createPointerToAddress(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(new (F.getModule())
                    PointerToAddressInst(Loc, Op, Ty));
  }
  
  RefToObjectPointerInst *createRefToObjectPointer(SILLocation Loc, SILValue Op,
                                                   SILType Ty) {
    return insert(new (F.getModule())
                    RefToObjectPointerInst(Loc, Op, Ty));
  }
  
  ObjectPointerToRefInst *createObjectPointerToRef(SILLocation Loc, SILValue Op,
                                                   SILType Ty) {
    return insert(new (F.getModule())
                    ObjectPointerToRefInst(Loc, Op, Ty));
  }
  
  RefToRawPointerInst *createRefToRawPointer(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new (F.getModule())
                    RefToRawPointerInst(Loc, Op, Ty));
  }
  
  RawPointerToRefInst *createRawPointerToRef(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new (F.getModule())
                    RawPointerToRefInst(Loc, Op, Ty));
  }

  ConvertCCInst *createConvertCC(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule())
                    ConvertCCInst(Loc, Op, Ty));
  }

  ThinToThickFunctionInst *createThinToThickFunction(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(new (F.getModule())
                    ThinToThickFunctionInst(Loc, Op, Ty));
  }

  BridgeToBlockInst *createBridgeToBlock(SILLocation Loc,
                                         SILValue Op, SILType Ty) {
    return insert(new (F.getModule())
                    BridgeToBlockInst(Loc, Op, Ty));
  }

  ArchetypeRefToSuperInst *createArchetypeRefToSuper(SILLocation Loc,
                                                     SILValue Archetype,
                                                     SILType BaseTy) {
    return insert(new (F.getModule())
                    ArchetypeRefToSuperInst(Loc, Archetype, BaseTy));
  }
  
  DowncastInst *createDowncast(SILLocation Loc, SILValue Op, SILType Ty,
                               CheckedCastMode Mode) {
    return insert(new (F.getModule())
                  DowncastInst(Loc, Op, Ty, Mode));
  }
  
  SuperToArchetypeRefInst *createSuperToArchetypeRef(SILLocation Loc,
                                                     SILValue Archetype,
                                                     SILType BaseTy,
                                                     CheckedCastMode Mode) {
    return insert(new (F.getModule())
                    SuperToArchetypeRefInst(Loc, Archetype, BaseTy, Mode));
  }
  
  DowncastArchetypeAddrInst *createDowncastArchetypeAddr(SILLocation Loc,
                                                         SILValue Archetype,
                                                         SILType Ty,
                                                         CheckedCastMode Mode) {
    return insert(new (F.getModule())
                    DowncastArchetypeAddrInst(Loc, Archetype, Ty, Mode));
  }
  DowncastArchetypeRefInst *createDowncastArchetypeRef(SILLocation Loc,
                                                       SILValue Archetype,
                                                       SILType Ty,
                                                       CheckedCastMode Mode) {
    return insert(new (F.getModule())
                    DowncastArchetypeRefInst(Loc, Archetype, Ty, Mode));
  }
  ProjectDowncastExistentialAddrInst *createProjectDowncastExistentialAddr(
                                                         SILLocation Loc,
                                                         SILValue Existential,
                                                         SILType Ty,
                                                         CheckedCastMode Mode) {
    return insert(new (F.getModule())
              ProjectDowncastExistentialAddrInst(Loc, Existential, Ty, Mode));
  }
  DowncastExistentialRefInst *createDowncastExistentialRef(SILLocation Loc,
                                                       SILValue Existential,
                                                       SILType Ty,
                                                       CheckedCastMode Mode) {
    return insert(new (F.getModule())
                  DowncastExistentialRefInst(Loc, Existential, Ty, Mode));
  }
  
  IsNonnullInst *createIsNonnull(SILLocation Loc,
                           SILValue Operand,
                           SILType ResultType) {
    return insert(new (F.getModule())
                    IsNonnullInst(Loc, Operand, ResultType));
  }

  StructInst *createStruct(SILLocation Loc, SILType Ty,
                           ArrayRef<SILValue> Elements){
    return insert(StructInst::create(Loc, Ty, Elements, F));
  }

  TupleInst *createTuple(SILLocation Loc, SILType Ty,
                         ArrayRef<SILValue> Elements){
    return insert(TupleInst::create(Loc, Ty, Elements, F));
  }
  
  SILValue createTupleExtract(SILLocation Loc,
                              SILValue Operand,
                              unsigned FieldNo,
                              SILType ResultTy) {
    // Fold extract(tuple(a,b,c), 1) -> b.
    if (TupleInst *TI = dyn_cast<TupleInst>(Operand))
      return TI->getElements()[FieldNo];

    return insert(new (F.getModule())
                    TupleExtractInst(Loc, Operand, FieldNo, ResultTy));
  }

  TupleElementAddrInst *createTupleElementAddr(SILLocation Loc,
                                               SILValue Operand,
                                               unsigned FieldNo,
                                               SILType ResultTy) {
    return insert(new (F.getModule())
                    TupleElementAddrInst(Loc, Operand, FieldNo, ResultTy));
  }
  
  StructExtractInst *createStructExtract(SILLocation Loc,
                                         SILValue Operand,
                                         VarDecl *Field,
                                         SILType ResultTy) {
    return insert(new (F.getModule())
                    StructExtractInst(Loc, Operand, Field, ResultTy));
  }
  
  StructElementAddrInst *createStructElementAddr(SILLocation Loc,
                                                 SILValue Operand,
                                                 VarDecl *Field,
                                                 SILType ResultTy) {
    return insert(new (F.getModule())
                    StructElementAddrInst(Loc, Operand, Field, ResultTy));
  }
  
  SILValue createRefElementAddr(SILLocation Loc, SILValue Operand,
                                VarDecl *Field, SILType ResultTy) {
    return insert(new (F.getModule())
                    RefElementAddrInst(Loc, Operand, Field, ResultTy));
  }
  
  ClassMethodInst *createClassMethod(SILLocation Loc, SILValue Operand,
                                     SILConstant Member,
                                     SILType MethodTy,
                                     bool Volatile = false)
  {
    return insert(new (F.getModule())
                    ClassMethodInst(Loc, Operand, Member, MethodTy, Volatile));
  }
  
  SuperMethodInst *createSuperMethod(SILLocation Loc, SILValue Operand,
                                     SILConstant Member,
                                     SILType MethodTy,
                                     bool Volatile = false)
  {
    return insert(new (F.getModule())
                    SuperMethodInst(Loc, Operand, Member, MethodTy, Volatile));
  }
  
  ArchetypeMethodInst *createArchetypeMethod(SILLocation Loc,
                                             SILType LookupTy,
                                             SILConstant Member,
                                             SILType MethodTy,
                                             bool Volatile = false)
  {
    return insert(new (F.getModule())
                    ArchetypeMethodInst(Loc, LookupTy, Member, MethodTy,
                                        Volatile));
  }
  
  ProtocolMethodInst *createProtocolMethod(SILLocation Loc,
                                           SILValue Operand,
                                           SILConstant Member,
                                           SILType MethodTy,
                                           bool Volatile = false) {
    return insert(new (F.getModule())
                    ProtocolMethodInst(Loc, Operand, Member, MethodTy,
                                       Volatile));
  }
  
  ProjectExistentialInst *createProjectExistential(SILLocation Loc,
                                                   SILValue Operand) {
    return insert(new (F.getModule())
                    ProjectExistentialInst(Loc, Operand, F));
  }
  
  ProjectExistentialRefInst *createProjectExistentialRef(SILLocation Loc,
                                                         SILValue Operand) {
    return insert(new (F.getModule())
                    ProjectExistentialRefInst(Loc, Operand, F));
  }
  
  InitExistentialInst *createInitExistential(SILLocation Loc,
                                 SILValue Existential,
                                 SILType ConcreteType,
                                 ArrayRef<ProtocolConformance*> Conformances) {
    return insert(new (F.getModule())
                    InitExistentialInst(Loc,
                                          Existential,
                                          ConcreteType,
                                          Conformances));
  }
  
  InitExistentialRefInst *createInitExistentialRef(SILLocation Loc,
                                 SILType ExistentialType,
                                 SILValue Concrete,
                                 ArrayRef<ProtocolConformance*> Conformances) {
    return insert(new (F.getModule())
                    InitExistentialRefInst(Loc,
                                           ExistentialType,
                                           Concrete,
                                           Conformances));
  }
  
  UpcastExistentialInst *createUpcastExistential(SILLocation Loc,
                                 SILValue SrcExistential,
                                 SILValue DestExistential,
                                 bool isTakeOfSrc) {
    return insert(new (F.getModule())
                    UpcastExistentialInst(Loc,
                                          SrcExistential,
                                          DestExistential,
                                          isTakeOfSrc));
  }
  
  UpcastExistentialRefInst *createUpcastExistentialRef(SILLocation Loc,
                                 SILValue Operand,
                                 SILType Ty) {
    return insert(new (F.getModule())
                    UpcastExistentialRefInst(Loc, Operand, Ty));
  }
  
  DeinitExistentialInst *createDeinitExistential(SILLocation Loc,
                                                 SILValue Existential) {
    return insert(new (F.getModule())
                    DeinitExistentialInst(Loc, Existential));
  }
  
  BuiltinZeroInst *createBuiltinZero(SILLocation Loc, SILType Type) {
    return insert(new (F.getModule())
                    BuiltinZeroInst(Loc, Type));
  }
  
  MetatypeInst *createMetatype(SILLocation Loc, SILType Metatype) {
    return insert(new (F.getModule())
                    MetatypeInst(Loc, Metatype));
  }

  ClassMetatypeInst *createClassMetatype(SILLocation Loc, SILType Metatype,
                                         SILValue Base) {
    return insert(new (F.getModule())
                    ClassMetatypeInst(Loc, Metatype, Base));
  }

  ArchetypeMetatypeInst *createArchetypeMetatype(SILLocation Loc,
                                                 SILType Metatype,
                                                 SILValue Base) {
    return insert(new (F.getModule())
                    ArchetypeMetatypeInst(Loc, Metatype, Base));
  }
  
  ProtocolMetatypeInst *createProtocolMetatype(SILLocation Loc,
                                               SILType Metatype,
                                               SILValue Base) {
    return insert(new (F.getModule())
                    ProtocolMetatypeInst(Loc, Metatype, Base));
  }
  
  ModuleInst *createModule(SILLocation Loc, SILType ModuleType) {
    return insert(new (F.getModule())
                    ModuleInst(Loc, ModuleType));
  }
  
  AssociatedMetatypeInst *createAssociatedMetatype(SILLocation Loc,
                                                   SILValue MetatypeSrc,
                                                   SILType MetatypeDest) {
    return insert(new (F.getModule())
                    AssociatedMetatypeInst(Loc, MetatypeSrc, MetatypeDest));
  }
  
  void createRetain(SILLocation Loc, SILValue Operand) {
    // Retaining a function_ref is a no-op.
    if (!isa<FunctionRefInst>(Operand))
      createRetainInst(Loc, Operand);
  }
  RetainInst *createRetainInst(SILLocation Loc, SILValue Operand) {
    // Retaining a function_ref is a no-op.
    return insert(new (F.getModule())
                    RetainInst(Loc, Operand));
  }
  void createRelease(SILLocation Loc, SILValue Operand) {
    // Releasing a function_ref is a no-op.
    if (!isa<FunctionRefInst>(Operand))
      createReleaseInst(Loc, Operand);
  }
  ReleaseInst *createReleaseInst(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                    ReleaseInst(Loc, Operand));
  }
  RetainAutoreleasedInst *
  createRetainAutoreleased(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                    RetainAutoreleasedInst(Loc, Operand));
  }
  DeallocVarInst *createDeallocVar(SILLocation loc, AllocKind allocKind,
                                   SILValue operand) {
    return insert(new (F.getModule())
                    DeallocVarInst(loc, allocKind, operand));
  }
  DeallocRefInst *createDeallocRef(SILLocation loc, SILValue operand) {
    return insert(new (F.getModule())
                    DeallocRefInst(loc, operand));
  }
  DestroyAddrInst *createDestroyAddr(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                    DestroyAddrInst(Loc, Operand));
  }

  //===--------------------------------------------------------------------===//
  // Array indexing instructions
  //===--------------------------------------------------------------------===//

  IndexAddrInst *createIndexAddr(SILLocation loc,
                                 SILValue Operand, SILValue Index) {
    return insert(new (F.getModule()) IndexAddrInst(loc, Operand, Index));
  }

  IndexRawPointerInst *createIndexRawPointer(SILLocation loc,
                                             SILValue Operand, SILValue Index) {
    return insert(new (F.getModule()) IndexRawPointerInst(loc, Operand, Index));
  }
  
  //===--------------------------------------------------------------------===//
  // Terminator SILInstruction Creation Methods
  //===--------------------------------------------------------------------===//

  UnreachableInst *createUnreachable() {
    return insertTerminator(new (F.getModule()) UnreachableInst(F));
  }

  ReturnInst *createReturn(SILLocation Loc, SILValue ReturnValue) {
    return insertTerminator(new (F.getModule()) ReturnInst(Loc, ReturnValue));
  }

  AutoreleaseReturnInst *createAutoreleaseReturn(SILLocation Loc,
                                                 SILValue ReturnValue) {
    return insertTerminator(new (F.getModule())
                              AutoreleaseReturnInst(Loc, ReturnValue));
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

    if (DebugScopeStack.size())
      TheInst->setDebugScope(DebugScopeStack.back());

    BB->getInsts().insert(InsertPt, TheInst);
  }
};

} // end swift namespace

#endif
