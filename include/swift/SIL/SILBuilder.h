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

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"

namespace swift {

class SILBuilder {
  /// BB - If this is non-null, the instruction is inserted in the specified
  /// basic block, at the specified InsertPt.  If null, created instructions
  /// are not auto-inserted.
  SILFunction &F;
  SILBasicBlock *BB;
  SILBasicBlock::iterator InsertPt;

  /// InsertedInstrs - If this pointer is non-null, then any inserted
  /// instruction is recorded in this list.
  SmallVectorImpl<SILInstruction*> *InsertedInstrs = nullptr;
public:
  SILBuilder(SILFunction &F) : F(F), BB(0) {}

  static SILType getTupleElementType(SILType Ty, unsigned EltNo);
  static SILType getStructFieldType(VarDecl *Field);

  explicit SILBuilder(SILInstruction *I,
                      SmallVectorImpl<SILInstruction*> *InsertedInstrs = 0)
    : F(*I->getParent()->getParent()), InsertedInstrs(InsertedInstrs) {
    setInsertionPoint(I);
  }

  explicit SILBuilder(SILBasicBlock *BB,
                      SmallVectorImpl<SILInstruction*> *InsertedInstrs = 0)
    : F(*BB->getParent()), InsertedInstrs(InsertedInstrs) {
    setInsertionPoint(BB);
  }

  SILBuilder(SILBasicBlock *BB, SILBasicBlock::iterator InsertPt,
             SmallVectorImpl<SILInstruction*> *InsertedInstrs = 0)
    : F(*BB->getParent()), InsertedInstrs(InsertedInstrs) {
    setInsertionPoint(BB, InsertPt);
  }

  SILFunction &getFunction() const { return F; }

  //===--------------------------------------------------------------------===//
  // Insertion Point Management
  //===--------------------------------------------------------------------===//

  bool hasValidInsertionPoint() const { return BB != nullptr; }
  SILBasicBlock *getInsertionBB() { return BB; }
  SILBasicBlock::iterator getInsertionPoint() { return InsertPt; }

  /// insertingAtEndOfBlock - Return true if the insertion point is at the end
  /// of the current basic block.  False if we're inserting before an existing
  /// instruction.
  bool insertingAtEndOfBlock() const {
    assert(hasValidInsertionPoint() &&
           "Must have insertion point to ask about it");
    return InsertPt == BB->end();
  }
  
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

  //===--------------------------------------------------------------------===//
  // Instruction Tracking
  //===--------------------------------------------------------------------===//

  /// Clients of SILBuilder who want to know about any newly created
  /// instructions can install a SmallVector into the builder to collect them.
  void setTrackingList(SmallVectorImpl<SILInstruction*> *II) {
    InsertedInstrs = II;
  }

  SmallVectorImpl<SILInstruction*> *getTrackingList() {
    return InsertedInstrs;
  }

  //===--------------------------------------------------------------------===//
  // CFG Manipulation
  //===--------------------------------------------------------------------===//

  /// moveBlockToEnd - Move a block to a new position in its function.
  void moveBlockTo(SILBasicBlock *BB, SILFunction::iterator IP) {
    SILFunction *F = BB->getParent();
    auto &Blocks = F->getBlocks();
    Blocks.splice(IP, Blocks, BB);
  }

  /// moveBlockToEnd - Reorder a block to the end of its containing function.
  void moveBlockToEnd(SILBasicBlock *BB) {
    moveBlockTo(BB, BB->getParent()->end());
  }

  /// \brief Move the specified block to the end of the function and reset the
  /// insertion point to point to the first instruction in the emitted block.
  ///
  /// Assumes that no insertion point is currently active.
  void emitBlock(SILBasicBlock *BB);

  /// \brief Move the specified block to the current insertion point (which
  /// is the end of the function if there is no insertion point) and reset the
  /// insertion point to point to the first instruction in the emitted block.
  void emitBlock(SILBasicBlock *BB, SILLocation BranchLoc);

  /// splitBlockForFallthrough - Prepare for the insertion of a terminator.  If
  /// the builder's insertion point is at the end of the current block (as when
  /// SILGen is creating the initial code for a function), just create and
  /// return a new basic block that will be later used for the continue point.
  ///
  /// If the insertion point is valid (i.e., pointing to an existing
  /// instruction) then split the block at that instruction and return the
  /// continuation block.
  SILBasicBlock *splitBlockForFallthrough();

  //===--------------------------------------------------------------------===//
  // SILInstruction Creation Methods
  //===--------------------------------------------------------------------===//

  AllocStackInst *createAllocStack(SILLocation Loc, SILType elementType) {
    return insert(new (F.getModule()) AllocStackInst(Loc, elementType, F));
  }

  AllocRefInst *createAllocRef(SILLocation Loc, SILType elementType) {
    return insert(new (F.getModule()) AllocRefInst(Loc, elementType, F));
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
                         SILType Result, ArrayRef<SILValue> Args,
                         bool Transparent = false) {
    return insert(ApplyInst::create(Loc, Fn, Result, Args, Transparent, F));
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
                                           intmax_t Value) {
    return insert(IntegerLiteralInst::create(Loc, Ty, Value, F));
  }
  IntegerLiteralInst *createIntegerLiteral(SILLocation Loc, SILType Ty,
                                           const APInt &Value) {
    return insert(IntegerLiteralInst::create(Loc, Ty, Value, F));
  }

  FloatLiteralInst *createFloatLiteral(FloatLiteralExpr *E) {
    return insert(FloatLiteralInst::create(E, F));
  }
  FloatLiteralInst *createFloatLiteral(SILLocation Loc, SILType Ty,
                                       const APFloat &Value) {
    return insert(FloatLiteralInst::create(Loc, Ty, Value, F));
  }
  
  StringLiteralInst *createStringLiteral(StringLiteralExpr *E, SILType Ty) {
    return insert(StringLiteralInst::create(E, Ty, F));
  }
  StringLiteralInst *createStringLiteral(SILLocation Loc, SILType Ty,
                                         StringRef Text) {
    return insert(StringLiteralInst::create(Loc, Ty, Text, F));
  }

  LoadInst *createLoad(SILLocation Loc, SILValue LV) {
    return insert(new (F.getModule()) LoadInst(Loc, LV));
  }

  StoreInst *createStore(SILLocation Loc, SILValue Src, SILValue DestLValue) {
    return insert(new (F.getModule()) StoreInst(Loc, Src, DestLValue));
  }
  AssignInst *createAssign(SILLocation Loc, SILValue Src, SILValue DestLValue) {
    return insert(new (F.getModule()) AssignInst(Loc, Src, DestLValue));
  }

  MarkUninitializedInst *createMarkUninitialized(SILLocation loc, SILValue src){
    return insert(new (F.getModule()) MarkUninitializedInst(loc, src));
  }

  MarkFunctionEscapeInst *createMarkFunctionEscape(SILLocation loc,
                                                   ArrayRef<SILValue> vars){
    return insert(MarkFunctionEscapeInst::create(loc, vars, F));
  }

  LoadWeakInst *createLoadWeak(SILLocation loc, SILValue src, IsTake_t isTake) {
    return insert(new (F.getModule()) LoadWeakInst(loc, src, isTake));
  }

  StoreWeakInst *createStoreWeak(SILLocation loc, SILValue value,
                                 SILValue dest, IsInitialization_t isInit) {
    return insert(new (F.getModule()) StoreWeakInst(loc, value, dest, isInit));
  }

  InitializeVarInst *createInitializeVar(SILLocation Loc,
                                         SILValue DestLValue,
                                         bool canDefaultConstruct) {
    return insert(new (F.getModule())
                    InitializeVarInst(Loc, DestLValue, canDefaultConstruct));
  }

  CopyAddrInst *createCopyAddr(SILLocation loc,
                               SILValue srcLValue, SILValue destLValue,
                               IsTake_t isTake,
                               IsInitialization_t isInitialize) {
    return insert(new (F.getModule())
                    CopyAddrInst(loc, srcLValue, destLValue,
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

  UnownedToRefInst *createUnownedToRef(SILLocation loc,
                                       SILValue op, SILType ty) {
    return insert(new (F.getModule()) UnownedToRefInst(loc, op, ty));
  }

  RefToUnownedInst *createRefToUnowned(SILLocation loc,
                                       SILValue op, SILType ty) {
    return insert(new (F.getModule()) RefToUnownedInst(loc, op, ty));
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
  
  IsNonnullInst *createIsNonnull(SILLocation loc,
                                 SILValue operand) {
    return insert(new (F.getModule())
                    IsNonnullInst(loc, operand,
                      SILType::getBuiltinIntegerType(1, F.getASTContext())));
  }

  StructInst *createStruct(SILLocation Loc, SILType Ty,
                           ArrayRef<SILValue> Elements) {
    return insert(StructInst::create(Loc, Ty, Elements, F));
  }

  TupleInst *createTuple(SILLocation Loc, SILType Ty,
                         ArrayRef<SILValue> Elements) {
    return insert(TupleInst::create(Loc, Ty, Elements, F));
  }
  
  UnionInst *createUnion(SILLocation Loc, SILValue Operand,
                         UnionElementDecl *Element, SILType Ty) {
    return insert(new (F.getModule()) UnionInst(Loc, Operand, Element, Ty));
  }
  
  UnionDataAddrInst *createUnionDataAddr(SILLocation Loc, SILValue Operand,
                                         UnionElementDecl *Element, SILType Ty){
    return insert(
              new (F.getModule()) UnionDataAddrInst(Loc, Operand, Element, Ty));
  }
  
  InjectUnionAddrInst *createInjectUnionAddr(SILLocation Loc, SILValue Operand,
                                             UnionElementDecl *Element) {
    return insert(new (F.getModule())
                    InjectUnionAddrInst(Loc, Operand, Element));
  }
  
  SILValue createTupleExtract(SILLocation Loc, SILValue Operand,
                              unsigned FieldNo, SILType ResultTy) {
    return insert(new (F.getModule())
                    TupleExtractInst(Loc, Operand, FieldNo, ResultTy));
  }

  SILValue createTupleExtract(SILLocation Loc, SILValue Operand,
                              unsigned FieldNo) {
    return createTupleExtract(Loc, Operand, FieldNo,
                              getTupleElementType(Operand.getType(), FieldNo));
  }

  TupleExtractInst *createTupleExtractInst(SILLocation Loc, SILValue Operand,
                                           unsigned FieldNo, SILType ResultTy) {
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

  StructExtractInst *createStructExtract(SILLocation Loc,
                                         SILValue Operand,
                                         VarDecl *Field) {
    return createStructExtract(Loc, Operand, Field,
                               getStructFieldType(Field));
  }


  StructElementAddrInst *createStructElementAddr(SILLocation Loc,
                                                 SILValue Operand,
                                                 VarDecl *Field,
                                                 SILType ResultTy) {
    return insert(new (F.getModule())
                    StructElementAddrInst(Loc, Operand, Field, ResultTy));
  }
  
  RefElementAddrInst *createRefElementAddr(SILLocation Loc, SILValue Operand,
                                           VarDecl *Field, SILType ResultTy) {
    return insert(new (F.getModule())
                    RefElementAddrInst(Loc, Operand, Field, ResultTy));
  }
  
  ClassMethodInst *createClassMethod(SILLocation Loc, SILValue Operand,
                                     SILDeclRef Member,
                                     SILType MethodTy,
                                     bool Volatile = false)
  {
    return insert(new (F.getModule())
                    ClassMethodInst(Loc, Operand, Member, MethodTy, Volatile));
  }
  
  SuperMethodInst *createSuperMethod(SILLocation Loc, SILValue Operand,
                                     SILDeclRef Member,
                                     SILType MethodTy,
                                     bool Volatile = false)
  {
    return insert(new (F.getModule())
                    SuperMethodInst(Loc, Operand, Member, MethodTy, Volatile));
  }
  
  ArchetypeMethodInst *createArchetypeMethod(SILLocation Loc,
                                             SILType LookupTy,
                                             SILDeclRef Member,
                                             SILType MethodTy,
                                             bool Volatile = false)
  {
    return insert(new (F.getModule())
                    ArchetypeMethodInst(Loc, LookupTy, Member, MethodTy,
                                        Volatile));
  }
  
  ProtocolMethodInst *createProtocolMethod(SILLocation Loc,
                                           SILValue Operand,
                                           SILDeclRef Member,
                                           SILType MethodTy,
                                           bool Volatile = false) {
    return insert(new (F.getModule())
                    ProtocolMethodInst(Loc, Operand, Member, MethodTy,
                                       Volatile));
  }

  DynamicMethodInst *createDynamicMethod(SILLocation Loc,
                                         SILValue Operand,
                                         SILDeclRef Member,
                                         SILType MethodTy,
                                         bool Volatile = false) {
    return insert(new (F.getModule())
                  DynamicMethodInst(Loc, Operand, Member, MethodTy,
                                    Volatile));
  }

  ProjectExistentialInst *createProjectExistential(SILLocation Loc,
                                                   SILValue Operand,
                                                   SILType SelfTy) {
    return insert(new (F.getModule())
                    ProjectExistentialInst(Loc, Operand, SelfTy));
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
                                 IsTake_t isTakeOfSrc) {
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
  
  void createStrongRetain(SILLocation Loc, SILValue Operand) {
    // Retaining a function_ref is a no-op.
    if (!isa<FunctionRefInst>(Operand))
      createStrongRetainInst(Loc, Operand);
  }
  StrongRetainInst *createStrongRetainInst(SILLocation Loc, SILValue Operand) {
    // Retaining a function_ref is a no-op.
    return insert(new (F.getModule())
                    StrongRetainInst(Loc, Operand));
  }
  void createStrongRelease(SILLocation Loc, SILValue Operand) {
    // Releasing a function_ref is a no-op.
    if (!isa<FunctionRefInst>(Operand))
      createStrongReleaseInst(Loc, Operand);
  }
  StrongReleaseInst *createStrongReleaseInst(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule()) StrongReleaseInst(Loc, Operand));
  }
  StrongRetainAutoreleasedInst *
  createStrongRetainAutoreleased(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule()) StrongRetainAutoreleasedInst(Loc, Operand));
  }
  StrongRetainUnownedInst *createStrongRetainUnowned(SILLocation Loc,
                                                     SILValue Operand) {
    return insert(new (F.getModule()) StrongRetainUnownedInst(Loc, Operand));
  }
  UnownedRetainInst *createUnownedRetain(SILLocation Loc,
                                         SILValue Operand) {
    return insert(new (F.getModule()) UnownedRetainInst(Loc, Operand));
  }
  UnownedReleaseInst *createUnownedRelease(SILLocation Loc,
                                           SILValue Operand) {
    return insert(new (F.getModule()) UnownedReleaseInst(Loc, Operand));
  }
  DeallocStackInst *createDeallocStack(SILLocation loc, SILValue operand) {
    return insert(new (F.getModule()) DeallocStackInst(loc, operand));
  }
  DeallocRefInst *createDeallocRef(SILLocation loc, SILValue operand) {
    return insert(new (F.getModule()) DeallocRefInst(loc, operand));
  }
  DeallocBoxInst *createDeallocBox(SILLocation loc, SILType eltType,
                                   SILValue operand) {
    return insert(new (F.getModule()) DeallocBoxInst(loc, eltType, operand));
  }
  DestroyAddrInst *createDestroyAddr(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule()) DestroyAddrInst(Loc, Operand));
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

  UnreachableInst *createUnreachable(SILLocation Loc) {
    return insertTerminator(new (F.getModule()) UnreachableInst(Loc));
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

  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock,
                           OperandValueArrayRef Args);

  SwitchIntInst *createSwitchInt(SILLocation Loc, SILValue Operand,
         SILBasicBlock *DefaultBB,
         ArrayRef<std::pair<APInt, SILBasicBlock*>> CaseBBs) {
    return insertTerminator(SwitchIntInst::create(Loc, Operand, DefaultBB,
                                                  CaseBBs, F));
  }

  SwitchUnionInst *createSwitchUnion(SILLocation Loc, SILValue Operand,
         SILBasicBlock *DefaultBB,
         ArrayRef<std::pair<UnionElementDecl*, SILBasicBlock*>> CaseBBs) {
    return insertTerminator(SwitchUnionInst::create(Loc, Operand, DefaultBB,
                                                    CaseBBs, F));
  }

  DestructiveSwitchUnionAddrInst *
  createDestructiveSwitchUnionAddr(SILLocation Loc, SILValue Operand,
         SILBasicBlock *DefaultBB,
         ArrayRef<std::pair<UnionElementDecl*, SILBasicBlock*>> CaseBBs) {
    return insertTerminator(
              DestructiveSwitchUnionAddrInst::create(Loc, Operand, DefaultBB,
                                                     CaseBBs, F));
  }

  DynamicMethodBranchInst *
  createDynamicMethodBranch(SILLocation Loc, SILValue Operand,
                            SILDeclRef Member,
                            SILBasicBlock *HasMethodBB,
                            SILBasicBlock *NoMethodBB) {
    return insertTerminator(
             DynamicMethodBranchInst::create(Loc, Operand, Member, HasMethodBB,
                                             NoMethodBB, F));
  }

  //===--------------------------------------------------------------------===//
  // Memory management helpers
  //===--------------------------------------------------------------------===//
  
  /// Convenience function for calling emitRetain on the type lowering
  /// for the non-address value.
  void emitRetainValue(SILLocation loc, SILValue v) {
    assert(!v.getType().isAddress());
    F.getModule().getTypeLowering(v.getType()).emitRetain(*this, loc, v);
  }

  /// Convenience function for calling emitRelease on the type
  /// lowering for the non-address value.
  void emitReleaseValue(SILLocation loc, SILValue v) {
    assert(!v.getType().isAddress());
    F.getModule().getTypeLowering(v.getType()).emitRelease(*this, loc, v);
  }

  /// Generalize a function value.  This is a hack and probably not
  /// actually implementable at the SILBuilder level because it may
  /// require memory management.
  SILValue emitGeneralizedValue(SILLocation loc, SILValue value);
  
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

    // If the SILBuilder client wants to know about new instructions, record
    // this.
    if (InsertedInstrs)
      InsertedInstrs->push_back(TheInst);

    BB->getInstList().insert(InsertPt, TheInst);
  }
};

} // end swift namespace

#endif
