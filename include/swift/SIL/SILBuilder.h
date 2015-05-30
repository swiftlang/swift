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
#include "swift/SIL/SILDebugScope.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/PointerUnion.h"

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

  explicit SILBuilder(SILInstruction *I,
                      SmallVectorImpl<SILInstruction*> *InsertedInstrs = 0)
    : F(*I->getFunction()), InsertedInstrs(InsertedInstrs) {
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
  SILModule &getModule() const { return F.getModule(); }
  ASTContext &getASTContext() const { return F.getASTContext(); }
  const Lowering::TypeLowering &getTypeLowering(SILType T) const {
    return F.getModule().getTypeLowering(T);
  }

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

  SmallVectorImpl<SILInstruction *> *getTrackingList() {
    return InsertedInstrs;
  }

  //===--------------------------------------------------------------------===//
  // Type remapping
  //===--------------------------------------------------------------------===//

  static SILType getPartialApplyResultType(SILType Ty, unsigned ArgCount,
                                           SILModule &M,
                                           ArrayRef<Substitution> subs);

  //===--------------------------------------------------------------------===//
  // CFG Manipulation
  //===--------------------------------------------------------------------===//

  /// moveBlockTo - Move a block to immediately before the given iterator.
  void moveBlockTo(SILBasicBlock *BB, SILFunction::iterator IP) {
    assert(SILFunction::iterator(BB) != IP && "moving block before itself?");
    SILFunction *F = BB->getParent();
    auto &Blocks = F->getBlocks();
    Blocks.remove(BB);
    Blocks.insert(IP, BB);
  }

  /// moveBlockToEnd - Reorder a block to the end of its containing function.
  void moveBlockToEnd(SILBasicBlock *BB) {
    moveBlockTo(BB, BB->getParent()->end());
  }

  /// \brief Move the insertion point to the end of the given block.
  ///
  /// Assumes that no insertion point is currently active.
  void emitBlock(SILBasicBlock *BB) {
    assert(!hasValidInsertionPoint());
    setInsertionPoint(BB);
  }

  /// \brief Branch to the given block if there's an active insertion point,
  /// then move the insertion point to the end of that block.
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
    Loc.markAsPrologue();
    return insert(new (F.getModule()) AllocStackInst(Loc, elementType, F));
  }

  AllocRefInst *createAllocRef(SILLocation Loc, SILType elementType, bool objc){
    Loc.markAsPrologue();
    return insert(new (F.getModule()) AllocRefInst(Loc, elementType, F, objc));
  }

  AllocRefDynamicInst *createAllocRefDynamic(SILLocation loc, SILValue operand,
                                             SILType type, bool objc) {
    loc.markAsPrologue();
    return insert(new (F.getModule()) AllocRefDynamicInst(loc, operand, type, 
                                                          objc));
  }

  AllocValueBufferInst *createAllocValueBuffer(SILLocation loc,
                                               SILType valueType,
                                               SILValue operand) {
    return insert(new (F.getModule())
                    AllocValueBufferInst(loc, valueType, operand));
  }
  
  AllocBoxInst *createAllocBox(SILLocation Loc, SILType ElementType) {
    Loc.markAsPrologue();
    return insert(new (F.getModule())
                    AllocBoxInst(Loc, ElementType, F));
  }
  
  AllocExistentialBoxInst *createAllocExistentialBox(SILLocation Loc,
                                 SILType ExistentialType,
                                 CanType ConcreteType,
                                 SILType ConcreteLoweredType,
                                 ArrayRef<ProtocolConformance *> Conformances) {
    return insert(AllocExistentialBoxInst::create(Loc, ExistentialType,
                          ConcreteType, ConcreteLoweredType, Conformances, &F));
  }

  ApplyInst *createApply(SILLocation Loc, SILValue Fn,
                         SILType SubstFnTy,
                         SILType Result,
                         ArrayRef<Substitution> Subs,
                         ArrayRef<SILValue> Args) {
    return insert(ApplyInst::create(Loc, Fn, SubstFnTy, Result,
                                    Subs, Args, F));
  }
  
  ApplyInst *createApply(SILLocation Loc, SILValue Fn,
                         ArrayRef<SILValue> Args) {
    auto FnTy = Fn.getType();
    return createApply(Loc, Fn, FnTy,
                       FnTy.castTo<SILFunctionType>()
                         ->getResult()
                         .getSILType(),
                       ArrayRef<Substitution>(), Args);
  }

  TryApplyInst *createTryApply(SILLocation loc, SILValue fn,
                               SILType substFnTy,
                               ArrayRef<Substitution> subs,
                               ArrayRef<SILValue> args,
                               SILBasicBlock *normalBB,
                               SILBasicBlock *errorBB) {
    return insertTerminator(TryApplyInst::create(loc, fn, substFnTy, subs, args,
                                                 normalBB, errorBB, F));
  }


  PartialApplyInst *createPartialApply(SILLocation Loc, SILValue Fn,
                                       SILType SubstFnTy,
                                       ArrayRef<Substitution> Subs,
                                       ArrayRef<SILValue> Args,
                                       SILType ClosureTy) {
    return insert(PartialApplyInst::create(Loc, Fn, SubstFnTy,
                                           Subs, Args, ClosureTy, F));
  }

  BuiltinInst *createBuiltin(SILLocation Loc, Identifier Name,
                             SILType ResultTy, ArrayRef<Substitution> Subs,
                             ArrayRef<SILValue> Args) {
    return insert(BuiltinInst::create(Loc, Name, ResultTy, Subs, Args, F));
  }

  /// Create a binary function with the signature: OpdTy, OpdTy -> ResultTy.
  BuiltinInst *createBuiltinBinaryFunction(SILLocation Loc, StringRef Name,
                                           SILType OpdTy, SILType ResultTy,
                                           ArrayRef<SILValue> Args) {
    CanType IntTy = OpdTy.getSwiftRValueType();
    auto BuiltinIntTy = cast<BuiltinIntegerType>(IntTy);
    llvm::SmallString<16> NameStr = Name;
    if (BuiltinIntTy == BuiltinIntegerType::getWordType(getASTContext())) {
      NameStr += "_Word";
    } else {
      unsigned NumBits = BuiltinIntTy->getWidth().getFixedWidth();
      NameStr += "_Int" + llvm::utostr(NumBits);
    }
    auto Ident = getASTContext().getIdentifier(NameStr);
    return insert(BuiltinInst::create(Loc, Ident, ResultTy, {}, Args, F));
  }

  /// Create a binary function with the signature:
  /// OpdTy, OpdTy, Int1 -> (OpdTy, Int1)
  BuiltinInst *createBuiltinBinaryFunctionWithOverflow(
      SILLocation Loc, StringRef Name, ArrayRef<SILValue> Args) {
    assert(Args.size() == 3 && "Need three arguments");
    assert(Args[0].getType() == Args[1].getType() &&
           "Binary operands must match");
    assert(Args[2].getType().is<BuiltinIntegerType>() &&
           Args[2].getType().getSwiftRValueType()->isBuiltinIntegerType(1) &&
           "Must have a third Int1 operand");

    SILType OpdTy = Args[0].getType();
    SILType Int1Ty = Args[2].getType();

    TupleTypeElt ResultElts[] = {OpdTy.getSwiftRValueType(),
                                 Int1Ty.getSwiftRValueType()};
    Type ResultTy = TupleType::get(ResultElts, getASTContext());
    SILType SILResultTy =
        SILType::getPrimitiveObjectType(ResultTy->getCanonicalType());

    return createBuiltinBinaryFunction(Loc, Name, OpdTy, SILResultTy, Args);
  }

  FunctionRefInst *createFunctionRef(SILLocation loc, SILFunction *f) {
    return insert(new (F.getModule()) FunctionRefInst(loc, f));
  }
  GlobalAddrInst *createGlobalAddr(SILLocation loc, SILGlobalVariable *g){
    return insert(new (F.getModule()) GlobalAddrInst(loc, g));
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
  
  StringLiteralInst *createStringLiteral(SILLocation loc, StringRef text,
                                         StringLiteralInst::Encoding encoding) {
    return insert(StringLiteralInst::create(loc, text, encoding, F));
  }

  StringLiteralInst *createStringLiteral(SILLocation loc, const Twine& text,
                                         StringLiteralInst::Encoding encoding) {
    SmallVector<char, 256> Out;
    return insert(StringLiteralInst::create(loc,
                                            text.toStringRef(Out),
                                            encoding,
                                            F));
  }

  LoadInst *createLoad(SILLocation Loc, SILValue LV) {
    return insert(new (F.getModule()) LoadInst(Loc, LV));
  }

  StoreInst *createStore(SILLocation Loc, SILValue Src, SILValue DestAddr) {
    return insert(new (F.getModule()) StoreInst(Loc, Src, DestAddr));
  }
  AssignInst *createAssign(SILLocation Loc, SILValue Src, SILValue DestAddr) {
    return insert(new (F.getModule()) AssignInst(Loc, Src, DestAddr));
  }

  MarkUninitializedInst *createMarkUninitialized(SILLocation loc, SILValue src,
                                                 MarkUninitializedInst::Kind k){
    return insert(new (F.getModule()) MarkUninitializedInst(loc, src, k));
  }
  MarkUninitializedInst *createMarkUninitializedVar(SILLocation loc,
                                                    SILValue src) {
    return createMarkUninitialized(loc, src, MarkUninitializedInst::Var);
  }
  MarkUninitializedInst *createMarkUninitializedRootSelf(SILLocation loc,
                                                         SILValue src) {
    return createMarkUninitialized(loc, src, MarkUninitializedInst::RootSelf);
  }

  MarkFunctionEscapeInst *createMarkFunctionEscape(SILLocation loc,
                                                   ArrayRef<SILValue> vars) {
    return insert(MarkFunctionEscapeInst::create(loc, vars, F));
  }
  DebugValueInst *createDebugValue(SILLocation loc, SILValue src) {
    return insert(new (F.getModule()) DebugValueInst(loc, src));
  }
  DebugValueAddrInst *createDebugValueAddr(SILLocation loc, SILValue src) {
    return insert(new (F.getModule()) DebugValueAddrInst(loc, src));
  }

  LoadWeakInst *createLoadWeak(SILLocation loc, SILValue src, IsTake_t isTake) {
    return insert(new (F.getModule()) LoadWeakInst(loc, src, isTake));
  }

  StoreWeakInst *createStoreWeak(SILLocation loc, SILValue value,
                                 SILValue dest, IsInitialization_t isInit) {
    return insert(new (F.getModule()) StoreWeakInst(loc, value, dest, isInit));
  }

  CopyAddrInst *createCopyAddr(SILLocation loc, SILValue srcAddr,
                               SILValue destAddr, IsTake_t isTake,
                               IsInitialization_t isInitialize) {
    assert(srcAddr.getType() == destAddr.getType());
    return insert(new (F.getModule())
                    CopyAddrInst(loc, srcAddr, destAddr, isTake, isInitialize));
  }
  
  ConvertFunctionInst *createConvertFunction(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new (F.getModule()) ConvertFunctionInst(Loc, Op, Ty));
  }

  ThinFunctionToPointerInst *
  createThinFunctionToPointer(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ThinFunctionToPointerInst(Loc, Op, Ty));
  }

  PointerToThinFunctionInst *
  createPointerToThinFunction(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) PointerToThinFunctionInst(Loc, Op, Ty));
  }

  UpcastInst *createUpcast(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) UpcastInst(Loc, Op, Ty));
  }
  
  AddressToPointerInst *createAddressToPointer(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(new (F.getModule()) AddressToPointerInst(Loc, Op, Ty));
  }
  
  PointerToAddressInst *createPointerToAddress(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(new (F.getModule()) PointerToAddressInst(Loc, Op, Ty));
  }
  
  UncheckedRefCastInst *createUncheckedRefCast(SILLocation Loc, SILValue Op,
                                                   SILType Ty) {
    return insert(new (F.getModule()) UncheckedRefCastInst(Loc, Op, Ty));
  }
  
  UncheckedAddrCastInst *createUncheckedAddrCast(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(new (F.getModule()) UncheckedAddrCastInst(Loc, Op, Ty));
  }

  UncheckedTrivialBitCastInst *createUncheckedTrivialBitCast(SILLocation Loc,
                                                             SILValue Op,
                                                             SILType Ty) {
    return insert(new (F.getModule()) UncheckedTrivialBitCastInst(Loc, Op, Ty));
  }

  UncheckedRefBitCastInst *createUncheckedRefBitCast(SILLocation Loc,
                                                     SILValue Op,
                                                     SILType Ty) {
    return insert(new (F.getModule()) UncheckedRefBitCastInst(Loc, Op, Ty));
  }
  
  RefToBridgeObjectInst *createRefToBridgeObject(SILLocation Loc,
                                                 SILValue Ref,
                                                 SILValue Bits) {
    auto Ty = SILType::getBridgeObjectType(getASTContext());
    return insert(new (F.getModule()) RefToBridgeObjectInst(Loc, Ref, Bits, Ty));
  }
  
  BridgeObjectToRefInst *createBridgeObjectToRef(SILLocation Loc,
                                                 SILValue Op,
                                                 SILType Ty) {
    return insert(new (F.getModule()) BridgeObjectToRefInst(Loc, Op, Ty));
  }

  BridgeObjectToWordInst *createBridgeObjectToWord(SILLocation Loc,
                                                   SILValue Op) {
    auto Ty = SILType::getBuiltinWordType(getASTContext());
    return createBridgeObjectToWord(Loc, Op, Ty);
  }
  
  BridgeObjectToWordInst *createBridgeObjectToWord(SILLocation Loc,
                                                   SILValue Op,
                                                   SILType Ty) {
    return insert(new (F.getModule()) BridgeObjectToWordInst(Loc, Op, Ty));
  }

  RefToRawPointerInst *createRefToRawPointer(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new (F.getModule()) RefToRawPointerInst(Loc, Op, Ty));
  }
  
  RawPointerToRefInst *createRawPointerToRef(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new (F.getModule()) RawPointerToRefInst(Loc, Op, Ty));
  }

  ThinToThickFunctionInst *createThinToThickFunction(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ThinToThickFunctionInst(Loc, Op, Ty));
  }

  ThickToObjCMetatypeInst *createThickToObjCMetatype(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ThickToObjCMetatypeInst(Loc, Op, Ty));
  }

  ObjCToThickMetatypeInst *createObjCToThickMetatype(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ObjCToThickMetatypeInst(Loc, Op, Ty));
  }

  ObjCProtocolInst *createObjCProtocol(SILLocation Loc,
                                       ProtocolDecl *P,
                                       SILType Ty) {
    return insert(new (F.getModule()) ObjCProtocolInst(Loc, P, Ty));
  }
  
  UnownedToRefInst *createUnownedToRef(SILLocation loc,
                                       SILValue op, SILType ty) {
    return insert(new (F.getModule()) UnownedToRefInst(loc, op, ty));
  }

  RefToUnownedInst *createRefToUnowned(SILLocation loc,
                                       SILValue op, SILType ty) {
    return insert(new (F.getModule()) RefToUnownedInst(loc, op, ty));
  }

  UnmanagedToRefInst *createUnmanagedToRef(SILLocation loc,
                                           SILValue op, SILType ty) {
    return insert(new (F.getModule()) UnmanagedToRefInst(loc, op, ty));
  }

  RefToUnmanagedInst *createRefToUnmanaged(SILLocation loc,
                                           SILValue op, SILType ty) {
    return insert(new (F.getModule()) RefToUnmanagedInst(loc, op, ty));
  }

  IsNonnullInst *createIsNonnull(SILLocation loc,
                                 SILValue operand) {
    return insert(new (F.getModule())
                    IsNonnullInst(loc, operand,
                      SILType::getBuiltinIntegerType(1, getASTContext())));
  }

  NullClassInst *createNullClass(SILLocation loc, SILType resultType) {
    return insert(new (F.getModule()) NullClassInst(loc, resultType));
  }

  UnconditionalCheckedCastInst *createUnconditionalCheckedCast(SILLocation loc,
                                                               SILValue op,
                                                               SILType destTy) {
    return insert(new (F.getModule())
                    UnconditionalCheckedCastInst(loc, op, destTy));
  }

  UnconditionalCheckedCastAddrInst *
  createUnconditionalCheckedCastAddr(SILLocation loc,
                                     CastConsumptionKind consumption,
                                     SILValue src, CanType sourceType,
                                     SILValue dest, CanType targetType) {
    return insert(new (F.getModule())
                    UnconditionalCheckedCastAddrInst(loc, consumption,
                                                     src, sourceType,
                                                     dest, targetType));
  }
  
  RetainValueInst *createRetainValue(SILLocation loc, SILValue operand) {
    return insert(new (F.getModule()) RetainValueInst(loc, operand));
  }

  ReleaseValueInst *createReleaseValue(SILLocation loc, SILValue operand) {
    return insert(new (F.getModule()) ReleaseValueInst(loc, operand));
  }

  AutoreleaseValueInst *createAutoreleaseValue(SILLocation loc, SILValue operand) {
    return insert(new (F.getModule()) AutoreleaseValueInst(loc, operand));
  }

  StructInst *createStruct(SILLocation Loc, SILType Ty,
                           ArrayRef<SILValue> Elements) {
    return insert(StructInst::create(Loc, Ty, Elements, F));
  }

  TupleInst *createTuple(SILLocation Loc, SILType Ty,
                         ArrayRef<SILValue> Elements) {
    return insert(TupleInst::create(Loc, Ty, Elements, F));
  }
  
  EnumInst *createEnum(SILLocation Loc, SILValue Operand,
                       EnumElementDecl *Element, SILType Ty) {
    return insert(new (F.getModule()) EnumInst(Loc, Operand, Element, Ty));
  }

  /// Inject a loadable value into the corresponding optional type.
  EnumInst *createOptionalSome(SILLocation loc, SILValue operand, SILType ty) {
    return createOptionalSome(loc, operand, ty.getOptionalTypeKind(), ty);
  }

  /// Inject a loadable value into the corresponding optional type.
  EnumInst *createOptionalSome(SILLocation loc, SILValue operand,
                               OptionalTypeKind optKind, SILType ty) {
    assert(ty.getOptionalTypeKind() == optKind);
    auto someDecl = F.getModule().getASTContext().getOptionalSomeDecl(optKind);
    return createEnum(loc, operand, someDecl, ty);
  }

  /// Create the nil value of a loadable optional type.
  EnumInst *createOptionalNone(SILLocation loc, SILType ty) {
    return createOptionalNone(loc, ty.getOptionalTypeKind(), ty);
  }

  /// Create the nil value of a loadable optional type.
  EnumInst *createOptionalNone(SILLocation loc, OptionalTypeKind optKind,
                               SILType ty) {
    assert(ty.getOptionalTypeKind() == optKind);
    auto noneDecl = F.getModule().getASTContext().getOptionalNoneDecl(optKind);
    return createEnum(loc, nullptr, noneDecl, ty);
  }

  InitEnumDataAddrInst *createInitEnumDataAddr(SILLocation Loc, SILValue Operand,
                                       EnumElementDecl *Element, SILType Ty) {
    return insert(
          new (F.getModule()) InitEnumDataAddrInst(Loc, Operand, Element, Ty));
  }

  UncheckedEnumDataInst *
  createUncheckedEnumData(SILLocation Loc, SILValue Operand,
                          EnumElementDecl *Element, SILType Ty) {
    return insert(
          new (F.getModule()) UncheckedEnumDataInst(Loc, Operand, Element, Ty));
  }

  UncheckedEnumDataInst *
  createUncheckedEnumData(SILLocation Loc, SILValue Operand,
                          EnumElementDecl *Element) {
    SILType EltType = Operand.getType().getEnumElementType(Element,
                                                           getModule());
    return createUncheckedEnumData(Loc, Operand, Element, EltType);
  }

  UncheckedTakeEnumDataAddrInst *
  createUncheckedTakeEnumDataAddr(SILLocation Loc, SILValue Operand,
                                  EnumElementDecl *Element, SILType Ty) {
    return insert(
          new (F.getModule()) UncheckedTakeEnumDataAddrInst(Loc, Operand,
                                                            Element, Ty));
  }

  UncheckedTakeEnumDataAddrInst *
  createUncheckedTakeEnumDataAddr(SILLocation Loc, SILValue Operand,
                                  EnumElementDecl *Element) {
    SILType EltType = Operand.getType().getEnumElementType(Element,
                                                           getModule());
    return createUncheckedTakeEnumDataAddr(Loc, Operand, Element, EltType);
  }

  InjectEnumAddrInst *createInjectEnumAddr(SILLocation Loc, SILValue Operand,
                                             EnumElementDecl *Element) {
    return insert(new (F.getModule())
                    InjectEnumAddrInst(Loc, Operand, Element));
  }
  
  SelectEnumInst *createSelectEnum(SILLocation Loc, SILValue Operand,
         SILType Ty,
         SILValue DefaultValue,
         ArrayRef<std::pair<EnumElementDecl*, SILValue>> CaseValues) {
    return insert(SelectEnumInst::create(Loc, Operand, Ty, DefaultValue,
                                         CaseValues, F));
  }

  SelectEnumAddrInst *createSelectEnumAddr(SILLocation Loc, SILValue Operand,
         SILType Ty,
         SILValue DefaultValue,
         ArrayRef<std::pair<EnumElementDecl*, SILValue>> CaseValues) {
    return insert(SelectEnumAddrInst::create(Loc, Operand, Ty, DefaultValue,
                                             CaseValues, F));
  }
  
  SelectValueInst *createSelectValue(SILLocation Loc, SILValue Operand,
         SILType Ty,
         SILValue DefaultResult,
         ArrayRef<std::pair<SILValue, SILValue>> CaseValuesAndResults) {
    return insert(SelectValueInst::create(Loc, Operand, Ty, DefaultResult,
                                          CaseValuesAndResults, F));
  }

  TupleExtractInst *createTupleExtract(SILLocation Loc, SILValue Operand,
                                       unsigned FieldNo, SILType ResultTy) {
    return insert(new (F.getModule())
                    TupleExtractInst(Loc, Operand, FieldNo, ResultTy));
  }

  TupleExtractInst *createTupleExtract(SILLocation Loc, SILValue Operand,
                                       unsigned FieldNo) {
    auto type = Operand.getType().getTupleElementType(FieldNo);
    return createTupleExtract(Loc, Operand, FieldNo, type);
  }

  TupleElementAddrInst *createTupleElementAddr(SILLocation Loc,
                                               SILValue Operand,
                                               unsigned FieldNo,
                                               SILType ResultTy) {
    return insert(new (F.getModule())
                    TupleElementAddrInst(Loc, Operand, FieldNo, ResultTy));
  }

  TupleElementAddrInst *createTupleElementAddr(SILLocation Loc,
                                               SILValue Operand,
                                               unsigned FieldNo) {
    return insert(new (F.getModule())
                  TupleElementAddrInst(Loc, Operand, FieldNo,
                            Operand.getType().getTupleElementType(FieldNo)));
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
    auto type = Operand.getType().getFieldType(Field, F.getModule());
    return createStructExtract(Loc, Operand, Field, type);
  }


  StructElementAddrInst *createStructElementAddr(SILLocation Loc,
                                                 SILValue Operand,
                                                 VarDecl *Field,
                                                 SILType ResultTy) {
    return insert(new (F.getModule())
                    StructElementAddrInst(Loc, Operand, Field, ResultTy));
  }
  
  StructElementAddrInst *createStructElementAddr(SILLocation Loc,
                                                 SILValue Operand,
                                                 VarDecl *Field) {
    auto ResultTy = Operand.getType().getFieldType(Field, F.getModule());
    return createStructElementAddr(Loc, Operand, Field, ResultTy);
  }
  
  RefElementAddrInst *createRefElementAddr(SILLocation Loc, SILValue Operand,
                                           VarDecl *Field, SILType ResultTy) {
    return insert(new (F.getModule())
                    RefElementAddrInst(Loc, Operand, Field, ResultTy));
  }
  RefElementAddrInst *createRefElementAddr(SILLocation Loc, SILValue Operand,
                                           VarDecl *Field) {
    auto ResultTy = Operand.getType().getFieldType(Field, F.getModule());
    return createRefElementAddr(Loc, Operand, Field, ResultTy);
  }

  ClassMethodInst *createClassMethod(SILLocation Loc, SILValue Operand,
                                     SILDeclRef Member, SILType MethodTy,
                                     bool Volatile = false) {
    return insert(new (F.getModule())
                    ClassMethodInst(Loc, Operand, Member, MethodTy, Volatile));
  }
  
  /// Emit a class_method reference to the least derived overridden decl for
  /// the given method, and upcast the "self" pointer to the matching superclass
  /// type.
  std::pair<ClassMethodInst *, SILValue>
  emitClassMethod(SILLocation Loc, SILValue Self, SILDeclRef Member,
                  bool Volatile = false);
  
  SuperMethodInst *createSuperMethod(SILLocation Loc, SILValue Operand,
                                     SILDeclRef Member, SILType MethodTy,
                                     bool Volatile = false) {
    return insert(new (F.getModule())
                    SuperMethodInst(Loc, Operand, Member, MethodTy, Volatile));
  }

  WitnessMethodInst *createWitnessMethod(SILLocation Loc, CanType LookupTy,
                                         ProtocolConformance *Conformance,
                                         SILDeclRef Member,
                                         SILType MethodTy,
                                         SILValue OptionalOpenedExistential,
                                         bool Volatile = false)
  {
    return insert(
        WitnessMethodInst::create(Loc, LookupTy, Conformance, Member, MethodTy,
                                  &F, OptionalOpenedExistential, Volatile));
  }

  DynamicMethodInst *createDynamicMethod(SILLocation Loc, SILValue Operand,
                                         SILDeclRef Member, SILType MethodTy,
                                         bool Volatile = false) {
    return insert(new (F.getModule())
                  DynamicMethodInst(Loc, Operand, Member, MethodTy,
                                    Volatile));
  }

  OpenExistentialAddrInst *createOpenExistentialAddr(SILLocation Loc, SILValue Operand,
                                             SILType SelfTy) {
    return insert(new (F.getModule())
                    OpenExistentialAddrInst(Loc, Operand, SelfTy));
  }
  
  OpenExistentialMetatypeInst *
  createOpenExistentialMetatype(SILLocation loc, SILValue operand,
                                SILType selfTy) {
    return insert(new (F.getModule())
                    OpenExistentialMetatypeInst(loc, operand, selfTy));
  }

  OpenExistentialRefInst *createOpenExistentialRef(SILLocation Loc,
                                                   SILValue Operand,
                                                   SILType Ty) {
    return insert(new (F.getModule())
                    OpenExistentialRefInst(Loc, Operand, Ty));
  }

  OpenExistentialBoxInst *createOpenExistentialBox(SILLocation Loc,
                                                   SILValue Operand,
                                                   SILType Ty) {
    return insert(new (F.getModule())
                    OpenExistentialBoxInst(Loc, Operand, Ty));
  }

  InitExistentialAddrInst *
  createInitExistentialAddr(SILLocation Loc,
                        SILValue Existential,
                        CanType FormalConcreteType,
                        SILType LoweredConcreteType,
                        ArrayRef<ProtocolConformance*> Conformances) {
    return insert(InitExistentialAddrInst::create(Loc, Existential,
                                              FormalConcreteType,
                                              LoweredConcreteType,
                                              Conformances, &F));
  }
  
  InitExistentialMetatypeInst *
  createInitExistentialMetatype(SILLocation loc, SILValue metatype,
                                SILType existentialType,
                                ArrayRef<ProtocolConformance*> conformances) {
    return insert(InitExistentialMetatypeInst::create(loc, existentialType,
                                                      metatype,
                                                      conformances, &F));
  }
  
  InitExistentialRefInst *
  createInitExistentialRef(SILLocation Loc, SILType ExistentialType,
                           CanType FormalConcreteType,
                           SILValue Concrete,
                           ArrayRef<ProtocolConformance*> Conformances) {
    return insert(InitExistentialRefInst::create(Loc, ExistentialType,
                                                 FormalConcreteType,
                                                 Concrete,
                                                 Conformances, &F));
  }

  DeinitExistentialAddrInst *createDeinitExistentialAddr(SILLocation Loc,
                                                 SILValue Existential) {
    return insert(new (F.getModule()) DeinitExistentialAddrInst(Loc, Existential));
  }
  
  ProjectBlockStorageInst *createProjectBlockStorage(SILLocation Loc,
                                                     SILValue Storage) {
    auto CaptureTy =
      Storage.getType().castTo<SILBlockStorageType>()->getCaptureAddressType();
    return createProjectBlockStorage(Loc, Storage, CaptureTy);
  }
  ProjectBlockStorageInst *createProjectBlockStorage(SILLocation Loc,
                                                     SILValue Storage,
                                                     SILType CaptureTy) {
    return insert(new (F.getModule()) ProjectBlockStorageInst(Loc, Storage,
                                                              CaptureTy));
  }
  
  InitBlockStorageHeaderInst *createInitBlockStorageHeader(SILLocation Loc,
                                                         SILValue BlockStorage,
                                                         SILValue InvokeFunction,
                                                         SILType BlockType) {
    return insert(new (F.getModule()) InitBlockStorageHeaderInst(Loc,
                                                                 BlockStorage,
                                                                 InvokeFunction,
                                                                 BlockType));
  }
  
  MetatypeInst *createMetatype(SILLocation Loc, SILType Metatype) {
    return insert(new (F.getModule()) MetatypeInst(Loc, Metatype));
  }
  
  ObjCMetatypeToObjectInst *createObjCMetatypeToObject(SILLocation Loc,
                                                       SILValue Op,
                                                       SILType Ty) {
    return insert(new (F.getModule()) ObjCMetatypeToObjectInst(Loc, Op, Ty));
  }
  
  ObjCExistentialMetatypeToObjectInst *
  createObjCExistentialMetatypeToObject(SILLocation Loc,
                                        SILValue Op,
                                        SILType Ty) {
    return insert(new (F.getModule())
                    ObjCExistentialMetatypeToObjectInst(Loc, Op, Ty));
  }
  
  ValueMetatypeInst *createValueMetatype(SILLocation Loc,
                                                 SILType Metatype,
                                                 SILValue Base) {
    return insert(new (F.getModule())
                    ValueMetatypeInst(Loc, Metatype, Base));
  }
  
  ExistentialMetatypeInst *createExistentialMetatype(SILLocation Loc,
                                               SILType Metatype,
                                               SILValue Base) {
    return insert(new (F.getModule()) ExistentialMetatypeInst(Loc, Metatype,Base));
  }
  
  CopyBlockInst *createCopyBlock(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule()) CopyBlockInst(Loc, Operand));
  }
  StrongRetainInst *createStrongRetain(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule()) StrongRetainInst(Loc, Operand));
  }
  StrongReleaseInst *createStrongRelease(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule()) StrongReleaseInst(Loc, Operand));
  }
  StrongRetainAutoreleasedInst *
  createStrongRetainAutoreleased(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule()) StrongRetainAutoreleasedInst(Loc, Operand));
  }
  StrongPinInst *createStrongPin(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule()) StrongPinInst(Loc, Operand));
  }
  StrongUnpinInst *createStrongUnpin(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule()) StrongUnpinInst(Loc, Operand));
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
  FixLifetimeInst *createFixLifetime(SILLocation Loc,
                                     SILValue Operand) {
    return insert(new (F.getModule()) FixLifetimeInst(Loc, Operand));
  }
  void emitFixLifetime(SILLocation Loc, SILValue Operand) {
    if (getTypeLowering(Operand.getType()).isTrivial())
      return;
    createFixLifetime(Loc, Operand);
  }
  MarkDependenceInst *createMarkDependence(SILLocation loc,
                                           SILValue value,
                                           SILValue base) {
    return insert(new (F.getModule()) MarkDependenceInst(loc, value, base));
  }
  IsUniqueInst *createIsUnique(SILLocation loc, SILValue operand) {
    auto Int1Ty = SILType::getBuiltinIntegerType(1, getASTContext());
    return insert(new (F.getModule()) IsUniqueInst(loc, operand, Int1Ty));
  }
  IsUniqueOrPinnedInst *createIsUniqueOrPinned(SILLocation loc,
                                               SILValue value) {
    auto Int1Ty = SILType::getBuiltinIntegerType(1, getASTContext());
    return insert(new (F.getModule()) IsUniqueOrPinnedInst(loc, value, Int1Ty));
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
  DeallocExistentialBoxInst *createDeallocExistentialBox(SILLocation loc,
                                                         CanType concreteType,
                                                         SILValue operand) {
    return insert(new (F.getModule()) DeallocExistentialBoxInst(loc,
                                                                concreteType,
                                                                operand));
  }
  DeallocValueBufferInst *createDeallocValueBuffer(SILLocation loc,
                                                   SILType valueType,
                                                   SILValue operand) {
    return insert(new (F.getModule())
                    DeallocValueBufferInst(loc, valueType, operand));
  }  
  DestroyAddrInst *createDestroyAddr(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule()) DestroyAddrInst(Loc, Operand));
  }

  ProjectValueBufferInst *createProjectValueBuffer(SILLocation loc,
                                                   SILType valueType,
                                                   SILValue operand) {
    return insert(new (F.getModule())
                    ProjectValueBufferInst(loc, valueType, operand));
  }
  
  //===--------------------------------------------------------------------===//
  // Runtime failure
  //===--------------------------------------------------------------------===//
  
  CondFailInst *createCondFail(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule()) CondFailInst(Loc, Operand));
  }

  BuiltinInst *createBuiltinTrap(SILLocation Loc) {
    ASTContext &AST = F.getModule().getASTContext();
    auto Id_trap  = AST.getIdentifier("int_trap");
    return createBuiltin(Loc, Id_trap,
                         F.getModule().Types.getEmptyTupleType(),
                         {}, {});
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

  ThrowInst *createThrow(SILLocation loc, SILValue errorValue) {
    return insertTerminator(new (F.getModule()) ThrowInst(loc, errorValue));
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

  CondBranchInst *createCondBranch(SILLocation Loc, SILValue Cond,
                                   SILBasicBlock *Target1,
                                   OperandValueArrayRef Args1,
                                   SILBasicBlock *Target2,
                                   OperandValueArrayRef Args2) {
    SmallVector<SILValue, 6> ArgsCopy1;
    SmallVector<SILValue, 6> ArgsCopy2;
    
    ArgsCopy1.reserve(Args1.size());
    ArgsCopy2.reserve(Args2.size());

    for (auto I = Args1.begin(), E = Args1.end(); I != E; ++I)
      ArgsCopy1.push_back(*I);
    for (auto I = Args2.begin(), E = Args2.end(); I != E; ++I)
      ArgsCopy2.push_back(*I);

    return insertTerminator(CondBranchInst::create(Loc, Cond,
                                                   Target1, ArgsCopy1,
                                                   Target2, ArgsCopy2,
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

  SwitchValueInst *createSwitchValue(SILLocation Loc, SILValue Operand,
         SILBasicBlock *DefaultBB,
         ArrayRef<std::pair<SILValue, SILBasicBlock*>> CaseBBs) {
    return insertTerminator(SwitchValueInst::create(Loc, Operand, DefaultBB,
                                                    CaseBBs, F));
  }

  SwitchEnumInst *createSwitchEnum(SILLocation Loc, SILValue Operand,
         SILBasicBlock *DefaultBB,
         ArrayRef<std::pair<EnumElementDecl*, SILBasicBlock*>> CaseBBs) {
    return insertTerminator(SwitchEnumInst::create(Loc, Operand, DefaultBB,
                                                    CaseBBs, F));
  }

  SwitchEnumAddrInst *
  createSwitchEnumAddr(SILLocation Loc, SILValue Operand,
         SILBasicBlock *DefaultBB,
         ArrayRef<std::pair<EnumElementDecl*, SILBasicBlock*>> CaseBBs) {
    return insertTerminator(
              SwitchEnumAddrInst::create(Loc, Operand, DefaultBB,
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
  
  CheckedCastBranchInst *createCheckedCastBranch(SILLocation loc,
                                                 bool isExact,
                                                 SILValue op,
                                                 SILType destTy,
                                                 SILBasicBlock *successBB,
                                                 SILBasicBlock *failureBB) {
    return insertTerminator(new (F.getModule())
                              CheckedCastBranchInst(loc, isExact, op, destTy,
                                                    successBB, failureBB));
  }

  CheckedCastAddrBranchInst *createCheckedCastAddrBranch(SILLocation loc,
                                              CastConsumptionKind consumption,
                                                         SILValue src,
                                                         CanType sourceType,
                                                         SILValue dest,
                                                         CanType targetType,
                                                 SILBasicBlock *successBB,
                                                 SILBasicBlock *failureBB) {
    return insertTerminator(new (F.getModule())
                              CheckedCastAddrBranchInst(loc, consumption,
                                                        src, sourceType,
                                                        dest, targetType,
                                                        successBB, failureBB));
  }

  //===--------------------------------------------------------------------===//
  // Memory management helpers
  //===--------------------------------------------------------------------===//

  /// Try to fold a destroy_addr operation into the previous instructions, or
  /// generate an explicit one if that fails.  If this inserts a new
  /// instruction, it returns it, otherwise it returns null.
  DestroyAddrInst *emitDestroyAddrAndFold(SILLocation Loc, SILValue Operand) {
    auto U = emitDestroyAddr(Loc, Operand);
    if (U.isNull() || !U.is<DestroyAddrInst *>())
      return nullptr;
    return U.get<DestroyAddrInst *>();
  }

  /// Perform a strong_release instruction at the current location, attempting
  /// to fold it locally into nearby retain instructions or emitting an explicit
  /// strong release if necessary.  If this inserts a new instruction, it
  /// returns it, otherwise it returns null.
  StrongReleaseInst *emitStrongReleaseAndFold(SILLocation Loc,
                                              SILValue Operand) {
    auto U = emitStrongRelease(Loc, Operand);
    if (U.isNull())
      return nullptr;
    if (auto *SRI = U.dyn_cast<StrongReleaseInst *>())
      return SRI;
    U.get<StrongRetainInst *>()->eraseFromParent();
    return nullptr;
  }

  /// Emit a release_value instruction at the current location, attempting to
  /// fold it locally into another nearby retain_value instruction.  This
  /// returns the new instruction if it inserts one, otherwise it returns null.
  ///
  /// This instruction doesn't handle strength reduction of release_value into
  /// a noop / strong_release / unowned_release.  For that, use the
  /// emitReleaseValueOperation method below or use the TypeLowering API.
  ReleaseValueInst *emitReleaseValueAndFold(SILLocation Loc, SILValue Operand) {
    auto U = emitReleaseValue(Loc, Operand);
    if (U.isNull())
      return nullptr;
    if (auto *RVI = U.dyn_cast<ReleaseValueInst *>())
      return RVI;
    U.get<RetainValueInst *>()->eraseFromParent();
    return nullptr;
  }

  /// Emit a release_value instruction at the current location, attempting to
  /// fold it locally into another nearby retain_value instruction. Returns a
  /// pointer union initialized with a release value inst if it inserts one,
  /// otherwise returns the retain. It is expected that the caller will remove
  /// the retain_value. This allows for the caller to update any state before
  /// the retain_value is destroyed.
  PointerUnion<RetainValueInst *, ReleaseValueInst *>
  emitReleaseValue(SILLocation Loc, SILValue Operand);

  /// Emit a strong_release instruction at the current location, attempting to
  /// fold it locally into another nearby strong_retain instruction. Returns a
  /// pointer union initialized with a strong_release inst if it inserts one,
  /// otherwise returns the pointer union initialized with the strong_retain. It
  /// is expected that the caller will remove the returned strong_retain. This
  /// allows for the caller to update any state before the release value is
  /// destroyed.
  PointerUnion<StrongRetainInst *, StrongReleaseInst *>
  emitStrongRelease(SILLocation Loc, SILValue Operand);

  /// Emit a destroy_addr instruction at \p Loc attempting to fold the
  /// destroy_addr locally into a copy_addr instruction. Returns a pointer union
  /// initialized with the folded copy_addr if the destroy_addr was folded into
  /// a copy_addr. Otherwise, returns the newly inserted destroy_addr.
  PointerUnion<CopyAddrInst *, DestroyAddrInst *>
  emitDestroyAddr(SILLocation Loc, SILValue Operand);

  /// Convenience function for calling emitRetain on the type lowering
  /// for the non-address value.
  void emitRetainValueOperation(SILLocation loc, SILValue v) {
    assert(!v.getType().isAddress());
    auto &lowering = getTypeLowering(v.getType());
    return lowering.emitRetainValue(*this, loc, v);
  }
  
  /// Convenience function for calling TypeLowering.emitRelease on the type
  /// lowering for the non-address value.
  void emitReleaseValueOperation(SILLocation loc, SILValue v) {
    assert(!v.getType().isAddress());
    auto &lowering = getTypeLowering(v.getType());
    lowering.emitReleaseValue(*this, loc, v);
  }


  SILValue emitTupleExtract(SILLocation Loc, SILValue Operand,
                            unsigned FieldNo, SILType ResultTy) {
    // Fold tuple_extract(tuple(x,y,z),2)
    if (auto *TI = dyn_cast<TupleInst>(Operand))
      return TI->getOperand(FieldNo);
    
    return createTupleExtract(Loc, Operand, FieldNo, ResultTy);
  }
  
  SILValue emitTupleExtract(SILLocation Loc, SILValue Operand,
                            unsigned FieldNo) {
    return emitTupleExtract(Loc, Operand, FieldNo,
                            Operand.getType().getTupleElementType(FieldNo));
  }
  
  SILValue emitStructExtract(SILLocation Loc, SILValue Operand,
                             VarDecl *Field, SILType ResultTy) {
    if (auto *SI = dyn_cast<StructInst>(Operand))
      return SI->getFieldValue(Field);
    
    return createStructExtract(Loc, Operand, Field, ResultTy);
  }
  
  SILValue emitStructExtract(SILLocation Loc, SILValue Operand, VarDecl *Field){
    auto type = Operand.getType().getFieldType(Field, F.getModule());
    return emitStructExtract(Loc, Operand, Field, type);
  }

  SILValue emitThickToObjCMetatype(SILLocation Loc, SILValue Op, SILType Ty);
  SILValue emitObjCToThickMetatype(SILLocation Loc, SILValue Op, SILType Ty);

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


/// An RAII version of SILBuilder that automatically sets up identical
/// SILDebugScopes for all instructions.  This is useful for
/// situations where a single SIL instruction is lowered into a
/// sequence of SIL instructions.
template<unsigned N = 4> class SILBuilderWithScope : public SILBuilder {
  SmallVector<SILInstruction*, N> InsertedInstrs;
  SILDebugScope *DebugScope;

public:
  explicit SILBuilderWithScope(SILInstruction *I)
    : SILBuilder(I, &InsertedInstrs), DebugScope(I->getDebugScope()) {
    assert((DebugScope || maybeScopeless(*I)) && "no debug scope");
  }

  explicit SILBuilderWithScope(SILInstruction *I, SILDebugScope *DS)
    : SILBuilder(I, &InsertedInstrs), DebugScope(DS) {
  }

  explicit SILBuilderWithScope(SILBasicBlock *BB, SILDebugScope *DS)
    : SILBuilder(BB, &InsertedInstrs), DebugScope(DS) {
  }

  ~SILBuilderWithScope() {
    for (auto *I : InsertedInstrs) {
      assert(!I->getDebugScope() && "instruction was already assigned a scope");
      I->setDebugScope(DebugScope);
    }
  }
};

} // end swift namespace

#endif
