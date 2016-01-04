//===--- SILBuilder.h - Class for creating SIL Constructs -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringExtras.h"

namespace swift {

class SILDebugScope;

class SILBuilder {
  SILFunction &F;
  /// If this is non-null, the instruction is inserted in the specified
  /// basic block, at the specified InsertPt.  If null, created instructions
  /// are not auto-inserted.
  SILBasicBlock *BB;
  SILBasicBlock::iterator InsertPt;
  const SILDebugScope *CurDebugScope = nullptr;

  /// InsertedInstrs - If this pointer is non-null, then any inserted
  /// instruction is recorded in this list.
  SmallVectorImpl<SILInstruction *> *InsertedInstrs = nullptr;

  friend class SILBuilderWithScope;
  /// This is the set of debug locations and scopes used in this module.
  llvm::SmallDenseMap<DebugLocKey, SILDebugLocation *, 1> DebugLocs;

public:
  SILBuilder(SILFunction &F) : F(F), BB(0) {}

  SILBuilder(SILFunction &F, SmallVectorImpl<SILInstruction *> *InsertedInstrs)
      : F(F), BB(0), InsertedInstrs(InsertedInstrs) {}

  explicit SILBuilder(SILInstruction *I,
                      SmallVectorImpl<SILInstruction *> *InsertedInstrs = 0)
      : F(*I->getFunction()), InsertedInstrs(InsertedInstrs) {
    setInsertionPoint(I);
  }

  explicit SILBuilder(SILBasicBlock::iterator I,
                      SmallVectorImpl<SILInstruction *> *InsertedInstrs = 0)
      : SILBuilder(&*I, InsertedInstrs) {}

  explicit SILBuilder(SILBasicBlock *BB,
                      SmallVectorImpl<SILInstruction *> *InsertedInstrs = 0)
      : F(*BB->getParent()), InsertedInstrs(InsertedInstrs) {
    setInsertionPoint(BB);
  }

  SILBuilder(SILBasicBlock *BB, SILBasicBlock::iterator InsertPt,
             SmallVectorImpl<SILInstruction *> *InsertedInstrs = 0)
      : F(*BB->getParent()), InsertedInstrs(InsertedInstrs) {
    setInsertionPoint(BB, InsertPt);
  }

  SILFunction &getFunction() const { return F; }
  SILModule &getModule() const { return F.getModule(); }
  ASTContext &getASTContext() const { return F.getASTContext(); }
  const Lowering::TypeLowering &getTypeLowering(SILType T) const {
    return F.getModule().getTypeLowering(T);
  }

  void setCurrentDebugScope(const SILDebugScope *DS) {
    assert((DS->InlinedCallSite || DS->SILFn == &getFunction()) &&
         "non-inlined scope belongs to different function");
    CurDebugScope = DS;
  }
  const SILDebugScope *getCurrentDebugScope() const { return CurDebugScope; }

  SILDebugLocation *getOrCreateDebugLocation(SILLocation Loc,
                                             const SILDebugScope *DS);

  /// Convenience function for allocating a SILDebugLocation.
  SILDebugLocation *createSILDebugLocation(SILLocation Loc) {
    // FIXME: Audit all uses and enable this assertion.
    // assert(getCurrentDebugScope() && "no debug scope");
    auto Scope = getCurrentDebugScope();
    return getOrCreateDebugLocation(Loc,
                                Scope ? Scope : getFunction().getDebugScope());
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
  void clearInsertionPoint() { BB = nullptr; }

  /// setInsertionPoint - Set the insertion point.
  void setInsertionPoint(SILBasicBlock *BB, SILBasicBlock::iterator InsertPt) {
    this->BB = BB;
    this->InsertPt = InsertPt;
    if (InsertPt != BB->end())
      cacheDebugLoc(InsertPt->getDebugLocation());
  }

  /// setInsertionPoint - Set the insertion point to insert before the specified
  /// instruction.
  void setInsertionPoint(SILInstruction *I) {
    setInsertionPoint(I->getParent(), I->getIterator());
  }

  /// setInsertionPoint - Set the insertion point to insert before the specified
  /// instruction.
  void setInsertionPoint(SILBasicBlock::iterator IIIter) {
    setInsertionPoint(IIIter->getParent(), IIIter);
  }

  /// setInsertionPoint - Set the insertion point to insert at the end of the
  /// specified block.
  void setInsertionPoint(SILBasicBlock *BB) {
    setInsertionPoint(BB, BB->end());
  }

  /// setInsertionPoint - Set the insertion point to insert at the end of the
  /// specified block.
  void setInsertionPoint(SILFunction::iterator BBIter) {
    setInsertionPoint(&*BBIter);
  }

  SILBasicBlock *getInsertionPoint() const { return BB; }

  //===--------------------------------------------------------------------===//
  // Instruction Tracking
  //===--------------------------------------------------------------------===//

  /// Clients of SILBuilder who want to know about any newly created
  /// instructions can install a SmallVector into the builder to collect them.
  void setTrackingList(SmallVectorImpl<SILInstruction *> *II) {
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

  /// moveBlockTo - Move \p BB to immediately before \p Before.
  void moveBlockTo(SILBasicBlock *BB, SILBasicBlock *Before) {
    moveBlockTo(BB, Before->getIterator());
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

  AllocStackInst *createAllocStack(SILLocation Loc, SILType elementType,
                                   SILDebugVariable Var = SILDebugVariable()) {
    Loc.markAsPrologue();
    return insert(AllocStackInst::create(createSILDebugLocation(Loc),
                                         elementType, F, Var));
  }

  AllocRefInst *createAllocRef(SILLocation Loc, SILType elementType, bool objc,
                               bool canAllocOnStack) {
    // AllocRefInsts expand to function calls and can therefore not be
    // counted towards the function prologue.
    assert(!Loc.isInPrologue());
    return insert(new (F.getModule()) AllocRefInst(
        createSILDebugLocation(Loc), elementType, F, objc, canAllocOnStack));
  }

  AllocRefDynamicInst *createAllocRefDynamic(SILLocation Loc, SILValue operand,
                                             SILType type, bool objc) {
    // AllocRefDynamicInsts expand to function calls and can therefore
    // not be counted towards the function prologue.
    assert(!Loc.isInPrologue());
    return insert(new (F.getModule()) AllocRefDynamicInst(
        createSILDebugLocation(Loc), operand, type, objc));
  }

  AllocValueBufferInst *
  createAllocValueBuffer(SILLocation Loc, SILType valueType, SILValue operand) {
    return insert(new (F.getModule()) AllocValueBufferInst(
        createSILDebugLocation(Loc), valueType, operand));
  }

  AllocBoxInst *createAllocBox(SILLocation Loc, SILType ElementType,
                               SILDebugVariable Var = SILDebugVariable()) {
    Loc.markAsPrologue();
    return insert(
        AllocBoxInst::create(createSILDebugLocation(Loc), ElementType, F, Var));
  }

  AllocExistentialBoxInst *
  createAllocExistentialBox(SILLocation Loc, SILType ExistentialType,
                            CanType ConcreteType, SILType ConcreteLoweredType,
                            ArrayRef<ProtocolConformance *> Conformances) {
    return insert(AllocExistentialBoxInst::create(
        createSILDebugLocation(Loc), ExistentialType, ConcreteType,
        ConcreteLoweredType, Conformances, &F));
  }

  ApplyInst *createApply(SILLocation Loc, SILValue Fn, SILType SubstFnTy,
                         SILType Result, ArrayRef<Substitution> Subs,
                         ArrayRef<SILValue> Args, bool isNonThrowing) {
    return insert(ApplyInst::create(createSILDebugLocation(Loc), Fn, SubstFnTy,
                                    Result, Subs, Args, isNonThrowing, F));
  }

  ApplyInst *createApply(SILLocation Loc, SILValue Fn, ArrayRef<SILValue> Args,
                         bool isNonThrowing) {
    auto FnTy = Fn.getType();
    return createApply(Loc, Fn, FnTy,
                       FnTy.castTo<SILFunctionType>()->getResult().getSILType(),
                       ArrayRef<Substitution>(), Args, isNonThrowing);
  }

  TryApplyInst *createTryApply(SILLocation Loc, SILValue fn, SILType substFnTy,
                               ArrayRef<Substitution> subs,
                               ArrayRef<SILValue> args, SILBasicBlock *normalBB,
                               SILBasicBlock *errorBB) {
    return insertTerminator(TryApplyInst::create(createSILDebugLocation(Loc),
                                                 fn, substFnTy, subs, args,
                                                 normalBB, errorBB, F));
  }

  PartialApplyInst *createPartialApply(SILLocation Loc, SILValue Fn,
                                       SILType SubstFnTy,
                                       ArrayRef<Substitution> Subs,
                                       ArrayRef<SILValue> Args,
                                       SILType ClosureTy) {
    return insert(PartialApplyInst::create(
        createSILDebugLocation(Loc), Fn, SubstFnTy, Subs, Args, ClosureTy, F));
  }

  BuiltinInst *createBuiltin(SILLocation Loc, Identifier Name, SILType ResultTy,
                             ArrayRef<Substitution> Subs,
                             ArrayRef<SILValue> Args) {
    return insert(BuiltinInst::create(createSILDebugLocation(Loc), Name,
                                      ResultTy, Subs, Args, F));
  }

  /// Create a binary function with the signature: OpdTy, OpdTy -> ResultTy.
  BuiltinInst *createBuiltinBinaryFunction(SILLocation Loc, StringRef Name,
                                           SILType OpdTy, SILType ResultTy,
                                           ArrayRef<SILValue> Args) {
    auto &C = getASTContext();

    llvm::SmallString<16> NameStr = Name;
    if (auto BuiltinIntTy =
            dyn_cast<BuiltinIntegerType>(OpdTy.getSwiftRValueType())) {
      if (BuiltinIntTy == BuiltinIntegerType::getWordType(getASTContext())) {
        NameStr += "_Word";
      } else {
        unsigned NumBits = BuiltinIntTy->getWidth().getFixedWidth();
        NameStr += "_Int" + llvm::utostr(NumBits);
      }
    } else {
      assert(OpdTy.getSwiftRValueType() == C.TheRawPointerType);
      NameStr += "_RawPointer";
    }
    auto Ident = C.getIdentifier(NameStr);
    return insert(BuiltinInst::create(createSILDebugLocation(Loc), Ident,
                                      ResultTy, {}, Args, F));
  }

  /// Create a binary function with the signature:
  /// OpdTy, OpdTy, Int1 -> (OpdTy, Int1)
  BuiltinInst *
  createBuiltinBinaryFunctionWithOverflow(SILLocation Loc, StringRef Name,
                                          ArrayRef<SILValue> Args) {
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

  FunctionRefInst *createFunctionRef(SILLocation Loc, SILFunction *f) {
    return insert(new (F.getModule())
                      FunctionRefInst(createSILDebugLocation(Loc), f));
  }
  GlobalAddrInst *createGlobalAddr(SILLocation Loc, SILGlobalVariable *g) {
    return insert(new (F.getModule())
                      GlobalAddrInst(createSILDebugLocation(Loc), g));
  }

  IntegerLiteralInst *createIntegerLiteral(IntegerLiteralExpr *E) {
    return insert(IntegerLiteralInst::create(E, createSILDebugLocation(E), F));
  }
  IntegerLiteralInst *createIntegerLiteral(SILLocation Loc, SILType Ty,
                                           intmax_t Value) {
    return insert(
        IntegerLiteralInst::create(createSILDebugLocation(Loc), Ty, Value, F));
  }
  IntegerLiteralInst *createIntegerLiteral(SILLocation Loc, SILType Ty,
                                           const APInt &Value) {
    return insert(
        IntegerLiteralInst::create(createSILDebugLocation(Loc), Ty, Value, F));
  }

  FloatLiteralInst *createFloatLiteral(FloatLiteralExpr *E) {
    return insert(FloatLiteralInst::create(E, createSILDebugLocation(E), F));
  }
  FloatLiteralInst *createFloatLiteral(SILLocation Loc, SILType Ty,
                                       const APFloat &Value) {
    return insert(
        FloatLiteralInst::create(createSILDebugLocation(Loc), Ty, Value, F));
  }

  StringLiteralInst *createStringLiteral(SILLocation Loc, StringRef text,
                                         StringLiteralInst::Encoding encoding) {
    return insert(StringLiteralInst::create(createSILDebugLocation(Loc), text,
                                            encoding, F));
  }

  StringLiteralInst *createStringLiteral(SILLocation Loc, const Twine &text,
                                         StringLiteralInst::Encoding encoding) {
    SmallVector<char, 256> Out;
    return insert(StringLiteralInst::create(
        createSILDebugLocation(Loc), text.toStringRef(Out), encoding, F));
  }

  LoadInst *createLoad(SILLocation Loc, SILValue LV) {
    return insert(new (F.getModule())
                      LoadInst(createSILDebugLocation(Loc), LV));
  }

  StoreInst *createStore(SILLocation Loc, SILValue Src, SILValue DestAddr) {
    return insert(new (F.getModule())
                      StoreInst(createSILDebugLocation(Loc), Src, DestAddr));
  }
  AssignInst *createAssign(SILLocation Loc, SILValue Src, SILValue DestAddr) {
    return insert(new (F.getModule())
                      AssignInst(createSILDebugLocation(Loc), Src, DestAddr));
  }

  MarkUninitializedInst *
  createMarkUninitialized(SILLocation Loc, SILValue src,
                          MarkUninitializedInst::Kind k) {
    return insert(new (F.getModule()) MarkUninitializedInst(
        createSILDebugLocation(Loc), src, k));
  }
  MarkUninitializedInst *createMarkUninitializedVar(SILLocation Loc,
                                                    SILValue src) {
    return createMarkUninitialized(Loc, src, MarkUninitializedInst::Var);
  }
  MarkUninitializedInst *createMarkUninitializedRootSelf(SILLocation Loc,
                                                         SILValue src) {
    return createMarkUninitialized(Loc, src, MarkUninitializedInst::RootSelf);
  }

  MarkFunctionEscapeInst *createMarkFunctionEscape(SILLocation Loc,
                                                   ArrayRef<SILValue> vars) {
    return insert(
        MarkFunctionEscapeInst::create(createSILDebugLocation(Loc), vars, F));
  }

  DebugValueInst *createDebugValue(SILLocation Loc, SILValue src,
                                   SILDebugVariable Var = SILDebugVariable()) {
    return insert(DebugValueInst::create(createSILDebugLocation(Loc), src,
                                         F.getModule(), Var));
  }
  DebugValueAddrInst *
  createDebugValueAddr(SILLocation Loc, SILValue src,
                       SILDebugVariable Var = SILDebugVariable()) {
    return insert(DebugValueAddrInst::create(createSILDebugLocation(Loc), src,
                                             F.getModule(), Var));
  }

  LoadWeakInst *createLoadWeak(SILLocation Loc, SILValue src, IsTake_t isTake) {
    return insert(new (F.getModule())
                      LoadWeakInst(createSILDebugLocation(Loc), src, isTake));
  }

  StoreWeakInst *createStoreWeak(SILLocation Loc, SILValue value, SILValue dest,
                                 IsInitialization_t isInit) {
    return insert(new (F.getModule()) StoreWeakInst(createSILDebugLocation(Loc),
                                                    value, dest, isInit));
  }

  LoadUnownedInst *createLoadUnowned(SILLocation loc, SILValue src,
                                     IsTake_t isTake) {
    return insert(new (F.getModule())
                    LoadUnownedInst(createSILDebugLocation(loc), src, isTake));
  }

  StoreUnownedInst *createStoreUnowned(SILLocation loc, SILValue value,
                                       SILValue dest,
                                       IsInitialization_t isInit) {
    return insert(new (F.getModule())
                    StoreUnownedInst(createSILDebugLocation(loc),
                                     value, dest, isInit));
  }

  CopyAddrInst *createCopyAddr(SILLocation Loc, SILValue srcAddr,
                               SILValue destAddr, IsTake_t isTake,
                               IsInitialization_t isInitialize) {
    assert(srcAddr.getType() == destAddr.getType());
    return insert(new (F.getModule()) CopyAddrInst(
        createSILDebugLocation(Loc), srcAddr, destAddr, isTake, isInitialize));
  }

  ConvertFunctionInst *createConvertFunction(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new (F.getModule())
                      ConvertFunctionInst(createSILDebugLocation(Loc), Op, Ty));
  }

  ThinFunctionToPointerInst *
  createThinFunctionToPointer(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ThinFunctionToPointerInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  PointerToThinFunctionInst *
  createPointerToThinFunction(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) PointerToThinFunctionInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  UpcastInst *createUpcast(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule())
                      UpcastInst(createSILDebugLocation(Loc), Op, Ty));
  }

  AddressToPointerInst *createAddressToPointer(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(new (F.getModule()) AddressToPointerInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  PointerToAddressInst *createPointerToAddress(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(new (F.getModule()) PointerToAddressInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  UncheckedRefCastInst *createUncheckedRefCast(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(new (F.getModule()) UncheckedRefCastInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  UncheckedRefCastAddrInst *
  createUncheckedRefCastAddr(SILLocation Loc, SILValue src, CanType sourceType,
                             SILValue dest, CanType targetType) {
    return insert(new (F.getModule()) UncheckedRefCastAddrInst(
        createSILDebugLocation(Loc), src, sourceType, dest, targetType));
  }

  UncheckedAddrCastInst *createUncheckedAddrCast(SILLocation Loc, SILValue Op,
                                                 SILType Ty) {
    return insert(new (F.getModule()) UncheckedAddrCastInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  UncheckedTrivialBitCastInst *
  createUncheckedTrivialBitCast(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) UncheckedTrivialBitCastInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  UncheckedBitwiseCastInst *
  createUncheckedBitwiseCast(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) UncheckedBitwiseCastInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  RefToBridgeObjectInst *createRefToBridgeObject(SILLocation Loc, SILValue Ref,
                                                 SILValue Bits) {
    auto Ty = SILType::getBridgeObjectType(getASTContext());
    return insert(new (F.getModule()) RefToBridgeObjectInst(
        createSILDebugLocation(Loc), Ref, Bits, Ty));
  }

  BridgeObjectToRefInst *createBridgeObjectToRef(SILLocation Loc, SILValue Op,
                                                 SILType Ty) {
    return insert(new (F.getModule()) BridgeObjectToRefInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  BridgeObjectToWordInst *createBridgeObjectToWord(SILLocation Loc,
                                                   SILValue Op) {
    auto Ty = SILType::getBuiltinWordType(getASTContext());
    return createBridgeObjectToWord(Loc, Op, Ty);
  }

  BridgeObjectToWordInst *createBridgeObjectToWord(SILLocation Loc, SILValue Op,
                                                   SILType Ty) {
    return insert(new (F.getModule()) BridgeObjectToWordInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  RefToRawPointerInst *createRefToRawPointer(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new (F.getModule())
                      RefToRawPointerInst(createSILDebugLocation(Loc), Op, Ty));
  }

  RawPointerToRefInst *createRawPointerToRef(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new (F.getModule())
                      RawPointerToRefInst(createSILDebugLocation(Loc), Op, Ty));
  }

  ThinToThickFunctionInst *createThinToThickFunction(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ThinToThickFunctionInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  ThickToObjCMetatypeInst *createThickToObjCMetatype(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ThickToObjCMetatypeInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  ObjCToThickMetatypeInst *createObjCToThickMetatype(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ObjCToThickMetatypeInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  ObjCProtocolInst *createObjCProtocol(SILLocation Loc, ProtocolDecl *P,
                                       SILType Ty) {
    return insert(new (F.getModule())
                      ObjCProtocolInst(createSILDebugLocation(Loc), P, Ty));
  }

  UnownedToRefInst *createUnownedToRef(SILLocation Loc, SILValue op,
                                       SILType ty) {
    return insert(new (F.getModule())
                      UnownedToRefInst(createSILDebugLocation(Loc), op, ty));
  }

  RefToUnownedInst *createRefToUnowned(SILLocation Loc, SILValue op,
                                       SILType ty) {
    return insert(new (F.getModule())
                      RefToUnownedInst(createSILDebugLocation(Loc), op, ty));
  }

  UnmanagedToRefInst *createUnmanagedToRef(SILLocation Loc, SILValue op,
                                           SILType ty) {
    return insert(new (F.getModule())
                      UnmanagedToRefInst(createSILDebugLocation(Loc), op, ty));
  }

  RefToUnmanagedInst *createRefToUnmanaged(SILLocation Loc, SILValue op,
                                           SILType ty) {
    return insert(new (F.getModule())
                      RefToUnmanagedInst(createSILDebugLocation(Loc), op, ty));
  }

  IsNonnullInst *createIsNonnull(SILLocation Loc, SILValue operand) {
    return insert(new (F.getModule()) IsNonnullInst(
        createSILDebugLocation(Loc), operand,
        SILType::getBuiltinIntegerType(1, getASTContext())));
  }

  UnconditionalCheckedCastInst *
  createUnconditionalCheckedCast(SILLocation Loc, SILValue op, SILType destTy) {
    return insert(new (F.getModule()) UnconditionalCheckedCastInst(
        createSILDebugLocation(Loc), op, destTy));
  }

  UnconditionalCheckedCastAddrInst *createUnconditionalCheckedCastAddr(
      SILLocation Loc, CastConsumptionKind consumption, SILValue src,
      CanType sourceType, SILValue dest, CanType targetType) {
    return insert(new (F.getModule()) UnconditionalCheckedCastAddrInst(
        createSILDebugLocation(Loc), consumption, src, sourceType, dest,
        targetType));
  }

  RetainValueInst *createRetainValue(SILLocation Loc, SILValue operand) {
    return insert(new (F.getModule())
                      RetainValueInst(createSILDebugLocation(Loc), operand));
  }

  ReleaseValueInst *createReleaseValue(SILLocation Loc, SILValue operand) {
    return insert(new (F.getModule())
                      ReleaseValueInst(createSILDebugLocation(Loc), operand));
  }

  AutoreleaseValueInst *createAutoreleaseValue(SILLocation Loc,
                                               SILValue operand) {
    return insert(new (F.getModule()) AutoreleaseValueInst(
        createSILDebugLocation(Loc), operand));
  }

  StructInst *createStruct(SILLocation Loc, SILType Ty,
                           ArrayRef<SILValue> Elements) {
    return insert(
        StructInst::create(createSILDebugLocation(Loc), Ty, Elements, F));
  }

  TupleInst *createTuple(SILLocation Loc, SILType Ty,
                         ArrayRef<SILValue> Elements) {
    return insert(
        TupleInst::create(createSILDebugLocation(Loc), Ty, Elements, F));
  }

  EnumInst *createEnum(SILLocation Loc, SILValue Operand,
                       EnumElementDecl *Element, SILType Ty) {
    return insert(new (F.getModule()) EnumInst(createSILDebugLocation(Loc),
                                               Operand, Element, Ty));
  }

  /// Inject a loadable value into the corresponding optional type.
  EnumInst *createOptionalSome(SILLocation Loc, SILValue operand, SILType ty) {
    return createOptionalSome(Loc, operand, ty.getOptionalTypeKind(), ty);
  }

  /// Inject a loadable value into the corresponding optional type.
  EnumInst *createOptionalSome(SILLocation Loc, SILValue operand,
                               OptionalTypeKind optKind, SILType ty) {
    assert(ty.getOptionalTypeKind() == optKind);
    auto someDecl = F.getModule().getASTContext().getOptionalSomeDecl(optKind);
    return createEnum(Loc, operand, someDecl, ty);
  }

  /// Create the nil value of a loadable optional type.
  EnumInst *createOptionalNone(SILLocation Loc, SILType ty) {
    return createOptionalNone(Loc, ty.getOptionalTypeKind(), ty);
  }

  /// Create the nil value of a loadable optional type.
  EnumInst *createOptionalNone(SILLocation Loc, OptionalTypeKind optKind,
                               SILType ty) {
    assert(ty.getOptionalTypeKind() == optKind);
    auto noneDecl = F.getModule().getASTContext().getOptionalNoneDecl(optKind);
    return createEnum(Loc, nullptr, noneDecl, ty);
  }

  InitEnumDataAddrInst *createInitEnumDataAddr(SILLocation Loc,
                                               SILValue Operand,
                                               EnumElementDecl *Element,
                                               SILType Ty) {
    return insert(new (F.getModule()) InitEnumDataAddrInst(
        createSILDebugLocation(Loc), Operand, Element, Ty));
  }

  UncheckedEnumDataInst *createUncheckedEnumData(SILLocation Loc,
                                                 SILValue Operand,
                                                 EnumElementDecl *Element,
                                                 SILType Ty) {
    return insert(new (F.getModule()) UncheckedEnumDataInst(
        createSILDebugLocation(Loc), Operand, Element, Ty));
  }

  UncheckedEnumDataInst *createUncheckedEnumData(SILLocation Loc,
                                                 SILValue Operand,
                                                 EnumElementDecl *Element) {
    SILType EltType =
        Operand.getType().getEnumElementType(Element, getModule());
    return createUncheckedEnumData(Loc, Operand, Element, EltType);
  }

  UncheckedTakeEnumDataAddrInst *
  createUncheckedTakeEnumDataAddr(SILLocation Loc, SILValue Operand,
                                  EnumElementDecl *Element, SILType Ty) {
    return insert(new (F.getModule()) UncheckedTakeEnumDataAddrInst(
        createSILDebugLocation(Loc), Operand, Element, Ty));
  }

  UncheckedTakeEnumDataAddrInst *
  createUncheckedTakeEnumDataAddr(SILLocation Loc, SILValue Operand,
                                  EnumElementDecl *Element) {
    SILType EltType =
        Operand.getType().getEnumElementType(Element, getModule());
    return createUncheckedTakeEnumDataAddr(Loc, Operand, Element, EltType);
  }

  InjectEnumAddrInst *createInjectEnumAddr(SILLocation Loc, SILValue Operand,
                                           EnumElementDecl *Element) {
    return insert(new (F.getModule()) InjectEnumAddrInst(
        createSILDebugLocation(Loc), Operand, Element));
  }

  SelectEnumInst *createSelectEnum(
      SILLocation Loc, SILValue Operand, SILType Ty, SILValue DefaultValue,
      ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues) {
    return insert(SelectEnumInst::create(createSILDebugLocation(Loc), Operand,
                                         Ty, DefaultValue, CaseValues, F));
  }

  SelectEnumAddrInst *createSelectEnumAddr(
      SILLocation Loc, SILValue Operand, SILType Ty, SILValue DefaultValue,
      ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues) {
    return insert(SelectEnumAddrInst::create(
        createSILDebugLocation(Loc), Operand, Ty, DefaultValue, CaseValues, F));
  }

  SelectValueInst *createSelectValue(
      SILLocation Loc, SILValue Operand, SILType Ty, SILValue DefaultResult,
      ArrayRef<std::pair<SILValue, SILValue>> CaseValuesAndResults) {
    return insert(SelectValueInst::create(createSILDebugLocation(Loc), Operand,
                                          Ty, DefaultResult,
                                          CaseValuesAndResults, F));
  }

  TupleExtractInst *createTupleExtract(SILLocation Loc, SILValue Operand,
                                       unsigned FieldNo, SILType ResultTy) {
    return insert(new (F.getModule()) TupleExtractInst(
        createSILDebugLocation(Loc), Operand, FieldNo, ResultTy));
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
    return insert(new (F.getModule()) TupleElementAddrInst(
        createSILDebugLocation(Loc), Operand, FieldNo, ResultTy));
  }

  TupleElementAddrInst *
  createTupleElementAddr(SILLocation Loc, SILValue Operand, unsigned FieldNo) {
    return insert(new (F.getModule()) TupleElementAddrInst(
        createSILDebugLocation(Loc), Operand, FieldNo,
        Operand.getType().getTupleElementType(FieldNo)));
  }

  StructExtractInst *createStructExtract(SILLocation Loc, SILValue Operand,
                                         VarDecl *Field, SILType ResultTy) {
    return insert(new (F.getModule()) StructExtractInst(
        createSILDebugLocation(Loc), Operand, Field, ResultTy));
  }

  StructExtractInst *createStructExtract(SILLocation Loc, SILValue Operand,
                                         VarDecl *Field) {
    auto type = Operand.getType().getFieldType(Field, F.getModule());
    return createStructExtract(Loc, Operand, Field, type);
  }

  StructElementAddrInst *createStructElementAddr(SILLocation Loc,
                                                 SILValue Operand,
                                                 VarDecl *Field,
                                                 SILType ResultTy) {
    return insert(new (F.getModule()) StructElementAddrInst(
        createSILDebugLocation(Loc), Operand, Field, ResultTy));
  }

  StructElementAddrInst *
  createStructElementAddr(SILLocation Loc, SILValue Operand, VarDecl *Field) {
    auto ResultTy = Operand.getType().getFieldType(Field, F.getModule());
    return createStructElementAddr(Loc, Operand, Field, ResultTy);
  }

  RefElementAddrInst *createRefElementAddr(SILLocation Loc, SILValue Operand,
                                           VarDecl *Field, SILType ResultTy) {
    return insert(new (F.getModule()) RefElementAddrInst(
        createSILDebugLocation(Loc), Operand, Field, ResultTy));
  }
  RefElementAddrInst *createRefElementAddr(SILLocation Loc, SILValue Operand,
                                           VarDecl *Field) {
    auto ResultTy = Operand.getType().getFieldType(Field, F.getModule());
    return createRefElementAddr(Loc, Operand, Field, ResultTy);
  }

  ClassMethodInst *createClassMethod(SILLocation Loc, SILValue Operand,
                                     SILDeclRef Member, SILType MethodTy,
                                     bool Volatile = false) {
    return insert(new (F.getModule()) ClassMethodInst(
        createSILDebugLocation(Loc), Operand, Member, MethodTy, Volatile));
  }

  ClassMethodInst *createClassMethod(SILLocation Loc, SILValue Operand,
                                     SILDeclRef Member, bool Volatile = false) {
    auto MethodTy = getModule().Types.getConstantOverrideType(Member);
    return createClassMethod(Loc, Operand, Member,
                             SILType::getPrimitiveObjectType(MethodTy),
                             Volatile);
  }

  /// Emit a class_method reference to the least derived overridden decl for
  /// the given method, and upcast the "self" pointer to the matching superclass
  /// type.
  std::pair<ClassMethodInst *, SILValue> emitClassMethod(SILLocation Loc,
                                                         SILValue Self,
                                                         SILDeclRef Member,
                                                         bool Volatile = false);

  SuperMethodInst *createSuperMethod(SILLocation Loc, SILValue Operand,
                                     SILDeclRef Member, SILType MethodTy,
                                     bool Volatile = false) {
    return insert(new (F.getModule()) SuperMethodInst(
        createSILDebugLocation(Loc), Operand, Member, MethodTy, Volatile));
  }

  WitnessMethodInst *createWitnessMethod(SILLocation Loc, CanType LookupTy,
                                         ProtocolConformance *Conformance,
                                         SILDeclRef Member, SILType MethodTy,
                                         SILValue OptionalOpenedExistential,
                                         bool Volatile = false) {
    return insert(WitnessMethodInst::create(
        createSILDebugLocation(Loc), LookupTy, Conformance, Member, MethodTy,
        &F, OptionalOpenedExistential, Volatile));
  }

  DynamicMethodInst *createDynamicMethod(SILLocation Loc, SILValue Operand,
                                         SILDeclRef Member, SILType MethodTy,
                                         bool Volatile = false) {
    return insert(new (F.getModule()) DynamicMethodInst(
        createSILDebugLocation(Loc), Operand, Member, MethodTy, Volatile));
  }

  OpenExistentialAddrInst *
  createOpenExistentialAddr(SILLocation Loc, SILValue Operand, SILType SelfTy) {
    return insert(new (F.getModule()) OpenExistentialAddrInst(
        createSILDebugLocation(Loc), Operand, SelfTy));
  }

  OpenExistentialMetatypeInst *createOpenExistentialMetatype(SILLocation Loc,
                                                             SILValue operand,
                                                             SILType selfTy) {
    return insert(new (F.getModule()) OpenExistentialMetatypeInst(
        createSILDebugLocation(Loc), operand, selfTy));
  }

  OpenExistentialRefInst *
  createOpenExistentialRef(SILLocation Loc, SILValue Operand, SILType Ty) {
    return insert(new (F.getModule()) OpenExistentialRefInst(
        createSILDebugLocation(Loc), Operand, Ty));
  }

  OpenExistentialBoxInst *
  createOpenExistentialBox(SILLocation Loc, SILValue Operand, SILType Ty) {
    return insert(new (F.getModule()) OpenExistentialBoxInst(
        createSILDebugLocation(Loc), Operand, Ty));
  }

  InitExistentialAddrInst *
  createInitExistentialAddr(SILLocation Loc, SILValue Existential,
                            CanType FormalConcreteType,
                            SILType LoweredConcreteType,
                            ArrayRef<ProtocolConformance *> Conformances) {
    return insert(InitExistentialAddrInst::create(
        createSILDebugLocation(Loc), Existential, FormalConcreteType,
        LoweredConcreteType, Conformances, &F));
  }

  InitExistentialMetatypeInst *
  createInitExistentialMetatype(SILLocation Loc, SILValue metatype,
                                SILType existentialType,
                                ArrayRef<ProtocolConformance *> conformances) {
    return insert(InitExistentialMetatypeInst::create(
        createSILDebugLocation(Loc), existentialType, metatype, conformances,
        &F));
  }

  InitExistentialRefInst *
  createInitExistentialRef(SILLocation Loc, SILType ExistentialType,
                           CanType FormalConcreteType, SILValue Concrete,
                           ArrayRef<ProtocolConformance *> Conformances) {
    return insert(InitExistentialRefInst::create(
        createSILDebugLocation(Loc), ExistentialType, FormalConcreteType,
        Concrete, Conformances, &F));
  }

  DeinitExistentialAddrInst *createDeinitExistentialAddr(SILLocation Loc,
                                                         SILValue Existential) {
    return insert(new (F.getModule()) DeinitExistentialAddrInst(
        createSILDebugLocation(Loc), Existential));
  }

  ProjectBlockStorageInst *createProjectBlockStorage(SILLocation Loc,
                                                     SILValue Storage) {
    auto CaptureTy = Storage.getType()
                         .castTo<SILBlockStorageType>()
                         ->getCaptureAddressType();
    return createProjectBlockStorage(Loc, Storage, CaptureTy);
  }
  ProjectBlockStorageInst *createProjectBlockStorage(SILLocation Loc,
                                                     SILValue Storage,
                                                     SILType CaptureTy) {
    return insert(new (F.getModule()) ProjectBlockStorageInst(
        createSILDebugLocation(Loc), Storage, CaptureTy));
  }

  InitBlockStorageHeaderInst *
  createInitBlockStorageHeader(SILLocation Loc, SILValue BlockStorage,
                               SILValue InvokeFunction, SILType BlockType) {
    return insert(new (F.getModule()) InitBlockStorageHeaderInst(
        createSILDebugLocation(Loc), BlockStorage, InvokeFunction, BlockType));
  }

  MetatypeInst *createMetatype(SILLocation Loc, SILType Metatype) {
    return insert(new (F.getModule())
                      MetatypeInst(createSILDebugLocation(Loc), Metatype));
  }

  ObjCMetatypeToObjectInst *
  createObjCMetatypeToObject(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ObjCMetatypeToObjectInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  ObjCExistentialMetatypeToObjectInst *
  createObjCExistentialMetatypeToObject(SILLocation Loc, SILValue Op,
                                        SILType Ty) {
    return insert(new (F.getModule()) ObjCExistentialMetatypeToObjectInst(
        createSILDebugLocation(Loc), Op, Ty));
  }

  ValueMetatypeInst *createValueMetatype(SILLocation Loc, SILType Metatype,
                                         SILValue Base) {
    return insert(new (F.getModule()) ValueMetatypeInst(
        createSILDebugLocation(Loc), Metatype, Base));
  }

  ExistentialMetatypeInst *
  createExistentialMetatype(SILLocation Loc, SILType Metatype, SILValue Base) {
    return insert(new (F.getModule()) ExistentialMetatypeInst(
        createSILDebugLocation(Loc), Metatype, Base));
  }

  CopyBlockInst *createCopyBlock(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      CopyBlockInst(createSILDebugLocation(Loc), Operand));
  }
  StrongRetainInst *createStrongRetain(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      StrongRetainInst(createSILDebugLocation(Loc), Operand));
  }
  StrongReleaseInst *createStrongRelease(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      StrongReleaseInst(createSILDebugLocation(Loc), Operand));
  }
  StrongPinInst *createStrongPin(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      StrongPinInst(createSILDebugLocation(Loc), Operand));
  }
  StrongUnpinInst *createStrongUnpin(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      StrongUnpinInst(createSILDebugLocation(Loc), Operand));
  }
  StrongRetainUnownedInst *createStrongRetainUnowned(SILLocation Loc,
                                                     SILValue Operand) {
    return insert(new (F.getModule()) StrongRetainUnownedInst(
        createSILDebugLocation(Loc), Operand));
  }
  UnownedRetainInst *createUnownedRetain(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      UnownedRetainInst(createSILDebugLocation(Loc), Operand));
  }
  UnownedReleaseInst *createUnownedRelease(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      UnownedReleaseInst(createSILDebugLocation(Loc), Operand));
  }
  FixLifetimeInst *createFixLifetime(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      FixLifetimeInst(createSILDebugLocation(Loc), Operand));
  }
  void emitFixLifetime(SILLocation Loc, SILValue Operand) {
    if (getTypeLowering(Operand.getType()).isTrivial())
      return;
    createFixLifetime(Loc, Operand);
  }
  MarkDependenceInst *createMarkDependence(SILLocation Loc, SILValue value,
                                           SILValue base) {
    return insert(new (F.getModule()) MarkDependenceInst(
        createSILDebugLocation(Loc), value, base));
  }
  IsUniqueInst *createIsUnique(SILLocation Loc, SILValue operand) {
    auto Int1Ty = SILType::getBuiltinIntegerType(1, getASTContext());
    return insert(new (F.getModule()) IsUniqueInst(createSILDebugLocation(Loc),
                                                   operand, Int1Ty));
  }
  IsUniqueOrPinnedInst *createIsUniqueOrPinned(SILLocation Loc,
                                               SILValue value) {
    auto Int1Ty = SILType::getBuiltinIntegerType(1, getASTContext());
    return insert(new (F.getModule()) IsUniqueOrPinnedInst(
        createSILDebugLocation(Loc), value, Int1Ty));
  }

  DeallocStackInst *createDeallocStack(SILLocation Loc, SILValue operand) {
    return insert(new (F.getModule())
                      DeallocStackInst(createSILDebugLocation(Loc), operand));
  }
  DeallocRefInst *createDeallocRef(SILLocation Loc, SILValue operand,
                                   bool canBeOnStack) {
    return insert(new (F.getModule()) DeallocRefInst(
        createSILDebugLocation(Loc), operand, canBeOnStack));
  }
  DeallocPartialRefInst *createDeallocPartialRef(SILLocation Loc,
                                                 SILValue operand,
                                                 SILValue metatype) {
    return insert(new (F.getModule()) DeallocPartialRefInst(
        createSILDebugLocation(Loc), operand, metatype));
  }
  DeallocBoxInst *createDeallocBox(SILLocation Loc, SILType eltType,
                                   SILValue operand) {
    return insert(new (F.getModule()) DeallocBoxInst(
        createSILDebugLocation(Loc), eltType, operand));
  }
  DeallocBoxInst *createDeallocBox(SILLocation Loc, SILValue operand) {
    auto eltType =
        operand.getType().castTo<SILBoxType>()->getBoxedAddressType();
    return insert(new (F.getModule()) DeallocBoxInst(
        createSILDebugLocation(Loc), eltType, operand));
  }
  DeallocExistentialBoxInst *createDeallocExistentialBox(SILLocation Loc,
                                                         CanType concreteType,
                                                         SILValue operand) {
    return insert(new (F.getModule()) DeallocExistentialBoxInst(
        createSILDebugLocation(Loc), concreteType, operand));
  }
  DeallocValueBufferInst *createDeallocValueBuffer(SILLocation Loc,
                                                   SILType valueType,
                                                   SILValue operand) {
    return insert(new (F.getModule()) DeallocValueBufferInst(
        createSILDebugLocation(Loc), valueType, operand));
  }
  DestroyAddrInst *createDestroyAddr(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      DestroyAddrInst(createSILDebugLocation(Loc), Operand));
  }

  ProjectValueBufferInst *createProjectValueBuffer(SILLocation Loc,
                                                   SILType valueType,
                                                   SILValue operand) {
    return insert(new (F.getModule()) ProjectValueBufferInst(
        createSILDebugLocation(Loc), valueType, operand));
  }
  ProjectBoxInst *createProjectBox(SILLocation Loc, SILValue boxOperand) {
    auto valueTy =
        boxOperand.getType().castTo<SILBoxType>()->getBoxedAddressType();

    return insert(new (F.getModule()) ProjectBoxInst(
        createSILDebugLocation(Loc), valueTy, boxOperand));
  }
  ProjectBoxInst *createProjectBox(SILLocation Loc, SILType valueTy,
                                   SILValue boxOperand) {
    return insert(new (F.getModule()) ProjectBoxInst(
        createSILDebugLocation(Loc), valueTy, boxOperand));
  }

  //===--------------------------------------------------------------------===//
  // Unchecked cast helpers
  //===--------------------------------------------------------------------===//

  // Create an UncheckedRefCast if the source and dest types are legal,
  // otherwise return null.
  // Unwrap or wrap optional types as needed.
  SILInstruction *tryCreateUncheckedRefCast(SILLocation Loc, SILValue Op,
                                            SILType ResultTy);

  // Create the appropriate cast instruction based on result type.
  SILInstruction *createUncheckedBitCast(SILLocation Loc, SILValue Op,
                                         SILType Ty);

  //===--------------------------------------------------------------------===//
  // Runtime failure
  //===--------------------------------------------------------------------===//

  CondFailInst *createCondFail(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      CondFailInst(createSILDebugLocation(Loc), Operand));
  }

  BuiltinInst *createBuiltinTrap(SILLocation Loc) {
    ASTContext &AST = F.getModule().getASTContext();
    auto Id_trap = AST.getIdentifier("int_trap");
    return createBuiltin(Loc, Id_trap, F.getModule().Types.getEmptyTupleType(),
                         {}, {});
  }

  //===--------------------------------------------------------------------===//
  // Array indexing instructions
  //===--------------------------------------------------------------------===//

  IndexAddrInst *createIndexAddr(SILLocation Loc, SILValue Operand,
                                 SILValue Index) {
    return insert(new (F.getModule()) IndexAddrInst(createSILDebugLocation(Loc),
                                                    Operand, Index));
  }

  IndexRawPointerInst *createIndexRawPointer(SILLocation Loc, SILValue Operand,
                                             SILValue Index) {
    return insert(new (F.getModule()) IndexRawPointerInst(
        createSILDebugLocation(Loc), Operand, Index));
  }

  //===--------------------------------------------------------------------===//
  // Terminator SILInstruction Creation Methods
  //===--------------------------------------------------------------------===//

  UnreachableInst *createUnreachable(SILLocation Loc) {
    return insertTerminator(new (F.getModule())
                                UnreachableInst(createSILDebugLocation(Loc)));
  }

  ReturnInst *createReturn(SILLocation Loc, SILValue ReturnValue) {
    return insertTerminator(new (F.getModule()) ReturnInst(
        createSILDebugLocation(Loc), ReturnValue));
  }

  ThrowInst *createThrow(SILLocation Loc, SILValue errorValue) {
    return insertTerminator(
        new (F.getModule()) ThrowInst(createSILDebugLocation(Loc), errorValue));
  }

  CondBranchInst *createCondBranch(SILLocation Loc, SILValue Cond,
                                   SILBasicBlock *Target1,
                                   SILBasicBlock *Target2) {
    return insertTerminator(CondBranchInst::create(createSILDebugLocation(Loc),
                                                   Cond, Target1, Target2, F));
  }

  CondBranchInst *createCondBranch(SILLocation Loc, SILValue Cond,
                                   SILBasicBlock *Target1,
                                   ArrayRef<SILValue> Args1,
                                   SILBasicBlock *Target2,
                                   ArrayRef<SILValue> Args2) {
    return insertTerminator(CondBranchInst::create(
        createSILDebugLocation(Loc), Cond, Target1, Args1, Target2, Args2, F));
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

    return insertTerminator(CondBranchInst::create(createSILDebugLocation(Loc),
                                                   Cond, Target1, ArgsCopy1,
                                                   Target2, ArgsCopy2, F));
  }

  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock) {
    return insertTerminator(
        BranchInst::create(createSILDebugLocation(Loc), TargetBlock, F));
  }

  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock,
                           ArrayRef<SILValue> Args) {
    return insertTerminator(
        BranchInst::create(createSILDebugLocation(Loc), TargetBlock, Args, F));
  }

  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock,
                           OperandValueArrayRef Args);

  SwitchValueInst *
  createSwitchValue(SILLocation Loc, SILValue Operand, SILBasicBlock *DefaultBB,
                    ArrayRef<std::pair<SILValue, SILBasicBlock *>> CaseBBs) {
    return insertTerminator(SwitchValueInst::create(
        createSILDebugLocation(Loc), Operand, DefaultBB, CaseBBs, F));
  }

  SwitchEnumInst *createSwitchEnum(
      SILLocation Loc, SILValue Operand, SILBasicBlock *DefaultBB,
      ArrayRef<std::pair<EnumElementDecl *, SILBasicBlock *>> CaseBBs) {
    return insertTerminator(SwitchEnumInst::create(
        createSILDebugLocation(Loc), Operand, DefaultBB, CaseBBs, F));
  }

  SwitchEnumAddrInst *createSwitchEnumAddr(
      SILLocation Loc, SILValue Operand, SILBasicBlock *DefaultBB,
      ArrayRef<std::pair<EnumElementDecl *, SILBasicBlock *>> CaseBBs) {
    return insertTerminator(SwitchEnumAddrInst::create(
        createSILDebugLocation(Loc), Operand, DefaultBB, CaseBBs, F));
  }

  DynamicMethodBranchInst *
  createDynamicMethodBranch(SILLocation Loc, SILValue Operand,
                            SILDeclRef Member, SILBasicBlock *HasMethodBB,
                            SILBasicBlock *NoMethodBB) {
    return insertTerminator(
        DynamicMethodBranchInst::create(createSILDebugLocation(Loc), Operand,
                                        Member, HasMethodBB, NoMethodBB, F));
  }

  CheckedCastBranchInst *createCheckedCastBranch(SILLocation Loc, bool isExact,
                                                 SILValue op, SILType destTy,
                                                 SILBasicBlock *successBB,
                                                 SILBasicBlock *failureBB) {
    return insertTerminator(new (F.getModule()) CheckedCastBranchInst(
        createSILDebugLocation(Loc), isExact, op, destTy, successBB,
        failureBB));
  }

  CheckedCastAddrBranchInst *
  createCheckedCastAddrBranch(SILLocation Loc, CastConsumptionKind consumption,
                              SILValue src, CanType sourceType, SILValue dest,
                              CanType targetType, SILBasicBlock *successBB,
                              SILBasicBlock *failureBB) {
    return insertTerminator(new (F.getModule()) CheckedCastAddrBranchInst(
        createSILDebugLocation(Loc), consumption, src, sourceType, dest,
        targetType, successBB, failureBB));
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
  void emitRetainValueOperation(SILLocation Loc, SILValue v) {
    assert(!v.getType().isAddress());
    auto &lowering = getTypeLowering(v.getType());
    return lowering.emitRetainValue(*this, Loc, v);
  }

  /// Convenience function for calling TypeLowering.emitRelease on the type
  /// lowering for the non-address value.
  void emitReleaseValueOperation(SILLocation Loc, SILValue v) {
    assert(!v.getType().isAddress());
    auto &lowering = getTypeLowering(v.getType());
    lowering.emitReleaseValue(*this, Loc, v);
  }

  SILValue emitTupleExtract(SILLocation Loc, SILValue Operand, unsigned FieldNo,
                            SILType ResultTy) {
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

  SILValue emitStructExtract(SILLocation Loc, SILValue Operand, VarDecl *Field,
                             SILType ResultTy) {
    if (auto *SI = dyn_cast<StructInst>(Operand))
      return SI->getFieldValue(Field);

    return createStructExtract(Loc, Operand, Field, ResultTy);
  }

  SILValue emitStructExtract(SILLocation Loc, SILValue Operand,
                             VarDecl *Field) {
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
  template <typename T> T *insert(T *TheInst) {
    insertImpl(TheInst);
    return TheInst;
  }

  /// insertTerminator - This is the same as insert, but clears the insertion
  /// point after doing the insertion.  This is used by terminators, since it
  /// isn't valid to insert something after a terminator.
  template <typename T> T *insertTerminator(T *TheInst) {
    insertImpl(TheInst);
    clearInsertionPoint();
    return TheInst;
  }

  void insertImpl(SILInstruction *TheInst) {
    if (BB == 0)
      return;

    // If the SILBuilder client wants to know about new instructions, record
    // this.
    if (InsertedInstrs)
      InsertedInstrs->push_back(TheInst);

    BB->insert(InsertPt, TheInst);
  }

  void cacheDebugLoc(SILDebugLocation &Loc) {
    DebugLocs.insert({SILDebugLocationID(Loc), &Loc});
  }  
};

/// An RAII version of SILBuilder that automatically sets up identical
/// SILDebugScopes for all instructions.  This is useful for
/// situations where a single SIL instruction is lowered into a
/// sequence of SIL instructions.
class SILBuilderWithScope : public SILBuilder {
  void inheritScopeFrom(SILInstruction *I) {
    assert(I->getDebugScope() && "instruction has no debug scope");
    setCurrentDebugScope(I->getDebugScope());
    cacheDebugLoc(I->getDebugLocation());
  }

public:
  explicit SILBuilderWithScope(
      SILInstruction *I,
      SmallVectorImpl<SILInstruction *> *InsertedInstrs = nullptr)
      : SILBuilder(I, InsertedInstrs) {
    assert(I->getDebugScope() && "instruction has no debug scope");
    setCurrentDebugScope(I->getDebugScope());
  }

  explicit SILBuilderWithScope(SILBasicBlock::iterator I)
      : SILBuilderWithScope(&*I) {}

  explicit SILBuilderWithScope(SILInstruction *I,
                               SILInstruction *InheritScopeFrom)
      : SILBuilderWithScope(I) {
    inheritScopeFrom(InheritScopeFrom);
  }

  explicit SILBuilderWithScope(SILBasicBlock::iterator I,
                               SILInstruction *InheritScopeFrom)
      : SILBuilderWithScope(&*I) {
    inheritScopeFrom(InheritScopeFrom);
  }

  explicit SILBuilderWithScope(SILBasicBlock *BB,
                               SILInstruction *InheritScopeFrom)
      : SILBuilder(BB) {
    inheritScopeFrom(InheritScopeFrom);
  }
};

} // end swift namespace

#endif
