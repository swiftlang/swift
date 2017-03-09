//===--- SILBuilder.h - Class for creating SIL Constructs -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILBUILDER_H
#define SWIFT_SIL_SILBUILDER_H

#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILOpenedArchetypesTracker.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringExtras.h"

namespace swift {

using Atomicity = RefCountingInst::Atomicity;

class SILDebugScope;

class SILBuilder {
  friend class SILBuilderWithScope;

  SILFunction &F;
  /// If this is non-null, the instruction is inserted in the specified
  /// basic block, at the specified InsertPt.  If null, created instructions
  /// are not auto-inserted.
  SILBasicBlock *BB;
  SILBasicBlock::iterator InsertPt;
  const SILDebugScope *CurDebugScope = nullptr;

  /// If this pointer is non-null, then any inserted instruction is
  /// recorded in this list.
  SmallVectorImpl<SILInstruction *> *InsertedInstrs = nullptr;

  /// An immutable view on the set of available opened archetypes.
  /// It is passed down to SILInstruction constructors and create
  /// methods.
  SILOpenedArchetypesState OpenedArchetypes;

  /// Maps opened archetypes to their definitions. If provided,
  /// can be used by the builder. It is supposed to be used
  /// only by SILGen or SIL deserializers.
  SILOpenedArchetypesTracker *OpenedArchetypesTracker = nullptr;

  /// True if this SILBuilder is being used for parsing.
  ///
  /// This is important since in such a case, we want to not perform any
  /// Ownership Verification in SILBuilder. This functionality is very useful
  /// for determining if qualified or unqualified instructions are being created
  /// in appropriate places, but prevents us from inferring ownership
  /// qualification of functions when parsing. The ability to perform this
  /// inference is important since otherwise, we would need to update all SIL
  /// test cases while bringing up SIL ownership.
  bool isParsing = false;

public:
  SILBuilder(SILFunction &F, bool isParsing = false)
      : F(F), BB(0), isParsing(isParsing) {}

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

  void setOpenedArchetypesTracker(SILOpenedArchetypesTracker *Tracker) {
    this->OpenedArchetypesTracker = Tracker;
    this->OpenedArchetypes.setOpenedArchetypesTracker(OpenedArchetypesTracker);
  }

  SILOpenedArchetypesTracker *getOpenedArchetypesTracker() const {
    return OpenedArchetypesTracker;
  }

  SILOpenedArchetypesState &getOpenedArchetypes() {
    return OpenedArchetypes;
  }

  void setCurrentDebugScope(const SILDebugScope *DS) { CurDebugScope = DS; }
  const SILDebugScope *getCurrentDebugScope() const { return CurDebugScope; }

  /// Convenience function for building a SILDebugLocation.
  SILDebugLocation getSILDebugLocation(SILLocation Loc) {
    // FIXME: Audit all uses and enable this assertion.
    // assert(getCurrentDebugScope() && "no debug scope");
    auto Scope = getCurrentDebugScope();
    return SILDebugLocation(Loc, Scope ? Scope : getFunction().getDebugScope());
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
    if (InsertPt == BB->end())
      return;
    // Set the opened archetype context from the instruction.
    this->getOpenedArchetypes().addOpenedArchetypeOperands(
        InsertPt->getTypeDependentOperands());
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
  // Opened archetypes handling
  //===--------------------------------------------------------------------===//
  void addOpenedArchetypeOperands(SILInstruction *I) {
    getOpenedArchetypes().addOpenedArchetypeOperands(
        I->getTypeDependentOperands());
  }

  //===--------------------------------------------------------------------===//
  // Type remapping
  //===--------------------------------------------------------------------===//

  static SILType getPartialApplyResultType(SILType Ty, unsigned ArgCount,
                                         SILModule &M,
                                         SubstitutionList subs,
                                         ParameterConvention calleeConvention);

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
    return insert(AllocStackInst::create(getSILDebugLocation(Loc),
                                         elementType, F, OpenedArchetypes,
                                         Var));
  }

  AllocRefInst *createAllocRef(SILLocation Loc, SILType ObjectType,
                               bool objc, bool canAllocOnStack,
                               ArrayRef<SILType> ElementTypes,
                               ArrayRef<SILValue> ElementCountOperands) {
    // AllocRefInsts expand to function calls and can therefore not be
    // counted towards the function prologue.
    assert(!Loc.isInPrologue());
    return insert(AllocRefInst::create(getSILDebugLocation(Loc),
                                       F, ObjectType, objc, canAllocOnStack,
                                       ElementTypes, ElementCountOperands,
                                       OpenedArchetypes));
  }

  AllocRefDynamicInst *createAllocRefDynamic(SILLocation Loc, SILValue operand,
                                             SILType type, bool objc,
                                    ArrayRef<SILType> ElementTypes,
                                    ArrayRef<SILValue> ElementCountOperands) {
    // AllocRefDynamicInsts expand to function calls and can therefore
    // not be counted towards the function prologue.
    assert(!Loc.isInPrologue());
    return insert(AllocRefDynamicInst::create(getSILDebugLocation(Loc), F,
                                              operand, type, objc,
                                              ElementTypes,
                                              ElementCountOperands,
                                              OpenedArchetypes));
  }

  AllocValueBufferInst *
  createAllocValueBuffer(SILLocation Loc, SILType valueType, SILValue operand) {
    return insert(AllocValueBufferInst::create(
        getSILDebugLocation(Loc), valueType, operand, F, OpenedArchetypes));
  }

  AllocBoxInst *createAllocBox(SILLocation Loc, CanSILBoxType BoxType,
                               SILDebugVariable Var = SILDebugVariable()) {
    Loc.markAsPrologue();
    return insert(AllocBoxInst::create(getSILDebugLocation(Loc), BoxType, F,
                                       OpenedArchetypes, Var));
  }

  AllocExistentialBoxInst *
  createAllocExistentialBox(SILLocation Loc, SILType ExistentialType,
                            CanType ConcreteType,
                            ArrayRef<ProtocolConformanceRef> Conformances) {
    return insert(AllocExistentialBoxInst::create(
        getSILDebugLocation(Loc), ExistentialType, ConcreteType, Conformances,
        &F, OpenedArchetypes));
  }

  ApplyInst *createApply(SILLocation Loc, SILValue Fn, SILType SubstFnTy,
                         SILType Result, SubstitutionList Subs,
                         ArrayRef<SILValue> Args, bool isNonThrowing) {
    return insert(ApplyInst::create(getSILDebugLocation(Loc), Fn, SubstFnTy,
                                    Result, Subs, Args, isNonThrowing, F,
                                    OpenedArchetypes));
  }

  ApplyInst *createApply(SILLocation Loc, SILValue Fn, ArrayRef<SILValue> Args,
                         bool isNonThrowing) {
    SILFunctionConventions conventions(Fn->getType().castTo<SILFunctionType>(),
                                       getModule());
    return createApply(Loc, Fn, Fn->getType(), conventions.getSILResultType(),
                       SubstitutionList(), Args, isNonThrowing);
  }

  TryApplyInst *createTryApply(SILLocation Loc, SILValue fn, SILType substFnTy,
                               SubstitutionList subs,
                               ArrayRef<SILValue> args, SILBasicBlock *normalBB,
                               SILBasicBlock *errorBB) {
    return insertTerminator(TryApplyInst::create(getSILDebugLocation(Loc),
                                                 fn, substFnTy, subs, args,
                                                 normalBB, errorBB, F,
                                                 OpenedArchetypes));
  }

  PartialApplyInst *createPartialApply(SILLocation Loc, SILValue Fn,
                                       SILType SubstFnTy,
                                       SubstitutionList Subs,
                                       ArrayRef<SILValue> Args,
                                       SILType ClosureTy) {
    return insert(PartialApplyInst::create(getSILDebugLocation(Loc), Fn,
                                           SubstFnTy, Subs, Args, ClosureTy, F,
                                           OpenedArchetypes));
  }

  BuiltinInst *createBuiltin(SILLocation Loc, Identifier Name, SILType ResultTy,
                             SubstitutionList Subs,
                             ArrayRef<SILValue> Args) {
    return insert(BuiltinInst::create(getSILDebugLocation(Loc), Name,
                                      ResultTy, Subs, Args, F));
  }

  /// Create a binary function with the signature: OpdTy, OpdTy -> ResultTy.
  BuiltinInst *createBuiltinBinaryFunction(SILLocation Loc, StringRef Name,
                                           SILType OpdTy, SILType ResultTy,
                                           ArrayRef<SILValue> Args) {
    auto &C = getASTContext();

    llvm::SmallString<16> NameStr = Name;
    appendOperandTypeName(OpdTy, NameStr);
    auto Ident = C.getIdentifier(NameStr);
    return insert(BuiltinInst::create(getSILDebugLocation(Loc), Ident, ResultTy,
                                      {}, Args, F));
  }

  // Create a binary function with the signature: OpdTy1, OpdTy2 -> ResultTy.
  BuiltinInst *createBuiltinBinaryFunctionWithTwoOpTypes(
      SILLocation Loc, StringRef Name, SILType OpdTy1, SILType OpdTy2,
      SILType ResultTy, ArrayRef<SILValue> Args) {
    auto &C = getASTContext();

    llvm::SmallString<16> NameStr = Name;
    appendOperandTypeName(OpdTy1, NameStr);
    appendOperandTypeName(OpdTy2, NameStr);
    auto Ident = C.getIdentifier(NameStr);
    return insert(BuiltinInst::create(getSILDebugLocation(Loc), Ident,
                                      ResultTy, {}, Args, F));
  }

  /// Create a binary function with the signature:
  /// OpdTy, OpdTy, Int1 -> (OpdTy, Int1)
  BuiltinInst *
  createBuiltinBinaryFunctionWithOverflow(SILLocation Loc, StringRef Name,
                                          ArrayRef<SILValue> Args) {
    assert(Args.size() == 3 && "Need three arguments");
    assert(Args[0]->getType() == Args[1]->getType() &&
           "Binary operands must match");
    assert(Args[2]->getType().is<BuiltinIntegerType>() &&
           Args[2]->getType().getSwiftRValueType()->isBuiltinIntegerType(1) &&
           "Must have a third Int1 operand");

    SILType OpdTy = Args[0]->getType();
    SILType Int1Ty = Args[2]->getType();

    TupleTypeElt ResultElts[] = {OpdTy.getSwiftRValueType(),
                                 Int1Ty.getSwiftRValueType()};
    Type ResultTy = TupleType::get(ResultElts, getASTContext());
    SILType SILResultTy =
        SILType::getPrimitiveObjectType(ResultTy->getCanonicalType());

    return createBuiltinBinaryFunction(Loc, Name, OpdTy, SILResultTy, Args);
  }

  FunctionRefInst *createFunctionRef(SILLocation Loc, SILFunction *f) {
    return insert(new (F.getModule())
                      FunctionRefInst(getSILDebugLocation(Loc), f));
  }
  AllocGlobalInst *createAllocGlobal(SILLocation Loc, SILGlobalVariable *g) {
    return insert(new (F.getModule())
                      AllocGlobalInst(getSILDebugLocation(Loc), g));
  }
  GlobalAddrInst *createGlobalAddr(SILLocation Loc, SILGlobalVariable *g) {
    return insert(new (F.getModule())
                      GlobalAddrInst(getSILDebugLocation(Loc), g));
  }

  IntegerLiteralInst *createIntegerLiteral(IntegerLiteralExpr *E) {
    return insert(IntegerLiteralInst::create(E, getSILDebugLocation(E), F));
  }
  IntegerLiteralInst *createIntegerLiteral(SILLocation Loc, SILType Ty,
                                           intmax_t Value) {
    return insert(
        IntegerLiteralInst::create(getSILDebugLocation(Loc), Ty, Value, F));
  }
  IntegerLiteralInst *createIntegerLiteral(SILLocation Loc, SILType Ty,
                                           const APInt &Value) {
    return insert(
        IntegerLiteralInst::create(getSILDebugLocation(Loc), Ty, Value, F));
  }

  FloatLiteralInst *createFloatLiteral(FloatLiteralExpr *E) {
    return insert(FloatLiteralInst::create(E, getSILDebugLocation(E), F));
  }
  FloatLiteralInst *createFloatLiteral(SILLocation Loc, SILType Ty,
                                       const APFloat &Value) {
    return insert(
        FloatLiteralInst::create(getSILDebugLocation(Loc), Ty, Value, F));
  }

  StringLiteralInst *createStringLiteral(SILLocation Loc, StringRef text,
                                         StringLiteralInst::Encoding encoding) {
    return insert(StringLiteralInst::create(getSILDebugLocation(Loc), text,
                                            encoding, F));
  }

  StringLiteralInst *createStringLiteral(SILLocation Loc, const Twine &text,
                                         StringLiteralInst::Encoding encoding) {
    SmallVector<char, 256> Out;
    return insert(StringLiteralInst::create(
        getSILDebugLocation(Loc), text.toStringRef(Out), encoding, F));
  }

  LoadInst *createLoad(SILLocation Loc, SILValue LV,
                       LoadOwnershipQualifier Qualifier) {
    assert((Qualifier != LoadOwnershipQualifier::Unqualified) ||
           F.hasUnqualifiedOwnership() &&
               "Unqualified inst in qualified function");
    assert((Qualifier == LoadOwnershipQualifier::Unqualified) ||
           F.hasQualifiedOwnership() &&
               "Qualified inst in unqualified function");
    assert(!SILModuleConventions(F.getModule()).useLoweredAddresses()
           || LV->getType().isLoadable(F.getModule()));
    return insert(new (F.getModule())
                      LoadInst(getSILDebugLocation(Loc), LV, Qualifier));
  }

  /// Convenience function for calling emitLoad on the type lowering for
  /// non-address values.
  SILValue emitLoadValueOperation(SILLocation Loc, SILValue LV,
                                  LoadOwnershipQualifier Qualifier) {
    assert(!SILModuleConventions(F.getModule()).useLoweredAddresses()
           || LV->getType().isLoadable(F.getModule()));
    const auto &lowering = getTypeLowering(LV->getType());
    return lowering.emitLoad(*this, Loc, LV, Qualifier);
  }

  LoadBorrowInst *createLoadBorrow(SILLocation Loc, SILValue LV) {
    assert(LV->getType().isLoadable(F.getModule()));
    return insert(new (F.getModule())
                      LoadBorrowInst(getSILDebugLocation(Loc), LV));
  }

  BeginBorrowInst *createBeginBorrow(SILLocation Loc, SILValue LV) {
    return insert(new (F.getModule())
                      BeginBorrowInst(getSILDebugLocation(Loc), LV));
  }

  StoreInst *createStore(SILLocation Loc, SILValue Src, SILValue DestAddr,
                         StoreOwnershipQualifier Qualifier) {
    assert((Qualifier != StoreOwnershipQualifier::Unqualified) ||
           F.hasUnqualifiedOwnership() &&
               "Unqualified inst in qualified function");
    assert((Qualifier == StoreOwnershipQualifier::Unqualified) ||
           F.hasQualifiedOwnership() &&
               "Qualified inst in unqualified function");
    return insert(new (F.getModule()) StoreInst(getSILDebugLocation(Loc), Src,
                                                DestAddr, Qualifier));
  }

  /// Convenience function for calling emitStore on the type lowering for
  /// non-address values.
  void emitStoreValueOperation(SILLocation Loc, SILValue Src, SILValue DestAddr,
                               StoreOwnershipQualifier Qualifier) {
    assert(!Src->getType().isAddress());
    const auto &lowering = getTypeLowering(Src->getType());
    return lowering.emitStore(*this, Loc, Src, DestAddr, Qualifier);
  }

  EndBorrowInst *createEndBorrow(SILLocation Loc, SILValue BorrowedValue,
                                 SILValue OriginalValue) {
    return insert(new (F.getModule()) EndBorrowInst(
        getSILDebugLocation(Loc), BorrowedValue, OriginalValue));
  }

  EndBorrowArgumentInst *createEndBorrowArgument(SILLocation Loc,
                                                 SILValue Arg) {
    return insert(new (F.getModule()) EndBorrowArgumentInst(
        getSILDebugLocation(Loc), cast<SILArgument>(Arg)));
  }

  AssignInst *createAssign(SILLocation Loc, SILValue Src, SILValue DestAddr) {
    return insert(new (F.getModule())
                      AssignInst(getSILDebugLocation(Loc), Src, DestAddr));
  }

  StoreBorrowInst *createStoreBorrow(SILLocation Loc, SILValue Src,
                                     SILValue DestAddr) {
    return insert(new (F.getModule())
                      StoreBorrowInst(getSILDebugLocation(Loc), Src, DestAddr));
  }

  MarkUninitializedInst *
  createMarkUninitialized(SILLocation Loc, SILValue src,
                          MarkUninitializedInst::Kind k) {
    return insert(new (F.getModule()) MarkUninitializedInst(
        getSILDebugLocation(Loc), src, k));
  }
  MarkUninitializedInst *createMarkUninitializedVar(SILLocation Loc,
                                                    SILValue src) {
    return createMarkUninitialized(Loc, src, MarkUninitializedInst::Var);
  }
  MarkUninitializedInst *createMarkUninitializedRootSelf(SILLocation Loc,
                                                         SILValue src) {
    return createMarkUninitialized(Loc, src, MarkUninitializedInst::RootSelf);
  }
  
  MarkUninitializedBehaviorInst *
  createMarkUninitializedBehavior(SILLocation Loc,
                                  SILValue initStorageFunc,
                                  SubstitutionList initStorageSubs,
                                  SILValue storage,
                                  SILValue setterFunc,
                                  SubstitutionList setterSubs,
                                  SILValue self,
                                  SILType ty) {
    return insert(MarkUninitializedBehaviorInst::create(F.getModule(),
         getSILDebugLocation(Loc),
         initStorageFunc, initStorageSubs, storage,
         setterFunc, setterSubs, self, ty));
  }

  MarkFunctionEscapeInst *createMarkFunctionEscape(SILLocation Loc,
                                                   ArrayRef<SILValue> vars) {
    return insert(
        MarkFunctionEscapeInst::create(getSILDebugLocation(Loc), vars, F));
  }

  DebugValueInst *createDebugValue(SILLocation Loc, SILValue src,
                                   SILDebugVariable Var = SILDebugVariable()) {
    return insert(DebugValueInst::create(getSILDebugLocation(Loc), src,
                                         F.getModule(), Var));
  }
  DebugValueAddrInst *
  createDebugValueAddr(SILLocation Loc, SILValue src,
                       SILDebugVariable Var = SILDebugVariable()) {
    return insert(DebugValueAddrInst::create(getSILDebugLocation(Loc), src,
                                             F.getModule(), Var));
  }

  LoadWeakInst *createLoadWeak(SILLocation Loc, SILValue src, IsTake_t isTake) {
    return insert(new (F.getModule())
                      LoadWeakInst(getSILDebugLocation(Loc), src, isTake));
  }

  StoreWeakInst *createStoreWeak(SILLocation Loc, SILValue value, SILValue dest,
                                 IsInitialization_t isInit) {
    return insert(new (F.getModule()) StoreWeakInst(getSILDebugLocation(Loc),
                                                    value, dest, isInit));
  }

  LoadUnownedInst *createLoadUnowned(SILLocation loc, SILValue src,
                                     IsTake_t isTake) {
    return insert(new (F.getModule())
                    LoadUnownedInst(getSILDebugLocation(loc), src, isTake));
  }

  StoreUnownedInst *createStoreUnowned(SILLocation loc, SILValue value,
                                       SILValue dest,
                                       IsInitialization_t isInit) {
    return insert(new (F.getModule())
                    StoreUnownedInst(getSILDebugLocation(loc),
                                     value, dest, isInit));
  }

  CopyAddrInst *createCopyAddr(SILLocation Loc, SILValue srcAddr,
                               SILValue destAddr, IsTake_t isTake,
                               IsInitialization_t isInitialize) {
    assert(srcAddr->getType() == destAddr->getType());
    return insert(new (F.getModule()) CopyAddrInst(
        getSILDebugLocation(Loc), srcAddr, destAddr, isTake, isInitialize));
  }

  BindMemoryInst *createBindMemory(SILLocation Loc, SILValue base,
                                   SILValue index, SILType boundType) {
    return insert(BindMemoryInst::create(getSILDebugLocation(Loc), base, index,
                                         boundType, F, OpenedArchetypes));
  }

  ConvertFunctionInst *createConvertFunction(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new (F.getModule())
                      ConvertFunctionInst(getSILDebugLocation(Loc), Op, Ty));
  }

  ThinFunctionToPointerInst *
  createThinFunctionToPointer(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ThinFunctionToPointerInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  PointerToThinFunctionInst *
  createPointerToThinFunction(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) PointerToThinFunctionInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  UpcastInst *createUpcast(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule())
                      UpcastInst(getSILDebugLocation(Loc), Op, Ty));
  }

  AddressToPointerInst *createAddressToPointer(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(new (F.getModule()) AddressToPointerInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  PointerToAddressInst *createPointerToAddress(SILLocation Loc, SILValue Op,
                                               SILType Ty,
                                               bool isStrict,
                                               bool isInvariant = false){
    return insert(new (F.getModule()) PointerToAddressInst(
                    getSILDebugLocation(Loc), Op, Ty, isStrict, isInvariant));
  }

  UncheckedRefCastInst *createUncheckedRefCast(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(UncheckedRefCastInst::create(getSILDebugLocation(Loc), Op, Ty,
                                               F, OpenedArchetypes));
  }

  UncheckedRefCastAddrInst *
  createUncheckedRefCastAddr(SILLocation Loc, SILValue src, CanType sourceType,
                             SILValue dest, CanType targetType) {
    return insert(new (F.getModule()) UncheckedRefCastAddrInst(
        getSILDebugLocation(Loc), src, sourceType, dest, targetType));
  }

  UncheckedAddrCastInst *createUncheckedAddrCast(SILLocation Loc, SILValue Op,
                                                 SILType Ty) {
    return insert(UncheckedAddrCastInst::create(getSILDebugLocation(Loc), Op,
                                                Ty, F, OpenedArchetypes));
  }

  UncheckedTrivialBitCastInst *
  createUncheckedTrivialBitCast(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(UncheckedTrivialBitCastInst::create(
        getSILDebugLocation(Loc), Op, Ty, F, OpenedArchetypes));
  }

  UncheckedBitwiseCastInst *
  createUncheckedBitwiseCast(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(UncheckedBitwiseCastInst::create(getSILDebugLocation(Loc), Op,
                                                   Ty, F, OpenedArchetypes));
  }

  RefToBridgeObjectInst *createRefToBridgeObject(SILLocation Loc, SILValue Ref,
                                                 SILValue Bits) {
    auto Ty = SILType::getBridgeObjectType(getASTContext());
    return insert(new (F.getModule()) RefToBridgeObjectInst(
        getSILDebugLocation(Loc), Ref, Bits, Ty));
  }

  BridgeObjectToRefInst *createBridgeObjectToRef(SILLocation Loc, SILValue Op,
                                                 SILType Ty) {
    return insert(new (F.getModule()) BridgeObjectToRefInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  BridgeObjectToWordInst *createBridgeObjectToWord(SILLocation Loc,
                                                   SILValue Op) {
    auto Ty = SILType::getBuiltinWordType(getASTContext());
    return createBridgeObjectToWord(Loc, Op, Ty);
  }

  BridgeObjectToWordInst *createBridgeObjectToWord(SILLocation Loc, SILValue Op,
                                                   SILType Ty) {
    return insert(new (F.getModule()) BridgeObjectToWordInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  RefToRawPointerInst *createRefToRawPointer(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new (F.getModule())
                      RefToRawPointerInst(getSILDebugLocation(Loc), Op, Ty));
  }

  RawPointerToRefInst *createRawPointerToRef(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new (F.getModule())
                      RawPointerToRefInst(getSILDebugLocation(Loc), Op, Ty));
  }

  ThinToThickFunctionInst *createThinToThickFunction(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ThinToThickFunctionInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  ThickToObjCMetatypeInst *createThickToObjCMetatype(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ThickToObjCMetatypeInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  ObjCToThickMetatypeInst *createObjCToThickMetatype(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ObjCToThickMetatypeInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  ObjCProtocolInst *createObjCProtocol(SILLocation Loc, ProtocolDecl *P,
                                       SILType Ty) {
    return insert(new (F.getModule())
                      ObjCProtocolInst(getSILDebugLocation(Loc), P, Ty));
  }

  UnownedToRefInst *createUnownedToRef(SILLocation Loc, SILValue op,
                                       SILType ty) {
    return insert(new (F.getModule())
                      UnownedToRefInst(getSILDebugLocation(Loc), op, ty));
  }

  RefToUnownedInst *createRefToUnowned(SILLocation Loc, SILValue op,
                                       SILType ty) {
    return insert(new (F.getModule())
                      RefToUnownedInst(getSILDebugLocation(Loc), op, ty));
  }

  UnmanagedToRefInst *createUnmanagedToRef(SILLocation Loc, SILValue op,
                                           SILType ty) {
    return insert(new (F.getModule())
                      UnmanagedToRefInst(getSILDebugLocation(Loc), op, ty));
  }

  RefToUnmanagedInst *createRefToUnmanaged(SILLocation Loc, SILValue op,
                                           SILType ty) {
    return insert(new (F.getModule())
                      RefToUnmanagedInst(getSILDebugLocation(Loc), op, ty));
  }

  IsNonnullInst *createIsNonnull(SILLocation Loc, SILValue operand) {
    return insert(new (F.getModule()) IsNonnullInst(
        getSILDebugLocation(Loc), operand,
        SILType::getBuiltinIntegerType(1, getASTContext())));
  }

  UnconditionalCheckedCastInst *
  createUnconditionalCheckedCast(SILLocation Loc, SILValue op, SILType destTy) {
    return insert(UnconditionalCheckedCastInst::create(
        getSILDebugLocation(Loc), op, destTy, F, OpenedArchetypes));
  }

  UnconditionalCheckedCastAddrInst *createUnconditionalCheckedCastAddr(
      SILLocation Loc, CastConsumptionKind consumption, SILValue src,
      CanType sourceType, SILValue dest, CanType targetType) {
    return insert(new (F.getModule()) UnconditionalCheckedCastAddrInst(
        getSILDebugLocation(Loc), consumption, src, sourceType, dest,
        targetType));
  }

  UnconditionalCheckedCastValueInst *
  createUnconditionalCheckedCastValue(SILLocation Loc, SILValue op,
                                      SILType destTy) {
    return insert(UnconditionalCheckedCastValueInst::create(
        getSILDebugLocation(Loc), op, destTy, F, OpenedArchetypes));
  }

  RetainValueInst *createRetainValue(SILLocation Loc, SILValue operand,
                                     Atomicity atomicity) {
    assert(isParsing || F.hasUnqualifiedOwnership());
    return insert(new (F.getModule()) RetainValueInst(getSILDebugLocation(Loc),
                                                      operand, atomicity));
  }

  ReleaseValueInst *createReleaseValue(SILLocation Loc, SILValue operand,
                                       Atomicity atomicity) {
    assert(isParsing || F.hasUnqualifiedOwnership());
    return insert(new (F.getModule()) ReleaseValueInst(getSILDebugLocation(Loc),
                                                       operand, atomicity));
  }

  UnmanagedRetainValueInst *createUnmanagedRetainValue(SILLocation Loc,
                                                       SILValue operand,
                                                       Atomicity atomicity) {
    assert(F.hasQualifiedOwnership());
    return insert(new (F.getModule()) UnmanagedRetainValueInst(
        getSILDebugLocation(Loc), operand, atomicity));
  }

  UnmanagedReleaseValueInst *createUnmanagedReleaseValue(SILLocation Loc,
                                                         SILValue operand,
                                                         Atomicity atomicity) {
    assert(F.hasQualifiedOwnership());
    return insert(new (F.getModule()) UnmanagedReleaseValueInst(
        getSILDebugLocation(Loc), operand, atomicity));
  }

  CopyValueInst *createCopyValue(SILLocation Loc, SILValue operand) {
    return insert(new (F.getModule())
                      CopyValueInst(getSILDebugLocation(Loc), operand));
  }

  CopyUnownedValueInst *createCopyUnownedValue(SILLocation Loc,
                                               SILValue operand) {
    return insert(new (F.getModule()) CopyUnownedValueInst(
        getSILDebugLocation(Loc), operand, getModule()));
  }

  DestroyValueInst *createDestroyValue(SILLocation Loc, SILValue operand) {
    return insert(new (F.getModule())
                      DestroyValueInst(getSILDebugLocation(Loc), operand));
  }

  AutoreleaseValueInst *createAutoreleaseValue(SILLocation Loc,
                                               SILValue operand,
                                               Atomicity atomicity) {
    return insert(new (F.getModule()) AutoreleaseValueInst(
        getSILDebugLocation(Loc), operand, atomicity));
  }

  UnmanagedAutoreleaseValueInst *
  createUnmanagedAutoreleaseValue(SILLocation Loc, SILValue operand,
                                  Atomicity atomicity) {
    return insert(new (F.getModule()) UnmanagedAutoreleaseValueInst(
                      getSILDebugLocation(Loc), operand, atomicity));
  }

  SetDeallocatingInst *createSetDeallocating(SILLocation Loc,
                                            SILValue operand,
                                            Atomicity atomicity) {
    return insert(new (F.getModule()) SetDeallocatingInst(
        getSILDebugLocation(Loc), operand, atomicity));
  }

  StructInst *createStruct(SILLocation Loc, SILType Ty,
                           ArrayRef<SILValue> Elements) {
    return insert(
        StructInst::create(getSILDebugLocation(Loc), Ty, Elements, F));
  }

  TupleInst *createTuple(SILLocation Loc, SILType Ty,
                         ArrayRef<SILValue> Elements) {
    return insert(
        TupleInst::create(getSILDebugLocation(Loc), Ty, Elements, F));
  }

  TupleInst *createTuple(SILLocation loc, ArrayRef<SILValue> elts);

  EnumInst *createEnum(SILLocation Loc, SILValue Operand,
                       EnumElementDecl *Element, SILType Ty) {
    return insert(new (F.getModule()) EnumInst(getSILDebugLocation(Loc),
                                               Operand, Element, Ty));
  }

  /// Inject a loadable value into the corresponding optional type.
  EnumInst *createOptionalSome(SILLocation Loc, SILValue operand, SILType ty) {
    auto someDecl = F.getModule().getASTContext().getOptionalSomeDecl();
    return createEnum(Loc, operand, someDecl, ty);
  }

  /// Create the nil value of a loadable optional type.
  EnumInst *createOptionalNone(SILLocation Loc, SILType ty) {
    auto noneDecl = F.getModule().getASTContext().getOptionalNoneDecl();
    return createEnum(Loc, nullptr, noneDecl, ty);
  }

  InitEnumDataAddrInst *createInitEnumDataAddr(SILLocation Loc,
                                               SILValue Operand,
                                               EnumElementDecl *Element,
                                               SILType Ty) {
    return insert(new (F.getModule()) InitEnumDataAddrInst(
        getSILDebugLocation(Loc), Operand, Element, Ty));
  }

  UncheckedEnumDataInst *createUncheckedEnumData(SILLocation Loc,
                                                 SILValue Operand,
                                                 EnumElementDecl *Element,
                                                 SILType Ty) {
    return insert(new (F.getModule()) UncheckedEnumDataInst(
        getSILDebugLocation(Loc), Operand, Element, Ty));
  }

  UncheckedEnumDataInst *createUncheckedEnumData(SILLocation Loc,
                                                 SILValue Operand,
                                                 EnumElementDecl *Element) {
    SILType EltType =
        Operand->getType().getEnumElementType(Element, getModule());
    return createUncheckedEnumData(Loc, Operand, Element, EltType);
  }

  UncheckedTakeEnumDataAddrInst *
  createUncheckedTakeEnumDataAddr(SILLocation Loc, SILValue Operand,
                                  EnumElementDecl *Element, SILType Ty) {
    return insert(new (F.getModule()) UncheckedTakeEnumDataAddrInst(
        getSILDebugLocation(Loc), Operand, Element, Ty));
  }

  UncheckedTakeEnumDataAddrInst *
  createUncheckedTakeEnumDataAddr(SILLocation Loc, SILValue Operand,
                                  EnumElementDecl *Element) {
    SILType EltType =
        Operand->getType().getEnumElementType(Element, getModule());
    return createUncheckedTakeEnumDataAddr(Loc, Operand, Element, EltType);
  }

  InjectEnumAddrInst *createInjectEnumAddr(SILLocation Loc, SILValue Operand,
                                           EnumElementDecl *Element) {
    return insert(new (F.getModule()) InjectEnumAddrInst(
        getSILDebugLocation(Loc), Operand, Element));
  }

  SelectEnumInst *createSelectEnum(
      SILLocation Loc, SILValue Operand, SILType Ty, SILValue DefaultValue,
      ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues) {
    return insert(SelectEnumInst::create(getSILDebugLocation(Loc), Operand,
                                         Ty, DefaultValue, CaseValues, F));
  }

  SelectEnumAddrInst *createSelectEnumAddr(
      SILLocation Loc, SILValue Operand, SILType Ty, SILValue DefaultValue,
      ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues) {
    return insert(SelectEnumAddrInst::create(
        getSILDebugLocation(Loc), Operand, Ty, DefaultValue, CaseValues, F));
  }

  SelectValueInst *createSelectValue(
      SILLocation Loc, SILValue Operand, SILType Ty, SILValue DefaultResult,
      ArrayRef<std::pair<SILValue, SILValue>> CaseValuesAndResults) {
    return insert(SelectValueInst::create(getSILDebugLocation(Loc), Operand,
                                          Ty, DefaultResult,
                                          CaseValuesAndResults, F));
  }

  TupleExtractInst *createTupleExtract(SILLocation Loc, SILValue Operand,
                                       unsigned FieldNo, SILType ResultTy) {
    return insert(new (F.getModule()) TupleExtractInst(
        getSILDebugLocation(Loc), Operand, FieldNo, ResultTy));
  }

  TupleExtractInst *createTupleExtract(SILLocation Loc, SILValue Operand,
                                       unsigned FieldNo) {
    auto type = Operand->getType().getTupleElementType(FieldNo);
    return createTupleExtract(Loc, Operand, FieldNo, type);
  }

  TupleElementAddrInst *createTupleElementAddr(SILLocation Loc,
                                               SILValue Operand,
                                               unsigned FieldNo,
                                               SILType ResultTy) {
    return insert(new (F.getModule()) TupleElementAddrInst(
        getSILDebugLocation(Loc), Operand, FieldNo, ResultTy));
  }

  TupleElementAddrInst *
  createTupleElementAddr(SILLocation Loc, SILValue Operand, unsigned FieldNo) {
    return insert(new (F.getModule()) TupleElementAddrInst(
        getSILDebugLocation(Loc), Operand, FieldNo,
        Operand->getType().getTupleElementType(FieldNo)));
  }

  StructExtractInst *createStructExtract(SILLocation Loc, SILValue Operand,
                                         VarDecl *Field, SILType ResultTy) {
    return insert(new (F.getModule()) StructExtractInst(
        getSILDebugLocation(Loc), Operand, Field, ResultTy));
  }

  StructExtractInst *createStructExtract(SILLocation Loc, SILValue Operand,
                                         VarDecl *Field) {
    auto type = Operand->getType().getFieldType(Field, F.getModule());
    return createStructExtract(Loc, Operand, Field, type);
  }

  StructElementAddrInst *createStructElementAddr(SILLocation Loc,
                                                 SILValue Operand,
                                                 VarDecl *Field,
                                                 SILType ResultTy) {
    return insert(new (F.getModule()) StructElementAddrInst(
        getSILDebugLocation(Loc), Operand, Field, ResultTy));
  }

  StructElementAddrInst *
  createStructElementAddr(SILLocation Loc, SILValue Operand, VarDecl *Field) {
    auto ResultTy = Operand->getType().getFieldType(Field, F.getModule());
    return createStructElementAddr(Loc, Operand, Field, ResultTy);
  }

  RefElementAddrInst *createRefElementAddr(SILLocation Loc, SILValue Operand,
                                           VarDecl *Field, SILType ResultTy) {
    return insert(new (F.getModule()) RefElementAddrInst(
        getSILDebugLocation(Loc), Operand, Field, ResultTy));
  }
  RefElementAddrInst *createRefElementAddr(SILLocation Loc, SILValue Operand,
                                           VarDecl *Field) {
    auto ResultTy = Operand->getType().getFieldType(Field, F.getModule());
    return createRefElementAddr(Loc, Operand, Field, ResultTy);
  }

  RefTailAddrInst *createRefTailAddr(SILLocation Loc, SILValue Ref,
                                     SILType ResultTy) {
    return insert(new (F.getModule()) RefTailAddrInst(getSILDebugLocation(Loc),
                                                      Ref, ResultTy));
  }

  ClassMethodInst *createClassMethod(SILLocation Loc, SILValue Operand,
                                     SILDeclRef Member, SILType MethodTy,
                                     bool Volatile = false) {
    return insert(new (F.getModule()) ClassMethodInst(
        getSILDebugLocation(Loc), Operand, Member, MethodTy, Volatile));
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
        getSILDebugLocation(Loc), Operand, Member, MethodTy, Volatile));
  }

  WitnessMethodInst *createWitnessMethod(SILLocation Loc, CanType LookupTy,
                                         ProtocolConformanceRef Conformance,
                                         SILDeclRef Member, SILType MethodTy,
                                         bool Volatile = false) {
    return insert(WitnessMethodInst::create(
        getSILDebugLocation(Loc), LookupTy, Conformance, Member, MethodTy,
        &F, OpenedArchetypes, Volatile));
  }

  DynamicMethodInst *createDynamicMethod(SILLocation Loc, SILValue Operand,
                                         SILDeclRef Member, SILType MethodTy,
                                         bool Volatile = false) {
    return insert(DynamicMethodInst::create(
        getSILDebugLocation(Loc), Operand, Member, MethodTy, Volatile,
        &F, OpenedArchetypes));
  }

  OpenExistentialAddrInst *
  createOpenExistentialAddr(SILLocation Loc, SILValue Operand, SILType SelfTy,
                            OpenedExistentialAccess ForAccess) {
    auto *I = insert(new (F.getModule()) OpenExistentialAddrInst(
        getSILDebugLocation(Loc), Operand, SelfTy, ForAccess));
    if (OpenedArchetypesTracker)
      OpenedArchetypesTracker->registerOpenedArchetypes(I);
    return I;
  }

  OpenExistentialOpaqueInst *createOpenExistentialOpaque(SILLocation Loc,
                                                         SILValue Operand,
                                                         SILType SelfTy) {
    auto *I = insert(new (F.getModule()) OpenExistentialOpaqueInst(
        getSILDebugLocation(Loc), Operand, SelfTy));
    if (OpenedArchetypesTracker)
      OpenedArchetypesTracker->registerOpenedArchetypes(I);
    return I;
  }

  OpenExistentialMetatypeInst *createOpenExistentialMetatype(SILLocation Loc,
                                                             SILValue operand,
                                                             SILType selfTy) {
    auto *I = insert(new (F.getModule()) OpenExistentialMetatypeInst(
        getSILDebugLocation(Loc), operand, selfTy));
    if (OpenedArchetypesTracker)
      OpenedArchetypesTracker->registerOpenedArchetypes(I);
    return I;
  }

  OpenExistentialRefInst *
  createOpenExistentialRef(SILLocation Loc, SILValue Operand, SILType Ty) {
    auto *I = insert(new (F.getModule()) OpenExistentialRefInst(
        getSILDebugLocation(Loc), Operand, Ty));
    if (OpenedArchetypesTracker)
      OpenedArchetypesTracker->registerOpenedArchetypes(I);
    return I;
  }

  OpenExistentialBoxInst *
  createOpenExistentialBox(SILLocation Loc, SILValue Operand, SILType Ty) {
    auto *I = insert(new (F.getModule()) OpenExistentialBoxInst(
        getSILDebugLocation(Loc), Operand, Ty));
    if (OpenedArchetypesTracker)
      OpenedArchetypesTracker->registerOpenedArchetypes(I);
    return I;
  }

  InitExistentialAddrInst *
  createInitExistentialAddr(SILLocation Loc, SILValue Existential,
                            CanType FormalConcreteType,
                            SILType LoweredConcreteType,
                            ArrayRef<ProtocolConformanceRef> Conformances) {
    return insert(InitExistentialAddrInst::create(
        getSILDebugLocation(Loc), Existential, FormalConcreteType,
        LoweredConcreteType, Conformances, &F, OpenedArchetypes));
  }

  InitExistentialOpaqueInst *
  createInitExistentialOpaque(SILLocation Loc, SILType ExistentialType,
                              CanType FormalConcreteType, SILValue Concrete,
                              ArrayRef<ProtocolConformanceRef> Conformances) {
    return insert(InitExistentialOpaqueInst::create(
        getSILDebugLocation(Loc), ExistentialType, FormalConcreteType, Concrete,
        Conformances, &F, OpenedArchetypes));
  }

  InitExistentialMetatypeInst *
  createInitExistentialMetatype(SILLocation Loc, SILValue metatype,
                                SILType existentialType,
                                ArrayRef<ProtocolConformanceRef> conformances) {
    return insert(InitExistentialMetatypeInst::create(
        getSILDebugLocation(Loc), existentialType, metatype, conformances, &F,
        OpenedArchetypes));
  }

  InitExistentialRefInst *
  createInitExistentialRef(SILLocation Loc, SILType ExistentialType,
                           CanType FormalConcreteType, SILValue Concrete,
                           ArrayRef<ProtocolConformanceRef> Conformances) {
    return insert(InitExistentialRefInst::create(
        getSILDebugLocation(Loc), ExistentialType, FormalConcreteType, Concrete,
        Conformances, &F, OpenedArchetypes));
  }

  DeinitExistentialAddrInst *createDeinitExistentialAddr(SILLocation Loc,
                                                         SILValue Existential) {
    return insert(new (F.getModule()) DeinitExistentialAddrInst(
        getSILDebugLocation(Loc), Existential));
  }

  DeinitExistentialOpaqueInst *
  createDeinitExistentialOpaque(SILLocation Loc, SILValue Existential) {
    return insert(new (F.getModule()) DeinitExistentialOpaqueInst(
        getSILDebugLocation(Loc), Existential));
  }

  ProjectBlockStorageInst *createProjectBlockStorage(SILLocation Loc,
                                                     SILValue Storage) {
    auto CaptureTy = Storage->getType()
                         .castTo<SILBlockStorageType>()
                         ->getCaptureAddressType();
    return createProjectBlockStorage(Loc, Storage, CaptureTy);
  }
  ProjectBlockStorageInst *createProjectBlockStorage(SILLocation Loc,
                                                     SILValue Storage,
                                                     SILType CaptureTy) {
    return insert(new (F.getModule()) ProjectBlockStorageInst(
        getSILDebugLocation(Loc), Storage, CaptureTy));
  }

  InitBlockStorageHeaderInst *
  createInitBlockStorageHeader(SILLocation Loc, SILValue BlockStorage,
                               SILValue InvokeFunction, SILType BlockType,
                               SubstitutionList Subs) {
    return insert(InitBlockStorageHeaderInst::create(F,
      getSILDebugLocation(Loc), BlockStorage, InvokeFunction, BlockType, Subs));
  }

  MetatypeInst *createMetatype(SILLocation Loc, SILType Metatype) {
    return insert(MetatypeInst::create(getSILDebugLocation(Loc), Metatype,
                                       &F, OpenedArchetypes));
  }

  ObjCMetatypeToObjectInst *
  createObjCMetatypeToObject(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (F.getModule()) ObjCMetatypeToObjectInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  ObjCExistentialMetatypeToObjectInst *
  createObjCExistentialMetatypeToObject(SILLocation Loc, SILValue Op,
                                        SILType Ty) {
    return insert(new (F.getModule()) ObjCExistentialMetatypeToObjectInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  ValueMetatypeInst *createValueMetatype(SILLocation Loc, SILType Metatype,
                                         SILValue Base) {
    return insert(new (F.getModule()) ValueMetatypeInst(
        getSILDebugLocation(Loc), Metatype, Base));
  }

  ExistentialMetatypeInst *
  createExistentialMetatype(SILLocation Loc, SILType Metatype, SILValue Base) {
    return insert(new (F.getModule()) ExistentialMetatypeInst(
        getSILDebugLocation(Loc), Metatype, Base));
  }

  CopyBlockInst *createCopyBlock(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      CopyBlockInst(getSILDebugLocation(Loc), Operand));
  }
  StrongRetainInst *createStrongRetain(SILLocation Loc, SILValue Operand,
                                       Atomicity atomicity) {
    assert(isParsing || F.hasUnqualifiedOwnership());
    return insert(new (F.getModule()) StrongRetainInst(getSILDebugLocation(Loc),
                                                       Operand, atomicity));
  }
  StrongReleaseInst *createStrongRelease(SILLocation Loc, SILValue Operand,
                                         Atomicity atomicity) {
    assert(isParsing || F.hasUnqualifiedOwnership());
    return insert(new (F.getModule()) StrongReleaseInst(
        getSILDebugLocation(Loc), Operand, atomicity));
  }
  StrongPinInst *createStrongPin(SILLocation Loc, SILValue Operand,
                                 Atomicity atomicity) {
    return insert(new (F.getModule()) StrongPinInst(getSILDebugLocation(Loc),
                                                    Operand, atomicity));
  }
  StrongUnpinInst *createStrongUnpin(SILLocation Loc, SILValue Operand,
                                     Atomicity atomicity) {
    return insert(new (F.getModule()) StrongUnpinInst(getSILDebugLocation(Loc),
                                                      Operand, atomicity));
  }
  StrongRetainUnownedInst *createStrongRetainUnowned(SILLocation Loc,
                                                     SILValue Operand,
                                                     Atomicity atomicity) {
    return insert(new (F.getModule()) StrongRetainUnownedInst(
        getSILDebugLocation(Loc), Operand, atomicity));
  }
  UnownedRetainInst *createUnownedRetain(SILLocation Loc, SILValue Operand,
                                         Atomicity atomicity) {
    return insert(new (F.getModule()) UnownedRetainInst(
        getSILDebugLocation(Loc), Operand, atomicity));
  }
  UnownedReleaseInst *createUnownedRelease(SILLocation Loc, SILValue Operand,
                                           Atomicity atomicity) {
    return insert(new (F.getModule()) UnownedReleaseInst(
        getSILDebugLocation(Loc), Operand, atomicity));
  }

  EndLifetimeInst *createEndLifetime(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      EndLifetimeInst(getSILDebugLocation(Loc), Operand));
  }

  UncheckedOwnershipConversionInst *
  createUncheckedOwnershipConversion(SILLocation Loc, SILValue Operand,
                                     ValueOwnershipKind Kind) {
    return insert(new (F.getModule()) UncheckedOwnershipConversionInst(
        getSILDebugLocation(Loc), Operand, Kind));
  }

  FixLifetimeInst *createFixLifetime(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      FixLifetimeInst(getSILDebugLocation(Loc), Operand));
  }
  void emitFixLifetime(SILLocation Loc, SILValue Operand) {
    if (getTypeLowering(Operand->getType()).isTrivial())
      return;
    createFixLifetime(Loc, Operand);
  }
  MarkDependenceInst *createMarkDependence(SILLocation Loc, SILValue value,
                                           SILValue base) {
    return insert(new (F.getModule()) MarkDependenceInst(
        getSILDebugLocation(Loc), value, base));
  }
  IsUniqueInst *createIsUnique(SILLocation Loc, SILValue operand) {
    auto Int1Ty = SILType::getBuiltinIntegerType(1, getASTContext());
    return insert(new (F.getModule()) IsUniqueInst(getSILDebugLocation(Loc),
                                                   operand, Int1Ty));
  }
  IsUniqueOrPinnedInst *createIsUniqueOrPinned(SILLocation Loc,
                                               SILValue value) {
    auto Int1Ty = SILType::getBuiltinIntegerType(1, getASTContext());
    return insert(new (F.getModule()) IsUniqueOrPinnedInst(
        getSILDebugLocation(Loc), value, Int1Ty));
  }

  DeallocStackInst *createDeallocStack(SILLocation Loc, SILValue operand) {
    return insert(new (F.getModule())
                      DeallocStackInst(getSILDebugLocation(Loc), operand));
  }
  DeallocRefInst *createDeallocRef(SILLocation Loc, SILValue operand,
                                   bool canBeOnStack) {
    return insert(new (F.getModule()) DeallocRefInst(
        getSILDebugLocation(Loc), operand, canBeOnStack));
  }
  DeallocPartialRefInst *createDeallocPartialRef(SILLocation Loc,
                                                 SILValue operand,
                                                 SILValue metatype) {
    return insert(new (F.getModule()) DeallocPartialRefInst(
        getSILDebugLocation(Loc), operand, metatype));
  }
  DeallocBoxInst *createDeallocBox(SILLocation Loc,
                                   SILValue operand) {
    return insert(new (F.getModule()) DeallocBoxInst(
        getSILDebugLocation(Loc), operand));
  }
  DeallocExistentialBoxInst *createDeallocExistentialBox(SILLocation Loc,
                                                         CanType concreteType,
                                                         SILValue operand) {
    return insert(new (F.getModule()) DeallocExistentialBoxInst(
        getSILDebugLocation(Loc), concreteType, operand));
  }
  DeallocValueBufferInst *createDeallocValueBuffer(SILLocation Loc,
                                                   SILType valueType,
                                                   SILValue operand) {
    return insert(new (F.getModule()) DeallocValueBufferInst(
        getSILDebugLocation(Loc), valueType, operand));
  }
  DestroyAddrInst *createDestroyAddr(SILLocation Loc, SILValue Operand) {
    return insert(new (F.getModule())
                      DestroyAddrInst(getSILDebugLocation(Loc), Operand));
  }

  ProjectValueBufferInst *createProjectValueBuffer(SILLocation Loc,
                                                   SILType valueType,
                                                   SILValue operand) {
    return insert(new (F.getModule()) ProjectValueBufferInst(
        getSILDebugLocation(Loc), valueType, operand));
  }
  ProjectBoxInst *createProjectBox(SILLocation Loc, SILValue boxOperand,
                                   unsigned index) {
    auto boxTy = boxOperand->getType().castTo<SILBoxType>();
    auto fieldTy = boxTy->getFieldType(getModule(), index);

    return insert(new (F.getModule()) ProjectBoxInst(
        getSILDebugLocation(Loc), boxOperand, index, fieldTy));
  }
  ProjectExistentialBoxInst *createProjectExistentialBox(SILLocation Loc,
                                                         SILType valueTy,
                                                         SILValue boxOperand) {
    return insert(new (F.getModule()) ProjectExistentialBoxInst(
        getSILDebugLocation(Loc), valueTy, boxOperand));
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
                      CondFailInst(getSILDebugLocation(Loc), Operand));
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
    return insert(new (F.getModule()) IndexAddrInst(getSILDebugLocation(Loc),
                                                    Operand, Index));
  }

  TailAddrInst *createTailAddr(SILLocation Loc, SILValue Operand,
                               SILValue Count, SILType ResultTy) {
    return insert(new (F.getModule()) TailAddrInst(getSILDebugLocation(Loc),
                                                   Operand, Count, ResultTy));
  }

  IndexRawPointerInst *createIndexRawPointer(SILLocation Loc, SILValue Operand,
                                             SILValue Index) {
    return insert(new (F.getModule()) IndexRawPointerInst(
        getSILDebugLocation(Loc), Operand, Index));
  }

  //===--------------------------------------------------------------------===//
  // Terminator SILInstruction Creation Methods
  //===--------------------------------------------------------------------===//

  UnreachableInst *createUnreachable(SILLocation Loc) {
    return insertTerminator(new (F.getModule())
                                UnreachableInst(getSILDebugLocation(Loc)));
  }

  ReturnInst *createReturn(SILLocation Loc, SILValue ReturnValue) {
    return insertTerminator(new (F.getModule()) ReturnInst(
        getSILDebugLocation(Loc), ReturnValue));
  }

  ThrowInst *createThrow(SILLocation Loc, SILValue errorValue) {
    return insertTerminator(
        new (F.getModule()) ThrowInst(getSILDebugLocation(Loc), errorValue));
  }

  CondBranchInst *createCondBranch(SILLocation Loc, SILValue Cond,
                                   SILBasicBlock *Target1,
                                   SILBasicBlock *Target2) {
    return insertTerminator(CondBranchInst::create(getSILDebugLocation(Loc),
                                                   Cond, Target1, Target2, F));
  }

  CondBranchInst *createCondBranch(SILLocation Loc, SILValue Cond,
                                   SILBasicBlock *Target1,
                                   ArrayRef<SILValue> Args1,
                                   SILBasicBlock *Target2,
                                   ArrayRef<SILValue> Args2) {
    return insertTerminator(CondBranchInst::create(
        getSILDebugLocation(Loc), Cond, Target1, Args1, Target2, Args2, F));
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

    return insertTerminator(CondBranchInst::create(getSILDebugLocation(Loc),
                                                   Cond, Target1, ArgsCopy1,
                                                   Target2, ArgsCopy2, F));
  }

  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock) {
    return insertTerminator(
        BranchInst::create(getSILDebugLocation(Loc), TargetBlock, F));
  }

  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock,
                           ArrayRef<SILValue> Args) {
    return insertTerminator(
        BranchInst::create(getSILDebugLocation(Loc), TargetBlock, Args, F));
  }

  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock,
                           OperandValueArrayRef Args);

  SwitchValueInst *
  createSwitchValue(SILLocation Loc, SILValue Operand, SILBasicBlock *DefaultBB,
                    ArrayRef<std::pair<SILValue, SILBasicBlock *>> CaseBBs) {
    return insertTerminator(SwitchValueInst::create(
        getSILDebugLocation(Loc), Operand, DefaultBB, CaseBBs, F));
  }

  SwitchEnumInst *createSwitchEnum(
      SILLocation Loc, SILValue Operand, SILBasicBlock *DefaultBB,
      ArrayRef<std::pair<EnumElementDecl *, SILBasicBlock *>> CaseBBs) {
    return insertTerminator(SwitchEnumInst::create(
        getSILDebugLocation(Loc), Operand, DefaultBB, CaseBBs, F));
  }

  SwitchEnumAddrInst *createSwitchEnumAddr(
      SILLocation Loc, SILValue Operand, SILBasicBlock *DefaultBB,
      ArrayRef<std::pair<EnumElementDecl *, SILBasicBlock *>> CaseBBs) {
    return insertTerminator(SwitchEnumAddrInst::create(
        getSILDebugLocation(Loc), Operand, DefaultBB, CaseBBs, F));
  }

  DynamicMethodBranchInst *
  createDynamicMethodBranch(SILLocation Loc, SILValue Operand,
                            SILDeclRef Member, SILBasicBlock *HasMethodBB,
                            SILBasicBlock *NoMethodBB) {
    return insertTerminator(
        DynamicMethodBranchInst::create(getSILDebugLocation(Loc), Operand,
                                        Member, HasMethodBB, NoMethodBB, F));
  }

  CheckedCastBranchInst *createCheckedCastBranch(SILLocation Loc, bool isExact,
                                                 SILValue op, SILType destTy,
                                                 SILBasicBlock *successBB,
                                                 SILBasicBlock *failureBB) {
    return insertTerminator(CheckedCastBranchInst::create(
        getSILDebugLocation(Loc), isExact, op, destTy, successBB, failureBB, F,
        OpenedArchetypes));
  }

  CheckedCastValueBranchInst *
  createCheckedCastValueBranch(SILLocation Loc, SILValue op, SILType destTy,
                               SILBasicBlock *successBB,
                               SILBasicBlock *failureBB) {
    return insertTerminator(CheckedCastValueBranchInst::create(
        getSILDebugLocation(Loc), op, destTy, successBB, failureBB, F,
        OpenedArchetypes));
  }

  CheckedCastAddrBranchInst *
  createCheckedCastAddrBranch(SILLocation Loc, CastConsumptionKind consumption,
                              SILValue src, CanType sourceType, SILValue dest,
                              CanType targetType, SILBasicBlock *successBB,
                              SILBasicBlock *failureBB) {
    return insertTerminator(new (F.getModule()) CheckedCastAddrBranchInst(
        getSILDebugLocation(Loc), consumption, src, sourceType, dest,
        targetType, successBB, failureBB));
  }

  //===--------------------------------------------------------------------===//
  // Memory management helpers
  //===--------------------------------------------------------------------===//

  /// Returns the default atomicity of the module.
  Atomicity getDefaultAtomicity() {
    return getModule().isDefaultAtomic() ? Atomicity::Atomic : Atomicity::NonAtomic;
  }

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
  /// fold it locally into another nearby retain_value instruction.  This
  /// returns the new instruction if it inserts one, otherwise it returns null.
  ///
  /// This instruction doesn't handle strength reduction of release_value into
  /// a noop / strong_release / unowned_release.  For that, use the
  /// emitReleaseValueOperation method below or use the TypeLowering API.
  DestroyValueInst *emitDestroyValueAndFold(SILLocation Loc, SILValue Operand) {
    auto U = emitDestroyValue(Loc, Operand);
    if (U.isNull())
      return nullptr;
    if (auto *DVI = U.dyn_cast<DestroyValueInst *>())
      return DVI;
    auto *CVI = U.get<CopyValueInst *>();
    CVI->replaceAllUsesWith(CVI->getOperand());
    CVI->eraseFromParent();
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

  /// Emit a destroy_value instruction at the current location, attempting to
  /// fold it locally into another nearby copy_value instruction. Returns a
  /// pointer union initialized with a destroy_value inst if it inserts one,
  /// otherwise returns the copy_value. It is expected that the caller will
  /// remove the copy_value. This allows for the caller to update any state
  /// before the copy_value is destroyed.
  PointerUnion<CopyValueInst *, DestroyValueInst *>
  emitDestroyValue(SILLocation Loc, SILValue Operand);

  /// Convenience function for calling emitCopy on the type lowering
  /// for the non-address value.
  SILValue emitCopyValueOperation(SILLocation Loc, SILValue v) {
    assert(!v->getType().isAddress());
    auto &lowering = getTypeLowering(v->getType());
    return lowering.emitCopyValue(*this, Loc, v);
  }

  /// Convenience function for calling TypeLowering.emitDestroy on the type
  /// lowering for the non-address value.
  void emitDestroyValueOperation(SILLocation Loc, SILValue v) {
    assert(!v->getType().isAddress());
    auto &lowering = getTypeLowering(v->getType());
    lowering.emitDestroyValue(*this, Loc, v);
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
                            Operand->getType().getTupleElementType(FieldNo));
  }

  SILValue emitStructExtract(SILLocation Loc, SILValue Operand, VarDecl *Field,
                             SILType ResultTy) {
    if (auto *SI = dyn_cast<StructInst>(Operand))
      return SI->getFieldValue(Field);

    return createStructExtract(Loc, Operand, Field, ResultTy);
  }

  SILValue emitStructExtract(SILLocation Loc, SILValue Operand,
                             VarDecl *Field) {
    auto type = Operand->getType().getFieldType(Field, F.getModule());
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
// TODO: We really shouldn't be creating instructions unless we are going to
// insert them into a block... This failed in SimplifyCFG.
#ifndef NDEBUG
    TheInst->verifyOperandOwnership();
#endif
  }

  void appendOperandTypeName(SILType OpdTy, llvm::SmallString<16> &Name) {
    if (auto BuiltinIntTy =
            dyn_cast<BuiltinIntegerType>(OpdTy.getSwiftRValueType())) {
      if (BuiltinIntTy == BuiltinIntegerType::getWordType(getASTContext())) {
        Name += "_Word";
      } else {
        unsigned NumBits = BuiltinIntTy->getWidth().getFixedWidth();
        Name += "_Int" + llvm::utostr(NumBits);
      }
    } else {
      assert(OpdTy.getSwiftRValueType() == getASTContext().TheRawPointerType);
      Name += "_RawPointer";
    }
  }
};

/// An wrapper on top of SILBuilder's constructor that automatically sets the
/// current SILDebugScope based on the specified insertion point. This is useful
/// for situations where a single SIL instruction is lowered into a sequence of
/// SIL instructions.
class SILBuilderWithScope : public SILBuilder {
  void inheritScopeFrom(SILInstruction *I) {
    assert(I->getDebugScope() && "instruction has no debug scope");
    setCurrentDebugScope(I->getDebugScope());
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
