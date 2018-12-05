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

#include "swift/Basic/ProfileCounter.h"
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
class IntegerLiteralExpr;
class FloatLiteralExpr;
class SILGlobalVariable;

/// Manage the state needed for a SIL pass across multiple, independent
/// SILBuilder invocations.
///
/// A SIL pass can instantiate a SILBuilderContext object to track information
/// across multiple, potentially independent invocations of SILBuilder. This
/// allows utilities used within the pass to construct a new SILBuilder instance
/// whenever it is convenient or appropriate. For example, a separate SILBuilder
/// should be constructed whenever the current debug location or insertion point
/// changed. Reusing the same SILBuilder and calling setInsertionPoint() easily
/// leads to incorrect debug information.
class SILBuilderContext {
  friend class SILBuilder;

  SILModule &Module;

  /// Allow the SIL module conventions to be overriden within the builder.
  /// This supports passes that lower SIL to a new stage.
  SILModuleConventions silConv = SILModuleConventions(Module);

  /// If this pointer is non-null, then any inserted instruction is
  /// recorded in this list.
  ///
  /// TODO: Give this ownership of InsertedInstrs and migrate users that
  /// currently provide their own InsertedInstrs.
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
  explicit SILBuilderContext(
      SILModule &M, SmallVectorImpl<SILInstruction *> *InsertedInstrs = 0)
      : Module(M), InsertedInstrs(InsertedInstrs) {}

  SILModule &getModule() { return Module; }

  // Allow a pass to override the current SIL module conventions. This should
  // only be done by a pass responsible for lowering SIL to a new stage
  // (e.g. AddressLowering).
  void setSILConventions(SILModuleConventions silConv) {
    this->silConv = silConv;
  }

  void setOpenedArchetypesTracker(SILOpenedArchetypesTracker *Tracker) {
    OpenedArchetypesTracker = Tracker;
    OpenedArchetypes.setOpenedArchetypesTracker(OpenedArchetypesTracker);
  }

  SILOpenedArchetypesTracker *getOpenedArchetypesTracker() const {
    return OpenedArchetypesTracker;
  }

protected:
  /// Notify the context of each new instruction after it is inserted in the
  /// instruction stream.
  void notifyInserted(SILInstruction *Inst) {
    // If the SILBuilder client wants to know about new instructions, record
    // this.
    if (InsertedInstrs)
      InsertedInstrs->push_back(Inst);
  }
};

class SILBuilder {
  friend class SILBuilderWithScope;

  /// Temporary context for clients that don't provide their own.
  SILBuilderContext TempContext;

  /// Reference to the provided SILBuilderContext.
  SILBuilderContext &C;

  /// The SILFunction that we are currently inserting into if we have one.
  ///
  /// If we are building into a block associated with a SILGlobalVariable this
  /// will be a nullptr.
  ///
  /// TODO: This can be made cleaner by using a PointerUnion or the like so we
  /// can store the SILGlobalVariable here as well.
  SILFunction *F;

  /// If the current block that we are inserting into must assume that
  /// the current context we are in has ownership.
  bool hasOwnership;

  /// If this is non-null, the instruction is inserted in the specified
  /// basic block, at the specified InsertPt.  If null, created instructions
  /// are not auto-inserted.
  SILBasicBlock *BB;
  SILBasicBlock::iterator InsertPt;
  const SILDebugScope *CurDebugScope = nullptr;
  Optional<SILLocation> CurDebugLocOverride = None;

public:
  explicit SILBuilder(SILFunction &F, bool isParsing = false)
      : TempContext(F.getModule()), C(TempContext), F(&F),
        hasOwnership(F.hasQualifiedOwnership()), BB(0) {
    C.isParsing = isParsing;
  }

  SILBuilder(SILFunction &F, SmallVectorImpl<SILInstruction *> *InsertedInstrs)
      : TempContext(F.getModule(), InsertedInstrs), C(TempContext), F(&F),
        hasOwnership(F.hasQualifiedOwnership()), BB(0) {}

  explicit SILBuilder(SILInstruction *I,
                      SmallVectorImpl<SILInstruction *> *InsertedInstrs = 0)
      : TempContext(I->getFunction()->getModule(), InsertedInstrs),
        C(TempContext), F(I->getFunction()),
        hasOwnership(F->hasQualifiedOwnership()) {
    setInsertionPoint(I);
  }

  explicit SILBuilder(SILBasicBlock::iterator I,
                      SmallVectorImpl<SILInstruction *> *InsertedInstrs = 0)
      : SILBuilder(&*I, InsertedInstrs) {}

  explicit SILBuilder(SILBasicBlock *BB,
                      SmallVectorImpl<SILInstruction *> *InsertedInstrs = 0)
      : TempContext(BB->getParent()->getModule(), InsertedInstrs),
        C(TempContext), F(BB->getParent()),
        hasOwnership(F->hasQualifiedOwnership()) {
    setInsertionPoint(BB);
  }

  explicit SILBuilder(SILGlobalVariable *GlobVar,
                      SmallVectorImpl<SILInstruction *> *InsertedInstrs = 0);

  SILBuilder(SILBasicBlock *BB, SILBasicBlock::iterator InsertPt,
             SmallVectorImpl<SILInstruction *> *InsertedInstrs = 0)
      : TempContext(BB->getParent()->getModule(), InsertedInstrs),
        C(TempContext), F(BB->getParent()),
        hasOwnership(F->hasQualifiedOwnership()) {
    setInsertionPoint(BB, InsertPt);
  }

  /// Build instructions before the given insertion point, inheriting the debug
  /// location.
  ///
  /// SILBuilderContext must outlive this SILBuilder instance.
  SILBuilder(SILInstruction *I, const SILDebugScope *DS, SILBuilderContext &C)
      : TempContext(C.getModule()), C(C), F(I->getFunction()),
        hasOwnership(F->hasQualifiedOwnership()) {
    assert(DS && "instruction has no debug scope");
    setCurrentDebugScope(DS);
    setInsertionPoint(I);
  }

  /// Build instructions before the given insertion point, inheriting the debug
  /// location.
  ///
  /// SILBuilderContext must outlive this SILBuilder instance.
  SILBuilder(SILBasicBlock *BB, const SILDebugScope *DS, SILBuilderContext &C)
      : TempContext(C.getModule()), C(C), F(BB->getParent()),
        hasOwnership(F->hasQualifiedOwnership()) {
    assert(DS && "block has no debug scope");
    setCurrentDebugScope(DS);
    setInsertionPoint(BB);
  }

  // Allow a pass to override the current SIL module conventions. This should
  // only be done by a pass responsible for lowering SIL to a new stage
  // (e.g. AddressLowering).
  void setSILConventions(SILModuleConventions silConv) { C.silConv = silConv; }

  SILFunction &getFunction() const {
    assert(F && "cannot create this instruction without a function context");
    return *F;
  }
  SILBuilderContext &getBuilderContext() const { return C; }
  SILModule &getModule() const { return C.Module; }
  ASTContext &getASTContext() const { return getModule().getASTContext(); }
  const Lowering::TypeLowering &getTypeLowering(SILType T) const {
    return getModule().getTypeLowering(T);
  }

  void setOpenedArchetypesTracker(SILOpenedArchetypesTracker *Tracker) {
    C.setOpenedArchetypesTracker(Tracker);
  }

  SILOpenedArchetypesTracker *getOpenedArchetypesTracker() const {
    return C.getOpenedArchetypesTracker();
  }

  SILOpenedArchetypesState &getOpenedArchetypes() { return C.OpenedArchetypes; }

  void setCurrentDebugScope(const SILDebugScope *DS) { CurDebugScope = DS; }
  const SILDebugScope *getCurrentDebugScope() const { return CurDebugScope; }

  /// Apply a debug location override. If loc is None, the current override is
  /// removed. Otherwise, newly created debug locations use the given location.
  /// Note: the override location does not apply to debug_value[_addr].
  void applyDebugLocOverride(Optional<SILLocation> loc) {
    CurDebugLocOverride = loc;
  }

  /// Get the current debug location override.
  Optional<SILLocation> getCurrentDebugLocOverride() const {
    return CurDebugLocOverride;
  }

  /// Convenience function for building a SILDebugLocation.
  SILDebugLocation getSILDebugLocation(SILLocation Loc) {
    // FIXME: Audit all uses and enable this assertion.
    // assert(getCurrentDebugScope() && "no debug scope");
    auto Scope = getCurrentDebugScope();
    if (!Scope && F)
        Scope = F->getDebugScope();
    auto overriddenLoc = CurDebugLocOverride ? *CurDebugLocOverride : Loc;
    return SILDebugLocation(overriddenLoc, Scope);
  }

  /// Allow for users to override has ownership if necessary.
  ///
  /// This is only used in the SILParser since it sets whether or not ownership
  /// is qualified after the SILBuilder is constructed due to the usage of
  /// AssumeUnqualifiedOwnershipWhenParsing.
  ///
  /// TODO: Once we start printing [ossa] on SILFunctions to indicate ownership
  /// and get rid of this global option, this can go away.
  void setHasOwnership(bool newHasOwnership) { hasOwnership = newHasOwnership; }

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
    addOpenedArchetypeOperands(&*InsertPt);
  }

  /// setInsertionPoint - Set the insertion point to insert before the specified
  /// instruction.
  void setInsertionPoint(SILInstruction *I) {
    assert(I && "can't set insertion point to a null instruction");
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
    assert(BB && "can't set insertion point to a null basic block");
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
    C.InsertedInstrs = II;
  }

  SmallVectorImpl<SILInstruction *> *getTrackingList() {
    return C.InsertedInstrs;
  }

  //===--------------------------------------------------------------------===//
  // Opened archetypes handling
  //===--------------------------------------------------------------------===//
  void addOpenedArchetypeOperands(SILInstruction *I);

  //===--------------------------------------------------------------------===//
  // Type remapping
  //===--------------------------------------------------------------------===//

  static SILType getPartialApplyResultType(SILType Ty, unsigned ArgCount,
                                         SILModule &M,
                                         SubstitutionMap subs,
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

  /// Move the insertion point to the end of the given block.
  ///
  /// Assumes that no insertion point is currently active.
  void emitBlock(SILBasicBlock *BB) {
    assert(!hasValidInsertionPoint());
    setInsertionPoint(BB);
  }

  /// Branch to the given block if there's an active insertion point,
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

  /// Convenience for creating a fall-through basic block on-the-fly without
  /// affecting the insertion point.
  SILBasicBlock *createFallthroughBlock(SILLocation loc,
                                        SILBasicBlock *targetBB) {
    auto *newBB = F->createBasicBlock();
    SILBuilder(newBB, this->getCurrentDebugScope(), this->getBuilderContext())
        .createBranch(loc, targetBB);
    return newBB;
  }

  //===--------------------------------------------------------------------===//
  // SILInstruction Creation Methods
  //===--------------------------------------------------------------------===//

  AllocStackInst *createAllocStack(SILLocation Loc, SILType elementType,
                                   Optional<SILDebugVariable> Var = None) {
    Loc.markAsPrologue();
    return insert(AllocStackInst::create(getSILDebugLocation(Loc), elementType,
                                         getFunction(), C.OpenedArchetypes,
                                         Var));
  }

  AllocRefInst *createAllocRef(SILLocation Loc, SILType ObjectType,
                               bool objc, bool canAllocOnStack,
                               ArrayRef<SILType> ElementTypes,
                               ArrayRef<SILValue> ElementCountOperands) {
    // AllocRefInsts expand to function calls and can therefore not be
    // counted towards the function prologue.
    assert(!Loc.isInPrologue());
    return insert(AllocRefInst::create(getSILDebugLocation(Loc), getFunction(),
                                       ObjectType, objc, canAllocOnStack,
                                       ElementTypes, ElementCountOperands,
                                       C.OpenedArchetypes));
  }

  AllocRefDynamicInst *createAllocRefDynamic(SILLocation Loc, SILValue operand,
                                             SILType type, bool objc,
                                    ArrayRef<SILType> ElementTypes,
                                    ArrayRef<SILValue> ElementCountOperands) {
    // AllocRefDynamicInsts expand to function calls and can therefore
    // not be counted towards the function prologue.
    assert(!Loc.isInPrologue());
    return insert(AllocRefDynamicInst::create(
        getSILDebugLocation(Loc), *F, operand, type, objc, ElementTypes,
        ElementCountOperands, C.OpenedArchetypes));
  }

  AllocValueBufferInst *
  createAllocValueBuffer(SILLocation Loc, SILType valueType, SILValue operand) {
    return insert(AllocValueBufferInst::create(
        getSILDebugLocation(Loc), valueType, operand, *F, C.OpenedArchetypes));
  }

  AllocBoxInst *createAllocBox(SILLocation Loc, CanSILBoxType BoxType,
                               Optional<SILDebugVariable> Var = None) {
    Loc.markAsPrologue();
    return insert(AllocBoxInst::create(getSILDebugLocation(Loc), BoxType, *F,
                                       C.OpenedArchetypes, Var));
  }

  AllocExistentialBoxInst *
  createAllocExistentialBox(SILLocation Loc, SILType ExistentialType,
                            CanType ConcreteType,
                            ArrayRef<ProtocolConformanceRef> Conformances) {
    return insert(AllocExistentialBoxInst::create(
        getSILDebugLocation(Loc), ExistentialType, ConcreteType, Conformances,
        F, C.OpenedArchetypes));
  }

  ApplyInst *createApply(
      SILLocation Loc, SILValue Fn, SubstitutionMap Subs,
      ArrayRef<SILValue> Args, bool isNonThrowing,
      const GenericSpecializationInformation *SpecializationInfo = nullptr) {
    return insert(ApplyInst::create(getSILDebugLocation(Loc), Fn, Subs, Args,
                                    isNonThrowing, C.silConv, *F,
                                    C.OpenedArchetypes, SpecializationInfo));
  }

  ApplyInst *createApply(
      SILLocation Loc, SILValue Fn, ArrayRef<SILValue> Args, bool isNonThrowing,
      const GenericSpecializationInformation *SpecializationInfo = nullptr) {
    SILFunctionConventions conventions(Fn->getType().castTo<SILFunctionType>(),
                                       getModule());
    return createApply(Loc, Fn, SubstitutionMap(), Args, isNonThrowing,
                       SpecializationInfo);
  }

  TryApplyInst *createTryApply(
      SILLocation Loc, SILValue fn, SubstitutionMap subs,
      ArrayRef<SILValue> args, SILBasicBlock *normalBB, SILBasicBlock *errorBB,
      const GenericSpecializationInformation *SpecializationInfo = nullptr) {
    return insertTerminator(TryApplyInst::create(
        getSILDebugLocation(Loc), fn, subs, args, normalBB, errorBB, *F,
        C.OpenedArchetypes, SpecializationInfo));
  }

  PartialApplyInst *createPartialApply(
      SILLocation Loc, SILValue Fn, SubstitutionMap Subs,
      ArrayRef<SILValue> Args, ParameterConvention CalleeConvention,
      const GenericSpecializationInformation *SpecializationInfo = nullptr) {
    return insert(PartialApplyInst::create(
        getSILDebugLocation(Loc), Fn, Args, Subs, CalleeConvention, *F,
        C.OpenedArchetypes, SpecializationInfo));
  }

  BeginApplyInst *createBeginApply(
      SILLocation Loc, SILValue Fn, SubstitutionMap Subs,
      ArrayRef<SILValue> Args, bool isNonThrowing,
      const GenericSpecializationInformation *SpecializationInfo = nullptr) {
    return insert(BeginApplyInst::create(
        getSILDebugLocation(Loc), Fn, Subs, Args, isNonThrowing, C.silConv, *F,
        C.OpenedArchetypes, SpecializationInfo));
  }

  AbortApplyInst *createAbortApply(SILLocation loc, SILValue beginApply) {
    return insert(new (getModule()) AbortApplyInst(getSILDebugLocation(loc),
                                                   beginApply));
  }

  EndApplyInst *createEndApply(SILLocation loc, SILValue beginApply) {
    return insert(new (getModule()) EndApplyInst(getSILDebugLocation(loc),
                                                 beginApply));
  }

  BuiltinInst *createBuiltin(SILLocation Loc, Identifier Name, SILType ResultTy,
                             SubstitutionMap Subs,
                             ArrayRef<SILValue> Args) {
    return insert(BuiltinInst::create(getSILDebugLocation(Loc), Name,
                                      ResultTy, Subs, Args, getModule()));
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
                                      {}, Args, getModule()));
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
                                      ResultTy, {}, Args, getModule()));
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
           Args[2]->getType().getASTType()->isBuiltinIntegerType(1) &&
           "Must have a third Int1 operand");

    SILType OpdTy = Args[0]->getType();
    SILType Int1Ty = Args[2]->getType();

    TupleTypeElt ResultElts[] = {OpdTy.getASTType(), Int1Ty.getASTType()};
    Type ResultTy = TupleType::get(ResultElts, getASTContext());
    SILType SILResultTy =
        SILType::getPrimitiveObjectType(ResultTy->getCanonicalType());

    return createBuiltinBinaryFunction(Loc, Name, OpdTy, SILResultTy, Args);
  }

  // Creates a dynamic_function_ref or function_ref depending on whether f is
  // dynamically_replaceable.
  FunctionRefBaseInst *createFunctionRefFor(SILLocation Loc, SILFunction *f) {
    if (f->isDynamicallyReplaceable())
      return createDynamicFunctionRef(Loc, f);
    else return createFunctionRef(Loc, f);
  }

  FunctionRefBaseInst *createFunctionRef(SILLocation Loc, SILFunction *f,
                                         SILInstructionKind kind) {
    if (kind == SILInstructionKind::FunctionRefInst)
      return createFunctionRef(Loc, f);
    else if (kind == SILInstructionKind::DynamicFunctionRefInst)
      return createDynamicFunctionRef(Loc, f);
    else if (kind == SILInstructionKind::PreviousDynamicFunctionRefInst)
      return createPreviousDynamicFunctionRef(Loc, f);
    assert(false && "Should not get here");
    return nullptr;
  }

  FunctionRefInst *createFunctionRef(SILLocation Loc, SILFunction *f) {
    return insert(new (getModule())
                      FunctionRefInst(getSILDebugLocation(Loc), f));
  }

  DynamicFunctionRefInst *
  createDynamicFunctionRef(SILLocation Loc, SILFunction *f) {
    return insert(new (getModule()) DynamicFunctionRefInst(
        getSILDebugLocation(Loc), f));
  }

  PreviousDynamicFunctionRefInst *
  createPreviousDynamicFunctionRef(SILLocation Loc, SILFunction *f) {
    return insert(new (getModule()) PreviousDynamicFunctionRefInst(
        getSILDebugLocation(Loc), f));
  }

  AllocGlobalInst *createAllocGlobal(SILLocation Loc, SILGlobalVariable *g) {
    return insert(new (getModule())
                      AllocGlobalInst(getSILDebugLocation(Loc), g));
  }
  GlobalAddrInst *createGlobalAddr(SILLocation Loc, SILGlobalVariable *g) {
    return insert(new (getModule())
                      GlobalAddrInst(getSILDebugLocation(Loc), g));
  }
  GlobalAddrInst *createGlobalAddr(SILLocation Loc, SILType Ty) {
    return insert(new (F->getModule())
                  GlobalAddrInst(getSILDebugLocation(Loc), Ty));
  }
  GlobalValueInst *createGlobalValue(SILLocation Loc, SILGlobalVariable *g) {
    return insert(new (getModule())
                      GlobalValueInst(getSILDebugLocation(Loc), g));
  }
  IntegerLiteralInst *createIntegerLiteral(IntegerLiteralExpr *E);

  IntegerLiteralInst *createIntegerLiteral(SILLocation Loc, SILType Ty,
                                           intmax_t Value) {
    return insert(
        IntegerLiteralInst::create(getSILDebugLocation(Loc), Ty, Value,
                                   getModule()));
  }
  IntegerLiteralInst *createIntegerLiteral(SILLocation Loc, SILType Ty,
                                           const APInt &Value) {
    return insert(
        IntegerLiteralInst::create(getSILDebugLocation(Loc), Ty, Value,
                                   getModule()));
  }

  FloatLiteralInst *createFloatLiteral(FloatLiteralExpr *E);

  FloatLiteralInst *createFloatLiteral(SILLocation Loc, SILType Ty,
                                       const APFloat &Value) {
    return insert(
        FloatLiteralInst::create(getSILDebugLocation(Loc), Ty, Value,
                                 getModule()));
  }

  StringLiteralInst *createStringLiteral(SILLocation Loc, StringRef text,
                                         StringLiteralInst::Encoding encoding) {
    return insert(StringLiteralInst::create(getSILDebugLocation(Loc), text,
                                            encoding, getModule()));
  }

  StringLiteralInst *createStringLiteral(SILLocation Loc, const Twine &text,
                                         StringLiteralInst::Encoding encoding) {
    SmallVector<char, 256> Out;
    return insert(StringLiteralInst::create(
        getSILDebugLocation(Loc), text.toStringRef(Out), encoding, getModule()));
  }

  /// If \p LV is non-trivial, return a \p Qualifier load of \p LV. If \p LV is
  /// trivial, use trivial instead.
  ///
  /// *NOTE* The SupportUnqualifiedSIL is an option to ease the bring up of
  /// Semantic SIL. It enables a pass that must be able to run on both Semantic
  /// SIL and non-Semantic SIL. It has a default argument of false, so if this
  /// is not necessary for your pass, just ignore the parameter.
  LoadInst *createTrivialLoadOr(SILLocation Loc, SILValue LV,
                                LoadOwnershipQualifier Qualifier,
                                bool SupportUnqualifiedSIL = false) {
    if (SupportUnqualifiedSIL && !getFunction().hasQualifiedOwnership()) {
      assert(
          Qualifier != LoadOwnershipQualifier::Copy &&
          "In unqualified SIL, a copy must be done separately form the load");
      return createLoad(Loc, LV, LoadOwnershipQualifier::Unqualified);
    }

    if (LV->getType().isTrivial(getModule())) {
      return createLoad(Loc, LV, LoadOwnershipQualifier::Trivial);
    }
    return createLoad(Loc, LV, Qualifier);
  }

  LoadInst *createLoad(SILLocation Loc, SILValue LV,
                       LoadOwnershipQualifier Qualifier) {
    assert((Qualifier != LoadOwnershipQualifier::Unqualified) ||
           !getFunction().hasQualifiedOwnership() &&
               "Unqualified inst in qualified function");
    assert((Qualifier == LoadOwnershipQualifier::Unqualified) ||
           getFunction().hasQualifiedOwnership() &&
               "Qualified inst in unqualified function");
    assert(LV->getType().isLoadableOrOpaque(getModule()));
    return insert(new (getModule())
                      LoadInst(getSILDebugLocation(Loc), LV, Qualifier));
  }
  
  KeyPathInst *createKeyPath(SILLocation Loc,
                             KeyPathPattern *Pattern,
                             SubstitutionMap Subs,
                             ArrayRef<SILValue> Args,
                             SILType Ty) {
    return insert(KeyPathInst::create(getSILDebugLocation(Loc),
                                      Pattern, Subs, Args,
                                      Ty, getFunction()));
  }

  /// Convenience function for calling emitLoad on the type lowering for
  /// non-address values.
  SILValue emitLoadValueOperation(SILLocation Loc, SILValue LV,
                                  LoadOwnershipQualifier Qualifier) {
    assert(LV->getType().isLoadableOrOpaque(getModule()));
    const auto &lowering = getTypeLowering(LV->getType());
    return lowering.emitLoad(*this, Loc, LV, Qualifier);
  }

  LoadBorrowInst *createLoadBorrow(SILLocation Loc, SILValue LV) {
    assert(LV->getType().isLoadableOrOpaque(getModule()));
    return insert(new (getModule())
                      LoadBorrowInst(getSILDebugLocation(Loc), LV));
  }

  BeginBorrowInst *createBeginBorrow(SILLocation Loc, SILValue LV) {
    return insert(new (getModule())
                      BeginBorrowInst(getSILDebugLocation(Loc), LV));
  }

  // Pass in an address or value, perform a begin_borrow/load_borrow and pass
  // the value to the passed in closure. After the closure has finished
  // executing, automatically insert the end_borrow. The closure can assume that
  // it will receive a loaded loadable value.
  void emitScopedBorrowOperation(SILLocation loc, SILValue original,
                                 function_ref<void(SILValue)> &&fun);

  /// Utility function that returns a trivial store if the stored type is
  /// trivial and a \p Qualifier store if the stored type is non-trivial.
  ///
  /// *NOTE* The SupportUnqualifiedSIL is an option to ease the bring up of
  /// Semantic SIL. It enables a pass that must be able to run on both Semantic
  /// SIL and non-Semantic SIL. It has a default argument of false, so if this
  /// is not necessary for your pass, just ignore the parameter.
  StoreInst *createTrivialStoreOr(SILLocation Loc, SILValue Src,
                                  SILValue DestAddr,
                                  StoreOwnershipQualifier Qualifier,
                                  bool SupportUnqualifiedSIL = false) {
    if (SupportUnqualifiedSIL && !getFunction().hasQualifiedOwnership()) {
      assert(
          Qualifier != StoreOwnershipQualifier::Assign &&
          "In unqualified SIL, assigns must be represented via 2 instructions");
      return createStore(Loc, Src, DestAddr,
                         StoreOwnershipQualifier::Unqualified);
    }
    if (Src->getType().isTrivial(getModule())) {
      return createStore(Loc, Src, DestAddr, StoreOwnershipQualifier::Trivial);
    }
    return createStore(Loc, Src, DestAddr, Qualifier);
  }

  StoreInst *createStore(SILLocation Loc, SILValue Src, SILValue DestAddr,
                         StoreOwnershipQualifier Qualifier) {
    assert((Qualifier != StoreOwnershipQualifier::Unqualified) ||
           !getFunction().hasQualifiedOwnership() &&
               "Unqualified inst in qualified function");
    assert((Qualifier == StoreOwnershipQualifier::Unqualified) ||
           getFunction().hasQualifiedOwnership() &&
               "Qualified inst in unqualified function");
    return insert(new (getModule()) StoreInst(getSILDebugLocation(Loc), Src,
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

  EndBorrowInst *createEndBorrow(SILLocation loc, SILValue borrowedValue) {
    return insert(new (getModule())
                      EndBorrowInst(getSILDebugLocation(loc), borrowedValue));
  }

  EndBorrowInst *createEndBorrow(SILLocation Loc, SILValue BorrowedValue,
                                 SILValue OriginalValue) {
    return insert(new (getModule())
                      EndBorrowInst(getSILDebugLocation(Loc), BorrowedValue));
  }

  BeginAccessInst *createBeginAccess(SILLocation loc, SILValue address,
                                     SILAccessKind accessKind,
                                     SILAccessEnforcement enforcement,
                                     bool noNestedConflict,
                                     bool fromBuiltin) {
    return insert(new (getModule()) BeginAccessInst(
        getSILDebugLocation(loc), address, accessKind, enforcement,
        noNestedConflict, fromBuiltin));
  }

  EndAccessInst *createEndAccess(SILLocation loc, SILValue address,
                                 bool aborted) {
    return insert(new (getModule()) EndAccessInst(
        getSILDebugLocation(loc), address, aborted));
  }

  BeginUnpairedAccessInst *
  createBeginUnpairedAccess(SILLocation loc, SILValue address, SILValue buffer,
                            SILAccessKind accessKind,
                            SILAccessEnforcement enforcement,
                            bool noNestedConflict,
                            bool fromBuiltin) {
    return insert(new (getModule()) BeginUnpairedAccessInst(
        getSILDebugLocation(loc), address, buffer, accessKind, enforcement,
        noNestedConflict, fromBuiltin));
  }

  EndUnpairedAccessInst *
  createEndUnpairedAccess(SILLocation loc, SILValue buffer,
                          SILAccessEnforcement enforcement, bool aborted,
                          bool fromBuiltin) {
    return insert(new (getModule()) EndUnpairedAccessInst(
        getSILDebugLocation(loc), buffer, enforcement, aborted, fromBuiltin));
  }

  AssignInst *createAssign(SILLocation Loc, SILValue Src, SILValue DestAddr) {
    return insert(new (getModule())
                      AssignInst(getSILDebugLocation(Loc), Src, DestAddr));
  }

  StoreBorrowInst *createStoreBorrow(SILLocation Loc, SILValue Src,
                                     SILValue DestAddr) {
    return insert(new (getModule())
                      StoreBorrowInst(getSILDebugLocation(Loc), Src, DestAddr));
  }

  MarkUninitializedInst *
  createMarkUninitialized(SILLocation Loc, SILValue src,
                          MarkUninitializedInst::Kind k) {
    return insert(new (getModule()) MarkUninitializedInst(
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
                                  SubstitutionMap initStorageSubs,
                                  SILValue storage,
                                  SILValue setterFunc,
                                  SubstitutionMap setterSubs,
                                  SILValue self,
                                  SILType ty) {
    return insert(MarkUninitializedBehaviorInst::create(getModule(),
         getSILDebugLocation(Loc),
         initStorageFunc, initStorageSubs, storage,
         setterFunc, setterSubs, self, ty));
  }

  MarkFunctionEscapeInst *createMarkFunctionEscape(SILLocation Loc,
                                                   ArrayRef<SILValue> vars) {
    return insert(
        MarkFunctionEscapeInst::create(getSILDebugLocation(Loc), vars, getFunction()));
  }

  DebugValueInst *createDebugValue(SILLocation Loc, SILValue src,
                                   SILDebugVariable Var);
  DebugValueAddrInst *createDebugValueAddr(SILLocation Loc, SILValue src,
                                           SILDebugVariable Var);

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  Load##Name##Inst *createLoad##Name(SILLocation Loc, \
                                     SILValue src, \
                                     IsTake_t isTake) { \
    return insert(new (getModule()) \
      Load##Name##Inst(getSILDebugLocation(Loc), src, isTake)); \
  } \
  Store##Name##Inst *createStore##Name(SILLocation Loc, \
                                       SILValue value, \
                                       SILValue dest, \
                                       IsInitialization_t isInit) { \
    return insert(new (getModule()) \
      Store##Name##Inst(getSILDebugLocation(Loc), value, dest, isInit)); \
  }
#define LOADABLE_REF_STORAGE_HELPER(Name) \
  Name##ToRefInst *create##Name##ToRef(SILLocation Loc, SILValue op, \
                                       SILType ty) { \
    return insert(new (getModule()) \
      Name##ToRefInst(getSILDebugLocation(Loc), op, ty)); \
  } \
  RefTo##Name##Inst *createRefTo##Name(SILLocation Loc, SILValue op, \
                                       SILType ty) { \
    return insert(new (getModule()) \
      RefTo##Name##Inst(getSILDebugLocation(Loc), op, ty)); \
  }
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  LOADABLE_REF_STORAGE_HELPER(Name) \
  StrongRetain##Name##Inst *createStrongRetain##Name(SILLocation Loc, \
                                                     SILValue Operand, \
                                                     Atomicity atomicity) { \
    return insert(new (getModule()) \
      StrongRetain##Name##Inst(getSILDebugLocation(Loc), Operand, atomicity)); \
  } \
  Name##RetainInst *create##Name##Retain(SILLocation Loc, SILValue Operand, \
                                         Atomicity atomicity) { \
    return insert(new (getModule()) \
      Name##RetainInst(getSILDebugLocation(Loc), Operand, atomicity)); \
  } \
  Name##ReleaseInst *create##Name##Release(SILLocation Loc, \
                                           SILValue Operand, \
                                           Atomicity atomicity) { \
    return insert(new (getModule()) \
      Name##ReleaseInst(getSILDebugLocation(Loc), Operand, atomicity)); \
  } \
  Copy##Name##ValueInst *createCopy##Name##Value(SILLocation Loc, \
                                                 SILValue operand) { \
    return insert(new (getModule()) \
      Copy##Name##ValueInst(getSILDebugLocation(Loc), operand, getModule())); \
  }
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, "...") \
  ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, "...")
#define UNCHECKED_REF_STORAGE(Name, ...) \
  LOADABLE_REF_STORAGE_HELPER(Name)
#include "swift/AST/ReferenceStorage.def"
#undef LOADABLE_REF_STORAGE_HELPER

  CopyAddrInst *createCopyAddr(SILLocation Loc, SILValue srcAddr,
                               SILValue destAddr, IsTake_t isTake,
                               IsInitialization_t isInitialize) {
    assert(srcAddr->getType() == destAddr->getType());
    return insert(new (getModule()) CopyAddrInst(
        getSILDebugLocation(Loc), srcAddr, destAddr, isTake, isInitialize));
  }

  BindMemoryInst *createBindMemory(SILLocation Loc, SILValue base,
                                   SILValue index, SILType boundType) {
    return insert(BindMemoryInst::create(getSILDebugLocation(Loc), base, index,
                                         boundType, getFunction(),
                                         C.OpenedArchetypes));
  }

  ConvertFunctionInst *createConvertFunction(SILLocation Loc, SILValue Op,
                                             SILType Ty,
                                             bool WithoutActuallyEscaping) {
    return insert(ConvertFunctionInst::create(getSILDebugLocation(Loc), Op, Ty,
                                              getFunction(), C.OpenedArchetypes,
                                              WithoutActuallyEscaping));
  }

  ConvertEscapeToNoEscapeInst *
  createConvertEscapeToNoEscape(SILLocation Loc, SILValue Op, SILType Ty,
                                bool isEscapedByUser, bool lifetimeGuaranteed) {
    return insert(ConvertEscapeToNoEscapeInst::create(
        getSILDebugLocation(Loc), Op, Ty, getFunction(), C.OpenedArchetypes,
        isEscapedByUser, lifetimeGuaranteed));
  }

  ThinFunctionToPointerInst *
  createThinFunctionToPointer(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (getModule()) ThinFunctionToPointerInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  PointerToThinFunctionInst *
  createPointerToThinFunction(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(PointerToThinFunctionInst::create(
        getSILDebugLocation(Loc), Op, Ty, getFunction(), C.OpenedArchetypes));
  }

  UpcastInst *createUpcast(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(UpcastInst::create(getSILDebugLocation(Loc), Op, Ty,
                                     getFunction(), C.OpenedArchetypes));
  }

  AddressToPointerInst *createAddressToPointer(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(new (getModule()) AddressToPointerInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  PointerToAddressInst *createPointerToAddress(SILLocation Loc, SILValue Op,
                                               SILType Ty,
                                               bool isStrict,
                                               bool isInvariant = false){
    return insert(new (getModule()) PointerToAddressInst(
                    getSILDebugLocation(Loc), Op, Ty, isStrict, isInvariant));
  }

  UncheckedRefCastInst *createUncheckedRefCast(SILLocation Loc, SILValue Op,
                                               SILType Ty) {
    return insert(UncheckedRefCastInst::create(
        getSILDebugLocation(Loc), Op, Ty, getFunction(), C.OpenedArchetypes));
  }

  UncheckedRefCastAddrInst *
  createUncheckedRefCastAddr(SILLocation Loc, SILValue src, CanType sourceType,
                             SILValue dest, CanType targetType) {
    return insert(new (getModule()) UncheckedRefCastAddrInst(
        getSILDebugLocation(Loc), src, sourceType, dest, targetType));
  }

  UncheckedAddrCastInst *createUncheckedAddrCast(SILLocation Loc, SILValue Op,
                                                 SILType Ty) {
    return insert(UncheckedAddrCastInst::create(
        getSILDebugLocation(Loc), Op, Ty, getFunction(), C.OpenedArchetypes));
  }

  UncheckedTrivialBitCastInst *
  createUncheckedTrivialBitCast(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(UncheckedTrivialBitCastInst::create(
        getSILDebugLocation(Loc), Op, Ty, getFunction(), C.OpenedArchetypes));
  }

  UncheckedBitwiseCastInst *
  createUncheckedBitwiseCast(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(UncheckedBitwiseCastInst::create(
        getSILDebugLocation(Loc), Op, Ty, getFunction(), C.OpenedArchetypes));
  }

  RefToBridgeObjectInst *createRefToBridgeObject(SILLocation Loc, SILValue Ref,
                                                 SILValue Bits) {
    auto Ty = SILType::getBridgeObjectType(getASTContext());
    return insert(new (getModule()) RefToBridgeObjectInst(
        getSILDebugLocation(Loc), Ref, Bits, Ty));
  }

  BridgeObjectToRefInst *createBridgeObjectToRef(SILLocation Loc, SILValue Op,
                                                 SILType Ty) {
    return insert(new (getModule()) BridgeObjectToRefInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  ValueToBridgeObjectInst *createValueToBridgeObject(SILLocation Loc,
                                                     SILValue value) {
    auto Ty = SILType::getBridgeObjectType(getASTContext());
    return insert(new (getModule()) ValueToBridgeObjectInst(
        getSILDebugLocation(Loc), value, Ty));
  }

  BridgeObjectToWordInst *createBridgeObjectToWord(SILLocation Loc,
                                                   SILValue Op) {
    auto Ty = SILType::getBuiltinWordType(getASTContext());
    return createBridgeObjectToWord(Loc, Op, Ty);
  }

  BridgeObjectToWordInst *createBridgeObjectToWord(SILLocation Loc, SILValue Op,
                                                   SILType Ty) {
    return insert(new (getModule()) BridgeObjectToWordInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  RefToRawPointerInst *createRefToRawPointer(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new (getModule())
                      RefToRawPointerInst(getSILDebugLocation(Loc), Op, Ty));
  }

  RawPointerToRefInst *createRawPointerToRef(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
    return insert(new (getModule())
                      RawPointerToRefInst(getSILDebugLocation(Loc), Op, Ty));
  }

  ThinToThickFunctionInst *createThinToThickFunction(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(ThinToThickFunctionInst::create(
        getSILDebugLocation(Loc), Op, Ty, getFunction(), C.OpenedArchetypes));
  }

  ThickToObjCMetatypeInst *createThickToObjCMetatype(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(new (getModule()) ThickToObjCMetatypeInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  ObjCToThickMetatypeInst *createObjCToThickMetatype(SILLocation Loc,
                                                     SILValue Op, SILType Ty) {
    return insert(new (getModule()) ObjCToThickMetatypeInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  ObjCProtocolInst *createObjCProtocol(SILLocation Loc, ProtocolDecl *P,
                                       SILType Ty) {
    return insert(new (getModule())
                      ObjCProtocolInst(getSILDebugLocation(Loc), P, Ty));
  }

  CopyValueInst *createCopyValue(SILLocation Loc, SILValue operand) {
    return insert(new (getModule())
                      CopyValueInst(getSILDebugLocation(Loc), operand));
  }

  DestroyValueInst *createDestroyValue(SILLocation Loc, SILValue operand) {
    assert(operand->getType().isLoadableOrOpaque(getModule()));
    return insert(new (getModule())
                      DestroyValueInst(getSILDebugLocation(Loc), operand));
  }

  UnconditionalCheckedCastInst *
  createUnconditionalCheckedCast(SILLocation Loc, SILValue op, SILType destTy) {
    return insert(UnconditionalCheckedCastInst::create(
        getSILDebugLocation(Loc), op, destTy, getFunction(),
        C.OpenedArchetypes));
  }

  UnconditionalCheckedCastAddrInst *
  createUnconditionalCheckedCastAddr(SILLocation Loc, SILValue src,
                                     CanType sourceType, SILValue dest,
                                     CanType targetType) {
    return insert(new (getModule()) UnconditionalCheckedCastAddrInst(
        getSILDebugLocation(Loc), src, sourceType, dest, targetType));
  }

  UnconditionalCheckedCastValueInst *
  createUnconditionalCheckedCastValue(SILLocation Loc,
                                      SILValue op, SILType destTy) {
    return insert(UnconditionalCheckedCastValueInst::create(
        getSILDebugLocation(Loc), op, destTy, getFunction(),
        C.OpenedArchetypes));
  }

  RetainValueInst *createRetainValue(SILLocation Loc, SILValue operand,
                                     Atomicity atomicity) {
    assert(C.isParsing || !getFunction().hasQualifiedOwnership());
    assert(operand->getType().isLoadableOrOpaque(getModule()));
    return insert(new (getModule()) RetainValueInst(getSILDebugLocation(Loc),
                                                      operand, atomicity));
  }

  RetainValueAddrInst *createRetainValueAddr(SILLocation Loc, SILValue operand,
                                             Atomicity atomicity) {
    assert(C.isParsing || !getFunction().hasQualifiedOwnership());
    return insert(new (getModule()) RetainValueAddrInst(
        getSILDebugLocation(Loc), operand, atomicity));
  }

  ReleaseValueInst *createReleaseValue(SILLocation Loc, SILValue operand,
                                       Atomicity atomicity) {
    assert(C.isParsing || !getFunction().hasQualifiedOwnership());
    assert(operand->getType().isLoadableOrOpaque(getModule()));
    return insert(new (getModule()) ReleaseValueInst(getSILDebugLocation(Loc),
                                                       operand, atomicity));
  }

  ReleaseValueAddrInst *createReleaseValueAddr(SILLocation Loc,
                                               SILValue operand,
                                               Atomicity atomicity) {
    assert(C.isParsing || !getFunction().hasQualifiedOwnership());
    return insert(new (getModule()) ReleaseValueAddrInst(
        getSILDebugLocation(Loc), operand, atomicity));
  }

  UnmanagedRetainValueInst *createUnmanagedRetainValue(SILLocation Loc,
                                                       SILValue operand,
                                                       Atomicity atomicity) {
    assert(getFunction().hasQualifiedOwnership());
    assert(operand->getType().isLoadableOrOpaque(getModule()));
    return insert(new (getModule()) UnmanagedRetainValueInst(
        getSILDebugLocation(Loc), operand, atomicity));
  }

  UnmanagedReleaseValueInst *createUnmanagedReleaseValue(SILLocation Loc,
                                                         SILValue operand,
                                                         Atomicity atomicity) {
    assert(getFunction().hasQualifiedOwnership());
    assert(operand->getType().isLoadableOrOpaque(getModule()));
    return insert(new (getModule()) UnmanagedReleaseValueInst(
        getSILDebugLocation(Loc), operand, atomicity));
  }

  AutoreleaseValueInst *createAutoreleaseValue(SILLocation Loc,
                                               SILValue operand,
                                               Atomicity atomicity) {
    return insert(new (getModule()) AutoreleaseValueInst(
        getSILDebugLocation(Loc), operand, atomicity));
  }

  UnmanagedAutoreleaseValueInst *
  createUnmanagedAutoreleaseValue(SILLocation Loc, SILValue operand,
                                  Atomicity atomicity) {
    return insert(new (getModule()) UnmanagedAutoreleaseValueInst(
                      getSILDebugLocation(Loc), operand, atomicity));
  }

  SetDeallocatingInst *createSetDeallocating(SILLocation Loc,
                                            SILValue operand,
                                            Atomicity atomicity) {
    return insert(new (getModule()) SetDeallocatingInst(
        getSILDebugLocation(Loc), operand, atomicity));
  }

  ObjectInst *createObject(SILLocation Loc, SILType Ty,
                           ArrayRef<SILValue> Elements,
                           unsigned NumBaseElements) {
    return insert(ObjectInst::create(getSILDebugLocation(Loc), Ty, Elements,
                                     NumBaseElements, getModule(),
                                     hasOwnership));
  }

  StructInst *createStruct(SILLocation Loc, SILType Ty,
                           ArrayRef<SILValue> Elements) {
    assert(Ty.isLoadableOrOpaque(getModule()));
    return insert(StructInst::create(getSILDebugLocation(Loc), Ty, Elements,
                                     getModule(), hasOwnership));
  }

  TupleInst *createTuple(SILLocation Loc, SILType Ty,
                         ArrayRef<SILValue> Elements) {
    assert(Ty.isLoadableOrOpaque(getModule()));
    return insert(TupleInst::create(getSILDebugLocation(Loc), Ty, Elements,
                                    getModule(), hasOwnership));
  }

  TupleInst *createTuple(SILLocation loc, ArrayRef<SILValue> elts);

  EnumInst *createEnum(SILLocation Loc, SILValue Operand,
                       EnumElementDecl *Element, SILType Ty) {
    assert(Ty.isLoadableOrOpaque(getModule()));
    return insert(new (getModule()) EnumInst(getSILDebugLocation(Loc),
                                               Operand, Element, Ty));
  }

  /// Inject a loadable value into the corresponding optional type.
  EnumInst *createOptionalSome(SILLocation Loc, SILValue operand, SILType ty) {
    assert(ty.isLoadableOrOpaque(getModule()));
    auto someDecl = getModule().getASTContext().getOptionalSomeDecl();
    return createEnum(Loc, operand, someDecl, ty);
  }

  /// Create the nil value of a loadable optional type.
  EnumInst *createOptionalNone(SILLocation Loc, SILType ty) {
    assert(ty.isLoadableOrOpaque(getModule()));
    auto noneDecl = getModule().getASTContext().getOptionalNoneDecl();
    return createEnum(Loc, nullptr, noneDecl, ty);
  }

  InitEnumDataAddrInst *createInitEnumDataAddr(SILLocation Loc,
                                               SILValue Operand,
                                               EnumElementDecl *Element,
                                               SILType Ty) {
    return insert(new (getModule()) InitEnumDataAddrInst(
        getSILDebugLocation(Loc), Operand, Element, Ty));
  }

  UncheckedEnumDataInst *createUncheckedEnumData(SILLocation Loc,
                                                 SILValue Operand,
                                                 EnumElementDecl *Element,
                                                 SILType Ty) {
    assert(Ty.isLoadableOrOpaque(getModule()));
    return insert(new (getModule()) UncheckedEnumDataInst(
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
    return insert(new (getModule()) UncheckedTakeEnumDataAddrInst(
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
    return insert(new (getModule()) InjectEnumAddrInst(
        getSILDebugLocation(Loc), Operand, Element));
  }

  SelectEnumInst *
  createSelectEnum(SILLocation Loc, SILValue Operand, SILType Ty,
                   SILValue DefaultValue,
                   ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues,
                   Optional<ArrayRef<ProfileCounter>> CaseCounts = None,
                   ProfileCounter DefaultCount = ProfileCounter()) {
    assert(Ty.isLoadableOrOpaque(getModule()));
    return insert(SelectEnumInst::create(
        getSILDebugLocation(Loc), Operand, Ty, DefaultValue, CaseValues,
        getModule(), CaseCounts, DefaultCount, hasOwnership));
  }

  SelectEnumAddrInst *createSelectEnumAddr(
      SILLocation Loc, SILValue Operand, SILType Ty, SILValue DefaultValue,
      ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues,
      Optional<ArrayRef<ProfileCounter>> CaseCounts = None,
      ProfileCounter DefaultCount = ProfileCounter()) {
    return insert(SelectEnumAddrInst::create(
        getSILDebugLocation(Loc), Operand, Ty, DefaultValue, CaseValues,
        getModule(), CaseCounts, DefaultCount));
  }

  SelectValueInst *createSelectValue(
      SILLocation Loc, SILValue Operand, SILType Ty, SILValue DefaultResult,
      ArrayRef<std::pair<SILValue, SILValue>> CaseValuesAndResults) {
    return insert(SelectValueInst::create(getSILDebugLocation(Loc), Operand, Ty,
                                          DefaultResult, CaseValuesAndResults,
                                          getModule(), hasOwnership));
  }

  TupleExtractInst *createTupleExtract(SILLocation Loc, SILValue Operand,
                                       unsigned FieldNo, SILType ResultTy) {
    return insert(new (getModule()) TupleExtractInst(
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
    return insert(new (getModule()) TupleElementAddrInst(
        getSILDebugLocation(Loc), Operand, FieldNo, ResultTy));
  }

  TupleElementAddrInst *
  createTupleElementAddr(SILLocation Loc, SILValue Operand, unsigned FieldNo) {
    return insert(new (getModule()) TupleElementAddrInst(
        getSILDebugLocation(Loc), Operand, FieldNo,
        Operand->getType().getTupleElementType(FieldNo)));
  }

  StructExtractInst *createStructExtract(SILLocation Loc, SILValue Operand,
                                         VarDecl *Field, SILType ResultTy) {
    return insert(new (getModule()) StructExtractInst(
        getSILDebugLocation(Loc), Operand, Field, ResultTy));
  }

  StructExtractInst *createStructExtract(SILLocation Loc, SILValue Operand,
                                         VarDecl *Field) {
    auto type = Operand->getType().getFieldType(Field, getModule());
    return createStructExtract(Loc, Operand, Field, type);
  }

  StructElementAddrInst *createStructElementAddr(SILLocation Loc,
                                                 SILValue Operand,
                                                 VarDecl *Field,
                                                 SILType ResultTy) {
    return insert(new (getModule()) StructElementAddrInst(
        getSILDebugLocation(Loc), Operand, Field, ResultTy));
  }

  StructElementAddrInst *
  createStructElementAddr(SILLocation Loc, SILValue Operand, VarDecl *Field) {
    auto ResultTy = Operand->getType().getFieldType(Field, getModule());
    return createStructElementAddr(Loc, Operand, Field, ResultTy);
  }

  RefElementAddrInst *createRefElementAddr(SILLocation Loc, SILValue Operand,
                                           VarDecl *Field, SILType ResultTy) {
    return insert(new (getModule()) RefElementAddrInst(
        getSILDebugLocation(Loc), Operand, Field, ResultTy));
  }
  RefElementAddrInst *createRefElementAddr(SILLocation Loc, SILValue Operand,
                                           VarDecl *Field) {
    auto ResultTy = Operand->getType().getFieldType(Field, getModule());
    return createRefElementAddr(Loc, Operand, Field, ResultTy);
  }

  RefTailAddrInst *createRefTailAddr(SILLocation Loc, SILValue Ref,
                                     SILType ResultTy) {
    return insert(new (getModule()) RefTailAddrInst(getSILDebugLocation(Loc),
                                                      Ref, ResultTy));
  }

  DestructureStructInst *createDestructureStruct(SILLocation Loc,
                                                 SILValue Operand) {
    return insert(DestructureStructInst::create(
        getModule(), getSILDebugLocation(Loc), Operand));
  }

  DestructureTupleInst *createDestructureTuple(SILLocation Loc,
                                               SILValue Operand) {
    return insert(DestructureTupleInst::create(
        getModule(), getSILDebugLocation(Loc), Operand));
  }

  MultipleValueInstruction *emitDestructureValueOperation(SILLocation Loc,
                                                          SILValue Operand) {
    SILType OpTy = Operand->getType();
    if (OpTy.is<TupleType>())
      return createDestructureTuple(Loc, Operand);
    if (OpTy.getStructOrBoundGenericStruct())
      return createDestructureStruct(Loc, Operand);
    llvm_unreachable("Can not emit a destructure for this type of operand.");
  }

  void
  emitDestructureValueOperation(SILLocation Loc, SILValue Operand,
                                function_ref<void(unsigned, SILValue)> Func) {
    // Do a quick check to see if we have a tuple without elements. In that
    // case, bail early since we are not going to ever invoke Func.
    if (auto TTy = Operand->getType().castTo<TupleType>())
      if (0 == TTy->getNumElements())
        return;

    auto *Destructure = emitDestructureValueOperation(Loc, Operand);
    auto Results = Destructure->getResults();
    // TODO: For some reason, llvm::enumerate(Results) does not
    // compile.
    for (unsigned i : indices(Results)) {
      Func(i, Results[i]);
    }
  }

  void
  emitShallowDestructureValueOperation(SILLocation Loc, SILValue Operand,
                                       llvm::SmallVectorImpl<SILValue> &Result);

  void emitShallowDestructureAddressOperation(
      SILLocation Loc, SILValue Operand,
      llvm::SmallVectorImpl<SILValue> &Result);

  ClassMethodInst *createClassMethod(SILLocation Loc, SILValue Operand,
                                     SILDeclRef Member, SILType MethodTy) {
    return insert(new (getModule()) ClassMethodInst(
        getSILDebugLocation(Loc), Operand, Member, MethodTy));
  }

  SuperMethodInst *createSuperMethod(SILLocation Loc, SILValue Operand,
                                     SILDeclRef Member, SILType MethodTy) {
    return insert(new (getModule()) SuperMethodInst(
        getSILDebugLocation(Loc), Operand, Member, MethodTy));
  }

  ObjCMethodInst *createObjCMethod(SILLocation Loc, SILValue Operand,
                                   SILDeclRef Member, SILType MethodTy) {
    return insert(ObjCMethodInst::create(getSILDebugLocation(Loc), Operand,
                                         Member, MethodTy, &getFunction(),
                                         C.OpenedArchetypes));
  }

  ObjCSuperMethodInst *createObjCSuperMethod(SILLocation Loc, SILValue Operand,
                                             SILDeclRef Member, SILType MethodTy) {
    return insert(new (getModule()) ObjCSuperMethodInst(
        getSILDebugLocation(Loc), Operand, Member, MethodTy));
  }

  WitnessMethodInst *createWitnessMethod(SILLocation Loc, CanType LookupTy,
                                         ProtocolConformanceRef Conformance,
                                         SILDeclRef Member, SILType MethodTy) {
    return insert(WitnessMethodInst::create(
        getSILDebugLocation(Loc), LookupTy, Conformance, Member, MethodTy,
        &getFunction(), C.OpenedArchetypes));
  }

  OpenExistentialAddrInst *
  createOpenExistentialAddr(SILLocation Loc, SILValue Operand, SILType SelfTy,
                            OpenedExistentialAccess ForAccess) {
    auto *I = insert(new (getModule()) OpenExistentialAddrInst(
        getSILDebugLocation(Loc), Operand, SelfTy, ForAccess));
    if (C.OpenedArchetypesTracker)
      C.OpenedArchetypesTracker->registerOpenedArchetypes(I);
    return I;
  }

  OpenExistentialValueInst *createOpenExistentialValue(SILLocation Loc,
                                                         SILValue Operand,
                                                         SILType SelfTy) {
    auto *I = insert(new (getModule()) OpenExistentialValueInst(
        getSILDebugLocation(Loc), Operand, SelfTy));
    if (C.OpenedArchetypesTracker)
      C.OpenedArchetypesTracker->registerOpenedArchetypes(I);
    return I;
  }

  OpenExistentialMetatypeInst *createOpenExistentialMetatype(SILLocation Loc,
                                                             SILValue operand,
                                                             SILType selfTy) {
    auto *I = insert(new (getModule()) OpenExistentialMetatypeInst(
        getSILDebugLocation(Loc), operand, selfTy));
    if (C.OpenedArchetypesTracker)
      C.OpenedArchetypesTracker->registerOpenedArchetypes(I);
    return I;
  }

  OpenExistentialRefInst *
  createOpenExistentialRef(SILLocation Loc, SILValue Operand, SILType Ty) {
    auto *I = insert(new (getModule()) OpenExistentialRefInst(
        getSILDebugLocation(Loc), Operand, Ty, hasOwnership));
    if (C.OpenedArchetypesTracker)
      C.OpenedArchetypesTracker->registerOpenedArchetypes(I);
    return I;
  }

  OpenExistentialBoxInst *
  createOpenExistentialBox(SILLocation Loc, SILValue Operand, SILType Ty) {
    auto *I = insert(new (getModule()) OpenExistentialBoxInst(
        getSILDebugLocation(Loc), Operand, Ty));
    if (C.OpenedArchetypesTracker)
      C.OpenedArchetypesTracker->registerOpenedArchetypes(I);
    return I;
  }

  OpenExistentialBoxValueInst *
  createOpenExistentialBoxValue(SILLocation Loc, SILValue Operand, SILType Ty) {
    auto *I = insert(new (getModule()) OpenExistentialBoxValueInst(
        getSILDebugLocation(Loc), Operand, Ty));
    if (C.OpenedArchetypesTracker)
      C.OpenedArchetypesTracker->registerOpenedArchetypes(I);
    return I;
  }

  InitExistentialAddrInst *
  createInitExistentialAddr(SILLocation Loc, SILValue Existential,
                            CanType FormalConcreteType,
                            SILType LoweredConcreteType,
                            ArrayRef<ProtocolConformanceRef> Conformances) {
    return insert(InitExistentialAddrInst::create(
        getSILDebugLocation(Loc), Existential, FormalConcreteType,
        LoweredConcreteType, Conformances, &getFunction(), C.OpenedArchetypes));
  }

  InitExistentialValueInst *
  createInitExistentialValue(SILLocation Loc, SILType ExistentialType,
                              CanType FormalConcreteType, SILValue Concrete,
                              ArrayRef<ProtocolConformanceRef> Conformances) {
    return insert(InitExistentialValueInst::create(
        getSILDebugLocation(Loc), ExistentialType, FormalConcreteType, Concrete,
        Conformances, &getFunction(), C.OpenedArchetypes));
  }

  InitExistentialMetatypeInst *
  createInitExistentialMetatype(SILLocation Loc, SILValue metatype,
                                SILType existentialType,
                                ArrayRef<ProtocolConformanceRef> conformances) {
    return insert(InitExistentialMetatypeInst::create(
        getSILDebugLocation(Loc), existentialType, metatype, conformances,
        &getFunction(), C.OpenedArchetypes));
  }

  InitExistentialRefInst *
  createInitExistentialRef(SILLocation Loc, SILType ExistentialType,
                           CanType FormalConcreteType, SILValue Concrete,
                           ArrayRef<ProtocolConformanceRef> Conformances) {
    return insert(InitExistentialRefInst::create(
        getSILDebugLocation(Loc), ExistentialType, FormalConcreteType, Concrete,
        Conformances, &getFunction(), C.OpenedArchetypes));
  }

  DeinitExistentialAddrInst *createDeinitExistentialAddr(SILLocation Loc,
                                                         SILValue Existential) {
    return insert(new (getModule()) DeinitExistentialAddrInst(
        getSILDebugLocation(Loc), Existential));
  }

  DeinitExistentialValueInst *
  createDeinitExistentialValue(SILLocation Loc, SILValue Existential) {
    return insert(new (getModule()) DeinitExistentialValueInst(
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
    return insert(new (getModule()) ProjectBlockStorageInst(
        getSILDebugLocation(Loc), Storage, CaptureTy));
  }

  InitBlockStorageHeaderInst *
  createInitBlockStorageHeader(SILLocation Loc, SILValue BlockStorage,
                               SILValue InvokeFunction, SILType BlockType,
                               SubstitutionMap Subs) {
    return insert(InitBlockStorageHeaderInst::create(getFunction(),
      getSILDebugLocation(Loc), BlockStorage, InvokeFunction, BlockType, Subs));
  }

  MetatypeInst *createMetatype(SILLocation Loc, SILType Metatype) {
    return insert(MetatypeInst::create(getSILDebugLocation(Loc), Metatype,
                                       &getFunction(), C.OpenedArchetypes));
  }

  ObjCMetatypeToObjectInst *
  createObjCMetatypeToObject(SILLocation Loc, SILValue Op, SILType Ty) {
    return insert(new (getModule()) ObjCMetatypeToObjectInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  ObjCExistentialMetatypeToObjectInst *
  createObjCExistentialMetatypeToObject(SILLocation Loc, SILValue Op,
                                        SILType Ty) {
    return insert(new (getModule()) ObjCExistentialMetatypeToObjectInst(
        getSILDebugLocation(Loc), Op, Ty));
  }

  ValueMetatypeInst *createValueMetatype(SILLocation Loc, SILType Metatype,
                                         SILValue Base);

  ExistentialMetatypeInst *
  createExistentialMetatype(SILLocation Loc, SILType Metatype, SILValue Base) {
    return insert(new (getModule()) ExistentialMetatypeInst(
        getSILDebugLocation(Loc), Metatype, Base));
  }

  CopyBlockInst *createCopyBlock(SILLocation Loc, SILValue Operand) {
    return insert(new (getModule())
                      CopyBlockInst(getSILDebugLocation(Loc), Operand));
  }

  CopyBlockWithoutEscapingInst *
  createCopyBlockWithoutEscaping(SILLocation Loc, SILValue Block,
                                 SILValue Closure) {
    return insert(new (getModule()) CopyBlockWithoutEscapingInst(
        getSILDebugLocation(Loc), Block, Closure));
  }

  StrongRetainInst *createStrongRetain(SILLocation Loc, SILValue Operand,
                                       Atomicity atomicity) {
    assert(C.isParsing || !getFunction().hasQualifiedOwnership());
    return insert(new (getModule()) StrongRetainInst(getSILDebugLocation(Loc),
                                                       Operand, atomicity));
  }
  StrongReleaseInst *createStrongRelease(SILLocation Loc, SILValue Operand,
                                         Atomicity atomicity) {
    assert(C.isParsing || !getFunction().hasQualifiedOwnership());
    return insert(new (getModule()) StrongReleaseInst(
        getSILDebugLocation(Loc), Operand, atomicity));
  }

  EndLifetimeInst *createEndLifetime(SILLocation Loc, SILValue Operand) {
    return insert(new (getModule())
                      EndLifetimeInst(getSILDebugLocation(Loc), Operand));
  }

  UncheckedOwnershipConversionInst *
  createUncheckedOwnershipConversion(SILLocation Loc, SILValue Operand,
                                     ValueOwnershipKind Kind) {
    return insert(new (getModule()) UncheckedOwnershipConversionInst(
        getSILDebugLocation(Loc), Operand, Kind));
  }

  FixLifetimeInst *createFixLifetime(SILLocation Loc, SILValue Operand) {
    return insert(new (getModule())
                      FixLifetimeInst(getSILDebugLocation(Loc), Operand));
  }
  void emitFixLifetime(SILLocation Loc, SILValue Operand) {
    if (getTypeLowering(Operand->getType()).isTrivial())
      return;
    createFixLifetime(Loc, Operand);
  }
  ClassifyBridgeObjectInst *createClassifyBridgeObject(SILLocation Loc,
                                                       SILValue value);
  MarkDependenceInst *createMarkDependence(SILLocation Loc, SILValue value,
                                           SILValue base) {
    return insert(new (getModule()) MarkDependenceInst(
        getSILDebugLocation(Loc), value, base));
  }
  IsUniqueInst *createIsUnique(SILLocation Loc, SILValue operand) {
    auto Int1Ty = SILType::getBuiltinIntegerType(1, getASTContext());
    return insert(new (getModule()) IsUniqueInst(getSILDebugLocation(Loc),
                                                   operand, Int1Ty));
  }
  IsEscapingClosureInst *createIsEscapingClosure(SILLocation Loc,
                                                 SILValue operand,
                                                 unsigned VerificationType) {
    auto Int1Ty = SILType::getBuiltinIntegerType(1, getASTContext());
    return insert(new (getModule()) IsEscapingClosureInst(
        getSILDebugLocation(Loc), operand, Int1Ty, VerificationType));
  }

  DeallocStackInst *createDeallocStack(SILLocation Loc, SILValue operand) {
    return insert(new (getModule())
                      DeallocStackInst(getSILDebugLocation(Loc), operand));
  }
  DeallocRefInst *createDeallocRef(SILLocation Loc, SILValue operand,
                                   bool canBeOnStack) {
    return insert(new (getModule()) DeallocRefInst(
        getSILDebugLocation(Loc), operand, canBeOnStack));
  }
  DeallocPartialRefInst *createDeallocPartialRef(SILLocation Loc,
                                                 SILValue operand,
                                                 SILValue metatype) {
    return insert(new (getModule()) DeallocPartialRefInst(
        getSILDebugLocation(Loc), operand, metatype));
  }
  DeallocBoxInst *createDeallocBox(SILLocation Loc,
                                   SILValue operand) {
    return insert(new (getModule()) DeallocBoxInst(
        getSILDebugLocation(Loc), operand));
  }
  DeallocExistentialBoxInst *createDeallocExistentialBox(SILLocation Loc,
                                                         CanType concreteType,
                                                         SILValue operand) {
    return insert(new (getModule()) DeallocExistentialBoxInst(
        getSILDebugLocation(Loc), concreteType, operand));
  }
  DeallocValueBufferInst *createDeallocValueBuffer(SILLocation Loc,
                                                   SILType valueType,
                                                   SILValue operand) {
    return insert(new (getModule()) DeallocValueBufferInst(
        getSILDebugLocation(Loc), valueType, operand));
  }
  DestroyAddrInst *createDestroyAddr(SILLocation Loc, SILValue Operand) {
    return insert(new (getModule())
                      DestroyAddrInst(getSILDebugLocation(Loc), Operand));
  }

  ProjectValueBufferInst *createProjectValueBuffer(SILLocation Loc,
                                                   SILType valueType,
                                                   SILValue operand) {
    return insert(new (getModule()) ProjectValueBufferInst(
        getSILDebugLocation(Loc), valueType, operand));
  }
  ProjectBoxInst *createProjectBox(SILLocation Loc, SILValue boxOperand,
                                   unsigned index) {
    auto boxTy = boxOperand->getType().castTo<SILBoxType>();
    auto fieldTy = boxTy->getFieldType(getModule(), index);

    return insert(new (getModule()) ProjectBoxInst(
        getSILDebugLocation(Loc), boxOperand, index, fieldTy));
  }
  ProjectExistentialBoxInst *createProjectExistentialBox(SILLocation Loc,
                                                         SILType valueTy,
                                                         SILValue boxOperand) {
    return insert(new (getModule()) ProjectExistentialBoxInst(
        getSILDebugLocation(Loc), valueTy, boxOperand));
  }

  //===--------------------------------------------------------------------===//
  // Unchecked cast helpers
  //===--------------------------------------------------------------------===//

  // Create an UncheckedRefCast if the source and dest types are legal,
  // otherwise return null.
  // Unwrap or wrap optional types as needed.
  SingleValueInstruction *tryCreateUncheckedRefCast(SILLocation Loc, SILValue Op,
                                                    SILType ResultTy);

  // Create the appropriate cast instruction based on result type.
  SingleValueInstruction *createUncheckedBitCast(SILLocation Loc, SILValue Op,
                                                 SILType Ty);

  //===--------------------------------------------------------------------===//
  // Runtime failure
  //===--------------------------------------------------------------------===//

  CondFailInst *createCondFail(SILLocation Loc, SILValue Operand,
                               bool Inverted = false) {
    if (Inverted) {
      SILType Ty = Operand->getType();
      SILValue True(createIntegerLiteral(Loc, Ty, 1));
      Operand =
          createBuiltinBinaryFunction(Loc, "xor", Ty, Ty, {Operand, True});
    }
    return insert(new (getModule())
                      CondFailInst(getSILDebugLocation(Loc), Operand));
  }

  BuiltinInst *createBuiltinTrap(SILLocation Loc) {
    ASTContext &AST = getASTContext();
    auto Id_trap = AST.getIdentifier("int_trap");
    return createBuiltin(Loc, Id_trap, getModule().Types.getEmptyTupleType(),
                         {}, {});
  }

  //===--------------------------------------------------------------------===//
  // Array indexing instructions
  //===--------------------------------------------------------------------===//

  IndexAddrInst *createIndexAddr(SILLocation Loc, SILValue Operand,
                                 SILValue Index) {
    return insert(new (getModule()) IndexAddrInst(getSILDebugLocation(Loc),
                                                    Operand, Index));
  }

  TailAddrInst *createTailAddr(SILLocation Loc, SILValue Operand,
                               SILValue Count, SILType ResultTy) {
    return insert(new (getModule()) TailAddrInst(getSILDebugLocation(Loc),
                                                   Operand, Count, ResultTy));
  }

  IndexRawPointerInst *createIndexRawPointer(SILLocation Loc, SILValue Operand,
                                             SILValue Index) {
    return insert(new (getModule()) IndexRawPointerInst(
        getSILDebugLocation(Loc), Operand, Index));
  }

  //===--------------------------------------------------------------------===//
  // Terminator SILInstruction Creation Methods
  //===--------------------------------------------------------------------===//

  UnreachableInst *createUnreachable(SILLocation Loc) {
    return insertTerminator(new (getModule())
                                UnreachableInst(getSILDebugLocation(Loc)));
  }

  ReturnInst *createReturn(SILLocation Loc, SILValue ReturnValue) {
    return insertTerminator(new (getModule()) ReturnInst(
        getSILDebugLocation(Loc), ReturnValue));
  }

  ThrowInst *createThrow(SILLocation Loc, SILValue errorValue) {
    return insertTerminator(
        new (getModule()) ThrowInst(getSILDebugLocation(Loc), errorValue));
  }

  UnwindInst *createUnwind(SILLocation loc) {
    return insertTerminator(
        new (getModule()) UnwindInst(getSILDebugLocation(loc)));
  }

  YieldInst *createYield(SILLocation loc, ArrayRef<SILValue> yieldedValues,
                         SILBasicBlock *resumeBB, SILBasicBlock *unwindBB) {
    return insertTerminator(
        YieldInst::create(getSILDebugLocation(loc), yieldedValues,
                          resumeBB, unwindBB, getFunction()));
  }

  CondBranchInst *
  createCondBranch(SILLocation Loc, SILValue Cond, SILBasicBlock *Target1,
                   SILBasicBlock *Target2,
                   ProfileCounter Target1Count = ProfileCounter(),
                   ProfileCounter Target2Count = ProfileCounter()) {
    return insertTerminator(
        CondBranchInst::create(getSILDebugLocation(Loc), Cond, Target1, Target2,
                               Target1Count, Target2Count, getFunction()));
  }

  CondBranchInst *
  createCondBranch(SILLocation Loc, SILValue Cond, SILBasicBlock *Target1,
                   ArrayRef<SILValue> Args1, SILBasicBlock *Target2,
                   ArrayRef<SILValue> Args2,
                   ProfileCounter Target1Count = ProfileCounter(),
                   ProfileCounter Target2Count = ProfileCounter()) {
    return insertTerminator(
        CondBranchInst::create(getSILDebugLocation(Loc), Cond, Target1, Args1,
                               Target2, Args2, Target1Count, Target2Count, getFunction()));
  }

  CondBranchInst *
  createCondBranch(SILLocation Loc, SILValue Cond, SILBasicBlock *Target1,
                   OperandValueArrayRef Args1, SILBasicBlock *Target2,
                   OperandValueArrayRef Args2,
                   ProfileCounter Target1Count = ProfileCounter(),
                   ProfileCounter Target2Count = ProfileCounter()) {
    SmallVector<SILValue, 6> ArgsCopy1;
    SmallVector<SILValue, 6> ArgsCopy2;

    ArgsCopy1.reserve(Args1.size());
    ArgsCopy2.reserve(Args2.size());

    for (auto I = Args1.begin(), E = Args1.end(); I != E; ++I)
      ArgsCopy1.push_back(*I);
    for (auto I = Args2.begin(), E = Args2.end(); I != E; ++I)
      ArgsCopy2.push_back(*I);

    return insertTerminator(CondBranchInst::create(
        getSILDebugLocation(Loc), Cond, Target1, ArgsCopy1, Target2, ArgsCopy2,
        Target1Count, Target2Count, getFunction()));
  }

  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock) {
    return insertTerminator(
        BranchInst::create(getSILDebugLocation(Loc), TargetBlock, getFunction()));
  }

  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock,
                           ArrayRef<SILValue> Args) {
    return insertTerminator(
        BranchInst::create(getSILDebugLocation(Loc), TargetBlock, Args,
                           getFunction()));
  }

  BranchInst *createBranch(SILLocation Loc, SILBasicBlock *TargetBlock,
                           OperandValueArrayRef Args);

  SwitchValueInst *
  createSwitchValue(SILLocation Loc, SILValue Operand, SILBasicBlock *DefaultBB,
                    ArrayRef<std::pair<SILValue, SILBasicBlock *>> CaseBBs) {
    return insertTerminator(SwitchValueInst::create(
        getSILDebugLocation(Loc), Operand, DefaultBB, CaseBBs, getFunction()));
  }

  SwitchEnumInst *createSwitchEnum(
      SILLocation Loc, SILValue Operand, SILBasicBlock *DefaultBB,
      ArrayRef<std::pair<EnumElementDecl *, SILBasicBlock *>> CaseBBs,
      Optional<ArrayRef<ProfileCounter>> CaseCounts = None,
      ProfileCounter DefaultCount = ProfileCounter()) {
    return insertTerminator(SwitchEnumInst::create(
        getSILDebugLocation(Loc), Operand, DefaultBB, CaseBBs, getFunction(),
        CaseCounts, DefaultCount));
  }

  SwitchEnumAddrInst *createSwitchEnumAddr(
      SILLocation Loc, SILValue Operand, SILBasicBlock *DefaultBB,
      ArrayRef<std::pair<EnumElementDecl *, SILBasicBlock *>> CaseBBs,
      Optional<ArrayRef<ProfileCounter>> CaseCounts = None,
      ProfileCounter DefaultCount = ProfileCounter()) {
    return insertTerminator(SwitchEnumAddrInst::create(
        getSILDebugLocation(Loc), Operand, DefaultBB, CaseBBs, getFunction(),
        CaseCounts, DefaultCount));
  }

  DynamicMethodBranchInst *
  createDynamicMethodBranch(SILLocation Loc, SILValue Operand,
                            SILDeclRef Member, SILBasicBlock *HasMethodBB,
                            SILBasicBlock *NoMethodBB) {
    return insertTerminator(
        DynamicMethodBranchInst::create(getSILDebugLocation(Loc), Operand,
                              Member, HasMethodBB, NoMethodBB, getFunction()));
  }

  CheckedCastBranchInst *
  createCheckedCastBranch(SILLocation Loc, bool isExact, SILValue op,
                          SILType destTy, SILBasicBlock *successBB,
                          SILBasicBlock *failureBB,
                          ProfileCounter Target1Count = ProfileCounter(),
                          ProfileCounter Target2Count = ProfileCounter()) {
    return insertTerminator(CheckedCastBranchInst::create(
        getSILDebugLocation(Loc), isExact, op, destTy, successBB, failureBB,
        getFunction(), C.OpenedArchetypes, Target1Count, Target2Count));
  }

  CheckedCastValueBranchInst *
  createCheckedCastValueBranch(SILLocation Loc, SILValue op, SILType destTy,
                               SILBasicBlock *successBB,
                               SILBasicBlock *failureBB) {
    return insertTerminator(CheckedCastValueBranchInst::create(
        getSILDebugLocation(Loc), op, destTy, successBB, failureBB,
        getFunction(), C.OpenedArchetypes));
  }

  CheckedCastAddrBranchInst *
  createCheckedCastAddrBranch(SILLocation Loc, CastConsumptionKind consumption,
                              SILValue src, CanType sourceType, SILValue dest,
                              CanType targetType, SILBasicBlock *successBB,
                              SILBasicBlock *failureBB,
                              ProfileCounter Target1Count = ProfileCounter(),
                              ProfileCounter Target2Count = ProfileCounter()) {
    return insertTerminator(new (getModule()) CheckedCastAddrBranchInst(
        getSILDebugLocation(Loc), consumption, src, sourceType, dest,
        targetType, successBB, failureBB, Target1Count, Target2Count));
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
    if (F->hasQualifiedOwnership() &&
        v.getOwnershipKind() == ValueOwnershipKind::Any)
      return;
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
    auto type = Operand->getType().getFieldType(Field, getModule());
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

    BB->insert(InsertPt, TheInst);

    C.notifyInserted(TheInst);

// TODO: We really shouldn't be creating instructions unless we are going to
// insert them into a block... This failed in SimplifyCFG.
#ifndef NDEBUG
    TheInst->verifyOperandOwnership();
#endif
  }

  void appendOperandTypeName(SILType OpdTy, llvm::SmallString<16> &Name) {
    if (auto BuiltinIntTy =
            dyn_cast<BuiltinIntegerType>(OpdTy.getASTType())) {
      if (BuiltinIntTy == BuiltinIntegerType::getWordType(getASTContext())) {
        Name += "_Word";
      } else {
        unsigned NumBits = BuiltinIntTy->getWidth().getFixedWidth();
        Name += "_Int" + llvm::utostr(NumBits);
      }
    } else if (auto BuiltinFloatTy =
                   dyn_cast<BuiltinFloatType>(OpdTy.getASTType())) {
      Name += "_FP";
      switch (BuiltinFloatTy->getFPKind()) {
      case BuiltinFloatType::IEEE16: Name += "IEEE16"; break;
      case BuiltinFloatType::IEEE32: Name += "IEEE32"; break;
      case BuiltinFloatType::IEEE64: Name += "IEEE64"; break;
      case BuiltinFloatType::IEEE80: Name += "IEEE80"; break;
      case BuiltinFloatType::IEEE128: Name += "IEEE128"; break;
      case BuiltinFloatType::PPC128: Name += "PPC128"; break;
      }
    } else {
      assert(OpdTy.getASTType() == getASTContext().TheRawPointerType);
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
  /// Build instructions before the given insertion point, inheriting the debug
  /// location.
  ///
  /// Clients should prefer this constructor.
  SILBuilderWithScope(SILInstruction *I, SILBuilderContext &C)
    : SILBuilder(I, I->getDebugScope(), C)
  {}

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

  /// Creates a new SILBuilder with an insertion point at the
  /// beginning of BB and the debug scope from the first
  /// non-metainstruction in the BB.
  explicit SILBuilderWithScope(SILBasicBlock *BB) : SILBuilder(BB->begin()) {
    const SILDebugScope *DS = BB->getScopeOfFirstNonMetaInstruction();
    assert(DS && "Instruction without debug scope associated!");
    setCurrentDebugScope(DS);
  }
};

class SavedInsertionPointRAII {
  SILBuilder &Builder;
  PointerUnion<SILInstruction *, SILBasicBlock *> SavedIP;

public:
  SavedInsertionPointRAII(SILBuilder &B, SILInstruction *NewIP)
      : Builder(B), SavedIP(&*B.getInsertionPoint()) {
    Builder.setInsertionPoint(NewIP);
  }

  SavedInsertionPointRAII(SILBuilder &B, SILBasicBlock *NewIP)
      : Builder(B), SavedIP(B.getInsertionBB()) {
    Builder.setInsertionPoint(NewIP);
  }

  SavedInsertionPointRAII(const SavedInsertionPointRAII &) = delete;
  SavedInsertionPointRAII &operator=(const SavedInsertionPointRAII &) = delete;
  SavedInsertionPointRAII(SavedInsertionPointRAII &&) = delete;
  SavedInsertionPointRAII &operator=(SavedInsertionPointRAII &&) = delete;

  ~SavedInsertionPointRAII() {
    if (SavedIP.isNull()) {
      Builder.clearInsertionPoint();
    } else if (SavedIP.is<SILInstruction *>()) {
      Builder.setInsertionPoint(SavedIP.get<SILInstruction *>());
    } else {
      Builder.setInsertionPoint(SavedIP.get<SILBasicBlock *>());
    }
  }
};

/// Apply a debug location override for the duration of the current scope.
class DebugLocOverrideRAII {
  SILBuilder &Builder;
  Optional<SILLocation> oldOverride;
#ifndef NDEBUG
  Optional<SILLocation> installedOverride;
#endif

public:
  DebugLocOverrideRAII(SILBuilder &B, Optional<SILLocation> Loc) : Builder(B) {
    oldOverride = B.getCurrentDebugLocOverride();
    Builder.applyDebugLocOverride(Loc);
#ifndef NDEBUG
    installedOverride = Loc;
#endif
  }

  ~DebugLocOverrideRAII() {
    assert(Builder.getCurrentDebugLocOverride() == installedOverride &&
           "Restoring debug location override to an unexpected state");
    Builder.applyDebugLocOverride(oldOverride);
  }

  DebugLocOverrideRAII(const DebugLocOverrideRAII &) = delete;
  DebugLocOverrideRAII &operator=(const DebugLocOverrideRAII &) = delete;
  DebugLocOverrideRAII(DebugLocOverrideRAII &&) = delete;
  DebugLocOverrideRAII &operator=(DebugLocOverrideRAII &&) = delete;
};

} // end swift namespace

#endif
