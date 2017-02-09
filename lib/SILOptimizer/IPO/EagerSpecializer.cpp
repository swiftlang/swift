//===--- EagerSpecializer.cpp - Performs Eager Specialization -------------===//
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
///
/// Eager Specializer
/// -----------------
///
/// This transform specializes functions that are annotated with the
/// @_specialize(<type list>) attribute. A function may be annotated with
/// multiple @_specialize attributes, each with a list of concrete types.  For
/// each @_specialize attribute, this transform clones the annotated generic
/// function, creating a new function signature by substituting the concrete
/// types specified in the attribute into the function's generic
/// signature. Dispatch to each specialized function is implemented by inserting
/// call at the beginning of the original generic function guarded by a type
/// check.
///
/// TODO: We have not determined whether to support inexact type checks. It
/// will be a tradeoff between utility of the attribute vs. cost of the check.

#define DEBUG_TYPE "eager-specializer"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Type.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using llvm::dbgs;

// Temporary flag.
llvm::cl::opt<bool> EagerSpecializeFlag(
    "enable-eager-specializer", llvm::cl::init(true),
    llvm::cl::desc("Run the eager-specializer pass."));

/// Returns true if the given return or throw block can be used as a merge point
/// for new return or error values.
static bool isTrivialReturnBlock(SILBasicBlock *RetBB) {
  auto *RetInst = RetBB->getTerminator();
  assert(RetInst->isFunctionExiting() &&
         "expected a properly terminated return or throw block");

  auto RetOperand = RetInst->getOperand(0);

  // Allow:
  //   % = tuple ()
  //   return % : $()
  if (RetOperand->getType().isVoid()) {
    auto *TupleI = dyn_cast<TupleInst>(RetBB->begin());
    if (!TupleI || !TupleI->getType().isVoid())
      return false;

    if (&*std::next(RetBB->begin()) != RetInst)
      return false;

    return RetOperand == TupleI;
  }
  // Allow:
  //   bb(% : $T)
  //   return % : $T
  if (&*RetBB->begin() != RetInst)
    return false;

  if (RetBB->args_size() != 1)
    return false;

  return (RetOperand == RetBB->getArgument(0));
}

/// Adds a CFG edge from the unterminated NewRetBB to a merged "return" or
/// "throw" block. If the return block is not already a canonical merged return
/// block, then split it. If the return type is not Void, add a BBArg that
/// propagates NewRetVal to the return instruction.
static void addReturnValueImpl(SILBasicBlock *RetBB, SILBasicBlock *NewRetBB,
                               SILValue NewRetVal) {
  auto *F = NewRetBB->getParent();

  SILBuilder Builder(*F);
  Builder.setCurrentDebugScope(F->getDebugScope());
  SILLocation Loc = F->getLocation();
  
  auto *RetInst = RetBB->getTerminator();
  assert(RetInst->isFunctionExiting() &&
         "expected a properly terminated return or throw block");
  assert(RetInst->getOperand(0)->getType() == NewRetVal->getType() &&
         "Mismatched return type");
  SILBasicBlock *MergedBB = RetBB;

  // Split the return block if it is nontrivial.
  if (!isTrivialReturnBlock(RetBB)) {
    if (NewRetVal->getType().isVoid()) {
      // Canonicalize Void return type into something that isTrivialReturnBlock
      // expects.
      auto *TupleI = cast<SILInstruction>(RetInst->getOperand(0));
      if (TupleI->hasOneUse()) {
        TupleI->removeFromParent();
        RetBB->insert(RetInst, TupleI);
      } else {
        TupleI = TupleI->clone(RetInst);
        RetInst->setOperand(0, TupleI);
      }
      MergedBB = RetBB->split(TupleI->getIterator());
      Builder.setInsertionPoint(RetBB);
      Builder.createBranch(Loc, MergedBB);
    } else {
      // Forward the existing return argument to a new BBArg.
      MergedBB = RetBB->split(RetInst->getIterator());
      SILValue OldRetVal = RetInst->getOperand(0);
      RetInst->setOperand(
          0, MergedBB->createPHIArgument(OldRetVal->getType(),
                                         ValueOwnershipKind::Owned));
      Builder.setInsertionPoint(RetBB);
      Builder.createBranch(Loc, MergedBB, {OldRetVal});
    }
  }
  // Create a CFG edge from NewRetBB to MergedBB.
  Builder.setInsertionPoint(NewRetBB);
  SmallVector<SILValue, 1> BBArgs;
  if (!NewRetVal->getType().isVoid())
    BBArgs.push_back(NewRetVal);
  Builder.createBranch(Loc, MergedBB, BBArgs);
}

/// Adds a CFG edge from the unterminated NewRetBB to a merged "return" block.
static void addReturnValue(SILBasicBlock *NewRetBB, SILBasicBlock *OldRetBB,
                           SILValue NewRetVal) {
  auto *RetBB = OldRetBB;
  addReturnValueImpl(RetBB, NewRetBB, NewRetVal);
}

/// Adds a CFG edge from the unterminated NewThrowBB to a merged "throw" block.
static void addThrowValue(SILBasicBlock *NewThrowBB, SILValue NewErrorVal) {
  auto *ThrowBB = &*NewThrowBB->getParent()->findThrowBB();
  addReturnValueImpl(ThrowBB, NewThrowBB, NewErrorVal);
}

/// Emits a call to a throwing function as defined by FuncRef, and passes the
/// specified Args. Uses the provided Builder to insert a try_apply at the given
/// SILLocation and generates control flow to handle the rethrow.
///
/// TODO: Move this to Utils.
static SILValue
emitApplyWithRethrow(SILBuilder &Builder,
                     SILLocation Loc,
                     SILValue FuncRef,
                     CanSILFunctionType CanSILFuncTy,
                     SubstitutionList Subs,
                     ArrayRef<SILValue> CallArgs,
                     void (*EmitCleanup)(SILBuilder&, SILLocation)) {

  auto &F = Builder.getFunction();
  SILFunctionConventions fnConv(CanSILFuncTy, Builder.getModule());

  SILBasicBlock *ErrorBB = F.createBasicBlock();
  SILBasicBlock *NormalBB = F.createBasicBlock();

  Builder.createTryApply(Loc,
                         FuncRef,
                         SILType::getPrimitiveObjectType(CanSILFuncTy),
                         Subs,
                         CallArgs,
                         NormalBB,
                         ErrorBB);

  {
    // Emit the rethrow logic.
    Builder.emitBlock(ErrorBB);
    SILValue Error = ErrorBB->createPHIArgument(fnConv.getSILErrorType(),
                                                ValueOwnershipKind::Owned);

    Builder.createBuiltin(Loc,
                          Builder.getASTContext().getIdentifier("willThrow"),
                          Builder.getModule().Types.getEmptyTupleType(),
                          SubstitutionList(),
                          {Error});

    EmitCleanup(Builder, Loc);
    addThrowValue(ErrorBB, Error);
  }
  // Advance Builder to the fall-thru path and return a SILArgument holding the
  // result value.
  Builder.clearInsertionPoint();
  Builder.emitBlock(NormalBB);
  return Builder.getInsertionBB()->createPHIArgument(fnConv.getSILResultType(),
                                                     ValueOwnershipKind::Owned);
}

/// Emits code to invoke the specified specialized CalleeFunc using the
/// provided SILBuilder.
///
/// TODO: Move this to Utils.
static SILValue
emitInvocation(SILBuilder &Builder,
               const ReabstractionInfo &ReInfo,
               SILLocation Loc,
               SILFunction *CalleeFunc,
               ArrayRef<SILValue> CallArgs,
               void (*EmitCleanup)(SILBuilder&, SILLocation)) {

  auto *FuncRefInst = Builder.createFunctionRef(Loc, CalleeFunc);
  auto CanSILFuncTy = CalleeFunc->getLoweredFunctionType();
  auto CalleeSubstFnTy = CanSILFuncTy;
  SubstitutionList Subs;
  if (CanSILFuncTy->isPolymorphic()) {
    // Create a substituted callee type.
    assert(CanSILFuncTy == ReInfo.getSpecializedType() &&
           "Types should be the same");

    // We form here the list of substitutions and the substituted callee
    // type. For specializations with layout constraints, we claim that
    // the substitution T satisfies the specialized requirement
    // 'TS : LayoutConstraint', where LayoutConstraint could be
    // e.g. _Trivial(64). We claim it, because we ensure it by the
    // method how this call is constructed.
    // This is a hack and works currently just by coincidence.
    // But it is not quite true from the SIL type system
    // point of view as we do not really cast at the SIL level the original
    // parameter value of type T into a more specialized generic
    // type 'TS : LayoutConstraint'.
    //
    // TODO: Introduce a proper way to express such a cast.
    // It could be an instruction similar to checked_cast_br, e.g.
    // something like:
    // 'checked_constraint_cast_br %1 : T to $opened("") <TS : _Trivial(64)>',
    // where <TS: _Trivial(64)> introduces a new archetype with the given
    // constraints.
    if (ReInfo.getSpecializedType()->isPolymorphic()) {
      Subs = ReInfo.getCallerParamSubstitutions();
      CalleeSubstFnTy = CanSILFuncTy->substGenericArgs(
          Builder.getModule(), ReInfo.getCallerParamSubstitutions());
      assert(!CalleeSubstFnTy->isPolymorphic() &&
             "Substituted callee type should not be polymorphic");
      assert(!CalleeSubstFnTy->hasTypeParameter() &&
             "Substituted callee type should not have type parameters");
    }
  }

  auto CalleeSILSubstFnTy = SILType::getPrimitiveObjectType(CalleeSubstFnTy);
  SILFunctionConventions fnConv(CalleeSILSubstFnTy.castTo<SILFunctionType>(),
                                Builder.getModule());

  if (!CanSILFuncTy->hasErrorResult()) {
    return Builder.createApply(
        CalleeFunc->getLocation(), FuncRefInst, CalleeSILSubstFnTy,
        fnConv.getSILResultType(), Subs, CallArgs, false);
  }
  return emitApplyWithRethrow(Builder, CalleeFunc->getLocation(),
                              FuncRefInst, CalleeSubstFnTy, Subs,
                              CallArgs,
                              EmitCleanup);
}

/// Returns the thick metatype for the given SILType.
/// e.g. $*T -> $@thick T.Type
static SILType getThickMetatypeType(CanType Ty) {
  auto SwiftTy = CanMetatypeType::get(Ty, MetatypeRepresentation::Thick);
  return SILType::getPrimitiveObjectType(SwiftTy);
}

namespace {
/// Helper class for emitting code to dispatch to a specialized function.
class EagerDispatch {
  SILFunction *GenericFunc;
  const ReabstractionInfo &ReInfo;
  const SILFunctionConventions substConv;

  SILBuilder Builder;
  SILLocation Loc;
  // Function to check if a given object is a class.
  SILFunction *IsClassF;

public:
  // Instantiate a SILBuilder for inserting instructions at the top of the
  // original generic function.
  EagerDispatch(SILFunction *GenericFunc,
                const ReabstractionInfo &ReInfo)
      : GenericFunc(GenericFunc), ReInfo(ReInfo),
        substConv(ReInfo.getSubstitutedType(), GenericFunc->getModule()),
        Builder(*GenericFunc), Loc(GenericFunc->getLocation()) {
    Builder.setCurrentDebugScope(GenericFunc->getDebugScope());
    IsClassF = Builder.getModule().hasFunction(
      "_swift_isClassOrObjCExistentialType", SILLinkage::PublicExternal);
    assert(IsClassF);
  }

  void emitDispatchTo(SILFunction *NewFunc);

protected:
  void emitTypeCheck(SILBasicBlock *FailedTypeCheckBB,
                     SubstitutableType *ParamTy, Type SubTy);

  void emitTrivialAndSizeCheck(SILBasicBlock *FailedTypeCheckBB,
                               SubstitutableType *ParamTy, Type SubTy,
                               LayoutConstraint Layout);

  void emitIsTrivialCheck(SILBasicBlock *FailedTypeCheckBB,
                          SubstitutableType *ParamTy, Type SubTy,
                          LayoutConstraint Layout);

  void emitRefCountedObjectCheck(SILBasicBlock *FailedTypeCheckBB,
                                 SubstitutableType *ParamTy, Type SubTy,
                                 LayoutConstraint Layout);

  void emitLayoutCheck(SILBasicBlock *FailedTypeCheckBB,
                       SubstitutableType *ParamTy, Type SubTy);

  SILValue emitArgumentCast(CanSILFunctionType CalleeSubstFnTy,
                            SILFunctionArgument *OrigArg, unsigned Idx);

  SILValue emitArgumentConversion(SmallVectorImpl<SILValue> &CallArgs);
};
} // end anonymous namespace

/// Inserts type checks in the original generic function for dispatching to the
/// given specialized function. Converts call arguments. Emits an invocation of
/// the specialized function. Handle the return value.
void EagerDispatch::emitDispatchTo(SILFunction *NewFunc) {
  SILBasicBlock *OldReturnBB = &*GenericFunc->findReturnBB();
  // 1. Emit a cascading sequence of type checks blocks.

  // First split the entry BB, moving all instructions to the FailedTypeCheckBB.
  auto &EntryBB = GenericFunc->front();
  SILBasicBlock *FailedTypeCheckBB = EntryBB.split(EntryBB.begin());
  Builder.setInsertionPoint(&EntryBB, EntryBB.begin());

  // Iterate over all dependent types in the generic signature, which will match
  // the specialized attribute's substitution list. Visit only
  // SubstitutableTypes, skipping DependentTypes.
  auto GenericSig =
    GenericFunc->getLoweredFunctionType()->getGenericSignature();
  auto SubIt = ReInfo.getClonerParamSubstitutions().begin();
  auto SubEnd = ReInfo.getClonerParamSubstitutions().end();
  auto DepTypes = GenericSig->getAllDependentTypes();
  for (auto DepTy : DepTypes) {
    assert(SubIt != SubEnd && "Not enough substitutions.");
    if (auto ParamTy = DepTy->getAs<SubstitutableType>()) {
      auto Replacement = SubIt->getReplacement();
      auto GenericEnv = ReInfo.getSpecializedGenericEnvironment();
      if (!Replacement->hasArchetype()) {
        if (GenericEnv)
          Replacement = GenericEnv->mapTypeIntoContext(Replacement);
        assert(!Replacement->hasTypeParameter());
        // Dispatch on concrete type.
        emitTypeCheck(FailedTypeCheckBB, ParamTy, Replacement);
      } else {
        // If Replacement has a layout constraint, then dispatch based
        // on its size and the fact that it is trivial.
        auto LayoutInfo = Replacement->getLayoutConstraint();
        if (LayoutInfo && LayoutInfo->isTrivial()) {
          // Emit a check that it is a trivial type of a certain size.
          emitTrivialAndSizeCheck(FailedTypeCheckBB, ParamTy,
                                  GenericEnv->mapTypeIntoContext(Replacement),
                                  LayoutInfo);
        } else if (LayoutInfo && LayoutInfo->isRefCountedObject()) {
          // Emit a check that it is an object of a reference counted type.
          emitRefCountedObjectCheck(FailedTypeCheckBB, ParamTy,
                                    GenericEnv->mapTypeIntoContext(Replacement),
                                    LayoutInfo);
        }
      }
    }
    ++SubIt;
  }
  assert(SubIt == SubEnd && "Too many substitutions.");
  (void) SubEnd;
  static_cast<void>(FailedTypeCheckBB);

  if (OldReturnBB == &EntryBB) {
    OldReturnBB = FailedTypeCheckBB;
  }

  // 2. Convert call arguments, casting and adjusting for calling convention.

  SmallVector<SILValue, 8> CallArgs;
  SILValue StoreResultTo = emitArgumentConversion(CallArgs);

  // 3. Emit an invocation of the specialized function.

  // Emit any rethrow with no cleanup since all args have been forwarded and
  // nothing has been locally allocated or copied.
  auto NoCleanup = [](SILBuilder&, SILLocation){};
  SILValue Result =
      emitInvocation(Builder, ReInfo, Loc, NewFunc, CallArgs, NoCleanup);

  // 4. Handle the return value.

  auto VoidTy = Builder.getModule().Types.getEmptyTupleType();
  if (StoreResultTo) {
    // Store the direct result to the original result address.
    Builder.createStore(Loc, Result, StoreResultTo,
                        StoreOwnershipQualifier::Unqualified);
    // And return Void.
    Result = Builder.createTuple(Loc, VoidTy, { });
  }
  // Ensure that void return types original from a tuple instruction.
  else if (Result->getType().isVoid())
    Result = Builder.createTuple(Loc, VoidTy, { });

  // Function marked as @NoReturn must be followed by 'unreachable'.
  if (NewFunc->isNoReturnFunction())
    Builder.createUnreachable(Loc);
  else {
    auto resultTy = GenericFunc->getConventions().getSILResultType();
    auto GenResultTy = GenericFunc->mapTypeIntoContext(resultTy);
    auto CastResult = Builder.createUncheckedBitCast(Loc, Result, GenResultTy);
    addReturnValue(Builder.getInsertionBB(), OldReturnBB, CastResult);
  }
}

// Emits a type check in the current block.
// Advances the builder to the successful type check's block.
// 
// Precondition: Builder's current insertion block is not terminated.
//
// Postcondition: Builder's insertion block is a new block that defines the
// specialized call argument and has not been terminated.
//
// The type check is emitted in the current block as: 
// metatype $@thick T.Type
// %a = unchecked_bitwise_cast % to $Builtin.Int64
// metatype $@thick <Specialized>.Type
// %b = unchecked_bitwise_cast % to $Builtin.Int64
// builtin "cmp_eq_Int64"(%a : $Builtin.Int64, %b : $Builtin.Int64)
//   : $Builtin.Int1
// cond_br %
void EagerDispatch::
emitTypeCheck(SILBasicBlock *FailedTypeCheckBB, SubstitutableType *ParamTy,
              Type SubTy) {
  // Instantiate a thick metatype for T.Type
  auto ContextTy = GenericFunc->mapTypeIntoContext(ParamTy);
  auto GenericMT = Builder.createMetatype(
    Loc, getThickMetatypeType(ContextTy->getCanonicalType()));

  // Instantiate a thick metatype for <Specialized>.Type
  auto SpecializedMT = Builder.createMetatype(
    Loc, getThickMetatypeType(SubTy->getCanonicalType()));

  auto &Ctx = Builder.getASTContext();
  auto WordTy = SILType::getBuiltinWordType(Ctx);
  auto GenericMTVal =
    Builder.createUncheckedBitwiseCast(Loc, GenericMT, WordTy);
  auto SpecializedMTVal =
    Builder.createUncheckedBitwiseCast(Loc, SpecializedMT, WordTy);

  auto Cmp =
    Builder.createBuiltinBinaryFunction(Loc, "cmp_eq", WordTy,
                                        SILType::getBuiltinIntegerType(1, Ctx),
                                        {GenericMTVal, SpecializedMTVal});

  auto *SuccessBB = Builder.getFunction().createBasicBlock();
  Builder.createCondBranch(Loc, Cmp, SuccessBB, FailedTypeCheckBB);
  Builder.emitBlock(SuccessBB);
}

void EagerDispatch::emitIsTrivialCheck(SILBasicBlock *FailedTypeCheckBB,
                                       SubstitutableType *ParamTy, Type SubTy,
                                       LayoutConstraint Layout) {
  auto &Ctx = Builder.getASTContext();
  // Instantiate a thick metatype for T.Type
  auto ContextTy = GenericFunc->mapTypeIntoContext(ParamTy);
  auto GenericMT = Builder.createMetatype(
      Loc, getThickMetatypeType(ContextTy->getCanonicalType()));
  auto BoolTy = SILType::getBuiltinIntegerType(1, Ctx);
  Substitution Sub(ContextTy, {});

  // Emit a check that it is a pod object.
  auto IsPOD = Builder.createBuiltin(Loc, Ctx.getIdentifier("ispod"), BoolTy,
                                     Sub, {GenericMT});
  auto *SuccessBB = Builder.getFunction().createBasicBlock();
  Builder.createCondBranch(Loc, IsPOD, SuccessBB, FailedTypeCheckBB);
  Builder.emitBlock(SuccessBB);
}

void EagerDispatch::emitTrivialAndSizeCheck(SILBasicBlock *FailedTypeCheckBB,
                                            SubstitutableType *ParamTy,
                                            Type SubTy,
                                            LayoutConstraint Layout) {
  if (Layout->isAddressOnlyTrivial()) {
    emitIsTrivialCheck(FailedTypeCheckBB, ParamTy, SubTy, Layout);
    return;
  }
  auto &Ctx = Builder.getASTContext();
  // Instantiate a thick metatype for T.Type
  auto ContextTy = GenericFunc->mapTypeIntoContext(ParamTy);
  auto GenericMT = Builder.createMetatype(
    Loc, getThickMetatypeType(ContextTy->getCanonicalType()));

  auto WordTy = SILType::getBuiltinWordType(Ctx);
  auto BoolTy = SILType::getBuiltinIntegerType(1, Ctx);
  Substitution Sub(ContextTy, {});
  auto ParamSize = Builder.createBuiltin(Loc, Ctx.getIdentifier("sizeof"),
                                         WordTy, Sub, { GenericMT });
  auto LayoutSize =
      Builder.createIntegerLiteral(Loc, WordTy, Layout->getTrivialSizeInBytes());
  const char *CmpOpName = Layout->isFixedSizeTrivial() ? "cmp_eq" : "cmp_le";
  auto Cmp =
    Builder.createBuiltinBinaryFunction(Loc, CmpOpName, WordTy,
                                        BoolTy,
                                        {ParamSize, LayoutSize});

  auto *SuccessBB1 = Builder.getFunction().createBasicBlock();
  Builder.createCondBranch(Loc, Cmp, SuccessBB1, FailedTypeCheckBB);
  Builder.emitBlock(SuccessBB1);
  // Emit a check that it is a pod object.
  // TODO: Perform this check before all the fixed size checks!
  auto IsPOD = Builder.createBuiltin(Loc, Ctx.getIdentifier("ispod"),
                                         BoolTy, Sub, { GenericMT });
  auto *SuccessBB2 = Builder.getFunction().createBasicBlock();
  Builder.createCondBranch(Loc, IsPOD, SuccessBB2, FailedTypeCheckBB);
  Builder.emitBlock(SuccessBB2);
}

void EagerDispatch::emitRefCountedObjectCheck(SILBasicBlock *FailedTypeCheckBB,
                                              SubstitutableType *ParamTy,
                                              Type SubTy,
                                              LayoutConstraint Layout) {
  auto &Ctx = Builder.getASTContext();
  // Instantiate a thick metatype for T.Type
  auto ContextTy = GenericFunc->mapTypeIntoContext(ParamTy);
  auto GenericMT = Builder.createMetatype(
    Loc, getThickMetatypeType(ContextTy->getCanonicalType()));

  auto Int8Ty = SILType::getBuiltinIntegerType(8, Ctx);
  auto BoolTy = SILType::getBuiltinIntegerType(1, Ctx);
  Substitution Sub(ContextTy, {});

  // Emit a check that it is a reference-counted object.
  // TODO: Perform this check before all fixed size checks.
  // FIXME: What builtin do we use to check it????
  auto CanBeClass = Builder.createBuiltin(
      Loc, Ctx.getIdentifier("canBeClass"), Int8Ty, Sub, {GenericMT});
  auto ClassConst =
      Builder.createIntegerLiteral(Loc, Int8Ty, 1);
  auto Cmp1 =
    Builder.createBuiltinBinaryFunction(Loc, "cmp_eq", Int8Ty,
                                        BoolTy,
                                        {CanBeClass, ClassConst});

  auto *SuccessBB = Builder.getFunction().createBasicBlock();
  auto *MayBeCalssCheckBB = Builder.getFunction().createBasicBlock();
  Builder.createCondBranch(Loc, Cmp1, SuccessBB,
                           MayBeCalssCheckBB);

  Builder.emitBlock(MayBeCalssCheckBB);

  auto MayBeClassConst =
      Builder.createIntegerLiteral(Loc, Int8Ty, 2);
  auto Cmp2 =
    Builder.createBuiltinBinaryFunction(Loc, "cmp_eq", Int8Ty,
                                        BoolTy,
                                        {CanBeClass, MayBeClassConst});

  auto *IsClassCheckBB = Builder.getFunction().createBasicBlock();
  Builder.createCondBranch(Loc, Cmp2, IsClassCheckBB,
                           FailedTypeCheckBB);

  Builder.emitBlock(IsClassCheckBB);

  auto *FRI = Builder.createFunctionRef(Loc, IsClassF);
  auto CanFnTy = IsClassF->getLoweredFunctionType()->substGenericArgs(
      Builder.getModule(), {Sub});
  auto SILFnTy = SILType::getPrimitiveObjectType(CanFnTy);
  SILFunctionConventions fnConv(CanFnTy, Builder.getModule());
  auto SILResultTy = fnConv.getSILResultType();
  auto IsClassRuntimeCheck =
      Builder.createApply(Loc, FRI, SILFnTy, SILResultTy, {Sub}, {GenericMT},
                          /* isNonThrowing */ false);
  // Extract the i1 from the Bool struct.
  StructDecl *BoolStruct = cast<StructDecl>(Ctx.getBoolDecl());
  auto Members = BoolStruct->lookupDirect(Ctx.Id_value_);
  assert(Members.size() == 1 &&
         "Bool should have only one property with name '_value'");
  auto Member = dyn_cast<VarDecl>(Members[0]);
  assert(Member &&"Bool should have a property with name '_value' of type Int1");
  auto BoolValue =
      Builder.emitStructExtract(Loc, IsClassRuntimeCheck, Member, BoolTy);
  Builder.createCondBranch(Loc, BoolValue, SuccessBB, FailedTypeCheckBB);

  Builder.emitBlock(SuccessBB);
}

/// Cast a generic argument to its specialized type.
SILValue EagerDispatch::emitArgumentCast(CanSILFunctionType CalleeSubstFnTy,
                                         SILFunctionArgument *OrigArg,
                                         unsigned Idx) {
  SILFunctionConventions substConv(CalleeSubstFnTy,
                                   Builder.getModule());
  auto CastTy = substConv.getSILArgumentType(Idx);
  assert(CastTy.isAddress()
             == (OrigArg->isIndirectResult()
                 || substConv.isSILIndirect(OrigArg->getKnownParameterInfo()))
         && "bad arg type");

  if (CastTy.isAddress())
    return Builder.createUncheckedAddrCast(Loc, OrigArg, CastTy);

  return Builder.createUncheckedBitCast(Loc, OrigArg, CastTy);
}

/// Converts each generic function argument into a SILValue that can be passed
/// to the specialized call by emitting a cast followed by a load.
///
/// Populates the CallArgs with the converted arguments.
///
/// Returns the SILValue to store the result into if the specialized function
/// has a direct result.
SILValue EagerDispatch::
emitArgumentConversion(SmallVectorImpl<SILValue> &CallArgs) {
  auto OrigArgs = GenericFunc->begin()->getFunctionArguments();
  assert(OrigArgs.size() == substConv.getNumSILArguments()
         && "signature mismatch");
  // Create a substituted callee type.
  auto SubstitutedType = ReInfo.getSubstitutedType();
  auto SpecializedType = ReInfo.getSpecializedType();
  auto CanSILFuncTy = SubstitutedType;
  auto CalleeSubstFnTy = CanSILFuncTy;
  if (CanSILFuncTy->isPolymorphic()) {
    CalleeSubstFnTy = CanSILFuncTy->substGenericArgs(
        Builder.getModule(), ReInfo.getCallerParamSubstitutions());
    assert(!CalleeSubstFnTy->isPolymorphic() &&
           "Substituted callee type should not be polymorphic");
    assert(!CalleeSubstFnTy->hasTypeParameter() &&
           "Substituted callee type should not have type parameters");

    SubstitutedType = CalleeSubstFnTy;
    SpecializedType =
        ReInfo.createSpecializedType(SubstitutedType, Builder.getModule());
  }

  assert(OrigArgs.size() == ReInfo.getNumArguments() && "signature mismatch");

  CallArgs.reserve(OrigArgs.size());
  SILValue StoreResultTo;
  for (auto *OrigArg : OrigArgs) {
    unsigned ArgIdx = OrigArg->getIndex();

    auto CastArg = emitArgumentCast(SubstitutedType, OrigArg, ArgIdx);
    DEBUG(dbgs() << "  Cast generic arg: "; CastArg->print(dbgs()));

    if (!substConv.useLoweredAddresses()) {
      CallArgs.push_back(CastArg);
      continue;
    }
    if (ArgIdx < substConv.getSILArgIndexOfFirstParam()) {
      // Handle result arguments.
      unsigned formalIdx =
          substConv.getIndirectFormalResultIndexForSILArg(ArgIdx);
      if (ReInfo.isFormalResultConverted(formalIdx)) {
        // The result is converted from indirect to direct. We need to insert
        // a store later.
        assert(!StoreResultTo);
        StoreResultTo = CastArg;
        continue;
      }
    } else {
      // Handle arguments for formal parameters.
      unsigned paramIdx = ArgIdx - substConv.getSILArgIndexOfFirstParam();
      if (ReInfo.isParamConverted(paramIdx)) {
        // An argument is converted from indirect to direct. Instead of the
        // address we pass the loaded value.
        // FIXME: If type of CastArg is an archetype, but it is loadable because
        // of a layout constraint on the caller side, we have a problem here 
        // We need to load the value on the caller side, but this archetype is
        // not statically known to be loadable on the caller side (though we
        // have proven dynamically that it has a fixed size).
        // We can try to load it as an int value of width N, but then it is not
        // clear how to convert it into a value of the archetype type, which is
        // expected. May be we should pass it as @in parameter and make it
        // loadable on the caller's side?
        SILValue Val = Builder.createLoad(Loc, CastArg,
                                          LoadOwnershipQualifier::Unqualified);
        CallArgs.push_back(Val);
        continue;
      }
    }
    CallArgs.push_back(CastArg);
  }
  return StoreResultTo;
}

namespace {
// FIXME: This should be a function transform that pushes cloned functions on
// the pass manager worklist.
class EagerSpecializerTransform : public SILModuleTransform {
public:
  EagerSpecializerTransform() {}

  void run() override;

  StringRef getName() override { return "Eager Specializer"; }
};
} // end anonymous namespace

/// Specializes a generic function for a concrete type list.
static SILFunction *eagerSpecialize(SILFunction *GenericFunc,
                                    const SILSpecializeAttr &SA,
                                    const ReabstractionInfo &ReInfo) {
  DEBUG(dbgs() << "Specializing " << GenericFunc->getName() << "\n");

  DEBUG(auto FT = GenericFunc->getLoweredFunctionType();
        dbgs() << "  Generic Sig:";
        dbgs().indent(2); FT->getGenericSignature()->print(dbgs());
        dbgs() << "\n  Substituting: <";
        auto depTypes = FT->getGenericSignature()->getAllDependentTypes();
        interleave(depTypes.begin(), depTypes.end(),
                   [&](Type t){
                     GenericFunc->mapTypeIntoContext(t).print(dbgs()); },
                   []{ dbgs() << ", "; });
        dbgs() << "> with ";
        SA.print(dbgs()); dbgs() << "\n");

  GenericFuncSpecializer
        FuncSpecializer(GenericFunc, ReInfo.getClonerParamSubstitutions(),
                        GenericFunc->isFragile(), ReInfo);

  SILFunction *NewFunc = FuncSpecializer.trySpecialization();
  if (!NewFunc)
    DEBUG(dbgs() << "  Failed. Cannot specialize function.\n");
  return NewFunc;
}

/// Run the pass.
void EagerSpecializerTransform::run() {
  if (!EagerSpecializeFlag)
    return;

  // Process functions in any order.
  bool Changed = false;
  for (auto &F : *getModule()) {
    if (!F.shouldOptimize()) {
      DEBUG(dbgs() << "  Cannot specialize function " << F.getName()
                   << " marked to be excluded from optimizations.\n");
      continue;
    }
    // Only specialize functions in their home module.
    if (F.isExternalDeclaration() || F.isAvailableExternally())
      continue;

    if (!F.getLoweredFunctionType()->getGenericSignature())
      continue;

    // Create a specialized function with ReabstractionInfo for each attribute.
    SmallVector<SILFunction *, 8> SpecializedFuncs;
    SmallVector<ReabstractionInfo, 4> ReInfoVec;
    ReInfoVec.reserve(F.getSpecializeAttrs().size());

    // TODO: Use a decision-tree to reduce the amount of dynamic checks being
    // performed.
    for (auto *SA : F.getSpecializeAttrs()) {
      auto AttrRequirements = SA->getRequirements();
      ReInfoVec.emplace_back(&F, AttrRequirements);
      auto *NewFunc = eagerSpecialize(&F, *SA, ReInfoVec.back());

      SpecializedFuncs.push_back(NewFunc);

      if (SA->isExported()) {
        NewFunc->setKeepAsPublic(true);
        continue;
      }
    }

    // TODO: Optimize the dispatch code to minimize the amount
    // of checks. Use decision trees for this purpose.
    for_each3(F.getSpecializeAttrs(), SpecializedFuncs, ReInfoVec,
              [&](const SILSpecializeAttr *SA, SILFunction *NewFunc,
                  const ReabstractionInfo &ReInfo) {
                if (NewFunc) {
                  Changed = true;
                  EagerDispatch(&F, ReInfo).emitDispatchTo(NewFunc);
                }
              });
    // As specializations are created, the attributes should be removed.
    F.clearSpecializeAttrs();
  }
  // Invalidate everything since we delete calls as well as add new
  // calls and branches.
  if (Changed) {
    invalidateAnalysis(SILAnalysis::InvalidationKind::Everything);
  }
}

SILTransform *swift::createEagerSpecializer() {
  return new EagerSpecializerTransform();
}
