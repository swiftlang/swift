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
static void addReturnValue(SILBasicBlock *NewRetBB, SILValue NewRetVal) {
  auto *RetBB = &*NewRetBB->getParent()->findReturnBB();
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
                     ArrayRef<SILValue> CallArgs,
                     void (*EmitCleanup)(SILBuilder&, SILLocation)) {

  auto &F = Builder.getFunction();
  SILFunctionConventions fnConv(CanSILFuncTy, Builder.getModule());

  SILBasicBlock *ErrorBB = F.createBasicBlock();
  SILBasicBlock *NormalBB = F.createBasicBlock();
  Builder.createTryApply(Loc,
                         FuncRef,
                         SILType::getPrimitiveObjectType(CanSILFuncTy),
                         SubstitutionList(),
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

/// Emits code to invoke the specified nonpolymorphic CalleeFunc using the
/// provided SILBuilder.
/// 
/// TODO: Move this to Utils.
static SILValue
emitInvocation(SILBuilder &Builder, SILLocation Loc,
               SILFunction *CalleeFunc,
               ArrayRef<SILValue> CallArgs,
               void (*EmitCleanup)(SILBuilder&, SILLocation)) {

  auto *FuncRefInst = Builder.createFunctionRef(Loc, CalleeFunc);
  auto CanSILFuncTy = CalleeFunc->getLoweredFunctionType();
  assert(!CanSILFuncTy->isPolymorphic());

  if (!CanSILFuncTy->hasErrorResult()) {
    assert(!CanSILFuncTy->isPolymorphic());
    return Builder.createApply(CalleeFunc->getLocation(), FuncRefInst, CallArgs,
                               false);
  }
  return emitApplyWithRethrow(Builder, CalleeFunc->getLocation(), FuncRefInst,
                              CanSILFuncTy, CallArgs, EmitCleanup);
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
#if 0
  const SILSpecializeAttr &SA;
#endif
  const ReabstractionInfo &ReInfo;
  const SILFunctionConventions substConv;

  SILBuilder Builder;
  SILLocation Loc;

public:
  // Instantiate a SILBuilder for inserting instructions at the top of the
  // original generic function.
  EagerDispatch(SILFunction *GenericFunc, const SILSpecializeAttr &SA,
                const ReabstractionInfo &ReInfo)
      : GenericFunc(GenericFunc), ReInfo(ReInfo),
        substConv(ReInfo.getSubstitutedType(), GenericFunc->getModule()),
        Builder(*GenericFunc), Loc(GenericFunc->getLocation()) {

    Builder.setCurrentDebugScope(GenericFunc->getDebugScope());
  }

  void emitDispatchTo(SILFunction *NewFunc);

protected:
  void emitTypeCheck(SILBasicBlock *FailedTypeCheckBB,
                     SubstitutableType *ParamTy, Type SubTy);

  SILValue emitArgumentCast(SILFunctionArgument *OrigArg, unsigned Idx);

  SILValue emitArgumentConversion(SmallVectorImpl<SILValue> &CallArgs);
};
} // end anonymous namespace

/// Inserts type checks in the original generic function for dispatching to the
/// given specialized function. Converts call arguments. Emits an invocation of
/// the specialized function. Handle the return value.
void EagerDispatch::emitDispatchTo(SILFunction *NewFunc) {

  // 1. Emit a cascading sequence of type checks blocks.

  // First split the entry BB, moving all instructions to the FailedTypeCheckBB.
  auto &EntryBB = GenericFunc->front();
  SILBasicBlock *FailedTypeCheckBB = EntryBB.split(EntryBB.begin());
  Builder.setInsertionPoint(&EntryBB, EntryBB.begin());

  // Iterate over all dependent types in the generic signature, which will match
  // the specialized attribute's substitution list. Visit only
  // SubstitutableTypes, skipping DependentTypes.
  // TODO: Uncomment when Generics.cpp is updated to use the
  // new @_specialize attribute for partial specializations.
#if 0
  auto GenericSig =
    GenericFunc->getLoweredFunctionType()->getGenericSignature();

  auto SubIt = SA.getSubstitutions().begin();
  auto SubEnd = SA.getSubstitutions().end();
  for (auto DepTy : GenericSig->getAllDependentTypes()) {
    assert(SubIt != SubEnd && "Not enough substitutions.");
    
    if (auto ParamTy = DepTy->getAs<SubstitutableType>())
      emitTypeCheck(FailedTypeCheckBB, ParamTy, SubIt->getReplacement());
    ++SubIt;
  }
  assert(SubIt == SubEnd && "Too many substitutions.");
  (void) SubEnd;
#else
  static_cast<void>(FailedTypeCheckBB);
#endif
  // 2. Convert call arguments, casting and adjusting for calling convention.

  SmallVector<SILValue, 8> CallArgs;
  SILValue StoreResultTo = emitArgumentConversion(CallArgs);

  // 3. Emit an invocation of the specialized function.

  // Emit any rethrow with no cleanup since all args have been forwarded and
  // nothing has been locally allocated or copied.
  auto NoCleanup = [](SILBuilder&, SILLocation){};
  SILValue Result = emitInvocation(Builder, Loc, NewFunc, CallArgs, NoCleanup);

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
    addReturnValue(Builder.getInsertionBB(), CastResult);
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

/// Cast a generic argument to its specialized type.
SILValue EagerDispatch::emitArgumentCast(SILFunctionArgument *OrigArg,
                                         unsigned Idx) {
  SILFunctionConventions substConv(ReInfo.getSubstitutedType(),
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

  CallArgs.reserve(OrigArgs.size());
  SILValue StoreResultTo;
  for (auto *OrigArg : OrigArgs) {
    unsigned ArgIdx = OrigArg->getIndex();

    auto CastArg = emitArgumentCast(OrigArg, ArgIdx);
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

    // TODO: Uncomment when Generics.cpp is updated to use the
    // new @_specialize attribute for partial specializations.
#if 0
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
  
  // Create a specialized function.
  // TODO: Uncomment when Generics.cpp is updated to use the
  // new @_specialize attribute for partial specializations.
#if 0
  GenericFuncSpecializer
        FuncSpecializer(GenericFunc, SA.getSubstitutions(),
                        GenericFunc->isFragile(), ReInfo);

  SILFunction *NewFunc = FuncSpecializer.trySpecialization();
  if (!NewFunc)
    DEBUG(dbgs() << "  Failed. Cannot specialize function.\n");
  return NewFunc;
#else
  return nullptr;
#endif
}
#endif

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

    // Create a specialized function with ReabstractionInfo for each attribute.
    SmallVector<SILFunction*, 8> SpecializedFuncs;
    SmallVector<ReabstractionInfo, 4> ReInfoVec;
    ReInfoVec.reserve(F.getSpecializeAttrs().size());

    // TODO: Uncomment when Generics.cpp is updated to use the
    // new @_specialize attribute for partial specializations.
#if 0
    for (auto *SA : F.getSpecializeAttrs()) {
      ReInfoVec.emplace_back(&F, SA->getSubstitutions());
      auto *NewFunc = eagerSpecialize(&F, *SA, ReInfoVec.back());
      SpecializedFuncs.push_back(NewFunc);
    }

    // Emit a type check and dispatch to each specialized function.
    for_each3(F.getSpecializeAttrs(), SpecializedFuncs, ReInfoVec,
             [&](const SILSpecializeAttr *SA, SILFunction *NewFunc,
                 const ReabstractionInfo &ReInfo) {
      if (NewFunc) {
        Changed = true;
        EagerDispatch(&F, *SA, ReInfo).emitDispatchTo(NewFunc);
      }
    });
#endif
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
