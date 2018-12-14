//===--- SILCombinerApplyVisitors.cpp -------------------------------------===//
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

#define DEBUG_TYPE "sil-combine"
#include "SILCombiner.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/CFG.h"
#include "swift/SILOptimizer/Analysis/ValueTracking.h"
#include "swift/SILOptimizer/Utils/Existential.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;
using namespace swift::PatternMatch;

/// Remove pointless reabstraction thunk closures.
///   partial_apply %reabstraction_thunk_typeAtoB(
///      partial_apply %reabstraction_thunk_typeBtoA %closure_typeB))
///   ->
///   %closure_typeB
static bool foldInverseReabstractionThunks(PartialApplyInst *PAI,
                                           SILCombiner *Combiner) {
  auto PAIArg = isPartialApplyOfReabstractionThunk(PAI);
  if (!PAIArg)
    return false;

  auto *PAI2 = dyn_cast<PartialApplyInst>(PAIArg);
  if (!PAI2)
    return false;

  if (!hasOneNonDebugUse(PAI2))
    return false;

  auto PAI2Arg = isPartialApplyOfReabstractionThunk(PAI2);
  if (!PAI2Arg)
    return false;

  // The types must match.
  if (PAI->getType() != PAI2->getArgument(0)->getType())
    return false;

  // Replace the partial_apply(partial_apply(X)) by X and remove the
  // partial_applies.

  Combiner->replaceInstUsesWith(*PAI, PAI2->getArgument(0));
  Combiner->eraseInstFromFunction(*PAI);
  assert(onlyHaveDebugUses(PAI2) && "Should not have any uses");
  Combiner->eraseInstFromFunction(*PAI2);

  return true;
}

SILInstruction *SILCombiner::visitPartialApplyInst(PartialApplyInst *PAI) {
  // partial_apply without any substitutions or arguments is just a
  // thin_to_thick_function.
  if (!PAI->hasSubstitutions() && (PAI->getNumArguments() == 0))
    return Builder.createThinToThickFunction(PAI->getLoc(), PAI->getCallee(),
                                             PAI->getType());

  // partial_apply %reabstraction_thunk_typeAtoB(
  //    partial_apply %reabstraction_thunk_typeBtoA %closure_typeB))
  // -> %closure_typeB
  if (foldInverseReabstractionThunks(PAI, this))
    return nullptr;

  tryOptimizeApplyOfPartialApply(PAI);

  // Try to delete dead closures.
  tryDeleteDeadClosure(
      PAI, InstModCallbacks(
               [this](SILInstruction *DeadInst) {
                 eraseInstFromFunction(*DeadInst);
               },
               [this](SILInstruction *NewInst) { Worklist.add(NewInst); }));
  return nullptr;
}

// Helper class performing the apply{partial_apply(x,y)}(z) -> apply(z,x,y)
// peephole.
class PartialApplyCombiner {
  // True if temporaries are not created yet.
  bool isFirstTime = true;

  // partial_apply which is being processed.
  PartialApplyInst *PAI;

  // Temporaries created as copies of alloc_stack arguments of
  // the partial_apply.
  SmallVector<SILValue, 8> Tmps;

  // Mapping from the original argument of partial_apply to
  // the temporary containing its copy.
  llvm::DenseMap<SILValue, SILValue> ArgToTmp;

  // Set of lifetime endpoints for this partial_apply.
  //
  // Used to find the last uses of partial_apply, which is need to insert
  // releases/destroys of temporaries as early as possible.
  ValueLifetimeAnalysis::Frontier PAFrontier;

  SILBuilder &Builder;

  SILCombiner *SilCombiner;

  bool processSingleApply(FullApplySite AI);
  bool allocateTemporaries();
  void deallocateTemporaries();
  void releaseTemporaries();

public:
  PartialApplyCombiner(PartialApplyInst *PAI, SILBuilder &Builder,
                       SILCombiner *SilCombiner)
      : isFirstTime(true), PAI(PAI), Builder(Builder),
        SilCombiner(SilCombiner) {}
  SILInstruction *combine();
};

/// Returns true on success.
bool PartialApplyCombiner::allocateTemporaries() {
  // Copy the original arguments of the partial_apply into newly created
  // temporaries and use these temporaries instead of the original arguments
  // afterwards.
  //
  // This is done to "extend" the life-time of original partial_apply arguments,
  // as they may be destroyed/deallocated before the last use by one of the
  // apply instructions.
  //
  // TODO: Copy arguments of the partial_apply into new temporaries only if the
  // lifetime of arguments ends before their uses by apply instructions.
  bool needsReleases = false;
  CanSILFunctionType PAITy =
    PAI->getCallee()->getType().getAs<SILFunctionType>();

  // Emit a destroy value for each captured closure argument.
  ArrayRef<SILParameterInfo> Params = PAITy->getParameters();
  auto Args = PAI->getArguments();
  Params = Params.drop_front(Params.size() - Args.size());

  llvm::SmallVector<std::pair<SILValue, uint16_t>, 8> ArgsToHandle;
  for (unsigned i : indices(Args)) {
    SILValue Arg = Args[i];
    SILParameterInfo Param = Params[i];
    if (Param.isIndirectMutating())
      continue;

    // Create a temporary and copy the argument into it, if:
    // - the argument stems from an alloc_stack
    // - the argument is consumed by the callee and is indirect
    //   (e.g. it is an @in argument)
    if (isa<AllocStackInst>(Arg)
        || (Param.isConsumed()
            && PAI->getSubstCalleeConv().isSILIndirect(Param))) {
      // If the argument has a dependent type, then we can not create a
      // temporary for it at the beginning of the function, so we must bail.
      //
      // TODO: This is because we are inserting alloc_stack at the beginning/end
      // of functions where the dependent type may not exist yet.
      if (Arg->getType().hasOpenedExistential())
        return false;

      // If the temporary is non-trivial, we need to release it later.
      if (!Arg->getType().isTrivial(PAI->getModule()))
        needsReleases = true;
      ArgsToHandle.push_back(std::make_pair(Arg, i));
    }
  }

  if (needsReleases) {
    // Compute the set of endpoints, which will be used to insert releases of
    // temporaries. This may fail if the frontier is located on a critical edge
    // which we may not split (no CFG changes in SILCombine).
    ValueLifetimeAnalysis VLA(PAI);
    if (!VLA.computeFrontier(PAFrontier, ValueLifetimeAnalysis::DontModifyCFG))
      return false;
  }

  for (auto ArgWithIdx : ArgsToHandle) {
    SILValue Arg = ArgWithIdx.first;
    Builder.setInsertionPoint(PAI->getFunction()->begin()->begin());
    // Create a new temporary at the beginning of a function.
    SILDebugVariable DbgVar(/*Constant*/ true, ArgWithIdx.second);
    auto *Tmp = Builder.createAllocStack(PAI->getLoc(), Arg->getType(), DbgVar);
    Builder.setInsertionPoint(PAI);
    // Copy argument into this temporary.
    Builder.createCopyAddr(PAI->getLoc(), Arg, Tmp,
                           IsTake_t::IsNotTake,
                           IsInitialization_t::IsInitialization);

    Tmps.push_back(Tmp);
    ArgToTmp.insert(std::make_pair(Arg, Tmp));
  }
  return true;
}

/// Emit dealloc_stack for all temporaries.
void PartialApplyCombiner::deallocateTemporaries() {
  // Insert dealloc_stack instructions at all function exit points.
  for (SILBasicBlock &BB : *PAI->getFunction()) {
    TermInst *Term = BB.getTerminator();
    if (!Term->isFunctionExiting())
      continue;

    for (auto Op : Tmps) {
      Builder.setInsertionPoint(Term);
      Builder.createDeallocStack(PAI->getLoc(), Op);
    }
  }
}

/// Emit code to release/destroy temporaries.
void PartialApplyCombiner::releaseTemporaries() {
  // Insert releases and destroy_addrs as early as possible,
  // because we don't want to keep objects alive longer than
  // its really needed.
  for (auto Op : Tmps) {
    auto TmpType = Op->getType().getObjectType();
    if (TmpType.isTrivial(PAI->getModule()))
      continue;
    for (auto *EndPoint : PAFrontier) {
      Builder.setInsertionPoint(EndPoint);
      if (!TmpType.isAddressOnly(PAI->getModule())) {
        auto *Load = Builder.createLoad(PAI->getLoc(), Op,
                                        LoadOwnershipQualifier::Unqualified);
        Builder.createReleaseValue(PAI->getLoc(), Load, Builder.getDefaultAtomicity());
      } else {
        Builder.createDestroyAddr(PAI->getLoc(), Op);
      }
    }
  }
}

/// Process an apply instruction which uses a partial_apply
/// as its callee.
/// Returns true on success.
bool PartialApplyCombiner::processSingleApply(FullApplySite AI) {
  Builder.setInsertionPoint(AI.getInstruction());
  Builder.setCurrentDebugScope(AI.getDebugScope());

  // Prepare the args.
  SmallVector<SILValue, 8> Args;
  // First the ApplyInst args.
  for (auto Op : AI.getArguments())
    Args.push_back(Op);

  SILInstruction *InsertionPoint = &*Builder.getInsertionPoint();
  // Next, the partial apply args.

  // Pre-process partial_apply arguments only once, lazily.
  if (isFirstTime) {
    isFirstTime = false;
    if (!allocateTemporaries())
      return false;
  }

  // Now, copy over the partial apply args.
  for (auto Op : PAI->getArguments()) {
    auto Arg = Op;
    // If there is new temporary for this argument, use it instead.
    if (ArgToTmp.count(Arg)) {
      Op = ArgToTmp.lookup(Arg);
    }
    Args.push_back(Op);
  }

  Builder.setInsertionPoint(InsertionPoint);
  Builder.setCurrentDebugScope(AI.getDebugScope());

  // The thunk that implements the partial apply calls the closure function
  // that expects all arguments to be consumed by the function. However, the
  // captured arguments are not arguments of *this* apply, so they are not
  // pre-incremented. When we combine the partial_apply and this apply into
  // a new apply we need to retain all of the closure non-address type
  // arguments.
  auto ParamInfo = PAI->getSubstCalleeType()->getParameters();
  auto PartialApplyArgs = PAI->getArguments();
  // Set of arguments that need to be released after each invocation.
  SmallVector<SILValue, 8> ToBeReleasedArgs;
  for (unsigned i = 0, e = PartialApplyArgs.size(); i < e; ++i) {
    SILValue Arg = PartialApplyArgs[i];
    if (!Arg->getType().isAddress()) {
      // Retain the argument as the callee may consume it.
      Arg = Builder.emitCopyValueOperation(PAI->getLoc(), Arg);
      // For non consumed parameters (e.g. guaranteed), we also need to
      // insert releases after each apply instruction that we create.
      if (!ParamInfo[ParamInfo.size() - PartialApplyArgs.size() + i].
            isConsumed())
        ToBeReleasedArgs.push_back(Arg);
    }
  }

  auto Callee = PAI->getCallee();
  SubstitutionMap Subs = PAI->getSubstitutionMap();

  // The partial_apply might be substituting in an open existential type.
  Builder.addOpenedArchetypeOperands(PAI);

  FullApplySite NAI;
  if (auto *TAI = dyn_cast<TryApplyInst>(AI))
    NAI = Builder.createTryApply(AI.getLoc(), Callee, Subs, Args,
                                 TAI->getNormalBB(), TAI->getErrorBB());
  else
    NAI = Builder.createApply(AI.getLoc(), Callee, Subs, Args,
                              cast<ApplyInst>(AI)->isNonThrowing());

  // We also need to release the partial_apply instruction itself because it
  // is consumed by the apply_instruction.
  if (auto *TAI = dyn_cast<TryApplyInst>(AI)) {
    Builder.setInsertionPoint(TAI->getNormalBB()->begin());
    for (auto Arg : ToBeReleasedArgs) {
      Builder.emitDestroyValueOperation(PAI->getLoc(), Arg);
    }
    if (!PAI->hasCalleeGuaranteedContext())
      Builder.createStrongRelease(AI.getLoc(), PAI,
                                  Builder.getDefaultAtomicity());
    Builder.setInsertionPoint(TAI->getErrorBB()->begin());
    // Release the non-consumed parameters.
    for (auto Arg : ToBeReleasedArgs) {
      Builder.emitDestroyValueOperation(PAI->getLoc(), Arg);
    }
    if (!PAI->hasCalleeGuaranteedContext())
      Builder.emitDestroyValueOperation(PAI->getLoc(), PAI);
    Builder.setInsertionPoint(AI.getInstruction());
  } else {
    // Release the non-consumed parameters.
    for (auto Arg : ToBeReleasedArgs) {
      Builder.emitDestroyValueOperation(PAI->getLoc(), Arg);
    }
    if (!PAI->hasCalleeGuaranteedContext())
      Builder.emitDestroyValueOperation(PAI->getLoc(), PAI);
  }

  if (auto apply = dyn_cast<ApplyInst>(AI))
    SilCombiner->replaceInstUsesWith(*apply,
                                     cast<ApplyInst>(NAI.getInstruction()));
  SilCombiner->eraseInstFromFunction(*AI.getInstruction());
  return true;
}

/// Perform the apply{partial_apply(x,y)}(z) -> apply(z,x,y) peephole
/// by iterating over all uses of the partial_apply and searching
/// for the pattern to transform.
SILInstruction *PartialApplyCombiner::combine() {
  // We need to model @unowned_inner_pointer better before we can do the
  // peephole here.
  for (auto R : PAI->getSubstCalleeType()->getResults())
    if (R.getConvention() == ResultConvention::UnownedInnerPointer)
      return nullptr;

  // Iterate over all uses of the partial_apply
  // and look for applies that use it as a callee.

  // Worklist of operands.
  SmallVector<Operand *, 8> Uses(PAI->getUses());

  // Uses may grow in this loop.
  for (size_t UseIndex = 0; UseIndex < Uses.size(); ++UseIndex) {
    auto *Use = Uses[UseIndex];
    auto *User = Use->getUser();

    // Recurse through conversions.
    if (auto *CFI = dyn_cast<ConvertEscapeToNoEscapeInst>(User)) {
      // TODO: Handle argument conversion. All the code in this file needs to be
      // cleaned up and generalized. The argument conversion handling in
      // optimizeApplyOfConvertFunctionInst should apply to any combine
      // involving an apply, not just a specific pattern.
      //
      // For now, just handle conversion to @noescape, which is irrelevant for
      // direct application of the closure.
      auto ConvertCalleeTy = CFI->getType().castTo<SILFunctionType>();
      auto EscapingCalleeTy =
          ConvertCalleeTy->getWithExtInfo(
            ConvertCalleeTy->getExtInfo().withNoEscape(false));
      assert(Use->get()->getType().castTo<SILFunctionType>() ==
             EscapingCalleeTy);
      (void)EscapingCalleeTy;
      Uses.append(CFI->getUses().begin(), CFI->getUses().end());
      continue;
    }
    // If this use of a partial_apply is not
    // an apply which uses it as a callee, bail.
    auto AI = FullApplySite::isa(User);
    if (!AI)
      continue;

    if (AI.getCallee() != Use->get())
      continue;

    // We cannot handle generic apply yet. Bail.
    if (AI.hasSubstitutions())
      continue;

    if (!processSingleApply(AI))
      return nullptr;
  }

  // release/destroy and deallocate introduced temporaries.
  if (!Tmps.empty()) {
    releaseTemporaries();
    deallocateTemporaries();
  }

  return nullptr;
}

/// Iterate over all uses of a given partial_apply and check
/// if any of those uses are apply instructions. Try to
/// combine those applies with this partial_apply.
SILInstruction *
SILCombiner::tryOptimizeApplyOfPartialApply(PartialApplyInst *PAI) {

  PartialApplyCombiner PACombiner(PAI, Builder, this);
  return PACombiner.combine();
}

SILInstruction *
SILCombiner::optimizeApplyOfConvertFunctionInst(FullApplySite AI,
                                                ConvertFunctionInst *CFI) {
  // We only handle simplification of static function references. If we don't
  // have one, bail.
  SILValue funcOper = CFI->getOperand();
  if (auto *TTI = dyn_cast<ThinToThickFunctionInst>(funcOper))
    funcOper = TTI->getOperand();

  auto *FRI = dyn_cast<FunctionRefInst>(funcOper);
  if (!FRI)
    return nullptr;

  // Grab our relevant callee types...
  CanSILFunctionType SubstCalleeTy = AI.getSubstCalleeType();
  auto ConvertCalleeTy = funcOper->getType().castTo<SILFunctionType>();

  // ... and make sure they have no unsubstituted generics. If they do, bail.
  if (SubstCalleeTy->hasArchetype() || ConvertCalleeTy->hasArchetype())
    return nullptr;

  // Indirect results are not currently handled.
  if (AI.hasIndirectSILResults())
    return nullptr;

  // Bail if the result type of the converted callee is different from the callee's
  // result type of the apply instruction.
  if (SubstCalleeTy->getAllResultsType() != ConvertCalleeTy->getAllResultsType()) {
    return nullptr;
  }

  // Ok, we can now perform our transformation. Grab AI's operands and the
  // relevant types from the ConvertFunction function type and AI.
  Builder.setCurrentDebugScope(AI.getDebugScope());
  OperandValueArrayRef Ops = AI.getArgumentsWithoutIndirectResults();
  SILFunctionConventions substConventions(SubstCalleeTy, FRI->getModule());
  SILFunctionConventions convertConventions(ConvertCalleeTy, FRI->getModule());
  auto oldOpTypes = substConventions.getParameterSILTypes();
  auto newOpTypes = convertConventions.getParameterSILTypes();

  assert(Ops.size() == SubstCalleeTy->getNumParameters()
         && "Ops and op types must have same size.");
  assert(Ops.size() == ConvertCalleeTy->getNumParameters()
         && "Ops and op types must have same size.");

  llvm::SmallVector<SILValue, 8> Args;
  auto newOpI = newOpTypes.begin();
  auto oldOpI = oldOpTypes.begin();
  for (unsigned i = 0, e = Ops.size(); i != e; ++i, ++newOpI, ++oldOpI) {
    SILValue Op = Ops[i];
    SILType OldOpType = *oldOpI;
    SILType NewOpType = *newOpI;

    // Convert function takes refs to refs, address to addresses, and leaves
    // other types alone.
    if (OldOpType.isAddress()) {
      assert(NewOpType.isAddress() && "Addresses should map to addresses.");
      auto UAC = Builder.createUncheckedAddrCast(AI.getLoc(), Op, NewOpType);
      Args.push_back(UAC);
    } else if (OldOpType.getASTType() != NewOpType.getASTType()) {
      auto URC = Builder.createUncheckedBitCast(AI.getLoc(), Op, NewOpType);
      Args.push_back(URC);
    } else {
      Args.push_back(Op);
    }
  }

  // Create the new apply inst.
  SILInstruction *NAI;
  if (auto *TAI = dyn_cast<TryApplyInst>(AI))
    NAI = Builder.createTryApply(AI.getLoc(), FRI, 
                                 SubstitutionMap(), Args,
                                 TAI->getNormalBB(), TAI->getErrorBB());
  else {
    NAI = Builder.createApply(AI.getLoc(), FRI, SubstitutionMap(), Args,
                              cast<ApplyInst>(AI)->isNonThrowing());
    assert(FullApplySite::isa(NAI).getSubstCalleeType()->getAllResultsType() ==
           AI.getSubstCalleeType()->getAllResultsType() &&
           "Function types should be the same");
  }
  return NAI;
}

bool
SILCombiner::recursivelyCollectARCUsers(UserListTy &Uses, ValueBase *Value) {
  // FIXME: We could probably optimize this case too
  if (auto *AI = dyn_cast<ApplyInst>(Value))
    if (AI->hasIndirectResults())
      return false;

  for (auto *Use : Value->getUses()) {
    SILInstruction *Inst = Use->getUser();
    if (isa<RefCountingInst>(Inst) ||
        isa<DebugValueInst>(Inst)) {
      Uses.push_back(Inst);
      continue;
    }
    if (isa<TupleExtractInst>(Inst) ||
        isa<StructExtractInst>(Inst) ||
        isa<PointerToAddressInst>(Inst)) {
      Uses.push_back(Inst);
      if (recursivelyCollectARCUsers(Uses, cast<SingleValueInstruction>(Inst)))
        continue;
    }
    return false;
  }
  return true;
}

bool SILCombiner::eraseApply(FullApplySite FAS, const UserListTy &Users) {

  // Compute the places where we have to insert release-instructions for the
  // owned arguments. This must not be done before the result of the
  // apply is destroyed. Therefore we compute the lifetime of the apply-result.
  ValueLifetimeAnalysis VLA(FAS.getInstruction(), Users);
  ValueLifetimeAnalysis::Frontier Frontier;
  if (Users.empty()) {
    // If the call does not have any ARC-uses or if there is no return value at
    // all, we insert the argument release instructions right before the call.
    Frontier.push_back(FAS.getInstruction());
  } else {
    if (!VLA.computeFrontier(Frontier, ValueLifetimeAnalysis::DontModifyCFG))
      return false;
  }

  // Release and destroy any owned or in-arguments.
  auto FuncType = FAS.getOrigCalleeType();
  assert(FuncType->getParameters().size() == FAS.getNumArguments() &&
         "mismatching number of arguments");
  for (SILInstruction *FrontierInst : Frontier) {
    Builder.setInsertionPoint(FrontierInst);
    for (int i = 0, e = FAS.getNumArguments(); i < e; ++i) {
      SILParameterInfo PI = FuncType->getParameters()[i];
      auto Arg = FAS.getArgument(i);
      switch (PI.getConvention()) {
        case ParameterConvention::Indirect_In:
        case ParameterConvention::Indirect_In_Constant:
          Builder.createDestroyAddr(FAS.getLoc(), Arg);
          break;
        case ParameterConvention::Direct_Owned:
          Builder.createReleaseValue(FAS.getLoc(), Arg, Builder.getDefaultAtomicity());
          break;
        case ParameterConvention::Indirect_In_Guaranteed:
        case ParameterConvention::Indirect_Inout:
        case ParameterConvention::Indirect_InoutAliasable:
        case ParameterConvention::Direct_Unowned:
        case ParameterConvention::Direct_Guaranteed:
          break;
      }
    }
  }

  // Erase all of the reference counting instructions (in reverse order to have
  // no dangling uses).
  for (auto rit = Users.rbegin(), re = Users.rend(); rit != re; ++rit)
    eraseInstFromFunction(**rit);

  // And the Apply itself.
  eraseInstFromFunction(*FAS.getInstruction());

  return true;
}

SILInstruction *
SILCombiner::optimizeConcatenationOfStringLiterals(ApplyInst *AI) {
  // String literals concatenation optimizer.
  return tryToConcatenateStrings(AI, Builder);
}

/// This routine replaces the old witness method inst with a new one.
void SILCombiner::replaceWitnessMethodInst(
    WitnessMethodInst *WMI, SILBuilderContext &BuilderCtx, CanType ConcreteType,
    const ProtocolConformanceRef ConformanceRef) {
  SILBuilderWithScope WMIBuilder(WMI, BuilderCtx);
  auto *NewWMI = WMIBuilder.createWitnessMethod(
      WMI->getLoc(), ConcreteType, ConformanceRef, WMI->getMember(),
      WMI->getType());
  WMI->replaceAllUsesWith(NewWMI);
  if (WMI->use_empty())
    eraseInstFromFunction(*WMI);
}

//  This function propagates concrete type of existential self argument using
//  ProtocolConformanceAnalysis. The concrete type of self can be a class,
//  struct, or an enum. It replaces the witness_method instruction
//  with one that has a concrete type, allowing other optimizations to
//  devirtualize it later.
SILInstruction *
SILCombiner::propagateSoleConformingType(FullApplySite Apply,
                                         WitnessMethodInst *WMI) {
  // If WMI has concrete conformance, it can be optimized.
  if (WMI->getConformance().isConcrete())
    return nullptr;

  // If the lookup type is an opened existential type,
  // it cannot be made concrete.
  if (!WMI->getLookupType()->isOpenedExistential())
    return nullptr;

  // If the witness method mutates self, we cannot replace self.
  if (Apply.getOrigCalleeType()->getSelfParameter().isIndirectMutating())
    return nullptr;

  // Only applicable in whole-module compilation.
  if (!Apply.getModule().isWholeModule())
    return nullptr;

  auto *PD = WMI->getLookupProtocol();

  // Determine the sole conforming type.
  auto *NTD = PCA->findSoleConformingType(PD);
  if (!NTD)
    return nullptr;

  // Sole conforming class should not be open access or have any derived class.
  ClassDecl *CD;
  if ((CD = dyn_cast<ClassDecl>(NTD)) &&
      (CD->getEffectiveAccess() == AccessLevel::Open ||
       CHA->hasKnownDirectSubclasses(CD))) {
    return nullptr;
  }

  // Create SIL type for the concrete type.
  auto ElementType = NTD->getDeclaredType();
  auto ConcreteType = ElementType->getCanonicalType();
  auto &M = Builder.getModule();

  /// Determine OpenedArchetypeDef and SubstituionMap.
  ConcreteExistentialInfo CEI(Apply.getSelfArgumentOperand(), ConcreteType, PD);
  if (!CEI.isValid())
    return nullptr;

  if (!CEI.InitExistential) {
    // Create SIL type for the concrete type.
    SILType ConcreteSILType = M.Types.getLoweredType(ConcreteType);

    // Prepare the code by adding UncheckedCast instructions that cast opened
    // existentials to concrete types. Set the ConcreteValue of CEI.
    if (auto *OER = dyn_cast<OpenExistentialRefInst>(CEI.OpenedArchetypeDef)) {
      auto *URCI =
          Builder.createUncheckedRefCast(OER->getLoc(), OER, ConcreteSILType);
      CEI.ConcreteValue = URCI;
    } else if (auto *OEA =
                   dyn_cast<OpenExistentialAddrInst>(CEI.OpenedArchetypeDef)) {
      auto *UACI = Builder.createUncheckedAddrCast(
          OEA->getLoc(), OEA, ConcreteSILType.getAddressType());
      CEI.ConcreteValue = UACI;
    } else {
      llvm_unreachable(
          "Unhandled Argument Type in propagateSoleConformingType");
    }
  }

  assert(CEI.ConcreteValue);

  /// Replace the old WitnessMethod with a new one that has concrete type and
  /// conformance.
  SILBuilderContext BuilderCtx(M, Builder.getTrackingList());
  replaceWitnessMethodInst(WMI, BuilderCtx, ConcreteType,
                           *(CEI.ExistentialSubs.getConformances().begin()));
  // Construct the map for Self to be used for createApplyWithConcreteType.
  llvm::SmallDenseMap<unsigned, ConcreteExistentialInfo> CEIs;
  CEIs.insert(std::pair<unsigned, ConcreteExistentialInfo>(
      Apply.getCalleeArgIndex(Apply.getSelfArgumentOperand()), CEI));
  /// Create the new apply instruction using the concrete type.
  auto *NewAI = createApplyWithConcreteType(Apply, CEIs, BuilderCtx);
  return NewAI;
}

/// Given an Apply and an argument value produced by InitExistentialAddrInst,
/// return true if the argument can be replaced by a copy of its value.
///
/// FIXME: remove this helper when we can assume SIL opaque values.
static bool canReplaceCopiedArg(FullApplySite Apply,
                                 SILInstruction *InitExistential,
                                 DominanceAnalysis *DA, unsigned ArgIdx) {
  // If the witness method mutates Arg, we cannot replace Arg with
  // the source of a copy. Otherwise the call would modify another value than
  // the original argument.
  auto origConv = Apply.getOrigCalleeConv();
  if (origConv.getParamInfoForSILArg(ArgIdx).isIndirectMutating())
    return false;

  auto *DT = DA->get(Apply.getFunction());
  auto *AI = Apply.getInstruction();
  // Only init_existential_addr may be copied.
  SILValue existentialAddr =
      cast<InitExistentialAddrInst>(InitExistential)->getOperand();

  // If we peeked through an InitEnumDataAddr or some such, then don't assume we
  // can reuse the copied value. It's likely destroyed by
  // UncheckedTakeEnumDataInst before the copy.
  auto *ASI = dyn_cast<AllocStackInst>(existentialAddr);
  if (!ASI)
    return false;

  // Return true only if the given value is guaranteed to be initialized across
  // the given call site.
  //
  // It's possible for an address to be initialized/deinitialized/reinitialized.
  // Rather than keeping track of liveness, we very conservatively check that
  // all deinitialization occures after the call.
  auto isDestroy = [](Operand *use) {
    switch (use->getUser()->getKind()) {
    default:
      return false;
    case SILInstructionKind::DestroyAddrInst:
    case SILInstructionKind::DeinitExistentialAddrInst:
      return true;
    case SILInstructionKind::CopyAddrInst: {
      auto *copy = cast<CopyAddrInst>(use->getUser());
      return copy->getSrc() == use->get() && copy->isTakeOfSrc();
    }
    }
  };
  for (auto use : existentialAddr->getUses()) {
    SILInstruction *user = use->getUser();
    if (isDestroy(use)) {
      if (!DT->properlyDominates(AI, user))
        return false;
    } else {
      // The caller has to guarantee that there are no other instructions which
      // use the address. This is done in findInitExistential called from
      // the constructor of ConcreteExistentialInfo.
      assert(isa<CopyAddrInst>(user) || isa<InitExistentialAddrInst>(user) ||
             isa<OpenExistentialAddrInst>(user) ||
             isa<DeallocStackInst>(user) ||
             isa<ApplyInst>(user) || isa<TryApplyInst>(user) ||
             user->isDebugInstruction() && "Unexpected instruction");
    }
  }
  return true;
}

// Check the legal conditions under which a Arg parameter (specified as ArgIdx)
// can be replaced with a concrete type. Concrete type info is passed as CEI
// argument.
bool SILCombiner::canReplaceArg(FullApplySite Apply,
                                const ConcreteExistentialInfo &CEI,
                                unsigned ArgIdx) {

  // Don't specialize apply instructions that return the callee's Arg type,
  // because this optimization does not know how to substitute types in the
  // users of this apply. In the function type substitution below, all
  // references to OpenedArchetype will be substituted. So walk to type to
  // find all possible references, such as returning Optional<Arg>.
  if (Apply.getType().getASTType().findIf(
          [&CEI](Type t) -> bool { return t->isEqual(CEI.OpenedArchetype); })) {
    return false;
  }
  // Bail out if any other arguments or indirect result that refer to the
  // OpenedArchetype. The following optimization substitutes all occurrences
  // of OpenedArchetype in the function signature, but will only rewrite the
  // Arg operand.
  //
  // Note that the language does not allow Self to occur in contravariant
  // position. However, SIL does allow this and it can happen as a result of
  // upstream transformations. Since this is bail-out logic, it must handle
  // all verifiable SIL.

  // This bailout check is also needed for non-Self arguments [including Self].
  unsigned NumApplyArgs = Apply.getNumArguments();
  for (unsigned Idx = 0; Idx < NumApplyArgs; Idx++) {
    if (Idx == ArgIdx)
      continue;
    if (Apply.getArgument(Idx)->getType().getASTType().findIf(
            [&CEI](Type t) -> bool {
              return t->isEqual(CEI.OpenedArchetype);
            })) {
      return false;
    }
  }
  // The apply can only be rewritten in terms of the concrete value if it is
  // legal to pass that value as the Arg argument.
  if (CEI.isCopied &&
      (!CEI.InitExistential ||
       !canReplaceCopiedArg(Apply, CEI.InitExistential, DA, ArgIdx))) {
    return false;
  }
  // It is safe to replace Arg.
  return true;
}

/// Rewrite the given method apply instruction in terms of the provided conrete
/// type information.
///
/// If the rewrite is successful, the original apply will be removed and the new
/// apply is returned. Otherwise, the original apply will not be removed and
/// nullptr is returned.
///
/// Creates a new apply instruction that uses the concrete type instead of the
/// existential type. Type substitution will be performed from all occurrences
/// of CEI.OpenedArchetype to the replacement type CEI.ConcreteType within the
/// applied function type. The single self argument of the apply will be
/// rewritten. This helps the devirtualizer to replace witness_method by
/// class_method instructions and then devirtualize.
///
/// Note that the substituted type, CEI.OpenedArchetype, is the same type as the
/// self argument for nonstatic methods, but for static methods self is the
/// metatype instead. For witness methods, CEI.OpenedArchetype is usually the
/// same as WMI->getLookupType() but differs in the unusual situation in which
/// the witness method is looked up using a different opened archetype.
///
/// FIXME: Protocol methods (witness or default) that return Self will be given
/// a new return type. This implementation fails to update the type signature of
/// SSA uses in those cases. Currently we bail out on methods that return Self.
SILInstruction *SILCombiner::createApplyWithConcreteType(
    FullApplySite Apply,
    const llvm::SmallDenseMap<unsigned, ConcreteExistentialInfo> &CEIs,
    SILBuilderContext &BuilderCtx) {

  // Ensure that the callee is polymorphic.
  assert(Apply.getOrigCalleeType()->isPolymorphic());

  // Create the new set of arguments to apply including their substitutions.
  SubstitutionMap NewCallSubs = Apply.getSubstitutionMap();
  SmallVector<SILValue, 8> NewArgs;
  bool UpdatedArgs = false;
  unsigned ArgIdx = 0;
  // Push the indirect result arguments.
  for (unsigned EndIdx = Apply.getSubstCalleeConv().getSILArgIndexOfFirstParam();
       ArgIdx < EndIdx; ++ArgIdx) {
      NewArgs.push_back(Apply.getArgument(ArgIdx));
  }
  // Transform the parameter arguments.
  for (unsigned EndIdx = Apply.getNumArguments(); ArgIdx < EndIdx; ++ArgIdx) {
    auto ArgIt = CEIs.find(ArgIdx);
    if (ArgIt == CEIs.end()) {
      // Use the old argument if it does not have a valid concrete existential.
      NewArgs.push_back(Apply.getArgument(ArgIdx));
      continue;
    }
    auto &CEI = ArgIt->second;
    // Check for Arg's concrete type propagation legality.
    if (!canReplaceArg(Apply, CEI, ArgIdx)) {
      NewArgs.push_back(Apply.getArgument(ArgIdx));
      continue;
    }
    UpdatedArgs = true;
    // Ensure that we have a concrete value to propagate.
    assert(CEI.ConcreteValue);
    NewArgs.push_back(CEI.ConcreteValue);
    // Form a new set of substitutions where the argument is
    // replaced with a concrete type.
    NewCallSubs = NewCallSubs.subst(
        [&](SubstitutableType *type) -> Type {
          if (type == CEI.OpenedArchetype)
            return CEI.ConcreteType;
          return type;
        },
        [&](CanType origTy, Type substTy,
            ProtocolDecl *proto) -> Optional<ProtocolConformanceRef> {
          if (origTy->isEqual(CEI.OpenedArchetype)) {
            assert(substTy->isEqual(CEI.ConcreteType));
            // Do a conformance lookup on this witness requirement using the
            // existential's conformances. The witness requirement may be a
            // base type of the existential's requirements.
            return CEI.lookupExistentialConformance(proto);
          }
          return ProtocolConformanceRef(proto);
        });
  }

  if (!UpdatedArgs)
    return nullptr;

  // Now create the new apply instruction.
  SILBuilderWithScope ApplyBuilder(Apply.getInstruction(), BuilderCtx);
  FullApplySite NewApply;
  if (auto *TAI = dyn_cast<TryApplyInst>(Apply))
    NewApply = ApplyBuilder.createTryApply(
        Apply.getLoc(), Apply.getCallee(), NewCallSubs, NewArgs,
        TAI->getNormalBB(), TAI->getErrorBB());
  else
    NewApply = ApplyBuilder.createApply(
        Apply.getLoc(), Apply.getCallee(), NewCallSubs, NewArgs,
        cast<ApplyInst>(Apply)->isNonThrowing());

  if (auto NewAI = dyn_cast<ApplyInst>(NewApply))
    replaceInstUsesWith(*cast<ApplyInst>(Apply.getInstruction()), NewAI);

  eraseInstFromFunction(*Apply.getInstruction());

  return NewApply.getInstruction();
}

/// Rewrite a witness method's lookup type from an archetype to a concrete type.
/// Example:
///   %existential = alloc_stack $Protocol
///   %value = init_existential_addr %existential : $Concrete
///   copy_addr ... to %value
///   %witness = witness_method $@opened
///   apply %witness<T : Protocol>(%existential)
///
/// ==> apply %witness<Concrete : Protocol>(%existential)
SILInstruction *
SILCombiner::propagateConcreteTypeOfInitExistential(FullApplySite Apply,
                                                    WitnessMethodInst *WMI) {
  // Check if it is legal to perform the propagation.
  if (WMI->getConformance().isConcrete())
    return nullptr;

  // If the lookup type is not an opened existential type,
  // it cannot be made more concrete.
  if (!WMI->getLookupType()->isOpenedExistential())
    return nullptr;

  // Try to derive the concrete type of self and the related conformance by
  // searching for a preceding init_existential.
  const ConcreteExistentialInfo CEI(Apply.getSelfArgumentOperand());
  if (!CEI.isValid())
    return nullptr;

  // Get the conformance of the init_existential type, which is passed as the
  // self argument, on the witness' protocol.
  ProtocolConformanceRef SelfConformance =
      *CEI.lookupExistentialConformance(WMI->getLookupProtocol());

  SILBuilderContext BuilderCtx(Builder.getModule(), Builder.getTrackingList());
  SILOpenedArchetypesTracker OpenedArchetypesTracker(&Builder.getFunction());
  BuilderCtx.setOpenedArchetypesTracker(&OpenedArchetypesTracker);
  if (CEI.ConcreteType->isOpenedExistential()) {
    // Temporarily record this opened existential def in this local
    // BuilderContext before rewriting the witness method.
    OpenedArchetypesTracker.addOpenedArchetypeDef(
        cast<ArchetypeType>(CEI.ConcreteType), CEI.ConcreteTypeDef);
  }
  // Propagate the concrete type into a callee-operand, which is a
  // witness_method instruction. It's ok to rewrite the witness method in terms
  // of a concrete type without rewriting the apply itself. In fact, doing so
  // may allow the Devirtualizer pass to finish the job.
  //
  // If we create a new instruction that’s the same as the old one we’ll
  // cause an infinite loop:
  // NewWMI will be added to the Builder’s tracker list.
  // SILCombine, in turn, uses the tracker list to populate the worklist
  // As such, if we don’t remove the witness method later on in the pass, we
  // are stuck:
  // We will re-create the same instruction and re-populate the worklist
  // with it.
  if (CEI.ConcreteType != WMI->getLookupType() ||
      SelfConformance != WMI->getConformance()) {
    replaceWitnessMethodInst(WMI, BuilderCtx, CEI.ConcreteType,
                             SelfConformance);
  }
  // Construct the map for Self to be used for createApplyWithConcreteType.
  llvm::SmallDenseMap<unsigned, ConcreteExistentialInfo> CEIs;
  CEIs.insert(std::pair<unsigned, ConcreteExistentialInfo>(
      Apply.getCalleeArgIndex(Apply.getSelfArgumentOperand()), CEI));
  // Try to rewrite the apply.
  return createApplyWithConcreteType(Apply, CEIs, BuilderCtx);
}

/// Rewrite a protocol extension lookup type from an archetype to a concrete
/// type.
/// Example:
///   %ref = alloc_ref $C
///   %existential = init_existential_ref %ref : $C : $C, $P
///   %opened = open_existential_ref %existential : $P to $@opened
///   %f = function_ref @defaultMethod
///   apply %f<@opened P>(%opened)
///
/// ==> apply %f<C : P>(%ref)
SILInstruction *
SILCombiner::propagateConcreteTypeOfInitExistential(FullApplySite Apply) {
  // This optimization requires a generic argument.
  if (!Apply.hasSubstitutions())
    return nullptr;

  SILBuilderContext BuilderCtx(Builder.getModule(), Builder.getTrackingList());
  SILOpenedArchetypesTracker OpenedArchetypesTracker(&Builder.getFunction());
  BuilderCtx.setOpenedArchetypesTracker(&OpenedArchetypesTracker);
  llvm::SmallDenseMap<unsigned, ConcreteExistentialInfo> CEIs;
  for (unsigned ArgIdx = 0; ArgIdx < Apply.getNumArguments(); ArgIdx++) {
    auto ArgASTType = Apply.getArgument(ArgIdx)->getType().getASTType();
    if (!ArgASTType->hasArchetype())
      continue;
    const ConcreteExistentialInfo CEI(Apply.getArgumentOperands()[ArgIdx]);
    if (!CEI.isValid())
      continue;

    CEIs.insert(std::pair<unsigned, ConcreteExistentialInfo>(ArgIdx, CEI));

    if (CEI.ConcreteType->isOpenedExistential()) {
      // Temporarily record this opened existential def in this local
      // BuilderContext before rewriting the witness method.
      OpenedArchetypesTracker.addOpenedArchetypeDef(
          cast<ArchetypeType>(CEI.ConcreteType), CEI.ConcreteTypeDef);
    }
  }
  // Bail, if no argument has a concrete existential to propagate.
  if (CEIs.empty())
    return nullptr;
  return createApplyWithConcreteType(Apply, CEIs, BuilderCtx);
}

/// Check that all users of the apply are retain/release ignoring one
/// user.
static bool
hasOnlyRetainReleaseUsers(ApplyInst *AI, SILInstruction *IgnoreUser,
                          SmallVectorImpl<SILInstruction *> &Users) {
  for (auto *Use : getNonDebugUses(AI)) {
    if (Use->getUser() == IgnoreUser)
      continue;

    if (!isa<RetainValueInst>(Use->getUser()) &&
        !isa<ReleaseValueInst>(Use->getUser()) &&
        !isa<StrongRetainInst>(Use->getUser()) &&
        !isa<StrongReleaseInst>(Use->getUser()))
      return false;

    Users.push_back(Use->getUser());
  }
  return true;
};

/// We only know how to simulate reference call effects for unary
/// function calls that take their argument @owned or @guaranteed and return an
/// @owned value.
static bool knowHowToEmitReferenceCountInsts(ApplyInst *Call) {
  if (Call->getNumArguments() != 1)
    return false;

  FunctionRefInst *FRI = cast<FunctionRefInst>(Call->getCallee());
  SILFunction *F = FRI->getReferencedFunction();
  auto FnTy = F->getLoweredFunctionType();

  // Look at the result type.
  if (FnTy->getNumResults() != 1)
    return false;
  auto ResultInfo = FnTy->getResults()[0];
  if (ResultInfo.getConvention() != ResultConvention::Owned)
    return false;

  // Look at the parameter.
  auto Params = FnTy->getParameters();
  (void) Params;
  assert(Params.size() == 1 && "Expect one parameter");
  auto ParamConv = FnTy->getParameters()[0].getConvention();

  return ParamConv == ParameterConvention::Direct_Owned ||
         ParamConv == ParameterConvention::Direct_Guaranteed;
}

/// Add reference counting operations equal to the effect of the call.
static void emitMatchingRCAdjustmentsForCall(ApplyInst *Call, SILValue OnX) {
  FunctionRefInst *FRI = cast<FunctionRefInst>(Call->getCallee());
  SILFunction *F = FRI->getReferencedFunction();
  auto FnTy = F->getLoweredFunctionType();
  assert(FnTy->getNumResults() == 1);
  auto ResultInfo = FnTy->getResults()[0];
  (void) ResultInfo;

  assert(ResultInfo.getConvention() == ResultConvention::Owned &&
         "Expect a @owned return");
  assert(Call->getNumArguments() == 1 && "Expect a unary call");

  // Emit a retain for the @owned return.
  SILBuilderWithScope Builder(Call);
  Builder.createRetainValue(Call->getLoc(), OnX, Builder.getDefaultAtomicity());

  // Emit a release for the @owned parameter, or none for a @guaranteed
  // parameter.
  auto Params = FnTy->getParameters();
  (void) Params;
  assert(Params.size() == 1 && "Expect one parameter");
  auto ParamInfo = FnTy->getParameters()[0].getConvention();
  assert(ParamInfo == ParameterConvention::Direct_Owned ||
         ParamInfo == ParameterConvention::Direct_Guaranteed);

  if (ParamInfo == ParameterConvention::Direct_Owned)
    Builder.createReleaseValue(Call->getLoc(), OnX, Builder.getDefaultAtomicity());
}

/// Replace an application of a cast composition f_inverse(f(x)) by x.
bool SILCombiner::optimizeIdentityCastComposition(ApplyInst *FInverse,
                                              StringRef FInverseName,
                                              StringRef FName) {
  // Needs to have a known semantics.
  if (!FInverse->hasSemantics(FInverseName))
    return false;

  // We need to know how to replace the call by reference counting instructions.
  if (!knowHowToEmitReferenceCountInsts(FInverse))
    return false;

  // Need to have a matching 'f'.
  auto *F = dyn_cast<ApplyInst>(FInverse->getArgument(0));
  if (!F)
    return false;
  if (!F->hasSemantics(FName))
    return false;
  if (!knowHowToEmitReferenceCountInsts(F))
    return false;

  // The types must match.
  if (F->getArgument(0)->getType() != FInverse->getType())
    return false;

  // Retains, releases of the result of F.
  SmallVector<SILInstruction *, 16> RetainReleases;
  if (!hasOnlyRetainReleaseUsers(F, FInverse, RetainReleases))
    return false;

  // Okay, now we know we can remove the calls.
  auto X = F->getArgument(0);

  // Redirect f's result's retains/releases to affect x.
  for (auto *User : RetainReleases) {
    // X might not be strong_retain/release'able. Replace it by a
    // retain/release_value on X instead.
    if (isa<StrongRetainInst>(User)) {
      SILBuilderWithScope Builder(User);
      Builder.createRetainValue(User->getLoc(), X,
                                cast<StrongRetainInst>(User)->getAtomicity());
      eraseInstFromFunction(*User);
      continue;
    }
    if (isa<StrongReleaseInst>(User)) {
      SILBuilderWithScope Builder(User);
      Builder.createReleaseValue(User->getLoc(), X,
                                 cast<StrongReleaseInst>(User)->getAtomicity());
      eraseInstFromFunction(*User);
      continue;
    }
    User->setOperand(0, X);
  }

  // Simulate the reference count effects of the calls before removing
  // them.
  emitMatchingRCAdjustmentsForCall(F, X);
  emitMatchingRCAdjustmentsForCall(FInverse, X);

  // Replace users of f_inverse by x.
  replaceInstUsesWith(*FInverse, X);

  // Remove the calls.
  eraseInstFromFunction(*FInverse);
  eraseInstFromFunction(*F);

  return true;
}

// Return a new apply with the specified callee. This creates a new apply rather
// than simply rewriting the callee operand because the apply's SubstCalleeType,
// derived from the callee and substitution list, may change.
FullApplySite SILCombiner::rewriteApplyCallee(FullApplySite apply,
                                              SILValue callee) {
  SmallVector<SILValue, 4> arguments;
  for (SILValue arg : apply.getArguments())
    arguments.push_back(arg);

  Builder.addOpenedArchetypeOperands(apply.getInstruction());
  if (auto *TAI = dyn_cast<TryApplyInst>(apply)) {
    return Builder.createTryApply(TAI->getLoc(), callee,
                                  TAI->getSubstitutionMap(), arguments,
                                  TAI->getNormalBB(), TAI->getErrorBB());
  } else {
    return Builder.createApply(apply.getLoc(), callee,
                               apply.getSubstitutionMap(), arguments,
                               cast<ApplyInst>(apply)->isNonThrowing());
  }
}

SILInstruction *SILCombiner::visitApplyInst(ApplyInst *AI) {
  Builder.setCurrentDebugScope(AI->getDebugScope());
  // apply{partial_apply(x,y)}(z) -> apply(z,x,y) is triggered
  // from visitPartialApplyInst(), so bail here.
  if (isa<PartialApplyInst>(AI->getCallee()))
    return nullptr;

  if (auto *CFI = dyn_cast<ConvertFunctionInst>(AI->getCallee()))
    return optimizeApplyOfConvertFunctionInst(AI, CFI);

  // Optimize readonly functions with no meaningful users.
  SILFunction *SF = AI->getReferencedFunction();
  if (SF && SF->getEffectsKind() < EffectsKind::ReleaseNone) {
    UserListTy Users;
    if (recursivelyCollectARCUsers(Users, AI)) {
      if (eraseApply(AI, Users))
        return nullptr;
    }
    // We found a user that we can't handle.
  }

  if (SF) {
    if (SF->getEffectsKind() < EffectsKind::ReleaseNone) {
      // Try to optimize string concatenation.
      if (auto I = optimizeConcatenationOfStringLiterals(AI)) {
        return I;
      }
    }
    if (SF->hasSemanticsAttr("array.uninitialized")) {
      UserListTy Users;
      // If the uninitialized array is only written into then it can be removed.
      if (recursivelyCollectARCUsers(Users, AI)) {
        if (eraseApply(AI, Users))
          return nullptr;
      }
    }
  }


  // (apply (thin_to_thick_function f)) to (apply f)
  if (auto *TTTFI = dyn_cast<ThinToThickFunctionInst>(AI->getCallee())) {
    // We currently don't remove any possible retain associated with the thick
    // function when rewriting the callsite. This should be ok because the
    // ABI normally expects a guaranteed callee.
    if (!AI->getOrigCalleeType()->isCalleeConsumed())
      return rewriteApplyCallee(AI, TTTFI->getOperand()).getInstruction();
  }

  // (apply (witness_method)) -> propagate information about
  // a concrete type from init_existential_addr or init_existential_ref.
  if (auto *WMI = dyn_cast<WitnessMethodInst>(AI->getCallee())) {
    if (!propagateConcreteTypeOfInitExistential(AI, WMI)) {
      if (auto *WitnessMethod = dyn_cast<WitnessMethodInst>(AI->getCallee())) {
        // Propagate concrete type from ProtocolConformanceAnalysis.
        propagateSoleConformingType(AI, WitnessMethod);
      }
    }
    return nullptr;
  }

  // (apply (function_ref method_from_protocol_extension)) ->
  // propagate information about a concrete type from init_existential_addr or
  // init_existential_ref.
  if (isa<FunctionRefInst>(AI->getCallee())) {
    if (propagateConcreteTypeOfInitExistential(AI)) {
      return nullptr;
    }
  }

  // Optimize f_inverse(f(x)) -> x.
  if (optimizeIdentityCastComposition(AI, "convertFromObjectiveC",
                                      "convertToObjectiveC"))
    return nullptr;
  if (optimizeIdentityCastComposition(AI, "convertToObjectiveC",
                                      "convertFromObjectiveC"))
    return nullptr;

  return nullptr;
}

bool SILCombiner::
isTryApplyResultNotUsed(UserListTy &AcceptedUses, TryApplyInst *TAI) {
  SILBasicBlock *NormalBB = TAI->getNormalBB();
  SILBasicBlock *ErrorBB = TAI->getErrorBB();

  // The results of a try_apply are not only the normal and error return values,
  // but also the decision whether it throws or not. Therefore we have to check
  // if both, the normal and the error block, are empty and lead to a common
  // destination block.

  // Check if the normal and error blocks have a common single successor.
  auto *NormalBr = dyn_cast<BranchInst>(NormalBB->getTerminator());
  if (!NormalBr)
    return false;
  auto *ErrorBr = dyn_cast<BranchInst>(ErrorBB->getTerminator());
  if (!ErrorBr || ErrorBr->getDestBB() != NormalBr->getDestBB())
    return false;

  assert(NormalBr->getNumArgs() == ErrorBr->getNumArgs() &&
         "mismatching number of arguments for the same destination block");

  // Check if both blocks pass the same arguments to the common destination.
  for (unsigned Idx = 0, End = NormalBr->getNumArgs(); Idx < End; Idx++) {
    if (NormalBr->getArg(Idx) != ErrorBr->getArg(Idx))
      return false;
  }

  // Check if the normal and error results only have ARC operations as uses.
  if (!recursivelyCollectARCUsers(AcceptedUses, NormalBB->getArgument(0)))
    return false;
  if (!recursivelyCollectARCUsers(AcceptedUses, ErrorBB->getArgument(0)))
    return false;

  SmallPtrSet<SILInstruction *, 8> UsesSet;
  for (auto *I : AcceptedUses)
    UsesSet.insert(I);

  // Check if the normal and error blocks are empty, except the ARC uses.
  for (auto &I : *NormalBB) {
    if (!UsesSet.count(&I) && !isa<TermInst>(&I))
      return false;
  }
  for (auto &I : *ErrorBB) {
    if (!UsesSet.count(&I) && !isa<TermInst>(&I))
      return false;
  }
  return true;
}

SILInstruction *SILCombiner::visitTryApplyInst(TryApplyInst *AI) {
  // apply{partial_apply(x,y)}(z) -> apply(z,x,y) is triggered
  // from visitPartialApplyInst(), so bail here.
  if (isa<PartialApplyInst>(AI->getCallee()))
    return nullptr;

  if (auto *CFI = dyn_cast<ConvertFunctionInst>(AI->getCallee())) {
    return optimizeApplyOfConvertFunctionInst(AI, CFI);
  }

  // Optimize readonly functions with no meaningful users.
  SILFunction *Fn = AI->getReferencedFunction();
  if (Fn && Fn->getEffectsKind() < EffectsKind::ReleaseNone) {
    UserListTy Users;
    if (isTryApplyResultNotUsed(Users, AI)) {
      SILBasicBlock *BB = AI->getParent();
      SILBasicBlock *NormalBB = AI->getNormalBB();
      SILBasicBlock *ErrorBB = AI->getErrorBB();
      SILLocation Loc = AI->getLoc();
      const SILDebugScope *DS = AI->getDebugScope();
      if (eraseApply(AI, Users)) {
        // Replace the try_apply with a cond_br false, which will be removed by
        // SimplifyCFG. We don't want to modify the CFG in SILCombine.
        Builder.setInsertionPoint(BB);
        Builder.setCurrentDebugScope(DS);
        auto *TrueLit = Builder.createIntegerLiteral(Loc,
                  SILType::getBuiltinIntegerType(1, Builder.getASTContext()), 0);
        Builder.createCondBranch(Loc, TrueLit, NormalBB, ErrorBB);

        NormalBB->eraseArgument(0);
        ErrorBB->eraseArgument(0);
        return nullptr;
      }
    }
    // We found a user that we can't handle.
  }

  // (try_apply (thin_to_thick_function f)) to (try_apply f)
  if (auto *TTTFI = dyn_cast<ThinToThickFunctionInst>(AI->getCallee())) {
    // We currently don't remove any possible retain associated with the thick
    // function when rewriting the callsite. This should be ok because the
    // ABI normally expects a guaranteed callee.
    if (!AI->getOrigCalleeType()->isCalleeConsumed())
      return rewriteApplyCallee(AI, TTTFI->getOperand()).getInstruction();
  }

  // (apply (witness_method)) -> propagate information about
  // a concrete type from init_existential_addr or init_existential_ref.
  if (auto *WMI = dyn_cast<WitnessMethodInst>(AI->getCallee())) {
    if (!propagateConcreteTypeOfInitExistential(AI, WMI)) {
      if (auto *WitnessMethod = dyn_cast<WitnessMethodInst>(AI->getCallee())) {
        // Propagate concrete type from ProtocolConformanceAnalysis.
        propagateSoleConformingType(AI, WitnessMethod);
      }
    }
    return nullptr;
  }

  // (apply (function_ref method_from_protocol_extension)) ->
  // propagate information about a concrete type from init_existential_addr or
  // init_existential_ref.
  if (isa<FunctionRefInst>(AI->getCallee())) {
    if (propagateConcreteTypeOfInitExistential(AI)) {
      return nullptr;
    }
  }

  return nullptr;
}
