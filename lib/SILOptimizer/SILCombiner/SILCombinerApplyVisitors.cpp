//===--- SILCombinerApplyVisitors.cpp -------------------------------------===//
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

#define DEBUG_TYPE "sil-combine"
#include "SILCombiner.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/CFG.h"
#include "swift/SILOptimizer/Analysis/ValueTracking.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"

using namespace swift;
using namespace swift::PatternMatch;

/// Check that this is a partial apply of a reabstraction thunk and return the
/// argument of the partial apply if it is.
static SILValue
isPartialApplyOfReabstractionThunk(PartialApplyInst *PAI, bool requireSingleUse) {
  if (requireSingleUse) {
    SILValue PAIVal(PAI, 0);
    if (!hasOneNonDebugUse(PAIVal))
      return SILValue();
  }

  if (PAI->getNumArguments() != 1)
    return SILValue();

  auto *Fun = PAI->getCalleeFunction();
  if (!Fun)
    return SILValue();

  // Make sure we have a reabstraction thunk.
  if (Fun->isThunk() != IsReabstractionThunk)
    return SILValue();

  // The argument should be a closure.
  auto Arg = PAI->getArgument(0);
  if (!Arg.getType().is<SILFunctionType>() ||
      !Arg.getType().isReferenceCounted(PAI->getFunction()->getModule()))
    return SILValue();

  return PAI->getArgument(0);
}

/// Remove pointless reabstraction thunk closures.
///   partial_apply %reabstraction_thunk_typeAtoB(
///      partial_apply %reabstraction_thunk_typeBtoA %closure_typeB))
///   ->
///   %closure_typeB
static bool foldInverseReabstractionThunks(PartialApplyInst *PAI,
                                           SILCombiner *Combiner) {
  auto PAIArg = isPartialApplyOfReabstractionThunk(PAI, false);
  if (!PAIArg)
    return false;

  auto *PAI2 = dyn_cast<PartialApplyInst>(PAIArg);
  if (!PAI2)
    return false;

  auto PAI2Arg = isPartialApplyOfReabstractionThunk(PAI2, true);
  if (!PAI2Arg)
    return false;

  // The types must match.
  if (PAI->getType() != PAI2->getArgument(0).getType())
    return false;

  // Replace the partial_apply(partial_apply(X)) by X and remove the
  // partial_applies.

  Combiner->replaceInstUsesWith(*PAI, PAI2->getArgument(0).getDef());
  Combiner->eraseInstFromFunction(*PAI);
  assert(hasNoUsesExceptDebug(PAI2) && "Should not have any uses");
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

static bool canCombinePartialApply(const PartialApplyInst *PAI) {
  // Only process partial apply if the callee is a known function.
  auto *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee());
  if (!FRI)
    return false;

  // Make sure that the substitution list of the PAI does not contain any
  // archetypes.
  ArrayRef<Substitution> Subs = PAI->getSubstitutions();
  for (Substitution S : Subs)
    if (S.getReplacement()->getCanonicalType()->hasArchetype())
      return false;

  return true;
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
  // releases/destroys of temporaries as early as possible.  If no releases are
  // needed, Lifetime remains empty.
  ValueLifetime Lifetime;

  SILBuilder &Builder;

  // Function referenced by partial_apply.
  FunctionRefInst *FRI;

  SILCombiner *SilCombiner;

  void processSingleApply(FullApplySite AI);
  void allocateTemporaries();
  void deallocateTemporaries();
  void releaseTemporaries();

public:
  PartialApplyCombiner(PartialApplyInst *PAI, SILBuilder &Builder,
                       SILCombiner *SilCombiner)
      : isFirstTime(true), PAI(PAI), Builder(Builder),
        FRI(nullptr), SilCombiner(SilCombiner) {}
  SILInstruction *combine();
};

void PartialApplyCombiner::allocateTemporaries() {
  // Copy the original arguments of the partial_apply into
  // newly created temporaries and use these temporaries instead of
  // the original arguments afterwards.
  // This is done to "extend" the life-time of original partial_apply
  // arguments, as they may be destroyed/deallocated before the last
  // use by one of the apply instructions.
  // TODO:
  // Copy arguments of the partial_apply into new temporaries
  // only if the lifetime of arguments ends before their uses
  // by apply instructions.
  bool needsReleases = false;
  CanSILFunctionType PAITy =
      dyn_cast<SILFunctionType>(PAI->getCallee().getType().getSwiftType());

  // Emit a destroy value for each captured closure argument.
  ArrayRef<SILParameterInfo> Params = PAITy->getParameters();
  auto Args = PAI->getArguments();
  unsigned Delta = Params.size() - Args.size();

  for (unsigned AI = 0, AE = Args.size(); AI != AE; ++AI) {
    SILValue Arg = Args[AI];
    SILParameterInfo Param = Params[AI + Delta];
    if (Param.isIndirectMutating())
      continue;
    // Create a temporary and copy the argument into it, if:
    // - the argument stems from an alloc_stack
    // - the argument is consumed by the callee and is indirect
    //   (e.g. it is an @in argument)
    if (isa<AllocStackInst>(Arg) ||
        (Param.isConsumed() && Param.isIndirect())) {
      Builder.setInsertionPoint(PAI->getFunction()->begin()->begin());
      // Create a new temporary at the beginning of a function.
      auto *Tmp = Builder.createAllocStack(PAI->getLoc(), Arg.getType(),
                                           {/*Constant*/ true, AI});
      Builder.setInsertionPoint(PAI);
      // Copy argument into this temporary.
      Builder.createCopyAddr(PAI->getLoc(), Arg, Tmp,
                              IsTake_t::IsNotTake,
                              IsInitialization_t::IsInitialization);

      Tmps.push_back(SILValue(Tmp, 0));
      // If the temporary is non-trivial, we need to release it later.
      if (!Arg.getType().isTrivial(PAI->getModule()))
        needsReleases = true;
      ArgToTmp.insert(std::make_pair(Arg, SILValue(Tmp, 0)));
    }
  }

  if (needsReleases) {
    // Compute the set of endpoints, which will be used
    // to insert releases of temporaries.
    ValueLifetimeAnalysis VLA(PAI);
    Lifetime = VLA.computeFromDirectUses();
  }
}

/// Emit dealloc_stack for all temporaries.
void PartialApplyCombiner::deallocateTemporaries() {
  // Insert dealloc_stack instructions.
  TinyPtrVector<SILBasicBlock *> ExitBBs;
  findAllNonFailureExitBBs(PAI->getFunction(), ExitBBs);

  for (auto Op : Tmps) {
    for (auto *ExitBB : ExitBBs) {
      auto *Term = ExitBB->getTerminator();
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
    auto TmpType = Op.getType().getObjectType();
    if (TmpType.isTrivial(PAI->getModule()))
      continue;
    for (auto *EndPoint : Lifetime.LastUsers) {
      Builder.setInsertionPoint(next(SILBasicBlock::iterator(EndPoint)));
      if (!TmpType.isAddressOnly(PAI->getModule())) {
        auto *Load = Builder.createLoad(PAI->getLoc(), Op);
        Builder.createReleaseValue(PAI->getLoc(), Load);
      } else {
        Builder.createDestroyAddr(PAI->getLoc(), Op);
      }
    }
  }
}

/// Process an apply instruction which uses a partial_apply
/// as its callee.
void PartialApplyCombiner::processSingleApply(FullApplySite AI) {
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
    allocateTemporaries();
  }

  // Now, copy over the partial apply args.
  for (auto Op : PAI->getArguments()) {
    auto Arg = Op;
    // If there is new temporary for this argument, use it instead.
    if (isa<AllocStackInst>(Arg)) {
      if (ArgToTmp.count(Arg)) {
        Op = ArgToTmp.lookup(Arg);
      }
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
    if (!Arg.getType().isAddress()) {
      // Retain the argument as the callee may consume it.
      Builder.emitRetainValueOperation(PAI->getLoc(), Arg);
      // For non consumed parameters (e.g. guaranteed), we also need to
      // insert releases after each apply instruction that we create.
      if (!ParamInfo[ParamInfo.size() - PartialApplyArgs.size() + i].
            isConsumed())
        ToBeReleasedArgs.push_back(Arg);
    }
  }

  auto *F = FRI->getReferencedFunction();
  SILType FnType = F->getLoweredType();
  SILType ResultTy = F->getLoweredFunctionType()->getSILResult();
  ArrayRef<Substitution> Subs = PAI->getSubstitutions();
  if (!Subs.empty()) {
    FnType = FnType.substGenericArgs(PAI->getModule(), Subs);
    ResultTy = FnType.getAs<SILFunctionType>()->getSILResult();
  }

  FullApplySite NAI;
  if (auto *TAI = dyn_cast<TryApplyInst>(AI))
    NAI =
      Builder.createTryApply(AI.getLoc(), FRI, FnType, Subs, Args,
                              TAI->getNormalBB(), TAI->getErrorBB());
  else
    NAI =
      Builder.createApply(AI.getLoc(), FRI, FnType, ResultTy, Subs, Args,
                           cast<ApplyInst>(AI)->isNonThrowing());

  // We also need to release the partial_apply instruction itself because it
  // is consumed by the apply_instruction.
  if (auto *TAI = dyn_cast<TryApplyInst>(AI)) {
    Builder.setInsertionPoint(TAI->getNormalBB()->begin());
    for (auto Arg : ToBeReleasedArgs) {
      Builder.emitReleaseValueOperation(PAI->getLoc(), Arg);
    }
    Builder.createStrongRelease(AI.getLoc(), PAI);
    Builder.setInsertionPoint(TAI->getErrorBB()->begin());
    // Release the non-consumed parameters.
    for (auto Arg : ToBeReleasedArgs) {
      Builder.emitReleaseValueOperation(PAI->getLoc(), Arg);
    }
    Builder.createStrongRelease(AI.getLoc(), PAI);
    Builder.setInsertionPoint(AI.getInstruction());
  } else {
    // Release the non-consumed parameters.
    for (auto Arg : ToBeReleasedArgs) {
      Builder.emitReleaseValueOperation(PAI->getLoc(), Arg);
    }
    Builder.createStrongRelease(AI.getLoc(), PAI);
  }
  // Update the set endpoints.
  if (Lifetime.LastUsers.count(AI.getInstruction())) {
    Lifetime.LastUsers.remove(AI.getInstruction());
    Lifetime.LastUsers.insert(NAI.getInstruction());
  }

  SilCombiner->replaceInstUsesWith(*AI.getInstruction(), NAI.getInstruction());
  SilCombiner->eraseInstFromFunction(*AI.getInstruction());
}

/// Perform the apply{partial_apply(x,y)}(z) -> apply(z,x,y) peephole
/// by iterating over all uses of the partial_apply and searching
/// for the pattern to transform.
SILInstruction *PartialApplyCombiner::combine() {
  if (!canCombinePartialApply(PAI))
    return nullptr;

  // Only process partial apply if the callee is a known function.
  FRI = dyn_cast<FunctionRefInst>(PAI->getCallee());

  // Iterate over all uses of the partial_apply
  // and look for applies that use it as a callee.
  for (auto UI = PAI->use_begin(), UE = PAI->use_end(); UI != UE; ) {
    auto Use = *UI;
    ++UI;
    auto User = Use->getUser();
    // If this use of a partial_apply is not
    // an apply which uses it as a callee, bail.
    auto AI = FullApplySite::isa(User);
    if (!AI)
      continue;

    if (AI.getCallee() != PAI)
      continue;

    // We cannot handle generic apply yet. Bail.
    if (AI.hasSubstitutions())
      continue;

    processSingleApply(AI);
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
  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CFI->getOperand());
  if (!FRI)
    return nullptr;

  // Grab our relevant callee types...
  CanSILFunctionType SubstCalleeTy = AI.getSubstCalleeType();
  auto ConvertCalleeTy =
      CFI->getOperand().getType().castTo<SILFunctionType>();

  // ... and make sure they have no unsubstituted generics. If they do, bail.
  if (SubstCalleeTy->hasArchetype() || ConvertCalleeTy->hasArchetype())
    return nullptr;

  // Ok, we can now perform our transformation. Grab AI's operands and the
  // relevant types from the ConvertFunction function type and AI.
  Builder.setCurrentDebugScope(AI.getDebugScope());
  OperandValueArrayRef Ops = AI.getArgumentsWithoutIndirectResult();
  auto OldOpTypes = SubstCalleeTy->getParameterSILTypes();
  auto NewOpTypes = ConvertCalleeTy->getParameterSILTypes();

  assert(Ops.size() == OldOpTypes.size() &&
         "Ops and op types must have same size.");
  assert(Ops.size() == NewOpTypes.size() &&
         "Ops and op types must have same size.");

  llvm::SmallVector<SILValue, 8> Args;
  for (unsigned i = 0, e = Ops.size(); i != e; ++i) {
    SILValue Op = Ops[i];
    SILType OldOpType = OldOpTypes[i];
    SILType NewOpType = NewOpTypes[i];

    // Convert function takes refs to refs, address to addresses, and leaves
    // other types alone.
    if (OldOpType.isAddress()) {
      assert(NewOpType.isAddress() && "Addresses should map to addresses.");
      auto UAC = Builder.createUncheckedAddrCast(AI.getLoc(), Op, NewOpType);
      Args.push_back(UAC);
    } else if (OldOpType.isHeapObjectReferenceType()) {
      assert(NewOpType.isHeapObjectReferenceType() &&
             "refs should map to refs.");
      auto URC = Builder.createUncheckedRefCast(AI.getLoc(), Op, NewOpType);
      Args.push_back(URC);
    } else {
      Args.push_back(Op);
    }
  }

  SILType CCSILTy = SILType::getPrimitiveObjectType(ConvertCalleeTy);
  // Create the new apply inst.
  SILInstruction *NAI;
  if (auto *TAI = dyn_cast<TryApplyInst>(AI))
    NAI = Builder.createTryApply(AI.getLoc(), FRI, CCSILTy,
                                 ArrayRef<Substitution>(), Args,
                                 TAI->getNormalBB(), TAI->getErrorBB());
  else
    NAI = Builder.createApply(AI.getLoc(), FRI, CCSILTy,
                              ConvertCalleeTy->getSILResult(),
                              ArrayRef<Substitution>(), Args,
                              cast<ApplyInst>(AI)->isNonThrowing());
  return NAI;
}

bool
SILCombiner::recursivelyCollectARCUsers(UserListTy &Uses, ValueBase *Value) {
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
      if (recursivelyCollectARCUsers(Uses, Inst))
        continue;
    }
    return false;
  }
  return true;
}

void SILCombiner::eraseApply(FullApplySite FAS, const UserListTy &Users) {
  // Make sure to release and destroy any owned or in-arguments.
  auto FuncType = FAS.getOrigCalleeType();
  assert(FuncType->getParameters().size() == FAS.getNumArguments() &&
         "mismatching number of arguments");
  for (int i = 0, e = FAS.getNumArguments(); i < e; ++i) {
    SILParameterInfo PI = FuncType->getParameters()[i];
    auto Arg = FAS.getArgument(i);
    switch (PI.getConvention()) {
      case ParameterConvention::Indirect_In:
        Builder.createDestroyAddr(FAS.getLoc(), Arg);
        break;
      case ParameterConvention::Direct_Owned:
        Builder.createReleaseValue(FAS.getLoc(), Arg);
        break;
      case ParameterConvention::Indirect_In_Guaranteed:
      case ParameterConvention::Indirect_Inout:
      case ParameterConvention::Indirect_InoutAliasable:
      case ParameterConvention::Indirect_Out:
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Direct_Deallocating:
      case ParameterConvention::Direct_Guaranteed:
        break;
    }
  }

  // Erase all of the reference counting instructions (in reverse order to have
  // no dangling uses).
  for (auto rit = Users.rbegin(), re = Users.rend(); rit != re; ++rit)
    eraseInstFromFunction(**rit);

  // And the Apply itself.
  eraseInstFromFunction(*FAS.getInstruction());
}

SILInstruction *
SILCombiner::optimizeConcatenationOfStringLiterals(ApplyInst *AI) {
  // String literals concatenation optimizer.
  return tryToConcatenateStrings(AI, Builder);
}

/// Find an init_open_existential_addr or open_existential_addr, which
/// is used to initialize a given alloc_stack value.
static SILValue getInitOrOpenExistential(AllocStackInst *ASI, SILValue &Src) {
  CopyAddrInst *FoundCAI = nullptr;
  InitExistentialAddrInst *FoundIEAI = nullptr;
  bool isLegal = true;
  // Check that this alloc_stack is initialized only once.
  for (auto Use : ASI->getUses()) {
    auto *User = Use->getUser();
    if (isa<DeallocStackInst>(User) ||
        isa<DestroyAddrInst>(User) || isa<WitnessMethodInst>(User) ||
        isa<DeinitExistentialAddrInst>(User) || isa<OpenExistentialAddrInst>(User) ||
        isa<ApplyInst>(User))
      continue;
    if (auto *CAI = dyn_cast<CopyAddrInst>(User)) {
      if (!FoundCAI && !FoundIEAI) {
        if (CAI->getDest().getDef() == ASI)
          FoundCAI = CAI;
      }
      continue;
    }
    else if (auto *IEAI = dyn_cast<InitExistentialAddrInst>(User)) {
      if (!FoundIEAI && !FoundCAI) {
        FoundIEAI = IEAI;
        continue;
      }
    }
    isLegal = false;
    break;
  }

  SILValue SrcValue;

  if (isLegal && FoundCAI) {
    // Try to derive the type from the copy_addr that was used to
    // initialize the alloc_stack.
    SrcValue = FoundCAI->getSrc();
    if (auto *ASI = dyn_cast<AllocStackInst>(SrcValue)) {
      SILValue Tmp;
      SrcValue = getInitOrOpenExistential(ASI, Tmp);
    }
  }

  if (isLegal && FoundIEAI) {
    SrcValue = FoundIEAI;
  }

  if (!SrcValue)
    return SILValue();

  if (auto *OEAI = dyn_cast<OpenExistentialAddrInst>(SrcValue)) {
    Src = OEAI->getOperand();
    return OEAI;
  }
  if (auto *IEAI = dyn_cast<InitExistentialAddrInst>(SrcValue)) {
    Src = IEAI->getOperand();
    return IEAI;
  }

  return SrcValue;
}

/// find the init_existential, which could be used to  determine a concrete
/// type of the \p Self.
static SILInstruction *findInitExistential(FullApplySite AI, SILValue Self,
                                           SILType &InstanceType) {
  SILInstruction *InitExistential = nullptr;

  if (auto *Instance = dyn_cast<AllocStackInst>(Self)) {
    SILValue Src;
    auto Existential = getInitOrOpenExistential(Instance, Src);
    if (Existential)
      Self = Existential;
  }

  if (auto *Instance = dyn_cast<OpenExistentialAddrInst>(Self)) {
    auto Op = Instance->getOperand();
    if (auto *ASI = dyn_cast<AllocStackInst>(Op)) {
      SILValue Src;
      if (getInitOrOpenExistential(ASI, Src)) {
        if (Src)
          Op = Src;
      }
    }

    for (auto Use : Op.getUses()) {
      SILValue User = Use->getUser();

      if (auto *IE = dyn_cast<InitExistentialAddrInst>(User)) {
        // IE should dominate Instance.
        // Without a DomTree we want to be very defensive
        // and only allow this optimization when it is used
        // inside the same BB.
        if (IE->getParent() != AI.getParent())
          continue;

        InstanceType = Instance->getType();
        InitExistential = IE;
      }
    }
  }

  if (auto *Instance = dyn_cast<OpenExistentialRefInst>(Self)) {
    if (auto *IE = dyn_cast<InitExistentialRefInst>(Instance->getOperand())) {
      // IE should dominate Instance.
      // Without a DomTree we want to be very defensive
      // and only allow this optimization when it is used
      // inside the same BB.
      if (IE->getParent() != AI.getParent())
        return nullptr;
      InstanceType = Instance->getType();
      InitExistential = IE;
    }
  }

  return InitExistential;
}

/// Create a new apply instructions that uses the concrete type instead
/// of the existential type.
SILInstruction *
SILCombiner::createApplyWithConcreteType(FullApplySite AI,
                                         SILValue NewSelf,
                                         SILValue Self,
                                         CanType ConcreteType,
                                         ProtocolConformance *Conformance,
                                         SILType InstanceType) {
  // Create a set of arguments.
  SmallVector<SILValue, 8> Args;
  for (auto Arg : AI.getArgumentsWithoutSelf()) {
    Args.push_back(Arg);
  }
  Args.push_back(NewSelf);

  // Form a new set of substitutions where Self is
  // replaced by a concrete type.
  SmallVector<Substitution, 8> Substitutions;
  for (auto Subst : AI.getSubstitutions()) {
    if (Subst.getReplacement().getCanonicalTypeOrNull() ==
        Self.getType().getSwiftRValueType()) {
      auto Conformances = AI.getModule().getASTContext()
                            .Allocate<ProtocolConformance*>(1);
      Conformances[0] = Conformance;
      Substitution NewSubst(Subst.getArchetype(),
                            ConcreteType,
                            Conformances);
      Substitutions.push_back(NewSubst);
    } else
      Substitutions.push_back(Subst);
  }

  SILType SubstCalleeType = AI.getSubstCalleeSILType();

  SILType NewSubstCalleeType;

  auto FnTy = AI.getCallee().getType().getAs<SILFunctionType>();
  if (FnTy && FnTy->isPolymorphic()) {
    // Handle polymorphic functions by properly substituting
    // their parameter types.
    CanSILFunctionType SFT = FnTy->substGenericArgs(
                                        AI.getModule(),
                                        AI.getModule().getSwiftModule(),
                                        Substitutions);
    NewSubstCalleeType = SILType::getPrimitiveObjectType(SFT);
  } else {
    TypeSubstitutionMap TypeSubstitutions;
    TypeSubstitutions[InstanceType.getSwiftType().getPointer()] = ConcreteType;
    NewSubstCalleeType = SubstCalleeType.subst(AI.getModule(),
                                               AI.getModule().getSwiftModule(),
                                               TypeSubstitutions);
  }

  FullApplySite NewAI;
  Builder.setCurrentDebugScope(AI.getDebugScope());

  if (auto *TAI = dyn_cast<TryApplyInst>(AI))
    NewAI = Builder.createTryApply(AI.getLoc(), AI.getCallee(),
                                    NewSubstCalleeType,
                                    Substitutions, Args,
                                    TAI->getNormalBB(), TAI->getErrorBB());
  else
    NewAI = Builder.createApply(AI.getLoc(), AI.getCallee(),
                                 NewSubstCalleeType,
                                 AI.getType(), Substitutions, Args,
                                 cast<ApplyInst>(AI)->isNonThrowing());

  if (isa<ApplyInst>(NewAI))
    replaceInstUsesWith(*AI.getInstruction(), NewAI.getInstruction(), 0);
  eraseInstFromFunction(*AI.getInstruction());

  return NewAI.getInstruction();
}

/// Derive a concrete type of self and conformance from the init_existential
/// instruction.
static std::pair<ProtocolConformance *, CanType>
getConformanceAndConcreteType(FullApplySite AI,
                              SILInstruction *InitExistential,
                              ProtocolDecl *Protocol,
                              SILValue &NewSelf,
                              ArrayRef<ProtocolConformance*> &Conformances) {
  // Try to derive the concrete type of self from the found init_existential.
  CanType ConcreteType;
  if (auto IE = dyn_cast<InitExistentialAddrInst>(InitExistential)) {
    Conformances = IE->getConformances();
    ConcreteType = IE->getFormalConcreteType();
    NewSelf = IE;
  } else if (auto IER = dyn_cast<InitExistentialRefInst>(InitExistential)) {
    Conformances = IER->getConformances();
    ConcreteType = IER->getFormalConcreteType();
    NewSelf = IER->getOperand();
  }

  if (Conformances.empty())
    return std::make_pair(nullptr, CanType());

  // If ConcreteType depends on any archetypes, then propagating it does not
  // help resolve witness table lookups. Catch these cases before calling
  // gatherAllSubstitutions, which only works on nominal types.
  if (ConcreteType->hasArchetype())
    return std::make_pair(nullptr, CanType());

  // Check the substitutions.
  auto ConcreteTypeSubsts = ConcreteType->gatherAllSubstitutions(
      AI.getModule().getSwiftModule(), nullptr);
  if (!ConcreteTypeSubsts.empty()) {
    // Bail if any generic types parameters of the concrete type are unbound.
    if (hasUnboundGenericTypes(ConcreteTypeSubsts))
      return std::make_pair(nullptr, CanType());
    // At this point we know that all replacements use concrete types
    // and therefore the whole Lookup type is concrete. So, we can
    // propagate it, because we know how to devirtualize it.
  }

  // Find the conformance related to witness_method.
  ProtocolConformance *Conformance = nullptr;
  for (auto Con : Conformances) {
    if (Con->getProtocol() == Protocol) {
      Conformance = Con;
      break;
    }
  }

  return std::make_pair(Conformance, ConcreteType);
}

/// Propagate information about a concrete type from init_existential_addr
/// or init_existential_ref into witness_method conformances and into
/// apply instructions.
/// This helps the devirtualizer to replace witness_method by
/// class_method instructions and then devirtualize.
SILInstruction *
SILCombiner::propagateConcreteTypeOfInitExistential(FullApplySite AI,
    ProtocolDecl *Protocol,
    std::function<void(CanType , ProtocolConformance *)> Propagate) {

  // Get the self argument.
  SILValue Self;
  if (auto *Apply = dyn_cast<ApplyInst>(AI)) {
    if (Apply->hasSelfArgument())
      Self = Apply->getSelfArgument();
  } else if (auto *Apply = dyn_cast<TryApplyInst>(AI)) {
    if (Apply->hasSelfArgument())
      Self = Apply->getSelfArgument();
  }

  assert (Self && "Self argument should be present");

  // Try to find the init_existential, which could be used to
  // determine a concrete type of the self.
  SILType InstanceType;
  SILInstruction *InitExistential = findInitExistential(AI, Self, InstanceType);
  if (!InitExistential)
    return nullptr;

  // Try to derive the concrete type of self and a related conformance from
  // the found init_existential.
  ArrayRef<ProtocolConformance*> Conformances;
  auto NewSelf = SILValue();
  auto ConformanceAndConcreteType =
      getConformanceAndConcreteType(AI, InitExistential,
                                    Protocol, NewSelf, Conformances);
  auto ConcreteType = ConformanceAndConcreteType.second;
  auto Conformance = ConformanceAndConcreteType.first;
  if (!Conformance)
    return nullptr;

  // Propagate the concrete type into the callee-operand if required.
  Propagate(ConcreteType, Conformance);

  // Create a new apply instructions that uses the concrete type instead
  // of the existential type.
  return createApplyWithConcreteType(AI, NewSelf, Self,
                                     ConcreteType, Conformance, InstanceType);
}

SILInstruction *
SILCombiner::propagateConcreteTypeOfInitExistential(FullApplySite AI,
                                                    WitnessMethodInst *WMI) {
  // Check if it is legal to perform the propagation.
  if (WMI->getConformance())
    return nullptr;

  // Don't specialize Apply instructions that return the Self type.
  // Notice that it is sufficient to compare the return type to the
  // substituted type because types that depend on the Self type are
  // not allowed (for example [Self] is not allowed).
  if (AI.getType().getSwiftType().getLValueOrInOutObjectType() ==
      WMI->getLookupType())
    return nullptr;

  // We need to handle the Self return type.
  // In we find arguments that are not the 'self' argument and if
  // they are of the Self type then we abort the optimization.
  for (auto Arg : AI.getArgumentsWithoutSelf()) {
    if (Arg.getType().getSwiftType().getLValueOrInOutObjectType() ==
        WMI->getLookupType())
      return nullptr;
  }

  // Obtain the protocol whose which should be used by the conformance.
  auto *PD = WMI->getLookupProtocol();

  // Propagate the concrete type into a callee-operand, which is a
  // witness_method instruction.
  auto PropagateIntoOperand = [this, &WMI] (CanType ConcreteType,
                                            ProtocolConformance *Conformance) {
    SILValue OptionalExistential =
        WMI->hasOperand() ? WMI->getOperand() : SILValue();
    auto *NewWMI = Builder.createWitnessMethod(WMI->getLoc(),
                                                ConcreteType,
                                                Conformance, WMI->getMember(),
                                                WMI->getType(),
                                                OptionalExistential,
                                                WMI->isVolatile());
    replaceInstUsesWith(*WMI, NewWMI, 0);
    eraseInstFromFunction(*WMI);
  };

  // Try to perform the propagation.
  return propagateConcreteTypeOfInitExistential(AI, PD, PropagateIntoOperand);
}


SILInstruction *
SILCombiner::propagateConcreteTypeOfInitExistential(FullApplySite AI) {
  // Check if it is legal to perform the propagation.
  if (!AI.hasSubstitutions())
    return nullptr;
  auto *Callee = AI.getCalleeFunction();
  if (!Callee || !Callee->getDeclContext())
    return nullptr;

  // Bail, if there is no self argument.
  SILValue Self;
  if (auto *Apply = dyn_cast<ApplyInst>(AI)) {
    if (Apply->hasSelfArgument())
      Self = Apply->getSelfArgument();
  } else if (auto *Apply = dyn_cast<TryApplyInst>(AI)) {
    if (Apply->hasSelfArgument())
      Self = Apply->getSelfArgument();
  }
  if (!Self)
    return nullptr;

  // We need to handle the Self return type.
  // In we find arguments that are not the 'self' argument and if
  // they are of the Self type then we abort the optimization.
  for (auto Arg : AI.getArgumentsWithoutSelf()) {
    if (Arg.getType().getSwiftType().getLValueOrInOutObjectType() ==
        AI.getArguments().back().getType().getSwiftRValueType())
      return nullptr;
  }

  // Obtain the protocol whose which should be used by the conformance.
  auto *AFD = dyn_cast<AbstractFunctionDecl>(Callee->getDeclContext());
  if (!AFD)
    return nullptr;
  auto *PD = AFD->getDeclContext()->isProtocolOrProtocolExtensionContext();


  // No need to propagate anything into the callee operand.
  auto PropagateIntoOperand = [] (CanType ConcreteType,
                                  ProtocolConformance *Conformance) {};

  // Try to perform the propagation.
  return propagateConcreteTypeOfInitExistential(AI, PD, PropagateIntoOperand);
}

/// Optimize thin_func_to_ptr->ptr_to_thin_func casts into a type substituted
/// apply.
/// This kind of code arises in generic materializeForSet code that was
/// specialized for a concrete type.
///
/// Note: this is not as general as it should be. The general solution is the
/// introduction of a partial_apply_thin_recoverable (an instruction that
/// partially applies a type and returns a thin_function) as suggested in
/// SILGenBuiltin.cpp.
///
/// %208 = thin_function_to_pointer %207 :
///  $@convention(thin) <τ_0_0> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer,
///                  @inout UnsafeMutableBufferPointer<τ_0_0>,
///                  @thick UnsafeMutableBufferPointer<τ_0_0>.Type) -> ()
///                  to $Builtin.RawPointer
/// %209 = pointer_to_thin_function %217 : $Builtin.RawPointer to
///  $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer,
///          @inout UnsafeMutableBufferPointer<Int>,
///          @thick UnsafeMutableBufferPointer<Int>.Type) -> ()
/// apply %209(%227, %200#1, %0, %224) : $@convention(thin) (Builtin.RawPointer,
///  @inout Builtin.UnsafeValueBuffer, @inout UnsafeMutableBufferPointer<Int>,
///  @thick UnsafeMutableBufferPointer<Int>.Type) -> ()
///
///  => apply %207<Int>(%227, ...)
static ApplyInst *optimizeCastThroughThinFunctionPointer(
    SILBuilder &Builder, ApplyInst *AI, FunctionRefInst *OrigThinFun,
    PointerToThinFunctionInst *CastedThinFun) {

  // The original function type needs to be polymorphic.
  auto ConvertCalleeTy = OrigThinFun->getType().castTo<SILFunctionType>();
  assert(ConvertCalleeTy);
  if (!ConvertCalleeTy->isPolymorphic())
    return nullptr;

  // Need to have four parameters.
  auto OrigParams = ConvertCalleeTy->getParameters();
  if (OrigParams.size() != 4)
    return nullptr;

  // There must only be one parameter to substitute.
  auto *ReferencedFunction = OrigThinFun->getReferencedFunction();
  assert(ReferencedFunction);
  if (ReferencedFunction->isExternalDeclaration())
    return nullptr;
  auto Params = ReferencedFunction->getContextGenericParams()->getParams();
  if (Params.size() != 1)
    return nullptr;

  // Get the concrete type from the casted to function.
  auto CastedFunTy = CastedThinFun->getType().castTo<SILFunctionType>();
  auto CastedParams = CastedFunTy->getParameters();
  if (CastedParams.size() != 4)
    return nullptr;

  // The fourth parameter is a metatype of a bound generic type. Use it to
  // obtain  the type substitutions to apply.
  auto MetaTy = dyn_cast<MetatypeType>(CastedParams[3].getType());
  if (!MetaTy)
    return nullptr;

  // Get the bound generic type from the metatype.
  auto BoundGenericInstTy = dyn_cast_or_null<BoundGenericType>(
      MetaTy->getInstanceType().getCanonicalTypeOrNull());
  if (!BoundGenericInstTy)
    return nullptr;

  // The bound generic type will carry the substitutions to apply.
  auto Subs = BoundGenericInstTy->getSubstitutions(
      AI->getModule().getSwiftModule(), nullptr);

  if (Subs.size() == 0)
    return nullptr;

  // We expect one type variable to be substituted. The substitution might have
  // recursive substitutions. Recognize and allow the case of one substitution
  // with recursive substitutions.
  // Container<T>
  //   T = ...
  //   T.A = ...
  //   T.A.C = ...
  if (Subs.size() != 1) {
    SmallPtrSet<ArchetypeType *, 16> Archetypes;
    bool SawPrimary = false;
    // Collect all the archetypes and make sure there is only one primary.
    for (unsigned i = 0, e = Subs.size(); i != e; ++i) {
      auto *AT = Subs[i].getArchetype();
      Archetypes.insert(AT);
      // Two primary arche types. We can't handle this case.
      if (SawPrimary && AT->isPrimary())
        return nullptr;
      else if (AT->isPrimary())
        SawPrimary = true;
    }

    // Make sure all the non primary archetypes have a parent archetype in the
    // set.
    for (unsigned i = 0, e = Subs.size(); i != e; ++i) {
      auto *AT = Subs[i].getArchetype();
      // Ignore the one primary archetype.
      if (AT->isPrimary())
        continue;
      if (!Archetypes.count(AT))
          return nullptr;
    }
  }

  SmallVector<SILValue, 16> Args;
  for (auto Arg : AI->getArguments())
    Args.push_back(Arg);

  auto NewSubstCalleeType =
      SILType::getPrimitiveObjectType(ConvertCalleeTy->substGenericArgs(
          AI->getModule(), AI->getModule().getSwiftModule(), Subs));

  Builder.setCurrentDebugScope(AI->getDebugScope());
  ApplyInst *NewApply = Builder.createApply(
      AI->getLoc(), OrigThinFun, NewSubstCalleeType, AI->getType(), Subs, Args,
                                             AI->isNonThrowing());

  return NewApply;
}

/// \brief Check that all users of the apply are retain/release ignoring one
/// user.
static bool
hasOnlyRetainReleaseUsers(ApplyInst *AI, SILInstruction *IgnoreUser,
                          SmallVectorImpl<SILInstruction *> &Users) {
  for (auto *Use : getNonDebugUses(*AI)) {
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

/// \brief We only know how to simulate reference call effects for unary
/// function calls that take their argument @owned or @guaranteed and return an
/// @owned value.
static bool knowHowToEmitReferenceCountInsts(ApplyInst *Call) {
  if (Call->getNumArguments() != 1)
    return false;

  FunctionRefInst *FRI = cast<FunctionRefInst>(Call->getCallee());
  SILFunction *F = FRI->getReferencedFunction();
  auto FnTy = F->getLoweredFunctionType();

  // Look at the result type.
  auto ResultInfo = FnTy->getResult();
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

/// \brief Add reference counting operations equal to the effect of the call.
static void emitMatchingRCAdjustmentsForCall(ApplyInst *Call, SILValue OnX) {
  FunctionRefInst *FRI = cast<FunctionRefInst>(Call->getCallee());
  SILFunction *F = FRI->getReferencedFunction();
  auto FnTy = F->getLoweredFunctionType();
  auto ResultInfo = FnTy->getResult();
  (void) ResultInfo;

  assert(ResultInfo.getConvention() == ResultConvention::Owned &&
         "Expect a @owned return");
  assert(Call->getNumArguments() == 1 && "Expect a unary call");

  // Emit a retain for the @owned return.
  SILBuilderWithScope Builder(Call);
  Builder.createRetainValue(Call->getLoc(), OnX);

  // Emit a release for the @owned parameter, or none for a @guaranteed
  // parameter.
  auto Params = FnTy->getParameters();
  (void) Params;
  assert(Params.size() == 1 && "Expect one parameter");
  auto ParamInfo = FnTy->getParameters()[0].getConvention();
  assert(ParamInfo == ParameterConvention::Direct_Owned ||
         ParamInfo == ParameterConvention::Direct_Guaranteed);

  if (ParamInfo == ParameterConvention::Direct_Owned)
    Builder.createReleaseValue(Call->getLoc(), OnX);
}

static bool isCastTypeKnownToSucceed(SILType Type, SILModule &Mod) {
  auto *M = Mod.getSwiftModule();
  return M->getASTContext()
      .getBridgedToObjC(M, Type.getSwiftRValueType(), nullptr)
      .hasValue();
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

  // We need to know that the cast will succeed.
  if (!isCastTypeKnownToSucceed(FInverse->getArgument(0).getType(),
                                FInverse->getModule()) ||
      !isCastTypeKnownToSucceed(FInverse->getType(), FInverse->getModule()))
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
  if (F->getArgument(0).getType() != FInverse->getType())
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
      SILBuilderWithScope(User).createRetainValue(User->getLoc(), X);
      eraseInstFromFunction(*User);
      continue;
    }
    if (isa<StrongReleaseInst>(User)) {
      SILBuilderWithScope(User).createReleaseValue(User->getLoc(), X);
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
  replaceInstUsesWith(*FInverse, X.getDef());

  // Remove the calls.
  eraseInstFromFunction(*FInverse);
  eraseInstFromFunction(*F);

  return true;
}

SILInstruction *SILCombiner::visitApplyInst(ApplyInst *AI) {
  Builder.setCurrentDebugScope(AI->getDebugScope());
  // apply{partial_apply(x,y)}(z) -> apply(z,x,y) is triggered
  // from visitPartialApplyInst(), so bail here.
  if (isa<PartialApplyInst>(AI->getCallee()))
    return nullptr;

  if (auto *CFI = dyn_cast<ConvertFunctionInst>(AI->getCallee()))
    return optimizeApplyOfConvertFunctionInst(AI, CFI);

  if (auto *CastedThinFun =
          dyn_cast<PointerToThinFunctionInst>(AI->getCallee()))
    if (auto *Ptr =
            dyn_cast<ThinFunctionToPointerInst>(CastedThinFun->getOperand()))
      if (auto *OrigThinFun = dyn_cast<FunctionRefInst>(Ptr->getOperand()))
        if (auto *NewAI = optimizeCastThroughThinFunctionPointer(
                Builder, AI, OrigThinFun, CastedThinFun)) {
          replaceInstUsesWith(*AI, NewAI, 0);
          eraseInstFromFunction(*AI);
          return nullptr;
        }

  // Optimize readonly functions with no meaningful users.
  SILFunction *SF = AI->getCalleeFunction();
  if (SF && SF->getEffectsKind() < EffectsKind::ReadWrite) {
    UserListTy Users;
    if (recursivelyCollectARCUsers(Users, AI)) {
      eraseApply(AI, Users);
      return nullptr;
    }
    // We found a user that we can't handle.
  }

  if (SF) {
    if (SF->getEffectsKind() < EffectsKind::ReadWrite) {
      // Try to optimize string concatenation.
      if (auto I = optimizeConcatenationOfStringLiterals(AI)) {
        return I;
      }
    }
    if (SF->hasSemanticsAttr("array.uninitialized")) {
      UserListTy Users;
      // If the uninitialized array is only written into then it can be removed.
      if (recursivelyCollectARCUsers(Users, AI)) {
        eraseApply(AI, Users);
        return nullptr;
      }
    }
  }


  // (apply (thin_to_thick_function f)) to (apply f)
  if (auto *TTTFI = dyn_cast<ThinToThickFunctionInst>(AI->getCallee())) {
    // TODO: Handle substitutions and indirect results
    if (AI->hasSubstitutions() || AI->hasIndirectResult())
      return nullptr;
    SmallVector<SILValue, 4> Arguments;
    for (auto &Op : AI->getArgumentOperands()) {
      Arguments.push_back(Op.get());
    }
    // The type of the substitution is the source type of the thin to thick
    // instruction.
    SILType substTy = TTTFI->getOperand().getType();
    auto *NewAI = Builder.createApply(AI->getLoc(), TTTFI->getOperand(),
                                      substTy, AI->getType(),
                                      AI->getSubstitutions(), Arguments,
                                      AI->isNonThrowing());
    return NewAI;
  }

  // (apply (witness_method)) -> propagate information about
  // a concrete type from init_existential_addr or init_existential_ref.
  if (auto *WMI = dyn_cast<WitnessMethodInst>(AI->getCallee())) {
    propagateConcreteTypeOfInitExistential(AI, WMI);
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
  if (!recursivelyCollectARCUsers(AcceptedUses, NormalBB->getBBArg(0)))
    return false;
  if (!recursivelyCollectARCUsers(AcceptedUses, ErrorBB->getBBArg(0)))
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
  SILFunction *Fn = AI->getCalleeFunction();
  if (Fn && Fn->getEffectsKind() < EffectsKind::ReadWrite) {
    UserListTy Users;
    if (isTryApplyResultNotUsed(Users, AI)) {
      SILBasicBlock *BB = AI->getParent();
      SILBasicBlock *NormalBB = AI->getNormalBB();
      SILBasicBlock *ErrorBB = AI->getErrorBB();
      SILLocation Loc = AI->getLoc();
      Builder.setInsertionPoint(BB);
      Builder.setCurrentDebugScope(AI->getDebugScope());
      eraseApply(AI, Users);

      // Replace the try_apply with a cond_br false, which will be removed by
      // SimplifyCFG. We don't want to modify the CFG in SILCombine.
      auto *TrueLit = Builder.createIntegerLiteral(Loc,
                SILType::getBuiltinIntegerType(1, Builder.getASTContext()), 0);
      Builder.createCondBranch(Loc, TrueLit, NormalBB, ErrorBB);

      NormalBB->eraseBBArg(0);
      ErrorBB->eraseBBArg(0);
      return nullptr;
    }
    // We found a user that we can't handle.
  }

  // (try_apply (thin_to_thick_function f)) to (try_apply f)
  if (auto *TTTFI = dyn_cast<ThinToThickFunctionInst>(AI->getCallee())) {
    // TODO: Handle substitutions and indirect results
    if (AI->hasSubstitutions() || AI->hasIndirectResult())
      return nullptr;
    SmallVector<SILValue, 4> Arguments;
    for (auto &Op : AI->getArgumentOperands()) {
      Arguments.push_back(Op.get());
    }
    // The type of the substitution is the source type of the thin to thick
    // instruction.
    SILType substTy = TTTFI->getOperand().getType();
    auto *NewAI = Builder.createTryApply(AI->getLoc(), TTTFI->getOperand(),
                                         substTy,
                                         AI->getSubstitutions(), Arguments,
                                         AI->getNormalBB(), AI->getErrorBB());
    return NewAI;
  }

  // (apply (witness_method)) -> propagate information about
  // a concrete type from init_existential_addr or init_existential_ref.
  if (auto *WMI = dyn_cast<WitnessMethodInst>(AI->getCallee())) {
    propagateConcreteTypeOfInitExistential(AI, WMI);
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

