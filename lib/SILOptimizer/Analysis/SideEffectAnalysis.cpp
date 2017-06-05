//===--- SideEffectAnalysis.cpp - SIL Side Effect Analysis ----------------===//
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

#define DEBUG_TYPE "sil-sea"
#include "swift/SILOptimizer/Analysis/SideEffectAnalysis.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/Analysis/FunctionOrder.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SIL/SILArgument.h"

using namespace swift;

using FunctionEffects = SideEffectAnalysis::FunctionEffects;
using Effects = SideEffectAnalysis::Effects;
using MemoryBehavior = SILInstruction::MemoryBehavior;

MemoryBehavior
FunctionEffects::getMemBehavior(RetainObserveKind ScanKind) const {

  bool Observe = (ScanKind == RetainObserveKind::ObserveRetains);
  if ((Observe && mayAllocObjects()) || mayReadRC())
    return MemoryBehavior::MayHaveSideEffects;
  
  // Start with the global effects.
  auto Behavior = GlobalEffects.getMemBehavior(ScanKind);
  
  // Add effects from the parameters.
  for (auto &ParamEffect : ParamEffects) {
    MemoryBehavior ArgBehavior = ParamEffect.getMemBehavior(ScanKind);

    Behavior = combineMemoryBehavior(Behavior, ArgBehavior);

    // Stop the scan if we've reached the highest level of side effect.
    if (Behavior == MemoryBehavior::MayHaveSideEffects)
      break;
  }
  return Behavior;
}

bool FunctionEffects::mergeFrom(const FunctionEffects &RHS) {
  bool Changed = mergeFlags(RHS);
  Changed |= GlobalEffects.mergeFrom(RHS.GlobalEffects);
  Changed |= LocalEffects.mergeFrom(RHS.LocalEffects);
  // In case of an external function, the RHS may have 0 arguments.
  unsigned NumArgs = RHS.ParamEffects.size();
  for (unsigned Idx = 0; Idx < NumArgs; Idx++) {
    // In case of a partial_apply, the RHS (= callee) may have more arguments
    // than the apply instruction.
    if (Idx < ParamEffects.size()) {
      Changed |= ParamEffects[Idx].mergeFrom(RHS.ParamEffects[Idx]);
    } else {
      Changed |= GlobalEffects.mergeFrom(RHS.ParamEffects[Idx]);
    }
  }
  return Changed;
}

bool FunctionEffects::mergeFromApply(
                  const FunctionEffects &ApplyEffects, FullApplySite FAS) {
  return mergeFromApply(ApplyEffects, FAS.getInstruction());
}

bool FunctionEffects::mergeFromApply(
                  const FunctionEffects &ApplyEffects, SILInstruction *AS) {
  bool Changed = mergeFlags(ApplyEffects);
  Changed |= GlobalEffects.mergeFrom(ApplyEffects.GlobalEffects);
  auto FAS = FullApplySite::isa(AS);
  unsigned numCallerArgs = FAS ? FAS.getNumArguments() : 1;
  unsigned numCalleeArgs = ApplyEffects.ParamEffects.size();
  assert(numCalleeArgs >= numCallerArgs);
  for (unsigned Idx = 0; Idx < numCalleeArgs; Idx++) {
    // Map the callee argument effects to parameters of this function.
    // If there are more callee parameters than arguments it means that the
    // callee is the result of a partial_apply.
    Effects *E = (Idx < numCallerArgs ? getEffectsOn(FAS ? FAS.getArgument(Idx) : AS->getOperand(Idx)) :
                  &GlobalEffects);
    Changed |= E->mergeFrom(ApplyEffects.ParamEffects[Idx]);
  }
  return Changed;
}

void FunctionEffects::dump() const {
  llvm::errs() << *this << '\n';
}

static SILValue skipAddrProjections(SILValue V) {
  for (;;) {
    switch (V->getKind()) {
      case ValueKind::IndexAddrInst:
      case ValueKind::IndexRawPointerInst:
      case ValueKind::StructElementAddrInst:
      case ValueKind::TupleElementAddrInst:
      case ValueKind::RefElementAddrInst:
      case ValueKind::RefTailAddrInst:
      case ValueKind::ProjectBoxInst:
      case ValueKind::UncheckedTakeEnumDataAddrInst:
      case ValueKind::PointerToAddressInst:
        V = cast<SILInstruction>(V)->getOperand(0);
        break;
      default:
        return V;
    }
  }
  llvm_unreachable("there is no escape from an infinite loop");
}

static SILValue skipValueProjections(SILValue V) {
  for (;;) {
    switch (V->getKind()) {
      case ValueKind::StructExtractInst:
      case ValueKind::TupleExtractInst:
      case ValueKind::UncheckedEnumDataInst:
      case ValueKind::UncheckedTrivialBitCastInst:
        V = cast<SILInstruction>(V)->getOperand(0);
        break;
      default:
        return V;
    }
  }
  llvm_unreachable("there is no escape from an infinite loop");
}

Effects *FunctionEffects::getEffectsOn(SILValue Addr) {
  SILValue BaseAddr = skipValueProjections(skipAddrProjections(Addr));
  switch (BaseAddr->getKind()) {
  case swift::ValueKind::SILFunctionArgument: {
    // Can we associate the address to a function parameter?
    auto *Arg = cast<SILFunctionArgument>(BaseAddr);
    return &ParamEffects[Arg->getIndex()];
    break;
    }
    case ValueKind::AllocStackInst:
    case ValueKind::AllocRefInst:
    case ValueKind::AllocRefDynamicInst:
    case ValueKind::AllocBoxInst:
      // Effects on locally allocated storage.
      return &LocalEffects;
    default:
      break;
  }
  // Everything else.
  return &GlobalEffects;
}

bool SideEffectAnalysis::getDefinedEffects(FunctionEffects &Effects,
                                           SILFunction *F) {
  if (F->hasSemanticsAttr("arc.programtermination_point")) {
    Effects.Traps = true;
    return true;
  }
  switch (F->getEffectsKind()) {
    case EffectsKind::ReadNone:
      return true;
    case EffectsKind::ReadOnly:
      // @effects(readonly) is worthless if we have owned parameters, because
      // the release inside the callee may call a deinit, which itself can do
      // anything.
      if (!F->hasOwnedParameters()) {
        Effects.GlobalEffects.Reads = true;
        return true;
      }
      break;
    default:
      break;
  }

  return false;
}

bool SideEffectAnalysis::getSemanticEffects(FunctionEffects &FE,
                                            ArraySemanticsCall ASC) {
  assert(ASC.hasSelf());
  auto &SelfEffects = FE.ParamEffects[FE.ParamEffects.size() - 1];

  // Currently we only handle array semantics.
  // TODO: also handle other semantic functions.
  
  switch (ASC.getKind()) {
    case ArrayCallKind::kGetCount:
    case ArrayCallKind::kGetCapacity:
      if (!ASC.mayHaveBridgedObjectElementType()) {
        SelfEffects.Reads = true;
        SelfEffects.Releases |= !ASC.hasGuaranteedSelf();
        return true;
      }
      return false;

    case ArrayCallKind::kCheckSubscript:
    case ArrayCallKind::kCheckIndex:
      if (!ASC.mayHaveBridgedObjectElementType()) {
        SelfEffects.Reads = true;
        SelfEffects.Releases |= !ASC.hasGuaranteedSelf();
        FE.Traps = true;
        return true;
      }
      return false;

    case ArrayCallKind::kGetElement:
      if (!ASC.mayHaveBridgedObjectElementType()) {
        SelfEffects.Reads = true;
        SelfEffects.Releases |= !ASC.hasGuaranteedSelf();
        for (auto i : range(((ApplyInst *)ASC)
                                ->getOrigCalleeConv()
                                .getNumIndirectSILResults())) {
          assert(!ASC.hasGetElementDirectResult());
          FE.ParamEffects[i].Writes = true;
        }
        return true;
      }
      return false;

    case ArrayCallKind::kArrayPropsIsNativeTypeChecked:
      SelfEffects.Releases |= !ASC.hasGuaranteedSelf();
      // The isNative checks evaluate to a constant (no read!) if the array
      // cannot be bridged.
      if (ASC.mayHaveBridgedObjectElementType())
        SelfEffects.Reads = true;
      return true;

    case ArrayCallKind::kGetElementAddress:
      SelfEffects.Reads = true;
      SelfEffects.Releases |= !ASC.hasGuaranteedSelf();
      return true;

    case ArrayCallKind::kMakeMutable:
      if (!ASC.mayHaveBridgedObjectElementType()) {
        SelfEffects.Writes = true;
        FE.GlobalEffects.Releases = true;
        FE.AllocsObjects = true;
        FE.ReadsRC = true;
        return true;
      }
      return false;

    default:
      return false;
  }
}

void SideEffectAnalysis::analyzeFunction(FunctionInfo *FInfo,
                                         FunctionOrder &BottomUpOrder,
                                         int RecursionDepth) {
  FInfo->NeedUpdateCallers = true;

  if (BottomUpOrder.prepareForVisiting(FInfo))
    return;

  // Handle @effects attributes
  if (getDefinedEffects(FInfo->FE, FInfo->F)) {
    DEBUG(llvm::dbgs() << "  -- has defined effects " <<
          FInfo->F->getName() << '\n');
    return;
  }
  
  if (!FInfo->F->isDefinition()) {
    // We can't assume anything about external functions.
    DEBUG(llvm::dbgs() << "  -- is external " << FInfo->F->getName() << '\n');
    FInfo->FE.setWorstEffects();
    return;
  }
  
  DEBUG(llvm::dbgs() << "  >> analyze " << FInfo->F->getName() << '\n');

  // Check all instructions of the function
  for (auto &BB : *FInfo->F) {
    for (auto &I : BB) {
      analyzeInstruction(FInfo, &I, BottomUpOrder, RecursionDepth);
    }
  }
  DEBUG(llvm::dbgs() << "  << finished " << FInfo->F->getName() << '\n');
}

void SideEffectAnalysis::analyzeInstruction(FunctionInfo *FInfo,
                                            SILInstruction *I,
                                            FunctionOrder &BottomUpOrder,
                                            int RecursionDepth) {
  if (FullApplySite FAS = FullApplySite::isa(I)) {
    // Is this a call to a semantics function?
    ArraySemanticsCall ASC(I);
    if (ASC && ASC.hasSelf()) {
      FunctionEffects ApplyEffects(FAS.getNumArguments());
      if (getSemanticEffects(ApplyEffects, ASC)) {
        FInfo->FE.mergeFromApply(ApplyEffects, FAS);
        return;
      }
    }

    if (SILFunction *SingleCallee = FAS.getReferencedFunction()) {
      // Does the function have any @effects?
      if (getDefinedEffects(FInfo->FE, SingleCallee))
        return;
    }

    if (RecursionDepth < MaxRecursionDepth) {
      CalleeList Callees = BCA->getCalleeList(FAS);
      if (Callees.allCalleesVisible() &&
          // @callee_owned function calls implicitly release the context, which
          // may call deinits of boxed values.
          // TODO: be less conservative about what destructors might be called.
          !FAS.getOrigCalleeType()->isCalleeConsumed()) {
        // Derive the effects of the apply from the known callees.
        for (SILFunction *Callee : Callees) {
          FunctionInfo *CalleeInfo = getFunctionInfo(Callee);
          CalleeInfo->addCaller(FInfo, FAS);
          if (!CalleeInfo->isVisited()) {
            // Recursively visit the called function.
            analyzeFunction(CalleeInfo, BottomUpOrder, RecursionDepth + 1);
            BottomUpOrder.tryToSchedule(CalleeInfo);
          }
        }
        return;
      }
    }
    // Be conservative for everything else.
    FInfo->FE.setWorstEffects();
    return;
  }
  // Handle some kind of instructions specially.
  switch (I->getKind()) {
    case ValueKind::FixLifetimeInst:
      // A fix_lifetime instruction acts like a read on the operand. Retains can move after it
      // but the last release can't move before it.
      FInfo->FE.getEffectsOn(I->getOperand(0))->Reads = true;
      return;
    case ValueKind::AllocStackInst:
    case ValueKind::DeallocStackInst:
      return;
    case ValueKind::StrongRetainInst:
    case ValueKind::StrongRetainUnownedInst:
    case ValueKind::RetainValueInst:
    case ValueKind::UnownedRetainInst:
      FInfo->FE.getEffectsOn(I->getOperand(0))->Retains = true;
      return;
    case ValueKind::StrongReleaseInst:
    case ValueKind::ReleaseValueInst:
    case ValueKind::UnownedReleaseInst:
      FInfo->FE.getEffectsOn(I->getOperand(0))->Releases = true;
      
      // TODO: Check the call graph to be less conservative about what
      // destructors might be called.
      FInfo->FE.setWorstEffects();
      return;
    case ValueKind::LoadInst:
      FInfo->FE.getEffectsOn(cast<LoadInst>(I)->getOperand())->Reads = true;
      return;
    case ValueKind::StoreInst:
      FInfo->FE.getEffectsOn(cast<StoreInst>(I)->getDest())->Writes = true;
      return;
    case ValueKind::CondFailInst:
      FInfo->FE.Traps = true;
      return;
    case ValueKind::PartialApplyInst: {
      FInfo->FE.AllocsObjects = true;
      auto *PAI = cast<PartialApplyInst>(I);
      auto Args = PAI->getArguments();
      auto Params = PAI->getSubstCalleeType()->getParameters();
      Params = Params.slice(Params.size() - Args.size(), Args.size());
      for (unsigned Idx : indices(Args)) {
        if (isIndirectFormalParameter(Params[Idx].getConvention()))
          FInfo->FE.getEffectsOn(Args[Idx])->Reads = true;
      }
      return;
    }
    case ValueKind::BuiltinInst: {
      auto *BInst = cast<BuiltinInst>(I);
      auto &BI = BInst->getBuiltinInfo();
      switch (BI.ID) {
        case BuiltinValueKind::IsUnique:
          // TODO: derive this information in a more general way, e.g. add it
          // to Builtins.def
          FInfo->FE.ReadsRC = true;
          break;
        case BuiltinValueKind::CondUnreachable:
          FInfo->FE.Traps = true;
          return;
        default:
          break;
      }
      const IntrinsicInfo &IInfo = BInst->getIntrinsicInfo();
      if (IInfo.ID == llvm::Intrinsic::trap) {
        FInfo->FE.Traps = true;
        return;
      }
      // Detailed memory effects of builtins are handled below by checking the
      // memory behavior of the instruction.
      break;
    }
    default:
      break;
  }

  if (isa<AllocationInst>(I)) {
    // Excluding AllocStackInst (which is handled above).
    FInfo->FE.AllocsObjects = true;
  }
  
  // Check the general memory behavior for instructions we didn't handle above.
  switch (I->getMemoryBehavior()) {
    case MemoryBehavior::None:
      break;
    case MemoryBehavior::MayRead:
      FInfo->FE.GlobalEffects.Reads = true;
      break;
    case MemoryBehavior::MayWrite:
      FInfo->FE.GlobalEffects.Writes = true;
      break;
    case MemoryBehavior::MayReadWrite:
      FInfo->FE.GlobalEffects.Reads = true;
      FInfo->FE.GlobalEffects.Writes = true;
      break;
    case MemoryBehavior::MayHaveSideEffects:
      FInfo->FE.setWorstEffects();
      break;
  }
  if (I->mayTrap())
    FInfo->FE.Traps = true;
}

void SideEffectAnalysis::initialize(SILPassManager *PM) {
  BCA = PM->getAnalysis<BasicCalleeAnalysis>();
}

void SideEffectAnalysis::recompute(FunctionInfo *Initial) {
  allocNewUpdateID();

  DEBUG(llvm::dbgs() << "recompute side-effect analysis with UpdateID " <<
        getCurrentUpdateID() << '\n');

  // Collect and analyze all functions to recompute, starting at Initial.
  FunctionOrder BottomUpOrder(getCurrentUpdateID());
  analyzeFunction(Initial, BottomUpOrder, 0);

  // Build the bottom-up order.
  BottomUpOrder.tryToSchedule(Initial);
  BottomUpOrder.finishScheduling();

  // Second step: propagate the side-effect information up the call-graph until
  // it stabilizes.
  bool NeedAnotherIteration;
  do {
    DEBUG(llvm::dbgs() << "new iteration\n");
    NeedAnotherIteration = false;

    for (FunctionInfo *FInfo : BottomUpOrder) {
      if (FInfo->NeedUpdateCallers) {
        DEBUG(llvm::dbgs() << "  update callers of " << FInfo->F->getName() <<
              '\n');
        FInfo->NeedUpdateCallers = false;

        // Propagate the side-effects to all callers.
        for (const auto &E : FInfo->getCallers()) {
          assert(E.isValid());

          // Only include callers which we are actually recomputing.
          if (BottomUpOrder.wasRecomputedWithCurrentUpdateID(E.Caller)) {
            DEBUG(llvm::dbgs() << "    merge into caller " <<
                  E.Caller->F->getName() << '\n');

            if (E.Caller->FE.mergeFromApply(FInfo->FE, E.FAS)) {
              E.Caller->NeedUpdateCallers = true;
              if (!E.Caller->isScheduledAfter(FInfo)) {
                // This happens if we have a cycle in the call-graph.
                NeedAnotherIteration = true;
              }
            }
          }
        }
      }
    }
  } while (NeedAnotherIteration);
}

void SideEffectAnalysis::getEffects(FunctionEffects &ApplyEffects, FullApplySite FAS) {
  assert(ApplyEffects.ParamEffects.size() == 0 &&
         "Not using a new ApplyEffects?");
  ApplyEffects.ParamEffects.resize(FAS.getNumArguments());

  // Is this a call to a semantics function?
  ArraySemanticsCall ASC(FAS.getInstruction());
  if (ASC && ASC.hasSelf()) {
    if (getSemanticEffects(ApplyEffects, ASC))
      return;
  }

  if (SILFunction *SingleCallee = FAS.getReferencedFunction()) {
    // Does the function have any @effects?
    if (getDefinedEffects(ApplyEffects, SingleCallee))
      return;
  }

  auto Callees = BCA->getCalleeList(FAS);
  if (!Callees.allCalleesVisible() ||
      // @callee_owned function calls implicitly release the context, which
      // may call deinits of boxed values.
      // TODO: be less conservative about what destructors might be called.
      FAS.getOrigCalleeType()->isCalleeConsumed()) {
    ApplyEffects.setWorstEffects();
    return;
  }

  // We can see all the callees. So we just merge the effects from all of
  // them.
  for (auto *Callee : Callees) {
    const FunctionEffects &CalleeFE = getEffects(Callee);
    ApplyEffects.mergeFrom(CalleeFE);
  }
}

void SideEffectAnalysis::invalidate() {
  Function2Info.clear();
  Allocator.DestroyAll();
  DEBUG(llvm::dbgs() << "invalidate all\n");
}

void SideEffectAnalysis::invalidate(SILFunction *F, InvalidationKind K) {
  if (FunctionInfo *FInfo = Function2Info.lookup(F)) {
    DEBUG(llvm::dbgs() << "  invalidate " << FInfo->F->getName() << '\n');
    invalidateIncludingAllCallers(FInfo);
  }
}

SILAnalysis *swift::createSideEffectAnalysis(SILModule *M) {
  return new SideEffectAnalysis();
}
