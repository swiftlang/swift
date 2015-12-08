//===---------- SideEffectAnalysis.cpp - SIL Side Effect Analysis ---------===//
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

#define DEBUG_TYPE "sil-sea"
#include "swift/SILAnalysis/SideEffectAnalysis.h"
#include "swift/SILAnalysis/BasicCalleeAnalysis.h"
#include "swift/SILAnalysis/FunctionOrder.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILAnalysis/ArraySemantic.h"
#include "swift/SILPasses/PassManager.h"
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

    if (ArgBehavior > Behavior)
      Behavior = ArgBehavior;

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
  bool Changed = mergeFlags(ApplyEffects);
  Changed |= GlobalEffects.mergeFrom(ApplyEffects.GlobalEffects);
  auto Args = FAS.getArguments();
  unsigned NumCalleeArgs = ApplyEffects.ParamEffects.size();
  assert(NumCalleeArgs == Args.size());
  for (unsigned Idx = 0; Idx < NumCalleeArgs; Idx++) {
    // Map the callee argument effects to parameters of this function.
    Effects *E = getEffectsOn(Args[Idx]);
    Changed |= E->mergeFrom(ApplyEffects.ParamEffects[Idx]);
  }
  return Changed;
}

void FunctionEffects::dump() {
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
    case swift::ValueKind::SILArgument: {
      // Can we associate the address to a function parameter?
      SILArgument *Arg = cast<SILArgument>(BaseAddr);
      if (Arg->isFunctionArg()) {
        return &ParamEffects[Arg->getIndex()];
      }
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
  if (F->getLoweredFunctionType()->isNoReturn()) {
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
                                            FullApplySite FAS) {
  ArraySemanticsCall ASC(FAS.getInstruction());
  if (!ASC || !ASC.hasSelf())
    return false;
  
  auto &SelfEffects = FE.ParamEffects[FAS.getNumArguments() - 1];

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
        if (FAS.getOrigCalleeType()->hasIndirectResult())
          FE.ParamEffects[0].Writes = true;
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

void SideEffectAnalysis::analyzeFunction(SILFunction *F,
                                         WorkListType &WorkList,
                                         CallGraph &CG) {
  DEBUG(llvm::dbgs() << "analyze " << F->getName() << "\n");
  auto *FE = getFunctionEffects(F, true);
  
  // Handle @effects attributes
  if (getDefinedEffects(*FE, F))
    return;
  
  if (!F->isDefinition()) {
    // We can't assume anything about external functions.
    FE->setWorstEffects();
    return;
  }
  
  FunctionEffects NewEffects(F->getArguments().size());
#ifndef NDEBUG
  FunctionEffects RefEffects = *FE;
#endif
  // Check all instructions of the function
  for (auto &BB : *F) {
    for (auto &I : BB) {
      analyzeInstruction(NewEffects, &I);
      DEBUG(if (RefEffects.mergeFrom(NewEffects))
              llvm::dbgs() << "  " << NewEffects << "\t changed in " << I);
    }
  }
  if (FE->mergeFrom(NewEffects)) {
    // The effects have changed. We also have to recompute the effects of all
    // callers.
    for (auto *CallerEdge : CG.getCallerEdges(F)) {
      SILFunction *Caller = CallerEdge->getInstruction()->getFunction();
      WorkList.insert(Caller);
    }
  }
}

void SideEffectAnalysis::analyzeInstruction(FunctionEffects &FE,
                                            SILInstruction *I) {
  if (FullApplySite FAS = FullApplySite::isa(I)) {
    FunctionEffects ApplyEffects;
    getEffectsOfApply(ApplyEffects, FAS, true);
    FE.mergeFromApply(ApplyEffects, FAS);
    return;
  }
  // Handle some kind of instructions specially.
  switch (I->getKind()) {
    case ValueKind::AllocStackInst:
    case ValueKind::DeallocStackInst:
      return;
    case ValueKind::StrongRetainInst:
    case ValueKind::StrongRetainUnownedInst:
    case ValueKind::RetainValueInst:
    case ValueKind::UnownedRetainInst:
      FE.getEffectsOn(I->getOperand(0))->Retains = true;
      return;
    case ValueKind::StrongReleaseInst:
    case ValueKind::ReleaseValueInst:
    case ValueKind::UnownedReleaseInst:
      FE.getEffectsOn(I->getOperand(0))->Releases = true;
      
      // TODO: Check the call graph to be less conservative about what
      // destructors might be called.
      FE.setWorstEffects();
      return;
    case ValueKind::LoadInst:
      FE.getEffectsOn(cast<LoadInst>(I)->getOperand())->Reads = true;
      return;
    case ValueKind::StoreInst:
      FE.getEffectsOn(cast<StoreInst>(I)->getDest())->Writes = true;
      return;
    case ValueKind::CondFailInst:
      FE.Traps = true;
      return;
    case ValueKind::PartialApplyInst:
      FE.AllocsObjects = true;
      return;
    case ValueKind::BuiltinInst: {
      auto &BI = cast<BuiltinInst>(I)->getBuiltinInfo();
      switch (BI.ID) {
        case BuiltinValueKind::IsUnique:
          // TODO: derive this information in a more general way, e.g. add it
          // to Builtins.def
          FE.ReadsRC = true;
          break;
        default:
          break;
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
    FE.AllocsObjects = true;
  }
  
  // Check the general memory behavior for instructions we didn't handle above.
  switch (I->getMemoryBehavior()) {
    case MemoryBehavior::None:
      break;
    case MemoryBehavior::MayRead:
      FE.GlobalEffects.Reads = true;
      break;
    case MemoryBehavior::MayWrite:
      FE.GlobalEffects.Writes = true;
      break;
    case MemoryBehavior::MayReadWrite:
      FE.GlobalEffects.Reads = true;
      FE.GlobalEffects.Writes = true;
      break;
    case MemoryBehavior::MayHaveSideEffects:
      FE.setWorstEffects();
      break;
  }
  if (I->mayTrap())
    FE.Traps = true;
}

void SideEffectAnalysis::getEffectsOfApply(FunctionEffects &ApplyEffects,
                                           FullApplySite FAS,
                                           bool isRecomputing) {
  
  assert(ApplyEffects.ParamEffects.size() == 0 &&
         "Not using a new ApplyEffects?");
  ApplyEffects.ParamEffects.resize(FAS.getNumArguments());

  // Is this a call to a semantics function?
  if (getSemanticEffects(ApplyEffects, FAS))
    return;

  auto Callees = BCA->getCalleeList(FAS);
  if (Callees.isIncomplete()) {
    ApplyEffects.setWorstEffects();
    return;
  }

  // We can see all the callees. So we just merge the effects from all of
  // them.
  for (auto *F : Callees) {
    auto *E = getFunctionEffects(F, isRecomputing);
    ApplyEffects.mergeFrom(*E);
  }
}

void SideEffectAnalysis::initialize(SILPassManager *PM) {
  BCA = PM->getAnalysis<BasicCalleeAnalysis>();
  CGA = PM->getAnalysis<CallGraphAnalysis>();
}

void SideEffectAnalysis::recompute() {

  // Did anything change since the last recompuation? (Probably yes)
  if (!shouldRecompute)
    return;

  Function2Effects.clear();
  Allocator.DestroyAll();
  shouldRecompute = false;

  WorkListType WorkList;

  BottomUpFunctionOrder BottomUpOrder(M, BCA);
  auto BottomUpFunctions = BottomUpOrder.getFunctions();

  // Copy the bottom-up function list into the worklist.
  for (auto I = BottomUpFunctions.rbegin(), E = BottomUpFunctions.rend();
       I != E; ++I)
    WorkList.insert(*I);

  CallGraph &CG = CGA->getOrBuildCallGraph();

  // Iterate until the side-effect information stabilizes.
  while (!WorkList.empty()) {
    auto *F = WorkList.pop_back_val();
    analyzeFunction(F, WorkList, CG);
  }
}

void SideEffectAnalysis::getEffects(FunctionEffects &FE, FullApplySite FAS) {
  getEffectsOfApply(FE, FAS, false);
}

SILAnalysis *swift::createSideEffectAnalysis(SILModule *M) {
  return new SideEffectAnalysis(M);
}
