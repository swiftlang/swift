//===--- MandatoryInlining.cpp - Perform inlining of "transparent" sites --===//
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

#define DEBUG_TYPE "mandatory-inlining"
#include "swift/Subsystems.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Diagnostics.h"
#include "swift/SILPasses/Utils/SILInliner.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/ImmutableSet.h"
#include "llvm/Support/Debug.h"
using namespace swift;

typedef llvm::DenseSet<SILFunction*> DenseFunctionSet;
typedef llvm::ImmutableSet<SILFunction*> ImmutableFunctionSet;

STATISTIC(NumMandatoryInlines,
          "Number of function application sites inlined by the mandatory "
          "inlining pass");

template<typename...T, typename...U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
              U &&...args) {
  Context.Diags.diagnose(loc,
                         diag, std::forward<U>(args)...);
}

/// \brief Fixup reference counts after inlining a thick function call. Note
/// that this function makes assumptions about the release/retain convention of
/// thick function applications: namely, that an apply of a thick function
/// consumes the callee and that the function implementing the closure releases
/// its context arguments.
static void
fixupReferenceCounts(SILBuilder &B, SILBasicBlock::iterator &I, SILLocation Loc,
                     SILValue CalleeValue, bool IsThick,
                     SmallVectorImpl<SILValue>& ContextArgs) {
  if (!IsThick)
    return;

  // Either release the callee (which the apply would have done) or remove a
  // retain that happens to be the immediately preceding instruction
  SILBasicBlock::iterator Prev = I;
  StrongRetainInst *RetainToErase;
  if (I != I->getParent()->begin() &&
      (RetainToErase = dyn_cast<StrongRetainInst>(--Prev)) &&
      RetainToErase->getOperand() == CalleeValue) {
    RetainToErase->eraseFromParent();
    B.setInsertionPoint(I);
  } else {
    B.setInsertionPoint(I);
    StrongReleaseInst *InsertedRelease =
      B.createStrongReleaseInst(Loc, CalleeValue);
    // Important: we move the insertion point before this new release, just in
    // case this inserted release would have caused the deallocation of the
    // closure and its contained context arguments
    B.setInsertionPoint(InsertedRelease);
  }

  // Add a retain of each non-address type context argument, because it will be
  // consumed by the closure body
  SILModule &M = B.getFunction().getModule();
  for (auto &ContextArg : ContextArgs) {
    if (!ContextArg.getType().isAddress())
      M.getTypeLowering(ContextArg.getType()).emitRetain(B, Loc, ContextArg);
  }
}

/// \brief Returns the callee SILFunction called at a call site, in the case
/// that the call is transparent (as in, both that the call is marked
/// with the transparent flag and that callee function is actually transparently
/// determinable from the SIL) or nullptr otherwise. This assumes that the SIL
/// is already in SSA form.
///
/// In the case that a non-null value is returned, FullArgs contains effective
/// argument operands for the callee function and PossiblyDeadInsts contains
/// instructions that might be dead after inlining and removal of any dead
/// instructions earlier in the vector.
static SILFunction *
getCalleeFunction(ApplyInst* AI, bool &IsThick,
                  SmallVectorImpl<SILValue>& ContextArgs,
                  SmallVectorImpl<SILValue>& FullArgs,
                  SmallVectorImpl<SILInstruction*>& PossiblyDeadInsts) {
  if (!AI->isTransparent())
    return nullptr;

  IsThick = false;
  ContextArgs.clear();
  FullArgs.clear();
  PossiblyDeadInsts.clear();

  for (const auto &Arg : AI->getArguments())
    FullArgs.push_back(Arg);
  SILValue Callee = AI->getCallee();

  if (LoadInst *LI = dyn_cast<LoadInst>(Callee.getDef())) {
    assert(Callee.getResultNumber() == 0);
    // Conservatively only see through alloc_box; we assume this pass is run
    // immediately after SILGen
    SILInstruction *ABSI = dyn_cast<SILInstruction>(LI->getOperand().getDef());
    if (!ABSI || !(isa<AllocBoxInst>(ABSI) || isa<AllocStackInst>(ABSI)))
      return nullptr;
    assert(LI->getOperand().getResultNumber() == 1);

    // Scan forward from the alloc box to find the first store, which
    // (conservatively) must be in the same basic block as the alloc box
    StoreInst *SI = nullptr;
    for (auto I = SILBasicBlock::iterator(ABSI), E = I->getParent()->end();
         I != E; ++I) {
      // If we find the load instruction first, then the load is loading from
      // a non-initialized alloc; this shouldn't really happen but I'm not
      // making any assumptions
      if (static_cast<SILInstruction*>(I) == LI)
        return nullptr;
      if ((SI = dyn_cast<StoreInst>(I)) && SI->getDest().getDef() == ABSI) {
        // We found a store that we know dominates the load; now ensure there
        // are no other uses of the alloc other than loads, retains, releases
        // and dealloc stacks
        for (auto UI = ABSI->use_begin(), UE = ABSI->use_end(); UI != UE;
             ++UI)
          if (UI.getUser() != SI && !isa<LoadInst>(UI.getUser()) &&
              !isa<StrongRetainInst>(UI.getUser()) &&
              !isa<StrongReleaseInst>(UI.getUser()) &&
              !isa<DeallocStackInst>(UI.getUser()))
            return nullptr;
        // We can conservatively see through the store
        break;
      }
    }
    if (!SI)
      return nullptr;
    Callee = SI->getSrc();
  }

  // We are allowed to see through exactly one "partial apply" instruction or
  // one "thin to thick function" instructions, since those are the patterns
  // generated when using auto closures
  if (PartialApplyInst *PAI = dyn_cast<PartialApplyInst>(Callee.getDef())) {
    assert(Callee.getResultNumber() == 0);

    for (const auto &Arg : PAI->getArguments()) {
      ContextArgs.push_back(Arg);
      FullArgs.push_back(Arg);
    }

    Callee = PAI->getCallee();
    IsThick = true;
    PossiblyDeadInsts.push_back(PAI);
  } else if (ThinToThickFunctionInst *TTTFI =
               dyn_cast<ThinToThickFunctionInst>(Callee.getDef())) {
    assert(Callee.getResultNumber() == 0);
    Callee = TTTFI->getOperand();
    IsThick = true;
    PossiblyDeadInsts.push_back(TTTFI);
  }

  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(Callee.getDef());
  if (!FRI)
    return nullptr;
  assert(Callee.getResultNumber() == 0);

  SILFunction *CalleeFunction = FRI->getFunction();
  if (!CalleeFunction || CalleeFunction->empty() ||
      (CalleeFunction->getAbstractCC() != AbstractCC::Freestanding &&
       CalleeFunction->getAbstractCC() != AbstractCC::Method))
    return nullptr;
  PossiblyDeadInsts.push_back(FRI);
  return CalleeFunction;
}

/// \brief Inlines all mandatory inlined functions into the body of a function,
/// first recursively inlining all mandatory apply instructions in those
/// functions into their bodies if necessary.
///
/// \param F the function to be processed
/// \param AI nullptr if this is being called from the top level; the relevant
///   ApplyInst requiring the recursive call when non-null
/// \param FullyInlinedSet the set of all functions already known to be fully
///   processed, to avoid processing them over again
/// \param SetFactory an instance of ImmutableFunctionSet::Factory
/// \param CurrentInliningSet the set of functions currently being inlined in
///   the current call stack of recursive calls
///
/// \returns true if successful, false if failed due to circular inlining.
static bool
runOnFunctionRecursively(SILFunction *F, ApplyInst* AI,
                         DenseFunctionSet &FullyInlinedSet,
                         ImmutableFunctionSet::Factory &SetFactory,
                         ImmutableFunctionSet CurrentInliningSet) {
  // Avoid reprocessing functions needlessly
  if (FullyInlinedSet.find(F) != FullyInlinedSet.end())
    return true;

  // Prevent attempt to circularly inline.
  if (CurrentInliningSet.contains(F)) {
    // This cannot happen on a top-level call, so AI should be non-null.
    assert(AI && "Cannot have circular inline without apply");
    SILLocation L = AI->getLoc();
    assert(L && "Must have location for transparent inline apply");
    diagnose(F->getModule().getASTContext(), L.getStartSourceLoc(),
             diag::circular_transparent);
    return false;
  }

  // Add to the current inlining set (immutably, so we only affect the set
  // during this call and recursive subcalls).
  CurrentInliningSet = SetFactory.add(CurrentInliningSet, F);

  SmallVector<SILValue, 16> ContextArgs;
  SmallVector<SILValue, 32> FullArgs;
  SmallVector<SILInstruction*, 16> PossiblyDeadInsts;
  SILInliner Inliner(*F);
  SILBuilder Builder(*F);
  for (auto FI = F->begin(), FE = F->end(); FI != FE; ++FI) {
    auto I = FI->begin(), E = FI->end();
    while (I != E) {
      ApplyInst *InnerAI = dyn_cast<ApplyInst>(I);
      if (!InnerAI) {
        ++I;
        continue;
      }

      SILLocation Loc = InnerAI->getLoc();
      SILValue CalleeValue = InnerAI->getCallee();
      bool IsThick;
      SILFunction *CalleeFunction = getCalleeFunction(InnerAI, IsThick,
                                                      ContextArgs, FullArgs,
                                                      PossiblyDeadInsts);
      if (!CalleeFunction) {
        ++I;
        continue;
      }

      // Then recursively process it first before trying to inline it.
      if (!runOnFunctionRecursively(CalleeFunction, InnerAI, FullyInlinedSet,
                                    SetFactory, CurrentInliningSet)) {
        // If we failed due to circular inlining, then emit some notes to
        // trace back the failure if we have more information.
        // FIXME: possibly it could be worth recovering and attempting other
        // inlines within this same recursive call rather than simply
        // propogating the failure.
        if (AI) {
          SILLocation L = AI->getLoc();
          assert(L && "Must have location for transparent inline apply");
          diagnose(F->getModule().getASTContext(), L.getStartSourceLoc(),
                   diag::note_while_inlining);
        }
        return false;
      }

      // Inline function at I, which also changes I to refer to the first
      // instruction inlined in the case that it succeeds. We purposely
      // process the inlined body after inlining, because the inlining may
      // have exposed new inlining opportunities beyond those present in
      // the inlined function when processed independently
      if (!Inliner.inlineFunction(I, CalleeFunction, FullArgs)) {
        // If inlining failed, then I is left unchanged, so increment it
        // before continuing rather than process the same apply instruction
        // twice
        ++I;
        continue;
      }

      fixupReferenceCounts(Builder, I, Loc, CalleeValue, IsThick, ContextArgs);

      // Reposition iterators possibly invalidated by mutation
      FI = SILFunction::iterator(I->getParent());
      FE = F->end();
      E = FI->end();

      for (SILInstruction *PossiblyDeadInst : PossiblyDeadInsts)
        if (PossiblyDeadInst->use_empty())
          PossiblyDeadInst->eraseFromParent();

      ++NumMandatoryInlines;
    }
  }

  // Keep track of full inlined functions so we don't waste time recursively
  // reprocessing them.
  FullyInlinedSet.insert(F);
  return true;
}

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILMandatoryInlining(SILModule *M) {
  DenseFunctionSet FullyInlinedSet;
  ImmutableFunctionSet::Factory SetFactory;
  for (auto &F : *M)
    runOnFunctionRecursively(&F, nullptr, FullyInlinedSet, SetFactory,
                              SetFactory.getEmptySet());
}
