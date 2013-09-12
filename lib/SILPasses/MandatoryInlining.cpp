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

/// \brief Fixup reference counts after inlining a function call (which is a
/// no-op unless the function is a thick function). Note that this function
/// makes assumptions about the release/retain convention of thick function
/// applications: namely, that an apply of a thick function consumes the callee
/// and that the function implementing the closure consumes its capture
/// arguments.
static void
fixupReferenceCounts(SILBuilder &B, SILBasicBlock::iterator I, SILLocation Loc,
                     SILValue CalleeValue, bool IsThick,
                     SmallVectorImpl<SILValue>& CaptureArgs) {
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
    // closure and its contained capture arguments
    B.setInsertionPoint(InsertedRelease);
  }

  // Add a retain of each non-address type capture argument, because it will be
  // consumed by the closure body
  SILModule &M = B.getFunction().getModule();
  for (auto &CaptureArg : CaptureArgs) {
    if (!CaptureArg.getType().isAddress())
      M.getTypeLowering(CaptureArg.getType()).emitRetain(B, Loc, CaptureArg);
  }
}

/// \brief Removes instructions that create the callee value if they are no
/// longer necessary after inlining.
static void
cleanupCalleeValue(SILBuilder &B, SILBasicBlock::iterator &I,
                   SILValue CalleeValue,
                   SmallVectorImpl<SILValue>& CaptureArgs) {
  auto eraseInstruction = [&](SILInstruction *Inst) {
    if (static_cast<SILInstruction*>(I) == Inst)
      ++I;
    Inst->eraseFromParent();
  };

  // Handle the case where the callee of the apply is a load instruction
  if (LoadInst *LI = dyn_cast<LoadInst>(CalleeValue.getDef())) {
    assert(CalleeValue.getResultNumber() == 0);
    SILInstruction *ABI = dyn_cast<AllocBoxInst>(LI->getOperand().getDef());
    assert(ABI && LI->getOperand().getResultNumber() == 1);

    // The load instruction must have no more uses left to erase it
    if (!LI->use_empty())
      return;
    eraseInstruction(LI);

    // Look through uses of the alloc box the load is loading from to find up to
    // one store and up to one strong release
    StoreInst *SI = nullptr;
    StrongReleaseInst *SRI = nullptr;
    for (auto UI = ABI->use_begin(), UE = ABI->use_end(); UI != UE; ++UI) {
      if (SI == nullptr && isa<StoreInst>(UI.getUser())) {
        SI = cast<StoreInst>(UI.getUser());
        assert(SI->getDest() == SILValue(ABI, 1));
      } else if (SRI == nullptr && isa<StrongReleaseInst>(UI.getUser())) {
        SRI = cast<StrongReleaseInst>(UI.getUser());
        assert(SRI->getOperand() == SILValue(ABI, 0));
      } else
        return;
    }

    // If we found a store, record its source and erase it
    if (SI) {
      CalleeValue = SI->getSrc();
      eraseInstruction(SI);
    } else {
      CalleeValue = SILValue();
    }

    // If we found a strong release, replace it with a strong release of the
    // source of the store and erase it
    if (SRI) {
      if (CalleeValue.isValid()) {
        B.setInsertionPoint(SRI);
        B.createStrongRelease(SRI->getLoc(), CalleeValue);
      }
      eraseInstruction(SRI);
    }

    assert(ABI->use_empty());
    eraseInstruction(ABI);
    if (!CalleeValue.isValid())
      return;
  }

  if (PartialApplyInst *PAI =
        dyn_cast<PartialApplyInst>(CalleeValue.getDef())) {
    assert(CalleeValue.getResultNumber() == 0);

    // Look through remaining uses of the partial apply inst to find at most one
    // strong release instruction
    StrongReleaseInst *SRI = nullptr;
    for (auto UI = PAI->use_begin(), UE = PAI->use_end(); UI != UE; ++UI) {
      if (SRI == nullptr && isa<StrongReleaseInst>(UI.getUser())) {
        SRI = cast<StrongReleaseInst>(UI.getUser());
        assert(SRI->getOperand() == SILValue(PAI, 0));
      } else
        return;
    }

    // If there is a strong release of the partial apply, then replace it with
    // releases of the captured arguments
    if (SRI) {
      B.setInsertionPoint(SRI);
      SILModule &M = B.getFunction().getModule();
      for (auto &CaptureArg : CaptureArgs) {
        if (!CaptureArg.getType().isAddress())
          M.getTypeLowering(CaptureArg.getType()).emitRelease(B, SRI->getLoc(),
                                                              CaptureArg);
      }
      eraseInstruction(SRI);
    }

    CalleeValue = PAI->getCallee();
    assert(PAI->use_empty());
    eraseInstruction(PAI);
  } else if (ThinToThickFunctionInst *TTTFI =
               dyn_cast<ThinToThickFunctionInst>(CalleeValue.getDef())) {
    assert(CalleeValue.getResultNumber() == 0);

    // Look through remaining uses of the thin-to-thick inst to find at most one
    // strong release instruction
    StrongReleaseInst *SRI = nullptr;
    for (auto UI = TTTFI->use_begin(), UE = TTTFI->use_end(); UI != UE; ++UI) {
      if (SRI == nullptr && isa<StrongReleaseInst>(UI.getUser())) {
        SRI = cast<StrongReleaseInst>(UI.getUser());
        assert(SRI->getOperand() == SILValue(TTTFI, 0));
      } else
        return;
    }

    // If there is a strong release of the thin-to-thick function, erase it
    if (SRI)
      eraseInstruction(SRI);

    CalleeValue = TTTFI->getOperand();
    assert(TTTFI->use_empty());
    eraseInstruction(TTTFI);
  }

  if (FunctionRefInst *FRI = cast<FunctionRefInst>(CalleeValue)) {
    assert(CalleeValue.getResultNumber() == 0);
    if (FRI->use_empty())
      eraseInstruction(FRI);
  }
}

/// \brief Returns the callee SILFunction called at a call site, in the case
/// that the call is transparent (as in, both that the call is marked
/// with the transparent flag and that callee function is actually transparently
/// determinable from the SIL) or nullptr otherwise. This assumes that the SIL
/// is already in SSA form.
///
/// In the case that a non-null value is returned, FullArgs contains effective
/// argument operands for the callee function.
static SILFunction *
getCalleeFunction(ApplyInst* AI, bool &IsThick,
                  SmallVectorImpl<SILValue>& CaptureArgs,
                  SmallVectorImpl<SILValue>& FullArgs) {
  if (!AI->isTransparent())
    return nullptr;

  IsThick = false;
  CaptureArgs.clear();
  FullArgs.clear();

  for (const auto &Arg : AI->getArguments())
    FullArgs.push_back(Arg);
  SILValue CalleeValue = AI->getCallee();

  if (LoadInst *LI = dyn_cast<LoadInst>(CalleeValue.getDef())) {
    assert(CalleeValue.getResultNumber() == 0);
    // Conservatively only see through alloc_box; we assume this pass is run
    // immediately after SILGen
    SILInstruction *ABI = dyn_cast<AllocBoxInst>(LI->getOperand().getDef());
    if (!ABI)
      return nullptr;
    assert(LI->getOperand().getResultNumber() == 1);

    // Scan forward from the alloc box to find the first store, which
    // (conservatively) must be in the same basic block as the alloc box
    StoreInst *SI = nullptr;
    for (auto I = SILBasicBlock::iterator(ABI), E = I->getParent()->end();
         I != E; ++I) {
      // If we find the load instruction first, then the load is loading from
      // a non-initialized alloc; this shouldn't really happen but I'm not
      // making any assumptions
      if (static_cast<SILInstruction*>(I) == LI)
        return nullptr;
      if ((SI = dyn_cast<StoreInst>(I)) && SI->getDest().getDef() == ABI) {
        // We found a store that we know dominates the load; now ensure there
        // are no other uses of the alloc other than loads, retains, releases
        // and dealloc stacks
        for (auto UI = ABI->use_begin(), UE = ABI->use_end(); UI != UE;
             ++UI)
          if (UI.getUser() != SI && !isa<LoadInst>(UI.getUser()) &&
              !isa<StrongRetainInst>(UI.getUser()) &&
              !isa<StrongReleaseInst>(UI.getUser()))
            return nullptr;
        // We can conservatively see through the store
        break;
      }
    }
    if (!SI)
      return nullptr;
    CalleeValue = SI->getSrc();
  }

  // We are allowed to see through exactly one "partial apply" instruction or
  // one "thin to thick function" instructions, since those are the patterns
  // generated when using auto closures
  if (PartialApplyInst *PAI =
        dyn_cast<PartialApplyInst>(CalleeValue.getDef())) {
    assert(CalleeValue.getResultNumber() == 0);

    for (const auto &Arg : PAI->getArguments()) {
      CaptureArgs.push_back(Arg);
      FullArgs.push_back(Arg);
    }

    CalleeValue = PAI->getCallee();
    IsThick = true;
  } else if (ThinToThickFunctionInst *TTTFI =
               dyn_cast<ThinToThickFunctionInst>(CalleeValue.getDef())) {
    assert(CalleeValue.getResultNumber() == 0);
    CalleeValue = TTTFI->getOperand();
    IsThick = true;
  }

  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeValue.getDef());
  if (!FRI)
    return nullptr;
  assert(CalleeValue.getResultNumber() == 0);

  SILFunction *CalleeFunction = FRI->getFunction();
  if (!CalleeFunction || CalleeFunction->empty() ||
      (CalleeFunction->getAbstractCC() != AbstractCC::Freestanding &&
       CalleeFunction->getAbstractCC() != AbstractCC::Method))
    return nullptr;
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

  SmallVector<SILValue, 16> CaptureArgs;
  SmallVector<SILValue, 32> FullArgs;
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
                                                      CaptureArgs, FullArgs);
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

      fixupReferenceCounts(Builder, I, Loc, CalleeValue, IsThick, CaptureArgs);
      cleanupCalleeValue(Builder, I, CalleeValue, CaptureArgs);

      // Reposition iterators possibly invalidated by mutation
      FI = SILFunction::iterator(I->getParent());
      FE = F->end();
      E = FI->end();
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
