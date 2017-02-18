//===--- MandatoryInlining.cpp - Perform inlining of "transparent" sites --===//
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

#define DEBUG_TYPE "mandatory-inlining"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/ImmutableSet.h"
#include "llvm/ADT/Statistic.h"
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
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

namespace {

/// A helper class to update an instruction iterator if
/// removal of instructions would invalidate it.
class DeleteInstructionsHandler : public DeleteNotificationHandler {
  SILBasicBlock::iterator &CurrentI;
  SILModule &Module;

public:
  DeleteInstructionsHandler(SILBasicBlock::iterator &I)
      : CurrentI(I), Module(I->getModule()) {
    Module.registerDeleteNotificationHandler(this);
  }

  ~DeleteInstructionsHandler() override {
     // Unregister the handler.
    Module.removeDeleteNotificationHandler(this);
  }

  // Handling of instruction removal notifications.
  bool needsNotifications() override { return true; }

  // Handle notifications about removals of instructions.
  void handleDeleteNotification(swift::ValueBase *Value) override {
    if (auto DeletedI = dyn_cast<SILInstruction>(Value)) {
      if (CurrentI == SILBasicBlock::iterator(DeletedI)) {
        if (CurrentI != CurrentI->getParent()->begin()) {
          --CurrentI;
        } else {
          ++CurrentI;
        }
      }
    }
  }
};

} // end of namespace

/// \brief Fixup reference counts after inlining a function call (which is a
/// no-op unless the function is a thick function). Note that this function
/// makes assumptions about the release/retain convention of thick function
/// applications: namely, that an apply of a thick function consumes the callee
/// and that the function implementing the closure consumes its capture
/// arguments.
static void fixupReferenceCounts(SILBasicBlock::iterator I, SILLocation Loc,
                                 SILValue CalleeValue,
                                 SmallVectorImpl<SILValue> &CaptureArgs) {
  // Either release the callee (which the apply would have done) or remove a
  // retain that happens to be the immediately preceding instruction.
  SILBuilderWithScope B(I);
  auto *NewRelease = B.emitStrongReleaseAndFold(Loc, CalleeValue);

  // Important: we move the insertion point before this new release, just in
  // case this inserted release would have caused the deallocation of the
  // closure and its contained capture arguments.
  if (NewRelease)
    B.setInsertionPoint(NewRelease);

  // Add a retain of each non-address type capture argument, because it will be
  // consumed by the closure body.
  for (auto &CaptureArg : CaptureArgs)
    if (!CaptureArg->getType().isAddress())
      B.emitCopyValueOperation(Loc, CaptureArg);
}

/// \brief Removes instructions that create the callee value if they are no
/// longer necessary after inlining.
static void
cleanupCalleeValue(SILValue CalleeValue, ArrayRef<SILValue> CaptureArgs,
                   ArrayRef<SILValue> FullArgs) {
  SmallVector<SILInstruction*, 16> InstsToDelete;
  for (SILValue V : FullArgs) {
    if (SILInstruction *I = dyn_cast<SILInstruction>(V))
      if (I != CalleeValue &&
          isInstructionTriviallyDead(I))
        InstsToDelete.push_back(I);
  }
  recursivelyDeleteTriviallyDeadInstructions(InstsToDelete, true);

  // Handle the case where the callee of the apply is a load instruction.
  if (LoadInst *LI = dyn_cast<LoadInst>(CalleeValue)) {
    auto *PBI = cast<ProjectBoxInst>(LI->getOperand());
    auto *ABI = cast<AllocBoxInst>(PBI->getOperand());

    // The load instruction must have no more uses left to erase it.
    if (!LI->use_empty())
      return;
    LI->eraseFromParent();

    // Look through uses of the alloc box the load is loading from to find up to
    // one store and up to one strong release.
    StrongReleaseInst *SRI = nullptr;
    for (Operand *ABIUse : ABI->getUses()) {
      if (SRI == nullptr && isa<StrongReleaseInst>(ABIUse->getUser())) {
        SRI = cast<StrongReleaseInst>(ABIUse->getUser());
      } else if (ABIUse->getUser() != PBI)
        return;
    }
    StoreInst *SI = nullptr;
    for (Operand *PBIUse : PBI->getUses()) {
      if (SI == nullptr && isa<StoreInst>(PBIUse->getUser())) {
        SI = cast<StoreInst>(PBIUse->getUser());
      } else
        return;
    }

    // If we found a store, record its source and erase it.
    if (SI) {
      CalleeValue = SI->getSrc();
      SI->eraseFromParent();
    } else {
      CalleeValue = SILValue();
    }

    // If we found a strong release, replace it with a strong release of the
    // source of the store and erase it.
    if (SRI) {
      if (CalleeValue)
        SILBuilderWithScope(SRI)
            .emitStrongReleaseAndFold(SRI->getLoc(), CalleeValue);
      SRI->eraseFromParent();
    }

    assert(PBI->use_empty());
    PBI->eraseFromParent();
    assert(ABI->use_empty());
    ABI->eraseFromParent();
    if (!CalleeValue)
      return;
  }

  if (auto *PAI = dyn_cast<PartialApplyInst>(CalleeValue)) {
    SILValue Callee = PAI->getCallee();
    if (!tryDeleteDeadClosure(PAI))
      return;
    CalleeValue = Callee;
  }

  if (auto *TTTFI = dyn_cast<ThinToThickFunctionInst>(CalleeValue)) {
    SILValue Callee = TTTFI->getCallee();
    if (!tryDeleteDeadClosure(TTTFI))
      return;
    CalleeValue = Callee;
  }

  if (FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeValue)) {
    if (!FRI->use_empty())
      return;
    FRI->eraseFromParent();
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
getCalleeFunction(FullApplySite AI, bool &IsThick,
                  SmallVectorImpl<SILValue>& CaptureArgs,
                  SmallVectorImpl<SILValue>& FullArgs,
                  PartialApplyInst *&PartialApply,
                  SILModule::LinkingMode Mode) {
  IsThick = false;
  PartialApply = nullptr;
  CaptureArgs.clear();
  FullArgs.clear();

  for (const auto &Arg : AI.getArguments())
    FullArgs.push_back(Arg);
  SILValue CalleeValue = AI.getCallee();

  if (LoadInst *LI = dyn_cast<LoadInst>(CalleeValue)) {
    // Conservatively only see through alloc_box; we assume this pass is run
    // immediately after SILGen
    auto *PBI = dyn_cast<ProjectBoxInst>(LI->getOperand());
    if (!PBI)
      return nullptr;
    auto *ABI = dyn_cast<AllocBoxInst>(PBI->getOperand());
    if (!ABI)
      return nullptr;
    // Ensure there are no other uses of alloc_box than the project_box and
    // retains, releases.
    for (Operand *ABIUse : ABI->getUses())
      if (ABIUse->getUser() != PBI &&
          !isa<StrongRetainInst>(ABIUse->getUser()) &&
          !isa<StrongReleaseInst>(ABIUse->getUser()))
        return nullptr;

    // Scan forward from the alloc box to find the first store, which
    // (conservatively) must be in the same basic block as the alloc box
    StoreInst *SI = nullptr;
    for (auto I = SILBasicBlock::iterator(ABI), E = I->getParent()->end();
         I != E; ++I) {
      // If we find the load instruction first, then the load is loading from
      // a non-initialized alloc; this shouldn't really happen but I'm not
      // making any assumptions
      if (&*I == LI)
        return nullptr;
      if ((SI = dyn_cast<StoreInst>(I)) && SI->getDest() == PBI) {
        // We found a store that we know dominates the load; now ensure there
        // are no other uses of the project_box except loads.
        for (Operand *PBIUse : PBI->getUses())
          if (PBIUse->getUser() != SI && !isa<LoadInst>(PBIUse->getUser()))
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
  // generated when using auto closures.
  if (PartialApplyInst *PAI =
        dyn_cast<PartialApplyInst>(CalleeValue)) {
    for (const auto &Arg : PAI->getArguments()) {
      CaptureArgs.push_back(Arg);
      FullArgs.push_back(Arg);
    }

    CalleeValue = PAI->getCallee();
    IsThick = true;
    PartialApply = PAI;
  } else if (ThinToThickFunctionInst *TTTFI =
               dyn_cast<ThinToThickFunctionInst>(CalleeValue)) {
    CalleeValue = TTTFI->getOperand();
    IsThick = true;
  }

  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeValue);

  if (!FRI)
    return nullptr;

  SILFunction *CalleeFunction = FRI->getReferencedFunction();

  switch (CalleeFunction->getRepresentation()) {
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::WitnessMethod:
    break;
    
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::Block:
    return nullptr;
  }

  // If CalleeFunction is a declaration, see if we can load it. If we fail to
  // load it, bail.
  if (CalleeFunction->empty()
      && !AI.getModule().linkFunction(CalleeFunction, Mode))
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
runOnFunctionRecursively(SILFunction *F, FullApplySite AI,
                         SILModule::LinkingMode Mode,
                         DenseFunctionSet &FullyInlinedSet,
                         ImmutableFunctionSet::Factory &SetFactory,
                         ImmutableFunctionSet CurrentInliningSet,
                         ClassHierarchyAnalysis *CHA) {
  // Avoid reprocessing functions needlessly.
  if (FullyInlinedSet.count(F))
    return true;

  // Prevent attempt to circularly inline.
  if (CurrentInliningSet.contains(F)) {
    // This cannot happen on a top-level call, so AI should be non-null.
    assert(AI && "Cannot have circular inline without apply");
    SILLocation L = AI.getLoc();
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

  for (auto FI = F->begin(), FE = F->end(); FI != FE; ++FI) {
    for (auto I = FI->begin(), E = FI->end(); I != E; ++I) {
      FullApplySite InnerAI = FullApplySite::isa(&*I);

      if (!InnerAI)
        continue;

      auto *ApplyBlock = InnerAI.getParent();

      auto NewInstPair = tryDevirtualizeApply(InnerAI, CHA);
      if (auto *NewInst = NewInstPair.first) {
        replaceDeadApply(InnerAI, NewInst);
        if (auto *II = dyn_cast<SILInstruction>(NewInst))
          I = II->getIterator();
        else
          I = NewInst->getParentBlock()->begin();
        auto NewAI = FullApplySite::isa(NewInstPair.second.getInstruction());
        if (!NewAI)
          continue;

        InnerAI = NewAI;
      }

      SILLocation Loc = InnerAI.getLoc();
      SILValue CalleeValue = InnerAI.getCallee();
      bool IsThick;
      PartialApplyInst *PAI;
      SILFunction *CalleeFunction = getCalleeFunction(InnerAI, IsThick,
                                                      CaptureArgs, FullArgs,
                                                      PAI,
                                                      Mode);
      if (!CalleeFunction ||
          CalleeFunction->isTransparent() == IsNotTransparent)
        continue;

      if (F->isFragile() &&
          !CalleeFunction->hasValidLinkageForFragileRef()) {
        if (!CalleeFunction->hasValidLinkageForFragileInline()) {
          llvm::errs() << "caller: " << F->getName() << "\n";
          llvm::errs() << "callee: " << CalleeFunction->getName() << "\n";
          llvm_unreachable("Should never be inlining a resilient function into "
                           "a fragile function");
        }
        continue;
      }

      // Then recursively process it first before trying to inline it.
      if (!runOnFunctionRecursively(CalleeFunction, InnerAI, Mode,
                                    FullyInlinedSet, SetFactory,
                                    CurrentInliningSet, CHA)) {
        // If we failed due to circular inlining, then emit some notes to
        // trace back the failure if we have more information.
        // FIXME: possibly it could be worth recovering and attempting other
        // inlines within this same recursive call rather than simply
        // propagating the failure.
        if (AI) {
          SILLocation L = AI.getLoc();
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
      // the inlined function when processed independently.
      DEBUG(llvm::errs() << "Inlining @" << CalleeFunction->getName()
                         << " into @" << InnerAI.getFunction()->getName()
                         << "\n");

      // If we intend to inline a thick function, then we need to balance the
      // reference counts for correctness.
      if (IsThick && I != ApplyBlock->begin()) {
        // We need to find an appropriate location for our fix up code
        // We used to do this after inlining Without any modifications
        // This caused us to add a release in a wrong place:
        // It would release a value *before* retaining it!
        // It is really problematic to do this after inlining -
        // Finding a valid insertion point is tricky:
        // Inlining might add new basic blocks and/or remove the apply
        // We want to add the fix up *just before* where the current apply is!
        // Unfortunately, we *can't* add the fix up code here:
        // Inlining might fail for any reason -
        // If that occurred we'd need to undo our fix up code.
        // Instead, we split the current basic block -
        // Making sure we have a basic block that starts with our apply.
        SILBuilderWithScope B(I);
        ApplyBlock = splitBasicBlockAndBranch(B, &*I, nullptr, nullptr);
        I = ApplyBlock->begin();
      }

      // Decrement our iterator (carefully, to avoid going off the front) so it
      // is valid after inlining is done.  Inlining deletes the apply, and can
      // introduce multiple new basic blocks.
      if (I != ApplyBlock->begin())
        --I;
      else
        I = ApplyBlock->end();

      std::vector<Substitution> ApplySubs(InnerAI.getSubstitutions());

      if (PAI) {
        auto PAISubs = PAI->getSubstitutions();
        ApplySubs.insert(ApplySubs.end(), PAISubs.begin(), PAISubs.end());
      }

      SILOpenedArchetypesTracker OpenedArchetypesTracker(*F);
      F->getModule().registerDeleteNotificationHandler(
          &OpenedArchetypesTracker);
      // The callee only needs to know about opened archetypes used in
      // the substitution list.
      OpenedArchetypesTracker.registerUsedOpenedArchetypes(InnerAI.getInstruction());
      if (PAI) {
        OpenedArchetypesTracker.registerUsedOpenedArchetypes(PAI);
      }

      SILInliner Inliner(*F, *CalleeFunction,
                         SILInliner::InlineKind::MandatoryInline,
                         ApplySubs, OpenedArchetypesTracker);
      if (!Inliner.inlineFunction(InnerAI, FullArgs)) {
        I = InnerAI.getInstruction()->getIterator();
        continue;
      }

      // Inlining was successful. Remove the apply.
      InnerAI.getInstruction()->eraseFromParent();

      // Reestablish our iterator if it wrapped.
      if (I == ApplyBlock->end())
        I = ApplyBlock->begin();

      // Update the iterator when instructions are removed.
      DeleteInstructionsHandler DeletionHandler(I);

      // If the inlined apply was a thick function, then we need to balance the
      // reference counts for correctness.
      if (IsThick)
        fixupReferenceCounts(I, Loc, CalleeValue, CaptureArgs);

      // Now that the IR is correct, see if we can remove dead callee
      // computations (e.g. dead partial_apply closures).
      cleanupCalleeValue(CalleeValue, CaptureArgs, FullArgs);

      // Reposition iterators possibly invalidated by mutation.
      FI = SILFunction::iterator(ApplyBlock);
      E = ApplyBlock->end();
      assert(FI == SILFunction::iterator(I->getParent()) &&
             "Mismatch between the instruction and basic block");
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

namespace {
class MandatoryInlining : public SILModuleTransform {
  /// The entry point to the transformation.
  void run() override {
    ClassHierarchyAnalysis *CHA = getAnalysis<ClassHierarchyAnalysis>();
    SILModule *M = getModule();
    SILModule::LinkingMode Mode = getOptions().LinkMode;
    bool ShouldCleanup = !getOptions().DebugSerialization;
    DenseFunctionSet FullyInlinedSet;
    ImmutableFunctionSet::Factory SetFactory;

    for (auto &F : *M) {
      
      // Don't inline into thunks, even transparent callees.
      if (F.isThunk())
        continue;

      runOnFunctionRecursively(&F,
                               FullApplySite(static_cast<ApplyInst*>(nullptr)),
                               Mode, FullyInlinedSet,
                               SetFactory, SetFactory.getEmptySet(), CHA);
    }

    // Make sure that we de-serialize all transparent functions,
    // even if we didn't inline them for some reason.
    // Transparent functions are not available externally, so we
    // have to generate code for them.
    M->linkTransparentFunctions();
    
    if (!ShouldCleanup)
      return;

    bool isWholeModule = M->isWholeModule();
    
    // Now that we've inlined some functions, clean up.  If there are any
    // transparent functions that are deserialized from another module that are
    // now unused, just remove them from the module.
    //
    // We do this with a simple linear scan, because transparent functions that
    // reference each other have already been flattened.
    for (auto FI = M->begin(), E = M->end(); FI != E; ) {
      SILFunction &F = *FI++;

      if (F.getRefCount() != 0) continue;

      // Leave non-transparent functions alone.
      if (!F.isTransparent())
        continue;

      // We discard functions that don't have external linkage,
      // e.g. deserialized functions, internal functions, and thunks.
      // Being marked transparent controls this.
      if (isPossiblyUsedExternally(F.getLinkage(), isWholeModule)) continue;

      // ObjC functions are called through the runtime and are therefore alive
      // even if not referenced inside SIL.
      if (F.getRepresentation() == SILFunctionTypeRepresentation::ObjCMethod)
        continue;
      
      // Okay, just erase the function from the module.
      M->eraseFunction(&F);
    }

    invalidateAnalysis(SILAnalysis::InvalidationKind::Everything);
  }

  StringRef getName() override { return "Mandatory Inlining"; }
};
} // end anonymous namespace

SILTransform *swift::createMandatoryInlining() {
  return new MandatoryInlining();
}
