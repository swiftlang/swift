//===--- SILInliner.cpp - Inlines SIL functions ---------------------------===//
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

#define DEBUG_TYPE "sil-inliner"

#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/StackNesting.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Debug.h"
using namespace swift;

/// Does the given coroutine make any stack allocations that are live across
/// its yields?
static bool allocatesStackAcrossYields(SILFunction *F) {
  assert(F->getLoweredFunctionType()->isCoroutine());

  return hasStackDifferencesAt(&F->getEntryBlock()->front(),
                               [](SILInstruction *i) -> InstructionMatchResult {
    if (isa<YieldInst>(i)) {
      return {
        /*matches*/ true,
        /*halt*/ i->getFunction()->getLoweredFunctionType()->getCoroutineKind()
                       == SILCoroutineKind::YieldOnce
      };
    }

    // Otherwise, search until the end of the function.
    return { false, false };
  });
}

static bool isEndOfApply(SILInstruction *i, BeginApplyInst *beginApply) {
  if (auto endApply = dyn_cast<EndApplyInst>(i)) {
    return endApply->getBeginApply() == beginApply;
  } else if (auto abortApply = dyn_cast<AbortApplyInst>(i)) {
    return abortApply->getBeginApply() == beginApply;
  } else {
    return false;
  }
}

/// Are there any stack differences from the given begin_apply to any
/// corresponding end_apply/abort_apply?
static bool hasStackDifferencesAtEnds(BeginApplyInst *apply) {
  return hasStackDifferencesAt(apply, [apply](SILInstruction *i)
                                                     -> InstructionMatchResult {
    // Search for ends of the original apply.  We can stop searching
    // at these points.
    if (isEndOfApply(i, apply))
      return { true, true };
    return { false, false };
  });
}

static bool canInlineBeginApply(BeginApplyInst *BA) {
  // Don't inline if we have multiple resumption sites (i.e. end_apply or
  // abort_apply instructions).  The current implementation clones a single
  // copy of the end_apply and abort_apply paths, so it can't handle values
  // that might be live in the caller across different resumption sites.  To
  // handle this in general, we'd need to separately clone the resume/unwind
  // paths into each end/abort.
  bool hasEndApply = false, hasAbortApply = false;
  for (auto tokenUse : BA->getTokenResult()->getUses()) {
    auto user = tokenUse->getUser();
    if (isa<EndApplyInst>(user)) {
      if (hasEndApply) return false;
      hasEndApply = true;
    } else {
      assert(isa<AbortApplyInst>(user));
      if (hasAbortApply) return false;
      hasAbortApply = true;
    }
  }

  // Don't inline a coroutine with multiple yields.  The current
  // implementation doesn't clone code from the caller, so it can't handle
  // values that might be live in the callee across different yields.
  // To handle this in general, we'd need to clone code in the caller,
  // both between the begin_apply and the resumption site and then
  // potentially after the resumption site when there are un-mergeable
  // values alive across it.
  bool hasYield = false;
  for (auto &B : BA->getReferencedFunction()->getBlocks()) {
    if (isa<YieldInst>(B.getTerminator())) {
      if (hasYield) return false;
      hasYield = true;
    }
  }
  // Note that zero yields is fine; it just means the begin_apply is
  // basically noreturn.

  return true;
}

bool SILInliner::canInlineApplySite(FullApplySite apply) {
  if (!apply.canOptimize())
    return false;

  if (auto BA = dyn_cast<BeginApplyInst>(apply))
    return canInlineBeginApply(BA);

  return true;
}

namespace swift {
/// Utility class for rewiring control-flow of inlined begin_apply functions.
class BeginApplySite {
  SILLocation Loc;
  SILBuilder *Builder;
  BeginApplyInst *BeginApply;
  bool HasYield = false;
  bool NeedsStackCorrection;

  EndApplyInst *EndApply = nullptr;
  SILBasicBlock *EndApplyBB = nullptr;
  SILBasicBlock *EndApplyReturnBB = nullptr;

  AbortApplyInst *AbortApply = nullptr;
  SILBasicBlock *AbortApplyBB = nullptr;
  SILBasicBlock *AbortApplyReturnBB = nullptr;

public:
  BeginApplySite(BeginApplyInst *BeginApply, SILLocation Loc,
                 SILBuilder *Builder, bool NeedsStackCorrection)
      : Loc(Loc), Builder(Builder), BeginApply(BeginApply),
        NeedsStackCorrection(NeedsStackCorrection) {}

  static Optional<BeginApplySite> get(FullApplySite AI, SILLocation Loc,
                                      SILBuilder *Builder) {
    auto *BeginApply = dyn_cast<BeginApplyInst>(AI);
    if (!BeginApply)
      return None;

    // We need stack correction if there are both:
    //   - stack allocations in the callee that are live across the yield and
    //   - stack differences in the caller from the begin_apply to any
    //     end_apply or abort_apply.
    // In these cases, naive cloning will cause the allocations to become
    // improperly nested.
    //
    // We need to compute this here before we do any splitting in the parent
    // function.
    bool NeedsStackCorrection = false;
    if (allocatesStackAcrossYields(BeginApply->getReferencedFunction()) &&
        hasStackDifferencesAtEnds(BeginApply))
      NeedsStackCorrection = true;

    return BeginApplySite(BeginApply, Loc, Builder, NeedsStackCorrection);
  }

  void preprocess(SILBasicBlock *returnToBB) {
    // Get the end_apply, abort_apply instructions.
    auto Token = BeginApply->getTokenResult();
    for (auto *TokenUse : Token->getUses()) {
      if (auto End = dyn_cast<EndApplyInst>(TokenUse->getUser())) {
        collectEndApply(End);
      } else {
        collectAbortApply(cast<AbortApplyInst>(TokenUse->getUser()));
      }
    }
  }

  // Split the basic block before the end/abort_apply. We will insert code
  // to jump to the resume/unwind blocks depending on the integer token
  // later. And the inlined resume/unwind return blocks will jump back to
  // the merge blocks.
  void collectEndApply(EndApplyInst *End) {
    assert(!EndApply);
    EndApply = End;
    EndApplyBB = EndApply->getParent();
    EndApplyReturnBB = EndApplyBB->split(SILBasicBlock::iterator(EndApply));
  }
  void collectAbortApply(AbortApplyInst *Abort) {
    assert(!AbortApply);
    AbortApply = Abort;
    AbortApplyBB = AbortApply->getParent();
    AbortApplyReturnBB = AbortApplyBB->split(SILBasicBlock::iterator(Abort));
  }

  /// Perform special processing for the given terminator if necessary.
  ///
  /// \return false to use the normal inlining logic
  bool processTerminator(
      TermInst *terminator, SILBasicBlock *returnToBB,
      llvm::function_ref<SILBasicBlock *(SILBasicBlock *)> remapBlock,
      llvm::function_ref<SILValue(SILValue)> getMappedValue) {
    // A yield branches to the begin_apply return block passing the yielded
    // results as branch arguments. Collect the yields target block for
    // resuming later. Pass an integer token to the begin_apply return block
    // to mark the yield we came from.
    if (auto *yield = dyn_cast<YieldInst>(terminator)) {
      assert(!HasYield);
      HasYield = true;

      // Pairwise replace the yielded values of the BeginApply with the
      // values that were yielded.
      auto calleeYields = yield->getYieldedValues();
      auto callerYields = BeginApply->getYieldedValues();
      assert(calleeYields.size() == callerYields.size());
      for (auto i : indices(calleeYields)) {
        auto remappedYield = getMappedValue(calleeYields[i]);
        callerYields[i]->replaceAllUsesWith(remappedYield);
      }
      Builder->createBranch(Loc, returnToBB);

      // Add branches at the resumption sites to the resume/unwind block.
      if (EndApply) {
        SavedInsertionPointRAII savedIP(*Builder, EndApplyBB);
        auto resumeBB = remapBlock(yield->getResumeBB());
        Builder->createBranch(EndApply->getLoc(), resumeBB);
      }
      if (AbortApply) {
        SavedInsertionPointRAII savedIP(*Builder, AbortApplyBB);
        auto unwindBB = remapBlock(yield->getUnwindBB());
        Builder->createBranch(AbortApply->getLoc(), unwindBB);
      }
      return true;
    }

    // 'return' and 'unwind' instructions turn into branches to the
    // end_apply/abort_apply return blocks, respectively.  If those blocks
    // are null, it's because there weren't any of the corresponding
    // instructions in the caller.  That means this entire path is
    // unreachable.
    if (isa<ReturnInst>(terminator) || isa<UnwindInst>(terminator)) {
      bool isNormal = isa<ReturnInst>(terminator);
      auto returnBB = isNormal ? EndApplyReturnBB : AbortApplyReturnBB;
      if (returnBB) {
        Builder->createBranch(Loc, returnBB);
      } else {
        Builder->createUnreachable(Loc);
      }
      return true;
    }

    assert(!isa<ThrowInst>(terminator) &&
           "Unexpected throw instruction in yield_once function");

    // Otherwise, we just map the instruction normally.
    return false;
  }

  /// Complete the begin_apply-specific inlining work. Delete vestiges of the
  /// apply site except the callee value. Return a valid iterator after the
  /// original begin_apply.
  void complete() {
    // If there was no yield in the coroutine, then control never reaches
    // the end of the begin_apply, so all the downstream code is unreachable.
    // Make sure the function is well-formed, since we otherwise rely on
    // having visited a yield instruction.
    if (!HasYield) {
      // Make sure the split resumption blocks have terminators.
      if (EndApplyBB) {
        SavedInsertionPointRAII savedIP(*Builder, EndApplyBB);
        Builder->createUnreachable(Loc);
      }
      if (AbortApplyBB) {
        SavedInsertionPointRAII savedIP(*Builder, AbortApplyBB);
        Builder->createUnreachable(Loc);
      }

      // Replace all the yielded values in the callee with undef.
      for (auto calleeYield : BeginApply->getYieldedValues()) {
        calleeYield->replaceAllUsesWith(
            SILUndef::get(calleeYield->getType(), Builder->getModule()));
      }
    }

    // Remove the resumption sites.
    if (EndApply)
      EndApply->eraseFromParent();
    if (AbortApply)
      AbortApply->eraseFromParent();

    assert(!BeginApply->hasUsesOfAnyResult());

    // Correct the stack if necessary.
    if (NeedsStackCorrection) {
      StackNesting().correctStackNesting(BeginApply->getFunction());
    }
  }
};
} // namespace swift

namespace swift {
class SILInlineCloner
    : public TypeSubstCloner<SILInlineCloner, SILOptFunctionBuilder> {
  friend class SILInstructionVisitor<SILInlineCloner>;
  friend class SILCloner<SILInlineCloner>;
  using SuperTy = TypeSubstCloner<SILInlineCloner, SILOptFunctionBuilder>;
  using InlineKind = SILInliner::InlineKind;

  SILOptFunctionBuilder &FuncBuilder;
  InlineKind IKind;

  // The original, noninlined apply site. These become invalid after fixUp,
  // which is called as the last step in SILCloner::cloneFunctionBody.
  FullApplySite Apply;
  Optional<BeginApplySite> BeginApply;

  SILInliner::DeletionFuncTy DeletionCallback;

  /// The location representing the inlined instructions.
  ///
  /// This location wraps the call site AST node that is being inlined.
  /// Alternatively, it can be the SIL file location of the call site (in case
  /// of SIL-to-SIL transformations).
  Optional<SILLocation> Loc;
  const SILDebugScope *CallSiteScope = nullptr;
  llvm::SmallDenseMap<const SILDebugScope *, const SILDebugScope *, 8>
      InlinedScopeCache;

  // Block in the original caller serving as the successor of the inlined
  // control path.
  SILBasicBlock *ReturnToBB = nullptr;

  // Keep track of the next instruction after inlining the call.
  SILBasicBlock::iterator NextIter;

public:
  SILInlineCloner(SILFunction *CalleeFunction, FullApplySite Apply,
                  SILOptFunctionBuilder &FuncBuilder, InlineKind IKind,
                  SubstitutionMap ApplySubs,
                  SILOpenedArchetypesTracker &OpenedArchetypesTracker,
                  SILInliner::DeletionFuncTy deletionCallback);

  SILFunction *getCalleeFunction() const { return &Original; }

  SILBasicBlock::iterator cloneInline(ArrayRef<SILValue> AppliedArgs);

protected:
  SILValue borrowFunctionArgument(SILValue callArg, FullApplySite AI);

  void visitDebugValueInst(DebugValueInst *Inst);
  void visitDebugValueAddrInst(DebugValueAddrInst *Inst);

  void visitTerminator(SILBasicBlock *BB);

  /// This hook is called after either of the top-level visitors:
  /// cloneReachableBlocks or cloneSILFunction.
  ///
  /// After fixUp, the SIL must be valid and semantically equivalent to the SIL
  /// before cloning.
  void fixUp(SILFunction *calleeFunction);

  const SILDebugScope *getOrCreateInlineScope(const SILDebugScope *DS);

  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    // We just updated the debug scope information. Intentionally
    // don't call SILClonerWithScopes<SILInlineCloner>::postProcess().
    SILCloner<SILInlineCloner>::postProcess(Orig, Cloned);
  }

  SILLocation remapLocation(SILLocation InLoc) {
    // For performance inlining return the original location.
    if (IKind == InlineKind::PerformanceInline)
      return InLoc;
    // Inlined location wraps the call site that is being inlined, regardless
    // of the input location.
    return Loc.hasValue()
               ? Loc.getValue()
               : MandatoryInlinedLocation::getMandatoryInlinedLocation(
                     (Decl *)nullptr);
  }

  const SILDebugScope *remapScope(const SILDebugScope *DS) {
    if (IKind == InlineKind::MandatoryInline)
      // Transparent functions are absorbed into the call
      // site. No soup, err, debugging for you!
      return CallSiteScope;
    else
      // Create an inlined version of the scope.
      return getOrCreateInlineScope(DS);
  }
};
} // namespace swift

std::pair<SILBasicBlock::iterator, SILBasicBlock *>
SILInliner::inlineFunction(SILFunction *calleeFunction, FullApplySite apply,
                           ArrayRef<SILValue> appliedArgs) {
  assert(canInlineApplySite(apply)
         && "Asked to inline function that is unable to be inlined?!");

  SILInlineCloner cloner(calleeFunction, apply, FuncBuilder, IKind, ApplySubs,
                         OpenedArchetypesTracker, DeletionCallback);
  auto nextI = cloner.cloneInline(appliedArgs);
  return std::make_pair(nextI, cloner.getLastClonedBB());
}

SILInlineCloner::SILInlineCloner(
    SILFunction *calleeFunction, FullApplySite apply,
    SILOptFunctionBuilder &funcBuilder, InlineKind inlineKind,
    SubstitutionMap applySubs,
    SILOpenedArchetypesTracker &openedArchetypesTracker,
    SILInliner::DeletionFuncTy deletionCallback)
    : SuperTy(*apply.getFunction(), *calleeFunction, applySubs,
              openedArchetypesTracker, /*Inlining=*/true),
      FuncBuilder(funcBuilder), IKind(inlineKind), Apply(apply),
      DeletionCallback(deletionCallback) {

  SILFunction &F = getBuilder().getFunction();
  assert(apply.getFunction() && apply.getFunction() == &F
         && "Inliner called on apply instruction in wrong function?");
  assert(((calleeFunction->getRepresentation()
               != SILFunctionTypeRepresentation::ObjCMethod
           && calleeFunction->getRepresentation()
                  != SILFunctionTypeRepresentation::CFunctionPointer)
          || IKind == InlineKind::PerformanceInline)
         && "Cannot inline Objective-C methods or C functions in mandatory "
            "inlining");

  // Compute the SILLocation which should be used by all the inlined
  // instructions.
  if (IKind == InlineKind::PerformanceInline)
    Loc = InlinedLocation::getInlinedLocation(apply.getLoc());
  else {
    assert(IKind == InlineKind::MandatoryInline && "Unknown InlineKind.");
    Loc = MandatoryInlinedLocation::getMandatoryInlinedLocation(apply.getLoc());
  }

  auto applyScope = apply.getDebugScope();
  // FIXME: Turn this into an assertion instead.
  if (!applyScope)
    applyScope = apply.getFunction()->getDebugScope();

  if (IKind == InlineKind::MandatoryInline) {
    // Mandatory inlining: every instruction inherits scope/location
    // from the call site.
    CallSiteScope = applyScope;
  } else {
    // Performance inlining. Construct a proper inline scope pointing
    // back to the call site.
    CallSiteScope = new (F.getModule()) SILDebugScope(
        apply.getLoc(), nullptr, applyScope, applyScope->InlinedCallSite);
  }
  assert(CallSiteScope && "call site has no scope");
  assert(CallSiteScope->getParentFunction() == &F);

  // Set up the coroutine-specific inliner if applicable.
  BeginApply = BeginApplySite::get(apply, Loc.getValue(), &getBuilder());
}

// Clone the entire callee function into the caller function at the apply site.
// Delete the original apply and all dead arguments except the callee. Return an
// iterator the the first instruction after the original apply.
SILBasicBlock::iterator
SILInlineCloner::cloneInline(ArrayRef<SILValue> AppliedArgs) {
  assert(getCalleeFunction()->getArguments().size() == AppliedArgs.size()
         && "Unexpected number of callee arguments.");

  getBuilder().setInsertionPoint(Apply.getInstruction());

  SmallVector<SILValue, 4> entryArgs;
  entryArgs.reserve(AppliedArgs.size());
  auto calleeConv = getCalleeFunction()->getConventions();
  for (unsigned argIdx = 0, endIdx = AppliedArgs.size(); argIdx < endIdx;
       ++argIdx) {
    SILValue callArg = AppliedArgs[argIdx];
    // Insert begin/end borrow for guaranteed arguments.
    if (argIdx >= calleeConv.getSILArgIndexOfFirstParam()
        && calleeConv.getParamInfoForSILArg(argIdx).isGuaranteed()) {
      callArg = borrowFunctionArgument(callArg, Apply);
    }
    entryArgs.push_back(callArg);
  }

  // Create the return block and set ReturnToBB for use in visitTerminator
  // callbacks.
  SILBasicBlock *callerBB = Apply.getParent();
  switch (Apply.getKind()) {
  case FullApplySiteKind::ApplyInst: {
    auto *AI = dyn_cast<ApplyInst>(Apply);

    // Split the BB and do NOT create a branch between the old and new
    // BBs; we will create the appropriate terminator manually later.
    ReturnToBB =
      callerBB->split(std::next(Apply.getInstruction()->getIterator()));

    // Create an argument on the return-to BB representing the returned value.
    auto *retArg =
        ReturnToBB->createPhiArgument(AI->getType(), ValueOwnershipKind::Owned);
    // Replace all uses of the ApplyInst with the new argument.
    AI->replaceAllUsesWith(retArg);
    break;
  }
  case FullApplySiteKind::BeginApplyInst:
    ReturnToBB =
      callerBB->split(std::next(Apply.getInstruction()->getIterator()));
    BeginApply->preprocess(ReturnToBB);
    break;

  case FullApplySiteKind::TryApplyInst:
    ReturnToBB = cast<TryApplyInst>(Apply)->getNormalBB();
    break;
  }

  // Visit original BBs in depth-first preorder, starting with the
  // entry block, cloning all instructions and terminators.
  //
  // NextIter is initialized during `fixUp`.
  cloneFunctionBody(getCalleeFunction(), callerBB, entryArgs);

  // For non-throwing applies, the inlined body now unconditionally branches to
  // the returned-to-code, which was previously part of the call site's basic
  // block. We could trivially merge these blocks now, however, this would be
  // quadratic: O(num-calls-in-block * num-instructions-in-block). Also,
  // guaranteeing that caller instructions following the inlined call are in a
  // separate block gives the inliner control over revisiting only the inlined
  // instructions.
  //
  // Once all calls in a function are inlined, unconditional branches are
  // eliminated by mergeBlocks.
  return NextIter;
}

void SILInlineCloner::visitTerminator(SILBasicBlock *BB) {
  // Coroutine terminators need special handling.
  if (BeginApply) {
    if (BeginApply->processTerminator(
            BB->getTerminator(), ReturnToBB,
            [=](SILBasicBlock *Block) -> SILBasicBlock * {
              return this->remapBasicBlock(Block);
            },
            [=](SILValue Val) -> SILValue { return this->getMappedValue(Val); }))
      return;
  }

  // Modify return terminators to branch to the return-to BB, rather than
  // trying to clone the ReturnInst.
  if (auto *RI = dyn_cast<ReturnInst>(BB->getTerminator())) {
    auto returnedValue = getMappedValue(RI->getOperand());
    getBuilder().createBranch(Loc.getValue(), ReturnToBB, returnedValue);
    return;
  }

  // Modify throw terminators to branch to the error-return BB, rather than
  // trying to clone the ThrowInst.
  if (auto *TI = dyn_cast<ThrowInst>(BB->getTerminator())) {
    switch (Apply.getKind()) {
    case FullApplySiteKind::ApplyInst:
      assert(cast<ApplyInst>(Apply)->isNonThrowing()
             && "apply of a function with error result must be non-throwing");
      getBuilder().createUnreachable(Loc.getValue());
      return;
    case FullApplySiteKind::BeginApplyInst:
      assert(cast<BeginApplyInst>(Apply)->isNonThrowing()
             && "apply of a function with error result must be non-throwing");
      getBuilder().createUnreachable(Loc.getValue());
      return;
    case FullApplySiteKind::TryApplyInst:
      auto tryAI = cast<TryApplyInst>(Apply);
      auto returnedValue = getMappedValue(TI->getOperand());
      getBuilder().createBranch(Loc.getValue(), tryAI->getErrorBB(),
                                returnedValue);
      return;
    }
  }
  // Otherwise use normal visitor, which clones the existing instruction
  // but remaps basic blocks and values.
  visit(BB->getTerminator());
}

void SILInlineCloner::fixUp(SILFunction *calleeFunction) {
  // "Completing" the BeginApply only fixes the end of the apply scope. The
  // begin_apply itself lingers.
  if (BeginApply)
    BeginApply->complete();

  NextIter = std::next(Apply.getInstruction()->getIterator());

  assert(!Apply.getInstruction()->hasUsesOfAnyResult());

  auto deleteCallback = [this](SILInstruction *deletedI) {
    if (DeletionCallback)
      DeletionCallback(deletedI);
  };
  NextIter = recursivelyDeleteTriviallyDeadInstructions(Apply.getInstruction(),
                                                        true, deleteCallback);
}

SILValue SILInlineCloner::borrowFunctionArgument(SILValue callArg,
                                                 FullApplySite AI) {
  if (!AI.getFunction()->hasQualifiedOwnership()
      || callArg.getOwnershipKind() != ValueOwnershipKind::Owned) {
    return callArg;
  }
  auto *borrow = getBuilder().createBeginBorrow(AI.getLoc(), callArg);
  if (auto *tryAI = dyn_cast<TryApplyInst>(AI)) {
    SILBuilder returnBuilder(tryAI->getNormalBB()->begin());
    returnBuilder.createEndBorrow(AI.getLoc(), borrow, callArg);

    SILBuilder throwBuilder(tryAI->getErrorBB()->begin());
    throwBuilder.createEndBorrow(AI.getLoc(), borrow, callArg);
  } else {
    SILBuilder returnBuilder(std::next(AI.getInstruction()->getIterator()));
    returnBuilder.createEndBorrow(AI.getLoc(), borrow, callArg);
  }
  return borrow;
}

void SILInlineCloner::visitDebugValueInst(DebugValueInst *Inst) {
  // The mandatory inliner drops debug_value instructions when inlining, as if
  // it were a "nodebug" function in C.
  if (IKind == InlineKind::MandatoryInline) return;

  return SILCloner<SILInlineCloner>::visitDebugValueInst(Inst);
}
void SILInlineCloner::visitDebugValueAddrInst(DebugValueAddrInst *Inst) {
  // The mandatory inliner drops debug_value_addr instructions when inlining, as
  // if it were a "nodebug" function in C.
  if (IKind == InlineKind::MandatoryInline) return;

  return SILCloner<SILInlineCloner>::visitDebugValueAddrInst(Inst);
}

const SILDebugScope *
SILInlineCloner::getOrCreateInlineScope(const SILDebugScope *CalleeScope) {
  if (!CalleeScope)
    return CallSiteScope;
  auto it = InlinedScopeCache.find(CalleeScope);
  if (it != InlinedScopeCache.end())
    return it->second;

  auto &M = getBuilder().getModule();
  auto InlinedAt =
      getOrCreateInlineScope(CalleeScope->InlinedCallSite);

  auto *ParentFunction = CalleeScope->Parent.dyn_cast<SILFunction *>();
  if (ParentFunction)
    ParentFunction = remapParentFunction(
        FuncBuilder, M, ParentFunction, SubsMap,
        getCalleeFunction()->getLoweredFunctionType()->getGenericSignature(),
        ForInlining);

  auto *ParentScope = CalleeScope->Parent.dyn_cast<const SILDebugScope *>();
  auto *InlinedScope = new (M) SILDebugScope(
      CalleeScope->Loc, ParentFunction,
      ParentScope ? getOrCreateInlineScope(ParentScope) : nullptr, InlinedAt);
  InlinedScopeCache.insert({CalleeScope, InlinedScope});
  return InlinedScope;
}

//===----------------------------------------------------------------------===//
//                                 Cost Model
//===----------------------------------------------------------------------===//

static InlineCost getEnforcementCost(SILAccessEnforcement enforcement) {
  switch (enforcement) {
  case SILAccessEnforcement::Unknown:
    llvm_unreachable("evaluating cost of access with unknown enforcement?");
  case SILAccessEnforcement::Dynamic:
    return InlineCost::Expensive;
  case SILAccessEnforcement::Static:
  case SILAccessEnforcement::Unsafe:
    return InlineCost::Free;
  }
  llvm_unreachable("bad enforcement");
}

/// For now just assume that every SIL instruction is one to one with an LLVM
/// instruction. This is of course very much so not true.
InlineCost swift::instructionInlineCost(SILInstruction &I) {
  switch (I.getKind()) {
  case SILInstructionKind::IntegerLiteralInst:
  case SILInstructionKind::FloatLiteralInst:
  case SILInstructionKind::DebugValueInst:
  case SILInstructionKind::DebugValueAddrInst:
  case SILInstructionKind::StringLiteralInst:
  case SILInstructionKind::FixLifetimeInst:
  case SILInstructionKind::EndBorrowInst:
  case SILInstructionKind::BeginBorrowInst:
  case SILInstructionKind::MarkDependenceInst:
  case SILInstructionKind::PreviousDynamicFunctionRefInst:
  case SILInstructionKind::DynamicFunctionRefInst:
  case SILInstructionKind::FunctionRefInst:
  case SILInstructionKind::AllocGlobalInst:
  case SILInstructionKind::GlobalAddrInst:
  case SILInstructionKind::EndLifetimeInst:
  case SILInstructionKind::UncheckedOwnershipConversionInst:
    return InlineCost::Free;

  // Typed GEPs are free.
  case SILInstructionKind::TupleElementAddrInst:
  case SILInstructionKind::StructElementAddrInst:
  case SILInstructionKind::ProjectBlockStorageInst:
    return InlineCost::Free;

  // Aggregates are exploded at the IR level; these are effectively no-ops.
  case SILInstructionKind::TupleInst:
  case SILInstructionKind::StructInst:
  case SILInstructionKind::StructExtractInst:
  case SILInstructionKind::TupleExtractInst:
  case SILInstructionKind::DestructureStructInst:
  case SILInstructionKind::DestructureTupleInst:
    return InlineCost::Free;

  // Unchecked casts are free.
  case SILInstructionKind::AddressToPointerInst:
  case SILInstructionKind::PointerToAddressInst:

  case SILInstructionKind::UncheckedRefCastInst:
  case SILInstructionKind::UncheckedRefCastAddrInst:
  case SILInstructionKind::UncheckedAddrCastInst:
  case SILInstructionKind::UncheckedTrivialBitCastInst:
  case SILInstructionKind::UncheckedBitwiseCastInst:

  case SILInstructionKind::RawPointerToRefInst:
  case SILInstructionKind::RefToRawPointerInst:

  case SILInstructionKind::UpcastInst:

  case SILInstructionKind::ThinToThickFunctionInst:
  case SILInstructionKind::ThinFunctionToPointerInst:
  case SILInstructionKind::PointerToThinFunctionInst:
  case SILInstructionKind::ConvertFunctionInst:
  case SILInstructionKind::ConvertEscapeToNoEscapeInst:

  case SILInstructionKind::BridgeObjectToWordInst:
    return InlineCost::Free;

  // Access instructions are free unless we're dynamically enforcing them.
  case SILInstructionKind::BeginAccessInst:
    return getEnforcementCost(cast<BeginAccessInst>(I).getEnforcement());
  case SILInstructionKind::EndAccessInst:
    return getEnforcementCost(cast<EndAccessInst>(I).getBeginAccess()
                                                   ->getEnforcement());
  case SILInstructionKind::BeginUnpairedAccessInst:
    return getEnforcementCost(cast<BeginUnpairedAccessInst>(I)
                                .getEnforcement());
  case SILInstructionKind::EndUnpairedAccessInst:
    return getEnforcementCost(cast<EndUnpairedAccessInst>(I)
                                .getEnforcement());

  // TODO: These are free if the metatype is for a Swift class.
  case SILInstructionKind::ThickToObjCMetatypeInst:
  case SILInstructionKind::ObjCToThickMetatypeInst:
    return InlineCost::Expensive;
    
  // TODO: Bridge object conversions imply a masking operation that should be
  // "hella cheap" but not really expensive.
  case SILInstructionKind::BridgeObjectToRefInst:
  case SILInstructionKind::RefToBridgeObjectInst:
  case SILInstructionKind::ClassifyBridgeObjectInst:
  case SILInstructionKind::ValueToBridgeObjectInst:
    return InlineCost::Expensive;

  case SILInstructionKind::MetatypeInst:
    // Thin metatypes are always free.
    if (cast<MetatypeInst>(I).getType().castTo<MetatypeType>()
          ->getRepresentation() == MetatypeRepresentation::Thin)
      return InlineCost::Free;
    // TODO: Thick metatypes are free if they don't require generic or lazy
    // instantiation.
    return InlineCost::Expensive;

  // Protocol descriptor references are free.
  case SILInstructionKind::ObjCProtocolInst:
    return InlineCost::Free;

  // Metatype-to-object conversions are free.
  case SILInstructionKind::ObjCExistentialMetatypeToObjectInst:
  case SILInstructionKind::ObjCMetatypeToObjectInst:
    return InlineCost::Free;

  // Return and unreachable are free.
  case SILInstructionKind::UnreachableInst:
  case SILInstructionKind::ReturnInst:
  case SILInstructionKind::ThrowInst:
  case SILInstructionKind::UnwindInst:
  case SILInstructionKind::YieldInst:
    return InlineCost::Free;

  case SILInstructionKind::AbortApplyInst:
  case SILInstructionKind::ApplyInst:
  case SILInstructionKind::TryApplyInst:
  case SILInstructionKind::AllocBoxInst:
  case SILInstructionKind::AllocExistentialBoxInst:
  case SILInstructionKind::AllocRefInst:
  case SILInstructionKind::AllocRefDynamicInst:
  case SILInstructionKind::AllocStackInst:
  case SILInstructionKind::AllocValueBufferInst:
  case SILInstructionKind::BindMemoryInst:
  case SILInstructionKind::BeginApplyInst:
  case SILInstructionKind::ValueMetatypeInst:
  case SILInstructionKind::WitnessMethodInst:
  case SILInstructionKind::AssignInst:
  case SILInstructionKind::BranchInst:
  case SILInstructionKind::CheckedCastBranchInst:
  case SILInstructionKind::CheckedCastValueBranchInst:
  case SILInstructionKind::CheckedCastAddrBranchInst:
  case SILInstructionKind::ClassMethodInst:
  case SILInstructionKind::ObjCMethodInst:
  case SILInstructionKind::CondBranchInst:
  case SILInstructionKind::CondFailInst:
  case SILInstructionKind::CopyBlockInst:
  case SILInstructionKind::CopyBlockWithoutEscapingInst:
  case SILInstructionKind::CopyAddrInst:
  case SILInstructionKind::RetainValueInst:
  case SILInstructionKind::RetainValueAddrInst:
  case SILInstructionKind::UnmanagedRetainValueInst:
  case SILInstructionKind::CopyValueInst:
  case SILInstructionKind::DeallocBoxInst:
  case SILInstructionKind::DeallocExistentialBoxInst:
  case SILInstructionKind::DeallocRefInst:
  case SILInstructionKind::DeallocPartialRefInst:
  case SILInstructionKind::DeallocStackInst:
  case SILInstructionKind::DeallocValueBufferInst:
  case SILInstructionKind::DeinitExistentialAddrInst:
  case SILInstructionKind::DeinitExistentialValueInst:
  case SILInstructionKind::DestroyAddrInst:
  case SILInstructionKind::EndApplyInst:
  case SILInstructionKind::ProjectValueBufferInst:
  case SILInstructionKind::ProjectBoxInst:
  case SILInstructionKind::ProjectExistentialBoxInst:
  case SILInstructionKind::ReleaseValueInst:
  case SILInstructionKind::ReleaseValueAddrInst:
  case SILInstructionKind::UnmanagedReleaseValueInst:
  case SILInstructionKind::DestroyValueInst:
  case SILInstructionKind::AutoreleaseValueInst:
  case SILInstructionKind::UnmanagedAutoreleaseValueInst:
  case SILInstructionKind::DynamicMethodBranchInst:
  case SILInstructionKind::EnumInst:
  case SILInstructionKind::IndexAddrInst:
  case SILInstructionKind::TailAddrInst:
  case SILInstructionKind::IndexRawPointerInst:
  case SILInstructionKind::InitEnumDataAddrInst:
  case SILInstructionKind::InitExistentialAddrInst:
  case SILInstructionKind::InitExistentialValueInst:
  case SILInstructionKind::InitExistentialMetatypeInst:
  case SILInstructionKind::InitExistentialRefInst:
  case SILInstructionKind::InjectEnumAddrInst:
  case SILInstructionKind::LoadInst:
  case SILInstructionKind::LoadBorrowInst:
  case SILInstructionKind::OpenExistentialAddrInst:
  case SILInstructionKind::OpenExistentialBoxInst:
  case SILInstructionKind::OpenExistentialBoxValueInst:
  case SILInstructionKind::OpenExistentialMetatypeInst:
  case SILInstructionKind::OpenExistentialRefInst:
  case SILInstructionKind::OpenExistentialValueInst:
  case SILInstructionKind::PartialApplyInst:
  case SILInstructionKind::ExistentialMetatypeInst:
  case SILInstructionKind::RefElementAddrInst:
  case SILInstructionKind::RefTailAddrInst:
  case SILInstructionKind::StoreInst:
  case SILInstructionKind::StoreBorrowInst:
  case SILInstructionKind::StrongReleaseInst:
  case SILInstructionKind::SetDeallocatingInst:
  case SILInstructionKind::StrongRetainInst:
  case SILInstructionKind::SuperMethodInst:
  case SILInstructionKind::ObjCSuperMethodInst:
  case SILInstructionKind::SwitchEnumAddrInst:
  case SILInstructionKind::SwitchEnumInst:
  case SILInstructionKind::SwitchValueInst:
  case SILInstructionKind::UncheckedEnumDataInst:
  case SILInstructionKind::UncheckedTakeEnumDataAddrInst:
  case SILInstructionKind::UnconditionalCheckedCastInst:
  case SILInstructionKind::UnconditionalCheckedCastAddrInst:
  case SILInstructionKind::UnconditionalCheckedCastValueInst:
  case SILInstructionKind::IsEscapingClosureInst:
  case SILInstructionKind::IsUniqueInst:
  case SILInstructionKind::InitBlockStorageHeaderInst:
  case SILInstructionKind::SelectEnumAddrInst:
  case SILInstructionKind::SelectEnumInst:
  case SILInstructionKind::SelectValueInst:
  case SILInstructionKind::KeyPathInst:
  case SILInstructionKind::GlobalValueInst:
#define COMMON_ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name) \
  case SILInstructionKind::Name##ToRefInst: \
  case SILInstructionKind::RefTo##Name##Inst:
#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  case SILInstructionKind::Load##Name##Inst: \
  case SILInstructionKind::Store##Name##Inst:
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  COMMON_ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name) \
  case SILInstructionKind::Name##RetainInst: \
  case SILInstructionKind::Name##ReleaseInst: \
  case SILInstructionKind::StrongRetain##Name##Inst: \
  case SILInstructionKind::Copy##Name##ValueInst:
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, "...") \
  ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, "...")
#define UNCHECKED_REF_STORAGE(Name, ...) \
  COMMON_ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name)
#include "swift/AST/ReferenceStorage.def"
#undef COMMON_ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE
    return InlineCost::Expensive;

  case SILInstructionKind::BuiltinInst: {
    auto *BI = cast<BuiltinInst>(&I);
    // Expect intrinsics are 'free' instructions.
    if (BI->getIntrinsicInfo().ID == llvm::Intrinsic::expect)
      return InlineCost::Free;
    if (BI->getBuiltinInfo().ID == BuiltinValueKind::OnFastPath)
      return InlineCost::Free;

    return InlineCost::Expensive;
  }
  case SILInstructionKind::MarkFunctionEscapeInst:
  case SILInstructionKind::MarkUninitializedInst:
    llvm_unreachable("not valid in canonical sil");
  case SILInstructionKind::ObjectInst:
    llvm_unreachable("not valid in a function");
  }

  llvm_unreachable("Unhandled ValueKind in switch.");
}
