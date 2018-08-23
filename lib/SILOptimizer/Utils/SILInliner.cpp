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
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SIL/SILDebugScope.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Debug.h"
using namespace swift;

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

bool SILInliner::canInline(FullApplySite AI) {
  if (auto BA = dyn_cast<BeginApplyInst>(AI)) {
    return canInlineBeginApply(BA);
  }
  return true;
}

bool SILInliner::canInlineFunction(FullApplySite AI) {
  if (!canInline(AI))
    return false;
  return AI.getFunction() != &Original;
}

/// Utility class for rewiring control-flow of inlined begin_apply functions.
class BeginApplySite {
  SILLocation Loc;
  SILBuilder &Builder;
  BeginApplyInst *BeginApply;
  bool HasYield = false;

  EndApplyInst *EndApply = nullptr;
  SILBasicBlock *EndApplyBB = nullptr;
  SILBasicBlock *EndApplyReturnBB = nullptr;

  AbortApplyInst *AbortApply = nullptr;
  SILBasicBlock *AbortApplyBB = nullptr;
  SILBasicBlock *AbortApplyReturnBB = nullptr;

public:
  BeginApplySite(BeginApplyInst *BeginApply, SILLocation Loc,
                 SILBuilder &Builder)
      : Loc(Loc), Builder(Builder), BeginApply(BeginApply) {}

  static Optional<BeginApplySite> get(FullApplySite AI, SILLocation Loc,
                                      SILBuilder &Builder) {
    auto *BeginApply = dyn_cast<BeginApplyInst>(AI);
    if (!BeginApply)
      return None;
    return BeginApplySite(BeginApply, Loc, Builder);
  }

  void preprocess(SILBasicBlock *ReturnToBB) {
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
      llvm::function_ref<SILValue(SILValue)> remapValue) {
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
        auto remappedYield = remapValue(calleeYields[i]);
        callerYields[i]->replaceAllUsesWith(remappedYield);
      }
      Builder.createBranch(Loc, returnToBB);

      // Add branches at the resumption sites to the resume/unwind block.
      if (EndApply) {
        SavedInsertionPointRAII savedIP(Builder, EndApplyBB);
        auto resumeBB = remapBlock(yield->getResumeBB());
        Builder.createBranch(EndApply->getLoc(), resumeBB);
      }
      if (AbortApply) {
        SavedInsertionPointRAII savedIP(Builder, AbortApplyBB);
        auto unwindBB = remapBlock(yield->getUnwindBB());
        Builder.createBranch(AbortApply->getLoc(), unwindBB);
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
        Builder.createBranch(Loc, returnBB);
      } else {
        Builder.createUnreachable(Loc);
      }
      return true;
    }

    assert(!isa<ThrowInst>(terminator) &&
           "Unexpected throw instruction in yield_once function");

    // Otherwise, we just map the instruction normally.
    return false;
  }

  /// Complete the begin_apply-specific inlining work.
  void complete() {
    // If there was no yield in the coroutine, then control never reaches
    // the end of the begin_apply, so all the downstream code is unreachable.
    // Make sure the function is well-formed, since we otherwise rely on
    // having visited a yield instruction.
    if (!HasYield) {
      // Make sure the split resumption blocks have terminators.
      if (EndApplyBB) {
        SavedInsertionPointRAII savedIP(Builder, EndApplyBB);
        Builder.createUnreachable(Loc);
      }
      if (AbortApplyBB) {
        SavedInsertionPointRAII savedIP(Builder, AbortApplyBB);
        Builder.createUnreachable(Loc);
      }

      // Replace all the yielded values in the callee with undef.
      for (auto calleeYield : BeginApply->getYieldedValues()) {
        calleeYield->replaceAllUsesWith(SILUndef::get(calleeYield->getType(),
                                                      Builder.getModule()));
      }
    }

    // Remove the resumption sites.
    if (EndApply)
      EndApply->eraseFromParent();
    if (AbortApply)
      AbortApply->eraseFromParent();

    assert(!BeginApply->hasUsesOfAnyResult());
  }
};

/// \brief Inlines the callee of a given ApplyInst (which must be the value of a
/// FunctionRefInst referencing a function with a known body), into the caller
/// containing the ApplyInst, which must be the same function as provided to the
/// constructor of SILInliner. It only performs one step of inlining: it does
/// not recursively inline functions called by the callee.
///
/// It is the responsibility of the caller of this function to delete
/// the given ApplyInst when inlining is successful.
///
/// \returns true on success or false if it is unable to inline the function
/// (for any reason).
void SILInliner::inlineFunction(FullApplySite AI, ArrayRef<SILValue> Args) {
  assert(canInlineFunction(AI) &&
         "Asked to inline function that is unable to be inlined?!");

  SILFunction &F = getBuilder().getFunction();
  assert(AI.getFunction() && AI.getFunction() == &F &&
         "Inliner called on apply instruction in wrong function?");
  assert(((CalleeFunction->getRepresentation()
             != SILFunctionTypeRepresentation::ObjCMethod &&
           CalleeFunction->getRepresentation()
             != SILFunctionTypeRepresentation::CFunctionPointer) ||
          IKind == InlineKind::PerformanceInline) &&
         "Cannot inline Objective-C methods or C functions in mandatory "
         "inlining");

  CalleeEntryBB = &*CalleeFunction->begin();

  // Compute the SILLocation which should be used by all the inlined
  // instructions.
  if (IKind == InlineKind::PerformanceInline) {
    Loc = InlinedLocation::getInlinedLocation(AI.getLoc());
  } else {
    assert(IKind == InlineKind::MandatoryInline && "Unknown InlineKind.");
    Loc = MandatoryInlinedLocation::getMandatoryInlinedLocation(AI.getLoc());
  }

  auto AIScope = AI.getDebugScope();
  // FIXME: Turn this into an assertion instead.
  if (!AIScope)
    AIScope = AI.getFunction()->getDebugScope();

  if (IKind == InlineKind::MandatoryInline) {
    // Mandatory inlining: every instruction inherits scope/location
    // from the call site.
    CallSiteScope = AIScope;
  } else {
    // Performance inlining. Construct a proper inline scope pointing
    // back to the call site.
    CallSiteScope = new (F.getModule())
        SILDebugScope(AI.getLoc(), nullptr, AIScope, AIScope->InlinedCallSite);
  }
  assert(CallSiteScope && "call site has no scope");
  assert(CallSiteScope->getParentFunction() == &F);

  // If the caller's BB is not the last BB in the calling function, then keep
  // track of the next BB so we always insert new BBs before it; otherwise,
  // we just leave the new BBs at the end as they are by default.
  auto IBI = std::next(SILFunction::iterator(AI.getParent()));
  InsertBeforeBB = IBI != F.end() ? &*IBI : nullptr;

  BBMap.clear();
  // Do not allow the entry block to be cloned again
  SILBasicBlock::iterator InsertPoint =
      SILBasicBlock::iterator(AI.getInstruction());
  BBMap.insert(std::make_pair(CalleeEntryBB, AI.getParent()));
  getBuilder().setInsertionPoint(InsertPoint);

  // Clear argument map and map ApplyInst arguments to the arguments of the
  // callee's entry block.
  ValueMap.clear();
  assert(CalleeFunction->getArguments().size() == Args.size()
         && "Unexpected number of callee arguments.");
  auto calleeConv = CalleeFunction->getConventions();
  for (unsigned argIdx = 0, endIdx = Args.size(); argIdx < endIdx; ++argIdx) {
    SILValue callArg = Args[argIdx];
    // Insert begin/end borrow for guaranteed arguments.
    if (argIdx >= calleeConv.getSILArgIndexOfFirstParam()
        && calleeConv.getParamInfoForSILArg(argIdx).isGuaranteed()) {
      callArg = borrowFunctionArgument(callArg, AI);
    }
    auto *calleeArg = CalleeFunction->getArgument(argIdx);
    ValueMap.insert(std::make_pair(calleeArg, callArg));
  }

  // Set up the coroutine-specific inliner if applicable.
  auto BeginApply = BeginApplySite::get(AI, Loc.getValue(), getBuilder());

  // Recursively visit callee's BB in depth-first preorder, starting with the
  // entry block, cloning all instructions other than terminators.
  visitSILBasicBlock(CalleeEntryBB);

  // If we're inlining into a normal apply and the callee's entry
  // block ends in a return, then we can avoid a split.
  if (auto nonTryAI = dyn_cast<ApplyInst>(AI)) {
    if (auto *RI = dyn_cast<ReturnInst>(CalleeEntryBB->getTerminator())) {
      // Replace all uses of the apply instruction with the operands of the
      // return instruction, appropriately mapped.
      nonTryAI->replaceAllUsesWith(remapValue(RI->getOperand()));
      return;
    }
  }

  // If we're inlining into a try_apply, we already have a return-to BB.
  SILBasicBlock *ReturnToBB;

  if (auto tryAI = dyn_cast<TryApplyInst>(AI)) {
    ReturnToBB = tryAI->getNormalBB();

  // Otherwise, split the caller's basic block to create a return-to BB.
  } else {
    SILBasicBlock *CallerBB = AI.getParent();
    // Split the BB and do NOT create a branch between the old and new
    // BBs; we will create the appropriate terminator manually later.
    ReturnToBB = CallerBB->split(InsertPoint);
    // Place the return-to BB after all the other mapped BBs.
    if (InsertBeforeBB)
      F.getBlocks().splice(SILFunction::iterator(InsertBeforeBB), F.getBlocks(),
                           SILFunction::iterator(ReturnToBB));
    else
      F.getBlocks().splice(F.getBlocks().end(), F.getBlocks(),
                           SILFunction::iterator(ReturnToBB));

    // Create an argument on the return-to BB representing the returned value.
    if (auto apply = dyn_cast<ApplyInst>(AI.getInstruction())) {
      auto *RetArg = ReturnToBB->createPHIArgument(apply->getType(),
                                                   ValueOwnershipKind::Owned);
      // Replace all uses of the ApplyInst with the new argument.
      apply->replaceAllUsesWith(RetArg);
    } else {
      // Handle begin_apply.
      BeginApply->preprocess(ReturnToBB);
    }
  }

  // Now iterate over the callee BBs and fix up the terminators.
  for (auto BI = BBMap.begin(), BE = BBMap.end(); BI != BE; ++BI) {
    getBuilder().setInsertionPoint(BI->second);

    // Coroutine terminators need special handling.
    if (BeginApply) {
      if (BeginApply->processTerminator(
            BI->first->getTerminator(), ReturnToBB,
            [=](SILBasicBlock *Block) -> SILBasicBlock * {
              return this->remapBasicBlock(Block);
            },
            [=](SILValue Val) -> SILValue {
              return this->remapValue(Val);
            }))
        continue;
    }

    // Modify return terminators to branch to the return-to BB, rather than
    // trying to clone the ReturnInst.
    if (auto *RI = dyn_cast<ReturnInst>(BI->first->getTerminator())) {
      auto thrownValue = remapValue(RI->getOperand());
      getBuilder().createBranch(Loc.getValue(), ReturnToBB,
                                thrownValue);
      continue;
    }

    // Modify throw terminators to branch to the error-return BB, rather than
    // trying to clone the ThrowInst.
    if (auto *TI = dyn_cast<ThrowInst>(BI->first->getTerminator())) {
      auto tryAI = dyn_cast<TryApplyInst>(AI);
      if (!tryAI) {
        assert((isa<ApplyInst>(AI)
                  ? cast<ApplyInst>(AI)->isNonThrowing()
                  : cast<BeginApplyInst>(AI)->isNonThrowing()) &&
               "apply of a function with error result must be non-throwing");
        getBuilder().createUnreachable(Loc.getValue());
        continue;
      }

      auto returnedValue = remapValue(TI->getOperand());
      getBuilder().createBranch(Loc.getValue(), tryAI->getErrorBB(),
                                returnedValue);
      continue;
    }

    // Otherwise use normal visitor, which clones the existing instruction
    // but remaps basic blocks and values.
    visit(BI->first->getTerminator());
  }

  // Clean up after inlining into a begin_apply.
  if (BeginApply)
    BeginApply->complete();
}

SILValue SILInliner::borrowFunctionArgument(SILValue callArg,
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

void SILInliner::visitDebugValueInst(DebugValueInst *Inst) {
  // The mandatory inliner drops debug_value instructions when inlining, as if
  // it were a "nodebug" function in C.
  if (IKind == InlineKind::MandatoryInline) return;

  return SILCloner<SILInliner>::visitDebugValueInst(Inst);
}
void SILInliner::visitDebugValueAddrInst(DebugValueAddrInst *Inst) {
  // The mandatory inliner drops debug_value_addr instructions when inlining, as
  // if it were a "nodebug" function in C.
  if (IKind == InlineKind::MandatoryInline) return;

  return SILCloner<SILInliner>::visitDebugValueAddrInst(Inst);
}

const SILDebugScope *
SILInliner::getOrCreateInlineScope(const SILDebugScope *CalleeScope) {
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
        CalleeFunction->getLoweredFunctionType()->getGenericSignature(),
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
  case SILInstructionKind::ConstStringLiteralInst:
  case SILInstructionKind::FixLifetimeInst:
  case SILInstructionKind::EndBorrowInst:
  case SILInstructionKind::EndBorrowArgumentInst:
  case SILInstructionKind::BeginBorrowInst:
  case SILInstructionKind::MarkDependenceInst:
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
  case SILInstructionKind::MarkUninitializedBehaviorInst:
    llvm_unreachable("not valid in canonical sil");
  case SILInstructionKind::ObjectInst:
    llvm_unreachable("not valid in a function");
  }

  llvm_unreachable("Unhandled ValueKind in switch.");
}
