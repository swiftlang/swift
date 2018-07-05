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
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Debug.h"
using namespace swift;

bool SILInliner::canInlineFunction(FullApplySite AI) {
  return AI.getFunction() != &Original;
}

/// Utility class for rewiring control-flow of inlined begin_apply functions.
class BeginApplySite {
  SmallVector<SILBasicBlock *, 4> ExitingBlocks;
  SmallVector<AllocStackInst*, 8> YieldedIndirectValues;
  SILLocation Loc;
  SILBuilder &Builder;
  BeginApplyInst *BeginApply;
  SILFunction *F;
  EndApplyInst *EndApply = nullptr;
  SILBasicBlock *EndApplyBB = nullptr;
  SILBasicBlock *EndApplyBBMerge = nullptr;
  AbortApplyInst *AbortApply = nullptr;
  SILBasicBlock *AbortApplyBB = nullptr;
  SILBasicBlock *AbortApplyBBMerge = nullptr;
  SILArgument *IntToken = nullptr;

  unsigned YieldNum = 0;
  SmallVector<SILBasicBlock*, 8> YieldResumes;
  SmallVector<SILBasicBlock*, 8> YieldUnwinds;

  void
  getYieldCaseBBs(SmallVectorImpl<std::pair<SILValue, SILBasicBlock *>> &Result,
                  SmallVectorImpl<SILBasicBlock *> &Dests) {
    unsigned Token = 0;
    for (auto *Blk : Dests) {
      Result.push_back(std::make_pair(
          SILValue(Builder.createIntegerLiteral(
              Loc,
              SILType::getBuiltinIntegerType(
                  32, Builder.getFunction().getModule().getASTContext()),
              Token++)),
          Blk));
    }
  }

public:
  BeginApplySite(BeginApplyInst *BeginApply, SILLocation Loc,
                 SILBuilder &Builder)
      : Loc(Loc), Builder(Builder), BeginApply(BeginApply),
        F(BeginApply->getFunction()) {}

  static Optional<BeginApplySite> isa(FullApplySite AI, SILLocation Loc,
                                      SILBuilder &Builder) {
    auto *BeginApply = dyn_cast<BeginApplyInst>(AI);
    if (!BeginApply)
      return None;
    return BeginApplySite(BeginApply, Loc, Builder);
  }

  void collectCallerExitingBlocks() {
    F->findExitingBlocks(ExitingBlocks);
  }

  void processApply(SILBasicBlock *ReturnToBB) {
    // Handle direct and indirect results.
    for (auto YieldedValue : BeginApply->getYieldedValues()) {
      // Insert an alloc_stack for indirect results.
      if (YieldedValue->getType().isAddress()) {
        Builder.setInsertionPoint(F->getEntryBlock()->begin());
        auto Addr = Builder.createAllocStack(
            Loc, YieldedValue->getType().getObjectType());
        YieldedValue->replaceAllUsesWith(Addr);
        YieldedIndirectValues.push_back(Addr);
        for (auto *Exit : ExitingBlocks) {
          Builder.setInsertionPoint(Exit->getTerminator());
          Builder.createDeallocStack(Loc, Addr);
        }
        continue;
      }
      // Insert a phi for direct results.
      auto *RetArg = ReturnToBB->createPHIArgument(YieldedValue->getType(),
                                                   ValueOwnershipKind::Owned);
      // Replace all uses of the ApplyInst with the new argument.
      YieldedValue->replaceAllUsesWith(RetArg);
    }

    // Add a trailing phi argument for the token integer (tells us which yield
    // we came from).
    IntToken = ReturnToBB->createPHIArgument(
        SILType::getBuiltinIntegerType(32, F->getModule().getASTContext()),
        ValueOwnershipKind::Owned);

    // Get the end_apply, abort_apply instructions.
    auto Token = BeginApply->getTokenResult();
    for (auto *TokenUse : Token->getUses()) {
      EndApply = dyn_cast<EndApplyInst>(TokenUse->getUser());
      if (EndApply)
        continue;
      AbortApply = cast<AbortApplyInst>(TokenUse->getUser());
    }

    // Split the basic block before the end/abort_apply. We will insert code
    // to jump to the resume/unwind blocks depending on the integer token
    // later. And the inlined resume/unwind return blocks will jump back to
    // the merge blocks.
    EndApplyBB = EndApply->getParent();
    EndApplyBBMerge = EndApplyBB->split(SILBasicBlock::iterator(EndApply));
    if (AbortApply) {
      AbortApplyBB = AbortApply->getParent();
      AbortApplyBBMerge =
          AbortApplyBB->split(SILBasicBlock::iterator(AbortApply));
    }
  }

  void processTerminator(
      TermInst *Terminator, SILBasicBlock *ReturnToBB,
      llvm::function_ref<SILBasicBlock *(SILBasicBlock *)> remapBlock,
      llvm::function_ref<SILValue(SILValue)> remapValue,
      llvm::function_ref<void(TermInst *)> mapTerminator) {
    // A yield branches to the begin_apply return block passing the yielded
    // results as branch arguments. Collect the yields target block for
    // resuming later. Pass an integer token to the begin_apply return block
    // to mark the yield we came from.
    if (auto *Yield = dyn_cast<YieldInst>(Terminator)) {
      YieldResumes.push_back(remapBlock(Yield->getResumeBB()));
      YieldUnwinds.push_back(remapBlock(Yield->getUnwindBB()));
      auto ContextToken = Builder.createIntegerLiteral(
          Loc,
          SILType::getBuiltinIntegerType(32, F->getModule().getASTContext()),
          YieldNum++);

      SmallVector<SILValue, 8> BrResults;
      unsigned IndirectIdx = 0;
      for (auto CalleeYieldedVal : Yield->getYieldedValues()) {
        auto YieldedVal = remapValue(CalleeYieldedVal);
        if (YieldedVal->getType().isAddress()) {
          auto YieldedDestAddr = YieldedIndirectValues[IndirectIdx++];
          Builder.createCopyAddr(Loc, YieldedVal, YieldedDestAddr, IsTake,
                                 IsInitialization);
        } else
          BrResults.push_back(YieldedVal);
      }
      BrResults.push_back(SILValue(ContextToken));
      Builder.createBranch(Loc, ReturnToBB, BrResults);
      return;
    }

    // Return and unwind terminators branch to the end_apply/abort_apply merge
    // block respectively.
    if (auto *RI = dyn_cast<ReturnInst>(Terminator)) {
      Builder.createBranch(Loc, EndApplyBBMerge);
      return;
    }
    if (auto *Unwind = dyn_cast<UnwindInst>(Terminator)) {
      Builder.createBranch(Loc, AbortApplyBBMerge);
      return;
    }

    // Otherwise, we just map the branch instruction.
    assert(!::isa<ThrowInst>(Terminator) &&
           "Unexpected throw instruction in yield_once function");
    mapTerminator(Terminator);
  }

  void dispatchToResumeUnwindBlocks() {
    // Resume edge.
    Builder.setInsertionPoint(EndApplyBB);
    SmallVector<std::pair<SILValue, SILBasicBlock *>, 8> CaseBBs;
    getYieldCaseBBs(CaseBBs, YieldResumes);
    Builder.createSwitchValue(Loc, IntToken, nullptr, CaseBBs);
    EndApply->eraseFromParent();
    // Unwind edge.
    if (AbortApplyBB) {
      Builder.setInsertionPoint(AbortApplyBB);
      SmallVector<std::pair<SILValue, SILBasicBlock *>, 8> CaseBBs;
      getYieldCaseBBs(CaseBBs, YieldUnwinds);
      Builder.createSwitchValue(Loc, IntToken, nullptr, CaseBBs);
      AbortApply->eraseFromParent();
    }
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

  // Increment the ref count for the inlined function, so it doesn't
  // get deleted before we can emit abstract debug info for it.
  CalleeFunction->setInlined();

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

  // Find the existing blocks. We will need them for inlining the co-routine
  // call.
  auto BeginApply = BeginApplySite::isa(AI, Loc.getValue(), getBuilder());
  if (BeginApply)
    BeginApply->collectCallerExitingBlocks();

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
      BeginApply->processApply(ReturnToBB);
    }
  }

  // Now iterate over the callee BBs and fix up the terminators.
  for (auto BI = BBMap.begin(), BE = BBMap.end(); BI != BE; ++BI) {
    getBuilder().setInsertionPoint(BI->second);

    // Coroutine terminators need special handling.
    if (BeginApply) {
      BeginApply->processTerminator(
          BI->first->getTerminator(), ReturnToBB,
          [=](SILBasicBlock *Block) -> SILBasicBlock * {
            return this->remapBasicBlock(Block);
          },
          [=](SILValue Val) -> SILValue {
            return this->remapValue(Val);
          },
          [=](TermInst *Term) { this->visit(Term); });
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
      if (auto *A = dyn_cast<ApplyInst>(AI)) {
        (void)A;
        assert(A->isNonThrowing() &&
               "apply of a function with error result must be non-throwing");
        getBuilder().createUnreachable(Loc.getValue());
        continue;
      }
      auto tryAI = cast<TryApplyInst>(AI);
      auto returnedValue = remapValue(TI->getOperand());
      getBuilder().createBranch(Loc.getValue(), tryAI->getErrorBB(),
                                returnedValue);
      continue;
    }

    // Otherwise use normal visitor, which clones the existing instruction
    // but remaps basic blocks and values.
    visit(BI->first->getTerminator());
  }

  // Insert dispatch code at end/abort_apply to the resume/unwind target blocks.
  if (BeginApply)
    BeginApply->dispatchToResumeUnwindBlocks();
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

  auto &M = getBuilder().getFunction().getModule();
  auto InlinedAt =
      getOrCreateInlineScope(CalleeScope->InlinedCallSite);
  auto ParentScope = CalleeScope->Parent.dyn_cast<const SILDebugScope *>();
  auto *InlinedScope = new (M) SILDebugScope(
      CalleeScope->Loc, CalleeScope->Parent.dyn_cast<SILFunction *>(),
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
  case SILInstructionKind::StrongPinInst:
  case SILInstructionKind::StrongReleaseInst:
  case SILInstructionKind::SetDeallocatingInst:
  case SILInstructionKind::StrongRetainInst:
  case SILInstructionKind::StrongUnpinInst:
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
  case SILInstructionKind::IsUniqueOrPinnedInst:
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
