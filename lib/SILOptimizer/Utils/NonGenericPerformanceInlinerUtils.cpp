//===- NonGenericInlinerUtils.cpp - Basic cost based non-generic inlining -===//
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

#define DEBUG_TYPE "sil-inliner"
#include "swift/SILOptimizer/Transforms/PerformanceInliner.h"

using namespace swift;

/// Return true if inlining this call site is profitable.
bool SILPerformanceInliner::isProfitableToInlineNonGeneric(FullApplySite AI,
                                              Weight CallerWeight,
                                              ConstantTracker &callerTracker,
                                              int &NumCallerBlocks) {
  assert(AI.getSubstitutions().empty() &&
         "Expected a non-generic apply");

  SILFunction *Callee = AI.getReferencedFunction();

  if (Callee->getInlineStrategy() == AlwaysInline)
    return true;

  SILLoopInfo *LI = LA->get(Callee);
  ShortestPathAnalysis *SPA = getSPA(Callee, LI);
  assert(SPA->isValid());

  ConstantTracker constTracker(Callee, &callerTracker, AI);
  DominanceInfo *DT = DA->get(Callee);
  SILBasicBlock *CalleeEntry = &Callee->front();
  DominanceOrder domOrder(CalleeEntry, DT, Callee->size());

  // Calculate the inlining cost of the callee.
  int CalleeCost = 0;
  int Benefit = 0;
  
  // Start with a base benefit.
  int BaseBenefit = RemovedCallBenefit;
  const SILOptions &Opts = Callee->getModule().getOptions();
  
  // For some reason -Ounchecked can accept a higher base benefit without
  // increasing the code size too much.
  if (Opts.Optimization == SILOptions::SILOptMode::OptimizeUnchecked)
    BaseBenefit *= 2;

  CallerWeight.updateBenefit(Benefit, BaseBenefit);

  // Go through all blocks of the function, accumulate the cost and find
  // benefits.
  while (SILBasicBlock *block = domOrder.getNext()) {
    constTracker.beginBlock();
    Weight BlockW = SPA->getWeight(block, CallerWeight);

    for (SILInstruction &I : *block) {
      constTracker.trackInst(&I);
      
      CalleeCost += (int)instructionInlineCost(I);

      if (FullApplySite AI = FullApplySite::isa(&I)) {
        
        // Check if the callee is passed as an argument. If so, increase the
        // threshold, because inlining will (probably) eliminate the closure.
        SILInstruction *def = constTracker.getDefInCaller(AI.getCallee());
        if (def && (isa<FunctionRefInst>(def) || isa<PartialApplyInst>(def)))
          BlockW.updateBenefit(Benefit, RemovedClosureBenefit);
      } else if (auto *LI = dyn_cast<LoadInst>(&I)) {
        // Check if it's a load from a stack location in the caller. Such a load
        // might be optimized away if inlined.
        if (constTracker.isStackAddrInCaller(LI->getOperand()))
          BlockW.updateBenefit(Benefit, RemovedLoadBenefit);
      } else if (auto *SI = dyn_cast<StoreInst>(&I)) {
        // Check if it's a store to a stack location in the caller. Such a load
        // might be optimized away if inlined.
        if (constTracker.isStackAddrInCaller(SI->getDest()))
          BlockW.updateBenefit(Benefit, RemovedStoreBenefit);
      } else if (isa<StrongReleaseInst>(&I) || isa<ReleaseValueInst>(&I)) {
        SILValue Op = stripCasts(I.getOperand(0));
        if (SILArgument *Arg = dyn_cast<SILArgument>(Op)) {
          if (Arg->isFunctionArg() && Arg->getArgumentConvention() ==
              SILArgumentConvention::Direct_Guaranteed) {
            BlockW.updateBenefit(Benefit, RefCountBenefit);
          }
        }
      } else if (auto *BI = dyn_cast<BuiltinInst>(&I)) {
        if (BI->getBuiltinInfo().ID == BuiltinValueKind::OnFastPath)
          BlockW.updateBenefit(Benefit, FastPathBuiltinBenefit);
      }
    }
    // Don't count costs in blocks which are dead after inlining.
    SILBasicBlock *takenBlock = constTracker.getTakenBlock(block->getTerminator());
    if (takenBlock) {
      BlockW.updateBenefit(Benefit, RemovedTerminatorBenefit);
      domOrder.pushChildrenIf(block, [=] (SILBasicBlock *child) {
        return child->getSinglePredecessor() != block || child == takenBlock;
      });
    } else {
      domOrder.pushChildren(block);
    }
  }

  if (AI.getFunction()->isThunk()) {
    // Only inline trivial functions into thunks (which will not increase the
    // code size).
    if (CalleeCost > TrivialFunctionThreshold)
      return false;

    DEBUG(
      
      dumpCaller(AI.getFunction());
      llvm::dbgs() << "    decision {" << CalleeCost << " into thunk} " <<
          Callee->getName() << '\n';
    );
    return true;
  }

  // We reduce the benefit if the caller is too large. For this we use a
  // cubic function on the number of caller blocks. This starts to prevent
  // inlining at about 800 - 1000 caller blocks.
  int blockMinus =
    (NumCallerBlocks * NumCallerBlocks) / BlockLimitDenominator *
                        NumCallerBlocks / BlockLimitDenominator;
  Benefit -= blockMinus;

  // This is the final inlining decision.
  if (CalleeCost > Benefit) {
    return false;
  }

  NumCallerBlocks += Callee->size();

  DEBUG(
    dumpCaller(AI.getFunction());
    llvm::dbgs() << "    decision {c=" << CalleeCost << ", b=" << Benefit <<
        ", l=" << SPA->getScopeLength(CalleeEntry, 0) <<
        ", c-w=" << CallerWeight << ", bb=" << Callee->size() <<
        ", c-bb=" << NumCallerBlocks << "} " << Callee->getName() << '\n';
  );
  return true;
}