//===- PerformanceInliner.cpp - Basic cost based inlining for performance -===//
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

#define DEBUG_TYPE "sil-inliner"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILAnalysis/ColdBlockInfo.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Utils/SILInliner.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/MapVector.h"
using namespace swift;

STATISTIC(NumFunctionsInlined, "Number of functions inlined");

namespace {

  // Controls the decision to inline functions with @semantics, @effect and
  // global_init attributes.
  enum class InlineSelection {
    Everything,
    NoGlobalInit,
    NoSemanticsAndGlobalInit
  };

  class SILPerformanceInliner {
    /// The inline threashold.
    const unsigned InlineCostThreshold;
    /// The linking mode.
    SILModule::LinkingMode LinkMode;
    /// Specifies which functions not to inline, based on @semantics and
    /// global_init attributes.
    InlineSelection WhatToInline;

  public:
    SILPerformanceInliner(unsigned threshold, SILModule::LinkingMode M,
                          InlineSelection WhatToInline)
      : InlineCostThreshold(threshold), LinkMode(M),
    WhatToInline(WhatToInline) {}

    bool inlineCallsIntoFunction(SILFunction *F, DominanceAnalysis *DA);

  protected:
    bool isProfitableToInline(SILFunction *Caller, SILFunction *Callee,
                              const ApplyInst *AI, unsigned CalleeCount);
  };
}

//===----------------------------------------------------------------------===//
//                            Call Graph Creation
//===----------------------------------------------------------------------===//

/// \brief Returns a SILFunction if this ApplyInst calls a recognizable function
/// that is legal to inline.
static SILFunction *getInlinableFunction(ApplyInst *AI,
                                         SILModule::LinkingMode Mode,
                                         InlineSelection WhatToInline) {
  // Avoid substituion lists, we don't support them.
  if (AI->hasSubstitutions())
    return nullptr;

  auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (!FRI)
    return nullptr;

  SILFunction *F = FRI->getReferencedFunction();

  // Don't inline functions that are marked with the @semantics or @effects
  // attribute if the inliner is asked not to inline them.
  if (F->hasDefinedSemantics() || F->hasSpecifiedEffectsInfo()) {
    if (WhatToInline == InlineSelection::NoSemanticsAndGlobalInit) {
      DEBUG(llvm::dbgs() << " Function " << F->getName()
            << " has special semantics or effects attribute.\n");
      return nullptr;
    }
  } else if (F->isGlobalInit()) {
    if (WhatToInline != InlineSelection::Everything) {
      DEBUG(llvm::dbgs() << " Function " << F->getName()
            << " has the global-init attribute.\n");
      return nullptr;
    }
  }

  // If F is an external declaration, we can't inline...
  if (F->empty() || F->isExternalDeclaration()) {
    DEBUG(llvm::dbgs() << "        FAIL! Cannot inline " << F->getName()
          << ".\n");
    return nullptr;
  }

  DEBUG(llvm::dbgs() << "        SUCCESS! Can inline " << F->getName()
        << ".\n");
  return F;
}

//===----------------------------------------------------------------------===//
//                                  Inliner
//===----------------------------------------------------------------------===//

/// return true if inlining this call site is sane and profitable.
bool SILPerformanceInliner::isProfitableToInline(SILFunction *Caller,
                                                 SILFunction *Callee,
                                                 const ApplyInst *AI,
                                                 unsigned CalleeCount) {
  /// Always inline transparent calls. This should have been done during
  /// MandatoryInlining, but generics are not currenly handled.
  if (AI->isTransparent())
    return true;

  // To handle recursion and prevent massive code size expansion, we prevent
  // inlining the same callee many times into the caller. The recursion
  // detection logic in CallGraphAnalysis can't handle class_method in the
  // callee. To avoid inlining the recursion too many times, we stop at the
  // threshold (currently set to 1024).
  const unsigned CallsToCalleeThreshold = 1024;
  if (CalleeCount > CallsToCalleeThreshold) {
    DEBUG(llvm::dbgs() <<
          "        FAIL! Skipping callees that are called too many times.\n");
    return false;
  }

  // Prevent circular inlining.
  if (Callee == Caller) {
    DEBUG(llvm::dbgs() << "        FAIL! Skipping recursive calls.\n");
    return false;
  }

  // If Callee has a less visible linkage than caller or references something
  // with a less visible linkage than caller, don't inline Callee into caller.
  if (Caller->isFragile() && !Callee->isFragile()) {
    DEBUG(llvm::dbgs() << "        FAIL! Can't inline resilient into fragile.");
    return false;
  }

  // Check if the function takes a closure.
  bool HasClosure = false;
  for (auto &Op : AI->getAllOperands()) {
    if (isa<PartialApplyInst>(Op.get())) {
      HasClosure = true;
      break;
    }
  }
  // If the function accepts a closure increase the threshold because
  // inlining has the potential to eliminate the closure.
  unsigned BoostFactor = HasClosure ? 2 : 1;

  // Calculate the inlining cost of the callee.
  unsigned CalleeCost = getFunctionCost(Callee, Caller,
                                        InlineCostThreshold * BoostFactor);

  // When the cost is UINT_MAX it means that it is illegal to inline.
  // In this case we have to ignore the AlwaysInline strategy.
  if (Callee->getInlineStrategy() == AlwaysInline && CalleeCost < UINT_MAX) {
    return true;
  }

  unsigned Threshold = InlineCostThreshold * BoostFactor;
  if (CalleeCost > Threshold) {
    DEBUG(llvm::dbgs() << "        FAIL! Function too big to inline. "
          "Skipping. CalleeCost: " << CalleeCost << ". Threshold: "
          << Threshold << "\n");
    return false;
  }
  return true;
}

/// \brief Attempt to inline all calls smaller than our threshold into F until.
/// returns True if a function was inlined.
bool SILPerformanceInliner::inlineCallsIntoFunction(SILFunction *Caller,
                                                    DominanceAnalysis *DA) {
  bool Changed = false;
  DEBUG(llvm::dbgs() << "Visiting Function: " << Caller->getName() << "\n");

  llvm::SmallVector<ApplyInst*, 8> CallSites;

  // Keep track of the cold blocks.
  // Avoid recomputing dominance by checking cold call blocks before inlining.
  DominanceInfo *DT = DA->getDomInfo(Caller);
  if (!DT->isValid(Caller))
    DT->recalculate(*Caller);
  ColdBlockInfo ColdBlocks(DA);

  // Collect all of the ApplyInsts in this function. We will be changing the
  // control flow and collecting the AIs simplifies the scan.
  for (auto &BB : *Caller) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      // Check if this is a call site.
      ApplyInst *AI = dyn_cast<ApplyInst>(I++);
      if (AI) {
        // If the call site is on a slow path, do not inline.
        if (!AI->isTransparent() && ColdBlocks.isCold(AI->getParent())) {
          DEBUG(llvm::dbgs() << "    Pruning cold call site:" <<  *AI);
          continue;
        }
        CallSites.push_back(AI);
      }
    }
  }

  // Calculate how many times a callee is called from this caller.
  llvm::DenseMap<SILFunction *, unsigned> CalleeCount; 
  for (auto AI : CallSites) {
    assert(AI && "Invalid AI");
    SILFunction *Callee = getInlinableFunction(AI, LinkMode,
                                               WhatToInline);
    if (Callee)
      CalleeCount[Callee]++;
  }

  for (auto AI : CallSites) {
    DEBUG(llvm::dbgs() << "    Found call site:" <<  *AI);

    // Get the callee.
    SILFunction *Callee = getInlinableFunction(AI, LinkMode,
                                               WhatToInline);
    if (!Callee) {
      DEBUG(llvm::dbgs() << "        FAIL! Cannot find inlineable callee.\n");
      continue;
    }
    
    if (Callee->getInlineStrategy() == NoInline)
      continue;

    if (AI->hasSubstitutions()) {
      DEBUG(llvm::dbgs() << "         FAIL! Generic substitutions present!");
      continue;
    }

    DEBUG(llvm::dbgs() << "        Found callee:" <<  Callee->getName()
          << ".\n");

    if (!isProfitableToInline(Caller, Callee, AI, CalleeCount[Callee]))
      continue;

    // Add the arguments from AI into a SILValue list.
    SmallVector<SILValue, 8> Args;
    for (const auto &Arg : AI->getArguments())
    Args.push_back(Arg);

    // Ok, we are within budget. Attempt to inline.
    DEBUG(llvm::dbgs() << "        SUCCESS! Inlining " << Callee->getName()
          << " Into " << Caller->getName() << "\n");

    // We already moved the iterator to the next instruction because the AI
    // will be erased by the inliner. Notice that we will skip all of the
    // newly inlined ApplyInsts. That's okay because we will visit them in
    // our next invocation of the inliner.
    TypeSubstitutionMap ContextSubs;
    SILInliner Inliner(*Caller, *Callee,
                       SILInliner::InlineKind::PerformanceInline,
                       ContextSubs, AI->getSubstitutions());
    Inliner.inlineFunction(AI, Args);
    DT->reset();
    NumFunctionsInlined++;
    Changed = true;
  }

  DEBUG(llvm::dbgs() << "\n");
  return Changed;
}

namespace {
class SILPerformanceInlinerPass : public SILModuleTransform {
  /// Specifies which functions not to inline, based on @semantics and
  /// global_init attributes.
  InlineSelection WhatToInline;
public:
  SILPerformanceInlinerPass(InlineSelection WhatToInline):
      WhatToInline(WhatToInline) {}

  void run() {
    CallGraphAnalysis* CGA = PM->getAnalysis<CallGraphAnalysis>();
    DominanceAnalysis* DA = PM->getAnalysis<DominanceAnalysis>();

    if (getOptions().InlineThreshold == 0) {
      DEBUG(llvm::dbgs() << "*** The Performance Inliner is disabled ***\n");
      return;
    }

    SILPerformanceInliner inliner(getOptions().InlineThreshold,
                                  SILModule::LinkingMode::LinkAll,
                                  WhatToInline);

    bool Changed = false;
    // Inline functions bottom up from the leafs.
    for (auto *F : CGA->getCallGraph().getBottomUpFunctionOrder()) {
      // If F is empty, attempt to link it. Skip it if we fail to do so.
      if (F->empty() &&
          !getModule()->linkFunction(F, SILModule::LinkingMode::LinkAll))
        continue;

      Changed |= inliner.inlineCallsIntoFunction(F, DA);
    }

    // Invalidate the call graph.
    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
  }

  StringRef getName() override { return "Performance Inliner"; }
};
} // end anonymous namespace

/// Create an inliner pass that does not inline functions that are marked with
/// the @semantics, @effects or global_init attributes.
SILTransform *swift::createEarlyInliner() {
  return new SILPerformanceInlinerPass(InlineSelection::NoSemanticsAndGlobalInit);
}

/// Create an inliner pass that does not inline functions that are marked with
/// the global_init attribute.
SILTransform *swift::createPerfInliner() {
  return new SILPerformanceInlinerPass(InlineSelection::NoGlobalInit);
}

/// Create an inliner pass that inlines all functions that are marked with
/// the @semantics, @effects or global_init attributes.
SILTransform *swift::createLateInliner() {
  return new SILPerformanceInlinerPass(InlineSelection::Everything);
}
