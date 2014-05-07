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
#include "swift/SIL/CallGraph.h"
#include "swift/SIL/SILModule.h"
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
  class SILPerformanceInliner {
    const unsigned InlineCostThreshold;
    SILModule::LinkingMode Mode;

  public:
    SILPerformanceInliner(unsigned threshold,
                          SILModule::LinkingMode M)
      : InlineCostThreshold(threshold), Mode(M) {}

    bool inlineCallsIntoFunction(SILFunction *F);
  };
}

//===----------------------------------------------------------------------===//
//                            Call Graph Creation
//===----------------------------------------------------------------------===//

/// \brief Returns a SILFunction if this ApplyInst calls a recognizable function
/// that is legal to inline.
static SILFunction *getInlinableFunction(ApplyInst *AI,
                                         SILModule::LinkingMode Mode) {
  // Avoid substituion lists, we don't support them.
  if (AI->hasSubstitutions())
    return nullptr;

  auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (!FRI)
    return nullptr;

  SILFunction *F = FRI->getReferencedFunction();
  // If F is an external declaration, we can't inline...
  if (F->empty() || F->isExternalDeclaration()) {
    DEBUG(llvm::dbgs() << "        FAIL! Can't inline " << F->getName()
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

// Check if F transitively references a global, function, vtable, or witness
// table with a less visible SILLinkage than linkage.
//
// FIXME: When vtables/witness tables get linkage, update this.
static bool transitivelyReferencesLessVisibleLinkage(const SILFunction &F,
                                                     SILLinkage linkage) {
  for (auto &BB : F)
    for (auto &I : BB) {
      if (auto *GA = dyn_cast<SILGlobalAddrInst>(&I))
        if (isLessVisibleThan(GA->getReferencedGlobal()->getLinkage(),
                              linkage))
          return true;
      if (auto *FRI = dyn_cast<FunctionRefInst>(&I))
        if (isLessVisibleThan(FRI->getReferencedFunction()->getLinkage(),
                              linkage))
          return true;
    }
  return false;
}

/// \brief Attempt to inline all calls smaller than our threshold into F until.
/// returns True if a function was inlined.
bool SILPerformanceInliner::inlineCallsIntoFunction(SILFunction *Caller) {
  bool Changed = false;
  SILInliner Inliner(*Caller, SILInliner::InlineKind::PerformanceInline);

  DEBUG(llvm::dbgs() << "Visiting Function: " << Caller->getName() << "\n");

  llvm::SmallVector<ApplyInst*, 8> CallSites;

  // Collect all of the ApplyInsts in this function. We will be changing the
  // control flow and collecting the AIs simplifies the scan.
  for (auto &BB : *Caller) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      // Check if this is a call site.
      ApplyInst *AI = dyn_cast<ApplyInst>(I++);
      if (AI)
        CallSites.push_back(AI);
    }
  }

  const unsigned CallsToCalleeThreshold = 1024;
  // Calculate how many times a callee is called from this caller.
  llvm::DenseMap<SILFunction *, unsigned> CalleeCount; 
  for (auto AI : CallSites) {
    SILFunction *Callee = getInlinableFunction(AI, Mode);
    if (Callee)
      CalleeCount[Callee]++;
  }

  for (auto AI : CallSites) {
    DEBUG(llvm::dbgs() << "    Found call site:" <<  *AI);

    // Get the callee.
    SILFunction *Callee = getInlinableFunction(AI, Mode);
    if (!Callee) {
      DEBUG(llvm::dbgs() << "        FAIL! Couldn't find inlineable callee.\n");
      continue;
    }

    DEBUG(llvm::dbgs() << "        Found callee:" <<  Callee->getName()
          << ".\n");

    // To handle recursion and prevent massive code size expansion, we prevent
    // inlining the same callee many times into the caller. The recursion
    // detection logic in CallGraphAnalysis can't handle class_method in the
    // callee. To avoid inlining the recursion too many times, we stop at the
    // threshold (currently set to 1024).
    if (CalleeCount[Callee] > CallsToCalleeThreshold) {
      DEBUG(llvm::dbgs() <<
        "        FAIL! Skipping callees that are called too many times.\n");
      continue;
    }

    // Prevent circular inlining.
    if (Callee == Caller) {
      DEBUG(llvm::dbgs() << "        FAIL! Skipping recursive calls.\n");
      continue;
    }

    // If Callee has a less visible linkage than caller or references something
    // with a less visible linkage than caller, don't inline Callee into caller.
    if (transitivelyReferencesLessVisibleLinkage(*Callee,
                                                 Caller->getLinkage())) {
      DEBUG(llvm::dbgs() << "        FAIL! Skipping less visible call.");
      continue;
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

    unsigned Threshold = InlineCostThreshold * BoostFactor;
    if (CalleeCost > Threshold) {
      DEBUG(llvm::dbgs() << "        FAIL! Function too big to inline. "
            "Skipping. CalleeCost: " << CalleeCost << ". Threshold: "
            << Threshold << "\n");
      continue;
    }

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
    Inliner.inlineFunction(AI, Callee, ArrayRef<Substitution>(), Args);
    NumFunctionsInlined++;
    Changed = true;
  }

  DEBUG(llvm::dbgs() << "\n");
  return Changed;
}

namespace {
class SILPerformanceInlinerPass : public SILModuleTransform {
public:
  SILPerformanceInlinerPass() {}

  void run() {
    CallGraphAnalysis* CGA = PM->getAnalysis<CallGraphAnalysis>();

    if (getOptions().InlineThreshold == 0) {
      DEBUG(llvm::dbgs() << "*** The Performance Inliner is disabled ***\n");
      return;
    }

    // Initialize the worklist with a bottom-up call-graph order list of
    // functions.
    const std::vector<SILFunction *> &Order = CGA->bottomUpCallGraphOrder();
    std::vector<SILFunction *> Worklist(Order);
    std::reverse(Worklist.begin(), Worklist.end());

    SILPerformanceInliner inliner(getOptions().InlineThreshold,
                                  SILModule::LinkingMode::LinkAll);

    bool Changed = false;
    while (!Worklist.empty()) {
      SILFunction *F = Worklist.back();
      Worklist.pop_back();

      // If F is empty, attempt to link it. Skip it if we fail to do so.
      if (F->empty() &&
          !getModule()->linkFunction(F, SILModule::LinkingMode::LinkAll))
        continue;

      Changed |= inliner.inlineCallsIntoFunction(F);
    }

    // Invalidate the call graph.
    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
  }

  StringRef getName() override { return "Performance Inlining"; }
};
} // end anonymous namespace


SILTransform *swift::createPerfInliner() {
  return new SILPerformanceInlinerPass();
}
