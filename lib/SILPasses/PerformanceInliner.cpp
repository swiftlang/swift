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
#include "swift/SILPasses/Utils/SILInliner.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/MapVector.h"
using namespace swift;

static llvm::cl::opt<unsigned>
InlineCostThreshold("sil-inline-threshold", llvm::cl::Hidden,
                    llvm::cl::init(50));

STATISTIC(NumApply, "Total number of ApplyInst.");
STATISTIC(NumApplyInlined, "Number of ApplyInst inlined");
STATISTIC(NumApplyNotValidForInlining,
          "Number of ApplyInst that are not valid for inlining.");
STATISTIC(NumTimesHitCostLimit,
          "Number of times the cost limit was hit when inlining.");
STATISTIC(CostIncrease, "Total increased cost in all functions from inlining");

//===----------------------------------------------------------------------===//
//                            Call Graph Creation
//===----------------------------------------------------------------------===//

namespace {
  typedef std::vector<ApplyInst *> AIList;
  typedef llvm::MapVector<SILFunction *, AIList> FunctionToCallMapTy;
}

/// General way to get a SILFunction from an apply inst.
///
/// The reason to have a separate function from getInlineableCalleeFromApplyInst
/// is that we want a way to get the actual value vs checking that the value is
/// legal. This will allow us to perform tricks like looking through partial
/// applies and thin to thick function insts.
///
/// TODO: See if we can add some of the tricks from the Mandatory Inlining here
/// vis-a-vis handling partial apply and friends.
static SILFunction *getCalleeFromApplyInst(ApplyInst *AI) {
  auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee().getDef());
  if (!FRI)
    return nullptr;
  return FRI->getReferencedFunction();
}

/// A general way to get a SILFunction from an ApplyInst that additionally
/// performs checks to ensure that the SILFunction is ok for inlining.
static SILFunction *validateGetReturnCalleeFromApplyInst(ApplyInst *AI) {
  // Avoid substituion lists, we don't support them.
  if (AI->hasSubstitutions()) {
    ++NumApplyNotValidForInlining;
    return nullptr;
  }

  SILFunction *F = getCalleeFromApplyInst(AI);

  if (!F || F->empty() ||
      (F->getAbstractCC() != AbstractCC::Freestanding &&
       F->getAbstractCC() != AbstractCC::Method) ||
      F->isExternalDeclaration()) {
    ++NumApplyNotValidForInlining;
    return nullptr;
  }

  return F;
}

/// Go through F looking for ApplyInsts that we can inline. Store them in F's
/// list in the FunctionToCallMap.
static void collectApplyInst(SILFunction &F,
                             FunctionToCallMapTy &FunctionToCallMap) {
  // For now we do not inline when the Caller is a transparent function due to
  // it creating DI issues of unknown provenance when inlining the transparent
  // function into a different translation unit from the one in which we are
  // compiling.
  //
  // TODO: Investigate where the DI issue comes from.
  if (F.isTransparent())
    return;

  // For all basic blocks...
  for (auto &BB : F) {
    // For all instructions...
    for (auto &I : BB) {

      // If it is not an apply inst, skip it...
      ApplyInst *AI = dyn_cast<ApplyInst>(&I);
      if (!AI)
        continue;

      ++NumApply;

      // Get the callee from the ApplyInst if both the apply inst and the callee
      // pass the conditions needed for inlining.
      SILFunction *Callee = validateGetReturnCalleeFromApplyInst(AI);

      // If the conditions were not satisfied, continue...
      if (!Callee)
        continue;

      // Otherwise save the ApplyInst into the Caller -> ApplyInstList map.
      FunctionToCallMap[&F].push_back(AI);
    }
  }
}

/// Return the bottom up call-graph order for module M. Notice that we don't
/// include functions that don't participate in any call (caller or callee).
static void TopDownCallGraphOrder(SILModule *M,
                                  std::vector<SILFunction *> &order) {
  CallGraphSorter<SILFunction *> sorter;

  // Construct the call graph, mapping callee to callers to that the resulting
  // topological ordering has callees before callers.
  //
  // *NOTE* From the typical callgraph perspective, we are inserting edges in
  // reverse.
  for (auto &Caller : *M)
    for (auto &BB : Caller)
      for (auto &I : BB)
        if (FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(&I)) {
          SILFunction *Callee = FRI->getReferencedFunction();
          sorter.addEdge(Callee, &Caller);
        }

  // Perform the topological sorting.
  sorter.sort(order);
}

//===----------------------------------------------------------------------===//
//                                 Cost Model
//===----------------------------------------------------------------------===//

/// For now just assume that every SIL instruction is one to one with an LLVM
/// instruction. This is of course very much so not true.
///
/// TODO: Fill this out.
static unsigned instructionInlineCost(SILInstruction &I) {
  switch (I.getKind()) {
    case ValueKind::FunctionRefInst:
    case ValueKind::BuiltinFunctionRefInst:
    case ValueKind::GlobalAddrInst:
    case ValueKind::SILGlobalAddrInst:
    case ValueKind::IntegerLiteralInst:
    case ValueKind::FloatLiteralInst:
    case ValueKind::StringLiteralInst:
      return 0;
    default:
      return 1;
  }
}

/// Just sum over all of the instructions.
///
/// TODO: Memoize.
static unsigned functionInlineCost(SILFunction *F) {
  unsigned i = 0;
  for (auto &BB : *F) {
    for (auto &I : BB) {
      i += instructionInlineCost(I);

      // If i is greater than the InlineCostThreshold, we already know we are
      // not going to inline this given function, so there is no point in
      // continuing to visit instructions.
      if (i > InlineCostThreshold)
        return i;
    }
  }
  return i;
}

//===----------------------------------------------------------------------===//
//                                  Inliner
//===----------------------------------------------------------------------===//

/// Attempt to inline all calls in ApplyInstList into F until we run out of cost
/// budge.
///
/// TODO: This should be refactored to use a worklist approach so we process any
/// additional functions that we may uncover via inlining. If we have a bunch of
/// extra cost in our budget, we may be able to inline those.
static void inlineCallsIntoFunction(SILFunction *F, AIList &ApplyInstList) {
  SILInliner Inliner(*F);

  // Get the initial cost of the function in question.
  unsigned CurrentCost = functionInlineCost(F);
  DEBUG(llvm::dbgs() << "Visiting Function: " << F->getName() << ". Initial "
        "Cost: " << CurrentCost << "\n");

  bool HitCostLimit = false;

  // For each apply in our list...
  for (ApplyInst *AI : ApplyInstList) {

    DEBUG(llvm::dbgs() << "  Visiting Apply Inst " << *AI);

    // Get its callee.
    SILFunction *Callee = getCalleeFromApplyInst(AI);
    assert(Callee && "We already know at this point that callees are valid for "
           "inlining.");

    // If the Callee is F, skip it to prevent circular inlining...
    if (Callee == F) {
      DEBUG(llvm::dbgs() << "    Recursive Call... Skipping...\n");
      continue;
    }

    // Calculate the cost for the new callee.
    unsigned CalleeCost = functionInlineCost(Callee);

    // If our CalleeCost is over the InlineCostThreshold, skip it.
    if (CalleeCost > InlineCostThreshold)
      continue;

    // Compute the new cost for the caller given we inlined the callee into the
    // caller...
    unsigned NewCost = CalleeCost + CurrentCost;

    DEBUG(llvm::dbgs() << "    Old Cost = " << CurrentCost << "; CalleeCost = "
                 << CalleeCost << "; New Cost: " << NewCost << "\n");

    // If we would go over our threshold, continue. There may be other functions
    // of less weight further on.
    if (NewCost > InlineCostThreshold) {
      HitCostLimit = true;
      continue;
    }

    // Add the arguments from AI into a SILValue list.
    SmallVector<SILValue, 8> Args;
    for (const auto &Arg : AI->getArguments())
      Args.push_back(Arg);

    // Ok, we are within budget. Attempt to inline.
    DEBUG(llvm::dbgs() << "    Everything Looks Good! Inlining...");
    Inliner.inlineFunction(SILInliner::InlineKind::PerformanceInline, AI,
                           Callee, ArrayRef<Substitution>(), Args);

    // Perform book keeping.
    CurrentCost = NewCost;
    NumApplyInlined++;
    CostIncrease += CalleeCost;

    // If the new current cost is equal to our threshold, exit.
    if (CurrentCost == InlineCostThreshold) {
      HitCostLimit = true;
      break;
    }
  }

  NumTimesHitCostLimit += unsigned(HitCostLimit);
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILPerformanceInlining(SILModule *M) {

  DEBUG(llvm::dbgs() << "*** SIL Performance Inlining ***\n\n");

  FunctionToCallMapTy FunctionToCallMap;

  // Gather apply instructions.
  for (auto &Fn : *M)
    collectApplyInst(Fn, FunctionToCallMap);

  // Collect a call-graph bottom-up list of functions.
  std::vector<SILFunction *> Worklist;
  TopDownCallGraphOrder(M, Worklist);

  // For each function in the worklist, attempt to inline its list of apply
  // inst.
  while (Worklist.size()) {
    SILFunction *F = Worklist.back();
    Worklist.pop_back();

    // If we have any applies in F, it will have an entry in
    // FunctionsToCallMap. If it does, attempt to inline those applies.
    if (FunctionToCallMap.count(F))
      inlineCallsIntoFunction(F, FunctionToCallMap[F]);
  }
}
