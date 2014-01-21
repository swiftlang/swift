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

STATISTIC(NumFunctionsInlined, "Number of functions inlined");

//===----------------------------------------------------------------------===//
//                            Call Graph Creation
//===----------------------------------------------------------------------===//

/// \brief Returns a SILFunction if this ApplyInst calls a recognizable function
/// that is legal to inline.
static SILFunction *getInlinableFunction(ApplyInst *AI) {
  // Avoid substituion lists, we don't support them.
  if (AI->hasSubstitutions())
    return nullptr;

  auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee().getDef());
  if (!FRI)
    return nullptr;

  SILFunction *F = FRI->getReferencedFunction();

  if (F->empty() || F->isExternalDeclaration()) {
    DEBUG(llvm::dbgs() << "  Can't inline " << F->getName() << ".\n");
    return nullptr;
  }

  DEBUG(llvm::dbgs() << "  Can inline " << F->getName() << ".\n");
  return F;
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
    case ValueKind::DebugValueInst:
    case ValueKind::DebugValueAddrInst:
      return 0;
    case ValueKind::TupleElementAddrInst:
    case ValueKind::StructElementAddrInst: {
      // A gep whose operand is a gep with no other users will get folded by
      // LLVM into one gep implying the second should be free.
      SILValue Op = I.getOperand(0);
      if ((Op->getKind() == ValueKind::TupleElementAddrInst ||
           Op->getKind() == ValueKind::StructElementAddrInst) &&
          Op->hasOneUse())
        return 0;
    }
    default:
      return 1;
  }
}

/// \brief Returns the inlining cost of the function.
static unsigned getFunctionCost(SILFunction *F) {
  DEBUG(llvm::dbgs() << "  Calculating cost for " << F->getName() << ".\n");

  if (F->isTransparent() == IsTransparent_t::IsTransparent)
    return 0;

  unsigned Cost = 0;
  for (auto &BB : *F) {
    for (auto &I : BB) {
      Cost += instructionInlineCost(I);

      // If i is greater than the InlineCostThreshold, we already know we are
      // not going to inline this given function, so there is no point in
      // continuing to visit instructions.
      if (Cost > InlineCostThreshold) {
        DEBUG(llvm::dbgs() << "  Cost too high.\n");
        return Cost;
      }
    }
  }

  DEBUG(llvm::dbgs() << "  Found cost: " << Cost << "\n");
  return Cost;
}

//===----------------------------------------------------------------------===//
//                                  Inliner
//===----------------------------------------------------------------------===//

/// Attempt to inline all calls in ApplyInstList into F until we run out of cost
/// budge.
static void inlineCallsIntoFunction(SILFunction *Caller) {
  SILInliner Inliner(*Caller, SILInliner::InlineKind::PerformanceInline);

  // Initialize the budget.
  unsigned InlineBudget = InlineCostThreshold;
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

  for (auto AI : CallSites) {
    DEBUG(llvm::dbgs() << "  Found call site:" <<  *AI);

    // Get the callee.
    SILFunction *Callee = getInlinableFunction(AI);
    if (!Callee)
      continue;

    DEBUG(llvm::dbgs() << "  Found callee:" <<  Callee->getName() << ".\n");

    // Prevent circular inlining.
    if (Callee == Caller) {
      DEBUG(llvm::dbgs() << "  Skipping recursive calls.\n");
      continue;
    }

    // Calculate the inlining cost of the callee.
    unsigned CalleeCost = getFunctionCost(Callee);

    if (CalleeCost > InlineBudget) {
      DEBUG(llvm::dbgs() << "  Budget exceeded.\n");
      continue;
    }

    // Add the arguments from AI into a SILValue list.
    SmallVector<SILValue, 8> Args;
    for (const auto &Arg : AI->getArguments())
    Args.push_back(Arg);

    // Ok, we are within budget. Attempt to inline.
    DEBUG(llvm::dbgs() << "  Inlining " << Callee->getName() << " Into " <<
          Caller->getName() << "\n");

    // We already moved the iterator to the next instruction because the AI
    // will be erased by the inliner. Notice that we will skip all of the
    // newly inlined ApplyInsts. That's okay because we will visit them in
    // our next invocation of the inliner.
    Inliner.inlineFunction(AI, Callee, ArrayRef<Substitution>(), Args);
    NumFunctionsInlined++;
    InlineBudget -= CalleeCost;
    DEBUG(llvm::dbgs() << "  Remaining budget " << InlineBudget << "\n");
  }
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILPerformanceInlining(SILModule *M) {
  DEBUG(llvm::dbgs() << "*** SIL Performance Inlining ***\n\n");

  if (InlineCostThreshold == 0) {
    DEBUG(llvm::dbgs() << "*** The SIL performance Inliner is disabled ***\n");
    return;
  }

  // Collect a call-graph bottom-up list of functions.
  std::vector<SILFunction *> Worklist;
  TopDownCallGraphOrder(M, Worklist);

  // For each function in the worklist, attempt to inline its list of apply
  // inst.
  while (Worklist.size()) {
    SILFunction *F = Worklist.back();
    Worklist.pop_back();

    // Do not inline into transparent functions. This is exposing a diagnostics
    // bug. We will still inline after we perform mandatory inlining of the
    // transparent function.
    inlineCallsIntoFunction(F);
  }
}
