//===----- CallGraphAnalysis.cpp - Call graph construction ----*- C++ -*---===//
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

#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

#define DEBUG_TYPE "call-graph"

STATISTIC(NumCallGraphNodes, "# of call graph nodes created");
STATISTIC(NumCallSitesWithEdges, "# of call sites with edges");
STATISTIC(NumCallSitesWithoutEdges,
          "# of non-builtin call sites without call graph edges");
STATISTIC(NumCallSitesOfBuiltins, "# of call sites calling builtins");

CallGraph::CallGraph(SILModule *M, bool completeModule) {
  // Build the initial call graph by creating a node for each
  // function, and an edge for each direct call to a free function.
  // TODO: Handle other kinds of applies.

  unsigned NodeOrdinal = 0;
  for (auto &F : *M)
    if (F.isDefinition())
      addCallGraphNode(&F, NodeOrdinal++);

  for (auto &F : *M)
    if (F.isDefinition())
      addEdges(&F);
}

void CallGraph::addCallGraphNode(SILFunction *F, unsigned NodeOrdinal) {
  // TODO: Compute this from the call graph itself after stripping
  //       unreachable nodes from graph.
  ++NumCallGraphNodes;
  auto *Node = new CallGraphNode(F, NodeOrdinal);

  assert(!FunctionToNodeMap.count(F) &&
         "Added function already has a call graph node!");

  FunctionToNodeMap[F] = Node;

  // TODO: Only add functions clearly visible from outside our
  //       compilation scope as roots.
  CallGraphRoots.insert(Node);
}

void CallGraph::addEdgesForApply(ApplyInst *AI, CallGraphNode *CallerNode) {
  auto Callee = AI->getCallee();

  switch (Callee->getKind()) {
  case ValueKind::FunctionRefInst: {
    auto *CalleeFn = cast<FunctionRefInst>(Callee)->getReferencedFunction();
    // TODO: Compute this from the call graph itself after stripping
    //       unreachable nodes from graph.
    ++NumCallSitesWithEdges;
    auto *CallSite = new CallGraphEdge(AI, CalleeFn);
    CallerNode->addCallSite(CallSite);

    auto *CalleeNode = getCallGraphNode(CalleeFn);
    if (CalleeNode)
      CalleeNode->addCaller(CallSite);
    break;
  }

  case ValueKind::DynamicMethodInst: {
    // TODO: Decide how to handle these in graph construction and
    //       analysis passes. We might just leave them out of the
    //       graph.
    ++NumCallSitesWithoutEdges;
    break;
  }

  case ValueKind::ThinToThickFunctionInst: {
    auto ThinCallee = cast<ThinToThickFunctionInst>(Callee)->getOperand();
    // TODO: We want to see through these to the underlying function.
    assert(cast<FunctionRefInst>(ThinCallee) && "Expected function reference!");
    ++NumCallSitesWithoutEdges;
    break;
  }

  case ValueKind::SILArgument: {
    // First-pass call-graph construction will not do anything with
    // these, but a second pass can potentially statically determine
    // the called function in some cases.
    ++NumCallSitesWithoutEdges;
    break;
  }

  case ValueKind::ApplyInst: {
    // TODO: Probably not worth iterating invocation- then
    //       reverse-invocation order to catch this.
    ++NumCallSitesWithoutEdges;
    break;
  }

  case ValueKind::TupleExtractInst: {
    // TODO: It would be good to tunnel through extracts so that we
    //       can build a more accurate call graph prior to any
    //       optimizations.
    ++NumCallSitesWithoutEdges;
    break;
  }

  case ValueKind::StructExtractInst: {
    // TODO: It would be good to tunnel through extracts so that we
    //       can build a more accurate call graph prior to any
    //       optimizations.
    ++NumCallSitesWithoutEdges;
    break;
  }

  case ValueKind::BuiltinFunctionRefInst: {
    ++NumCallSitesOfBuiltins;
    break;
  }

  case ValueKind::PartialApplyInst:
  case ValueKind::ClassMethodInst:
  case ValueKind::ProtocolMethodInst:
  case ValueKind::WitnessMethodInst:
  case ValueKind::SuperMethodInst:
    // TODO: Each of these requires specific handling.
    ++NumCallSitesWithoutEdges;
    break;

  default:
    assert(!isa<MethodInst>(Callee)
           && "Unhandled method instruction in call graph construction!");

    // There are cases where we will be very hard pressed to determine
    // what we are calling.
    ++NumCallSitesWithoutEdges;
    break;
  }
}

void CallGraph::addEdges(SILFunction *F) {
  auto *CallerNode = getCallGraphNode(F);
  assert(CallerNode && "Expected call graph node for function!");

  for (auto &BB : *F)
    for (auto &I : BB)
      if (auto *AI = dyn_cast<ApplyInst>(&I))
        addEdgesForApply(AI, CallerNode);
}
