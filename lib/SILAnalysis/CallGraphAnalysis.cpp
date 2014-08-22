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

using namespace swift;

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
  auto *Node = new CallGraphNode(F, NodeOrdinal);

  assert(!FunctionToNodeMap.count(F) &&
         "Added function already has a call graph node!");

  FunctionToNodeMap[F] = Node;

  // TODO: Only add functions clearly visible from outside our
  //       compilation scope as roots.
  CallGraphRoots.insert(Node);
}

void CallGraph::addEdges(SILFunction *F) {
  auto *CallerNode = getCallGraphNode(F);
  assert(CallerNode && "Expected call graph node for function!");

  for (auto &BB : *F)
    for (auto &I : BB)
      if (auto *AI = dyn_cast<ApplyInst>(&I))
        if (auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee())) {
          auto *Callee = FRI->getReferencedFunction();
          auto *CallSite = new CallGraphEdge(AI, Callee);
          CallerNode->addCallSite(CallSite);

          auto *CalleeNode = getCallGraphNode(Callee);
          if (CalleeNode)
            CalleeNode->addCaller(CallSite);
        }
}
