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
#include "swift/Basic/Fallthrough.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include <algorithm>
#include <utility>

using namespace swift;

#define DEBUG_TYPE "call-graph"

STATISTIC(NumCallGraphNodes, "# of call graph nodes created");
STATISTIC(NumAppliesWithEdges, "# of call sites with edges");
STATISTIC(NumAppliesWithoutEdges,
          "# of call sites without edges");
STATISTIC(NumAppliesOfBuiltins, "# of call sites calling builtins");

CallGraph::CallGraph(SILModule *Mod, bool completeModule) : M(*Mod) {
  // Build the initial call graph by creating a node for each
  // function, and an edge for each direct call to a free function.
  // TODO: Handle other kinds of applies.

  unsigned NodeOrdinal = 0;
  for (auto &F : M)
    addCallGraphNode(&F, NodeOrdinal++);

  for (auto &F : M)
    if (F.isDefinition())
      addEdges(&F);
}

CallGraph::~CallGraph() {
  // Clean up all call graph nodes.
  for (auto &P : FunctionToNodeMap) {
    P.second->~CallGraphNode();
  }

  // Clean up all call graph edges.
  for (auto &P : ApplyToEdgeMap) {
    P.second->~CallGraphEdge();
  }

  // Clean up all SCCs.
  for (CallGraphSCC *SCC : BottomUpSCCOrder) {
    SCC->~CallGraphSCC();
  }
}

void CallGraph::addCallGraphNode(SILFunction *F, unsigned NodeOrdinal) {
  // TODO: Compute this from the call graph itself after stripping
  //       unreachable nodes from graph.
  ++NumCallGraphNodes;
  auto *Node = new (Allocator) CallGraphNode(F, NodeOrdinal);

  assert(!FunctionToNodeMap.count(F) &&
         "Added function already has a call graph node!");

  FunctionToNodeMap[F] = Node;

  // TODO: Only add functions clearly visible from outside our
  //       compilation scope as roots.
  if (F->isDefinition())
    CallGraphRoots.push_back(Node);
}

bool CallGraph::tryGetCalleeSet(SILValue Callee,
                                CallGraphEdge::CalleeSetType &CalleeSet,
                                bool &Complete) {

  assert(CalleeSet.empty() && "Expected empty callee set!");

  switch (Callee->getKind()) {
  case ValueKind::ThinToThickFunctionInst:
    Callee = cast<ThinToThickFunctionInst>(Callee)->getOperand();
    SWIFT_FALLTHROUGH;
  case ValueKind::FunctionRefInst: {
    auto *CalleeFn = cast<FunctionRefInst>(Callee)->getReferencedFunction();
    auto *CalleeNode = getCallGraphNode(CalleeFn);
    assert(CalleeNode &&
           "Expected to have a call graph node for all functions!");

    CalleeSet.insert(CalleeNode);
    Complete = true;
    return true;
  }

  case ValueKind::DynamicMethodInst:
    // TODO: Decide how to handle these in graph construction and
    //       analysis passes. We might just leave them out of the
    //       graph.
    return false;

  case ValueKind::SILArgument:
    // First-pass call-graph construction will not do anything with
    // these, but a second pass can potentially statically determine
    // the called function in some cases.
    return false;

  case ValueKind::ApplyInst:
    // TODO: Probably not worth iterating invocation- then
    //       reverse-invocation order to catch this.
    return false;

  case ValueKind::TupleExtractInst:
    // TODO: It would be good to tunnel through extracts so that we
    //       can build a more accurate call graph prior to any
    //       optimizations.
    return false;

  case ValueKind::StructExtractInst:
    // TODO: It would be good to tunnel through extracts so that we
    //       can build a more accurate call graph prior to any
    //       optimizations.
    return false;

  case ValueKind::BuiltinInst:
    ++NumAppliesOfBuiltins;
    return false;

  case ValueKind::WitnessMethodInst: {
    auto *WMI = cast<WitnessMethodInst>(Callee);
    SILFunction *CalleeFn;
    ArrayRef<Substitution> Subs;
    SILWitnessTable *WT;

    std::tie(CalleeFn, WT, Subs) =
      WMI->getModule().lookUpFunctionInWitnessTable(WMI->getConformance(),
                                                    WMI->getMember());

    if (!CalleeFn)
      return false;

    auto *CalleeNode = getCallGraphNode(CalleeFn);
    assert(CalleeNode &&
           "Expected to have a call graph node for all functions!");

    CalleeSet.insert(CalleeNode);
    Complete = true;
    return true;
  }

  case ValueKind::PartialApplyInst:
  case ValueKind::ClassMethodInst:
  case ValueKind::SuperMethodInst:
    // TODO: Each of these requires specific handling.
    return false;

  default:
    assert(!isa<MethodInst>(Callee)
           && "Unhandled method instruction in call graph construction!");

    // There are cases where we will be very hard pressed to determine
    // what we are calling.
    return false;
  }
}

static void orderCallees(const CallGraphEdge::CalleeSetType &Callees,
                         llvm::SmallVectorImpl<CallGraphNode *> &OrderedNodes) {
  for (auto *Node : Callees)
    OrderedNodes.push_back(Node);

  std::sort(OrderedNodes.begin(), OrderedNodes.end(),
            [](CallGraphNode *left, CallGraphNode *right) {
              return left->getOrdinal() < right->getOrdinal();
            });
}

void CallGraph::addEdgesForApply(ApplyInst *AI, CallGraphNode *CallerNode) {
  CallGraphEdge::CalleeSetType CalleeSet;
  bool Complete = false;

  if (tryGetCalleeSet(AI->getCallee(), CalleeSet, Complete)) {
    auto *Edge = new (Allocator) CallGraphEdge(AI, CalleeSet, Complete);
    assert(!ApplyToEdgeMap.count(AI) &&
           "Added apply that already has an edge node!\n");
    ApplyToEdgeMap[AI] = Edge;
    CallerNode->addCalleeEdge(Edge);

    llvm::SmallVector<CallGraphNode *, 4> OrderedNodes;
    orderCallees(CalleeSet, OrderedNodes);

    for (auto *CalleeNode : OrderedNodes)
      CalleeNode->addCallerEdge(Edge);

    // TODO: Compute this from the call graph itself after stripping
    //       unreachable nodes from graph.
    ++NumAppliesWithEdges;
    return;
  }

  ++NumAppliesWithoutEdges;
}

void CallGraph::addEdges(SILFunction *F) {
  auto *CallerNode = getCallGraphNode(F);
  assert(CallerNode && "Expected call graph node for function!");

  for (auto &BB : *F) {
    for (auto &I : BB) {
      if (auto *AI = dyn_cast<ApplyInst>(&I)) {
        addEdgesForApply(AI, CallerNode);
      }

      if (auto *FRI = dyn_cast<FunctionRefInst>(&I)) {
        auto *CalleeFn = FRI->getReferencedFunction();
        if (!CalleeFn->isPossiblyUsedExternally()) {
          bool hasAllApplyUsers = std::none_of(FRI->use_begin(), FRI->use_end(),
            [](const Operand *Op) {
              return !isa<ApplyInst>(Op->getUser());
            });

          // If we have a non-apply user of this function, mark its caller set
          // as being incomplete.
          if (!hasAllApplyUsers) {
            auto *CalleeNode = getCallGraphNode(CalleeFn);
            CalleeNode->markCallerEdgesIncomplete();
          }
        }
      }
    }
  }
}

/// Finds SCCs in the call graph. Our call graph has an unconventional
/// form where each edge of the graph is really a multi-edge that can
/// point to multiple call graph nodes in the case where we can call
/// one of several different functions.
class CallGraphSCCFinder {
  unsigned NextDFSNum;
  llvm::SmallVectorImpl<CallGraphSCC *> &TheSCCs;

  llvm::DenseMap<CallGraphNode *, unsigned> DFSNum;
  llvm::DenseMap<CallGraphNode *, unsigned> MinDFSNum;
  llvm::SetVector<CallGraphNode *> DFSStack;

  /// The CallGraphSCCFinder does not own this bump ptr allocator, so does not
  /// call the destructor of objects allocated from it.
  llvm::BumpPtrAllocator &BPA;

public:
  CallGraphSCCFinder(llvm::SmallVectorImpl<CallGraphSCC *> &TheSCCs,
                     llvm::BumpPtrAllocator &BPA)
    : NextDFSNum(0), TheSCCs(TheSCCs), BPA(BPA) {}

  void DFS(CallGraphNode *Node) {
    // Set the DFSNum for this node if we haven't already, and if we
    // have, which indicates it's already been visited, return.
    if (!DFSNum.insert(std::make_pair(Node, NextDFSNum)).second)
      return;

    assert(MinDFSNum.find(Node) == MinDFSNum.end() &&
           "Node should not already have a minimum DFS number!");

    MinDFSNum[Node] = NextDFSNum;
    ++NextDFSNum;

    DFSStack.insert(Node);

    for (auto *ApplyEdge : Node->getCalleeEdges()) {
      llvm::SmallVector<CallGraphNode *, 4> OrderedNodes;
      orderCallees(ApplyEdge->getPartialCalleeSet(), OrderedNodes);

      for (auto *CalleeNode : OrderedNodes) {
        if (DFSNum.find(CalleeNode) == DFSNum.end()) {
          DFS(CalleeNode);
          MinDFSNum[Node] = std::min(MinDFSNum[Node], MinDFSNum[CalleeNode]);
        } else if (DFSStack.count(CalleeNode)) {
          MinDFSNum[Node] = std::min(MinDFSNum[Node], DFSNum[CalleeNode]);
        }
      }
    }

    // If this node is the root of an SCC (including SCCs with a
    // single node), pop the SCC and push it on our SCC stack.
    if (DFSNum[Node] == MinDFSNum[Node]) {
      auto *SCC = new (BPA) CallGraphSCC();

      CallGraphNode *Popped;
      do {
        Popped = DFSStack.pop_back_val();
        SCC->SCCNodes.push_back(Popped);
      } while (Popped != Node);

      TheSCCs.push_back(SCC);
    }
  }
};

void CallGraph::computeBottomUpSCCOrder() {
  if (!BottomUpSCCOrder.empty()) {
    for (auto *SCC : BottomUpSCCOrder)
      SCC->~CallGraphSCC();
    BottomUpSCCOrder.clear();
  }

  CallGraphSCCFinder SCCFinder(BottomUpSCCOrder, Allocator);
  for (auto *Node : getCallGraphRoots())
    SCCFinder.DFS(Node);
}

void CallGraph::computeBottomUpFunctionOrder() {
  // We do not need to call any destructors here.
  BottomUpFunctionOrder.clear();

  computeBottomUpSCCOrder();

  for (auto *SCC : BottomUpSCCOrder)
    for (auto *Node : SCC->SCCNodes)
      BottomUpFunctionOrder.push_back(Node->getFunction());
}

//===----------------------------------------------------------------------===//
//                           CallGraph Verification
//===----------------------------------------------------------------------===//

void CallGraph::verify() const {
#ifndef NDEBUG
  // For every function in the module, add it to our SILFunction set.
  llvm::DenseSet<SILFunction *> Functions;
  for (auto &F : M)
    Functions.insert(&F);

  // For every (SILFunction, CallGraphNode) pair FuncPair in the SILFunction to
  // CallGraphNode map check that:
  //
  //    a. FuncPair.first is a SILFunction in the current module.
  //    b. FuncPair.first is the SILFunction inside the CallGraphNode
  //    FuncPair.second.
  //    c. All callee CallGraphEdges mapped to FuncPair.second have ApplyInsts
  //       which are in the SILFunction FuncPair.first.
  //
  for (auto &P : FunctionToNodeMap) {
    assert(Functions.count(P.first) && "Func in FunctionToNodeMap but not "
                                       "in module!?");
    assert(P.second->getFunction() == P.first &&
           "Func mapped to node, but node has different Function inside?!");
    for (CallGraphEdge *Edge : P.second->getCalleeEdges()) {
      assert(Edge->getApply()->getFunction() == P.first &&
             "ApplyInst in callee set that is not in the Callee function?!");
    }
  }

  // For every pair (ApplyInst, CallGraphEdge) ApplyPair in the Apply to
  // CallGraphEdge map, check that:
  //
  //    a. ApplyPair.second.getApply() == ApplyPair.first.
  //    b. ApplyPair.first->getFunction() is in the SILFunction to
  //       CallGraphNode map and the CallGraphEdge for ApplyPair is one of
  //       CallSiteEdges in the mapped to CallGraphNode.
  //
  for (auto &P : ApplyToEdgeMap) {
    assert(P.second->getApply() == P.first &&
           "Apply mapped to CallSiteEdge but not vis-a-versa?!");
    assert(Functions.count(P.first->getFunction()) &&
           "Apply in func not in module?!");
    CallGraphNode *Node = getCallGraphNode(P.first->getFunction());
    assert(Node && "Apply without call graph node");

    bool FoundEdge = false;
    for (CallGraphEdge *Edge : Node->getCalleeEdges()) {
      if (Edge == P.second) {
        FoundEdge = true;
        break;
      }
    }
    assert(FoundEdge && "Failed to find Apply CallGraphEdge in Apply inst "
                        "parent function's caller");
  }
#endif
}

void CallGraphAnalysis::verify() const {
#ifndef NDEBUG
  // If we don't have a callgraph, return.
  if (!CG)
    return;
  CG->verify();
#endif
}
