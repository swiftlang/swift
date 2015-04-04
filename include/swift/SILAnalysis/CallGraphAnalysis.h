//===--- CallGraphAnalysis.h - Analysis of the Call Graph ------*- C++ -*--===//
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

#ifndef SWIFT_SILANALYSIS_CALLGRAPHANALYSIS_H
#define SWIFT_SILANALYSIS_CALLGRAPHANALYSIS_H

#include "swift/SILAnalysis/Analysis.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Allocator.h"
#include <vector>

namespace swift {

inline bool canHaveIndirectUses(SILFunction *F) {
  if (swift::isPossiblyUsedExternally(F->getLinkage(),
                                      F->getModule().isWholeModule()))
    return true;

  // TODO: main is currently marked as internal so we explicitly check
  // for functions with this name and keep them around.
  if (F->getName() == SWIFT_ENTRY_POINT_FUNCTION)
    return true;

  // ObjC functions are called through the runtime and are therefore alive
  // even if not referenced inside SIL.
  if (canBeCalledIndirectly(F->getLoweredFunctionType()->getAbstractCC()))
    return true;

  return false;
}

class CallGraphNode;

class CallGraphEdge {
public:
  // SmallSize = 1, because direct calls only need a single entry in the set and
  // currently we don't handle method calls.
  // TODO: Consider increasing SmallSize when we handle method calls.
  typedef llvm::SmallPtrSet<CallGraphNode *, 1> CalleeSetType;

private:
  // The call site represented by this call graph edge.
  FullApplySite TheApply;

  // The set of functions potentially called from this call site. This
  // might include functions that are not actually callable based on
  // dynamic types. If the int bit is non-zero, the set is complete in
  // the sense that no function outside the set could be called.
  //
  // This is currently owned by the CallGraphEdge.
  llvm::PointerIntPair<CalleeSetType *, 1> CalleeSet;

  // A unique identifier for this edge based on the order in which it
  // was created relative to other edges.
  unsigned Ordinal;

public:
  /// Create a call graph edge for a call site where we will fill in
  /// the set of potentially called functions later.
  CallGraphEdge(FullApplySite TheApply, CalleeSetType &KnownCallees,
                bool Complete, unsigned Ordinal)
    : TheApply(TheApply),
      // FIXME: Do not allocate memory for the singleton callee case.
      CalleeSet(new CalleeSetType, Complete),
      Ordinal(Ordinal) {

    // TODO: We will probably have many call sites that can share a
    //       callee set so we should optimize allocation and
    //       deallocation of these accordingly.
    CalleeSet.getPointer()->insert(KnownCallees.begin(), KnownCallees.end());
  }

  ~CallGraphEdge() {
    delete CalleeSet.getPointer();
  }

  const FullApplySite getApply() const { return TheApply; }

  FullApplySite getApply() { return TheApply; }

  /// Return a callee set that is known to be complete.
  const CalleeSetType &getCompleteCalleeSet() const {
    assert(isCalleeSetComplete() && "Attempt to get an incomplete call set!");
    return *CalleeSet.getPointer();
  }

  /// Return a callee set that is not known to be complete.
  const CalleeSetType &getPartialCalleeSet() {
    return *CalleeSet.getPointer();
  }

  /// Add the given function to the set of functions that we could
  /// call from this call site.
  void addCallee(CallGraphNode *Node) {
    assert(!isCalleeSetComplete() &&
           "Attempting to add another callee to a complete call set!");
    CalleeSet.getPointer()->insert(Node);
  }

  /// Return whether the call set is known to be complete.
  bool isCalleeSetComplete() const {
    return CalleeSet.getInt();
  }

  /// The apply has a complete callee set, and it's of size one. In
  /// other words we can replace its callee with a function_ref
  /// regardless of what kind of instruction the callee is now.
  bool hasSingleCallee() const {
    return isCalleeSetComplete() && CalleeSet.getPointer()->size() == 1;
  }

  unsigned getOrdinal() const {
    return Ordinal;
  }

  void dump();
};

class CallGraphNode {
  /// The function represented by this call graph node.
  SILFunction *Function;

  /// The call graph node ordinal within the SILModule.
  const unsigned Ordinal;

  /// Edges representing the known call sites that could call into
  /// this function.
  ///
  /// This is owned by the callgraph itself, not the callgraph node.
  llvm::SmallPtrSet<CallGraphEdge *, 4> CallerEdges;

  /// Edges representing the call sites within this function.
  ///
  /// This is owned by the callgraph itself, not the callgraph node.
  llvm::SmallPtrSet<CallGraphEdge *, 4> CalleeEdges;

  /// Do we know all the potential callers of this function? We initialize this
  /// to !canHaveIndirectUses(F) optimistically and if we find any use that we
  /// can not prove does not cause a reference to a function_ref to escape in a
  /// we set this to false.
  bool CallerEdgesComplete;

public:
  friend class CallGraph;

  CallGraphNode(SILFunction *Function, unsigned Ordinal)
    : Function(Function), Ordinal(Ordinal),
      CallerEdgesComplete(!canHaveIndirectUses(Function)) {
    assert(Function &&
           "Cannot build a call graph node with a null function pointer!");
  }

  ~CallGraphNode() = default;

  SILFunction *getFunction() {
    return Function;
  }

  /// Get the complete set of edges associated with call sites that can call
  /// into this function.
  const llvm::SmallPtrSetImpl<CallGraphEdge *> &getCompleteCallerEdges() const {
    assert(isCallerEdgesComplete() &&
           "Attempt to get an incomplete caller set!");
    return CallerEdges;
  }

  /// Get the known set of call graph edges that represent possible
  /// calls into this function.
  const llvm::SmallPtrSetImpl<CallGraphEdge *> &getPartialCallerEdges() const {
    return CallerEdges;
  }

  /// Get the set of call sites in this function.
  const llvm::SmallPtrSetImpl<CallGraphEdge *> &getCalleeEdges() const {
    return CalleeEdges;
  }

  /// Do we know that the set of call sites is complete - i.e. that
  /// there is no other place that we can call from that can reach
  /// this function?
  bool isCallerEdgesComplete() const {
    return CallerEdgesComplete;
  }

  unsigned getOrdinal() const {
    return Ordinal;
  }

  void dump();

private:
  /// Mark a set of callers as known to not be complete.
  void markCallerEdgesIncomplete() {
    CallerEdgesComplete = false;
  }

  /// Add an edge representing a call site within this function.
  void addCalleeEdge(CallGraphEdge *CallSite) {
    CalleeEdges.insert(CallSite);
  }

  /// Remove an edge representing a call site within this function.
  void removeCalleeEdge(CallGraphEdge *CallSite) {
    assert(CalleeEdges.count(CallSite) && "Expected edge to be in set!");
    CalleeEdges.erase(CallSite);
  }

  /// Add an edge representing a call site that calls into this function.
  void addCallerEdge(CallGraphEdge *CallerCallSite) {
    CallerEdges.insert(CallerCallSite);
  }

  /// Remove an edge representing a call site that calls into this function.
  void removeCallerEdge(CallGraphEdge *CallerCallSite) {
    assert(CallerEdges.count(CallerCallSite) && "Expected edge to be in set!");
    CallerEdges.erase(CallerCallSite);
  }
};

struct CallGraphSCC {
  /// The CallGraphSCC does not own these CallGraphNodes. They are owned by the
  /// CallGraph itself where they are allocated via a bump ptr allocator.
  llvm::SmallVector<CallGraphNode *, 1> SCCNodes;
};

class CallGraph {
  /// The module that this call graph belongs to.
  SILModule &M;

  /// The root nodes of the call graph. This consists of functions that are
  /// definitions in our module currently. It can be expanded to only include
  /// functions clearly visible from outside our compilation scope (i.e. ignore
  /// private functions that don't escape).
  ///
  /// These are allocated via Allocator so are owned by the CallGraph. Thus the
  /// callgraph calls the CallGraphNode's destructors in its destructor.
  llvm::SmallVector<CallGraphNode *, 16> CallGraphRoots;

  /// A map from a function to the function's node in the call graph.
  llvm::DenseMap<SILFunction *, CallGraphNode *> FunctionToNodeMap;

  /// A map from an apply inst to its call edge in the call graph.
  llvm::DenseMap<FullApplySite , CallGraphEdge *> ApplyToEdgeMap;

  /// A vector of SCCs in bottom up SCC order.
  llvm::SmallVector<CallGraphSCC *, 16> BottomUpSCCOrder;

  /// A vector of functions in bottom up function order.
  std::vector<SILFunction *> BottomUpFunctionOrder;

  /// An allocator used by the callgraph.
  llvm::BumpPtrAllocator Allocator;

  /// Ordinal incremented for each edge we add.
  unsigned EdgeOrdinal;

public:
  CallGraph(SILModule *M, bool completeModule);
  ~CallGraph();

  // Query funtions for getting roots, nodes, and edges from the call
  // graph.

  ArrayRef<CallGraphNode *> getCallGraphRoots() {
    return CallGraphRoots;
  }

  CallGraphNode *getCallGraphNode(SILFunction *F) const {
    return const_cast<CallGraph *>(this)->getCallGraphNode(F);
  }

  CallGraphNode *getCallGraphNode(SILFunction *F) {
    auto Found = FunctionToNodeMap.find(F);
    if (Found == FunctionToNodeMap.end())
      return nullptr;

    assert(Found->second && "Unexpected null call graph node in map!");
    return Found->second;
  }

  CallGraphEdge *getCallGraphEdge(FullApplySite AI) {
    auto Found = ApplyToEdgeMap.find(AI);
    if (Found == ApplyToEdgeMap.end())
      return nullptr;

    assert(Found->second && "Unexpected null call graph edge in map!");
    return Found->second;
  }

  CallGraphEdge *getCallGraphEdge(FullApplySite AI) const {
    return const_cast<CallGraph *>(this)->getCallGraphEdge(AI);
  }

  // Functions for getting bottom-up lists of SCCs or functions in the
  // call graph.

  ArrayRef<CallGraphSCC *> getBottomUpSCCOrder() {
    if (BottomUpSCCOrder.empty())
      computeBottomUpSCCOrder();

    return BottomUpSCCOrder;
  }

  ArrayRef<SILFunction *> getBottomUpFunctionOrder() {
    if (BottomUpFunctionOrder.empty())
      computeBottomUpFunctionOrder();

    return BottomUpFunctionOrder;
  }

  // Functions for editing an existing call graph.

  void addEdgesForApply(FullApplySite AI) {
    addEdgesForApply(AI, getCallGraphNode(AI.getFunction()));
  }

  void removeEdge(CallGraphEdge *Edge);
  void removeEdgesForApply(FullApplySite AI);

  void markCallerEdgesOfCalleesIncomplete(FullApplySite AI);

  void dump();

  void verify() const;

private:
  void addCallGraphNode(SILFunction *F, unsigned Ordinal);
  void addEdges(SILFunction *F);
  bool tryGetCalleeSet(SILValue Callee, CallGraphEdge::CalleeSetType &CalleeSet,
                       bool &Complete);
  void addEdgesForApply(FullApplySite AI, CallGraphNode *CallerNode);
  void computeBottomUpSCCOrder();
  void computeBottomUpFunctionOrder();
};

/// The Call Graph Analysis provides information about the call graph.
class CallGraphAnalysis : public SILAnalysis {
  SILModule *M;
  std::vector<SILFunction *> BottomUpFunctionOrder;
  CallGraph *CG;

public:
  virtual ~CallGraphAnalysis() {
    delete CG;
  }
  CallGraphAnalysis(SILModule *MM) : SILAnalysis(AnalysisKind::CallGraph),
                                     M(MM), CG(nullptr) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::CallGraph;
  }

  bool haveCallGraph() { return CG; }
  CallGraph *getCallGraphOrNull() { return CG; }
  CallGraph &getCallGraph() {
    assert(haveCallGraph() && "Expected constructed call graph!");
    return *CG;
  }

  CallGraph &getOrBuildCallGraph() {
    if (!CG)
      CG = new CallGraph(M, false);
    return *CG;
  }

  virtual void invalidate(SILAnalysis::PreserveKind K) {
    if (K & PreserveKind::Calls) return;

    BottomUpFunctionOrder.clear();
    delete CG;
    CG = nullptr;
  }

  virtual void invalidate(SILFunction*, SILAnalysis::PreserveKind K) {
    invalidate(K);
  }

  virtual void verify() const;
};

} // end namespace swift

#endif
