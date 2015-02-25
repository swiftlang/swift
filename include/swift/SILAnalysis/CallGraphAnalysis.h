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
  ApplyInst *TheApply;

  // The set of functions potentially called from this call site. This
  // might include functions that are not actually callable based on
  // dynamic types. If the int bit is non-zero, the set is complete in
  // the sense that no function outside the set could be called.
  //
  // This is currently owned by the CallGraphEdge.
  llvm::PointerIntPair<CalleeSetType *, 1> CalleeSet;

public:
  /// Create a call graph edge for a call site where we will fill in
  /// the set of potentially called functions later.
  CallGraphEdge(ApplyInst *TheApply, CalleeSetType &KnownCallees, bool Complete)
    : TheApply(TheApply),
      // FIXME: Do not allocate memory for the singleton callee case.
      CalleeSet(new CalleeSetType, Complete) {

    // TODO: We will probably have many call sites that can share a
    //       callee set so we should optimize allocation and
    //       deallocation of these accordingly.
    CalleeSet.getPointer()->insert(KnownCallees.begin(), KnownCallees.end());
  }

  ~CallGraphEdge() {
    delete CalleeSet.getPointer();
  }

  const ApplyInst *getApply() const { return TheApply; }

  ApplyInst *getApply() { return TheApply; }

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
};

/// A helper method for use with ArrayRefView. Just returns the
/// ApplyInst of E.
inline const ApplyInst *getEdgeApplyInst(CallGraphEdge * const &E) {
  return E->getApply();
}

class CallGraphNode {
  /// The function represented by this call graph node.
  SILFunction *Function;

  /// The call graph node ordinal within the SILModule.
  const unsigned Ordinal;

  /// Edges representing the known call sites that could call into
  /// this function.
  ///
  /// This is owned by the callgraph itself, not the callgraph node.
  llvm::SmallVector<CallGraphEdge *, 4> CallerEdges;

  /// Edges representing the call sites within this function.
  ///
  /// This is owned by the callgraph itself, not the callgraph node.
  llvm::SmallVector<CallGraphEdge *, 4> CalleeEdges;

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
  const llvm::SmallVectorImpl<CallGraphEdge *> &getCompleteCallerEdges() const {
    assert(isCallerEdgesComplete() &&
           "Attempt to get an incomplete caller set!");
    return CallerEdges;
  }

  // An adaptor that is used to show all of the apply insts which call the
  // SILFunction of this node.
  using CallerCallSiteList = ArrayRefView<CallGraphEdge *, const ApplyInst *,
                                          getEdgeApplyInst>;

  /// Return the set of apply insts that can call into this function.
  CallerCallSiteList getCompleteCallerEdgesApplies() const {
    return CallerCallSiteList(getCompleteCallerEdges());
  }

  /// Get the known set of call graph edges that represent possible
  /// calls into this function.
  llvm::SmallVectorImpl<CallGraphEdge *> &getPartialCallerEdges() {
    return CallerEdges;
  }

  /// Return the known set of apply insts that can call into this function.
  CallerCallSiteList getPartialCallerEdgesApplies() {
    return CallerCallSiteList(getPartialCallerEdges());
  }

  /// Get the set of call sites in this function.
  llvm::SmallVectorImpl<CallGraphEdge *> &getCalleeEdges() {
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

private:
  /// Mark a set of callers as known to not be complete.
  void markCallerEdgesIncomplete() {
    CallerEdgesComplete = false;
  }

  /// Add an edge representing a call site within this function.
  void addCalleeEdge(CallGraphEdge *CallSite) {
    CalleeEdges.push_back(CallSite);
  }

  /// Add an edge representing a call site that calls into this function.
  void addCallerEdge(CallGraphEdge *CallerCallSite) {
    CallerEdges.push_back(CallerCallSite);
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
  llvm::DenseMap<ApplyInst *, CallGraphEdge *> ApplyToEdgeMap;

  /// A vector of SCCs in bottom up SCC order.
  llvm::SmallVector<CallGraphSCC *, 16> BottomUpSCCOrder;

  /// A vector of functions in bottom up function order.
  std::vector<SILFunction *> BottomUpFunctionOrder;

  /// An allocator used by the callgraph.
  llvm::BumpPtrAllocator Allocator;

public:
  CallGraph(SILModule *M, bool completeModule);
  ~CallGraph();

  llvm::SmallVectorImpl<CallGraphNode *> &getCallGraphRoots() {
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

  CallGraphEdge *getCallGraphEdge(ApplyInst *AI) {
    auto Found = ApplyToEdgeMap.find(AI);
    if (Found == ApplyToEdgeMap.end())
      return nullptr;

    assert(Found->second && "Unexpected null call graph edge in map!");
    return Found->second;
  }

  CallGraphEdge *getCallGraphEdge(ApplyInst *AI) const {
    return const_cast<CallGraph *>(this)->getCallGraphEdge(AI);
  }

  llvm::SmallVectorImpl<CallGraphSCC *> &getBottomUpSCCOrder() {
    if (BottomUpSCCOrder.empty())
      computeBottomUpSCCOrder();

    return BottomUpSCCOrder;
  }

  std::vector<SILFunction *> &getBottomUpFunctionOrder() {
    if (BottomUpFunctionOrder.empty())
      computeBottomUpFunctionOrder();

    return BottomUpFunctionOrder;
  }

  void verify() const;

private:
  void addCallGraphNode(SILFunction *F, unsigned Ordinal);
  void addEdges(SILFunction *F);
  bool tryGetCalleeSet(SILValue Callee, CallGraphEdge::CalleeSetType &CalleeSet,
                       bool &Complete);
  void addEdgesForApply(ApplyInst *AI, CallGraphNode *CallerNode);
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

  CallGraph &getCallGraph() {
    if (!CG)
      CG = new CallGraph(M, false);
    return *CG;
  }

  virtual void invalidate(InvalidationKind K) {
    if (K >= InvalidationKind::CallGraph) {
      BottomUpFunctionOrder.clear();
      delete CG;
      CG = nullptr;
    }
  }

  virtual void invalidate(SILFunction*, InvalidationKind K) { invalidate(K); }

  virtual void verify() const;
};

} // end namespace swift

#endif
