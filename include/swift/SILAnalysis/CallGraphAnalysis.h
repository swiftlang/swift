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
  typedef llvm::DenseSet<CallGraphNode *> CalleeSetType;

private:
  // The call site represented by this call graph edge.
  ApplyInst *CallSite;

  // The set of functions potentially called from this call site. This
  // might include functions that are not actually callable based on
  // dynamic types. If the int bit is non-zero, the set is complete in
  // the sense that no function outside the set could be called.
  llvm::PointerIntPair<CalleeSetType *, 1> CalleeSet;

public:
  /// Create a call graph edge for a call site where we will fill in
  /// the set of potentially called functions later.
  CallGraphEdge(ApplyInst *CallSite, CalleeSetType &KnownCallees, bool Complete)
    : CallSite(CallSite),
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

  const ApplyInst *getCallSite() const {
    return CallSite;
  }

  /// Return a callee set that is known to be complete.
  const CalleeSetType &getCalleeSet() const {
    assert(isCalleeSetComplete() && "Attempt to get an incomplete call set!");
    return *CalleeSet.getPointer();
  }

  /// Return a callee set that is not known to be complete.
  const CalleeSetType &getKnownCalleeSet() {
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

/// A helper method for use with ArrayRefView. Just returns the CallSite
/// ApplyInst of E.
inline ApplyInst *getEdgeApplyInst(CallGraphEdge * const &E) {
  return const_cast<ApplyInst *>(E->getCallSite());
}

class CallGraphNode {
  /// The function represented by this call graph node.
  SILFunction *Function;

  /// The call graph node ordinal within the SILModule.
  const unsigned Ordinal;

  /// The known call sites that call into this function. The
  /// caller's call graph node owns these.
  llvm::SmallVector<CallGraphEdge *, 4> Callers;

  /// The call sites within this function. This CallGraph node owns these.
  llvm::SmallVector<CallGraphEdge *, 4> CallSites;

  /// Do we know all the potential callers of this function? We initialize this
  /// to !canHaveIndirectUses(F) optimistically and if we find any use that we
  /// can not prove does not cause a reference to a function_ref to escape in a
  /// we set this to false.
  bool CallerSetComplete;

public:
  friend class CallGraph;

  CallGraphNode(SILFunction *Function, unsigned Ordinal)
    : Function(Function), Ordinal(Ordinal),
      CallerSetComplete(!canHaveIndirectUses(Function)) {
    assert(Function &&
           "Cannot build a call graph node with a null function pointer!");
  }

  ~CallGraphNode() {
    for (auto *CallSite : getCallSites())
      delete CallSite;
  }

  SILFunction *getFunction() {
    return Function;
  }

  /// Get the complete set of edges associated with call sites that can call
  /// into this function.
  const llvm::SmallVectorImpl<CallGraphEdge *> &getCallers() const {
    assert(isCallerSetComplete() &&
           "Attempt to get an incomplete caller set!");
    return Callers;
  }

  // An adaptor that is used to show all of the apply insts which call the
  // SILFunction of this node.
  using CallerCallSiteList = ArrayRefView<CallGraphEdge *, ApplyInst *,
                                          getEdgeApplyInst>;

  /// Return the set of apply insts that can call into this function.
  CallerCallSiteList getCallerCallSites() const {
    return CallerCallSiteList(getCallers());
  }

  /// Get the known set of call graph edges that represent calls into this
  /// function.
  llvm::SmallVectorImpl<CallGraphEdge *> &getKnownCallers() {
    return Callers;
  }

  /// Return the known set of apply insts that can call into this function.
  CallerCallSiteList getKnownCallerCallSites() {
    return CallerCallSiteList(getKnownCallers());
  }

  /// Get the known set of call sites in this function.
  llvm::SmallVectorImpl<CallGraphEdge *> &getCallSites() {
    return CallSites;
  }

  /// Do we know that the set of call sites is complete - i.e. that
  /// there is no other place that we can call from that can reach
  /// this function?
  bool isCallerSetComplete() const {
    return CallerSetComplete;
  }

  unsigned getOrdinal() const {
    return Ordinal;
  }

private:
  /// Mark a set of callers as known to not be complete.
  void markCallerSetIncomplete() {
    CallerSetComplete = false;
  }

  /// Add an edge representing a call site within this function.
  void addCallSite(CallGraphEdge *CallSite) {
    CallSites.push_back(CallSite);
  }

  /// Add an edge representing a call site that calls into this function.
  void addCaller(CallGraphEdge *CallerCallSite) {
    Callers.push_back(CallerCallSite);
  }
};

struct CallGraphSCC {
  llvm::SmallVector<CallGraphNode *, 1> SCCNodes;
};

class CallGraph {
  llvm::SmallVector<CallGraphNode *, 16> CallGraphRoots;
  llvm::DenseMap<SILFunction *, CallGraphNode *> FunctionToNodeMap;
  llvm::SmallVector<CallGraphSCC *, 16> BottomUpSCCOrder;
  std::vector<SILFunction *> BottomUpFunctionOrder;

public:
  CallGraph(SILModule *M, bool completeModule);

  ~CallGraph() {
    for (auto &MapEntry : FunctionToNodeMap)
      delete MapEntry.second;

    for (auto *SCC : BottomUpSCCOrder)
      delete SCC;
  }

  llvm::SmallVectorImpl<CallGraphNode *> &getCallGraphRoots() {
    return CallGraphRoots;
  }

  CallGraphNode *getCallGraphNode(SILFunction *F) {
    auto Found = FunctionToNodeMap.find(F);
    if (Found == FunctionToNodeMap.end())
      return nullptr;

    assert(Found->second && "Unexpected null call graph node in map!");
    return Found->second;
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
};

} // end namespace swift

#endif
