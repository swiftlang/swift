//===----------- EscapeAnalysis.h - SIL Escape Analysis -*- C++ -*---------===//
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

#ifndef SWIFT_SILANALYSIS_ESCAPEANALYSIS_H_
#define SWIFT_SILANALYSIS_ESCAPEANALYSIS_H_

#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILAnalysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallBitVector.h"

struct CGForDotView;

namespace swift {

class CallGraphAnalysis;
class CallGraph;

/// The EscapeAnalysis provides information if the lifetime of an object exceeds
/// the scope of a function.
/// The analysis is done intra- and interprocedural. The intraprocedural
/// analysis is updated on the fly based on the usual invalidation mechanism.
/// The interprocedural analysis is explicitly updated at some points in the
/// pass pipeline by the UpdateSideEffects pass.
///
/// We compute the escape analysis by building a connection graph for each
/// function. For the interprocedural analysis the connection graphs are merged
/// in bottom-up order of the call graph.
/// The idea is based on "Escape analysis for Java." by J.-D. Choi, M. Gupta, M.
/// Serrano, V. C. Sreedhar, and S. Midkiff
/// http://dx.doi.org/10.1145/320384.320386
class EscapeAnalysis : public SILAnalysis {

  /// The types of edges in the connection graph.
  /// Escape information is propagated along edges in the connection graph:
  /// for an edge a -> b: if a escapes then also b escapes.
  enum EdgeType {
    /// Represents a points-to relationship: a pointer points to a content.
    /// The destination node must always be of type Content. For each pointer p
    /// there is also a points-to edge p -> c, where c is the content node for p.
    PointsTo = 0,
    
    /// Represents an assignment: "a = b" creates a defer-edge a -> b.
    /// A load "a = *p" is represented by a defer-edge a -> c, where c is p's
    /// content node. Similarly, A store "*p = b" is represented by c -> b.
    Defer = 1
  };

  /// The types of nodes (CGNode) in the connection graph.
  enum class NodeType : char {
    /// Represents a "pointer" value. We define a "pointer" to be an address-type
    /// value, an object reference or any value-type (struct, enum, etc.) which
    /// contains a reference. If a value-type (e.g. a struct) contains multiple
    /// references, it is treated as a single "pointer" which may point to any
    /// of the referenced objects.
    Value,
    
    /// Represents the "memory content" to which a pointer points to.
    /// The "content" represents all stored properties of the referenced object.
    /// Note that currently we don't consider projections and we summarize all
    /// stored properties of an object into a single "content".
    Content,
    
    /// A function argument, which is just a special case of Value type.
    Argument,

    /// The function return value, which is also just a special case of Value
    /// type.
    Return
   };

  /// Bits in the CGNode's escape set.
  enum {
    /// The node's value escapes to any global or unidentified memory.
    GlobalEscape,
    
    /// The node's value escapes through the function's return value.
    ReturnEscape,
    
    /// The first of the consecutive function argument bits: The node's value
    /// escapes through a function argument.
    FirstArg
  };

  class CGNode;
  class CGNodeMap;
  class ConnectionGraph;

  /// The int-part is an EdgeType and specifies which kind of predecessor it is.
  typedef llvm::PointerIntPair<CGNode *, 1> Predecessor;

  /// A node in the connection graph.
  /// A node basically represents a "pointer" or the "memory content" where a
  /// pointer points to (see NodeType).
  class CGNode {

    /// The associated value in the functino. It is only used for debug printing.
    /// There may be multiple nodes associated to the same value, e.g. a Content
    /// node has the same V as its points-to predecessor.
    ValueBase *V;

    /// The outgoing points-to edge (if any) to a Content node. See also:
    /// pointsToIsEdge.
    /// If we ever want to distinguish between different fields of an object,
    /// then we should replace the single pointsTo edge with multiple edges -
    /// one for each field.
    CGNode *pointsTo = nullptr;
    
    /// The outgoing defer edges.
    llvm::SmallVector<CGNode *, 8> defersTo;
    
    /// The predecessor edges (points-to and defer).
    llvm::SmallVector<Predecessor, 8> Preds;
    
    /// If this Content node is merged with another Content node, mergeTo is
    /// the merge destination.
    CGNode *mergeTo = nullptr;
    
    /// If true, the pointsTo is a real edge in the graph. Otherwise it is not
    /// and edge (e.g. this does not appear in the pointsTo Preds list), but
    /// still must point to the same Content node as all successor nodes.
    bool pointsToIsEdge = false;
    
    /// Used for various worklist algorithms.
    bool isInWorkList = false;
    
    /// True if the merge is finished (see mergeTo). In this state this node
    /// is completly unlinked from the graph,
    bool isMerged = false;
    
    /// The type of the node (mainly distinguishes between content and value
    /// nodes).
    NodeType Type;
    
    /// The actual result of the escape analysis. It tells if and how (global or
    /// through arguments) the pointer escapes.
    llvm::SmallBitVector EscapeSet;

    /// The constructor. \p numArgs is the number of function arguments.
    CGNode(ValueBase *V, NodeType Type, int numArgs) :
       V(V), Type(Type), EscapeSet(numArgs + FirstArg) { }
    
    /// Merges the escape set from another node and returns true if any new bits
    /// were set.
    bool mergeEscapeSet(const CGNode *From) {
      bool changed = From->EscapeSet.test(EscapeSet);
      EscapeSet |= From->EscapeSet;
      return changed;
    }

    /// Returns the Content node if this node has an outgoing points-to edge.
    CGNode *getPointsToEdge() const {
      return pointsToIsEdge ? pointsTo : nullptr;
    }

    /// Finds a successor node in the outgoing defer edges.
    llvm::SmallVectorImpl<CGNode *>::iterator findDefered(CGNode *Def) {
      return std::find(defersTo.begin(), defersTo.end(), Def);
    }

    /// Finds a predecessor node in the incoming points-to or defer edges.
    llvm::SmallVectorImpl<Predecessor>::iterator findPred(Predecessor Pred) {
      return std::find(Preds.begin(), Preds.end(), Pred);
    }

    /// Removes a predecessor node.
    void removeFromPreds(Predecessor Pred) {
      auto Iter = findPred(Pred);
      assert(Iter != Preds.end() && "Predecessor to remove not found");
      Preds.erase(Iter);
    }

    /// Adds a defer-edge to another node \p To. Not done if \p To is this node.
    bool addDefered(CGNode *To) {
      assert(!To->isMerged);
      if (To == this)
        return false;
      for (auto *Def : defersTo) {
        if (Def == To)
          return false;
      }
      defersTo.push_back(To);
      To->Preds.push_back(Predecessor(this, EdgeType::Defer));
      return true;
    }

    /// Sets the outgoing points-to edge. The \p To node must be a Content node.
    void setPointsTo(CGNode *To) {
      assert(!To->mergeTo);
      assert(To->Type == NodeType::Content &&
             "Wrong node type for points-to edge");
      pointsToIsEdge = true;
      pointsTo = To;
      To->Preds.push_back(Predecessor(this, EdgeType::PointsTo));
    }

    /// If this node was merged with another node, the final merge target is
    /// returned.
    CGNode *getMergeTarget() {
      CGNode *Target = this;
      while (Target->mergeTo) {
        Target = Target->mergeTo;
        assert(Target->Type == NodeType::Content);
      }
      return Target;
    }

    /// For debug dumping.
    void dump() const;

    /// Returns a string representation of the node type. Also for debug dumping.
    const char *getTypeStr() const;

    /// Checks an invariant of the connection graph: The points-to nodes of
    /// the defer-successors must match with the points-to of this node.
    bool matchPointToOfDefers() const {
      for (CGNode *Def : defersTo) {
        if (pointsTo != Def->pointsTo)
          return false;
      }
      /// A defer-path in the graph must not end without the specified points-to
      /// node.
      if (pointsTo && !pointsToIsEdge && defersTo.empty())
        return false;
      return true;
    }
    
    friend class CGNodeMap;
    friend class ConnectionGraph;
    friend struct ::CGForDotView;

  public:
    
    /// Specifies that the node's value escapes to global or unidentified
    /// memory. Returns true if this changed the state.
    bool setEscapesGlobal() {
      bool isAlreadyEscaping = escapesGlobal();
      EscapeSet.set(GlobalEscape);
      return !isAlreadyEscaping;
    }

    /// Returns true if the node's value escapes to global or unidentified
    /// memory.
    bool escapesGlobal() const {
      return EscapeSet.test(GlobalEscape);
    }
  };

  /// Mapping from nodes in a calleee-graph to nodes in a caller-graph.
  class CGNodeMap {
    llvm::DenseMap<CGNode *, CGNode *> Map;
  public:
    void insert(CGNode *From, CGNode *To) {
      assert(!From->isMerged && !To->isMerged);
      Map[From] = To;
    }
    CGNode *get(CGNode *From) const {
      auto Iter = Map.find(From);
      if (Iter == Map.end())
        return nullptr;

      return Iter->second->getMergeTarget();
    }
  };

  /// The connection graph for a function. See also: EdgeType, NodeType and
  /// CGNode.
  /// A connection graph has these invariants:
  /// 1) A defer-edge must not form a self cycle, i.e. must have different
  ///    source and target nodes.
  /// 2) A node can only have a single outgoing points-to edge (is enforced by
  ///    CGNode::pointsTo being a single pointer and not a vector).
  /// 3) The target of a points-to edge must be a Content node.
  /// 4) For any node N, all pathes starting at N which consist of only
  ///    defer-edges and a single trailing points-to edge must lead to the same
  ///    Content node.
  class ConnectionGraph {

    /// Backlink to the graph's function.
    SILFunction *F;

    /// Backlink to the parent escape analysis.
    EscapeAnalysis *EA;

    /// Mapping from pointer SIL values to nodes in the graph. Such a value can
    /// never be a projection, because in case of projection-instruction the
    /// based operand value is used instead.
    /// Multiple values can map to the same node. See setNode().
    llvm::DenseMap<ValueBase *, CGNode *> Values2Nodes;

    /// All nodes.
    llvm::SmallVector<CGNode *, 16> Nodes;

    /// A to-do list of nodes to merge.
    llvm::SmallVector<CGNode *, 8> ToMerge;

    /// The pseudo node which represents the return value. It's type is
    /// NodeType::Return.
    CGNode *ReturnNode = nullptr;
    
    /// The callsites from which we have to merge the callee graphs.
    llvm::SmallVector<FullApplySite, 8> KnownCallees;
    
    /// If true, at least one of the callee graphs has changed. We have to merge
    /// them again.
    bool NeedMergeCallees = false;

    /// True if the graph is computed.
    bool Valid = false;

    /// The allocator for nodes.
    llvm::SpecificBumpPtrAllocator<CGNode> NodeAllocator;

    /// Allocates a node of a given type.
    CGNode *allocNode(ValueBase *V, NodeType Type) {
      CGNode *Node = new (NodeAllocator.Allocate()) CGNode(V, Type,
                                                   F->getArguments().size());
      Nodes.push_back(Node);
      return Node;
    }

    /// Adds a defer-edge and updates pointsTo of all defer-reachable nodes.
    /// The addition of a defer-edge may invalidate the graph invariance 4).
    /// If this is the case, all "mismatching" Content nodes are merged until
    /// invariance 4) is reached again.
    bool addDeferEdge(CGNode *From, CGNode *To);

    /// Adds the node \p From (to be merged with \p To) to the ToMerge list.
    /// The actual merging is done in mergeAllScheduledNodes().
    void scheduleToMerge(CGNode *From, CGNode *To) {
      assert(From->Type == NodeType::Content);
      assert(To->Type == NodeType::Content);
      CGNode *FromMergeTarget = From->getMergeTarget();
      CGNode *ToMergeTarget = To->getMergeTarget();
      if (FromMergeTarget != ToMergeTarget) {
        FromMergeTarget->mergeTo = ToMergeTarget;
        ToMerge.push_back(FromMergeTarget);
      }
    }

    /// Merges all nodes which are added to the ToMerge list.
    void mergeAllScheduledNodes();

    /// Transitively updates pointsTo of all nodes in the defer-edge web,
    /// starting at \p InitialNode.
    /// If a node in the web already points to another content node, the other
    /// content node is scheduled to be merged with \p pointsTo.
    void updatePointsTo(CGNode *InitialNode, CGNode *pointsTo);

    /// Merges all defer-edges from the callee graph.
    bool addDeferEdgesFromCallee(CGNode *CalleeSource,
                                   const CGNodeMap &Callee2CallerMapping);

  public:
    /// Constructs a connection graph for a function.
    ConnectionGraph(SILFunction *F, EscapeAnalysis *EA) : F(F), EA(EA) {
    }

    void setValid() { Valid = true; }

    /// Removes all nodes from the graph and sets it to invalid.
    void invalidate() {
      Values2Nodes.clear();
      Nodes.clear();
      ReturnNode = nullptr;
      Valid = false;
      NodeAllocator.DestroyAll();
      assert(ToMerge.empty());
      assert(!NeedMergeCallees);
    }

    SILFunction *getFunction() const { return F; }
    
    void addKnownCallee(FullApplySite FAS) { KnownCallees.push_back(FAS); }
    
    const llvm::SmallVectorImpl<FullApplySite> &getKnownCallees() {
      return KnownCallees;
    }

    void setNeedMergeCallees() { NeedMergeCallees = true; }

    // Returns true if we need to merge callee graphs and sets the flag to false.
    bool handleMergeCallees() {
      bool Result = NeedMergeCallees;
      NeedMergeCallees = false;
      return Result;
    }

    /// Gets or creates a node for a value \p V.
    /// If V is a projection(-path) then the base of the projection(-path) is
    /// taken. This means the node is always created for the "outermost" value
    /// where V is contained.
    /// Returns null, if V is not a "pointer".
    CGNode *getNode(ValueBase *V);

    /// Gets or creates a node for a SILValue (same as above).
    CGNode *getNode(SILValue V) {
      return getNode(V.getDef());
    }

    /// Gets or creates a content node to which \a AddrNode points to.
    CGNode *getContentNode(CGNode *AddrNode);

    /// Get or creates a pseudo node for the function return value.
    CGNode *getReturnNode(ReturnInst *RI) {
      if (!ReturnNode) {
        ReturnNode = allocNode(RI, NodeType::Return);
        ReturnNode->EscapeSet.set(ReturnEscape);
      }
      return ReturnNode;
    }

    /// Returns the node of the "exact" value \p V (no projections are skipped)
    /// if one exists.
    CGNode *getNodeOrNull(ValueBase *V) {
      return Values2Nodes.lookup(V);
    }
    
    /// Re-uses a node for another SIL value.
    void setNode(ValueBase *V, CGNode *Node) {
      assert(Values2Nodes.find(V) == Values2Nodes.end());
      Values2Nodes[V] = Node;
    }

    /// If V is a pointer, set it to global escaping.
    bool setEscapesGlobal(SILValue V) {
      if (CGNode *Node = getNode(V))
        return Node->setEscapesGlobal();
      return false;
    }

    /// Returns true if the node's value escapes to global or unidentified
    /// memory.
    bool escapesGlobal(SILValue V) {
      if (CGNode *Node = getNode(V))
        return Node->setEscapesGlobal();
      return false;
    }

    /// Creates a defer-edge between \p From and \p To.
    /// This may invalidate the graph invariance 4). See addDeferEdge.
    bool defer(CGNode *From, CGNode *To) {
      bool EdgeAdded = addDeferEdge(From, To);
      mergeAllScheduledNodes();
      verify();
      return EdgeAdded;
    }

    /// Merges the graph of a callee function (called by \p FAS) into this graph.
    bool mergeCalleeGraph(FullApplySite FAS, ConnectionGraph *CalleeGraph);

    /// Propagates the escape bits through the graph.
    void propagateEscapes();

    /// Debug print the graph.
    void print(llvm::raw_ostream &OS) const;

    /// Debug dump the graph.
    void dump() const;

    /// This function is meant for use from the debugger.  You can just say 'call
    /// CG->viewCG()' and a dot graph viewer window should pop up from the
    /// program, displaying the connection graph. This depends on there being a
    /// dot graph viewer program, like 'graphviz', in your path.
    ///
    /// Defer-edges are grey, points-to edges are black.
    /// Content nodes are rounded rectangles, argument/return nodes are bold.
    /// Global escaping nodes are red, argument escaping nodes are blue.
    void viewCG() const;

    /// Checks if the graph is OK.
    void verify() const;

    /// Just verifies the graph structure. This function can also be called
    /// during the graph is modified, e.g. in mergeAllScheduledNodes().
    void verifyStructure() const;

    friend struct ::CGForDotView;
  };

private:

  enum {
    /// A limit for the interprocedural algorithm.
    MaxGraphMerges = 4
  };

  /// The connection graphs for all functions (does not include external
  /// functions).
  llvm::DenseMap<SILFunction *, ConnectionGraph *> Function2ConGraph;
  
  /// The allocator for the connection graphs in Function2ConGraph.
  llvm::SpecificBumpPtrAllocator<ConnectionGraph> Allocator;

  /// Cache for isPointer().
  llvm::DenseMap<SILType, bool> isPointerCache;

  SILModule *M;

  /// This analysis depends on the call graph.
  CallGraphAnalysis *CGA;
  
  /// If false, nothing has changed between two recompute() calls.
  bool shouldRecompute;

  /// Returns true if \p V is a "pointer" value.
  /// See EscapeAnalysis::NodeType::Value.
  bool isPointer(ValueBase *V);

  /// Builds the connection graph for a function, but does not handle applys to
  /// known callees. This is done afterwards by mergeAllCallees.
  void buildConnectionGraph(SILFunction *F, ConnectionGraph *ConGraph);

  /// Updates the graph by analysing instruction \p I.
  void analyzeInstruction(SILInstruction *I, ConnectionGraph *ConGraph);

  /// Sets all operands and results of \p I as global escaping.
  bool setAllEscaping(SILInstruction *I, ConnectionGraph *ConGraph);

  /// Merge the graphs of all known callees into this graph.
  bool mergeAllCallees(ConnectionGraph *ConGraph, CallGraph &CG);

  /// Set all arguments and return values of all callees to global escaping.
  void finalizeGraphConservatively(ConnectionGraph *ConGraph);

  friend struct ::CGForDotView;

public:
  EscapeAnalysis(SILModule *M) :
    SILAnalysis(AnalysisKind::Escape), M(M), CGA(nullptr), shouldRecompute(true) {
  }

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::Escape;
  }

  virtual void initialize(SILPassManager *PM);

  /// Gets or creates a connection graph for \a F.
  ConnectionGraph *getConnectionGraph(SILFunction *F) {
    ConnectionGraph *&CG = Function2ConGraph[F];
    if (!CG) {
      CG = new (Allocator.Allocate()) ConnectionGraph(F, this);
    }
    return CG;
  }
  
  /// Recomputes the connection graphs for all functions the module.
  void recompute();

  virtual void invalidate(PreserveKind K) {
    Function2ConGraph.clear();
    Allocator.DestroyAll();
    shouldRecompute = true;
  }
  
  virtual void invalidate(SILFunction *F, PreserveKind K) {
    getConnectionGraph(F)->invalidate();
    shouldRecompute = true;
  }

  virtual void verify() const {
#ifndef NDEBUG
    for (auto Iter : Function2ConGraph) {
      ConnectionGraph *ConGraph = Iter.second;
      if (ConGraph)
        ConGraph->verify();
    }
#endif
  }

  virtual void verify(SILFunction *F) const {
#ifndef NDEBUG
    ConnectionGraph *ConGraph = Function2ConGraph.lookup(F);
    if (ConGraph)
      ConGraph->verify();
#endif
  }

};

} // end namespace swift

#endif
