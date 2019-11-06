//===--- EscapeAnalysis.h - SIL Escape Analysis -----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// EscapeAnalysis provides information about whether the lifetime of an object
/// exceeds the scope of a function.
///
/// We compute escape analysis by building a connection graph for each
/// function. For interprocedural analysis the connection graphs are merged
/// in bottom-up order of the call graph.
/// The idea is based on "Escape analysis for Java." by J.-D. Choi, M. Gupta, M.
/// Serrano, V. C. Sreedhar, and S. Midkiff
/// http://dx.doi.org/10.1145/320384.320386
///
/// This design is customized for SIL and the Swift memory model as follows:
///
/// Each SILValue holding a memory address or object reference is mapped to a
/// node in the connection graph. The node's type depends on the value's
/// origin. SILArguments have "argument" type. Locally allocated storage and
/// values of unknown origin have "value" type. Loaded values have "content"
/// type. A "return" type node represents the returned value and has no
/// associated SILValue.
///
/// "Content" nodes are special in that they represent the identity of some set
/// of memory locations. Content nodes are created to represent the memory
/// pointed to by one of the other node types. So, except for loads, SILValues
/// do not directly map to content nodes. For debugging purposes only, content
/// nodes do refer back to the SILValue that originally pointed to them. When
/// content nodes are merged, only one of those SILValue back-references is
/// arbitrarily preserved. The content of the returned value is the only content
/// node that has no back-reference to a SILValue.
///
/// This code:
///   let a = SomeClass()
///   return a
///
/// Generates the following connection graph, where 'a' is in the SILValue %0:
///   Val %0 Esc: R, Succ: (%0.1) // Represents 'a', and points to 'a's content
///   Con %0.1 Esc: G, Succ:      // Represents the content of 'a'
///   Ret  Esc: R, Succ: %0       // The returned value, aliased with 'a'
///
/// Each node has an escaping state: None, (R)eturn, (A)rguments, or (G)lobal.
/// These states form a lattice in which None is the most refined, or top, state
/// and Global is the least refined, or bottom, state. Merging nodes performs a
/// meet operation on their escaping states. At a call site, the callee graph is
/// merged with the callee graph by merging the respective call argument
/// nodes. A node has a "Return" escaping state if it only escapes by being
/// returned from the current function. A node has an "Argument" escaping state
/// if only escapes by being passed as an incoming argument to this function.
///
/// A directed edge between two connection graph nodes indicates that the memory
/// represented by the destination node memory is reachable via an address
/// contained in the source node. A node may only have one "pointsTo" edge,
/// whose destination is always a content node. Additional "defer" edges allow a
/// form of aliasing between nodes. A single content node represents any and all
/// memory that any other node may point to. This content node can be found by
/// following any path of defer edges until the path terminates in a pointsTo
/// edge. The final pointsTo edge refers to the representative content node, and
/// all such paths in the graph must reach the same content node. To maintain
/// this invariant, the algorithm that builds the connection graph must
/// incrementally merge content nodes.
///
/// Note that a defer edge may occur between any node types. A value node that
/// holds a reference may defer to another value or content node whose value was
/// merged via a phi; a content node that holds a reference may defer to a value
/// node that was stored into the content; a content node may defer to another
/// content node that was loaded and stored.
///
/// Now consider the same example, but declaring a 'var' instead of a 'let':
///
///   var a = SomeClass()
///   return a
///
/// Generates the following connection graph, where the alloc_stack for variable
/// 'a' is in the SILValue %0 and class allocation returns SILValue %3.
///   Val %0 Esc: G, Succ: (%0.1)
///   Con %0.1 Esc: G, Succ: %3
///   Val %3 Esc: G, Succ: (%3.1)
///   Con %3.1 Esc: G, Succ:
///   Ret  Esc: R, Succ: %3
///
/// The value node for variable 'a' now points to local variable storage
/// (%0.1). That local variable storage contains a reference. Assignment into
/// that reference creates a defer edge to the allocated reference (%3). The
/// allocated reference in turn points to the object storage (%3.1).
///
/// Note that a variable holding a single class reference and a variable
/// holding a non-trivial struct has the same graph representation. The
/// variable's content node only represents the value of the references, not the
/// memory pointed-to by the reference.
///
/// A pointsTo edge does not necessarily indicate pointer indirection. It may
/// simply represent a derived address within the same object. This allows
/// escape analysis to view an object's memory in layers, each with separate
/// escaping properties. For example, a class object's first-level content node
/// represents the object header including the metadata pointer and reference
/// count. An object's second level content node only represents the
/// reference-holding fields within that object. Consider the connection graph
/// for a class with properties:
///
///   class HasObj {
///     var obj: AnyObject
///   }
///   func assignProperty(h: HasObj, o: AnyObject) {
///     h.obj = o
///   }
///
/// Which generates this graph where the argument 'h' is %0, and 'o' is %1:
///   Arg %0 Esc: A, Succ: (%0.1)
///   Con %0.1 Esc: A, Succ: (%0.2)
///   Con %0.2 Esc: A, Succ: %1
///   Arg %1 Esc: A, Succ: (%1.1)
///   Con %1.1 Esc: A, Succ: (%1.2)
///   Con %1.2 Esc: G, Succ:
///
/// Node %0.1 represents the header of 'h', including reference count and
/// metadata pointer. This node points to %0.2 which represents the 'obj'
/// property. The assignment 'h.obj = o' creates a defer edge from %0.2 to
/// %1. Similarly, %1.1 represents the header of 'o', and %1.2 represents any
/// potential nontrivial properties in 'o' which may have escaped globally when
/// 'o' was released.
///
/// The connection graph is constructed by summarizing all memory operations in
/// a flow-insensitive way. Hint: ConGraph->viewCG() displays the Dot-formatted
/// connection graph.
///
/// In addition to the connection graph, EscapeAnalysis stores information about
/// "use points". Each release operation is a use points. These instructions are
/// recorded in a table and given an ID. Each connection graph node stores a
/// bitset indicating the use points reachable via the CFG by that node. This
/// provides some flow-sensitive information on top of the otherwise flow
/// insensitive connection graph.
///
/// Note: storing bitsets in each node may be unnecessary overhead since the
/// same information can be obtained with a graph traversal, typically of only
/// 1-3 hops.
// ===---------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_ESCAPEANALYSIS_H_
#define SWIFT_SILOPTIMIZER_ANALYSIS_ESCAPEANALYSIS_H_

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/Analysis/BottomUpIPAnalysis.h"
#include "swift/SILOptimizer/Analysis/ValueTracking.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallVector.h"

struct CGForDotView;

namespace swift {

class BasicCalleeAnalysis;

/// The EscapeAnalysis results for functions in the current module, computed
/// bottom-up in the call graph. Each function with valid EscapeAnalysis
/// information is associated with a ConnectionGraph.
class EscapeAnalysis : public BottomUpIPAnalysis {

  /// The types of edges in the connection graph.
  /// Escape information is propagated along edges in the connection graph:
  /// for an edge a -> b: if a escapes then also b escapes.
  enum EdgeType {
    /// Represents a points-to relationship: a pointer points to a content.
    /// The destination node must always be of type Content. For each pointer p
    /// there is also a points-to edge p -> c, where c is the content node for p.
    /// We use the points-to relationship also for a ref_element_addr
    /// projection (see NodeType::Content).
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
    /// We also treat the elements of a reference-counted object as a "content"
    /// of that object. Although ref_element_addr is just a pointer addition, we
    /// treat it as a "pointer" pointing to the elements. Having this additional
    /// indirection in the graph, we avoid letting a reference escape just
    /// because an element address escapes.
    /// Note that currently we don't consider other projections and we summarize
    /// all stored properties of an object into a single "content".
    ///
    Content,
    
    /// A function argument, which is just a special case of Value type.
    Argument,

    /// The function return value, which is also just a special case of Value
    /// type.
    Return
  };

  /// Indicates to what a value escapes. Note: the order of values is important.
  enum class EscapeState : char {

    /// The node's value does not escape.
    /// The value points to a locally allocated object who's lifetime ends in
    /// the same function.
    None,
    
    /// The node's value escapes through the return value.
    /// The value points to a locally allocated object which escapes via the
    /// return instruction.
    Return,

    /// The node's value escapes through a function argument.
    Arguments,

    /// The node's value escapes to any global or unidentified memory.
    Global
  };

public:
  class CGNode;
  class ConnectionGraph;

private:
  class CGNodeMap;
  struct CGNodeWorklist;

  /// The int-part is an EdgeType and specifies which kind of predecessor it is.
  typedef llvm::PointerIntPair<CGNode *, 1> Predecessor;

public:

  /// A node in the connection graph.
  /// A node basically represents a "pointer" or the "memory content" where a
  /// pointer points to (see NodeType).
  class CGNode {

    /// The associated value in the function. This is always valid for Argument
    /// and Value nodes, and always nullptr for Return nodes. For Content nodes,
    /// i is only used for debug printing. A Content's value 'V' is unreliable
    /// because it can change as a result of the order the the graph is
    /// constructed and summary graphs are merged. Sometimes it is derived from
    /// the instructions that access the content, other times, it's simply
    /// inherited from its parent. There may be multiple nodes associated to the
    /// same value, e.g. a Content node has the same V as its points-to
    /// predecessor.
    ValueBase *V;

    /// The outgoing points-to edge (if any) to a Content node. See also:
    /// pointsToIsEdge.
    /// If we ever want to distinguish between different fields of an object,
    /// then we should replace the single pointsTo edge with multiple edges -
    /// one for each field.
    ///
    /// Note: A content node with proper a "pointsTo" edge to another content
    /// node often does *not* represent a pointer. There is no
    /// indirection. Instead, these content nodes are all part of the same
    /// object and only represent different layers within the object.
    CGNode *pointsTo = nullptr;

    /// The outgoing defer edges.
    llvm::SmallVector<CGNode *, 8> defersTo;
    
    /// The predecessor edges (points-to and defer).
    llvm::SmallVector<Predecessor, 8> Preds;
    
    /// If this Content node is merged with another Content node, mergeTo is
    /// the merge destination.
    CGNode *mergeTo = nullptr;

    /// Information where the node's value is used in its function.
    /// Each bit corresponds to an argument/instruction where the value is used.
    /// The UsePoints on demand when calling ConnectionGraph::getUsePoints().
    SmallBitVector UsePoints;

    /// The actual result of the escape analysis. It tells if and how (global or
    /// through arguments) the value escapes.
    EscapeState State = EscapeState::None;

    /// If true, the pointsTo is a real edge in the graph. Otherwise it is not
    /// an edge (e.g. this does not appear in the pointsTo Preds list), but
    /// still must point to the same Content node as all successor nodes.
    bool pointsToIsEdge = false;
    
    /// Used for various worklist algorithms.
    bool isInWorkList = false;
    
    /// True if the merge is finished (see mergeTo). In this state this node
    /// is completely unlinked from the graph,
    bool isMerged = false;
    
    /// The type of the node (mainly distinguishes between content and value
    /// nodes).
    NodeType Type;
    
    /// The constructor.
    CGNode(ValueBase *V, NodeType Type) : V(V), UsePoints(0), Type(Type) {
      switch (Type) {
      case NodeType::Argument:
      case NodeType::Value:
        assert(V);
        break;
      case NodeType::Return:
        assert(!V);
        break;
      case NodeType::Content:
        // A content node representing the returned value has no associated
        // SILValue.
        break;
      }
    }

    /// Merges the use points from another node and returns true if there are
    /// any changes.
    bool mergeUsePoints(CGNode *RHS) {
      bool Changed = RHS->UsePoints.test(UsePoints);
      UsePoints |= RHS->UsePoints;
      return Changed;
    }

    // Merge the properties of \p fromNode into this node.
    void mergeProperties(CGNode *fromNode);

    /// Finds a successor node in the outgoing defer edges.
    llvm::SmallVectorImpl<CGNode *>::iterator findDeferred(CGNode *Def) {
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

    bool canAddDeferred(CGNode *To) {
      if (To == this)
        return false;
      for (auto *Def : defersTo) {
        if (Def == To)
          return false;
      }
      return true;
    }

    /// Adds a defer-edge to another node \p To. Not done if \p To is this node.
    bool addDeferred(CGNode *To) {
      assert(!To->isMerged);
      if (!canAddDeferred(To))
        return false;
      To->Preds.push_back(Predecessor(this, EdgeType::Defer));
      defersTo.push_back(To);
      return true;
    }

    /// Sets the outgoing points-to edge. The \p To node must be a Content node.
    void setPointsToEdge(CGNode *To) {
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

    void setUsePointBit(int Idx) {
      UsePoints.resize(Idx + 1, false);
      UsePoints.set(Idx);
    }

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
    friend struct CGNodeWorklist;

  public:
    SILValue getValue() const {
      assert(Type == NodeType::Argument || Type == NodeType::Value);
      return V;
    }
    SILValue getValueOrNull() const { return V; }

    void updateValue(SILValue newValue) {
      assert(isContent());
      V = newValue;
    }

    /// Return true if this node represents content.
    bool isContent() const { return Type == NodeType::Content; }
    /// Returns the escape state.
    EscapeState getEscapeState() const { return State; }

    /// Returns true if the node's value escapes within the function or via
    /// the return instruction.
    bool escapes() const { return getEscapeState() != EscapeState::None; }

    /// Specifies that this content node's memory escapes to global or
    /// unidentified memory.
    void markEscaping() {
      assert(Type == NodeType::Content);
      mergeEscapeState(EscapeState::Global);
    }

    /// Merges the state from another state and returns true if it changed.
    bool mergeEscapeState(EscapeState OtherState) {
      if (OtherState > State) {
        State = OtherState;
        return true;
      }
      return false;
    }

    /// Returns true if the node's value escapes within the function. This
    /// means that any unidentified pointer in the function may alias to
    /// the node's value.
    /// Note that in the false-case the node's value can still escape via
    /// the return instruction.
    ///
    /// \p nodeValue is often the same as 'this->V', but is sometimes a more
    /// refined value. For content nodes, 'this->V' is only a placeholder that
    /// does not necessarilly represent the node's memory.
    bool escapesInsideFunction(SILValue nodeValue) const {
      switch (getEscapeState()) {
        case EscapeState::None:
        case EscapeState::Return:
          return false;
        case EscapeState::Arguments:
          return !isExclusiveArgument(nodeValue);
        case EscapeState::Global:
          return true;
      }

      llvm_unreachable("Unhandled EscapeState in switch.");
    }

    /// Returns the content node if of this node if it exists in the graph.
    CGNode *getContentNodeOrNull() const {
      return pointsTo;
    }

    /// Returns the Content node if this node has an outgoing points-to edge.
    CGNode *getPointsToEdge() const {
      return pointsToIsEdge ? pointsTo : nullptr;
    }

    /// Visit all successors of this node in the connection graph until \p
    /// visitor returns false. Return true if all successors were visited
    /// without \p visitor returning false.
    ///
    /// Note that a node may be the pointsTo successor of itself.
    template <typename Visitor> bool visitSuccessors(Visitor &&visitor) const;

    /// Visit all adjacent defers. Halt when the visitor returns false. Return
    /// true if the visitor returned true for all defers.
    template <typename Visitor> bool visitDefers(Visitor &&visitor) const;

    /// For debug dumping.
    void dump() const;

    /// Returns a string representation of the node type. For debug dumping.
    const char *getTypeStr() const;
  };

public:
  /// The connection graph for a function. See also: EdgeType, NodeType and
  /// CGNode.
  /// A connection graph has these invariants:
  /// 1) A defer-edge must not form a self cycle, i.e. must have different
  ///    source and target nodes.
  /// 2) A node can only have a single outgoing points-to edge (is enforced by
  ///    CGNode::pointsTo being a single pointer and not a vector).
  /// 3) The target of a points-to edge must be a Content node.
  /// 4) For any node N, all paths starting at N which consist of only
  ///    defer-edges and a single trailing points-to edge must lead to the same
  ///    Content node.
  ///
  /// Additionally, all nodes in a path consisting of defer-edges must have the
  /// same pointsTo field--either all pointsTo fields are null, or they all
  /// point to the same target of the points-to edges at the leaves of the defer
  /// web, which must have been merged into a single content node.
  ///
  /// Paths comprised of points-to edges may contain cycles and self-cycles.
  class ConnectionGraph {
    /// Backlink to the graph's function.
    SILFunction *F;
    /// Backlink to the EscapeAnalysis
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

    /// The list of use points.
    llvm::SmallVector<SILNode *, 16> UsePointTable;

    /// Mapping of use points to bit indices in CGNode::UsePoints.
    llvm::DenseMap<SILNode *, int> UsePoints;

    /// The allocator for nodes.
    llvm::SpecificBumpPtrAllocator<CGNode> NodeAllocator;

    /// True if this is a summary graph.
    bool isSummaryGraph;

    /// Track the currently active intrusive worklist -- one at a time.
    CGNodeWorklist *activeWorklist = nullptr;

    /// Constructs a connection graph for a function.
    ConnectionGraph(SILFunction *F, EscapeAnalysis *EA, bool isSummaryGraph)
        : F(F), EA(EA), isSummaryGraph(isSummaryGraph) {}

    /// Returns true if the connection graph is empty.
    bool isEmpty() {
      return Values2Nodes.empty() && Nodes.empty() && UsePoints.empty();
    }

    /// Removes all nodes from the graph.
    void clear();
    
    /// Allocates a node of a given type.
    CGNode *allocNode(ValueBase *V, NodeType Type) {
      CGNode *Node = new (NodeAllocator.Allocate()) CGNode(V, Type);
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
        ToMergeTarget->mergeProperties(FromMergeTarget);
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

    /// Utility function to clear the isInWorkList flags of all nodes in
    /// \p WorkList.
    static void clearWorkListFlags(const SmallVectorImpl<CGNode *> &WorkList) {
      for (CGNode *Node : WorkList) {
        Node->isInWorkList = false;
      }
    }

    /// Gets or creates a node for a value \p V.
    /// If V is a projection(-path) then the base of the projection(-path) is
    /// taken. This means the node is always created for the "outermost" value
    /// where V is contained.
    /// Returns null, if V is not a "pointer".
    CGNode *getNode(ValueBase *V, bool createIfNeeded = true);

    /// Gets or creates a content node to which \a AddrNode points to during
    /// initial graph construction. This may not be called after defer edges
    /// have been created. Doing so would break the invariant that all
    /// non-content nodes ultimately have a pointsTo edge to a single content
    /// node.
    CGNode *getContentNode(CGNode *AddrNode);

    /// Get or creates a pseudo node for the function return value.
    CGNode *getReturnNode() {
      if (!ReturnNode) {
        ReturnNode = allocNode(nullptr, NodeType::Return);
        if (!isSummaryGraph)
          ReturnNode->mergeEscapeState(EscapeState::Return);
      }
      return ReturnNode;
    }

    /// Returns the node for the function return value if present.
    CGNode *getReturnNodeOrNull() const {
      return ReturnNode;
    }

    /// Returns the node of the "exact" value \p V (no projections are skipped)
    /// if one exists.
    CGNode *lookupNode(ValueBase *V) {
      CGNode *Node = Values2Nodes.lookup(V);
      if (Node)
        return Node->getMergeTarget();
      return nullptr;
    }
    
    /// Re-uses a node for another SIL value.
    void setNode(ValueBase *V, CGNode *Node) {
      assert(Values2Nodes.find(V) == Values2Nodes.end());
      Values2Nodes[V] = Node;
    }

    /// Adds an argument/instruction in which the node's value is used.
    int addUsePoint(CGNode *Node, SILNode *User) {
      if (Node->getEscapeState() >= EscapeState::Global)
        return -1;

      User = User->getRepresentativeSILNodeInObject();
      int Idx = (int)UsePoints.size();
      assert(UsePoints.count(User) == 0 && "value is already a use-point");
      UsePoints[User] = Idx;
      UsePointTable.push_back(User);
      assert(UsePoints.size() == UsePointTable.size());
      Node->setUsePointBit(Idx);
      return Idx;
    }

    /// Specifies that the node's value escapes to global or unidentified
    /// memory.
    void setEscapesGlobal(CGNode *Node) {
      Node->mergeEscapeState(EscapeState::Global);

      // Make sure to have a content node. Otherwise we may end up not merging
      // the global-escape state into a caller graph (only content nodes are
      // merged). Either the node itself is a content node or we let the node
      // point to one.
      if (Node->Type != NodeType::Content)
        getContentNode(Node);
    }

    /// Creates a defer-edge between \p From and \p To.
    /// This may trigger node merges to keep the graph invariance 4).
    /// Returns the \p From node or its merge-target in case \p From was merged
    /// during adding the edge.
    /// The \p EdgeAdded is set to true if there was no defer-edge between
    /// \p From and \p To, yet.
    CGNode *defer(CGNode *From, CGNode *To, bool &EdgeAdded) {
      if (addDeferEdge(From, To))
        EdgeAdded = true;
      mergeAllScheduledNodes();
      return From->getMergeTarget();
    }

    /// Creates a defer-edge between \p From and \p To.
    /// This may trigger node merges to keep the graph invariance 4).
    /// Returns the \p From node or its merge-target in case \p From was merged
    /// during adding the edge.
    CGNode *defer(CGNode *From, CGNode *To) {
      bool UnusedChangedFlag = false;
      return defer(From, To, UnusedChangedFlag);
    }

    /// Merges the \p SourceGraph into this graph. The \p Mapping contains the
    /// initial node mapping of the nodes to start the merge.
    bool mergeFrom(ConnectionGraph *SourceGraph, CGNodeMap &Mapping);

    /// Propagates the escape states through the graph.
    void propagateEscapeStates();

    /// Removes a value from the graph.
    /// It does not delete its node but makes sure that the value cannot be
    /// lookup-up with getNode() anymore.
    void removeFromGraph(ValueBase *V) { Values2Nodes.erase(V); }

    enum class Traversal { Follow, Backtrack, Halt };

    /// Traverse backward from startNode, following predecessor edges.
    ///
    /// CGNodeVisitor takes the current CGNode and returns Traversal::Follow if
    /// traversal should proceed along its predecessors, Traversal::Backtrack,
    /// if it should not follow its predecessors, and Traversal::Halt if it
    /// should immediately stop visiting nodes.
    ///
    /// Return true if the visitor did not halt traversal.
    template <typename CGPredVisitor>
    bool backwardTraverse(CGNode *startNode, CGPredVisitor &&visitor);

    /// Traverse forward from startNode, following defer edges.
    ///
    /// CGNodeVisitor takes the current CGNode and returns Traversal::Follow if
    /// traversal should proceed along its predecessors, Traversal::Backtrack,
    /// if it should not follow its predecessors, and Traversal::Halt if it
    /// should immediately stop visiting nodes.
    ///
    /// Return true if the visitor did not halt traversal.
    template <typename CGNodeVisitor>
    bool forwardTraverseDefer(CGNode *startNode, CGNodeVisitor &&visitor);

    /// Return true if \p pointer may indirectly point to \pointee via pointers
    /// and object references.
    bool mayReach(CGNode *pointer, CGNode *pointee);

  public:
    /// Gets or creates a node for a value \p V.
    /// If V is a projection(-path) then the base of the projection(-path) is
    /// taken. This means the node is always created for the "outermost" value
    /// where V is contained.
    /// Returns null, if V is not a "pointer".
    CGNode *getNodeOrNull(ValueBase *V) { return getNode(V, false); }

    /// Returns the number of use-points of a node.
    int getNumUsePoints(CGNode *Node) {
      assert(!Node->escapes() &&
             "Use points are only valid for non-escaping nodes");
      return Node->UsePoints.count();
    }

    /// Returns true if \p UsePoint is a use of \p Node, i.e. UsePoint may
    /// (indirectly) somehow refer to the Node's value.
    /// Use-points are only values which are relevant for lifeness computation,
    /// e.g. release or apply instructions.
    bool isUsePoint(SILNode *UsePoint, CGNode *Node);

    /// Returns all use points of \p Node in \p UsePoints.
    void getUsePoints(CGNode *Node,
                      llvm::SmallVectorImpl<SILNode *> &UsePoints);

    /// Computes the use point information.
    void computeUsePoints();

    /// Debug print the graph.
    void print(llvm::raw_ostream &OS) const;

    /// Debug dump the graph.
    void dump() const;

    /// This function is meant for use from the debugger.  You can just say 'call
    /// CG->viewCG()' and a dot graph viewer window should pop up from the
    /// program, displaying the connection graph. This depends on there being a
    /// dot graph viewer program, like 'graphviz', in your path.
    ///
    /// Defer-edges are gray, points-to edges are black.
    /// Content nodes are rounded rectangles, argument/return nodes are bold.
    /// Global escaping nodes are red, argument escaping nodes are blue.
    void viewCG() const;

    /// Dump the connection graph to a DOT file for remote debugging.
    void dumpCG() const;

    /// Checks if the graph is OK.
    void verify() const;

    /// Just verifies the graph structure. This function can also be called
    /// during the graph is modified, e.g. in mergeAllScheduledNodes().
    void verifyStructure() const;

    friend struct ::CGForDotView;
    friend class EscapeAnalysis;
    friend struct CGNodeWorklist;
  };

private:

  /// All the information we keep for a function.
  struct FunctionInfo : public FunctionInfoBase<FunctionInfo> {
    FunctionInfo(SILFunction *F, EscapeAnalysis *EA)
        : Graph(F, EA, false), SummaryGraph(F, EA, true) {}

    /// The connection graph for the function. This is what clients of the
    /// analysis will see.
    /// On invalidation, this graph is invalidated and recomputed on demand.
    ConnectionGraph Graph;

    /// The summary graph for the function. It is used when computing the
    /// connection graph of caller functions.
    /// This graph is _not_ be invalidated on invalidation. It is only updated
    /// when explicitly calling recompute().
    ConnectionGraph SummaryGraph;

    /// If true, at least one of the callee graphs has changed. We have to merge
    /// them again.
    bool NeedUpdateSummaryGraph = true;

    /// Clears the analysis data on invalidation.
    void clear() {
      Graph.clear();
      SummaryGraph.clear();
    }
  };

  typedef BottomUpFunctionOrder<FunctionInfo> FunctionOrder;

  enum {
    /// The maximum call-graph recursion depth for recomputing the analysis.
    /// This is a relatively small number to reduce compile time in case of
    /// large cycles in the call-graph.
    /// In case of no cycles, we should not hit this limit at all because the
    /// pass manager processes functions in bottom-up order.
    MaxRecursionDepth = 3,

    /// A limit for the number of call-graph iterations in recompute().
    MaxGraphMerges = 4
  };

  /// The connection graphs for all functions (does not include external
  /// functions).
  llvm::DenseMap<SILFunction *, FunctionInfo *> Function2Info;
  
  /// The allocator for the connection graphs in Function2ConGraph.
  llvm::SpecificBumpPtrAllocator<FunctionInfo> Allocator;

  /// Cache for isPointer().
  llvm::DenseMap<SILType, bool> isPointerCache;

  SILModule *M;

  /// The Array<Element> type of the stdlib.
  NominalTypeDecl *ArrayType;

  /// Callee analysis, used for determining the callees at call sites.
  BasicCalleeAnalysis *BCA;

  /// Returns true if \p V may encapsulate a "pointer" value.
  /// See EscapeAnalysis::NodeType::Value.
  bool isPointer(ValueBase *V) const;

  /// If EscapeAnalysis should consider the given value to be a derived address
  /// or pointer based on one of its address or pointer operands, then return
  /// that operand value. Otherwise, return an invalid value.
  SILValue getPointerBase(SILValue value) const;

  /// Recursively find the given value's pointer base. If the value cannot be
  /// represented in EscapeAnalysis as one of its operands, then return the same
  /// value.
  SILValue getPointerRoot(SILValue value) const;

  /// If \p pointer is a pointer, set it to global escaping.
  void setEscapesGlobal(ConnectionGraph *ConGraph, ValueBase *pointer) {
    if (CGNode *Node = ConGraph->getNode(pointer))
      ConGraph->setEscapesGlobal(Node);
  }

  /// Gets or creates FunctionEffects for \p F.
  FunctionInfo *getFunctionInfo(SILFunction *F) {
    FunctionInfo *&FInfo = Function2Info[F];
    if (!FInfo)
      FInfo = new (Allocator.Allocate()) FunctionInfo(F, this);
    return FInfo;
  }

  /// Build a connection graph for reach callee from the callee list.
  bool buildConnectionGraphForCallees(SILInstruction *Caller,
                                      CalleeList Callees,
                                      FunctionInfo *FInfo,
                                      FunctionOrder &BottomUpOrder,
                                      int RecursionDepth);

  /// Build a connection graph for the destructor invoked for a provided
  /// SILValue.
  bool buildConnectionGraphForDestructor(SILValue V,
                                         SILInstruction *Caller,
                                         FunctionInfo *FInfo,
                                         FunctionOrder &BottomUpOrder,
                                         int RecursionDepth);

  /// Builds the connection graph for a function, including called functions.
  /// Visited callees are added to \p BottomUpOrder until \p RecursionDepth
  /// reaches MaxRecursionDepth.
  void buildConnectionGraph(FunctionInfo *FInfo, FunctionOrder &BottomUpOrder,
                            int RecursionDepth);

  /// Updates the graph by analyzing instruction \p I.
  /// Visited callees are added to \p BottomUpOrder until \p RecursionDepth
  /// reaches MaxRecursionDepth.
  void analyzeInstruction(SILInstruction *I, FunctionInfo *FInfo,
                          FunctionOrder &BottomUpOrder,
                          int RecursionDepth);

  /// Updates the graph by analyzing instruction \p SI, which may be a
  /// select_enum, select_enum_addr or select_value.
  template<class SelectInst>
  void analyzeSelectInst(SelectInst *SI, ConnectionGraph *ConGraph);

  /// Returns true if a release of \p V is known to not capture its content.
  bool deinitIsKnownToNotCapture(SILValue V);

  /// Sets all operands and results of \p I as global escaping.
  void setAllEscaping(SILInstruction *I, ConnectionGraph *ConGraph);

  /// Recomputes the connection graph for the function \p Initial and
  /// all called functions, up to a recursion depth of MaxRecursionDepth.
  void recompute(FunctionInfo *Initial);

  /// Merges the graph of a callee function into the graph of
  /// a caller function, whereas \p FAS is the call-site.
  bool mergeCalleeGraph(SILInstruction *FAS,
                        ConnectionGraph *CallerGraph,
                        ConnectionGraph *CalleeGraph);

  /// Merge the \p Graph into \p SummaryGraph.
  bool mergeSummaryGraph(ConnectionGraph *SummaryGraph,
                         ConnectionGraph *Graph);

  /// Returns true if the value \p V can escape to the \p UsePoint, where
  /// \p UsePoint is either a release-instruction or a function call.
  bool canEscapeToUsePoint(SILValue V, SILNode *UsePoint,
                           ConnectionGraph *ConGraph);

  friend struct ::CGForDotView;

public:
  EscapeAnalysis(SILModule *M);

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::Escape;
  }

  virtual void initialize(SILPassManager *PM) override;

  /// Gets the connection graph for \a F.
  ConnectionGraph *getConnectionGraph(SILFunction *F) {
    FunctionInfo *FInfo = getFunctionInfo(F);
    if (!FInfo->isValid())
      recompute(FInfo);
    return &FInfo->Graph;
  }

  /// Returns true if the value \p V can escape to the function call \p FAS.
  /// This means that the called function may access the value \p V.
  /// If \p V has reference semantics, this function returns false if only the
  /// address of a contained property escapes, but not the object itself.
  bool canEscapeTo(SILValue V, FullApplySite FAS);

  /// Returns true if the value \p V can escape to the release-instruction \p
  /// RI. This means that \p RI may release \p V or any called destructor may
  /// access (or release) \p V.
  /// Note that if \p RI is a retain-instruction always false is returned.
  bool canEscapeTo(SILValue V, RefCountingInst *RI);

  /// Returns true if the value \p V can escape to any other pointer \p To.
  /// This means that either \p To is the same as \p V or contains a reference
  /// to \p V.
  bool canEscapeToValue(SILValue V, SILValue To);

  /// Returns true if the parameter with index \p ParamIdx can escape in the
  /// called function of apply site \p FAS.
  /// If it is an indirect parameter and \p checkContentOfIndirectParam is true
  /// then the escape status is not checked for the address itself but for the
  /// referenced pointer (if the referenced type is a pointer).
  bool canParameterEscape(FullApplySite FAS, int ParamIdx,
                          bool checkContentOfIndirectParam);

  /// Returns true if the pointers \p V1 and \p V2 can possibly point to the
  /// same memory.
  /// If at least one of the pointers refers to a local object and the
  /// connection-graph-nodes of both pointers do not point to the same content
  /// node, the pointers do not alias.
  bool canPointToSameMemory(SILValue V1, SILValue V2);

  /// Invalidate all information in this analysis.
  virtual void invalidate() override;

  /// Invalidate all of the information for a specific function.
  virtual void invalidate(SILFunction *F, InvalidationKind K) override;

  /// Notify the analysis about a newly created function.
  virtual void notifyAddedOrModifiedFunction(SILFunction *F) override {}

  /// Notify the analysis about a function which will be deleted from the
  /// module.
  virtual void notifyWillDeleteFunction(SILFunction *F) override {
    invalidate(F, InvalidationKind::Nothing);
  }

  /// Notify the analysis about changed witness or vtables.
  virtual void invalidateFunctionTables() override { }

  virtual void handleDeleteNotification(SILNode *N) override;

  virtual bool needsNotifications() override { return true; }

  virtual void verify() const override {
#ifndef NDEBUG
    for (auto Iter : Function2Info) {
      FunctionInfo *FInfo = Iter.second;
      FInfo->Graph.verify();
      FInfo->SummaryGraph.verify();
    }
#endif
  }

  virtual void verify(SILFunction *F) const override {
#ifndef NDEBUG
    if (FunctionInfo *FInfo = Function2Info.lookup(F)) {
      FInfo->Graph.verify();
      FInfo->SummaryGraph.verify();
    }
#endif
  }

};

} // end namespace swift

#endif
