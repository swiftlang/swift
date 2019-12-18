//===--- EscapeAnalysis.cpp - SIL Escape Analysis -------------------------===//
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

#define DEBUG_TYPE "sil-escape"
#include "swift/SILOptimizer/Analysis/EscapeAnalysis.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using CGNode = EscapeAnalysis::CGNode;

static llvm::cl::opt<bool> EnableInternalVerify(
    "escapes-internal-verify",
    llvm::cl::desc("Enable internal verification of escape analysis"),
    llvm::cl::init(false));

// Returns true if \p Ty recursively contains a reference.  If \p mustBeRef is
// true, only return true if the type is guaranteed to hold a reference. If \p
// mustBeRef is false, only return false if the type is guaranteed not to hold a
// reference.
//
// If \p Ty is itself an address, return false.
static bool findRecursiveRefType(SILType Ty, const SILFunction &F,
                                 bool mustBeRef) {
  if (mustBeRef) {
    // An address *may* be converted into a reference via something like
    // raw_pointer_to_ref. However, addresses don't normally refer to the head
    // of a reference counted object.
    //
    // The check for trivial types catches types that have AST "reference
    // semantics", but are determined by type lowering to be trivial, such as
    // noescape function types.
    if (Ty.isAddress() || Ty.isTrivial(F))
      return false;
  }

  if (!mustBeRef) {
    // Opaque types may contain a reference. Speculatively track them too.
    //
    // 1. It may be possible to optimize opaque values based on known mutation
    // points.
    //
    // 2. A specialized function may call a generic function passing a concrete
    // reference type via incomplete specialization.
    //
    // 3. A generic function may call a specialized function taking a concrete
    // reference type via devirtualization.
    if (Ty.isAddressOnly(F))
      return true;

    if (Ty.getASTType() == F.getModule().getASTContext().TheRawPointerType)
      return true;
  }

  if (Ty.hasReferenceSemantics())
    return true;

  auto &Mod = F.getModule();

  if (auto *Str = Ty.getStructOrBoundGenericStruct()) {
    for (auto *Field : Str->getStoredProperties()) {
      if (findRecursiveRefType(
              Ty.getFieldType(Field, Mod, F.getTypeExpansionContext()), F,
              mustBeRef))
        return true;
    }
    return false;
  }
  if (auto TT = Ty.getAs<TupleType>()) {
    for (unsigned i = 0, e = TT->getNumElements(); i != e; ++i) {
      if (findRecursiveRefType(Ty.getTupleElementType(i), F, mustBeRef))
        return true;
    }
    return false;
  }
  if (auto En = Ty.getEnumOrBoundGenericEnum()) {
    for (auto *ElemDecl : En->getAllElements()) {
      if (ElemDecl->hasAssociatedValues() &&
          findRecursiveRefType(
              Ty.getEnumElementType(ElemDecl, Mod, F.getTypeExpansionContext()),
              F, mustBeRef))
        return true;
    }
    return false;
  }
  // FIXME: without a covered switch, this is not robust for mayContainReference
  // in the event that new reference-holding AST types are invented.
  return false;
}

// Returns true if the type \p Ty is a reference or may transitively contain
// a reference. If \p Ty is itself an address, return false.
//
// An address may contain a reference because addresses can be cast into
// reference types.
static bool mayContainReference(SILType Ty, const SILFunction &F) {
  if (Ty.isAddress())
    return true;
  return findRecursiveRefType(Ty, F, false);
}

// Returns true if the type \p Ty must be a reference or must transitively
// contain a reference. If \p Ty is itself an address, return false.
static bool mustContainReference(SILType Ty, const SILFunction &F) {
  return findRecursiveRefType(Ty, F, true);
}

bool EscapeAnalysis::isPointer(ValueBase *V) const {
  auto *F = V->getFunction();

  // The function can be null, e.g. if V is an undef.
  if (!F)
    return false;

  SILType Ty = V->getType();
  auto Iter = isPointerCache.find(Ty);
  if (Iter != isPointerCache.end())
    return Iter->second;

  bool IP = mayContainReference(Ty, *F);
  const_cast<EscapeAnalysis *>(this)->isPointerCache[Ty] = IP;
  return IP;
}

static bool isExtractOfArrayUninitializedPointer(TupleExtractInst *TEI) {
  if (TEI->getFieldNo() == 1) {
    if (auto apply = dyn_cast<ApplyInst>(TEI->getOperand()))
      if (ArraySemanticsCall(apply, "array.uninitialized", false))
        return true;
  }
  return false;
}

// If EscapeAnalysis should consider the given value to be a derived address or
// pointer based on one of its address or pointer operands, then return that
// operand value. Otherwise, return an invalid value.
SILValue EscapeAnalysis::getPointerBase(SILValue value) const {
  switch (value->getKind()) {
  case ValueKind::IndexAddrInst:
  case ValueKind::IndexRawPointerInst:
  case ValueKind::StructElementAddrInst:
  case ValueKind::StructExtractInst:
  case ValueKind::TupleElementAddrInst:
  case ValueKind::UncheckedTakeEnumDataAddrInst:
  case ValueKind::UncheckedEnumDataInst:
  case ValueKind::MarkDependenceInst:
  case ValueKind::PointerToAddressInst:
  case ValueKind::AddressToPointerInst:
  case ValueKind::InitEnumDataAddrInst:
  case ValueKind::UncheckedRefCastInst:
  case ValueKind::ConvertFunctionInst:
  case ValueKind::UpcastInst:
  case ValueKind::InitExistentialRefInst:
  case ValueKind::OpenExistentialRefInst:
  case ValueKind::RawPointerToRefInst:
  case ValueKind::RefToRawPointerInst:
  case ValueKind::RefToBridgeObjectInst:
  case ValueKind::BridgeObjectToRefInst:
  case ValueKind::UncheckedAddrCastInst:
  case ValueKind::UnconditionalCheckedCastInst:
    // DO NOT use LOADABLE_REF_STORAGE because unchecked references don't have
    // retain/release instructions that trigger the 'default' case.
#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)            \
  case ValueKind::RefTo##Name##Inst:                                           \
  case ValueKind::Name##ToRefInst:
#include "swift/AST/ReferenceStorage.def"
    return cast<SingleValueInstruction>(value)->getOperand(0);

  case ValueKind::TupleExtractInst: {
    auto *TEI = cast<TupleExtractInst>(value);
    // Special handling for extracting the pointer-result from an
    // array construction. We handle this like a ref_element_addr
    // rather than a projection. See the handling of tuple_extract
    // in analyzeInstruction().
    if (isExtractOfArrayUninitializedPointer(TEI))
      return SILValue();
    return TEI->getOperand();
  }
  case ValueKind::StructInst:
  case ValueKind::TupleInst:
  case ValueKind::EnumInst: {
    // Allow a single-operand aggregate to share its operand's node.
    auto *SVI = cast<SingleValueInstruction>(value);
    SILValue pointerOperand;
    for (SILValue opV : SVI->getOperandValues()) {
      if (!isPointer(opV))
        continue;

      if (pointerOperand)
        return SILValue();

      pointerOperand = opV;
    }
    return pointerOperand;
  }
  default:
    return SILValue();
  }
}

// Recursively find the given value's pointer base. If the value cannot be
// represented in EscapeAnalysis as one of its operands, then return the same
// value.
SILValue EscapeAnalysis::getPointerRoot(SILValue value) const {
  while (true) {
    if (SILValue v2 = getPointerBase(value))
      value = v2;
    else
      break;
  }
  return value;
}

static bool isNonWritableMemoryAddress(SILNode *V) {
  switch (V->getKind()) {
  case SILNodeKind::FunctionRefInst:
  case SILNodeKind::DynamicFunctionRefInst:
  case SILNodeKind::PreviousDynamicFunctionRefInst:
  case SILNodeKind::WitnessMethodInst:
  case SILNodeKind::ClassMethodInst:
  case SILNodeKind::SuperMethodInst:
  case SILNodeKind::ObjCMethodInst:
  case SILNodeKind::ObjCSuperMethodInst:
  case SILNodeKind::StringLiteralInst:
  case SILNodeKind::ThinToThickFunctionInst:
  case SILNodeKind::ThinFunctionToPointerInst:
  case SILNodeKind::PointerToThinFunctionInst:
    // These instructions return pointers to memory which can't be a
    // destination of a store.
    return true;
  default:
    return false;
  }
}

// Implement an intrusive worklist of CGNode. Only one may be in use at a time.
struct EscapeAnalysis::CGNodeWorklist {
  llvm::SmallVector<CGNode *, 8> nodeVector;
  EscapeAnalysis::ConnectionGraph *conGraph;

  CGNodeWorklist(const CGNodeWorklist &) = delete;

  CGNodeWorklist(EscapeAnalysis::ConnectionGraph *conGraph)
      : conGraph(conGraph) {
    conGraph->activeWorklist = this;
  }
  ~CGNodeWorklist() { reset(); }
  // Clear the intrusive isInWorkList flags, but leave the nodeVector vector in
  // place for subsequent iteration.
  void reset() {
    ConnectionGraph::clearWorkListFlags(nodeVector);
    conGraph->activeWorklist = nullptr;
  }
  unsigned size() const { return nodeVector.size(); }

  bool empty() const { return nodeVector.empty(); }

  bool contains(CGNode *node) const {
    assert(conGraph->activeWorklist == this);
    return node->isInWorkList;
  }
  CGNode *operator[](unsigned idx) const {
    assert(idx < size());
    return nodeVector[idx];
  }
  bool tryPush(CGNode *node) {
    assert(conGraph->activeWorklist == this);
    if (node->isInWorkList)
      return false;

    node->isInWorkList = true;
    nodeVector.push_back(node);
    return true;
  }
  void push(CGNode *node) {
    assert(conGraph->activeWorklist == this);
    assert(!node->isInWorkList);
    node->isInWorkList = true;
    nodeVector.push_back(node);
  }
};

/// Mapping from nodes in a callee-graph to nodes in a caller-graph.
class EscapeAnalysis::CGNodeMap {
  /// The map itself.
  llvm::DenseMap<CGNode *, CGNode *> Map;

  /// The list of source nodes (= keys in Map), which is used as a work-list.
  CGNodeWorklist MappedNodes;

public:
  CGNodeMap(ConnectionGraph *conGraph) : MappedNodes(conGraph) {}
  CGNodeMap(const CGNodeMap &) = delete;

  /// Adds a mapping and pushes the \p From node into the work-list
  /// MappedNodes.
  void add(CGNode *From, CGNode *To) {
    assert(From && To && !From->isMerged && !To->isMerged);
    Map[From] = To;
    MappedNodes.tryPush(From);
  }
  /// Looks up a node in the mapping.
  CGNode *get(CGNode *From) const {
    auto Iter = Map.find(From);
    if (Iter == Map.end())
      return nullptr;

    return Iter->second->getMergeTarget();
  }
  CGNodeWorklist &getMappedNodes() { return MappedNodes; }
};

//===----------------------------------------------------------------------===//
//                        ConnectionGraph Implementation
//===----------------------------------------------------------------------===//

std::pair<const CGNode *, unsigned> EscapeAnalysis::CGNode::getRepNode(
    SmallPtrSetImpl<const CGNode *> &visited) const {
  if (!isContent() || mappedValue)
    return {this, 0};

  for (Predecessor pred : Preds) {
    if (!pred.is(EdgeType::PointsTo))
      continue;
    if (!visited.insert(pred.getPredNode()).second)
      continue;
    auto repNodeAndDepth = pred.getPredNode()->getRepNode(visited);
    if (repNodeAndDepth.first)
      return {repNodeAndDepth.first, repNodeAndDepth.second + 1};
    // If a representative node was not found on this pointsTo node, recursion
    // must have hit a cycle. Try the next pointsTo edge.
  }
  return {nullptr, 0};
}

EscapeAnalysis::CGNode::RepValue EscapeAnalysis::CGNode::getRepValue() const {
  // We don't use CGNodeWorklist because CGNode::dump() should be callable
  // anywhere, even while another worklist is active, and getRepValue() itself
  // is not on any critical path.
  SmallPtrSet<const CGNode *, 4> visited({this});
  const CGNode *repNode;
  unsigned depth;
  std::tie(repNode, depth) = getRepNode(visited);
  return {{repNode ? SILValue(repNode->mappedValue) : SILValue(),
           repNode && repNode->Type == EscapeAnalysis::NodeType::Return},
          depth};
}

void EscapeAnalysis::CGNode::mergeProperties(CGNode *fromNode) {
  // TODO: Optimistically merge hasRC. 'this' node can only be merged with
  // `fromNode` if their pointer values are compatible. If `fromNode->hasRC` is
  // true, then it is guaranteed to represent the head of a heap object. Thus,
  // it can only be merged with 'this' when the pointer values that access
  // 'this' are also references.
  //
  // For now, this is pessimistic until we understand performance implications.
  hasRC &= fromNode->hasRC;
}

template <typename Visitor>
bool EscapeAnalysis::CGNode::visitSuccessors(Visitor &&visitor) const {
  if (CGNode *pointsToSucc = getPointsToEdge()) {
    // Visit pointsTo, even if pointsTo == this.
    if (!visitor(pointsToSucc))
      return false;
  }
  for (CGNode *def : defersTo) {
    if (!visitor(def))
      return false;
  }
  return true;
}

template <typename Visitor>
bool EscapeAnalysis::CGNode::visitDefers(Visitor &&visitor) const {
  for (Predecessor pred : Preds) {
    if (!pred.is(EdgeType::Defer))
      continue;
    if (!visitor(pred.getPredNode(), false))
      return false;
  }
  for (auto *deferred : defersTo) {
    if (!visitor(deferred, true))
      return false;
  }
  return true;
}

void EscapeAnalysis::ConnectionGraph::clear() {
  Values2Nodes.clear();
  Nodes.clear();
  ReturnNode = nullptr;
  UsePoints.clear();
  UsePointTable.clear();
  NodeAllocator.DestroyAll();
  assert(ToMerge.empty());
}

EscapeAnalysis::CGNode *
EscapeAnalysis::ConnectionGraph::getNode(ValueBase *V, bool createIfNeeded) {
  if (isa<FunctionRefInst>(V) || isa<DynamicFunctionRefInst>(V) ||
      isa<PreviousDynamicFunctionRefInst>(V))
    return nullptr;

  // In the case of a struct or tuple extract, 'V' may not be a pointer
  // even if it's pointer root is a pointer. Bail first because we only expect
  // graph nodes for pointer values.
  if (!EA->isPointer(V))
    return nullptr;

  // Look past address projections, pointer casts, and the like within the same
  // object. Does not look past a dereference such as ref_element_addr, or
  // project_box.
  V = EA->getPointerRoot(V);

  if (!createIfNeeded)
    return lookupNode(V);
  
  CGNode * &Node = Values2Nodes[V];
  if (!Node) {
    if (isa<SILFunctionArgument>(V)) {
      Node = allocNode(V, NodeType::Argument);
      if (!isSummaryGraph)
        Node->mergeEscapeState(EscapeState::Arguments);
    } else {
      Node = allocNode(V, NodeType::Value);
    }
  }
  return Node->getMergeTarget();
}

CGNode *EscapeAnalysis::ConnectionGraph::defer(CGNode *From, CGNode *To,
                                               bool &Changed) {
  if (!From->canAddDeferred(To))
    return From;

  CGNode *FromPointsTo = From->pointsTo;
  CGNode *ToPointsTo = To->pointsTo;
  // If necessary, merge nodes while the graph is still in a valid state.
  if (FromPointsTo && ToPointsTo && FromPointsTo != ToPointsTo) {
    // We are adding an edge between two pointers which point to different
    // content nodes. This will require merging the content nodes (and maybe
    // other content nodes as well), because of the graph invariance 4).
    //
    // Once the pointee's are merged, the defer edge can be added without
    // creating an inconsistency.
    scheduleToMerge(FromPointsTo, ToPointsTo);
    mergeAllScheduledNodes();
    Changed = true;
  }
  // 'From' and 'To' may have been merged, so addDeferred may no longer succeed.
  if (From->getMergeTarget()->addDeferred(To->getMergeTarget()))
    Changed = true;

  // If pointsTo on either side of the defer was uninitialized, initialize that
  // side of the defer web. Do this after adding the new edge to avoid creating
  // useless pointsTo edges.
  if (!FromPointsTo && ToPointsTo)
    initializePointsTo(From, ToPointsTo);
  else if (FromPointsTo && !ToPointsTo)
    initializePointsTo(To, FromPointsTo);

  return From->getMergeTarget();
}

// Precondition: The pointsTo fields of all nodes in initializeNode's defer web
// are either uninitialized or already initialized to newPointsTo.
void EscapeAnalysis::ConnectionGraph::initializePointsTo(CGNode *initialNode,
                                                         CGNode *newPointsTo,
                                                         bool createEdge) {
  // Track nodes that require pointsTo edges.
  llvm::SmallVector<CGNode *, 4> pointsToEdgeNodes;
  if (createEdge)
    pointsToEdgeNodes.push_back(initialNode);

  // Step 1: Visit each node that reaches or is reachable via defer edges until
  // reaching a node with the newPointsTo or with a proper pointsTo edge.

  // A worklist to gather updated nodes in the defer web.
  CGNodeWorklist updatedNodes(this);
  unsigned updateCount = 0;

  auto visitDeferTarget = [&](CGNode *node, bool /*isSuccessor*/) {
    if (updatedNodes.contains(node))
      return true;

    if (node->pointsTo) {
      assert(node->pointsTo == newPointsTo);
      // Since this node already had a pointsTo, it must reach a pointsTo
      // edge. Stop traversing the defer-web here--this is complete becaused
      // nodes are initialized one at a time, each time a new defer edge is
      // created. If this were not complete, then the backward traversal below
      // in Step 2 could reach uninitialized nodes not seen here in Step 1.
      pointsToEdgeNodes.push_back(node);
      return true;
    }
    ++updateCount;
    if (node->defersTo.empty()) {
      // If this node is the end of a defer-edge path with no pointsTo
      // edge. Create a "fake" pointsTo edge to maintain the graph invariant
      // (this changes the structure of the graph but adding this edge has no
      // effect on the process of merging nodes or creating new defer edges).
      pointsToEdgeNodes.push_back(node);
    }
    updatedNodes.push(node);
    return true;
  };
  // Seed updatedNodes with initialNode.
  visitDeferTarget(initialNode, true);
  // updatedNodes may grow during this loop.
  for (unsigned idx = 0; idx < updatedNodes.size(); ++idx)
    updatedNodes[idx]->visitDefers(visitDeferTarget);
  // Reset this worklist so others can be used, but updateNode.nodeVector still
  // holds all the nodes found by step 1.
  updatedNodes.reset();

  // Step 2: Update pointsTo fields by propagating backward from nodes that
  // already have a pointsTo edge.
  do {
    while (!pointsToEdgeNodes.empty()) {
      CGNode *edgeNode = pointsToEdgeNodes.pop_back_val();
      if (!edgeNode->pointsTo) {
        // This node is either (1) a leaf node in the defer web (identified in
        // step 1) or (2) an arbitrary node in a defer-cycle (identified in a
        // previous iteration of the outer loop).
        edgeNode->setPointsToEdge(newPointsTo);
        newPointsTo->mergeUsePoints(edgeNode);
        assert(updateCount--);
      }
      // If edgeNode is already set to newPointsTo, it either was already
      // up-to-date before calling initializePointsTo, or it was visited during
      // a previous iteration of the backward traversal below. Rather than
      // distinguish these cases, always retry backward traversal--it just won't
      // revisit any edges in the later case.
      backwardTraverse(edgeNode, [&](Predecessor pred) {
        if (!pred.is(EdgeType::Defer))
          return Traversal::Backtrack;

        CGNode *predNode = pred.getPredNode();
        if (predNode->pointsTo) {
          assert(predNode->pointsTo->getMergeTarget() == newPointsTo);
          return Traversal::Backtrack;
        }
        predNode->pointsTo = newPointsTo;
        newPointsTo->mergeUsePoints(predNode);
        assert(updateCount--);
        return Traversal::Follow;
      });
    }
    // For all nodes visited in step 1, pick a single node that was not
    // backward-reachable from a pointsTo edge, create an edge for it and
    // restart traversal. This only happens when step 1 fails to find leaves in
    // the defer web because of defer edge cycles.
    while (!updatedNodes.empty()) {
      CGNode *node = updatedNodes.nodeVector.pop_back_val();
      if (!node->pointsTo) {
        pointsToEdgeNodes.push_back(node);
        break;
      }
    }
    // This outer loop is exceedingly unlikely to execute more than twice.
  } while (!pointsToEdgeNodes.empty());
  assert(updateCount == 0);
}

void EscapeAnalysis::ConnectionGraph::mergeAllScheduledNodes() {
  // Each merge step is self contained and verifiable, with one exception. When
  // merging a node that points to itself with a node points to another node,
  // multiple merge steps are necessary to make the defer web consistent.
  // Example:
  //   NodeA pointsTo-> From
  //   From  defersTo-> NodeA (an indirect self-cycle)
  //   To    pointsTo-> NodeB
  // Merged:
  //   NodeA pointsTo-> To
  //   To    defersTo-> NodeA (To *should* pointTo itself)
  //   To    pointsTo-> NodeB (but still has a pointsTo edge to NodeB)
  while (!ToMerge.empty()) {
    if (EnableInternalVerify)
      verify(/*allowMerge=*/true);

    CGNode *From = ToMerge.pop_back_val();
    CGNode *To = From->getMergeTarget();
    assert(To != From && "Node scheduled to merge but no merge target set");
    assert(!From->isMerged && "Merge source is already merged");
    assert(From->Type == NodeType::Content && "Can only merge content nodes");
    assert(To->Type == NodeType::Content && "Can only merge content nodes");

    // Redirect the incoming pointsTo edge and unlink the defer predecessors.
    //
    // Don't redirect the defer-edges because it may trigger mergePointsTo() or
    // initializePointsTo(). By ensuring that 'From' is unreachable first, the
    // graph appears consistent during those operations.
    for (Predecessor Pred : From->Preds) {
      CGNode *PredNode = Pred.getPredNode();
      if (Pred.is(EdgeType::PointsTo)) {
        assert(PredNode->getPointsToEdge() == From
               && "Incoming pointsTo edge not set in predecessor");
        if (PredNode != From)
          PredNode->setPointsToEdge(To);
      } else {
        assert(PredNode != From);
        auto Iter = PredNode->findDeferred(From);
        assert(Iter != PredNode->defersTo.end()
               && "Incoming defer-edge not found in predecessor's defer list");
        PredNode->defersTo.erase(Iter);
      }
    }
    // Unlink the outgoing defer edges.
    for (CGNode *Defers : From->defersTo) {
      assert(Defers != From && "defer edge may not form a self-cycle");
      Defers->removeFromPreds(Predecessor(From, EdgeType::Defer));
    }
    // Handle self-cycles on From by creating a self-cycle at To.
    auto redirectPointsTo = [&](CGNode *pointsTo) {
      return (pointsTo == From) ? To : pointsTo;
    };
    // Redirect the outgoing From -> pointsTo edge.
    if (From->pointsToIsEdge) {
      From->pointsTo->removeFromPreds(Predecessor(From, EdgeType::PointsTo));
      if (To->pointsToIsEdge) {
        // If 'To' had a pointsTo edge to 'From', then it was redirected above.
        // Otherwise FromPT and ToPT will be merged below; nothing to do here.
        assert(To->pointsTo != From);
      } else {
        // If 'To' has no pointsTo at all, initialize its defer web.
        if (!To->pointsTo) {
          initializePointsToEdge(To, redirectPointsTo(From->pointsTo));
        } else {
          // Upgrade 'To's pointsTo to an edge to preserve the fact that 'From'
          // had a pointsTo edge.
          To->pointsToIsEdge = true;
          To->pointsTo = redirectPointsTo(To->pointsTo);
          To->pointsTo->Preds.push_back(Predecessor(To, EdgeType::PointsTo));
        }
      }
    }
    // Merge 'From->pointsTo' and 'To->pointsTo' if needed, regardless of
    // whether either is a proper edge. Merging may be needed because other
    // nodes may have points-to edges to From->PointsTo that won't be visited
    // when updating 'From's defer web.
    //
    // If To doesn't already have a points-to, it will simply be initialized
    // when updating the merged defer web below.
    if (CGNode *toPT = To->pointsTo) {
      // If 'To' already points to 'From', then it will already point to 'From's
      // pointTo after merging. An additional merge would be too conservative.
      if (From->pointsTo && toPT != From)
        scheduleToMerge(redirectPointsTo(From->pointsTo), toPT);
    }
    // Redirect adjacent defer edges, and immediately update all points-to
    // fields in the defer web.
    //
    // Calling initializePointsTo may create new pointsTo edges from nodes in
    // the defer-web. It is unsafe to mutate or query the graph in its currently
    // inconsistent state. However, this particular case is safe because:
    // - The graph is only locally inconsistent w.r.t. nodes still connected to
    // 'From' via defer edges.
    // - 'From' itself is no longer reachable via graph edges (it may only be
    // referenced in points-to fields which haven't all been updated).
    // - Calling initializePointsTo on one from 'From's deferred nodes implies
    // that all nodes in 'From's defer web had a null pointsTo.
    // - 'To's defer web remains consistent each time a new defer edge is
    // added below. Any of 'To's existing deferred nodes either still need to
    // be initialized or have already been initialized to the same pointsTo.
    //
    // Start by updating 'To's own pointsTo field.
    if (To->pointsTo == From)
      mergePointsTo(To, To);

    auto mergeDeferPointsTo = [&](CGNode *deferred, bool isSuccessor) {
      assert(From != deferred && "defer edge may not form a self-cycle");
      if (To == deferred)
        return true;

      // In case 'deferred' points to 'From', update its pointsTo before
      // exposing it to 'To's defer web.
      if (deferred->pointsTo == From)
        mergePointsTo(deferred, To);

      if (isSuccessor)
        To->addDeferred(deferred);
      else
        deferred->addDeferred(To);

      if (deferred->pointsTo && To->pointsTo)
        mergePointsTo(deferred, To->pointsTo);
      else if (deferred->pointsTo)
        initializePointsTo(To, deferred->pointsTo);
      else if (To->pointsTo)
        initializePointsTo(deferred, To->pointsTo);

      return true;
    };
    // Redirect the adjacent defer edges.
    From->visitDefers(mergeDeferPointsTo);

    // Update the web of nodes that originally pointed to 'From' via 'From's old
    // pointsTo predecessors (which are now attached to 'To').
    for (unsigned PredIdx = 0; PredIdx < To->Preds.size(); ++PredIdx) {
      auto predEdge = To->Preds[PredIdx];
      if (!predEdge.is(EdgeType::PointsTo))
        continue;
      predEdge.getPredNode()->visitDefers(
          [&](CGNode *deferred, bool /*isSucc*/) {
            mergePointsTo(deferred, To);
            return true;
          });
    }
    To->mergeEscapeState(From->State);

    // Cleanup the merged node.
    From->isMerged = true;

    if (From->mappedValue) {
      if (To->mappedValue)
        Values2Nodes.erase(From->mappedValue);
      else {
        To->mappedValue = From->mappedValue;
        Values2Nodes[To->mappedValue] = To;
      }
      From->mappedValue = nullptr;
    }
    From->Preds.clear();
    From->defersTo.clear();
    From->pointsTo = nullptr;
  }
  if (EnableInternalVerify)
    verify(/*allowMerge=*/true);
}

// As a result of a merge, update the pointsTo field of initialNode and
// everything in its defer web to newPointsTo.
//
// This may modify the graph by redirecting a pointsTo edges.
void EscapeAnalysis::ConnectionGraph::mergePointsTo(CGNode *initialNode,
                                                    CGNode *newPointsTo) {
  CGNode *oldPointsTo = initialNode->pointsTo;
  assert(oldPointsTo && "merging content should not initialize any pointsTo");
  if (oldPointsTo == newPointsTo)
    return;

  CGNodeWorklist updateNodes(this);
  auto updatePointsTo = [&](CGNode *node) {
    if (node->pointsTo == newPointsTo)
      return;
    // If the original graph was: 'node->From->To->newPointsTo' or
    // 'node->From->From', then node is already be updated to point to
    // 'To' and 'To' must be merged with newPointsTo. We must still update
    // pointsTo so that all nodes in the defer web have the same pointsTo.
    assert(node->pointsTo == oldPointsTo
           || node->pointsTo->getMergeTarget() == newPointsTo);
    if (node->pointsToIsEdge) {
      node->pointsTo->removeFromPreds(Predecessor(node, EdgeType::PointsTo));
      node->setPointsToEdge(newPointsTo);
    } else
      node->pointsTo = newPointsTo;
    updateNodes.push(node);
  };
  updatePointsTo(initialNode);

  // Visit each node that reaches or is reachable via defer edges until reaching
  // a node with the newPointsTo.
  auto visitDeferTarget = [&](CGNode *node, bool /*isSuccessor*/) {
    if (!updateNodes.contains(node))
      updatePointsTo(node);
    return true;
  };
  for (unsigned Idx = 0; Idx < updateNodes.size(); ++Idx)
    updateNodes[Idx]->visitDefers(visitDeferTarget);
}

void EscapeAnalysis::ConnectionGraph::propagateEscapeStates() {
  bool Changed = false;
  do {
    Changed = false;

    for (CGNode *Node : Nodes) {
      // Propagate the state to all successor nodes.
      if (Node->pointsTo) {
        Changed |= Node->pointsTo->mergeEscapeState(Node->State);
      }
      for (CGNode *Def : Node->defersTo) {
        Changed |= Def->mergeEscapeState(Node->State);
      }
    }
  } while (Changed);
}

void EscapeAnalysis::ConnectionGraph::computeUsePoints() {
#ifndef NDEBUG
  for (CGNode *Nd : Nodes)
    assert(Nd->UsePoints.empty() && "premature use point computation");
#endif
  // First scan the whole function and add relevant instructions as use-points.
  for (auto &BB : *F) {
    for (SILArgument *BBArg : BB.getArguments()) {
      /// In addition to releasing instructions (see below) we also add block
      /// arguments as use points. In case of loops, block arguments can
      /// "extend" the liferange of a reference in upward direction.
      if (CGNode *ArgNode = lookupNode(BBArg)) {
        addUsePoint(ArgNode, BBArg);
      }
    }

    for (auto &I : BB) {
      switch (I.getKind()) {
#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
        case SILInstructionKind::Name##ReleaseInst:
#include "swift/AST/ReferenceStorage.def"
        case SILInstructionKind::StrongReleaseInst:
        case SILInstructionKind::ReleaseValueInst:
        case SILInstructionKind::ApplyInst:
        case SILInstructionKind::TryApplyInst: {
          /// Actually we only add instructions which may release a reference.
          /// We need the use points only for getting the end of a reference's
          /// liferange. And that must be a releasing instruction.
          int ValueIdx = -1;
          for (const Operand &Op : I.getAllOperands()) {
            ValueBase *OpV = Op.get();
            if (CGNode *OpNd = lookupNode(EA->getPointerRoot(OpV))) {
              if (ValueIdx < 0) {
                ValueIdx = addUsePoint(OpNd, &I);
              } else {
                OpNd->setUsePointBit(ValueIdx);
              }
            }
          }
          break;
        }
        default:
          break;
      }
    }
  }

  // Second, we propagate the use-point information through the graph.
  bool Changed = false;
  do {
    Changed = false;
    for (CGNode *Node : Nodes) {
      // Propagate the bits to all successor nodes.
      Node->visitSuccessors([&Changed, Node](CGNode *succ) {
        Changed |= succ->mergeUsePoints(Node);
        return true;
      });
    }
  } while (Changed);
}

CGNode *EscapeAnalysis::ConnectionGraph::createContentNode(CGNode *addrNode,
                                                           bool hasRC) {
  CGNode *newContent = allocNode(nullptr, NodeType::Content, hasRC);
  initializePointsToEdge(addrNode, newContent);
  assert(ToMerge.empty()
         && "Initially setting pointsTo should not require any node merges");
  return newContent;
}

// Create a content node for merging based on an address node in the destination
// graph and a content node in the source graph.
CGNode *
EscapeAnalysis::ConnectionGraph::createMergedContent(CGNode *destAddrNode,
                                                     CGNode *srcContent) {
  // destAddrNode may itself be a content node, so its value may be null. Since
  // we don't have the original pointer value, build a new content node based
  // on the source content.
  return createContentNode(destAddrNode, srcContent->hasRC);
}

// Get a node representing the field data within the given reference-counted
// node. The caller has already determined that rcNode represents the head of a
// heap object rather than field content or the address of a local variable or
// argument.
CGNode *EscapeAnalysis::ConnectionGraph::getFieldContent(CGNode *rcNode) {
  assert(rcNode->isContent());
  if (rcNode->pointsTo)
    return rcNode->pointsTo;

  return createContentNode(rcNode, /*hasRC=*/false);
}

bool EscapeAnalysis::ConnectionGraph::mergeFrom(ConnectionGraph *SourceGraph,
                                                CGNodeMap &Mapping) {
  // The main point of the merging algorithm is to map each content node in the
  // source graph to a content node in this (destination) graph. This may
  // require to create new nodes or to merge existing nodes in this graph.

  // First step: replicate the points-to edges and the content nodes of the
  // source graph in this graph.
  bool Changed = false;
  bool NodesMerged;
  do {
    NodesMerged = false;
    for (unsigned Idx = 0; Idx < Mapping.getMappedNodes().size(); ++Idx) {
      CGNode *SourceNd = Mapping.getMappedNodes()[Idx];
      CGNode *DestNd = Mapping.get(SourceNd);
      assert(DestNd);
      
      if (SourceNd->getEscapeState() >= EscapeState::Global) {
        // We don't need to merge the source subgraph of nodes which have the
        // global escaping state set.
        // Just set global escaping in the caller node and that's it.
        Changed |= DestNd->mergeEscapeState(EscapeState::Global);
        continue;
      }

      CGNode *SourcePT = SourceNd->pointsTo;
      if (!SourcePT)
        continue;

      CGNode *DestPT = DestNd->pointsTo;
      if (!DestPT) {
        DestPT = createMergedContent(DestNd, SourcePT);
        Changed = true;
      }
      CGNode *MappedDestPT = Mapping.get(SourcePT);
      if (!MappedDestPT) {
        // This is the first time the dest node is seen; just add the mapping.
        Mapping.add(SourcePT, DestPT);
        continue;
      }
      // We already found the destination node through another path.
      assert(Mapping.getMappedNodes().contains(SourcePT));
      if (DestPT == MappedDestPT)
        continue;

      // There are two content nodes in this graph which map to the same
      // content node in the source graph -> we have to merge them.
      scheduleToMerge(DestPT, MappedDestPT);
      mergeAllScheduledNodes();
      Changed = true;
      NodesMerged = true;
    }
  } while (NodesMerged);
  Mapping.getMappedNodes().reset(); // Make way for a different worklist.

  // Second step: add the source graph's defer edges to this graph.
  for (CGNode *SourceNd : Mapping.getMappedNodes().nodeVector) {
    CGNodeWorklist Worklist(SourceGraph);
    Worklist.push(SourceNd);
    CGNode *DestFrom = Mapping.get(SourceNd);
    assert(DestFrom && "node should have been merged to the graph");

    // Collect all nodes which are reachable from the SourceNd via a path
    // which only contains defer-edges.
    for (unsigned Idx = 0; Idx < Worklist.size(); ++Idx) {
      CGNode *SourceReachable = Worklist[Idx];
      CGNode *DestReachable = Mapping.get(SourceReachable);
      // Create the edge in this graph. Note: this may trigger merging of
      // content nodes.
      if (DestReachable) {
        DestFrom = defer(DestFrom, DestReachable, Changed);
      } else if (SourceReachable->getEscapeState() >= EscapeState::Global) {
        // If we don't have a mapped node in the destination graph we still have
        // to honor its escaping state. We do that simply by setting the source
        // node of the defer-edge to escaping.
        Changed |= DestFrom->mergeEscapeState(EscapeState::Global);
      }
      for (auto *Deferred : SourceReachable->defersTo)
        Worklist.tryPush(Deferred);
    }
  }
  return Changed;
}

/// Returns true if \p V is a use of \p Node, i.e. V may (indirectly)
/// somehow refer to the Node's value.
/// Use-points are only values which are relevant for lifeness computation,
/// e.g. release or apply instructions.
bool EscapeAnalysis::ConnectionGraph::isUsePoint(SILNode *UsePoint,
                                                 CGNode *Node) {
  assert(Node->getEscapeState() < EscapeState::Global &&
         "Use points are only valid for non-escaping nodes");
  UsePoint = UsePoint->getRepresentativeSILNodeInObject();
  auto Iter = UsePoints.find(UsePoint);
  if (Iter == UsePoints.end())
    return false;
  int Idx = Iter->second;
  if (Idx >= (int)Node->UsePoints.size())
    return false;
  return Node->UsePoints.test(Idx);
}

void EscapeAnalysis::ConnectionGraph::
getUsePoints(CGNode *Node, llvm::SmallVectorImpl<SILNode *> &UsePoints) {
  assert(Node->getEscapeState() < EscapeState::Global &&
         "Use points are only valid for non-escaping nodes");
  for (int Idx = Node->UsePoints.find_first(); Idx >= 0;
       Idx = Node->UsePoints.find_next(Idx)) {
    UsePoints.push_back(UsePointTable[Idx]);
  }
}

// Traverse backward from startNode and return true if \p visitor did not halt
// traversal..
//
// The graph may have cycles.
template <typename CGPredVisitor>
bool EscapeAnalysis::ConnectionGraph::backwardTraverse(
    CGNode *startNode, CGPredVisitor &&visitor) {
  CGNodeWorklist worklist(this);
  worklist.push(startNode);

  for (unsigned idx = 0; idx < worklist.size(); ++idx) {
    CGNode *reachingNode = worklist[idx];

    for (Predecessor pred : reachingNode->Preds) {
      switch (visitor(pred)) {
      case Traversal::Follow: {
        CGNode *predNode = pred.getPredNode();
        worklist.tryPush(predNode);
        break;
      }
      case Traversal::Backtrack:
        break;
      case Traversal::Halt:
        return false;
      }
    }
  }
  return true;
}

// Traverse forward from startNode, following defer edges and return true if \p
// visitor did not halt traversal.
//
// The graph may have cycles.
template <typename CGNodeVisitor>
bool EscapeAnalysis::ConnectionGraph::forwardTraverseDefer(
    CGNode *startNode, CGNodeVisitor &&visitor) {
  CGNodeWorklist worklist(this);
  worklist.push(startNode);

  for (unsigned idx = 0; idx < worklist.size(); ++idx) {
    CGNode *reachableNode = worklist[idx];

    for (CGNode *deferNode : reachableNode->defersTo) {
      switch (visitor(deferNode)) {
      case Traversal::Follow:
        worklist.tryPush(deferNode);
        break;
      case Traversal::Backtrack:
        break;
      case Traversal::Halt:
        return false;
      }
    }
  }
  return true;
}

bool EscapeAnalysis::ConnectionGraph::mayReach(CGNode *pointer,
                                               CGNode *pointee) {
  if (pointer == pointee)
    return true;

  // This query is successful when the traversal halts and returns false.
  return !backwardTraverse(pointee, [pointer](Predecessor pred) {
    if (pred.getPredNode() == pointer)
      return Traversal::Halt;
    return Traversal::Follow;
  });
}

void EscapeAnalysis::ConnectionGraph::removeFromGraph(ValueBase *V) {
  CGNode *node = Values2Nodes.lookup(V);
  if (!node)
    return;
  Values2Nodes.erase(V);
  if (node->mappedValue == V)
    node->mappedValue = nullptr;
}

//===----------------------------------------------------------------------===//
//                      Dumping, Viewing and Verification
//===----------------------------------------------------------------------===//

#ifndef NDEBUG

/// For the llvm's GraphWriter we copy the connection graph into CGForDotView.
/// This makes iterating over the edges easier.
struct CGForDotView {

  enum EdgeTypes {
    PointsTo,
    Deferred
  };

  struct Node {
    EscapeAnalysis::CGNode *OrigNode;
    CGForDotView *Graph;
    SmallVector<Node *, 8> Children;
    SmallVector<EdgeTypes, 8> ChildrenTypes;
  };

  CGForDotView(const EscapeAnalysis::ConnectionGraph *CG);
  
  std::string getNodeLabel(const Node *Node) const;
  
  std::string getNodeAttributes(const Node *Node) const;

  std::vector<Node> Nodes;

  SILFunction *F;

  const EscapeAnalysis::ConnectionGraph *OrigGraph;

  // The same IDs as the SILPrinter uses.
  llvm::DenseMap<const SILNode *, unsigned> InstToIDMap;

  typedef std::vector<Node>::iterator iterator;
  typedef SmallVectorImpl<Node *>::iterator child_iterator;
};

CGForDotView::CGForDotView(const EscapeAnalysis::ConnectionGraph *CG) :
    F(CG->F), OrigGraph(CG) {
  Nodes.resize(CG->Nodes.size());
  llvm::DenseMap<EscapeAnalysis::CGNode *, Node *> Orig2Node;
  int idx = 0;
  for (auto *OrigNode : CG->Nodes) {
    if (OrigNode->isMerged)
      continue;

    Orig2Node[OrigNode] = &Nodes[idx++];
  }
  Nodes.resize(idx);
  CG->F->numberValues(InstToIDMap);

  idx = 0;
  for (auto *OrigNode : CG->Nodes) {
    if (OrigNode->isMerged)
      continue;

    auto &Nd = Nodes[idx++];
    Nd.Graph = this;
    Nd.OrigNode = OrigNode;
    if (auto *PT = OrigNode->getPointsToEdge()) {
      Nd.Children.push_back(Orig2Node[PT]);
      Nd.ChildrenTypes.push_back(PointsTo);
    }
    for (auto *Def : OrigNode->defersTo) {
      Nd.Children.push_back(Orig2Node[Def]);
      Nd.ChildrenTypes.push_back(Deferred);
    }
  }
}

void CGNode::RepValue::print(
    llvm::raw_ostream &stream,
    const llvm::DenseMap<const SILNode *, unsigned> &instToIDMap) const {
  if (auto v = getValue())
    stream << '%' << instToIDMap.lookup(v);
  else
    stream << (isReturn() ? "return" : "deleted");
  if (depth > 0)
    stream << '.' << depth;
}

std::string CGForDotView::getNodeLabel(const Node *Node) const {
  std::string Label;
  llvm::raw_string_ostream O(Label);
  Node->OrigNode->getRepValue().print(O, InstToIDMap);
  O << '\n';
  if (Node->OrigNode->mappedValue) {
    std::string Inst;
    llvm::raw_string_ostream OI(Inst);
    SILValue(Node->OrigNode->mappedValue)->print(OI);
    size_t start = Inst.find(" = ");
    if (start != std::string::npos) {
      start += 3;
    } else {
      start = 2;
    }
    O << Inst.substr(start, 20);
    O << '\n';
  }
  if (!Node->OrigNode->matchPointToOfDefers()) {
    O << "\nPT mismatch: ";
    if (Node->OrigNode->pointsTo)
      Node->OrigNode->pointsTo->getRepValue().print(O, InstToIDMap);
    else
      O << "null";
  }
  O.flush();
  return Label;
}

std::string CGForDotView::getNodeAttributes(const Node *Node) const {
  auto *Orig = Node->OrigNode;
  std::string attr;
  switch (Orig->Type) {
  case EscapeAnalysis::NodeType::Content:
    attr = "style=\"rounded";
    if (Orig->hasRefCount()) {
      attr += ",filled";
    }
    attr += "\"";
    break;
  case EscapeAnalysis::NodeType::Argument:
  case EscapeAnalysis::NodeType::Return:
    attr = "style=\"bold\"";
    break;
  default:
    break;
  }
  if (Orig->getEscapeState() != EscapeAnalysis::EscapeState::None
      && !attr.empty())
    attr += ',';
  
  switch (Orig->getEscapeState()) {
  case EscapeAnalysis::EscapeState::None:
    break;
  case EscapeAnalysis::EscapeState::Return:
    attr += "color=\"green\"";
    break;
  case EscapeAnalysis::EscapeState::Arguments:
    attr += "color=\"blue\"";
    break;
  case EscapeAnalysis::EscapeState::Global:
    attr += "color=\"red\"";
    break;
  }
  return attr;
}

namespace llvm {


  /// GraphTraits specialization so the CGForDotView can be
  /// iterable by generic graph iterators.
  template <> struct GraphTraits<CGForDotView::Node *> {
    typedef CGForDotView::child_iterator ChildIteratorType;
    typedef CGForDotView::Node *NodeRef;

    static NodeRef getEntryNode(NodeRef N) { return N; }
    static inline ChildIteratorType child_begin(NodeRef N) {
      return N->Children.begin();
    }
    static inline ChildIteratorType child_end(NodeRef N) {
      return N->Children.end();
    }
  };

  template <> struct GraphTraits<CGForDotView *>
  : public GraphTraits<CGForDotView::Node *> {
    typedef CGForDotView *GraphType;
    typedef CGForDotView::Node *NodeRef;

    static NodeRef getEntryNode(GraphType F) { return nullptr; }

    typedef pointer_iterator<CGForDotView::iterator> nodes_iterator;
    static nodes_iterator nodes_begin(GraphType OCG) {
      return nodes_iterator(OCG->Nodes.begin());
    }
    static nodes_iterator nodes_end(GraphType OCG) {
      return nodes_iterator(OCG->Nodes.end());
    }
    static unsigned size(GraphType CG) { return CG->Nodes.size(); }
  };

  /// This is everything the llvm::GraphWriter needs to write the call graph in
  /// a dot file.
  template <>
  struct DOTGraphTraits<CGForDotView *> : public DefaultDOTGraphTraits {

    DOTGraphTraits(bool isSimple = false) : DefaultDOTGraphTraits(isSimple) {}

    static std::string getGraphName(const CGForDotView *Graph) {
      return "CG for " + Graph->F->getName().str();
    }

    std::string getNodeLabel(const CGForDotView::Node *Node,
                             const CGForDotView *Graph) {
      return Graph->getNodeLabel(Node);
    }

    static std::string getNodeAttributes(const CGForDotView::Node *Node,
                                         const CGForDotView *Graph) {
      return Graph->getNodeAttributes(Node);
    }

    static std::string getEdgeAttributes(const CGForDotView::Node *Node,
                                         CGForDotView::child_iterator I,
                                         const CGForDotView *Graph) {
      unsigned ChildIdx = I - Node->Children.begin();
      switch (Node->ChildrenTypes[ChildIdx]) {
        case CGForDotView::PointsTo: return "";
        case CGForDotView::Deferred: return "color=\"gray\"";
      }

      llvm_unreachable("Unhandled CGForDotView in switch.");
    }
  };
} // namespace llvm

#endif

void EscapeAnalysis::ConnectionGraph::viewCG() const {
  /// When asserts are disabled, this should be a NoOp.
#ifndef NDEBUG
  CGForDotView CGDot(this);
  llvm::ViewGraph(&CGDot, "connection-graph");
#endif
}

void EscapeAnalysis::ConnectionGraph::dumpCG() const {
  /// When asserts are disabled, this should be a NoOp.
#ifndef NDEBUG
  CGForDotView CGDot(this);
  llvm::WriteGraph(&CGDot, "connection-graph");
#endif
}

void EscapeAnalysis::CGNode::dump() const {
  llvm::errs() << getTypeStr();
  if (hasRefCount())
    llvm::errs() << " [rc]";

  auto rep = getRepValue();
  if (rep.depth > 0)
    llvm::errs() << " ." << rep.depth;
  llvm::errs() << ": ";
  if (auto v = rep.getValue())
    llvm::errs() << ": " << v;
  else
    llvm::errs() << (rep.isReturn() ? "return" : "deleted") << '\n';

  if (mergeTo) {
    llvm::errs() << "   -> merged to ";
    mergeTo->dump();
  }
}

const char *EscapeAnalysis::CGNode::getTypeStr() const {
  switch (Type) {
    case NodeType::Value:      return "Val";
    case NodeType::Content:    return "Con";
    case NodeType::Argument:   return "Arg";
    case NodeType::Return:     return "Ret";
  }

  llvm_unreachable("Unhandled NodeType in switch.");
}

void EscapeAnalysis::ConnectionGraph::dump() const {
  print(llvm::errs());
}

void EscapeAnalysis::ConnectionGraph::print(llvm::raw_ostream &OS) const {
#ifndef NDEBUG
  OS << "CG of " << F->getName() << '\n';

  // Assign the same IDs to SILValues as the SILPrinter does.
  llvm::DenseMap<const SILNode *, unsigned> InstToIDMap;
  InstToIDMap[nullptr] = (unsigned)-1;
  F->numberValues(InstToIDMap);

  // Sort by SILValue ID+depth. To make the output somehow consistent with
  // the output of the function's SIL.
  auto sortNodes = [&](llvm::SmallVectorImpl<CGNode *> &Nodes) {
    std::sort(Nodes.begin(), Nodes.end(),
              [&](CGNode *Nd1, CGNode *Nd2) -> bool {
                auto rep1 = Nd1->getRepValue();
                auto rep2 = Nd2->getRepValue();
                unsigned VIdx1 = -1;
                if (auto v = rep1.getValue())
                  VIdx1 = InstToIDMap[v];
                unsigned VIdx2 = -1;
                if (auto v = rep2.getValue())
                  VIdx2 = InstToIDMap[v];
                if (VIdx1 != VIdx2)
                  return VIdx1 < VIdx2;
                return rep1.depth < rep2.depth;
              });
  };

  auto nodeStr = [&](CGNode *Nd) -> std::string {
    std::string Str;
    llvm::raw_string_ostream OS(Str);
    if (Nd->hasRefCount())
      OS << "[rc] ";
    Nd->getRepValue().print(OS, InstToIDMap);
    OS.flush();
    return Str;
  };

  llvm::SmallVector<CGNode *, 8> SortedNodes;
  for (CGNode *Nd : Nodes) {
    if (!Nd->isMerged)
      SortedNodes.push_back(Nd);
  }
  sortNodes(SortedNodes);

  for (CGNode *Nd : SortedNodes) {
    OS << "  " << Nd->getTypeStr() << ' ' << nodeStr(Nd) << " Esc: ";
    switch (Nd->getEscapeState()) {
      case EscapeState::None: {
        const char *Separator = "";
        for (unsigned VIdx = Nd->UsePoints.find_first(); VIdx != -1u;
             VIdx = Nd->UsePoints.find_next(VIdx)) {
          auto node = UsePointTable[VIdx];
          OS << Separator << '%' << InstToIDMap[node];
          Separator = ",";
        }
        break;
      }
      case EscapeState::Return:
        OS << 'R';
        break;
      case EscapeState::Arguments:
        OS << 'A';
        break;
      case EscapeState::Global:
        OS << 'G';
        break;
    }
    OS << ", Succ: ";
    const char *Separator = "";
    if (CGNode *PT = Nd->getPointsToEdge()) {
      OS << '(' << nodeStr(PT) << ')';
      Separator = ", ";
    }
    llvm::SmallVector<CGNode *, 8> SortedDefers = Nd->defersTo;
    sortNodes(SortedDefers);
    for (CGNode *Def : SortedDefers) {
      OS << Separator << nodeStr(Def);
      Separator = ", ";
    }
    OS << '\n';
  }
  OS << "End\n";
#endif
}

/// Checks an invariant of the connection graph: The points-to nodes of
/// the defer-successors must match with the points-to of this node.
bool CGNode::matchPointToOfDefers(bool allowMerge) const {
  auto redirect = [allowMerge](CGNode *node) {
    return (allowMerge && node) ? node->getMergeTarget() : node;
  };
  for (CGNode *Def : defersTo) {
    if (redirect(pointsTo) != redirect(Def->pointsTo))
      return false;
  }
  /// A defer-path in the graph must not end without the specified points-to
  /// node.
  if (pointsTo && !pointsToIsEdge && defersTo.empty())
    return false;
  return true;
}

void EscapeAnalysis::ConnectionGraph::verify(bool allowMerge) const {
#ifndef NDEBUG
  verifyStructure(allowMerge);

  // Check graph invariants
  for (CGNode *Nd : Nodes) {
    // ConnectionGraph invariant #4: For any node N, all paths starting at N
    // which consist of only defer-edges and a single trailing points-to edge
    // must lead to the same
    assert(Nd->matchPointToOfDefers(allowMerge));
    if (Nd->hasRefCount()) {
      SILValue v = Nd->getRepValue().getValue();
      (void)v;
      assert(!v || mayContainReference(v->getType(), *F));
    }
  }
#endif
}

void EscapeAnalysis::ConnectionGraph::verifyStructure(bool allowMerge) const {
#ifndef NDEBUG
  for (CGNode *Nd : Nodes) {
    if (Nd->mappedValue && !(allowMerge && Nd->mergeTo))
      assert(Nd == Values2Nodes.lookup(Nd->mappedValue));

    if (Nd->isMerged) {
      assert(Nd->mergeTo);
      assert(!Nd->pointsTo);
      assert(Nd->defersTo.empty());
      assert(Nd->Preds.empty());
      assert(Nd->Type == NodeType::Content);
      continue;
    }
    // Check if predecessor and successor edges are linked correctly.
    for (Predecessor Pred : Nd->Preds) {
      CGNode *PredNode = Pred.getPredNode();
      if (Pred.is(EdgeType::Defer)) {
        assert(PredNode->findDeferred(Nd) != PredNode->defersTo.end());
      } else {
        assert(Pred.is(EdgeType::PointsTo));
        assert(PredNode->getPointsToEdge() == Nd);
      }
    }
    for (CGNode *Def : Nd->defersTo) {
      assert(Def->findPred(Predecessor(Nd, EdgeType::Defer)) != Def->Preds.end());
      assert(Def != Nd);
    }
    if (CGNode *PT = Nd->getPointsToEdge()) {
      assert(PT->Type == NodeType::Content);
      assert(PT->findPred(Predecessor(Nd, EdgeType::PointsTo)) != PT->Preds.end());
    }
  }
#endif
}

//===----------------------------------------------------------------------===//
//                          EscapeAnalysis
//===----------------------------------------------------------------------===//

EscapeAnalysis::EscapeAnalysis(SILModule *M)
    : BottomUpIPAnalysis(SILAnalysisKind::Escape), M(M),
      ArrayType(M->getASTContext().getArrayDecl()), BCA(nullptr) {}

void EscapeAnalysis::initialize(SILPassManager *PM) {
  BCA = PM->getAnalysis<BasicCalleeAnalysis>();
}

/// Returns true if we need to add defer edges for the arguments of a block.
static bool linkBBArgs(SILBasicBlock *BB) {
  // Don't need to handle function arguments.
  if (BB == &BB->getParent()->front())
    return false;
  // We don't need to link to the try_apply's normal result argument, because
  // we handle it separately in setAllEscaping() and mergeCalleeGraph().
  if (SILBasicBlock *SinglePred = BB->getSinglePredecessorBlock()) {
    auto *TAI = dyn_cast<TryApplyInst>(SinglePred->getTerminator());
    if (TAI && BB == TAI->getNormalBB())
      return false;
  }
  return true;
}

EscapeAnalysis::CGNode *
EscapeAnalysis::getValueContent(ConnectionGraph *conGraph, SILValue addrVal) {
  CGNode *addrNode = conGraph->getNode(addrVal);
  if (!addrNode)
    return nullptr;

  if (CGNode *content = addrNode->getPointsToEdge())
    return content;

#ifndef NDEBUG
  if (!addrNode->isContent()) {
    if (SILValue addrNodeValue = addrNode->getRepValue().getValue()) {
      assert(isPointer(addrNodeValue));
      assert(addrNodeValue == getPointerRoot(addrVal));
    }
  }
#endif
  SILValue baseAddr = getPointerRoot(addrVal);
  auto *F = addrVal->getFunction();
  auto hasRC = [&](){
    return mustContainReference(baseAddr->getType(), *F)
      || mustContainReference(addrVal->getType(), *F);
  };
  // Have we already merged a content node for this address?
  if (CGNode *content = addrNode->getContentNodeOrNull()) {
    // TODO: Optimistically merge hasRC content. The original content might not
    // have an RC if one of the values pointing to this content was cast to an
    // unknown type. If any of the types must contain a reference, then the
    // content should contain a reference.
    //
    // For now, conservatively merge the RC flag instead.
    if (content->hasRefCount() && !hasRC())
      content->setRefCount(false);

    return content;
  }
  return conGraph->createContentNode(addrNode, hasRC());
}

void EscapeAnalysis::buildConnectionGraph(FunctionInfo *FInfo,
                                          FunctionOrder &BottomUpOrder,
                                          int RecursionDepth) {
  if (BottomUpOrder.prepareForVisiting(FInfo))
    return;

  LLVM_DEBUG(llvm::dbgs() << "  >> build graph for "
                          << FInfo->Graph.F->getName() << '\n');

  FInfo->NeedUpdateSummaryGraph = true;

  ConnectionGraph *ConGraph = &FInfo->Graph;
  assert(ConGraph->isEmpty());

  // We use a worklist for iteration to visit the blocks in dominance order.
  llvm::SmallPtrSet<SILBasicBlock*, 32> VisitedBlocks;
  llvm::SmallVector<SILBasicBlock *, 16> WorkList;
  VisitedBlocks.insert(&*ConGraph->F->begin());
  WorkList.push_back(&*ConGraph->F->begin());

  while (!WorkList.empty()) {
    SILBasicBlock *BB = WorkList.pop_back_val();

    // Create edges for the instructions.
    for (auto &I : *BB) {
      analyzeInstruction(&I, FInfo, BottomUpOrder, RecursionDepth);
    }
    for (auto &Succ : BB->getSuccessors()) {
      if (VisitedBlocks.insert(Succ.getBB()).second)
        WorkList.push_back(Succ.getBB());
    }
  }

  // Second step: create defer-edges for block arguments.
  for (SILBasicBlock &BB : *ConGraph->F) {
    if (!linkBBArgs(&BB))
      continue;

    // Create defer-edges from the block arguments to it's values in the
    // predecessor's terminator instructions.
    for (SILArgument *BBArg : BB.getArguments()) {
      llvm::SmallVector<SILValue,4> Incoming;
      if (!BBArg->getSingleTerminatorOperands(Incoming)) {
        // We don't know where the block argument comes from -> treat it
        // conservatively.
        setEscapesGlobal(ConGraph, BBArg);
        continue;
      }
      CGNode *ArgNode = ConGraph->getNode(BBArg);
      if (!ArgNode)
        continue;

      for (SILValue Src : Incoming) {
        CGNode *SrcArg = ConGraph->getNode(Src);
        if (SrcArg) {
          ArgNode = ConGraph->defer(ArgNode, SrcArg);
        } else {
          setEscapesGlobal(ConGraph, BBArg);
          break;
        }
      }
    }
  }
  LLVM_DEBUG(llvm::dbgs() << "  << finished graph for "
                          << FInfo->Graph.F->getName() << '\n');
}

/// Returns true if all uses of \p I are tuple_extract instructions.
static bool onlyUsedInTupleExtract(SILValue V) {
  for (Operand *Use : getNonDebugUses(V)) {
    if (!isa<TupleExtractInst>(Use->getUser()))
      return false;
  }
  return true;
}

bool EscapeAnalysis::buildConnectionGraphForCallees(
    SILInstruction *Caller, CalleeList Callees, FunctionInfo *FInfo,
    FunctionOrder &BottomUpOrder, int RecursionDepth) {
  if (Callees.allCalleesVisible()) {
    // Derive the connection graph of the apply from the known callees.
    for (SILFunction *Callee : Callees) {
      FunctionInfo *CalleeInfo = getFunctionInfo(Callee);
      CalleeInfo->addCaller(FInfo, Caller);
      if (!CalleeInfo->isVisited()) {
        // Recursively visit the called function.
        buildConnectionGraph(CalleeInfo, BottomUpOrder, RecursionDepth + 1);
        BottomUpOrder.tryToSchedule(CalleeInfo);
      }
    }
    return true;
  }
  return false;
}

/// Build the connection graph for destructors that may be called
/// by a given instruction \I for the object \V.
/// Returns true if V is a local object and destructors called by a given
/// instruction could be determined. In all other cases returns false.
bool EscapeAnalysis::buildConnectionGraphForDestructor(
    SILValue V, SILInstruction *I, FunctionInfo *FInfo,
    FunctionOrder &BottomUpOrder, int RecursionDepth) {
  // It should be a locally allocated object.
  if (!pointsToLocalObject(V))
    return false;

  // Determine the exact type of the value.
  auto Ty = getExactDynamicTypeOfUnderlyingObject(V, nullptr);
  if (!Ty) {
    // The object is local, but we cannot determine its type.
    return false;
  }
  // If Ty is an optional, its deallocation is equivalent to the deallocation
  // of its payload.
  // TODO: Generalize it. Destructor of an aggregate type is equivalent to calling
  // destructors for its components.
  while (auto payloadTy = Ty.getOptionalObjectType())
    Ty = payloadTy;
  auto Class = Ty.getClassOrBoundGenericClass();
  if (!Class || Class->hasClangNode())
    return false;
  auto Destructor = Class->getDestructor();
  SILDeclRef DeallocRef(Destructor, SILDeclRef::Kind::Deallocator);
  // Find a SILFunction for destructor.
  SILFunction *Dealloc = M->lookUpFunction(DeallocRef);
  if (!Dealloc)
    return false;
  CalleeList Callees(Dealloc);
  return buildConnectionGraphForCallees(I, Callees, FInfo, BottomUpOrder,
                                        RecursionDepth);
}

void EscapeAnalysis::analyzeInstruction(SILInstruction *I,
                                        FunctionInfo *FInfo,
                                        FunctionOrder &BottomUpOrder,
                                        int RecursionDepth) {
  ConnectionGraph *ConGraph = &FInfo->Graph;
  FullApplySite FAS = FullApplySite::isa(I);
  if (FAS &&
      // We currently don't support co-routines. In most cases co-routines will be inlined anyway.
      !isa<BeginApplyInst>(I)) {
    ArraySemanticsCall ASC(FAS.getInstruction());
    switch (ASC.getKind()) {
      case ArrayCallKind::kArrayPropsIsNativeTypeChecked:
      case ArrayCallKind::kCheckSubscript:
      case ArrayCallKind::kCheckIndex:
      case ArrayCallKind::kGetCount:
      case ArrayCallKind::kGetCapacity:
      case ArrayCallKind::kMakeMutable:
        // These array semantics calls do not capture anything.
        return;
      case ArrayCallKind::kArrayUninitialized:
        // Check if the result is used in the usual way: extracting the
        // array and the element pointer with tuple_extract.
        if (onlyUsedInTupleExtract(ASC.getCallResult())) {
          // array.uninitialized may have a first argument which is the
          // allocated array buffer. The call is like a struct(buffer)
          // instruction.
          if (CGNode *BufferNode = ConGraph->getNode(FAS.getArgument(0))) {
            SILValue ArrayBase = ASC.getCallResult();
            CGNode *ArrayContent = getValueContent(ConGraph, ArrayBase);
            assert(ArrayContent && "Array base must have a node");
            ConGraph->defer(ArrayContent, BufferNode);
          }
          return;
        }
        break;
      case ArrayCallKind::kGetElement:
        if (CGNode *ArrayRefNode = getValueContent(ConGraph, ASC.getSelf())) {
          CGNode *LoadedElement = nullptr;
          // This is like a load from a ref_element_addr.
          if (ASC.hasGetElementDirectResult()) {
            LoadedElement = ConGraph->getNode(ASC.getCallResult());
          } else {
            // The content of the destination address.
            LoadedElement = getValueContent(ConGraph, FAS.getArgument(0));
            assert(LoadedElement && "indirect result must have node");
          }
          if (LoadedElement) {
            CGNode *ArrayElementStorage =
                ConGraph->getFieldContent(ArrayRefNode);
            ConGraph->defer(LoadedElement, ArrayElementStorage);
            return;
          }
        }
        break;
      case ArrayCallKind::kGetElementAddress:
        // This is like a ref_element_addr.
        if (CGNode *ArrayRefNode = getValueContent(ConGraph, ASC.getSelf())) {
          ConGraph->defer(ConGraph->getNode(ASC.getCallResult()), ArrayRefNode);
        }
        return;
      case ArrayCallKind::kWithUnsafeMutableBufferPointer:
        // Model this like an escape of the elements of the array and a capture
        // of anything captured by the closure.
        // Self is passed inout.
        if (CGNode *ArrayStructValue =
                getValueContent(ConGraph, ASC.getSelf())) {

          // One content node for going from the array buffer pointer to
          // the element address (like ref_element_addr).
          CGNode *ArrayRefNode = ArrayStructValue->getContentNodeOrNull();
          // TODO: If ArrayRefNode already exists, optimistically do
          // ArrayRefNode->setRefCount(true).
          if (!ArrayRefNode) {
            ArrayRefNode = ConGraph->createContentNode(
                ArrayStructValue, /*hasRC=*/true);
          }
          // Another content node for the element storage.
          CGNode *ArrayElementStorage = ConGraph->getFieldContent(ArrayRefNode);
          ArrayElementStorage->markEscaping();
          // The first non indirect result is the closure.
          auto Args = FAS.getArgumentsWithoutIndirectResults();
          setEscapesGlobal(ConGraph, Args[0]);
          return;
        }
        break;
      default:
        break;
    }

    if (FAS.getReferencedFunctionOrNull() &&
        FAS.getReferencedFunctionOrNull()->hasSemanticsAttr(
            "self_no_escaping_closure") &&
        ((FAS.hasIndirectSILResults() && FAS.getNumArguments() == 3) ||
         (!FAS.hasIndirectSILResults() && FAS.getNumArguments() == 2)) &&
        FAS.hasSelfArgument()) {
      // The programmer has guaranteed that the closure will not capture the
      // self pointer passed to it or anything that is transitively reachable
      // from the pointer.
      auto Args = FAS.getArgumentsWithoutIndirectResults();
      // The first not indirect result argument is the closure.
      setEscapesGlobal(ConGraph, Args[0]);
      return;
    }

    if (FAS.getReferencedFunctionOrNull() &&
        FAS.getReferencedFunctionOrNull()->hasSemanticsAttr(
            "pair_no_escaping_closure") &&
        ((FAS.hasIndirectSILResults() && FAS.getNumArguments() == 4) ||
         (!FAS.hasIndirectSILResults() && FAS.getNumArguments() == 3)) &&
        FAS.hasSelfArgument()) {
      // The programmer has guaranteed that the closure will not capture the
      // self pointer passed to it or anything that is transitively reachable
      // from the pointer.
      auto Args = FAS.getArgumentsWithoutIndirectResults();
      // The second not indirect result argument is the closure.
      setEscapesGlobal(ConGraph, Args[1]);
      return;
    }

    if (RecursionDepth < MaxRecursionDepth) {
      CalleeList Callees = BCA->getCalleeList(FAS);
      if (buildConnectionGraphForCallees(FAS.getInstruction(), Callees, FInfo,
                                         BottomUpOrder, RecursionDepth))
        return;
    }

    if (auto *Fn = FAS.getReferencedFunctionOrNull()) {
      if (Fn->getName() == "swift_bufferAllocate")
        // The call is a buffer allocation, e.g. for Array.
        return;
    }
  }

  if (isa<StrongReleaseInst>(I) || isa<ReleaseValueInst>(I)) {
    // Treat the release instruction as if it is the invocation
    // of a deinit function.
    if (RecursionDepth < MaxRecursionDepth) {
      // Check if the destructor is known.
      auto OpV = cast<RefCountingInst>(I)->getOperand(0);
      if (buildConnectionGraphForDestructor(OpV, I, FInfo, BottomUpOrder,
                                            RecursionDepth))
        return;
    }
  }

  // If this instruction produces a single value whose pointer is represented by
  // a different base pointer, then skip it.
  if (auto *SVI = dyn_cast<SingleValueInstruction>(I)) {
    if (getPointerBase(SVI))
      return;
  }

  // Instructions which return the address of non-writable memory cannot have
  // an effect on escaping.
  if (isNonWritableMemoryAddress(I))
    return;

  switch (I->getKind()) {
    case SILInstructionKind::AllocStackInst:
    case SILInstructionKind::AllocRefInst:
    case SILInstructionKind::AllocBoxInst:
      ConGraph->getNode(cast<SingleValueInstruction>(I));
      return;

#define UNCHECKED_REF_STORAGE(Name, ...)                                       \
  case SILInstructionKind::StrongCopy##Name##ValueInst:
#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)            \
  case SILInstructionKind::Name##RetainInst:                                   \
  case SILInstructionKind::StrongRetain##Name##Inst:                           \
  case SILInstructionKind::StrongCopy##Name##ValueInst:
#include "swift/AST/ReferenceStorage.def"
    case SILInstructionKind::DeallocStackInst:
    case SILInstructionKind::StrongRetainInst:
    case SILInstructionKind::RetainValueInst:
    case SILInstructionKind::BranchInst:
    case SILInstructionKind::CondBranchInst:
    case SILInstructionKind::SwitchEnumInst:
    case SILInstructionKind::DebugValueInst:
    case SILInstructionKind::DebugValueAddrInst:
    case SILInstructionKind::ValueMetatypeInst:
    case SILInstructionKind::InitExistentialMetatypeInst:
    case SILInstructionKind::OpenExistentialMetatypeInst:
    case SILInstructionKind::ExistentialMetatypeInst:
    case SILInstructionKind::DeallocRefInst:
    case SILInstructionKind::SetDeallocatingInst:
    case SILInstructionKind::FixLifetimeInst:
    case SILInstructionKind::ClassifyBridgeObjectInst:
    case SILInstructionKind::ValueToBridgeObjectInst:
      // These instructions don't have any effect on escaping.
      return;

#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    case SILInstructionKind::Name##ReleaseInst:
#include "swift/AST/ReferenceStorage.def"
    case SILInstructionKind::StrongReleaseInst:
    case SILInstructionKind::ReleaseValueInst: {
      // A release instruction may deallocate the pointer operand. This may
      // capture anything pointed to by the released object, but not the pointer
      // to the object itself (because it will be a dangling pointer after
      // deallocation).
      SILValue OpV = I->getOperand(0);
      CGNode *rcContent = getValueContent(ConGraph, OpV);
      if (!rcContent)
        return;

      // rcContent->hasRefCount() may or may not be true depending on whether
      // the type could be analyzed. Either way, treat it structurally like a
      // refcounted object.
      CGNode *fieldContent = ConGraph->getFieldContent(rcContent);
      if (!deinitIsKnownToNotCapture(OpV)) {
        fieldContent->markEscaping();
        return;
      }
      // This deinit is known to not directly capture it's own field content,
      // however, indirect deinitializers could still capture anything pointed
      // to by those fields.
      ConGraph->escapeContentsOf(fieldContent);
      return;
    }

#define NEVER_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    case SILInstructionKind::Load##Name##Inst:
#include "swift/AST/ReferenceStorage.def"
    case SILInstructionKind::LoadInst:
      assert(!cast<SingleValueInstruction>(I)->getType().isAddress());
      LLVM_FALLTHROUGH;
    case SILInstructionKind::RefElementAddrInst:
    case SILInstructionKind::RefTailAddrInst:
    case SILInstructionKind::ProjectBoxInst:
    case SILInstructionKind::InitExistentialAddrInst:
    case SILInstructionKind::OpenExistentialAddrInst: {
      // Loads and projections into RC objects have a similar pattern:
      //
      // For RC object projections, get the non-address reference operand and
      // return an RC content node that the reference directly points to. It is
      // as-if the RC content node holds the pointer to the object fields.
      //
      // For loads, get the address-type operand and return the content node
      // that the address directly points to. The load's address may itself come
      // from a ref_element_addr, project_box or open_existential, in which
      // case, the loaded content will be the field content, not the RC content.
      auto SVI = cast<SingleValueInstruction>(I);
      if (!isPointer(SVI))
        return;

      SILValue pointerVal = SVI->getOperand(0);
      if (CGNode *PointsTo = getValueContent(ConGraph, pointerVal)) {
        ConGraph->setNode(SVI, PointsTo);
        return;
      }
      // A load or projection from an address we don't handle -> be
      // conservative.
      setEscapesGlobal(ConGraph, SVI);
      return;
    }
    case SILInstructionKind::CopyAddrInst: {
      // Be conservative if the dest may be the final release.
      if (!cast<CopyAddrInst>(I)->isInitializationOfDest()) {
        setAllEscaping(I, ConGraph);
        break;
      }

      // A copy_addr is like a 'store (load src) to dest'.
      SILValue srcAddr = I->getOperand(CopyAddrInst::Src);
      CGNode *loadedContent = getValueContent(ConGraph, srcAddr);
      if (!loadedContent) {
        setAllEscaping(I, ConGraph);
        break;
      }
      SILValue destAddr = I->getOperand(CopyAddrInst::Dest);
      // Create a defer-edge from the store location to the loaded content.
      if (CGNode *destContent = getValueContent(ConGraph, destAddr)) {
        ConGraph->defer(destContent, loadedContent);
        return;
      }
      // A store to an address we don't handle -> be conservative.
      setEscapesGlobal(ConGraph, srcAddr);
      return;
    }

#define NEVER_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    case SILInstructionKind::Store##Name##Inst:
#include "swift/AST/ReferenceStorage.def"
    case SILInstructionKind::StoreInst: {
      SILValue srcVal = I->getOperand(StoreInst::Src);
      CGNode *valueNode = ConGraph->getNode(srcVal);
      // If the stored value isn't tracked, ignore the store.
      if (!valueNode)
        return;

      // The store destination content is always one pointsTo level away from
      // its address.  Either the address points to a variable or argument, and
      // the pointee is removed by a level of pointer indirection, or the
      // address corresponds is a projection within a reference counted object
      // (via ref_element_addr, project_box, or open_existential_addr) where the
      // stored field content is chained one level below the RC content.
      SILValue destAddr = I->getOperand(StoreInst::Dest);
      if (CGNode *pointsTo = getValueContent(ConGraph, destAddr)) {
        // Create a defer-edge from the content to the stored value.
        ConGraph->defer(pointsTo, valueNode);
        return;
      }
      // A store to an address we don't handle -> be conservative.
      setEscapesGlobal(ConGraph, srcVal);
      return;
    }
    case SILInstructionKind::PartialApplyInst: {
      // The result of a partial_apply is a thick function which stores the
      // boxed partial applied arguments. We create defer-edges from the
      // partial_apply values to the arguments.
      auto PAI = cast<PartialApplyInst>(I);
      if (CGNode *ResultNode = ConGraph->getNode(PAI)) {
        for (const Operand &Op : PAI->getAllOperands()) {
          if (CGNode *ArgNode = ConGraph->getNode(Op.get())) {
            ResultNode = ConGraph->defer(ResultNode, ArgNode);
          }
        }
      }
      return;
    }
    case SILInstructionKind::SelectEnumInst:
    case SILInstructionKind::SelectEnumAddrInst:
      analyzeSelectInst(cast<SelectEnumInstBase>(I), ConGraph);
      return;
    case SILInstructionKind::SelectValueInst:
      analyzeSelectInst(cast<SelectValueInst>(I), ConGraph);
      return;
    case SILInstructionKind::StructInst:
    case SILInstructionKind::TupleInst:
    case SILInstructionKind::EnumInst: {
      // Aggregate composition is like assigning the aggregate fields to the
      // resulting aggregate value.
      auto svi = cast<SingleValueInstruction>(I);
      CGNode *resultNode = ConGraph->getNode(svi);
      for (const Operand &operand : svi->getAllOperands()) {
        if (CGNode *subNode = ConGraph->getNode(operand.get()))
          ConGraph->defer(resultNode, subNode);
      }
      return;
    }
    case SILInstructionKind::TupleExtractInst: {
      // This is a tuple_extract which extracts the second result of an
      // array.uninitialized call (otherwise getPointerBase should have already
      // looked through it). The first result is the array itself.
      // The second result (which is a pointer to the array elements) must be
      // the content node of the first result. It's just like a ref_element_addr
      // instruction.
      auto *TEI = cast<TupleExtractInst>(I);
      assert(isExtractOfArrayUninitializedPointer(TEI)
             && "tuple_extract should be handled as projection");
      if (CGNode *ArrayElements = getValueContent(ConGraph, TEI->getOperand()))
        ConGraph->setNode(TEI, ArrayElements);
      return;
    }
    case SILInstructionKind::UncheckedRefCastAddrInst: {
      auto *URCAI = cast<UncheckedRefCastAddrInst>(I);
      CGNode *SrcNode = ConGraph->getNode(URCAI->getSrc());
      CGNode *DestNode = ConGraph->getNode(URCAI->getDest());
      assert(SrcNode && DestNode && "must have nodes for address operands");
      ConGraph->defer(DestNode, SrcNode);
      return;
    }
    case SILInstructionKind::ReturnInst:
      if (CGNode *ValueNd =
              ConGraph->getNode(cast<ReturnInst>(I)->getOperand())) {
        ConGraph->defer(ConGraph->getReturnNode(), ValueNd);
      }
      return;
    default:
      // We handle all other instructions conservatively.
      setAllEscaping(I, ConGraph);
      return;
  }
}

template<class SelectInst> void EscapeAnalysis::
analyzeSelectInst(SelectInst *SI, ConnectionGraph *ConGraph) {
  if (auto *ResultNode = ConGraph->getNode(SI)) {
    // Connect all case values to the result value.
    // Note that this does not include the first operand (the condition).
    for (unsigned Idx = 0, End = SI->getNumCases(); Idx < End; ++Idx) {
      SILValue CaseVal = SI->getCase(Idx).second;
      auto *ArgNode = ConGraph->getNode(CaseVal);
      assert(ArgNode &&
             "there should be an argument node if there is a result node");
      ResultNode = ConGraph->defer(ResultNode, ArgNode);
    }
    // ... also including the default value.
    auto *DefaultNode = ConGraph->getNode(SI->getDefaultResult());
    assert(DefaultNode &&
           "there should be an argument node if there is a result node");
    ConGraph->defer(ResultNode, DefaultNode);
  }
}

bool EscapeAnalysis::deinitIsKnownToNotCapture(SILValue V) {
  for (;;) {
    // The deinit of an array buffer does not capture the array elements.
    if (V->getType().getNominalOrBoundGenericNominal() == ArrayType)
      return true;

    // The deinit of a box does not capture its content.
    if (V->getType().is<SILBoxType>())
      return true;

    if (isa<FunctionRefInst>(V) || isa<DynamicFunctionRefInst>(V) ||
        isa<PreviousDynamicFunctionRefInst>(V))
      return true;

    // Check all operands of a partial_apply
    if (auto *PAI = dyn_cast<PartialApplyInst>(V)) {
      for (Operand &Op : PAI->getAllOperands()) {
        if (isPointer(Op.get()) && !deinitIsKnownToNotCapture(Op.get()))
          return false;
      }
      return true;
    }
    if (auto base = getPointerBase(V)) {
      V = base;
      continue;
    }
    return false;
  }
}

void EscapeAnalysis::setAllEscaping(SILInstruction *I,
                                    ConnectionGraph *ConGraph) {
  if (auto *TAI = dyn_cast<TryApplyInst>(I)) {
    setEscapesGlobal(ConGraph, TAI->getNormalBB()->getArgument(0));
    setEscapesGlobal(ConGraph, TAI->getErrorBB()->getArgument(0));
  }
  // Even if the instruction does not write memory we conservatively set all
  // operands to escaping, because they may "escape" to the result value in
  // an unspecified way. For example consider bit-casting a pointer to an int.
  // In this case we don't even create a node for the resulting int value.
  for (const Operand &Op : I->getAllOperands()) {
    SILValue OpVal = Op.get();
    if (!isNonWritableMemoryAddress(OpVal))
      setEscapesGlobal(ConGraph, OpVal);
  }
  // Even if the instruction does not write memory it could e.g. return the
  // address of global memory. Therefore we have to define it as escaping.
  for (auto result : I->getResults())
    setEscapesGlobal(ConGraph, result);
}

void EscapeAnalysis::recompute(FunctionInfo *Initial) {
  allocNewUpdateID();

  LLVM_DEBUG(llvm::dbgs() << "recompute escape analysis with UpdateID "
                          << getCurrentUpdateID() << '\n');

  // Collect and analyze all functions to recompute, starting at Initial.
  FunctionOrder BottomUpOrder(getCurrentUpdateID());
  buildConnectionGraph(Initial, BottomUpOrder, 0);

  // Build the bottom-up order.
  BottomUpOrder.tryToSchedule(Initial);
  BottomUpOrder.finishScheduling();

  // Second step: propagate the connection graphs up the call-graph until it
  // stabilizes.
  int Iteration = 0;
  bool NeedAnotherIteration;
  do {
    LLVM_DEBUG(llvm::dbgs() << "iteration " << Iteration << '\n');
    NeedAnotherIteration = false;

    for (FunctionInfo *FInfo : BottomUpOrder) {
      bool SummaryGraphChanged = false;
      if (FInfo->NeedUpdateSummaryGraph) {
        LLVM_DEBUG(llvm::dbgs() << "  create summary graph for "
                                << FInfo->Graph.F->getName() << '\n');

        FInfo->Graph.propagateEscapeStates();

        // Derive the summary graph of the current function. Even if the
        // complete graph of the function did change, it does not mean that the
        // summary graph will change.
        SummaryGraphChanged = mergeSummaryGraph(&FInfo->SummaryGraph,
                                                &FInfo->Graph);
        FInfo->NeedUpdateSummaryGraph = false;
      }

      if (Iteration < MaxGraphMerges) {
        // In the first iteration we have to merge the summary graphs, even if
        // they didn't change (in not recomputed leaf functions).
        if (Iteration == 0 || SummaryGraphChanged) {
          // Merge the summary graph into all callers.
          for (const auto &E : FInfo->getCallers()) {
            assert(E.isValid());

            // Only include callers which we are actually recomputing.
            if (BottomUpOrder.wasRecomputedWithCurrentUpdateID(E.Caller)) {
              LLVM_DEBUG(llvm::dbgs() << "  merge  "
                                      << FInfo->Graph.F->getName()
                                      << " into "
                                      << E.Caller->Graph.F->getName() << '\n');
              if (mergeCalleeGraph(E.FAS, &E.Caller->Graph,
                                   &FInfo->SummaryGraph)) {
                E.Caller->NeedUpdateSummaryGraph = true;
                if (!E.Caller->isScheduledAfter(FInfo)) {
                  // This happens if we have a cycle in the call-graph.
                  NeedAnotherIteration = true;
                }
              }
            }
          }
        }
      } else if (Iteration == MaxGraphMerges) {
        // Limit the total number of iterations. First to limit compile time,
        // second to make sure that the loop terminates. Theoretically this
        // should always be the case, but who knows?
        LLVM_DEBUG(llvm::dbgs() << "  finalize conservatively "
                                << FInfo->Graph.F->getName() << '\n');
        for (const auto &E : FInfo->getCallers()) {
          assert(E.isValid());
          if (BottomUpOrder.wasRecomputedWithCurrentUpdateID(E.Caller)) {
            setAllEscaping(E.FAS, &E.Caller->Graph);
            E.Caller->NeedUpdateSummaryGraph = true;
            NeedAnotherIteration = true;
          }
        }
      }
    }
    Iteration++;
  } while (NeedAnotherIteration);

  for (FunctionInfo *FInfo : BottomUpOrder) {
    if (BottomUpOrder.wasRecomputedWithCurrentUpdateID(FInfo)) {
      FInfo->Graph.computeUsePoints();
      FInfo->Graph.verify();
      FInfo->SummaryGraph.verify();
    }
  }
}

bool EscapeAnalysis::mergeCalleeGraph(SILInstruction *AS,
                                      ConnectionGraph *CallerGraph,
                                      ConnectionGraph *CalleeGraph) {
  // This CGNodeMap uses an intrusive worklist to keep track of Mapped nodes
  // from the CalleeGraph. Meanwhile, mergeFrom uses separate intrusive
  // worklists to update nodes in the CallerGraph.
  CGNodeMap Callee2CallerMapping(CalleeGraph);

  // First map the callee parameters to the caller arguments.
  SILFunction *Callee = CalleeGraph->F;
  auto FAS = FullApplySite::isa(AS);
  unsigned numCallerArgs = FAS ? FAS.getNumArguments() : 1;
  unsigned numCalleeArgs = Callee->getArguments().size();
  assert(numCalleeArgs >= numCallerArgs);
  for (unsigned Idx = 0; Idx < numCalleeArgs; ++Idx) {
    // If there are more callee parameters than arguments it means that the
    // callee is the result of a partial_apply - a thick function. A thick
    // function also references the boxed partially applied arguments.
    // Therefore we map all the extra callee parameters to the callee operand
    // of the apply site.
    SILValue CallerArg;
    if (FAS)
      CallerArg =
          (Idx < numCallerArgs ? FAS.getArgument(Idx) : FAS.getCallee());
    else
      CallerArg = (Idx < numCallerArgs ? AS->getOperand(Idx) : SILValue());

    CGNode *CalleeNd = CalleeGraph->getNode(Callee->getArgument(Idx));
    if (!CalleeNd)
      continue;

    CGNode *CallerNd = CallerGraph->getNode(CallerArg);
    // There can be the case that we see a callee argument as pointer but not
    // the caller argument. E.g. if the callee argument has a @convention(c)
    // function type and the caller passes a function_ref.
    if (!CallerNd)
      continue;

    Callee2CallerMapping.add(CalleeNd, CallerNd);
  }

  // Map the return value.
  if (CGNode *RetNd = CalleeGraph->getReturnNodeOrNull()) {
    // The non-ApplySite instructions that cause calls are to things like
    // destructors that don't have return values.
    assert(FAS);
    ValueBase *CallerReturnVal = nullptr;
    if (auto *TAI = dyn_cast<TryApplyInst>(AS)) {
      CallerReturnVal = TAI->getNormalBB()->getArgument(0);
    } else {
      CallerReturnVal = cast<ApplyInst>(AS);
    }
    CGNode *CallerRetNd = CallerGraph->getNode(CallerReturnVal);
    if (CallerRetNd)
      Callee2CallerMapping.add(RetNd, CallerRetNd);
  }
  return CallerGraph->mergeFrom(CalleeGraph, Callee2CallerMapping);
}

bool EscapeAnalysis::mergeSummaryGraph(ConnectionGraph *SummaryGraph,
                                        ConnectionGraph *Graph) {

  // Make a 1-to-1 mapping of all arguments and the return value. This CGNodeMap
  // node map uses an intrusive worklist to keep track of Mapped nodes from the
  // Graph. Meanwhile, mergeFrom uses separate intrusive worklists to
  // update nodes in the SummaryGraph.
  CGNodeMap Mapping(Graph);
  for (SILArgument *Arg : Graph->F->getArguments()) {
    if (CGNode *ArgNd = Graph->getNode(Arg)) {
      Mapping.add(ArgNd, SummaryGraph->getNode(Arg));
    }
  }
  if (CGNode *RetNd = Graph->getReturnNodeOrNull()) {
    Mapping.add(RetNd, SummaryGraph->getReturnNode());
  }
  // Merging actually creates the summary graph.
  return SummaryGraph->mergeFrom(Graph, Mapping);
}

bool EscapeAnalysis::canEscapeToUsePoint(SILValue V, SILNode *UsePoint,
                                         ConnectionGraph *ConGraph) {

  assert((FullApplySite::isa(UsePoint) || isa<RefCountingInst>(UsePoint)) &&
         "use points are only created for calls and refcount instructions");

  CGNode *Node = ConGraph->getNodeOrNull(V);
  if (!Node)
    return true;

  // First check if there are escape paths which we don't explicitly see
  // in the graph.
  if (Node->valueEscapesInsideFunction(V))
    return true;

  // No hidden escapes: check if the Node is reachable from the UsePoint.
  // Check if the object itself can escape to the called function.
  if (ConGraph->isUsePoint(UsePoint, Node))
    return true;

  assert(isPointer(V) && "should not have a node for a non-pointer");

  // Check if the object "content" can escape to the called function.
  // This will catch cases where V is a reference and a pointer to a stored
  // property escapes.
  // It's also important in case of a pointer assignment, e.g.
  //    V = V1
  //    apply(V1)
  // In this case the apply is only a use-point for V1 and V1's content node.
  // As V1's content node is the same as V's content node, we also make the
  // check for the content node.
  CGNode *ContentNode = getValueContent(ConGraph, V);
  if (ContentNode->valueEscapesInsideFunction(V))
    return true;

  if (ConGraph->isUsePoint(UsePoint, ContentNode))
    return true;

  return false;
}

bool EscapeAnalysis::canEscapeTo(SILValue V, FullApplySite FAS) {
  // If it's not a local object we don't know anything about the value.
  if (!isUniquelyIdentified(V))
    return true;
  auto *ConGraph = getConnectionGraph(FAS.getFunction());
  return canEscapeToUsePoint(V, FAS.getInstruction(), ConGraph);
}

// FIXME: remove this to avoid confusion with SILType.hasReferenceSemantics.
static bool hasReferenceSemantics(SILType T) {
  // Exclude address types.
  return T.isObject() && T.hasReferenceSemantics();
}

bool EscapeAnalysis::canEscapeTo(SILValue V, RefCountingInst *RI) {
  // If it's not uniquely identified we don't know anything about the value.
  if (!isUniquelyIdentified(V))
    return true;
  auto *ConGraph = getConnectionGraph(RI->getFunction());
  return canEscapeToUsePoint(V, RI, ConGraph);
}

/// Utility to get the function which contains both values \p V1 and \p V2.
static SILFunction *getCommonFunction(SILValue V1, SILValue V2) {
  SILBasicBlock *BB1 = V1->getParentBlock();
  SILBasicBlock *BB2 = V2->getParentBlock();
  if (!BB1 || !BB2)
    return nullptr;

  SILFunction *F = BB1->getParent();
  assert(BB2->getParent() == F && "values not in same function");
  return F;
}

bool EscapeAnalysis::canEscapeToValue(SILValue V, SILValue To) {
  if (!isUniquelyIdentified(V))
    return true;

  SILFunction *F = getCommonFunction(V, To);
  if (!F)
    return true;
  auto *ConGraph = getConnectionGraph(F);

  CGNode *Node = ConGraph->getNodeOrNull(V);
  if (!Node)
    return true;
  CGNode *ToNode = ConGraph->getNodeOrNull(To);
  if (!ToNode)
    return true;
  return ConGraph->mayReach(ToNode, Node);
}

bool EscapeAnalysis::canPointToSameMemory(SILValue V1, SILValue V2) {
  // At least one of the values must be a non-escaping local object.
  bool isUniq1 = isUniquelyIdentified(V1);
  bool isUniq2 = isUniquelyIdentified(V2);
  if (!isUniq1 && !isUniq2)
    return true;

  SILFunction *F = getCommonFunction(V1, V2);
  if (!F)
    return true;
  auto *ConGraph = getConnectionGraph(F);

  CGNode *Node1 = ConGraph->getNodeOrNull(V1);
  if (!Node1)
    return true;
  CGNode *Node2 = ConGraph->getNodeOrNull(V2);
  if (!Node2)
    return true;

  // Finish the check for one value being a non-escaping local object.
  if (isUniq1 && Node1->valueEscapesInsideFunction(V1))
    isUniq1 = false;

  if (isUniq2 && Node2->valueEscapesInsideFunction(V2))
    isUniq2 = false;

  if (!isUniq1 && !isUniq2)
    return true;

  // Check if both nodes may point to the same content.
  CGNode *Content1 = getValueContent(ConGraph, V1);
  CGNode *Content2 = getValueContent(ConGraph, V2);

  SILType T1 = V1->getType();
  SILType T2 = V2->getType();
  if (T1.isAddress() && T2.isAddress()) {
    return Content1 == Content2;
  }
  if (hasReferenceSemantics(T1) && hasReferenceSemantics(T2)) {
    return Content1 == Content2;
  }
  // As we model the ref_element_addr instruction as a content-relationship, we
  // have to go down one content level if just one of the values is a
  // ref-counted object.
  if (T1.isAddress() && hasReferenceSemantics(T2)) {
    Content2 = ConGraph->getFieldContent(Content2);
    return Content1 == Content2;
  }
  if (T2.isAddress() && hasReferenceSemantics(T1)) {
    Content1 = ConGraph->getFieldContent(Content1);
    return Content1 == Content2;
  }
  return true;
}

bool EscapeAnalysis::canParameterEscape(FullApplySite FAS, int ParamIdx,
                                        bool checkContentOfIndirectParam) {
  CalleeList Callees = BCA->getCalleeList(FAS);
  if (!Callees.allCalleesVisible())
    return true;

  // Derive the connection graph of the apply from the known callees.
  for (SILFunction *Callee : Callees) {
    FunctionInfo *FInfo = getFunctionInfo(Callee);
    if (!FInfo->isValid())
      recompute(FInfo);

    CGNode *Node =
        FInfo->SummaryGraph.getNodeOrNull(Callee->getArgument(ParamIdx));
    if (!Node)
      return true;

    if (checkContentOfIndirectParam) {
      Node = Node->getContentNodeOrNull();
      if (!Node)
        continue;
    }

    if (Node->escapes())
      return true;
  }
  return false;
}

void EscapeAnalysis::invalidate() {
  Function2Info.clear();
  Allocator.DestroyAll();
  LLVM_DEBUG(llvm::dbgs() << "invalidate all\n");
}

void EscapeAnalysis::invalidate(SILFunction *F, InvalidationKind K) {
  if (FunctionInfo *FInfo = Function2Info.lookup(F)) {
    LLVM_DEBUG(llvm::dbgs() << "  invalidate "
                            << FInfo->Graph.F->getName() << '\n');
    invalidateIncludingAllCallers(FInfo);
  }
}

void EscapeAnalysis::handleDeleteNotification(SILNode *node) {
  auto value = dyn_cast<ValueBase>(node);
  if (!value) return;

  if (SILBasicBlock *Parent = node->getParentBlock()) {
    SILFunction *F = Parent->getParent();
    if (FunctionInfo *FInfo = Function2Info.lookup(F)) {
      if (FInfo->isValid()) {
        FInfo->Graph.removeFromGraph(value);
        FInfo->SummaryGraph.removeFromGraph(value);
      }
    }
  }
}

SILAnalysis *swift::createEscapeAnalysis(SILModule *M) {
  return new EscapeAnalysis(M);
}
