//===-------------- EscapeAnalysis.cpp - SIL Escape Analysis --------------===//
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

#define DEBUG_TYPE "sil-escape"
#include "swift/SILAnalysis/EscapeAnalysis.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILAnalysis/ArraySemantic.h"
#include "swift/SILPasses/PassManager.h"
#include "swift/SIL/SILArgument.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

static bool isProjection(ValueBase *V) {
  switch (V->getKind()) {
    case ValueKind::IndexAddrInst:
    case ValueKind::IndexRawPointerInst:
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::RefElementAddrInst:
    case ValueKind::UncheckedTakeEnumDataAddrInst:
    case ValueKind::PointerToAddressInst:
    case ValueKind::StructExtractInst:
    case ValueKind::TupleExtractInst:
    case ValueKind::UncheckedEnumDataInst:
      return true;
    default:
      return false;
  }
}

static ValueBase *skipProjections(ValueBase *V) {
  for (;;) {
    if (!isProjection(V))
      return V;
    V = cast<SILInstruction>(V)->getOperand(0).getDef();
  }
  llvm_unreachable("there is no escape from an infinite loop");
}

/// Returns true if the type \p Ty is a reference or transitively contains one,
/// i.e. if it is a "pointer" type.
static bool isOrContainsReference(SILType Ty, SILModule &Mod) {
  if (Ty.hasReferenceSemantics())
    return true;

  if (auto *Str = Ty.getStructOrBoundGenericStruct()) {
    for (auto *Field : Str->getStoredProperties()) {
      if (isOrContainsReference(Ty.getFieldType(Field, Mod), Mod))
        return true;
    }
    return false;
  }
  if (auto TT = Ty.getAs<TupleType>()) {
    for (unsigned i = 0, e = TT->getNumElements(); i != e; ++i) {
      if (isOrContainsReference(Ty.getTupleElementType(i), Mod))
        return true;
    }
    return false;
  }
  if (auto En = Ty.getEnumOrBoundGenericEnum()) {
    for (auto *ElemDecl : En->getAllElements()) {
      if (ElemDecl->hasArgumentType() &&
          isOrContainsReference(Ty.getEnumElementType(ElemDecl, Mod), Mod))
        return true;
    }
    return false;
  }
  return false;
}

/// Returns true if \p V is a "pointer" value.
/// See EscapeAnalysis::NodeType::Value.
static bool isPointer(ValueBase *V, SILModule &Mod) {
  assert(V->hasValue());
  SILType Ty = V->getType(0);
  return Ty.isAddress() || Ty.isLocalStorage() || isOrContainsReference(Ty, Mod);
}

EscapeAnalysis::CGNode *EscapeAnalysis::ConnectionGraph::getNode(ValueBase *V) {
  if (isa<FunctionRefInst>(V))
    return nullptr;

  if (!V->hasValue())
    return nullptr;

  if (!isPointer(V, F->getModule()))
    return nullptr;

  V = skipProjections(V);

  CGNode * &Node = Values2Nodes[V];
  if (!Node) {
    auto *Arg = dyn_cast<SILArgument>(V);
    if (Arg && Arg->isFunctionArg()) {
      Node = allocNode(V, NodeType::Argument);
      Node->EscapeSet.set(Arg->getIndex() + FirstArg);
    } else {
      Node = allocNode(V, NodeType::Value);
    }
  }
  return Node->getMergeTarget();
}

EscapeAnalysis::CGNode *EscapeAnalysis::ConnectionGraph::getContentNode(
                                                          CGNode *AddrNode) {
  // Do we already have a content node (which is not necessarliy an immediate
  // successor of AddrNode)?
  if (AddrNode->pointsTo)
    return AddrNode->pointsTo;

  CGNode *Node = allocNode(AddrNode->V, NodeType::Content);
  updatePointsTo(AddrNode, Node);
  assert(ToMerge.empty() &&
         "Initialiiy setting pointsTo should not require any node merges");
  return Node;
}

bool EscapeAnalysis::ConnectionGraph::addDeferEdge(CGNode *From, CGNode *To) {
  if (!From->addDefered(To))
    return false;

  CGNode *FromPointsTo = From->pointsTo;
  CGNode *ToPointsTo = To->pointsTo;
  if (FromPointsTo != ToPointsTo) {
    if (!ToPointsTo) {
      updatePointsTo(To, FromPointsTo);
      assert(ToMerge.empty() &&
             "Initialiiy setting pointsTo should not require any node merges");
    } else {
      // We are adding an edge between two pointers which point to different
      // content nodes. This will require to merge the content nodes (and maybe
      // other content nodes as well), because of the graph invariance 4).
      updatePointsTo(From, ToPointsTo);
    }
  }
  return true;
}

void EscapeAnalysis::ConnectionGraph::mergeAllScheduledNodes() {
  while (!ToMerge.empty()) {
    CGNode *From = ToMerge.pop_back_val();
    CGNode *To = From->mergeTo;
    assert(To && "Node scheduled to merge but no merge target set");
    assert(!From->isMerged && "Merge source is already merged");
    assert(From->Type == NodeType::Content && "Can only merge content nodes");
    assert(To->Type == NodeType::Content && "Can only merge content nodes");

    // Unlink the predecessors and redirect the incoming pointsTo edge.
    // Note: we don't redirect the defer-edges because we don't want to trigger
    // updatePointsTo (which is called by addDeferEdge) right now.
    for (Predecessor Pred : From->Preds) {
      CGNode *PredNode = Pred.getPointer();
      if (Pred.getInt() == EdgeType::PointsTo) {
        assert(PredNode->getPointsToEdge() == From &&
               "Incoming pointsTo edge not set in predecessor");
        if (PredNode != From)
          PredNode->setPointsTo(To);
      } else {
        assert(PredNode != From);
        auto Iter = PredNode->findDefered(From);
        assert(Iter != PredNode->defersTo.end() &&
               "Incoming defer-edge not found in predecessor's defer list");
        PredNode->defersTo.erase(Iter);
      }
    }
    // Unlink and redirect the outgoing pointsTo edge.
    if (CGNode *PT = From->getPointsToEdge()) {
      if (PT != From) {
        PT->removeFromPreds(Predecessor(From, EdgeType::PointsTo));
      } else {
        PT = To;
      }
      if (CGNode *ExistingPT = To->getPointsToEdge()) {
        // The To node already has an outgoing pointsTo edge, so the only thing
        // we can do is to merge both content nodes.
        scheduleToMerge(ExistingPT, PT);
      } else {
        To->setPointsTo(PT);
      }
    }
    // Unlink the outgoing defer edges.
    for (CGNode *Defers : From->defersTo) {
      assert(Defers != From && "defer edge may not form a self-cycle");
      Defers->removeFromPreds(Predecessor(From, EdgeType::Defer));
    }
    // Redirect the incoming defer edges. This may trigger other node merges.
    for (Predecessor Pred : From->Preds) {
      CGNode *PredNode = Pred.getPointer();
      if (Pred.getInt() == EdgeType::Defer) {
        assert(PredNode != From && "defer edge may not form a self-cycle");
        addDeferEdge(PredNode, To);
      }
    }
    // Redirect the outgoing defer edges, which may also trigger other node
    // merges.
    for (CGNode *Defers : From->defersTo) {
      addDeferEdge(To, Defers);
    }

    // Ensure that graph invariance 4) is kept. At this point there may be still
    // some violations because of the new adjacent edges of the To node.
    for (Predecessor Pred : To->Preds) {
      if (Pred.getInt() == EdgeType::PointsTo) {
        CGNode *PredNode = Pred.getPointer();
        for (Predecessor PredOfPred : PredNode->Preds) {
          if (PredOfPred.getInt() == EdgeType::Defer)
            updatePointsTo(PredOfPred.getPointer(), To);
        }
      }
    }
    if (CGNode *ToPT = To->getPointsToEdge()) {
      for (CGNode *ToDef : To->defersTo) {
        updatePointsTo(ToDef, ToPT);
      }
      for (Predecessor Pred : To->Preds) {
        if (Pred.getInt() == EdgeType::Defer)
          updatePointsTo(Pred.getPointer(), ToPT);
      }
    }
    To->EscapeSet |= From->EscapeSet;

    // Cleanup the merged node.
    From->isMerged = true;
    From->Preds.clear();
    From->defersTo.clear();
    From->pointsTo = nullptr;
  }
}

void EscapeAnalysis::ConnectionGraph::
updatePointsTo(CGNode *InitialNode, CGNode *pointsTo) {
  // Visit all nodes in the defer web, which don't have the right pointsTo set.
  llvm::SmallVector<CGNode *, 8> WorkList;
  WorkList.push_back(InitialNode);
  InitialNode->isInWorkList = true;
  for (unsigned Idx = 0; Idx < WorkList.size(); ++Idx) {
    auto *Node = WorkList[Idx];
    if (Node->pointsTo == pointsTo)
      continue;

    if (Node->pointsTo) {
      // Mismatching: we need to merge!
      scheduleToMerge(Node->pointsTo, pointsTo);
    }

    // If the node already has a pointsTo _edge_ we don't change it (we don't
    // want to change the structure of the graph at this point).
    if (!Node->pointsToIsEdge) {
      if (Node->defersTo.empty()) {
        // This node is the end of a defer-edge path with no pointsTo connected.
        // We create an edge to pointsTo (agreed, this changes the structure of
        // the graph but adding this edge is harmless).
        Node->setPointsTo(pointsTo);
      } else {
        Node->pointsTo = pointsTo;
      }
    }

    // Add all adjacent nodes to the WorkList.
    for (auto *Defered : Node->defersTo) {
      if (!Defered->isInWorkList) {
        WorkList.push_back(Defered);
        Defered->isInWorkList = true;
      }
    }
    for (Predecessor Pred : Node->Preds) {
      if (Pred.getInt() == EdgeType::Defer) {
        CGNode *PredNode = Pred.getPointer();
        if (!PredNode->isInWorkList) {
          WorkList.push_back(PredNode);
          PredNode->isInWorkList = true;
        }
      }
    }
  }
  for (CGNode *Node : WorkList) {
    Node->isInWorkList = false;
  }
}

void EscapeAnalysis::ConnectionGraph::propagateEscapes() {
  bool Changed = false;
  do {
    Changed = false;

    for (CGNode *Node : Nodes) {
      // Propagate the bits to all successor nodes.
      if (Node->pointsTo) {
        Changed |= Node->pointsTo->mergeEscapeSet(Node);
      }
      for (CGNode *Def : Node->defersTo) {
        Changed |= Def->mergeEscapeSet(Node);
      }
    }
  } while (Changed);
}

bool EscapeAnalysis::ConnectionGraph::mergeCalleeGraph(FullApplySite FAS,
                                              ConnectionGraph *CalleeGraph) {
  // The main point of the merging algorithm is to map each content node in the
  // callee graph to a content node in this (caller) graph. This may require to
  // create new nodes or to merge existing nodes in the caller graph.
  CGNodeMap Callee2CallerMapping;
  llvm::SmallVector<CGNode *, 8> MappedNodes;

  // First map the callee parameters to the caller arguments.
  SILFunction *Callee = CalleeGraph->F;
  unsigned numCallerArgs = FAS.getNumArguments();
  unsigned numCalleeArgs = Callee->getArguments().size();
  assert(numCalleeArgs >= numCallerArgs);
  for (unsigned Idx = 0; Idx < numCalleeArgs; ++Idx) {
    // If there are more callee parameters than arguments it means that the
    // callee is the result of a partial_apply - a thick function. A thick
    // thick function also references the boxed partially applied arguments.
    // Therefore we map all the extra callee paramters to the callee operand
    // of the apply site.
    SILValue CallerArg = (Idx < numCallerArgs ? FAS.getArgument(Idx) :
                                                FAS.getCallee());
    if (CGNode *CalleeNd = CalleeGraph->getNode(Callee->getArgument(Idx))) {
      CGNode *CallerNd = getNode(CallerArg);
      assert(CallerNd);
      Callee2CallerMapping.insert(CalleeNd, CallerNd);
      MappedNodes.push_back(CalleeNd);
      CalleeNd->isInWorkList = true;
    }
  }

  // Map the return value.
  if (CalleeGraph->ReturnNode) {
    ValueBase *CallerReturnVal = nullptr;
    if (auto *TAI = dyn_cast<TryApplyInst>(FAS.getInstruction())) {
      CallerReturnVal = TAI->getNormalBB()->getBBArg(0);
    } else {
      CallerReturnVal = FAS.getInstruction();
    }
    CGNode *CallerRetNd = getNode(CallerReturnVal);
    assert(CallerRetNd);
    Callee2CallerMapping.insert(CalleeGraph->ReturnNode, CallerRetNd);
    MappedNodes.push_back(CalleeGraph->ReturnNode);
    CalleeGraph->ReturnNode->isInWorkList = true;
  }

  // First step: replicate the points-to edges and the content nodes in the
  // caller graph.
  bool Changed = false;
  bool NodesMerged;
  do {
    NodesMerged = false;
    for (unsigned Idx = 0; Idx < MappedNodes.size(); ++Idx) {
      CGNode *CalleeNd = MappedNodes[Idx];
      CGNode *CallerNd = Callee2CallerMapping.get(CalleeNd);
      assert(CallerNd);
      
      if (CalleeNd->EscapeSet.test(GlobalEscape)) {
        // We don't need to merge the callee subgraph of nodes which have the
        // global escaping state set.
        if (!CallerNd->EscapeSet.test(GlobalEscape)) {
          // Just set global escaping in the caller node and that's it.
          CallerNd->EscapeSet.set(GlobalEscape);
          Changed = true;
        }
        continue;
      }

      CGNode *CalleePT = CalleeNd->pointsTo;
      if (!CalleePT)
        continue;

      CGNode *MappedCallerPT = Callee2CallerMapping.get(CalleePT);
      if (!CallerNd->pointsTo) {
        // The following getContentNode() will create a new content node.
        Changed = true;
      }
      CGNode *CallerPT = getContentNode(CallerNd);
      if (MappedCallerPT) {
        // We already found the caller node through another path.
        if (CallerPT != MappedCallerPT) {
          // There are two content nodes in the caller which map to the same
          // content node in the callee -> we have to merge the caller nodes.
          scheduleToMerge(CallerPT, MappedCallerPT);
          mergeAllScheduledNodes();
          Changed = true;
          NodesMerged = true;
        }
      } else {
        // It's the first time we see the caller node, so we add it to the
        // mapping.
        Callee2CallerMapping.insert(CalleePT, CallerPT);
      }
      if (!CalleePT->isInWorkList) {
        MappedNodes.push_back(CalleePT);
        CalleePT->isInWorkList = true;
      }
    }
  } while (NodesMerged);

  for (CGNode *CalleeNd : MappedNodes) {
    CalleeNd->isInWorkList = false;
  }

  // Second step: add the callee's graph defer edges to the caller graph.
  for (CGNode *CalleeNd : MappedNodes) {
    Changed |= addDeferEdgesFromCallee(CalleeNd, Callee2CallerMapping);
  }
  return Changed;
}

bool EscapeAnalysis::ConnectionGraph::addDeferEdgesFromCallee(
                  CGNode *CalleeSource, const CGNodeMap &Callee2CallerMapping) {
  llvm::SmallVector<CGNode *, 8> WorkList;
  WorkList.push_back(CalleeSource);
  CalleeSource->isInWorkList = true;
  CGNode *Source = Callee2CallerMapping.get(CalleeSource);
  assert(Source && "node should have been merged to caller graph");
  
  // Collect all nodes which are reachable from the CalleeSource via a path
  // which only contains defer-edges.
  bool Changed = false;
  for (unsigned Idx = 0; Idx < WorkList.size(); ++Idx) {
    CGNode *CalleeNode = WorkList[Idx];
    CGNode *Node = Callee2CallerMapping.get(CalleeNode);
    // Create the edge in the caller graph. Note: this may trigger merging of
    // content nodes in the caller graph.
    if (Node)
      Changed |= defer(Source, Node);

    for (auto *Defered : CalleeNode->defersTo) {
      if (!Defered->isInWorkList) {
        WorkList.push_back(Defered);
        Defered->isInWorkList = true;
      }
    }
  }
  for (CGNode *CalleeNode : WorkList) {
    CalleeNode->isInWorkList = false;
  }
  return Changed;
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
    Defered
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

  const EscapeAnalysis::ConnectionGraph *OrigGraph;

  // The same IDs as the SILPrinter uses.
  llvm::DenseMap<const ValueBase *, unsigned> InstToIDMap;

  typedef std::vector<Node>::iterator iterator;
  typedef SmallVectorImpl<Node *>::iterator child_iterator;
};

CGForDotView::CGForDotView(const EscapeAnalysis::ConnectionGraph *CG) :
    OrigGraph(CG) {
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
      Nd.ChildrenTypes.push_back(Defered);
    }
  }
}

std::string CGForDotView::getNodeLabel(const Node *Node) const {
  std::string Label;
  llvm::raw_string_ostream O(Label);
  O << '%' << InstToIDMap.lookup(Node->OrigNode->V) << '\n';
  
  switch (Node->OrigNode->Type) {
    case swift::EscapeAnalysis::NodeType::Content:
      O << "content";
      break;
    case swift::EscapeAnalysis::NodeType::Return:
      O << "return";
      break;
    default: {
      std::string Inst;
      llvm::raw_string_ostream OI(Inst);
      SILValue(Node->OrigNode->V).print(OI);
      size_t start = Inst.find(" = ");
      if (start != std::string::npos) {
        start += 3;
      } else {
        start = 2;
      }
      O << Inst.substr(start, 20);
      break;
    }
  }
  if (!Node->OrigNode->matchPointToOfDefers()) {
    O << "\nPT mismatch: ";
    if (Node->OrigNode->pointsTo) {
      O << '%' << Node->Graph->InstToIDMap[Node->OrigNode->pointsTo->V];
    } else {
      O << "null";
    }
  }
  O.flush();
  return Label;
}

std::string CGForDotView::getNodeAttributes(const Node *Node) const {
  auto *Orig = Node->OrigNode;
  std::string attr;
  
  switch (Orig->Type) {
    case swift::EscapeAnalysis::NodeType::Content:
      attr = "style=\"rounded\"";
      break;
    case swift::EscapeAnalysis::NodeType::Argument:
    case swift::EscapeAnalysis::NodeType::Return:
      attr = "style=\"bold\"";
      break;
    default:
      break;
  }
  if (Orig->EscapeSet.any()) {
    if (!attr.empty())
      attr += ',';
    if (Orig->EscapeSet.test(EscapeAnalysis::GlobalEscape)) {
      attr += "color=\"red\"";
    } else {
      attr += "color=\"blue\"";
    }
  }
  return attr;
}

namespace llvm {


  /// GraphTraits specialization so the CGForDotView can be
  /// iterable by generic graph iterators.
  template <> struct GraphTraits<CGForDotView::Node *> {
    typedef CGForDotView::Node NodeType;
    typedef CGForDotView::child_iterator ChildIteratorType;

    static NodeType *getEntryNode(NodeType *N) { return N; }
    static inline ChildIteratorType child_begin(NodeType *N) {
      return N->Children.begin();
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return N->Children.end();
    }
  };

  template <> struct GraphTraits<CGForDotView *>
  : public GraphTraits<CGForDotView::Node *> {
    typedef CGForDotView *GraphType;

    static NodeType *getEntryNode(GraphType F) { return nullptr; }

    typedef CGForDotView::iterator nodes_iterator;
    static nodes_iterator nodes_begin(GraphType OCG) {
      return OCG->Nodes.begin();
    }
    static nodes_iterator nodes_end(GraphType OCG) { return OCG->Nodes.end(); }
    static unsigned size(GraphType CG) { return CG->Nodes.size(); }
  };

  /// This is everything the llvm::GraphWriter needs to write the call graph in
  /// a dot file.
  template <>
  struct DOTGraphTraits<CGForDotView *> : public DefaultDOTGraphTraits {

    DOTGraphTraits(bool isSimple = false) : DefaultDOTGraphTraits(isSimple) {}

    static std::string getGraphName(const CGForDotView *Graph) {
      return "CG for " + Graph->OrigGraph->getFunction()->getName().str();
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
        case CGForDotView::Defered: return "color=\"gray\"";
      }
    }
  };
} // end llvm namespace

#endif

void EscapeAnalysis::ConnectionGraph::viewCG() const {
  /// When asserts are disabled, this should be a NoOp.
#ifndef NDEBUG
  CGForDotView CGDot(this);
  llvm::ViewGraph(&CGDot, "connection-graph");
#endif
}

void EscapeAnalysis::CGNode::dump() const {
  llvm::errs() << getTypeStr() << ": " << *V;
  if (mergeTo) {
    llvm::errs() << "   -> merged to ";
    mergeTo->dump();
  }
}

const char *EscapeAnalysis::CGNode::getTypeStr() const {
  switch (Type) {
    case NodeType::Value:    return "Val";
    case NodeType::Content:  return "Con";
    case NodeType::Argument: return "Arg";
    case NodeType::Return:   return "Ret";
  }
}


void EscapeAnalysis::ConnectionGraph::dump() const {
  print(llvm::errs());
}

void EscapeAnalysis::ConnectionGraph::print(llvm::raw_ostream &OS) const {
#ifndef NDEBUG
  OS << "CG of " << F->getName() << '\n';

  if (!Valid) {
    OS << "  <invalid>\n";
    return;
  }

  // Assign the same IDs to SILValues as the SILPrinter does.
  llvm::DenseMap<const ValueBase *, unsigned> InstToIDMap;
  F->numberValues(InstToIDMap);

  // Assign consecutive subindices for nodes which map to the same value.
  llvm::DenseMap<const ValueBase *, unsigned> NumSubindicesPerValue;
  llvm::DenseMap<CGNode *, unsigned> Node2Subindex;
  std::vector<CGNode *> SortedNodes;
  SortedNodes.reserve(Nodes.size());
  for (CGNode *Nd : Nodes) {
    if (!Nd->isMerged) {
      unsigned &Idx = NumSubindicesPerValue[Nd->V];
      Node2Subindex[Nd] = Idx++;
      SortedNodes.push_back(Nd);
    }
  }
  // Sort by SILValue ID+Subindex. To make the output somehow consistent with
  // the output of the function's SIL.
  std::sort(SortedNodes.begin(), SortedNodes.end(),
    [&](CGNode *Nd1, CGNode *Nd2) -> bool {
      unsigned VIdx1 = InstToIDMap[Nd1->V];
      unsigned VIdx2 = InstToIDMap[Nd2->V];
      if (VIdx1 != VIdx2)
        return VIdx1 < VIdx2;
      return Node2Subindex[Nd1] < Node2Subindex[Nd2];
    });

  auto NodeStr = [&](CGNode *Nd) -> std::string {
    std::string Str;
    llvm::raw_string_ostream OS(Str);
    OS << '%' << InstToIDMap[Nd->V];
    unsigned Idx = Node2Subindex[Nd];
    if (Idx != 0)
      OS << '.' << Idx;
    OS.flush();
    return Str;
  };

  for (CGNode *Nd : SortedNodes) {
    OS << "  " << Nd->getTypeStr() << ' ' << NodeStr(Nd) << " Esc: ";
    const char *Separator = "";
    for (unsigned Bit = 0; Bit < Nd->EscapeSet.size(); Bit++) {
      if (Nd->EscapeSet.test(Bit)) {
        switch (Bit) {
          case GlobalEscape: OS << Separator << 'G'; break;
          case ReturnEscape: OS << Separator << 'R'; break;
          default: OS << Separator << 'A' << (Bit - FirstArg); break;
        }
        OS << ", ";
      }
    }
    OS << "Succ: ";
    Separator = "";
    if (CGNode *PT = Nd->getPointsToEdge()) {
      OS << '(' << NodeStr(PT) << ')';
      Separator = ", ";
    }
    for (CGNode *Def : Nd->defersTo) {
      OS << Separator << NodeStr(Def);
      Separator = ", ";
    }
    OS << '\n';
  }
  OS << "End\n";
#endif
}

void EscapeAnalysis::ConnectionGraph::verify() const {
#ifndef NDEBUG
  verifyStructure();

  // Check graph invariance 4)
  for (CGNode *Nd : Nodes) {
    assert(Nd->matchPointToOfDefers());
  }
#endif
}

void EscapeAnalysis::ConnectionGraph::verifyStructure() const {
#ifndef NDEBUG
  if (!Valid) {
    assert(Nodes.empty());
    assert(Values2Nodes.empty());
    assert(!ReturnNode);
    return;
  }
  for (CGNode *Nd : Nodes) {
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
      CGNode *PredNode = Pred.getPointer();
      if (Pred.getInt() == EdgeType::Defer) {
        assert(PredNode->findDefered(Nd) != PredNode->defersTo.end());
      } else {
        assert(Pred.getInt() == EdgeType::PointsTo);
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

void EscapeAnalysis::initialize(SILPassManager *PM) {
  CGA = PM->getAnalysis<CallGraphAnalysis>();
}

/// Returns true if we need to add defer edges for the arguments of a block.
static bool linkBBArgs(SILBasicBlock *BB) {
  // Don't need to handle function arguments.
  if (BB == &BB->getParent()->front())
    return false;
  // We don't need to link to the try_apply's normal result argument, because
  // we handle it separatly in setAllEscaping() and mergeCalleeGraph().
  if (SILBasicBlock *SinglePred = BB->getSinglePredecessor()) {
    auto *TAI = dyn_cast<TryApplyInst>(SinglePred->getTerminator());
    if (TAI && BB == TAI->getNormalBB())
      return false;
  }
  return true;
}

void EscapeAnalysis::buildConnectionGraph(SILFunction *F,
                                          ConnectionGraph *ConGraph) {
  ConGraph->setValid();
  for (auto &BB : *F) {
    if (linkBBArgs(&BB)) {
      // Create defer-edges from the block arguments to it's values in the
      // predecessor's terminator instructions.
      for (SILArgument *BBArg : BB.getBBArgs()) {
        CGNode *ArgNode = ConGraph->getNode(BBArg);
        if (!ArgNode)
          continue;

        llvm::SmallVector<SILValue,4> Incoming;
        if (!BBArg->getIncomingValues(Incoming)) {
          // We don't know where the block argument comes from -> treat it
          // conservatively.
          ArgNode->setEscapesGlobal();
          continue;
        }

        for (SILValue Src : Incoming) {
          CGNode *SrcArg = ConGraph->getNode(Src);
          if (SrcArg) {
            ConGraph->defer(ArgNode, SrcArg);
          } else {
            ArgNode->setEscapesGlobal();
            break;
          }
        }
      }
    }

    // Now create edges for the instructions.
    for (auto &I : BB) {
      analyzeInstruction(&I, ConGraph);
    }
  }
  ConGraph->propagateEscapes();
}

/// Returns true if all called functions from an apply site are known and not
/// external.
static bool allCalleeFunctionsVisible(FullApplySite FAS, CallGraph &CG) {
  auto Callees = CG.getCallees(FAS);
  if (Callees.canCallUnknownFunction())
    return false;

  for (SILFunction *Callee : Callees) {
    if (Callee->isExternalDeclaration())
      return false;
  }
  return true;
}

void EscapeAnalysis::analyzeInstruction(SILInstruction *I,
                                        ConnectionGraph *ConGraph) {
  FullApplySite FAS = FullApplySite::isa(I);
  if (FAS && allCalleeFunctionsVisible(FAS, CGA->getCallGraph())) {
    // We handle this apply site afterwards by merging the callee graph(s) into
    // the caller graph.
    ConGraph->addKnownCallee(FAS);
    return;
  }
  if (isProjection(I))
    return;

  switch (I->getKind()) {
    case ValueKind::AllocStackInst:
    case ValueKind::AllocRefInst:
    case ValueKind::AllocBoxInst:
    case ValueKind::DeallocStackInst:
    case ValueKind::StrongRetainInst:
    case ValueKind::StrongRetainUnownedInst:
    case ValueKind::RetainValueInst:
    case ValueKind::UnownedRetainInst:
    case ValueKind::BranchInst:
    case ValueKind::CondBranchInst:
    case ValueKind::SwitchEnumInst:
    case ValueKind::DebugValueInst:
    case ValueKind::FunctionRefInst:
      // These instructions don't have any effect on escaping.
      return;
    case ValueKind::StrongReleaseInst:
    case ValueKind::ReleaseValueInst:
    case ValueKind::UnownedReleaseInst:
      if (CGNode *AddrNode = ConGraph->getNode(I->getOperand(0))) {
        // A release instruction may deallocate the pointer operand. This may
        // capture any content of the released object, but not the pointer to
        // the object itself (because it will be a dangling pointer after
        // deallocation).
        CGNode *CapturedByDeinit = ConGraph->getContentNode(AddrNode);
        SILType ReleasedType = I->getOperand(0).getType();
        if (ReleasedType.is<SILBoxType>() || ReleasedType.is<SILFunctionType>()) {
          // The deinit of a box or thick function does not capture anything,
          // but may call the deinit of the boxed value. So we can go down
          // one pointsTo-level again.
          CapturedByDeinit = ConGraph->getContentNode(CapturedByDeinit);
        }
        CapturedByDeinit->setEscapesGlobal();
      }
      return;
    case ValueKind::LoadInst:
      if (isPointer(I, ConGraph->getFunction()->getModule())) {
        CGNode *AddrNode = ConGraph->getNode(cast<LoadInst>(I)->getOperand());
        CGNode *PointsTo = ConGraph->getContentNode(AddrNode);
        if (CGNode *ValueNode = ConGraph->getNodeOrNull(I)) {
          // We already have a node for the loaded value. Create a defer edge
          // from the loaded value to the content.
          ConGraph->defer(ValueNode, PointsTo);
        } else {
          // No need for a separate node for the load instruction.
          ConGraph->setNode(I, PointsTo);
        }
      }
      return;
    case ValueKind::StoreInst:
      if (CGNode *ValueNode = ConGraph->getNode(cast<StoreInst>(I)->getSrc())) {
        // Create a defer-edge from the content to the stored value.
        CGNode *AddrNode = ConGraph->getNode(cast<StoreInst>(I)->getDest());
        CGNode *PointsTo = ConGraph->getContentNode(AddrNode);
        ConGraph->defer(PointsTo, ValueNode);
      }
      return;
    case ValueKind::PartialApplyInst: {
      // The result of a partial_apply is a thick function which stores the
      // boxed partial applied arguments. We create defer-edges from the
      // partial_apply values to the arguments.
      CGNode *ResultNode = ConGraph->getNode(I);
      assert(ResultNode && "thick functions must have a CG node");
      for (const Operand &Op : I->getAllOperands()) {
        if (CGNode *ArgNode = ConGraph->getNode(Op.get())) {
          ConGraph->defer(ResultNode, ArgNode);
        }
      }
      return;
    }
    case ValueKind::StructInst:
    case ValueKind::TupleInst:
    case ValueKind::EnumInst: {
      // Aggregate composition is like assigning the aggregate fields to the
      // resulting aggregate value.
      CGNode *ResultNode = ConGraph->getNodeOrNull(I);
      for (const Operand &Op : I->getAllOperands()) {
        if (CGNode *FieldNode = ConGraph->getNode(Op.get())) {
          if (!ResultNode) {
            // A small optimization to reduce the graph size: we re-use the
            // first field node as result node.
            ConGraph->setNode(I, FieldNode);
            ResultNode = FieldNode;
            assert(isPointer(I, ConGraph->getFunction()->getModule()));
          } else {
            ConGraph->defer(ResultNode, FieldNode);
          }
        }
      }
      return;
    }
    case ValueKind::ReturnInst:
      if (CGNode *ValueNd = ConGraph->getNode(cast<ReturnInst>(I)->getOperand())) {
        ConGraph->defer(ConGraph->getReturnNode(cast<ReturnInst>(I)),
                                 ValueNd);
      }
      return;
    default:
      // We handle all other instructions conservatively.
      setAllEscaping(I, ConGraph);
      return;
  }
}

bool EscapeAnalysis::setAllEscaping(SILInstruction *I,
                                    ConnectionGraph *ConGraph) {
  bool Changed = false;
  if (auto *TAI = dyn_cast<TryApplyInst>(I)) {
    Changed |= ConGraph->setEscapesGlobal(TAI->getNormalBB()->getBBArg(0));
    Changed |= ConGraph->setEscapesGlobal(TAI->getErrorBB()->getBBArg(0));
  }
  // Even if the instruction does not write memory we conservatively set all
  // operands to escaping, because they may "escape" to the result value in
  // an unspecified way. For example consider bit-casting a pointer to an int.
  // In this case we don't even create a node for the resulting int value.
  for (const Operand &Op : I->getAllOperands()) {
    Changed |= ConGraph->setEscapesGlobal(Op.get());
  }
  // Even if the instruction does not write memory it could e.g. return the
  // address of global memory. Therefore we have to define it as escaping.
  Changed |= ConGraph->setEscapesGlobal(I);
  return Changed;
}

void EscapeAnalysis::recompute() {

  // Did anything change since the last recompuation? (Probably yes)
  if (!shouldRecompute)
    return;

  DEBUG(llvm::dbgs() << "recompute escape analysis\n");

  Function2ConGraph.clear();
  Allocator.DestroyAll();
  shouldRecompute = false;

  CallGraph &CG = CGA->getOrBuildCallGraph();
  auto BottomUpFunctions = CG.getBottomUpFunctionOrder();
  std::vector<ConnectionGraph *> Graphs;
  Graphs.reserve(BottomUpFunctions.size());

  // First step: create the initial connection graphs for all functions.
  for (SILFunction *F : BottomUpFunctions) {
    if (F->isExternalDeclaration())
      continue;

    DEBUG(llvm::dbgs() << "  build initial graph for " << F->getName() << '\n');

    auto *ConGraph = getConnectionGraph(F);
    buildConnectionGraph(F, ConGraph);

    Graphs.push_back(ConGraph);
    ConGraph->setNeedMergeCallees();
  }

  // Second step: propagate the connection graphs up the call tree until it
  // stabalizes.
  int Iteration = 0;
  bool Changed;
  do {
    DEBUG(llvm::dbgs() << "iteration " << Iteration << '\n');
    Changed = false;
    for (ConnectionGraph *ConGraph : Graphs) {
      if (!ConGraph->handleMergeCallees())
        continue;

      // Limit the total number of iterations. First to limit compile time,
      // second to make sure that the loop terminates. Theoretically this
      // should alwasy be the case, but who knows?
      if (Iteration < MaxGraphMerges) {
        DEBUG(llvm::dbgs() << "  merge into " <<
              ConGraph->getFunction()->getName() << '\n');

        if (mergeAllCallees(ConGraph, CG)) {
          // Trigger another graph merging action in all caller functions.
          auto &CallerSet = CG.getCallerEdges(ConGraph->getFunction());
          for (CallGraphEdge *CallerEdge : CallerSet) {
            if (!CallerEdge->canCallUnknownFunction()) {
              SILFunction *Caller = CallerEdge->getApply().getFunction();
              ConnectionGraph *CallerCG = Function2ConGraph[Caller];
              CallerCG->setNeedMergeCallees();
            }
          }
          Changed = true;
        }
      } else {
        DEBUG(llvm::dbgs() << "  finalize " <<
              ConGraph->getFunction()->getName() << '\n');

        finalizeGraphConservatively(ConGraph);
      }
    }
    Iteration++;
  } while (Changed);

  verify();
}

bool EscapeAnalysis::mergeAllCallees(ConnectionGraph *ConGraph, CallGraph &CG) {
  bool Changed = false;
  for (FullApplySite FAS : ConGraph->getKnownCallees()) {
    auto Callees = CG.getCallees(FAS);
    assert(!Callees.canCallUnknownFunction() &&
           "knownCallees should not contain an unknown function");
    for (SILFunction *Callee : Callees) {
      DEBUG(llvm::dbgs() << "    callee " << Callee->getName() << '\n');
      ConnectionGraph *CalleeCG = Function2ConGraph[Callee];
      // TODO: just assert CalleeCG != nullptr when rdar://problem/23157111 is
      // fixed.
      // TODO: Handle self-cycles in the call graph. We can't merge a graph to
      // itself. All we have to do is to copy the graph before we merge it.
      if (CalleeCG && CalleeCG != ConGraph) {
        Changed |= ConGraph->mergeCalleeGraph(FAS, CalleeCG);
      } else {
        Changed |= setAllEscaping(FAS.getInstruction(), ConGraph);
        break;
      }
    }
  }

  if (Changed) {
    ConGraph->propagateEscapes();
    return true;
  }
  return false;
}

void EscapeAnalysis::finalizeGraphConservatively(ConnectionGraph *ConGraph) {
  for (FullApplySite FAS : ConGraph->getKnownCallees()) {
    setAllEscaping(FAS.getInstruction(), ConGraph);
  }
  ConGraph->propagateEscapes();
}

SILAnalysis *swift::createEscapeAnalysis(SILModule *M) {
  return new EscapeAnalysis(M);
}
