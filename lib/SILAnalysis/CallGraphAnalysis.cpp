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
#include "swift/SILPasses/Utils/Local.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/DemangleWrappers.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <utility>

using namespace swift;

#define DEBUG_TYPE "call-graph"

STATISTIC(NumCallGraphNodes, "# of call graph nodes created");
STATISTIC(NumAppliesWithEdges, "# of call sites with edges");
STATISTIC(NumCallGraphsBuilt, "# of times the call graph is built");

llvm::cl::opt<bool> DumpCallGraph("sil-dump-call-graph",
                                  llvm::cl::init(false), llvm::cl::Hidden);

llvm::cl::opt<bool> DumpCallGraphStats("sil-dump-call-graph-stats",
                                       llvm::cl::init(false), llvm::cl::Hidden);

CallGraph::CallGraph(SILModule *Mod, bool completeModule)
  : M(*Mod), NodeOrdinal(0), EdgeOrdinal(0) {
  ++NumCallGraphsBuilt;

  // Create a call graph node for each function in the module and add
  // each to a worklist of functions to process.
  std::vector<SILFunction *> Workitems;
  for (auto &F : M) {
    addCallGraphNode(&F);

    if (F.isDefinition())
      Workitems.push_back(&F);
  }

  // Now compute the sets of call graph nodes any given class method
  // decl could target.
  computeClassMethodCallees();

  // Add edges for each function in the worklist. We capture the
  // initial functions in the module up-front into this worklist
  // because the process of adding edges can deseralize new functions,
  // at which point we process those functions (adding edges), and it
  // would be an error to process those functions again when we come
  // across them in the module.
  for (auto I = Workitems.rbegin(), E = Workitems.rend(); I != E; ++I) {
    addEdges(*I);
  }

  if (DumpCallGraph)
    dump();

  if (DumpCallGraphStats)
    dumpStats();
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

/// Update the callee set for each method of a given class, along with
/// all the overridden methods from superclasses.
void CallGraph::computeClassMethodCalleesForClass(ClassDecl *CD) {
  for (auto *Member : CD->getMembers()) {
    auto *AFD = dyn_cast<AbstractFunctionDecl>(Member);
    if (!AFD)
      continue;

    auto Method = SILDeclRef(AFD);
    auto *CalledFn = M.lookUpFunctionInVTable(CD, Method);
    if (!CalledFn)
      continue;

    // FIXME: Link in external declarations, at least for complete call sets.

    auto *Node = getOrAddCallGraphNode(CalledFn);

    // Update the callee sets for this method and all the methods it
    // overrides by inserting the call graph node for the function
    // that this method invokes.
    do {
      CallGraphEdge::CalleeSetType *CalleeSet;
      bool Complete;

      std::tie(CalleeSet, Complete) = getOrCreateCalleeSetForClassMethod(Method);
      assert(CalleeSet && "Unexpected null callee set!");

      CalleeSet->insert(Node);
      Method = Method.getOverriddenVTableEntry();
    } while (Method);
  }
}

/// Incrementally compute the callees for each method of each class
/// that we have a VTable for.
void CallGraph::computeClassMethodCallees() {
  for (auto &VTable : M.getVTableList())
    computeClassMethodCalleesForClass(VTable.getClass());
}

CallGraphNode *CallGraph::addCallGraphNode(SILFunction *F) {
  // TODO: Compute this from the call graph itself after stripping
  //       unreachable nodes from graph.
  ++NumCallGraphNodes;

  auto *Node = new (Allocator) CallGraphNode(F, ++NodeOrdinal);

  assert(!FunctionToNodeMap.count(F) &&
         "Added function already has a call graph node!");

  FunctionToNodeMap[F] = Node;

  // TODO: Only add functions clearly visible from outside our
  //       compilation scope as roots.
  if (F->isDefinition())
    CallGraphRoots.push_back(Node);

  return Node;
}

std::pair<CallGraphEdge::CalleeSetType *, bool>
CallGraph::tryGetCalleeSetForClassMethod(SILDeclRef Decl) {
  auto *AFD = Decl.getAbstractFunctionDecl();
  assert(AFD && "Expected abstract function decl!");

  auto Found = CalleeSets.find(AFD);
  if (Found == CalleeSets.end())
    return std::make_pair(nullptr, false);

  // FIXME: Determine whether the set is complete.
  return std::make_pair(Found->second, false);
}

std::pair<CallGraphEdge::CalleeSetType *, bool>
CallGraph::getOrCreateCalleeSetForClassMethod(SILDeclRef Decl) {
  auto *AFD = cast<AbstractFunctionDecl>(Decl.getDecl());
  auto Found = CalleeSets.find(AFD);
  if (Found != CalleeSets.end())
    return std::make_pair(Found->second, false);

  auto *CalleeSet = new (Allocator) CallGraphEdge::CalleeSetType;
  CalleeSets.insert(std::make_pair(AFD, CalleeSet));

  // FIXME: Determine whether the set is complete.
  return std::make_pair(CalleeSet, false);
}


CallGraphEdge *CallGraph::makeCallGraphEdgeForCallee(FullApplySite Apply,
                                                     SILValue Callee) {
  switch (Callee->getKind()) {
  case ValueKind::ThinToThickFunctionInst:
    Callee = cast<ThinToThickFunctionInst>(Callee)->getOperand();
    SWIFT_FALLTHROUGH;
  case ValueKind::FunctionRefInst: {
    auto *CalleeFn = cast<FunctionRefInst>(Callee)->getReferencedFunction();
    if (CalleeFn->isExternalDeclaration())
      M.linkFunction(CalleeFn, SILModule::LinkingMode::LinkAll,
                     CallGraphLinkerEditor(*this).getCallback());

    auto *CalleeNode = getOrAddCallGraphNode(CalleeFn);
    return new (Allocator) CallGraphEdge(Apply, CalleeNode, EdgeOrdinal++);
  }

  case ValueKind::PartialApplyInst: {
    Callee = cast<PartialApplyInst>(Callee)->getCallee();
    return makeCallGraphEdgeForCallee(Apply, Callee);
  }

  case ValueKind::DynamicMethodInst:
    // TODO: Decide how to handle these in graph construction and
    //       analysis passes. We might just leave them out of the
    //       graph.
    return new (Allocator) CallGraphEdge(Apply, EdgeOrdinal++);

  case ValueKind::SILArgument:
    // First-pass call-graph construction will not do anything with
    // these, but a second pass can potentially statically determine
    // the called function in some cases.
    return new (Allocator) CallGraphEdge(Apply, EdgeOrdinal++);

  case ValueKind::ApplyInst:
  case ValueKind::TryApplyInst:
    // TODO: Probably not worth iterating invocation- then
    //       reverse-invocation order to catch this.
    return new (Allocator) CallGraphEdge(Apply, EdgeOrdinal++);

  case ValueKind::TupleExtractInst:
    // TODO: It would be good to tunnel through extracts so that we
    //       can build a more accurate call graph prior to any
    //       optimizations.
    return new (Allocator) CallGraphEdge(Apply, EdgeOrdinal++);

  case ValueKind::StructExtractInst:
    // TODO: It would be good to tunnel through extracts so that we
    //       can build a more accurate call graph prior to any
    //       optimizations.
    return new (Allocator) CallGraphEdge(Apply, EdgeOrdinal++);

  case ValueKind::WitnessMethodInst: {
    auto *WMI = cast<WitnessMethodInst>(Callee);
    SILFunction *CalleeFn;
    ArrayRef<Substitution> Subs;
    SILWitnessTable *WT;

    std::tie(CalleeFn, WT, Subs) =
      WMI->getModule().lookUpFunctionInWitnessTable(WMI->getConformance(),
                                                    WMI->getMember());

    if (!CalleeFn)
      return new (Allocator) CallGraphEdge(Apply, EdgeOrdinal++);

    if (CalleeFn->isExternalDeclaration())
      M.linkFunction(CalleeFn, SILModule::LinkingMode::LinkAll,
                     CallGraphLinkerEditor(*this).getCallback());

    auto *CalleeNode = getOrAddCallGraphNode(CalleeFn);
    return new (Allocator) CallGraphEdge(Apply, CalleeNode, EdgeOrdinal++);
  }

  case ValueKind::ClassMethodInst: {
    auto *CMI = cast<ClassMethodInst>(Callee);

    CallGraphEdge::CalleeSetType *CalleeSet;
    bool Complete;

    std::tie(CalleeSet, Complete) =
      tryGetCalleeSetForClassMethod(CMI->getMember());

    // FIXME: Review the cases where we are not currently generating
    //        callee sets.
    if (!CalleeSet)
      return new (Allocator) CallGraphEdge(Apply, EdgeOrdinal++);

    return new (Allocator) CallGraphEdge(Apply, CalleeSet, Complete,
                                         EdgeOrdinal++);
  }

  case ValueKind::SuperMethodInst:
    return new (Allocator) CallGraphEdge(Apply, EdgeOrdinal++);

  default:
    assert(!isa<MethodInst>(Callee) &&
           "Unhandled method instruction in call graph construction!");
    return new (Allocator) CallGraphEdge(Apply, EdgeOrdinal++);
  }
}

static void orderEdges(const llvm::SmallPtrSetImpl<CallGraphEdge *> &Edges,
                       llvm::SmallVectorImpl<CallGraphEdge *> &OrderedEdges) {
  for (auto *Edge : Edges)
    OrderedEdges.push_back(Edge);

  std::sort(OrderedEdges.begin(), OrderedEdges.end(),
            [](CallGraphEdge *left, CallGraphEdge *right) {
              return left->getOrdinal() < right->getOrdinal();
            });
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

void CallGraph::addEdgesForApply(FullApplySite Apply,
                                 CallGraphNode *CallerNode) {
  CallerNode->MayBindDynamicSelf |=
    hasDynamicSelfTypes(Apply.getSubstitutions());

  auto *Edge = makeCallGraphEdgeForCallee(Apply, Apply.getCallee());
  assert(Edge && "Expected to be able to make call graph edge for callee!");
  assert(!ApplyToEdgeMap.count(Apply) &&
         "Added apply that already has an edge node!\n");
  ApplyToEdgeMap[Apply] = Edge;
  CallerNode->addCalleeEdge(Edge);

  llvm::SmallVector<CallGraphNode *, 4> OrderedNodes;
  orderCallees(Edge->getPartialCalleeSet(), OrderedNodes);

  for (auto *CalleeNode : OrderedNodes)
    CalleeNode->addCallerEdge(Edge);

  // TODO: Compute this from the call graph itself after stripping
  //       unreachable nodes from graph.
  ++NumAppliesWithEdges;
}

void CallGraph::removeEdge(CallGraphEdge *Edge) {
  // Remove the edge from all the potential callee call graph nodes.
  auto CalleeSet = Edge->getPartialCalleeSet();
  for (auto *CalleeNode : CalleeSet)
    CalleeNode->removeCallerEdge(Edge);

  // Remove the edge from the caller's call graph node.
  auto Apply = Edge->getApply();
  auto *CallerNode = getCallGraphNode(Apply.getFunction());
  CallerNode->removeCalleeEdge(Edge);

  // Remove the mapping from the apply to this edge.
  ApplyToEdgeMap.erase(Apply);

  // Call the destructor for the edge. The memory will be reclaimed
  // when the call graph is deleted by virtue of the bump pointer
  // allocator.
  Edge->~CallGraphEdge();
}

// Remove the call graph edges associated with an apply, where the
// apply is known to the call graph.
void CallGraph::removeEdgesForApply(FullApplySite AI) {
  assert(ApplyToEdgeMap.count(AI) && "Expected apply to be in edge map!");
  removeEdge(ApplyToEdgeMap[AI]);
}

void CallGraph::markCallerEdgesOfCalleesIncomplete(FullApplySite AI) {
  auto *Edge = getCallGraphEdge(AI);

  // We are not guaranteed to have an edge for every apply.
  if (!Edge)
    return;

  for (auto *Node : Edge->getPartialCalleeSet())
    Node->markCallerEdgesIncomplete();
}

void CallGraph::addEdges(SILFunction *F) {
  auto *CallerNode = getOrAddCallGraphNode(F);

  for (auto &BB : *F) {
    for (auto &I : BB) {
      if (auto AI = FullApplySite::isa(&I))
        addEdgesForApply(AI, CallerNode);

      auto *FRI = dyn_cast<FunctionRefInst>(&I);
      if (!FRI)
        continue;

      auto *CalleeFn = FRI->getReferencedFunction();

      if (CalleeFn->isExternalDeclaration())
        M.linkFunction(CalleeFn, SILModule::LinkingMode::LinkAll,
                       CallGraphLinkerEditor(*this).getCallback());

      if (CalleeFn->isPossiblyUsedExternally()) {
        auto *CalleeNode = tryGetCallGraphNode(CalleeFn);
        assert((!CalleeNode || !CalleeNode->isCallerEdgesComplete()) &&
               "Expected function to have incomplete set of caller edges!");
        (void) CalleeNode;
        continue;
      }

      bool hasAllApplyUsers =
        std::none_of(FRI->use_begin(), FRI->use_end(),
                     [](Operand *Op) {
                       return !FullApplySite::isa(Op->getUser());
                     });

      // If we have a non-apply user of this function, mark its caller set
      // as being incomplete.
      if (!hasAllApplyUsers) {
        auto *CalleeNode = getOrAddCallGraphNode(CalleeFn);
        CalleeNode->markCallerEdgesIncomplete();
      }
    }
  }
}

static void indent(llvm::raw_ostream &OS, int Indent) {
  std::string Blanks(Indent, ' ');
  OS << Blanks;
}

static void printFlag(llvm::raw_ostream &OS,
                      const char *Description, bool Value, int Indent =0) {
  indent(OS, Indent);
  OS << Description << ": " << (Value ? "yes\n" : "no\n");
}

void CallGraphEdge::print(llvm::raw_ostream &OS, int Indent) {
  indent(OS, Indent);
  OS << "Call site #" << Ordinal << ": ";
  OS << *getApply().getInstruction();

  printFlag(OS, "All callees known", isCalleeSetComplete(), Indent);

  auto CalleeSet = getPartialCalleeSet();
  if (CalleeSet.empty())
    return;

  llvm::SmallVector<CallGraphNode *, 4> OrderedNodes;
  orderCallees(CalleeSet, OrderedNodes);

  bool First = true;
  indent(OS, Indent);
  OS << "Known callees:\n";
  for (auto *Callee : OrderedNodes) {
    if (!First)
      OS << "\n";
    First = false;
    auto Name = Callee->getFunction()->getName();
    indent(OS, Indent + 2);
    OS << "Name: " << Name << "\n";
    indent(OS, Indent + 2);
    OS << "Demangled: " <<
      demangle_wrappers::demangleSymbolAsString(Name) << "\n";
  }
}

void CallGraphEdge::dump(int Indent) {
#ifndef NDEBUG
  print(llvm::errs(), Indent);
#endif
}

void CallGraphEdge::dump() {
#ifndef NDEBUG
  dump(0);
#endif
}


void CallGraphNode::print(llvm::raw_ostream &OS) {
  OS << "Function #" << Ordinal << ": " <<
    getFunction()->getName() << "\n";
  OS << "Demangled: " <<
    demangle_wrappers::demangleSymbolAsString(getFunction()->getName()) << "\n";
  printFlag(OS, "Trivially dead", isDead());
  printFlag(OS, "All callers known", isCallerEdgesComplete());
  printFlag(OS, "Binds self", mayBindDynamicSelf());

  auto &Edges = getCalleeEdges();
  if (Edges.empty())
    return;

  llvm::SmallVector<CallGraphEdge *, 8> OrderedEdges;
  orderEdges(Edges, OrderedEdges);

  for (auto *Edge : OrderedEdges) {
    OS << "\n";
    Edge->print(OS, /* Indent= */ 2);
  }
  OS << "\n";
}

void CallGraphNode::dump() {
#ifndef NDEBUG
  print(llvm::errs());
#endif
}

void CallGraph::print(llvm::raw_ostream &OS) {
#ifndef NDEBUG
  OS << "*** Call Graph ***\n";

  auto const &Funcs = getBottomUpFunctionOrder();
  for (auto *F : Funcs) {
    auto *Node = getCallGraphNode(F);
    if (Node)
      Node->print(OS);
    else
      OS << "!!! Missing node for " << F->getName() << "!!!";
    OS << "\n";
  }
#endif
}

void CallGraph::dump() {
#ifndef NDEBUG
  print(llvm::errs());
#endif
}

namespace {

template<int NumBuckets>
struct Histogram {
  unsigned Data[NumBuckets];

  Histogram() {
    for (auto i = 0; i < NumBuckets; ++i) {
      Data[i] = 0;
    }
  }

  void increment(unsigned Bucket) {
    Bucket = Bucket < NumBuckets ? Bucket : NumBuckets - 1;
    Data[Bucket]++;
  }

  void print(llvm::raw_ostream &OS) {
    auto Last = NumBuckets - 1;
    for (auto i = 0; i < NumBuckets; ++i) {
      auto *Separator = Last == i ? "+: " : ": ";
      if (Data[i])
        OS << i << Separator << Data[i] << "\n";
    }
    OS << "\n";
  }
};

} // end anonymous namespace

void CallGraph::printStats(llvm::raw_ostream &OS) {
  Histogram<256> CallSitesPerFunction;
  Histogram<256> CallersPerFunction;
  Histogram<256> CalleesPerCallSite;
  unsigned CountNodes = 0;
  unsigned CountCallSites = 0;

  auto const &Funcs = getBottomUpFunctionOrder();
  for (auto *F : Funcs) {
    ++CountNodes;
    auto *Node = getCallGraphNode(F);
    if (Node) {
      CallSitesPerFunction.increment(Node->getCalleeEdges().size());
      CallersPerFunction.increment(Node->getPartialCallerEdges().size());

      CountCallSites += Node->getCalleeEdges().size();

      for (auto *Edge : Node->getCalleeEdges())
        CalleesPerCallSite.increment(Edge->getPartialCalleeSet().size());
    } else {
      OS << "!!! Missing node for " << F->getName() << "!!!";
    }
  }

  OS << "*** Call Graph Statistics ***\n";

  OS << "Number of call graph nodes: " << CountNodes << "\n";
  OS << "Number of call graph edges: " << CountCallSites << "\n";

  OS << "Histogram of number of call sites per function:\n";
  CallSitesPerFunction.print(OS);

  OS << "Histogram of number of callees per call site:\n";
  CalleesPerCallSite.print(OS);

  OS << "Histogram of number of callers per function:\n";
  CallersPerFunction.print(OS);

  OS << "Bump pointer allocated memory (bytes): " <<
    Allocator.getTotalMemory() << "\n";
}

void CallGraph::dumpStats() {
#ifndef NDEBUG
  printStats(llvm::errs());
#endif
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

    llvm::SmallVector<CallGraphEdge *, 4> OrderedEdges;
    orderEdges(Node->getCalleeEdges(), OrderedEdges);

    for (auto *ApplyEdge : OrderedEdges) {
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

  // For every pair (SILFunction, CallGraphNode) in the
  // function-to-node map, verify:
  //
  //    a. The function is in the current module.
  //    b. The call graph node is for that same function.
  //    c. All the callee edges of the node have an apply that lives
  //       in that function.
  //
  for (auto &P : FunctionToNodeMap) {
    assert(Functions.count(P.first) &&
           "Function in call graph but not in module!?");
    assert(P.second->getFunction() == P.first &&
           "Func mapped to node, but node has different Function inside?!");
    for (CallGraphEdge *Edge : P.second->getCalleeEdges()) {
      assert(Edge->getApply().getFunction() == P.first &&
             "Apply in callee set that is not in the callee function?!");
    }
  }

  // For every pair (FullApplySite, CallGraphEdge) in the apply-to-edge
  // map, verify:
  //
  //    a. The edge's apply is identical to the map key that maps
  //       to the edge.
  //    b. The apply is in a function in the module.
  //    c. That function has a call graph node.
  //    d. The edge is one of the callee edges of that call graph node.
  //
  for (auto &P : ApplyToEdgeMap) {
    assert(P.second->getApply() == P.first &&
           "Apply mapped to CallSiteEdge but not vis-a-versa?!");
    assert(Functions.count(P.first.getFunction()) &&
           "Apply in func not in module?!");
    CallGraphNode *Node = getCallGraphNode(P.first.getFunction());

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

void CallGraphEditor::replaceApplyWithNew(FullApplySite Old,
                                          FullApplySite New) {
  if (auto *Edge = CG.getCallGraphEdge(Old))
    CG.removeEdge(Edge);

  CG.addEdgesForApply(New);
}

void CallGraphEditor::replaceApplyWithNew(FullApplySite Old,
                             llvm::SmallVectorImpl<FullApplySite> &NewApplies) {
  if (auto *Edge = CG.getCallGraphEdge(Old))
    CG.removeEdge(Edge);

  for (auto NewApply : NewApplies)
    CG.addEdgesForApply(NewApply);
}

void CallGraphAnalysis::verify() const {
#ifndef NDEBUG
  // If we don't have a callgraph, return.
  if (!CG)
    return;
  CG->verify();
#endif
}

//===----------------------------------------------------------------------===//
//                          View CG Implementation
//===----------------------------------------------------------------------===//

#ifndef NDEBUG

/// Another representation of the call graph using sorted vectors instead of
/// sets. Used for viewing the callgraph as dot file with llvm::ViewGraph.
struct OrderedCallGraph {
  
  struct Node;
  
  struct Edge {
    Edge(CallGraphEdge *CGEdge, Node *Child) : CGEdge(CGEdge), Child(Child) { }
    CallGraphEdge *CGEdge;
    Node *Child;
  };
  
  struct Node {
    CallGraphNode *CGNode;
    OrderedCallGraph *OCG;
    int NumCallSites = 0;
    SmallVector<Edge, 8> Children;
  };
  
  struct child_iterator : public std::iterator<std::random_access_iterator_tag,
                                               Node *, ptrdiff_t> {
    SmallVectorImpl<Edge>::iterator baseIter;
    
    child_iterator(SmallVectorImpl<Edge>::iterator baseIter) :
      baseIter(baseIter)
    { }
    
    child_iterator &operator++() { baseIter++; return *this; }
    child_iterator operator++(int) { auto tmp = *this; baseIter++; return tmp; }
    Node *operator*() const { return baseIter->Child; }
    bool operator==(const child_iterator &RHS) const {
      return baseIter == RHS.baseIter;
    }
    bool operator!=(const child_iterator &RHS) const {
      return baseIter != RHS.baseIter;
    }
    difference_type operator-(const child_iterator &RHS) const {
      return baseIter - RHS.baseIter;
    }
  };
  
  OrderedCallGraph(CallGraph *CG);
  
  CallGraph *CG;
  std::vector<Node> Nodes;

  /// The SILValue IDs which are printed as edge source labels.
  llvm::DenseMap<const ValueBase *, unsigned> InstToIDMap;

  typedef std::vector<Node>::iterator iterator;
};

OrderedCallGraph::OrderedCallGraph(CallGraph *CG) {
  auto const &Funcs = CG->getBottomUpFunctionOrder();
  Nodes.resize(Funcs.size());
  llvm::DenseMap<CallGraphNode *, Node *> NodeMap;
  int idx = 0;
  for (auto *F : Funcs) {
    auto *CGNode = CG->getCallGraphNode(F);
    Node &ONode = Nodes[idx++];
    ONode.CGNode = CGNode;
    ONode.OCG = this;
    NodeMap[CGNode] = &ONode;

    F->numberValues(InstToIDMap);
  }

  for (Node &ONode : Nodes) {
    llvm::SmallVector<CallGraphEdge *, 8> OrderedEdges;
    orderEdges(ONode.CGNode->getCalleeEdges(), OrderedEdges);
    
    ONode.NumCallSites = OrderedEdges.size();
    for (auto *CGEdge : OrderedEdges) {
      llvm::SmallVector<CallGraphNode *, 4> OrderedCallees;
      orderCallees(CGEdge->getPartialCalleeSet(), OrderedCallees);

      for (auto *CalleeNode : OrderedCallees) {
        auto *OrderedChild = NodeMap[CalleeNode];
        assert(OrderedChild);
        ONode.Children.push_back(Edge(CGEdge, OrderedChild));
      }
    }
  }
}

namespace llvm {
  
  /// Wraps a dot node label string to multiple lines. The \p NumEdgeLabels
  /// gives an estimate on the minimum width of the node shape.
  static void wrap(std::string &Str, int NumEdgeLabels) {
    unsigned ColNum = 0;
    unsigned LastSpace = 0;
    unsigned MaxColumns = std::max(60, NumEdgeLabels * 8);
    for (unsigned i = 0; i != Str.length(); ++i) {
      if (ColNum == MaxColumns) {
        if (!LastSpace)
          LastSpace = i;
        Str.insert(LastSpace + 1, "\\l");
        ColNum = i - LastSpace - 1;
        LastSpace = 0;
      } else
        ++ColNum;
      if (Str[i] == ' ' || Str[i] == '.')
        LastSpace = i;
    }
  }
  
  /// CallGraph GraphTraits specialization so the CallGraph can be
  /// iterable by generic graph iterators.
  template <> struct GraphTraits<OrderedCallGraph::Node *> {
    typedef OrderedCallGraph::Node NodeType;
    typedef OrderedCallGraph::child_iterator ChildIteratorType;
    
    static NodeType *getEntryNode(NodeType *N) { return N; }
    static inline ChildIteratorType child_begin(NodeType *N) {
      return N->Children.begin();
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return N->Children.end();
    }
  };

  template <> struct GraphTraits<OrderedCallGraph *>
  : public GraphTraits<OrderedCallGraph::Node *> {
    typedef OrderedCallGraph *GraphType;
    
    static NodeType *getEntryNode(GraphType F) { return nullptr; }
    
    typedef OrderedCallGraph::iterator nodes_iterator;
    static nodes_iterator nodes_begin(GraphType OCG) {
      return OCG->Nodes.begin();
    }
    static nodes_iterator nodes_end(GraphType OCG) { return OCG->Nodes.end(); }
    static unsigned size(GraphType CG) { return CG->Nodes.size(); }
  };

  /// This is everything the llvm::GraphWriter needs to write the call graph in
  /// a dot file.
  template <>
  struct DOTGraphTraits<OrderedCallGraph *> : public DefaultDOTGraphTraits {
    
    DOTGraphTraits(bool isSimple = false) : DefaultDOTGraphTraits(isSimple) {}
    
    std::string getNodeLabel(const OrderedCallGraph::Node *Node,
                             const OrderedCallGraph *Graph) {
      SILFunction *F = Node->CGNode->getFunction();
      std::string Label = F->getName();
      wrap(Label, Node->NumCallSites);
      return Label;
    }
    
    std::string getNodeDescription(const OrderedCallGraph::Node *Node,
                             const OrderedCallGraph *Graph) {
      SILFunction *F = Node->CGNode->getFunction();
      std::string Label = demangle_wrappers::
                          demangleSymbolAsString(F->getName());
      wrap(Label, Node->NumCallSites);
      return Label;
    }
    
    static std::string getEdgeSourceLabel(const OrderedCallGraph::Node *Node,
                                          OrderedCallGraph::child_iterator I) {
      std::string Label;
      raw_string_ostream O(Label);
      SILInstruction *Inst = I.baseIter->CGEdge->getApply().getInstruction();
      O << '%' << Node->OCG->InstToIDMap[Inst];
      return Label;
    }
    
    static std::string getEdgeAttributes(const OrderedCallGraph::Node *Node,
                                         OrderedCallGraph::child_iterator I,
                                         const OrderedCallGraph *Graph) {
      CallGraphEdge *Edge = I.baseIter->CGEdge;
      if (!Edge->isCalleeSetComplete())
        return "color=\"red\"";
      return "";
    }
  };
} // end llvm namespace
#endif

void CallGraph::viewCG() {
  /// When asserts are disabled, this should be a NoOp.
#ifndef NDEBUG
  OrderedCallGraph OCG(this);
  llvm::ViewGraph(&OCG, "callgraph");
#endif
}

