//===------ CallGraph.cpp - The Call Graph Data Structure ----*- C++ -*----===//
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

#include "swift/SILAnalysis/CallGraph.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/DemangleWrappers.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILWitnessTable.h"
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
  computeMethodCallees();

  // Add edges for each function in the worklist. We capture the
  // initial functions in the module up-front into this worklist
  // because the process of adding edges can deseralize new functions,
  // at which point we process those functions (adding edges), and it
  // would be an error to process those functions again when we come
  // across them in the module.
  for (auto I = Workitems.begin(), E = Workitems.end(); I != E; ++I)
    addEdges(*I);

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
  for (auto &P : InstToEdgeMap) {
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

    bool canCallUnknown = !calleesAreStaticallyKnowable(M, Method);

    // Update the callee sets for this method and all the methods it
    // overrides by inserting the call graph node for the function
    // that this method invokes.
    do {
      auto &TheCalleeSet = getOrCreateCalleeSetForMethod(Method);
      assert(TheCalleeSet.getPointer() && "Unexpected null callee set!");

      TheCalleeSet.getPointer()->insert(Node);
      if (canCallUnknown)
        TheCalleeSet.setInt(true);

      Method = Method.getOverriddenVTableEntry();
    } while (Method);
  }
}

void
CallGraph::computeWitnessMethodCalleesForWitnessTable(SILWitnessTable &WTable) {
  for (const SILWitnessTable::Entry &Entry : WTable.getEntries()) {
    if (Entry.getKind() != SILWitnessTable::Method)
      continue;

    auto &WitnessEntry = Entry.getMethodWitness();
    auto Requirement = WitnessEntry.Requirement;
    auto *WitnessFn = WitnessEntry.Witness;
    // Dead function elimination nulls out entries for functions it removes.
    if (!WitnessFn)
      continue;

    auto *Node = getOrAddCallGraphNode(WitnessFn);

    auto &TheCalleeSet = getOrCreateCalleeSetForMethod(Requirement);
    assert(TheCalleeSet.getPointer() && "Unexpected null callee set!");

    TheCalleeSet.getPointer()->insert(Node);

    // FIXME: For now, conservatively assume that unknown functions
    //        can be called from any witness_method call site.
    TheCalleeSet.setInt(true);
  }
}

/// Remove a function from all the callee sets. This should be called
/// on any function that we'll eventually remove.
// FIXME: Consider adding a reverse mapping from call graph node to
//        the sets it appears in.
void CallGraph::removeFunctionFromCalleeSets(SILFunction *F) {
  auto *Node = getCallGraphNode(F);

  for (auto I = CalleeSetCache.begin(), E = CalleeSetCache.end();
       I != E; ++I) {
    auto *Callees = I->second.getPointer();

    Callees->remove(Node);
  }
}

/// Compute the callees for each method that appears in a VTable or
/// Witness Table.
void CallGraph::computeMethodCallees() {
  // Remove contents of old callee sets - in case we are updating the sets.
  for (auto Iter : CalleeSetCache) {
    auto *TheCalleeSet = Iter.second.getPointer();
    Iter.second.setInt(false);
    TheCalleeSet->clear();
  }

  for (auto &VTable : M.getVTableList())
    computeClassMethodCalleesForClass(VTable.getClass());

  for (auto &WTable : M.getWitnessTableList())
    computeWitnessMethodCalleesForWitnessTable(WTable);
}

CallGraphNode *CallGraph::addCallGraphNode(SILFunction *F) {
  // TODO: Compute this from the call graph itself after stripping
  //       unreachable nodes from graph.
  ++NumCallGraphNodes;

  auto *Node = new (Allocator) CallGraphNode(F, ++NodeOrdinal);

  assert(!FunctionToNodeMap.count(F) &&
         "Added function already has a call graph node!");

  FunctionToNodeMap[F] = Node;

  return Node;
}

CallGraphEdge::CalleeSet &
CallGraph::getOrCreateCalleeSetForMethod(SILDeclRef Decl) {
  auto *AFD = cast<AbstractFunctionDecl>(Decl.getDecl());
  auto Found = CalleeSetCache.find(AFD);
  if (Found != CalleeSetCache.end())
    return Found->second;

  auto *NodeSet = new (Allocator) CallGraphEdge::CallGraphNodeSet;

  bool canCallUnknown = !calleesAreStaticallyKnowable(M, Decl);

  // Allocate a new callee set, and for now just assume it can call
  // unknown functions.
  CallGraphEdge::CalleeSet TheCalleeSet(NodeSet, canCallUnknown);

  bool Inserted;
  CalleeSetMap::iterator It;
  std::tie(It, Inserted) =
    CalleeSetCache.insert(std::make_pair(AFD, TheCalleeSet));
  assert(Inserted && "Expected new entry to be inserted!");

  return It->second;
}

SILFunction *
CallGraphEdge::Callees::getFunctionFromNode(CallGraphNode *const &Node) {
  return Node->getFunction();
}

CallGraphEdge *CallGraph::makeCallGraphEdgeForCallee(FullApplySite Apply,
                                                     SILValue Callee) {
  switch (Callee->getKind()) {
  case ValueKind::ThinToThickFunctionInst:
    Callee = cast<ThinToThickFunctionInst>(Callee)->getOperand();
    SWIFT_FALLTHROUGH;
  case ValueKind::FunctionRefInst: {
    auto *CalleeFn = cast<FunctionRefInst>(Callee)->getReferencedFunction();

    auto *CalleeNode = getOrAddCallGraphNode(CalleeFn);
    return new (Allocator) CallGraphEdge(Apply.getInstruction(), CalleeNode,
                                         EdgeOrdinal++);
  }

  case ValueKind::PartialApplyInst: {
    Callee = cast<PartialApplyInst>(Callee)->getCallee();
    return makeCallGraphEdgeForCallee(Apply, Callee);
  }

  case ValueKind::DynamicMethodInst:
    // TODO: Decide how to handle these in graph construction and
    //       analysis passes. We might just leave them out of the
    //       graph.
    return new (Allocator) CallGraphEdge(Apply.getInstruction(), EdgeOrdinal++);

  case ValueKind::SILArgument:
    // First-pass call-graph construction will not do anything with
    // these, but a second pass can potentially statically determine
    // the called function in some cases.
    return new (Allocator) CallGraphEdge(Apply.getInstruction(), EdgeOrdinal++);

  case ValueKind::ApplyInst:
  case ValueKind::TryApplyInst:
    // TODO: Probably not worth iterating invocation- then
    //       reverse-invocation order to catch this.
    return new (Allocator) CallGraphEdge(Apply.getInstruction(), EdgeOrdinal++);

  case ValueKind::TupleExtractInst:
    // TODO: It would be good to tunnel through extracts so that we
    //       can build a more accurate call graph prior to any
    //       optimizations.
    return new (Allocator) CallGraphEdge(Apply.getInstruction(), EdgeOrdinal++);

  case ValueKind::StructExtractInst:
    // TODO: It would be good to tunnel through extracts so that we
    //       can build a more accurate call graph prior to any
    //       optimizations.
    return new (Allocator) CallGraphEdge(Apply.getInstruction(), EdgeOrdinal++);

  case ValueKind::WitnessMethodInst: {
    auto *WMI = cast<WitnessMethodInst>(Callee);
    SILFunction *CalleeFn;
    ArrayRef<Substitution> Subs;
    SILWitnessTable *WT;

    // Attempt to find a specific callee for the given conformance and member.
    std::tie(CalleeFn, WT, Subs) =
      WMI->getModule().lookUpFunctionInWitnessTable(WMI->getConformance(),
                                                    WMI->getMember());

    if (CalleeFn) {
      auto *CalleeNode = getOrAddCallGraphNode(CalleeFn);
      return new (Allocator) CallGraphEdge(Apply.getInstruction(), CalleeNode,
                                           EdgeOrdinal++);
    }

    // Lookup the previously computed callee set if we didn't find a
    // specific callee.
    auto *AFD = cast<AbstractFunctionDecl>(WMI->getMember().getDecl());

    auto Found = CalleeSetCache.find(AFD);
    if (Found != CalleeSetCache.end())
      return new (Allocator) CallGraphEdge(Apply.getInstruction(),
                                           Found->second, EdgeOrdinal++);

    // Otherwise default to an edge with unknown callees.
    return new (Allocator) CallGraphEdge(Apply.getInstruction(), EdgeOrdinal++);
  }

  case ValueKind::ClassMethodInst: {
    auto *CMI = cast<ClassMethodInst>(Callee);
    auto *AFD = cast<AbstractFunctionDecl>(CMI->getMember().getDecl());

    auto Found = CalleeSetCache.find(AFD);
    if (Found != CalleeSetCache.end())
      return new (Allocator) CallGraphEdge(Apply.getInstruction(),
                                           Found->second, EdgeOrdinal++);

    // We don't know the callees in cases where the method isn't
    // present in a vtable, so create a call graph edge with no known
    // callees, assumed to be able to call unknown functions.
    return new (Allocator) CallGraphEdge(Apply.getInstruction(), EdgeOrdinal++);
  }

  case ValueKind::SuperMethodInst:
    return new (Allocator) CallGraphEdge(Apply.getInstruction(), EdgeOrdinal++);

  default:
    assert(!isa<MethodInst>(Callee) &&
           "Unhandled method instruction in call graph construction!");
    return new (Allocator) CallGraphEdge(Apply.getInstruction(), EdgeOrdinal++);
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

void CallGraph::addEdgesForInstruction(SILInstruction *I,
                                       CallGraphNode *CallerNode) {
  auto Apply = FullApplySite::isa(I);

  // TODO: Support non-apply instructions.
  if (!Apply)
    return;

  auto *Edge = makeCallGraphEdgeForCallee(Apply, Apply.getCallee());
  assert(Edge && "Expected to be able to make call graph edge for callee!");
  assert(!InstToEdgeMap.count(Apply.getInstruction()) &&
         "Added apply that already has an edge node!\n");
  InstToEdgeMap[Apply.getInstruction()] = Edge;
  CallerNode->addCalleeEdge(Edge);

  for (auto *CalleeNode : Edge->getCalleeSet())
    CalleeNode->addCallerEdge(Edge);

  // TODO: Compute this from the call graph itself after stripping
  //       unreachable nodes from graph.
  ++NumAppliesWithEdges;
}

void CallGraph::removeEdgeFromFunction(CallGraphEdge *Edge, SILFunction *F) {
  // Remove the edge from all the potential callee call graph nodes.
  auto CalleeSet = Edge->getCalleeSet();
  for (auto *CalleeNode : CalleeSet)
    CalleeNode->removeCallerEdge(Edge);

  // Remove the edge from the caller's call graph node.
  auto *CallerNode = getCallGraphNode(F);
  CallerNode->removeCalleeEdge(Edge);

  // Remove the mapping from the apply to this edge.
  auto Apply = Edge->getInstruction();
  InstToEdgeMap.erase(Apply);

  // Call the destructor for the edge. The memory will be reclaimed
  // when the call graph is deleted by virtue of the bump pointer
  // allocator.
  Edge->~CallGraphEdge();
}

void CallGraph::removeNode(CallGraphNode *Node) {
  assert(Node->getCallerEdges().size() == 0 &&
         "Node to delete must not have any caller edges");
  assert(Node->getCalleeEdges().size() == 0 &&
         "Node to delete must not have any callee edges");

  // Avoid keeping a dangling node pointers in the vectors.
  // Next time they will be computed from scratch.
  BottomUpFunctionOrder.clear();
  clearBottomUpSCCOrder();

  FunctionToNodeMap.erase(Node->getFunction());

  // Call the destructor for the node. The memory will be reclaimed
  // when the call graph is deleted by virtue of the bump pointer
  // allocator.
  Node->~CallGraphNode();
}

// Remove the call graph edges associated with an apply, where the
// apply is known to the call graph.
void CallGraph::removeEdgesForInstruction(SILInstruction *I) {
  assert(InstToEdgeMap.count(I) && "Expected apply to be in edge map!");
  removeEdgeFromFunction(InstToEdgeMap[I], I->getFunction());
}

void CallGraph::addEdges(SILFunction *F) {
  auto *CallerNode = getOrAddCallGraphNode(F);

  for (auto &BB : *F) {
    for (auto &I : BB) {
      if (FullApplySite::isa(&I))
        addEdgesForInstruction(&I, CallerNode);

      auto *FRI = dyn_cast<FunctionRefInst>(&I);
      if (!FRI)
        continue;

      auto *CalleeFn = FRI->getReferencedFunction();

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

// Print check lines prior to call graph output, and also print the
// function bodies. This can be used to minimize the effort in
// creating new call graph test cases.
static llvm::cl::opt<std::string>
CallGraphFileCheckPrefix("call-graph-file-check-prefix", llvm::cl::init(""),
                   llvm::cl::desc("Print a FileCheck prefix before each line"));

static void indent(llvm::raw_ostream &OS, int Indent) {
  if (!CallGraphFileCheckPrefix.empty()) return;

  std::string Blanks(Indent, ' ');
  OS << Blanks;
}

static void printFlag(llvm::raw_ostream &OS,
                      const char *Description, bool Value, int Indent =0) {
  indent(OS, Indent);
  OS << CallGraphFileCheckPrefix << Description << ": " <<
    (Value ? "yes\n" : "no\n");
}

void CallGraphEdge::print(llvm::raw_ostream &OS, int Indent) const {
  indent(OS, Indent);
  OS << CallGraphFileCheckPrefix << "Call site #" << Ordinal << ": ";
  OS << *getInstruction();

  printFlag(OS, "Unknown callees", canCallUnknownFunction(), Indent);

  if (getCalleeSet().empty())
    return;

  bool First = true;
  indent(OS, Indent);
  OS << CallGraphFileCheckPrefix << "Known callees:\n";
  for (auto *Callee : getCalleeSet()) {
    if (!First)
      OS << "\n";
    First = false;
    auto Name = Callee->getFunction()->getName();
    indent(OS, Indent + 2);
    OS << CallGraphFileCheckPrefix << "Name: " << Name << "\n";
    indent(OS, Indent + 2);
    OS << CallGraphFileCheckPrefix << "Demangled: " <<
      demangle_wrappers::demangleSymbolAsString(Name) << "\n";
  }
}

void CallGraphEdge::dump(int Indent) const {
#ifndef NDEBUG
  print(llvm::errs(), Indent);
#endif
}

void CallGraphEdge::dump() const {
#ifndef NDEBUG
  dump(0);
#endif
}


void CallGraphNode::print(llvm::raw_ostream &OS) const {
  OS << CallGraphFileCheckPrefix << "Function #" << Ordinal << ": " <<
    getFunction()->getName() << "\n";
  OS << CallGraphFileCheckPrefix << "Demangled: " <<
    demangle_wrappers::demangleSymbolAsString(getFunction()->getName()) << "\n";
  printFlag(OS, "Trivially dead", isTriviallyDead());
  printFlag(OS, "All callers known", isCallerEdgesComplete());

  auto &CalleeEdges = getCalleeEdges();
  if (!CalleeEdges.empty()) {
    OS << CallGraphFileCheckPrefix << "Call sites:\n";

    llvm::SmallVector<CallGraphEdge *, 8> OrderedCalleeEdges;
    orderEdges(CalleeEdges, OrderedCalleeEdges);

    for (auto *Edge : OrderedCalleeEdges) {
      OS << "\n";
      Edge->print(OS, /* Indent= */ 2);
    }
    OS << "\n";
  }

  auto &CallerEdges = getCallerEdges();
  if (!CallerEdges.empty()) {
    OS << CallGraphFileCheckPrefix <<
      (!isCallerEdgesComplete() ? "Known " : "");
    OS << "Callers:\n";

    llvm::SmallVector<CallGraphEdge *, 8> OrderedCallerEdges;
    orderEdges(CallerEdges, OrderedCallerEdges);

    llvm::SetVector<SILFunction *> Callers;

    for (auto *Edge : OrderedCallerEdges)
      Callers.insert(Edge->getInstruction()->getFunction());

    for (auto *Caller : Callers) {
      OS << "\n";
      indent(OS, 2);
      OS << CallGraphFileCheckPrefix << "Name: " << Caller->getName() << "\n";
      indent(OS, 2);
      OS << CallGraphFileCheckPrefix << "Demangled: " <<
        demangle_wrappers::demangleSymbolAsString(Caller->getName()) << "\n";
    }
    OS << "\n";
  }

  if (!CallGraphFileCheckPrefix.empty())
    getFunction()->print(OS);
}

void CallGraphNode::dump() const {
#ifndef NDEBUG
  print(llvm::errs());
#endif
}

void CallGraph::print(llvm::raw_ostream &OS) {
  OS << CallGraphFileCheckPrefix << "*** Call Graph ***\n";

  auto const &Funcs = getBottomUpFunctionOrder();
  for (auto *F : Funcs) {
    auto *Node = getCallGraphNode(F);
    if (Node)
      Node->print(OS);
    else
      OS << "!!! Missing node for " << F->getName() << "!!!";
    OS << "\n";
  }
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
        OS << CallGraphFileCheckPrefix << i << Separator << Data[i] << "\n";
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
      CallersPerFunction.increment(Node->getCallerEdges().size());

      CountCallSites += Node->getCalleeEdges().size();

      for (auto *Edge : Node->getCalleeEdges())
        CalleesPerCallSite.increment(Edge->getCalleeSet().size());
    } else {
      OS << "!!! Missing node for " << F->getName() << "!!!";
    }
  }

  OS << CallGraphFileCheckPrefix << "*** Call Graph Statistics ***\n";

  OS << CallGraphFileCheckPrefix << "Number of call graph nodes: " <<
    CountNodes << "\n";
  OS << CallGraphFileCheckPrefix << "Number of call graph edges: " <<
    CountCallSites << "\n";

  OS << CallGraphFileCheckPrefix <<
    "Histogram of number of call sites per function:\n";
  CallSitesPerFunction.print(OS);

  OS << CallGraphFileCheckPrefix <<
    "Histogram of number of callees per call site:\n";
  CalleesPerCallSite.print(OS);

  OS << CallGraphFileCheckPrefix <<
    "Histogram of number of callers per function:\n";
  CallersPerFunction.print(OS);

  OS << CallGraphFileCheckPrefix << "Bump pointer allocated memory (bytes): " <<
    Allocator.getTotalMemory() << "\n";

  OS << CallGraphFileCheckPrefix << "Number of callee sets allocated: " <<
    CalleeSetCache.size() << "\n";
}

void CallGraph::dumpStats() {
#ifndef NDEBUG
  printStats(llvm::errs());
#endif
}

namespace {

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
      for (auto *CalleeNode : ApplyEdge->getCalleeSet()) {
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
        SCC->SCCNodes.push_back(Popped->getFunction());
      } while (Popped != Node);

      TheSCCs.push_back(SCC);
    }
  }
};

} // end anonymous namespace

void CallGraph::clearBottomUpSCCOrder() {
  for (auto *SCC : BottomUpSCCOrder)
    SCC->~CallGraphSCC();
  BottomUpSCCOrder.clear();
}

void CallGraph::computeBottomUpSCCOrder() {
  if (!BottomUpSCCOrder.empty()) {
    clearBottomUpSCCOrder();
  }

  CallGraphSCCFinder SCCFinder(BottomUpSCCOrder, Allocator);
  for (auto &F : M) {
    if (F.isDefinition()) {
      auto *Node = getCallGraphNode(&F);
      SCCFinder.DFS(Node);
    }
  }
}

void CallGraph::computeBottomUpFunctionOrder() {
  // We do not need to call any destructors here.
  BottomUpFunctionOrder.clear();

  computeBottomUpSCCOrder();

  for (auto *SCC : BottomUpSCCOrder)
    for (auto *Fn : SCC->SCCNodes)
      BottomUpFunctionOrder.push_back(Fn);
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
  //
  // In addition, call the verify method for the function.
  unsigned numEdges = 0;
  for (auto &P : FunctionToNodeMap) {
    SILFunction *F = P.first;
    CallGraphNode *Node = P.second;
    assert(Functions.count(F) &&
           "Function in call graph but not in module!?");
    assert(Node->getFunction() == F &&
           "Func mapped to node, but node has different Function inside?!");
    verify(F);
    numEdges += Node->getCalleeEdges().size();
  }

  assert(InstToEdgeMap.size() == numEdges &&
         "Some edges in InstToEdgeMap are not contained in any node");

  // Verify the callee sets.
  for (auto Iter : CalleeSetCache) {
    auto *CalleeSet = Iter.second.getPointer();
    for (CallGraphNode *Node : *CalleeSet) {
      SILFunction *F = Node->getFunction();
      assert(tryGetCallGraphNode(F) &&
             "Callee set contains dangling node poiners");
    }
  }
#endif
}

void CallGraph::verify(SILFunction *F) const {
#ifndef NDEBUG
  // Collect all full apply sites of the function.

  auto *Node = getCallGraphNode(F);
  unsigned numEdges = 0;

  for (auto &BB : *F) {
    for (auto &I : BB) {
      auto FAS = FullApplySite::isa(&I);
      if (!FAS)
        continue;

      auto *Edge = getCallGraphEdge(FAS.getInstruction());

      numEdges++;

      assert(Edge->getInstruction() == &I &&
             "Edge is not linked to the correct apply site");

      assert(InstToEdgeMap.lookup(FAS.getInstruction()) == Edge &&
             "Edge is not in InstToEdgeMap");

      if (!Edge->canCallUnknownFunction()) {
        // In the trivial case that we call a known function, check if we have
        // exactly one callee in the edge.
        SILValue Callee = FAS.getCallee();
        if (auto *PAI = dyn_cast<PartialApplyInst>(Callee))
          Callee = PAI->getCallee();

        if (auto *FRI = dyn_cast<FunctionRefInst>(Callee)) {
          auto *CalleeNode = Edge->getSingleCalleeOrNull();
          assert(CalleeNode &&
                 CalleeNode->getFunction() == FRI->getReferencedFunction() &&
                 "Direct apply is not represented by a single-callee edge");
        }
      }
    }
  }

  // Check if we have an exact 1-to-1 mapping from full apply sites to edges.
  auto &CalleeEdges = Node->getCalleeEdges();
  assert(numEdges == CalleeEdges.size() && "More edges than full apply sites");

  // Make structural graph checks:
  // 1.) Check that the callee edges are part of the callee node's caller set.
  for (auto *Edge : CalleeEdges) {
    assert(Edge->getInstruction()->getFunction() == F &&
           "Apply in callee set that is not in the callee function?!");

    for (auto *CalleeNode : Edge->getCalleeSet()) {
      auto &CallerEdges = CalleeNode->getCallerEdges();
      assert(std::find(CallerEdges.begin(), CallerEdges.end(), Edge) !=
               CallerEdges.end() &&
             "Edge not in caller set of callee");
    }
  }
  // 2.) Check that the caller edges have this node in their callee sets.
  for (auto *Edge : Node->getCallerEdges()) {
    auto CalleeSet = Edge->getCalleeSet();
    assert(std::find(CalleeSet.begin(), CalleeSet.end(), Node) !=
             CalleeSet.end() &&
           "Node not in callee set of caller edge");
  }
#endif
}

//===----------------------------------------------------------------------===//
//                          View CG Implementation
//===----------------------------------------------------------------------===//

#ifndef NDEBUG

namespace swift {

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
      for (auto *CalleeNode : CGEdge->getCalleeSet()) {
        auto *OrderedChild = NodeMap[CalleeNode];
        assert(OrderedChild);
        ONode.Children.push_back(Edge(CGEdge, OrderedChild));
      }
    }
  }
}

} // end swift namespace

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
      SILInstruction *Inst = I.baseIter->CGEdge->getInstruction();
      O << '%' << Node->OCG->InstToIDMap[Inst];
      return Label;
    }

    static std::string getEdgeAttributes(const OrderedCallGraph::Node *Node,
                                         OrderedCallGraph::child_iterator I,
                                         const OrderedCallGraph *Graph) {
      CallGraphEdge *Edge = I.baseIter->CGEdge;
      if (Edge->canCallUnknownFunction())
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
