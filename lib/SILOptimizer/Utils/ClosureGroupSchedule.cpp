//===--- ClosureGroupSchedule.cpp - Closure groups ------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/ClosureGroupSchedule.h"
#include "swift/AST/DeclContext.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/Test.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/Analysis/ClosureScope.h"
#include "swift/SILOptimizer/Analysis/FunctionOrder.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/IntEqClasses.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/ADT/STLExtras.h"
#include <utility>
#include <vector>

using namespace swift;

namespace {
/// A call graph over closure components (ClosureGroupSchedule.h): a node per
/// component, an edge for each call into another component, and a root with
/// an edge to every component.
/// Satisfies the requirements of llvm::scc_iterator.
struct ComponentGraph {
  struct Node {
    llvm::SmallVector<Node *, 4> callees;
  };
  /// One node per component, at its component number, plus the root at the end
  std::vector<Node> nodes;

  /// \p componentOf maps each bottomUp index to its component number.
  ComponentGraph(llvm::ArrayRef<std::pair<unsigned, unsigned>> callEdges,
                 llvm::ArrayRef<unsigned> componentOf, unsigned numComponents)
      : nodes(numComponents + 1) {
    // A call inside a component is dropped.
    for (auto [caller, callee] : callEdges) {
      unsigned from = componentOf[caller], to = componentOf[callee];
      if (from != to)
        nodes[from].callees.push_back(&nodes[to]);
    }

    // The root calls the components in their leaders' bottom-up order, which
    // makes the DFS deterministic.
    for (unsigned c = 0; c != numComponents; ++c)
      getRoot()->callees.push_back(&nodes[c]);
  }

  Node *getRoot() { return &nodes.back(); }
};
} // end anonymous namespace

namespace llvm {
/// Lets llvm::scc_iterator walk a ComponentGraph from its root along callees.
template <>
struct GraphTraits<ComponentGraph *> {
  using NodeRef = ComponentGraph::Node *;
  using ChildIteratorType = SmallVectorImpl<NodeRef>::iterator;

  static NodeRef getEntryNode(ComponentGraph *graph) {
    return graph->getRoot();
  }
  static ChildIteratorType child_begin(NodeRef node) {
    return node->callees.begin();
  }
  static ChildIteratorType child_end(NodeRef node) {
    return node->callees.end();
  }
};
} // end namespace llvm

/// If \p ref is a closure link, returns the function it refers to, otherwise
/// returns null. Function must be a definition, must not be a thunk, and
/// must have one of two markers of a closure or local function:
///   - An argument is a closure capture. SILGen marks every kind of capture.
///   - Its declaration is nested in a local context. This covers closures and
///     local functions that capture nothing.
/// Thunks are excluded so that a shared one doesn't link unrelated functions.
/// Hand-written SIL needn't mark its captures; capture edges join the
/// functions it passes a box or an address to.
static SILFunction *getLinkedClosure(FunctionRefBaseInst *ref) {
  SILFunction *f = ref->getInitiallyReferencedFunction();
  if (!f || !f->isDefinition() || f->isThunk() != IsNotThunk)
    return nullptr;
  // A function's own context is always local, so test the parent.
  if (DeclContext *dc = f->getDeclContext())
    if (DeclContext *parent = dc->getParent())
      if (parent->isLocalContext())
        return f;
  if (llvm::any_of(f->getArguments(), [](SILArgument *arg) {
        return cast<SILFunctionArgument>(arg)->isClosureCapture();
      }))
    return f;
  return nullptr;
}

/// Scans every definition once. Each closure link (getLinkedClosure) joins its
/// scope and closure in \p components. So does each capture edge
/// (getCaptureEdge): the group passes follow a capture only along one, so a
/// group must be closed under them. Unlike a closure link, a capture edge can
/// end at a thunk. The call edges BottomUpFunctionOrder follows are returned as
/// (caller, callee) bottomUp indices, once per pair, because components aren't
/// final until the scan ends.
static llvm::SmallVector<std::pair<unsigned, unsigned>, 64>
collectEdges(llvm::ArrayRef<SILFunction *> bottomUp,
             const llvm::DenseMap<SILFunction *, unsigned> &bottomUpIndex,
             BasicCalleeAnalysis *bca, llvm::IntEqClasses &components) {
  llvm::SmallVector<std::pair<unsigned, unsigned>, 64> callEdges;
  // The current caller's callees. A function calls the same callee, or
  // releases the same type, from many instructions.
  llvm::SmallDenseSet<unsigned, 16> callees;
  for (unsigned i = 0, e = bottomUp.size(); i != e; ++i) {
    callees.clear();
    auto join = [&](SILFunction *closure) {
      auto it = bottomUpIndex.find(closure);
      if (it != bottomUpIndex.end())
        components.join(i, it->second);
    };
    for (SILBasicBlock &block : *bottomUp[i]) {
      for (SILInstruction &inst : block) {
        if (auto *ref = dyn_cast<FunctionRefBaseInst>(&inst)) {
          if (SILFunction *closure = getLinkedClosure(ref))
            join(closure);
          continue;
        }
        if (auto apply = ApplySite::isa(&inst)) {
          if (auto edge = getCaptureEdge(apply))
            join(edge->callee);
        }
        BottomUpFunctionOrder::visitCallees(
            inst, bca, [&](SILFunction *callee) {
              auto it = bottomUpIndex.find(callee);
              if (it != bottomUpIndex.end() &&
                  callees.insert(it->second).second)
                callEdges.push_back({i, it->second});
            });
      }
    }
  }
  return callEdges;
}

// The constructor runs in four stages. Definitions are indexed by their
// position in bottom-up order. Union-find numbers the closure components 0 to
// M-1 in the order of their "leaders", their smallest position members.
//   1. Number function definitions in bottom-up order.
//   2. Scan every instruction once, building closure components (scopes
//      with their closures) and recording call edges.
//   3. Contract the call graph to one node per component.
//   4. Take the SCCs of that graph, callees first. Each SCC is a group;
//      ClosureGroupSchedule.h shows why one can hold several components.
ClosureGroupSchedule::ClosureGroupSchedule(SILModule &module,
                                           BasicCalleeAnalysis *bca) {
  // 1. Number the definitions. Declarations have no body to schedule.
  llvm::SmallVector<SILFunction *, 32> bottomUp;
  llvm::DenseMap<SILFunction *, unsigned> bottomUpIndex;
  BottomUpFunctionOrder order(module, bca);
  for (SILFunction *f : order.getFunctions()) {
    if (!f->isDefinition())
      continue;
    bottomUpIndex[f] = bottomUp.size();
    bottomUp.push_back(f);
  }
  unsigned n = bottomUp.size();

  // 2. Closure components and call edges.
  llvm::IntEqClasses components(n);
  auto callEdges = collectEdges(bottomUp, bottomUpIndex, bca, components);
  components.compress();
  unsigned numComponents = components.getNumClasses();
  llvm::SmallVector<unsigned, 32> componentOf(n);
  // Each component's members in bottom-up order, so the first is its leader.
  llvm::SmallVector<llvm::SmallVector<unsigned, 4>, 32> componentMembers(
      numComponents);
  for (unsigned i = 0; i != n; ++i) {
    componentOf[i] = components[i];
    componentMembers[componentOf[i]].push_back(i);
  }

  // 3. The call graph between components.
  ComponentGraph graph(callEdges, componentOf, numComponents);

  // 4. scc_iterator emits an SCC only after every SCC it calls into, so groups
  // come out callees first. The last SCC holds only the root.
  unsigned numScheduled = 0;
  for (auto scc = llvm::scc_begin(&graph); !scc.isAtEnd(); ++scc) {
    const std::vector<ComponentGraph::Node *> &sccNodes = *scc;
    if (sccNodes.front() == graph.getRoot())
      continue;
    llvm::SmallVector<unsigned, 8> members;
    for (ComponentGraph::Node *node : sccNodes)
      llvm::append_range(members, componentMembers[node - graph.nodes.data()]);
    llvm::sort(members); // Members keep bottom-up order.
    Group &group = groups.emplace_back();
    for (unsigned i : members) {
      group.push_back(bottomUp[i]);
      memberLeaders.push_back(
          bottomUp[componentMembers[componentOf[i]].front()]);
    }
    numScheduled += members.size();
  }
  assert(numScheduled == n && "a component is unreachable from the root");
  (void)numScheduled;
}

void ClosureGroupSchedule::print(llvm::raw_ostream &os) const {
  auto leader = memberLeaders.begin();
  for (unsigned i = 0, e = groups.size(); i != e; ++i) {
    os << "group " << i << "\n";
    for (SILFunction *f : groups[i])
      os << "  [" << (*leader++)->getName() << "] " << f->getName() << "\n";
  }
}

namespace swift::test {
// Dumps the module's closure groups in schedule order, each member prefixed
// by its closure component's leader in brackets.
static FunctionTest ClosureGroupScheduleTest(
    "closure_group_schedule", [](auto &function, auto &arguments, auto &test) {
      auto *bca = test.template getAnalysis<BasicCalleeAnalysis>();
      ClosureGroupSchedule schedule(function.getModule(), bca);
      schedule.print(llvm::outs());
    });
} // end namespace swift::test
