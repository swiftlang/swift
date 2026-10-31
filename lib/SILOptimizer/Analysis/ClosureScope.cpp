//===--- ClosureScope.cpp - Closure scope graph and order -----------------===//
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
/// Implementation of ClosureGraph and ClosureFunctionOrder.
///
/// The "scope" of a closure is the function body that refers to the closure.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "closure-scope"

#include "swift/SILOptimizer/Analysis/ClosureScope.h"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/ApplySite.h"
#include "llvm/ADT/iterator.h"

namespace swift {

// A function is a node in this graph if it either refers to a closure or is
// itself a closure.
//
// The graph is represented with a vector of forward edges from "scope
// functions" to closures, and the set of closures that have a scope.
class ClosureGraph {
  struct Edge {
    SILFunction *from;
    SILFunction *to;

    bool operator<(const Edge &other) const {
      unsigned from1 = from->getIndex();
      unsigned from2 = other.from->getIndex();
      if (from1 != from2)
        return from1 < from2;

      return to->getIndex() < other.to->getIndex();
    }
  };

  using EdgePos = Edge *;
  using EdgeRange = llvm::iterator_range<EdgePos>;

  struct NodeIterator
      : llvm::iterator_adaptor_base<
            NodeIterator, EdgePos, std::random_access_iterator_tag,
            SILFunction *, ptrdiff_t, SILFunction *, SILFunction *> {

    NodeIterator() = default;

    explicit NodeIterator(EdgePos pos) : iterator_adaptor_base(pos) {}

    EdgePos edgeIter() const { return I; }

    SILFunction *operator*() const { return I->from; }
  };

  // Compare the node identified by the from index of \p edge with the node
  // identified by \p functionIndex.
  struct NodeCompare {
    bool operator()(SILFunction *node1, SILFunction *node2) {
      return node1->getIndex() < node2->getIndex();
    }
  };

  // A set of edges in the relational mapping from a function to the set of
  // nonescaping closures that are referenced within its body.
  //
  // from: parent function
  //   to: nonescaping child closure
  llvm::SmallVector<Edge, 8> functionToClosures;

  // Every nonescaping closure that a parent function refers to.
  //
  // A closure almost always has a single parent. A local function, however, may
  // be referenced recursively, even within a different closure. Notice that
  // capturedInt is captured from the outer-most function, but is passed down
  //
  //   func localFunc(b: Apply) {
  //     capturedInt += 1
  //     let closure = { (c: Apply) in
  //       c.apply(localFunc)
  //     }
  //     b.apply(closure)
  //   }
  //   a.apply(localFunc)
  llvm::SmallPtrSet<SILFunction *, 8> scopedClosures;

public:
  ClosureGraph() = default;

  /// The edges to the closures directly referenced by \p scopeFunc.
  EdgeRange getFunctionToClosureEdges(SILFunction *scopeFunc) {
    return getEdgeRange(scopeFunc, functionToClosures);
  }

  /// Return true if this function is a reachable closure.
  bool isReachableClosure(SILFunction *function) {
    return scopedClosures.contains(function);
  }

  /// Records the scope edges among \p functions, which must be closed under
  /// scope edges to definitions.
  void compute(ArrayRef<SILFunction *> functions);

protected:
  ClosureGraph(const ClosureGraph &) = delete;
  ClosureGraph &operator=(const ClosureGraph &) = delete;

  EdgeRange getEdgeRange(SILFunction *node,
                         llvm::SmallVectorImpl<Edge> &edges) {
    auto it = std::lower_bound(NodeIterator(edges.begin()),
                               NodeIterator(edges.end()),
                               node, NodeCompare());
    auto next = it.edgeIter();
    for (auto end = edges.end(); next != end; ++next) {
      if (next->from != node)
        break;
    }
    return EdgeRange(it.edgeIter(), next);
  }

  void recordScope(ApplySite apply,
                   const llvm::SmallPtrSetImpl<SILFunction *> &within);

  void finalize();

  SWIFT_ASSERT_ONLY_DECL(void dump());
};

std::optional<CaptureEdge> getCaptureEdge(ApplySite apply) {
  // The callee value's type, so a partial_apply that captures a box is an edge
  // and an apply of its result isn't.
  auto calleeTy = apply.getCallee()->getType().castTo<SILFunctionType>();
  bool byAddress = isNonEscapingClosure(calleeTy);
  if (!byAddress &&
      llvm::none_of(calleeTy->getParameters(), [](SILParameterInfo param) {
        return param.getInterfaceType()->is<SILBoxType>();
      }))
    return std::nullopt;

  if (SILFunction *callee = apply.getCalleeFunction())
    return CaptureEdge{callee, byAddress, /*dynamicallyReplaceable=*/false};

  if (!apply.isCalleeDynamicallyReplaceable())
    return std::nullopt; // An indirect callee.

  SILValue origin = apply.getCalleeOrigin();
  while (auto *pai = dyn_cast<PartialApplyInst>(origin))
    origin = pai->getCalleeOrigin();

  return CaptureEdge{
      cast<FunctionRefBaseInst>(origin)->getInitiallyReferencedFunction(),
      byAddress, /*dynamicallyReplaceable=*/true};
}

void ClosureGraph::recordScope(
    ApplySite apply, const llvm::SmallPtrSetImpl<SILFunction *> &within) {
  // The graph holds the capture edges into non-escaping closures.
  auto edge = getCaptureEdge(apply);
  if (!edge || !edge->byAddress || edge->dynamicallyReplaceable)
    return;
  SILFunction *closureFunc = edge->callee;

  auto scopeFunc = apply.getFunction();
  if (!within.count(closureFunc)) {
    // The function set must be closed under scope edges to definitions. An
    // edge to a declaration is fine to drop.
    ASSERT(!closureFunc->isDefinition() &&
           "function set is not closed under scope edges");
    return;
  }
  // Passes may assume that a deserialized function can only refer to
  // deserialized closures. For example, AccessEnforcementSelection skips
  // deserialized functions but assumes all a closure's parent scope have been
  // processed.
  assert(scopeFunc->wasDeserializedCanonical() ==
             closureFunc->wasDeserializedCanonical() &&
         "A closure cannot be serialized in a different module than its "
         "parent context");

  functionToClosures.push_back({scopeFunc, closureFunc});
  scopedClosures.insert(closureFunc);
}

void ClosureGraph::finalize() {
  llvm::stable_sort(functionToClosures);

  LLVM_DEBUG(dump());
}

#ifndef NDEBUG
static void dumpFunctionName(SILFunction *function) {
  auto opts = Demangle::DemangleOptions::SimplifiedUIDemangleOptions();
  opts.ShowAsyncResumePartial = true;
  llvm::dbgs() << Demangle::demangleSymbolAsString(function->getName(), opts)
               << " '" << function->getName() << "'\n";
}

void ClosureGraph::dump() {
  llvm::dbgs() << "\n";
  SILFunction *currentFunc = nullptr;
  for (auto &edge : functionToClosures) {
    auto *scopeFunc = edge.from;
    if (currentFunc != scopeFunc) {
      currentFunc = scopeFunc;
      llvm::dbgs() << "SCOPE: ";
      dumpFunctionName(scopeFunc);
    }
    llvm::dbgs() << "    CLOSURE: ";
    dumpFunctionName(edge.to);
  }
  // The backward edges, from closures to scope functions.
  llvm::SmallVector<Edge, 8> closureToFunctions;
  for (auto &edge : functionToClosures)
    closureToFunctions.push_back({edge.to, edge.from});
  llvm::sort(closureToFunctions);
  currentFunc = nullptr;
  for (auto &edge : closureToFunctions) {
    auto *closure = edge.from;
    if (currentFunc != closure) {
      currentFunc = closure;
      llvm::dbgs() << "CLOSURE: ";
      dumpFunctionName(closure);
    }
    llvm::dbgs() << "    SCOPE: ";
    dumpFunctionName(edge.to);
  }
}
#endif

void ClosureGraph::compute(ArrayRef<SILFunction *> functions) {
  llvm::SmallPtrSet<SILFunction *, 16> within(functions.begin(),
                                              functions.end());
  for (SILFunction *F : functions)
    for (auto &BB : *F)
      for (auto &I : BB)
        if (auto apply = ApplySite::isa(&I))
          recordScope(apply, within);
  finalize();
}

class ClosureFunctionOrder::ClosureDFS {
  ClosureFunctionOrder &functionOrder;
  ClosureGraph &graph;

  llvm::SmallPtrSet<SILFunction *, 16> visited;
  llvm::SmallPtrSet<SILFunction *, 16> finished;

  SmallVector<SILFunction *, 4> postorderClosures;

public:
  ClosureDFS(ClosureFunctionOrder &functionOrder, ClosureGraph &graph)
      : functionOrder(functionOrder), graph(graph) {}

  bool isVisited(SILFunction *function) const {
    return visited.contains(function);
  }

  void performDFS(SILFunction *root) {
    postorderClosures.clear();

    recursiveDFS(root);

    // Closures are discovered in postorder, bottom-up.
    // Reverse-append them onto the top-down function list.
    llvm::append_range(functionOrder.topDownFunctions,
                       llvm::reverse(postorderClosures));
  }

protected:
  void recursiveDFS(SILFunction *function) {
    if (!visited.insert(function).second) {
      if (!finished.contains(function)) {
        // Cycle in the closure graph.
        functionOrder.closureCycleHeads.insert(function);
      }
      return;
    }

    for (auto &edge : graph.getFunctionToClosureEdges(function))
      recursiveDFS(edge.to);
    finished.insert(function);
    postorderClosures.push_back(function);
  };
};

ClosureFunctionOrder::ClosureFunctionOrder(ArrayRef<SILFunction *> functions) {
  ClosureGraph graph;
  graph.compute(functions);
  topDownFunctions.reserve(functions.size());

  ClosureDFS dfs(*this, graph);
  SmallVector<SILFunction *, 4> closureWorklist;

  for (SILFunction *function : functions) {
    if (dfs.isVisited(function))
      continue;

    if (graph.isReachableClosure(function)) {
      // This is a closure, reachable from some other function. Reaching this
      // point is rare, because closures typically follow their parent in the
      // function list.
      //
      // A closure at the head of a cycle might only be reachable from other
      // closures. So use a worklist to visit them once more as DFS roots after
      // finishing DFS from all acyclic functions.
      closureWorklist.push_back(function);
      continue;
    }
    dfs.performDFS(function);
  }
  // Revisit closures in forward order. DFS will immediately return except in
  // the unlikely event that this is an orphaned closure.
  for (auto *closure : closureWorklist) {
    dfs.performDFS(closure);
  }
  LLVM_DEBUG(dump());
  assert(functions.size() == topDownFunctions.size() &&
         "DFS missed a function");
}

#ifndef NDEBUG
void ClosureFunctionOrder::dump() {
  llvm::dbgs() << "\nRPO function order:\n";
  for (auto *function : getTopDownFunctions()) {
    llvm::dbgs() << "[" << function->getIndex() << "] ";
    if (isHeadOfClosureCycle(function))
      llvm::dbgs() << "CYCLE HEAD: ";

    dumpFunctionName(function);
  }
}
#endif

} // namespace swift
