//===--- GraphViz.h - Utilities for outputting graphs -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines data structures for creating transient graphs in order to print
/// them to GraphViz DOT format.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_GRAPHVIZ_H
#define SWIFT_BASIC_GRAPHVIZ_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"

#include <iterator>
#include <string>
#include <utility>
#include <vector>

namespace swift {
// MARK: - MultiGraph

/// A multigraph data structure, with a set of edges connecting pairs of nodes.
///
/// The fields are left public so that you can potentially customize printing
/// directly, instead of relying on the existing customization points.
///
/// \invariant {
///   \code forall n1 n2. nodePairs[{n1, n2}] < edgeSets.size() \endcode
/// }
template <typename Node, typename Edge, typename EdgeSet>
struct MultiGraph {
  /// List of nodes stored in the graph.
  llvm::DenseMap<std::pair<Node, Node>, size_t> nodePairs;

  /// List of edges stored in the graph.
  std::vector<EdgeSet> edgeSets;

  /// Three way comparison function for nodes used for deterministic output.
  llvm::function_ref<int (Node, Node)> compareNodes;

  /// Three way comparison function for edges used for deterministic output.
  llvm::function_ref<int (Edge, Edge)> compareEdges;

  /// Default attributes used for styling nodes.
  std::string defaultNodeAttrs =
      "shape = \"box\", style = \"rounded\", penwidth = \"2\"";

  /// Print the name of a node.
  llvm::function_ref<void (Node, llvm::raw_ostream &)> printNodeName;

  /// Print the attributes used to style a node, potentially null.
  llvm::function_ref<void (Node, llvm::raw_ostream &)> printNodeAttr;

  /// Print the label for the edge set between a pair of nodes, optionally
  /// customizing it based on the nodes themselves.
  llvm::function_ref<
      void (Node, Node, const SmallVectorImpl<Edge> &,
            llvm::raw_ostream &)> printEdgeSet;

public:
  MultiGraph() = default;
  MultiGraph(const MultiGraph &) = delete;
  MultiGraph(MultiGraph &&) = default;

  /// Insert an edge to the set of edges between two nodes.
  void updateEdge(Node from, Node to, Edge edge) {
    auto it = nodePairs.find(std::pair<Node, Node>{from, to});
    if (it != nodePairs.end()) {
      edgeSets[it->second].insert(edge);
      return;
    }
    nodePairs.insert({{from, to}, edgeSets.size()});
    EdgeSet edgeSet{};
    edgeSet.insert(edge);
    edgeSets.push_back(edgeSet);
  };

  /// Output the graph in DOT format to \p os.
  void printAsGraphviz(llvm::raw_ostream &os, bool horizontal = false) {
    os << "digraph CustomGraph {\n";
    os << "  node [" << defaultNodeAttrs; os << "];\n";
    if (horizontal)
      os << "  rankdir = \"LR\";\n";

    auto copyAndSort = [](const auto &set, auto &vec, auto cmp) {
      vec.reserve(set.size());
      llvm::copy(set, std::back_inserter(vec));
      llvm::sort(vec, [&](auto t1, auto t2) { return cmp(t1, t2) < 0;});
    };

    // Print the nodes first in case we have custom attributes for nodes.
    if (printNodeAttr) {
      llvm::DenseSet<Node> nodes{};
      for (auto &entry: nodePairs) {
        nodes.insert(entry.first.first);
        nodes.insert(entry.first.second);
      }
      std::vector<Node> nodesVec{};
      copyAndSort(nodes, nodesVec, compareNodes);
      for (auto node: nodes) {
        os << "  ";
        printNodeName(node, os);
        printNodeAttr(node, os);
        os << ";\n";
      }
    }
    std::vector<std::pair<std::pair<Node, Node>, size_t>> nodePairsVec{};
    copyAndSort(nodePairs, nodePairsVec,
                [this](auto e1, auto e2) -> int {
      auto nodePair1 = e1.first;
      auto nodePair2 = e1.first;
      auto cmp1 = compareNodes(nodePair1.first, nodePair2.first);
      if (cmp1 < 0) return cmp1;
      return compareNodes(nodePair1.second, nodePair2.second);
    });
    SmallVector<Edge, 5> scratchEdges;
    for (auto &entry: nodePairsVec) {
      auto from = entry.first.first;
      auto to = entry.first.second;
      auto edgeSetIndex = entry.second;
      os << "  ";
      printNodeName(from, os);
      os << " -> ";
      printNodeName(to, os);
      if (printEdgeSet) {
        copyAndSort(edgeSets[edgeSetIndex], scratchEdges, compareEdges);
        os << " [label = \"";
        printEdgeSet(from, to, scratchEdges, os);
        os << "\"]";
      }
      scratchEdges.clear();
      os << ";\n";
    }
    os << "}\n";
  }
};

} // end namespace swift

#endif // SWIFT_BASIC_GRAPHVIZ_H
