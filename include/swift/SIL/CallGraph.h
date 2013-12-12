//===--- CallGraph.h --- Call-Graph Utilities -------------------*- C++ -*-===//
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

#ifndef SWIFT_SIL_CALLGRAPH_H
#define SWIFT_SIL_CALLGRAPH_H

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/DenseMap.h"
#include <vector>

namespace swift {

/// \brief a utility class that is used to traverse the call-graph
/// and return a top-down or bottom up list of functions to process.
template <typename Node> struct CallGraphSorter {
  typedef std::pair<Node, Node> Edge;
  typedef std::vector<Node> NodeList;
  typedef llvm::DenseSet<Edge> EdgeList;
  typedef llvm::DenseMap<Node, NodeList> Graph;

  CallGraphSorter() {}

  /// \brief Add an edge in the call graph.
  void addEdge(Node From, Node To) {
    // Construct a stable order of nodes based on the insertion order. We only
    // record 'From' nodes because this is where we start our search. We use
    // self edges to record the list of recorded nodes.
    if (edges.insert(std::make_pair(From, From)).second)
      StableNodeList.push_back(From);

    // Add new edges to the graph.
    if (edges.insert(std::make_pair(From, To)).second)
      graph[From].push_back(To);
  }

  /// Perform a post-order scan of the graph while ignoring back-edges.
  /// Returns a list of edged sorted bottom-up.
  void sort(std::vector<Node> &Res) {
    std::vector<Node> Worklist;
    llvm::DenseSet<Node> Seen;

    // We start the scan from each of the nodes in our stable node list. This
    // is equivalent to constructing a new node with edges to each one of our
    // nodes. We need to do this because we don't know if there is a single
    // node that dominates all others.
    for (auto StartNode : StableNodeList) {
      // Start the DFS scan with this node.
      if (Seen.insert(StartNode).second)
        Worklist.push_back(StartNode);

      // For each starting point in the graph, perform a post-order DFS scan
      // and insert the values into Res.
      while (Worklist.size()) {
        Node N = Worklist.back();
        assert(Seen.count(N) && "unknown node");

        // Insert all of the successors what were not seen before (unvisited
        // nodes) into the worklist.
        NodeList &Succ = graph[N];
        for (Node S : Succ)
          if (Seen.insert(S).second)
            Worklist.push_back(S);

        // If we did not push any successors then we can schedule this node.
        if (Worklist.back() == N) {
          Worklist.pop_back();
          Res.push_back(N);
        }
      }
    }
  }

private:
  /// Adjacency list.
  Graph graph;
  /// A global list of edges with no repetitions.
  EdgeList edges;
  /// A stable order for the nodes in the graph.
  NodeList StableNodeList;
};

}  // end namespace swift

#endif
