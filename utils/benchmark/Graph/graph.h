//===--- Graph.h - Header for Graph based Benchmarks ------------*- C++ -*-===//
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

#include <vector>
#include <functional>

namespace graph {

// It is expected that the user uses a closure + data_index to perform
// comparisons. This will enable us to test closure computations.
struct Node {
  Node(unsigned i) : Id(i), adjList() { }
  /// The index in the data array of Node's data.
  unsigned Id;

  /// The indices in the graph array of the nodes adjacent to node.
  std::vector<unsigned> adjList;
};

/// Calculate the minimum spanning tree of the connected graph G using the
/// weight function Fun. Returns result in TreeEdges.
void prims(std::vector<Node *> &Graph,
           std::vector<unsigned> &TreeEdges,
           std::function<double (unsigned, unsigned)> Fun);

} // end namespace graph
