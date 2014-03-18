//===--- prims.cpp - Implementation of Prims MST algorithm ----------------===//
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

#include <cassert>
#include <cmath>

#include "graph.h"

namespace {

enum { UnknownIndex = -1 };

struct CostQueueElt {
  unsigned NodeId;
  double Cost;
};
static_assert(std::is_pod<CostQueueElt>::value, "CostQueueElt must be a POD "
              "type.");

static unsigned getLeftChildIndex(unsigned Index) {
  return Index*2 + 1;
}

static unsigned getRightChildIndex(unsigned Index) {
  return (Index + 1)*2;
}

static unsigned getParentIndex(unsigned ChildIndex) {
  return (ChildIndex - 1)/2;
}

/// A simple priority queue implementation based off of a binary heap that is
/// also able to use a map to enable fast update operations.
struct PriorityQueue {
  std::vector<CostQueueElt> Heap;
  std::vector<unsigned> GraphIndexToHeapIndexMap;

  CostQueueElt popHeap();
  bool updateCostIfLessThan(unsigned Id, double NewCost);
  unsigned updateHeapAtIndex(unsigned Index);
  void dump() {
#if 0
    dump_debug()
#endif
  }
  void dump_debug() {
    printf("QUEUE\n");
    for (unsigned i = 0, e = Heap.size(); i != e; ++i) {
      printf("(%u, %f)\n", Heap[i].NodeId, Heap[i].Cost);
    }
  }

  void check_invariants() {
#if 0
    std::vector<unsigned> Stack;
    Stack.push_back(0);

    while (!Stack.empty()) {
      unsigned Index = Stack.back();
      Stack.pop_back();

      unsigned LeftChild = getLeftChildIndex(Index);
      unsigned RightChild = getRightChildIndex(Index);

      if (LeftChild < Heap.size()) {
        assert(Heap[LeftChild].Cost >= Heap[Index].Cost);
        Stack.push_back(LeftChild);
      }
      if (RightChild < Heap.size()) {
        assert(Heap[RightChild].Cost >= Heap[Index].Cost);
        Stack.push_back(LeftChild);
      }
    }
#endif
  }
};

} // end anonymous namespace.

/// Pop off the smallest cost element, updating all data structures along the
/// way.
CostQueueElt
PriorityQueue::popHeap() {
  assert(!Heap.empty() && "This method should only be called if the heap is "
         "known non-empty.");

  // Copy the current heap head before doing anything.
  CostQueueElt Result = Heap.front();  

  // Swap the heap head with the last heap element and pop the heap.
  std::swap(Heap.front(), Heap.back());
  Heap.pop_back();

  // Invalidate the graph index of our old head and update the graph index of
  // the new head value.
  GraphIndexToHeapIndexMap[Heap.front().NodeId] = 0;
  GraphIndexToHeapIndexMap[Result.NodeId] = UnknownIndex;

  // Re-establish the heap property.
  unsigned HeapIndex = 0, SmallestIndex;
  while (true) {
    SmallestIndex = updateHeapAtIndex(HeapIndex);
    if (SmallestIndex == HeapIndex)
      break;
    HeapIndex = SmallestIndex;
  }

  // Return the copy.
  return Result;
}

/// Find the cost associated with GraphIndex. If NewCost is less than that
/// value, map GraphIndex to NewCost instead and return true. Return false
/// otherwise.
bool
PriorityQueue::updateCostIfLessThan(unsigned GraphIndex, double NewCost) {
  // Look up the heap index corresponding to GraphIndex.
  unsigned HeapIndex = GraphIndexToHeapIndexMap[GraphIndex];

  // If graph index is not in the heap, return false.
  if (HeapIndex == UnknownIndex)
    return false;

  // Otherwise, look up the cost of GraphIndex.
  auto &QueueElt = Heap[HeapIndex];

  // If NewCost >= QueueElt.cost, dont update anything and return false.
  if (NewCost >= QueueElt.Cost)
    return false;

  // Replace QueueElt.Cost with NewCost, update all relevant data structures,
  // and return true.
  QueueElt.Cost = NewCost;

  while (HeapIndex > 0 && HeapIndex != UnknownIndex) {
    HeapIndex = getParentIndex(HeapIndex);
    updateHeapAtIndex(HeapIndex);
  }

  return true;
}

/// Restore the heap property at Index.
unsigned
PriorityQueue::updateHeapAtIndex(unsigned Index) {
  unsigned LeftChildIndex = getLeftChildIndex(Index);
  unsigned RightChildIndex = getRightChildIndex(Index);
  unsigned SmallestIndex;
  if (LeftChildIndex < Heap.size() && Heap[LeftChildIndex].Cost < Heap[Index].Cost)
    SmallestIndex = LeftChildIndex;
  else
    SmallestIndex = Index;

  if (RightChildIndex < Heap.size() && Heap[RightChildIndex].Cost < Heap[SmallestIndex].Cost)
    SmallestIndex = RightChildIndex;

  if (SmallestIndex != Index) {
    std::swap(GraphIndexToHeapIndexMap[Heap[Index].NodeId],
              GraphIndexToHeapIndexMap[Heap[SmallestIndex].NodeId]);
    std::swap(Heap[Index], Heap[SmallestIndex]);
  }
  return SmallestIndex;
}

/// Compute the minimum spanning tree of the connected graph Graph.
///
/// Graph is the graph.
///
/// TreeEdges is the resulting set of tree edges mapping a node to its parent in
/// the tree.
///
/// Fun is the weight function. It is assumed that it uses data injected via a
/// closure.
void graph::prims(std::vector<Node *> &Graph,
                  std::vector<unsigned> &TreeEdges,
                  std::function<double (unsigned, unsigned)> Fun) {
  assert(Graph.size() < UnknownIndex && "We do not support more than "
         "(unsigned)-1 sized graphs since we use -1 as a sentinel value.");
  PriorityQueue Queue;
  Queue.dump();

  // Initialize our data structures. They will contain at most Graph.size()
  // elements, so just reserve that space now.
  unsigned GraphSize = Graph.size();
  Queue.Heap.reserve(GraphSize);
  Queue.GraphIndexToHeapIndexMap.reserve(GraphSize);
  TreeEdges.reserve(GraphSize);

  // Create our queue, selecting the first element of the graph as the root of
  // our tree for simplicity.
  Queue.Heap.push_back({0, 0.0});
  Queue.GraphIndexToHeapIndexMap.push_back(0);
  // Make the minimum spanning tree root its own parent for simplicity.
  TreeEdges.push_back(0);

  //printf("Creating graph...\n");
  for (unsigned i = 1; i < GraphSize; ++i) {
    Queue.Heap.push_back({i, INFINITY});
    Queue.GraphIndexToHeapIndexMap.push_back(i);
    Queue.dump();
    TreeEdges.push_back(UnknownIndex);
  }

  //printf("\nPerforming Algorithm...\n");
  // Until our queue is empty...
  while (!Queue.Heap.empty()) {
    // Extract the minimum element of the queue (i.e. the last one).
    CostQueueElt E = Queue.popHeap();
    Queue.check_invariants();
    unsigned NodeId = E.NodeId;

    // For each AdjIndex in the adjacentcy list of the node...
    for (unsigned AdjNodeIndex : Graph[NodeId]->adjList) {
      // Compute the distance from NodeIndex to AdjNodeIndex. If the distance in
      // between the two nodes is closer than the current set distance to the
      // spanning tree of the adjacent node, set NodeIndex to be the parent of
      // AdjNodeIndex and set V's set distance to be that value.
      if (Queue.updateCostIfLessThan(AdjNodeIndex,
                                     Fun(Graph[NodeId]->Id,
                                         Graph[AdjNodeIndex]->Id)))
        TreeEdges[AdjNodeIndex] = NodeId;
      Queue.check_invariants();
    }

    Queue.check_invariants();
    Queue.dump();
  }

#if 0
  // Make sure that every node is assigned a parent.
  for (unsigned i = 0, e = TreeEdges.size(); i != e; ++i) {
    unsigned ParentIndex = TreeEdges[i];
    assert(ParentIndex != UnknownIndex && "Found node with unknown parent index"
           " set.");
  }
#endif
}
