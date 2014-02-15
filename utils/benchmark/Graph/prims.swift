//===--- prims.swift - Implementation of Prims MST algorithm --------------===//
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

struct Node {
  var id : Int
  var adjList : Array<Int>
}

struct NodeCost {
  var graphIndex : Int
  var cost : Double
}

class PriorityQueue {
  var heap : Array<NodeCost?> 
  var graphIndexToHeapIndexMap : Array<Int?>

  init(size : Int) {
    heap = Array<NodeCost?>()
    graphIndexToHeapIndexMap = Array<Int?>()

    heap.reserve(size)
    graphIndexToHeapIndexMap.reserve(size)

    for i in 0...size {
      heap.append(.Some(NodeCost(graphIndex : i, cost : Double.inf())))
      graphIndexToHeapIndexMap.append(.Some(i))
    }
  }

  // Pop off the smallest cost element, updating all data structures along the
  // way.
  func popHeap() -> NodeCost {
    assert(!heap.isEmpty, "This method should only be called if heap is not empty.")

    // Swap the heap head with the last element of the heap and pop the heap.
    swap(&heap[0], &heap[heap.count-1])
    let result = heap.popLast()

    // Invalidate the graph index of our old head and update the graph index of
    // the new head value.
    graphIndexToHeapIndexMap[heap[0]!.graphIndex] = .Some(0)
    graphIndexToHeapIndexMap[result!.graphIndex] = .None

    // Re-establish our min heap.
    updateHeapAtIndex(0)

    // Return result.
    return result!
  }

  func updateCostIfLessThan(graphIndex : Int, newCost : Double) -> Bool {
    // Look up the heap index corresponding to the input graph index.
    let optHeapIndex = graphIndexToHeapIndexMap[graphIndex]

    // If the graph index is not in the heap, return false.
    if !optHeapIndex {
      return false
    }

    let heapIndex = optHeapIndex!

    // Otherwise, look up the cost of the node.
    let nodeCost = heap[heapIndex]

    // If newCost >= nodeCost.1, don't update anything and return false.
    if newCost >= nodeCost!.cost {
      return false
    }

    // Ok, we know that newCost < nodeCost.1 so replace nodeCost.1 with 
    // newCost and update all relevant data structures. Return true.
    heap[heapIndex] = .Some(NodeCost(nodeCost!.graphIndex, newCost))
    updateHeapAtIndex(heapIndex)

    return true
  }

  func getLeftChildIndex(index : Int) -> Int {
    return index*2 + 1
  }

  func getRightChildIndex(index : Int) -> Int {
    return (index + 1)*2
  }

  func updateHeapAtIndex(index : Int) {
    let leftChildIndex = getLeftChildIndex(index)
    let rightChildIndex = getRightChildIndex(index)

    var smallestIndex : Int = 0
    if leftChildIndex <= heap.count &&
       heap[leftChildIndex]!.cost < heap[index]!.cost {
      smallestIndex = leftChildIndex
    } else {
      smallestIndex = index
    }

    if rightChildIndex <= heap.count &&
       heap[rightChildIndex]!.cost < heap[smallestIndex]!.cost {
      smallestIndex = rightChildIndex
    }

    if smallestIndex != index {
      swap(&graphIndexToHeapIndexMap[heap[index]!.graphIndex],
           &graphIndexToHeapIndexMap[heap[smallestIndex]!.graphIndex])
      swap(&heap[index], &heap[smallestIndex])
      updateHeapAtIndex(smallestIndex)
    }
  }

  func isEmpty() -> Bool {
    return heap.isEmpty
  }
}

func prims(Graph : Array<Node>,
           Fun : (Int, Int) -> Double) -> Array<Int?> {
  // Initialize our data structures.
  var Queue = PriorityQueue(Graph.count)
  var TreeEdges = Array<Int?>(Graph.count, .None)

  // If graph is empty, just return tree edges with .None set to everything.
  if (Graph.isEmpty) {
    return TreeEdges
  }

  // Make the head of our tree its own parent for simplicity.
  TreeEdges[0] = .Some(0)

  // Until our queue is empty...
  while (!Queue.isEmpty()) {
    // Extract the minimum element of the queue (i.e. the last one).
    let cost = Queue.popHeap();
    let nodeIndex = cost.graphIndex

    // For each AdjIndex in the adjacentcy list of the node...
    for adjNodeIndex in Graph[nodeIndex].adjList {
      // Compute the distance from NodeIndex to AdjNodeIndex. If the distance in
      // between the two nodes is closer than the current set distance to the
      // spanning tree of the adjacent node, set NodeIndex to be the parent of
      // AdjNodeIndex and set V's set distance to be that value.
      if (Queue.updateCostIfLessThan(adjNodeIndex,
                                     Fun(Graph[nodeIndex].id,
                                         Graph[adjNodeIndex].id))) {
        TreeEdges[adjNodeIndex] = .Some(nodeIndex)
      }
    }
  }

  // Until we have #ifndef like support, just wrap this check in an assert
  // since that currently has the same affect due to the standard library
  // benchmark hack that is inplace.
  //
  // Remove x when rdar://16076294 is fixed.
  var x = { () -> Bool in
    for parentIndex in TreeEdges {
      assert(parentIndex, "Found node with unknown parent index  set.")
    }
    return true
  }
  assert(x(), "")

  return TreeEdges
}
