//===--- Prims.swift ------------------------------------------------------===//
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

// The test implements Prim's algorithm for minimum spanning tree building.
// http://en.wikipedia.org/wiki/Prim%27s_algorithm

// This class implements array-based heap (priority queue).
// It is used to store edges from nodes in spanning tree to nodes outside of it.
// We are interested only in the edges with the smallest costs, so if there are
// several edges pointing to the same node, we keep only one from them. Thus,
// it is enough to record this node instead.
// We maintain a map (node index in graph)->(node index in heap) to be able to
// update the heap fast when we add a new node to the tree.
import TestsUtils

class PriorityQueue {
  final var heap: Array<EdgeCost>
  final var graphIndexToHeapIndexMap: Array<Int?>

  // Create heap for graph with NUM nodes.
  init(Num: Int) {
    heap = Array<EdgeCost>()
    graphIndexToHeapIndexMap = Array<Int?>(repeating:nil, count: Num)
  }

  func isEmpty() -> Bool {
    return heap.isEmpty
  }

  // Insert element N to heap, maintaining the heap property.
  func insert(_ n: EdgeCost) {
    let ind: Int = heap.count
    heap.append(n)
    graphIndexToHeapIndexMap[n.to] = heap.count - 1
    bubbleUp(ind)
  }

  // Insert element N if in's not in the heap, or update its cost if the new
  // value is less than the existing one.
  func insertOrUpdate(_ n: EdgeCost) {
    let id = n.to
    let c  = n.cost
    if let ind = graphIndexToHeapIndexMap[id] {
      if heap[ind].cost <= c {
        // We don't need an edge with a bigger cost
        return
      }
      heap[ind].cost = c
      heap[ind].from = n.from
      bubbleUp(ind)
    } else {
      insert(n)
    }
  }

  // Restore heap property by moving element at index IND up.
  // This is needed after insertion, and after decreasing an element's cost.
  func bubbleUp(_ ind: Int) {
    var ind = ind
    let c = heap[ind].cost
    while (ind != 0) {
      let p = getParentIndex(ind)
      if heap[p].cost > c {
        Swap(p, with: ind)
        ind = p
      } else {
        break
      }
    }
  }

  // Pop minimum element from heap and restore the heap property after that.
  func pop() -> EdgeCost? {
    if (heap.isEmpty) {
      return nil
    }
    Swap(0, with:heap.count-1)
    let r = heap.removeLast()
    graphIndexToHeapIndexMap[r.to] = nil
    bubbleDown(0)
    return r
  }

  // Restore heap property by moving element at index IND down.
  // This is needed after removing an element, and after increasing an
  // element's cost.
  func bubbleDown(_ ind: Int) {
    var ind = ind
    let n = heap.count
    while (ind < n) {
      let l = getLeftChildIndex(ind)
      let r = getRightChildIndex(ind)
      if (l >= n) {
        break
      }
      var min: Int
      if (r < n && heap[r].cost < heap[l].cost) {
        min = r
      } else {
        min = l
      }
      if (heap[ind].cost <= heap[min].cost) {
        break
      }
      Swap(ind, with: min)
      ind = min
    }
  }

  // Swaps elements I and J in the heap and correspondingly updates
  // graphIndexToHeapIndexMap.
  func Swap(_ i: Int, with j : Int) {
    if (i == j) {
      return
    }
    (heap[i], heap[j]) = (heap[j], heap[i])
    let (I, J) = (heap[i].to, heap[j].to)
    (graphIndexToHeapIndexMap[I], graphIndexToHeapIndexMap[J]) =
    (graphIndexToHeapIndexMap[J], graphIndexToHeapIndexMap[I])
  }

  // Dumps the heap.
  func dump() {
    print("QUEUE")
    for nodeCost in heap {
      let to: Int = nodeCost.to
      let from: Int = nodeCost.from
      let cost: Double = nodeCost.cost
      print("(\(from)->\(to), \(cost))")
    }
  }

  func getLeftChildIndex(_ index : Int) -> Int {
    return index*2 + 1
  }
  func getRightChildIndex(_ index : Int) -> Int {
    return (index + 1)*2
  }
  func getParentIndex(_ childIndex : Int) -> Int {
    return (childIndex - 1)/2
  }
}

struct GraphNode {
  var id: Int
  var adjList: Array<Int>

  init(i : Int) {
    id = i
    adjList = Array<Int>()
  }
}

struct EdgeCost {
  var to: Int
  var cost: Double
  var from: Int
}

struct Edge : Equatable {
  var start: Int
  var end: Int
}

func ==(lhs: Edge, rhs: Edge) -> Bool {
  return lhs.start == rhs.start && lhs.end == rhs.end
}

extension Edge : Hashable {
  func hash(into hasher: inout Hasher) {
    hasher.combine(start)
    hasher.combine(end)
  }
}

func Prims(_ graph : Array<GraphNode>, _ fun : (Int, Int) -> Double) -> Array<Int?> {
  var treeEdges = Array<Int?>(repeating:nil, count:graph.count)

  let queue = PriorityQueue(Num:graph.count)
  // Make the minimum spanning tree root its own parent for simplicity.
  queue.insert(EdgeCost(to: 0, cost: 0.0, from: 0))

  // Take an element with the smallest cost from the queue and add its
  // neighbors to the queue if their cost was updated
  while !queue.isEmpty() {
    // Add an edge with minimum cost to the spanning tree
    let e = queue.pop()!
    let newnode = e.to
    // Add record about the edge newnode->e.from to treeEdges
    treeEdges[newnode] = e.from

    // Check all adjacent nodes and add edges, ending outside the tree, to the
    // queue. If the queue already contains an edge to an adjacent node, we
    // replace existing one with the new one in case the new one costs less.
    for adjNodeIndex in graph[newnode].adjList {
      if treeEdges[adjNodeIndex] != nil {
        continue
      }
      let newcost = fun(newnode, graph[adjNodeIndex].id)
      queue.insertOrUpdate(EdgeCost(to: adjNodeIndex, cost: newcost, from: newnode))
    }
  }
  return treeEdges
}
